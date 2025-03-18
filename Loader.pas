unit Loader;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.IOUtils, System.Types,
  System.Variants, Winapi.Windows, System.Generics.Collections, System.DateUtils,
  Log, Utils, tlhelp32, Winapi.PsAPI, Injector, SyncObjs,
  LoaderManager;     // Contains TChainLoader

type
  // TLoader is responsible for loading modules (DLLs) according to a configuration file.
  // It supports both immediate and delayed loading, module version selection, and logging.
  TLoader = class
  private
    // Counters for delayed tasks and synchronization objects.
    DelayedTasksCompleted: Integer;
    DelayedTasksTotal: Integer;
    TaskLock: TObject;
    FDelayedTaskEvent: TEvent;
    // Configuration file path and search directories.
    FConfigPath: string;
    SearchDirectories: TArray<string>;
    ModulesToLoad: TArray<string>;
    // Collections for loaded and failed modules, plus a dictionary for module handles.
    LoadedModules: TStringList;
    FailedModules: TStringList;
    LoadedModuleHandles: TDictionary<string, HMODULE>;
    MaxRetries: Integer;
    HostAppDirectory: string;
    TerminateFlag: Boolean;
    FLog: TLog;  // Internal logging instance.
    // Cache for directory files to speed up searches.
    FDirectoryCache: TDictionary<string, TStringDynArray>;
    // Helper methods.
    procedure SignalDelayedTaskCompleted;
    procedure UpdateBestCandidate(const AFile: string; var BestVersion, BestFile: string;
      var BestFileDate: TDateTime; const Lock: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    function ReadCFG(const filename, Section, Key: string; const DefaultValue: Variant): Variant;
    procedure LoadConfig(const ConfigPath: string);
    function LoadModule(const ModulePath: string): Boolean;
    procedure StartDelayedTask(const ModuleName, WaitForModule, WaitForProcess: string; Delay: Integer);
    function FindModule(const ModuleName: string; SearchAppDir: Boolean = False): string;
    function IsCompatible(const ModulePath: string): Boolean;
    function GetVersion(const DLLPath: string): string;
    function GetFileDate(const FileName: string): TDateTime;
    function GetHostAppDirectory: string;
    function IsHostX64: Boolean;
    function IsDllX64(const FileName: string): Boolean;
    function FindProcessByName(const ProcessName: string): Integer;
    function ModuleExistsInHost(const ModuleName: string): Boolean;
  end;

type
  // Record type for delayed task parameters.
  TDelayedTaskParams = record
    DelayMS: Cardinal;
    ModuleName: string;
    ProcessName: string;
    TaskMethod: string;
    DLLPath: string;
  end;

const
  MODULE_NAME = 'Lib-Loader (Chain Loading)';
  LOG_FILENAME = 'Loader.log';
  CONFIG_FILENAME = 'Loader.cfg';

var
  LibLoader: TLoader;
  ChainLoader: TChainLoader;

implementation

uses
  System.Threading; // For TTask and TParallel

// --------------------------
// TLoader Implementation
// --------------------------

// Constructor: Initializes the loader, sets up the logging, configuration, and internal collections.
constructor TLoader.Create;
var
  DLLmodule: array[0..MAX_PATH] of Char;
  CfgFile: TIniFile;
begin
  TaskLock := TObject.Create;
  DelayedTasksCompleted := 0;
  DelayedTasksTotal := 0;
  FDelayedTaskEvent := TEvent.Create(nil, True, False, '');
  FDirectoryCache := TDictionary<string, TStringDynArray>.Create;
  // Determine the configuration file location based on the module path.
  GetModuleFileName(hInstance, DLLmodule, Length(DLLmodule));
  FConfigPath := ExtractFilePath(DLLmodule) + CONFIG_FILENAME;
  HostAppDirectory := GetHostAppDirectory;
  TerminateFlag := False;
  CfgFile := TIniFile.Create(FConfigPath);
  try
    try
      // Create the log object and write the header messages.
      FLog := TLog.Create(LOG_FILENAME, 80, CfgFile.ReadBool('LOG', 'Append', False),
        CfgFile.ReadInteger('Debug', 'Mode', 1));
      FLog.Header(MODULE_NAME);
      FLog.HR('=');
      FLog.Add('Initializing...', PLAIN, '', 4);
      FLog.Add('Module: ' + ExtractFileName(DLLmodule), PLAIN, '', 4);
      FLog.Add('Version: ' + GetVersion(DLLmodule), PLAIN, '', 4);
      FLog.Add('Config File: ' + FConfigPath, PLAIN, '', 4);
      FLog.DebugEnabled := CfgFile.ReadBool('DEBUG', 'Enabled', False);
      FLog.Add('DEBUG Output: ' + BoolToStr(FLog.DebugEnabled, True), PLAIN, '', 4);
    finally
      CfgFile.Free;
    end;
  except
    on E: Exception do
      FLog.Add('Error during initialization: ' + E.Message, ERROR, '', 4);
  end;
  // Initialize module collections.
  LoadedModules := TStringList.Create;
  FailedModules := TStringList.Create;
  LoadedModuleHandles := TDictionary<string, HMODULE>.Create;
  MaxRetries := 3;
end;

// Destructor: Frees all allocated objects.
destructor TLoader.Destroy;
begin
  FDirectoryCache.Free;
  FDelayedTaskEvent.Free;
  TaskLock.Free;
  LoadedModules.Free;
  FailedModules.Free;
  LoadedModuleHandles.Free;
  FLog.Free;
  inherited;
end;

// Returns the directory of the host application.
function TLoader.GetHostAppDirectory: string;
var
  HostAppModule: array[0..MAX_PATH] of Char;
begin
  GetModuleFileName(0, HostAppModule, Length(HostAppModule));
  Result := ExtractFilePath(HostAppModule);
end;

// Checks if a module (DLL) is already loaded in the host process.
function TLoader.ModuleExistsInHost(const ModuleName: string): Boolean;
var
  ModuleHandles: array[0..1023] of HMODULE;
  ModuleCount: DWORD;
  ModuleBaseName: array[0..MAX_PATH] of Char;
  i: Integer;
begin
  Result := False;
  if EnumProcessModules(GetCurrentProcess, @ModuleHandles, SizeOf(ModuleHandles), ModuleCount) then
  begin
    ModuleCount := ModuleCount div SizeOf(HMODULE);
    for i := 0 to ModuleCount - 1 do
      if (GetModuleBaseName(GetCurrentProcess, ModuleHandles[i], ModuleBaseName, SizeOf(ModuleBaseName)) > 0)
         and SameText(ModuleBaseName, ModuleName) then
      begin
        Result := True;
        Break;
      end;
  end;
end;

// Attempts to load a module from the specified path.
// Logs success or failure along with timing information.
function TLoader.LoadModule(const ModulePath: string): Boolean;
var
  StartTime, EndTime: TDateTime;
  ModuleHandle: HMODULE;
begin
  StartTime := Now;
  Result := False;
  // Log attempt to load module.
  FLog.Add(Format('Attempting to load module...', [ModulePath]), DEBUG, '', 4);
  // Check if file exists.
  if not FileExists(ModulePath) then
  begin
    FLog.Add(Format('File does not exist: %s', [ModulePath]), ERROR, '', 4);
    Exit;
  end;
  // Check compatibility (architecture) of the module.
  if not IsCompatible(ModulePath) then
  begin
    FLog.Add(Format('Module is not compatible: %s', [ModulePath]), ERROR, '', 4);
    Exit;
  end;
  // Load the DLL.
  ModuleHandle := LoadLibrary(PChar(ModulePath));
  if ModuleHandle = 0 then
  begin
    FLog.Add(Format('LoadLibrary failed for: %s. Error: %s', [ModulePath, SysErrorMessage(GetLastError)]), ERROR, '', 4);
    Exit;
  end;
  // Store the handle and record success.
  LoadedModuleHandles.Add(ExtractFileName(ModulePath), ModuleHandle);
  LoadedModules.Add(ModulePath);
  FLog.Add(Format('Successfully loaded module: %s', [ModulePath]), PLAIN, '', 4);
  Result := True;
  EndTime := Now;
  FLog.Add(Format('Time taken to load module: %s = %.2f ms', [ModulePath, MilliSecondsBetween(EndTime, StartTime)]), DEBUG, '', 4);
end;

// Searches for the module by name in the configured directories or in the host application directory.
// Detailed logging is provided for each search step and evaluation of candidate files.
function TLoader.FindModule(const ModuleName: string; SearchAppDir: Boolean = False): string;
var
  i: Integer;
  Directory: string;
  Files: TStringDynArray;
  BestVersion, BestFile: string;
  BestFileDate: TDateTime;
  SearchStart, SearchEnd: TDateTime;  // Timer variables
  ElapsedMs: Double;
begin
  SearchStart := Now;  // Start the timer
  Result := '';
  BestVersion := '0.0.0.0';
  BestFileDate := 0;
  BestFile := '';
  FLog.Add(Format('Searching for module: %s (SearchAppDir=%s)', [ModuleName, BoolToStr(SearchAppDir, True)]), DEBUG, '', 4);

  if not SearchAppDir then
  begin
    for i := Low(SearchDirectories) to High(SearchDirectories) do
    begin
      Directory := SearchDirectories[i].Trim;
      if not DirectoryExists(Directory) then
      begin
        FLog.Add('Directory does not exist: ' + Directory, ERROR, '', 12);
        Continue;
      end;
      if not IsDirectoryAccessible(Directory) then
      begin
        FLog.Add('Directory not accessible: ' + Directory, ERROR, '', 12);
        Continue;
      end;
      FLog.Add('Searching in directory: ' + Directory, DEBUG, '', 12);
      try
        Files := TDirectory.GetFiles(Directory, ModuleName, TSearchOption.soAllDirectories);
      except
        on E: Exception do
        begin
          FLog.Add('Error retrieving files from directory: ' + Directory + '. ' + E.Message, ERROR, '', 12);
          Continue;
        end;
      end;
      if Length(Files) > 0 then
      begin
        for var AFile in Files do
          UpdateBestCandidate(AFile, BestVersion, BestFile, BestFileDate, TaskLock);
        if BestFile <> '' then
        begin
          FLog.Add('Best match found: ' + BestFile, DEBUG, '', 12);
          Break;
        end;
      end;
    end;
    if BestFile = '' then
    begin
      FLog.Add('Module not found in specified directories, falling back to application directory.', DEBUG, '', 12);
      Result := IncludeTrailingPathDelimiter(HostAppDirectory) + ModuleName;
      if FileExists(Result) then
        FLog.Add('Module found in application directory: ' + Result, DEBUG, '', 12)
      else
      begin
        FLog.Add('File ' + ModuleName + ' not found in the application/client directory.', ERROR, '', 4);
        Result := '';
      end;
    end
    else
      Result := BestFile;
  end
  else
  begin
    Result := IncludeTrailingPathDelimiter(HostAppDirectory) + ModuleName;
    if FileExists(Result) then
      FLog.Add('Module found in application directory: ' + Result, DEBUG, '', 12)
    else
    begin
      FLog.Add('File ' + ModuleName + ' not found in the application/client directory.', ERROR, '', 4);
      Result := '';
    end;
  end;
  // End the timer and log the elapsed time.
  SearchEnd := Now;
  ElapsedMs := MilliSecondsBetween(SearchEnd, SearchStart);
  FLog.Add(Format('Time taken to find module %s: %.2f ms', [ModuleName, ElapsedMs]), DEBUG, '', 4);
  if Result <> '' then
    FLog.Add(Format('Module Path: %s', [Result]), DEBUG, '', 4)
  else
    FLog.Add(Format('Module not found: %s', [ModuleName]), ERROR, '', 4);
end;



// Retrieves the version string of the given DLL using Windows version APIs.
function TLoader.GetVersion(const DLLPath: string): string;
var
  Size: DWORD;
  Handle: DWORD;
  Buffer: Pointer;
  VerInfo: PVSFixedFileInfo;
  VerInfoSize: UINT;
begin
  Result := '';
  Size := GetFileVersionInfoSize(PChar(DLLPath), Handle);
  if Size = 0 then Exit;
  GetMem(Buffer, Size);
  try
    try
      if GetFileVersionInfo(PChar(DLLPath), 0, Size, Buffer) then
        if VerQueryValue(Buffer, '\', Pointer(VerInfo), VerInfoSize) then
          Result := Format('%d.%d.%d.%d', [HiWord(VerInfo^.dwFileVersionMS),
            LoWord(VerInfo^.dwFileVersionMS), HiWord(VerInfo^.dwFileVersionLS),
            LoWord(VerInfo^.dwFileVersionLS)]);
    finally
      FreeMem(Buffer);
    end;
  except
    on E: Exception do
      FLog.Add('Error reading version from ' + DLLPath + ': ' + E.Message, ERROR, '', 4);
  end;
end;

// Retrieves the file date (last write time) of a file.
function TLoader.GetFileDate(const FileName: string): TDateTime;
var
  FileAttributes: TWin32FileAttributeData;
  FileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := 0;
  if GetFileAttributesEx(PChar(FileName), GetFileExInfoStandard, @FileAttributes) and
     FileTimeToSystemTime(FileAttributes.ftLastWriteTime, SystemTime) then
    Result := SystemTimeToDateTime(SystemTime);
end;

// Loads the configuration file, sets up search directories, modules to load, and delayed tasks.
procedure TLoader.LoadConfig(const ConfigPath: string);
var
  Ini: TIniFile;
  Sections, DelayedModules, RegularModules: TStringList;
  SectionName, WaitForModule, WaitForProcess, DelayedModuleFile: string;
  DelayValue: Integer;
  i: Integer;
begin
  Ini := TIniFile.Create(ConfigPath);
  Sections := TStringList.Create;
  DelayedModules := TStringList.Create;
  RegularModules := TStringList.Create;
  try
    try
      // Read search directories and module file names.
      SearchDirectories := Ini.ReadString('Loader', 'ModFolders', '').Split([',']);
      for i := 0 to High(SearchDirectories) do
        SearchDirectories[i] := SearchDirectories[i].Trim;
      RegularModules.AddStrings(Ini.ReadString('Loader', 'Files', '').Split([',']));
      for i := 0 to RegularModules.Count - 1 do
        RegularModules[i] := RegularModules[i].Trim;
      // Build a directory cache to avoid repeated disk reads.
      for i := 0 to High(SearchDirectories) do
        if DirectoryExists(SearchDirectories[i]) and IsDirectoryAccessible(SearchDirectories[i]) then
          FDirectoryCache.AddOrSetValue(SearchDirectories[i],
            TDirectory.GetFiles(SearchDirectories[i], '*', TSearchOption.soAllDirectories));
      // Read all sections in the INI file.
      Ini.ReadSections(Sections);
      // Process each section that isn't part of the global configuration.
      for SectionName in Sections do
      begin
        if SameText(SectionName, 'Loader') or SameText(SectionName, 'Log') or SameText(SectionName, 'Debug') then
          Continue;
        WaitForModule := Ini.ReadString(SectionName, 'WaitForModule', '').Trim;
        WaitForProcess := Ini.ReadString(SectionName, 'WaitForProcess', '').Trim;
        DelayValue := Ini.ReadInteger(SectionName, 'Delay', 0);
        // Only add delayed tasks for modules in the ModulesToLoad list.
        if RegularModules.IndexOf(SectionName.Trim) = -1 then
        begin
          FLog.Add(Format('Ignoring delayed task for module: %s (Not in ModulesToLoad list)', [SectionName.Trim]), DEBUG, '', 4);
          Continue;
        end;
        DelayedModules.Add(SectionName.Trim);
        // Start the delayed task.
        StartDelayedTask(SectionName.Trim, WaitForModule, WaitForProcess, DelayValue);
        FLog.Add(Format('Delayed task identified: %s [WaitForModule=%s, WaitForProcess=%s, Delay=%d ms]',
          [SectionName.Trim, WaitForModule, WaitForProcess, DelayValue]), DEBUG, '', 4);
      end;
      // Remove delayed modules from the regular modules list.
      RegularModules.CaseSensitive := False;
      for var DelayedModule in DelayedModules do
      begin
        DelayedModuleFile := DelayedModule.Trim;
        for i := RegularModules.Count - 1 downto 0 do
          if SameText(RegularModules[i].Trim, DelayedModuleFile) then
          begin
            RegularModules.Delete(i);
            Break;
          end;
      end;
      ModulesToLoad := RegularModules.ToStringArray;
      // Set up the delayed task synchronization if any exist.
      if DelayedModules.Count > 0 then
      begin
        TMonitor.Enter(TaskLock);
        try
          DelayedTasksTotal := DelayedModules.Count;
          DelayedTasksCompleted := 0;
          FDelayedTaskEvent.ResetEvent;
        finally
          TMonitor.Exit(TaskLock);
        end;
      end;
      FLog.LB;
      FLog.Add(Format('Regular Modules (%d)', [RegularModules.Count]), PLAIN, '', 4);
      FLog.Add(Format('Delayed Modules (%d)', [DelayedModules.Count]), PLAIN, '', 4);
      FLog.LB;
    finally
      Sections.Free;
      DelayedModules.Free;
      RegularModules.Free;
      Ini.Free;
    end;
  except
    on E: Exception do
      FLog.Add('Error in LoadConfig: ' + E.Message, ERROR, '', 4);
  end;
end;

// Signals that a delayed task has been completed.
// When all delayed tasks are finished, the event is set.
procedure TLoader.SignalDelayedTaskCompleted;
begin
  TMonitor.Enter(TaskLock);
  try
    Inc(DelayedTasksCompleted);
    if DelayedTasksCompleted = DelayedTasksTotal then
      FDelayedTaskEvent.SetEvent;
  finally
    TMonitor.Exit(TaskLock);
  end;
end;

// Evaluates a candidate file by reading its version and file date,
// then compares it with the current best candidate to decide acceptance.
procedure TLoader.UpdateBestCandidate(const AFile: string; var BestVersion, BestFile: string;
  var BestFileDate: TDateTime; const Lock: TObject);
var
  CurrentVersion: string;
  CurrentFileDate: TDateTime;
begin
  try
    CurrentVersion := GetVersion(AFile);
    CurrentFileDate := GetFileDate(AFile);
    if CurrentFileDate = 0 then
      CurrentFileDate := Now;
    // Log file evaluation details.
    FLog.Add(Format('Evaluating file: %s [Version: %s, Date: %s]', [AFile, CurrentVersion, DateTimeToStr(CurrentFileDate)]), DEBUG, '', 16);
    TMonitor.Enter(Lock);
    try
      if (CurrentVersion = '') or (CurrentVersion = '0.0.0.0') then
      begin
        if CurrentFileDate > BestFileDate then
        begin
          BestFileDate := CurrentFileDate;
          BestFile := AFile;
          FLog.Add(Format('Accepted based on file date: %s [Date: %s]', [AFile, DateTimeToStr(CurrentFileDate)]), DEBUG, '', 16);
        end
        else
          FLog.Add(Format('Rejected: %s. Reason: Older file date', [AFile]), DEBUG, '', 16);
      end
      else
      begin
        if (BestVersion = '') or (BestVersion = '0.0.0.0') then
        begin
          BestVersion := CurrentVersion;
          BestFileDate := CurrentFileDate;
          BestFile := AFile;
          FLog.Add(Format('Accepted based on version: %s [Version: %s]', [AFile, CurrentVersion]), DEBUG, '', 16);
        end
        else if CompareVersions(CurrentVersion, BestVersion) > 0 then
        begin
          BestVersion := CurrentVersion;
          BestFileDate := CurrentFileDate;
          BestFile := AFile;
          FLog.Add(Format('Accepted based on version: %s [Version: %s]', [AFile, CurrentVersion]), DEBUG, '', 16);
        end
        else
          FLog.Add(Format('Rejected: %s. Reason: Lower or same version', [AFile]), DEBUG, '', 16);
      end;
    finally
      TMonitor.Exit(Lock);
    end;
  except
    on E: Exception do
      FLog.Add(Format('Error evaluating file %s: %s', [AFile, E.Message]), ERROR, '', 4);
  end;
end;

// Starts a delayed task for a module. This task may wait for a specific module or process before loading.
procedure TLoader.StartDelayedTask(const ModuleName, WaitForModule, WaitForProcess: string; Delay: Integer);
var
  InjectionStatus : boolean;
begin
  if FConfigPath = '' then
    raise Exception.Create('Configuration file path not specified.');
  // Run the delayed task on a background thread.
  TTask.Run(
    procedure
    var
      ThreadID: TThreadID;
      ModulePath: string;
      Injector: TInjector;
      StartTime, EndTime: TDateTime;
      // Local procedure to perform a time delay.
      procedure PerformTimeDelay;
      begin
        if Delay > 0 then
        begin
          FLog.Add(Format('Task [%d]: Delaying execution for %d ms', [ThreadID, Delay]), PLAIN, '', 4);
          TThread.Sleep(Delay);
        end;
      end;
      // Waits for a target process to appear.
      function MonitorForProcess: Boolean;
      begin
        Result := True;
        if WaitForProcess <> '' then
        begin
          FLog.Add(Format('Task [%d]: Monitoring for process %s', [ThreadID, WaitForProcess]), PLAIN, '', 4);
          repeat
            if FindProcessByName(WaitForProcess) > 0 then
            begin
              FLog.Add(Format('Task [%d]: Process detected: %s', [ThreadID, WaitForProcess]), PLAIN, '', 4);
              Break;
            end;
            TThread.Sleep(100);
          until TerminateFlag;
        end;
      end;
      // Waits for a target module to be loaded in the host process.
      function MonitorForModule: Boolean;
      begin
        Result := True;
        if WaitForModule <> '' then
        begin
          FLog.Add(Format('Task [%d]: Monitoring for module %s', [ThreadID, WaitForModule]), PLAIN, '', 4);
          repeat
            if ModuleExistsInHost(WaitForModule) then
            begin
              FLog.Add(Format('Task [%d]: Module detected: %s', [ThreadID, WaitForModule]), PLAIN, '', 4);
              Break;
            end;
            TThread.Sleep(100);
          until TerminateFlag;
        end;
      end;
    begin
      StartTime := Now;
      ThreadID := TThread.CurrentThread.ThreadID;
      FLog.Add(Format('Task [%d]: Started for %s', [ThreadID, ModuleName]), PLAIN, '', 4);
      try
        // Delay execution if required.
        PerformTimeDelay;
        // Monitor for specified process or module.
        if not MonitorForProcess then Exit;
        if not MonitorForModule then Exit;
        // Attempt to resolve the module path.
        ModulePath := FindModule(ModuleName);
        if ModulePath = '' then
          FLog.Add(Format('Task [%d]: Module path not resolved: %s', [ThreadID, ModuleName]), ERROR, '', 4)
        else
          try
            // Read the loading method from the configuration.
            case TIniFile.Create(FConfigPath).ReadInteger(ModuleName, 'Method', 1) of
              1: // Use LoadLibrary method.
                 if not LoadModule(ModulePath) then
                   FLog.Add(Format('Task [%d]: Failed to load module: %s', [ThreadID, ModulePath]), ERROR, '', 4)
                 else
                   FLog.Add(Format('Task [%d]: Successfully loaded module: %s', [ThreadID, ModuleName]), PLAIN, '', 4);
              2: // Use injection method.
                 begin
                   if TIniFile.Create(FConfigPath).ReadString(ModuleName, 'Target', '') = '' then
                     FLog.Add(Format('Task [%d]: Target process not specified for injection: %s', [ThreadID, ModuleName]), ERROR, '', 4)
                   else
                   begin
                     Injector := TInjector.Create;
                     try
                       InjectionStatus := Injector.LoadLibrary(ModulePath, TIniFile.Create(FConfigPath).ReadString(ModuleName, 'Target', ''));
                       FLog.Add('Injection method: LoadLibrary', DEBUG, '', 4);

                       if InjectionStatus then FLog.Add(Format('Task [%d]: Sucessfully injected: %s', [ThreadID, ModuleName]), PLAIN, '', 4)
                       else FLog.Add(Format('Task [%d]: LoadLibrary Injection failed...', [ThreadID]), PLAIN, '', 4);
                     finally
                       Injector.Free;
                     end;
                   end;
                 end;
            else
              FLog.Add(Format('Task [%d]: Unknown loading method for module: %s', [ThreadID, ModuleName]), ERROR, '', 4);
            end;
          except
            on E: Exception do
              FLog.Add(Format('Task [%d]: Exception loading module %s - %s', [ThreadID, ModuleName, E.Message]), ERROR, '', 4);
          end;
      finally
        EndTime := Now;
        FLog.Add(Format('Task [%d]: Completed for %s in %.2fs', [ThreadID, ModuleName, (EndTime - StartTime) * 86400]), PLAIN, '', 4);
        // Signal that this delayed task is completed.
        SignalDelayedTaskCompleted;
      end;
    end
  );
end;

// Searches for a process by name and returns its process ID.
function TLoader.FindProcessByName(const ProcessName: string): Integer;
var
  SnapShot: THandle;
  ProcessEntry: TProcessEntry32;
begin
  Result := -1;
  SnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapShot = INVALID_HANDLE_VALUE then Exit;
  try
    ProcessEntry.dwSize := SizeOf(ProcessEntry);
    if Process32First(SnapShot, ProcessEntry) then
      repeat
        if SameText(ProcessEntry.szExeFile, ProcessName) then
        begin
          Result := ProcessEntry.th32ProcessID;
          Break;
        end;
      until not Process32Next(SnapShot, ProcessEntry);
  finally
    CloseHandle(SnapShot);
  end;
end;

// Determines if a DLL is a 64-bit module.
function TLoader.IsDllX64(const FileName: string): Boolean;
var
  ModuleHandle: THandle;
  ImageNtHeaders: PImageNtHeaders;
begin
  ModuleHandle := LoadLibraryEx(PChar(FileName), 0, DONT_RESOLVE_DLL_REFERENCES);
  if ModuleHandle = 0 then RaiseLastOSError;
  try
    ImageNtHeaders := PImageNtHeaders(PByte(ModuleHandle) + PImageDosHeader(ModuleHandle)._lfanew);
    Result := ImageNtHeaders^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC;
  finally
    FreeLibrary(ModuleHandle);
  end;
end;

// Determines if the host application is running as a 64-bit process.
function TLoader.IsHostX64: Boolean;
var
  IsWow64: BOOL;
begin
  if not IsWow64Process(GetCurrentProcess, IsWow64) then
    RaiseLastOSError;
  Result := not IsWow64;
end;

// Checks if the module's architecture (x86/x64) is compatible with the host application.
function TLoader.IsCompatible(const ModulePath: string): Boolean;
var
  HostIsX64, DllIsX64: Boolean;
begin
  HostIsX64 := IsHostX64;
  DllIsX64 := IsDllX64(ModulePath);
  if HostIsX64 then
    FLog.Add('Host is x64', DEBUG, '', 8)
  else
    FLog.Add('Host is x86', DEBUG, '', 8);
  if DllIsX64 then
    FLog.Add(ExtractFileName(ModulePath) + ' is x64', DEBUG, '', 8)
  else
    FLog.Add(ExtractFileName(ModulePath) + ' is x86', DEBUG, '', 8);
  Result := HostIsX64 = DllIsX64;
end;

// Reads a configuration value from the INI file, supporting multiple data types.
function TLoader.ReadCFG(const filename, Section, Key: string; const DefaultValue: Variant): Variant;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(filename);
  try
    case VarType(DefaultValue) of
      varInteger: Result := IniFile.ReadInteger(Section, Key, DefaultValue);
      varBoolean: Result := IniFile.ReadBool(Section, Key, DefaultValue);
    else
      Result := IniFile.ReadString(Section, Key, DefaultValue);
    end;
  finally
    IniFile.Free;
  end;
end;

// Main execution method that processes immediate modules and waits for delayed tasks.
procedure TLoader.Execute;
var
  ModuleName, ModulePath: string;
  RetryCount: Integer;
  LoadedFileNames: TStringList;
  StartTime, EndTime: TDateTime;
  AllFilesLoaded: Boolean;
begin
  StartTime := Now;
  FLog.Add('Starting Module loading process...', PLAIN, '', 4);
  // Load the configuration and set up tasks.
  LoadConfig(FConfigPath);
  if Length(ModulesToLoad) > 0 then
    FLog.Add(Format('ModulesToLoad contains (%d) modules: %s', [Length(ModulesToLoad), String.Join(',', ModulesToLoad)]), DEBUG, '', 4)
  else
    FLog.Add('ModulesToLoad is empty.', DEBUG, '', 4);
  LoadedFileNames := TStringList.Create;
  try
    AllFilesLoaded := True;
    // Process each immediate module.
    for ModuleName in ModulesToLoad do
    begin
      FLog.Add('Processing Module: ' + ModuleName, PLAIN, '', 4);
      if LoadedModuleHandles.ContainsKey(ModuleName) then
      begin
        FLog.Add('Module already loaded: ' + ModuleName, DEBUG, '', 8);
        Continue;
      end;
      ModulePath := FindModule(ModuleName);
      if ModulePath = '' then
      begin
        FLog.Add(Format('Failed to locate module: %s', [ModuleName]), ERROR, '', 4);
        FailedModules.Add(ModuleName);
        AllFilesLoaded := False;
        Continue;
      end;
      RetryCount := 0;
      // Attempt to load the module with retry logic.
      while RetryCount < MaxRetries do
      begin
        if LoadModule(ModulePath) then
        begin
          LoadedFileNames.Add(ExtractFileName(ModulePath));
          Break;
        end
        else
        begin
          Inc(RetryCount);
          FLog.Add(Format('Retrying to load module: %s (Attempt %d)', [ModuleName, RetryCount]), ERROR, '', 4);
        end;
      end;
      if RetryCount = MaxRetries then
      begin
        FLog.Add(Format('Max retries reached for module: %s', [ModuleName]), ERROR, '', 4);
        FailedModules.Add(ModuleName);
        AllFilesLoaded := False;
      end;
    end;
    // If there are delayed tasks, wait until they are all completed.
    if (Length(ModulesToLoad) > 0) and (DelayedTasksTotal > 0) then
    begin
      FLog.Add('Waiting for delayed tasks to complete...', DEBUG, '', 4);
      FDelayedTaskEvent.WaitFor(INFINITE);
    end;
    FLog.LB;
    if LoadedFileNames.Count > 0 then
      FLog.Add(Format('(%d) loaded File(s): %s', [LoadedFileNames.Count, LoadedFileNames.CommaText]), PLAIN, '', 4)
    else
      FLog.Add('No files were loaded.', PLAIN, '', 4);
    if FailedModules.Count > 0 then
      FLog.Add(Format('(%d) failed File(s): %s', [FailedModules.Count, FailedModules.CommaText]), ERROR, '', 4)
    else if AllFilesLoaded then
      FLog.Add('All non-delayed files were loaded.', PLAIN, '', 4);
  finally
    LoadedFileNames.Free;
    EndTime := Now;
    FLog.Add(Format('Total time taken for loader execution: %.2f seconds', [MilliSecondsBetween(EndTime, StartTime) / 1000]), PLAIN, '', 4);
    FLog.LB;
    if DelayedTasksTotal > 0 then
    begin
      FLog.Add('Delayed Loading tasks...', PLAIN, '', 4);
      FLog.HR('-', 0, 4);
    end;
  end;
end;

initialization
  try
    // Create the chain loader and log initialization messages.
    ChainLoader := TChainLoader.Create;
    ChainLoader.FLog.LB;
    ChainLoader.FLog.Header('Chain Loader Initializing...');
    ChainLoader.FLog.HR('=');
    // Run the chain loader on a background thread.
    TTask.Run(
      procedure
      begin
        ChainLoader.Run;
      end
    );
  except
    on E: Exception do
      WriteLn('Chain Loader initialization error: ', E.Message);
  end;

finalization
  ChainLoader.Free;

end.

