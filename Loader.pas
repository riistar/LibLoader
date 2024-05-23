unit Loader;

interface

uses
  System.SysUtils, System.Classes, System.IniFiles, System.IOUtils, System.Types, System.Variants, Winapi.Windows,
  System.Generics.Collections, Log;

type
  TLoader = class
  private
    ConfigPath: string; // Path to the configuration file
    SearchDirectories: TArray<string>; // Directories to search for modules
    ModulesToLoad: TArray<string>; // List of module names to load
    LoadedModules: TStringList; // List of successfully loaded modules
    FailedModules: TStringList; // List of modules that failed to load
    LoadedModuleHandles: TDictionary<string, HMODULE>; // Dictionary to store module handles
    MaxRetries: Integer; // Maximum number of retries for loading a module
    HostAppDirectory: string; // Directory of the host application

    procedure LoadConfig(const ConfigPath: string); // Load configuration from file
    function FindModule(const ModuleName: string; SearchAppDir: Boolean = False): string; // Find the module in specified directories
    function LoadModule(const ModulePath: string): Boolean; // Load the module
    function IsCompatible(const ModulePath: string): Boolean; // Check if the module is compatible with the host architecture
    function GetVersion(const DLLPath: string): string; // Get the version of the module
    function GetHostAppDirectory: string; // Get the directory of the host application
    function IsHostX64: Boolean; // Check if the host application is 64-bit
    function IsDllX64(const FileName: string): Boolean; // Check if the DLL is 64-bit
  public
    constructor Create; // Constructor
    destructor Destroy; override; // Destructor
    procedure Execute; // Execute the loading process
    function ReadCFG(const filename: string; const Section, Key: string; const DefaultValue: Variant): Variant; // Read configuration values
  end;

const
  MODULE_NAME = 'Lib-Loader (Chain Loading)';
  LOG_FILENAME = 'Loader.log';
  CONFIG_FILENAME = 'Loader.cfg';

var
  DEBUGMSG: Boolean = TRUE; // Global debug message flag
  Success: Boolean = FALSE; // Global success flag
  Config: String; // Global configuration path
  LibLoader: TLoader; // Global instance of the loader
  Log: TLog; // Global log instance

implementation

{ TLoader }

constructor TLoader.Create;
var
  DLLmodule: array[0..MAX_PATH] of Char;
  CfgFile: TIniFile;
begin
  // Get the module file name of the current DLL
  GetModuleFileName(hInstance, DLLmodule, Length(DLLmodule));
  // Set the configuration path globally
  Config := ExtractFilePath(DLLmodule) + CONFIG_FILENAME;
  // Get the host application directory
  HostAppDirectory := GetHostAppDirectory;

  // Open the configuration file
  CfgFile := TIniFile.Create(Config);
  try
    // Initialize the log with settings from the configuration file
    Log := TLog.Create(LOG_FILENAME, 80, CfgFile.ReadBool('LOG', 'Append', FALSE));
    LoadedModules := TStringList.Create;
    FailedModules := TStringList.Create;
    LoadedModuleHandles := TDictionary<string, HMODULE>.Create;
    MaxRetries := 3;

    // Log initialization details
    Log.Header(MODULE_NAME);
    Log.HR('=');
    Log.Add('Initializing...', PLAIN, '', 4);
    Log.Add('Module: ' + ExtractFileName(DLLmodule), PLAIN, '', 4);
    Log.Add('Version: ' + GetVersion(DLLmodule), PLAIN, '', 4);
    Log.Add('Config File: ' + Config, PLAIN, '', 4);

    // Read debug settings from the configuration file
    DEBUGMSG := CfgFile.ReadBool('DEBUG', 'Enabled', FALSE);
    Log.DebugEnabled := DEBUGMSG;
    Log.Add('DEBUG Output: ' + BoolToStr(DEBUGMSG, True), PLAIN, '', 4);
  finally
    CfgFile.Free;
  end;
end;

destructor TLoader.Destroy;
begin
  // Free resources
  LoadedModules.Free;
  FailedModules.Free;
  LoadedModuleHandles.Free;
  Log.Free;
  inherited;
end;

function TLoader.GetHostAppDirectory: string;
var
  HostAppModule: array[0..MAX_PATH] of Char;
begin
  // Get the module file name of the host application
  GetModuleFileName(0, HostAppModule, Length(HostAppModule));
  Result := ExtractFilePath(HostAppModule);
end;

function TLoader.GetVersion(const DLLPath: string): string;
var
  Size: DWORD;
  Handle: DWORD;
  Buffer: Pointer;
  VerInfo: PVSFixedFileInfo;
  VerInfoSize: UINT;
begin
  Result := '';
  // Get the size of the version information
  Size := GetFileVersionInfoSize(PChar(DLLPath), Handle);
  if Size = 0 then Exit;

  // Allocate memory for the version information
  GetMem(Buffer, Size);
  try
    // Get the version information
    if GetFileVersionInfo(PChar(DLLPath), 0, Size, Buffer) then
    begin
      // Retrieve the fixed file info
      if VerQueryValue(Buffer, '\', Pointer(VerInfo), VerInfoSize) then
      begin
        // Format the version string
        Result := Format('%d.%d.%d.%d', [
          HiWord(VerInfo^.dwFileVersionMS), LoWord(VerInfo^.dwFileVersionMS),
          HiWord(VerInfo^.dwFileVersionLS), LoWord(VerInfo^.dwFileVersionLS)]);
      end;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

procedure TLoader.LoadConfig(const ConfigPath: string);
var
  Ini: TIniFile;
begin
  // Open the configuration file
  Ini := TIniFile.Create(ConfigPath);
  try
    // Read directories and files to load from the configuration file
    SearchDirectories := Ini.ReadString('Loader', 'ModFolders', '').Split([',']);
    for var i := 0 to Length(SearchDirectories) - 1 do
      SearchDirectories[i] := SearchDirectories[i].Trim;

    ModulesToLoad := Ini.ReadString('Loader', 'Files', '').Split([',']);
    for var i := 0 to Length(ModulesToLoad) - 1 do
      ModulesToLoad[i] := ModulesToLoad[i].Trim;

    // Log configuration details
    Log.Add('CFG file: ' + ConfigPath, DEBUG, '', 4);
    Log.Add(IntToStr(Length(SearchDirectories)) + ' mod directories listed in config.', DEBUG, '', 4);
    Log.Add('Mod directory list in CFG: ' + String.Join(',', SearchDirectories), DEBUG, '', 4);
    Log.Add('CFG -> Files2Load list: ' + String.Join(',', ModulesToLoad), DEBUG, '', 4);
  finally
    Ini.Free;
  end;
end;

function TLoader.FindModule(const ModuleName: string; SearchAppDir: Boolean = False): string;
var
  Directory: string;
  FoundFiles: TStringDynArray;
  BestFile: string;
  BestVersion, CurrentVersion: string;
  FoundFilesList: TStringList;
begin
  // Initialize the best version to a very low value and best file to an empty string
  BestVersion := '0.0.0.0';
  BestFile := '';
  FoundFilesList := TStringList.Create;

  try
    // If not searching the application directory, perform the recursive search
    if not SearchAppDir then
    begin
      Log.Add('Recursive search for Module: ' + ModuleName, DEBUG, '', 8);

      // Loop through each search directory
      for Directory in SearchDirectories do
      begin
        Log.Add('Searching in directory: ' + Directory, DEBUG, '', 12);

        // Get all files matching the module name in the directory and its subdirectories
        FoundFiles := TDirectory.GetFiles(Directory, ModuleName, TSearchOption.soAllDirectories);

        // If files are found, add them to the list and check their versions
        if Length(FoundFiles) > 0 then
        begin
          FoundFilesList.AddStrings(FoundFiles);

          // Loop through each found file to determine the newest version
          for var FilePath in FoundFiles do
          begin
            CurrentVersion := GetVersion(FilePath);

            // If the current file's version is newer, update the best version and file
            if CompareStr(CurrentVersion, BestVersion) > 0 then
            begin
              BestVersion := CurrentVersion;
              BestFile := FilePath;
            end;
          end;
        end;
      end;

      // If multiple versions of the module were found, log the details
      if FoundFilesList.Count > 1 then
      begin
        Log.Add('Found multiple versions of module ' + ModuleName + ':', DEBUG, '', 12);
        for var FilePath in FoundFilesList do
        begin
          Log.Add(FilePath, DEBUG, '', 16);
        end;
        Log.Add('Loading newest: ' + BestFile + ' [Version: ' + BestVersion + ']', DEBUG, '', 12);
      end;
    end;

    // If no suitable file was found in the specified directories
    if BestFile = '' then
    begin
      // If not already searching the application directory, attempt to search there
      if not SearchAppDir then
      begin
        BestFile := IncludeTrailingPathDelimiter(HostAppDirectory) + ModuleName;
        Log.Add('Module not found in specified directories, falling back to application directory.', DEBUG, '', 12);

        // Recursively call FindModule to search in the application directory
        Result := FindModule(ModuleName, True);
        Exit;
      end
      else
      begin
        // Check if the file exists in the application directory
        BestFile := IncludeTrailingPathDelimiter(HostAppDirectory) + ModuleName;
        if FileExists(BestFile) then
        begin
          Log.Add('Module found in application directory: ' + BestFile, DEBUG, '', 12);
        end
        else
        begin
          // Log an error if the file is not found in the application directory
          Log.Add('File ' + ModuleName + ' not found in the application/client directory.', ERROR, '', 4);
        end;
      end;
    end
    else
    begin
      // Log the directory where the module was found and the module to be loaded
      Log.Add('Module found in ' + ExtractFilePath(BestFile), DEBUG, '', 12);
      Log.Add('Module to load: ' + BestFile, DEBUG, '', 12);
    end;
  finally
    // Free the list of found files
    FoundFilesList.Free;
  end;

  // Return the path of the best file found
  Result := BestFile;
end;

function TLoader.LoadModule(const ModulePath: string): Boolean;
var
  ModuleHandle: HMODULE;
begin
  Result := False; // Initialize result as False indicating failure to load

  // Check if the module is compatible with the host architecture
  if not IsCompatible(ModulePath) then
  begin
    Log.Add('LoadLib failed, ' + ExtractFileName(ModulePath) + ' is not compatible with the host architecture.', ERROR, '', 4);
    Exit; // Exit if the module is not compatible
  end;

  // Attempt to load the module
  ModuleHandle := LoadLibrary(PChar(ModulePath));

  // Check if the module was successfully loaded
  if ModuleHandle <> 0 then
  begin
    // Add the module handle to the dictionary
    LoadedModuleHandles.Add(ExtractFileName(ModulePath), ModuleHandle);
    // Add the module path to the list of loaded modules
    LoadedModules.Add(ModulePath);
    // Log the success message
    Log.Add('Success, loaded ' + ModulePath, CUSTOM, 'LoadLib', 4);
    Result := True; // Set result as True indicating success
  end
  else
  begin
    // Log the failure message with the error
    Log.Add('Failed to load Module: ' + ModulePath + ' Error: ' + SysErrorMessage(GetLastError), ERROR, '', 4);
  end;
end;


function TLoader.IsCompatible(const ModulePath: string): Boolean;
var
  HostIsX64, DllIsX64: Boolean;
begin
  // Determine if the host application is 64-bit
  HostIsX64 := IsHostX64;
  // Determine if the DLL is 64-bit
  DllIsX64 := IsDllX64(ModulePath);

  // Log the architecture of the host application
  case HostIsX64 of
    True: Log.Add('Host is x64', DEBUG, '', 8);
    False: Log.Add('Host is x86', DEBUG, '', 8);
  end;

  // Log the architecture of the DLL
  case DllIsX64 of
    True: Log.Add('DLL ' + ExtractFileName(ModulePath) + ' is x64', DEBUG, '', 8);
    False: Log.Add('DLL ' + ExtractFileName(ModulePath) + ' is x86', DEBUG, '', 8);
  end;

  // Check if the architectures match
  Result := HostIsX64 = DllIsX64;
end;


function TLoader.IsHostX64: Boolean;
var
  IsWow64: BOOL;
begin
  // Determine if the current process is running under WOW64 (Windows 32-bit on Windows 64-bit)
  if not IsWow64Process(GetCurrentProcess, IsWow64) then
    RaiseLastOSError;

  // If IsWow64 is FALSE, the host is x64; if TRUE, the host is x86 on a x64 OS
  Result := not IsWow64;
end;

function TLoader.IsDllX64(const FileName: string): Boolean;
var
  ModuleHandle: THandle;
  ImageNtHeaders: PImageNtHeaders;
begin
  // Load the DLL without resolving references
  ModuleHandle := LoadLibraryEx(PChar(FileName), 0, DONT_RESOLVE_DLL_REFERENCES);
  if ModuleHandle = 0 then RaiseLastOSError;

  try
    // Get the NT headers of the DLL
    ImageNtHeaders := PImageNtHeaders(PByte(ModuleHandle) + PImageDosHeader(ModuleHandle)._lfanew);

    // Check if the DLL is 64-bit
    Result := ImageNtHeaders^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC;
  finally
    // Free the DLL
    FreeLibrary(ModuleHandle);
  end;
end;


function TLoader.ReadCFG(const filename: string; const Section, Key: string; const DefaultValue: Variant): Variant;
var
  IniFile: TIniFile;
begin
  // Create an instance of TIniFile to read the configuration file
  IniFile := TIniFile.Create(filename);
  try
    // Determine the type of the default value to decide how to read the configuration value
    case VarType(DefaultValue) of
      varInteger:
        // If the default value is an integer, read the configuration value as an integer
        Result := IniFile.ReadInteger(Section, Key, DefaultValue);
      varBoolean:
        // If the default value is a boolean, read the configuration value as a boolean
        Result := IniFile.ReadBool(Section, Key, DefaultValue);
    else
      // For all other types (e.g., string), read the configuration value as a string
      Result := IniFile.ReadString(Section, Key, DefaultValue);
    end;
  finally
    // Free the TIniFile instance to release the file handle and resources
    IniFile.Free;
  end;
end;


procedure TLoader.Execute;
var
  ModuleName, ModulePath: string;
  RetryCount: Integer;
  LoadedFileNames: TStringList;
begin
  // Log the start of the module loading process
  Log.Add('Starting Module loading process...', PLAIN, '', 4);

  // Load configuration from the config file
  LoadConfig(Config);

  // Add a line break and header for the search process
  Log.LB;
  Log.Header('Searching specified directories and sub dirs', 4);
  Log.HR('-', 0, 4);

  // Initialize a list to hold the names of loaded files
  LoadedFileNames := TStringList.Create;
  try
    // Loop through each module name specified in the configuration
    for ModuleName in ModulesToLoad do
    begin
      Log.Add('Processing Module: ' + ModuleName, PLAIN, '', 4);

      // Check if the module has already been loaded
      if LoadedModuleHandles.ContainsKey(ModuleName) then
      begin
        Log.Add('Module already loaded: ' + ModuleName, DEBUG, '', 8);
        Continue;
      end;

      // Find the module in the specified directories
      ModulePath := FindModule(ModuleName);

      // If the module is not found, log an error and add it to the failed modules list
      if not FileExists(ModulePath) then
      begin
        Log.Add('File ' + ModuleName + ' not found in specified directories.', ERROR, '', 4);
        FailedModules.Add(ModuleName);
        Continue;
      end;

      // Attempt to load the module with retries
      RetryCount := 0;
      while RetryCount < MaxRetries do
      begin
        if LoadModule(ModulePath) then
        begin
          // If successful, add the file name to the loaded file names list
          LoadedFileNames.Add(ExtractFileName(ModulePath));
          Break; // Exit retry loop
        end
        else
        begin
          // Increment retry count and log retry attempt
          Inc(RetryCount);
          Log.Add('Retrying to load Module: ' + ModuleName + ' (Attempt ' + IntToStr(RetryCount) + ')', ERROR, '', 4);
        end;
      end;

      // If maximum retries reached, log an error and add to the failed modules list
      if RetryCount = MaxRetries then
      begin
        Log.Add('Max retries reached for Module: ' + ModuleName, ERROR, '', 4);
        FailedModules.Add(ModuleName);
      end;
    end;

    // If there are any failed modules, attempt to load them from the application directory
    if FailedModules.Count > 0 then
    begin
      Log.LB;
      Log.Header('Search in application/client dir', 4);
      Log.HR('-', 0, 4);
      Log.Add('Application/Client Folder: ' + HostAppDirectory, PLAIN, '', 4);
      Log.Add('Lib-Loader will recursively search for remaining files in ' + HostAppDirectory + ' and sub dirs...', PLAIN, '', 4);

      // Loop through each failed module
      for ModuleName in FailedModules do
      begin
        ModulePath := IncludeTrailingPathDelimiter(HostAppDirectory) + ModuleName;

        // If the module is not found in the application directory, log an error
        if not FileExists(ModulePath) then
        begin
          Log.Add('File ' + ModuleName + ' not found in the application/client directory.', ERROR, '', 4);
          Continue;
        end;

        // Attempt to load the module with retries from the application directory
        RetryCount := 0;
        while RetryCount < MaxRetries do
        begin
          if LoadModule(ModulePath) then
          begin
            // If successful, add the file name to the loaded file names list
            LoadedFileNames.Add(ExtractFileName(ModulePath));
            Break; // Exit retry loop
          end
          else
          begin
            // Increment retry count and log retry attempt
            Inc(RetryCount);
            Log.Add('Retrying to load Module: ' + ModuleName + ' (Attempt ' + IntToStr(RetryCount) + ')', ERROR, '', 4);
          end;
        end;

        // If maximum retries reached, log an error
        if RetryCount = MaxRetries then
        begin
          Log.Add('Max retries reached for Module: ' + ModuleName, ERROR, '', 4);
        end;
      end;
    end;

    // Log the summary of loaded and failed modules
    Log.LB;
    Log.HR;
    if LoadedFileNames.Count > 0 then
      Log.Add('(' + IntToStr(LoadedFileNames.Count) + ') loaded File(s): ' + LoadedFileNames.CommaText, PLAIN, '', 4)
    else
      Log.Add('No files were loaded.', PLAIN, '', 4);

    if FailedModules.Count > 0 then
      Log.Add('(' + IntToStr(FailedModules.Count) + ') unloaded File(s): ' + FailedModules.CommaText, ERROR, '', 4)
    else
      Log.Add('All specified files were loaded.', PLAIN, '', 4);

    Log.Add('Operation completed.', PLAIN, '', 4);
  finally
    // Free the loaded file names list
    LoadedFileNames.Free;
  end;
end;


initialization
// Initialize the log and loader instance at the start
Log := TLog.Create(LOG_FILENAME, 100, False);
LibLoader := TLoader.Create;
Log.LB;
Log.Header('Chain Loader Initializing...');
Log.HR('=');

// Execute the loader if enabled in the configuration file
if LibLoader.ReadCFG(Config, 'Loader', 'Enabled', False) and (LibLoader.ReadCFG(Config, 'Loader', 'Files', '') <> '') then
begin
  Log.Add('Begin Loader Execution...', PLAIN, '', 4);
  LibLoader.Execute;
end
else
begin
  if LibLoader.ReadCFG(Config, 'Loader', 'Files', '') = '' then
  begin
    Log.Add('No files specified.', CUSTOM, 'Notice', 4);
    Log.Add('LoadLib (Chain loading) skipped.', CUSTOM, 'Notice', 4);
  end;
  Log.Free;
  LibLoader.Free;
end;

finalization
// Free the log and loader instance when the module is unloaded
Log.Free;
LibLoader.Free;

end.

