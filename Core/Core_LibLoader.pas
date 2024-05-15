// Utility class LibLoader
// Intended for chain-loading DLL modules
// Author: RiiStar
// 2023

unit Core_LibLoader;

interface

uses Windows, SysUtils, Classes, inifiles, Variants, Core_Log;

type
  // Define the new class instance
  LibLoader = Class

    // These variables and methods are not visible outside this class
  private

    // Variables
    DLLmodule: Array [0 .. MAX_PATH] of Char;
    CfgFile: TIniFile;
    Files2Load: TStringList;  // List of files to load
    FilesLoaded: TStringList;  // List of loaded files

    // Private/Internal Functions
    function WhereIs(path: string; const filename: string): string;
    procedure LoadLib(dllname: PWideChar);

    // The following methods/functions/procedures and properties are all usable by instances of the class
  published
    constructor Create;
    destructor Destroy; override;
    Procedure Execute;
    function ReadCFG(const filename: string; const Section, Key: string; const DefaultValue: Variant): Variant;
    function GetDLLVersion(const DLLPath: string): string;
    function IsHostX64: Boolean;
    function IsDllX64(const FileName: string): Boolean;
  end;

const
  MODULE_NAME = 'Lib-Loader (Chain Loading)';
  LOG_FILENAME = 'Loader.log';
  CONFIG_FILENAME = 'Loader.cfg';

var
  DEBUGMSG: Boolean = TRUE;
  Success: Boolean = FALSE;
  //Files2Load, FilesLoaded: TStringList;
  Config: String;
  Log: TLog;

implementation

// Constructor : Create an instance of the class. Takes a string as argument.
// -----------------------------------------------------------------------------
constructor LibLoader.Create();
begin
  // Grab module info and path.
  GetModuleFileName(hInstance, DLLmodule, Length(DLLmodule));
  // Set Config path+file globally
  Config := ExtractFilePath(DLLmodule) + CONFIG_FILENAME;
  // Open config file for use
  CfgFile := TIniFile.Create(ExtractFilePath(DLLmodule) + CONFIG_FILENAME);

  Log := TLog.Create(LOG_FILENAME, 80, CfgFile.ReadBool('LOG', 'Append', FALSE));
  Files2Load := TStringList.Create;  // Initialize list of files to load
  FilesLoaded := TStringList.Create;  // Initialize list of loaded files

  Log.Header(MODULE_NAME);
  Log.HR('=');
  Log.Add('Initializing...', Plain, '', 4);
  Log.Add('Module: ' + ExtractFileName(DLLmodule), Plain, '', 4);
  Log.Add('Version: ' + GetDLLVersion(DLLmodule), Plain, '', 4);
  Log.Add('Config File: ' + ExtractFilePath(DLLmodule) + CONFIG_FILENAME, Plain, '', 4);

  with CfgFile do
  begin
    case CfgFile.ReadBool('DEBUG', 'Enabled', FALSE) of
      FALSE:
        begin
          DEBUGMSG := FALSE;
          Log.DEBUGEnabled := FALSE;
          Log.Add('DEBUG Output: FALSE', Plain, '', 4);
        end;
      TRUE:
        begin
          DEBUGMSG := TRUE;
          Log.DEBUGEnabled := TRUE;
          Log.Add('DEBUG Output: TRUE', Plain, '', 4);
        end;
    end;
  end;

  CfgFile.Free;
end;

destructor LibLoader.Destroy;
begin
  CfgFile.Free;  // Free config file handler
  Files2Load.Free;  // Free list of files to load
  FilesLoaded.Free;  // Free list of loaded files
  inherited Destroy;  // Call the inherited destructor
end;


function LibLoader.IsHostX64: Boolean;
var
  IsWow64: BOOL;
begin
  // Check if the host process is running in 64-bit mode
  if not IsWow64Process(GetCurrentProcess, IsWow64) then
    RaiseLastOSError;
  Result := IsWow64;
end;

function LibLoader.IsDllX64(const FileName: string): Boolean;
var
  ModuleHandle: THandle;
  ImageNtHeaders: PImageNtHeaders;
begin
  // Load the DLL file
  ModuleHandle := LoadLibraryEx(PChar(FileName), 0, DONT_RESOLVE_DLL_REFERENCES);
  if ModuleHandle = 0 then RaiseLastOSError;
  try
    // Get the NT headers of the DLL
    ImageNtHeaders := PImageNtHeaders(Integer(ModuleHandle) + PImageDosHeader(ModuleHandle)._lfanew);
    // Check if the DLL is 64-bit
    Result := ImageNtHeaders^.OptionalHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC;
  finally
    // Free the DLL
    FreeLibrary(ModuleHandle);
  end;
end;

function LibLoader.GetDLLVersion(const DLLPath: string): string;
var
  Size: DWORD;
  Buffer: Pointer;
  VerInfo: PVSFixedFileInfo;
begin
  Result := '';

  // Get the size of the version information
  Size := GetFileVersionInfoSize(PChar(DLLPath), Size);
  if Size = 0 then
    Exit;

  // Allocate memory for the version information
  GetMem(Buffer, Size);
  try
    // Get the version information
    if GetFileVersionInfo(PChar(DLLPath), 0, Size, Buffer) then
    begin
      // Get the fixed file info
      VerInfo := nil;
      VerQueryValue(Buffer, '\', Pointer(VerInfo), Size);

      // Build the version string
      if VerInfo <> nil then
      begin
        Result := Format('%d.%d.%d.%d', [HiWord(VerInfo^.dwFileVersionMS),
          LoWord(VerInfo^.dwFileVersionMS), HiWord(VerInfo^.dwFileVersionLS),
          LoWord(VerInfo^.dwFileVersionLS)]);
      end;
    end;
  finally
    FreeMem(Buffer);
  end;
end;

// Reads Config value based on Type
{
  Str  := ReadCFG('myconfig.ini', 'Section1', 'Key1', 'DefaultString');
  Int  := ReadCFG('myconfig.ini', 'Section2', 'Key2', 123);
  Bool := ReadCFG('myconfig.ini', 'Section3', 'Key3');
}
function LibLoader.ReadCFG(const filename: string; const Section, Key: string; const DefaultValue: Variant): Variant;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(filename);

  try
    case VarType(DefaultValue) of
      varInteger:
      begin
        Result := IniFile.ReadInteger(Section, Key, DefaultValue);
      end;
      varBoolean:
      begin
        Result := IniFile.ReadBool(Section, Key, DefaultValue);
      end
    else
      begin
      Result := IniFile.ReadString(Section, Key, DefaultValue);
      end;
    end;
  finally
    IniFile.Free;
  end;

end;

// ========================================================================================================================
// Recursive file search, returning location if found.
function LibLoader.WhereIs(path: string; const filename: string): string;
  // Helper function to check if a TSearchRec is a directory.
  function IsDirectory(const tsr: TSearchRec): Boolean;
  begin
    Result := (tsr.Attr and faDirectory) = faDirectory;
  end;
  // Recursive procedure to search for the file within the given path.
  function RecursiveWhereIs(const searchPath: string; const targetFile: string): string;
  var
    searchRec: TSearchRec;
    currentPath: string;
  begin
    Result := '';  // Initialize the result to an empty string.
    currentPath := IncludeTrailingPathDelimiter(searchPath);  // Ensure the path ends with a directory separator.
    // Find the first file/directory in the given path.
    if FindFirst(currentPath + '*.*', faDirectory, searchRec) = 0 then
    begin
      try
        repeat
          // If the current item matches the filename, set the result to the current path.
          if AnsiCompareText(searchRec.Name, targetFile) = 0 then
          begin
            Result := currentPath;
            Exit;
          end
          // If the current item is a directory (and not '.' or '..'), recursively search within it.
          else if IsDirectory(searchRec) and (searchRec.Name <> '.') and (searchRec.Name <> '..') then
          begin
            Result := RecursiveWhereIs(currentPath + searchRec.Name, targetFile);
            if Result <> '' then
              Exit;
          end;
        // Continue searching until no more files/directories are found.
        until FindNext(searchRec) <> 0;
      finally
        FindClose(searchRec);  // Ensure FindClose is called to release resources.
      end;
    end;
  end;
begin
  Result := RecursiveWhereIs(path, filename);  // Start the recursive search and return the result.
end;


{
// ========================================================================================================================
// Recursive file search, returning location if found.
function LibLoader.WhereIs(path: string; const filename: string): string;

  function IsDirectory(const tsr: TSearchRec): Boolean;
  begin
    Result := (tsr.Attr and faDirectory) = faDirectory;
  end;

  procedure RecursiveWhereIs(path: string; filename: string);
  var
    tsr: TSearchRec;
  begin
    path := IncludeTrailingPathDelimiter(path);
    if FindFirst(path + '*.*', faDirectory, tsr) = 0 then
    begin
      repeat
        if AnsiCompareText(tsr.Name, filename) = 0 then
          Result := path
        else if IsDirectory(tsr) and (tsr.Name[1] <> '.') then
          RecursiveWhereIs(path + tsr.Name, filename);
        if Result <> '' then
          Exit;
      until FindNext(tsr) <> 0;
      FindClose(tsr);
    end;
  end;

begin
  Result := '';
  RecursiveWhereIs(path, filename);
end;
}

// ========================================================================================================================
// Load DLL libs
procedure LibLoader.LoadLib(dllname: PWideChar);
var
  h: HMODULE;
  Index: Integer;
  ModFile: String;
  function IsDllCompatibleWithHost(const FileName: string): Boolean;
  begin
    // Check if the DLL is compatible with the host architecture
    if IsHostX64 then
      Result := IsDllX64(FileName)
    else
      Result := TRUE; // For x86 host, assume compatibility
  end;
begin
  // Check if the DLL is compatible with the host architecture
  if not IsDllCompatibleWithHost(dllname) then
  begin
    Log.Add('LoadLib failed, ' + dllname + ' is not compatible with the host architecture.', ERROR, '', 4);
    Exit;
  end;
  // Attempt to load the DLL
  h := LoadLibrary(dllname);
  ModFile := ExtractFileName(dllname);
  if h = 0 then
  begin
    // Handle the case when loading the DLL fails
    Log.Add('LoadLib failed, unable to load ' + dllname, ERROR, '', 4);
    Success := FALSE;
  end
  else
  begin
    // Handle the case when loading the DLL is successful
    Log.Add('Success, loaded ' + dllname, Custom, 'LoadLib', 4);
    Success := TRUE;
    try
      // Add to FilesLoaded if it doesn't already exist to avoid duplicates
      if FilesLoaded.IndexOf(ModFile) = -1 then
      begin
        FilesLoaded.Add(ModFile);
        Log.Add('Added ' + ModFile + ' to Files Loaded list', DEBUG, '', 6);
      end;
      Index := Files2Load.IndexOf(ModFile);
      if Index <> -1 then
      begin
        Log.Add('Removing: ' + ModFile, DEBUG, '', 6);
        Files2Load.Delete(Index);
        // Only log Files2Load.CommaText if Files2Load is not empty
        if Files2Load.Count > 0 then
          Log.Add('Amended Files2Load List: ' + Files2Load.CommaText, DEBUG, '', 6)
        else
          Log.Add('Files2Load is now empty', DEBUG, '', 6);
      end
      else
        Log.Add(ModFile + ' was not found in Files2Load list...', DEBUG, '', 6);
    except
      on E: Exception do
      begin
        Log.Add(E.ClassName + ' error raised, with message : ' + E.Message, ERROR, '', 6);
      end;
    end;
  end;
  Log.Add('Leaving LoadLib...', DEBUG, '', 6);
end;


procedure LibLoader.Execute;
var
  ModFile, ModFolder: String;
  ModFolders, FailedFiles, FilesToProcess, FilesToRemove, FilesLoaded: TStringList;
  FileLoaded: Boolean;

  // Nested procedure to load a specific file from a given folder.
  procedure LoadFileFromFolder(const Folder: String; const FileName: String; out Loaded: Boolean);
  var
    FileLoc, File2Load: String;
  begin
    Loaded := False;
    Log.Add('Recursively looking for file: ' + FileName + ' in ' + Folder + ' and sub dirs...', DEBUG, '', 4);
    FileLoc := WhereIs(Folder, FileName);  // Search for the file in the given folder.
    if FileLoc <> '' then
    begin
      File2Load := FileLoc + FileName;  // Construct the full path to the file.
      Log.Add('WhereIs returned: ' + File2Load, DEBUG, '', 4);
      if FileExists(File2Load) then  // Check if the file exists.
      begin
        Log.Add('Mod file found, loading ' + FileName + ' ...', Custom, 'LoadLib', 4);
        LoadLib(PWideChar(File2Load));  // Load the file.
        Loaded := True;
      end
      else
        Log.Add('LoadLib failed, unable to find ' + FileName + ' !', ERROR, '', 4);
    end
    else
    begin
      Log.Add('File ' + FileName + ' not found in ' + Folder, DEBUG, '', 4);
    end;
  end;

begin
  Log.Add('Lib-Loader will recursively search for specified files in listed mod directories and sub dirs...', Plain, '', 4);

  ModFolders := TStringList.Create();  // Create a TStringList for mod folders.
  FailedFiles := TStringList.Create();  // Create a TStringList for failed files.
  FilesToProcess := TStringList.Create();  // Create a TStringList for files to process.
  FilesToRemove := TStringList.Create();  // Create a TStringList for files to remove.
  FilesLoaded := TStringList.Create();  // Create a TStringList for files loaded successfully.
  try
    ModFolders.Delimiter := ',';  // Set the delimiter for mod folders.
    ModFolders.StrictDelimiter := TRUE;  // Use strict delimiter to handle paths with spaces.
    Files2Load.Delimiter := ',';  // Set the delimiter for files to load.
    Files2Load.StrictDelimiter := TRUE;  // Use strict delimiter to handle filenames with spaces.

    Log.Add('CFG file: ' + Config, DEBUG, '', 4);
    // Read and parse mod folders from the config file.
    ModFolders.DelimitedText := StringReplace(ReadCFG(Config, 'Loader', 'ModFolders', ''), ', ', ',', [rfReplaceAll]);
    Log.Add(IntToStr(ModFolders.Count) + ' mod directories listed in config.', DEBUG, '', 4);
    Log.Add('Mod directory list in CFG: ' + ModFolders.DelimitedText, DEBUG, '', 4);

    // Read and parse files to load from the config file.
    Files2Load.DelimitedText := StringReplace(ReadCFG(Config, 'Loader', 'Files', ''), ', ', ',', [rfReplaceAll]);
    Log.Add('CFG -> Files2Load list: ' + Files2Load.DelimitedText, DEBUG, '', 4);
    FilesToProcess.Assign(Files2Load);  // Make a copy of the files to load list.

    if FilesToProcess.Count > 0 then  // If there are mod folders specified.
    begin
      Log.LB;
      Log.Header('Searching specified directories and sub dirs', 4);
      Log.HR('-', 0, 4);

      for ModFile in FilesToProcess do
      begin
        FileLoaded := False;

        // Iterate through each mod folder to search for the current file.
        for ModFolder in ModFolders do
        begin
          Log.Add('Search Folder: ' + ModFolder, DEBUG, '', 4);
          LoadFileFromFolder(ModFolder, ModFile, FileLoaded);  // Load the current file from the current mod folder.

          // If the file was loaded, add to FilesToRemove and exit the folder loop.
          if FileLoaded then
          begin
            FilesToRemove.Add(ModFile);
            if FilesLoaded.IndexOf(ModFile) = -1 then  // Prevent duplicates
              FilesLoaded.Add(ModFile);  // Track the successfully loaded file.
            Break;
          end;
        end;

        // If the file was not found in any folder, add to FailedFiles and log an appropriate message.
        if not FileLoaded then
        begin
          if FailedFiles.IndexOf(ModFile) = -1 then
          begin
            Log.Add('File ' + ModFile + ' not found in any specified directories.', ERROR, '', 4);
            FailedFiles.Add(ModFile);
          end;
        end;
      end;

      // Remove the files from Files2Load that were successfully loaded.
      for ModFile in FilesToRemove do
      begin
        if Files2Load.IndexOf(ModFile) <> -1 then
          Files2Load.Delete(Files2Load.IndexOf(ModFile));
      end;
    end
    else
    begin
      ModFolder := GetCurrentDir;  // Default to the current directory if no mod folders are specified.
      Log.Add('Mod Folder(s): None specified, defaulting to application/client dir.', Plain, '', 4);
    end;

    Log.Add('Recursive directory search completed.', Custom, 'LoadLib', 4);

    if Files2Load.Count > 0 then  // If there are still files to load, search in the application/client directory.
    begin
      Log.LB;
      Log.Header('Search in application/client dir', 4);
      Log.HR('-', 0, 4);

      ModFolder := GetCurrentDir;  // Get the current directory.
      Log.Add('Application/Client Folder: ' + ModFolder, Plain, '', 4);
      Log.Add('Lib-Loader will recursively search for specified files in ' + ModFolder + ' and sub dirs...', Plain, '', 4);
      Log.Add('Search folder: ' + ModFolder, DEBUG, '', 4);

      FilesToRemove.Clear;  // Clear the list of files to remove for the new directory search.

      for ModFile in Files2Load do
      begin
        FileLoaded := False;
        LoadFileFromFolder(ModFolder, ModFile, FileLoaded);  // Load the current file from the application/client directory.

        // If the file was loaded, add to FilesToRemove and remove from FailedFiles if present.
        if FileLoaded then
        begin
          FilesToRemove.Add(ModFile);
          if FilesLoaded.IndexOf(ModFile) = -1 then  // Prevent duplicates
            FilesLoaded.Add(ModFile);  // Track the successfully loaded file.
          if FailedFiles.IndexOf(ModFile) <> -1 then
            FailedFiles.Delete(FailedFiles.IndexOf(ModFile));
        end
        else
        begin
          // Ensure error is logged for files not found in application/client dir
          if FailedFiles.IndexOf(ModFile) = -1 then
          begin
            FailedFiles.Add(ModFile);
          end;

          Log.Add('File ' + ModFile + ' not found in the application/client directory.', ERROR, '', 4);
        end;
      end;

      // Remove the files from Files2Load that were successfully loaded.
      for ModFile in FilesToRemove do
      begin
        if Files2Load.IndexOf(ModFile) <> -1 then
          Files2Load.Delete(Files2Load.IndexOf(ModFile));
      end;
    end;

    Log.LB;
    Log.HR;
    if FilesLoaded.Count > 0 then
      Log.Add('Loaded File(s) list: ' + FilesLoaded.CommaText, Plain, '', 4)
    else
      Log.Add('No files were loaded.', Plain, '', 4);

    // Log the status of the remaining unloaded files.
    if FailedFiles.Count > 0 then
    begin
      Log.Add('(' + IntToStr(FailedFiles.Count) + ') Remaining unloaded files: ' + FailedFiles.CommaText, Plain, '', 4);
      Log.Add('Not all specified files were loaded.', ERROR, '', 4);
    end
    else
      Log.Add('All specified files loaded!', Plain, '', 4);

    Log.Add('Operation completed.', Plain, '', 4);
  finally
    ModFolders.Free;  // Free the TStringList for mod folders.
    FilesToProcess.Free;  // Free the TStringList for files to process.
    FilesToRemove.Free;  // Free the TStringList for files to remove.
    FilesLoaded.Free;  // Free the TStringList for files loaded successfully.
    FailedFiles.Free;  // Free the TStringList for failed files.
  end;
end;



// ========================================================================================================================
// All code below is excuted when this module is loaded according to compile order
var
  LibLoading: LibLoader;

initialization

LibLoading := LibLoader.Create();

Log.LB;
Log.Header('Chain Loader Initializing...');
Log.HR('=');

if LibLoading.ReadCFG(Config, 'Loader', 'Enabled', FALSE) and (LibLoading.ReadCFG(Config, 'Loader', 'Files', '') <> '') then
begin
  Log.Add('Begin Loader Execution...', Plain, '', 4);
  LibLoading.Execute;
end
else
begin
  if LibLoading.ReadCFG(Config, 'Loader', 'Files', '') = '' then

  Log.Add('No files specified.', Custom, 'Notice', 4);
  Log.Add('LoadLib (Chain loading) skipped.', Custom, 'Notice', 4);
  Log.Free;

  LibLoading.Free;
end;

// ========================================================================================================================
// All code below is excuted when this module is unloaded according to compile order
finalization

Log.Free;
LibLoading.Free;

end.
