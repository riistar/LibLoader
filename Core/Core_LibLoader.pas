// Utility class LibLoader
// Intended for chain-loading DLL modules
// Author: RiiStar
// 2023

unit Core_LibLoader;

interface

uses Windows, SysUtils, Classes, inifiles, Variants;

type
  // Define the new class instance
  LibLoader = Class

    // These variables and methods are not visible outside this class
  private

    // Variables
    Locale: TFormatSettings;
    DLLmodule: Array [0 .. MAX_PATH] of Char;
    CfgFile: TIniFile;

    // Private/Internal Functions
    function WhereIs(path: string; const filename: string): string;
    procedure LoadLib(dllname: PWideChar);

    // The following methods/functions/procedures and properties are all usable by instances of the class
  published
    constructor Create;
    // Called when creating an instance (object) from this class
    Procedure Execute;
    Procedure WriteLog(Data: string; Enabled: Boolean = TRUE);
    function ReadCFG(const filename: string; const Section, Key: string;
      const DefaultValue: Variant): Variant;
    function GetDLLVersion(const DLLPath: string): string;
  end;

const
  MODULE_NAME = 'Lib-Loader (Chain Loading)';
  LOG_FILENAME = 'Loader.log';
  CONFIG_FILENAME = 'Loader.cfg';

var
  DEBUG: Boolean = TRUE;
  Success: Boolean = FALSE;
  Files2Load, FilesLoaded: TStringList;
  Config: String;

implementation

// Constructor : Create an instance of the class. Takes a string as argument.
// -----------------------------------------------------------------------------
constructor LibLoader.Create();
begin
  DeleteFile(LOG_FILENAME);
  FilesLoaded := TStringList.Create();

  WriteLog(MODULE_NAME);
  WriteLog('====================================================================================================================');
  WriteLog('    Initializing...');

  GetModuleFileName(hInstance, DLLmodule, Length(DLLmodule));

  WriteLog('    Module: ' + ExtractFileName(DLLmodule));
  WriteLog('    Version: ' + GetDLLVersion(DLLmodule));
  WriteLog('    Config File: ' + ExtractFilePath(DLLmodule) + CONFIG_FILENAME);

  // Set Config path+file globally
  Config := ExtractFilePath(DLLmodule) + CONFIG_FILENAME;

  CfgFile := TIniFile.Create(ExtractFilePath(DLLmodule) + CONFIG_FILENAME);

  with CfgFile do
  begin
    case CfgFile.ReadBool('Debug', 'Enabled', FALSE) of
      FALSE:
        begin
          DEBUG := FALSE;
          WriteLog('    Debug Output: FALSE');
        end;
      TRUE:
        begin
          DEBUG := TRUE;
          WriteLog('    Debug Output: TRUE');
        end;
    end;
  end;

  CfgFile.Free;
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

Procedure LibLoader.WriteLog(Data: string; Enabled: Boolean = TRUE);
var
  LogFile: TextFile;
  formattedDateTime: string;
begin
  IF Enabled = TRUE THEN
  Begin
    AssignFile(LogFile, LOG_FILENAME);

    IF FileExists(LOG_FILENAME) <> TRUE THEN
      Rewrite(LogFile)
    ELSE
      Append(LogFile);
    GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, Locale);
    DateTimeToString(formattedDateTime, Locale.ShortDateFormat +
      ' hh:nnampm', now);
    WriteLn(LogFile, '[' + formattedDateTime + '] ' + Data);
    CloseFile(LogFile);
  end;
end;

// Reads Config value based on Type
{
  Str  := ReadCFG('myconfig.ini', 'Section1', 'Key1', 'DefaultString');
  Int  := ReadCFG('myconfig.ini', 'Section2', 'Key2', 123);
  Bool := ReadCFG('myconfig.ini', 'Section3', 'Key3');
}
function LibLoader.ReadCFG(const filename: string; const Section, Key: string;
  const DefaultValue: Variant): Variant;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(filename);
  try
    case VarType(DefaultValue) of
      varInteger:
        Result := IniFile.ReadInteger(Section, Key, DefaultValue);
      varBoolean:
        Result := IniFile.ReadBool(Section, Key, DefaultValue);
    else
      Result := IniFile.ReadString(Section, Key, DefaultValue);
    end;
  finally
    IniFile.Free;
  end;
end;

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

// ========================================================================================================================
// Load DLL libs
procedure LibLoader.LoadLib(dllname: PWideChar);
var
  h: HMODULE;
  Index: Integer;
  ModFile: String;
begin
  if FileExists(dllname) then
  begin
    h := LoadLibrary(dllname);
    ModFile := ExtractFileName(dllname);
    WriteLog('    -- [Debug] LoadLibrary returned: ' + IntToStr(h), DEBUG);

    if h = 0 then
    begin;
      WriteLog(Format('    [Error] %s', ['LoadLib failed, unable to load ' +
        dllname]), TRUE);
      Success := FALSE;
    end
    else
    begin
      WriteLog(Format('    [LoadLib] %s',
        ['Success, loaded ' + dllname]), TRUE);
      Success := TRUE;

      FilesLoaded.Add(ExtractFileName(dllname));
      WriteLog('    -- [Debug] Added ' + ExtractFileName(dllname) +
        ' to Files Loaded list', DEBUG);
      WriteLog('    -- [Debug] Updated Files Loaded list: ' +
        FilesLoaded.CommaText, DEBUG);

      Index := Files2Load.IndexOf(ModFile);
      WriteLog('    -- [Debug] Get ' + ModFile +
        ' file index in File2Load list: ' + IntToStr(Index), DEBUG);

      Try
        if Index <> -1 then
        begin
          WriteLog('    -- [Debug] Removing: ' + ModFile, DEBUG);
          ModFile := Files2Load.ValueFromIndex[0];
          Files2Load.Delete(Index);
          WriteLog('    -- [Debug] Amended Files2Load List: ' +
            Files2Load.CommaText, DEBUG);
        end
        else
          WriteLog('    -- [Debug] Index of Files2Load was not removed...',
            DEBUG);

      except
        on E: Exception do
        begin
          WriteLog('[Error] ' + E.ClassName + ' error raised, with message : ' +
            E.Message);
          WriteLog('    -- [Debug] Failed? Amended Files2Load List: ' +
            Files2Load.CommaText, DEBUG);
        end;
      end;
    end;

  end
  else
  begin
    WriteLog('    -- [Debug] File ' + dllname +
      ' not found at this location...', DEBUG);
    WriteLog(Format('    [Error] %s', ['LoadLib failed, unable to load ' +
      dllname]), TRUE);
    Success := FALSE;
  end;

end;

Procedure LibLoader.Execute;
var
  File2Load, FileLoc, ModFile, ModFolder: String;
  ModFolders, ModFiles: TStringList;
  FilesChecked, TotalFiles: Integer;
Label
  LoadMod, LoadSMod;
begin

  WriteLog('    Lib-Loader will recursively search for specified files in listed mod directories and sub dirs...');

  // -------------------------------------------------------------------------------------------
  // Multi Mod dir

  ModFolders := TStringList.Create();
  Files2Load := TStringList.Create();
  ModFiles := TStringList.Create();

  // Define delimited text for Mod Folders from Config file. Avoids strings breaking when parsed and path has spaces between text.
  ModFolders.Delimiter := ',';
  ModFolders.StrictDelimiter := TRUE;
  // Define delimited text for Mod Files from Config file. Avoids strings breaking when parsed and filename has spaces between text.
  Files2Load.Delimiter := ',';
  Files2Load.StrictDelimiter := TRUE;

  try
    // Get Mod Folders from config and parse each folder-string without breaking if there are spaces in the string item
    ModFolders.DelimitedText :=
      StringReplace(ReadCFG(Config, 'Loader', 'ModFolders', ''), ', ', ',',
      [rfReplaceAll]);

    WriteLog('    [Debug] ' + IntToStr(ModFolders.Count) +
      ' mod directories listed in config.', DEBUG);
    WriteLog('    [Debug] Mod directory list in CFG: ' +
      ModFolders.DelimitedText, DEBUG);

    // Load Mod file list from config CFG and parse each file-string without breaking if there are spaces in the string item
    Files2Load.DelimitedText := StringReplace(ReadCFG(Config, 'Loader', 'Files',
      ''), ', ', ',', [rfReplaceAll]);
    WriteLog('    [Debug] CFG -> Files2Load list: ' +
      Files2Load.DelimitedText, DEBUG);

    ModFiles := Files2Load;

    if ModFolders.Count <> 0 then
    begin

      WriteLog('');
      WriteLog('    Searching specified directories and sub dirs');
      WriteLog('    ----------------------------------------------------------------------------------------------------------------');

      try

        FilesChecked := 0;
        TotalFiles := Files2Load.Count;

      LoadMod:

        for ModFolder in ModFolders do
        begin
          WriteLog('    [Debug] Search Folder: ' + ModFolder, DEBUG);
          // WriteLog('    -- Search Folder: '+ModFolder);
          // WriteLog('    -- Looking for file: '+ModFile+' in '+ModFolder+' and sub dirs...');

          for ModFile in ModFiles do
          begin

            WriteLog('    ---- [Debug] Files2Load List: ' +
              Files2Load.CommaText, DEBUG);
            WriteLog('    [Debug] Recursively Looking for file: ' + ModFile +
              ' in ' + ModFolder + ' and sub dirs...', DEBUG);

            FileLoc := WhereIs(ModFolder, ModFile);
            if FileLoc <> '' then
              File2Load := FileLoc + ModFile
            else
              FileLoc := 'File not found...';

            WriteLog('    [Debug] Running WhereIs(' + ModFolder + '\' + ModFile
              + '): ' + FileLoc, DEBUG);

            if (FileLoc <> '') or (FileLoc <> 'File not found...') then
            begin
              File2Load := FileLoc + ModFile;
              WriteLog('    [Debug] WhereIs returned: ' + File2Load, DEBUG);

              if FileExists(File2Load) then
              begin
                WriteLog(Format('    [LoadLib] %s',
                  ['Mod file found, loading ' + ModFile + ' ...']), TRUE);
                LoadLib(PWideChar(File2Load));
              end
              else
                WriteLog(Format('    [Error] %s',
                  ['LoadLib failed, unable to find ' + ModFile + ' !']), TRUE);
            end;

            Inc(FilesChecked);

          end;

          WriteLog('    [Debug] Loaded File list: ' +
            FilesLoaded.CommaText, DEBUG);
          WriteLog('    [Debug] Files Checked: ' +
            IntToStr(FilesChecked), DEBUG);

        end;

        if FilesChecked <> TotalFiles then
          Goto LoadMod;

      finally
        WriteLog('    -- [Debug] Remaining unloaded files (' +
          IntToStr(Files2Load.Count) + '): ' + Files2Load.CommaText, DEBUG);
        WriteLog('    [LoadLib] Recursive directory search has completed...');
      end;

      if Files2Load.Count > 0 then
        WriteLog('    Files remaining (' + IntToStr(Files2Load.Count) + '): ' +
          Files2Load.CommaText);

    end
    else
    begin
      ModFolder := GetCurrentDir;
      WriteLog('    Mod Folder(s): None specified, defaulting to application/client dir.');
    end;

  finally
    ModFolders.Free;
    FileLoc := '';
    File2Load := '';
  end;


  // -----------------------------------------------------------------------------------------------------
  // Default to application/client dir for any mod files that were not found using specified directories

  if Files2Load.Count > 0 then
  begin

    ModFiles := Files2Load;

    WriteLog('');
    WriteLog('    Search in application/client dir');
    WriteLog('    ----------------------------------------------------------------------------------------------------------------');

    try

      ModFolder := GetCurrentDir;

      if ModFolder <> '' then
      begin
        WriteLog('    Application/Client Folder: ' + ModFolder);
        WriteLog('    Lib-Loader will recursively search for specified files in '
          + ModFolder + ' and sub dirs...')
      end
      else
      begin
        WriteLog('    Mod Folder: None specified, defaulting to application/client dir.');
        WriteLog('    Lib-Loader will recursively search for specified files in the application dir and sub dirs...');
      end;

      WriteLog('    [Debug] Search folder: ' + ModFolder, DEBUG);
      WriteLog('    [Debug] Files2Load list: ' + Files2Load.CommaText, DEBUG);

      FilesChecked := 0;
      TotalFiles := Files2Load.Count;
    LoadSMod:

      for ModFile in Files2Load do
      begin

        WriteLog('    ---- [Debug] Files2Load List: ' +
          Files2Load.CommaText, DEBUG);
        WriteLog('    [Debug] Recursively Looking for file: ' + ModFile + ' in '
          + ModFolder + ' and sub dirs...', DEBUG);

        FileLoc := WhereIs(ModFolder, ModFile);
        if FileLoc <> '' then
          File2Load := FileLoc + ModFile
        else
          FileLoc := 'File not found...';

        WriteLog('    [Debug] Running WhereIs(' + ModFolder + '\' + ModFile +
          '): ' + FileLoc, DEBUG);

        if (FileLoc <> '') or (FileLoc <> 'File not found...') then
        begin
          File2Load := FileLoc + ModFile;
          WriteLog('    [Debug] WhereIs returned: ' + File2Load, DEBUG);

          if FileExists(File2Load) then
          begin
            WriteLog(Format('    [LoadLib] %s',
              ['Mod file found, loading ' + ModFile + ' ...']), TRUE);
            LoadLib(PWideChar(File2Load));
          end
          else
            WriteLog(Format('    [Error] %s',
              ['LoadLib failed, unable to find ' + ModFile + ' !']), TRUE);
        end;

        Inc(FilesChecked);

      end;

      if FilesChecked <> TotalFiles then
        Goto LoadSMod;

    finally

    end;

  end;

  WriteLog('');
  WriteLog('    ----------------------------------------------------------------------------------------------------------------');
  WriteLog('    Loaded File(s) list: ' + FilesLoaded.CommaText);

  if Files2Load.Count > 0 then
    WriteLog('    (' + IntToStr(Files2Load.Count) +
      ') Remaining unloaded files: ' + Files2Load.CommaText)
  else
    WriteLog('    All specified files loaded!');

  WriteLog('    Operation completed.');

  Files2Load.Free;
  FilesLoaded.Free;

end;

// ========================================================================================================================
// All code below is excuted when this module is loaded according to compile order
var
  LibLoading: LibLoader;

initialization

LibLoading := LibLoader.Create();

LibLoading.WriteLog('');
LibLoading.WriteLog('Chain Loader Initializing...');
LibLoading.WriteLog('====================================================================================================================');

if LibLoading.ReadCFG(Config, 'Loader', 'Enabled', FALSE) and
  (LibLoading.ReadCFG(Config, 'Loader', 'Files', '') <> '') then
begin
  LibLoading.WriteLog('    Executing...');
  LibLoading.Execute;
end
else
begin
  if LibLoading.ReadCFG(Config, 'Loader', 'Files', '') = '' then
    LibLoading.WriteLog('    [Notice] No files specified.');
  LibLoading.WriteLog('    [Notice] LoadLib (Chain loading) skipped.');
  LibLoading.Free;
end;

// ========================================================================================================================
// All code below is excuted when this module is unloaded according to compile order
finalization

LibLoading.Free;

end.
