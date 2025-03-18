unit Utils;

interface

uses System.SysUtils, System.Types, System.Variants, Winapi.Windows, System.Generics.Collections,
     System.DateUtils, Log, tlhelp32, Winapi.PsAPI, Injector, SyncObjs;

type
  TArrayUtils = class
  public
    class function Contains(const Arr: TArray<string>; const Value: string): Boolean; static;
  end;

function IsDirectoryAccessible(const Directory: string): Boolean;
function CompareVersions(const Version1, Version2: string): Integer;

implementation

class function TArrayUtils.Contains(const Arr: TArray<string>; const Value: string): Boolean;
var
  Item: string;
begin
  for Item in Arr do
  begin
    if SameText(Item, Value) then
      Exit(True);
  end;
  Result := False;
end;

// --------------------------
// Utility Functions
// --------------------------

// Checks whether a given directory is accessible (by trying to create and delete a temp file).
function IsDirectoryAccessible(const Directory: string): Boolean;
var
  TestFile: string;
  FileHandle: THandle;
begin
  Result := False;
  TestFile := IncludeTrailingPathDelimiter(Directory) + 'test.tmp';
  FileHandle := CreateFile(PChar(TestFile), GENERIC_WRITE, 0, nil,
    CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY, 0);
  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := True;
    CloseHandle(FileHandle);
    DeleteFile(PChar(TestFile));
  end
  else
  begin
    // If directory access fails, output a debug string similar to the original log message.
    OutputDebugString(PChar(Format('Directory not accessible: %s. Error: %s', [Directory, SysErrorMessage(GetLastError)])));
  end;
end;

// Compares two version strings by parsing major, minor, build, and revision numbers.
function CompareVersions(const Version1, Version2: string): Integer;
var
  Major1, Minor1, Build1, Revision1: Integer;
  Major2, Minor2, Build2, Revision2: Integer;
  Parts1, Parts2: TArray<string>;
begin
  // Initialize version parts to 0.
  Major1 := 0; Minor1 := 0; Build1 := 0; Revision1 := 0;
  Major2 := 0; Minor2 := 0; Build2 := 0; Revision2 := 0;
  Parts1 := Version1.Split(['.']);
  Parts2 := Version2.Split(['.']);
  if Length(Parts1) >= 1 then TryStrToInt(Parts1[0], Major1);
  if Length(Parts1) >= 2 then TryStrToInt(Parts1[1], Minor1);
  if Length(Parts1) >= 3 then TryStrToInt(Parts1[2], Build1);
  if Length(Parts1) >= 4 then TryStrToInt(Parts1[3], Revision1);
  if Length(Parts2) >= 1 then TryStrToInt(Parts2[0], Major2);
  if Length(Parts2) >= 2 then TryStrToInt(Parts2[1], Minor2);
  if Length(Parts2) >= 3 then TryStrToInt(Parts2[2], Build2);
  if Length(Parts2) >= 4 then TryStrToInt(Parts2[3], Revision2);
  // Compare version parts sequentially.
  if Major1 <> Major2 then Exit(Major1 - Major2);
  if Minor1 <> Minor2 then Exit(Minor1 - Minor2);
  if Build1 <> Build2 then Exit(Build1 - Build2);
  Result := Revision1 - Revision2;
end;

end.

