unit LoaderManager;

interface

uses
  System.SysUtils, Log;

type
  // TChainLoader encapsulates the global state and manages the loader instance.
  // Note: We do not reference TLoader here to avoid circular references.
  TChainLoader = class
  private
  public
    FLoader: TObject;  // Will be a TLoader instance at runtime.
    FLog: TLog;
    FConfig: string;
    FDebugEnabled: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Run; // Starts the loader execution process.
  end;

implementation

uses
  Loader; // Now we use the full Loader unit in the implementation.

{ TChainLoader }

constructor TChainLoader.Create;
var
  AppDir: string;
begin
  // Determine the application directory and build the config path.
  AppDir := ExtractFilePath(ParamStr(0));
  FConfig := AppDir + 'Loader.cfg';

  // Create the logger.
  FLog := TLog.Create('Loader.log', 100, False, 1);
  FDebugEnabled := FLog.DebugEnabled; // Optionally read debug flag.

  // Create the loader instance (casting to TObject in the interface).
  FLoader := TLoader.Create;
end;

destructor TChainLoader.Destroy;
begin
  if Assigned(FLoader) then
    TLoader(FLoader).Free;
  FLog.Free;
  inherited;
end;

procedure TChainLoader.Run;
begin
  // Check configuration via the loader instance.
  if TLoader(FLoader).ReadCFG(FConfig, 'Loader', 'Enabled', False) and
     (TLoader(FLoader).ReadCFG(FConfig, 'Loader', 'Files', '') <> '') then
  begin
    FLog.Add('Begin Loader Execution...', PLAIN, '', 4);
    TLoader(FLoader).Execute;
  end
  else
  begin
    FLog.Add('No files specified or loader disabled.', CUSTOM, 'Notice', 4);
    FLog.Add('LoadLib (Chain loading) skipped.', CUSTOM, 'Notice', 4);
  end;
end;

end.

