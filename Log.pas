// ----------------------------------------------------------------------------
// Log Unit
// ----------------------------------------------------------------------------
// Author: RiiStar
// Year: 2023
//
// This unit provides a logging functionality to write log messages to a file. It supports features such as customizable log levels, headers,
// horizontal rules, line breaks, and thread-safe logging in a threaded or non-threaded manner.
// The log messages are written to a specified log file with an optional maximum line width to wrap long messages.
//
// Usage:
// 1. Create an instance of the TLog class by providing the log file path and an optional maximum line width.
//
//    Example:
//    Log := TLog.Create('path/to/logfile.txt', 80);
//
// 2. (Optional) Set a header by calling the SetHeader method with an array of THeaderLine records.
//    Each THeaderLine record contains the text and the indentation level of the header line.
//
//    Example:
//    Log.SetHeader([
//      THeaderLine.Create('Log Header', 0),
//      THeaderLine.Create('Sub Header', 2)
//    ]);
//
// 3. Add log messages using the Add method, specifying the message text, the log level (default is Plain), and the indentation level (default is 0).
//
//    Example:
//    Log.Add('This is a plain log message');
//    Log.Add('An error occurred', Error);
//    Log.Add('This is a custom log message', Custom, 'CustomLevel');
//    Log.Add('This is a debug message', Debug);
//
//    Output in log file:
//    [20/07/2023 05:37pm] This is a plain log message
//    [20/07/2023 05:37pm] [Error] An error occurred
//    [20/07/2023 05:37pm] [Custom] This is a custom log message
//    [20/07/2023 05:37pm] [Debug] This is a debug message
//
// 4. Add horizontal rules using the HorizontalRule method, specifying the rule character (default is '-') and the line width (default is the maximum line width set during initialization).
//    Example:
//    Log.HorizontalRule('*');
//    Log.HorizontalRule('#', 40);
//
//    Output in log file:
//    [20/07/2023 05:37pm] **************************************************************
//    [20/07/2023 05:37pm]
//    [20/07/2023 05:37pm] ########################################
//
// 5. Add line breaks using the LineBreak method.
//    Example:
//    Log.LineBreak;
//
//    Output in log file:
//    [20/07/2023 05:37pm]
//
// 6. (Optional) Use the HR and LB aliases for HorizontalRule and LineBreak methods for a more compact syntax.
//    Example:
//    Log.HR('-');
//    Log.LB;
//
//    Output in log file:
//    [20/07/2023 05:37pm] ----------------------------------------
//
// 7. (Optional) Set the Threaded property to True to enable threaded logging.
//    Example:
//    Log.Threaded := True;
//
// 8. Destroy the TLog instance to flush the remaining log entries and release resources.
//    Example:
//    Log.Free();
//
// Note: When the Threaded property is set to True, logging is performed in a separate background thread. If the Threaded property is set to False, the log entries are processed immediately in the current thread, potentially blocking the execution until the log operation is complete.
//
// ----------------------------------------------------------------------------

{
To-Do

Allow setting the level of output - Simple / Debug ?
Toggle for Debug or no Debug messages
}

unit Log;

interface

uses
  Windows, SysUtils, Classes, SyncObjs, System.StrUtils; // Include SyncObjs for TCriticalSection

type
  TLogLevel = (Plain, Error, Custom, Debug);

  TLog = class
  private
    FLogFilePath: string;
    FMaxLineWidth: Integer;
    FDebugEnabled: Boolean;
    FDebugToOutput: Boolean;
    FLock: TCriticalSection; // Synchronization object for thread safety

    procedure AddLogEntry(const LogEntry: string);
    function GenerateHorizontalRule(const RuleChar: Char; LineWidth: Integer; Indentation: Integer): string;
  public
    constructor Create(const LogFilePath: string; MaxLineWidth: Integer = 0; AppendToLog: Boolean = FALSE; DebugMode: Integer = 1);
    destructor Destroy; override;

    procedure Header(const Text: string; const Indentation: Integer = 0);
    procedure Add(const Message: string; const Level: TLogLevel = Plain; const CustomLevel: string = ''; const Indentation: Integer = 0);
    procedure HorizontalRule(const RuleChar: Char = '-'; const LineWidth: Integer = -1; const Indentation: Integer = 0);
    procedure HR(const RuleChar: Char = '-'; const LineWidth: Integer = -1; const Indentation: Integer = 0);
    procedure LineBreak;
    procedure LB;

    property MaxLineWidth: Integer read FMaxLineWidth write FMaxLineWidth;
    property DebugEnabled: Boolean read FDebugEnabled write FDebugEnabled;
  end;

implementation

{ TLog }

constructor TLog.Create(const LogFilePath: string; MaxLineWidth: Integer = 0; AppendToLog: Boolean = FALSE; DebugMode: Integer = 1);
begin
  FLogFilePath := LogFilePath;
  FMaxLineWidth := MaxLineWidth;
  FDebugEnabled := False;

  FDebugToOutput := DebugMode = 2; // Enable OutputDebugString only if Mode = 2

  FLock := TCriticalSection.Create; // Initialize the critical section

  if not AppendToLog then
  begin
    if FileExists(FLogFilePath) then
      DeleteFile(FLogFilePath); // Wipe old log file
    Exit;
  end;

  LineBreak; // Separate appended log entries
end;

destructor TLog.Destroy;
begin
  FLock.Free; // Free the critical section
  LineBreak; // Write an empty line at the end
  inherited;
end;

procedure TLog.Header(const Text: string; const Indentation: Integer = 0);
begin
  Add(Text, Plain, '', Indentation);
end;

procedure TLog.Add(const Message: string; const Level: TLogLevel = Plain; const CustomLevel: string = ''; const Indentation: Integer = 0);
var
  LogEntry: string;
  LevelTag: string;
  DashCount: Integer;
begin
  LogEntry := Format('[%s]', [FormatDateTime('dd/mm/yyyy hh:nnampm', Now)]);

  if Indentation > 0 then
    LogEntry := LogEntry + StringOfChar(' ', Indentation);

  case Level of
    Error:
      LevelTag := '[Error]';
    Custom:
      LevelTag := Format('[%s]', [CustomLevel]);
    Debug:
      if FDebugEnabled then
      begin
        LevelTag := '[Debug]';
        DashCount := Indentation div 3;
        if DashCount > 1 then
          LevelTag := StringOfChar('-', DashCount) + ' ' + LevelTag;
      end
      else
        Exit; // Skip Debug level if DebugEnabled is False
  else
    LevelTag := '';
  end;

  case IndexStr(LevelTag, ['', '[Debug]']) of
    0: // Empty LevelTag
      LogEntry := LogEntry + ' ' + Message;
    1: // [Debug]
    begin
      LogEntry := LogEntry + ' ' + LevelTag + ' ' + Message;
    end
  else
    // Default case for other LevelTag values
    LogEntry := LogEntry + ' ' + LevelTag + ' ' + Message;
  end;

  if (FDebugToOutput) then OutputDebugString(PChar(LogEntry)); // Conditional debug output
  AddLogEntry(LogEntry);
end;

procedure TLog.HorizontalRule(const RuleChar: Char = '-'; const LineWidth: Integer = -1; const Indentation: Integer = 0);
var
  Rule: string;
begin
  if LineWidth < 1 then
    Rule := GenerateHorizontalRule(RuleChar, FMaxLineWidth, Indentation)
  else
    Rule := GenerateHorizontalRule(RuleChar, LineWidth, Indentation);

  Add(Rule, Plain, '', 0);
end;

procedure TLog.HR(const RuleChar: Char = '-'; const LineWidth: Integer = -1; const Indentation: Integer = 0);
begin
  HorizontalRule(RuleChar, LineWidth, Indentation);
end;

procedure TLog.LineBreak;
begin
  Add('', Plain, '', 0);
end;

procedure TLog.LB;
begin
  LineBreak;
end;

procedure TLog.AddLogEntry(const LogEntry: string);
var
  LogFile: TextFile;
begin
  FLock.Enter; // Ensure thread-safe access
  try
    AssignFile(LogFile, FLogFilePath);
    if FileExists(FLogFilePath) then
      Append(LogFile)
    else
      Rewrite(LogFile);

    try
      Writeln(LogFile, LogEntry);
    finally
      CloseFile(LogFile);
    end;
  finally
    FLock.Leave;
  end;
end;

function TLog.GenerateHorizontalRule(const RuleChar: Char; LineWidth: Integer; Indentation: Integer): string;
begin
  Result := StringOfChar(RuleChar, LineWidth);
  if Indentation > 0 then
    Result := StringOfChar(' ', Indentation) + Result;
end;

end.

