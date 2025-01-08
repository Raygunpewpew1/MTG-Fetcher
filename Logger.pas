unit Logger;

interface

uses
  APIConstants, System.Classes, System.IOUtils, System.SyncObjs,
  System.SysUtils;

procedure LogStuff(const Msg: string);

implementation

var
  LogCriticalSection: TCriticalSection;
  LogFilePath: string;

const
  MaxLogFileSize = 1024 * 1024; // Example: 1 MB
  TrimmedLogLines = 1000; // Number of lines to keep after trimming

procedure EnsureLogDirectory;
begin
{$IF DEFINED(ANDROID)}
  // Use TPath.GetDocumentsPath for internal storage
  LogFilePath := TPath.Combine(TPath.TPath.GetDocumentsPath, 'mtgfetch.txt');
{$ELSEIF DEFINED(MSWINDOWS)}
  LogFilePath := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
  if not TDirectory.Exists(LogFilePath) then
    TDirectory.CreateDirectory(LogFilePath);
  LogFilePath := TPath.Combine(LogFilePath, 'mtgfetch.log');
{$ELSE}
  LogFilePath := 'mtgfetch.log'; // Default for other platforms
{$ENDIF}
  // Create the log file if it doesn't exist
  if not TFile.Exists(LogFilePath) then
  begin
    try
      TFile.WriteAllText(LogFilePath, '', TEncoding.UTF8);
    except
      on E: Exception do
        Writeln('Failed to create log file: ' + E.Message);
    end;
  end;
end;

procedure TrimLogFile;
var
  AllLines: TArray<string>;
  TrimmedLines: TArray<string>;
begin
  try
    if not TFile.Exists(LogFilePath) then
      Exit;

    // Read all lines from the file
    AllLines := TFile.ReadAllLines(LogFilePath, TEncoding.UTF8);

    if Length(AllLines) > TrimmedLogLines then
    begin
      // Keep only the last `TrimmedLogLines`
      TrimmedLines := Copy(AllLines, Length(AllLines) - TrimmedLogLines,
        TrimmedLogLines);

      // Write the trimmed lines back to the file
      TFile.WriteAllLines(LogFilePath, TrimmedLines, TEncoding.UTF8);
    end;
  except
    on E: Exception do
      Writeln('Error trimming log file: ' + E.Message);
    // Log the error to console
  end;
end;

procedure LogStuff(const Msg: string);
var
  LogEntry: string;
begin
  if not Assigned(LogCriticalSection) then
    Exit;

  LogCriticalSection.Enter;
  try
    try
      // Format the log entry
      LogEntry := Format('[%s] [Thread %d] %s',
        [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
        TThread.CurrentThread.ThreadID, Msg]);

      // Check log file size and trim if necessary
      if TFile.Exists(LogFilePath) and
        (TFile.GetSize(LogFilePath) > MaxLogFileSize) then
        TrimLogFile;

      // Append the log entry
      TFile.AppendAllText(LogFilePath, LogEntry + sLineBreak, TEncoding.UTF8);
    except
      on E: Exception do
        Writeln('Error writing to log file: ' + E.Message);
      // Optional: log to console
    end;
  finally
    LogCriticalSection.Leave;
  end;
end;

initialization

LogCriticalSection := TCriticalSection.Create;
EnsureLogDirectory;

finalization

LogCriticalSection.Free;

end.
