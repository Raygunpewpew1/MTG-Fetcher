unit Logger;

interface

uses
  APIConstants, System.Classes, System.IOUtils, System.SyncObjs, System.SysUtils;

procedure LogStuff(const Msg: string);

implementation

var
  LogCriticalSection: TCriticalSection;
  LogFilePath: string;


procedure EnsureLogDirectory;
var
  LogInit: string;
begin
{$IF DEFINED(ANDROID)}
  LogFilePath := TPath.Combine(TPath.GetHomePath, 'mtgfetch.log');
{$ELSEIF DEFINED(MSWINDOWS)}
  LogInit := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
  if not TDirectory.Exists(LogInit) then
    TDirectory.CreateDirectory(LogInit);
  LogFilePath := TPath.Combine(LogInit, 'mtgfetch.log');
{$ELSE}
  // Default log path for other platforms
  LogFilePath := 'mtgfetch.log';
{$ENDIF}
end;


procedure TrimLogFile;
var
  AllLines: TArray<string>;
  TrimmedLines: TArray<string>;
  LogFile: TStringList;
  TotalLines, StartLine: Integer;
begin
  try
    if not TFile.Exists(LogFilePath) then
      Exit;


    AllLines := TFile.ReadAllLines(LogFilePath, TEncoding.UTF8);

    TotalLines := Length(AllLines);
    if TotalLines <= TrimmedLogLines then
      Exit;

    // Determine the starting line to retain
    StartLine := TotalLines - TrimmedLogLines;
    if StartLine < 0 then
      StartLine := 0;

    // Extract the lines to retain
    TrimmedLines := Copy(AllLines, StartLine, TrimmedLogLines);

    // Write the trimmed lines back to the log file
    LogFile := TStringList.Create;
    try
      LogFile.AddStrings(TrimmedLines);
      LogFile.SaveToFile(LogFilePath, TEncoding.UTF8);
    finally
      LogFile.Free;
    end;
  except
    on E: Exception do
    begin
      // If trimming fails, silently ignore to prevent application crash
    end;
  end;
end;

procedure RotateLogFile;
var
  NewLogFilePath: string;
begin
  try
    if TFile.Exists(LogFilePath) then
    begin
      NewLogFilePath := Format('%s_%s.log', [ChangeFileExt(LogFilePath, ''), FormatDateTime('yyyymmdd_hhnnss', Now)]);
      TFile.Move(LogFilePath, NewLogFilePath);
    end;
  except
    on E: Exception do
    begin
      // Handle errors during log rotation
    end;
  end;
end;

procedure LogStuff(const Msg: string);
var
  LogFile: TStringList;
  LogEntry: string;
  IsTrimming: Boolean;
begin
  if not Assigned(LogCriticalSection) then
    Exit; // Logger not initialized

  LogCriticalSection.Enter;
  try
    try
      // Determine if this is a trimming log to prevent recursion
      IsTrimming := Pos('Trimming the log file', Msg) > 0;

      // Check if the log file exists and exceeds the maximum size, but skip if already trimming
      if not IsTrimming and TFile.Exists(LogFilePath) then
      begin
        if TFile.GetSize(LogFilePath) > MaxLogFileSize then
        begin
          // Log the trimming action without triggering further trimming
          LogFile := TStringList.Create;
          try
            LogEntry := Format('[%s] [Thread %d] %s',
              [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), TThread.CurrentThread.ThreadID, 'Log file size exceeded. Trimming the log file.']);

            // Append the trimming log entry
            LogFile.LoadFromFile(LogFilePath, TEncoding.UTF8);
            LogFile.Add(LogEntry);
            LogFile.SaveToFile(LogFilePath, TEncoding.UTF8);

            // Perform the trimming
            TrimLogFile;
          finally
            LogFile.Free;
          end;
        end;
      end;


      LogFile := TStringList.Create;
      try

        LogEntry := Format('[%s] [Thread %d] %s',
          [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), TThread.CurrentThread.ThreadID, Msg]);

        // Append the log entry
        LogFile.LoadFromFile(LogFilePath, TEncoding.UTF8);
        LogFile.Add(LogEntry);
        LogFile.SaveToFile(LogFilePath, TEncoding.UTF8);
      finally
        LogFile.Free;
      end;
    except
      on E: Exception do
      begin
        // Silent failure or implement alternative logging (e.g., fallback to console)
      end;
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
