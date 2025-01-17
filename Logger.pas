unit Logger;

interface


{$IFDEF ANDROID}
uses
  APIConstants, System.Classes, System.IOUtils, System.SyncObjs,
  System.SysUtils, System.Generics.Collections, Androidapi.Log;
{$ENDIF}
{$IFDEF MSWINDOWS}
uses
  APIConstants, System.Classes, System.IOUtils, System.SyncObjs,
  System.SysUtils, System.Generics.Collections;
{$ENDIF}



type
  TLogLevel = (DEBUG, INFO, WARNING, ERROR);

procedure LogStuff(const Msg: string; Level: TLogLevel = INFO);

implementation

var
  LogCriticalSection: TCriticalSection;
  LogFilePath: string;
  LogQueue: TQueue<string>;
  AsyncLogThread: TThread;
  TerminateLogging: Boolean;

const
  MaxLogFileSize = 1024 * 1024; // Example: 1 MB
  TrimmedLogLines = 1000; // Number of lines to keep after trimming

function LogLevelToString(Level: TLogLevel): string;
begin
  case Level of
    DEBUG: Result := 'DEBUG';
    INFO: Result := 'INFO';
    WARNING: Result := 'WARNING';
    ERROR: Result := 'ERROR';
  else
    Result := 'UNKNOWN';
  end;
end;

procedure EnsureLogDirectory;
begin
{$IF DEFINED(ANDROID)}
  LogFilePath := TPath.Combine(TPath.GetDocumentsPath, 'mtgfetch.txt');
{$ELSEIF DEFINED(MSWINDOWS)}
  LogFilePath := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
  if not TDirectory.Exists(LogFilePath) then
    TDirectory.CreateDirectory(LogFilePath);
  LogFilePath := TPath.Combine(LogFilePath, 'mtgfetch.log');
{$ELSE}
  LogFilePath := 'mtgfetch.log';
{$ENDIF}
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

    AllLines := TFile.ReadAllLines(LogFilePath, TEncoding.UTF8);

    if Length(AllLines) > TrimmedLogLines then
    begin
      TrimmedLines := Copy(AllLines, Length(AllLines) - TrimmedLogLines,
        TrimmedLogLines);
      TFile.WriteAllLines(LogFilePath, TrimmedLines, TEncoding.UTF8);
    end;
  except
    on E: Exception do
      Writeln('Error trimming log file: ' + E.Message);
  end;
end;

procedure AsyncLogProcess;
begin
  while not TerminateLogging do
  begin
    LogCriticalSection.Enter;
    try
      while LogQueue.Count > 0 do
      begin
        try
          TFile.AppendAllText(LogFilePath, LogQueue.Dequeue + sLineBreak, TEncoding.UTF8);
        except
          on E: Exception do
            Writeln('Error writing to log file: ' + E.Message);
        end;
      end;
    finally
      LogCriticalSection.Leave;
    end;
    TThread.Sleep(50); // Adjust as needed for performance
  end;
end;

procedure LogStuff(const Msg: string; Level: TLogLevel);
var
  LogEntry: string;
begin



  if not Assigned(LogCriticalSection) then
    Exit;

  LogEntry := Format('[%s] [%s] [Thread %d] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), LogLevelToString(Level),
    TThread.CurrentThread.ThreadID, Msg]);
  {$IFDEF ANDROID}
  __android_log_write(ANDROID_LOG_DEBUG, 'MTG-Fetcher', PAnsiChar(AnsiString(LogEntry)));
  {$ENDIF}

  LogCriticalSection.Enter;
  try
    if TFile.Exists(LogFilePath) and
      (TFile.GetSize(LogFilePath) > MaxLogFileSize) then
      TrimLogFile;

    LogQueue.Enqueue(LogEntry);
  finally
    LogCriticalSection.Leave;
  end;
end;

initialization

LogCriticalSection := TCriticalSection.Create;
LogQueue := TQueue<string>.Create;
EnsureLogDirectory;
TerminateLogging := False;
AsyncLogThread := TThread.CreateAnonymousThread(AsyncLogProcess);
AsyncLogThread.FreeOnTerminate := False;
AsyncLogThread.Start;

finalization

TerminateLogging := True;
AsyncLogThread.Terminate;
AsyncLogThread.WaitFor;
FreeAndNil(AsyncLogThread);
FreeAndNil(LogQueue);
FreeAndNil(LogCriticalSection);

end.

