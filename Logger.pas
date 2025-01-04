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
{$ENDIF}
end;

procedure LogStuff(const Msg: string);
var
  LogFile: TStreamWriter;
begin
  if Assigned(LogCriticalSection) then
  begin
    LogCriticalSection.Enter;
    try
      try
        LogFile := TStreamWriter.Create(LogFilePath, True, TEncoding.UTF8);
        try
          LogFile.WriteLine(Format('[%s] [Thread %d] %s',
            [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), TThread.CurrentThread.ThreadID, Msg]));
        finally
          LogFile.Free;
        end;
      except
        // Silent failure
      end;
    finally
      LogCriticalSection.Leave;
    end;
  end;
end;

initialization
  LogCriticalSection := TCriticalSection.Create;
  EnsureLogDirectory;

finalization
  LogCriticalSection.Free;

end.
