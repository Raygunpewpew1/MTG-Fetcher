unit Logger;

interface
uses
System.SyncObjs, System.Classes, System.IOUtils, System.SysUtils;

procedure LogStuff(const Msg: string);

implementation

var
  LogCriticalSection: TCriticalSection;

procedure LogStuff(const Msg: string);
var
  LogFilePath: string;
  LogFile: TStreamWriter;
begin
  LogCriticalSection.Enter;
  try
    try
      {$IF DEFINED(ANDROID)}
      LogFilePath := TPath.Combine(TPath.GetSharedDownloadsPath, 'application_log.txt');
      {$ELSEIF DEFINED(MSWINDOWS)}
      LogFilePath := TPath.Combine(TPath.GetAppPath, 'application_log.txt');
      {$ENDIF}

      LogFile := TStreamWriter.Create(LogFilePath, True, TEncoding.UTF8);
      try
        LogFile.WriteLine(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Msg]));
      finally
        LogFile.Free;
      end;
    except
      // If logging fails, do nothing to avoid crashing the app.
    end;
  finally
    LogCriticalSection.Leave;
  end;
end;

initialization
  LogCriticalSection := TCriticalSection.Create;

finalization
  LogCriticalSection.Free;

end.
