unit ScryfallChunkedReader;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Diagnostics, Logger, Math;

type
  TScryfallStreamReader = class
  private
    FStream: TStream;
    FReader: TStreamReader;
    FBuffer: TStringBuilder;
    FChunkSize: Integer;
    FTotalSize: Int64;
    FCurrentPosition: Int64;
    FOnProgress: TProc<Int64, Int64>;

    procedure ReportProgress;
    function ReadUntilCompleteObject: string;
  public
    constructor Create(AStream: TStream; AChunkSize: Integer = 8192);
    destructor Destroy; override;

    function ReadNextCard: TJSONObject;

    property OnProgress: TProc<Int64, Int64> read FOnProgress write FOnProgress;
    property CurrentPosition: Int64 read FCurrentPosition;
    property TotalSize: Int64 read FTotalSize;
  end;

procedure ProcessScryfallBulkFile(const FileName: string);

implementation

{ TScryfallStreamReader }

constructor TScryfallStreamReader.Create(AStream: TStream; AChunkSize: Integer);
begin
  inherited Create;
  FStream := AStream;
  FReader := TStreamReader.Create(FStream, TEncoding.UTF8, True, AChunkSize);
  FBuffer := TStringBuilder.Create;
  FChunkSize := AChunkSize;
  FTotalSize := FStream.Size;
  FCurrentPosition := 0;
end;

destructor TScryfallStreamReader.Destroy;
begin
  FReader.Free;
  FBuffer.Free;
  inherited;
end;

procedure TScryfallStreamReader.ReportProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(FCurrentPosition, FTotalSize);
end;

function TScryfallStreamReader.ReadUntilCompleteObject: string;
var
  Line: string;
  BracketCount: Integer;
  InString: Boolean;
  EscapeNext: Boolean;
  I: Integer;
  CurrentChar: Char;
begin
  Result := '';
  BracketCount := 0;
  InString := False;
  EscapeNext := False;
  FBuffer.Clear;

  while not FReader.EndOfStream do
  begin
    Line := FReader.ReadLine;
    FCurrentPosition := FStream.Position;
    ReportProgress;

    for I := 1 to Line.Length do
    begin
      CurrentChar := Line[I];

      if EscapeNext then
      begin
        EscapeNext := False;
      end
      else
      begin
        case CurrentChar of
          '"': InString := not InString;
          '\': if InString then EscapeNext := True;
          '{': if not InString then Inc(BracketCount);
          '}':
            if not InString then
            begin
              Dec(BracketCount);
              if BracketCount = 0 then
              begin
                FBuffer.Append(CurrentChar);
                Exit(FBuffer.ToString);
              end;
            end;
        end;
      end;

      FBuffer.Append(CurrentChar);
    end;
  end;

  if BracketCount <> 0 then
    raise Exception.Create('Incomplete JSON object in stream.');
end;

function TScryfallStreamReader.ReadNextCard: TJSONObject;
var
  JsonStr: string;
  JsonValue: TJSONValue;
  JsonArray: TJSONArray;
  JsonObject: TJSONObject;
begin
  Result := nil;

  // Read a chunk of data
  JsonStr := ReadUntilCompleteObject;

  // Check for empty or invalid data
  if JsonStr.Trim = '' then
    Exit;

  try
    // Parse the JSON string into a TJSONValue
    JsonValue := TJSONObject.ParseJSONValue(JsonStr);
    try
      if JsonValue = nil then
        raise Exception.Create('Failed to parse JSON: Null or invalid format.');

      if JsonValue is TJSONArray then
      begin
        // Handle JSON arrays by returning the first object
        JsonArray := JsonValue as TJSONArray;
        if JsonArray.Count > 0 then
        begin
          JsonObject := JsonArray.Items[0] as TJSONObject;
          Result := TJSONObject(JsonObject.Clone); // Return a clone to avoid memory issues
        end
        else
          raise Exception.Create('Error processing JSON array: Empty array.');
      end
      else if JsonValue is TJSONObject then
      begin
        // Handle JSON objects
        JsonObject := JsonValue as TJSONObject;
        Result := TJSONObject(JsonObject.Clone);
      end
      else
      begin
        // Unexpected data type
        raise Exception.Create('Unexpected JSON type encountered: ' + JsonValue.ToString);
      end;
    finally
      JsonValue.Free;
    end;
  except
    on E: Exception do
    begin
      LogStuff('Error processing JSON: ' + E.Message);
      LogStuff('Problematic JSON: ' + JsonStr.Substring(0, Min(500, JsonStr.Length)));
      raise;
    end;
  end;
end;





procedure ProcessScryfallBulkFile(const FileName: string);
var
  FileStream: TFileStream;
  Reader: TScryfallStreamReader;
  Card: TJSONObject;
  ProcessedCount: Integer;
  LastUpdate: TStopwatch;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Reader := TScryfallStreamReader.Create(FileStream);
  try
    LastUpdate := TStopwatch.StartNew;

    Reader.OnProgress := procedure(Position, Total: Int64)
    begin
      if LastUpdate.ElapsedMilliseconds >= 1000 then
      begin
        LogStuff(Format('Progress: %.1f%%', [(Position / Total) * 100]));
        LastUpdate.Reset;
        LastUpdate.Start;
      end;
    end;

    ProcessedCount := 0;
    while True do
    begin
      try
        Card := Reader.ReadNextCard;
        if Card = nil then
          Break;

        Inc(ProcessedCount);

        // Log the parsed card
        LogStuff(Format('Card %d: %s (%s)', [
          ProcessedCount,
          Card.GetValue<string>('name', 'Unknown'),
          Card.GetValue<string>('set', 'Unknown')
        ]));

      except
        on E: Exception do
        begin
          LogStuff('Skipping problematic card: ' + E.Message);
          Continue; // Skip problematic card and move to the next
        end;
      end;

      Card.Free;
    end;

    LogStuff(Format('Successfully processed %d cards.', [ProcessedCount]));
  finally
    Reader.Free;
    FileStream.Free;
  end;
end;



end.

