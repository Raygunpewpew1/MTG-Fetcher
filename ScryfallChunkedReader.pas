unit ScryfallChunkedReader;
// TScryfallStreamReader: Efficiently reads Scryfall bulk JSON files incrementally.
// Ideal for handling large files without loading them entirely into memory.
// Potential uses: Offline card search, batch processing, or database population.

interface

uses
  System.Classes, System.SysUtils, JsonDataObjects,
  System.Generics.Collections, System.Diagnostics,Logger;

type
  TScryfallStreamReader = class
  private
    FStream: TStream;
    FBuffer: TStringBuilder;
    FBracketCount: Integer;
    FInString: Boolean;
    FEscapeNext: Boolean;
    FChunkSize: Integer;
    FTempBuffer: TBytes;

    // Progress tracking
    FCurrentPosition: Int64;
    FTotalSize: Int64;
    FOnProgress: TProc<Int64, Int64>;

    function ReadUntilCompleteObject: string;
    procedure ReportProgress;
  public
    constructor Create(AStream: TStream; AChunkSize: Integer = 8192);
    destructor Destroy; override;

    // Returns nil when no more cards are available
    function ReadNextCard: TJsonObject;

    property OnProgress: TProc<Int64, Int64> read FOnProgress write FOnProgress;
    property CurrentPosition: Int64 read FCurrentPosition;
    property TotalSize: Int64 read FTotalSize;
  end;

implementation

constructor TScryfallStreamReader.Create(AStream: TStream; AChunkSize: Integer);
var
  FirstChar: AnsiChar;
begin
  inherited Create;
  FStream := AStream;
  FChunkSize := AChunkSize;
  SetLength(FTempBuffer, FChunkSize);
  FBuffer := TStringBuilder.Create;
  FBracketCount := 0;
  FInString := False;
  FEscapeNext := False;
  FTotalSize := FStream.Size;
  FCurrentPosition := 0;

  // Skip the initial '[' character of the JSON array, ensuring the stream has data
  if FStream.Size > 0 then
  begin
    if FStream.Read(FirstChar, 1) <> 1 then
      raise Exception.Create('Failed to read from stream.');
    if FirstChar <> '[' then
      raise Exception.Create('Stream does not start with a JSON array.');
    Inc(FCurrentPosition);
  end
  else
    raise Exception.Create('Stream is empty.');
end;

destructor TScryfallStreamReader.Destroy;
begin
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
  BytesRead: Integer;
  I: Integer;
  CurrentChar: Char;
begin
  Result := '';
  FBuffer.Clear;

  while FCurrentPosition < FTotalSize do
  begin
    BytesRead := FStream.Read(FTempBuffer[0], FChunkSize);
    if BytesRead <= 0 then
      Break;

    Inc(FCurrentPosition, BytesRead);

    for I := 0 to BytesRead - 1 do
    begin
      CurrentChar := Char(FTempBuffer[I]);

      // Handle string escape sequences
      if FEscapeNext then
      begin
        FEscapeNext := False;
        FBuffer.Append(CurrentChar);
        Continue;
      end;

      case CurrentChar of
        '\':
          if FInString then
            FEscapeNext := True;

        '"':
          if not FEscapeNext then
            FInString := not FInString;

        '{':
          if not FInString then
            Inc(FBracketCount);

        '}':
          if not FInString then
          begin
            Dec(FBracketCount);
            if FBracketCount = 0 then
            begin
              FBuffer.Append(CurrentChar);
              Result := FBuffer.ToString;
              Exit;
            end;
          end;
      end;

      FBuffer.Append(CurrentChar);
    end;

    ReportProgress;
  end;

  if FBracketCount <> 0 then
    raise Exception.Create('Incomplete JSON object in stream.');
end;

function TScryfallStreamReader.ReadNextCard: TJsonObject;
var
  JsonStr: string;
begin
  Result := nil;
  JsonStr := ReadUntilCompleteObject;

  if JsonStr <> '' then
  begin
    try
      Result := TJsonObject.Create;
      Result.FromJSON(JsonStr);
    except
      on E: Exception do
      begin
        FreeAndNil(Result);
        raise Exception.Create('Error parsing card JSON: ' + E.Message);
      end;
    end;
  end;
end;

// Example usage with memory-efficient processing
procedure ProcessScryfallBulkFile(const FileName: string);
var
  FileStream: TFileStream;
  Reader: TScryfallStreamReader;
  Card: TJsonObject;
  ProcessedCount: Integer;
  LastUpdate: TStopwatch;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Reader := TScryfallStreamReader.Create(FileStream);
    try
      LastUpdate := TStopwatch.StartNew;

      Reader.OnProgress := procedure(Position, Total: Int64)
      begin
        // Update progress max once per second
        if LastUpdate.ElapsedMilliseconds >= 1000 then
        begin
          LogStuff(Format('Processing: %.1f%%', [(Position / Total) * 100]));
          LastUpdate.Reset;
          LastUpdate.Start;
        end;
      end;

      ProcessedCount := 0;
      while True do
      begin
        Card := Reader.ReadNextCard;
        if Card = nil then
          Break;

        try
          Inc(ProcessedCount);

          // Process the card
          LogStuff(Format('Card %d: %s (%s)', [
            ProcessedCount,
            Card.S['name'],
            Card.S['set']
          ]));

        finally
          Card.Free;
        end;
      end;

      LogStuff(Format('Successfully processed %d cards', [ProcessedCount]));

    finally
      Reader.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

end.
