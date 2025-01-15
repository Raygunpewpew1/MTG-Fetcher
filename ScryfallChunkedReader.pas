unit ScryfallChunkedReader;

interface

uses
  System.Classes, System.SysUtils, System.JSON, System.Diagnostics, Logger,
  Math, System.Generics.Collections;

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
    FCurrentArray: TJSONArray; // Tracks the current array being processed
    FArrayIndex: Integer;      // Tracks the current position within the array


    procedure ReportProgress;
    function ReadUntilCompleteObject: string;
  public
    constructor Create(AStream: TStream; AChunkSize: Integer = 65536);
    destructor Destroy; override;

    function ReadNextCard: TJSONObject;

    property OnProgress: TProc<Int64, Int64> read FOnProgress write FOnProgress;
    property CurrentPosition: Int64 read FCurrentPosition;
    property TotalSize: Int64 read FTotalSize;
  end;

procedure ProcessScryfallBulkFile(const FileName: string);
function FindCardByName(const FileName, CardName: string): TJSONObject;

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
  ObjectBracketCount, ArrayBracketCount: Integer;
  InString: Boolean;
  EscapeNext: Boolean;
  I: Integer;
  CurrentChar: Char;
begin
  Result := '';
  ObjectBracketCount := 0;
  ArrayBracketCount := 0;
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
          '"':
            InString := not InString; // Toggle string state
          '\':
            if InString then
              EscapeNext := True; // Handle escaped characters
          '{':
            if not InString then
              Inc(ObjectBracketCount); // Track object brackets
          '}':
            if not InString then
              Dec(ObjectBracketCount); // Close object bracket
          '[':
            if not InString then
              Inc(ArrayBracketCount); // Track array brackets
          ']':
            if not InString then
            begin
              Dec(ArrayBracketCount); // Close array bracket
              if (ObjectBracketCount = 0) and (ArrayBracketCount = 0) then
              begin
                FBuffer.Append(CurrentChar);
                Exit(FBuffer.ToString); // Return complete JSON array or object
              end;
            end;
        end;
      end;

      FBuffer.Append(CurrentChar);
    end;
  end;

  if (ObjectBracketCount <> 0) or (ArrayBracketCount <> 0) then
    raise Exception.Create('Incomplete JSON object or array in stream.');
end;


function TScryfallStreamReader.ReadNextCard: TJSONObject;
var
  JsonStr: string;
  JsonValue: TJSONValue;
begin
  Result := nil;

  // If we're currently processing an array, return the next item
  if Assigned(FCurrentArray) then
  begin
    if FArrayIndex < FCurrentArray.Count then
    begin
      // Return the next object in the array
      if FCurrentArray.Items[FArrayIndex] is TJSONObject then
      begin
        Result := TJSONObject(FCurrentArray.Items[FArrayIndex].Clone);
        Inc(FArrayIndex);
        Exit;
      end
      else
        raise Exception.Create('Array item is not a JSON object.');
    end
    else
    begin
      // Clear the array once we've processed all items
      FreeAndNil(FCurrentArray);
    end;
  end;

  // Read a chunk of data from the stream
  JsonStr := ReadUntilCompleteObject;

  // Skip empty data
  if JsonStr.Trim = '' then
    Exit;

  try
    JsonValue := TJSONObject.ParseJSONValue(JsonStr);
    try
      if JsonValue = nil then
        raise Exception.Create('Failed to parse JSON: Null or invalid format.');

      // Handle JSON objects directly
      if JsonValue is TJSONObject then
      begin
        Result := TJSONObject(JsonValue.Clone);
      end
      // Handle JSON arrays
      else if JsonValue is TJSONArray then
      begin
        FCurrentArray := TJSONArray(JsonValue.Clone); // Cache the array
        FArrayIndex := 0;

        // Process the first object in the array immediately
        if FCurrentArray.Items[FArrayIndex] is TJSONObject then
        begin
          Result := TJSONObject(FCurrentArray.Items[FArrayIndex].Clone);
          Inc(FArrayIndex);
        end
        else
          raise Exception.Create('First element in JSON array is not an object.');
      end
      else
        raise Exception.Create('Unexpected JSON type encountered.');
    finally
      JsonValue.Free;
    end;
  except
    on E: Exception do
    begin
      LogStuff('Error processing JSON: ' + E.Message);
      LogStuff('Problematic JSON: ' + JsonStr);
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
//         LogStuff(Format('Card %d: %s (%s)', [
//         ProcessedCount,
//         Card.GetValue<string>('name', 'Unknown'),
//         Card.GetValue<string>('set', 'Unknown')
//         ]));

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
    // LogStuff( Card.GetValue<string>('name', 'Unknown') );
  finally
    Reader.Free;
    FileStream.Free;
  end;
end;

function FindCardByName(const FileName, CardName: string): TJSONObject;
var
  FileStream: TFileStream;
  Reader: TScryfallStreamReader;
  Card: TJSONObject;
  CardNameValue: string;
begin
  Result := nil; // Default to nil if card not found

  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  Reader := TScryfallStreamReader.Create(FileStream);
  try
    while True do
    begin
      try
        Card := Reader.ReadNextCard;
        if Card = nil then
          Break; // End of file

        // Get the card's name and compare it
        CardNameValue := Card.GetValue<string>('name', 'Unknown');
        if SameText(CardNameValue, CardName) then
        begin
          Result := TJSONObject(Card.Clone); // Return a clone of the found card
          Break; // Exit the loop since the card is found
        end;
      finally
        Card.Free; // Free the current card to avoid memory leaks
      end;
    end;

  finally
    Reader.Free;
    FileStream.Free;
  end;
end;

end.
