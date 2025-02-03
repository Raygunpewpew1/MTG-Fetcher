unit ScryfallTypes;

interface

uses
  System.SysUtils, SGlobalsX, Logger,
  System.NetEncoding;

type
  EScryfallFilterError = class(Exception);

  TScryfallFilterType = (ftName, // name:
    ftOracle, // o:
    ftType, // t:
    ftColor, // c:
    ftColorId, // id:
    ftSet, // s:
    ftRarity, // r:
    ftCmc, // cmc:
    ftFormat, // f:
    ftPrice, // various price filters
    ftKeyword, // keyword:
    ftArtist, // a:
    ftPower, // pow:
    ftToughness, // tou:
    ftLegal, // legal:
    ftBanned, // banned:
    ftRestricted, // restricted:
    ftBlock, // b:
    ftCollector, // cn:
    ftSet_Type // st:
    );

  TScryfallOperator = (opEquals, // :
    opExact, // =
    opGreater, // >
    opLess, // <
    opGreaterEqual, // >=
    opLessEqual, // <=
    opNot, // -
    opOr // OR
    );

  TScryfallPriceType = (ptUSD, ptUSDFoil, ptEUR, ptTIX);

  TScryfallFilterValue = record
    Value: string;
    Operator: TScryfallOperator;
    ExtraValue: string; // For range values
  end;

  TScryfallFilter = record
    FilterType: TScryfallFilterType;
    Values: TArray<TScryfallFilterValue>;
    // Support multiple values for OR conditions
    function ToQueryPart: string;
    function Clone: TScryfallFilter;
    class function Create(AType: TScryfallFilterType; const AValue: string;
      AOperator: TScryfallOperator = opEquals): TScryfallFilter; static;
    procedure AddValue(const Value: string;
      Operator: TScryfallOperator = opEquals);
    procedure Clear;
  end;

  TScryfallQueryOptions = record
    IncludeExtras: Boolean;
    UniqueMode: string; // 'cards', 'art', 'prints'
    Sort: string;
    Direction: string; // 'auto', 'asc', 'desc'
    Page: Integer;
    function Clone: TScryfallQueryOptions;
    function ToString: string;
    procedure Clear;
  end;

const
  // Filter type to API syntax mapping
  ScryfallFilterPrefix: array [TScryfallFilterType] of string = ('name',
    // ftName
    'o', // ftOracle
    't', // ftType
    'c', // ftColor
    'id', // ftColorId
    'e', // ftSet
    'r', // ftRarity
    'cmc', // ftCmc
    'f', // ftFormat
    'usd', // ftPrice (default to USD)
    'keyword', // ftKeyword
    'a', // ftArtist
    'pow', // ftPower
    'tou', // ftToughness
    'legal', // ftLegal
    'banned', // ftBanned
    'restricted', // ftRestricted
    'b', // ftBlock
    'cn', // ftCollector
    'st' // ftSet_Type
    );

  // Operator to syntax mapping
  ScryfallOperatorStr: array [TScryfallOperator] of string = (':', // opEquals
    '=', // opExact
    '>', // opGreater
    '<', // opLess
    '>=', // opGreaterEqual
    '<=', // opLessEqual
    '-', // opNot
    'OR' // opOr
    );

  // Price type to API parameter mapping
  ScryfallPricePrefix: array [TScryfallPriceType] of string = ('usd',
    'usd_foil', 'eur', 'tix');

  // Cache constants
  DefaultCacheTimeout = 3600; // 1 hour in seconds
  MaxCacheSize = 1000; // Maximum number of cached queries

  // Helper functions
function GetOperatorStr(Op: TScryfallOperator): string;
function MapRarityToString(Rarity: TRarity): string;
function EscapeQueryValue(const Value: string): string;
function FormatPriceValue(const Value: Currency): string;

implementation

uses
  System.Math;

function GetOperatorStr(Op: TScryfallOperator): string;
begin
  Result := ScryfallOperatorStr[Op];
end;

function MapRarityToString(Rarity: TRarity): string;
begin
  Result := Rarity.ToString;  // This calls the helper method.
end;


function EscapeQueryValue(const Value: string): string;
var
  EscapedValue: string;
begin
  // Step 1: Escape double quotes for Scryfall's syntax
  if Value.Contains('"') then
    EscapedValue := StringReplace(Value, '"', '\"', [rfReplaceAll])
  else
    EscapedValue := Value;

  // Step 2: Wrap the value in double quotes if it contains spaces
  if Value.Contains(' ') then
    EscapedValue := '"' + EscapedValue + '"';

  // Step 3: URL-encode the escaped value
  Result := TNetEncoding.URL.Encode(EscapedValue);

  // Optional: Replace '%20' with '+' for Scryfall's preferred space encoding
  Result := Result.Replace('%20', '+');
end;

function FormatPriceValue(const Value: Currency): string;
begin
  Result := FormatFloat('0.00', Value);
end;

{ TScryfallFilter }

class function TScryfallFilter.Create(AType: TScryfallFilterType;
  const AValue: string; AOperator: TScryfallOperator): TScryfallFilter;
var
  Ch: Char;
begin
  if AValue.IsEmpty then
    raise EScryfallFilterError.Create('Filter value cannot be empty.');

  if AType = ftColorId then
  begin
    for Ch in AValue.ToLower do
      if not CharInSet(Ch, ['w', 'u', 'b', 'r', 'g']) then
        raise EScryfallFilterError.CreateFmt
          ('Invalid color identity value: %s', [Ch]);
  end;

  Result.FilterType := AType;
  SetLength(Result.Values, 1);
  Result.Values[0].Value := AValue;
  Result.Values[0].Operator := AOperator;
  Result.Values[0].ExtraValue := '';
end;

procedure TScryfallFilter.AddValue(const Value: string;
  Operator: TScryfallOperator);
var
  Idx: Integer;
begin
  Idx := Length(Values);
  SetLength(Values, Idx + 1);
  Values[Idx].Value := Value;
  Values[Idx].Operator := Operator;
  Values[Idx].ExtraValue := '';
end;

procedure TScryfallFilter.Clear;
begin
  SetLength(Values, 0);
end;

function TScryfallFilter.ToQueryPart: string;
var
  SB: TStringBuilder;
  i: Integer;
  Prefix: string;
begin
  if Length(Values) = 0 then
    Exit(''); // No values, return an empty string

  SB := TStringBuilder.Create;
  try
    Prefix := ScryfallFilterPrefix[FilterType]; // Get the filter type prefix

    for i := 0 to High(Values) do
    begin
      if i > 0 then
        SB.Append(' OR '); // Scryfall supports "OR" for multiple values

      if (FilterType = ftColor) and (Pos('M', Values[i].Value) > 0) then
        SB.Append(Prefix).Append(GetOperatorStr(Values[i].Operator)).Append('m')
        // Ensure multicolor is encoded as `m`
      else
        SB.Append(Prefix).Append(GetOperatorStr(Values[i].Operator))
          .Append(EscapeQueryValue(Values[i].Value));

      if Values[i].ExtraValue <> '' then
        SB.Append(' ').Append(Prefix).Append(GetOperatorStr(opLessEqual))
          .Append(EscapeQueryValue(Values[i].ExtraValue));
    end;

    Result := SB.ToString; // Return the constructed query part
  finally
    SB.Free;
  end;
end;

function TScryfallFilter.Clone: TScryfallFilter;
begin
  Result.FilterType := FilterType;
  Result.Values := Copy(Values); // Creates a new array copy
end;

function TScryfallQueryOptions.Clone: TScryfallQueryOptions;
begin
  Result := Self; // Simple record assignment (no managed types to deep-copy)
end;

{ TScryfallQueryOptions }

function TScryfallQueryOptions.ToString: string;
begin
  Result := Format
    ('IncludeExtras: %s, UniqueMode: %s, Sort: %s, Direction: %s, Page: %d',
    [BoolToStr(IncludeExtras, True), UniqueMode, Sort, Direction, Page]);
end;

procedure TScryfallQueryOptions.Clear;
begin
  LogStuff('TScryfallQueryOptions.Clear called.', DEBUG);
  IncludeExtras := False;
  UniqueMode := 'cards';
  Sort := 'name';
  Direction := 'auto';
  Page := 1;
end;

end.
