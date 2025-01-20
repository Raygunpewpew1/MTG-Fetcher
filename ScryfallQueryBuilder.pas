unit ScryfallQueryBuilder;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  SGlobalsZ, APIConstants, ScryfallTypes, Logger, System.NetEncoding,
  System.Math, System.SyncObjs, System.StrUtils;

type
  TScryfallQuery = class(TInterfacedObject)
  private
    FFilters: TList<TScryfallFilter>;
    FOptions: TScryfallQueryOptions;
//    FLock: TCriticalSection;
    procedure AddFilterInternal(const Filter: TScryfallFilter);
    // function GetCacheKey: string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetFilterCount: Integer;

    function GetFilters: TList<TScryfallFilter>;
    procedure SetFilters(const Value: TList<TScryfallFilter>);
    property Filters: TList<TScryfallFilter> read GetFilters write SetFilters;
    procedure LogQueryState(const Context: string);
    // Basic filter methods
    function BuildOptionsString: string;
    function AddFilter(FilterType: TScryfallFilterType; const Value: string;
      Operator: TScryfallOperator = opEquals): TScryfallQuery;
    function AddNotFilter(FilterType: TScryfallFilterType; const Value: string)
      : TScryfallQuery;
    function AddRangeFilter(FilterType: TScryfallFilterType;
      const MinValue, MaxValue: string): TScryfallQuery;
    function ClearFilters: TScryfallQuery;
    function Page(const PageNum: Integer): TScryfallQuery;
    function Clone: TScryfallQuery;
    function ValidateFilters: Boolean;
    function AreFiltersValid: Boolean;

    // Common search patterns
    function WithName(const Name: string; ExactMatch: Boolean = False)
      : TScryfallQuery;
    function WithSet(const SetCode: string): TScryfallQuery;
    function WithRarity(const Rarity: TRarity): TScryfallQuery;
    function WithColors(const Colors: string; ExactMatch: Boolean = False)
      : TScryfallQuery;
    function WithColorIdentity(const Colors: string;
      ExactMatch: Boolean = False): TScryfallQuery;
    function WithType(const TypeLine: string): TScryfallQuery;
    function WithCMC(const Value: Double;
      Operator: TScryfallOperator = opEquals): TScryfallQuery;
    function WithKeyword(const Keyword: string): TScryfallQuery;

    // Format and legality
    function WithFormat(const Format: string; const Status: string = 'legal')
      : TScryfallQuery;
    function WithFormats(const Formats: TArray<string>;
      const Status: string = 'legal'): TScryfallQuery;

    // Price filters
    function WithPrice(PriceType: TScryfallPriceType; const Value: Currency;
      Operator: TScryfallOperator = opEquals): TScryfallQuery;
    function WithPriceRange(PriceType: TScryfallPriceType;
      const MinValue, MaxValue: Currency): TScryfallQuery;

    // Search options
    function Unique(const Mode: string = 'cards'): TScryfallQuery;
    function IncludeExtras(const Include: Boolean = True): TScryfallQuery;
    function OrderBy(const Field: string; const Direction: string = 'auto')
      : TScryfallQuery;
    function SetPage(const PageNum: Integer): TScryfallQuery;

    // API compatibility methods
    function GetSearchTerm: string;
    function GetSetCode: string;
    function GetRarityString: string;
    function GetColorCode: string;
    function GetShowUnique: Boolean;
    function GetCurrentPage: Integer;

    // Query generation
    function BuildQuery: string;
    function BuildURL: string;
    function ToCacheKey: string;
    property Options: TScryfallQueryOptions read FOptions write FOptions;
  end;

  // Helper class for common search patterns
  TScryfallQueryHelper = class
  public
    // Standard format helpers
    class function CreateStandardLegalQuery: TScryfallQuery;
    class function CreateModernLegalQuery: TScryfallQuery;
    class function CreateCommanderQuery(const Colors: string): TScryfallQuery;

    // Price-based helpers
    class function CreateBudgetQuery(const MaxPrice: Currency): TScryfallQuery;
    class function CreatePremiumQuery(const MinPrice: Currency): TScryfallQuery;

    // Collection helpers
    class function CreateCollectorQuery(const SetCode, Number: string)
      : TScryfallQuery;
    class function CreateSetQuery(const SetCode: string;
      RarityFilter: TRarity = rAll): TScryfallQuery;

    // Color-based helpers
    class function CreateMonoColorQuery(const Color: string): TScryfallQuery;
    class function CreateMultiColorQuery(const Colors: string): TScryfallQuery;
    class function CreateColorlessQuery: TScryfallQuery;

    // Type-based helpers
    class function CreateCreatureQuery(const TypeLine: string = '')
      : TScryfallQuery;
    class function CreatePlaneswalkerQuery: TScryfallQuery;
    class function CreateLegendaryQuery(const TypeLine: string = '')
      : TScryfallQuery;
  end;

implementation

{ TScryfallQuery }

constructor TScryfallQuery.Create;
begin
  inherited Create;
  FFilters := TList<TScryfallFilter>.Create;
  FOptions.IncludeExtras := False;
  FOptions.UniqueMode := 'cards';
  FOptions.Sort := '';
  FOptions.Direction := 'auto';
  FOptions.Page := 1;
  LogStuff('TScryfallQuery created - FFilters initialized.', DEBUG);
end;

destructor TScryfallQuery.Destroy;
begin
  LogStuff(Format('TScryfallQuery destroyed. Address: %p',
    [Pointer(Self)]), DEBUG);
  FFilters.Free;
  FFilters := nil;
  inherited;
end;

function TScryfallQuery.GetFilterCount: Integer;
begin
  if Assigned(FFilters) then
    Result := FFilters.Count
  else
    Result := 0;
end;

procedure TScryfallQuery.LogQueryState(const Context: string);
begin
  var
  FilterCount := IfThen(Assigned(FFilters), FFilters.Count, 0);
  LogStuff(Format
    ('%s - Query State: Options: IncludeExtras: %s, UniqueMode: %s, Sort: %s, Direction: %s, Page: %d, Filters Count: %d',
    [Context, BoolToStr(FOptions.IncludeExtras, True), FOptions.UniqueMode,
    FOptions.Sort, FOptions.Direction, FOptions.Page, FilterCount]), DEBUG);
end;

function TScryfallQuery.GetFilters: TList<TScryfallFilter>;
begin
  Result := FFilters;
end;

procedure TScryfallQuery.SetFilters(const Value: TList<TScryfallFilter>);
begin
  if FFilters <> Value then
  begin
    FFilters.Free;
    FFilters := Value;
  end;
end;

function TScryfallQuery.ValidateFilters: Boolean;
begin
  if not Assigned(FFilters) then
  begin
    LogStuff('Error: FFilters is nil in ValidateFilters. Initializing to an empty list.',
      WARNING);
    FFilters := TList<TScryfallFilter>.Create; // Initialize FFilters
  end;

  // Further validation logic for filters
  for var Filter in FFilters do
  begin
    if (Filter.Values = nil) or (Length(Filter.Values) = 0) then
    begin
      LogStuff('Error: A filter in FFilters has empty or nil Values.', ERROR);
      Exit(False);
    end;
  end;

  Result := True; // Filters are valid
end;

function TScryfallQuery.AreFiltersValid: Boolean;
var
  Filter: TScryfallFilter;
  FilterIndex: Integer;
begin
  if not Assigned(FFilters) then
  begin
    LogStuff('AreFiltersValid: FFilters is nil.', ERROR);
    Exit(False);
  end;

  if FFilters.Count = 0 then
  begin
    LogStuff('AreFiltersValid: FFilters is empty.', ERROR);
    Exit(False);
  end;

  for FilterIndex := 0 to FFilters.Count - 1 do
  begin
    try
      Filter := FFilters[FilterIndex];
      LogStuff(Format('AreFiltersValid: Checking filter [%d], Type: [%s]',
        [FilterIndex, ScryfallFilterPrefix[Filter.FilterType]]), DEBUG);
    except
      on E: Exception do
      begin
        LogStuff(Format('AreFiltersValid: Error accessing filter [%d]: %s',
          [FilterIndex, E.Message]), ERROR);
        Exit(False);
      end;
    end;
  end;

  Result := True;
end;

function TScryfallQuery.Clone: TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  try
    // Initialize the filters list in the clone
    if Assigned(FFilters) then
    begin
      Result.FFilters := TList<TScryfallFilter>.Create;
      for var Filter in FFilters do
        Result.FFilters.Add(Filter.Clone); // Ensure deep copy of filters
    end;

    // Copy all options to the cloned query
    Result.FOptions := FOptions;

    LogStuff(Format
      ('Clone - After Cloning - Query State: Options: IncludeExtras: %s, UniqueMode: %s, Sort: %s, Direction: %s, Page: %d, Filters Count: %d',
      [BoolToStr(Result.FOptions.IncludeExtras, True),
      Result.FOptions.UniqueMode, Result.FOptions.Sort,
      Result.FOptions.Direction, Result.FOptions.Page,
      IfThen(Assigned(Result.FFilters), Result.FFilters.Count, 0)]), DEBUG);
  except
    Result.Free;
    raise;
  end;
end;

function TScryfallQuery.Page(const PageNum: Integer): TScryfallQuery;
begin
  FOptions.Page := Max(1, PageNum); // Ensure page is never less than 1
  Result := Self;
end;

function TScryfallQuery.AddFilter(FilterType: TScryfallFilterType;
  const Value: string; Operator: TScryfallOperator): TScryfallQuery;
begin
  AddFilterInternal(TScryfallFilter.Create(FilterType, Value, Operator));
  Result := Self;
end;

procedure TScryfallQuery.AddFilterInternal(const Filter: TScryfallFilter);
begin
  if not Assigned(FFilters) then
    raise Exception.Create('FFilters is not initialized');
  FFilters.Add(Filter);
end;

function TScryfallQuery.WithName(const Name: string; ExactMatch: Boolean)
  : TScryfallQuery;
begin
  if Name.Trim.IsEmpty then
    raise Exception.Create('Search term cannot be empty.');

  if ExactMatch then
    Result := AddFilter(ftName, Name, opExact)
  else
    Result := AddFilter(ftName, Name);
end;

function TScryfallQuery.WithSet(const SetCode: string): TScryfallQuery;
begin
  if SetCode.IsEmpty then
    Result := Self
  else
    Result := AddFilter(ftSet, SetCode.ToLower);
end;

function TScryfallQuery.WithRarity(const Rarity: TRarity): TScryfallQuery;
begin
  if Rarity = rAll then
    Result := Self
  else
    Result := AddFilter(ftRarity, MapRarityToString(Rarity));
end;

function TScryfallQuery.WithColors(const Colors: string; ExactMatch: Boolean)
  : TScryfallQuery;
begin
  if Colors.IsEmpty then
    Result := Self
  else if ExactMatch then
    Result := AddFilter(ftColor, Colors, opExact)
  else
    Result := AddFilter(ftColor, Colors);
end;

function TScryfallQuery.WithPrice(PriceType: TScryfallPriceType;
  const Value: Currency; Operator: TScryfallOperator): TScryfallQuery;
begin
  Result := AddFilter(ftPrice, FormatPriceValue(Value), Operator);
end;

function TScryfallQuery.WithPriceRange(PriceType: TScryfallPriceType;
  const MinValue, MaxValue: Currency): TScryfallQuery;
var
  Filter: TScryfallFilter;
begin
  Filter := TScryfallFilter.Create(ftPrice, FormatPriceValue(MinValue),
    opGreaterEqual);
  Filter.Values[0].ExtraValue := FormatPriceValue(MaxValue);
  AddFilterInternal(Filter);
  Result := Self;
end;

function TScryfallQuery.BuildQuery: string;
var
  SB: TStringBuilder;
  Filter: TScryfallFilter;
  RawQuery: string;
begin
  if not Assigned(FFilters) then
  begin
    LogStuff('BuildQuery: FFilters is nil.', ERROR);
    raise Exception.Create('BuildQuery: FFilters is not initialized.');
  end;

  SB := TStringBuilder.Create;
  try
    for Filter in FFilters do
    begin
      if SB.Length > 0 then
        SB.Append(' '); // Add space between filters

      // Append each filter's query part
      SB.Append(Filter.ToQueryPart);
    end;

    // Combine filters and options
    RawQuery := SB.ToString + BuildOptionsString;

    // Replace spaces in the final query string with %20 (URL encoding for spaces)
    Result := RawQuery.Replace(' ', '%20');
  finally
    SB.Free;
  end;
end;




function TScryfallQuery.BuildURL: string;
var
  Query: string;
begin
  Query := BuildQuery;
  Result := Format(StandardSStr, [EndpointSearch,
    TNetEncoding.URL.Encode(Query)]);
  LogStuff('Built URL: ' + Result);
end;

// function TScryfallQuery.GetCacheKey: string;
// begin
// Result := TNetEncoding.Base64.Encode(BuildQuery);
// end;

procedure ValidateSortingOptions(const SortField, Direction: string);
begin
  if not SortField.IsEmpty and not MatchStr(SortField,
    ['name', 'released_at', 'set', 'rarity', 'usd', 'cmc', 'power', 'toughness'])
  then
    raise Exception.CreateFmt('Invalid Sort Field: %s', [SortField]);

  if not MatchStr(Direction, ['asc', 'desc', 'auto']) then
    raise Exception.CreateFmt('Invalid Sort Direction: %s', [Direction]);
end;

function TScryfallQuery.BuildOptionsString: string;
begin
  LogStuff(Format('BuildOptionsString - Options: %s',
    [FOptions.ToString]), DEBUG);
  Result := '';
  if FOptions.UniqueMode <> '' then
    Result := Result + '&unique=' + FOptions.UniqueMode;
  if FOptions.IncludeExtras then
    Result := Result + '&include_extras=true';
  if FOptions.Sort <> '' then
    Result := Result + '&order=' + FOptions.Sort;
  if FOptions.Direction <> 'auto' then
    Result := Result + '&dir=' + FOptions.Direction;
  if FOptions.Page > 1 then
    Result := Result + '&page=' + FOptions.Page.ToString;
end;




// API Compatibility Methods

function TScryfallQuery.GetSearchTerm: string;
var
  Filter: TScryfallFilter;
begin
  Result := '';
  for Filter in FFilters do
    if Filter.FilterType = ftName then
    begin
      Result := Filter.Values[0].Value;
      Break;
    end;
end;

function TScryfallQuery.GetSetCode: string;
var
  Filter: TScryfallFilter;
begin
  Result := '';
  for Filter in FFilters do
    if Filter.FilterType = ftSet then
    begin
      Result := Filter.Values[0].Value;
      Break;
    end;
end;

function TScryfallQuery.GetRarityString: string;
var
  Filter: TScryfallFilter;
begin
  AreFiltersValid;
  Result := 'all'; // Default to 'all' if no rarity filter is found

  // Validate FFilters
  if not Assigned(FFilters) or (FFilters.Count = 0) then
  begin
    LogStuff('GetRarityString: FFilters is nil or empty.', ERROR);
    Exit;
  end;

  for Filter in FFilters do
    if Filter.FilterType = ftRarity then
    begin
      // Ensure Values[0] exists
      if (Length(Filter.Values) > 0) then
        Result := Filter.Values[0].Value
      else
        LogStuff('GetRarityString: Filter.Values is empty.', WARNING);
      Break;
    end;
end;

function TScryfallQuery.GetColorCode: string;
var
  Filter: TScryfallFilter;
begin
  AreFiltersValid;
  Result := '';

  // Validate FFilters
  if not Assigned(FFilters) or (FFilters.Count = 0) then
  begin
    LogStuff('GetColorCode: FFilters is nil or empty.', WARNING);
    Exit;
  end;

  for Filter in FFilters do
    if Filter.FilterType = ftColor then
    begin
      // Ensure Values[0] exists
      if (Length(Filter.Values) > 0) then
        Result := Filter.Values[0].Value
      else
        LogStuff('GetColorCode: Filter.Values is empty.', WARNING);
      Break;
    end;
end;

function TScryfallQuery.GetShowUnique: Boolean;
begin
  Result := FOptions.UniqueMode <> '';
end;

function TScryfallQuery.GetCurrentPage: Integer;
begin
  Result := FOptions.Page;
end;

{ TScryfallQueryHelper }

class function TScryfallQueryHelper.CreateStandardLegalQuery: TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithFormat('standard', 'legal');
end;

function TScryfallQuery.WithColorIdentity(const Colors: string;
  ExactMatch: Boolean = False): TScryfallQuery;
begin
  if Colors.IsEmpty then
    Result := Self
  else if ExactMatch then
    Result := AddFilter(ftColorId, Colors, opExact)
  else
    Result := AddFilter(ftColorId, Colors);
end;

function TScryfallQuery.WithType(const TypeLine: string): TScryfallQuery;
begin
  Result := AddFilter(ftType, TypeLine.ToLower);
end;

function TScryfallQuery.WithCMC(const Value: Double;
  Operator: TScryfallOperator = opEquals): TScryfallQuery;
begin
  Result := AddFilter(ftCmc, FormatFloat('0.#', Value), Operator);
end;

function TScryfallQuery.WithKeyword(const Keyword: string): TScryfallQuery;
begin
  Result := AddFilter(ftKeyword, Keyword.ToLower);
end;

function TScryfallQuery.WithFormat(const Format: string;
  const Status: string = 'legal'): TScryfallQuery;
begin
  Result := AddFilter(ftFormat, Format.ToLower + '=' + Status.ToLower);
end;

function TScryfallQuery.WithFormats(const Formats: TArray<string>;
  const Status: string = 'legal'): TScryfallQuery;
var
  Format: string;
begin
  for Format in Formats do
    WithFormat(Format, Status);
  Result := Self;
end;

function TScryfallQuery.Unique(const Mode: string = 'cards'): TScryfallQuery;
begin
  FOptions.UniqueMode := Mode.ToLower;
  Result := Self;
end;

function TScryfallQuery.IncludeExtras(const Include: Boolean = True)
  : TScryfallQuery;
begin
  FOptions.IncludeExtras := Include;
  LogStuff('IncludeExtras set to: ' + BoolToStr(FOptions.IncludeExtras,
    True), DEBUG);
  Result := Self;
end;

function TScryfallQuery.OrderBy(const Field: string;
  const Direction: string = 'auto'): TScryfallQuery;
begin
  if Field.Trim.IsEmpty then
    raise Exception.Create('Sort field cannot be empty in OrderBy.');
  if not MatchText(Direction.ToLower, ['auto', 'asc', 'desc']) then
    raise Exception.Create('Invalid Direction value in OrderBy.');

  FOptions.Sort := Field.ToLower;
  FOptions.Direction := Direction.ToLower;

  LogStuff(Format('OrderBy set - Sort: %s, Direction: %s',
    [FOptions.Sort, FOptions.Direction]), DEBUG);
  Result := Self;
end;

function TScryfallQuery.SetPage(const PageNum: Integer): TScryfallQuery;
begin
  FOptions.Page := Max(1, PageNum);
  Result := Self;
  // LogQueryState('SetPage');
end;

function TScryfallQuery.ToCacheKey: string;
begin
  Result := BuildQuery + '|' + BuildOptionsString;
end;

// Helper class implementations

class function TScryfallQueryHelper.CreateModernLegalQuery: TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithFormat('modern', 'legal');
end;

class function TScryfallQueryHelper.CreateCommanderQuery(const Colors: string)
  : TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithFormat('commander', 'legal').WithColorIdentity(Colors, True);
end;

class function TScryfallQueryHelper.CreateBudgetQuery(const MaxPrice: Currency)
  : TScryfallQuery;
begin
  // Create the query
  Result := TScryfallQuery.Create;

  // Add the price filter
  Result.WithPrice(ptUSD, MaxPrice, opLessEqual);

  // Log the query state for debugging
  Result.LogQueryState('CreateBudgetQuery');

  // Validate the filters and options
  if not Result.ValidateFilters then
  begin
    LogStuff('CreateBudgetQuery: Filters validation failed.', ERROR);
    FreeAndNil(Result); // Clean up the query if invalid
    raise Exception.Create
      ('CreateBudgetQuery failed: Filters validation failed.');
  end;
end;

class function TScryfallQueryHelper.CreatePremiumQuery(const MinPrice: Currency)
  : TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithPrice(ptUSD, MinPrice, opGreaterEqual);
end;

class function TScryfallQueryHelper.CreateCollectorQuery(const SetCode,
  Number: string): TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithSet(SetCode).AddFilter(ftCollector, Number);
end;

class function TScryfallQueryHelper.CreateSetQuery(const SetCode: string;
  RarityFilter: TRarity = rAll): TScryfallQuery;
begin
  Result := TScryfallQuery.Create.WithSet(SetCode);
  if RarityFilter <> rAll then
    Result.WithRarity(RarityFilter);
end;

class function TScryfallQueryHelper.CreateMonoColorQuery(const Color: string)
  : TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithColorIdentity(Color, True); // Exact match for mono-color
end;

class function TScryfallQueryHelper.CreateMultiColorQuery(const Colors: string)
  : TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithColorIdentity(Colors) // Not exact match for multi-color
    .AddFilter(ftColor, 'm'); // 'm' indicates multicolor in Scryfall
end;

class function TScryfallQueryHelper.CreateColorlessQuery: TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithColorIdentity(''); // Empty color identity for colorless
end;

class function TScryfallQueryHelper.CreateCreatureQuery(const TypeLine
  : string = ''): TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  if TypeLine.IsEmpty then
    Result.WithType('creature')
  else
    Result.WithType('creature ' + TypeLine);
end;

class function TScryfallQueryHelper.CreatePlaneswalkerQuery: TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  Result.WithType('planeswalker');
end;

class function TScryfallQueryHelper.CreateLegendaryQuery(const TypeLine
  : string = ''): TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
  if TypeLine.IsEmpty then
    Result.WithType('legendary')
  else
    Result.WithType('legendary ' + TypeLine);
end;

function TScryfallQuery.AddNotFilter(FilterType: TScryfallFilterType;
  const Value: string): TScryfallQuery;
begin
  Result := AddFilter(FilterType, Value, opNot);
end;

function TScryfallQuery.AddRangeFilter(FilterType: TScryfallFilterType;
  const MinValue, MaxValue: string): TScryfallQuery;
var
  Filter: TScryfallFilter;
begin
  Filter := TScryfallFilter.Create(FilterType, MinValue, opGreaterEqual);
  Filter.Values[0].ExtraValue := MaxValue;
  AddFilterInternal(Filter);
  Result := Self;
end;

function TScryfallQuery.ClearFilters: TScryfallQuery;
begin
  if Assigned(FFilters) then
  begin
    FFilters.Clear;
    LogStuff('ClearFilters: FFilters cleared.', DEBUG);
  end
  else
    LogStuff('ClearFilters: FFilters is nil.', ERROR);

  Result := Self;
end;

end.
