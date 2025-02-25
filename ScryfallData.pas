unit ScryfallData;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  JsonDataObjects,
  CardMainData,
  System.Net.HttpClient,
  Logger,
  System.SyncObjs,
  MLogic,
  System.IOUtils,
  ScryfallFilterType,
  ScryfallQuery,
  System.Diagnostics,
  System.StrUtils,
  CardMetaData;

type
  EScryfallAPIError = class(Exception);
  EScryfallRateLimitError = class(EScryfallAPIError);
  EScryfallServerError = class(EScryfallAPIError);

  /// <summary>
  /// Callback type for search completion with success, results, and error details.
  /// </summary>
type
  TOnSearchComplete = reference to procedure(Success: Boolean;
    Cards: TArray<TCardDetails>; HasMore: Boolean; TotalCards: Integer;
    ErrorMsg: string);

  /// <summary>
  /// Main class for interacting with the Scryfall API.
  /// Provides methods for fetching cards, sets, catalogs, and related data.
  /// </summary>
  TScryfallAPI = class
  private

//    FCache: TObjectDictionary<string, TJsonObject>;
    FCacheLock: TCriticalSection;
    FHttpClient: THTTPClient;
    FAutocompleteCache: TDictionary<string, TArray<string>>;
    /// <summary>
    /// Executes an HTTP request to the specified endpoint.
    /// Supports GET and POST requests.
    /// </summary>
    function ExecuteRequest(const Endpoint: string;
      const Payload: TJsonObject = nil): TJsonObject;
    /// <summary>
    /// Parses the search result JSON response into a TSearchResult instance.
    /// </summary>
    function ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;
    /// <summary>
    /// Executes an internal search query for cards with advanced filtering options.
    /// </summary>
    function InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
    /// <summary>
    /// Extracts image URIs from a JSON object into a TImageUris instance.
    /// </summary>
    function ParseImageUris(const JsonObj: TJsonObject): TImageUris;
    /// <summary>
    /// Fetches autocomplete suggestions for a partial card name.
    /// </summary>
    function FetchAutocompleteSuggestions(const PartialQuery: string)
      : TArray<string>;
    /// <summary>
    /// Retrieves all available sets from the Scryfall API.
    /// </summary>
    function FetchAllSetsFromAPI: TArray<TSetDetails>;
    /// <summary>
    /// Executes a search query and returns the results as a TSearchResult instance.
    /// </summary>
    function ExecuteQuery(const Query: TScryfallQuery): TSearchResult;
    /// <summary>
    /// Checks the cache for a specific key and retrieves the result if available.
    /// </summary>
//    function GetCachedResult(const CacheKey: string;
//      out JsonResponse: TJsonObject): Boolean;
//    procedure CacheResult(const CacheKey: string;
//      const JsonResponse: TJsonObject);
    /// <summary>
    /// Performs an HTTP request with retries for failure handling.
    /// </summary>
    function MakeRequestWithRetries(const URL: string;
      const Payload: TJsonObject): string;
    /// <summary>
    /// Fetches card details using a specific API endpoint.
    /// </summary>
    function GetCardByEndpoint(const Endpoint: string): TCardDetails;

  public

    constructor Create;
    destructor Destroy; override;

    function CreateQuery: TScryfallQuery;
    function IsInternetAvailable: Boolean;

    // Core card/set endpoints
    function GetSetByCode(const SetCode: string): TSetDetails;
    function GetCardByName(const CardName: string; Fuzzy: Boolean = False)
      : TCardDetails;
    function SearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer = 1): TArray<TCardDetails>;
    function GetAllSets: TArray<TSetDetails>;
    function SearchAllCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean): TArray<TCardDetails>;

    /// <summary>
    /// Performs an asynchronous search for all cards using a TScryfallQuery.
    /// </summary>
    // Async / Parallel
    procedure SearchAllCardsAsync(const Query: TScryfallQuery;
      const OnComplete: TOnSearchComplete); overload;

    // Random / Catalog
    function GetRandomCard: TCardDetails;
    function GetCatalog(const CatalogName: string): TScryfallCatalog;
    function FetchAllCatalogs: TDictionary<string, TScryfallCatalog>;

    // Additional
    function GetCardByUUID(const UUID: string): TCardDetails;
    function GetCardByArenaID(const UArenaID: Integer): TCardDetails;
    function GetCardImageUris(const UUID: string): TImageUris;
    function GetCardImage(const UUID: string;
      const ImageType: string = 'normal'): string;
    function GetCardRulings(const UUID: string): TArray<TRuling>;

    // Query-based
    function SearchWithQuery(Query: TScryfallQuery): TArray<TCardDetails>;
    procedure SearchWithQueryAsync(Query: TScryfallQuery;
      const OnComplete: TOnSearchComplete);
    function FetchCardsCollection(const Identifiers: TJSONArray)
      : TArray<TCardDetails>;

    // Autocomplete
    function AutocompleteCards(const PartialQuery: string): TArray<string>;

  end;

implementation

uses
  ScryfallDataHelper,
  APIConstants,
  System.NetEncoding,
  CardDisplayHelpers,
  System.Net.URLClient;

constructor TScryfallAPI.Create;
begin
  inherited Create;

  // Basic dictionary cache to store JSON results
//  FCache := TObjectDictionary<string, TJsonObject>.Create([doOwnsValues]);
  FCacheLock := TCriticalSection.Create;

  // Single THTTPClient for all requests
  FHttpClient := THTTPClient.Create;
  FHttpClient.CustomHeaders['User-Agent'] := UserAgent;
  FHttpClient.CustomHeaders['Accept'] := AcceptHeader;
  FHttpClient.ConnectionTimeout := 15000; // 15 seconds
  FHttpClient.ResponseTimeout := 30000; // 30 seconds

  // In-memory autocomplete cache
  FAutocompleteCache := TDictionary < string, TArray < string >>.Create;
end;

destructor TScryfallAPI.Destroy;
begin
  FCacheLock.Enter;
  try
//    FreeAndNil(FCache); // TObjectDictionary will automatically free its objects
  finally
    FCacheLock.Leave;
  end;

  FreeAndNil(FCacheLock);
  FreeAndNil(FHttpClient);
  FreeAndNil(FAutocompleteCache);

  inherited Destroy;
end;



function TScryfallAPI.CreateQuery: TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
end;

function TScryfallAPI.IsInternetAvailable: Boolean;
begin
  try
    FHttpClient.Head('https://www.google.com');
    Result := True;
  except
    Result := False;
  end;
end;

function TScryfallAPI.GetCardByEndpoint(const Endpoint: string): TCardDetails;
var
  JsonResponse: TJsonObject;
  TempCard: TCardDetails;
begin
  JsonResponse := ExecuteRequest(Endpoint);
  try
    TempCard := TCardDetails.Create;
    try
      TWrapperHelper.FillCardDetailsFromJson(JsonResponse, TempCard);
      Result := TempCard;
    except
      TempCard.Free;
      raise;
    end;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetSetByCode(const SetCode: string): TSetDetails;
var
  CachedSets: TArray<TSetDetails>;
  JsonResponse: TJsonObject;
  Endpoint: string;
begin
  CachedSets := LoadSetDetailsFromJson(GetCacheFilePath(SetCacheFile));
  try
    for var SetDetails in CachedSets do
    begin
      if SetDetails.Code = SetCode then
      begin
        LogStuff(Format('Set details for "%s" loaded from cache.', [SetCode]));
        Exit(SetDetails);
      end;
    end;
  finally
    FreeSetDetailsArray(CachedSets);
  end;

  // If not found in cache, fetch from API
  Endpoint := Format('%s%s', [EndpointSets, TNetEncoding.URL.Encode(SetCode)]);
  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
    begin
      TWrapperHelper.SFillSetDetailsFromJson(JsonResponse, Result);
      LogStuff(Format('Set details for "%s" fetched from API.', [SetCode]));

      // Save to local cache
      SaveSetDetailsToJson(GetCacheFilePath(SetCacheFile), [Result]);
    end
    else
      raise EScryfallAPIError.CreateFmt('No data found for set code "%s".',
        [SetCode]);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardByName(const CardName: string; Fuzzy: Boolean)
  : TCardDetails;
var
  Endpoint: string;
begin

  if Fuzzy then
    Endpoint := Format('%s?fuzzy=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(CardName)])
  else
    Endpoint := Format('%s?exact=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(CardName)]);

  Result := GetCardByEndpoint(Endpoint);
end;

function TScryfallAPI.SearchCards(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): TArray<TCardDetails>;
var
  SearchResult: TSearchResult;
begin
  SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors, Fuzzy,
    Unique, Page);
  try
    Result := SearchResult.Cards.ToArray;
  finally
    SearchResult.Free;
  end;
end;

function TScryfallAPI.GetAllSets: TArray<TSetDetails>;
var
  CacheFile: string;
begin
  CacheFile := GetCacheFilePath(SetCacheFile);

  if TFile.Exists(CacheFile) then
    LogStuff('Loading all sets from local cache.', DEBUG);

  LogStuff('Fetching all sets from Scryfall API.', DEBUG);
  Result := FetchAllSetsFromAPI;

  // Save fresh data in JSON form
  SaveSetDetailsToJson(CacheFile, Result);
end;

function TScryfallAPI.SearchAllCards(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean): TArray<TCardDetails>;
var
  Page: Integer;
  SearchResult: TSearchResult;
  AllCards: TObjectList<TCardDetails>;
  i: Integer;
  Card: TCardDetails;
begin

  AllCards := TObjectList<TCardDetails>.Create(true);
  try
    Page := 1;
    repeat
      SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors, Fuzzy, Unique, Page);
      try
        for i := 0 to SearchResult.Cards.Count - 1 do
        begin
          Card := SearchResult.Cards.Extract(SearchResult.Cards[i]);
          AllCards.Add(Card);
        end;
      finally
        SearchResult.Free;
      end;
      Inc(Page);
    until not SearchResult.HasMore;
    Result := AllCards.ToArray;
  finally
    AllCards.Free;
  end;
end;



function TScryfallAPI.GetCardRulings(const UUID: string): TArray<TRuling>;
var
  JsonResponse: TJsonObject;
begin
  if UUID.IsEmpty then
    raise EScryfallAPIError.Create('UUID cannot be empty.');

  JsonResponse := ExecuteRequest(Format(EndPointRuling, [UUID]));
  try
    TWrapperHelper.ParseRulings(JsonResponse, Result);
  finally
    JsonResponse.Free;
  end;
end;

procedure TScryfallAPI.SearchAllCardsAsync(const Query: TScryfallQuery;
  const OnComplete: TOnSearchComplete);
begin
  LogStuff(Format('SearchAllCardsAsync - Query Address: %p, IncludeExtras: %s',
    [Pointer(Query), BoolToStr(Query.Options.IncludeExtras, True)]), DEBUG);

  // Reuse our general-purpose async query method
  SearchWithQueryAsync(Query, OnComplete);
end;

function TScryfallAPI.GetRandomCard: TCardDetails;
var
  JsonResponse: TJsonObject;
begin
  JsonResponse := ExecuteRequest(EndpointRandomCard);
  try
    Result := TCardDetails.Create; // Allocate a new instance
    TWrapperHelper.FillCardDetailsFromJson(JsonResponse, Result);
  finally
    JsonResponse.Free;
  end;

end;

function TScryfallAPI.GetCatalog(const CatalogName: string): TScryfallCatalog;
var
  JsonResponse: TJsonObject;
  DataArray: TJSONArray;
  i: Integer;
  TempData: TArray<string>;
begin
  Result := TScryfallCatalog.Create; // Allocate new instance
  Result.Name := CatalogName;

  JsonResponse := ExecuteRequest(Format('catalog/%s', [CatalogName]));
  try
    if JsonResponse.Contains(FieldData) and
      (JsonResponse.Types[FieldData] = jdtArray) then
    begin
      DataArray := JsonResponse.A[FieldData];
      SetLength(TempData, DataArray.Count);
      for i := 0 to DataArray.Count - 1 do
        TempData[i] := DataArray.S[i];
      Result.Data := TempData;
    end;

    if JsonResponse.Contains(FieldCount) then
      Result.TotalItems := JsonResponse.i[FieldCount];
    if JsonResponse.Contains(FieldUri) then
      Result.Uri := JsonResponse.S[FieldUri];
    if JsonResponse.Contains(FieldObject) then
      Result.ObjectType := JsonResponse.S[FieldObject];
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.FetchAllCatalogs: TDictionary<string, TScryfallCatalog>;
var
  CatalogNames: TArray<string>;
  CatalogDict: TDictionary<string, TScryfallCatalog>;
  Name: string;
begin
  CatalogNames := TArray<string>.Create(CatalogCreatureTypes,
    CatalogPlaneswalkerTypes, CatalogArtifactTypes, CatalogEnchantmentTypes,
    CatalogLandTypes, CatalogSpellTypes, CatalogPowers, CatalogToughnesses,
    CatalogLoyalties, CatalogWatermarks, CatalogKeywordAbilities,
    CatalogKeywordActions, CatalogAbilityWords, CatalogWordBank);

  CatalogDict := TDictionary<string, TScryfallCatalog>.Create;
  try
    for Name in CatalogNames do
      CatalogDict.Add(Name, GetCatalog(Name));

    Result := CatalogDict;
  except
    CatalogDict.Clear;
    CatalogDict.Free;
    raise;
  end;
end;

function TScryfallAPI.GetCardByUUID(const UUID: string): TCardDetails;
var
  Endpoint: string;
begin
  if UUID.IsEmpty then
    raise EScryfallAPIError.Create('UUID cannot be empty');

  Endpoint := Format('cards/%s', [UUID]);

  Result := GetCardByEndpoint(Endpoint);
end;

function TScryfallAPI.GetCardByArenaID(const UArenaID: Integer): TCardDetails;
var
  Endpoint: string;
begin
  if UArenaID = 0 then
    raise EScryfallAPIError.Create('Arena ID cannot be empty');

  Endpoint := Format('cards/arena/%d', [UArenaID]);

  Result := GetCardByEndpoint(Endpoint);
end;

function TScryfallAPI.GetCardImageUris(const UUID: string): TImageUris;
var
  JsonResponse: TJsonObject;
begin
  if UUID.IsEmpty then
    raise EScryfallAPIError.Create('UUID cannot be empty');

  JsonResponse := ExecuteRequest(Format('cards/%s', [UUID]));
  try
    Result := ParseImageUris(JsonResponse);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardImage(const UUID: string;
  const ImageType: string): string;
var
  ImageUris: TImageUris;
  LowerType: string;
begin
  Result := '';
  LowerType := LowerCase(ImageType);

  ImageUris := GetCardImageUris(UUID);
  try
  // Match the requested size/type
  if LowerType = FieldSmall then
    Result := ImageUris.Small
  else if LowerType = FieldNormal then
    Result := ImageUris.Normal
  else if LowerType = FieldLarge then
    Result := ImageUris.Large
  else if LowerType = FieldPng then
    Result := ImageUris.Png
  else if LowerType = FieldArtCrop then
    Result := ImageUris.Art_crop
  else if LowerType = FieldBorderCrop then
    Result := ImageUris.Border_crop
  else
    Result := ImageUris.Normal; // fallback

finally
  ImageUris.Free;
end;

  if Result.IsEmpty then
    raise EScryfallAPIError.CreateFmt
      ('Image type "%s" not available for card with UUID: %s',
      [ImageType, UUID]);
end;

function TScryfallAPI.SearchWithQuery(Query: TScryfallQuery): TArray<TCardDetails>;
var
  SearchResult: TSearchResult;
begin
  SearchResult := ExecuteQuery(Query);
  try
    Result := SearchResult.Cards.ToArray;
  finally
    SearchResult.Free;
  end;
end;


procedure TScryfallAPI.SearchWithQueryAsync(Query: TScryfallQuery; const OnComplete: TOnSearchComplete);
var
  SearchResult: TSearchResult;
  Success: Boolean;
  ErrorMsg: string;
begin
SearchResult:= nil;
  TTask.Run(
    procedure
    begin
      try
        try
          SearchResult := ExecuteQuery(Query);  // Attempt to fetch result
          Success := True;
          ErrorMsg := '';
        except
          on E: Exception do
          begin
            Success := False;
            ErrorMsg := E.Message;
            if not Assigned(SearchResult) then
              SearchResult := TSearchResult.Create
            else
              SearchResult.Cards.Clear;
            SearchResult.HasMore := False;
          end;
        end;

        TThread.Queue(nil,
          procedure
          begin
            try
              OnComplete(Success, SearchResult.Cards.ToArray, SearchResult.HasMore,
                SearchResult.TotalCards, ErrorMsg);
            finally
              SearchResult.Free; // Free after callback
            end;
          end);
      except
        if Assigned(SearchResult) then
          SearchResult.Free;
        raise;
      end;
    end);
end;


function TScryfallAPI.FetchCardsCollection(const Identifiers: TJSONArray)
  : TArray<TCardDetails>;
var
  JsonResponse: TJsonObject;
  RequestPayload: TJsonObject;
  CardsArray: TJSONArray;
  i: Integer;
begin
  // Prepare
  SetLength(Result, 0);
  RequestPayload := TJsonObject.Create;
  try
    RequestPayload.A['identifiers'] := Identifiers;

    // Execute POST /cards/collection
    JsonResponse := ExecuteRequest('cards/collection', RequestPayload);
    try
      if JsonResponse.Contains('data') then
      begin
        CardsArray := JsonResponse.A['data'];
        SetLength(Result, CardsArray.Count);

        for i := 0 to CardsArray.Count - 1 do
          TWrapperHelper.FillCardDetailsFromJson(CardsArray.O[i], Result[i]);
      end
      else
        raise EScryfallAPIError.Create('Missing "data" field in response.');
    finally
      JsonResponse.Free;
    end;
  finally
    RequestPayload.Free;
  end;
end;

function TScryfallAPI.AutocompleteCards(const PartialQuery: string)
  : TArray<string>;
var
  FirstKey: string;
begin
  if PartialQuery.Length < 2 then
  begin
    LogStuff(Format('Query "%s" is too short. Returning empty suggestions.',
      [PartialQuery]));
    Exit(TArray<string>.Create());
  end;

  // Check local cache
  FCacheLock.Enter;
  try
    if FAutocompleteCache.TryGetValue(PartialQuery, Result) then
    begin
      LogStuff(Format('Cache hit for query: "%s"', [PartialQuery]));
      Exit;
    end
    else
      LogStuff(Format('Cache miss for query: "%s". Fetching from API.',
        [PartialQuery]));
  finally
    FCacheLock.Leave;
  end;

  // Fetch from API
  Result := FetchAutocompleteSuggestions(PartialQuery);

  // Store in cache
  FCacheLock.Enter;
  try
    FAutocompleteCache.AddOrSetValue(PartialQuery, Result);
    LogStuff(Format('Cached suggestions for query: "%s"', [PartialQuery]));

    // Evict oldest entry if above cache limit
    if FAutocompleteCache.Count > MaxAutocompleteCacheEntries then
    begin
      for FirstKey in FAutocompleteCache.Keys do
      begin
        LogStuff(Format('Evicting cache entry: "%s"', [FirstKey]));
        FAutocompleteCache.Remove(FirstKey);
        Break;
      end;
    end;
  finally
    FCacheLock.Leave;
  end;
end;

/// <summary>
/// A private helper to perform an HTTP GET or POST with retries.
/// </summary>
function TScryfallAPI.MakeRequestWithRetries(const URL: string;
const Payload: TJsonObject): string;
const
  MaxRetries = 3;
  RetryDelayMs = 500; // 0.5 seconds
var
  RequestStream: TStringStream;
  ResponseStream: TStringStream;
  TryCount: Integer;
  StatusCode: Integer;
  ContentTypeHeader: TNameValuePair;
begin
  // If there's a Payload, we assume POST; otherwise, we do GET
  if Assigned(Payload) then
    RequestStream := TStringStream.Create(Payload.ToJSON, TEncoding.UTF8)
  else
    RequestStream := nil;

  ResponseStream := TStringStream.Create;
  try
    for TryCount := 1 to MaxRetries do
    begin
      ResponseStream.Clear; // always reset for a new attempt

      LogStuff(Format('MakeRequestWithRetries -> URL: %s (attempt %d)',
        [URL, TryCount]), DEBUG);

      TMonitor.Enter(FHttpClient);
      try
        if Assigned(RequestStream) then
        begin
          // POST
          RequestStream.Position := 0;
          ContentTypeHeader := TNameValuePair.Create('Content-Type',
            'application/json');
          StatusCode := FHttpClient.Post(URL, RequestStream, ResponseStream,
            [ContentTypeHeader]).StatusCode;
        end
        else
        begin
          // GET
          StatusCode := FHttpClient.Get(URL, ResponseStream).StatusCode;
        end;
      finally
        TMonitor.Exit(FHttpClient);
      end;

      // Check status and handle success or known error codes
      case StatusCode of
        200:
          begin
            // Return the response body as soon as we see 200
            Result := ResponseStream.DataString;
            Exit;
          end;
        429:
          raise EScryfallRateLimitError.Create('Rate limit exceeded.');
        500 .. 599:
          raise EScryfallServerError.CreateFmt('Server error (%d) occurred.',
            [StatusCode]);
      else
        // Non-2xx and not 429/5xx
        raise EScryfallAPIError.CreateFmt
          ('Request failed with status %d for URL %s', [StatusCode, URL]);
      end;

      // If not successful , wait and retry
      if TryCount < MaxRetries then
        Sleep(RetryDelayMs);
    end;

    raise EScryfallAPIError.CreateFmt('Max retries reached for URL: %s', [URL]);
  finally
    ResponseStream.Free;
    if Assigned(RequestStream) then
      RequestStream.Free;
  end;
end;

function TScryfallAPI.ExecuteRequest(const Endpoint: string;
  const Payload: TJsonObject): TJsonObject;
var
  URL, ResponseStr: string;
  JsonObj: TJsonObject;
begin
  if not IsInternetAvailable then
    raise EScryfallAPIError.Create('No internet connection available.');

  URL := IfThen(Endpoint.StartsWith('http'), Endpoint,
    BaseUrl + '/' + Endpoint.TrimLeft(['/']));

  ResponseStr := MakeRequestWithRetries(URL, Payload);
  Result := nil;

  JsonObj := TJsonObject.Parse(ResponseStr) as TJsonObject;
  try
    try
      Result := JsonObj.Clone as TJsonObject;
    except
      on E: Exception do
      begin
        if Assigned(Result) then
          FreeAndNil(Result);
        raise EScryfallAPIError.CreateFmt('Error parsing JSON: %s', [E.Message]);
      end;
    end;
  finally
    JsonObj.Free; // Always free JsonObj
  end;
end;


/// //--- removed TParallel.For
function TScryfallAPI.ParseSearchResult(const JsonResponse: TJsonObject)
  : TSearchResult;
var
  CardsArray: TJSONArray;
  Stopwatch: TStopwatch;
  i: Integer;
  CardObj: TJsonObject;
  TempCard: TCardDetails;
begin
  // Return a valid object
  Result := TSearchResult.Create; // Assign ownership to the caller
  try
    if not Assigned(JsonResponse) then
      Exit; // Result is already allocated, no need to free

    // 1) Handle Scryfall error responses
    if JsonResponse.S[FieldObject] = 'error' then
    begin
      var
        ErrorCode, ErrorDetails: string;
      var
        ErrorStatus: Integer;

      ErrorCode := IfThen(JsonResponse.Contains('code'), JsonResponse.S['code'],
        'unknown');
      if JsonResponse.Contains('status') then
        ErrorStatus := JsonResponse.i['status']
      else
        ErrorStatus := -1;

      ErrorDetails := JsonResponse.S['details'];

      raise EScryfallAPIError.CreateFmt
        ('Scryfall error: %s (code: %s, status: %d)',
        [ErrorDetails, ErrorCode, ErrorStatus]);
    end;

    // 2) Process card list
    if JsonResponse.Contains(FieldData) then
    begin
      CardsArray := JsonResponse.A[FieldData];
      LogStuff('ParseSearchResult -> Data Array Count: ' +
        CardsArray.Count.ToString, DEBUG);

      Result.Cards.Capacity := CardsArray.Count;
      Result.Cards.Clear;

      Stopwatch := TStopwatch.StartNew;

      for i := 0 to CardsArray.Count - 1 do
      begin
        if CardsArray.Types[i] = jdtObject then
        begin
          CardObj := CardsArray.O[i];
          TempCard := TCardDetails.Create;
          try
            TWrapperHelper.FillCardDetailsFromJson(CardObj, TempCard);
            Result.Cards.Add(TempCard);
          except
            TempCard.Free;
            raise;
          end;
        end
        else
          LogStuff(Format('Skipping non-object element at index %d',
            [i]), WARNING);
      end;

      Stopwatch.Stop;
      LogStuff(Format('Parsing Time: %d ms',
        [Stopwatch.ElapsedMilliseconds]), DEBUG);

      // 3) Process metadata safely
      Result.HasMore := JsonResponse.B[FieldHasMore];
      Result.NextPageURL := JsonResponse.S[FieldNextPage];
      Result.TotalCards := JsonResponse.i[FieldTotalCards];
    end
    else
      LogStuff('ParseSearchResult -> Missing "data" key in JSON response.',
        ERROR);
  except
    Result.Free; // Ensure cleanup before re-raising the exception
    raise;
  end;
end;

function TScryfallAPI.InternalSearchCards(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
var
  CacheKey, SearchUrl: string;
   JsonResponse: TJsonObject;   //CachedResponse
begin
  // Build a unique key for this search
  CacheKey := Format('%s:%s:%s:%s:%d:%d', [Query, SetCode, Rarity, Colors, Page,
    Ord(Unique)]);

  // Check local memory cache
//  TMonitor.Enter(FCache);
//  try
//    if FCache.TryGetValue(CacheKey, CachedResponse) then
//      Exit(ParseSearchResult(CachedResponse));
//  finally
//    TMonitor.Exit(FCache);
//  end;

  // Construct the actual search URL
  SearchUrl := TWrapperHelper.ConstructSearchUrl(Query, SetCode, Rarity, Colors,
    Fuzzy, Unique, Page);

  // Execute
  JsonResponse := ExecuteRequest(SearchUrl);
  try
    Result := ParseSearchResult(JsonResponse);

    // Store in cache
//    TMonitor.Enter(FCache);
//    try
//      FCache.Add(CacheKey, JsonResponse.Clone as TJsonObject);
//    finally
//      TMonitor.Exit(FCache);
//    end;
  finally
    JsonResponse.Free;
    LogStuff('InternalSearchCards -> ' + SearchUrl);
  end;
end;

function TScryfallAPI.ExecuteQuery(const Query: TScryfallQuery): TSearchResult;
var
  CacheKey: string;
  JsonResponse: TJsonObject;
  URL: string;
begin
  CacheKey := Query.ToCacheKey;

  // Check cache
//  if GetCachedResult(CacheKey, JsonResponse) then
//  begin
//    LogStuff('Cache hit for query: ' + Query.BuildQuery);
//    Exit(ParseSearchResult(JsonResponse));
//  end;

  // Build URL
  if BaseUrl.EndsWith('/') then
    URL := BaseUrl + 'cards/search?q=' + Query.BuildQuery
  else
    URL := BaseUrl + '/cards/search?q=' + Query.BuildQuery;

  LogStuff('Executing query: ' + URL);

  JsonResponse := ExecuteRequest(URL);
  try
    Result := ParseSearchResult(JsonResponse);
  //  CacheResult(CacheKey, JsonResponse.Clone as TJsonObject);
  finally
    JsonResponse.Free;
  end;
end;

//function TScryfallAPI.GetCachedResult(const CacheKey: string; out JsonResponse: TJsonObject): Boolean;
//begin
//  FCacheLock.Enter;
//  try
//    Result := FCache.TryGetValue(CacheKey, JsonResponse);
//  finally
//    FCacheLock.Leave;
//  end;
//end;


//procedure TScryfallAPI.CacheResult(const CacheKey: string; const JsonResponse: TJsonObject);
//begin
//  FCacheLock.Enter;
//  try
//    if FCache.ContainsKey(CacheKey) then
//      FCache.Remove(CacheKey); // Removes and frees the old object automatically
//
//    FCache.Add(CacheKey, JsonResponse.Clone as TJsonObject); // Cloned object will be owned by FCache
//  finally
//    FCacheLock.Leave;
//  end;
//end;


function TScryfallAPI.ParseImageUris(const JsonObj: TJsonObject): TImageUris;
begin
  // Allocate a new instance of TImageUris.
  Result := TImageUris.Create;

  // If the JSON object is nil or doesn't contain the field, exit early.
  if (JsonObj = nil) or not JsonObj.Contains(FieldImageUris) then
    Exit;

  with JsonObj.O[FieldImageUris] do
  begin
    if Contains(FieldSmall) then
      Result.Small := S[FieldSmall];
    if Contains(FieldNormal) then
      Result.Normal := S[FieldNormal];
    if Contains(FieldLarge) then
      Result.Large := S[FieldLarge];
    if Contains(FieldPng) then
      Result.Png := S[FieldPng];
    if Contains(FieldBorderCrop) then
      Result.Border_crop := S[FieldBorderCrop];
    if Contains(FieldArtCrop) then
      Result.Art_crop := S[FieldArtCrop];
  end;
end;

function TScryfallAPI.FetchAutocompleteSuggestions(const PartialQuery: string)
  : TArray<string>;
var
  URL: string;
  JsonResponse: TJsonObject;
  DataArray: TJSONArray;
  i: Integer;
begin
  Result := [];
  URL := Format('cards/autocomplete?q=%s',
    [TNetEncoding.URL.Encode(PartialQuery)]);
  LogStuff(Format('Fetching autocomplete suggestions for query: "%s"',
    [PartialQuery]));

  JsonResponse := ExecuteRequest(URL);
  try
    if JsonResponse.Contains(FieldData) then
    begin
      DataArray := JsonResponse.A[FieldData];
      SetLength(Result, DataArray.Count);
      for i := 0 to DataArray.Count - 1 do
        Result[i] := DataArray.S[i];
    end
    else
      LogStuff('Autocomplete response missing "data" array.', WARNING);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.FetchAllSetsFromAPI: TArray<TSetDetails>;
var
  JsonResponse: TJsonObject;
  SetsArray: TJSONArray;
  i: Integer;
begin
  JsonResponse := ExecuteRequest(EndpointSets);
  try
    if JsonResponse.Contains(FieldData) then
    begin
      SetsArray := JsonResponse.A[FieldData];
      SetLength(Result, SetsArray.Count);

      for i := 0 to SetsArray.Count - 1 do
      begin
        // Allocate the class instance
        Result[i] := TSetDetails.Create;
        // Fill the newly created instance
        TWrapperHelper.SFillSetDetailsFromJson(SetsArray.O[i], Result[i]);
      end;
    end
    else
      raise EScryfallAPIError.Create('API response is missing set data.');
  finally
    JsonResponse.Free;
  end;
end;

end.
