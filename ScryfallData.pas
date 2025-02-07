unit ScryfallData;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  JsonDataObjects,
  SGlobalsX,
  System.Net.HttpClient,
  Logger,
  System.SyncObjs,
  MLogic,
  System.IOUtils,
  ScryfallTypes,
  ScryfallQuery,
  System.Diagnostics;

type
  EScryfallAPIError = class(Exception);
  EScryfallRateLimitError = class(EScryfallAPIError);
  EScryfallServerError = class(EScryfallAPIError);

type
  TOnSearchComplete = reference to procedure(
    Success: Boolean;
    Cards: TArray<TCardDetails>;
    HasMore: Boolean;
    TotalCards: Integer;
    ErrorMsg: string
  );


  TScryfallAPI = class
  private

    FCache: TDictionary<string, TJsonObject>;
    FCacheLock: TCriticalSection;
    FHttpClient: THTTPClient;
    FAutocompleteCache: TDictionary<string, TArray<string>>;

    function ExecuteRequest(const Endpoint: string;
      const Payload: TJsonObject = nil): TJsonObject;
    function ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;
    function InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
    function ParseImageUris(const JsonObj: TJsonObject): TImageUris;
    function FetchAutocompleteSuggestions(const PartialQuery: string)
      : TArray<string>;
    function FetchAllSetsFromAPI: TArray<TSetDetails>;
    function ExecuteQuery(const Query: TScryfallQuery): TSearchResult;
    function GetCachedResult(const CacheKey: string;
      out JsonResponse: TJsonObject): Boolean;
    procedure CacheResult(const CacheKey: string;
      const JsonResponse: TJsonObject);
    function MakeRequestWithRetries(const URL: string;
      const Payload: TJsonObject): string;
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
  FCache := TDictionary<string, TJsonObject>.Create;
  FCacheLock := TCriticalSection.Create;

  // Single THTTPClient for all requests
  FHttpClient := THTTPClient.Create;
  FHttpClient.CustomHeaders['User-Agent'] := UserAgent;
  FHttpClient.CustomHeaders['Accept'] := AcceptHeader;
  FHttpClient.ConnectionTimeout := 15000; // 15 seconds
  FHttpClient.ResponseTimeout := 30000;   // 30 seconds


  // In-memory autocomplete cache
  FAutocompleteCache := TDictionary < string, TArray < string >>.Create;
end;

destructor TScryfallAPI.Destroy;
begin
  // Protect shared caches with the lock
  FCacheLock.Enter;
  try
    FAutocompleteCache.Free;
    FAutocompleteCache := nil;
  finally
    FCacheLock.Leave;
  end;

  // Release resources
  FCacheLock.Free;
  FHttpClient.Free;
  FCache.Free;

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
  // Check local JSON cache first
  CachedSets := LoadSetDetailsFromJson(GetCacheFilePath(SetCacheFile));
  for var SetDetails in CachedSets do
  begin
    if SetDetails.Code = SetCode then
    begin
      LogStuff(Format('Set details for "%s" loaded from cache.', [SetCode]));
      Exit(SetDetails);
    end;
  end;

  // If not found in cache, fetch from Scryfall
  Endpoint := Format('%s%s', [EndpointSets, TNetEncoding.URL.Encode(SetCode)]);
  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
    begin
      TWrapperHelper.FillSetDetailsFromJson(JsonResponse, Result);
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
  Result := SearchResult.Cards.ToArray;
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

function TScryfallAPI.SearchAllCards(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean): TArray<TCardDetails>;
var
  Page: Integer;
  SearchResult: TSearchResult;
  AllCards: TList<TCardDetails>;
begin
  AllCards := TList<TCardDetails>.Create;
  try
    Page := 1;
    repeat
      SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors, Fuzzy,
        Unique, Page);
      AllCards.AddRange(SearchResult.Cards);
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

  if Result.IsEmpty then
    raise EScryfallAPIError.CreateFmt
      ('Image type "%s" not available for card with UUID: %s',
      [ImageType, UUID]);
end;

function TScryfallAPI.SearchWithQuery(Query: TScryfallQuery)
  : TArray<TCardDetails>;
var
  SearchResult: TSearchResult;
begin
  SearchResult := ExecuteQuery(Query);
  Result := SearchResult.Cards.ToArray;
end;

procedure TScryfallAPI.SearchWithQueryAsync(Query: TScryfallQuery;
  const OnComplete: TOnSearchComplete);
begin
  TTask.Run(
    procedure
    var
      SearchResult: TSearchResult;
      Temp: TSearchResult;
      Success: Boolean;
      ErrorMsg: string;
    begin

      SearchResult := TSearchResult.Create;
      try
        try
          Temp := ExecuteQuery(Query);

          SearchResult.Free;

          SearchResult := Temp;

          Success := True;
          ErrorMsg := '';
        except
          on E: Exception do
          begin
            Success := False;
            ErrorMsg := E.Message;

            SearchResult.Cards.Clear;
            SearchResult.HasMore := False;
            LogStuff('Query execution error: ' + E.Message, ERROR);
          end;
        end;

        // Then queue up the callback
        TThread.Queue(nil,
          procedure
          begin
            OnComplete(Success, SearchResult.Cards.ToArray,
              SearchResult.HasMore,SearchResult.TotalCards, ErrorMsg);
            SearchResult.Free; // free it on the main thread
          end);

      except
        // ensure we eventually free the allocated object
        on E: Exception do
        begin
          SearchResult.Free;
          raise;
        end;
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

    // If we exit the loop normally, it means we never hit a success or raised
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
  URL: string;
  ResponseStr: string;
begin
  // 1) Check for internet
  if not IsInternetAvailable then
    raise EScryfallAPIError.Create('No internet connection available.');

  // 2) Build full URL
  if Endpoint.StartsWith('http') then
    URL := Endpoint
  else
  begin
    if not BaseUrl.EndsWith('/') then
      URL := BaseUrl + '/'
    else
      URL := BaseUrl;

    URL := URL + Endpoint.TrimLeft(['/']);
  end;

  // 3) Perform the request (GET or POST) with retries
  ResponseStr := MakeRequestWithRetries(URL, Payload);

  // 4) Parse the JSON text
  try
    Result := TJsonObject.Parse(ResponseStr) as TJsonObject;
  except
    on E: Exception do
      raise EScryfallAPIError.CreateFmt('Error parsing JSON: %s', [E.Message]);
  end;
end;

function TScryfallAPI.ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;
var
  CardsArray: TJSONArray;
  Stopwatch: TStopwatch;
  LResult: TSearchResult;            // Final TSearchResult to return
  LocalCards: TList<TCardDetails>;   // Accumulate cards in parallel, then sync once
begin
  // TSearchResult to start
  LResult := TSearchResult.Create;

  // If there's no JSON at all, just return empty
  if not Assigned(JsonResponse) then
    Exit(LResult);

  // 1) Check if Scryfall returned an error object (HTTP 200 but "object": "error")
  if JsonResponse.S['object'] = 'error' then
  begin
    // Collect fields from Scryfall's error JSON
    var Code    := JsonResponse.S['code'];
    var Status  := JsonResponse.I['status'];
    var Details := JsonResponse.S['details'];

    // Raise an exception with that info
    raise EScryfallAPIError.CreateFmt(
      'Scryfall error: %s (code: %s, status: %d)',
      [Details, Code, Status]
    );
  end;

  try
    // Scryfall puts cards in the "data" array if it’s a valid search result
    if JsonResponse.Contains(FieldData) then
    begin
      CardsArray := JsonResponse.A[FieldData];
      LogStuff('ParseSearchResult -> Data Array Count: ' + CardsArray.Count.ToString, DEBUG);

      // Clear TSearchResult’s list in case there’s leftover data
      LResult.Cards.Clear;

      // measure how long it takes to parse
      Stopwatch := TStopwatch.StartNew;

      // Prepare a local list to store TCardDetails in the parallel loop
      LocalCards := TList<TCardDetails>.Create;
      try
        // Parallel parse: create TCardDetails for each JSON element
        TParallel.For(0, CardsArray.Count - 1,
          procedure(Index: Integer)
          var
            CardObj : TJsonObject;
            TempCard: TCardDetails;
          begin
            // Only parse objects
            if CardsArray.Types[Index] = jdtObject then
            begin
              CardObj := CardsArray.O[Index];
              TempCard := TCardDetails.Create;
              TWrapperHelper.FillCardDetailsFromJson(CardObj, TempCard);

              // Lock the local list for thread-safety
              TMonitor.Enter(LocalCards);
              try
                LocalCards.Add(TempCard);
              finally
                TMonitor.Exit(LocalCards);
              end;
            end
            else
            begin
              LogStuff(
                Format('Skipping non-object element at index %d', [Index]),
                WARNING
              );
            end;
          end
        );

        // Synchronize once to merge all parsed cards into LResult.Cards
        TThread.Synchronize(nil,
          procedure
          begin
            LResult.Cards.AddRange(LocalCards);
          end);

      finally
        LocalCards.Free;
      end;

      Stopwatch.Stop;
      LogStuff(Format('Parallel Parsing Time: %d ms', [Stopwatch.ElapsedMilliseconds]), DEBUG);

      // Fill TSearchResult metadata
      LResult.HasMore     := JsonResponse.B[FieldHasMore];
      LResult.NextPageURL := JsonResponse.S[FieldNextPage];
      LResult.TotalCards  := JsonResponse.I[FieldTotalCards];

      Result := LResult;
    end
    else
    begin
      // If there's no "data" field, log an error but return an empty TSearchResult
      LogStuff('ParseSearchResult -> Missing "data" key in JSON response.', ERROR);
      Result := LResult;
    end;
  except
    on E: Exception do
    begin
      // If something fails, free LResult (avoid memory leak) and re-raise
      LResult.Free;
      raise;
    end;
  end;
end;


function TScryfallAPI.InternalSearchCards(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
var
  CacheKey, SearchUrl: string;
  CachedResponse, JsonResponse: TJsonObject;
begin
  // Build a unique key for this search
  CacheKey := Format('%s:%s:%s:%s:%d:%d', [Query, SetCode, Rarity, Colors, Page,
    Ord(Unique)]);

  // Check local memory cache
  TMonitor.Enter(FCache);
  try
    if FCache.TryGetValue(CacheKey, CachedResponse) then
      Exit(ParseSearchResult(CachedResponse));
  finally
    TMonitor.Exit(FCache);
  end;

  // Construct the actual search URL
  SearchUrl := TWrapperHelper.ConstructSearchUrl(Query, SetCode, Rarity, Colors,
    Fuzzy, Unique, Page);

  // Execute
  JsonResponse := ExecuteRequest(SearchUrl);
  try
    Result := ParseSearchResult(JsonResponse);

    // Store in cache
    TMonitor.Enter(FCache);
    try
      FCache.Add(CacheKey, JsonResponse.Clone as TJsonObject);
    finally
      TMonitor.Exit(FCache);
    end;
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
  if GetCachedResult(CacheKey, JsonResponse) then
  begin
    LogStuff('Cache hit for query: ' + Query.BuildQuery);
    Exit(ParseSearchResult(JsonResponse));
  end;

  // Build URL
  if BaseUrl.EndsWith('/') then
    URL := BaseUrl + 'cards/search?q=' + Query.BuildQuery
  else
    URL := BaseUrl + '/cards/search?q=' + Query.BuildQuery;

  LogStuff('Executing query: ' + URL);

  JsonResponse := ExecuteRequest(URL);
  try
    Result := ParseSearchResult(JsonResponse);
    CacheResult(CacheKey, JsonResponse.Clone as TJsonObject);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCachedResult(const CacheKey: string;
out JsonResponse: TJsonObject): Boolean;
begin
  FCacheLock.Enter;
  try
    Result := FCache.TryGetValue(CacheKey, JsonResponse);
  finally
    FCacheLock.Leave;
  end;
end;

procedure TScryfallAPI.CacheResult(const CacheKey: string;
const JsonResponse: TJsonObject);
begin
  FCacheLock.Enter;
  try
    // If cache is full, remove one entry
    if FCache.Count >= MaxCacheSize then
    begin
      var
      FirstKey := '';
      for var Key in FCache.Keys do
      begin
        FirstKey := Key;
        Break;
      end;
      if FirstKey <> '' then
      begin
        FCache[FirstKey].Free;
        FCache.Remove(FirstKey);
      end;
    end;

    FCache.AddOrSetValue(CacheKey, JsonResponse);
  finally
    FCacheLock.Leave;
  end;
end;

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
        TWrapperHelper.FillSetDetailsFromJson(SetsArray.O[i], Result[i]);
      end;
    end
    else
      raise EScryfallAPIError.Create('API response is missing set data.');
  finally
    JsonResponse.Free;
  end;
end;

end.
