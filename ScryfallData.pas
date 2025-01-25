unit ScryfallData;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Threading,
  JsonDataObjects,
  SGlobalsZ,
  System.Net.HttpClient,
  Logger,
  System.SyncObjs,
  MLogic,
  System.IOUtils,
  ScryfallTypes,
  ScryfallQuery,
  System.Diagnostics;

type
  EScryfallAPIError        = class(Exception);
  EScryfallRateLimitError  = class(EScryfallAPIError);
  EScryfallServerError     = class(EScryfallAPIError);

  TOnSearchComplete = reference to procedure(Success: Boolean;
    Cards: TArray<TCardDetails>; HasMore: Boolean; ErrorMsg: string);

  TScryfallAPI = class
  private
    {$REGION 'Private Fields'}
    FCache: TDictionary<string, TJsonObject>;
    FCacheLock: TCriticalSection;
    FHttpClient: THTTPClient;
    FAutocompleteCache: TDictionary<string, TArray<string>>;
    {$ENDREGION}

    {$REGION 'Private Methods'}
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
    procedure CacheResult(const CacheKey: string; const JsonResponse: TJsonObject);

    {$ENDREGION}

  public
    {$REGION 'Construction and Destruction'}
    constructor Create;
    destructor Destroy; override;
    {$ENDREGION}

    {$REGION 'Public Methods / API'}
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
    procedure SearchAllCardsAsync(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer;
      Callback: TOnSearchComplete); overload;

    // Random / Catalog
    function GetRandomCard: TCardDetails;
    function GetCatalog(const CatalogName: string): TScryfallCatalog;
    function FetchAllCatalogs: TDictionary<string, TScryfallCatalog>;

    // Additional
    function GetCardByUUID(const UUID: string): TCardDetails;
    function GetCardByArenaID(const UArenaID: Integer): TCardDetails;
    function GetCardImageUris(const UUID: string): TImageUris;
    function GetCardImage(const UUID: string; const ImageType: string = 'normal'): string;
    function GetCardRulings(const UUID: string): TArray<TRuling>;

    // Query-based
    function SearchWithQuery(Query: TScryfallQuery): TArray<TCardDetails>;
    procedure SearchWithQueryAsync(Query: TScryfallQuery;
      const OnComplete: TOnSearchComplete);
    function FetchCardsCollection(const Identifiers: TJSONArray)
      : TArray<TCardDetails>;

    // Autocomplete
    function AutocompleteCards(const PartialQuery: string): TArray<string>;
    {$ENDREGION}
  end;

implementation

uses
  ScryfallDataHelper,
  APIConstants,
  System.NetEncoding,
  CardDisplayHelpers,
  System.Net.URLClient;

{$REGION 'Construction and Destruction'}

constructor TScryfallAPI.Create;
begin
  inherited Create;

  // Basic dictionary cache to store JSON results
  FCache      := TDictionary<string, TJsonObject>.Create;
  FCacheLock  := TCriticalSection.Create;

  // Single THTTPClient for all requests
  FHttpClient := THTTPClient.Create;
  FHttpClient.CustomHeaders['User-Agent'] := UserAgent;
  FHttpClient.CustomHeaders['Accept']     := AcceptHeader;
  FHttpClient.ConnectionTimeout           := 5000;
  FHttpClient.ResponseTimeout             := 10000;

  // In-memory autocomplete cache
  FAutocompleteCache := TDictionary<string, TArray<string>>.Create;
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

{$ENDREGION}

{$REGION 'Public: Utilities'}

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

{$ENDREGION}

{$REGION 'Public: Core Card/Set Endpoints'}

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
  JsonResponse: TJsonObject;
begin
  if Fuzzy then
    Endpoint := Format('%s?fuzzy=%s', [EndpointNamed, TNetEncoding.URL.Encode(CardName)])
  else
    Endpoint := Format('%s?exact=%s', [EndpointNamed, TNetEncoding.URL.Encode(CardName)]);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    TWrapperHelper.FillCardDetailsFromJson(JsonResponse, Result);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.SearchCards(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): TArray<TCardDetails>;
var
  SearchResult: TSearchResult;
begin
  SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors, Fuzzy,
    Unique, Page);
  Result := SearchResult.Cards;
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
      SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors,
        Fuzzy, Unique, Page);
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


{$ENDREGION}

{$REGION 'Public: Async / Parallel'}

procedure TScryfallAPI.SearchAllCardsAsync(const Query: TScryfallQuery;
  const OnComplete: TOnSearchComplete);
begin
  LogStuff(Format('SearchAllCardsAsync - Query Address: %p, IncludeExtras: %s',
    [Pointer(Query), BoolToStr(Query.Options.IncludeExtras, True)]), DEBUG);

  // Reuse our general-purpose async query method
  SearchWithQueryAsync(Query, OnComplete);
end;

procedure TScryfallAPI.SearchAllCardsAsync(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer;
  Callback: TOnSearchComplete);
var
  Q: TScryfallQuery;
begin
  Q := CreateQuery;
  try
    Q.WithName(Query, Fuzzy)
     .WithSet(SetCode)
     .WithRarity(StringToRarity(Rarity))
     .WithColors(Colors)
     .SetPage(Page)
     .IncludeExtras(False);

    SearchWithQueryAsync(Q, Callback);
  finally
    Q.Free;
  end;
end;

{$ENDREGION}

{$REGION 'Public: Random & Catalog'}

function TScryfallAPI.GetRandomCard: TCardDetails;
var
  JsonResponse: TJsonObject;
begin
  JsonResponse := ExecuteRequest(EndpointRandomCard);
  try
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
begin
  Result.Clear;
  Result.Name := CatalogName;

  JsonResponse := ExecuteRequest(Format('catalog/%s', [CatalogName]));
  try
    if JsonResponse.Contains(FieldData)
       and (JsonResponse.Types[FieldData] = jdtArray) then
    begin
      DataArray := JsonResponse.A[FieldData];
      SetLength(Result.Data, DataArray.Count);

      for i := 0 to DataArray.Count - 1 do
        Result.Data[i] := DataArray.S[i];
    end;

    if JsonResponse.Contains(FieldCount) then
      Result.TotalItems := JsonResponse.I[FieldCount];
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
  CatalogNames := TArray<string>.Create(
    CatalogCreatureTypes,
    CatalogPlaneswalkerTypes,
    CatalogArtifactTypes,
    CatalogEnchantmentTypes,
    CatalogLandTypes,
    CatalogSpellTypes,
    CatalogPowers,
    CatalogToughnesses,
    CatalogLoyalties,
    CatalogWatermarks,
    CatalogKeywordAbilities,
    CatalogKeywordActions,
    CatalogAbilityWords,
    CatalogWordBank
  );

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

{$ENDREGION}

{$REGION 'Public: Additional Card Accessors'}

function TScryfallAPI.GetCardByUUID(const UUID: string): TCardDetails;
var
  Endpoint: string;
  JsonResponse: TJsonObject;
begin
  if UUID.IsEmpty then
    raise EScryfallAPIError.Create('UUID cannot be empty');

  Endpoint := Format('cards/%s', [UUID]);
  JsonResponse := ExecuteRequest(Endpoint);
  try
    TWrapperHelper.FillCardDetailsFromJson(JsonResponse, Result);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardByArenaID(const UArenaID: Integer): TCardDetails;
var
  Endpoint: string;
  JsonResponse: TJsonObject;
begin
  if UArenaID = 0 then
    raise EScryfallAPIError.Create('Arena ID cannot be empty');

  Endpoint := Format('cards/arena/%d', [UArenaID]);
  JsonResponse := ExecuteRequest(Endpoint);
  try
    TWrapperHelper.FillCardDetailsFromJson(JsonResponse, Result);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardImageUris(const UUID: string): TImageUris;
var
  JsonResponse: TJsonObject;
begin
  Result.Clear;
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
  if LowerType = FieldSmall        then Result := ImageUris.Small
  else if LowerType = FieldNormal  then Result := ImageUris.Normal
  else if LowerType = FieldLarge   then Result := ImageUris.Large
  else if LowerType = FieldPng     then Result := ImageUris.Png
  else if LowerType = FieldArtCrop then Result := ImageUris.Art_crop
  else if LowerType = FieldBorderCrop then Result := ImageUris.Border_crop
  else
    Result := ImageUris.Normal; // fallback

  if Result.IsEmpty then
    raise EScryfallAPIError.CreateFmt(
      'Image type "%s" not available for card with UUID: %s',
      [ImageType, UUID]
    );
end;

{$ENDREGION}

{$REGION 'Public: Query-based'}

function TScryfallAPI.SearchWithQuery(Query: TScryfallQuery): TArray<TCardDetails>;
var
  SearchResult: TSearchResult;
begin
  SearchResult := ExecuteQuery(Query);
  Result := SearchResult.Cards;
end;

procedure TScryfallAPI.SearchWithQueryAsync(Query: TScryfallQuery;
  const OnComplete: TOnSearchComplete);
begin
  TTask.Run(
    procedure
    var
      SearchResult: TSearchResult;
      Success: Boolean;
      ErrorMsg: string;
    begin
      try
        SearchResult := ExecuteQuery(Query);
        Success := True;
        ErrorMsg := '';
      except
        on E: Exception do
        begin
          Success := False;
          ErrorMsg := E.Message;
          SearchResult.Cards := nil;
          SearchResult.HasMore := False;
          LogStuff('Query execution error: ' + E.Message, ERROR);
        end;
      end;

      TThread.Queue(nil,
        procedure
        begin
          OnComplete(Success, SearchResult.Cards, SearchResult.HasMore, ErrorMsg);
        end);
    end
  );
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

{$ENDREGION}

{$REGION 'Public: Autocomplete'}

function TScryfallAPI.AutocompleteCards(const PartialQuery: string)
  : TArray<string>;
var
  FirstKey: string;
begin
  if PartialQuery.Length < 2 then
  begin
    LogStuff(
      Format('Query "%s" is too short. Returning empty suggestions.', [PartialQuery])
    );
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
      LogStuff(Format('Cache miss for query: "%s". Fetching from API.', [PartialQuery]));
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

{$ENDREGION}

{$REGION 'Private: Core Execution'}

function TScryfallAPI.ExecuteRequest(const Endpoint: string;
  const Payload: TJsonObject): TJsonObject;
const
  MaxRetries   = 3;
  RetryDelayMs = 500; // milliseconds
var
  ResponseStream, RequestStream: TStringStream;
  StatusCode, RetryCount: Integer;
  URL: string;
begin
  if not IsInternetAvailable then
    raise EScryfallAPIError.Create('No internet connection available.');

  // Build the full URL if needed
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

  ResponseStream := TStringStream.Create;
  RequestStream  := nil;
  try
    // For POST requests
    if Assigned(Payload) then
      RequestStream := TStringStream.Create(Payload.ToString, TEncoding.UTF8);

    for RetryCount := 1 to MaxRetries do
    begin
      ResponseStream.Clear;
      LogStuff(Format('ExecuteRequest -> URL: %s (attempt %d)', [URL, RetryCount]));

      // Lock the HTTPClient for thread safety
      TMonitor.Enter(FHttpClient);
      try
        if Assigned(RequestStream) then
        begin
          RequestStream.Position := 0;
          StatusCode := FHttpClient.Post(
            URL,
            RequestStream,
            ResponseStream,
            [TNameValuePair.Create('Content-Type', 'application/json')]
          ).StatusCode;
        end
        else
        begin
          StatusCode := FHttpClient.Get(URL, ResponseStream).StatusCode;
        end;
      finally
        TMonitor.Exit(FHttpClient);
      end;

      // Check the response
      case StatusCode of
        200:
          begin
            // Parse JSON
            try
              Result := TJsonObject.Parse(ResponseStream.DataString) as TJsonObject;
              Exit; // success
            except
              on E: Exception do
                raise EScryfallAPIError.CreateFmt('Error parsing JSON: %s', [E.Message]);
            end;
          end;

        429:
          raise EScryfallRateLimitError.Create('Rate limit exceeded.');

        500..599:
          raise EScryfallServerError.Create('Server error occurred.');
      else
        // Other non-2xx codes, fail immediately
        raise EScryfallAPIError.CreateFmt('Request failed: %d %s',
          [StatusCode, URL]);
      end;

      // Retry if needed
      if RetryCount < MaxRetries then
        Sleep(RetryDelayMs);
    end;

    raise EScryfallAPIError.CreateFmt('Max retries reached for URL: %s', [URL]);
  finally
    ResponseStream.Free;
    if Assigned(RequestStream) then
      RequestStream.Free;
  end;
end;

function TScryfallAPI.ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;
var
  CardsArray: TJSONArray;
  Stopwatch: TStopwatch;
  LResult: TSearchResult;
begin
  LResult := Default(TSearchResult);

  if not Assigned(JsonResponse) then
    Exit; // Return empty

  try
    if JsonResponse.Contains('data') then
    begin
      CardsArray := JsonResponse.A['data'];
      LogStuff('ParseSearchResult -> Data Array Count: ' + CardsArray.Count.ToString, DEBUG);

      SetLength(LResult.Cards, CardsArray.Count);

      // Time the parsing
      Stopwatch := TStopwatch.StartNew;

      // Parallel parse
      TParallel.For(0, CardsArray.Count - 1,
        procedure(Index: Integer)
        var
          CardObj: TJsonObject;
        begin
          if CardsArray.Types[Index] = jdtObject then
          begin
            CardObj := CardsArray.O[Index];
            ScryfallDataHelper.TWrapperHelper.FillCardDetailsFromJson(CardObj, LResult.Cards[Index]);
          end
          else
          begin
            LogStuff(Format('Skipping non-object element at index %d', [Index]), WARNING);
            LResult.Cards[Index].Clear;
          end;
        end);

      Stopwatch.Stop;
      LogStuff(Format('Parallel Parsing Time: %d ms', [Stopwatch.ElapsedMilliseconds]), DEBUG);

      LResult.HasMore     := JsonResponse.B['has_more'];
      LResult.NextPageURL := JsonResponse.S['next_page'];
      LResult.TotalCards  := JsonResponse.I['total_cards'];

      Result := LResult;
    end
    else
    begin
      LogStuff('ParseSearchResult -> Missing "data" key in JSON response.', ERROR);
    end;
  except
    on E: Exception do
    begin
      LogStuff('Error in ParseSearchResult: ' + E.Message, ERROR);
      raise;
    end;
  end;
end;

function TScryfallAPI.InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
var
  CacheKey, SearchUrl: string;
  CachedResponse, JsonResponse: TJsonObject;
begin
  // Build a unique key for this search
  CacheKey := Format('%s:%s:%s:%s:%d:%d',
    [Query, SetCode, Rarity, Colors, Page, Ord(Unique)]);

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
      var FirstKey := '';
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

{$ENDREGION}

{$REGION 'Private: Images & Autocomplete'}

function TScryfallAPI.ParseImageUris(const JsonObj: TJsonObject): TImageUris;
begin
  Result.Clear;
  if (JsonObj = nil) or not JsonObj.Contains(FieldImageUris) then
    Exit;

  with JsonObj.O[FieldImageUris] do
  begin
    if Contains(FieldSmall)       then Result.Small       := S[FieldSmall];
    if Contains(FieldNormal)      then Result.Normal      := S[FieldNormal];
    if Contains(FieldLarge)       then Result.Large       := S[FieldLarge];
    if Contains(FieldPng)         then Result.Png         := S[FieldPng];
    if Contains(FieldBorderCrop)  then Result.Art_crop    := S[FieldBorderCrop];
    if Contains(FieldArtCrop)     then Result.Border_crop := S[FieldArtCrop];
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
  URL := Format('cards/autocomplete?q=%s', [TNetEncoding.URL.Encode(PartialQuery)]);
  LogStuff(Format('Fetching autocomplete suggestions for query: "%s"', [PartialQuery]));

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
        TWrapperHelper.FillSetDetailsFromJson(SetsArray.O[i], Result[i]);
    end
    else
      raise EScryfallAPIError.Create('API response is missing set data.');
  finally
    JsonResponse.Free;
  end;
end;

{$ENDREGION}

end.

