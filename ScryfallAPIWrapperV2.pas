unit ScryfallAPIWrapperV2;

interface


uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, JsonDataObjects, SGlobalsZ,
  System.Net.HttpClient,Logger,System.SyncObjs;

type
  EScryfallAPIError = class(Exception);

  TOnSearchComplete = reference to procedure(Success: Boolean;
    Cards: TArray<TCardDetails>; HasMore: Boolean; ErrorMsg: string);

  TScryfallAPI = class
  private
    FCache: TDictionary<string, TJsonObject>;
    FSetDetailsCache: TDictionary<string, TSetDetails>;
    FCacheLock: TCriticalSection;

    // Single THTTPClient for all requests (thread-synchronized).
    FHttpClient: THTTPClient;

    // In-memory cache for autocomplete results.
    FAutocompleteCache: TDictionary<string, TArray<string>>;

    function ExecuteRequest(const Endpoint: string): TJsonObject;
    function ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;
    function InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
    function ParseImageUris(const JsonObj: TJsonObject): TImageUris;

    /// <summary>
    /// Low-level method to fetch autocomplete suggestions from Scryfall's endpoint:
    /// GET /cards/autocomplete?q=PartialQuery
    /// </summary>
    function FetchAutocompleteSuggestions(const PartialQuery: string)
      : TArray<string>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure PreloadAllSets;
    function GetSetByCode(const SetCode: string): TSetDetails;
    function GetCardByName(const CardName: string; Fuzzy: Boolean = False)
      : TCardDetails;
    function SearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer = 1): TArray<TCardDetails>;
    function GetAllSets: TArray<TSetDetails>;
    function SearchAllCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean): TArray<TCardDetails>;
    procedure SearchAllCardsAsync(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer; Callback: TOnSearchComplete);
    function GetRandomCard: TCardDetails;
    function GetCreatureTypes: TScryfallCatalog;
    function GetCatalog(const CatalogName: string): TScryfallCatalog;
    function FetchAllCatalogs: TDictionary<string, TScryfallCatalog>;
    function GetCardByUUID(const UUID: string): TCardDetails;
    function GetCardImage(const UUID: string;
      const ImageType: string = 'normal'): string;
    function GetCardImageUris(const UUID: string): TImageUris;
    function GetCardByArenaID(const UArenaID: Integer): TCardDetails;

    /// <summary>
    /// Returns the autocomplete suggestions for a partial query, using
    /// an internal TDictionary-based cache to avoid redundant calls.
    /// If the partial query is too short, returns an empty array.
    /// UI-level debounce or throttling is your responsibility.
    /// </summary>
    function AutocompleteCards(const PartialQuery: string): TArray<string>;
  end;

implementation

uses
  WrapperHelper, APIConstants, System.NetEncoding;

{ TScryfallAPI }

constructor TScryfallAPI.Create;
begin
  inherited Create;
  FCache := TDictionary<string, TJsonObject>.Create;
  FCacheLock := TCriticalSection.Create;
  FSetDetailsCache := TDictionary<string, TSetDetails>.Create;

  // Create the single THTTPClient instance once
  FHttpClient := THTTPClient.Create;
  FHttpClient.CustomHeaders['User-Agent'] := UserAgent;
  FHttpClient.CustomHeaders['Accept'] := AcceptHeader;

  // Initialize optional autocomplete cache
  FAutocompleteCache := TDictionary < string, TArray < string >>.Create;
end;

destructor TScryfallAPI.Destroy;
begin
  // Free FAutocompleteCache safely
  FCacheLock.Enter; // Lock before accessing the cache
  try
    FAutocompleteCache.Free;
    FAutocompleteCache := nil; // Avoid dangling pointer
  finally
    FCacheLock.Leave; // Unlock before releasing the critical section
  end;

  // Free other resources
  FCacheLock.Free;  // Free the critical section
  FHttpClient.Free;
  FCache.Free;
  FSetDetailsCache.Free;

  inherited Destroy;
end;

function TScryfallAPI.ExecuteRequest(const Endpoint: string): TJsonObject;
const
  MaxRetries = 3;
  RetryDelayMs = 500;
var
  ResponseStream: TStringStream;
  Response: string;
  StatusCode, RetryCount: Integer;
  URL: string;
begin
  Result := nil;
  URL := BaseUrl + Endpoint;
  ResponseStream := TStringStream.Create;
  try
    for RetryCount := 1 to MaxRetries do
    begin
      ResponseStream.Clear;
      LogStuff(Format('ExecuteRequest -> URL: %s (attempt %d)',
        [URL, RetryCount]));

      // Because we share one THTTPClient, we lock around its usage
      TMonitor.Enter(FHttpClient);
      try
        StatusCode := FHttpClient.Get(URL, ResponseStream).StatusCode;
      finally
        TMonitor.Exit(FHttpClient);
      end;

      if StatusCode = 200 then
      begin
        // Success: parse JSON
        Response := ResponseStream.DataString;
        try
          Result := TJsonObject.Parse(Response) as TJsonObject;
          Exit; // returns the parsed JSON
        except
          on E: Exception do
            raise EScryfallAPIError.CreateFmt('Error parsing JSON: %s',
              [E.Message]);
        end;
      end
      else if StatusCode = 404 then
      begin
        // Not found
        raise EScryfallAPIError.Create(ErrorCardNotFound);
      end
      else
      begin
        // Some other status code
        LogStuff(Format('API returned status %d. Retrying...', [StatusCode]));
        if RetryCount = MaxRetries then
          raise EScryfallAPIError.CreateFmt(ErrorRequestFailed,
            [StatusCode, URL]);
        Sleep(RetryDelayMs);
      end;
    end;

  finally
    ResponseStream.Free;
  end;
end;

// ------------------------------------
// Autocomplete Implementation
// ------------------------------------

function TScryfallAPI.FetchAutocompleteSuggestions(const PartialQuery: string): TArray<string>;
var
  URL: string;
  JsonResponse: TJsonObject;
  DataArray: TJsonArray;
  i: Integer;
begin
  // Initialize Result as an empty array
  Result := TArray<string>.Create();

  // Build the URL for the autocomplete endpoint
  URL := Format('cards/autocomplete?q=%s', [TNetEncoding.URL.Encode(PartialQuery)]);
  LogStuff(Format('Fetching autocomplete suggestions for query: "%s"', [PartialQuery]));

  // Call the Scryfall API
  JsonResponse := ExecuteRequest(URL);
  try
    // Check if 'data' exists and is an array
    if JsonResponse.Contains('data') then
    begin
      DataArray := JsonResponse.A['data'];
      if Assigned(DataArray) then
      begin
        SetLength(Result, DataArray.Count);
        for i := 0 to DataArray.Count - 1 do
        begin
          // Safely access each element as a string
          Result[i] := DataArray.S[i];
          LogStuff(Format('Suggestion [%d]: %s', [i, Result[i]]));
        end;
      end
      else
      begin
        LogStuff('Autocomplete response "data" is not an array.');
      end;
    end
    else
    begin
      LogStuff('Autocomplete response missing "data" array.');
    end;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.AutocompleteCards(const PartialQuery: string): TArray<string>;
var
  FirstKey: string;
begin
  if PartialQuery.Length < 2 then
  begin
    LogStuff(Format('Query "%s" is too short. Returning empty suggestions.', [PartialQuery]));
    Exit(TArray<string>.Create());
  end;

  // Check cache
  FCacheLock.Enter;
  try
    if FAutocompleteCache.TryGetValue(PartialQuery, Result) then
    begin
      LogStuff(Format('Cache hit for query: "%s"', [PartialQuery]));
      Exit;
    end
    else
    begin
      LogStuff(Format('Cache miss for query: "%s". Fetching from API.', [PartialQuery]));
    end;
  finally
    FCacheLock.Leave;
  end;

  // Fetch suggestions
  Result := FetchAutocompleteSuggestions(PartialQuery);

  // Update cache
  FCacheLock.Enter;
  try
    FAutocompleteCache.AddOrSetValue(PartialQuery, Result);
    LogStuff(Format('Cached suggestions for query: "%s"', [PartialQuery]));

    // Evict oldest entry if cache exceeds the limit
    if FAutocompleteCache.Count > MaxAutocompleteCacheEntries then
    begin
      for FirstKey in FAutocompleteCache.Keys do
      begin
        LogStuff(Format('Evicting cache entry: "%s"', [FirstKey]));
        FAutocompleteCache.Remove(FirstKey);
        Break; // Remove only one entry
      end;
    end;
  finally
    FCacheLock.Leave;
  end;
end;

function TScryfallAPI.ParseSearchResult(const JsonResponse: TJsonObject)
  : TSearchResult;
var
  CardsArray: TJsonArray;
  i: Integer;
  CardObj: TJsonObject;
begin
  SetLength(Result.Cards, 0);
  Result.HasMore := False;
  Result.NextPageURL := '';
  Result.TotalCards := 0;

  if not Assigned(JsonResponse) then
    Exit;

  try
    if JsonResponse.Contains(FieldData) then
    begin
      CardsArray := JsonResponse.A[FieldData];
      LogStuff('ParseSearchResult -> Data Array Count: ' +
        CardsArray.Count.ToString);
      SetLength(Result.Cards, CardsArray.Count);

      for i := 0 to CardsArray.Count - 1 do
      begin
        try
          if CardsArray.Types[i] = jdtObject then
          begin
            CardObj := CardsArray.O[i];
            FillCardDetailsFromJson(CardObj, Result.Cards[i]);
          end
          else
          begin
            LogStuff(Format('Skipping non-object element at index %d', [i]));
            Result.Cards[i].Clear;
          end;
        except
          on E: Exception do
          begin
            LogStuff(Format('Error parsing card at index %d: %s',
              [i, E.Message]));
            Result.Cards[i].Clear;
          end;
        end;
      end;
    end
    else
    begin
      LogStuff(ErrorMissingDataKey);
      Exit;
    end;

    Result.HasMore := JsonResponse.B[FieldHasMore];
    Result.NextPageURL := JsonResponse.S[FieldNextPage];
    Result.TotalCards := JsonResponse.i[FieldTotalCards];

  except
    on E: Exception do
    begin
      LogStuff('Error in ParseSearchResult: ' + E.Message);
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
  CacheKey := Format('%s:%s:%s:%s:%d:%d', [Query, SetCode, Rarity, Colors, Page,
    Ord(Unique)]);

  TMonitor.Enter(FCache);
  try
    if FCache.TryGetValue(CacheKey, CachedResponse) then
      Exit(ParseSearchResult(CachedResponse));
  finally
    TMonitor.Exit(FCache);
  end;

  SearchUrl := ConstructSearchUrl(Query, SetCode, Rarity, Colors, Fuzzy,
    Unique, Page);
  JsonResponse := ExecuteRequest(SearchUrl);
  try
    Result := ParseSearchResult(JsonResponse);

    // Store a clone in the cache
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

function TScryfallAPI.GetRandomCard: TCardDetails;
var
  JsonResponse: TJsonObject;
begin
  JsonResponse := ExecuteRequest(EndpointRandomCard);
  try
    FillCardDetailsFromJson(JsonResponse, Result);
  finally
    JsonResponse.Free;
  end;
end;

procedure TScryfallAPI.SearchAllCardsAsync(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer;
  Callback: TOnSearchComplete);
begin
  TTask.Run(
    procedure
    var
      SearchResult: TSearchResult;
      ErrorMsg: string;
      Success: Boolean;
    begin
      try
        SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors,
          Fuzzy, Unique, Page);
        Success := True;
        ErrorMsg := '';
      except
        on E: Exception do
        begin
          Success := False;
          ErrorMsg := E.Message;
          SearchResult.Cards := nil;
          SearchResult.HasMore := False;
        end;
      end;

      TThread.Synchronize(nil,
        procedure
        begin
          Callback(Success, SearchResult.Cards, SearchResult.HasMore, ErrorMsg);
        end);
    end);
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

function TScryfallAPI.GetAllSets: TArray<TSetDetails>;
var
  JsonResponse: TJsonObject;
  SetsArray: TJsonArray;
  i: Integer;
begin
  JsonResponse := ExecuteRequest(EndpointSets);
  try
    if JsonResponse.Contains(FieldData) then
    begin
      SetsArray := JsonResponse.A[FieldData];
      SetLength(Result, SetsArray.Count);

      for i := 0 to SetsArray.Count - 1 do
        FillSetDetailsFromJson(SetsArray.O[i], Result[i]);
    end
    else
      SetLength(Result, 0);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetSetByCode(const SetCode: string): TSetDetails;
var
  Endpoint: string;
  JsonResponse: TJsonObject;
begin
  if FSetDetailsCache.TryGetValue(SetCode, Result) then
  begin
    LogStuff(Format(LogCacheHit, [SetCode]));
    Exit;
  end;

  TMonitor.Enter(FSetDetailsCache);
  try
    if FSetDetailsCache.TryGetValue(SetCode, Result) then
    begin
      LogStuff(Format(LogCacheHitDoubleCheck, [SetCode]));
      Exit;
    end;

    Endpoint := Format('%s%s',
      [EndpointSets, TNetEncoding.URL.Encode(SetCode)]);
    LogStuff(Format(LogFetchingSetCode, [SetCode]));

    JsonResponse := ExecuteRequest(Endpoint);
    try
      if Assigned(JsonResponse) then
      begin
        FillSetDetailsFromJson(JsonResponse, Result);
        FSetDetailsCache.Add(SetCode, Result);
        LogStuff(Format(LogSetCodeAddedToCache, [SetCode]));
      end
      else
        raise EScryfallAPIError.CreateFmt(ErrorNoDataForSetCode, [SetCode]);
    finally
      JsonResponse.Free;
    end;
  finally
    TMonitor.Exit(FSetDetailsCache);
  end;
end;

procedure TScryfallAPI.PreloadAllSets;
var
  AllSets: TArray<TSetDetails>;
  S: TSetDetails;
begin
  AllSets := GetAllSets;
  for S in AllSets do
    FSetDetailsCache.AddOrSetValue(S.Code, S);
end;

function TScryfallAPI.GetCardByName(const CardName: string; Fuzzy: Boolean)
  : TCardDetails;
var
  Endpoint: string;
  JsonResponse: TJsonObject;
begin
  if Fuzzy then
    Endpoint := Format('%s?fuzzy=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(CardName)])
  else
    Endpoint := Format('%s?exact=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(CardName)]);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    FillCardDetailsFromJson(JsonResponse, Result);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCreatureTypes: TScryfallCatalog;
var
  JsonResponse: TJsonObject;
  CatalogArray: TJsonArray;
  i: Integer;
begin
  Result.Clear;

  JsonResponse := ExecuteRequest(EndPointCreatureTypes);
  try
    if JsonResponse.Contains(FieldData) and
      (JsonResponse.Types[FieldData] = jdtArray) then
    begin
      CatalogArray := JsonResponse.A[FieldData];
      SetLength(Result.Data, CatalogArray.Count);

      for i := 0 to CatalogArray.Count - 1 do
        if CatalogArray.Types[i] = jdtString then
          Result.Data[i] := CatalogArray.S[i];

      Result.TotalItems := Length(Result.Data);
    end
    else
      raise EScryfallAPIError.Create
        ('Invalid response: "data" field not found.');
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCatalog(const CatalogName: string): TScryfallCatalog;
var
  JsonResponse: TJsonObject;
  DataArray: TJsonArray;
  i: Integer;
begin
  Result.Clear;
  Result.Name := CatalogName;

  JsonResponse := ExecuteRequest(Format('catalog/%s', [CatalogName]));
  try
    if JsonResponse.Contains('data') and (JsonResponse.Types['data'] = jdtArray)
    then
    begin
      DataArray := JsonResponse.A['data'];
      SetLength(Result.Data, DataArray.Count);
      for i := 0 to DataArray.Count - 1 do
        Result.Data[i] := DataArray.S[i];
    end;

    if JsonResponse.Contains('total_items') then
      Result.TotalItems := JsonResponse.i['total_items'];
    if JsonResponse.Contains('uri') then
      Result.Uri := JsonResponse.S['uri'];
    if JsonResponse.Contains('object') then
      Result.ObjectType := JsonResponse.S['object'];
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.FetchAllCatalogs: TDictionary<string, TScryfallCatalog>;
var
  CatalogNames: TArray<string>;
  CatalogDict: TDictionary<string, TScryfallCatalog>;
  CatalogName: string;
begin
  CatalogNames := TArray<string>.Create(CatalogCreatureTypes,
    CatalogPlaneswalkerTypes, CatalogArtifactTypes, CatalogEnchantmentTypes,
    CatalogLandTypes, CatalogSpellTypes, CatalogPowers, CatalogToughnesses,
    CatalogLoyalties, CatalogWatermarks, CatalogKeywordAbilities,
    CatalogKeywordActions, CatalogAbilityWords);

  CatalogDict := TDictionary<string, TScryfallCatalog>.Create;
  try
    for CatalogName in CatalogNames do
      CatalogDict.Add(CatalogName, GetCatalog(CatalogName));

    Result := CatalogDict;
  except
    CatalogDict.Free;
    raise;
  end;
end;

function TScryfallAPI.ParseImageUris(const JsonObj: TJsonObject): TImageUris;
begin
  Result.Clear;
  if (JsonObj = nil) or not JsonObj.Contains('image_uris') then
    Exit;

  with JsonObj.O['image_uris'] do
  begin
    if Contains('small') then
      Result.Small := S['small'];
    if Contains('normal') then
      Result.Normal := S['normal'];
    if Contains('large') then
      Result.Large := S['large'];
    if Contains('png') then
      Result.Png := S['png'];
    if Contains('art_crop') then
      Result.Art_crop := S['art_crop'];
    if Contains('border_crop') then
      Result.Border_crop := S['border_crop'];
  end;
end;

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
    FillCardDetailsFromJson(JsonResponse, Result);
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
    FillCardDetailsFromJson(JsonResponse, Result);
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
  LowerImageType: string;
begin
  Result := '';
  ImageUris := GetCardImageUris(UUID);
  LowerImageType := LowerCase(ImageType);

  if LowerImageType = 'small' then
    Result := ImageUris.Small
  else if LowerImageType = 'normal' then
    Result := ImageUris.Normal
  else if LowerImageType = 'large' then
    Result := ImageUris.Large
  else if LowerImageType = 'png' then
    Result := ImageUris.Png
  else if LowerImageType = 'art_crop' then
    Result := ImageUris.Art_crop
  else if LowerImageType = 'border_crop' then
    Result := ImageUris.Border_crop
  else
    Result := ImageUris.Normal; // default fallback

  if Result.IsEmpty then
    raise EScryfallAPIError.CreateFmt
      ('Image type "%s" not available for card with UUID: %s',
      [ImageType, UUID]);
end;

end.
