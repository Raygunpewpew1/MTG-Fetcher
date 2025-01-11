﻿unit ScryfallData;

interface


uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Threading, JsonDataObjects, SGlobalsZ,
  System.Net.HttpClient,Logger,System.SyncObjs, MLogic, system.IOUtils,ScryfallTypes, ScryfallQueryBuilder;

type
  EScryfallAPIError = class(Exception);

  TOnSearchComplete = reference to procedure(Success: Boolean;
    Cards: TArray<TCardDetails>; HasMore: Boolean; ErrorMsg: string);

  TScryfallAPI = class
  private
    FCache: TDictionary<string, TJsonObject>;
//    FSetDetailsCache: TDictionary<string, TSetDetails>;
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
    function FetchAllSetsFromAPI: TArray<TSetDetails>;

    function ExecuteQuery(const Query: TScryfallQuery): TSearchResult;
    function GetCachedResult(const CacheKey: string; out JsonResponse: TJsonObject): Boolean;
    procedure CacheResult(const CacheKey: string; const JsonResponse: TJsonObject);


  public
    constructor Create;
    destructor Destroy; override;



    function GetSetByCode(const SetCode: string): TSetDetails;
    function GetCardByName(const CardName: string; Fuzzy: Boolean = False)
      : TCardDetails;
    function SearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer = 1): TArray<TCardDetails>;
    function GetAllSets: TArray<TSetDetails>;
    function SearchAllCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean): TArray<TCardDetails>;
    procedure SearchAllCardsAsync(const Query: TScryfallQuery; const OnComplete: TOnSearchComplete); overload;
    procedure SearchAllCardsAsync(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer; Callback: TOnSearchComplete); overload;
    function GetRandomCard: TCardDetails;
    function GetCatalog(const CatalogName: string): TScryfallCatalog;
    function FetchAllCatalogs: TDictionary<string, TScryfallCatalog>;
    function GetCardByUUID(const UUID: string): TCardDetails;
    function GetCardImage(const UUID: string;
      const ImageType: string = 'normal'): string;
    function GetCardImageUris(const UUID: string): TImageUris;
    function GetCardByArenaID(const UArenaID: Integer): TCardDetails;
    function SearchWithQuery(Query: TScryfallQuery): TArray<TCardDetails>;
    procedure SearchWithQueryAsync(Query: TScryfallQuery; const OnComplete: TOnSearchComplete);
    function CreateQuery: TScryfallQuery;
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
  WrapperHelper, APIConstants, System.NetEncoding, CardDisplayHelpers;

{ TScryfallAPI }

constructor TScryfallAPI.Create;
begin
  inherited Create;
  FCache := TDictionary<string, TJsonObject>.Create;
  FCacheLock := TCriticalSection.Create;
 // FSetDetailsCache := TDictionary<string, TSetDetails>.Create;

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
//  FSetDetailsCache.Free;

  inherited Destroy;
end;

function TScryfallAPI.CreateQuery: TScryfallQuery;
begin
  Result := TScryfallQuery.Create;
end;

function TScryfallAPI.ExecuteQuery(const Query: TScryfallQuery): TSearchResult;
var
  CacheKey: string;
  JsonResponse: TJsonObject;
  URL: string;
begin
  CacheKey := Query.ToCacheKey;

  // Check cache first
  if GetCachedResult(CacheKey, JsonResponse) then
  begin
    LogStuff('Cache hit for query: ' + Query.BuildQuery);
    Result := ParseSearchResult(JsonResponse);
    Exit;
  end;

  // Ensure BaseUrl is not duplicated
  if BaseUrl.EndsWith('/') then
    URL := BaseUrl + 'cards/search?q=' + Query.BuildQuery
  else
    URL := BaseUrl + '/cards/search?q=' + Query.BuildQuery;

  LogStuff('Executing query: ' + URL);

  JsonResponse := ExecuteRequest(URL);
  try
    Result := ParseSearchResult(JsonResponse);

    // Cache the result
    CacheResult(CacheKey, JsonResponse.Clone as TJsonObject);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCachedResult(const CacheKey: string; out JsonResponse: TJsonObject): Boolean;
begin
  FCacheLock.Enter;
  try
    Result := FCache.TryGetValue(CacheKey, JsonResponse);
  finally
    FCacheLock.Leave;
  end;
end;

procedure TScryfallAPI.CacheResult(const CacheKey: string; const JsonResponse: TJsonObject);
begin
  FCacheLock.Enter;
  try
    // Add to cache, potentially removing oldest entry if cache is full
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

function TScryfallAPI.SearchWithQuery(Query: TScryfallQuery): TArray<TCardDetails>;
var
  SearchResult: TSearchResult;
begin
  SearchResult := ExecuteQuery(Query);
  Result := SearchResult.Cards;
end;

procedure TScryfallAPI.SearchWithQueryAsync(Query: TScryfallQuery; const OnComplete: TOnSearchComplete);
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
          LogStuff('Query execution error: ' + E.Message);
        end;
      end;

      TThread.Queue(nil,
        procedure
        begin
          OnComplete(Success, SearchResult.Cards, SearchResult.HasMore, ErrorMsg);
        end);
    end);
end;

procedure TScryfallAPI.SearchAllCardsAsync(const Query: TScryfallQuery;
  const OnComplete: TOnSearchComplete);
begin
  SearchWithQueryAsync(Query, OnComplete);
end;

function TScryfallAPI.ExecuteRequest(const Endpoint: string): TJsonObject;
const
  MaxRetries = 3;
  RetryDelayMs = 500; // 500ms delay between retries
var
  ResponseStream: TStringStream;
  StatusCode, RetryCount: Integer;
  URL: string;
begin
  // Ensure Endpoint is not a full URL
  if Endpoint.StartsWith('http') then
    URL := Endpoint
  else if BaseUrl.EndsWith('/') then
    URL := BaseUrl + Endpoint
  else
    URL := BaseUrl + '/' + Endpoint;

  ResponseStream := TStringStream.Create;
  try
    for RetryCount := 1 to MaxRetries do
    begin
      ResponseStream.Clear;
      LogStuff(Format('ExecuteRequest -> URL: %s (attempt %d)', [URL, RetryCount]));

      // Lock HTTP client for thread safety
      TMonitor.Enter(FHttpClient);
      try
        StatusCode := FHttpClient.Get(URL, ResponseStream).StatusCode;
      finally
        TMonitor.Exit(FHttpClient);
      end;

      // Handle response based on status code
      case StatusCode of
        200:
          begin
            // Success: Parse JSON and return
            try
              Result := TJsonObject.Parse(ResponseStream.DataString) as TJsonObject;
              Exit; // Exit immediately if successful
            except
              on E: Exception do
                raise EScryfallAPIError.CreateFmt('Error parsing JSON: %s', [E.Message]);
            end;
          end;
        429:
          begin
            // Too Many Requests: Log and retry after delay
            LogStuff(Format('Rate limited (429). Retrying in %d ms...', [RetryDelayMs]));
          end;
        500..599:
          begin
            // Server error: Log and retry
            LogStuff(Format('Server error (%d). Retrying in %d ms...', [StatusCode, RetryDelayMs]));
          end;
        else
          begin
            // For other status codes, raise an error without retrying
            raise EScryfallAPIError.CreateFmt('Request failed: %d %s', [StatusCode, URL]);
          end;
      end;

      // Delay before retrying
      if RetryCount < MaxRetries then
        Sleep(RetryDelayMs);
    end;

    // If all retries fail, raise an error
    raise EScryfallAPIError.CreateFmt('Max retries reached for URL: %s', [URL]);
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

procedure TScryfallAPI.SearchAllCardsAsync(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer; Callback: TOnSearchComplete);
var
  QueryBuilder: TScryfallQuery;
begin
  QueryBuilder := CreateQuery;
  try
    QueryBuilder.WithName(Query, Fuzzy)
                .WithSet(SetCode)
                .WithRarity(StringToRarity(Rarity))
                .WithColors(Colors)
                .SetPage(Page);

    if Unique then
      QueryBuilder.Unique('prints');

    SearchWithQueryAsync(QueryBuilder, Callback);
  except
    QueryBuilder.Free;
    raise;
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

function TScryfallAPI.FetchAllSetsFromAPI: TArray<TSetDetails>;
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
      raise EScryfallAPIError.Create('API response is missing set data.');
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetAllSets: TArray<TSetDetails>;
var
  CacheFile: string;
begin
  CacheFile := GetCacheFilePath(SetCacheFile);
  if TFile.Exists(CacheFile) then
  begin
    LogStuff('Loading all sets from local cache.');
    Exit(LoadSetDetailsFromJson(CacheFile));
  end;

  LogStuff('Fetching all sets from Scryfall API.');
  Result := FetchAllSetsFromAPI;
  SaveSetDetailsToJson(CacheFile, Result); // Cache results for future use
end;

function TScryfallAPI.GetSetByCode(const SetCode: string): TSetDetails;
var
  CachedSets: TArray<TSetDetails>;
  JsonResponse: TJsonObject;
  Endpoint: string;
begin
  // Try loading from the local JSON cache
  CachedSets := LoadSetDetailsFromJson(GetCacheFilePath(SetCacheFile));
  for var SetDetails in CachedSets do
  begin
    if SetDetails.Code = SetCode then
    begin
      LogStuff(Format('Set details for "%s" loaded from cache.', [SetCode]));
      Exit(SetDetails);
    end;
  end;

  // Fetch from the Scryfall API if not found in the cache
  Endpoint := Format('%s%s', [EndpointSets, TNetEncoding.URL.Encode(SetCode)]);
  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
    begin
      FillSetDetailsFromJson(JsonResponse, Result);
      LogStuff(Format('Set details for "%s" fetched from API.', [SetCode]));

      // Save the fetched details to the cache
      SaveSetDetailsToJson(GetCacheFilePath(SetCacheFile), [Result]);
    end
    else
      raise EScryfallAPIError.CreateFmt('No data found for set code "%s".', [SetCode]);
  finally
    JsonResponse.Free; // Ensure JsonResponse is freed
  end;
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
    CatalogKeywordActions, CatalogAbilityWords, CatalogWordBank);

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