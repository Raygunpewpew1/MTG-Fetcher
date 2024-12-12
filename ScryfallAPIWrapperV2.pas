unit ScryfallAPIWrapperV2;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.NetEncoding, System.Threading, JsonDataObjects, SGlobalsZ,
  System.Net.HttpClient,
  WrapperHelper, APIConstants;

type
  // Custom exception class for Scryfall API errors
  EScryfallAPIError = class(Exception);

  TOnSearchComplete = reference to procedure(Success: Boolean;
    Cards: TArray<TCardDetails>; HasMore: Boolean; ErrorMsg: string);

  TScryfallAPI = class

  private
    FCache: TDictionary<string, TJsonObject>;
    FSetDetailsCache: TDictionary<string, TSetDetails>;

    function ExecuteRequest(const Endpoint: string): TJsonObject;
    function ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;

    // Parsing methods
//    procedure FillCardDetailsFromJson(const JsonObj: TJsonObject;
//      out CardDetails: TCardDetails);

    function InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;




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
  end;

implementation

{ TScryfallAPI }

constructor TScryfallAPI.Create;
begin
  inherited Create;
  FCache := TDictionary<string, TJsonObject>.Create;
  FSetDetailsCache := TDictionary<string, TSetDetails>.Create;
end;

destructor TScryfallAPI.Destroy;
begin
  FCache.Free;
  FSetDetailsCache.Free;
  inherited;
end;

function TScryfallAPI.ExecuteRequest(const Endpoint: string): TJsonObject;
const
  MaxRetries = 3;
var
  Response: string;
  Client: THTTPClient;
  ResponseStream: TStringStream;
  RetryCount: Integer;
  StatusCode: Integer;
begin
  Result := nil;
  Client := THTTPClient.Create;
  ResponseStream := TStringStream.Create;
  try
    Client.CustomHeaders['User-Agent'] := UserAgent;
    Client.CustomHeaders['Accept'] := AcceptHeader;

    RetryCount := 0;
    repeat
      try
        // GET request
        StatusCode := Client.Get(BaseUrl + Endpoint, ResponseStream).StatusCode;

        // Check the response code
        if StatusCode = 200 then
        begin
          Response := ResponseStream.DataString;
       
          //LogError( Response );
          try
            Result := TJsonObject.Parse(Response) as TJsonObject;
          except
            on E: Exception do
              raise EScryfallAPIError.CreateFmt(
                'Error parsing JSON response: %s', [E.Message]);
          end;
          Exit; // Exit the retry loop on success
        end
        else if StatusCode = 404 then
          raise EScryfallAPIError.Create(ErrorCardNotFound)
        else
          raise EScryfallAPIError.CreateFmt(ErrorRequestFailed,
            [StatusCode, BaseUrl + Endpoint]);

      except
        on E: EScryfallAPIError do
        begin
          Inc(RetryCount);
          LogError(Format('Attempt %d: %s', [RetryCount, E.Message]));
          if RetryCount = MaxRetries then
            raise; // Re-raise after the last retry
          Sleep(500); // Small delay before retrying
        end;
        on E: Exception do
        begin
          LogError('Unexpected error during HTTP request: ' + E.Message);
          raise;
        end;
      end;
    until RetryCount >= MaxRetries;

  finally
    Client.Free;
    ResponseStream.Free;
  end;
end;

function TScryfallAPI.ParseSearchResult(const JsonResponse: TJsonObject)
  : TSearchResult;
var
  CardsArray: TJsonArray;
  I: Integer;
  CardObj: TJsonObject;
begin
  try
    if JsonResponse.Contains(FieldData) then
    begin
      CardsArray := JsonResponse.A[FieldData]; // Access the FieldData array
      LogError('Data Array Count: ' + CardsArray.Count.ToString);
      SetLength(Result.Cards, CardsArray.Count);

      for I := 0 to CardsArray.Count - 1 do
      begin
        try
          // Check if the element is a JSON object
          if CardsArray.Types[I] = jdtObject then
          begin
            CardObj := CardsArray.O[I];
            FillCardDetailsFromJson(CardObj, Result.Cards[I]);
          end
          else
          begin
            LogError(Format('Skipping non-object element at index %d', [I]));
            Result.Cards[I].Clear; // Ensure this index is cleared
          end;
        except
          on E: Exception do
          begin
            LogError(Format('Error parsing card at index %d: %s',
              [I, E.Message]));
            Result.Cards[I].Clear; // Clear invalid data
          end;
        end;
      end;
    end
    else
    begin
      LogError(ErrorMissingDataKey);
      SetLength(Result.Cards, 0); // No data
    end;

    // Parse metadata
    Result.HasMore := JsonResponse.B[FieldHasMore];
    Result.NextPageURL := JsonResponse.S[FieldNextPage];
    Result.TotalCards := JsonResponse.I[FieldTotalCards];
  except
    on E: Exception do
    begin
      LogError('Error in ParseSearchResult: ' + E.Message);
      raise; // Re-raise the exception after logging
    end;
  end;
end;



function TScryfallAPI.GetRandomCard: TCardDetails;
var
  JsonResponse: TJsonObject;
begin
  try
    // Make a request to the "random" endpoint
    JsonResponse := ExecuteRequest(EndpointRandomCard);
    try
      FillCardDetailsFromJson(JsonResponse, Result);
    finally
      JsonResponse.Free;
    end;
  except
    on E: Exception do
    begin
      LogError('Error fetching random card: ' + E.Message);
      raise;
    end;
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
        // Perform search for the given page
        SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors,
          Fuzzy, Unique, Page);
        Success := True;
        ErrorMsg := '';
      except
        on E: Exception do
        begin
          Success := False;
          ErrorMsg := E.Message;
          SearchResult.Cards := nil; // Ensure Cards is empty on failure
          SearchResult.HasMore := False;
        end;
      end;

      // Pass the results and HasMore status to the callback
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
  I: Integer;
begin
  JsonResponse := ExecuteRequest(EndpointSets);
  try
    if JsonResponse.Contains(FieldData) then
    begin
      SetsArray := JsonResponse.A[FieldData];
      SetLength(Result, SetsArray.Count);

      for I := 0 to SetsArray.Count - 1 do
        FillSetDetailsFromJson(SetsArray.O[I], Result[I]);
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
  // Check the cache outside the lock
  if FSetDetailsCache.TryGetValue(SetCode, Result) then
  begin
    LogError(Format(LogCacheHit, [SetCode]));
    Exit;
  end;

  // Lock the cache and check again
  TMonitor.Enter(FSetDetailsCache);
  try
    if FSetDetailsCache.TryGetValue(SetCode, Result) then
    begin
      LogError(Format(LogCacheHitDoubleCheck, [SetCode]));
      Exit;
    end;

    // Construct the API endpoint
    Endpoint := Format('%s%s',
      [EndpointSets, TNetEncoding.URL.Encode(SetCode)]);
    LogError(Format(LogFetchingSetCode, [SetCode]));

    // Fetch data from the API
    JsonResponse := nil;
    try
      JsonResponse := ExecuteRequest(Endpoint);
      if Assigned(JsonResponse) then
      begin
        // Populate the set details
        FillSetDetailsFromJson(JsonResponse, Result);

        // Add to cache
        FSetDetailsCache.Add(SetCode, Result);
        LogError(Format(LogSetCodeAddedToCache, [SetCode]));
      end
      else
        raise EScryfallAPIError.CreateFmt(ErrorNoDataForSetCode,
          [SetCode]);
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
  SetDetails: TSetDetails;
begin
  // Fetch all sets from the API
  AllSets := GetAllSets;

  // Cache each set detail in memory
  for SetDetails in AllSets do
  begin
    FSetDetailsCache.AddOrSetValue(SetDetails.Code, SetDetails);
  end;
end;



function TScryfallAPI.InternalSearchCards(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
var
  CacheKey, SearchUrl: string;
  CachedResponse: TJsonObject;
  JsonResponse: TJsonObject;
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
    FCache.Add(CacheKey, JsonResponse.Clone as TJsonObject);
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
  I: Integer;
begin
  JsonResponse := ExecuteRequest(EndPointCreatureTypes); // Endpoint for creature types
  try
    // Initialize the catalog
    Result.Clear;

    if JsonResponse.Contains(FieldData) and (JsonResponse.Types[FieldData] = jdtArray) then
    begin
      CatalogArray := JsonResponse.A[FieldData];
      SetLength(Result.Data, CatalogArray.Count);

      for I := 0 to CatalogArray.Count - 1 do
        if CatalogArray.Types[I] = jdtString then
          Result.Data[I] := CatalogArray.S[I];

      //Set metadata
      Result.TotalItems := Length(Result.Data);
    end
    else
      raise EScryfallAPIError.Create('Invalid response: "data" field not found.');
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCatalog(const CatalogName: string): TScryfallCatalog;
var
  JsonResponse: TJsonObject;
  DataArray: TJsonArray;
  I: Integer;
begin
  JsonResponse := ExecuteRequest(Format('catalog/%s', [CatalogName]));
  try
    Result.Clear; // Reset the catalog record
    Result.Name := CatalogName; // Set the catalog name

    // Check if 'data' exists and is a valid array
    if JsonResponse.Contains('data') and (JsonResponse.Types['data'] = jdtArray) then
    begin
      DataArray := JsonResponse.A['data'];
      SetLength(Result.Data, DataArray.Count);

      // Extract the array items
      for I := 0 to DataArray.Count - 1 do
        Result.Data[I] := DataArray.S[I]; // Use .S[index] for strings
    end;

    // Extract additional metadata
    if JsonResponse.Contains('total_items') then
      Result.TotalItems := JsonResponse.I['total_items'];
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
  // Catalog names to fetch
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
    CatalogAbilityWords
  );

  CatalogDict := TDictionary<string, TScryfallCatalog>.Create;
  try
    for CatalogName in CatalogNames do
      CatalogDict.Add(CatalogName, GetCatalog(CatalogName));

    Result := CatalogDict; // Return the filled dictionary
  except
    CatalogDict.Free;
    raise;
    CatalogDict.Free;
  end;
end;


end.
