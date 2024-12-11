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
    procedure FillCardDetailsFromJson(const JsonObj: TJsonObject;
      out CardDetails: TCardDetails);

    procedure FillSetDetailsFromJson(const JsonObj: TJsonObject;
      out SetDetails: TSetDetails);
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

procedure TScryfallAPI.FillCardDetailsFromJson(const JsonObj: TJsonObject;
  out CardDetails: TCardDetails);
begin

if (CardDetails.SFID.IsEmpty = false) or (CardDetails.OracleID.IsEmpty = false) then
CardDetails.Clear;

  try
    // Existing fields with platform-safe string assignment
    if JsonObj.Contains(FieldTypeLine) and (JsonObj.Types[FieldTypeLine] = jdtString) then
    begin
{$IF DEFINED(MSWINDOWS)}
      CardDetails.TypeLine := TEncoding.UTF8.GetString(
        TEncoding.ANSI.GetBytes(JsonObj.S[FieldTypeLine]));
{$ELSE}
      CardDetails.TypeLine := JsonObj.S[FieldTypeLine];
{$ENDIF}
    end;

    if JsonObj.Contains(FieldID) and (JsonObj.Types[FieldID] = jdtString) then
      CardDetails.SFID := JsonObj.S[FieldID];

    if JsonObj.Contains(FieldName) and (JsonObj.Types[FieldName] = jdtString) then
    begin
{$IF DEFINED(MSWINDOWS)}
      CardDetails.CardName := TEncoding.UTF8.GetString(
        TEncoding.ANSI.GetBytes(JsonObj.S[FieldName]));
{$ELSE}
      CardDetails.CardName := JsonObj.S[FieldName];
{$ENDIF}
    end;

    if JsonObj.Contains(FieldManaCost) and (JsonObj.Types[FieldManaCost] = jdtString) then
      CardDetails.ManaCost := JsonObj.S[FieldManaCost];

    if JsonObj.Contains(FieldOracleText) and (JsonObj.Types[FieldOracleText] = jdtString) then
    begin
{$IF DEFINED(MSWINDOWS)}
      CardDetails.OracleText := TEncoding.UTF8.GetString(
        TEncoding.ANSI.GetBytes(JsonObj.S[FieldOracleText]));
{$ELSE}
      CardDetails.OracleText := JsonObj.S[FieldOracleText];
{$ENDIF}
    end;

    if JsonObj.Contains(FieldKeywords) and (JsonObj.Types[FieldKeywords] = jdtArray) then
    begin
      var KeywordsArray := JsonObj.A[FieldKeywords];
      SetLength(CardDetails.Keywords, KeywordsArray.Count);
      for var I := 0 to KeywordsArray.Count - 1 do
        if KeywordsArray.Types[I] = jdtString then
          CardDetails.Keywords[I] := KeywordsArray.S[I];
    end
    else
      SetLength(CardDetails.Keywords, 0);

    if JsonObj.Contains(FieldSet) and (JsonObj.Types[FieldSet] = jdtString) then
      CardDetails.SetCode := JsonObj.S[FieldSet];

    if JsonObj.Contains(FieldSetName) and (JsonObj.Types[FieldSetName] = jdtString) then
      CardDetails.SetName := JsonObj.S[FieldSetName];

    if JsonObj.Contains(FieldRarity) and (JsonObj.Types[FieldRarity] = jdtString) then
      CardDetails.Rarity := JsonObj.S[FieldRarity];

    if JsonObj.Contains(FieldPower) and (JsonObj.Types[FieldPower] = jdtString) then
      CardDetails.Power := JsonObj.S[FieldPower];

    if JsonObj.Contains(FieldToughness) and (JsonObj.Types[FieldToughness] = jdtString) then
      CardDetails.Toughness := JsonObj.S[FieldToughness];

    if JsonObj.Contains(FieldLoyalty) and (JsonObj.Types[FieldLoyalty] = jdtString) then
      CardDetails.Loyalty := JsonObj.S[FieldLoyalty];

    if JsonObj.Contains(FieldPrintsSearchUri) and (JsonObj.Types[FieldPrintsSearchUri] = jdtString) then
      CardDetails.PrintsSearchUri := JsonObj.S[FieldPrintsSearchUri];

    if JsonObj.Contains(FieldOracleID) and (JsonObj.Types[FieldOracleID] = jdtString) then
      CardDetails.OracleID := JsonObj.S[FieldOracleID];

    if JsonObj.Contains(FieldFlavorText) and (JsonObj.Types[FieldFlavorText] = jdtString) then
    begin
{$IF DEFINED(MSWINDOWS)}
      CardDetails.FlavorText := TEncoding.UTF8.GetString(
        TEncoding.ANSI.GetBytes(JsonObj.S[FieldFlavorText]));
{$ELSE}
      CardDetails.FlavorText := JsonObj.S[FieldFlavorText];
{$ENDIF}
    end;

    if JsonObj.Contains(FieldLayout) and (JsonObj.Types[FieldLayout] = jdtString) then
      CardDetails.Layout := JsonObj.S[FieldLayout].ToLower;

    if JsonObj.Contains(FieldLang) and (JsonObj.Types[FieldLang] = jdtString) then
      CardDetails.Lang := JsonObj.S[FieldLang];

    if JsonObj.Contains(FieldReleasedAt) and (JsonObj.Types[FieldReleasedAt] = jdtString) then
      CardDetails.ReleasedAt := JsonObj.S[FieldReleasedAt];

    if JsonObj.Contains(FieldCMC) and (JsonObj.Types[FieldCMC] = jdtFloat) then
      CardDetails.CMC := JsonObj.F[FieldCMC];

    if JsonObj.Contains(FieldReserved) and (JsonObj.Types[FieldReserved] = jdtBool) then
      CardDetails.Reserved := JsonObj.B[FieldReserved];

    if JsonObj.Contains(FieldFoil) and (JsonObj.Types[FieldFoil] = jdtBool) then
      CardDetails.Foil := JsonObj.B[FieldFoil];

    if JsonObj.Contains(FieldNonFoil) and (JsonObj.Types[FieldNonFoil] = jdtBool) then
      CardDetails.NonFoil := JsonObj.B[FieldNonFoil];

    if JsonObj.Contains(FieldOversized) and (JsonObj.Types[FieldOversized] = jdtBool) then
      CardDetails.Oversized := JsonObj.B[FieldOversized];

    if JsonObj.Contains(FieldPromo) and (JsonObj.Types[FieldPromo] = jdtBool) then
      CardDetails.Promo := JsonObj.B[FieldPromo];

    if JsonObj.Contains(FieldReprint) and (JsonObj.Types[FieldReprint] = jdtBool) then
      CardDetails.Reprint := JsonObj.B[FieldReprint];

    if JsonObj.Contains(FieldDigital) and (JsonObj.Types[FieldDigital] = jdtBool) then
      CardDetails.Digital := JsonObj.B[FieldDigital];

    if JsonObj.Contains(FieldArtist) and (JsonObj.Types[FieldArtist] = jdtString) then
      CardDetails.Artist := JsonObj.S[FieldArtist];

    if JsonObj.Contains(FieldCollectorNumber) and (JsonObj.Types[FieldCollectorNumber] = jdtString) then
      CardDetails.CollectorNumber := JsonObj.S[FieldCollectorNumber];

    if JsonObj.Contains(FieldBorderColor) and (JsonObj.Types[FieldBorderColor] = jdtString) then
      CardDetails.BorderColor := JsonObj.S[FieldBorderColor];

    if JsonObj.Contains(FieldFrame) and (JsonObj.Types[FieldFrame] = jdtString) then
      CardDetails.Frame := JsonObj.S[FieldFrame];

    if JsonObj.Contains(FieldSecurityStamp) and (JsonObj.Types[FieldSecurityStamp] = jdtString) then
      CardDetails.SecurityStamp := JsonObj.S[FieldSecurityStamp];

    if JsonObj.Contains(FieldFullArt) and (JsonObj.Types[FieldFullArt] = jdtBool) then
      CardDetails.FullArt := JsonObj.B[FieldFullArt];

    if JsonObj.Contains(FieldTextless) and (JsonObj.Types[FieldTextless] = jdtBool) then
      CardDetails.Textless := JsonObj.B[FieldTextless];

    if JsonObj.Contains(FieldStorySpotlight) and (JsonObj.Types[FieldStorySpotlight] = jdtBool) then
      CardDetails.StorySpotlight := JsonObj.B[FieldStorySpotlight];

    // Parse nested objects
    ParseImageUris(JsonObj, CardDetails.ImageUris);
    ParseLegalities(JsonObj, CardDetails.Legalities);
    ParsePrices(JsonObj, CardDetails.Prices);
    ParseCardFaces(JsonObj, CardDetails.CardFaces);

  except
    on E: Exception do
    begin
      LogError(Format(ErrorFillingCardDetails, [E.Message]));
      CardDetails.Clear;
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

procedure TScryfallAPI.FillSetDetailsFromJson(const JsonObj: TJsonObject;
out SetDetails: TSetDetails);
begin
  SetDetails.Clear;
  SetDetails.SFID := JsonObj.S[FieldID];
  SetDetails.Code := JsonObj.S[FieldCode];
  SetDetails.Name := JsonObj.S[FieldName];
  SetDetails.ReleaseDate := JsonObj.S[FieldReleasedAt];
  SetDetails.SetType := JsonObj.S[FieldSetType];
  SetDetails.Block := JsonObj.S[FieldBlock];
  SetDetails.BlockCode := JsonObj.S[FieldBlockCode];
  SetDetails.ParentSetCode := JsonObj.S[FieldParentSetCode];
  SetDetails.CardCount := JsonObj.I[FieldCardCount];
  SetDetails.Digital := JsonObj.B[FieldDigital];
  SetDetails.FoilOnly := JsonObj.B[FieldFoilOnly];
  SetDetails.IconSVGURI := TEncoding.UTF8.GetString
    (TEncoding.ANSI.GetBytes(JsonObj.S[FieldIconSvgUri]));
  SetDetails.ScryfallURI := JsonObj.S[FieldScryfallUri];
  SetDetails.URI := JsonObj.S[FieldUri];
  SetDetails.SearchURI := JsonObj.S[FieldSearchUri];
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
