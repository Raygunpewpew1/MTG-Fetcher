unit ScryfallAPIWrapperV2;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.NetEncoding, System.Threading, JsonDataObjects, SGlobalsZ,System.Net.HttpClient,
  System.IOUtils;

type
  // Custom exception class for Scryfall API errors
  EScryfallAPIError = class(Exception);

  TOnSearchComplete = reference to procedure(Success: Boolean; Cards: TArray<TCardDetails>; ErrorMsg: string);

  TScryfallAPI = class
  private const
    BaseUrl = 'https://api.scryfall.com/';
    UserAgent = 'MTGCardFetch/1.0';
    AcceptHeader = 'application/json';

    EndpointCards = 'cards/';
    EndpointNamed = 'cards/named';
    EndpointSearch = 'cards/search';
    EndpointSets = 'sets/';
    EndpointBulkData = 'bulk-data';

  private
    FCache: TDictionary<string, TJsonObject>;
    FSetDetailsCache: TDictionary<string, TSetDetails>;

    procedure LogError(const Msg: string);
    function ExecuteRequest(const Endpoint: string): TJsonObject;
    function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean; Page: Integer): string;
    function ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;

    // Parsing methods
    procedure FillCardDetailsFromJson(const JsonObj: TJsonObject; out CardDetails: TCardDetails);
    procedure ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
    procedure ParseLegalities(const JsonObj: TJsonObject; out Legalities: TCardLegalities);
    procedure ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
    procedure ParseCardFaces(const JsonObj: TJsonObject; out CardFaces: TArray<TCardFace>);
    procedure FillSetDetailsFromJson(const JsonObj: TJsonObject; out SetDetails: TSetDetails);
    function InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
    function GetLocalSetIconPath(const SetCode: string): string;



  public
    constructor Create;
    destructor Destroy; override;

    procedure PreloadAllSets;
    function GetSetByCode(const SetCode: string): TSetDetails;
    function GetCardByName(const CardName: string; Fuzzy: Boolean = False): TCardDetails;
    function SearchCards(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean; Page: Integer = 1): TArray<TCardDetails>;
    function GetAllSets: TArray<TSetDetails>;
    function SearchAllCards(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean): TArray<TCardDetails>;
    procedure SearchAllCardsAsync(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean; Callback: TOnSearchComplete);
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

procedure TScryfallAPI.LogError(const Msg: string);
begin
  // Log errors (optional for debugging)
end;

function TScryfallAPI.ExecuteRequest(const Endpoint: string): TJsonObject;
var
  Response: string;
  Client: THTTPClient;
  ResponseStream: TStringStream;
begin
  Client := THTTPClient.Create;
  ResponseStream := TStringStream.Create;
  try
    Client.CustomHeaders['User-Agent'] := UserAgent;
    Client.CustomHeaders['Accept'] := AcceptHeader;

    if Client.Get(BaseUrl + Endpoint, ResponseStream).StatusCode <> 200 then
      raise EScryfallAPIError.CreateFmt('Request failed: %s', [BaseUrl + Endpoint]);

    Response := ResponseStream.DataString;
    Result := TJsonObject.Parse(Response) as TJsonObject;
  finally
    Client.Free;
    ResponseStream.Free;
  end;
end;

function TScryfallAPI.ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean; Page: Integer): string;
var
  BaseUrl: string;
begin
  if Fuzzy then
    Result := Format('%s?fuzzy=%s', [EndpointNamed, TNetEncoding.URL.Encode(Query)])
  else
  begin
    BaseUrl := Format('%s?q=%s', [EndpointSearch, TNetEncoding.URL.Encode(Query)]);

    if SetCode <> '' then
      BaseUrl := BaseUrl + '+set%3A' + TNetEncoding.URL.Encode(SetCode);
    if Rarity <> '' then
      BaseUrl := BaseUrl + '+rarity%3A' + TNetEncoding.URL.Encode(Rarity);
    if Colors <> '' then
      BaseUrl := BaseUrl + '+color%3A' + TNetEncoding.URL.Encode(Colors);
    if Unique then
      BaseUrl := BaseUrl + '&unique=prints';

    Result := BaseUrl + Format('&page=%d', [Page]);
  end;
end;

function TScryfallAPI.ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;
var
  CardsArray: TJsonArray;
  I: Integer;
  CardObj: TJsonObject;
begin
  if JsonResponse.Contains('data') then
  begin
    CardsArray := JsonResponse.A['data']; // Access the 'data' array
    SetLength(Result.Cards, CardsArray.Count);

    for I := 0 to CardsArray.Count - 1 do
    begin
      try
        // Use `O` to safely access the object at index I
        CardObj := CardsArray.O[I]; // Get the object
        FillCardDetailsFromJson(CardObj, Result.Cards[I]);
      except
        on E: Exception do
        begin
          LogError(Format('Error parsing card at index %d: %s', [I, E.Message]));
          Result.Cards[I].Clear; // Ensure invalid items are cleared
        end;
      end;
    end;
  end
  else
  begin
    SetLength(Result.Cards, 0); // No data found
  end;

  // Parse metadata
  Result.HasMore := JsonResponse.B['has_more'];
  Result.NextPageURL := JsonResponse.S['next_page'];
  Result.TotalCards := JsonResponse.I['total_cards'];
end;



procedure TScryfallAPI.FillCardDetailsFromJson(const JsonObj: TJsonObject; out CardDetails: TCardDetails);
var
SetDetails: TSetDetails;
begin
  CardDetails.Clear;

  CardDetails.TypeLine := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(JsonObj.S['type_line']));






  // Other fields
  CardDetails.SFID := JsonObj.S['id'];
  CardDetails.CardName := JsonObj.S['name'];
  CardDetails.ManaCost := JsonObj.S['mana_cost'];
  CardDetails.OracleText := JsonObj.S['oracle_text'];
  CardDetails.SetCode := JsonObj.S['set'];
  CardDetails.SetName := JsonObj.S['set_name'];
  CardDetails.Rarity := JsonObj.S['rarity'];
  CardDetails.Power := JsonObj.S['power'];
  CardDetails.Toughness := JsonObj.S['toughness'];
  CardDetails.Loyalty := JsonObj.S['loyalty'];
  CardDetails.PrintsSearchUri := JsonObj.S['prints_search_uri'];
  CardDetails.OracleID := JsonObj.S['oracle_id'];
  CardDetails.FlavorText := JsonObj.S['flavor_text'];
  CardDetails.Layout := JsonObj.S['layout'].ToLower;
  if not CardDetails.SetCode.IsEmpty then
  begin
    try
      SetDetails := GetSetByCode(CardDetails.SetCode);
      CardDetails.SetName := SetDetails.Name;
      CardDetails.SetIconURI := SetDetails.IconSVGURI;
    except
      on E: Exception do
        LogError('Failed to fetch set details: ' + E.Message);
    end;
  end;


  // Parse nested objects
  ParseImageUris(JsonObj, CardDetails.ImageUris);
  ParseLegalities(JsonObj, CardDetails.Legalities);
  ParsePrices(JsonObj, CardDetails.Prices);
  ParseCardFaces(JsonObj, CardDetails.CardFaces);
end;

procedure TScryfallAPI.ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
var
  ImageUrisObj: TJsonObject;
begin
  if JsonObj.Contains('image_uris') then
  begin
    ImageUrisObj := JsonObj.O['image_uris'];
    ImageUris.Small := ImageUrisObj.S['small'];
    ImageUris.Normal := ImageUrisObj.S['normal'];
    ImageUris.Large := ImageUrisObj.S['large'];
    ImageUris.PNG := ImageUrisObj.S['png'];
    ImageUris.border_crop := ImageUrisObj.S['border_crop'];
    ImageUris.art_crop := ImageUrisObj.S['art_crop'];
  end
  else
    ImageUris := Default(TImageUris);
end;

procedure TScryfallAPI.ParseLegalities(const JsonObj: TJsonObject; out Legalities: TCardLegalities);
var
  LegalitiesObj: TJsonObject;
begin
  if JsonObj.Contains('legalities') then
  begin
    LegalitiesObj := JsonObj.O['legalities'];
    Legalities.Standard := LegalitiesObj.S['standard'];
    Legalities.Pioneer := LegalitiesObj.S['pioneer'];
    Legalities.Modern := LegalitiesObj.S['modern'];
    Legalities.Legacy := LegalitiesObj.S['legacy'];
    Legalities.Commander := LegalitiesObj.S['commander'];
    Legalities.Vintage := LegalitiesObj.S['vintage'];
    Legalities.Pauper := LegalitiesObj.S['pauper'];
    Legalities.Historic := LegalitiesObj.S['historic'];
    Legalities.Explorer := LegalitiesObj.S['explorer'];
  end
  else
    Legalities := Default(TCardLegalities);
end;

procedure TScryfallAPI.ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
var
  PricesObj: TJsonObject;
begin
  if JsonObj.Contains('prices') then
  begin
    PricesObj := JsonObj.O['prices'];
    Prices.USD := PricesObj.S['usd'];
    Prices.USD_Foil := PricesObj.S['usd_foil'];
    Prices.EUR := PricesObj.S['eur'];
    Prices.Tix := PricesObj.S['tix'];
  end
  else
    Prices := Default(TCardPrices);
end;

procedure TScryfallAPI.ParseCardFaces(const JsonObj: TJsonObject; out CardFaces: TArray<TCardFace>);
var
  FacesArray: TJsonArray;
  I: Integer;
begin
  if JsonObj.Contains('card_faces') then
  begin
    FacesArray := JsonObj.A['card_faces'];
    SetLength(CardFaces, FacesArray.Count);

    for I := 0 to FacesArray.Count - 1 do
    begin
      CardFaces[I].Name := FacesArray.O[I].S['name'];
      CardFaces[I].ManaCost := FacesArray.O[I].S['mana_cost'];
      CardFaces[I].TypeLine := FacesArray.O[I].S['type_line'];
      CardFaces[I].OracleText := FacesArray.O[I].S['oracle_text'];
      CardFaces[I].Power := FacesArray.O[I].S['power'];
      CardFaces[I].Toughness := FacesArray.O[I].S['toughness'];
      ParseImageUris(FacesArray.O[I], CardFaces[I].ImageUris);
    end;
  end
  else
    SetLength(CardFaces, 0);
end;

procedure TScryfallAPI.SearchAllCardsAsync(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean; Callback: TOnSearchComplete);
begin
  TTask.Run(
    procedure
    var
      Cards: TArray<TCardDetails>;
      ErrorMsg: string;
      Success: Boolean;
    begin
      try
        Cards := SearchAllCards(Query, SetCode, Rarity, Colors, Fuzzy, Unique);
        Success := True;
        ErrorMsg := '';
      except
        on E: Exception do
        begin
          Success := False;
          Cards := nil;
          ErrorMsg := E.Message;
        end;
      end;

      TThread.Synchronize(nil,
        procedure
        begin
          Callback(Success, Cards, ErrorMsg);
        end);
    end);
end;

function TScryfallAPI.SearchCards(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean; Page: Integer): TArray<TCardDetails>;
var
  SearchResult: TSearchResult;
begin
  SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors, Fuzzy, Unique, Page);
  Result := SearchResult.Cards;
end;

function TScryfallAPI.SearchAllCards(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean): TArray<TCardDetails>;
var
  Page: Integer;
  SearchResult: TSearchResult;
  AllCards: TList<TCardDetails>;
begin
  AllCards := TList<TCardDetails>.Create;
  try
    Page := 1;
    repeat
      SearchResult := InternalSearchCards(Query, SetCode, Rarity, Colors, Fuzzy, Unique, Page);
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
    if JsonResponse.Contains('data') then
    begin
      SetsArray := JsonResponse.A['data'];
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

function TScryfallAPI.GetLocalSetIconPath(const SetCode: string): string;
begin
  Result := TPath.Combine(TPath.GetDocumentsPath, Format('%s_icon.svg', [SetCode]));
end;



function TScryfallAPI.GetSetByCode(const SetCode: string): TSetDetails;
var
  Endpoint: string;
  JsonResponse: TJsonObject;
begin
  // Check if the set details are already cached
  if FSetDetailsCache.TryGetValue(SetCode, Result) then
    Exit;

  // Fetch set details from the API
  Endpoint := Format('%s%s', [EndpointSets, TNetEncoding.URL.Encode(SetCode)]);
  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
    begin
      FillSetDetailsFromJson(JsonResponse, Result);

      // Cache the result
      FSetDetailsCache.Add(SetCode, Result);
    end
    else
      raise EScryfallAPIError.Create('Set details not found.');
  finally
    JsonResponse.Free;
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


procedure TScryfallAPI.FillSetDetailsFromJson(const JsonObj: TJsonObject; out SetDetails: TSetDetails);
begin
  SetDetails.Clear;
  SetDetails.SFID := JsonObj.S['id'];
  SetDetails.Code := JsonObj.S['code'];
  SetDetails.Name := JsonObj.S['name'];
  SetDetails.ReleaseDate := JsonObj.S['released_at'];
  SetDetails.SetType := JsonObj.S['set_type'];
  SetDetails.Block := JsonObj.S['block'];
  SetDetails.BlockCode := JsonObj.S['block_code'];
  SetDetails.ParentSetCode := JsonObj.S['parent_set_code'];
  SetDetails.CardCount := JsonObj.I['card_count'];
  SetDetails.Digital := JsonObj.B['digital'];
  SetDetails.FoilOnly := JsonObj.B['foil_only'];
  SetDetails.IconSVGURI := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(JsonObj.S['icon_svg_uri']));
  SetDetails.ScryfallURI := JsonObj.S['scryfall_uri'];
  SetDetails.URI := JsonObj.S['uri'];
  SetDetails.SearchURI := JsonObj.S['search_uri'];
end;

function TScryfallAPI.InternalSearchCards(const Query, SetCode, Rarity, Colors: string; Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
var
  CacheKey, SearchUrl: string;
  CachedResponse: TJsonObject;
  JsonResponse: TJsonObject;
begin
  CacheKey := Format('%s:%s:%s:%s:%d', [Query, SetCode, Rarity, Colors, Page]);

  if FCache.TryGetValue(CacheKey, CachedResponse) then
  begin
    Result := ParseSearchResult(CachedResponse);
    Exit;
  end;

  SearchUrl := ConstructSearchUrl(Query, SetCode, Rarity, Colors, Fuzzy, Unique, Page);
  JsonResponse := ExecuteRequest(SearchUrl);
  try
    Result := ParseSearchResult(JsonResponse);
    FCache.Add(CacheKey, JsonResponse.Clone as TJsonObject);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardByName(const CardName: string; Fuzzy: Boolean): TCardDetails;
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
    FillCardDetailsFromJson(JsonResponse, Result);
  finally
    JsonResponse.Free;
  end;
end;

end.
