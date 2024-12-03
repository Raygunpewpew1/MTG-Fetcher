unit ScryfallAPIWrapperV2;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.NetEncoding, System.Threading, JsonDataObjects, SGlobalsZ,
  System.Net.HttpClient,
  System.IOUtils;

type
  // Custom exception class for Scryfall API errors
  EScryfallAPIError = class(Exception);

  TOnSearchComplete = reference to procedure(Success: Boolean;
    Cards: TArray<TCardDetails>; ErrorMsg: string);

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


    function ExecuteRequest(const Endpoint: string): TJsonObject;
    function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): string;
    function ParseSearchResult(const JsonResponse: TJsonObject): TSearchResult;

    // Parsing methods
    procedure FillCardDetailsFromJson(const JsonObj: TJsonObject;
      out CardDetails: TCardDetails);
    procedure ParseImageUris(const JsonObj: TJsonObject;
      out ImageUris: TImageUris);
    procedure ParseLegalities(const JsonObj: TJsonObject;
      out Legalities: TCardLegalities);
    procedure ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
    procedure ParseCardFaces(const JsonObj: TJsonObject;
      out CardFaces: TArray<TCardFace>);
    procedure FillSetDetailsFromJson(const JsonObj: TJsonObject;
      out SetDetails: TSetDetails);
    function InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;

  public
    constructor Create;
    destructor Destroy; override;
    procedure LogError(const Msg: string);
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
      Fuzzy, Unique: Boolean; Callback: TOnSearchComplete);
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
var
  LogFilePath: string;
  LogFile: TStreamWriter;
begin
  try
    {$IF DEFINED(ANDROID)}
    // Save the log to the public "Downloads" folder
    LogFilePath := TPath.Combine(TPath.GetSharedDownloadsPath, 'application_log.txt');
    {$ELSEIF DEFINED(MSWINDOWS)}
    LogFilePath := TPath.Combine(TPath.GetDocumentsPath, 'application_log.txt');
    {$ENDIF}

    // Write to the log file
    LogFile := TStreamWriter.Create(LogFilePath, True, TEncoding.UTF8);
    try
      LogFile.WriteLine(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Msg]));
    finally
      LogFile.Free;
    end;
  except
    // If logging fails, avoid crashing the app
  end;
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
      raise EScryfallAPIError.CreateFmt('Request failed: %s',
        [BaseUrl + Endpoint]);

    Response := ResponseStream.DataString;
  //  LogError('Raw JSON Response: ' + Response);
    Result := TJsonObject.Parse(Response) as TJsonObject;
  finally
    Client.Free;
    ResponseStream.Free;
  end;
end;

function TScryfallAPI.ConstructSearchUrl(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer): string;
var
  BaseUrl: string;
begin
  if Fuzzy then
    Result := Format('%s?fuzzy=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(Query)])
  else
  begin
    BaseUrl := Format('%s?q=%s', [EndpointSearch,
      TNetEncoding.URL.Encode(Query)]);

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
  try
    if JsonResponse.Contains('data') then
    begin
      CardsArray := JsonResponse.A['data']; // Access the 'data' array
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
            LogError(Format('Error parsing card at index %d: %s', [I, E.Message]));
            Result.Cards[I].Clear; // Clear invalid data
          end;
        end;
      end;
    end
    else
    begin
      LogError('ParseSearchResult: "data" key not found in JSON response.');
      SetLength(Result.Cards, 0); // No data
    end;

    // Parse metadata
    Result.HasMore := JsonResponse.B['has_more'];
    Result.NextPageURL := JsonResponse.S['next_page'];
    Result.TotalCards := JsonResponse.I['total_cards'];
  except
    on E: Exception do
    begin
      LogError('Error in ParseSearchResult: ' + E.Message);
      raise; // Re-raise the exception after logging
    end;
  end;
end;

procedure TScryfallAPI.FillCardDetailsFromJson(const JsonObj: TJsonObject; out CardDetails: TCardDetails);
begin
  CardDetails.Clear;

  try
    // Existing fields with platform-safe string assignment
    if JsonObj.Contains('type_line') and (JsonObj.Types['type_line'] = jdtString) then
    begin
      {$IF DEFINED(MSWINDOWS)}
      CardDetails.TypeLine := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(JsonObj.S['type_line']));
      {$ELSE}
      CardDetails.TypeLine := JsonObj.S['type_line'];
      {$ENDIF}
    end;

    if JsonObj.Contains('id') and (JsonObj.Types['id'] = jdtString) then
      CardDetails.SFID := JsonObj.S['id'];

    if JsonObj.Contains('name') and (JsonObj.Types['name'] = jdtString) then
    begin
      {$IF DEFINED(MSWINDOWS)}
      CardDetails.CardName := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(JsonObj.S['name']));
      {$ELSE}
      CardDetails.CardName := JsonObj.S['name'];
      {$ENDIF}
    end;

    if JsonObj.Contains('mana_cost') and (JsonObj.Types['mana_cost'] = jdtString) then
      CardDetails.ManaCost := JsonObj.S['mana_cost'];

    if JsonObj.Contains('oracle_text') and (JsonObj.Types['oracle_text'] = jdtString) then
    begin
      {$IF DEFINED(MSWINDOWS)}
      CardDetails.OracleText := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(JsonObj.S['oracle_text']));
      {$ELSE}
      CardDetails.OracleText := JsonObj.S['oracle_text'];
      {$ENDIF}
    end;


    if JsonObj.Contains('keywords') and (JsonObj.Types['keywords'] = jdtArray) then
     begin
      var KeywordsArray := JsonObj.A['keywords'];
      SetLength(CardDetails.Keywords, KeywordsArray.Count);
       for var I := 0 to KeywordsArray.Count - 1 do
          if KeywordsArray.Types[I] = jdtString then
           CardDetails.Keywords[I] := KeywordsArray.S[I];
      end
   else
     SetLength(CardDetails.Keywords, 0); // Ensure the array is empty if no keywords


    if JsonObj.Contains('set') and (JsonObj.Types['set'] = jdtString) then
      CardDetails.SetCode := JsonObj.S['set'];

    if JsonObj.Contains('set_name') and (JsonObj.Types['set_name'] = jdtString) then
      CardDetails.SetName := JsonObj.S['set_name'];

    if JsonObj.Contains('rarity') and (JsonObj.Types['rarity'] = jdtString) then
      CardDetails.Rarity := JsonObj.S['rarity'];

    if JsonObj.Contains('power') and (JsonObj.Types['power'] = jdtString) then
      CardDetails.Power := JsonObj.S['power'];

    if JsonObj.Contains('toughness') and (JsonObj.Types['toughness'] = jdtString) then
      CardDetails.Toughness := JsonObj.S['toughness'];

    if JsonObj.Contains('loyalty') and (JsonObj.Types['loyalty'] = jdtString) then
      CardDetails.Loyalty := JsonObj.S['loyalty'];

    if JsonObj.Contains('prints_search_uri') and (JsonObj.Types['prints_search_uri'] = jdtString) then
      CardDetails.PrintsSearchUri := JsonObj.S['prints_search_uri'];

    if JsonObj.Contains('oracle_id') and (JsonObj.Types['oracle_id'] = jdtString) then
      CardDetails.OracleID := JsonObj.S['oracle_id'];

    if JsonObj.Contains('flavor_text') and (JsonObj.Types['flavor_text'] = jdtString) then
    begin
      {$IF DEFINED(MSWINDOWS)}
      CardDetails.FlavorText := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(JsonObj.S['flavor_text']));
      {$ELSE}
      CardDetails.FlavorText := JsonObj.S['flavor_text'];
      {$ENDIF}
    end;

    if JsonObj.Contains('layout') and (JsonObj.Types['layout'] = jdtString) then
      CardDetails.Layout := JsonObj.S['layout'].ToLower;

    // New fields
    if JsonObj.Contains('lang') and (JsonObj.Types['lang'] = jdtString) then
      CardDetails.Lang := JsonObj.S['lang'];

    if JsonObj.Contains('released_at') and (JsonObj.Types['released_at'] = jdtString) then
      CardDetails.ReleasedAt := JsonObj.S['released_at'];

   if JsonObj.Contains('cmc') and (JsonObj.Types['cmc'] = jdtFloat) then
     CardDetails.CMC := JsonObj.F['cmc'];

    if JsonObj.Contains('reserved') and (JsonObj.Types['reserved'] = jdtBool) then
      CardDetails.Reserved := JsonObj.B['reserved'];

    if JsonObj.Contains('foil') and (JsonObj.Types['foil'] = jdtBool) then
      CardDetails.Foil := JsonObj.B['foil'];

    if JsonObj.Contains('nonfoil') and (JsonObj.Types['nonfoil'] = jdtBool) then
      CardDetails.NonFoil := JsonObj.B['nonfoil'];

    if JsonObj.Contains('oversized') and (JsonObj.Types['oversized'] = jdtBool) then
      CardDetails.Oversized := JsonObj.B['oversized'];

    if JsonObj.Contains('promo') and (JsonObj.Types['promo'] = jdtBool) then
      CardDetails.Promo := JsonObj.B['promo'];

    if JsonObj.Contains('reprint') and (JsonObj.Types['reprint'] = jdtBool) then
      CardDetails.Reprint := JsonObj.B['reprint'];

    if JsonObj.Contains('digital') and (JsonObj.Types['digital'] = jdtBool) then
      CardDetails.Digital := JsonObj.B['digital'];

    if JsonObj.Contains('rarity') and (JsonObj.Types['rarity'] = jdtString) then
      CardDetails.Rarity := JsonObj.S['rarity'];

    if JsonObj.Contains('artist') and (JsonObj.Types['artist'] = jdtString) then
      CardDetails.Artist := JsonObj.S['artist'];

    if JsonObj.Contains('collector_number') and (JsonObj.Types['collector_number'] = jdtString) then
      CardDetails.CollectorNumber := JsonObj.S['collector_number'];

    if JsonObj.Contains('border_color') and (JsonObj.Types['border_color'] = jdtString) then
      CardDetails.BorderColor := JsonObj.S['border_color'];

    if JsonObj.Contains('frame') and (JsonObj.Types['frame'] = jdtString) then
      CardDetails.Frame := JsonObj.S['frame'];

    if JsonObj.Contains('security_stamp') and (JsonObj.Types['security_stamp'] = jdtString) then
      CardDetails.SecurityStamp := JsonObj.S['security_stamp'];

    if JsonObj.Contains('full_art') and (JsonObj.Types['full_art'] = jdtBool) then
      CardDetails.FullArt := JsonObj.B['full_art'];

    if JsonObj.Contains('textless') and (JsonObj.Types['textless'] = jdtBool) then
      CardDetails.Textless := JsonObj.B['textless'];

    if JsonObj.Contains('story_spotlight') and (JsonObj.Types['story_spotlight'] = jdtBool) then
      CardDetails.StorySpotlight := JsonObj.B['story_spotlight'];

    // Parse nested objects
    ParseImageUris(JsonObj, CardDetails.ImageUris);
    ParseLegalities(JsonObj, CardDetails.Legalities);
    ParsePrices(JsonObj, CardDetails.Prices);
    ParseCardFaces(JsonObj, CardDetails.CardFaces);

  except
    on E: Exception do
    begin
      LogError('Error filling card details: ' + E.Message);
      CardDetails.Clear; // Clear data on error
    end;
  end;
end;




procedure TScryfallAPI.ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
var
  ImageUrisObj: TJsonObject;
begin
  if JsonObj.Contains('image_uris') and (JsonObj.Types['image_uris'] = jdtObject) then
  begin
    ImageUrisObj := JsonObj.O['image_uris'];
    if ImageUrisObj.Contains('small') and (ImageUrisObj.Types['small'] = jdtString) then
      ImageUris.Small := ImageUrisObj.S['small'];
    if ImageUrisObj.Contains('normal') and (ImageUrisObj.Types['normal'] = jdtString) then
      ImageUris.Normal := ImageUrisObj.S['normal'];
    if ImageUrisObj.Contains('large') and (ImageUrisObj.Types['large'] = jdtString) then
      ImageUris.Large := ImageUrisObj.S['large'];
    if ImageUrisObj.Contains('png') and (ImageUrisObj.Types['png'] = jdtString) then
      ImageUris.PNG := ImageUrisObj.S['png'];
    if ImageUrisObj.Contains('border_crop') and (ImageUrisObj.Types['border_crop'] = jdtString) then
      ImageUris.border_crop := ImageUrisObj.S['border_crop'];
    if ImageUrisObj.Contains('art_crop') and (ImageUrisObj.Types['art_crop'] = jdtString) then
      ImageUris.art_crop := ImageUrisObj.S['art_crop'];
  end
  else
    ImageUris := Default(TImageUris); // Default values if not found or invalid
end;

procedure TScryfallAPI.ParseLegalities(const JsonObj: TJsonObject; out Legalities: TCardLegalities);
var
  LegalitiesObj: TJsonObject;

  function GetSafeStringField(const Obj: TJsonObject; const FieldName: string): string;
  begin
    if Obj.Contains(FieldName) and (Obj.Types[FieldName] = jdtString) then
      Result := Obj.S[FieldName]
    else
      Result := ''; // Return an empty string if the field is missing or invalid
  end;

begin
  if JsonObj.Contains('legalities') and (JsonObj.Types['legalities'] = jdtObject) then
  begin
    LegalitiesObj := JsonObj.O['legalities'];

    // Safely extract all legalities
    Legalities.Standard := GetSafeStringField(LegalitiesObj, 'standard');
    Legalities.Pioneer := GetSafeStringField(LegalitiesObj, 'pioneer');
    Legalities.Modern := GetSafeStringField(LegalitiesObj, 'modern');
    Legalities.Legacy := GetSafeStringField(LegalitiesObj, 'legacy');
    Legalities.Commander := GetSafeStringField(LegalitiesObj, 'commander');
    Legalities.Vintage := GetSafeStringField(LegalitiesObj, 'vintage');
    Legalities.Pauper := GetSafeStringField(LegalitiesObj, 'pauper');
    Legalities.Historic := GetSafeStringField(LegalitiesObj, 'historic');
    Legalities.Explorer := GetSafeStringField(LegalitiesObj, 'explorer');
    Legalities.Alchemy := GetSafeStringField(LegalitiesObj, 'alchemy');
    Legalities.Brawl := GetSafeStringField(LegalitiesObj, 'brawl');
    Legalities.Future := GetSafeStringField(LegalitiesObj, 'future');
    Legalities.Oldschool := GetSafeStringField(LegalitiesObj, 'oldschool');
    Legalities.Premodern := GetSafeStringField(LegalitiesObj, 'premodern');
    Legalities.Duel := GetSafeStringField(LegalitiesObj, 'duel');
    Legalities.Penny := GetSafeStringField(LegalitiesObj, 'penny');
  end
  else
  begin
    // Default to empty legalities if the legalities object is missing or invalid
    Legalities := Default(TCardLegalities);
  end;
end;

procedure TScryfallAPI.ParsePrices(const JsonObj: TJsonObject;
  out Prices: TCardPrices);
var
  PricesObj: TJsonObject;
begin
  if JsonObj.Contains('prices') and (JsonObj.Types['prices'] = jdtObject) then
  begin
    PricesObj := JsonObj.O['prices'];

    if PricesObj.Contains('usd') and (PricesObj.Types['usd'] = jdtString) then
      Prices.USD := PricesObj.S['usd'];
    if PricesObj.Contains('usd_foil') and (PricesObj.Types['usd_foil'] = jdtString) then
      Prices.USD_Foil := PricesObj.S['usd_foil'];
    if PricesObj.Contains('eur') and (PricesObj.Types['eur'] = jdtString) then
      Prices.EUR := PricesObj.S['eur'];
    if PricesObj.Contains('tix') and (PricesObj.Types['tix'] = jdtString) then
      Prices.Tix := PricesObj.S['tix'];
  end
  else
    Prices := Default(TCardPrices);
end;

procedure TScryfallAPI.ParseCardFaces(const JsonObj: TJsonObject;
  out CardFaces: TArray<TCardFace>);
var
  FacesArray: TJsonArray;
  I: Integer;
begin
  if JsonObj.Contains('card_faces') and (JsonObj.Types['card_faces'] = jdtArray) then
  begin
    FacesArray := JsonObj.A['card_faces'];
    SetLength(CardFaces, FacesArray.Count);

    for I := 0 to FacesArray.Count - 1 do
    begin
      if FacesArray.Types[I] = jdtObject then
      begin
        var FaceObj := FacesArray.O[I];

        if FaceObj.Contains('name') and (FaceObj.Types['name'] = jdtString) then
          CardFaces[I].Name := FaceObj.S['name'];

        if FaceObj.Contains('mana_cost') and (FaceObj.Types['mana_cost'] = jdtString) then
          CardFaces[I].ManaCost := FaceObj.S['mana_cost'];

        if FaceObj.Contains('type_line') and (FaceObj.Types['type_line'] = jdtString) then
          CardFaces[I].TypeLine := FaceObj.S['type_line'];

        if FaceObj.Contains('oracle_text') and (FaceObj.Types['oracle_text'] = jdtString) then
          CardFaces[I].OracleText := FaceObj.S['oracle_text'];

        if FaceObj.Contains('power') and (FaceObj.Types['power'] = jdtString) then
          CardFaces[I].Power := FaceObj.S['power'];

        if FaceObj.Contains('toughness') and (FaceObj.Types['toughness'] = jdtString) then
          CardFaces[I].Toughness := FaceObj.S['toughness'];

        // Parse nested ImageUris
        ParseImageUris(FaceObj, CardFaces[I].ImageUris);
      end;
    end;
  end
  else
    SetLength(CardFaces, 0); // No card faces
end;

procedure TScryfallAPI.SearchAllCardsAsync(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Callback: TOnSearchComplete);
begin
  TTask.Run(
    procedure
    var
      Cards: TArray<TCardDetails>;
      ErrorMsg: string;
      Success: Boolean;
      ValidCards: TList<TCardDetails>;
    begin
      try
        // Perform search
        Cards := SearchAllCards(Query, SetCode, Rarity, Colors, Fuzzy, Unique);

        // Filter out invalid cards
        ValidCards := TList<TCardDetails>.Create;
        try
          for var Card in Cards do
          begin
            if not Card.CardName.IsEmpty and not Card.SFID.IsEmpty then
              ValidCards.Add(Card)
            else
              LogError('Skipping invalid card: Missing CardName or SFID');
          end;

          // Assign valid cards
          Cards := ValidCards.ToArray;
        finally
          ValidCards.Free;
        end;

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

function TScryfallAPI.GetSetByCode(const SetCode: string): TSetDetails;
var
  Endpoint: string;
  JsonResponse: TJsonObject;
begin
  // Check the cache outside the lock
  if FSetDetailsCache.TryGetValue(SetCode, Result) then
  begin
    LogError(Format('Cache hit for SetCode: %s', [SetCode]));
    Exit;
  end;

  // Lock the cache and check again
  TMonitor.Enter(FSetDetailsCache);
  try
    if FSetDetailsCache.TryGetValue(SetCode, Result) then
    begin
      LogError(Format('Cache hit (double-check) for SetCode: %s', [SetCode]));
      Exit;
    end;

    // Construct the API endpoint
    Endpoint := Format('%s%s', [EndpointSets, TNetEncoding.URL.Encode(SetCode)]);
    LogError(Format('Fetching SetCode from API: %s', [SetCode]));

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
        LogError(Format('SetCode added to cache: %s', [SetCode]));
      end
      else
        raise EScryfallAPIError.CreateFmt('No data returned for SetCode: %s', [SetCode]);
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
  SetDetails.IconSVGURI := TEncoding.UTF8.GetString
    (TEncoding.ANSI.GetBytes(JsonObj.S['icon_svg_uri']));
  SetDetails.ScryfallURI := JsonObj.S['scryfall_uri'];
  SetDetails.URI := JsonObj.S['uri'];
  SetDetails.SearchURI := JsonObj.S['search_uri'];
end;

function TScryfallAPI.InternalSearchCards(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
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

end.
