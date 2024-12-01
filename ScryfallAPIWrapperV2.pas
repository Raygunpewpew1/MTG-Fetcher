unit ScryfallAPIWrapperV2;

interface

uses
  System.Classes, System.SysUtils, REST.Client, REST.Types, JSON,
  Data.Bind.Components, Data.Bind.ObjectScope, System.NetEncoding,
  System.Generics.Collections, System.IOUtils, System.Net.HttpClient,
  SGlobalsZ, System.Threading, JSONHelper;

type
  // Custom exception class for Scryfall API errors
  EScryfallAPIError = class(Exception);

type
  TOnSearchComplete = reference to procedure(Success: Boolean;
    Cards: TArray<TCardDetails>; ErrorMsg: string);

  /// <summary>
  /// Provides methods to interact with the Scryfall API.
  /// </summary>
  TScryfallAPI = class
  private const
    BaseUrl = 'https://api.scryfall.com/';
    UserAgent = 'MTGCardFetch/1.0';
    AcceptHeader = 'application/json';

    // API Endpoints
    EndpointCards = 'cards/';
    EndpointNamed = 'cards/named';
    EndpointSearch = 'cards/search';
    EndpointSets = 'sets/';
    EndpointBulkData = 'bulk-data';
    EndpointAutocomplete = 'cards/autocomplete';
    EndpointSymbology = 'symbology';
    EndpointCardNames = 'catalog/card-names';
    function GetSetByCodeCached(const SetCode: string): TSetDetails;

  var
    FClient: TRESTClient;
    FRequest: TRESTRequest;
    FResponse: TRESTResponse;
    SetDetailsCache: TDictionary<string, TSetDetails>;

    // Helper methods
    procedure AddCustomHeaders;
    procedure LogError(const Msg: string);
    function ExecuteRequest(const Endpoint: string): TJSONObject;
    function ParseJSONArray<T>(const JSONArray: TJSONArray;
      ParseItem: TFunc<TJSONObject, T>): TArray<T>;

    // Parsing methods
    procedure FillCardDetailsFromJson(const JsonObj: TJSONObject;
      out CardDetails: TCardDetails);
    procedure ParseImageUris(const JsonObj: TJSONObject;
      out ImageUris: TImageUris);
    procedure ParseLegalities(const JsonObj: TJSONObject;
      out Legalities: TCardLegalities);
    procedure ParsePrices(const JsonObj: TJSONObject; out Prices: TCardPrices);
    procedure ParseCardFaces(const JsonObj: TJSONObject;
      out CardFaces: TArray<TCardFace>);
    procedure FillSetDetailsFromJson(const JsonObj: TJSONObject;
      out SetDetails: TSetDetails);

    // Internal methods
    function InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
  public
    constructor Create;
    destructor Destroy; override;

    // Public API methods
    function GetCardByName(const CardName: string; Fuzzy: Boolean = False)
      : TCardDetails;
    function SearchCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer = 1): TArray<TCardDetails>;
    function FetchUniquePrints(const CardName: string): TArray<TCardDetails>;
    function GetCardByMultiverseID(const MultiverseID: Integer): TCardDetails;
    function GetCardByScryfallID(const ScryfallID: string): TCardDetails;
    function GetCardBySetAndCollectorNumber(const SetCode, CollectorNumber
      : string): TCardDetails;
    function GetAllSets: TArray<TSetDetails>;
    function GetSetByCode(const SetCode: string): TSetDetails;
    function GetRulingsByScryfallID(const ScryfallID: string): TArray<TRuling>;
    function GetBulkDataList: TArray<TBulkData>;
    function Autocomplete(const Query: string): TArray<string>;
    function GetSymbology: TArray<TSymbol>;
    function GetCardNameCatalog: TArray<string>;
    function SearchAllCards(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Limit: Integer = 0): TArray<TCardDetails>;
    function SearchCardsWithPagination(const Query, SetCode, Rarity,
      Colors: string; Fuzzy, Unique: Boolean; Page: Integer = 1): TSearchResult;
    procedure SearchAllCardsAsync(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Callback: TOnSearchComplete);
  end;

implementation

{ TScryfallAPI }

constructor TScryfallAPI.Create;
begin
  inherited Create;
  FClient := TRESTClient.Create(BaseUrl);
  FRequest := TRESTRequest.Create(nil);
  FResponse := TRESTResponse.Create(nil);
  FRequest.Client := FClient;
  FRequest.Response := FResponse;
  SetDetailsCache := TDictionary<string, TSetDetails>.Create;
  AddCustomHeaders;
end;

destructor TScryfallAPI.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FClient.Free;
  SetDetailsCache.Free;
  inherited;
end;

procedure TScryfallAPI.AddCustomHeaders;
begin
  FRequest.Params.Clear;
  FRequest.Params.AddHeader('User-Agent', UserAgent);
  FRequest.Params.AddHeader('Accept', AcceptHeader);
end;

procedure TScryfallAPI.LogError(const Msg: string);
begin
  // TFile.AppendAllText('debug_log_mtgcards.txt', Format('DEBUG [%s]: %s%s',
  // [DateTimeToStr(Now), Msg, sLineBreak]), TEncoding.UTF8);
end;

function TScryfallAPI.ExecuteRequest(const Endpoint: string): TJSONObject;
var
  JsonValue: TJSONValue;
begin
  Result := nil;
  try
    FRequest.Params.Clear;
    FRequest.Resource := Endpoint;
    FRequest.Method := TRESTRequestMethod.rmGET;

    FRequest.Execute;

    case FResponse.StatusCode of
      200: // Success
        begin
          JsonValue := TJSONObject.ParseJSONValue(FResponse.Content);
          if JsonValue is TJSONObject then
            Result := TJSONObject(JsonValue)
          else
            JsonValue.Free;
        end;

      404: // Card or resource not found
        begin
          LogError(Format('Resource not found [%s]: %d - %s',
            [Endpoint, FResponse.StatusCode, FResponse.StatusText]));
          raise EScryfallAPIError.CreateFmt('Card not found [%s]: %s',
            [Endpoint, FResponse.StatusText]);
        end;

    else // Other errors
      begin
        LogError(Format('API Error [%s]: %d - %s',
          [Endpoint, FResponse.StatusCode, FResponse.StatusText]));
        raise EScryfallAPIError.CreateFmt('API Error [%s]: %d - %s',
          [Endpoint, FResponse.StatusCode, FResponse.StatusText]);
      end;
    end;

  except
    on E: Exception do
    begin
      LogError('Network request failed: ' + E.Message);
      raise;
    end;
  end;
end;

function TScryfallAPI.ParseJSONArray<T>(const JSONArray: TJSONArray;
  ParseItem: TFunc<TJSONObject, T>): TArray<T>;
var
  I: Integer;
begin
  SetLength(Result, JSONArray.Count);
  for I := 0 to JSONArray.Count - 1 do
  begin
    Result[I] := ParseItem(JSONArray.Items[I] as TJSONObject);
  end;
end;

function TScryfallAPI.GetCardByName(const CardName: string; Fuzzy: Boolean)
  : TCardDetails;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
begin
  if Fuzzy then
    Endpoint := Format('%s?fuzzy=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(CardName)])
  else
    Endpoint := Format('%s?exact=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(CardName)]);

  LogError('GetCardByName Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
      FillCardDetailsFromJson(JsonResponse, Result)
    else
      Result.Clear;
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

function TScryfallAPI.SearchCardsWithPagination(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
begin
  Result := InternalSearchCards(Query, SetCode, Rarity, Colors, Fuzzy,
    Unique, Page);
end;

function TScryfallAPI.InternalSearchCards(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
var
  SearchUrl: string;
  JsonResponse: TJSONObject;
  CardsArray: TJSONArray;
begin
  // Construct the search URL
  if Fuzzy then
    SearchUrl := Format('%s?fuzzy=%s', [EndpointNamed, TNetEncoding.URL.Encode(Query)])
  else
  begin
    SearchUrl := Format('%s?q=%s', [EndpointSearch, TNetEncoding.URL.Encode(Query)]);
    if SetCode <> '' then
      SearchUrl := SearchUrl + '+set%3A' + TNetEncoding.URL.Encode(SetCode);
    if Rarity <> '' then
      SearchUrl := SearchUrl + '+rarity%3A' + TNetEncoding.URL.Encode(Rarity);
    if Colors <> '' then
      SearchUrl := SearchUrl + '+color%3A' + TNetEncoding.URL.Encode(Colors);
    if Unique then
      SearchUrl := SearchUrl + '&unique=prints';
  end;

  SearchUrl := SearchUrl + Format('&page=%d', [Page]);

  // Perform the request
  JsonResponse := ExecuteRequest(SearchUrl);
  try
    // Parse cards array
    CardsArray := TJSONParser.GetArray(JsonResponse, 'data');
    if Assigned(CardsArray) then
      Result.Cards := ParseJSONArray<TCardDetails>(CardsArray,
        function(CardObj: TJSONObject): TCardDetails
        begin
          FillCardDetailsFromJson(CardObj, Result);
        end)
    else
      Result.Cards := [];

    // Parse additional search metadata
    Result.HasMore := TJSONParser.GetBoolean(JsonResponse, 'has_more', False);
    Result.NextPageURL := TJSONParser.GetString(JsonResponse, 'next_page', '');
    Result.TotalCards := TJSONParser.GetInteger(JsonResponse, 'total_cards', 0);
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.FetchUniquePrints(const CardName: string)
  : TArray<TCardDetails>;
var
  SearchUrl: string;
  JsonResponse: TJSONObject;
  CardsArray: TJSONArray;
begin
  SearchUrl := Format('%s?q=%s&unique=prints',
    [EndpointSearch, TNetEncoding.URL.Encode(CardName)]);

  LogError('FetchUniquePrints Endpoint: ' + SearchUrl);

  JsonResponse := ExecuteRequest(SearchUrl);
  try
    if Assigned(JsonResponse) and JsonResponse.TryGetValue<TJSONArray>('data',
      CardsArray) then
    begin
      Result := ParseJSONArray<TCardDetails>(CardsArray,
        function(CardObj: TJSONObject): TCardDetails
        begin
          FillCardDetailsFromJson(CardObj, Result);
        end);
    end
    else
    begin
      LogError('No data found in JSON response.');
      SetLength(Result, 0);
    end;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardByMultiverseID(const MultiverseID: Integer)
  : TCardDetails;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
begin
  Endpoint := Format('%smultiverse/%d', [EndpointCards, MultiverseID]);

  LogError('GetCardByMultiverseID Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
      FillCardDetailsFromJson(JsonResponse, Result)
    else
      Result.Clear;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardByScryfallID(const ScryfallID: string)
  : TCardDetails;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
begin
  Endpoint := EndpointCards + TNetEncoding.URL.Encode(ScryfallID);

  LogError('GetCardByScryfallID Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
      FillCardDetailsFromJson(JsonResponse, Result)
    else
      Result.Clear;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardBySetAndCollectorNumber(const SetCode,
  CollectorNumber: string): TCardDetails;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
begin
  Endpoint := Format('%s%s/%s', [EndpointCards,
    TNetEncoding.URL.Encode(SetCode),
    TNetEncoding.URL.Encode(CollectorNumber)]);

  LogError('GetCardBySetAndCollectorNumber Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
      FillCardDetailsFromJson(JsonResponse, Result)
    else
      Result.Clear;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetAllSets: TArray<TSetDetails>;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
  SetsArray: TJSONArray;
begin
  Endpoint := EndpointSets;

  LogError('GetAllSets Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) and JsonResponse.TryGetValue<TJSONArray>('data',
      SetsArray) then
    begin
      Result := ParseJSONArray<TSetDetails>(SetsArray,
        function(SetObj: TJSONObject): TSetDetails
        begin
          FillSetDetailsFromJson(SetObj, Result);
        end);
    end
    else
    begin
      LogError('No data found in JSON response.');
      SetLength(Result, 0);
    end;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetSetByCode(const SetCode: string): TSetDetails;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
begin
  Endpoint := Format('%s%s', [EndpointSets, TNetEncoding.URL.Encode(SetCode)]);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
      FillSetDetailsFromJson(JsonResponse, Result)
    else
      raise EScryfallAPIError.Create('Set details not found.');
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetSetByCodeCached(const SetCode: string): TSetDetails;
begin
  if not SetDetailsCache.TryGetValue(SetCode, Result) then
  begin
    Result := GetSetByCode(SetCode);
    SetDetailsCache.AddOrSetValue(SetCode, Result);
  end;
end;

function TScryfallAPI.GetRulingsByScryfallID(const ScryfallID: string)
  : TArray<TRuling>;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
  RulingsArray: TJSONArray;
begin
  Endpoint := Format('%s%s/rulings',
    [EndpointCards, TNetEncoding.URL.Encode(ScryfallID)]);

  LogError('GetRulingsByScryfallID Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) and JsonResponse.TryGetValue<TJSONArray>('data',
      RulingsArray) then
    begin
      Result := ParseJSONArray<TRuling>(RulingsArray,
        function(RulingObj: TJSONObject): TRuling
        begin
          Result.Source := RulingObj.GetValue<string>('source', '');
          Result.PublishedAt := RulingObj.GetValue<string>('published_at', '');
          Result.Comment := RulingObj.GetValue<string>('comment', '');
        end);
    end
    else
    begin
      LogError('No data found in JSON response.');
      SetLength(Result, 0);
    end;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetBulkDataList: TArray<TBulkData>;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
  DataArray: TJSONArray;
begin
  Endpoint := EndpointBulkData;

  LogError('GetBulkDataList Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) and JsonResponse.TryGetValue<TJSONArray>('data',
      DataArray) then
    begin
      Result := ParseJSONArray<TBulkData>(DataArray,
        function(DataObj: TJSONObject): TBulkData
        begin
          Result.ID := DataObj.GetValue<string>('id', '');
          Result.DataType := DataObj.GetValue<string>('type', '');
          Result.DownloadURI := DataObj.GetValue<string>('download_uri', '');
          Result.UpdatedAt := DataObj.GetValue<string>('updated_at', '');
        end);
    end
    else
    begin
      LogError('No data found in JSON response.');
      SetLength(Result, 0);
    end;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.Autocomplete(const Query: string): TArray<string>;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
  DataArray: TJSONArray;
  I: Integer;
begin
  Endpoint := Format('%s?q=%s', [EndpointAutocomplete,
    TNetEncoding.URL.Encode(Query)]);

  LogError('Autocomplete Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) and JsonResponse.TryGetValue<TJSONArray>('data',
      DataArray) then
    begin
      SetLength(Result, DataArray.Count);
      for I := 0 to DataArray.Count - 1 do
        Result[I] := DataArray.Items[I].Value;
    end
    else
    begin
      LogError('No data found in JSON response.');
      SetLength(Result, 0);
    end;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetSymbology: TArray<TSymbol>;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
  SymbolsArray: TJSONArray;
begin
  Endpoint := EndpointSymbology;

  LogError('GetSymbology Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) and JsonResponse.TryGetValue<TJSONArray>('data',
      SymbolsArray) then
    begin
      Result := ParseJSONArray<TSymbol>(SymbolsArray,
        function(SymbolObj: TJSONObject): TSymbol
        begin
          Result.Symbol := SymbolObj.GetValue<string>('symbol', '');
          Result.English := SymbolObj.GetValue<string>('english', '');
          Result.SVGURI := SymbolObj.GetValue<string>('svg_uri', '');
        end);
    end
    else
    begin
      LogError('No data found in JSON response.');
      SetLength(Result, 0);
    end;
  finally
    JsonResponse.Free;
  end;
end;

function TScryfallAPI.GetCardNameCatalog: TArray<string>;
var
  Endpoint: string;
  JsonResponse: TJSONObject;
  DataArray: TJSONArray;
  I: Integer;
begin
  Endpoint := EndpointCardNames;

  LogError('GetCardNameCatalog Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) and JsonResponse.TryGetValue<TJSONArray>('data',
      DataArray) then
    begin
      SetLength(Result, DataArray.Count);
      for I := 0 to DataArray.Count - 1 do
        Result[I] := DataArray.Items[I].Value;
    end
    else
    begin
      LogError('No data found in JSON response.');
      SetLength(Result, 0);
    end;
  finally
    JsonResponse.Free;
  end;
end;

procedure TScryfallAPI.FillCardDetailsFromJson(const JsonObj: TJSONObject; out CardDetails: TCardDetails);
var
  I: Integer;
  FaceManaCosts, FaceTypeLines, FaceOracleTexts: TStringList;
  Layout: string;
  SetDetails: TSetDetails;
begin
  CardDetails.Clear;

  // Parse basic fields using TJSONParser
  with TJSONParser do
  begin
    CardDetails.SFID := GetString(JsonObj, 'id', '');
    CardDetails.CardName := GetString(JsonObj, 'name', '');
    CardDetails.TypeLine := GetString(JsonObj, 'type_line', '');
    CardDetails.ManaCost := GetString(JsonObj, 'mana_cost', '');
    CardDetails.OracleText := GetString(JsonObj, 'oracle_text', '');
    CardDetails.SetCode := GetString(JsonObj, 'set', '');
    CardDetails.SetName := GetString(JsonObj, 'set_name', '');
    CardDetails.Rarity := GetString(JsonObj, 'rarity', '');
    CardDetails.Power := GetString(JsonObj, 'power', '');
    CardDetails.Toughness := GetString(JsonObj, 'toughness', '');
    CardDetails.Loyalty := GetString(JsonObj, 'loyalty', '');
    CardDetails.PrintsSearchUri := GetString(JsonObj, 'prints_search_uri', '');
    CardDetails.OracleID := GetString(JsonObj, 'oracle_id', '');
    CardDetails.FlavorText := GetString(JsonObj, 'flavor_text', '');
    CardDetails.Layout := GetString(JsonObj, 'layout', '').ToLower;
  end;

  Layout := CardDetails.Layout;

  // Parse nested objects
  ParseImageUris(JsonObj, CardDetails.ImageUris);
  ParseLegalities(JsonObj, CardDetails.Legalities);
  ParsePrices(JsonObj, CardDetails.Prices);
  ParseCardFaces(JsonObj, CardDetails.CardFaces);

  // Fetch set details and assign the set icon URI
  try
    SetDetails := GetSetByCodeCached(CardDetails.SetCode);
    CardDetails.SetIconURI := SetDetails.IconSVGURI;
  except
    on E: Exception do
      LogError('Failed to fetch set details: ' + E.Message);
  end;

  // Handle cards with multiple faces or special layouts
  if Length(CardDetails.CardFaces) > 0 then
  begin
    FaceManaCosts := TStringList.Create;
    FaceTypeLines := TStringList.Create;
    FaceOracleTexts := TStringList.Create;

    try
      for I := 0 to High(CardDetails.CardFaces) do
      begin
        if CardDetails.CardFaces[I].ManaCost <> '' then
          FaceManaCosts.Add(CardDetails.CardFaces[I].ManaCost);

        if CardDetails.CardFaces[I].TypeLine <> '' then
          FaceTypeLines.Add(CardDetails.CardFaces[I].TypeLine);

        if CardDetails.CardFaces[I].OracleText <> '' then
          FaceOracleTexts.Add(CardDetails.CardFaces[I].OracleText);
      end;

      // Aggregate fields if main fields are empty
      if CardDetails.ManaCost.IsEmpty then
        CardDetails.ManaCost := String.Join(' // ', FaceManaCosts.ToStringArray);

      if CardDetails.TypeLine.IsEmpty then
        CardDetails.TypeLine := String.Join(' // ', FaceTypeLines.ToStringArray);

      if CardDetails.OracleText.IsEmpty then
        CardDetails.OracleText := String.Join(sLineBreak + '//' + sLineBreak, FaceOracleTexts.ToStringArray);

      // Handle image URIs for specific layouts
      if (Layout = 'transform') or (Layout = 'modal_dfc') then
      begin
        if Length(CardDetails.CardFaces) > 1 then
        begin
          CardDetails.ImageUris.Normal := CardDetails.CardFaces[0].ImageUris.Normal;
          CardDetails.ImageUris.BackFace := CardDetails.CardFaces[1].ImageUris.Normal;
        end;
      end
      else if (Layout = 'split') or (Layout = 'adventure') then
      begin
        if Length(CardDetails.CardFaces) > 0 then
          CardDetails.ImageUris.Normal := CardDetails.ImageUris.Normal;
      end;
    finally
      FaceManaCosts.Free;
      FaceTypeLines.Free;
      FaceOracleTexts.Free;
    end;
  end;
end;

procedure TScryfallAPI.ParseImageUris(const JsonObj: TJSONObject;
out ImageUris: TImageUris);
begin
  ImageUris.Small := TJSONParser.GetString(JsonObj, 'image_uris.small', '');
  ImageUris.Normal := TJSONParser.GetString(JsonObj, 'image_uris.normal', '');
  ImageUris.Large := TJSONParser.GetString(JsonObj, 'image_uris.large', '');
  ImageUris.PNG := TJSONParser.GetString(JsonObj, 'image_uris.png', '');
  ImageUris.border_crop := TJSONParser.GetString(JsonObj,
    'image_uris.border_crop', '');
  ImageUris.art_crop := TJSONParser.GetString(JsonObj,
    'image_uris.art_crop', '');
end;

procedure TScryfallAPI.ParseLegalities(const JsonObj: TJSONObject; out Legalities: TCardLegalities);
var
  LegalitiesObj: TJSONObject;
begin
  LegalitiesObj := TJSONParser.GetObject(JsonObj, 'legalities');
  if Assigned(LegalitiesObj) then
  begin
    Legalities.Standard := TJSONParser.GetString(LegalitiesObj, 'standard', '');
    Legalities.Pioneer := TJSONParser.GetString(LegalitiesObj, 'pioneer', '');
    Legalities.Modern := TJSONParser.GetString(LegalitiesObj, 'modern', '');
    Legalities.Legacy := TJSONParser.GetString(LegalitiesObj, 'legacy', '');
    Legalities.Commander := TJSONParser.GetString(LegalitiesObj, 'commander', '');
    Legalities.Vintage := TJSONParser.GetString(LegalitiesObj, 'vintage', '');
    Legalities.Pauper := TJSONParser.GetString(LegalitiesObj, 'pauper', '');
    Legalities.Historic := TJSONParser.GetString(LegalitiesObj, 'historic', '');
    Legalities.Explorer := TJSONParser.GetString(LegalitiesObj, 'explorer', '');
    Legalities.Alchemy := TJSONParser.GetString(LegalitiesObj, 'alchemy', '');
    Legalities.Brawl := TJSONParser.GetString(LegalitiesObj, 'brawl', '');
    Legalities.Future := TJSONParser.GetString(LegalitiesObj, 'future', '');
    Legalities.Oldschool := TJSONParser.GetString(LegalitiesObj, 'oldschool', '');
    Legalities.Premodern := TJSONParser.GetString(LegalitiesObj, 'premodern', '');
    Legalities.Duel := TJSONParser.GetString(LegalitiesObj, 'duel', '');
    Legalities.Penny := TJSONParser.GetString(LegalitiesObj, 'penny', '');
  end
  else
    Legalities := Default(TCardLegalities);
end;

procedure TScryfallAPI.ParsePrices(const JsonObj: TJSONObject; out Prices: TCardPrices);
var
  PricesObj: TJSONObject;
begin
  PricesObj := TJSONParser.GetObject(JsonObj, 'prices');
  if Assigned(PricesObj) then
  begin
    Prices.USD := TJSONParser.GetString(PricesObj, 'usd', '');
    Prices.USD_Foil := TJSONParser.GetString(PricesObj, 'usd_foil', '');
    Prices.EUR := TJSONParser.GetString(PricesObj, 'eur', '');
    Prices.Tix := TJSONParser.GetString(PricesObj, 'tix', '');
  end
  else
    Prices := Default(TCardPrices);
end;

procedure TScryfallAPI.ParseCardFaces(const JsonObj: TJSONObject; out CardFaces: TArray<TCardFace>);
var
  CardFacesArray: TJSONArray;
  I: Integer;
  CardFaceObj: TJSONObject;
begin
  CardFacesArray := TJSONParser.GetArray(JsonObj, 'card_faces');
  if Assigned(CardFacesArray) then
  begin
    SetLength(CardFaces, CardFacesArray.Count);
    for I := 0 to CardFacesArray.Count - 1 do
    begin
      CardFaceObj := CardFacesArray.Items[I] as TJSONObject;

      CardFaces[I].Name := TJSONParser.GetString(CardFaceObj, 'name', '');
      CardFaces[I].ManaCost := TJSONParser.GetString(CardFaceObj, 'mana_cost', '');
      CardFaces[I].TypeLine := TJSONParser.GetString(CardFaceObj, 'type_line', '');
      CardFaces[I].OracleText := TJSONParser.GetString(CardFaceObj, 'oracle_text', '');
      CardFaces[I].Power := TJSONParser.GetString(CardFaceObj, 'power', '');
      CardFaces[I].Toughness := TJSONParser.GetString(CardFaceObj, 'toughness', '');


      ParseImageUris(CardFaceObj, CardFaces[I].ImageUris);
    end;
  end
  else
    SetLength(CardFaces, 0);
end;


procedure TScryfallAPI.FillSetDetailsFromJson(const JsonObj: TJSONObject; out SetDetails: TSetDetails);
begin
  SetDetails.Clear;

  // Using TJSONParser for simplified parsing
  with TJSONParser do
  begin
    SetDetails.SFID := GetString(JsonObj, 'id', '');
    SetDetails.Code := GetString(JsonObj, 'code', '');
    SetDetails.Name := GetString(JsonObj, 'name', '');
    SetDetails.ReleaseDate := GetString(JsonObj, 'released_at', '');
    SetDetails.SetType := GetString(JsonObj, 'set_type', '');
    SetDetails.Block := GetString(JsonObj, 'block', '');
    SetDetails.BlockCode := GetString(JsonObj, 'block_code', '');
    SetDetails.ParentSetCode := GetString(JsonObj, 'parent_set_code', '');
    SetDetails.CardCount := GetInteger(JsonObj, 'card_count', 0);
    SetDetails.Digital := GetBoolean(JsonObj, 'digital', False);
    SetDetails.FoilOnly := GetBoolean(JsonObj, 'foil_only', False);
    SetDetails.IconSVGURI := GetString(JsonObj, 'icon_svg_uri', '');
    SetDetails.ScryfallURI := GetString(JsonObj, 'scryfall_uri', '');
    SetDetails.URI := GetString(JsonObj, 'uri', '');
    SetDetails.SearchURI := GetString(JsonObj, 'search_uri', '');
  end;
end;

function TScryfallAPI.SearchAllCards(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Limit: Integer = 0)
  : TArray<TCardDetails>;
var
  PageNumber, FetchedCount: Integer;
  SearchResult: TSearchResult;
  AllCards: TList<TCardDetails>;
begin
  AllCards := TList<TCardDetails>.Create;
  try
    PageNumber := 1;
    FetchedCount := 0;

    repeat
      SearchResult := SearchCardsWithPagination(Query, SetCode, Rarity, Colors,
        Fuzzy, Unique, PageNumber);
      AllCards.AddRange(SearchResult.Cards);
      Inc(FetchedCount, Length(SearchResult.Cards));
      Inc(PageNumber);
    until not SearchResult.HasMore or ((Limit > 0) and (FetchedCount >= Limit));

    Result := AllCards.ToArray;
  finally
    AllCards.Free;
  end;
end;

procedure TScryfallAPI.SearchAllCardsAsync(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Callback: TOnSearchComplete);
begin
  TTask.Run(
    procedure
    var
      Cards: TArray<TCardDetails>;
    begin
      try
        Cards := SearchAllCards(Query, SetCode, Rarity, Colors, Fuzzy, Unique);
        TThread.Queue(nil,
          procedure
          begin
            Callback(True, Cards, '');
          end);
      except
        on E: Exception do
        begin
          TThread.Queue(nil,
            procedure
            begin
              Callback(False, nil, E.Message);
            end);
        end;
      end;
    end);
end;

end.
