unit ScryfallAPIWrapperV2;

interface

uses
  System.Classes, System.SysUtils, REST.Client, REST.Types, JSON,
  Data.Bind.Components, Data.Bind.ObjectScope, System.NetEncoding,
  System.Generics.Collections, System.IOUtils, System.Net.HttpClient,
  SGlobalsZ;

type
  // Custom exception class for Scryfall API errors
  EScryfallAPIError = class(Exception);

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

  var
    FClient: TRESTClient;
    FRequest: TRESTRequest;
    FResponse: TRESTResponse;

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
      Fuzzy, Unique: Boolean): TArray<TCardDetails>;
    function SearchCardsWithPagination(const Query, SetCode, Rarity,
      Colors: string; Fuzzy, Unique: Boolean; Page: Integer = 1): TSearchResult;
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
  AddCustomHeaders;
end;

destructor TScryfallAPI.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FClient.Free;
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

    if FResponse.StatusCode = 200 then
    begin
      JsonValue := TJSONObject.ParseJSONValue(FResponse.Content);
      if JsonValue is TJSONObject then
        Result := TJSONObject(JsonValue)
      else
        JsonValue.Free;
    end
    else
    begin
      LogError(Format('API Error [%s]: %d - %s',
        [Endpoint, FResponse.StatusCode, FResponse.StatusText]));
      raise EScryfallAPIError.CreateFmt('API Error [%s]: %d - %s',
        [Endpoint, FResponse.StatusCode, FResponse.StatusText]);
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
  if Fuzzy then
    SearchUrl := Format('%s?fuzzy=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(Query)])
  else
  begin
    SearchUrl := Format('%s?q=%s',
      [EndpointSearch, TNetEncoding.URL.Encode(Query)]);

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

  LogError('SearchCards Endpoint: ' + SearchUrl);

  JsonResponse := ExecuteRequest(SearchUrl);
  try
    if Assigned(JsonResponse) then
    begin
      if JsonResponse.TryGetValue<TJSONArray>('data', CardsArray) then
      begin
        Result.Cards := ParseJSONArray<TCardDetails>(CardsArray,
          function(CardObj: TJSONObject): TCardDetails
          begin
            FillCardDetailsFromJson(CardObj, Result);
          end);
      end
      else
      begin
        LogError('No data found in JSON response.');
        SetLength(Result.Cards, 0);
      end;

      Result.HasMore := JsonResponse.GetValue<Boolean>('has_more', False);
      Result.NextPageURL := JsonResponse.GetValue<string>('next_page', '');
      Result.TotalCards := JsonResponse.GetValue<Integer>('total_cards', 0);
    end
    else
    begin
      LogError('No JSON response received.');
      SetLength(Result.Cards, 0);
      Result.HasMore := False;
      Result.NextPageURL := '';
      Result.TotalCards := 0;
    end;
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
  Endpoint := EndpointSets + TNetEncoding.URL.Encode(SetCode);

  LogError('GetSetByCode Endpoint: ' + Endpoint);

  JsonResponse := ExecuteRequest(Endpoint);
  try
    if Assigned(JsonResponse) then
      FillSetDetailsFromJson(JsonResponse, Result)
    else
      Result.Clear;
  finally
    JsonResponse.Free;
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

procedure TScryfallAPI.FillCardDetailsFromJson(const JsonObj: TJSONObject;
out CardDetails: TCardDetails);
var
  I: Integer;
  FaceManaCosts, FaceTypeLines, FaceOracleTexts: TStringList;
  FacePowers, FaceToughnesses: TStringList;
begin
  CardDetails.Clear;

  // Simple fields
  CardDetails.SFID := JsonObj.GetValue<string>('id', '');
  CardDetails.CardName := JsonObj.GetValue<string>('name', '');
  CardDetails.TypeLine := JsonObj.GetValue<string>('type_line', '');
  CardDetails.ManaCost := JsonObj.GetValue<string>('mana_cost', '');
  CardDetails.OracleText := JsonObj.GetValue<string>('oracle_text', '');
  CardDetails.SetCode := JsonObj.GetValue<string>('set', '');
  CardDetails.SetName := JsonObj.GetValue<string>('set_name', '');
  CardDetails.Rarity := JsonObj.GetValue<string>('rarity', '');
  CardDetails.Power := JsonObj.GetValue<string>('power', '');
  CardDetails.Toughness := JsonObj.GetValue<string>('toughness', '');
  CardDetails.PrintsSearchUri := JsonObj.GetValue<string>
    ('prints_search_uri', '');
  CardDetails.OracleID := JsonObj.GetValue<string>('oracle_id', '');

  // Parse nested objects
  ParseImageUris(JsonObj, CardDetails.ImageUris);
  ParseLegalities(JsonObj, CardDetails.Legalities);
  ParsePrices(JsonObj, CardDetails.Prices);
  ParseCardFaces(JsonObj, CardDetails.CardFaces);

  // Handle double-faced cards
  if Length(CardDetails.CardFaces) > 0 then
  begin
    // Aggregate Mana Cost
    FaceManaCosts := TStringList.Create;
    try
      for I := 0 to High(CardDetails.CardFaces) do
        FaceManaCosts.Add(CardDetails.CardFaces[I].ManaCost);
      CardDetails.ManaCost := String.Join(' // ', FaceManaCosts.ToStringArray);
    finally
      FaceManaCosts.Free;
    end;

    // Aggregate Type Line
    if CardDetails.TypeLine = '' then
    begin
      FaceTypeLines := TStringList.Create;
      try
        for I := 0 to High(CardDetails.CardFaces) do
          FaceTypeLines.Add(CardDetails.CardFaces[I].TypeLine);
        CardDetails.TypeLine := String.Join(' // ',
          FaceTypeLines.ToStringArray);
      finally
        FaceTypeLines.Free;
      end;
    end;

    // Aggregate Oracle Text
    if CardDetails.OracleText = '' then
    begin
      FaceOracleTexts := TStringList.Create;
      try
        for I := 0 to High(CardDetails.CardFaces) do
          FaceOracleTexts.Add(CardDetails.CardFaces[I].OracleText);
        CardDetails.OracleText := String.Join(sLineBreak + '//' + sLineBreak,
          FaceOracleTexts.ToStringArray);
      finally
        FaceOracleTexts.Free;
      end;
    end;

    // Handle Power and Toughness
    if (CardDetails.Power = '') and (CardDetails.Toughness = '') then
    begin
      FacePowers := TStringList.Create;
      FaceToughnesses := TStringList.Create;
      try
        for I := 0 to High(CardDetails.CardFaces) do
        begin
          FacePowers.Add(CardDetails.CardFaces[I].Power);
          FaceToughnesses.Add(CardDetails.CardFaces[I].Toughness);
        end;
        CardDetails.Power := String.Join(' // ', FacePowers.ToStringArray);
        CardDetails.Toughness := String.Join(' // ',
          FaceToughnesses.ToStringArray);
      finally
        FacePowers.Free;
        FaceToughnesses.Free;
      end;
    end;

    // Handle Image URIs
    if CardDetails.ImageUris.Small = '' then
    begin
      // Use the image URIs of the first face as a default
      CardDetails.ImageUris := CardDetails.CardFaces[0].ImageUris;
    end;
  end;
end;

procedure TScryfallAPI.ParseImageUris(const JsonObj: TJSONObject;
out ImageUris: TImageUris);
var
  ImageUrisObj: TJSONObject;
begin
  if JsonObj.TryGetValue<TJSONObject>('image_uris', ImageUrisObj) then
  begin
    ImageUris.Small := ImageUrisObj.GetValue<string>('small', '');
    ImageUris.Normal := ImageUrisObj.GetValue<string>('normal', '');
    ImageUris.Large := ImageUrisObj.GetValue<string>('large', '');
    ImageUris.PNG := ImageUrisObj.GetValue<string>('png', '');
    ImageUris.border_crop := ImageUrisObj.GetValue<string>('border_crop', '');
    ImageUris.art_crop := ImageUrisObj.GetValue<string>('art_crop', '');
  end
  else
    ImageUris := Default (TImageUris);
end;

procedure TScryfallAPI.ParseLegalities(const JsonObj: TJSONObject;
out Legalities: TCardLegalities);
var
  LegalitiesObj: TJSONObject;
begin
  if JsonObj.TryGetValue<TJSONObject>('legalities', LegalitiesObj) then
  begin
    Legalities.Standard := LegalitiesObj.GetValue<string>('standard', '');
    Legalities.Pioneer := LegalitiesObj.GetValue<string>('pioneer', '');
    Legalities.Modern := LegalitiesObj.GetValue<string>('modern', '');
    Legalities.Legacy := LegalitiesObj.GetValue<string>('legacy', '');
    Legalities.Commander := LegalitiesObj.GetValue<string>('commander', '');
    Legalities.Vintage := LegalitiesObj.GetValue<string>('vintage', '');
    Legalities.Pauper := LegalitiesObj.GetValue<string>('pauper', '');
    Legalities.Historic := LegalitiesObj.GetValue<string>('historic', '');
    Legalities.Explorer := LegalitiesObj.GetValue<string>('explorer', '');
    Legalities.Alchemy := LegalitiesObj.GetValue<string>('alchemy', '');
    Legalities.Brawl := LegalitiesObj.GetValue<string>('brawl', '');
    Legalities.Future := LegalitiesObj.GetValue<string>('future', '');
    Legalities.Oldschool := LegalitiesObj.GetValue<string>('oldschool', '');
    Legalities.Premodern := LegalitiesObj.GetValue<string>('premodern', '');
    Legalities.Duel := LegalitiesObj.GetValue<string>('duel', '');
    Legalities.Penny := LegalitiesObj.GetValue<string>('penny', '');
  end
  else
    Legalities := Default (TCardLegalities);
end;

procedure TScryfallAPI.ParsePrices(const JsonObj: TJSONObject;
out Prices: TCardPrices);
var
  PricesObj: TJSONObject;
begin
  if JsonObj.TryGetValue<TJSONObject>('prices', PricesObj) then
  begin
    Prices.USD := PricesObj.GetValue<string>('usd', '');
    Prices.USD_Foil := PricesObj.GetValue<string>('usd_foil', '');
    Prices.EUR := PricesObj.GetValue<string>('eur', '');
    Prices.Tix := PricesObj.GetValue<string>('tix', '');
  end
  else
    Prices := Default (TCardPrices);
end;

procedure TScryfallAPI.ParseCardFaces(const JsonObj: TJSONObject;
out CardFaces: TArray<TCardFace>);
var
  CardFacesArray: TJSONArray;
  I: Integer;
  CardFaceObj: TJSONObject;
begin
  if JsonObj.TryGetValue<TJSONArray>('card_faces', CardFacesArray) then
  begin
    SetLength(CardFaces, CardFacesArray.Count);
    for I := 0 to CardFacesArray.Count - 1 do
    begin
      CardFaceObj := CardFacesArray.Items[I] as TJSONObject;
      CardFaces[I].Name := CardFaceObj.GetValue<string>('name', '');
      CardFaces[I].ManaCost := CardFaceObj.GetValue<string>('mana_cost', '');
      CardFaces[I].TypeLine := CardFaceObj.GetValue<string>('type_line', '');
      CardFaces[I].OracleText := CardFaceObj.GetValue<string>
        ('oracle_text', '');
      CardFaces[I].Power := CardFaceObj.GetValue<string>('power', '');
      CardFaces[I].Toughness := CardFaceObj.GetValue<string>('toughness', '');
      ParseImageUris(CardFaceObj, CardFaces[I].ImageUris);
    end;
  end
  else
    SetLength(CardFaces, 0);
end;

procedure TScryfallAPI.FillSetDetailsFromJson(const JsonObj: TJSONObject;
out SetDetails: TSetDetails);
begin
  SetDetails.Clear;

  SetDetails.SFID := JsonObj.GetValue<string>('id', '');
  SetDetails.Code := JsonObj.GetValue<string>('code', '');
  SetDetails.Name := JsonObj.GetValue<string>('name', '');
  SetDetails.ReleaseDate := JsonObj.GetValue<string>('released_at', '');
  SetDetails.SetType := JsonObj.GetValue<string>('set_type', '');
  SetDetails.Block := JsonObj.GetValue<string>('block', '');
  SetDetails.BlockCode := JsonObj.GetValue<string>('block_code', '');
  SetDetails.ParentSetCode := JsonObj.GetValue<string>('parent_set_code', '');
  SetDetails.CardCount := JsonObj.GetValue<Integer>('card_count', 0);
  SetDetails.Digital := JsonObj.GetValue<Boolean>('digital', False);
  SetDetails.FoilOnly := JsonObj.GetValue<Boolean>('foil_only', False);
  SetDetails.IconSVGURI := JsonObj.GetValue<string>('icon_svg_uri', '');
  SetDetails.ScryfallURI := JsonObj.GetValue<string>('scryfall_uri', '');
  SetDetails.URI := JsonObj.GetValue<string>('uri', '');
  SetDetails.SearchURI := JsonObj.GetValue<string>('search_uri', '');
end;

function TScryfallAPI.SearchAllCards(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean): TArray<TCardDetails>;
var
  PageNumber: Integer;
  SearchResult: TSearchResult;
  AllCards: TList<TCardDetails>;
begin
  AllCards := TList<TCardDetails>.Create;
  try
    PageNumber := 1;
    repeat
      SearchResult := SearchCardsWithPagination(Query, SetCode, Rarity, Colors,
        Fuzzy, Unique, PageNumber);
      AllCards.AddRange(SearchResult.Cards);
      Inc(PageNumber);
    until not SearchResult.HasMore;

    Result := AllCards.ToArray;
  finally
    AllCards.Free;
  end;
end;

end.
