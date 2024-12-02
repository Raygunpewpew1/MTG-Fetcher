unit ScryfallAPIWrapperV2;

interface

uses
  System.Classes, System.SysUtils, REST.Client, REST.Types, JSON,
  Data.Bind.Components, Data.Bind.ObjectScope, System.NetEncoding,
  System.Generics.Collections, System.IOUtils, System.Net.HttpClient,
  SGlobalsZ, system.Threading, System.Math;

type
  // Custom exception class for Scryfall API errors
  EScryfallAPIError = class(Exception);


  type
  TOnSearchComplete = reference to procedure(Success: Boolean; Cards: TArray<TCardDetails>; ErrorMsg: string);


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
    function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
      Fuzzy, Unique: Boolean; Page: Integer): string;
    function ParseSearchResult(const JsonResponse: TJSONObject): TSearchResult;


  var
    FClient: TRESTClient;
    FRequest: TRESTRequest;
    FResponse: TRESTResponse;
    FCache: TDictionary<string, TJSONObject>;

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
  FCache := TDictionary<string, TJSONObject>.Create;
  FRequest.Client := FClient;
  FRequest.Response := FResponse;
  AddCustomHeaders;
end;

destructor TScryfallAPI.Destroy;
begin
  FRequest.Free;
  FResponse.Free;
  FClient.Free;
  FCache.Free;
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
  //  Sleep(200);
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

function TScryfallAPI.ParseJSONArray<T>(const JSONArray: TJSONArray; ParseItem: TFunc<TJSONObject, T>): TArray<T>;
var
  I: Integer;
  ParsedItem: T;
begin
  SetLength(Result, JSONArray.Count);
  for I := 0 to JSONArray.Count - 1 do
  begin
    ParsedItem := ParseItem(JSONArray.Items[I] as TJSONObject);
    Result[I] := ParsedItem; // Use a local variable instead of directly referencing `Result`
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


function TScryfallAPI.ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;
var
  BaseUrl: string;
begin
  if Fuzzy then
  begin
    // Fuzzy search
    Result := Format('%s?fuzzy=%s', [EndpointNamed, TNetEncoding.URL.Encode(Query)]);
  end
  else
  begin
    // Exact or filtered search
    BaseUrl := Format('%s?q=%s', [EndpointSearch, TNetEncoding.URL.Encode(Query)]);

    // Add optional filters
    if SetCode <> '' then
      BaseUrl := BaseUrl + '+set%3A' + TNetEncoding.URL.Encode(SetCode);

    if Rarity <> '' then
      BaseUrl := BaseUrl + '+rarity%3A' + TNetEncoding.URL.Encode(Rarity);

    if Colors <> '' then
      BaseUrl := BaseUrl + '+color%3A' + TNetEncoding.URL.Encode(Colors);

    if Unique then
      BaseUrl := BaseUrl + '&unique=prints';

    // Append the page parameter
    Result := BaseUrl + Format('&page=%d', [Page]);
  end;
end;

function TScryfallAPI.ParseSearchResult(const JsonResponse: TJSONObject): TSearchResult;
var
  CardsArray: TJSONArray;
begin
  // Check if the "data" array exists in the JSON response
  if JsonResponse.TryGetValue<TJSONArray>('data', CardsArray) then
  begin
    // Parse the cards into an array of TCardDetails
    Result.Cards := ParseJSONArray<TCardDetails>(CardsArray,
      function(CardObj: TJSONObject): TCardDetails
      begin
        FillCardDetailsFromJson(CardObj, Result);
      end);
  end
  else
    SetLength(Result.Cards, 0); // No cards found

  // Parse metadata from the JSON
  Result.HasMore := JsonResponse.GetValue<Boolean>('has_more', False);
  Result.NextPageURL := JsonResponse.GetValue<string>('next_page', '');
  Result.TotalCards := JsonResponse.GetValue<Integer>('total_cards', 0);
end;


function TScryfallAPI.InternalSearchCards(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): TSearchResult;
var
  SearchUrl, CacheKey: string;
  CachedResponse: TJSONObject;
  CardsArray: TJSONArray;
  JsonResponse: TJSONObject;
begin
  // Generate a cache key to uniquely identify this request
  CacheKey := Format('search:%s:%s:%s:%s:%d', [Query, SetCode, Rarity, Colors, Page]);

  // Check if the result is already cached
  if FCache.TryGetValue(CacheKey, CachedResponse) then
  begin
    Result := ParseSearchResult(CachedResponse);
    Exit;
  end;

  // Construct the API URL
  SearchUrl := ConstructSearchUrl(Query, SetCode, Rarity, Colors, Fuzzy, Unique, Page);

  // Fetch the response from the API
  JsonResponse := ExecuteRequest(SearchUrl);
  try
    if JsonResponse.TryGetValue<TJSONArray>('data', CardsArray) then
    begin
      SetLength(Result.Cards, CardsArray.Count);

      for var I := 0 to CardsArray.Count - 1 do
        FillCardDetailsFromJson(CardsArray.Items[I] as TJSONObject, Result.Cards[I]);

      // Cache the response
      FCache.Add(CacheKey, JsonResponse.Clone as TJSONObject);

      // Parse additional metadata
      Result.HasMore := JsonResponse.GetValue<Boolean>('has_more', False);
      Result.NextPageURL := JsonResponse.GetValue<string>('next_page', '');
      Result.TotalCards := JsonResponse.GetValue<Integer>('total_cards', 0);
    end
    else
    begin
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
  SetDetails: TSetDetails;
begin
  // Clear the existing card details
  CardDetails.Clear;

  // Parse basic fields from the JSON object
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
  CardDetails.Loyalty := JsonObj.GetValue<string>('loyalty', '');
  CardDetails.PrintsSearchUri := JsonObj.GetValue<string>('prints_search_uri', '');
  CardDetails.OracleID := JsonObj.GetValue<string>('oracle_id', '');
  CardDetails.FlavorText := JsonObj.GetValue<string>('flavor_text', '');
  CardDetails.Layout := JsonObj.GetValue<string>('layout', '').ToLower;

  // Parse nested objects for images, legalities, prices, and faces
  ParseImageUris(JsonObj, CardDetails.ImageUris);
  ParseLegalities(JsonObj, CardDetails.Legalities);
  ParsePrices(JsonObj, CardDetails.Prices);
  ParseCardFaces(JsonObj, CardDetails.CardFaces);

  // Fetch set details if the set code exists
  if not CardDetails.SetCode.IsEmpty then
  begin
    try
      SetDetails := GetSetByCode(CardDetails.SetCode);
      CardDetails.SetIconURI := SetDetails.IconSVGURI;
    except
      on E: Exception do
        LogError('Failed to fetch set details: ' + E.Message);
    end;
  end;

  // Handle multi-face cards or special layouts
  if Length(CardDetails.CardFaces) > 0 then
  begin
    FaceManaCosts := TStringList.Create;
    FaceTypeLines := TStringList.Create;
    FaceOracleTexts := TStringList.Create;

    try
      // Aggregate data from card faces
      for I := 0 to High(CardDetails.CardFaces) do
      begin
        if not CardDetails.CardFaces[I].ManaCost.IsEmpty then
          FaceManaCosts.Add(CardDetails.CardFaces[I].ManaCost);
        if not CardDetails.CardFaces[I].TypeLine.IsEmpty then
          FaceTypeLines.Add(CardDetails.CardFaces[I].TypeLine);
        if not CardDetails.CardFaces[I].OracleText.IsEmpty then
          FaceOracleTexts.Add(CardDetails.CardFaces[I].OracleText);
      end;

      // Populate fields if the main fields are empty
      if CardDetails.ManaCost.IsEmpty then
        CardDetails.ManaCost := String.Join(' // ', FaceManaCosts.ToStringArray);
      if CardDetails.TypeLine.IsEmpty then
        CardDetails.TypeLine := String.Join(' // ', FaceTypeLines.ToStringArray);
      if CardDetails.OracleText.IsEmpty then
        CardDetails.OracleText := String.Join(sLineBreak + '//' + sLineBreak, FaceOracleTexts.ToStringArray);

      // Assign images for specific layouts
      if (CardDetails.Layout = 'transform') or (CardDetails.Layout = 'modal_dfc') then
      begin
        if Length(CardDetails.CardFaces) > 1 then
        begin
          CardDetails.ImageUris.Normal := CardDetails.CardFaces[0].ImageUris.Normal;
          CardDetails.ImageUris.BackFace := CardDetails.CardFaces[1].ImageUris.Normal;
        end;
      end
      else if (CardDetails.Layout = 'split') or (CardDetails.Layout = 'adventure') then
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

procedure TScryfallAPI.ParseCardFaces(const JsonObj: TJSONObject; out CardFaces: TArray<TCardFace>);
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
      CardFaces[I].OracleText := CardFaceObj.GetValue<string>('oracle_text', '');
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

procedure TScryfallAPI.SearchAllCardsAsync(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Callback: TOnSearchComplete);
begin
  // Run the blocking operation in a background thread
  TTask.Run(
    procedure
    var
      Cards: TArray<TCardDetails>;
      ErrorMessage: string;
    begin
      try
        // Perform the blocking call
        Cards := SearchAllCards(Query, SetCode, Rarity, Colors, Fuzzy, Unique);

        // Call the callback on the main thread with the results
        TThread.Synchronize(nil,
          procedure
          begin
            if Assigned(Callback) then
              Callback(True, Cards, '');
          end);
      except
        on E: Exception do
        begin
          // Handle any errors
          ErrorMessage := E.Message;

          TThread.Synchronize(nil,
            procedure
            begin
              if Assigned(Callback) then
                Callback(False, nil, ErrorMessage);
            end);
        end;
      end;
    end);
end;


end.
