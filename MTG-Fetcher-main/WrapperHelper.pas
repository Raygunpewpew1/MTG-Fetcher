unit WrapperHelper;

interface

uses
  System.SysUtils, System.NetEncoding, System.Classes,
  JsonDataObjects, SGlobalsZ, Logger;

function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;

procedure ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
procedure ParseLegalities(const JsonObj: TJsonObject;
  out Legalities: TCardLegalities);
procedure ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
procedure ParseCardFaces(const JsonObj: TJsonObject;
  out CardFaces: TArray<TCardFace>);
procedure FillSetDetailsFromJson(const JsonObj: TJsonObject;
  out SetDetails: TSetDetails);
procedure FillCardDetailsFromJson(const JsonObj: TJsonObject;
  out CardDetails: TCardDetails);

implementation

uses
  APIConstants, CardDisplayHelpers, System.StrUtils, System.Character;

function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;
var
  EncodedQuery, EncSetCode, EncRarity, EncColors: string;
  BaseUrl: string;
begin
  // If we want a fuzzy search, bail out early
  if Fuzzy then
    Exit(Format(FuzzySStr, [EndpointNamed, TNetEncoding.URL.Encode(Query)]));

  // Cache pre-encoded strings (faster than repeated calls)
  EncodedQuery := TNetEncoding.URL.Encode(Query.ToLower);
  EncSetCode := TNetEncoding.URL.Encode(SetCode.ToLower);
  EncRarity := TNetEncoding.URL.Encode(Rarity.ToLower);
  EncColors := TNetEncoding.URL.Encode(Colors.ToLower);

  // Start building the base URL
  BaseUrl := Format(StandardSStr, [EndpointSearch, EncodedQuery]);

  if not SetCode.IsEmpty then
    BaseUrl := BaseUrl + BySetCode + EncSetCode;
  if not Rarity.IsEmpty then
    BaseUrl := BaseUrl + ByRarity + EncRarity;
  if not Colors.IsEmpty then
    BaseUrl := BaseUrl + ByColor + EncColors;
  if Unique then
    BaseUrl := BaseUrl + ShowUQ;

  Result := BaseUrl + Format(SPageStr, [Page]);
end;

function GetSafeStringField(const Obj: TJsonObject; const FieldName: string): string;
begin
  if Obj.Contains(FieldName) and (Obj.Types[FieldName] = jdtString) then
    Result := Obj.S[FieldName]
  else
    Result := '';
end;

{$IF DEFINED(MSWINDOWS)}

function GetUtf8String(const S: string): string;
begin
  // On Windows, convert from ANSI to UTF8.
  Result := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(S));
end;

{$ELSE}

function GetUtf8String(const S: string): string;
begin
  // On non-Windows platforms, no conversion needed.
  Result := S;
end;

{$ENDIF}

procedure GetSafeStringArrayField(const Obj: TJsonObject;
  const FieldName: string; out Arr: TArray<string>);
var
  JArr: TJsonArray;
  I: Integer;
begin
  if Obj.Contains(FieldName) and (Obj.Types[FieldName] = jdtArray) then
  begin
    JArr := Obj.A[FieldName];
    SetLength(Arr, JArr.Count);
    for I := 0 to JArr.Count - 1 do
      if JArr.Types[I] = jdtString then
        Arr[I] := JArr.S[I]
      else
        Arr[I] := '';
  end
  else
    SetLength(Arr, 0);
end;

procedure ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
var
  ImageUrisObj: TJsonObject;
begin
  if JsonObj.Contains(FieldImageUris) and
    (JsonObj.Types[FieldImageUris] = jdtObject) then
  begin
    ImageUrisObj := JsonObj.O[FieldImageUris];
    ImageUris.Small       := GetSafeStringField(ImageUrisObj, FieldSmall);
    ImageUris.Normal      := GetSafeStringField(ImageUrisObj, FieldNormal);
    ImageUris.Large       := GetSafeStringField(ImageUrisObj, FieldLarge);
    ImageUris.PNG         := GetSafeStringField(ImageUrisObj, FieldPng);
    ImageUris.Border_crop := GetSafeStringField(ImageUrisObj, FieldBorderCrop);
    ImageUris.Art_crop    := GetSafeStringField(ImageUrisObj, FieldArtCrop);
  end
  else
    ImageUris := Default(TImageUris);
end;

procedure ParseLegalities(const JsonObj: TJsonObject; out Legalities: TCardLegalities);
var
  LegalitiesObj: TJsonObject;
begin
  if JsonObj.Contains(FieldLegalities) and
    (JsonObj.Types[FieldLegalities] = jdtObject) then
  begin
    LegalitiesObj := JsonObj.O[FieldLegalities];

    Legalities.Standard        := GetSafeStringField(LegalitiesObj, FieldStandard);
    Legalities.Future          := GetSafeStringField(LegalitiesObj, FieldFuture);
    Legalities.Historic        := GetSafeStringField(LegalitiesObj, FieldHistoric);
    Legalities.Gladiator       := GetSafeStringField(LegalitiesObj, FieldGladiator);
    Legalities.Pioneer         := GetSafeStringField(LegalitiesObj, FieldPioneer);
    Legalities.Explorer        := GetSafeStringField(LegalitiesObj, FieldExplorer);
    Legalities.Modern          := GetSafeStringField(LegalitiesObj, FieldModern);
    Legalities.Legacy          := GetSafeStringField(LegalitiesObj, FieldLegacy);
    Legalities.Pauper          := GetSafeStringField(LegalitiesObj, FieldPauper);
    Legalities.Vintage         := GetSafeStringField(LegalitiesObj, FieldVintage);
    Legalities.Penny           := GetSafeStringField(LegalitiesObj, FieldPenny);
    Legalities.Commander       := GetSafeStringField(LegalitiesObj, FieldCommander);
    Legalities.Oathbreaker     := GetSafeStringField(LegalitiesObj, FieldOathbreaker);
    Legalities.Alchemy         := GetSafeStringField(LegalitiesObj, FieldAlchemy);
    Legalities.Brawl           := GetSafeStringField(LegalitiesObj, FieldBrawl);
    Legalities.PauperCommander := GetSafeStringField(LegalitiesObj, FieldPauperCommander);
    Legalities.Duel            := GetSafeStringField(LegalitiesObj, FieldDuel);
    Legalities.Oldschool       := GetSafeStringField(LegalitiesObj, FieldOldschool);
    Legalities.Premodern       := GetSafeStringField(LegalitiesObj, FieldPremodern);
  end
  else
    Legalities := Default(TCardLegalities);
end;

procedure ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
var
  PricesObj: TJsonObject;
begin
  if JsonObj.Contains(FieldPrices) and (JsonObj.Types[FieldPrices] = jdtObject) then
  begin
    PricesObj := JsonObj.O[FieldPrices];
    Prices.USD      := GetSafeStringField(PricesObj, FieldUsd);
    Prices.USD_Foil := GetSafeStringField(PricesObj, FieldUsdFoil);
    Prices.EUR      := GetSafeStringField(PricesObj, FieldEur);
    Prices.Tix      := GetSafeStringField(PricesObj, FieldTix);
  end
  else
    Prices := Default(TCardPrices);
end;

procedure ParseCardFaces(const JsonObj: TJsonObject; out CardFaces: TArray<TCardFace>);
var
  FacesArray: TJsonArray;
  I: Integer;
  FaceObj: TJsonObject;
begin
  if JsonObj.Contains(FieldCardFaces) and
    (JsonObj.Types[FieldCardFaces] = jdtArray) then
  begin
    FacesArray := JsonObj.A[FieldCardFaces];
    SetLength(CardFaces, FacesArray.Count);

    for I := 0 to FacesArray.Count - 1 do
    begin
      if FacesArray.Types[I] = jdtObject then
      begin
        FaceObj := FacesArray.O[I];
        CardFaces[I].Name      := GetSafeStringField(FaceObj, FieldName);
        CardFaces[I].ManaCost  := GetSafeStringField(FaceObj, FieldManaCost);
        CardFaces[I].TypeLine  := GetSafeStringField(FaceObj, FieldTypeLine);

        CardFaces[I].OracleText := GetUtf8String(
          GetSafeStringField(FaceObj, FieldOracleText)
        );
        CardFaces[I].FlavorText := GetUtf8String(
          GetSafeStringField(FaceObj, FieldFlavorText)
        );

        CardFaces[I].Power     := GetSafeStringField(FaceObj, FieldPower);
        CardFaces[I].Toughness := GetSafeStringField(FaceObj, FieldToughness);
        CardFaces[I].Loyalty   := GetSafeStringField(FaceObj, FieldCardFaceLoyalty);

        ParseImageUris(FaceObj, CardFaces[I].ImageUris);
      end;
    end;
  end
  else
    SetLength(CardFaces, 0);
end;

procedure FillSetDetailsFromJson(const JsonObj: TJsonObject; out SetDetails: TSetDetails);
begin
  SetDetails.Clear;

  SetDetails.SFID         := GetSafeStringField(JsonObj, FieldID);
  SetDetails.Name         := GetSafeStringField(JsonObj, FieldName);
  SetDetails.Code         := GetSafeStringField(JsonObj, FieldCode);
  SetDetails.ReleaseDate  := GetSafeStringField(JsonObj, FieldReleasedAt);
  SetDetails.SetType      := GetSafeStringField(JsonObj, FieldSetType);
  SetDetails.Block        := GetSafeStringField(JsonObj, FieldBlock);
  SetDetails.BlockCode    := GetSafeStringField(JsonObj, FieldBlockCode);
  SetDetails.ParentSetCode:= GetSafeStringField(JsonObj, FieldParentSetCode);

  SetDetails.CardCount    := JsonObj.I[FieldCardCount];
  SetDetails.Digital      := JsonObj.B[FieldDigital];
  SetDetails.FoilOnly     := JsonObj.B[FieldFoilOnly];

  SetDetails.IconSVGURI   := GetUtf8String(
    GetSafeStringField(JsonObj, FieldIconSvgUri)
  );

  SetDetails.ScryfallURI  := GetSafeStringField(JsonObj, FieldScryfallUri);
  SetDetails.URI          := GetSafeStringField(JsonObj, FieldUri);
  SetDetails.SearchURI    := GetSafeStringField(JsonObj, FieldSearchUri);
end;

procedure FillCardDetailsFromJson(const JsonObj: TJsonObject; out CardDetails: TCardDetails);
begin
  // If we already have IDs, clear them out before refilling
  if (not CardDetails.SFID.IsEmpty) or (not CardDetails.OracleID.IsEmpty) then
    CardDetails.Clear;

  try
    // TypeLine
    if JsonObj.Contains(FieldTypeLine) and
      (JsonObj.Types[FieldTypeLine] = jdtString) then
      CardDetails.TypeLine := GetUtf8String(JsonObj.S[FieldTypeLine]);

    CardDetails.SFID     := GetSafeStringField(JsonObj, FieldID);
    CardDetails.ArenaID  := JsonObj.I[FieldArena];
    CardDetails.EDHRank  := JsonObj.I[FieldEDHRank];

    CardDetails.CardName := GetUtf8String(
      GetSafeStringField(JsonObj, FieldName)
    );
    CardDetails.ManaCost := GetSafeStringField(JsonObj, FieldManaCost);

    CardDetails.OracleText := GetUtf8String(
      GetSafeStringField(JsonObj, FieldOracleText)
    );

    // Games array
    GetSafeStringArrayField(JsonObj, FieldGames, CardDetails.Games);

    // Keywords array
    GetSafeStringArrayField(JsonObj, FieldKeywords, CardDetails.Keywords);

    CardDetails.SetCode         := GetSafeStringField(JsonObj, FieldSet);
    CardDetails.SetName         := GetSafeStringField(JsonObj, FieldSetName);
    CardDetails.Rarity          := StringToRarity(JsonObj.S['rarity']);
    CardDetails.Power           := GetSafeStringField(JsonObj, FieldPower);
    CardDetails.Toughness       := GetSafeStringField(JsonObj, FieldToughness);
    CardDetails.Loyalty         := GetSafeStringField(JsonObj, FieldLoyalty);
    CardDetails.PrintsSearchUri := GetSafeStringField(JsonObj, FieldPrintsSearchUri);
    CardDetails.OracleID        := GetSafeStringField(JsonObj, FieldOracleID);
    CardDetails.FlavorText      := GetUtf8String(
      GetSafeStringField(JsonObj, FieldFlavorText)
    );

    CardDetails.Layout     := GetSafeStringField(JsonObj, FieldLayout).ToLower;
    CardDetails.Lang       := GetSafeStringField(JsonObj, FieldLang);
    CardDetails.ReleasedAt := GetSafeStringField(JsonObj, FieldReleasedAt);

    if (JsonObj.Contains(FieldCMC)) and (JsonObj.Types[FieldCMC] = jdtFloat) then
      CardDetails.CMC := JsonObj.F[FieldCMC];

    CardDetails.Reserved    := JsonObj.B[FieldReserved];
    CardDetails.Foil        := JsonObj.B[FieldFoil];
    CardDetails.NonFoil     := JsonObj.B[FieldNonFoil];
    CardDetails.Oversized   := JsonObj.B[FieldOversized];
    CardDetails.Promo       := JsonObj.B[FieldPromo];
    CardDetails.Reprint     := JsonObj.B[FieldReprint];
    CardDetails.Digital     := JsonObj.B[FieldDigital];
    CardDetails.Artist      := GetSafeStringField(JsonObj, FieldArtist);
    CardDetails.CollectorNumber := GetSafeStringField(JsonObj, FieldCollectorNumber);
    CardDetails.BorderColor := GetSafeStringField(JsonObj, FieldBorderColor);
    CardDetails.Frame       := GetSafeStringField(JsonObj, FieldFrame);
    CardDetails.SecurityStamp := GetSafeStringField(JsonObj, FieldSecurityStamp);
    CardDetails.FullArt     := JsonObj.B[FieldFullArt];
    CardDetails.Textless    := JsonObj.B[FieldTextless];
    CardDetails.StorySpotlight := JsonObj.B[FieldStorySpotlight];

    GetSafeStringArrayField(JsonObj, FieldColorIdentity, CardDetails.ColorIdentity);

    // Nested objects
    ParseImageUris(JsonObj, CardDetails.ImageUris);
    ParseLegalities(JsonObj, CardDetails.Legalities);
    ParsePrices(JsonObj, CardDetails.Prices);
    ParseCardFaces(JsonObj, CardDetails.CardFaces);
  except
    on E: Exception do
    begin
      LogStuff(Format(ErrorFillingCardDetails, [E.Message]));
      CardDetails.Clear;
    end;
  end;
end;

end.
