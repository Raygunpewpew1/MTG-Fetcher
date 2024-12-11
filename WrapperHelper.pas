unit WrapperHelper;

interface

uses
  System.SysUtils, System.NetEncoding, System.Classes, System.IOUtils,
  JsonDataObjects, SGlobalsZ, APIConstants;



function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;
procedure LogError(const Msg: string);
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

function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;
var
  BaseUrl: string;
begin
  if Fuzzy then
    Result := Format('%s?fuzzy=%s',
      [EndpointNamed, TNetEncoding.URL.Encode(Query)])
  else
  begin
    BaseUrl := Format('%s?q=%s', [EndpointSearch,
      TNetEncoding.URL.Encode(Query.ToLower)]);

    if SetCode <> '' then
      BaseUrl := BaseUrl + '+set%3A' + TNetEncoding.URL.Encode(SetCode.ToLower);
    if Rarity <> '' then
      BaseUrl := BaseUrl + '+rarity%3A' + TNetEncoding.URL.Encode(Rarity.ToLower);
    if Colors <> '' then
      BaseUrl := BaseUrl + '+color%3A' + TNetEncoding.URL.Encode(Colors.ToLower);
    if Unique then
      BaseUrl := BaseUrl + '&unique=prints';

    Result := BaseUrl + Format('&page=%d', [Page]);
  end;
end;

procedure LogError(const Msg: string);
var
  LogFilePath: string;
  LogFile: TStreamWriter;
begin
  try
{$IF DEFINED(ANDROID)}
    // Save the log to the public "Downloads" folder
    LogFilePath := TPath.Combine(TPath.GetSharedDownloadsPath,
      'application_log.txt');
{$ELSEIF DEFINED(MSWINDOWS)}
    LogFilePath := TPath.Combine(TPath.GetDocumentsPath, LogFileName);
{$ENDIF}
    // Write to the log file
    LogFile := TStreamWriter.Create(LogFilePath, True, TEncoding.UTF8);
    try
      LogFile.WriteLine(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss',
        Now), Msg]));
    finally
      LogFile.Free;
    end;
  except
    // If logging fails, avoid crashing the app
  end;
end;

procedure ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
var
  ImageUrisObj: TJsonObject;
begin
  if JsonObj.Contains(FieldImageUris) and (JsonObj.Types[FieldImageUris] = jdtObject)
  then
  begin
    ImageUrisObj := JsonObj.O[FieldImageUris];
    if ImageUrisObj.Contains(FieldSmall) and
      (ImageUrisObj.Types[FieldSmall] = jdtString) then
      ImageUris.Small := ImageUrisObj.S[FieldSmall];
    if ImageUrisObj.Contains(FieldNormal) and
      (ImageUrisObj.Types[FieldNormal] = jdtString) then
      ImageUris.Normal := ImageUrisObj.S[FieldNormal];
    if ImageUrisObj.Contains(FieldLarge) and
      (ImageUrisObj.Types[FieldLarge] = jdtString) then
      ImageUris.Large := ImageUrisObj.S[FieldLarge];
    if ImageUrisObj.Contains(FieldPng) and (ImageUrisObj.Types[FieldPng] = jdtString)
    then
      ImageUris.PNG := ImageUrisObj.S[FieldPng];
    if ImageUrisObj.Contains(FieldBorderCrop) and
      (ImageUrisObj.Types[FieldBorderCrop] = jdtString) then
      ImageUris.border_crop := ImageUrisObj.S[FieldBorderCrop];
    if ImageUrisObj.Contains(FieldArtCrop) and
      (ImageUrisObj.Types[FieldArtCrop] = jdtString) then
      ImageUris.art_crop := ImageUrisObj.S[FieldArtCrop];
  end
  else
    ImageUris := Default (TImageUris); // Default values if not found or invalid
end;

procedure ParseLegalities(const JsonObj: TJsonObject;
  out Legalities: TCardLegalities);
var
  LegalitiesObj: TJsonObject;

  function GetSafeStringField(const Obj: TJsonObject;
    const FieldName: string): string;
  begin
    if Obj.Contains(FieldName) and (Obj.Types[FieldName] = jdtString) then
      Result := Obj.S[FieldName]
    else
      Result := ''; // Return an empty string if the field is missing or invalid
  end;

begin
  if JsonObj.Contains(FieldLegalities) and (JsonObj.Types[FieldLegalities] = jdtObject)
  then
  begin
    LegalitiesObj := JsonObj.O[FieldLegalities];

    // Safely extract all legalities
    Legalities.Standard := GetSafeStringField(LegalitiesObj, FieldStandard);
    Legalities.Pioneer := GetSafeStringField(LegalitiesObj, FieldPioneer);
    Legalities.Modern := GetSafeStringField(LegalitiesObj, FieldModern);
    Legalities.Legacy := GetSafeStringField(LegalitiesObj, FieldLegacy);
    Legalities.Commander := GetSafeStringField(LegalitiesObj, FieldCommander);
    Legalities.Vintage := GetSafeStringField(LegalitiesObj, FieldVintage);
    Legalities.Pauper := GetSafeStringField(LegalitiesObj, FieldPauper);
    Legalities.Historic := GetSafeStringField(LegalitiesObj, FieldHistoric);
    Legalities.Explorer := GetSafeStringField(LegalitiesObj, FieldExplorer);
    Legalities.Alchemy := GetSafeStringField(LegalitiesObj, FieldAlchemy);
    Legalities.Brawl := GetSafeStringField(LegalitiesObj, FieldBrawl);
    Legalities.Future := GetSafeStringField(LegalitiesObj, FieldFuture);
    Legalities.Oldschool := GetSafeStringField(LegalitiesObj, FieldOldschool);
    Legalities.Premodern := GetSafeStringField(LegalitiesObj, FieldPremodern);
    Legalities.Duel := GetSafeStringField(LegalitiesObj, FieldDuel);
    Legalities.Penny := GetSafeStringField(LegalitiesObj, FieldPenny);
  end
  else
  begin
    // Default to empty legalities if the legalities object is missing or invalid
    Legalities := Default (TCardLegalities);
  end;
end;

procedure ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
var
  PricesObj: TJsonObject;
begin
  if JsonObj.Contains(FieldPrices) and (JsonObj.Types[FieldPrices] = jdtObject) then
  begin
    PricesObj := JsonObj.O[FieldPrices];

    if PricesObj.Contains(FieldUsd) and (PricesObj.Types[FieldUsd] = jdtString) then
      Prices.USD := PricesObj.S[FieldUsd];
    if PricesObj.Contains(FieldUsdFoil) and
      (PricesObj.Types[FieldUsdFoil] = jdtString) then
      Prices.USD_Foil := PricesObj.S[FieldUsdFoil];
    if PricesObj.Contains(FieldEur) and (PricesObj.Types[FieldEur] = jdtString) then
      Prices.EUR := PricesObj.S[FieldEur];
    if PricesObj.Contains(FieldTix) and (PricesObj.Types[FieldTix] = jdtString) then
      Prices.Tix := PricesObj.S[FieldTix];
  end
  else
    Prices := Default (TCardPrices);
end;

procedure ParseCardFaces(const JsonObj: TJsonObject;
  out CardFaces: TArray<TCardFace>);
var
  FacesArray: TJsonArray;
  I: Integer;
begin
  if JsonObj.Contains(FieldCardFaces) and (JsonObj.Types[FieldCardFaces] = jdtArray)
  then
  begin
    FacesArray := JsonObj.A[FieldCardFaces];
    SetLength(CardFaces, FacesArray.Count);

    for I := 0 to FacesArray.Count - 1 do
    begin
      if FacesArray.Types[I] = jdtObject then
      begin
        var
        FaceObj := FacesArray.O[I];

        if FaceObj.Contains(FieldName) and (FaceObj.Types[FieldName] = jdtString) then
          CardFaces[I].Name := FaceObj.S[FieldName];

        if FaceObj.Contains(FieldManaCost) and
          (FaceObj.Types[FieldManaCost] = jdtString) then
          CardFaces[I].ManaCost := FaceObj.S[FieldManaCost];

        if FaceObj.Contains(FieldTypeLine) and
          (FaceObj.Types[FieldTypeLine] = jdtString) then
          CardFaces[I].TypeLine := FaceObj.S[FieldTypeLine];

        if FaceObj.Contains(FieldOracleText) and
          (FaceObj.Types[FieldOracleText] = jdtString) then
          CardFaces[I].OracleText := FaceObj.S[FieldOracleText];

        if FaceObj.Contains(FieldPower) and (FaceObj.Types[FieldPower] = jdtString)
        then
          CardFaces[I].Power := FaceObj.S[FieldPower];

        if FaceObj.Contains(FieldToughness) and
          (FaceObj.Types[FieldToughness] = jdtString) then
          CardFaces[I].Toughness := FaceObj.S[FieldToughness];

         if FaceObj.Contains(FieldFlavorText) and
          (FaceObj.Types[FieldFlavorText] = jdtString) then
          CardFaces[I].FlavorText:= FaceObj.S[FieldFlavorText];


         if FaceObj.Contains(FieldCardFaceLoyalty) and
          (FaceObj.Types[FieldCardFaceLoyalty] = jdtString) then
          CardFaces[I].Loyalty:= FaceObj.S[FieldCardFaceLoyalty];





        // Parse nested ImageUris
        ParseImageUris(FaceObj, CardFaces[I].ImageUris);
      end;
    end;
  end
  else
    SetLength(CardFaces, 0); // No card faces



end;

procedure FillSetDetailsFromJson(const JsonObj: TJsonObject;
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
procedure FillCardDetailsFromJson(const JsonObj: TJsonObject;
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

end.
