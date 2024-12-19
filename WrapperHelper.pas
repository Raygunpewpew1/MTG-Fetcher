unit WrapperHelper;

interface

uses
  System.SysUtils, System.NetEncoding, System.Classes, System.IOUtils,
  JsonDataObjects, SGlobalsZ;

function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;

procedure LogStuff(const Msg: string);

procedure ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
procedure ParseLegalities(const JsonObj: TJsonObject; out Legalities: TCardLegalities);
procedure ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
procedure ParseCardFaces(const JsonObj: TJsonObject; out CardFaces: TArray<TCardFace>);
procedure FillSetDetailsFromJson(const JsonObj: TJsonObject; out SetDetails: TSetDetails);
procedure FillCardDetailsFromJson(const JsonObj: TJsonObject; out CardDetails: TCardDetails);

implementation

uses
  APIConstants;

function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;
var
  BaseUrl: string;
begin
  if Fuzzy then
    Exit(Format('%s?fuzzy=%s', [EndpointNamed, TNetEncoding.URL.Encode(Query)]));

  BaseUrl := Format('%s?q=%s', [EndpointSearch, TNetEncoding.URL.Encode(Query.ToLower)]);

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

procedure LogStuff(const Msg: string);
var
  LogFilePath: string;
  LogFile: TStreamWriter;
begin
  try
    {$IF DEFINED(ANDROID)}
    LogFilePath := TPath.Combine(TPath.GetSharedDownloadsPath, 'application_log.txt');
    {$ELSEIF DEFINED(MSWINDOWS)}
    LogFilePath := TPath.Combine(TPath.GetAppPath, LogFileName);
    {$ENDIF}

    // Consider adding synchronization if needed
    LogFile := TStreamWriter.Create(LogFilePath, True, TEncoding.UTF8);
    try
      LogFile.WriteLine(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), Msg]));
    finally
      LogFile.Free;
    end;
  except
    // If logging fails, don't crash the app
  end;
end;

function GetSafeStringField(const Obj: TJsonObject; const FieldName: string): string;
begin
  if Obj.Contains(FieldName) and (Obj.Types[FieldName] = jdtString) then
    Result := Obj.S[FieldName]
  else
    Result := '';
end;

procedure ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
var
  ImageUrisObj: TJsonObject;
begin
  if JsonObj.Contains(FieldImageUris) and (JsonObj.Types[FieldImageUris] = jdtObject) then
  begin
    ImageUrisObj := JsonObj.O[FieldImageUris];
    ImageUris.Small := GetSafeStringField(ImageUrisObj, FieldSmall);
    ImageUris.Normal := GetSafeStringField(ImageUrisObj, FieldNormal);
    ImageUris.Large := GetSafeStringField(ImageUrisObj, FieldLarge);
    ImageUris.PNG := GetSafeStringField(ImageUrisObj, FieldPng);
    ImageUris.Border_crop := GetSafeStringField(ImageUrisObj, FieldBorderCrop);
    ImageUris.Art_crop := GetSafeStringField(ImageUrisObj, FieldArtCrop);
  end
  else
    ImageUris := Default(TImageUris);
end;

procedure ParseLegalities(const JsonObj: TJsonObject; out Legalities: TCardLegalities);
var
  LegalitiesObj: TJsonObject;
begin
  if JsonObj.Contains(FieldLegalities) and (JsonObj.Types[FieldLegalities] = jdtObject) then
  begin
    LegalitiesObj := JsonObj.O[FieldLegalities];

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
    Legalities := Default(TCardLegalities);
end;

procedure ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
var
  PricesObj: TJsonObject;
begin
  if JsonObj.Contains(FieldPrices) and (JsonObj.Types[FieldPrices] = jdtObject) then
  begin
    PricesObj := JsonObj.O[FieldPrices];
    Prices.USD := GetSafeStringField(PricesObj, FieldUsd);
    Prices.USD_Foil := GetSafeStringField(PricesObj, FieldUsdFoil);
    Prices.EUR := GetSafeStringField(PricesObj, FieldEur);
    Prices.Tix := GetSafeStringField(PricesObj, FieldTix);
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
  if JsonObj.Contains(FieldCardFaces) and (JsonObj.Types[FieldCardFaces] = jdtArray) then
  begin
    FacesArray := JsonObj.A[FieldCardFaces];
    SetLength(CardFaces, FacesArray.Count);

    for I := 0 to FacesArray.Count - 1 do
      if FacesArray.Types[I] = jdtObject then
      begin
        FaceObj := FacesArray.O[I];

        CardFaces[I].Name := GetSafeStringField(FaceObj, FieldName);
        CardFaces[I].ManaCost := GetSafeStringField(FaceObj, FieldManaCost);
        CardFaces[I].TypeLine := GetSafeStringField(FaceObj, FieldTypeLine);

        {$IF DEFINED(MSWINDOWS)}
        CardFaces[I].OracleText := TEncoding.UTF8.GetString(
                                    TEncoding.ANSI.GetBytes(
                                      GetSafeStringField(FaceObj, FieldOracleText)));
        CardFaces[I].FlavorText := TEncoding.UTF8.GetString(
                                    TEncoding.ANSI.GetBytes(
                                      GetSafeStringField(FaceObj, FieldFlavorText)));
        {$ELSE}
        CardFaces[I].OracleText := GetSafeStringField(FaceObj, FieldOracleText);
        CardFaces[I].FlavorText := GetSafeStringField(FaceObj, FieldFlavorText);
        {$ENDIF}

        CardFaces[I].Power := GetSafeStringField(FaceObj, FieldPower);
        CardFaces[I].Toughness := GetSafeStringField(FaceObj, FieldToughness);
        CardFaces[I].Loyalty := GetSafeStringField(FaceObj, FieldCardFaceLoyalty);

        ParseImageUris(FaceObj, CardFaces[I].ImageUris);
      end;
  end
  else
    SetLength(CardFaces, 0);
end;

procedure FillSetDetailsFromJson(const JsonObj: TJsonObject; out SetDetails: TSetDetails);
begin
  SetDetails.Clear;

  SetDetails.SFID := GetSafeStringField(JsonObj, FieldID);
  SetDetails.Name := GetSafeStringField(JsonObj, FieldName);
  SetDetails.Code := GetSafeStringField(JsonObj, FieldCode);
  SetDetails.ReleaseDate := GetSafeStringField(JsonObj, FieldReleasedAt);
  SetDetails.SetType := GetSafeStringField(JsonObj, FieldSetType);
  SetDetails.Block := GetSafeStringField(JsonObj, FieldBlock);
  SetDetails.BlockCode := GetSafeStringField(JsonObj, FieldBlockCode);
  SetDetails.ParentSetCode := GetSafeStringField(JsonObj, FieldParentSetCode);
  SetDetails.CardCount := JsonObj.I[FieldCardCount];
  SetDetails.Digital := JsonObj.B[FieldDigital];
  SetDetails.FoilOnly := JsonObj.B[FieldFoilOnly];

  SetDetails.IconSVGURI :=
    TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(GetSafeStringField(JsonObj, FieldIconSvgUri)));

  SetDetails.ScryfallURI := GetSafeStringField(JsonObj, FieldScryfallUri);
  SetDetails.URI := GetSafeStringField(JsonObj, FieldUri);
  SetDetails.SearchURI := GetSafeStringField(JsonObj, FieldSearchUri);
end;

procedure FillCardDetailsFromJson(const JsonObj: TJsonObject; out CardDetails: TCardDetails);
begin
  if (not CardDetails.SFID.IsEmpty) or (not CardDetails.OracleID.IsEmpty) then
    CardDetails.Clear;

  try
    // TypeLine
    if JsonObj.Contains(FieldTypeLine) and (JsonObj.Types[FieldTypeLine] = jdtString) then
    begin
      {$IF DEFINED(MSWINDOWS)}
      CardDetails.TypeLine := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(JsonObj.S[FieldTypeLine]));
      {$ELSE}
      CardDetails.TypeLine := JsonObj.S[FieldTypeLine];
      {$ENDIF}
    end;

    // Simple string fields
    CardDetails.SFID := GetSafeStringField(JsonObj, FieldID);

    {$IF DEFINED(MSWINDOWS)}
    if JsonObj.Contains(FieldName) and (JsonObj.Types[FieldName] = jdtString) then
      CardDetails.CardName := TEncoding.UTF8.GetString(
                                TEncoding.ANSI.GetBytes(JsonObj.S[FieldName]));
    {$ELSE}
    CardDetails.CardName := GetSafeStringField(JsonObj, FieldName);
    {$ENDIF}

    CardDetails.ManaCost := GetSafeStringField(JsonObj, FieldManaCost);

    {$IF DEFINED(MSWINDOWS)}
    if JsonObj.Contains(FieldOracleText) then
      CardDetails.OracleText := TEncoding.UTF8.GetString(
                                  TEncoding.ANSI.GetBytes(GetSafeStringField(JsonObj, FieldOracleText)));
    {$ELSE}
    CardDetails.OracleText := GetSafeStringField(JsonObj, FieldOracleText);
    {$ENDIF}

    // Games array
    if JsonObj.Contains(FeildGames) and (JsonObj.Types[FeildGames] = jdtArray) then
    begin
      var GamesArray := JsonObj.A[FeildGames];
      SetLength(CardDetails.Games, GamesArray.Count);
      for var I := 0 to GamesArray.Count - 1 do
        if GamesArray.Types[I] = jdtString then
          CardDetails.Games[I] := GamesArray.S[I];
    end
    else
      SetLength(CardDetails.Games, 0);

    // Keywords array
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

    CardDetails.SetCode := GetSafeStringField(JsonObj, FieldSet);
    CardDetails.SetName := GetSafeStringField(JsonObj, FieldSetName);
    CardDetails.Rarity := GetSafeStringField(JsonObj, FieldRarity);
    CardDetails.Power := GetSafeStringField(JsonObj, FieldPower);
    CardDetails.Toughness := GetSafeStringField(JsonObj, FieldToughness);
    CardDetails.Loyalty := GetSafeStringField(JsonObj, FieldLoyalty);
    CardDetails.PrintsSearchUri := GetSafeStringField(JsonObj, FieldPrintsSearchUri);
    CardDetails.OracleID := GetSafeStringField(JsonObj, FieldOracleID);

    {$IF DEFINED(MSWINDOWS)}
    if JsonObj.Contains(FieldFlavorText) then
      CardDetails.FlavorText := TEncoding.UTF8.GetString(
                                  TEncoding.ANSI.GetBytes(GetSafeStringField(JsonObj, FieldFlavorText)));
    {$ELSE}
    CardDetails.FlavorText := GetSafeStringField(JsonObj, FieldFlavorText);
    {$ENDIF}

    if JsonObj.Contains(FieldLayout) then
      CardDetails.Layout := GetSafeStringField(JsonObj, FieldLayout).ToLower;

    CardDetails.Lang := GetSafeStringField(JsonObj, FieldLang);
    CardDetails.ReleasedAt := GetSafeStringField(JsonObj, FieldReleasedAt);

    if JsonObj.Contains(FieldCMC) and (JsonObj.Types[FieldCMC] = jdtFloat) then
      CardDetails.CMC := JsonObj.F[FieldCMC];

    CardDetails.Reserved := JsonObj.B[FieldReserved];
    CardDetails.Foil := JsonObj.B[FieldFoil];
    CardDetails.NonFoil := JsonObj.B[FieldNonFoil];
    CardDetails.Oversized := JsonObj.B[FieldOversized];
    CardDetails.Promo := JsonObj.B[FieldPromo];
    CardDetails.Reprint := JsonObj.B[FieldReprint];
    CardDetails.Digital := JsonObj.B[FieldDigital];
    CardDetails.Artist := GetSafeStringField(JsonObj, FieldArtist);
    CardDetails.CollectorNumber := GetSafeStringField(JsonObj, FieldCollectorNumber);
    CardDetails.BorderColor := GetSafeStringField(JsonObj, FieldBorderColor);
    CardDetails.Frame := GetSafeStringField(JsonObj, FieldFrame);
    CardDetails.SecurityStamp := GetSafeStringField(JsonObj, FieldSecurityStamp);
    CardDetails.FullArt := JsonObj.B[FieldFullArt];
    CardDetails.Textless := JsonObj.B[FieldTextless];
    CardDetails.StorySpotlight := JsonObj.B[FieldStorySpotlight];

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
