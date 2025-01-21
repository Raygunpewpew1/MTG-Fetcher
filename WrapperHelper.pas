unit WrapperHelper;

interface

uses
  System.SysUtils, System.NetEncoding, System.Classes,
  JsonDataObjects, SGlobalsZ, Logger;

function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;
procedure ParseAllParts(const JsonObj: TJsonObject;
  out AllParts: TArray<TCardPart>);
procedure ParseRelatedURIs(const JsonObj: TJsonObject;
  out RelatedURIs: TRelatedURIs);
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
function GetUtf8String(const S: string): string;

implementation

uses
  APIConstants, CardDisplayHelpers, System.Character;

function ConstructSearchUrl(const Query, SetCode, Rarity, Colors: string;
  Fuzzy, Unique: Boolean; Page: Integer): string;
const
  SearchParams: array [0 .. 3] of string = ('SetCode', 'Rarity', 'Colors',
    'Unique');
var
  Params: TStringBuilder;
begin
  if Fuzzy then
    Exit(Format(FuzzySStr, [EndpointNamed, TNetEncoding.URL.Encode(Query)]));

  Params := TStringBuilder.Create;
  try
    Params.Append(Format(StandardSStr, [EndpointSearch,
      TNetEncoding.URL.Encode(Query.ToLower)]));

    if not SetCode.IsEmpty then
      Params.Append(BySetCode + TNetEncoding.URL.Encode(SetCode.ToLower));
    if not Rarity.IsEmpty then
      Params.Append(ByRarity + TNetEncoding.URL.Encode(Rarity.ToLower));
    if not Colors.IsEmpty then
      Params.Append(ByColor + TNetEncoding.URL.Encode(Colors.ToLower));
    if Unique then
      Params.Append(ShowUQ);
    Result := Params.ToString + Format(SPageStr, [Page]);
  finally
    Params.Free;
  end;

  LogStuff(Result);

end;

function GetSafeStringField(const Obj: TJsonObject; const FieldName: string;
  const Default: string = ''): string;
begin
  if Obj.Contains(FieldName) and (Obj.Types[FieldName] = jdtString) then
    Exit(Obj.S[FieldName])
  else
    Exit(Default);
end;

procedure ParseAllParts(const JsonObj: TJsonObject;
  out AllParts: TArray<TCardPart>);
var
  PartsArray: TJsonArray;
  I: Integer;
  PartObj: TJsonObject;
begin
  if JsonObj.Contains(FieldAllParts) and
    (JsonObj.Types[FieldAllParts] = jdtArray) then
  begin
    PartsArray := JsonObj.A[FieldAllParts];
    // LogStuff('all_parts found. Count: ' + IntToStr(PartsArray.Count));
    SetLength(AllParts, PartsArray.Count);

    for I := 0 to PartsArray.Count - 1 do
    begin
      if PartsArray.Types[I] = jdtObject then
      begin
        PartObj := PartsArray.O[I];
        AllParts[I].ObjectType := GetSafeStringField(PartObj, FieldObject);
        AllParts[I].ID := GetSafeStringField(PartObj, FieldID);
        AllParts[I].Component := GetSafeStringField(PartObj, FieldComponent);
        AllParts[I].Name := GetSafeStringField(PartObj, FieldName);
        AllParts[I].TypeLine := GetSafeStringField(PartObj, FieldTypeLine);
        AllParts[I].URI := GetSafeStringField(PartObj, FieldUri);
      end;
    end;
  end
  else
  begin
    SetLength(AllParts, 0);
    // LogStuff('all_parts not found or is not an array.');
  end;
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

procedure ParseRelatedURIs(const JsonObj: TJsonObject;
  out RelatedURIs: TRelatedURIs);
var
  RelatedURIsObj: TJsonObject;
begin
  if JsonObj.Contains(FieldRelatedUris) and
    (JsonObj.Types[FieldRelatedUris] = jdtObject) then
  begin
    RelatedURIsObj := JsonObj.O[FieldRelatedUris];
    RelatedURIs.Gatherer := GetSafeStringField(RelatedURIsObj, FieldGatherer);
    RelatedURIs.Tcgplayerinfinitearticles := GetSafeStringField(RelatedURIsObj,
      FieldTcgplayerInfiniteArticles);
    RelatedURIs.Tcgplayerinfinitedecks := GetSafeStringField(RelatedURIsObj,
      FieldTcgplayerInfiniteDecks);
    RelatedURIs.Edhrec := GetSafeStringField(RelatedURIsObj, FieldEdhrec);
  end
  else
    RelatedURIs := Default (TRelatedURIs);
end;

procedure ParsePurchaseURIs(const JsonObj: TJsonObject;
  out PurchaseURIs: TPurchaseURIs);
var
  PurchaseURIsObj: TJsonObject;
begin
  if JsonObj.Contains(FieldPurchaseUris) and
    (JsonObj.Types[FieldPurchaseUris] = jdtObject) then
  begin
    PurchaseURIsObj := JsonObj.O[FieldPurchaseUris];
    PurchaseURIs.Tcgplayer := GetSafeStringField(PurchaseURIsObj,
      FieldTcgplayer);
    PurchaseURIs.Cardmarket := GetSafeStringField(PurchaseURIsObj,
      FieldCardmarket);
    PurchaseURIs.Cardhoarder := GetSafeStringField(PurchaseURIsObj,
      FieldCardhoarder);

  end
  else
    PurchaseURIs := Default (TPurchaseURIs);
end;

procedure ParseImageUris(const JsonObj: TJsonObject; out ImageUris: TImageUris);
var
  ImageUrisObj: TJsonObject;
begin
  if JsonObj.Contains(FieldImageUris) and
    (JsonObj.Types[FieldImageUris] = jdtObject) then
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
    ImageUris := Default (TImageUris);
end;

procedure ParseLegalities(const JsonObj: TJsonObject;
  out Legalities: TCardLegalities);
var
  LegalitiesObj: TJsonObject;
  Format: TLegalityFormat;
begin
  if JsonObj.Contains(FieldLegalities) and
    (JsonObj.Types[FieldLegalities] = jdtObject) then
  begin
    LegalitiesObj := JsonObj.O[FieldLegalities];
    for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
      Legalities.SetStatus(Format, GetSafeStringField(LegalitiesObj,
        LegalityToString[Format]));
  end
  else
    Legalities.Clear;
end;

procedure ParsePrices(const JsonObj: TJsonObject; out Prices: TCardPrices);
var
  PricesObj: TJsonObject;
begin
  Prices.Clear;

  if JsonObj.Contains(FieldPrices) and (JsonObj.Types[FieldPrices] = jdtObject)
  then
  begin
    PricesObj := JsonObj.O[FieldPrices];

    Prices.USD := StrToCurrDef(GetSafeStringField(PricesObj, FieldUsd), 0);
    Prices.USD_Foil := StrToCurrDef(GetSafeStringField(PricesObj,
      FieldUsdFoil), 0);
    Prices.EUR := StrToCurrDef(GetSafeStringField(PricesObj, FieldEur), 0);
    Prices.Tix := StrToCurrDef(GetSafeStringField(PricesObj, FieldTix), 0);
  end;
end;

procedure ParseCardFaces(const JsonObj: TJsonObject;
  out CardFaces: TArray<TCardFace>);
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
        CardFaces[I].Name := GetSafeStringField(FaceObj, FieldName);
        CardFaces[I].ManaCost := GetSafeStringField(FaceObj, FieldManaCost);
        CardFaces[I].TypeLine := GetSafeStringField(FaceObj, FieldTypeLine);

        CardFaces[I].OracleText :=
          GetUtf8String(GetSafeStringField(FaceObj, FieldOracleText));
        CardFaces[I].FlavorText :=
          GetUtf8String(GetSafeStringField(FaceObj, FieldFlavorText));

        CardFaces[I].Power := GetSafeStringField(FaceObj, FieldPower);
        CardFaces[I].Toughness := GetSafeStringField(FaceObj, FieldToughness);
        CardFaces[I].Loyalty := GetSafeStringField(FaceObj,
          FieldCardFaceLoyalty);
        CardFaces[I].CMC := JsonObj.F[FieldCMC];
        ParseImageUris(FaceObj, CardFaces[I].ImageUris);
      end;
    end;
  end
  else
    SetLength(CardFaces, 0);
end;

procedure FillSetDetailsFromJson(const JsonObj: TJsonObject;
  out SetDetails: TSetDetails);
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

  SetDetails.IconSVGURI := GetUtf8String(GetSafeStringField(JsonObj,
    FieldIconSvgUri));

  SetDetails.ScryfallURI := GetSafeStringField(JsonObj, FieldScryfallUri);
  SetDetails.URI := GetSafeStringField(JsonObj, FieldUri);
  SetDetails.SearchURI := GetSafeStringField(JsonObj, FieldSearchUri);
end;

procedure FillCardDetailsFromJson(const JsonObj: TJsonObject;
  out CardDetails: TCardDetails);
var
  AllParts: TArray<TCardPart>;
  Part: TCardPart;

begin
  // If we already have IDs, clear them out before refilling
  if (not CardDetails.SFID.IsEmpty) or (not CardDetails.OracleID.IsEmpty) then
    CardDetails.Clear;

  try
    // TypeLine
    if JsonObj.Contains(FieldTypeLine) and
      (JsonObj.Types[FieldTypeLine] = jdtString) then
      CardDetails.TypeLine := GetUtf8String(JsonObj.S[FieldTypeLine]);

    CardDetails.SFID := GetSafeStringField(JsonObj, FieldID);
    CardDetails.ArenaID := JsonObj.I[FieldArena];
    CardDetails.EDHRank := JsonObj.I[FieldEDHRank];

    CardDetails.CardName := GetUtf8String(GetSafeStringField(JsonObj,
      FieldName));
    CardDetails.ManaCost := GetSafeStringField(JsonObj, FieldManaCost);

    CardDetails.OracleText := GetUtf8String(GetSafeStringField(JsonObj,
      FieldOracleText));

    // Games array
    GetSafeStringArrayField(JsonObj, FieldGames, CardDetails.Games);

    // Keywords array
    GetSafeStringArrayField(JsonObj, FieldKeywords, CardDetails.Keywords);

    CardDetails.SetCode := GetSafeStringField(JsonObj, FieldSet);
    CardDetails.SetName := GetSafeStringField(JsonObj, FieldSetName);
    CardDetails.Rarity := StringToRarity(JsonObj.S[FieldRarity]);
    CardDetails.Power := GetSafeStringField(JsonObj, FieldPower);
    CardDetails.Toughness := GetSafeStringField(JsonObj, FieldToughness);
    CardDetails.Loyalty := GetSafeStringField(JsonObj, FieldLoyalty);
    CardDetails.PrintsSearchUri := GetSafeStringField(JsonObj,
      FieldPrintsSearchUri);
    CardDetails.OracleID := GetSafeStringField(JsonObj, FieldOracleID);
    CardDetails.FlavorText := GetUtf8String(GetSafeStringField(JsonObj,
      FieldFlavorText));

    CardDetails.Layout := GetSafeStringField(JsonObj, FieldLayout).ToLower;
    CardDetails.Lang := GetSafeStringField(JsonObj, FieldLang);
    CardDetails.ReleasedAt := GetSafeStringField(JsonObj, FieldReleasedAt);

    if (JsonObj.Contains(FieldCMC)) and (JsonObj.Types[FieldCMC] = jdtFloat)
    then
      CardDetails.CMC := JsonObj.F[FieldCMC];

    CardDetails.Reserved := JsonObj.B[FieldReserved];
    CardDetails.Foil := JsonObj.B[FieldFoil];
    CardDetails.NonFoil := JsonObj.B[FieldNonFoil];
    CardDetails.Oversized := JsonObj.B[FieldOversized];
    CardDetails.Promo := JsonObj.B[FieldPromo];
    CardDetails.Reprint := JsonObj.B[FieldReprint];
    CardDetails.Digital := JsonObj.B[FieldDigital];
    CardDetails.Artist := GetSafeStringField(JsonObj, FieldArtist);
    CardDetails.CollectorNumber := GetSafeStringField(JsonObj,
      FieldCollectorNumber);
    CardDetails.BorderColor := GetSafeStringField(JsonObj, FieldBorderColor);
    CardDetails.Frame := GetSafeStringField(JsonObj, FieldFrame);
    CardDetails.SecurityStamp := GetSafeStringField(JsonObj,
      FieldSecurityStamp);
    CardDetails.FullArt := JsonObj.B[FieldFullArt];
    CardDetails.Textless := JsonObj.B[FieldTextless];
    CardDetails.StorySpotlight := JsonObj.B[FieldStorySpotlight];

    GetSafeStringArrayField(JsonObj, FieldColorIdentity,
      CardDetails.ColorIdentity);

    // Nested objects
    ParseImageUris(JsonObj, CardDetails.ImageUris);
    ParseLegalities(JsonObj, CardDetails.Legalities);
    ParsePrices(JsonObj, CardDetails.Prices);
    ParseCardFaces(JsonObj, CardDetails.CardFaces);
    ParseRelatedURIs(JsonObj, CardDetails.RelatedURIs);
    ParsePurchaseURIs(JsonObj, CardDetails.PurchaseURIs);
    // ParseAllParts(JsonObj, CardDetails.AllParts);

    // Parse "all_parts" field
    ParseAllParts(JsonObj, AllParts);

    if Length(AllParts) > 0 then
    begin
      // LogStuff('Processing all_parts for meld detection...');
      for Part in AllParts do
      begin
        // LogStuff(Format('Part: Name=%s, Component=%s, URI=%s',
        // [Part.Name, Part.Component, Part.URI]));

        if Part.Component = 'meld_part' then
        begin
          CardDetails.IsMeld := True; // Set IsMeld to True
          CardDetails.MeldDetails.MeldParts :=
            CardDetails.MeldDetails.MeldParts + [Part];
          // LogStuff('Detected meld_part. Setting IsMeld to True.');
        end
        else if Part.Component = 'meld_result' then
        begin
          CardDetails.MeldDetails.MeldResult := Part;
          // LogStuff('Detected meld_result. Setting MeldResult.');
        end;
      end;
    end
    else
    begin
      // LogStuff('No all_parts found. Setting IsMeld to False.');
      CardDetails.IsMeld := False;
    end;

    // Log final state
    // if CardDetails.IsMeld then
    // LogStuff('Final IsMeld=True with MeldParts count: ' + IntToStr(Length(CardDetails.MeldDetails.MeldParts)))
    // else
    // LogStuff('Final IsMeld=False.');

  except
    on E: Exception do
    begin
      LogStuff(Format(ErrorFillingCardDetails, [E.Message]), ERROR);
      CardDetails.Clear;
    end;
  end;
end;

end.
