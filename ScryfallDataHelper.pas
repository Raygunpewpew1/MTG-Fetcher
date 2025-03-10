﻿unit ScryfallDataHelper;

interface

uses
  System.SysUtils, System.NetEncoding, System.Classes,
  JsonDataObjects, CardMainData, Logger, APIConstants, CardDisplayHelpers,
  System.Generics.Collections, CardMetaData, System.SyncObjs;

var
  SetDetailsLock: TCriticalSection;

type
  TWrapperHelper = class
  public
    // URL Construction
    class function ConstructSearchUrl(const Query, SetCode, Rarity,
      Colors: string; Fuzzy, Unique: Boolean; Page: Integer): string;

    // Parsing Methods
    class procedure ParseAllParts(const JsonObj: TJsonObject;
      out AllParts: TArray<TCardPart>);
    class procedure ParseRelatedURIs(const JsonObj: TJsonObject;
      out RelatedURIs: TRelatedURIs);
    class procedure ParseImageUris(const JsonObj: TJsonObject;
      out ImageUris: TImageUris);
    class procedure ParseLegalities(const JsonObj: TJsonObject;
      out Legalities: TCardLegalities);
    class procedure ParsePrices(const JsonObj: TJsonObject;
      out Prices: TCardPrices);
    class procedure ParseCardFaces(const JsonObj: TJsonObject;
      out CardFaces: TArray<TCardFace>);
    class procedure FillSetDetailsFromJson(const JsonObj: TJsonObject;
      out SetDetails: TSetDetails);
    class procedure FillCardDetailsFromJson(const JsonObj: TJsonObject;
      out CardDetails: TCardDetails);
    class procedure SFillSetDetailsFromJson(const JsonObj: TJsonObject;
      out SetDetails: TSetDetails);
    class procedure ParsePurchaseURIs(const JsonObj: TJsonObject;
      out PurchaseURIs: TPurchaseURIs);
    // Helper Methods
    class function GetSafeStringField(const Obj: TJsonObject;
      const FieldName: string; const Default: string = ''): string;
    // class procedure GetSafeStringArrayField(const Obj: TJsonObject;
    // const FieldName: string; out Arr: TArray<string>);

    class procedure GetSafeStringArrayField(const Obj: TJsonObject;
      const FieldName: string; var List: TList<string>);

    class function GetUtf8String(const S: string): string;
    class procedure ParseRulings(const JsonObj: TJsonObject;
      out Rulings: TArray<TRuling>); static;


    // Utility Methods

  private

  end;

implementation

{ TWrapperHelper }

class function TWrapperHelper.ConstructSearchUrl(const Query, SetCode, Rarity,
  Colors: string; Fuzzy, Unique: Boolean; Page: Integer): string;
var
  Params: TStringBuilder;
begin
  if Fuzzy then
  begin
    Result := Format(FuzzySStr, [EndpointNamed,
      TNetEncoding.URL.Encode(Query)]);
    LogStuff(Result);
    Exit;
  end;

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

class function TWrapperHelper.GetUtf8String(const S: string): string;
begin
{$IF DEFINED(MSWINDOWS)}
  if S <> '' then
    Result := TEncoding.UTF8.GetString(BytesOf(S))
  else
    Result := '';
{$ELSE}
  // On other platforms, assume UTF-8.
  if S <> '' then
    Result := S;
{$ENDIF}
end;

class function TWrapperHelper.GetSafeStringField(const Obj: TJsonObject;
  const FieldName: string; const Default: string): string;
begin
  if Obj.Contains(FieldName) and (Obj.Types[FieldName] = jdtString) then
    Result := Obj.S[FieldName]
  else
    Result := Default;
end;

class procedure TWrapperHelper.GetSafeStringArrayField(const Obj: TJsonObject;
  const FieldName: string; var List: TList<string>);
var
  JArr: TJsonArray;
  I: Integer;
begin
  // Clear the list first.
  if not Assigned(List) then
    List := TList<string>.Create
  else
    List.Clear;

  if Obj.Contains(FieldName) and (Obj.Types[FieldName] = jdtArray) then
  begin
    JArr := Obj.A[FieldName];
    for I := 0 to JArr.Count - 1 do
    begin
      if JArr.Types[I] = jdtString then
        List.Add(JArr.S[I])
      else
        LogStuff(Format('Non-string element at index %d in field "%s" skipped.',
          [I, FieldName]), WARNING);
    end;
  end
  else
  begin
    LogStuff(Format('Field "%s" is missing or not an array.',
      [FieldName]), DEBUG);
  end;
end;

class procedure TWrapperHelper.ParseAllParts(const JsonObj: TJsonObject;
  out AllParts: TArray<TCardPart>);
var
  PartsArray: TJsonArray;
  I: Integer;
  PartObj: TJsonObject;
begin

  for I := Low(AllParts) to High(AllParts) do
    FreeAndNil(AllParts[I]);

  if JsonObj.Contains(FieldAllParts) and
    (JsonObj.Types[FieldAllParts] = jdtArray) then
  begin
    PartsArray := JsonObj.A[FieldAllParts];
    Finalize(AllParts);
    SetLength(AllParts, PartsArray.Count);
    for I := 0 to PartsArray.Count - 1 do
    begin
      if PartsArray.Types[I] = jdtObject then
      begin
        PartObj := PartsArray.O[I];
        AllParts[I] := TCardPart.Create; // Allocate new instance
        AllParts[I].ObjectType := GetSafeStringField(PartObj, FieldObject);
        AllParts[I].ID := GetSafeStringField(PartObj, FieldID);
        AllParts[I].Component := GetSafeStringField(PartObj, FieldComponent);
        AllParts[I].Name := GetSafeStringField(PartObj, FieldName);
        AllParts[I].TypeLine :=
          GetUtf8String(GetSafeStringField(PartObj, FieldTypeLine));
        AllParts[I].URI := GetSafeStringField(PartObj, FieldUri);
      end;
    end;
  end
  else
    SetLength(AllParts, 0);
  for I := 0 to Length(AllParts) - 1 do
    AllParts[I].Free;

  SetLength(AllParts, 0);

end;

class procedure TWrapperHelper.ParseRelatedURIs(const JsonObj: TJsonObject;
  out RelatedURIs: TRelatedURIs);
var
  RelatedURIsObj: TJsonObject;
begin
  // Instead of using Default(TRelatedURIs) (which isn’t valid for classes),
  // we either clear the existing object or allocate a new one.
  if not Assigned(RelatedURIs) then
    RelatedURIs := TRelatedURIs.Create
  else
    RelatedURIs.Clear;

  if JsonObj.Contains(FieldRelatedUris) and
    (JsonObj.Types[FieldRelatedUris] = jdtObject) then
  begin
    RelatedURIsObj := JsonObj.O[FieldRelatedUris];
    RelatedURIs.Gatherer := GetSafeStringField(RelatedURIsObj, FieldGatherer);
    RelatedURIs.TcgplayerInfiniteArticles := GetSafeStringField(RelatedURIsObj,
      FieldTcgplayerInfiniteArticles);
    RelatedURIs.TcgplayerInfiniteDecks := GetSafeStringField(RelatedURIsObj,
      FieldTcgplayerInfiniteDecks);
    RelatedURIs.Edhrec := GetSafeStringField(RelatedURIsObj, FieldEdhrec);
  end;
end;

class procedure TWrapperHelper.ParsePurchaseURIs(const JsonObj: TJsonObject;
  out PurchaseURIs: TPurchaseURIs);
var
  PurchaseURIsObj: TJsonObject;
begin
  if not Assigned(PurchaseURIs) then
    PurchaseURIs := TPurchaseURIs.Create;
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
  end;
end;

class procedure TWrapperHelper.ParseImageUris(const JsonObj: TJsonObject;
  out ImageUris: TImageUris);
var
  ImageUrisObj: TJsonObject;
begin
  if not Assigned(ImageUris) then
    ImageUris := TImageUris.Create
  else
    ImageUris.Clear;

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
  end;
end;

class procedure TWrapperHelper.ParseLegalities(const JsonObj: TJsonObject;
  out Legalities: TCardLegalities);
var
  LegalitiesObj: TJsonObject;
  Format: TLegalityFormat;
begin
  // We assume Legalities is already allocated; if not, the caller should allocate it.
  Legalities.Clear;
  if JsonObj.Contains(FieldLegalities) and
    (JsonObj.Types[FieldLegalities] = jdtObject) then
  begin
    LegalitiesObj := JsonObj.O[FieldLegalities];
    for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
      Legalities.SetStatus(Format, GetSafeStringField(LegalitiesObj,
        Format.ToString));
  end;
end;

class procedure TWrapperHelper.ParsePrices(const JsonObj: TJsonObject;
  out Prices: TCardPrices);
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

class procedure TWrapperHelper.ParseCardFaces(const JsonObj: TJsonObject;
  out CardFaces: TArray<TCardFace>);
var
  FacesArray: TJsonArray;
  I: Integer;
  FaceObj: TJsonObject;
  TempImageUris: TImageUris;
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
        CardFaces[I] := TCardFace.Create;

        CardFaces[I].Name := GetSafeStringField(FaceObj, FieldName);
        CardFaces[I].ManaCost := GetSafeStringField(FaceObj, FieldManaCost);
        CardFaces[I].TypeLine :=
          GetUtf8String(GetSafeStringField(FaceObj, FieldTypeLine));
        CardFaces[I].OracleText :=
          GetUtf8String(GetSafeStringField(FaceObj, FieldOracleText));
        CardFaces[I].FlavorText :=
          GetUtf8String(GetSafeStringField(FaceObj, FieldFlavorText));
        CardFaces[I].Power := GetSafeStringField(FaceObj, FieldPower);
        CardFaces[I].Toughness := GetSafeStringField(FaceObj, FieldToughness);
        CardFaces[I].Loyalty := GetSafeStringField(FaceObj,
          FieldCardFaceLoyalty);
        CardFaces[I].CMC := FaceObj.F[FieldCMC];

        // Handle ImageUris correctly
        TempImageUris := nil;
        ParseImageUris(FaceObj, TempImageUris);
        if Assigned(CardFaces[I].ImageUris) then
          FreeAndNil(CardFaces[I].ImageUris); // Free any existing instance

        CardFaces[I].ImageUris := TempImageUris;
      end;
    end;
  end
  else
    SetLength(CardFaces, 0);
end;

class procedure TWrapperHelper.FillSetDetailsFromJson(const JsonObj
  : TJsonObject; out SetDetails: TSetDetails);
begin
  if not Assigned(SetDetails) then
  begin
    // LogStuff('Creating new TSetDetails instance inside FillSetDetailsFromJson', DEBUG);
    SetDetails := TSetDetails.Create;
  end;

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

class procedure TWrapperHelper.SFillSetDetailsFromJson(const JsonObj
  : TJsonObject; out SetDetails: TSetDetails);
begin
  if Assigned(SetDetails) then
    FreeAndNil(SetDetails);

  SetDetailsLock.Enter;
  try
    SetDetails := TSetDetails.Create;
    FillSetDetailsFromJson(JsonObj, SetDetails);
  finally
    SetDetailsLock.Leave;
  end;
end;

class procedure TWrapperHelper.FillCardDetailsFromJson(const JsonObj
  : TJsonObject; out CardDetails: TCardDetails);
var
  AllParts: TArray<TCardPart>;
  Part: TCardPart;
  TempGames, TempKeywords, TempColorID: TList<string>;
  I: Integer;
  TempImageUris: TImageUris;
  TempLegalities: TCardLegalities;
  TempPrices: TCardPrices;
  TempCardFaces: TArray<TCardFace>;
  TempRelatedURIs: TRelatedURIs;
  TempPurchaseURIs: TPurchaseURIs;
begin
  // If CardDetails already contains data, clear it
  if (not CardDetails.SFID.IsEmpty) or (not CardDetails.OracleID.IsEmpty) then
  begin
    CardDetails.Clear;
  end;

  try
    // Basic string fields and numbers
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

    // Games array (using a temporary variable)
    TempGames := CardDetails.Games;
    // local reference to the already allocated list
    TWrapperHelper.GetSafeStringArrayField(JsonObj, FieldGames, TempGames);
    CardDetails.Games := TempGames; // reassign if necessary

    // Keywords array (using a temporary variable)
    TempKeywords := CardDetails.Keywords;
    TWrapperHelper.GetSafeStringArrayField(JsonObj, FieldKeywords,
      TempKeywords);
    CardDetails.Keywords := TempKeywords;

    CardDetails.SetCode := GetSafeStringField(JsonObj, FieldSet);
    CardDetails.SetName := GetSafeStringField(JsonObj, FieldSetName);
    CardDetails.Rarity := TRarity.FromString(GetSafeStringField(JsonObj,
      FieldRarity));

    CardDetails.Power := GetSafeStringField(JsonObj, FieldPower);
    CardDetails.Toughness := GetSafeStringField(JsonObj, FieldToughness);
    CardDetails.Loyalty := GetSafeStringField(JsonObj, FieldLoyalty);
    CardDetails.PrintsSearchUri := GetSafeStringField(JsonObj,
      FieldPrintsSearchUri);
    CardDetails.OracleID := GetSafeStringField(JsonObj, FieldOracleID);
    CardDetails.FlavorText := GetUtf8String(GetSafeStringField(JsonObj,
      FieldFlavorText));

    CardDetails.Layout := LowerCase(GetSafeStringField(JsonObj, FieldLayout));
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

    // ColorIdentity array (using a temporary variable)
    TempColorID := CardDetails.ColorIdentity;
    GetSafeStringArrayField(JsonObj, FieldColorIdentity, TempColorID);
    CardDetails.ColorIdentity := TempColorID;

    // Nested objects
    // ImageUris
    if Assigned(CardDetails.ImageUris) then
      FreeAndNil(CardDetails.ImageUris);
    TempImageUris := nil;
    ParseImageUris(JsonObj, TempImageUris);
    CardDetails.ImageUris := TempImageUris;

    // Legalities
    if Assigned(CardDetails.Legalities) then
      FreeAndNil(CardDetails.Legalities);
    TempLegalities := TCardLegalities.Create;
    ParseLegalities(JsonObj, TempLegalities);
    CardDetails.Legalities := TempLegalities;

    // Prices
    if Assigned(CardDetails.Prices) then
      FreeAndNil(CardDetails.Prices);
    TempPrices := TCardPrices.Create;
    ParsePrices(JsonObj, TempPrices);
    CardDetails.Prices := TempPrices;

    // RelatedURIs
    if Assigned(CardDetails.RelatedURIs) then
      FreeAndNil(CardDetails.RelatedURIs);
    TempRelatedURIs := TRelatedURIs.Create;
    ParseRelatedURIs(JsonObj, TempRelatedURIs);
    CardDetails.RelatedURIs := TempRelatedURIs;

    // PurchaseURIs
    if Assigned(CardDetails.PurchaseURIs) then
      FreeAndNil(CardDetails.PurchaseURIs);
    TempPurchaseURIs := TPurchaseURIs.Create;
    ParsePurchaseURIs(JsonObj, TempPurchaseURIs);
    CardDetails.PurchaseURIs := TempPurchaseURIs;

    ParseCardFaces(JsonObj, TempCardFaces);
    CardDetails.CardFaces.Clear;
    for I := 0 to High(TempCardFaces) do
      CardDetails.CardFaces.Add(TempCardFaces[I]);

    // TWrapperHelper.ParseCardFaces(JsonObj, CardDetails.CardFaces);
    // TWrapperHelper.ParseRelatedURIs(JsonObj, CardDetails.RelatedURIs);
    // TWrapperHelper.ParsePurchaseURIs(JsonObj, CardDetails.PurchaseURIs);

    // Parse "all_parts" field into a temporary dynamic array
    AllParts := nil;
    if JsonObj.Contains(FieldAllParts) and
      (JsonObj.Types[FieldAllParts] = jdtArray) then
      ParseAllParts(JsonObj, AllParts);

    // Process "all_parts" to set up meld details
    if Length(AllParts) > 0 then

    begin
      CardDetails.MeldDetails.MeldParts.Clear;
      for I := 0 to Length(AllParts) - 1 do
      begin
        Part := AllParts[I];
        if Part.Component = 'meld_part' then
        begin
          CardDetails.IsMeld := True; // Mark as meld card
          // Append the Part to the existing MeldParts list:
          CardDetails.MeldDetails.MeldParts.Add(Part);
        end
        else if Part.Component = 'meld_result' then
        begin
          CardDetails.MeldDetails.MeldResult := Part;
        end;
      end;
    end
    else
      CardDetails.IsMeld := False;

  except
    on E: Exception do
    begin
      LogStuff(Format(ErrorFillingCardDetails, [E.Message]), ERROR);
      CardDetails.Clear;
    end;
  end;
end;

class procedure TWrapperHelper.ParseRulings(const JsonObj: TJsonObject;
  out Rulings: TArray<TRuling>);
var
  RulingsArray: TJsonArray;
  I: Integer;
begin
  if JsonObj.Contains(FieldData) then
  begin
    RulingsArray := JsonObj.A[FieldData];
    SetLength(Rulings, RulingsArray.Count);
    for I := 0 to RulingsArray.Count - 1 do
    begin
      Rulings[I] := TRuling.Create; // Allocate new instance
      Rulings[I].Source := RulingsArray.O[I].S[FieldSource];
      Rulings[I].PublishedAt := RulingsArray.O[I].S[FieldPublishedAt];
      Rulings[I].Comment := RulingsArray.O[I].S[FieldComment];
    end;
  end
  else
    Rulings := [];
end;

initialization

SetDetailsLock := TCriticalSection.Create;

finalization

SetDetailsLock.Free;

end.
