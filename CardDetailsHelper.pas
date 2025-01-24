// CardDetailsHelper.pas
unit CardDetailsHelper;

interface

uses
  JsonDataObjects, SGlobalsZ, WrapperHelper, APIConstants, System.SysUtils,
  CardDisplayHelpers;

type
  TCardDetailsHelper = record helper for TCardDetails
    procedure LoadFromJson(const JsonObj: TJsonObject);
  end;

implementation

procedure TCardDetailsHelper.LoadFromJson(const JsonObj: TJsonObject);
begin
  if not Assigned(JsonObj) then
    Exit;

  // Core Identifiers
  SFID := TWrapperHelper.GetSafeStringField(JsonObj, FieldID);
  OracleID := TWrapperHelper.GetSafeStringField(JsonObj, FieldOracleID);
  CardName := TWrapperHelper.GetUtf8String
    (TWrapperHelper.GetSafeStringField(JsonObj, FieldName));
  Lang := TWrapperHelper.GetSafeStringField(JsonObj, FieldLang);
  ReleasedAt := TWrapperHelper.GetSafeStringField(JsonObj, FieldReleasedAt);
  Layout := LowerCase(TWrapperHelper.GetSafeStringField(JsonObj, FieldLayout));
  ArenaID := JsonObj.I[FieldArena];
  EDHRank := JsonObj.I[FieldEDHRank];

  // Visuals and Presentation
  TypeLine := TWrapperHelper.GetUtf8String
    (TWrapperHelper.GetSafeStringField(JsonObj, FieldTypeLine));
  TWrapperHelper.GetSafeStringArrayField(JsonObj, FieldColorIdentity,
    ColorIdentity);
  ManaCost := TWrapperHelper.GetSafeStringField(JsonObj, FieldManaCost);
  OracleText := TWrapperHelper.GetUtf8String
    (TWrapperHelper.GetSafeStringField(JsonObj, FieldOracleText));
  FlavorText := TWrapperHelper.GetUtf8String
    (TWrapperHelper.GetSafeStringField(JsonObj, FieldFlavorText));
  Power := TWrapperHelper.GetSafeStringField(JsonObj, FieldPower);
  Toughness := TWrapperHelper.GetSafeStringField(JsonObj, FieldToughness);
  Loyalty := TWrapperHelper.GetSafeStringField(JsonObj, FieldLoyalty);
  // SetIconURI := TWrapperHelper.GetSafeStringField(JsonObj, FieldSetIconURI);
  Artist := TWrapperHelper.GetSafeStringField(JsonObj, FieldArtist);
  CollectorNumber := TWrapperHelper.GetSafeStringField(JsonObj,
    FieldCollectorNumber);
  BorderColor := TWrapperHelper.GetSafeStringField(JsonObj, FieldBorderColor);
  Frame := TWrapperHelper.GetSafeStringField(JsonObj, FieldFrame);
  SecurityStamp := TWrapperHelper.GetSafeStringField(JsonObj,
    FieldSecurityStamp);
  TWrapperHelper.GetSafeStringArrayField(JsonObj, FieldKeywords, Keywords);
  TWrapperHelper.GetSafeStringArrayField(JsonObj, FieldGames, Games);
  AllParts := nil; // Initialize as needed
  IsMeld := False; // Initialize as needed

  // Legalities and Rules
  TWrapperHelper.ParseLegalities(JsonObj, Legalities);
  PrintsSearchUri := TWrapperHelper.GetSafeStringField(JsonObj,
    FieldPrintsSearchURI);
  RulingsUri := TWrapperHelper.GetSafeStringField(JsonObj, FieldRulingsUri);

  // Pricing Information
  TWrapperHelper.ParsePrices(JsonObj, Prices);

  // Images and Data
  TWrapperHelper.ParseImageUris(JsonObj, ImageUris);
  ImageData := nil; // Initialize as needed

  // Set and Rarity Information
  SetCode := TWrapperHelper.GetSafeStringField(JsonObj, FieldSet);
  SetName := TWrapperHelper.GetSafeStringField(JsonObj, FieldSetName);
  Rarity := StringToRarity(TWrapperHelper.GetSafeStringField(JsonObj,
    FieldRarity));

  // Additional Attributes
  if JsonObj.Contains(FieldCMC) and (JsonObj.Types[FieldCMC] = jdtFloat) then
    CMC := JsonObj.F[FieldCMC]
  else
    CMC := 0.0;

  Reserved := JsonObj.B[FieldReserved];
  Foil := JsonObj.B[FieldFoil];
  NonFoil := JsonObj.B[FieldNonFoil];
  Oversized := JsonObj.B[FieldOversized];
  Promo := JsonObj.B[FieldPromo];
  Reprint := JsonObj.B[FieldReprint];
  Digital := JsonObj.B[FieldDigital];
  FullArt := JsonObj.B[FieldFullArt];
  Textless := JsonObj.B[FieldTextless];
  StorySpotlight := JsonObj.B[FieldStorySpotlight];

  // Card Faces
  TWrapperHelper.ParseCardFaces(JsonObj, CardFaces);

  // External Links
  ScryfallURI := TWrapperHelper.GetSafeStringField(JsonObj, FieldScryfallURI);
  URI := TWrapperHelper.GetSafeStringField(JsonObj, FieldURI);
  TWrapperHelper.ParseRelatedURIs(JsonObj, RelatedURIs);
  TWrapperHelper.ParsePurchaseURIs(JsonObj, PurchaseURIs);
  // ScryfallCardBackID := TWrapperHelper.GetSafeStringField(JsonObj, FieldScryfallCardBackID);
  // ScryfallID := TWrapperHelper.GetSafeStringField(JsonObj, FieldScryfallID);
  // ScryfallIllustrationID := TWrapperHelper.GetSafeStringField(JsonObj, FieldScryfallIllustrationID);
  // ScryfallOracleID := TWrapperHelper.GetSafeStringField(JsonObj, FieldScryfallOracleID);
end;

end.
