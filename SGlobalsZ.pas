unit SGlobalsZ;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.StrUtils;

type
  // Enumeration for Card Rarity
  TRarity = (
    rAll, rCommon, rUncommon, rRare, rMythic, rSpecial, rBonus,
    rTimeshifted, rMasterpiece, rToken, rDoubleFacedToken, rDraft,
    rPlaneshifted, rUnique, rBasic, rPromo
  );

  // Enumeration for Legality Formats
  TLegalityFormat = (
    lfStandard, lfFuture, lfHistoric, lfGladiator, lfPioneer,
    lfExplorer, lfModern, lfLegacy, lfPauper, lfVintage, lfPenny,
    lfCommander, lfAlchemy, lfBrawl, lfPauperCommander, lfDuel,
    lfOldschool, lfPremodern, lfOathbreaker
  );

  // Record for Card Prices in Different Currencies
  TCardPrices = record
    USD: Currency;
    USD_Foil: Currency;
    EUR: Currency;
    Tix: Currency;
    procedure Clear;
  end;

  // Record for Image URIs at Different Resolutions
  TImageUris = record
    Small: string;
    Normal: string;
    Large: string;
    BackFace: string;
    PNG: string;
    Border_crop: string;
    Art_crop: string;
    procedure Clear;
  end;

  // Record for Card Legalities Across Different Formats
  TCardLegalities = record
    Status: array [TLegalityFormat] of string;
    procedure Clear;
    // Retrieves the status for a specific format
    function GetStatus(Format: TLegalityFormat): string;
    // Sets the status for a specific format
    procedure SetStatus(Format: TLegalityFormat; const StatusStr: string);
  end;

  // Record for a Single Card Face (e.g., for Double-Faced Cards)
  TCardFace = record
    Name: string;
    FlavorText: string;
    ManaCost: string;
    TypeLine: string;
    OracleText: string;
    Power: string;
    Toughness: string;
    ImageUris: TImageUris;
    Loyalty: string;
    CMC: Double;
    procedure Clear;
  end;

  // Record for a Part of a Melded Card
  TCardPart = record
    ObjectType: string;
    ID: string;
    Component: string;
    Name: string;
    TypeLine: string;
    URI: string;
  end;

  // Record for Meld Details, including parts and result
  TMeldDetails = record
    MeldParts: TArray<TCardPart>;
    MeldResult: TCardPart;
    procedure Clear;
  end;

  // Record for Related URIs
  TRelatedURIs = record
    Gatherer: string;
    TcgplayerInfiniteArticles: string;
    TcgplayerInfiniteDecks: string;
    Edhrec: string;
    procedure Clear;
  end;

  // Record for Purchase URIs
  TPurchaseURIs = record
    Tcgplayer: string;
    Cardmarket: string;
    Cardhoarder: string;
    procedure Clear;
  end;

  // Main Record for Card Details
  TCardDetails = record
    // Core Identifiers
    SFID: string;
    OracleID: string;
    CardName: string;
    Lang: string;
    ReleasedAt: string;
    Layout: string;
    ArenaID: Integer;
    EDHRank: Integer;

    // Visuals and Presentation
    TypeLine: string;
    ColorIdentity: TArray<string>;
    ManaCost: string;
    OracleText: string;
    FlavorText: string;
    Power: string;
    Toughness: string;
    Loyalty: string;
    SetIconURI: string;
    Artist: string;
    CollectorNumber: string;
    BorderColor: string;
    Frame: string;
    SecurityStamp: string;
    Keywords: TArray<string>;
    AllParts: TArray<TCardPart>;
    IsMeld: Boolean;
    MeldDetails: TMeldDetails;

    // Legalities and Rules
    Legalities: TCardLegalities;
    PrintsSearchUri: string;
    RulingsUri: string;

    // Pricing Information
    Prices: TCardPrices;

    // Images and Data
    ImageUris: TImageUris;
    ImageData: TBytes;

    // Set and Rarity Information
    SetCode: string;
    SetName: string;
    Rarity: TRarity;

    // Additional Attributes
    CMC: Double;
    Reserved: Boolean;
    Foil: Boolean;
    NonFoil: Boolean;
    Oversized: Boolean;
    Promo: Boolean;
    Reprint: Boolean;
    Digital: Boolean;
    FullArt: Boolean;
    Textless: Boolean;
    StorySpotlight: Boolean;
    Games: TArray<string>;

    // Card Faces
    CardFaces: TArray<TCardFace>;

    // External Links
    ScryfallURI: string;
    URI: string;
    RelatedURIs: TRelatedURIs;
    PurchaseURIs: TPurchaseURIs;
    ScryfallCardBackID: string;
    ScryfallID: string;
    ScryfallIllustrationID: string;
    ScryfallOracleID: string;
    procedure Clear;
  end;

  // Record for Set Details
  TSetDetails = record
    SFID: string;
    Code: string;
    Name: string;
    ReleaseDate: string;
    SetType: string;
    Block: string;
    BlockCode: string;
    ParentSetCode: string;
    CardCount: Integer;
    Digital: Boolean;
    FoilOnly: Boolean;
    IconSVGURI: string;
    ScryfallURI: string;
    URI: string;
    SearchURI: string;
    procedure Clear;
  end;

  // Record for Ruling Details
  TRuling = record
    Source: string;
    PublishedAt: string;
    Comment: string;
    procedure Clear;
  end;

  // Record for Bulk Data Details
  TBulkData = record
    ID: string;
    DataType: string;
    DownloadURI: string;
    UpdatedAt: string;
    procedure Clear;
  end;

  // Record for Symbol Details
  TSymbol = record
    Symbol: string;
    English: string;
    SVGURI: string;
    procedure Clear;
  end;

  // Record for Scryfall Catalog
  TScryfallCatalog = record
    Name: string;
    Data: TArray<string>;
    TotalItems: Integer;
    URI: string;
    ObjectType: string;
    procedure Clear;
  end;

  // Record for Search Result
  TSearchResult = record
    Cards: TArray<TCardDetails>;
    HasMore: Boolean;
    NextPageURL: string;
    TotalCards: Integer;
    procedure Clear;
  end;

  // Class to Wrap TCardDetails as an Object
  TCardDetailsObject = class
  public
    CardDetails: TCardDetails;
    constructor Create(const ACardDetails: TCardDetails);
  end;

const
  // Mapping of TRarity to String
  RarityToString: array [TRarity] of string = (
    '', // rAll
    'common', // rCommon
    'uncommon', // rUncommon
    'rare', // rRare
    'mythic', // rMythic
    'special', // rSpecial
    'bonus', // rBonus
    'timeshifted', // rTimeshifted
    'masterpiece', // rMasterpiece
    'token', // rToken
    'double_faced_token', // rDoubleFacedToken
    'draft', // rDraft
    'planeshifted', // rPlaneshifted
    'unique', // rUnique
    'basic', // rBasic
    'promo' // rPromo
  );

const
  // Mapping of TLegalityFormat to String
  LegalityToString: array [TLegalityFormat] of string = (
    'standard', 'future', 'historic', 'gladiator', 'pioneer',
    'explorer', 'modern', 'legacy', 'pauper', 'vintage', 'penny',
    'commander', 'alchemy', 'brawl', 'paupercommander', 'duel',
    'oldschool', 'premodern', 'oathbreaker'
  );

  // Array for Legalities (for backward compatibility or other uses)
  LegalitiesArray: array [0 .. 18] of string = (
    'standard', 'future', 'historic', 'gladiator', 'pioneer',
    'explorer', 'modern', 'legacy', 'pauper', 'vintage', 'penny',
    'commander', 'alchemy', 'brawl', 'paupercommander', 'duel',
    'oldschool', 'premodern', 'oathbreaker'
  );

var
  AppClose: Boolean;

implementation

{ TCardDetailsObject }

constructor TCardDetailsObject.Create(const ACardDetails: TCardDetails);
begin
  inherited Create;
  CardDetails := ACardDetails;
end;

{ TCardPrices }

procedure TCardPrices.Clear;
begin
  USD := 0;
  USD_Foil := 0;
  EUR := 0;
  Tix := 0;
end;

{ TPurchaseURIs }

procedure TPurchaseURIs.Clear;
begin
  Self := Default(TPurchaseURIs);
end;

{ TRelatedURIs }

procedure TRelatedURIs.Clear;
begin
  Self := Default(TRelatedURIs);
end;

{ TImageUris }

procedure TImageUris.Clear;
begin
  Self := Default(TImageUris);
end;

{ TCardLegalities }

procedure TCardLegalities.Clear;
var
  Format: TLegalityFormat;
begin
  for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
    Status[Format] := '';
end;

function TCardLegalities.GetStatus(Format: TLegalityFormat): string;
begin
  Result := Status[Format];
end;

procedure TCardLegalities.SetStatus(Format: TLegalityFormat; const StatusStr: string);
begin
  Self.Status[Format] := StatusStr;
end;

// Function to Convert String to TLegalityFormat
function StringToLegalityFormat(const FormatName: string): TLegalityFormat;
begin
  case IndexText(LowerCase(FormatName), LegalityToString) of
    0: Result := lfStandard;
    1: Result := lfFuture;
    2: Result := lfHistoric;
    3: Result := lfGladiator;
    4: Result := lfPioneer;
    5: Result := lfExplorer;
    6: Result := lfModern;
    7: Result := lfLegacy;
    8: Result := lfPauper;
    9: Result := lfVintage;
    10: Result := lfPenny;
    11: Result := lfCommander;
    12: Result := lfAlchemy;
    13: Result := lfBrawl;
    14: Result := lfPauperCommander;
    15: Result := lfDuel;
    16: Result := lfOldschool;
    17: Result := lfPremodern;
    18: Result := lfOathbreaker;
  else
    raise Exception.CreateFmt('Unknown legality format: %s', [FormatName]);
  end;
end;

{ TCardFace }

procedure TCardFace.Clear;
begin
  Self := Default(TCardFace);
end;

{ TMeldDetails }

procedure TMeldDetails.Clear;
begin
  MeldParts := nil;
  MeldResult := Default(TCardPart);
end;

{ TCardDetails }

procedure TCardDetails.Clear;
begin
  // Core Identifiers
  SFID := '';
  OracleID := '';
  CardName := '';
  Lang := '';
  ReleasedAt := '';
  Layout := '';
  ArenaID := 0;
  EDHRank := 0;

  // Visuals and Presentation
  TypeLine := '';
  SetLength(ColorIdentity, 0);
  ManaCost := '';
  OracleText := '';
  FlavorText := '';
  Power := '';
  Toughness := '';
  Loyalty := '';
  SetIconURI := '';
  Artist := '';
  CollectorNumber := '';
  BorderColor := '';
  Frame := '';
  SecurityStamp := '';
  SetLength(Keywords, 0);
  SetLength(AllParts, 0);
  IsMeld := False;
  MeldDetails.Clear;

  // Legalities and Rules
  Legalities.Clear;
  PrintsSearchUri := '';
  RulingsUri := '';

  // Pricing Information
  Prices.Clear;

  // Images and Data
  ImageUris.Clear;
  SetLength(ImageData, 0);

  // Set and Rarity Information
  SetCode := '';
  SetName := '';
  Rarity := TRarity.rAll;

  // Additional Attributes
  CMC := 0.0;
  Reserved := False;
  Foil := False;
  NonFoil := False;
  Oversized := False;
  Promo := False;
  Reprint := False;
  Digital := False;
  FullArt := False;
  Textless := False;
  StorySpotlight := False;
  SetLength(Games, 0);

  // Card Faces
  SetLength(CardFaces, 0);

  // External Links
  ScryfallURI := '';
  URI := '';
  RelatedURIs.Clear;
  PurchaseURIs.Clear;
  ScryfallCardBackID := '';
  ScryfallID := '';
  ScryfallIllustrationID := '';
  ScryfallOracleID := '';
end;

{ TSetDetails }

procedure TSetDetails.Clear;
begin
  Self := Default(TSetDetails);
end;

{ TRuling }

procedure TRuling.Clear;
begin
  Source := '';
  PublishedAt := '';
  Comment := '';
end;

{ TBulkData }

procedure TBulkData.Clear;
begin
  ID := '';
  DataType := '';
  DownloadURI := '';
  UpdatedAt := '';
end;

{ TSymbol }

procedure TSymbol.Clear;
begin
  Symbol := '';
  English := '';
  SVGURI := '';
end;

{ TScryfallCatalog }

procedure TScryfallCatalog.Clear;
begin
  Name := '';
  SetLength(Data, 0);
  TotalItems := 0;
  URI := '';
  ObjectType := '';
end;

{ TSearchResult }

procedure TSearchResult.Clear;
begin
  SetLength(Cards, 0);
  HasMore := False;
  NextPageURL := '';
  TotalCards := 0;
end;

end.

