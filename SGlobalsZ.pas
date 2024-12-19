unit SGlobalsZ;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  // Prices for different currencies
  TCardPrices = record
    USD: string;
    USD_Foil: string;
    EUR: string;
    Tix: string;
    procedure Clear;
  end;

  // Image URIs for different resolutions
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

  // Legalities for different formats
  TCardLegalities = record
    Standard: string;
    Pioneer: string;
    Modern: string;
    Legacy: string;
    Commander: string;
    Vintage: string;
    Pauper: string;
    Historic: string;
    Explorer: string;
    Alchemy: string;
    Brawl: string;
    Future: string;
    Oldschool: string;
    Premodern: string;
    Duel: string;
    Penny: string;
    procedure Clear;
  end;

  // Card Face for multi-faced cards
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
    procedure Clear;
  end;

  // Main Card Details structure
  TCardDetails = record
    // Core Identifiers
    SFID: string;
    OracleID: string;
    CardName: string;
    Lang: string;
    ReleasedAt: string;
    Layout: string;

    // Visuals and Presentation
    TypeLine: string;
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
    Rarity: string;

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
    RelatedURIs: TDictionary<string, string>;
    PurchaseURIs: TDictionary<string, string>;

    procedure Clear;
  end;

  // Set Details
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

  // Ruling Details
  TRuling = record
    Source: string;
    PublishedAt: string;
    Comment: string;
    procedure Clear;
  end;

  // Bulk Data Details
  TBulkData = record
    ID: string;
    DataType: string;
    DownloadURI: string;
    UpdatedAt: string;
    procedure Clear;
  end;

  // Symbol Details
  TSymbol = record
    Symbol: string;
    English: string;
    SVGURI: string;
    procedure Clear;
  end;

  // Catalog for Scryfall
  TScryfallCatalog = record
    Name: string;
    Data: TArray<string>;
    TotalItems: Integer;
    Uri: string;
    ObjectType: string;
    procedure Clear;
  end;

  // Search Result
  TSearchResult = record
    Cards: TArray<TCardDetails>;
    HasMore: Boolean;
    NextPageURL: string;
    TotalCards: Integer;
    procedure Clear;
  end;

const
  LegalitiesArray: array [0 .. 15] of string = ('Standard', 'Pioneer', 'Modern',
    'Legacy', 'Commander', 'Vintage', 'Pauper', 'Historic', 'Explorer',
    'Alchemy', 'Brawl', 'Future', 'Oldschool', 'Premodern', 'Duel', 'Penny');

implementation

{ TCardPrices }

procedure TCardPrices.Clear;
begin
  USD := '';
  USD_Foil := '';
  EUR := '';
  Tix := '';
end;

{ TImageUris }

procedure TImageUris.Clear;
begin
  Small := '';
  Normal := '';
  Large := '';
  BackFace := '';
  PNG := '';
  Border_crop := '';
  Art_crop := '';
end;

{ TCardLegalities }

procedure TCardLegalities.Clear;
begin
  Self := Default(TCardLegalities);
end;

{ TCardFace }

procedure TCardFace.Clear;
begin
  Name := '';
  FlavorText := '';
  ManaCost := '';
  TypeLine := '';
  OracleText := '';
  Power := '';
  Toughness := '';
  Loyalty := '';
  ImageUris.Clear;
end;

{ TCardDetails }

procedure TCardDetails.Clear;
begin
  SFID := '';
  OracleID := '';
  CardName := '';
  Lang := '';
  ReleasedAt := '';
  Layout := '';
  TypeLine := '';
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
  Legalities.Clear;
  PrintsSearchUri := '';
  RulingsUri := '';
  Prices.Clear;
  ImageUris.Clear;
  SetLength(Games, 0);
  SetLength(CardFaces, 0);

  if Assigned(RelatedURIs) then
    FreeAndNil(RelatedURIs);

  if Assigned(PurchaseURIs) then
    FreeAndNil(PurchaseURIs);

  SetLength(ImageData, 0);
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
  Uri := '';
  ObjectType := '';
  SetLength(Data, 0);
  TotalItems := 0;
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
