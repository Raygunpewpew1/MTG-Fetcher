unit SGlobalsZ;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TRarity = (rAll, rCommon, rUncommon, rRare, rMythic, rSpecial, rBonus,
    rTimeshifted, rMasterpiece, rToken, rDoubleFacedToken, rDraft,
    rPlaneshifted, rUnique, rBasic, rPromo);

type
  // Prices for different currencies
  TCardPrices = record
    USD: Currency;
    USD_Foil: Currency;
    EUR: Currency;
    Tix: Currency;
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
    Gladiator: string;
    PauperCommander: string;
    Oathbreaker: string;
    procedure Clear;
    procedure SetStatus(const FormatName, Status: string);
    function GetStatus(const FormatName: string): string;
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
    RelatedURIs: TDictionary<string, string>;
    PurchaseURIs: TDictionary<string, string>;
    ScryfallCardBackID: string;
    ScryfallID: string;
    ScryfallIllustrationID: string;
    ScryfallOracleID: string;

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
    URI: string;
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
  RarityToString: array [TRarity] of string = ('', // rAll
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
  LegalitiesArray: array [0 .. 18] of string = ('standard', 'future',
    'historic', 'gladiator', 'pioneer', 'explorer', 'modern', 'legacy',
    'pauper', 'vintage', 'penny', 'commander', 'alchemy', 'brawl',
    'paupercommander', 'duel', 'oldschool', 'premodern', 'oathbreaker');

 var   AppClose: Boolean;

implementation

{ TCardPrices }

procedure TCardPrices.Clear;
begin
  USD := 0;
  USD_Foil := 0;
  EUR := 0;
  Tix := 0;
end;

{ TImageUris }

procedure TImageUris.Clear;
begin
  Self := Default (TImageUris);
end;

{ TCardLegalities }

procedure TCardLegalities.Clear;
begin
  Self := Default (TCardLegalities);
end;

procedure TCardLegalities.SetStatus(const FormatName, Status: string);
begin
  if FormatName = 'Standard' then
    Standard := Status
  else if FormatName = 'Future' then
    Future := Status
  else if FormatName = 'Historic' then
    Historic := Status
  else if FormatName = 'Gladiator' then
    Gladiator := Status
  else if FormatName = 'Pioneer' then
    Pioneer := Status
  else if FormatName = 'Explorer' then
    Explorer := Status
  else if FormatName = 'Modern' then
    Modern := Status
  else if FormatName = 'Legacy' then
    Legacy := Status
  else if FormatName = 'Pauper' then
    Pauper := Status
  else if FormatName = 'Vintage' then
    Vintage := Status
  else if FormatName = 'Penny' then
    Penny := Status
  else if FormatName = 'Commander' then
    Commander := Status
  else if FormatName = 'Alchemy' then
    Alchemy := Status
  else if FormatName = 'Brawl' then
    Brawl := Status
  else if FormatName = 'PauperCommander' then
    PauperCommander := Status
  else if FormatName = 'Duel' then
    Duel := Status
  else if FormatName = 'Oldschool' then
    Oldschool := Status
  else if FormatName = 'Premodern' then
    Premodern := Status
  else if FormatName = 'Oathbreaker' then
    Oathbreaker := Status;
end;

function TCardLegalities.GetStatus(const FormatName: string): string;
begin
  if FormatName = 'Standard' then
    Result := Standard
  else if FormatName = 'Future' then
    Result := Future
  else if FormatName = 'Historic' then
    Result := Historic
  else if FormatName = 'Gladiator' then
    Result := Gladiator
  else if FormatName = 'Pioneer' then
    Result := Pioneer
  else if FormatName = 'Explorer' then
    Result := Explorer
  else if FormatName = 'Modern' then
    Result := Modern
  else if FormatName = 'Legacy' then
    Result := Legacy
  else if FormatName = 'Pauper' then
    Result := Pauper
  else if FormatName = 'Vintage' then
    Result := Vintage
  else if FormatName = 'Penny' then
    Result := Penny
  else if FormatName = 'Commander' then
    Result := Commander
  else if FormatName = 'Alchemy' then
    Result := Alchemy
  else if FormatName = 'Brawl' then
    Result := Brawl
  else if FormatName = 'PauperCommander' then
    Result := PauperCommander
  else if FormatName = 'Duel' then
    Result := Duel
  else if FormatName = 'Oldschool' then
    Result := Oldschool
  else if FormatName = 'Premodern' then
    Result := Premodern
  else if FormatName = 'Oathbreaker' then
    Result := Oathbreaker
  else
    Result := '';
end;

{ TCardFace }

procedure TCardFace.Clear;
begin
  Self := Default (TCardFace);
end;

{ TCardDetails }

procedure TCardDetails.Clear;
begin
  Self := Default (TCardDetails);

  if Assigned(RelatedURIs) then
    FreeAndNil(RelatedURIs);

  if Assigned(PurchaseURIs) then
    FreeAndNil(PurchaseURIs);
end;

{ TSetDetails }

procedure TSetDetails.Clear;
begin
  Self := Default (TSetDetails);
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
  URI := '';
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
