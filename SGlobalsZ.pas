unit SGlobalsZ;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

var
  CardTitle: string;

type
  // Prices for different currencies
  TCardPrices = record
    USD: string;
    USD_Foil: string;
    EUR: string;
    Tix: string;
  end;

  // Image URIs for different resolutions
  TImageUris = record
    Small: string;
    Normal: string;
    Large: string;
    BackFace: string;
    PNG: string;
    border_crop: string;
    art_crop: string;
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
  end;

  // Card Face for multi-faced cards (e.g., double-sided cards)
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
  end;

type
  // Main Card Details structure
  TCardDetails = record
    // Core Identifiers
    SFID: string;                       // Unique card ID
    OracleID: string;                   // Oracle ID for card rulings and prints
    CardName: string;                   // Card name
    Lang: string;                       // Language of the card
    ReleasedAt: string;                 // Release date
    Layout: string;                     // Card layout (e.g., normal, transform)

    // Visuals and Presentation
    TypeLine: string;                   // Card type line (e.g., "Instant")
    ManaCost: string;                   // Mana cost (e.g., "{U}{U}")
    OracleText: string;                 // Rules text (e.g., "Draw a card.")
    FlavorText: string;                 // Flavor text (italicized story text)
    Power: string;                      // Power (for creatures)
    Toughness: string;                  // Toughness (for creatures)
    Loyalty: string;                    // Loyalty (for planeswalkers)
    SetIconURI: string;                 // URI for the set icon SVG
    Artist: string;                     // Artist name
    CollectorNumber: string;            // Collector number of the card
    BorderColor: string;                // Card border color
    Frame: string;                      // Frame type (e.g., "2015")
    SecurityStamp: string;              // Security stamp type (e.g., "oval")
    Keywords: TArray<string>;           // Keywords like "flying", or "firststrike"

    // Legalities and Rules
    Legalities: TCardLegalities;        // Card legality for formats
    PrintsSearchUri: string;            // URI for other printings of this card
    RulingsUri: string;                 // URI for card rulings

    // Pricing Information
    Prices: TCardPrices;                // Pricing (USD, EUR, Tix, etc.)

    // Images and Data
    ImageUris: TImageUris;              // URIs for images
    ImageData: TBytes;                  // Binary image data (if fetched)

    // Set and Rarity Information
    SetCode: string;                    // Set code (e.g., "vma")
    SetName: string;                    // Set name (e.g., "Vintage Masters")
    Rarity: string;                     // Card rarity (e.g., "rare", "mythic")

    // Additional Attributes
    CMC: Double;                        // Converted mana cost
    Reserved: Boolean;                  // If the card is on the reserved list
    Foil: Boolean;                      // If the card has a foil version
    NonFoil: Boolean;                   // If the card has a non-foil version
    Oversized: Boolean;                 // If the card is oversized
    Promo: Boolean;                     // If the card is a promo
    Reprint: Boolean;                   // If the card is a reprint
    Digital: Boolean;                   // If the card is digital-only
    FullArt: Boolean;                   // If the card is a full-art card
    Textless: Boolean;                  // If the card is textless
    StorySpotlight: Boolean;            // If the card is part of a story spotlight

    // Additional Card Faces (for transform or modal cards)
    CardFaces: TArray<TCardFace>;       // Array of card faces (if applicable)

    // External Links
    ScryfallURI: string;                // Link to Scryfall page
    URI: string;                        // Link to the API endpoint
    RelatedURIs: TDictionary<string, string>;   // Related URIs (e.g., rulings)
    PurchaseURIs: TDictionary<string, string>;  // Purchase URIs (e.g., TCGPlayer)

    // Methods
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
  end;

  // Bulk Data Details
  TBulkData = record
    ID: string;
    DataType: string;
    DownloadURI: string;
    UpdatedAt: string;
  end;

  // Symbol Details
  TSymbol = record
    Symbol: string;
    English: string;
    SVGURI: string;
  end;

  TSearchResult = record
    Cards: TArray<TCardDetails>;
    HasMore: Boolean;
    NextPageURL: string;
    TotalCards: Integer;
  end;

  type
  TScryfallCatalog = record
    Name: string;  // Name of the catalog
    Data: TArray<string>; // Array of creature types
    TotalItems: Integer;  // Track total items for additional metadata
    Uri: string;           // Optional: API URI for debugging or reference
    ObjectType: string;    // Optional: API object type (e.g., "catalog")
    procedure Clear;
  end;



const
  LegalitiesArray: array [0 .. 15] of string = ('Standard', 'Pioneer', 'Modern',
    'Legacy', 'Commander', 'Vintage', 'Pauper', 'Historic', 'Explorer',
    'Alchemy', 'Brawl', 'Future', 'Oldschool', 'Premodern', 'Duel', 'Penny');

implementation

{ TCardDetails }

// Clear method to reset all fields to default values
procedure TCardDetails.Clear;
begin
  if Assigned(RelatedURIs) then
    RelatedURIs.Free;
  if Assigned(PurchaseURIs) then
    PurchaseURIs.Free;
  SetLength(Keywords, 0); // Clear the Keywords array
  Self := Default(TCardDetails);
end;

{ TSetDetails }

procedure TSetDetails.Clear;
begin
  Self := Default (TSetDetails);
end;

procedure TScryfallCatalog.Clear;
begin
  Name := '';
  Uri := '';
  ObjectType := '';
  SetLength(Data, 0);
  TotalItems := 0;
end;

end.
