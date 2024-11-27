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
    ManaCost: string;
    TypeLine: string;
    OracleText: string;
    Power: string;
    Toughness: string;
    ImageUris: TImageUris;
  end;

  // Main Card Details structure
  TCardDetails = record
    SFID: string;
    CardName: string;
    TypeLine: string;
    ManaCost: string;
    OracleText: string;
    SetCode: string;
    SetName: string;
    Rarity: string;
    Power: string;
    Toughness: string;
    PrintsSearchUri: string;
    ImageUris: TImageUris;
    Legalities: TCardLegalities;
    Prices: TCardPrices;
    CardFaces: TArray<TCardFace>;
    ImageData: TBytes;
    OracleID: string;
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

const
  LegalitiesArray: array [0 .. 15] of string = ('Standard', 'Pioneer', 'Modern',
    'Legacy', 'Commander', 'Vintage', 'Pauper', 'Historic', 'Explorer',
    'Alchemy', 'Brawl', 'Future', 'Oldschool', 'Premodern', 'Duel', 'Penny');

implementation

{ TCardDetails }

// Clear method to reset all fields to default values
procedure TCardDetails.Clear;
begin
  Self := Default (TCardDetails);
end;

{ TSetDetails }

procedure TSetDetails.Clear;
begin
  Self := Default (TSetDetails);
end;

end.
