unit SGlobalsZ;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections,System.StrUtils;

type
  TRarity = (rAll, rCommon, rUncommon, rRare, rMythic, rSpecial, rBonus,
    rTimeshifted, rMasterpiece, rToken, rDoubleFacedToken, rDraft,
    rPlaneshifted, rUnique, rBasic, rPromo);


  TLegalityFormat = (
    lfStandard, lfFuture, lfHistoric, lfGladiator, lfPioneer, lfExplorer,
    lfModern, lfLegacy, lfPauper, lfVintage, lfPenny, lfCommander, lfAlchemy,
    lfBrawl, lfPauperCommander, lfDuel, lfOldschool, lfPremodern, lfOathbreaker
  );

//  TCardColor = (ccWhite, ccBlue, ccBlack, ccRed, ccGreen, ccColorless);


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
type
  TCardLegalities = record
    Status: array[TLegalityFormat] of string;
    procedure Clear;
    function GetStatus(Format: TLegalityFormat): string;
    procedure SetStatus(Format: TLegalityFormat; const Status: string);
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
    CMC: Double;
    procedure Clear;
  end;

  TCardPart = record
    ObjectType: string;
    ID: string;
    Component: string;
    Name: string;
    TypeLine: string;
    URI: string;
  end;

  TMeldDetails = record
    MeldParts: TArray<TCardPart>;
    MeldResult: TCardPart;
  end;

  TRelatedURIs = record
    Gatherer: string;
    Tcgplayerinfinitearticles: string;
    Tcgplayerinfinitedecks: string;
    Edhrec: string;
    procedure Clear;
  end;

  TPurchaseURIs = record
    Tcgplayer: string;
    Cardmarket: string;
    Cardhoarder: string;
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

  // TCardDetailsObject
type
  TCardDetailsObject = class
  public
    CardDetails: TCardDetails;
    constructor Create(const ACardDetails: TCardDetails);
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
  LegalityToString: array[TLegalityFormat] of string = (
    'standard', 'future', 'historic', 'gladiator', 'pioneer', 'explorer',
    'modern', 'legacy', 'pauper', 'vintage', 'penny', 'commander', 'alchemy',
    'brawl', 'paupercommander', 'duel', 'oldschool', 'premodern', 'oathbreaker'
  );


  LegalitiesArray: array [0 .. 18] of string = ('standard', 'future',
    'historic', 'gladiator', 'pioneer', 'explorer', 'modern', 'legacy',
    'pauper', 'vintage', 'penny', 'commander', 'alchemy', 'brawl',
    'paupercommander', 'duel', 'oldschool', 'premodern', 'oathbreaker');

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

procedure TPurchaseURIs.Clear;
begin
  Self := Default (TPurchaseURIs);
end;

procedure TRelatedURIs.Clear;
begin
  Self := Default (TRelatedURIs);
end;

{ TImageUris }

procedure TImageUris.Clear;
begin
  Self := Default (TImageUris);
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

procedure TCardLegalities.SetStatus(Format: TLegalityFormat; const Status: string);
begin
  Self.Status[Format] := Status;
end;


function StringToLegalityFormat(const FormatName: string): TLegalityFormat;
begin
  case IndexText(FormatName.ToLower, LegalityToString) of
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



//procedure TCardLegalities.SetStatus(const FormatName, Status: string);
//begin
//  case IndexText(FormatName.ToLower, LegalitiesArray) of
//    0: Standard := Status;
//    1: Future := Status;
//    2: Historic := Status;
//    3: Gladiator := Status;
//    4: Pioneer := Status;
//    5: Explorer := Status;
//    6: Modern := Status;
//    7: Legacy := Status;
//    8: Pauper := Status;
//    9: Vintage := Status;
//    10: Penny := Status;
//    11: Commander := Status;
//    12: Alchemy := Status;
//    13: Brawl := Status;
//    14: PauperCommander := Status;
//    15: Duel := Status;
//    16: Oldschool := Status;
//    17: Premodern := Status;
//    18: Oathbreaker := Status;
//  else
//    raise Exception.CreateFmt('Unknown format: %s', [FormatName]);
//  end;
//end;
//
//function TCardLegalities.GetStatus(const FormatName: string): string;
//begin
//  case IndexText(FormatName.ToLower, LegalitiesArray) of
//    0: Result := Standard;
//    1: Result := Future;
//    2: Result := Historic;
//    3: Result := Gladiator;
//    4: Result := Pioneer;
//    5: Result := Explorer;
//    6: Result := Modern;
//    7: Result := Legacy;
//    8: Result := Pauper;
//    9: Result := Vintage;
//    10: Result := Penny;
//    11: Result := Commander;
//    12: Result := Alchemy;
//    13: Result := Brawl;
//    14: Result := PauperCommander;
//    15: Result := Duel;
//    16: Result := Oldschool;
//    17: Result := Premodern;
//    18: Result := Oathbreaker;
//  else
//    Result := '';
//  end;
//end;


{ TCardFace }

procedure TCardFace.Clear;
begin
  Self := Default (TCardFace);
end;

{ TCardDetails }

procedure TCardDetails.Clear;
begin
  Self := Default(TCardDetails);

  Legalities.Clear;
  Prices.Clear;
  ImageUris.Clear;
  RelatedURIs.Clear;
  PurchaseURIs.Clear;

  SetLength(ColorIdentity, 0);
  SetLength(Keywords, 0);
  SetLength(Games, 0);
  SetLength(AllParts, 0);
  SetLength(CardFaces, 0);
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
