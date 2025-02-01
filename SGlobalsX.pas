unit SGlobalsX;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, System.StrUtils;

type
  TRarity = (rAll, rCommon, rUncommon, rRare, rMythic, rSpecial, rBonus,
    rTimeshifted, rMasterpiece, rToken, rDoubleFacedToken, rDraft,
    rPlaneshifted, rUnique, rBasic, rPromo);

  TLegalityFormat = (lfStandard, lfFuture, lfHistoric, lfGladiator, lfPioneer,
    lfExplorer, lfModern, lfLegacy, lfPauper, lfVintage, lfPenny, lfCommander,
    lfAlchemy, lfBrawl, lfPauperCommander, lfDuel, lfOldschool, lfPremodern,
    lfOathbreaker);

  TCardPrices = class
  private
    FUSD: Currency;
    FUSD_Foil: Currency;
    FEUR: Currency;
    FTix: Currency;
  public
    procedure Clear;
    property USD: Currency read FUSD write FUSD;
    property USD_Foil: Currency read FUSD_Foil write FUSD_Foil;
    property EUR: Currency read FEUR write FEUR;
    property Tix: Currency read FTix write FTix;
  end;

  TImageUris = class
  private
    FSmall: string;
    FNormal: string;
    FLarge: string;
    FBackFace: string;
    FPNG: string;
    FBorder_crop: string;
    FArt_crop: string;
  public
    procedure Clear;
    property Small: string read FSmall write FSmall;
    property Normal: string read FNormal write FNormal;
    property Large: string read FLarge write FLarge;
    property BackFace: string read FBackFace write FBackFace;
    property PNG: string read FPNG write FPNG;
    property Border_crop: string read FBorder_crop write FBorder_crop;
    property Art_crop: string read FArt_crop write FArt_crop;
  end;

  TCardLegalities = class
  private
    FStatus: array [TLegalityFormat] of string;
  public
    procedure Clear;
    function GetStatus(Format: TLegalityFormat): string;
    procedure SetStatus(Format: TLegalityFormat; const StatusStr: string);
  end;

  TCardFace = class
  private
    FName: string;
    FFlavorText: string;
    FManaCost: string;
    FTypeLine: string;
    FOracleText: string;
    FPower: string;
    FToughness: string;
    FImageUris: TImageUris;
    FLoyalty: string;
    FCMC: Double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Name: string read FName write FName;
    property FlavorText: string read FFlavorText write FFlavorText;
    property ManaCost: string read FManaCost write FManaCost;
    property TypeLine: string read FTypeLine write FTypeLine;
    property OracleText: string read FOracleText write FOracleText;
    property Power: string read FPower write FPower;
    property Toughness: string read FToughness write FToughness;
    property ImageUris: TImageUris read FImageUris;
    property Loyalty: string read FLoyalty write FLoyalty;
    property CMC: Double read FCMC write FCMC;
  end;

  TCardPart = class
  private
    FObjectType: string;
    FID: string;
    FComponent: string;
    FName: string;
    FTypeLine: string;
    FURI: string;
  public
    procedure Clear;
    property ObjectType: string read FObjectType write FObjectType;
    property ID: string read FID write FID;
    property Component: string read FComponent write FComponent;
    property Name: string read FName write FName;
    property TypeLine: string read FTypeLine write FTypeLine;
    property URI: string read FURI write FURI;
  end;

  TMeldDetails = class
  private
    FMeldParts: TObjectList<TCardPart>;
    FMeldResult: TCardPart;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property MeldParts: TObjectList<TCardPart> read FMeldParts;
    property MeldResult: TCardPart read FMeldResult;
  end;

  TRelatedURIs = class
  private
    FGatherer: string;
    FTcgplayerInfiniteArticles: string;
    FTcgplayerInfiniteDecks: string;
    FEdhrec: string;
  public
    procedure Clear;
    property Gatherer: string read FGatherer write FGatherer;
    property TcgplayerInfiniteArticles: string read FTcgplayerInfiniteArticles
      write FTcgplayerInfiniteArticles;
    property TcgplayerInfiniteDecks: string read FTcgplayerInfiniteDecks
      write FTcgplayerInfiniteDecks;
    property Edhrec: string read FEdhrec write FEdhrec;
  end;

  TPurchaseURIs = class
  private
    FTcgplayer: string;
    FCardmarket: string;
    FCardhoarder: string;
  public
    procedure Clear;
    property Tcgplayer: string read FTcgplayer write FTcgplayer;
    property Cardmarket: string read FCardmarket write FCardmarket;
    property Cardhoarder: string read FCardhoarder write FCardhoarder;
  end;

  TCardDetails = class
  private
    FSFID: string;
    FOracleID: string;
    FCardName: string;
    FLang: string;
    FReleasedAt: string;
    FLayout: string;
    FArenaID: Integer;
    FEDHRank: Integer;
    FTypeLine: string;
    FColorIdentity: TList<string>;
    FManaCost: string;
    FOracleText: string;
    FFlavorText: string;
    FPower: string;
    FToughness: string;
    FLoyalty: string;
    FSetIconURI: string;
    FArtist: string;
    FCollectorNumber: string;
    FBorderColor: string;
    FFrame: string;
    FSecurityStamp: string;
    FKeywords: TList<string>;
    FAllParts: TObjectList<TCardPart>;
    FIsMeld: Boolean;
    FMeldDetails: TMeldDetails;
    FLegalities: TCardLegalities;
    FPrintsSearchUri: string;
    FRulingsUri: string;
    FPrices: TCardPrices;
    FImageUris: TImageUris;
    FImageData: TBytes;
    FSetCode: string;
    FSetName: string;
    FRarity: TRarity;
    FCMC: Double;
    FReserved: Boolean;
    FFoil: Boolean;
    FNonFoil: Boolean;
    FOversized: Boolean;
    FPromo: Boolean;
    FReprint: Boolean;
    FDigital: Boolean;
    FFullArt: Boolean;
    FTextless: Boolean;
    FStorySpotlight: Boolean;
    FGames: TList<string>;
    FCardFaces: TObjectList<TCardFace>;
    FScryfallURI: string;
    FURI: string;
    FRelatedURIs: TRelatedURIs;
    FPurchaseURIs: TPurchaseURIs;
    FScryfallCardBackID: string;
    FScryfallID: string;
    FScryfallIllustrationID: string;
    FScryfallOracleID: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    // Properties
    property SFID: string read FSFID write FSFID;
    property OracleID: string read FOracleID write FOracleID;
    property CardName: string read FCardName write FCardName;
    property Lang: string read FLang write FLang;
    property ReleasedAt: string read FReleasedAt write FReleasedAt;
    property Layout: string read FLayout write FLayout;
    property ArenaID: Integer read FArenaID write FArenaID;
    property EDHRank: Integer read FEDHRank write FEDHRank;
    property TypeLine: string read FTypeLine write FTypeLine;
    property ColorIdentity: TList<string> read FColorIdentity;
    property ManaCost: string read FManaCost write FManaCost;
    property OracleText: string read FOracleText write FOracleText;
    property FlavorText: string read FFlavorText write FFlavorText;
    property Power: string read FPower write FPower;
    property Toughness: string read FToughness write FToughness;
    property Loyalty: string read FLoyalty write FLoyalty;
    property SetIconURI: string read FSetIconURI write FSetIconURI;
    property Artist: string read FArtist write FArtist;
    property CollectorNumber: string read FCollectorNumber
      write FCollectorNumber;
    property BorderColor: string read FBorderColor write FBorderColor;
    property Frame: string read FFrame write FFrame;
    property SecurityStamp: string read FSecurityStamp write FSecurityStamp;
    property Keywords: TList<string> read FKeywords;
    property AllParts: TObjectList<TCardPart> read FAllParts;
    property IsMeld: Boolean read FIsMeld write FIsMeld;
    property MeldDetails: TMeldDetails read FMeldDetails;
    property Legalities: TCardLegalities read FLegalities;
    property PrintsSearchUri: string read FPrintsSearchUri
      write FPrintsSearchUri;
    property RulingsUri: string read FRulingsUri write FRulingsUri;
    property Prices: TCardPrices read FPrices;
    property ImageUris: TImageUris read FImageUris;
    property ImageData: TBytes read FImageData write FImageData;
    property SetCode: string read FSetCode write FSetCode;
    property SetName: string read FSetName write FSetName;
    property Rarity: TRarity read FRarity write FRarity;
    property CMC: Double read FCMC write FCMC;
    property Reserved: Boolean read FReserved write FReserved;
    property Foil: Boolean read FFoil write FFoil;
    property NonFoil: Boolean read FNonFoil write FNonFoil;
    property Oversized: Boolean read FOversized write FOversized;
    property Promo: Boolean read FPromo write FPromo;
    property Reprint: Boolean read FReprint write FReprint;
    property Digital: Boolean read FDigital write FDigital;
    property FullArt: Boolean read FFullArt write FFullArt;
    property Textless: Boolean read FTextless write FTextless;
    property StorySpotlight: Boolean read FStorySpotlight write FStorySpotlight;
    property Games: TList<string> read FGames;
    property CardFaces: TObjectList<TCardFace> read FCardFaces;
    property ScryfallURI: string read FScryfallURI write FScryfallURI;
    property URI: string read FURI write FURI;
    property RelatedURIs: TRelatedURIs read FRelatedURIs;
    property PurchaseURIs: TPurchaseURIs read FPurchaseURIs;
    property ScryfallCardBackID: string read FScryfallCardBackID
      write FScryfallCardBackID;
    property ScryfallID: string read FScryfallID write FScryfallID;
    property ScryfallIllustrationID: string read FScryfallIllustrationID
      write FScryfallIllustrationID;
    property ScryfallOracleID: string read FScryfallOracleID
      write FScryfallOracleID;
  end;

type
  TSetDetails = class
  private
    FSFID: string;
    FCode: string;
    FName: string;
    FReleaseDate: string;
    FSetType: string;
    FBlock: string;
    FBlockCode: string;
    FParentSetCode: string;
    FCardCount: Integer;
    FDigital: Boolean;
    FFoilOnly: Boolean;
    FIconSVGURI: string;
    FScryfallURI: string;
    FURI: string;
    FSearchURI: string;
  public
    procedure Clear;
    property SFID: string read FSFID write FSFID;
    property Code: string read FCode write FCode;
    property Name: string read FName write FName;
    property ReleaseDate: string read FReleaseDate write FReleaseDate;
    property SetType: string read FSetType write FSetType;
    property Block: string read FBlock write FBlock;
    property BlockCode: string read FBlockCode write FBlockCode;
    property ParentSetCode: string read FParentSetCode write FParentSetCode;
    property CardCount: Integer read FCardCount write FCardCount;
    property Digital: Boolean read FDigital write FDigital;
    property FoilOnly: Boolean read FFoilOnly write FFoilOnly;
    property IconSVGURI: string read FIconSVGURI write FIconSVGURI;
    property ScryfallURI: string read FScryfallURI write FScryfallURI;
    property URI: string read FURI write FURI;
    property SearchURI: string read FSearchURI write FSearchURI;
  end;

  TRuling = class
  private
    FSource: string;
    FPublishedAt: string;
    FComment: string;
  public
    procedure Clear;
    property Source: string read FSource write FSource;
    property PublishedAt: string read FPublishedAt write FPublishedAt;
    property Comment: string read FComment write FComment;
  end;

  TBulkData = class
  private
    FID: string;
    FDataType: string;
    FDownloadURI: string;
    FUpdatedAt: string;
  public
    procedure Clear;
    property ID: string read FID write FID;
    property DataType: string read FDataType write FDataType;
    property DownloadURI: string read FDownloadURI write FDownloadURI;
    property UpdatedAt: string read FUpdatedAt write FUpdatedAt;
  end;

  TSymbol = class
  private
    FSymbol: string;
    FEnglish: string;
    FSVGURI: string;
  public
    procedure Clear;
    property Symbol: string read FSymbol write FSymbol;
    property English: string read FEnglish write FEnglish;
    property SVGURI: string read FSVGURI write FSVGURI;
  end;

  TScryfallCatalog = class
  private
    FName: string;
    FData: TArray<string>;
    FTotalItems: Integer;
    FURI: string;
    FObjectType: string;
  public
    procedure Clear;
    property Name: string read FName write FName;
    property Data: TArray<string> read FData write FData;
    property TotalItems: Integer read FTotalItems write FTotalItems;
    property URI: string read FURI write FURI;
    property ObjectType: string read FObjectType write FObjectType;
  end;

type
  TSearchResult = class
  private
    FCards: TObjectList<TCardDetails>;
    FHasMore: Boolean;
    FNextPageURL: string;
    FTotalCards: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    // Properties
    property Cards: TObjectList<TCardDetails> read FCards;
    property HasMore: Boolean read FHasMore write FHasMore;
    property NextPageURL: string read FNextPageURL write FNextPageURL;
    property TotalCards: Integer read FTotalCards write FTotalCards;
  end;

  // Helper classes for enum conversions
  TRarityHelper = record helper for TRarity
    function ToString: string;
    class function FromString(const Value: string): TRarity; static;
  end;

  TLegalityFormatHelper = record helper for TLegalityFormat
    function ToString: string;
    class function FromString(const Value: string): TLegalityFormat; static;
  end;

implementation

{ TCardPrices }
procedure TCardPrices.Clear;
begin
  FUSD := 0;
  FUSD_Foil := 0;
  FEUR := 0;
  FTix := 0;
end;

{ TImageUris }
procedure TImageUris.Clear;
begin
  FSmall := '';
  FNormal := '';
  FLarge := '';
  FBackFace := '';
  FPNG := '';
  FBorder_crop := '';
  FArt_crop := '';
end;

{ TCardLegalities }
procedure TCardLegalities.Clear;
var
  Format: TLegalityFormat;
begin
  for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
    FStatus[Format] := '';
end;

{ TCardPart }
procedure TCardPart.Clear;
begin
  FObjectType := '';
  FID := '';
  FComponent := '';
  FName := '';
  FTypeLine := '';
  FURI := '';
end;

{ TRelatedURIs }
procedure TRelatedURIs.Clear;
begin
  FGatherer := '';
  FTcgplayerInfiniteArticles := '';
  FTcgplayerInfiniteDecks := '';
  FEdhrec := '';
end;

{ TPurchaseURIs }
procedure TPurchaseURIs.Clear;
begin
  FTcgplayer := '';
  FCardmarket := '';
  FCardhoarder := '';
end;

{ TSetDetails }
procedure TSetDetails.Clear;
begin
  FSFID := '';
  FCode := '';
  FName := '';
  FReleaseDate := '';
  FSetType := '';
  FBlock := '';
  FBlockCode := '';
  FParentSetCode := '';
  FCardCount := 0;
  FDigital := False;
  FFoilOnly := False;
  FIconSVGURI := '';
  FScryfallURI := '';
  FURI := '';
  FSearchURI := '';
end;

{ TRuling }
procedure TRuling.Clear;
begin
  FSource := '';
  FPublishedAt := '';
  FComment := '';
end;

{ TBulkData }
procedure TBulkData.Clear;
begin
  FID := '';
  FDataType := '';
  FDownloadURI := '';
  FUpdatedAt := '';
end;

{ TSymbol }
procedure TSymbol.Clear;
begin
  FSymbol := '';
  FEnglish := '';
  FSVGURI := '';
end;

{ TScryfallCatalog }
procedure TScryfallCatalog.Clear;
begin
  FName := '';
  SetLength(FData, 0);
  FTotalItems := 0;
  FURI := '';
  FObjectType := '';
end;

{ TSearchResult }

constructor TSearchResult.Create;
begin
  inherited;
  FCards := TObjectList<TCardDetails>.Create(True); // Owns card objects
end;

destructor TSearchResult.Destroy;
begin
  FCards.Free;
  inherited;
end;

procedure TSearchResult.Clear;
begin
  FCards.Clear;
  FHasMore := False;
  FNextPageURL := '';
  FTotalCards := 0;
end;

function TCardLegalities.GetStatus(Format: TLegalityFormat): string;
begin
  Result := FStatus[Format];
end;

procedure TCardLegalities.SetStatus(Format: TLegalityFormat;
  const StatusStr: string);
begin
  FStatus[Format] := StatusStr;
end;

{ TCardFace }
constructor TCardFace.Create;
begin
  FImageUris := TImageUris.Create;
end;

destructor TCardFace.Destroy;
begin
  FImageUris.Free;
  inherited;
end;

procedure TCardFace.Clear;
begin
  FName := '';
  FFlavorText := '';
  FManaCost := '';
  FTypeLine := '';
  FOracleText := '';
  FPower := '';
  FToughness := '';
  FLoyalty := '';
  FCMC := 0;
  FImageUris.Clear;
end;

{ TMeldDetails }
constructor TMeldDetails.Create;
begin
  FMeldParts := TObjectList<TCardPart>.Create(True);
  FMeldResult := TCardPart.Create;
end;

destructor TMeldDetails.Destroy;
begin
  FMeldParts.Free;
  FMeldResult.Free;
  inherited;
end;

procedure TMeldDetails.Clear;
begin
  FMeldParts.Clear;
  FMeldResult.Clear;
end;

{ TCardDetails }
constructor TCardDetails.Create;
begin
  FColorIdentity := TList<string>.Create;
  FKeywords := TList<string>.Create;
  FAllParts := TObjectList<TCardPart>.Create(True);
  FMeldDetails := TMeldDetails.Create;
  FLegalities := TCardLegalities.Create;
  FPrices := TCardPrices.Create;
  FImageUris := TImageUris.Create;
  FGames := TList<string>.Create;
  FCardFaces := TObjectList<TCardFace>.Create(True);
  FRelatedURIs := TRelatedURIs.Create;
  FPurchaseURIs := TPurchaseURIs.Create;
end;

destructor TCardDetails.Destroy;
begin
  FColorIdentity.Free;
  FKeywords.Free;
  FAllParts.Free;
  FMeldDetails.Free;
  FLegalities.Free;
  FPrices.Free;
  FImageUris.Free;
  FGames.Free;
  FCardFaces.Free;
  FRelatedURIs.Free;
  FPurchaseURIs.Free;
  inherited;
end;

procedure TCardDetails.Clear;
begin
  FSFID := '';
  FOracleID := '';
  FCardName := '';
  FLang := '';
  FReleasedAt := '';
  FLayout := '';
  FArenaID := 0;
  FEDHRank := 0;
  FTypeLine := '';
  FColorIdentity.Clear;
  FManaCost := '';
  FOracleText := '';
  FFlavorText := '';
  FPower := '';
  FToughness := '';
  FLoyalty := '';
  FSetIconURI := '';
  FArtist := '';
  FCollectorNumber := '';
  FBorderColor := '';
  FFrame := '';
  FSecurityStamp := '';
  FKeywords.Clear;
  FAllParts.Clear;
  FIsMeld := False;
  FMeldDetails.Clear;
  FLegalities.Clear;
  FPrintsSearchUri := '';
  FRulingsUri := '';
  FPrices.Clear;
  FImageUris.Clear;
  SetLength(FImageData, 0);
  FSetCode := '';
  FSetName := '';
  FRarity := TRarity.rAll;
  FCMC := 0.0;
  FReserved := False;
  FFoil := False;
  FNonFoil := False;
  FOversized := False;
  FPromo := False;
  FReprint := False;
  FDigital := False;
  FFullArt := False;
  FTextless := False;
  FStorySpotlight := False;
  FGames.Clear;
  FCardFaces.Clear;
  FScryfallURI := '';
  FURI := '';
  FRelatedURIs.Clear;
  FPurchaseURIs.Clear;
  FScryfallCardBackID := '';
  FScryfallID := '';
  FScryfallIllustrationID := '';
  FScryfallOracleID := '';
end;

{ TRarityHelper }
function TRarityHelper.ToString: string;
begin
  case Self of
    rAll:
      Result := '';
    rCommon:
      Result := 'common';
    rUncommon:
      Result := 'uncommon';
    rRare:
      Result := 'rare';
    rMythic:
      Result := 'mythic';
    rSpecial:
      Result := 'special';
    rBonus:
      Result := 'bonus';
    rTimeshifted:
      Result := 'timeshifted';
    rMasterpiece:
      Result := 'masterpiece';
    rToken:
      Result := 'token';
    rDoubleFacedToken:
      Result := 'double_faced_token';
    rDraft:
      Result := 'draft';
    rPlaneshifted:
      Result := 'planeshifted';
    rUnique:
      Result := 'unique';
    rBasic:
      Result := 'basic';
    rPromo:
      Result := 'promo';
  else
    Result := '';
  end;
end;

class function TRarityHelper.FromString(const Value: string): TRarity;
var
  R: TRarity;
begin
  for R := Low(TRarity) to High(TRarity) do
    if SameText(Value, R.ToString) then
      Exit(R);
  Result := rAll;
end;

{ TLegalityFormatHelper }
function TLegalityFormatHelper.ToString: string;
begin
  case Self of
    lfStandard:
      Result := 'standard';
    lfFuture:
      Result := 'future';
    lfHistoric:
      Result := 'historic';
    lfGladiator:
      Result := 'gladiator';
    lfPioneer:
      Result := 'pioneer';
    lfExplorer:
      Result := 'explorer';
    lfModern:
      Result := 'modern';
    lfLegacy:
      Result := 'legacy';
    lfPauper:
      Result := 'pauper';
    lfVintage:
      Result := 'vintage';
    lfPenny:
      Result := 'penny';
    lfCommander:
      Result := 'commander';
    lfAlchemy:
      Result := 'alchemy';
    lfBrawl:
      Result := 'brawl';
    lfPauperCommander:
      Result := 'paupercommander';
    lfDuel:
      Result := 'duel';
    lfOldschool:
      Result := 'oldschool';
    lfPremodern:
      Result := 'premodern';
    lfOathbreaker:
      Result := 'oathbreaker';
  else
    Result := '';
  end;
end;

class function TLegalityFormatHelper.FromString(const Value: string)
  : TLegalityFormat;
var
  L: TLegalityFormat;
begin
  for L := Low(TLegalityFormat) to High(TLegalityFormat) do
    if SameText(Value, L.ToString) then
      Exit(L);
  raise Exception.CreateFmt('Unknown legality format: %s', [Value]);
end;

// Similar implementations for other classes...

end.
