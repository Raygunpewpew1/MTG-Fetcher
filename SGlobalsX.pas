unit SGlobalsX;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

type
  TRarity = (rAll, rCommon, rUncommon, rRare, rMythic, rSpecial, rBonus, rTimeshifted, rMasterpiece, rToken, rDoubleFacedToken, rDraft, rPlaneshifted, rUnique, rBasic, rPromo);

  TLegalityFormat = (lfStandard, lfFuture, lfHistoric, lfGladiator, lfPioneer, lfExplorer, lfModern, lfLegacy, lfPauper, lfVintage, lfPenny, lfCommander, lfAlchemy, lfBrawl, lfPauperCommander, lfDuel, lfOldschool, lfPremodern, lfOathbreaker);

  TCardPrices = class
  private
    FUSD: Currency;
    FUSD_Foil: Currency;
    FEUR: Currency;
    FTix: Currency;
    procedure Assign(Source: TCardPrices);
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
    procedure Assign(Source: TImageUris);
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
    FStatus: array[TLegalityFormat] of string;
    procedure Assign(Source: TCardLegalities);
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
    procedure Assign(Source: TCardFace);
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
    property ImageUris: TImageUris read FImageUris write FImageUris;
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
    procedure Assign(Source: TCardPart);
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
    procedure Assign(Source: TMeldDetails);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property MeldParts: TObjectList<TCardPart> read FMeldParts;
    property MeldResult: TCardPart read FMeldResult write FMeldResult;
  end;

  TRelatedURIs = class
  private
    FGatherer: string;
    FTcgplayerInfiniteArticles: string;
    FTcgplayerInfiniteDecks: string;
    FEdhrec: string;
    procedure Assign(Source: TRelatedURIs);
  public
    procedure Clear;
    property Gatherer: string read FGatherer write FGatherer;
    property TcgplayerInfiniteArticles: string read FTcgplayerInfiniteArticles write FTcgplayerInfiniteArticles;
    property TcgplayerInfiniteDecks: string read FTcgplayerInfiniteDecks write FTcgplayerInfiniteDecks;
    property Edhrec: string read FEdhrec write FEdhrec;
  end;

  TPurchaseURIs = class
  private
    FTcgplayer: string;
    FCardmarket: string;
    FCardhoarder: string;
    procedure Assign(Source: TPurchaseURIs);
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
    procedure Assign(Source: TCardDetails);
    constructor CreateFromCard(Source: TCardDetails);

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
    property ColorIdentity: TList<string> read FColorIdentity write FColorIdentity;
    property ManaCost: string read FManaCost write FManaCost;
    property OracleText: string read FOracleText write FOracleText;
    property FlavorText: string read FFlavorText write FFlavorText;
    property Power: string read FPower write FPower;
    property Toughness: string read FToughness write FToughness;
    property Loyalty: string read FLoyalty write FLoyalty;
    property SetIconURI: string read FSetIconURI write FSetIconURI;
    property Artist: string read FArtist write FArtist;
    property CollectorNumber: string read FCollectorNumber write FCollectorNumber;
    property BorderColor: string read FBorderColor write FBorderColor;
    property Frame: string read FFrame write FFrame;
    property SecurityStamp: string read FSecurityStamp write FSecurityStamp;
    property Keywords: TList<string> read FKeywords write FKeywords;
    property AllParts: TObjectList<TCardPart> read FAllParts;
    property IsMeld: Boolean read FIsMeld write FIsMeld;
    property MeldDetails: TMeldDetails read FMeldDetails;
    property Legalities: TCardLegalities read FLegalities write FLegalities;
    property PrintsSearchUri: string read FPrintsSearchUri write FPrintsSearchUri;
    property RulingsUri: string read FRulingsUri write FRulingsUri;
    property Prices: TCardPrices read FPrices write FPrices;
    property ImageUris: TImageUris read FImageUris write FImageUris;
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
    property Games: TList<string> read FGames write FGames;
    property CardFaces: TObjectList<TCardFace> read FCardFaces;
    property ScryfallURI: string read FScryfallURI write FScryfallURI;
    property URI: string read FURI write FURI;
    property RelatedURIs: TRelatedURIs read FRelatedURIs write FRelatedURIs;
    property PurchaseURIs: TPurchaseURIs read FPurchaseURIs write FPurchaseURIs;
    property ScryfallCardBackID: string read FScryfallCardBackID write FScryfallCardBackID;
    property ScryfallID: string read FScryfallID write FScryfallID;
    property ScryfallIllustrationID: string read FScryfallIllustrationID write FScryfallIllustrationID;
    property ScryfallOracleID: string read FScryfallOracleID write FScryfallOracleID;
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
    property Cards: TObjectList<TCardDetails> read FCards write FCards;
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

procedure TCardLegalities.SetStatus(Format: TLegalityFormat; const StatusStr: string);
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

procedure TCardPrices.Assign(Source: TCardPrices);
begin
  if Self = Source then
    Exit;
  USD := Source.USD;
  USD_Foil := Source.USD_Foil;
  EUR := Source.EUR;
  Tix := Source.Tix;
end;

procedure TImageUris.Assign(Source: TImageUris);
begin
  if Self = Source then
    Exit;
  Small := Source.Small;
  Normal := Source.Normal;
  Large := Source.Large;
  BackFace := Source.BackFace;
  PNG := Source.PNG;
  Border_crop := Source.Border_crop;
  Art_crop := Source.Art_crop;
end;

procedure TCardLegalities.Assign(Source: TCardLegalities);
var
  Format: TLegalityFormat;
begin
  for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
    FStatus[Format] := Source.FStatus[Format];
end;

procedure TCardPart.Assign(Source: TCardPart);
begin
  if Self = Source then
    Exit;
  ObjectType := Source.ObjectType;
  ID := Source.ID;
  Component := Source.Component;
  Name := Source.Name;
  TypeLine := Source.TypeLine;
  URI := Source.URI;
end;

procedure TMeldDetails.Assign(Source: TMeldDetails);
var
  i: Integer;
  NewPart: TCardPart;
begin
  if not Assigned(Source) then
    Exit;

  // Ensure FMeldParts exists.
  if not Assigned(FMeldParts) then
    FMeldParts := TObjectList<TCardPart>.Create(True)
  else
    FMeldParts.Clear;

  // If the source has meld parts, copy them.
  if Assigned(Source.MeldParts) then
  begin
    for i := 0 to Source.MeldParts.Count - 1 do
    begin
      NewPart := TCardPart.Create;
      NewPart.Assign(Source.MeldParts[i]);
      FMeldParts.Add(NewPart);
    end;
  end;

  // Deep copy MeldResult (optional).
  if Assigned(Source.MeldResult) then
  begin
    if Assigned(FMeldResult) then
      FMeldResult.Assign(Source.MeldResult)
    else
    begin
      FMeldResult := TCardPart.Create;
      FMeldResult.Assign(Source.MeldResult);
    end;
  end
  else
  begin
    if Assigned(FMeldResult) then
    begin
      FMeldResult.Free;
      FMeldResult := nil;
    end;
  end;
end;

procedure TCardDetails.Assign(Source: TCardDetails);
var
  s: string;
  i: Integer;
  NewPart: TCardPart;
  NewFace: TCardFace;
begin
  if Self = Source then
    Exit;

  // Copy mandatory (or expected) fields.
  SFID := Source.SFID;
  OracleID := Source.OracleID;
  CardName := Source.CardName;
  Lang := Source.Lang;
  ReleasedAt := Source.ReleasedAt;
  Layout := Source.Layout;
  ArenaID := Source.ArenaID;
  EDHRank := Source.EDHRank;
  TypeLine := Source.TypeLine;
  ManaCost := Source.ManaCost;
  OracleText := Source.OracleText;
  FlavorText := Source.FlavorText;
  Power := Source.Power;
  Toughness := Source.Toughness;
  Loyalty := Source.Loyalty;
  SetIconURI := Source.SetIconURI;
  Artist := Source.Artist;
  CollectorNumber := Source.CollectorNumber;
  BorderColor := Source.BorderColor;
  Frame := Source.Frame;
  SecurityStamp := Source.SecurityStamp;
  PrintsSearchUri := Source.PrintsSearchUri;
  RulingsUri := Source.RulingsUri;

  // Deep copy list of strings: ColorIdentity.
  if Assigned(Source.ColorIdentity) then
  begin
    ColorIdentity.Clear;
    for s in Source.ColorIdentity do
      ColorIdentity.Add(s);
  end
  else
    ColorIdentity.Clear;

  // Deep copy Keywords.
  if Assigned(Source.Keywords) then
  begin
    Keywords.Clear;
    for s in Source.Keywords do
      Keywords.Add(s);
  end
  else
    Keywords.Clear;

  // Deep copy AllParts.
  if Assigned(Source.AllParts) then
  begin
    AllParts.Clear;
    for i := 0 to Source.AllParts.Count - 1 do
    begin
      NewPart := TCardPart.Create;
      NewPart.Assign(Source.AllParts[i]);
      AllParts.Add(NewPart);
    end;
  end
  else
    AllParts.Clear;

  IsMeld := Source.IsMeld;

  // Deep copy MeldDetails (optional).
  if Assigned(Source.MeldDetails) then
  begin
    if Assigned(FMeldDetails) then
      FMeldDetails.Assign(Source.MeldDetails)
    else
    begin
      FMeldDetails := TMeldDetails.Create;
      FMeldDetails.Assign(Source.MeldDetails);
    end;
  end
  else
  begin
    if Assigned(FMeldDetails) then
    begin
      FMeldDetails.Free;
      FMeldDetails := nil;
    end;
  end;

  // Deep copy Legalities.
  if Assigned(Source.Legalities) then
  begin
    if Assigned(FLegalities) then
      FLegalities.Assign(Source.Legalities)
    else
    begin
      FLegalities := TCardLegalities.Create;
      FLegalities.Assign(Source.Legalities);
    end;
  end
  else
  begin
    if Assigned(FLegalities) then
    begin
      FLegalities.Free;
      FLegalities := nil;
    end;
  end;

  // Deep copy Prices (optional).
  if Assigned(Source.Prices) then
  begin
    if Assigned(FPrices) then
      FPrices.Assign(Source.Prices)
    else
    begin
      FPrices := TCardPrices.Create;
      FPrices.Assign(Source.Prices);
    end;
  end
  else
  begin
    if Assigned(FPrices) then
    begin
      FPrices.Free;
      FPrices := nil;
    end;
  end;

  // Deep copy ImageUris
  if Assigned(Source.ImageUris) then
  begin
    if Assigned(FImageUris) then
      FImageUris.Assign(Source.ImageUris)
    else
    begin
      FImageUris := TImageUris.Create;
      FImageUris.Assign(Source.ImageUris);
    end;
  end
  else
  begin
    if Assigned(FImageUris) then
    begin
      FImageUris.Free;
      FImageUris := nil;
    end;
  end;

  // Copy ImageData (dynamic array of bytes).
  ImageData := Copy(Source.ImageData, 0, Length(Source.ImageData));

  SetCode := Source.SetCode;
  SetName := Source.SetName;
  FRarity := Source.FRarity;
  CMC := Source.CMC;
  Reserved := Source.Reserved;
  Foil := Source.Foil;
  NonFoil := Source.NonFoil;
  Oversized := Source.Oversized;
  Promo := Source.Promo;
  Reprint := Source.Reprint;
  Digital := Source.Digital;
  FullArt := Source.FullArt;
  Textless := Source.Textless;
  StorySpotlight := Source.StorySpotlight;

  // Deep copy Games.
  if Assigned(Source.Games) then
  begin
    Games.Clear;
    for s in Source.Games do
      Games.Add(s);
  end
  else
    Games.Clear;

  // Deep copy CardFaces
  if Assigned(Source.CardFaces) then
  begin
    CardFaces.Clear;
    for i := 0 to Source.CardFaces.Count - 1 do
    begin
      NewFace := TCardFace.Create;
      NewFace.Assign(Source.CardFaces[i]);
      CardFaces.Add(NewFace);
    end;
  end
  else
    CardFaces.Clear;

  ScryfallURI := Source.ScryfallURI;
  URI := Source.URI;

  // Deep copy RelatedURIs
  if Assigned(Source.RelatedURIs) then
  begin
    if Assigned(FRelatedURIs) then
      FRelatedURIs.Assign(Source.RelatedURIs)
    else
    begin
      FRelatedURIs := TRelatedURIs.Create;
      FRelatedURIs.Assign(Source.RelatedURIs);
    end;
  end
  else
  begin
    if Assigned(FRelatedURIs) then
    begin
      FRelatedURIs.Free;
      FRelatedURIs := nil;
    end;
  end;

  // Deep copy PurchaseURIs
  if Assigned(Source.PurchaseURIs) then
  begin
    if Assigned(FPurchaseURIs) then
      FPurchaseURIs.Assign(Source.PurchaseURIs)
    else
    begin
      FPurchaseURIs := TPurchaseURIs.Create;
      FPurchaseURIs.Assign(Source.PurchaseURIs);
    end;
  end
  else
  begin
    if Assigned(FPurchaseURIs) then
    begin
      FPurchaseURIs.Free;
      FPurchaseURIs := nil;
    end;
  end;

  ScryfallCardBackID := Source.ScryfallCardBackID;
  ScryfallID := Source.ScryfallID;
  ScryfallIllustrationID := Source.ScryfallIllustrationID;
  ScryfallOracleID := Source.ScryfallOracleID;
end;

procedure TRelatedURIs.Assign(Source: TRelatedURIs);
begin
  if Self = Source then
    Exit;
  Gatherer := Source.Gatherer;
  TcgplayerInfiniteArticles := Source.TcgplayerInfiniteArticles;
  TcgplayerInfiniteDecks := Source.TcgplayerInfiniteDecks;
  Edhrec := Source.Edhrec;
end;

procedure TPurchaseURIs.Assign(Source: TPurchaseURIs);
begin
  if Self = Source then
    Exit;
  Tcgplayer := Source.Tcgplayer;
  Cardmarket := Source.Cardmarket;
  Cardhoarder := Source.Cardhoarder;
end;

procedure TCardFace.Assign(Source: TCardFace);
begin
  if Self = Source then
    Exit;
  Name := Source.Name;
  FlavorText := Source.FlavorText;
  ManaCost := Source.ManaCost;
  TypeLine := Source.TypeLine;
  OracleText := Source.OracleText;
  Power := Source.Power;
  Toughness := Source.Toughness;
  Loyalty := Source.Loyalty;
  CMC := Source.CMC;
  // Deep copy ImageUris:
  if Assigned(FImageUris) then
    FImageUris.Assign(Source.ImageUris)
  else
  begin
    FImageUris := TImageUris.Create;
    FImageUris.Assign(Source.ImageUris);
  end;
end;

constructor TCardDetails.CreateFromCard(Source: TCardDetails);
begin
  Create;
  Assign(Source);
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
  raise Exception.CreateFmt('Invalid rarity string: "%s"', [Value]);
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

class function TLegalityFormatHelper.FromString(const Value: string): TLegalityFormat;
var
  L: TLegalityFormat;
begin
  for L := Low(TLegalityFormat) to High(TLegalityFormat) do
    if SameText(Value, L.ToString) then
      Exit(L);
  raise Exception.CreateFmt('Unknown legality format: %s', [Value]);
end;

end.

