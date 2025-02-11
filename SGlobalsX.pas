unit SGlobalsX;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, CardMetaData;

type
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
    property ColorIdentity: TList<string> read FColorIdentity
      write FColorIdentity;
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
    property Keywords: TList<string> read FKeywords write FKeywords;
    property AllParts: TObjectList<TCardPart> read FAllParts;
    property IsMeld: Boolean read FIsMeld write FIsMeld;
    property MeldDetails: TMeldDetails read FMeldDetails;
    property Legalities: TCardLegalities read FLegalities write FLegalities;
    property PrintsSearchUri: string read FPrintsSearchUri
      write FPrintsSearchUri;
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
    property Cards: TObjectList<TCardDetails> read FCards write FCards;
    property HasMore: Boolean read FHasMore write FHasMore;
    property NextPageURL: string read FNextPageURL write FNextPageURL;
    property TotalCards: Integer read FTotalCards write FTotalCards;
  end;

implementation

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

{ TCardDetails }
constructor TCardDetails.Create;
begin
  inherited;
  if not Assigned(FColorIdentity) then
    FColorIdentity := TList<string>.Create;
  FColorIdentity.Capacity := 3;

  if not Assigned(FKeywords) then
    FKeywords := TList<string>.Create;
  FKeywords.Capacity := 5;

  if not Assigned(FAllParts) then
    FAllParts := TObjectList<TCardPart>.Create(True);
  FAllParts.Capacity := 5;

  if not Assigned(FMeldDetails) then
    FMeldDetails := TMeldDetails.Create;
  if not Assigned(FLegalities) then
    FLegalities := TCardLegalities.Create;
  if not Assigned(FPrices) then
    FPrices := TCardPrices.Create;
  if not Assigned(FImageUris) then
    FImageUris := TImageUris.Create;

  if not Assigned(FGames) then
    FGames := TList<string>.Create;
  FGames.Capacity := 3;

  if not Assigned(FCardFaces) then
    FCardFaces := TObjectList<TCardFace>.Create(True);
  FCardFaces.Capacity := 2;

  if not Assigned(FRelatedURIs) then
    FRelatedURIs := TRelatedURIs.Create;
  if not Assigned(FPurchaseURIs) then
    FPurchaseURIs := TPurchaseURIs.Create;
end;


destructor TCardDetails.Destroy;
begin
  if Assigned(FColorIdentity) then FreeAndNil(FColorIdentity);
  if Assigned(FKeywords) then FreeAndNil(FKeywords);
  if Assigned(FAllParts) then FreeAndNil(FAllParts);
  if Assigned(FGames) then FreeAndNil(FGames);
  if Assigned(FCardFaces) then FreeAndNil(FCardFaces);
  if Assigned(FMeldDetails) then FreeAndNil(FMeldDetails);
  if Assigned(FLegalities) then FreeAndNil(FLegalities);
  if Assigned(FPrices) then FreeAndNil(FPrices);
  if Assigned(FImageUris) then FreeAndNil(FImageUris);
  if Assigned(FRelatedURIs) then FreeAndNil(FRelatedURIs);
  if Assigned(FPurchaseURIs) then FreeAndNil(FPurchaseURIs);
  inherited;
end;



procedure TCardDetails.Assign(Source: TCardDetails);
var
  i: Integer;
  NewPart: TCardPart;
  NewFace: TCardFace;
begin
  if Self = Source then
    Exit;


   // Free old objects
  FreeAndNil(FMeldDetails);
  FreeAndNil(FLegalities);
  FreeAndNil(FPrices);
  FreeAndNil(FImageUris);
  FreeAndNil(FRelatedURIs);
  FreeAndNil(FPurchaseURIs);

  // Copy primitive fields
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
  ScryfallURI := Source.ScryfallURI;
  URI := Source.URI;
  ScryfallCardBackID := Source.ScryfallCardBackID;
  ScryfallID := Source.ScryfallID;
  ScryfallIllustrationID := Source.ScryfallIllustrationID;
  ScryfallOracleID := Source.ScryfallOracleID;
  IsMeld := Source.IsMeld;

  // Deep copy TList<string>
  ColorIdentity.Clear;
  for var s in Source.ColorIdentity do
    ColorIdentity.Add(s);

  Keywords.Clear;
  for var s in Source.Keywords do
    Keywords.Add(s);

  Games.Clear;
  for var s in Source.Games do
    Games.Add(s);

  // Deep copy AllParts
  AllParts.Clear;
  for i := 0 to Source.AllParts.Count - 1 do
  begin
    NewPart := TCardPart.Create;
    NewPart.Assign(Source.AllParts[i]);
    AllParts.Add(NewPart);
  end;

  // Deep copy CardFaces
  CardFaces.Clear;
  for i := 0 to Source.CardFaces.Count - 1 do
  begin
    NewFace := TCardFace.Create;
    NewFace.Assign(Source.CardFaces[i]);
    CardFaces.Add(NewFace);
  end;


  // Create new instances
  FMeldDetails := TMeldDetails.Create;
  FLegalities := TCardLegalities.Create;
  FPrices := TCardPrices.Create;
  FImageUris := TImageUris.Create;
  FRelatedURIs := TRelatedURIs.Create;
  FPurchaseURIs := TPurchaseURIs.Create;

  // Assign values
  if Assigned(Source.MeldDetails) then
    FMeldDetails.Assign(Source.MeldDetails);
  if Assigned(Source.Legalities) then
    FLegalities.Assign(Source.Legalities);
  if Assigned(Source.Prices) then
    FPrices.Assign(Source.Prices);
  if Assigned(Source.ImageUris) then
    FImageUris.Assign(Source.ImageUris);
  if Assigned(Source.RelatedURIs) then
    FRelatedURIs.Assign(Source.RelatedURIs);
  if Assigned(Source.PurchaseURIs) then
    FPurchaseURIs.Assign(Source.PurchaseURIs);

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
  FPrintsSearchUri := '';
  FRulingsUri := '';
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
  FScryfallURI := '';
  FURI := '';
  FScryfallCardBackID := '';
  FScryfallID := '';
  FScryfallIllustrationID := '';
  FScryfallOracleID := '';
  if Assigned(FColorIdentity) then FColorIdentity.Clear;
  if Assigned(FKeywords) then FKeywords.Clear;
  if Assigned(FAllParts) then FAllParts.Clear;
  if Assigned(FGames) then FGames.Clear;
  if Assigned(FCardFaces) then FCardFaces.Clear;

  if Assigned(FMeldDetails) then FMeldDetails.Clear;
  if Assigned(FLegalities) then FLegalities.Clear;
  if Assigned(FPrices) then FPrices.Clear;
  if Assigned(FImageUris) then FImageUris.Clear;
  if Assigned(FRelatedURIs) then FRelatedURIs.Clear;
  if Assigned(FPurchaseURIs) then FPurchaseURIs.Clear;
  FIsMeld := False;
end;

end.
