unit SGlobalsX;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections, JsonDataObjects,
  APIConstants;

type
  TRarity = (rAll, rCommon, rUncommon, rRare, rMythic, rSpecial, rBonus,
    rTimeshifted, rMasterpiece, rToken, rBasic, rPromo);

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
    procedure Assign(Source: TCardPrices);

  public
    function ToJSON: string;
    procedure FromJSON(const JSONStr: string);
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
    procedure FromJSON(const JSONStr: string);
    function ToJSON: string;
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
    procedure Assign(Source: TCardLegalities);

  public
    function ToJSON: string;
    procedure FromJSON(const JSONStr: string);
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
    function ToJSON: string;
    procedure FromJSON(const JSONStr: string);

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

function RarityFromString(const Value: string): TRarity;
var
  R: TRarity;
begin
  for R := Low(TRarity) to High(TRarity) do
    if SameText(Value, R.ToString) then
      Exit(R);
  Result := rAll; // Default value if no match found
end;

function GetSafeString(JSON: TJsonObject; const FieldName: string): string;
begin
  if JSON.Contains(FieldName) then
    Result := JSON.S[FieldName]
  else
    Result := '';
end;

function GetSafeFloat(JSON: TJsonObject; const FieldName: string): Double;
begin
  if JSON.Contains(FieldName) then
    Result := JSON.F[FieldName]
  else
    Result := 0.0;
end;

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
  FColorIdentity.Capacity := 5;
  FKeywords := TList<string>.Create;
  FKeywords.Capacity := 10;
  FAllParts := TObjectList<TCardPart>.Create(True);
  FAllParts.Capacity := 5;
  FMeldDetails := TMeldDetails.Create;
  FLegalities := TCardLegalities.Create;
  FPrices := TCardPrices.Create;
  FImageUris := TImageUris.Create;
  FGames := TList<string>.Create;
  FGames.Capacity := 3;
  FCardFaces := TObjectList<TCardFace>.Create(True);
  FCardFaces.Capacity := 2;
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
  i: Integer;
  NewPart: TCardPart;
  NewFace: TCardFace;
begin
  if Self = Source then
    Exit;

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

  // Deep copy TList<string> (manual copying)
  ColorIdentity.Clear;
  for var S in Source.ColorIdentity do
    ColorIdentity.Add(S);

  Keywords.Clear;
  for var S in Source.Keywords do
    Keywords.Add(S);

  Games.Clear;
  for var S in Source.Games do
    Games.Add(S);

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

  // Deep copy objects safely
  if Assigned(Source.MeldDetails) then
  begin
    if not Assigned(FMeldDetails) then
      FMeldDetails := TMeldDetails.Create;
    FMeldDetails.Assign(Source.MeldDetails);
  end
  else
    FreeAndNil(FMeldDetails);

  if Assigned(Source.Legalities) then
  begin
    if not Assigned(FLegalities) then
      FLegalities := TCardLegalities.Create;
    FLegalities.Assign(Source.Legalities);
  end
  else
    FreeAndNil(FLegalities);

  if Assigned(Source.Prices) then
  begin
    if not Assigned(FPrices) then
      FPrices := TCardPrices.Create;
    FPrices.Assign(Source.Prices);
  end
  else
    FreeAndNil(FPrices);

  if Assigned(Source.ImageUris) then
  begin
    if not Assigned(FImageUris) then
      FImageUris := TImageUris.Create;
    FImageUris.Assign(Source.ImageUris);
  end
  else
    FreeAndNil(FImageUris);

  if Assigned(Source.RelatedURIs) then
  begin
    if not Assigned(FRelatedURIs) then
      FRelatedURIs := TRelatedURIs.Create;
    FRelatedURIs.Assign(Source.RelatedURIs);
  end
  else
    FreeAndNil(FRelatedURIs);

  if Assigned(Source.PurchaseURIs) then
  begin
    if not Assigned(FPurchaseURIs) then
      FPurchaseURIs := TPurchaseURIs.Create;
    FPurchaseURIs.Assign(Source.PurchaseURIs);
  end
  else
    FreeAndNil(FPurchaseURIs);

  // // Copy ImageData (dynamic array of bytes).
  // ImageData := Copy(Source.ImageData, 0, Length(Source.ImageData));
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
      Result := 'Common';
    rUncommon:
      Result := 'Uncommon';
    rRare:
      Result := 'Rare';
    rMythic:
      Result := 'Mythic';
    rSpecial:
      Result := 'Special';
    rBonus:
      Result := 'Bonus';
    rTimeshifted:
      Result := 'Timeshifted';
    rMasterpiece:
      Result := 'Masterpiece';
    rToken:
      Result := 'Token';
    // rDoubleFacedToken:
    // Result := 'Double_faced_token';
    // rDraft:
    // Result := 'Draft';
    // rPlaneshifted:
    // Result := 'Planeshifted';
    // rUnique:
    // Result := 'Unique';
    rBasic:
      Result := 'Basic';
    rPromo:
      Result := 'Promo';
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

function TImageUris.ToJSON: string;
var
  JSON: TJsonObject;
begin
  JSON := TJsonObject.Create;
  try
    JSON.S[FieldSmall] := Self.Small;
    JSON.S[FieldNormal] := Self.Normal;
    JSON.S[FieldLarge] := Self.Large;
    JSON.S[FieldBackFace] := Self.BackFace;
    JSON.S[FieldPng] := Self.PNG;
    JSON.S[FieldBorderCrop] := Self.Border_crop;
    JSON.S[FieldArtCrop] := Self.Art_crop;
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

function TCardPrices.ToJSON: string;
var
  JSON: TJsonObject;
begin
  JSON := TJsonObject.Create;
  try
    JSON.F[FieldUsd] := Self.USD;
    JSON.F[FieldUsdFoil] := Self.USD_Foil;
    JSON.F[FieldEur] := Self.EUR;
    JSON.F[FieldTix] := Self.Tix;
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

function TCardLegalities.ToJSON: string;
var
  JSON: TJsonObject;
  Format: TLegalityFormat;
begin
  JSON := TJsonObject.Create;
  try
    for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
      JSON.S[Format.ToString] := Self.GetStatus(Format);
    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;

procedure TImageUris.FromJSON(const JSONStr: string);
var
  JSON: TJsonObject;
begin
  JSON := TJsonObject.Parse(JSONStr) as TJsonObject;
  try
    if Assigned(JSON) then
    begin
      Self.Small := GetSafeString(JSON, FieldSmall);
      Self.Normal := GetSafeString(JSON, FieldNormal);
      Self.Large := GetSafeString(JSON, FieldLarge);
      Self.BackFace := GetSafeString(JSON, FieldBackFace);
      Self.PNG := GetSafeString(JSON, FieldPng);
      Self.Border_crop := GetSafeString(JSON, FieldBorderCrop);
      Self.Art_crop := GetSafeString(JSON, FieldArtCrop);
    end;
  finally
    JSON.Free;
  end;
end;

procedure TCardPrices.FromJSON(const JSONStr: string);
var
  JSON: TJsonObject;
begin
  JSON := TJsonObject.Parse(JSONStr) as TJsonObject;
  try
    if Assigned(JSON) then
    begin
      Self.USD := GetSafeFloat(JSON, FieldUsd);
      Self.USD_Foil := GetSafeFloat(JSON, FieldUsdFoil);
      Self.EUR := GetSafeFloat(JSON, FieldEur);
      Self.Tix := GetSafeFloat(JSON, FieldTix);
    end;
  finally
    JSON.Free;
  end;
end;

procedure TCardLegalities.FromJSON(const JSONStr: string);
var
  JSON: TJsonObject;
  Format: TLegalityFormat;
begin
  JSON := TJsonObject.Parse(JSONStr) as TJsonObject;
  try
    if Assigned(JSON) then
    begin
      for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
        if JSON.Contains(Format.ToString) then
          Self.SetStatus(Format, GetSafeString(JSON, Format.ToString));
    end;
  finally
    JSON.Free;
  end;
end;

function TCardDetails.ToJSON: string;
var
  JSON: TJsonObject;
  Arr: TJsonArray;
  S: string;
begin
  JSON := TJsonObject.Create;
  try
    JSON.S[FieldID] := Self.SFID;
    JSON.S[FieldOracleID] := Self.OracleID;
    JSON.S[FieldName] := Self.CardName;
    JSON.S[FieldSet] := Self.SetCode;
    JSON.S[FieldSetName] := Self.SetName;
    JSON.S[FieldRarity] := Self.Rarity.ToString;
    JSON.S[FieldManaCost] := Self.ManaCost;
    JSON.S[FieldTypeLine] := Self.TypeLine;
    JSON.S[FieldOracleText] := Self.OracleText;
    JSON.S[FieldFlavorText] := Self.FlavorText;
    JSON.S[FieldPower] := Self.Power;
    JSON.S[FieldToughness] := Self.Toughness;
    JSON.S[FieldLoyalty] := Self.Loyalty;
    JSON.S[FieldReleasedAt] := Self.ReleasedAt;


    JSON.O[FieldImageUris] := TJsonObject.Parse(Self.ImageUris.ToJSON) as TJsonObject;
    JSON.O[FieldPrices] := TJsonObject.Parse(Self.Prices.ToJSON) as TJsonObject;
    JSON.O[FieldLegalities] := TJsonObject.Parse(Self.Legalities.ToJSON) as TJsonObject;


    Arr := JSON.A[FieldKeywords];
    for S in Self.Keywords do
      Arr.Add(S);

    Arr := JSON.A[FieldGames];
    for S in Self.Games do
      Arr.Add(S);


    Result := JSON.ToJSON;
  finally
    JSON.Free;
  end;
end;


procedure TCardDetails.FromJSON(const JSONStr: string);
var
  JSON: TJsonObject;
  Arr: TJsonArray;
  I: Integer;
begin
  JSON := TJsonObject.Parse(JSONStr) as TJsonObject;
  try
    if Assigned(JSON) then
    begin
      Self.SFID := GetSafeString(JSON, FieldID);
      Self.OracleID := GetSafeString(JSON, FieldOracleID);
      Self.CardName := GetSafeString(JSON, FieldName);
      Self.SetCode := GetSafeString(JSON, FieldSet);
      Self.SetName := GetSafeString(JSON, FieldSetName);
      Self.Rarity := RarityFromString(GetSafeString(JSON, FieldRarity));
      Self.ManaCost := GetSafeString(JSON, FieldManaCost);
      Self.TypeLine := GetSafeString(JSON, FieldTypeLine);
      Self.OracleText := GetSafeString(JSON, FieldOracleText);
      Self.FlavorText := GetSafeString(JSON, FieldFlavorText);
      Self.Power := GetSafeString(JSON, FieldPower);
      Self.Toughness := GetSafeString(JSON, FieldToughness);
      Self.Loyalty := GetSafeString(JSON, FieldLoyalty);
      Self.ReleasedAt := GetSafeString(JSON, FieldReleasedAt);


      if JSON.Contains(FieldImageUris) then
        Self.ImageUris.FromJSON(JSON.O[FieldImageUris].ToJSON);

      if JSON.Contains(FieldPrices) then
        Self.Prices.FromJSON(JSON.O[FieldPrices].ToJSON);

      if JSON.Contains(FieldLegalities) then
        Self.Legalities.FromJSON(JSON.O[FieldLegalities].ToJSON);


      Self.Keywords.Clear;
      if JSON.Contains(FieldKeywords) then
      begin
        Arr := JSON.A[FieldKeywords];
        for I := 0 to Arr.Count - 1 do
          Self.Keywords.Add(Arr.S[I]);
      end;

      Self.Games.Clear;
      if JSON.Contains(FieldGames) then
      begin
        Arr := JSON.A[FieldGames];
        for I := 0 to Arr.Count - 1 do
          Self.Games.Add(Arr.S[I]);
      end;
    end;
  finally
    JSON.Free;
  end;
end;


end.
