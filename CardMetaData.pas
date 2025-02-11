unit CardMetaData;

interface

uses
  System.Classes, System.SysUtils, System.Generics.Collections;

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

  public
    procedure Assign(Source: TCardPrices);
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
    procedure Assign(Source: TImageUris);
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
    procedure Assign(Source: TCardLegalities);
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
    procedure Assign(Source: TCardFace);
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

  public
    procedure Assign(Source: TCardPart);
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
    procedure Assign(Source: TMeldDetails);
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

  public
    procedure Assign(Source: TRelatedURIs);
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
    procedure Assign(Source: TPurchaseURIs);
    procedure Clear;
    property Tcgplayer: string read FTcgplayer write FTcgplayer;
    property Cardmarket: string read FCardmarket write FCardmarket;
    property Cardhoarder: string read FCardhoarder write FCardhoarder;
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

{ TMeldDetails }
constructor TMeldDetails.Create;
begin
  FMeldParts := TObjectList<TCardPart>.Create(True);
  FMeldResult := TCardPart.Create;
end;

destructor TMeldDetails.Destroy;
begin
  if Assigned(FMeldParts) then FreeAndNil(FMeldParts);
  if Assigned(FMeldResult) then FreeAndNil(FMeldResult);
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

  // Deep copy MeldResult
  if Assigned(Source.MeldResult) then
  begin
  if not Assigned(FMeldResult) then
    FMeldResult := TCardPart.Create;
  FMeldResult.Assign(Source.MeldResult);
  end
else
  FreeAndNil(FMeldResult);

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
  if Assigned(Source.ImageUris) then
  begin
  FreeAndNil(FImageUris);
  FImageUris := TImageUris.Create;
  FImageUris.Assign(Source.ImageUris);
  end;

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
begin
  FillChar(FStatus, SizeOf(FStatus), 0);
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

procedure TMeldDetails.Clear;
begin
  FMeldParts.Clear;
  FMeldResult.Clear;
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

class function TLegalityFormatHelper.FromString(const Value: string): TLegalityFormat;
var
  L: TLegalityFormat;
begin
  for L := Low(TLegalityFormat) to High(TLegalityFormat) do
    if SameText(Value, L.ToString) then
      Exit(L);
  Result := lfStandard;
end;


end.
