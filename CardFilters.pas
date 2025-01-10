unit CardFilters;

interface

uses
  System.SysUtils, System.Generics.Collections, ScryfallData, SGlobalsZ;

type
  TCurrencyType = (ctUSD, ctUSDFoil, ctEUR, ctTIX);

  TFormatFilter = record
    FormatName: string; // Must match LegalitiesArray values
    Status: string; // e.g., 'legal', 'not_legal'
  end;

  TPriceFilter = record
    CurrencyType: TCurrencyType;
    MaxPrice: Currency;
    MinPrice: Currency;
    IncludeFoil: Boolean;
    procedure Clear;
  end;

  TCardFilter = record
    // Search parameters
    SearchTerm: string;
    SetCode: string;
    ColorCode: string;
    Rarity: TRarity;
    ShowUnique: Boolean;

    // Card characteristics
    TypeLine: string;
    CMC: Double; // Changed to Double to match TCardDetails

    // Complex filters
    FormatFilter: TFormatFilter;
    PriceFilter: TPriceFilter;

    class function Create: TCardFilter; static;
    class function CreateEmpty: TCardFilter; static;
    procedure Clear;
  end;

  TCardFilterBuilder = class
  private
    FFilter: TCardFilter;
  public
    constructor Create;

    // Basic search parameters
    function WithSearchTerm(const Term: string): TCardFilterBuilder;
    function WithSet(const SetCode: string): TCardFilterBuilder;
    function WithColors(const Colors: string): TCardFilterBuilder;
    function WithRarity(const Rarity: TRarity): TCardFilterBuilder;
    // Updated to use TRarity
    function ShowUniqueOnly(const Value: Boolean): TCardFilterBuilder;

    // Card characteristics
    function WithType(const TypeLine: string): TCardFilterBuilder;
    function WithCMC(const CMC: Double): TCardFilterBuilder;

    // Format filters
    function WithFormat(const FormatName, Status: string): TCardFilterBuilder;

    // Price filters
    function WithPrice(const CurrencyType: TCurrencyType;
      const MaxPrice: Currency): TCardFilterBuilder;
    function WithPriceRange(const CurrencyType: TCurrencyType;
      const MinPrice, MaxPrice: Currency): TCardFilterBuilder;
    function IncludeFoilPrices(const Include: Boolean): TCardFilterBuilder;

    function Build: TCardFilter;
  end;

  // Helper functions
   function FilterCards(const Cards: TArray<TCardDetails>; const Filter: TCardFilter): TArray<TCardDetails>;

   function IsCardMatchingFilter(const Card: TCardDetails; const Filter: TCardFilter): Boolean;

   function GetCardPrice(const Card: TCardDetails; const CurrencyType: TCurrencyType; const IncludeFoil: Boolean): Currency;

   function CurrencyTypeToString(const CurrencyType: TCurrencyType): string;

   function StringToCurrencyType(const CurrencyStr: string): TCurrencyType;

   function IsValidFormat(const FormatName: string): Boolean;

implementation

uses
  System.Math;

{ TPriceFilter }

procedure TPriceFilter.Clear;
begin
  CurrencyType := ctUSD;
  MaxPrice := 0;
  MinPrice := 0;
  IncludeFoil := False;
end;

{ TCardFilter }

class function TCardFilter.Create: TCardFilter;
begin
  Result := CreateEmpty;
  Result.CMC := -1; // -1 indicates no CMC filter
  Result.ShowUnique := False;
  Result.PriceFilter.Clear;
  Result.FormatFilter.FormatName := '';
  Result.FormatFilter.Status := '';
  Result.Rarity := rAll; // Default rarity
end;

class function TCardFilter.CreateEmpty: TCardFilter;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.PriceFilter.CurrencyType := ctUSD;
end;

procedure TCardFilter.Clear;
begin
  SearchTerm := '';
  SetCode := '';
  ColorCode := '';
  Rarity := rAll;
  ShowUnique := False;
  TypeLine := '';
  CMC := -1;
  FormatFilter.FormatName := '';
  FormatFilter.Status := '';
  PriceFilter.Clear;
end;

{ TCardFilterBuilder }

constructor TCardFilterBuilder.Create;
begin
  inherited Create;
  FFilter := TCardFilter.Create;
end;

function TCardFilterBuilder.WithSearchTerm(const Term: string)
  : TCardFilterBuilder;
begin
  FFilter.SearchTerm := Term;
  Result := Self;
end;

function TCardFilterBuilder.WithSet(const SetCode: string): TCardFilterBuilder;
begin
  FFilter.SetCode := SetCode;
  Result := Self;
end;

function TCardFilterBuilder.WithColors(const Colors: string)
  : TCardFilterBuilder;
begin
  FFilter.ColorCode := Colors;
  Result := Self;
end;

function TCardFilterBuilder.WithRarity(const Rarity: TRarity)
  : TCardFilterBuilder;
begin
  FFilter.Rarity := Rarity;
  Result := Self;
end;

function TCardFilterBuilder.ShowUniqueOnly(const Value: Boolean)
  : TCardFilterBuilder;
begin
  FFilter.ShowUnique := Value;
  Result := Self;
end;

function TCardFilterBuilder.WithType(const TypeLine: string)
  : TCardFilterBuilder;
begin
  FFilter.TypeLine := TypeLine;
  Result := Self;
end;

function TCardFilterBuilder.WithCMC(const CMC: Double): TCardFilterBuilder;
begin
  FFilter.CMC := CMC;
  Result := Self;
end;

function TCardFilterBuilder.WithFormat(const FormatName, Status: string)
  : TCardFilterBuilder;
begin
  if IsValidFormat(FormatName) then
  begin
    FFilter.FormatFilter.FormatName := FormatName;
    FFilter.FormatFilter.Status := Status;
  end;
  Result := Self;
end;

function TCardFilterBuilder.WithPrice(const CurrencyType: TCurrencyType;
  const MaxPrice: Currency): TCardFilterBuilder;
begin
  FFilter.PriceFilter.CurrencyType := CurrencyType;
  FFilter.PriceFilter.MaxPrice := MaxPrice;
  FFilter.PriceFilter.MinPrice := 0;
  Result := Self;
end;

function TCardFilterBuilder.WithPriceRange(const CurrencyType: TCurrencyType;
  const MinPrice, MaxPrice: Currency): TCardFilterBuilder;
begin
  FFilter.PriceFilter.CurrencyType := CurrencyType;
  FFilter.PriceFilter.MaxPrice := MaxPrice;
  FFilter.PriceFilter.MinPrice := MinPrice;
  Result := Self;
end;

function TCardFilterBuilder.IncludeFoilPrices(const Include: Boolean)
  : TCardFilterBuilder;
begin
  FFilter.PriceFilter.IncludeFoil := Include;
  Result := Self;
end;

function TCardFilterBuilder.Build: TCardFilter;
begin
  Result := FFilter;
end;

{ Helper Functions }

function IsValidFormat(const FormatName: string): Boolean;
var
  Format: string;
begin
  Result := False;
  for Format in LegalitiesArray do
    if SameText(Format, FormatName) then
    begin
      Result := True;
      Break;
    end;
end;

function CurrencyTypeToString(const CurrencyType: TCurrencyType): string;
const
  CurrencyMap: array[TCurrencyType] of string = ('USD', 'USD_Foil', 'EUR', 'TIX');
begin
  Result := CurrencyMap[CurrencyType];
end;

function StringToCurrencyType(const CurrencyStr: string): TCurrencyType;
var
  CurrencyMap: TDictionary<string, TCurrencyType>;
begin
  CurrencyMap := TDictionary<string, TCurrencyType>.Create;
  try
    CurrencyMap.Add('USD', ctUSD);
    CurrencyMap.Add('USD_Foil', ctUSDFoil);
    CurrencyMap.Add('EUR', ctEUR);
    CurrencyMap.Add('TIX', ctTIX);

    if not CurrencyMap.TryGetValue(CurrencyStr, Result) then
      Result := ctUSD; // Default
  finally
    CurrencyMap.Free;
  end;
end;

function GetCardPrice(const Card: TCardDetails;
  const CurrencyType: TCurrencyType; const IncludeFoil: Boolean): Currency;
begin
  Result := 0;

  case CurrencyType of
    ctUSD:
      begin
        Result := Card.Prices.USD;
        if IncludeFoil and (Card.Prices.USD_Foil > 0) then
          Result := Min(Result, Card.Prices.USD_Foil);
      end;
    ctUSDFoil:
      Result := Card.Prices.USD_Foil;
    ctEUR:
      Result := Card.Prices.EUR;
    ctTIX:
      Result := Card.Prices.Tix;
  end;
end;

function IsCardMatchingFilter(const Card: TCardDetails; const Filter: TCardFilter): Boolean;
var
  CardPrice: Currency;
  FormatStatus: string;
begin
  Result := True;

  // Type line filter
  if (Filter.TypeLine <> '') and not Card.TypeLine.Contains(Filter.TypeLine) then
    Exit(False);

  // CMC filter
  if (Filter.CMC > -1) and (Card.CMC <> Filter.CMC) then
    Exit(False);

  // Format filter
  if (Filter.FormatFilter.FormatName <> '') and (Filter.FormatFilter.Status <> '') then
  begin
    FormatStatus := Card.Legalities.GetStatus(Filter.FormatFilter.FormatName);
    if FormatStatus <> Filter.FormatFilter.Status then
      Exit(False);
  end;

  // Price filter
  if (Filter.PriceFilter.MaxPrice > 0) or (Filter.PriceFilter.MinPrice > 0) then
  begin
    CardPrice := GetCardPrice(Card, Filter.PriceFilter.CurrencyType, Filter.PriceFilter.IncludeFoil);

    if (Filter.PriceFilter.MinPrice > 0) and (CardPrice < Filter.PriceFilter.MinPrice) then
      Exit(False);
    if (Filter.PriceFilter.MaxPrice > 0) and (CardPrice > Filter.PriceFilter.MaxPrice) then
      Exit(False);
  end;

  // Rarity filter
  if (Filter.Rarity <> rAll) and (Card.Rarity <> Filter.Rarity) then
    Exit(False);
end;

function FilterCards(const Cards: TArray<TCardDetails>; const Filter: TCardFilter): TArray<TCardDetails>;
var
  List: TList<TCardDetails>;
  Card: TCardDetails;
begin
  List := TList<TCardDetails>.Create;
  try
    for Card in Cards do
    begin
      if IsCardMatchingFilter(Card, Filter) then
        List.Add(Card);
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

end.
