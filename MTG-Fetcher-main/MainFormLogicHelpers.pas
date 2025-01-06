unit MainFormLogicHelpers;

interface

uses
  System.SysUtils, System.Generics.Collections, APIConstants,
  SGlobalsZ, JsonDataObjects, System.Classes, Logger, CardDisplayHelpers,
  WrapperHelper;

function SearchCards(const CardList: TList<TCardDetails>;
  const NameFilter: string; const RarityFilter: TRarity;
  const ColorFilter: string): TList<TCardDetails>;

function CardMatchesFilter(const Card: TCardDetails; const NameFilter: string;
  const RarityFilter: TRarity; const ColorFilter: string): Boolean;

function StringToRarity2(const RarityStr: string): TRarity;
function ColorNameToCode(const ColorName: string): string;

implementation

function SearchCards(const CardList: TList<TCardDetails>;
  const NameFilter: string; const RarityFilter: TRarity;
  const ColorFilter: string): TList<TCardDetails>;
var
  Card: TCardDetails;
begin
  Result := TList<TCardDetails>.Create;
  for Card in CardList do
  begin
    if CardMatchesFilter(Card, NameFilter, RarityFilter, ColorFilter) then
      Result.Add(Card);
  end;
end;

function CardMatchesFilter(const Card: TCardDetails; const NameFilter: string;
  const RarityFilter: TRarity; const ColorFilter: string): Boolean;
begin
  Result := True;

  // Filter by name (case-insensitive, partial match)
  if not NameFilter.IsEmpty then
    Result := Result and Card.CardName.ToLower.Contains(NameFilter.ToLower);

  // Filter by rarity, skip if RarityFilter is rSpecial = “any rarity”
  if (RarityFilter <> rSpecial) then
    Result := Result and (Card.Rarity = RarityFilter);

  // Filter by color, only works for one color so it needs work
  if not ColorFilter.IsEmpty then
    Result := Result and TArray.Contains<string>(Card.ColorIdentity,
      ColorFilter.ToLower);
end;

function StringToRarity2(const RarityStr: string): TRarity;
var
  R: TRarity;
begin
  for R := Low(TRarity) to High(TRarity) do
  begin
    if RarityToString[R] = RarityStr then
      Exit(R);
  end;
  raise Exception.CreateFmt('Unknown rarity: %s', [RarityStr]);
end;

function ColorNameToCode(const ColorName: string): string;
begin
  if ColorName.ToLower = 'white' then
    Result := 'W'
  else if ColorName.ToLower = 'blue' then
    Result := 'U'
  else if ColorName.ToLower = 'black' then
    Result := 'B'
  else if ColorName.ToLower = 'red' then
    Result := 'R'
  else if ColorName.ToLower = 'green' then
    Result := 'G'
  else if ColorName.ToLower = 'colorless' then
    Result := 'C'
  else
    Result := ''; // Return empty for "All Colors" or invalid input
end;

end.
