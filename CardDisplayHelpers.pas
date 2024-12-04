unit CardDisplayHelpers;

interface

uses
  System.Generics.Collections, System.SysUtils, SGlobalsZ;

procedure AddCoreReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
procedure AddImageReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
procedure AddPricesReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
procedure AddBadgesReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
procedure AddKeywordsReplacement(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
function BuildPowerToughnessHtml(const CardDetails: TCardDetails): string;
function GetRarityClass(const Rarity: string): string;
function GetStatusClass(const LegalityStatus: string): string;
function FormatLegalityStatus(const LegalityStatus: string): string;

implementation

uses
  System.NetEncoding, MLogic; // Include units you need

// Implement all the helper methods here, exactly as they were refactored earlier

procedure AddCoreReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
begin
  Replacements.Add('{{CardName}}', TNetEncoding.HTML.Encode(CardDetails.CardName));
  Replacements.Add('{{FlavorText}}', TNetEncoding.HTML.Encode(CardDetails.FlavorText));
  Replacements.Add('{{SetIcon}}', CardDetails.SetIconURI);
  Replacements.Add('{{SetName}}', TNetEncoding.HTML.Encode(CardDetails.SetName));
  Replacements.Add('{{TypeLine}}', TNetEncoding.HTML.Encode(CardDetails.TypeLine));
  Replacements.Add('{{ManaCost}}', ReplaceManaSymbolsWithImages(CardDetails.ManaCost));
  Replacements.Add('{{OracleText}}', ReplaceManaSymbolsWithImages(CardDetails.OracleText));
  Replacements.Add('{{PowerToughness}}', BuildPowerToughnessHtml(CardDetails));
  Replacements.Add('{{Rarity}}', TNetEncoding.HTML.Encode(CardDetails.Rarity));
  Replacements.Add('{{RarityClass}}', GetRarityClass(CardDetails.Rarity));
  Replacements.Add('{{Artist}}', TNetEncoding.HTML.Encode(CardDetails.Artist));
  Replacements.Add('{{CollectorNumber}}', TNetEncoding.HTML.Encode(CardDetails.CollectorNumber));
  Replacements.Add('{{Frame}}', TNetEncoding.HTML.Encode(CardDetails.Frame));
  Replacements.Add('{{BorderColor}}', TNetEncoding.HTML.Encode(CardDetails.BorderColor));
  Replacements.Add('{{ReleasedAt}}', TNetEncoding.HTML.Encode(CardDetails.ReleasedAt));
end;

 procedure AddImageReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
var
  CardImagesHtml, FlipIndicatorHtml: string;
begin
  FlipIndicatorHtml := ''; // Default empty

  if (CardDetails.Layout.ToLower = 'transform') or (CardDetails.Layout.ToLower = 'modal_dfc') then
  begin
    if Length(CardDetails.CardFaces) > 1 then
    begin
      FlipIndicatorHtml := '<div class="flip-indicator">Double-Faced Card: Click to Flip</div>';
      CardImagesHtml := Format('<div class="flip-card" onclick="flipCard()">' +
        '<div class="card-face front"><img src="%s" alt="Front Face"></div>' +
        '<div class="card-face back"><img src="%s" alt="Back Face"></div>' +
        '</div>', [
        TNetEncoding.HTML.Encode(CardDetails.CardFaces[0].ImageUris.Normal),
        TNetEncoding.HTML.Encode(CardDetails.CardFaces[1].ImageUris.Normal)]);
    end;
  end
  else
    CardImagesHtml := Format('<div class="single-card"><img src="%s" alt="Card Image"></div>',
      [TNetEncoding.HTML.Encode(CardDetails.ImageUris.Normal)]);

  Replacements.Add('{{CardImages}}', CardImagesHtml);
  Replacements.Add('{{FlipIndicator}}', FlipIndicatorHtml);
end;

procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
var
  LegalitiesRows, LegalityName, LegalityStatus, StatusClass: string;
  I: Integer;
begin
  LegalitiesRows := '';
  for I := Low(LegalitiesArray) to High(LegalitiesArray) do
  begin
    LegalityName := LegalitiesArray[I];
    LegalityStatus := GetLegalStatus(CardDetails.Legalities, LegalityName);

    if LegalityStatus <> '' then
    begin
      StatusClass := GetStatusClass(LegalityStatus);
      LegalityStatus := FormatLegalityStatus(LegalityStatus);
      LegalitiesRows := LegalitiesRows +
        Format('<tr><td class="format-name">%s</td>' +
        '<td class="status"><span class="%s">%s</span></td></tr>',
        [TNetEncoding.HTML.Encode(LegalityName), StatusClass,
        TNetEncoding.HTML.Encode(LegalityStatus)]);
    end;
  end;

  Replacements.Add('{{Legalities}}', LegalitiesRows);
end;

function GetStatusClass(const LegalityStatus: string): string;
begin
  if LegalityStatus.ToLower = 'legal' then
    Exit('legal')
  else if LegalityStatus.ToLower = 'not_legal' then
    Exit('not-legal')
  else if LegalityStatus.ToLower = 'banned' then
    Exit('banned')
  else if LegalityStatus.ToLower = 'restricted' then
    Exit('restricted');
  Exit('unknown');
end;

function FormatLegalityStatus(const LegalityStatus: string): string;
begin
  if LegalityStatus.ToLower = 'not_legal' then
    Exit('Not Legal')
  else if LegalityStatus.ToLower = 'banned' then
    Exit('Banned')
  else if LegalityStatus.ToLower = 'restricted' then
    Exit('Restricted')
  else if LegalityStatus.ToLower = 'legal' then
    Exit('Legal');
  Result := LegalityStatus;
end;


procedure AddPricesReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
begin
  Replacements.Add('{{USD}}', CardDetails.Prices.USD);
  Replacements.Add('{{USD_Foil}}', CardDetails.Prices.USD_Foil);
  Replacements.Add('{{EUR}}', CardDetails.Prices.EUR);
  Replacements.Add('{{Tix}}', CardDetails.Prices.Tix);
end;

procedure AddBadgesReplacements(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
begin
  if CardDetails.FullArt then
    Replacements.Add('{{FullArt}}', '<span class="badge full-art">Full Art</span>')
  else
    Replacements.Add('{{FullArt}}', '');

  if CardDetails.Promo then
    Replacements.Add('{{Promo}}', '<span class="badge promo">Promo</span>')
  else
    Replacements.Add('{{Promo}}', '');

  if CardDetails.Reserved then
    Replacements.Add('{{Reserved}}', '<span class="badge reserved">Reserved</span>')
  else
    Replacements.Add('{{Reserved}}', '');

   if CardDetails.StorySpotlight then
    Replacements.Add('{{StorySpotlight}}', 'Yes')
   else
    Replacements.Add('{{StorySpotlight}}', 'No');
    end;

procedure AddKeywordsReplacement(Replacements: TDictionary<string, string>; const CardDetails: TCardDetails);
var
  KeywordsList: string;
begin
  KeywordsList := String.Join(', ', CardDetails.Keywords);
  Replacements.Add('{{Keywords}}', TNetEncoding.HTML.Encode(KeywordsList));
end;

function BuildPowerToughnessHtml(const CardDetails: TCardDetails): string;
begin
  Result := ''; // Default to empty string

  if (CardDetails.Power <> '') and (CardDetails.Toughness <> '') then
  begin
    Result := Format('<p><strong>Power/Toughness:</strong> %s/%s</p>',
      [TNetEncoding.HTML.Encode(CardDetails.Power),
      TNetEncoding.HTML.Encode(CardDetails.Toughness)]);
  end
  else if CardDetails.Loyalty <> '' then
  begin
    Result := Format('<p><strong>Loyalty:</strong> %s</p>',
      [TNetEncoding.HTML.Encode(CardDetails.Loyalty)]);
  end;
end;

function GetRarityClass(const Rarity: string): string;
begin
  if Rarity.ToLower = 'common' then
    Result := 'common'
  else if Rarity.ToLower = 'uncommon' then
    Result := 'uncommon'
  else if Rarity.ToLower = 'rare' then
    Result := 'rare'
  else if Rarity.ToLower = 'mythic' then
    Result := 'mythic'
  else
    Result := ''; // Default class if no match
end;

end.
