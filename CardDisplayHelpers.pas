unit CardDisplayHelpers;

interface

uses
  System.Generics.Collections, System.SysUtils, SGlobalsZ, System.StrUtils;

procedure AddCoreReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
procedure AddImageReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
procedure AddPricesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
procedure AddBadgesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
procedure AddKeywordsReplacement(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
function BuildPowerToughnessHtml(const CardDetails: TCardDetails): string;
function GetRarityClass(const Rarity: string): string;
function GetStatusClass(const LegalityStatus: string): string;
function FormatLegalityStatus(const LegalityStatus: string): string;

implementation

uses
  System.NetEncoding, MLogic;


 {It took me hours to get this correct..}
procedure AddCoreReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  FacesHtml, ProcessedOracleText: string;

  function ProcessOracleText(const OracleText: string): string;
  var
    Parts: TArray<string>;
    Part: string;
  begin
    Result := ''; // Initialize result

    // Parse the Oracle text into parts (symbols and plain text)
    Parts := ParseTextWithSymbolsManual(OracleText);

    for Part in Parts do
    begin
      if Part.StartsWith('{') and Part.EndsWith('}') then
      begin
        // Handle mana symbols
        Result := Result + ReplaceManaSymbolsWithImages(Part);
      end
      else
      begin
        // Handle plain text with line breaks replaced by <br>'s / this works but ugly
        Result := Result + StringReplace(TNetEncoding.HTML.Encode(Part), #10, '<br><br>', [rfReplaceAll]);
      end;
    end;
  end;

begin
  if Length(CardDetails.CardFaces) = 0 then
    Replacements.AddOrSetValue('{{FlavorText}}',
      TNetEncoding.HTML.Encode(CardDetails.FlavorText));

  Replacements.AddOrSetValue('{{CardName}}', TNetEncoding.HTML.Encode(CardDetails.CardName));
  Replacements.AddOrSetValue('{{SetName}}', TNetEncoding.HTML.Encode(CardDetails.SetName));
  Replacements.AddOrSetValue('{{SetIcon}}', CardDetails.SetIconURI);
  Replacements.AddOrSetValue('{{Rarity}}', TNetEncoding.HTML.Encode(CardDetails.Rarity));
  Replacements.AddOrSetValue('{{RarityClass}}', GetRarityClass(CardDetails.Rarity));
  Replacements.AddOrSetValue('{{TypeLine}}', TNetEncoding.HTML.Encode(CardDetails.TypeLine));
  Replacements.AddOrSetValue('{{ManaCost}}', ReplaceManaSymbolsWithImages(CardDetails.ManaCost));
  Replacements.AddOrSetValue('{{Artist}}', TNetEncoding.HTML.Encode(CardDetails.Artist));
  Replacements.AddOrSetValue('{{CollectorNumber}}', TNetEncoding.HTML.Encode(CardDetails.CollectorNumber));
  Replacements.AddOrSetValue('{{Frame}}', TNetEncoding.HTML.Encode(CardDetails.Frame));
  Replacements.AddOrSetValue('{{BorderColor}}', TNetEncoding.HTML.Encode(CardDetails.BorderColor));
  Replacements.AddOrSetValue('{{ReleasedAt}}', TNetEncoding.HTML.Encode(CardDetails.ReleasedAt));
  Replacements.AddOrSetValue('{{StorySpotlight}}', IfThen(CardDetails.StorySpotlight, 'Yes', 'No'));

  // Handle Oracle Text and Multi-Faced Cards
  if Length(CardDetails.CardFaces) > 0 then
  begin
    FacesHtml := ''; // Initialize HTML for card faces

    for var Face in CardDetails.CardFaces do
    begin
      // Process Oracle Text for the face
      ProcessedOracleText := ProcessOracleText(Face.OracleText);

      FacesHtml := FacesHtml + Format(
        '<div class="card-face-details">' +
        '<p><strong>Face Name:</strong> %s</p>' +
        '<p><strong>Mana Cost:</strong> %s</p>' +
        '<p><strong>Type Line:</strong> %s</p>' +
        '<p><strong>Oracle Text:</strong> %s</p>' +
        '<p><strong>Power/Toughness:</strong> %s/%s</p>' +
        '<p><strong>Flavor Text:</strong> %s</p>' +
        '</div>',
        [
          TNetEncoding.HTML.Encode(Face.Name),
          ReplaceManaSymbolsWithImages(Face.ManaCost),
          TNetEncoding.HTML.Encode(Face.TypeLine),
          ProcessedOracleText,
          TNetEncoding.HTML.Encode(Face.Power),
          TNetEncoding.HTML.Encode(Face.Toughness),
          TNetEncoding.HTML.Encode(Face.FlavorText)
        ]
      );
    end;

    Replacements.AddOrSetValue('{{OracleText}}', FacesHtml); // Replace OracleText with faces
  end
  else
  begin
    // Single-faced card
    ProcessedOracleText := ProcessOracleText(CardDetails.OracleText);
    Replacements.AddOrSetValue('{{OracleText}}', ProcessedOracleText);
  end;

  // Handle Power/Toughness or Loyalty
  Replacements.AddOrSetValue('{{PowerToughness}}', BuildPowerToughnessHtml(CardDetails));
end;

procedure AddImageReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  CardImagesHtml, FlipIndicatorHtml: string;
begin
  FlipIndicatorHtml := ''; // Default empty

  if (CardDetails.Layout.ToLower = 'transform') or
    (CardDetails.Layout.ToLower = 'modal_dfc') or
    (CardDetails.Layout.ToLower = 'reversible_card') then
  begin
    if Length(CardDetails.CardFaces) > 1 then
    begin
      FlipIndicatorHtml :=
        '<div class="flip-indicator">Double-Faced Card: Click to Flip</div>';
      CardImagesHtml := Format('<div class="flip-card" onclick="flipCard()">' +
        '<div class="card-face front"><img src="%s" alt="Front Face"></div>' +
        '<div class="card-face back"><img src="%s" alt="Back Face"></div>' +
        '</div>', [TNetEncoding.HTML.Encode(CardDetails.CardFaces[0]
        .ImageUris.Normal), TNetEncoding.HTML.Encode(CardDetails.CardFaces[1]
        .ImageUris.Normal)]);
    end;
  end
  else
    CardImagesHtml :=
      Format('<div class="single-card"><img src="%s" alt="Card Image"></div>',
      [TNetEncoding.HTML.Encode(CardDetails.ImageUris.Normal)]);

  Replacements.Add('{{CardImages}}', CardImagesHtml);
  Replacements.Add('{{FlipIndicator}}', FlipIndicatorHtml);
end;

procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  LegalitiesHtml, LegalityName, LegalityStatus, StatusClass: string;
  I: Integer;
begin
  LegalitiesHtml := '<div class="legalities-grid">'; // Start the grid container

  for I := Low(LegalitiesArray) to High(LegalitiesArray) do
  begin
    LegalityName := LegalitiesArray[I];
    LegalityStatus := GetLegalStatus(CardDetails.Legalities, LegalityName);

    if not LegalityStatus.IsEmpty then
    begin
      StatusClass := GetStatusClass(LegalityStatus);
      LegalityStatus := FormatLegalityStatus(LegalityStatus);

      // Add a grid item for the format with the status badge
      LegalitiesHtml := LegalitiesHtml +
        Format('<div class="format-name">%s</div>' +
        '<div class="status"><span class="%s">%s</span></div>',
        [TNetEncoding.HTML.Encode(LegalityName), StatusClass,
        TNetEncoding.HTML.Encode(LegalityStatus)]);
    end;
  end;

  LegalitiesHtml := LegalitiesHtml + '</div>'; // Close the grid container
  Replacements.Add('{{Legalities}}', LegalitiesHtml);
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

procedure AddPricesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
begin
  Replacements.Add('{{USD}}', CardDetails.Prices.USD);
  Replacements.Add('{{USD_Foil}}', CardDetails.Prices.USD_Foil);
  Replacements.Add('{{EUR}}', CardDetails.Prices.EUR);
  Replacements.Add('{{Tix}}', CardDetails.Prices.Tix);
end;

procedure AddBadgesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
begin
  if CardDetails.FullArt then
    Replacements.Add('{{FullArt}}',
      '<span class="badge full-art">Full Art</span>')
  else
    Replacements.Add('{{FullArt}}', '');

  if CardDetails.Promo then
    Replacements.Add('{{Promo}}', '<span class="badge promo">Promo</span>')
  else
    Replacements.Add('{{Promo}}', '');

  if CardDetails.Reserved then
    Replacements.Add('{{Reserved}}',
      '<span class="badge reserved">Reserved</span>')
  else
    Replacements.Add('{{Reserved}}', '');

  // if CardDetails.StorySpotlight then
  // Replacements.Add('{{StorySpotlight}}', 'Yes')
  // else
  // Replacements.Add('{{StorySpotlight}}', 'No');
end;

procedure AddKeywordsReplacement(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  KeywordsList: string;
begin
  if Length(CardDetails.Keywords) > 0 then
  begin
    KeywordsList := String.Join(', ', CardDetails.Keywords);
    Replacements.Add('{{Keywords}}', TNetEncoding.HTML.Encode(KeywordsList));
    Replacements.Add('{{KeywordsDisplay}}', ''); // No style, section is visible
  end
  else
  begin
    Replacements.Add('{{Keywords}}', ''); // Empty content
    Replacements.Add('{{KeywordsDisplay}}', 'display:none;'); // Hide section
  end;
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
