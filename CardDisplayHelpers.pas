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
  System.NetEncoding, MLogic, Mana;

function EncodeHTML(const HtmlText: string): string;
begin
  Result := TNetEncoding.HTML.Encode(HtmlText);
end;

function ProcessOracleText(const OracleText: string): string;
var
  Parts: TArray<string>;
  Part: string;
begin
  Result := '';
  Parts := ParseTextWithSymbolsManual(OracleText);

  for Part in Parts do
  begin
    if Part.StartsWith('{') and Part.EndsWith('}') then
      Result := Result + ReplaceManaSymbolsWithImages(Part)
    else
      Result := Result + StringReplace(EncodeHTML(Part), #10, '<br>', [rfReplaceAll]);
  end;
end;

procedure AddReplacement(Replacements: TDictionary<string, string>;
  const Key, Value: string);
begin
  Replacements.AddOrSetValue(Key, Value);
end;

procedure AddMultiFaceOracleText(const CardDetails: TCardDetails;
  Replacements: TDictionary<string, string>);
var
  FacesHtml: string;
  Face: TCardFace;
  ProcessedOracleText: string;
  ExtraHtml: string;
  EncodeTypeLine: string;
begin
  FacesHtml := '<div class="card-faces-grid">'; // Start of grid container

  for Face in CardDetails.CardFaces do
  begin
    ProcessedOracleText := ProcessOracleText(Face.OracleText);
    EncodeTypeLine := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(Face.TypeLine));

    ExtraHtml := '';
    if not Face.Power.Trim.IsEmpty and not Face.Toughness.Trim.IsEmpty then
      ExtraHtml := ExtraHtml + Format('<p><strong>Power/Toughness:</strong> %s/%s</p>',
        [EncodeHTML(Face.Power), EncodeHTML(Face.Toughness)]);

    if not Face.FlavorText.Trim.IsEmpty then
      ExtraHtml := ExtraHtml + Format('<p><strong>Flavor Text:</strong> %s</p>',
        [EncodeHTML(Face.FlavorText)]);

    // Combine all the details into a single face grid item
    FacesHtml := FacesHtml + Format(
      '<div class="card-face-block">' + // Each face block
      '<p><strong>Face Name:</strong> %s</p>' +
      '<p><strong>Mana Cost:</strong> %s</p>' +
      '<p><strong>Type Line:</strong> %s</p>' +
      '<p><strong>Oracle Text:</strong> %s</p>' +
      '%s' + // ExtraHtml (Power/Toughness and/or Flavor Text)
      '</div>',
      [
        EncodeHTML(Face.Name),
        ReplaceManaSymbolsWithImages(Face.ManaCost),
        EncodeHTML(EncodeTypeLine),
        ProcessedOracleText,
        ExtraHtml
      ]
    );
  end;

  FacesHtml := FacesHtml + '</div>'; // End of grid container

  // Add the constructed HTML to replacements
  AddReplacement(Replacements, '{{OracleText}}', FacesHtml);

  // Clear single {{FlavorText}} for multi-faced cards
  AddReplacement(Replacements, '{{FlavorText}}', '');
end;

procedure AddCoreReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  ProcessedOracleText,GamesList: string;
begin
  // For single-faced cards, add flavor text. Otherwise, handled per-face.
  if Length(CardDetails.CardFaces) = 0 then
    AddReplacement(Replacements, '{{FlavorText}}', EncodeHTML(CardDetails.FlavorText))
  else
    AddReplacement(Replacements, '{{FlavorText}}', '');

  // Basic fields
  AddReplacement(Replacements, '{{CardName}}', EncodeHTML(CardDetails.CardName));
  AddReplacement(Replacements, '{{SetName}}', EncodeHTML(CardDetails.SetName));
  AddReplacement(Replacements, '{{SetIcon}}', CardDetails.SetIconURI);
  AddReplacement(Replacements, '{{Rarity}}', EncodeHTML(CardDetails.Rarity));
  AddReplacement(Replacements, '{{RarityClass}}', GetRarityClass(CardDetails.Rarity));
  AddReplacement(Replacements, '{{TypeLine}}', EncodeHTML(CardDetails.TypeLine));
  AddReplacement(Replacements, '{{ManaCost}}', ReplaceManaSymbolsWithImages(CardDetails.ManaCost));
  AddReplacement(Replacements, '{{Artist}}', EncodeHTML(CardDetails.Artist));
  AddReplacement(Replacements, '{{CollectorNumber}}', EncodeHTML(CardDetails.CollectorNumber));
  AddReplacement(Replacements, '{{Frame}}', EncodeHTML(CardDetails.Frame));
  AddReplacement(Replacements, '{{BorderColor}}', EncodeHTML(CardDetails.BorderColor));
  AddReplacement(Replacements, '{{ReleasedAt}}', EncodeHTML(CardDetails.ReleasedAt));
  AddReplacement(Replacements, '{{StorySpotlight}}', IfThen(CardDetails.StorySpotlight, 'Yes', 'No'));

  // Oracle text and faces
  if Length(CardDetails.CardFaces) > 0 then
    AddMultiFaceOracleText(CardDetails, Replacements)
  else
  begin
    ProcessedOracleText := ProcessOracleText(CardDetails.OracleText);
    AddReplacement(Replacements, '{{OracleText}}', ProcessedOracleText);
  end;

  AddReplacement(Replacements, '{{ScryfallURI}}', EncodeHTML(CardDetails.SFID));

  // Power/Toughness/Loyalty
  AddReplacement(Replacements, '{{PowerToughness}}', BuildPowerToughnessHtml(CardDetails));

    if Length(CardDetails.Games) > 0 then
  begin
    GamesList := String.Join(', ', CardDetails.Games);
    AddReplacement(Replacements, '{{Games}}', EncodeHTML(GamesList));
    AddReplacement(Replacements, '{{GamesClass}}', '');
  end
  else
  begin
    // No games
    AddReplacement(Replacements, '{{Games}}', '');
    AddReplacement(Replacements, '{{GamesClass}}', 'hidden');
  end;

  // **Add new card properties here**:
  // Example:
  // AddReplacement(Replacements, '{{NewProperty}}', EncodeHTML(CardDetails.NewProperty));
end;

procedure AddImageReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  CardImagesHtml, FlipIndicatorHtml: string;
  LayoutLower: string;
begin
  FlipIndicatorHtml := '';
  LayoutLower := CardDetails.Layout.ToLower;

  // Handle double-faced or modal cards
  if (LayoutLower = 'transform') or
     (LayoutLower = 'modal_dfc') or
     (LayoutLower = 'reversible_card') then
  begin
    if Length(CardDetails.CardFaces) > 1 then
    begin
      FlipIndicatorHtml := '<div class="flip-indicator">Double-Faced Card: Click to Flip</div>';
      CardImagesHtml := Format(
        '<div class="flip-card" onclick="flipCard()">' +
          '<div class="card-face front"><img src="%s" alt="Front Face"></div>' +
          '<div class="card-face back"><img src="%s" alt="Back Face"></div>' +
        '</div>',
        [EncodeHTML(CardDetails.CardFaces[0].ImageUris.Normal),
         EncodeHTML(CardDetails.CardFaces[1].ImageUris.Normal)]
      );
    end
    else
      // Fallback if only one face (unlikely but safe)
      CardImagesHtml := Format('<div class="single-card"><img src="%s" alt="Card Image"></div>',
        [EncodeHTML(CardDetails.ImageUris.Normal)]);
  end
  else
  begin
    // Single-faced card image
    CardImagesHtml := Format('<div class="single-card"><img src="%s" alt="Card Image"></div>',
      [EncodeHTML(CardDetails.ImageUris.Normal)]);
  end;

  AddReplacement(Replacements, '{{CardImages}}', CardImagesHtml);
  AddReplacement(Replacements, '{{FlipIndicator}}', FlipIndicatorHtml);
end;

procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  LegalitiesHtml: string;
  LegalityName, LegalityStatus, StatusClass: string;
  I: Integer;
begin
  LegalitiesHtml := '<div class="legalities-grid">';

  for I := Low(LegalitiesArray) to High(LegalitiesArray) do
  begin
    LegalityName := LegalitiesArray[I];
    LegalityStatus := GetLegalStatus(CardDetails.Legalities, LegalityName);

    if not LegalityStatus.IsEmpty then
    begin
      StatusClass := GetStatusClass(LegalityStatus);
      LegalityStatus := FormatLegalityStatus(LegalityStatus);

      LegalitiesHtml := LegalitiesHtml +
        Format('<div class="format-name">%s</div>' +
               '<div class="status"><span class="%s">%s</span></div>',
               [EncodeHTML(LegalityName), StatusClass, EncodeHTML(LegalityStatus)]);
    end;
  end;

  LegalitiesHtml := LegalitiesHtml + '</div>';
  AddReplacement(Replacements, '{{Legalities}}', LegalitiesHtml);
end;

function GetStatusClass(const LegalityStatus: string): string;
begin
  case AnsiIndexStr(LegalityStatus.ToLower, ['legal', 'not_legal', 'banned', 'restricted']) of
    0: Result := 'legal';
    1: Result := 'not-legal';
    2: Result := 'banned';
    3: Result := 'restricted';
  else
    Result := 'unknown';
  end;
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
  AddReplacement(Replacements, '{{USD}}', CardDetails.Prices.USD);
  AddReplacement(Replacements, '{{USD_Foil}}', CardDetails.Prices.USD_Foil);
  AddReplacement(Replacements, '{{EUR}}', CardDetails.Prices.EUR);
  AddReplacement(Replacements, '{{Tix}}', CardDetails.Prices.Tix);
end;

procedure AddBadgesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
begin
  AddReplacement(Replacements, '{{FullArt}}',
    IfThen(CardDetails.FullArt, '<span class="badge full-art">Full Art</span>', ''));

  AddReplacement(Replacements, '{{Promo}}',
    IfThen(CardDetails.Promo, '<span class="badge promo">Promo</span>', ''));

  AddReplacement(Replacements, '{{Reserved}}',
    IfThen(CardDetails.Reserved, '<span class="badge reserved">Reserved</span>', ''));
end;

procedure AddKeywordsReplacement(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  KeywordsList: string;
begin
  if Length(CardDetails.Keywords) > 0 then
  begin
    KeywordsList := String.Join(', ', CardDetails.Keywords);
    AddReplacement(Replacements, '{{Keywords}}', EncodeHTML(KeywordsList));
    AddReplacement(Replacements, '{{KeywordsClass}}', '');
  end
  else
  begin
    AddReplacement(Replacements, '{{Keywords}}', '');
    AddReplacement(Replacements, '{{KeywordsClass}}', 'hidden');
  end;
end;

function BuildPowerToughnessHtml(const CardDetails: TCardDetails): string;
begin
  Result := '';
  if (CardDetails.Power <> '') and (CardDetails.Toughness <> '') then
  begin
    Result := Format(
      '<div class="power-toughness">' +
        '<span class="label">Power/Toughness:</span>' +
        '<span class="value">%s/%s</span>' +
      '</div>',
      [EncodeHTML(CardDetails.Power), EncodeHTML(CardDetails.Toughness)]
    );
  end
  else if CardDetails.Loyalty <> '' then
  begin
    Result := Format(
      '<div class="power-toughness">' +
        '<span class="label">Loyalty:</span>' +
        '<span class="value">%s</span>' +
      '</div>',
      [EncodeHTML(CardDetails.Loyalty)]
    );
  end;
end;

function GetRarityClass(const Rarity: string): string;
begin
  case AnsiIndexStr(Rarity.ToLower, ['common', 'uncommon', 'rare', 'mythic']) of
    0: Result := 'common';
    1: Result := 'uncommon';
    2: Result := 'rare';
    3: Result := 'mythic';
  else
    Result := '';
  end;
end;

end.
