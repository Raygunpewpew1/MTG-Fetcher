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
function GetRarityClass(Rarity: TRarity): string;
function GetStatusClass(const LegalityStatus: string): string;
function FormatLegalityStatus(const LegalityStatus: string): string;
function StringToRarity(const RarityStr: string): TRarity;
function IsInArray(const Value: string; const Candidates: array of string): Boolean;

implementation

uses
  System.NetEncoding, MLogic, Mana, System.Classes;

function EncodeHTML(const HtmlText: string): string;
begin
  Result := TNetEncoding.HTML.Encode(HtmlText);
end;


procedure AddOrHide(Replacements: TDictionary<string, string>;
  const Key, HideKey, Value: string);
begin
  if Value.Trim.IsEmpty then
  begin
    Replacements.AddOrSetValue(Key, '');
    Replacements.AddOrSetValue(HideKey, 'hidden');
  end
  else
  begin
    Replacements.AddOrSetValue(Key, EncodeHTML(Value));
    Replacements.AddOrSetValue(HideKey, '');
  end;
end;

function ProcessOracleText(const OracleText: string): string;
var
  Parts: TArray<string>;
  Part: string;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Parts := ParseTextWithSymbolsManual(OracleText);
    for Part in Parts do
    begin
      if Part.StartsWith('{') and Part.EndsWith('}') then
        Builder.Append(ReplaceManaSymbolsWithImages(Part))
      else
        Builder.Append(StringReplace(EncodeHTML(Part), #10, '<br>', [rfReplaceAll]));
    end;
    Result := Builder.ToString;
  finally
    Builder.Free;
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
  Face: TCardFace;
  ExtraHtml: string;
  EncodeTypeLine: string;
  FaceBuilder: TStringBuilder;
begin
  FaceBuilder := TStringBuilder.Create;
  try
    FaceBuilder.Append('<div class="card-faces-grid">'); // Start grid container

    for Face in CardDetails.CardFaces do
    begin
      // Preprocess text, type line, etc.
      EncodeTypeLine := TEncoding.UTF8.GetString(
        TEncoding.ANSI.GetBytes(Face.TypeLine)
      );

      // Build up "extra" lines: Power/Toughness, Flavor
      ExtraHtml := '';
      if not Face.Power.Trim.IsEmpty and not Face.Toughness.Trim.IsEmpty then
        ExtraHtml := ExtraHtml + Format(
          '<p><strong>Power/Toughness:</strong> %s/%s</p>',
          [EncodeHTML(Face.Power), EncodeHTML(Face.Toughness)]
        );

      if not Face.FlavorText.Trim.IsEmpty then
        ExtraHtml := ExtraHtml + Format(
          '<p><strong>Flavor Text:</strong> %s</p>',
          [EncodeHTML(Face.FlavorText)]
        );

      // Combine all the details into a single face block
      FaceBuilder.AppendFormat(
        '<div class="card-face-block">' + // Face block
          '<p><strong>Face Name:</strong> %s</p>' +
          '<p><strong>Mana Cost:</strong> %s</p>' +
          '<p><strong>Type Line:</strong> %s</p>' +
          '<p><strong>Oracle Text:</strong> %s</p>' +
          '%s' + // ExtraHtml
        '</div>',
        [
          EncodeHTML(Face.Name),
          ReplaceManaSymbolsWithImages(Face.ManaCost),
          EncodeHTML(EncodeTypeLine),
          ProcessOracleText(Face.OracleText),
          ExtraHtml
        ]
      );
    end;

    FaceBuilder.Append('</div>'); // End grid container

    // Add constructed HTML to replacements
    AddReplacement(Replacements, '{{OracleText}}', FaceBuilder.ToString);

    // Clear single {{FlavorText}} for multi-faced cards
    AddReplacement(Replacements, '{{FlavorText}}', '');
  finally
    FaceBuilder.Free;
  end;
end;

procedure AddCoreReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  ProcessedOracleText, GamesList: string;
  RarityStr: string;
begin
  // For single-faced cards, add flavor text. Otherwise, let AddMultiFaceOracleText handle it.
  if Length(CardDetails.CardFaces) = 0 then
    AddReplacement(Replacements, '{{FlavorText}}', EncodeHTML(CardDetails.FlavorText))
  else
    AddReplacement(Replacements, '{{FlavorText}}', '');

  // Basic fields
  AddReplacement(Replacements, '{{CardName}}', EncodeHTML(CardDetails.CardName));
  AddReplacement(Replacements, '{{SetName}}', EncodeHTML(CardDetails.SetName));
  AddReplacement(Replacements, '{{SetIcon}}', CardDetails.SetIconURI);

  RarityStr := RarityToString[CardDetails.Rarity];
  AddReplacement(Replacements, '{{Rarity}}', EncodeHTML(RarityStr));
  AddReplacement(Replacements, '{{RarityClass}}', GetRarityClass(CardDetails.Rarity));
  AddReplacement(Replacements, '{{TypeLine}}', EncodeHTML(CardDetails.TypeLine));
  AddReplacement(Replacements, '{{ManaCost}}',
    ReplaceManaSymbolsWithImages(CardDetails.ManaCost));
  AddReplacement(Replacements, '{{Artist}}', EncodeHTML(CardDetails.Artist));
  AddReplacement(Replacements, '{{CollectorNumber}}',
    EncodeHTML(CardDetails.CollectorNumber));
  AddReplacement(Replacements, '{{Arena Id}}', EncodeHTML(CardDetails.ArenaID.ToString));
  AddReplacement(Replacements, '{{BorderColor}}',
    EncodeHTML(CardDetails.BorderColor));
  AddReplacement(Replacements, '{{ReleasedAt}}', EncodeHTML(CardDetails.ReleasedAt));
  AddReplacement(Replacements, '{{StorySpotlight}}',
    IfThen(CardDetails.StorySpotlight, 'Yes', 'No'));

  // Oracle text and faces
  if Length(CardDetails.CardFaces) > 0 then
    AddMultiFaceOracleText(CardDetails, Replacements)
  else
  begin
    ProcessedOracleText := ProcessOracleText(CardDetails.OracleText);
    AddReplacement(Replacements, '{{OracleText}}', ProcessedOracleText);
  end;

  // SFID -> Scryfall URI placeholder
  AddReplacement(Replacements, '{{ScryfallURI}}', EncodeHTML(CardDetails.SFID));

  // Power/Toughness/Loyalty
  AddReplacement(Replacements, '{{PowerToughness}}',
    BuildPowerToughnessHtml(CardDetails));

  // Games
    GamesList := String.Join(', ', CardDetails.Games);
    AddOrHide(Replacements, '{{Games}}', '{{GamesClass}}', GamesList);

  // **Add new card properties here**:
  // e.g.: AddReplacement(Replacements, '{{NewProperty}}',
  //         EncodeHTML(CardDetails.NewProperty));
end;

procedure AddImageReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  CardImagesHtml, FlipIndicatorHtml: string;
  LayoutLower: string;
begin
  LayoutLower := CardDetails.Layout.ToLower;
  FlipIndicatorHtml := '';

  // Detect double-faced or modal cards
if IsInArray(LayoutLower, ['transform', 'modal_dfc', 'reversible_card',
                          'art_series', 'double_faced_token']) then
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
    begin
      // Fallback if only one face
      CardImagesHtml := Format(
        '<div class="single-card"><img src="%s" alt="Card Image"></div>',
        [EncodeHTML(CardDetails.ImageUris.Normal)]
      );
    end;
  end
  else
  begin
    // Single-faced card image
    CardImagesHtml := Format(
      '<div class="single-card"><img src="%s" alt="Card Image"></div>',
      [EncodeHTML(CardDetails.ImageUris.Normal)]
    );
  end;

  AddReplacement(Replacements, '{{CardImages}}', CardImagesHtml);
  AddReplacement(Replacements, '{{FlipIndicator}}', FlipIndicatorHtml);
end;

function CapitalizeFirstLetter(const Input: string): string;
begin
  if Input.IsEmpty then
    Exit(Input);
  Result := UpperCase(Input[1]) + Input.Substring(1);
end;

procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  I: Integer;
  LegalitiesHtml: TStringBuilder;
  LegalityName, LegalityStatus, StatusClass: string;
begin
  LegalitiesHtml := TStringBuilder.Create;
  try
    LegalitiesHtml.Append('<div class="legalities-grid">');

    for I := Low(LegalitiesArray) to High(LegalitiesArray) do
    begin
      LegalityName := LegalitiesArray[I];
      LegalityStatus := GetLegalStatus(CardDetails.Legalities, LegalityName);

      if not LegalityStatus.IsEmpty then
      begin
        StatusClass := GetStatusClass(LegalityStatus);
        LegalityStatus := FormatLegalityStatus(LegalityStatus);

        LegalitiesHtml.AppendFormat(
          '<div class="format-name">%s</div>' +
          '<div class="status"><span class="%s">%s</span></div>',
          [
            EncodeHTML(CapitalizeFirstLetter(LegalityName)),
            StatusClass,
            EncodeHTML(LegalityStatus)
          ]
        );
      end;
    end;

    LegalitiesHtml.Append('</div>');
    AddReplacement(Replacements, '{{Legalities}}', LegalitiesHtml.ToString);
  finally
    LegalitiesHtml.Free;
  end;
end;

function StringToRarity(const RarityStr: string): TRarity;
begin
  if RarityStr = 'common' then Result := rCommon
  else if RarityStr = 'uncommon' then Result := rUncommon
  else if RarityStr = 'rare' then Result := rRare
  else if RarityStr = 'mythic' then Result := rMythic
  else if RarityStr = 'special' then Result := rSpecial
  else if RarityStr = 'bonus' then Result := rBonus
  else if RarityStr = 'timeshifted' then Result := rTimeshifted
  else if RarityStr = 'masterpiece' then Result := rMasterpiece
  else if RarityStr = 'token' then Result := rToken
  else if RarityStr = 'double_faced_token' then Result := rDoubleFacedToken
  else if RarityStr = 'draft' then Result := rDraft
  else if RarityStr = 'planeshifted' then Result := rPlaneshifted
  else if RarityStr = 'unique' then Result := rUnique
  else if RarityStr = 'basic' then Result := rBasic
  else if RarityStr = 'promo' then Result := rPromo
  else
    raise Exception.Create('Unknown rarity: ' + RarityStr);
end;

function GetStatusClass(const LegalityStatus: string): string;
begin
  case AnsiIndexStr(LegalityStatus.ToLower,
    ['legal','not_legal','banned','restricted']) of
    0: Result := 'legal';
    1: Result := 'not-legal';
    2: Result := 'banned';
    3: Result := 'restricted';
  else
    Result := 'unknown';
  end;
end;

function FormatLegalityStatus(const LegalityStatus: string): string;
var
  LoweredStatus: string;
begin
  LoweredStatus := LegalityStatus.ToLower;

  if LoweredStatus = 'not_legal' then
    Result := 'Not Legal'
  else if LoweredStatus = 'banned' then
    Result := 'Banned'
  else if LoweredStatus = 'restricted' then
    Result := 'Restricted'
  else if LoweredStatus = 'legal' then
    Result := 'Legal'
  else
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
  AddReplacement(
    Replacements,
    '{{FullArt}}',
    IfThen(CardDetails.FullArt, '<span class="badge full-art">Full Art</span>', '')
  );

  AddReplacement(
    Replacements,
    '{{Promo}}',
    IfThen(CardDetails.Promo, '<span class="badge promo">Promo</span>', '')
  );

  AddReplacement(
    Replacements,
    '{{Reserved}}',
    IfThen(CardDetails.Reserved, '<span class="badge reserved">Reserved</span>', '')
  );
end;

procedure AddKeywordsReplacement(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  KeywordsList: string;
begin

  KeywordsList := String.Join(', ', CardDetails.Keywords);


  AddOrHide(Replacements, '{{Keywords}}', '{{KeywordsClass}}', KeywordsList);
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

function IsInArray(const Value: string; const Candidates: array of string): Boolean;
var
  Candidate: string;
begin
  for Candidate in Candidates do
    if Value = Candidate then
      Exit(True);
  Result := False;
end;

function GetRarityClass(Rarity: TRarity): string;
begin
  Result := RarityToString[Rarity];
end;





end.
