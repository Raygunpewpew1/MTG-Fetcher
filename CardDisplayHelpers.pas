unit CardDisplayHelpers;

interface

uses
  System.Generics.Collections, System.SysUtils, SGlobalsX, System.StrUtils,
  ScryfallData;

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

procedure AddMeldDetails(const CardDetails: TCardDetails;
  Replacements: TDictionary<string, string>);

function BuildPowerToughnessHtml(const CardDetails: TCardDetails): string;

function GetRarityClass(Rarity: TRarity): string;

function GetStatusClass(const LegalityStatus: string): string;

function FormatLegalityStatus(const LegalityStatus: string): string;

function IsInArray(const Value: string;
  const Candidates: array of string): Boolean;

function EncodeHTML(const HtmlText: string): string;

implementation

uses
  System.NetEncoding, MLogic, Mana, System.Classes, JsonDataObjects,
  Logger;

/// <summary>
/// Simple HTML-encodes the given text.
/// </summary>
function EncodeHTML(const HtmlText: string): string;
begin
  Result := TNetEncoding.HTML.Encode(HtmlText);
end;

/// <summary>
/// Adds a (Key=Value) pair to the replacements dictionary, but if the Value
/// is empty, also sets a corresponding "hidden" class.
/// </summary>
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

/// <summary>
/// Simple helper to store (Key, Value) in the dictionary, ensuring Value
/// is properly placed or replaced.
/// </summary>
procedure AddReplacement(Replacements: TDictionary<string, string>;
  const Key, Value: string);
begin
  Replacements.AddOrSetValue(Key, Value);
end;

/// <summary>
/// Returns a string with only the first letter capitalized (if present).
/// </summary>
function CapitalizeFirstLetter(const Input: string): string;
begin
  if Input.IsEmpty then
    Exit(Input);
  Result := UpperCase(Input[1]) + Input.Substring(1);
end;

/// <summary>
/// Checks if the given Value is in the specified array of string Candidates.
/// </summary>
function IsInArray(const Value: string;
  const Candidates: array of string): Boolean;
var
  Candidate: string;
begin
  for Candidate in Candidates do
    if Value = Candidate then
      Exit(True);
  Result := False;
end;

/// <summary>
/// Maps a TRarity value to a simple CSS class string (e.g., "common", "rare").
/// </summary>
function GetRarityClass(Rarity: TRarity): string;
begin
  Result := Rarity.ToString;
  if Result = '' then
    Result := 'unknown';
end;

/// <summary>
/// Converts the raw LegalityStatus into a standard CSS class name.
/// </summary>
function GetStatusClass(const LegalityStatus: string): string;
begin
  case AnsiIndexStr(LegalityStatus.ToLower, ['legal', 'not_legal', 'banned',
    'restricted']) of
    0:
      Result := 'legal';
    1:
      Result := 'not-legal';
    2:
      Result := 'banned';
    3:
      Result := 'restricted';
  else
    Result := 'unknown';
  end;
end;

/// <summary>
/// Translates Scryfall's legality strings into friendlier display strings.
/// </summary>
function FormatLegalityStatus(const LegalityStatus: string): string;
var
  L: string;
begin
  L := LegalityStatus.ToLower;
  if L = 'not_legal' then
    Result := 'Not Legal'
  else if L = 'banned' then
    Result := 'Banned'
  else if L = 'restricted' then
    Result := 'Restricted'
  else if L = 'legal' then
    Result := 'Legal'
  else
    Result := LegalityStatus;
end;

/// <summary>
/// Takes Oracle text with mana symbols in braces (e.g., {W}, {U}, etc.)
/// parses them, and returns HTML that includes images for each symbol.
/// </summary>
function ProcessOracleText(const OracleText: string): string;
begin
  Result := StringReplace(OracleText, #10, '<br>', [rfReplaceAll]);
  // Faster replacement
  Result := ReplaceManaSymbolsWithImages(Result); // Replace all mana symbols
end;

/// <summary>
/// Renders the multi-face Oracle text and relevant data (type line, flavor, P/T)
/// into a grid-like HTML block. Used by AddCoreReplacements if card has multiple faces.
/// </summary>
procedure AddMultiFaceOracleText(const CardDetails: TCardDetails;
  Replacements: TDictionary<string, string>);
var
  Face: TCardFace;
  Builder: TStringBuilder;
  EncodedTypeLine, EncodedOracleText, EncodedFlavorText, PowerToughness,
    FaceImage: string;
  // IsTransform: Boolean;
begin
  Builder := TStringBuilder.Create;
  try
    Builder.Append('<div class="card-details multi-face-card">');
    // Start the multi-face card container

    // IsTransform := IsInArray(CardDetails.Layout.ToLower, ['transform', 'modal_dfc', 'reversible_card']);

    for Face in CardDetails.CardFaces do
    begin
      EncodedTypeLine := EncodeHTML(Face.TypeLine);
      EncodedOracleText := ProcessOracleText(Face.OracleText);
      EncodedFlavorText := EncodeHTML(Face.FlavorText);

      // Build Power/Toughness or Loyalty
      if not Face.Power.IsEmpty and not Face.Toughness.IsEmpty then
        PowerToughness := Format('<div class="power-toughness">%s/%s</div>',
          [EncodeHTML(Face.Power), EncodeHTML(Face.Toughness)])
      else if not Face.Loyalty.IsEmpty then
        PowerToughness :=
          Format('<div class="power-toughness">Loyalty: %s</div>',
          [EncodeHTML(Face.Loyalty)])
      else
        PowerToughness := '';

      // Image for the card face
      FaceImage := '';
      if Assigned(Face.ImageUris) and not Face.ImageUris.Normal.IsEmpty then
        FaceImage :=
          Format('<div class="single-card"><img src="%s" alt="%s"></div>',
          [EncodeHTML(Face.ImageUris.Normal), EncodeHTML(Face.Name)]);

      // Append face details to the HTML
      Builder.AppendFormat('<div class="card-face-details">' +
        // Wrapper for each face
        '%s' + // Face image
        '<div class="card-name">%s</div>' + // Name of the face
        '<div class="mana-cost">%s</div>' + // Mana cost with symbols
        '<div class="type-line">%s</div>' + // Type line
        '<div class="oracle-text">%s</div>' + // Oracle text
        '<div class="flavor-text">%s</div>' + // Flavor text
        '%s' + // Power/Toughness or Loyalty
        '</div>', [FaceImage, EncodeHTML(Face.Name),
        ReplaceManaSymbolsWithImages(Face.ManaCost), EncodedTypeLine,
        EncodedOracleText, EncodedFlavorText, PowerToughness]);
    end;

    Builder.Append('</div>'); // End the multi-face card container

    // Replace the placeholder in the replacements dictionary
    AddReplacement(Replacements, '{{OracleText}}', Builder.ToString);

    // Clear the single-face flavor text placeholder
    AddReplacement(Replacements, '{{FlavorText}}', '');
  finally
    Builder.Free;
  end;
end;

/// <summary>
/// Retrieves image URIs for each card part in a meld sequence.
/// </summary>
function FetchMeldPartImages(const MeldParts: TArray<TCardPart>)
  : TArray<TImageUris>;
var
  ScryfallAPI: TScryfallAPI;
  i: Integer;
begin
  ScryfallAPI := TScryfallAPI.Create;
  try
    SetLength(Result, Length(MeldParts));
    for i := 0 to High(MeldParts) do
    begin
      try
        Result[i] := ScryfallAPI.GetCardImageUris(MeldParts[i].ID);
      except
        on E: Exception do
        begin
          LogStuff(Format('Error fetching image for meld part %s: %s',
            [MeldParts[i].Name, E.Message]), ERROR);
          Result[i].Clear; // Ensure no invalid data
        end;
      end;
    end;
  finally
    ScryfallAPI.Free;
  end;
end;

/// <summary>
/// Adds the HTML block for meld details (meld parts + meld result),
/// or hides it if the card is not a meld card.
/// </summary>
procedure AddMeldDetails(const CardDetails: TCardDetails;
  Replacements: TDictionary<string, string>);
var
  MeldPartsHtml, PartHtml: string;
  PartImages: TArray<TImageUris>;
  Part: TCardPart;
  i: Integer;
  ImageUri: string;
begin
  if not CardDetails.IsMeld or not Assigned(CardDetails.MeldDetails) then
  begin
    Replacements.AddOrSetValue('{{MeldDetails}}', '');
    Replacements.AddOrSetValue('{{MeldClass}}', 'hidden');
    Exit;
  end;

  MeldPartsHtml := '<div class="meld-parts">';
  PartImages := FetchMeldPartImages(CardDetails.MeldDetails.MeldParts.ToArray);

  for i := 0 to CardDetails.MeldDetails.MeldParts.Count - 1 do
  begin
    Part := CardDetails.MeldDetails.MeldParts[i];
    if i < Length(PartImages) then
      ImageUri := PartImages[i].Small
    else
      ImageUri := '';
    var
    EncodedTypeLine := EncodeHTML(Part.TypeLine);
    PartHtml := Format('<div class="meld-part"><p><strong>%s</strong></p>' +
      '<p>%s</p><img src="%s" alt="%s"></div>',
      [EncodeHTML(Part.Name), EncodedTypeLine, EncodeHTML(ImageUri),
      EncodeHTML(Part.Name)]);
    MeldPartsHtml := MeldPartsHtml + PartHtml;
  end;

  if Assigned(CardDetails.MeldDetails.MeldResult) and
    (not CardDetails.MeldDetails.MeldResult.Name.IsEmpty) then
  begin
    var
    MeldResultImages := FetchMeldPartImages
      ([CardDetails.MeldDetails.MeldResult])[0];
    ImageUri := MeldResultImages.Small;
    MeldPartsHtml := MeldPartsHtml + Format('<div class="meld-result">' +
      '<p><strong>Meld Result:</strong> %s</p><img src="%s" alt="%s"></div>',
      [EncodeHTML(CardDetails.MeldDetails.MeldResult.Name),
      EncodeHTML(ImageUri),
      EncodeHTML(CardDetails.MeldDetails.MeldResult.Name)]);
  end;

  MeldPartsHtml := MeldPartsHtml + '</div>';
  Replacements.AddOrSetValue('{{MeldDetails}}', MeldPartsHtml);
  Replacements.AddOrSetValue('{{MeldClass}}', '');
end;

/// <summary>
/// Builds the single or multi-face Oracle text, flavor text, type line,
/// collector info, etc., and puts them into the Replacements dictionary.
/// </summary>
procedure AddCoreReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  ProcessedOracleText, GamesList, RarityStr: string;
begin
  // Meld info (may hide or show)
  if CardDetails.IsMeld then
    AddMeldDetails(CardDetails, Replacements)
  else
    AddReplacement(Replacements, '{{MeldDetails}}', '');

  // If single-faced, show flavor text; multi-face is handled in AddMultiFaceOracleText
  if CardDetails.CardFaces.Count = 0 then
    AddReplacement(Replacements, '{{FlavorText}}',
      EncodeHTML(CardDetails.FlavorText))
  else
    AddReplacement(Replacements, '{{FlavorText}}', '');

  // Core fields
  AddReplacement(Replacements, '{{CardName}}',
    EncodeHTML(CardDetails.CardName));
  AddReplacement(Replacements, '{{SetName}}', EncodeHTML(CardDetails.SetName));
  AddReplacement(Replacements, '{{SetIcon}}', CardDetails.SetIconURI);

  RarityStr := CardDetails.Rarity.ToString;
  /// /////////

  AddReplacement(Replacements, '{{Rarity}}', EncodeHTML(RarityStr));
  AddReplacement(Replacements, '{{RarityClass}}',
    GetRarityClass(CardDetails.Rarity));
  AddReplacement(Replacements, '{{TypeLine}}',
    EncodeHTML(CardDetails.TypeLine));
  AddReplacement(Replacements, '{{ManaCost}}',
    ReplaceManaSymbolsWithImages(CardDetails.ManaCost));
  AddReplacement(Replacements, '{{Artist}}', EncodeHTML(CardDetails.Artist));
  AddReplacement(Replacements, '{{CollectorNumber}}',
    EncodeHTML(CardDetails.CollectorNumber));
  AddReplacement(Replacements, '{{Arena Id}}',
    EncodeHTML(CardDetails.ArenaID.ToString));
  AddReplacement(Replacements, '{{BorderColor}}',
    EncodeHTML(CardDetails.BorderColor));
  AddReplacement(Replacements, '{{ReleasedAt}}',
    EncodeHTML(CardDetails.ReleasedAt));
  AddReplacement(Replacements, '{{StorySpotlight}}',
    IfThen(CardDetails.StorySpotlight, 'Yes', 'No'));
  AddReplacement(Replacements, '{{ScryfallURI}}', EncodeHTML(CardDetails.SFID));
  AddReplacement(Replacements, '{{PowerToughness}}',
    BuildPowerToughnessHtml(CardDetails));

  // Oracle text: single vs multi-face
  if CardDetails.CardFaces.Count > 0 then
    AddMultiFaceOracleText(CardDetails, Replacements)
  else
  begin
    ProcessedOracleText := ProcessOracleText(CardDetails.OracleText);
    AddReplacement(Replacements, '{{OracleText}}', ProcessedOracleText);
  end;

  // Games
  GamesList := string.Join(', ', CardDetails.Games.ToArray);

  AddOrHide(Replacements, '{{Games}}', '{{GamesClass}}', GamesList);
end;

/// <summary>
/// Handles generating HTML for card images, including double-faced
/// or modal cards with a 'flip' container.
/// </summary>
procedure AddImageReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  CardImagesHtml, FlipIndicatorHtml: string;
  LayoutLower: string;
begin
  LayoutLower := CardDetails.Layout.ToLower;
  FlipIndicatorHtml := '';

  // For transform, modal_dfc, etc., we handle multiple faces
  if IsInArray(LayoutLower, ['transform', 'modal_dfc', 'reversible_card',
    'art_series', 'double_faced_token']) then
  begin
    if CardDetails.CardFaces.Count > 1 then
    begin
      FlipIndicatorHtml :=
        '<div class="flip-indicator">Double-Faced Card: Click to Flip</div>';
      CardImagesHtml := Format('<div class="flip-card" onclick="flipCard()">' +
        '<div class="card-face front"><img src="%s" alt="Front Face"></div>' +
        '<div class="card-face back"><img src="%s" alt="Back Face"></div>' +
        '</div>', [EncodeHTML(CardDetails.CardFaces[0].ImageUris.Normal),
        EncodeHTML(CardDetails.CardFaces[1].ImageUris.Normal)]);
    end
    else
    begin
      // Fallback if only one face is actually present
      CardImagesHtml :=
        Format('<div class="single-card"><img src="%s" alt="Card Image"></div>',
        [EncodeHTML(CardDetails.ImageUris.Normal)]);
    end;
  end
  else
  begin
    // Single-faced card
    CardImagesHtml :=
      Format('<div class="single-card"><img src="%s" alt="Card Image"></div>',
      [EncodeHTML(CardDetails.ImageUris.Normal)]);
  end;

  AddReplacement(Replacements, '{{CardImages}}', CardImagesHtml);
  AddReplacement(Replacements, '{{FlipIndicator}}', FlipIndicatorHtml);
end;

/// <summary>
/// Adds rows to {{Legalities}} placeholder for each known legality format.
/// Each row has a color-coded label based on its status.
/// </summary>
// procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>;
// const CardDetails: TCardDetails);
// var
// Format: TLegalityFormat;
// LegalityName, LegalityStatus, StatusClass: string;
// Builder: TStringBuilder;
// begin
// Builder := TStringBuilder.Create;
// try
// for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
// begin
// LegalityName := Format.ToString;
// LegalityStatus := CardDetails.Legalities.GetStatus(Format);
//
// if not LegalityStatus.IsEmpty then
// begin
// StatusClass := GetStatusClass(LegalityStatus);
// LegalityStatus := FormatLegalityStatus(LegalityStatus);
//
// Builder.AppendFormat('<tr>' + '<td class="format-name">%s</td>' +
// '<td class="status"><span class="%s">%s</span></td>' + '</tr>',
// [EncodeHTML(CapitalizeFirstLetter(LegalityName)), StatusClass,
// EncodeHTML(LegalityStatus)]);
// end;
// end;
// AddReplacement(Replacements, '{{Legalities}}', Builder.ToString);
// finally
// Builder.Free;
// end;
// end;
procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  Format: TLegalityFormat;
  Builder: TStringBuilder;
  LegalityName, LegalityStatus, StatusClass: string;
begin
  Builder := TStringBuilder.Create;
  try
    for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
    begin
      LegalityStatus := CardDetails.Legalities.GetStatus(Format);
      if LegalityStatus.IsEmpty then
        Continue; // Skip empty legalities

      LegalityName := CapitalizeFirstLetter(Format.ToString);
      StatusClass := GetStatusClass(LegalityStatus);
      LegalityStatus := FormatLegalityStatus(LegalityStatus);

      Builder.AppendFormat
        ('<tr><td class="format-name">%s</td><td class="status"><span class="%s">%s</span></td></tr>',
        [LegalityName, StatusClass, LegalityStatus]);
    end;

    AddReplacement(Replacements, '{{Legalities}}', Builder.ToString);
  finally
    Builder.Free;
  end;
end;

/// <summary>
/// Inserts price data (USD, EUR, Tix) into the placeholders.
/// </summary>
procedure AddPricesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
begin
  AddReplacement(Replacements, '{{USD}}', CardDetails.Prices.USD.ToString);
  AddReplacement(Replacements, '{{USD_Foil}}',
    CardDetails.Prices.USD_Foil.ToString);
  AddReplacement(Replacements, '{{EUR}}', CardDetails.Prices.EUR.ToString);
  AddReplacement(Replacements, '{{Tix}}', CardDetails.Prices.Tix.ToString);
end;

/// <summary>
/// Adds "badge" placeholders like Full Art, Promo, or Reserved.
/// </summary>
procedure AddBadgesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
begin
  AddReplacement(Replacements, '{{FullArt}}', IfThen(CardDetails.FullArt,
    '<span class="badge full-art">Full Art</span>', ''));

  AddReplacement(Replacements, '{{Promo}}', IfThen(CardDetails.Promo,
    '<span class="badge promo">Promo</span>', ''));

  AddReplacement(Replacements, '{{Reserved}}', IfThen(CardDetails.Reserved,
    '<span class="badge reserved">Reserved</span>', ''));
end;

/// <summary>
/// Joins and adds any known card keywords, or hides that section if none.
/// </summary>
procedure AddKeywordsReplacement(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  KeywordsList: string;
begin
  KeywordsList := string.Join(', ', CardDetails.Keywords.ToArray);
  AddOrHide(Replacements, '{{Keywords}}', '{{KeywordsClass}}', KeywordsList);
end;

/// <summary>
/// Builds a small HTML snippet to display either Power/Toughness or Loyalty,
/// whichever is relevant to the card.
/// </summary>
function BuildPowerToughnessHtml(const CardDetails: TCardDetails): string;
var
  Power, Toughness, Loyalty: string;
begin
  Power := EncodeHTML(CardDetails.Power);
  Toughness := EncodeHTML(CardDetails.Toughness);
  Loyalty := EncodeHTML(CardDetails.Loyalty);

  if (Power <> '') and (Toughness <> '') then
    Exit(Format
      ('<div class="power-toughness"><span class="label">P/T:</span><span class="value">%s/%s</span></div>',
      [Power, Toughness]));

  if Loyalty <> '' then
    Exit(Format
      ('<div class="power-toughness"><span class="label">Loyalty:</span><span class="value">%s</span></div>',
      [Loyalty]));

  Result := ''; // No output if neither is set
end;

initialization

finalization

end.
