unit CardDisplayHelpers;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  SGlobalsX,
  System.StrUtils,
  System.Net.HttpClient,
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
function StringToRarity(const RarityStr: string): TRarity;
function IsInArray(const Value: string;
  const Candidates: array of string): Boolean;
function EncodeHTML(const HtmlText: string): string;

implementation

uses
  System.NetEncoding,
  MLogic,
  Mana,
  System.Classes,
  JsonDataObjects,
  System.IOUtils,
  Logger,
  APIConstants;

{$REGION 'Global Cache and Helpers'}

var
  SetIconCache: TDictionary<string, string>;

/// <summary>
///   Simple HTML-encodes the given text.
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
/// Converts a raw string to a TRarity enumeration. Returns rAll if unknown.
/// </summary>
function StringToRarity(const RarityStr: string): TRarity;
begin
  if RarityStr.IsEmpty then
    Exit(rAll); // Default or fallback

  if SameText(RarityStr, 'common') then
    Result := rCommon
  else if SameText(RarityStr, 'uncommon') then
    Result := rUncommon
  else if SameText(RarityStr, 'rare') then
    Result := rRare
  else if SameText(RarityStr, 'mythic') then
    Result := rMythic
  else if SameText(RarityStr, 'special') then
    Result := rSpecial
  else if SameText(RarityStr, 'bonus') then
    Result := rBonus
  else if SameText(RarityStr, 'timeshifted') then
    Result := rTimeshifted
  else if SameText(RarityStr, 'masterpiece') then
    Result := rMasterpiece
  else if SameText(RarityStr, 'token') then
    Result := rToken
  else if SameText(RarityStr, 'double_faced_token') then
    Result := rDoubleFacedToken
  else if SameText(RarityStr, 'draft') then
    Result := rDraft
  else if SameText(RarityStr, 'planeshifted') then
    Result := rPlaneshifted
  else if SameText(RarityStr, 'unique') then
    Result := rUnique
  else if SameText(RarityStr, 'basic') then
    Result := rBasic
  else if SameText(RarityStr, 'promo') then
    Result := rPromo
  else
    Result := rAll; // Default fallback
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
var
  Parts: TArray<string>;
  Part: string;
  EncodedPart: string;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Parts := ParseTextWithSymbolsManual(OracleText);
    for Part in Parts do
    begin
      // If it's a mana symbol, replace with an image
      if Part.StartsWith('{') and Part.EndsWith('}') then
        Builder.Append(ReplaceManaSymbolsWithImages(Part))
      else
      begin
        // Avoid unnecessary encoding conversions for Windows-only ifdef
{$IFDEF MSWINDOWS}
        try
          EncodedPart := EncodeHTML(Part);
        except
          on E: Exception do
          begin
            LogStuff('Error encoding part: ' + Part + '. Error: ' +
              E.Message, ERROR);
            EncodedPart := ''; // fallback to an empty string
          end;
        end;
{$ELSE}
        EncodedPart := EncodeHTML(Part);
{$ENDIF}
        // Replace newlines with <br>
        EncodedPart := StringReplace(EncodedPart, #10, '<br>', [rfReplaceAll]);
        Builder.Append(EncodedPart);
      end;
    end;
    Result := Builder.ToString;
  except
    on E: Exception do
    begin
      LogStuff('Error in ProcessOracleText: ' + E.Message, ERROR);
      Result := ''; // Return empty string if there's an error
    end;
  end;
  Builder.Free;
end;

/// <summary>
/// Renders the multi-face Oracle text and relevant data (type line, flavor, P/T)
/// into a grid-like HTML block. Used by AddCoreReplacements if card has multiple faces.
/// </summary>
procedure AddMultiFaceOracleText(const CardDetails: TCardDetails;
  Replacements: TDictionary<string, string>);
var
  Face: TCardFace;
  ExtraHtml: string;
  EncTypeLine: string;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    Builder.Append('<div class="card-faces-grid">'); // Start grid container

    for Face in CardDetails.CardFaces do
    begin
{$IFDEF MSWINDOWS}
      EncTypeLine := TEncoding.UTF8.GetString
        (TEncoding.ANSI.GetBytes(Face.TypeLine));
{$ELSE}
      EncTypeLine := EncodeHTML(Face.TypeLine);
{$ENDIF}
      // Build up lines: Power/Toughness, Flavor, etc.
      ExtraHtml := '';
      if not Face.Power.Trim.IsEmpty and not Face.Toughness.Trim.IsEmpty then
        ExtraHtml := ExtraHtml +
          Format('<p><strong>Power/Toughness:</strong> %s/%s</p>',
          [EncodeHTML(Face.Power), EncodeHTML(Face.Toughness)]);

      if not Face.FlavorText.Trim.IsEmpty then
        ExtraHtml := ExtraHtml + Format('<p>%s</p>',
          [EncodeHTML(Face.FlavorText)]);

      // Combine all details into a single face block
      Builder.AppendFormat('<div class="card-face-block">' +
        '<p><strong>Name:</strong> %s</p>' +
        '<p><strong>Mana Cost:</strong> %s</p>' +
        '<p><strong>Type Line:</strong> %s</p>' +
        '<p><strong>Oracle:</strong> %s</p>' + '%s' +
        // ExtraHtml for P/T, flavor
        '</div>', [EncodeHTML(Face.Name),
        ReplaceManaSymbolsWithImages(Face.ManaCost), EncodeHTML(EncTypeLine),
        ProcessOracleText(Face.OracleText), ExtraHtml]);
    end;

    Builder.Append('</div>'); // End container

    // Put the built HTML into the replacements
    AddReplacement(Replacements, '{{OracleText}}', Builder.ToString);
    // For multi-face cards, we typically clear out the single-face flavor placeholder
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
      ImageUri := ''; // fallback

    {$IFDEF MSWINDOWS}
    var EncodedTypeLine := TEncoding.UTF8.GetString(TEncoding.ANSI.GetBytes(Part.TypeLine));
    {$ELSE}
    var EncodedTypeLine := EncodeHTML(Part.TypeLine);
    {$ENDIF}

    PartHtml := Format(
      '<div class="meld-part">' +
        '<p><strong>%s</strong></p>' +
        '<p>%s</p>' +
        '<img src="%s" alt="%s">' +
      '</div>',
      [
        EncodeHTML(Part.Name),
        EncodedTypeLine,
        EncodeHTML(ImageUri),
        EncodeHTML(Part.Name)
      ]
    );
    MeldPartsHtml := MeldPartsHtml + PartHtml;
  end;

  if Assigned(CardDetails.MeldDetails.MeldResult) and
    (not CardDetails.MeldDetails.MeldResult.Name.IsEmpty) then
  begin
    var
    MeldResultImages := FetchMeldPartImages
      ([CardDetails.MeldDetails.MeldResult])[0];
    ImageUri := MeldResultImages.Small;
    MeldPartsHtml := MeldPartsHtml + Format(
      '<div class="meld-result">' +
        '<p><strong>Meld Result:</strong> %s</p>' +
        '<img src="%s" alt="%s">' +
      '</div>',
      [
        EncodeHTML(CardDetails.MeldDetails.MeldResult.Name),
        EncodeHTML(ImageUri),
        EncodeHTML(CardDetails.MeldDetails.MeldResult.Name)
      ]
    );
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
  if Length(CardDetails.CardFaces) = 0 then
    AddReplacement(Replacements, '{{FlavorText}}', EncodeHTML(CardDetails.FlavorText))
  else
    AddReplacement(Replacements, '{{FlavorText}}', '');

  // Core fields
  AddReplacement(Replacements, '{{CardName}}',       EncodeHTML(CardDetails.CardName));
  AddReplacement(Replacements, '{{SetName}}',        EncodeHTML(CardDetails.SetName));
  AddReplacement(Replacements, '{{SetIcon}}',        CardDetails.SetIconURI);
  RarityStr := RarityToString[CardDetails.Rarity];
  AddReplacement(Replacements, '{{Rarity}}',         EncodeHTML(RarityStr));
  AddReplacement(Replacements, '{{RarityClass}}',    GetRarityClass(CardDetails.Rarity));
  AddReplacement(Replacements, '{{TypeLine}}',       EncodeHTML(CardDetails.TypeLine));
  AddReplacement(Replacements, '{{ManaCost}}',       ReplaceManaSymbolsWithImages(CardDetails.ManaCost));
  AddReplacement(Replacements, '{{Artist}}',         EncodeHTML(CardDetails.Artist));
  AddReplacement(Replacements, '{{CollectorNumber}}',EncodeHTML(CardDetails.CollectorNumber));
  AddReplacement(Replacements, '{{Arena Id}}',       EncodeHTML(CardDetails.ArenaID.ToString));
  AddReplacement(Replacements, '{{BorderColor}}',    EncodeHTML(CardDetails.BorderColor));
  AddReplacement(Replacements, '{{ReleasedAt}}',     EncodeHTML(CardDetails.ReleasedAt));
  AddReplacement(Replacements, '{{StorySpotlight}}', IfThen(CardDetails.StorySpotlight, 'Yes', 'No'));
  AddReplacement(Replacements, '{{ScryfallURI}}',    EncodeHTML(CardDetails.SFID));
  AddReplacement(Replacements, '{{PowerToughness}}', BuildPowerToughnessHtml(CardDetails));

  // Oracle text: single vs multi-face
  if CardDetails.CardFaces.Count > 0 then
    AddMultiFaceOracleText(CardDetails, Replacements)
  else
  begin
    ProcessedOracleText := ProcessOracleText(CardDetails.OracleText);
    AddReplacement(Replacements, '{{OracleText}}', ProcessedOracleText);
  end;

  // Games
  GamesList := String.Join(', ', CardDetails.Games.ToArray);

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
procedure AddLegalitiesReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails);
var
  Format: TLegalityFormat;
  LegalityName, LegalityStatus, StatusClass: string;
  Builder: TStringBuilder;
begin
  Builder := TStringBuilder.Create;
  try
    for Format := Low(TLegalityFormat) to High(TLegalityFormat) do
    begin
      LegalityName   := LegalityToString[Format];
      LegalityStatus := CardDetails.Legalities.GetStatus(Format);

      if not LegalityStatus.IsEmpty then
      begin
        StatusClass := GetStatusClass(LegalityStatus);
        LegalityStatus := FormatLegalityStatus(LegalityStatus);

        Builder.AppendFormat('<tr>' + '<td class="format-name">%s</td>' +
          '<td class="status"><span class="%s">%s</span></td>' + '</tr>',
          [EncodeHTML(CapitalizeFirstLetter(LegalityName)), StatusClass,
          EncodeHTML(LegalityStatus)]);
      end;
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
  KeywordsList := String.Join(', ', CardDetails.Keywords.ToArray);
  AddOrHide(Replacements, '{{Keywords}}', '{{KeywordsClass}}', KeywordsList);
end;

{$ENDREGION}

{$REGION 'Power/Toughness & Helpers'}

/// <summary>
/// Builds a small HTML snippet to display either Power/Toughness or Loyalty,
/// whichever is relevant to the card.
/// </summary>
function BuildPowerToughnessHtml(const CardDetails: TCardDetails): string;
begin
  Result := '';
  if (CardDetails.Power <> '') and (CardDetails.Toughness <> '') then
  begin
    Result := Format('<div class="power-toughness">' +
      '<span class="label">Power/Toughness:</span>' +
      '<span class="value">%s/%s</span>' + '</div>',
      [EncodeHTML(CardDetails.Power), EncodeHTML(CardDetails.Toughness)]);
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

{$ENDREGION}

{$REGION 'Set Icon Caching'}

/// <summary>
///   Saves the entire in-memory SetIconCache to a JSON file on disk.
/// </summary>
procedure SaveSetIconCacheToFile;
var
  CacheJson: TJsonObject;
  Pair: TPair<string, string>;
  CachePath: string;
begin
  CacheJson := TJsonObject.Create;
  try
    for Pair in SetIconCache do
      CacheJson.S[Pair.Key] := Pair.Value;

    CachePath := GetCacheFilePath(SetIconCacheFile);
    CacheJson.SaveToFile(CachePath, False, TEncoding.UTF8, False);
    LogStuff('Set icon cache saved to disk: ' + CachePath);
  finally
    CacheJson.Free;
  end;
end;

/// <summary>
///   Loads the SetIconCache from a JSON file on disk (if it exists).
/// </summary>
procedure LoadSetIconCacheFromFile;
var
  CacheJson: TJsonObject;
  CachePath: string;
  i: Integer;
  Key: string;
begin
  CachePath := GetCacheFilePath(SetIconCacheFile);
  if not TFile.Exists(CachePath) then
    Exit;

  CacheJson := TJsonObject.Create;
  try
    try
      CacheJson.LoadFromFile(CachePath);
      for i := 0 to CacheJson.Count - 1 do
      begin
        Key := CacheJson.Names[i];
        SetIconCache.AddOrSetValue(Key, CacheJson.S[Key]);
      end;
      LogStuff('Set icon cache loaded from disk: ' + CachePath);
    except
      on E: Exception do
      begin
        LogStuff('Failed to load set icon cache: ' + E.Message);
        SetIconCache.Clear;
        TFile.Delete(CachePath);
      end;
    end;
  finally
    CacheJson.Free;
  end;
end;

/// <summary>
///   Downloads the SVG from IconURL and returns its Base64-encoded content.
/// </summary>
function FetchSVGAsBase64(const IconURL: string): string;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  MemoryStream: TMemoryStream;
  SVGContent: TBytes;
begin
  Result := '';
  HttpClient := THTTPClient.Create;
  MemoryStream := TMemoryStream.Create;
  try
    try
      Response := HttpClient.Get(IconURL, MemoryStream);
      if Response.StatusCode = 200 then
      begin
        MemoryStream.Position := 0;
        SetLength(SVGContent, MemoryStream.Size);
        MemoryStream.ReadBuffer(SVGContent, MemoryStream.Size);
        Result := TNetEncoding.Base64.EncodeBytesToString(SVGContent);
        LogStuff('Downloaded and converted SVG to Base64: ' + IconURL);
      end
      else
        LogStuff(Format('Failed to download SVG [%s]. HTTP %d: %s',
          [IconURL, Response.StatusCode, Response.StatusText]));
    except
      on E: Exception do
        LogStuff('Error fetching SVG: ' + E.Message);
    end;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
end;

/// <summary>
///   Checks if we already have a cached Base64 version of the set icon;
///   if not, downloads and caches it.
/// </summary>
function GetSetIconAsRawSVG(const IconURL, SetCode: string): string;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  MemoryStream: TMemoryStream;
  SVGContent: TBytes;
  RawSvg: string;
begin
  // If we've already cached this set code, just return it
  if SetIconCache.ContainsKey(SetCode) then
    Exit(SetIconCache[SetCode]);

  Result := '';
  HttpClient := THTTPClient.Create;
  MemoryStream := TMemoryStream.Create;
  try
    try
      Response := HttpClient.Get(IconURL, MemoryStream);
      if Response.StatusCode = 200 then
      begin
        MemoryStream.Position := 0;
        SetLength(SVGContent, MemoryStream.Size);
        MemoryStream.ReadBuffer(SVGContent, MemoryStream.Size);

        // Convert the raw bytes to a UTF8 string (assuming SVG is UTF8-encoded)
        RawSvg := TEncoding.UTF8.GetString(SVGContent);

        // Store this raw SVG in the cache dictionary
        SetIconCache.AddOrSetValue(SetCode, RawSvg);
        Result := RawSvg;

        // Persist the updated cache to disk
        SaveSetIconCacheToFile;
      end
      else
      begin
        // Log or handle the error if needed
        LogStuff(Format('Failed to download SVG. HTTP %d', [Response.StatusCode]), WARNING);
      end;
    except
      on E: Exception do
      begin
        LogStuff('Error fetching raw SVG: ' + E.Message, ERROR);
      end;
    end;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
end;


{$ENDREGION}

{$REGION 'Set JSON Load/Save'}

function LoadSetDetailsFromJson(const FileName: string): TArray<TSetDetails>;
var
  JsonObject: TJsonObject;
  JsonArray: TJsonArray;
  SetDetails: TSetDetails;
  i: Integer;
begin
  if not TFile.Exists(FileName) then
    Exit(nil);

  JsonObject := TJsonObject.Create;
  try
    JsonObject.LoadFromFile(FileName);

    if not JsonObject.Contains('sets') then
      Exit(nil);

    JsonArray := JsonObject.A['sets'];
    SetLength(Result, JsonArray.Count);

    for i := 0 to JsonArray.Count - 1 do
    begin
      SetDetails.Code       := JsonArray.O[i].S[FieldCode];
      SetDetails.Name       := JsonArray.O[i].S[FieldName];
      SetDetails.IconSVGURI := JsonArray.O[i].S[FieldIconSvgUri];
      Result[i] := SetDetails;
    end;
  finally
    JsonObject.Free;
  end;
end;

procedure SaveSetDetailsToJson(const FileName: string;
  const SetDetailsArray: TArray<TSetDetails>);
var
  JsonObject: TJsonObject;
  JsonArray: TJsonArray;
  JsonSet: TJsonObject;
  SetDetails: TSetDetails;
begin
  JsonObject := TJsonObject.Create;
  try
    JsonArray := JsonObject.A[FieldSets];

    for SetDetails in SetDetailsArray do
    begin
      JsonSet := TJsonObject.Create;
      JsonSet.S[FieldCode]       := SetDetails.Code;
      JsonSet.S[FieldName]       := SetDetails.Name;
      JsonSet.S[FieldIconSvgUri] := SetDetails.IconSVGURI;
      JsonArray.AddObject(JsonSet);
    end;

    JsonObject.SaveToFile(FileName, False, TEncoding.UTF8, False);
  finally
    JsonObject.Free;
  end;
end;

{$ENDREGION}

initialization
  SetIconCache := TDictionary<string, string>.Create;
  LoadSetIconCacheFromFile;

finalization
  SaveSetIconCacheToFile;
  SetIconCache.Free;

end.
