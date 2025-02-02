unit Mana2;

interface

uses
  FMX.Controls, System.Generics.Collections, FMX.Skia, System.SysUtils,
  FMX.StdCtrls, FMX.Types, JsonDataObjects, MLogic, System.RegularExpressions,
  FMX.Layouts, System.Classes, System.Types, System.UITypes,FMX.TextLayout,FMX.Graphics,System.Math;

procedure DisplayOracleTextWithIcons(const Parent: TControl;
  const OracleText: string; IconMap: TDictionary<string, string>);
function LoadIconMapFromJson(const JsonFilePath: string)
  : TDictionary<string, string>;
procedure BuildOracleText(Parent: TLayout; const Text: string; Icons: TJsonObject);
function LoadIconsFromJson(const FilePath: string): TJsonObject;


implementation

procedure DisplayOracleTextWithIcons(const Parent: TControl;
  const OracleText: string; IconMap: TDictionary<string, string>);
var
  Lines: TArray<string>;
  Words: TArray<string>;
  Line: string;
  Word: string;
  CurrentX, CurrentY: Single;
  LineHeight, WordSpacing, IndentSpacing: Single;
  MaxWidth: Single;
  SvgIcon: TSkSvg;
  LabelControl: TSkLabel;
  IconSvg: string;
begin
  // Layout settings
  CurrentX := 0;
  CurrentY := 0;
  LineHeight := 14; // Adjust for font/icon height
  WordSpacing := 4; // Space between words/icons
  IndentSpacing := 1; // Default indentation
  MaxWidth := Parent.Width - 10; // Leave a margin of 10 on the right

  // Replace \n with #10 to handle line breaks from API
  Lines := OracleText.Replace('\n', #10).Split([#10], TStringSplitOptions.None);

  for Line in Lines do
  begin
    // Detect if the line should be indented
    if Line.StartsWith('{') or TRegEx.IsMatch(Line, '^\d') then
      CurrentX := IndentSpacing
    else
      CurrentX := 0;

    // Split the line into words and placeholders
    Words := ParseTextWithSymbolsManual(Line);

    for Word in Words do
    begin
      if IconMap.TryGetValue(Word, IconSvg) then
      begin
        // If it's an icon placeholder, add an SVG icon
        SvgIcon := TSkSvg.Create(Parent);
        SvgIcon.Parent := Parent;
        SvgIcon.Svg.Source := IconSvg; // Use the SVG data from IconMap
        SvgIcon.Width := LineHeight; // Icon size matches the text height
        SvgIcon.Height := LineHeight;
        SvgIcon.Position.X := CurrentX;
        SvgIcon.Position.Y := CurrentY;
        CurrentX := CurrentX + SvgIcon.Width + WordSpacing;
      end
      else
      begin
        // If it's regular text, add a TSkLabel
        LabelControl := TSkLabel.Create(Parent);
        LabelControl.Parent := Parent;
        LabelControl.Text := Word;
        LabelControl.AutoSize := True;
        LabelControl.TextSettings.HorzAlign := TSkTextHorzAlign.Leading;
        LabelControl.Position.X := CurrentX;
        LabelControl.Position.Y := CurrentY + (LineHeight - LabelControl.Height)
          / 2; // Vertically align text with icons
        CurrentX := CurrentX + LabelControl.Width + WordSpacing;
      end;

      // Wrap to the next line if the current X position exceeds MaxWidth
      if CurrentX > MaxWidth then
      begin
        CurrentX := 0;
        CurrentY := CurrentY + LineHeight + WordSpacing;
      end;
    end;

    // Move to the next line after processing all words/icons in the current line
    CurrentX := 0;
    CurrentY := CurrentY + LineHeight + WordSpacing;
  end;

  // Adjust parent height dynamically based on content
  if Parent is TLayout then
    TLayout(Parent).Height := CurrentY
  else if Parent is TControl then
    TControl(Parent).Height := CurrentY;
end;



function LoadIconMapFromJson(const JsonFilePath: string)
  : TDictionary<string, string>;
var
  JsonObj: TJsonObject;
  Pair: TJsonNameValuePair;
begin
  Result := TDictionary<string, string>.Create;
  JsonObj := TJsonObject.Create;
  try
    JsonObj.LoadFromFile(JsonFilePath); // Load JSON file
    for Pair in JsonObj do
      Result.Add(Pair.Name, Pair.Value.Value);
    // Add key-value pairs to the dictionary
  finally
    JsonObj.Free;
  end;
end;


function LoadIconsFromJson(const FilePath: string): TJsonObject;
begin
  Result := TJsonObject.Create;
  try
    if FileExists(FilePath) then
      Result.LoadFromFile(FilePath)
    else
      raise Exception.CreateFmt('JSON file not found: %s', [FilePath]);
  except
    on E: Exception do
    begin
      Result.Free;
      raise; // Re-raise the exception for handling at a higher level
    end;
  end;
end;


procedure BuildOracleText(Parent: TLayout; const Text: string; Icons: TJsonObject);
const
  ICON_SIZE = 24;
  LINE_SPACING = 4;
  PARAGRAPH_SPACING = 8;
var
  Paragraphs: TArray<string>;
  CurrentY: Single;
  AvailableWidth: Single;

  // Helper function to split on literal "\n" strings
  function SplitLines(const Input: string): TArray<string>;
  begin
    Result := Input.Replace('\n', #1).Split([#1]); // Use temporary delimiter
  end;

  function MeasureText(const Text: string; Font: TFont): TSizeF;
  var
    Layout: TTextLayout;
  begin
    Layout := TTextLayoutManager.DefaultTextLayout.Create;
    try
      Layout.BeginUpdate;
      try
        Layout.Font := Font;
        Layout.Text := Text;
        Result := TSizeF.Create(Layout.Width, Layout.Height);
      finally
        Layout.EndUpdate;
      end;
    finally
      Layout.Free;
    end;
  end;

  procedure ProcessToken(const Token: string; var X, LineHeight: Single);
  var
    Control: TControl;
    TokenSize: TSizeF;
    Font: TFont;
  begin
    if (Token.Length > 2) and Token.StartsWith('{') and Token.EndsWith('}') then
    begin
      // Handle SVG icon using JsonDataObjects
      Control := TSkSVG.Create(Parent);
      try
        (Control as TSkSVG).SVG.Source := Icons.S[Token]; // JsonDataObjects access
        Control.Width := ICON_SIZE;
        Control.Height := ICON_SIZE;
        TokenSize := TSizeF.Create(ICON_SIZE, ICON_SIZE);
      except
        Control.Free;
        raise;
      end;
    end
    else
    begin
      // Handle text
      Font := TFont.Create;
      try
        Font.Size := 14;
        TokenSize := MeasureText(Token, Font);
        Control := TLabel.Create(Parent);
        (Control as TLabel).Text := Token;
        (Control as TLabel).StyledSettings := [];
        (Control as TLabel).Font := Font;
        (Control as TLabel).AutoSize := True;
      finally
        Font.Free;
      end;
    end;

    if X + TokenSize.Width > AvailableWidth then
    begin
      CurrentY := CurrentY + LineHeight + LINE_SPACING;
      X := 0;
      LineHeight := 0;
    end;

    Control.Position.X := X;
    Control.Position.Y := CurrentY + (LineHeight - TokenSize.Height)/2;
    Control.Parent := Parent;

    X := X + TokenSize.Width;
    LineHeight := Max(LineHeight, TokenSize.Height);
  end;

  procedure ProcessParagraph(const Paragraph: string);
  var
    Tokens: TArray<string>;
    X, LineHeight: Single;
  begin
    Tokens := TRegEx.Split(Paragraph, '(\{.*?\})');
    X := 0;
    LineHeight := 0;

    for var Token in Tokens do
    begin
      if Token.IsEmpty then Continue;
      ProcessToken(Token, X, LineHeight);
    end;

    CurrentY := CurrentY + LineHeight;
  end;

begin
  Parent.BeginUpdate;
  try
    while Parent.ControlsCount > 0 do
      Parent.Controls[0].Free;

    AvailableWidth := Parent.Width;
    CurrentY := 0;

    // Split paragraphs using custom splitter
    Paragraphs := SplitLines(Text);

    for var I := 0 to High(Paragraphs) do
    begin
      ProcessParagraph(Paragraphs[I]);
      if I < High(Paragraphs) then
        CurrentY := CurrentY + PARAGRAPH_SPACING;
    end;

    Parent.Height := CurrentY;
  finally
    Parent.EndUpdate;
  end;
end;


// GetCacheFilePath

// var
// IconMap: TDictionary<string, string>;


// IconMap := LoadIconMapFromJson('C:\Users\raygu\AppData\Roaming\MTGCardFetch\SymbolCache.json'); // Load icons from JSON
// try
// DisplayOracleTextWithIcons(LayoutContent1,
// 'Target creature becomes a {W} and {U} creature until end of turn.',
// IconMap);
// finally
// IconMap.Free;
// end;
// }

end.
