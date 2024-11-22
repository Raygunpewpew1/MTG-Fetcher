unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, ScryfallAPIWrapperV2, SGlobalsZ, FMX.Objects,
  FMX.Layouts, FMX.ExtCtrls, FMX.Ani, System.Generics.Collections,
  System.Net.HttpClient, Math, System.Threading, FMX.Edit,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, System.JSON, REST.JSON, FMX.Effects,
  System.IOUtils, HighResForm, DataModuleUnit, FMX.Surfaces, CardCollectionForm,
  System.Hash, System.StrUtils, FMX.WebBrowser, System.Skia,
  FMX.Skia, RegularExpressions, System.NetEncoding, System.Rtti,System.TypInfo;

type
  TCardDetailsObject = class(TObject)
  public
    CardDetails: TCardDetails;
    constructor Create(const ACardDetails: TCardDetails);
  end;

  TCardLayout = class(TLayout)
  public
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    ScrollBox1: TScrollBox;
    FlowLayout1: TFlowLayout;
    Edit1: TEdit;
    Label1: TLabel;
    StyleBook1: TStyleBook;
    Label2: TLabel;
    Button4: TButton;
    Button5: TButton;
    ShowHighResButton: TButton;
    Button6: TButton;
    ProgressBar1: TProgressBar;
    Button7: TButton;
    Edit3: TEdit;
    DelayTimer: TTimer;
    WebBrowser1: TWebBrowser;
    Edit2: TEdit;
    Button3: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ShowHighResButtonClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure DelayTimerTimer(Sender: TObject);
  private
    HttpClient: THTTPClient;
    FManaSymbolMap: TDictionary<string, string>;
    FCardDetailsObject: TCardDetailsObject; // Private field
    CardTitle: string; // Private field
    CardDataList: TList<TCardDetails>; // Encapsulated within the form
    CardCount: Integer; // Encapsulated within the form
    AppClose: Boolean; // Encapsulated within the form
    FBase64ImageCache: TDictionary<string, string>;
    procedure DisplayCardArtworks(const CardName: string);
    procedure DisplayImageFromURLAsync(const URL: string;
      FlowLayout: TFlowLayout; const CardDetails: TCardDetails);
    procedure ShowCardDetails(Sender: TObject);
    function GetCacheDirectory: string;
    function GetCachedImagePath(const URL: string): string;
    procedure LoadCachedOrDownloadImage(const URL: string; Bitmap: TBitmap);
    // Changed parameter to TBitmap
    procedure FilterDisplayedCards(const FilterText: string);
    procedure SetupPopularCards(PopularCards: TStringList);
    procedure LoadCachedOrDownloadImageAsync(const URL: string;
      ImageControl: TImageControl);
    procedure AddTextToHighlightRectangle(Rectangle: TRectangle;
      const Text: string);
    procedure SaveSelectedCardToDatabase;
    procedure ShowCardCollection;
    procedure GetSelectedCardRulings(const cardId: string);
    procedure CopyDatabaseToInternalStorage;
    function GetDatabasePath: string;
    procedure DisplayCardInBrowser(const CardDetails: TCardDetails; const Rulings: TArray<TRuling>);
    function GetAppPath: string;
    function GetIconPath(const FileName: string): string;
    procedure InitializeManaSymbolMap;
    function ParseTextWithSymbolsManual(const Input: string): TArray<string>;
    function ReplaceManaSymbolsWithImages(const OracleText: string): string;
    function ImageToBase64(const ImagePath: string): string;
    function GetStatusClass(const LegalityStatus: string): string;
    function FetchRulingsAsync(const cardId: string): TArray<TRuling>;

  public
    // Public declarations
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


{ TCardLayout }

destructor TCardLayout.Destroy;
begin
  FreeAndNil(TagObject);
  inherited;
end;

{ TCardDetailsObject }

constructor TCardDetailsObject.Create(const ACardDetails: TCardDetails);
begin
  inherited Create;
  CardDetails := ACardDetails;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);

begin
  HttpClient := THTTPClient.Create;
  CardDataList := TList<TCardDetails>.Create;
  DataModule1.CreateCardDetailsTable;
  CardCount := 0;
  AppClose := False;
  DataModule1.SetupDatabaseConnection(GetDatabasePath);
  CopyDatabaseToInternalStorage;
  InitializeManaSymbolMap;
  FBase64ImageCache := TDictionary<string, string>.Create;

  DelayTimer.Enabled := True;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AppClose := True;
  if Assigned(HttpClient) then
    FreeAndNil(HttpClient);
  if Assigned(CardDataList) then
    FreeAndNil(CardDataList);
  FreeAndNil(FManaSymbolMap);
  FreeAndNil(FBase64ImageCache);
end;

function TForm1.ImageToBase64(const ImagePath: string): string;
begin
  if FBase64ImageCache.TryGetValue(ImagePath, Result) then
    Exit; // Return cached value

  if not TFile.Exists(ImagePath) then
  begin
    Result := '';
    Exit;
  end;

  var
    FileStream: TFileStream := TFileStream.Create(ImagePath,
      fmOpenRead or fmShareDenyWrite);
    try var Bytes: TBytes;
  SetLength(Bytes, FileStream.Size);
  FileStream.ReadBuffer(Bytes[0], FileStream.Size);
  Result := TNetEncoding.Base64.EncodeBytesToString(Bytes);
finally
  FileStream.Free;
end;

// Add the result to the cache
FBase64ImageCache.Add(ImagePath, Result);
end;

procedure TForm1.SetupPopularCards(PopularCards: TStringList);
begin
  PopularCards.Clear;
  PopularCards.AddStrings(['Black Lotus', 'Ancestral Recall', 'Mox Sapphire',
    'Mox Jet', 'Mox Ruby', 'Mox Pearl', 'Mox Emerald', 'Time Walk',
    'Timetwister', 'Tarmogoyf', 'Jace, the Mind Sculptor',
    'Liliana of the Veil', 'Force of Will', 'Snapcaster Mage', 'Dark Confidant',
    'Lightning Bolt', 'Birds of Paradise', 'Serra Angel', 'Shivan Dragon',
    'Swords to Plowshares', 'Brainstorm', 'Counterspell', 'Sol Ring',
    'Mana Crypt', 'Mana Vault', 'Ancient Tomb', 'Bazaar of Baghdad',
    'Library of Alexandria', 'The Tabernacle at Pendrell Vale',
    'Gaea''s Cradle', 'Nicol Bolas, the Ravager', 'Teferi, Hero of Dominaria',
    'Elspeth, Sun''s Champion', 'Primeval Titan', 'Emrakul, the Aeons Torn',
    'Ulamog, the Infinite Gyre', 'Blightsteel Colossus', 'Aether Vial',
    'Sensei''s Divining Top', 'Thalia, Guardian of Thraben', 'Noble Hierarch',
    'Deathrite Shaman', 'Wasteland', 'Strip Mine', 'Blood Moon', 'Thoughtseize',
    'Inquisition of Kozilek', 'Cabal Therapy', 'Yawgmoth''s Will', 'Tinker',
    'Demonic Tutor', 'Vampiric Tutor', 'Mystical Tutor', 'Enlightened Tutor',
    'Imperial Seal', 'Necropotence', 'Phyrexian Arena', 'Sylvan Library',
    'Lotus Petal', 'Chrome Mox', 'Grim Monolith', 'Lion''s Eye Diamond',
    'Arcbound Ravager', 'Chalice of the Void', 'Karn Liberated',
    'Ugin, the Spirit Dragon', 'Cryptic Command', 'Thought-Knot Seer',
    'Reality Smasher', 'Scalding Tarn', 'Misty Rainforest', 'Verdant Catacombs',
    'Polluted Delta', 'Flooded Strand', 'Platinum Angel', 'Eternal Witness',
    'Dark Ritual', 'Gilded Lotus', 'Birthing Pod', 'Hullbreacher',
    'Opposition Agent', 'Dockside Extortionist', 'Rhystic Study',
    'Mystic Remora', 'Mother of Runes', 'Stoneforge Mystic',
    'Sword of Fire and Ice', 'Sword of Feast and Famine', 'Batterskull',
    'Sigarda, Host of Herons', 'Zur the Enchanter', 'Atraxa, Praetors'' Voice',
    'Edgar Markov', 'Marrow-Gnawer', 'Krenko, Mob Boss',
    'Prossh, Skyraider of Kher', 'Meren of Clan Nel Toth', 'The Gitrog Monster',
    'Omnath, Locus of Creation', 'Karn, the Great Creator',
    'Oko, Thief of Crowns', 'Thassa''s Oracle', 'Underworld Breach']);

end;

function TForm1.GetCacheDirectory: string;
begin
  Result := TPath.Combine(TPath.GetHomePath, 'MTGCardFetch');
  // Use GetHomePath for consistency
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

function TForm1.GetCachedImagePath(const URL: string): string;
var
  Hash: string;
  FileName: string;
begin
  // Use SHA2 hash to ensure unique filenames
  Hash := THashSHA2.GetHashString(URL);
  FileName := Hash;
  Result := TPath.Combine(GetCacheDirectory, FileName);
end;

procedure TForm1.LoadCachedOrDownloadImage(const URL: string; Bitmap: TBitmap);
var
  FilePath: string;
  MemoryStream: TMemoryStream;
begin
  FilePath := GetCachedImagePath(URL);

  if AppClose then
    Exit;

  if TFile.Exists(FilePath) then
  begin
    Bitmap.LoadFromFile(FilePath);
    Inc(CardCount);
  end
  else
  begin
    MemoryStream := TMemoryStream.Create;
    try
      try
        HttpClient.Get(URL, MemoryStream);
        MemoryStream.Position := 0;
        MemoryStream.SaveToFile(FilePath);
        MemoryStream.Position := 0;
        Bitmap.LoadFromStream(MemoryStream);
        Inc(CardCount);
      except
        on E: ENetHTTPRequestException do
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Network error: ' + E.Message);
            end);
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Error loading image: ' + E.Message);
            end);
      end;
    finally
      MemoryStream.Free;
    end;
  end;
end;

procedure TForm1.LoadCachedOrDownloadImageAsync(const URL: string;
ImageControl: TImageControl);
begin
  TTask.Run(
    procedure
    var
      Bitmap: TBitmap;
    begin
      Bitmap := TBitmap.Create;
      try
        LoadCachedOrDownloadImage(URL, Bitmap);
        TThread.Synchronize(nil,
          procedure
          begin
            ImageControl.Bitmap.Assign(Bitmap);
          end);
      finally
        Bitmap.Free;
      end;
    end);
end;

procedure TForm1.DelayTimerTimer(Sender: TObject);
var
  PopularCards: TStringList;
begin
  FlowLayout1.Enabled := False;
  Edit1.Enabled := False;
  PopularCards := TStringList.Create;
  try
    SetupPopularCards(PopularCards);
    if PopularCards.Count > 0 then
      DisplayCardArtworks(PopularCards[Random(PopularCards.Count)]);
    WebBrowser1.Reload;
  finally
    PopularCards.Free;
  end;

  FlowLayout1.Enabled := True;
  Edit1.Enabled := True;

  DelayTimer.Enabled := False;

end;

procedure TForm1.DisplayCardArtworks(const CardName: string);
const
  ImageWidth = 175;
  ImageHeight = 245;
  HorizontalSpacing = 10;
var
  ScryfallAPI: TScryfallAPI;
  AllCards: TArray<TCardDetails>;
  TotalImages: Integer;
  Card: TCardDetails;
  FlowLayoutWidth: Integer;
  FirstLayout: TLayout;
  CardIndex: Integer;
begin
  ScryfallAPI := TScryfallAPI.Create;
  try

    // Fetch all card results using SearchAllCards
    AllCards := ScryfallAPI.SearchAllCards(CardName, '', '', '', False, False);

    TotalImages := Length(AllCards);

    If TotalImages = 0 then
      Exit;

    CardDataList.Clear;
    FlowLayout1.DeleteChildren;

    // Set ProgressBar properties
    ProgressBar1.Min := 0;
    ProgressBar1.Max := TotalImages;
    ProgressBar1.Value := 0;
    ProgressBar1.Visible := True;

    // Set FlowLayout1's width to accommodate all images
    FlowLayoutWidth := (ImageWidth + HorizontalSpacing) * TotalImages;
    FlowLayout1.Width := FlowLayoutWidth;
    FlowLayout1.Height := ImageHeight;

    FlowLayout1.BeginUpdate;
    FirstLayout := nil;
    try
      for CardIndex := 0 to TotalImages - 1 do
      begin
        Card := AllCards[CardIndex];
        CardDataList.Add(Card);
        if Card.ImageUris.Small <> '' then
        begin
          DisplayImageFromURLAsync(Card.ImageUris.Small, FlowLayout1, Card);

          if FirstLayout = nil then
            FirstLayout :=
              TLayout(FlowLayout1.Controls[FlowLayout1.ControlsCount - 1]);
        end;

        // Update ProgressBar
        ProgressBar1.Value := CardIndex + 1;

        // Remove Application.ProcessMessages to prevent re-entrancy issues
      end;
    finally
      FlowLayout1.EndUpdate;
      // Hide ProgressBar after processing
      ProgressBar1.Visible := False;
    end;
  finally
    ScryfallAPI.Free;
    // Removed CardDataList.Free to prevent double-free
  end;

  // Update label with the count of found cards
  Label2.Text := 'Cards Found: ' + IntToStr(TotalImages);

  // Show details for the first loaded card, if available
  if Assigned(FirstLayout) then
    ShowCardDetails(FirstLayout);
end;

procedure TForm1.DisplayImageFromURLAsync(const URL: string;
FlowLayout: TFlowLayout; const CardDetails: TCardDetails);
var
  Layout: TCardLayout; // Use TCardLayout for proper memory management
  ImageControl: TImageControl;
  CardDetailsObject: TCardDetailsObject;
begin
  // Create a new layout for each card
  Layout := TCardLayout.Create(FlowLayout);
  Layout.Parent := FlowLayout;
  Layout.Width := 175;
  Layout.Height := 245;
  Layout.Align := TAlignLayout.Left;
  Layout.Margins.Left := 5;
  Layout.Margins.Right := 5;
  Layout.HitTest := True;

  // Store card details in the layout for reference
  CardDetailsObject := TCardDetailsObject.Create(CardDetails);
  Layout.TagObject := CardDetailsObject;
  Layout.OnClick := ShowCardDetails;

  // Create the image control to display card image
  ImageControl := TImageControl.Create(Layout);
  ImageControl.Parent := Layout;
  ImageControl.Align := TAlignLayout.Client;
  ImageControl.HitTest := False;

  // Load image asynchronously
  LoadCachedOrDownloadImageAsync(URL, ImageControl);
end;


function TForm1.FetchRulingsAsync(const cardId: string): TArray<TRuling>;
var
  ScryfallAPI: TScryfallAPI;
begin
  ScryfallAPI := TScryfallAPI.Create;
  try
    Result := ScryfallAPI.GetRulingsByScryfallID(cardId);
  finally
    ScryfallAPI.Free;
  end;
end;


procedure TForm1.AddTextToHighlightRectangle(Rectangle: TRectangle;
const Text: string);
var
  TextControl: TText;

begin
  // Create the text control
  TextControl := TText.Create(Rectangle);
  TextControl.Parent := Rectangle;
  TextControl.Align := TAlignLayout.Bottom;
  TextControl.Text := Text;
  TextControl.HitTest := False;
  TextControl.TextSettings.FontColor := TAlphaColors.White;
  TextControl.TextSettings.HorzAlign := TTextAlign.Center;
  TextControl.Margins.Bottom := -15;
  TextControl.TextSettings.Font.Size := 14;
  TextControl.TextSettings.Font.Style := [TFontStyle.fsBold];
end;

procedure TForm1.ShowCardDetails(Sender: TObject);
var
  Layout: TLayout;
  BorderRect: TRectangle;
  NormalImageURL: string;
  HighlightAnimation: TFloatAnimation;

  procedure ClearAllHighlights;
  var
    i, j: Integer;
    ChildLayout: TControl;
    ChildControl: TControl;
    ExistingBorder: TRectangle;
  begin
    for i := 0 to FlowLayout1.ControlsCount - 1 do
    begin
      ChildLayout := FlowLayout1.Controls[i];
      if ChildLayout is TLayout then
      begin
        for j := 0 to TLayout(ChildLayout).ControlsCount - 1 do
        begin
          ChildControl := TLayout(ChildLayout).Controls[j];
          if (ChildControl is TRectangle) and
            (TRectangle(ChildControl).Name = 'HighlightBorder') then
          begin
            ExistingBorder := TRectangle(ChildControl);
            ExistingBorder.Free;
            Break;
          end;
        end;
      end;
    end;
  end;

begin
  if Sender is TLayout then
  begin
    Layout := TLayout(Sender);

    if Layout.TagObject is TCardDetailsObject then
    begin
      FCardDetailsObject := TCardDetailsObject(Layout.TagObject);
      ClearAllHighlights;

      BorderRect := TRectangle.Create(Layout);
      BorderRect.Name := 'HighlightBorder';
      BorderRect.Parent := Layout;
      BorderRect.Align := TAlignLayout.Contents;
      BorderRect.Stroke.Thickness := 4;
      BorderRect.Stroke.Color := TAlphaColors.Red;
      BorderRect.Fill.Kind := TBrushKind.None;
      AddTextToHighlightRectangle(BorderRect,
        FCardDetailsObject.CardDetails.CardName);
      BorderRect.Hint := FCardDetailsObject.CardDetails.CardName;

      // Add animation to the border
      HighlightAnimation := TFloatAnimation.Create(BorderRect);
      HighlightAnimation.Parent := BorderRect;
      HighlightAnimation.PropertyName := 'Stroke.Thickness';
      HighlightAnimation.StartValue := 3;
      HighlightAnimation.StopValue := 6;
      HighlightAnimation.Duration := 0.5;
      HighlightAnimation.AutoReverse := True;
      HighlightAnimation.Enabled := True;

     TTask.Run(
  procedure
  var
    Rulings: TArray<TRuling>;
  begin
    // Fetch rulings asynchronously to avoid blocking the UI
    Rulings := FetchRulingsAsync(FCardDetailsObject.CardDetails.SFID);

    // Update the UI on the main thread
    TThread.Queue(nil,
      procedure
      begin
        DisplayCardInBrowser(FCardDetailsObject.CardDetails, Rulings);
      end);
  end);
      NormalImageURL := FCardDetailsObject.CardDetails.ImageUris.Normal;
      ShowHighResButton.Enabled := NormalImageURL <> '';
      ShowHighResButton.TagString := NormalImageURL;
      CardTitle := FCardDetailsObject.CardDetails.CardName;
    end
    else
      ShowMessage('Error: Invalid data type stored in TagObject.');
  end
  else
    ShowMessage('Error: Sender is not a TLayout.');
end;

function TForm1.GetStatusClass(const LegalityStatus: string): string;
begin
  if SameText(LegalityStatus, 'legal') then
    Result := 'legal'
  else if SameText(LegalityStatus, 'not_legal') then
    Result := 'not-legal'
  else if SameText(LegalityStatus, 'banned') then
    Result := 'banned'
  else if SameText(LegalityStatus, 'restricted') then
    Result := 'restricted'
  else
    Result := 'unknown';
end;


procedure TForm1.DisplayCardInBrowser(const CardDetails: TCardDetails; const Rulings: TArray<TRuling>);
var
  HTMLContent: TStringBuilder;
  FormattedOracleText, FormattedManaCost: string;
  RttiContext: TRttiContext;
  RttiType: TRttiType;
  RttiField: TRttiField;
  LegalityName, LegalityStatus: string;
  StatusClass: string;
  LegalitiesPtr: Pointer;
  i: Integer;
begin
  // Replace mana symbols in OracleText and ManaCost
  FormattedOracleText := ReplaceManaSymbolsWithImages(CardDetails.OracleText);
  FormattedManaCost := ReplaceManaSymbolsWithImages(CardDetails.ManaCost);

  // Build the HTML content
  HTMLContent := TStringBuilder.Create;
  try
    HTMLContent.AppendLine('<html>');
    HTMLContent.AppendLine('<head>');
    HTMLContent.AppendLine('<style>');
    HTMLContent.AppendLine('body { font-family: Arial, sans-serif; margin: 10px; background-color: #272727; color: white; }');
    HTMLContent.AppendLine('h1 { color: #1E90FF; }');
    HTMLContent.AppendLine('p { font-size: 14px; line-height: 1.5; }');
    HTMLContent.AppendLine('img { width: 300px; display: block; margin-bottom: 10px; }');
    HTMLContent.AppendLine('.legalities { margin-top: 20px; }');
    HTMLContent.AppendLine('.legalities th, .legalities td { padding: 5px; text-align: left; }');
    HTMLContent.AppendLine('.legal { color: green; }');
    HTMLContent.AppendLine('.not-legal { color: gray; }');
    HTMLContent.AppendLine('.banned { color: red; }');
    HTMLContent.AppendLine('.restricted { color: orange; }');
    HTMLContent.AppendLine('.unknown { color: black; }');
    HTMLContent.AppendLine('.rulings { margin-top: 20px; }');
    HTMLContent.AppendLine('.rulings h2 { color: #FFD700; }');
    HTMLContent.AppendLine('.rulings p { font-size: 12px; line-height: 1.4; }');
    HTMLContent.AppendLine('</style>');
    HTMLContent.AppendLine('</head>');
    HTMLContent.AppendLine('<body>');

    // Card Name
    HTMLContent.AppendLine(Format('<h1>%s</h1>', [TNetEncoding.HTML.Encode(CardDetails.CardName)]));

    // Card Details
    HTMLContent.AppendLine(Format('<p><strong>Type:</strong> %s</p>', [TNetEncoding.HTML.Encode(CardDetails.TypeLine)]));
    HTMLContent.AppendLine(Format('<p><strong>Mana Cost:</strong> %s</p>', [FormattedManaCost]));

    // Include Power/Toughness if available
    if (CardDetails.Power <> '') and (CardDetails.Toughness <> '') then
    begin
      HTMLContent.AppendLine(Format('<p><strong>Power/Toughness:</strong> %s/%s</p>',
        [TNetEncoding.HTML.Encode(CardDetails.Power), TNetEncoding.HTML.Encode(CardDetails.Toughness)]));
    end;

    // Oracle Text
    HTMLContent.AppendLine(Format('<p>%s</p>', [FormattedOracleText])); // Removed <strong> to allow formatting

    // Legalities Section
    HTMLContent.AppendLine('<div class="legalities">');
    HTMLContent.AppendLine('<h2>Legalities</h2>');
    HTMLContent.AppendLine('<table>');
    HTMLContent.AppendLine('<tr><th>Format</th><th>Status</th></tr>');

    // Use RTTI to iterate over the fields of the Legalities record
    RttiContext := TRttiContext.Create;
    try
      RttiType := RttiContext.GetType(TypeInfo(TCardLegalities));

      // Get a pointer to the Legalities record
      LegalitiesPtr := @CardDetails.Legalities;

      // Iterate over all fields in the record
      for RttiField in RttiType.GetFields do
      begin
        LegalityName := RttiField.Name;
        LegalityStatus := RttiField.GetValue(LegalitiesPtr).AsString;

        // Only process if LegalityStatus is not empty
        if LegalityStatus <> '' then
        begin
          // Format the legality name for display
          LegalityName := LegalityName.Replace('_', ' ');
          if Length(LegalityName) > 0 then
            LegalityName := AnsiUpperCase(LegalityName[1]) + LowerCase(Copy(LegalityName, 2, MaxInt));

          // Determine the CSS class based on LegalityStatus
          StatusClass := GetStatusClass(LegalityStatus);

          // Optionally, capitalize the status for better display
          if SameText(LegalityStatus, 'not_legal') then
            LegalityStatus := 'Not Legal'
          else
            LegalityStatus := TNetEncoding.HTML.Encode(LegalityStatus);

          // Add the row with the CSS class
          HTMLContent.AppendLine(Format('<tr><td>%s</td><td class="%s">%s</td></tr>',
            [LegalityName, StatusClass, LegalityStatus]));
        end;
      end;
    finally
      RttiContext.Free;
    end;

    HTMLContent.AppendLine('</table>');
    HTMLContent.AppendLine('</div>');

    // **Rulings Section**
    if Length(Rulings) > 0 then
    begin
      HTMLContent.AppendLine('<div class="rulings">');
      HTMLContent.AppendLine('<h2>Rulings</h2>');
      for i := 0 to High(Rulings) do
      begin
        // Format the ruling date to a more readable format if necessary
        HTMLContent.AppendLine(Format('<p><em>%s:</em> %s</p>',
          [TNetEncoding.HTML.Encode(Rulings[i].PublishedAt), // Assuming PublishedAt is a string date
           TNetEncoding.HTML.Encode(Rulings[i].Comment)]));
      end;
      HTMLContent.AppendLine('</div>');
    end;

    HTMLContent.AppendLine('</body>');
    HTMLContent.AppendLine('</html>');

    // Load the HTML into TWebBrowser on the main thread
    TThread.Queue(nil,
      procedure
      begin
        WebBrowser1.LoadFromStrings(HTMLContent.ToString, Application.Name);
      end);
  finally
    HTMLContent.Free;
  end;
end;

function TForm1.ReplaceManaSymbolsWithImages(const OracleText: string): string;
var
  Parts: TArray<string>;
  Part: string;
  ImageFileName: string;
  ImagePath, ImageBase64: string;
  ImageMimeType: string;
begin
  Result := ''; // Initialize the result string
  Parts := ParseTextWithSymbolsManual(OracleText);
  // Split OracleText into parts

  for Part in Parts do
  begin
    if Part.StartsWith('{') and Part.EndsWith('}') then
    begin
      // Look up the mana symbol image file
      if FManaSymbolMap.TryGetValue(Part, ImageFileName) then
        ImagePath := GetIconPath(ImageFileName)
      else
        ImagePath := GetIconPath('default.png'); // Fallback to default image

      // Validate the image file
      if not TFile.Exists(ImagePath) then
        ImagePath := GetIconPath('default.png');

      // Convert the image to Base64
      ImageBase64 := ImageToBase64(ImagePath);
      if ImageBase64 = '' then
        Continue; // Skip if conversion failed

      // Determine the MIME type based on the file extension
      if ImagePath.EndsWith('.png', True) then
        ImageMimeType := 'image/png'
      else if ImagePath.EndsWith('.jpg', True) or
        ImagePath.EndsWith('.jpeg', True) then
        ImageMimeType := 'image/jpeg'
      else if ImagePath.EndsWith('.gif', True) then
        ImageMimeType := 'image/gif'
      else
        ImageMimeType := 'application/octet-stream'; // Default MIME type

      // Construct the data URI
      ImagePath := Format('data:%s;base64,%s', [ImageMimeType, ImageBase64]);

      // Append the <img> tag with adjusted vertical alignment
      Result := Result +
        Format('<img src="%s" alt="%s" style="display:inline; width:16px; height:16px; vertical-align:-12px;">',
        [ImagePath, TNetEncoding.HTML.Encode(Part)]);
    end
    else
    begin
      // Append plain text, properly encoded
      Result := Result + TNetEncoding.HTML.Encode(Part);
    end;
  end;
end;

procedure TForm1.ShowHighResButtonClick(Sender: TObject);
var
  HighResURL: string;
  HighResImageForm: THighResImageForm; // Declare it here
begin
  HighResURL := ShowHighResButton.TagString;
  // Ensure TagString is properly assigned elsewhere

  if HighResURL <> '' then
  begin
    HighResImageForm := THighResImageForm.Create(Self);
    // Set Owner as Self (main form)
    try
      HighResImageForm.ShowImage(HighResURL, CardTitle);
      // ShowImage handles ShowModal

    finally
      HighResImageForm.Free; // Free the form after it's closed
    end;
  end
  else
    ShowMessage('No high-resolution image available.');
end;

procedure TForm1.FilterDisplayedCards(const FilterText: string);
var
  i: Integer;
  Layout: TLayout;
  CardDetails: TCardDetails;
  LowerFilter: string;
begin
  // Convert FilterText to lowercase once to optimize performance
  LowerFilter := Trim(LowerCase(FilterText));

  FlowLayout1.BeginUpdate;
  try
    for i := 0 to FlowLayout1.ControlsCount - 1 do
    begin
      // Safely cast the control to TLayout
      if not(FlowLayout1.Controls[i] is TLayout) then
        Continue;

      Layout := TLayout(FlowLayout1.Controls[i]);

      // Check if the TagObject contains valid card details
      if not(Layout.TagObject is TCardDetailsObject) then
        Continue;

      CardDetails := TCardDetailsObject(Layout.TagObject).CardDetails;

      // If no filter is provided, show all cards
      if LowerFilter.IsEmpty then
      begin
        Layout.Visible := True;
        Continue;
      end;

      // Check if any of the card's properties contain the filter text
      Layout.Visible := AnsiContainsText(CardDetails.CardName, LowerFilter) or
        AnsiContainsText(CardDetails.TypeLine, LowerFilter) or
        AnsiContainsText(CardDetails.OracleText, LowerFilter);
    end;
  finally
    FlowLayout1.EndUpdate;
  end;
end;

procedure TForm1.GetSelectedCardRulings(const cardId: string);
var
  ScryfallAPI: TScryfallAPI;
  Rulings: TArray<TRuling>;
  i: Integer;
begin
  if not Assigned(FCardDetailsObject) then
  begin
    ShowMessage('No card is selected.');
    Exit;
  end;

  ScryfallAPI := TScryfallAPI.Create;
  try
    // Use the Scryfall ID of the card
    Rulings := ScryfallAPI.GetRulingsByScryfallID(cardId);
    if Length(Rulings) = 0 then
    begin
      ShowMessage('No rulings available for this card.');
      Exit;
    end;
    for i := 0 to High(Rulings) do
    begin
      ShowMessage('Ruling: ' + Rulings[i].Comment);
    end;
  finally
    ScryfallAPI.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  Label1.Text := '';
  if Trim(Edit1.Text).IsEmpty <> True then
  begin
    Label1.Text := '';
    DisplayCardArtworks(Trim(Edit1.Text.Trim));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Filter for cards
  FilterDisplayedCards(Trim(Edit2.Text));

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FilterDisplayedCards(''); // Passing an empty string displays all cards
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Assigned(FCardDetailsObject) then
  begin
    if DataModule1.CheckCardExists(Trim(FCardDetailsObject.CardDetails.SFID))
    then
      ShowMessage('Card already exists in the database.')
    else
      SaveSelectedCardToDatabase;
  end
  else
    ShowMessage('No card is selected.');
end;

procedure TForm1.ShowCardCollection;
var
  CollectionForm: TCollectionsForm;
begin
  CollectionForm := TCollectionsForm.Create(Self);
  try
    CollectionForm.ShowModal;
  finally
    CollectionForm.Free;
  end;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ShowCardCollection;
  // form2.Show;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if not Assigned(FCardDetailsObject) then
  begin
    ShowMessage('No card is selected.');
    Exit;
  end;

  GetSelectedCardRulings(FCardDetailsObject.CardDetails.SFID);
end;

procedure TForm1.SaveSelectedCardToDatabase;
var
  ImageStream: TMemoryStream;
  ImageBitmap: TBitmap;
begin
  if not Assigned(FCardDetailsObject) then
  begin
    ShowMessage('No card is selected.');
    Exit;
  end;

  ImageStream := TMemoryStream.Create;
  ImageBitmap := TBitmap.Create;
  try
    // Load the selected card's image from the cache
    if FileExists(GetCachedImagePath(FCardDetailsObject.CardDetails.ImageUris.
      Small)) then
    begin
      ImageBitmap.LoadFromFile
        (GetCachedImagePath(FCardDetailsObject.CardDetails.ImageUris.Small));

      // Convert Bitmap to stream
      if ImageBitmap.Width > 0 then
      begin
        ImageBitmap.SaveToStream(ImageStream);
        ImageStream.Position := 0;

        // Call the data module to save card details and image
        DataModule1.SaveCardToDatabase(FCardDetailsObject.CardDetails,
          ImageStream, 1);

      end
      else
        ShowMessage('Selected card image is invalid.');
    end
    else
      ShowMessage('Image not found in cache.');
  finally
    ImageStream.Free;
    ImageBitmap.Free;
  end;
end;

function TForm1.GetDatabasePath: string;
begin
  // Get the path to the internal documents directory
  Result := TPath.Combine(TPath.GetDocumentsPath, 'Collection.db');
end;

procedure TForm1.CopyDatabaseToInternalStorage;
var
  SourcePath, DestinationPath: string;
begin
  DestinationPath := GetDatabasePath;

  if not TFile.Exists(DestinationPath) then
  begin
    // Get the path to the assets directory
    SourcePath := TPath.Combine(TPath.GetHomePath,
      'assets\internal\Collection.db');

    try
      TFile.Copy(SourcePath, DestinationPath);
    except
      on E: Exception do
        ShowMessage('Error copying database: ' + E.Message);
    end;
  end;
end;

// Get the application’s base path
procedure TForm1.InitializeManaSymbolMap;
begin
  FManaSymbolMap := TDictionary<string, string>.Create;

  // Basic Mana Symbols
  FManaSymbolMap.Add('{T}', '{T}.png');
  FManaSymbolMap.Add('{W}', '{W}.png');
  FManaSymbolMap.Add('{U}', '{U}.png');
  FManaSymbolMap.Add('{B}', '{B}.png');
  FManaSymbolMap.Add('{R}', '{R}.png');
  FManaSymbolMap.Add('{G}', '{G}.png');
  FManaSymbolMap.Add('{C}', '{C}.png');
  FManaSymbolMap.Add('{X}', '{X}.png');
  FManaSymbolMap.Add('{A}', '{A}.png');

  // Generic Mana Costs
  for var i := 0 to 20 do
    FManaSymbolMap.Add('{' + i.ToString + '}', '{' + i.ToString + '}.png');
  FManaSymbolMap.Add('{½}', '{½}.png');
  FManaSymbolMap.Add('{∞}', '{∞}.png');
  FManaSymbolMap.Add('{100}', '{100}.png');
  FManaSymbolMap.Add('{1000000}', '{1000000}.png');

  // Hybrid Mana Symbols
  FManaSymbolMap.Add('{W/U}', '{W_U}.png');
  FManaSymbolMap.Add('{W/B}', '{W_B}.png');
  FManaSymbolMap.Add('{U/B}', '{U_B}.png');
  FManaSymbolMap.Add('{U/R}', '{U_R}.png');
  FManaSymbolMap.Add('{B/R}', '{B_R}.png');
  FManaSymbolMap.Add('{B/G}', '{B_G}.png');
  FManaSymbolMap.Add('{R/G}', '{R_G}.png');
  FManaSymbolMap.Add('{R/W}', '{R_W}.png');
  FManaSymbolMap.Add('{G/W}', '{G_W}.png');
  FManaSymbolMap.Add('{G/U}', '{G_U}.png');

  // Phyrexian Mana Symbols
  FManaSymbolMap.Add('{W/P}', '{W_P}.png');
  FManaSymbolMap.Add('{U/P}', '{U_P}.png');
  FManaSymbolMap.Add('{B/P}', '{B_P}.png');
  FManaSymbolMap.Add('{R/P}', '{R_P}.png');
  FManaSymbolMap.Add('{G/P}', '{G_P}.png');

  // Miscellaneous Symbols
  FManaSymbolMap.Add('{Q}', '{Q}.png'); // Untap symbol
  FManaSymbolMap.Add('{S}', '{S}.png'); // Snow symbol
end;

function TForm1.GetAppPath: string;
begin
  Result := TPath.GetDirectoryName(ParamStr(0));
end;

function TForm1.GetIconPath(const FileName: string): string;
begin
  Result := TPath.Combine(GetAppPath, TPath.Combine('MTGIconsPNG', FileName));
end;

function TForm1.ParseTextWithSymbolsManual(const Input: string): TArray<string>;
var
  i, j: Integer;
  PartsList: TList<string>;
  CurrentText, Symbol: string;
begin
  PartsList := TList<string>.Create;
  try
    i := 1;
    CurrentText := '';

    while i <= Length(Input) do
    begin
      if Input[i] = '{' then
      begin
        // Add current text before the symbol
        if CurrentText <> '' then
        begin
          PartsList.Add(CurrentText);
          CurrentText := '';
        end;

        // Find the closing brace
        j := i + 1;
        while (j <= Length(Input)) and (Input[j] <> '}') do
          Inc(j);

        if j <= Length(Input) then
        begin
          // Add the symbol including braces
          Symbol := Copy(Input, i, j - i + 1);
          PartsList.Add(Symbol);
          i := j + 1;
        end
        else
        begin
          // No closing brace found, treat as normal text
          CurrentText := CurrentText + Input[i];
          Inc(i);
        end;
      end
      else
      begin
        CurrentText := CurrentText + Input[i];
        Inc(i);
      end;
    end;

    // Add any remaining text
    if CurrentText <> '' then
      PartsList.Add(CurrentText);

    Result := PartsList.ToArray;
  finally
    PartsList.Free;
  end;
end;

initialization

Randomize;

end.
