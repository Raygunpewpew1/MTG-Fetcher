unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, System.IOUtils, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.Objects,
  FMX.ExtCtrls, FMX.Ani, FMX.Edit, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.WebBrowser, FireDAC.Stan.Intf, FireDAC.Phys.Intf,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, FireDAC.Stan.Param, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, Data.DB, FMX.Skia, System.Net.HttpClient,
  System.Hash, ScryfallAPIWrapperV2, SGlobalsZ, DataModuleUnit,HighResForm,

  CardCollectionForm, MLogic, System.JSON, REST.JSON, System.StrUtils,
  System.TypInfo, System.NetEncoding, Math, System.Threading,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView;

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
    Edit1: TEdit;
    Button4: TButton;
    Button5: TButton;
    ShowHighResButton: TButton;
    ProgressBar1: TProgressBar;
    WebBrowser1: TWebBrowser;
    DelayTimer: TTimer;
    ListViewCards: TListView;
    StyleBook1: TStyleBook;
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
    procedure ListViewCardsItemClick(const Sender: TObject;
      const AItem: TListViewItem);

    // procedure ListView1ItemClick(const Sender: TObject;
    // const AItem: TListViewItem);
  private
    HttpClient: THTTPClient;
    FCardDetailsObject: TCardDetailsObject; // Private field
    CardTitle: string; // Private field
    CardDataList: TList<TCardDetails>;
    // Encapsulated within the form
    CardCount: Integer; // Encapsulated within the form
    AppClose: Boolean; // Encapsulated within the form

    BrIsLoaded: Boolean;
    procedure DisplayCardArtworks(const CardName: string);
    procedure DisplayImageFromURLAsync(const URL: string;
      FlowLayout: TFlowLayout; const CardDetails: TCardDetails);
    procedure ShowCardDetails(Sender: TObject);

    procedure LoadCachedOrDownloadImage(const URL: string; Bitmap: TBitmap);
    // Changed parameter to TBitmap
    // procedure FilterDisplayedCards(const FilterText: string);
    procedure LoadCachedOrDownloadImageAsync(const URL: string;
      ImageControl: TImageControl);
    // procedure AddTextToHighlightRectangle(Rectangle: TRectangle;
    // const Text: string);
    procedure SaveSelectedCardToDatabase;
    procedure ShowCardCollection;
    procedure GetSelectedCardRulings(const cardId: string);

    procedure DisplayCardInBrowser(const CardDetails: TCardDetails;
      const Rulings: TArray<TRuling>);

    function FetchRulingsAsync(const cardId: string): TArray<TRuling>;
    procedure AddCardToListView(const Card: TCardDetails);
    procedure LoadImageToListViewItemAsync(const URL: string;
      ListViewItem: TListViewItem);
    procedure ResizeListViewImage(NewWidth, NewHeight: Single);

  public
    // Public declarations
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiPh.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.LgXhdpiTb.fmx ANDROID}
{$R *.XLgXhdpiTb.fmx ANDROID}

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
  // DataModule1.CreateCardDetailsTable;
  CardCount := 0;
  AppClose := False;
  DataModule1.SetupDatabaseConnection(GetDatabasePath);
  CopyDatabaseToInternalStorage;
  // InitializeManaSymbolMap;
  FBase64ImageCache := TDictionary<string, string>.Create;
  BrIsLoaded := False;
  // DelayTimer.Enabled := True;
  WebBrowser1.URL := 'about:blank';
  // ListViewCards.ItemAppearanceName := 'ImageListItem';
  ListViewCards.OnItemClick := ListViewCardsItemClick;

  // Form1.StyleBook := HighResForm.HighResImageForm.StyleBook1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AppClose := True;
  if Assigned(HttpClient) then
    FreeAndNil(HttpClient);
  if Assigned(CardDataList) then
    FreeAndNil(CardDataList);
  // FreeAndNil(FManaSymbolMap);
  FreeAndNil(FBase64ImageCache);
end;

procedure TForm1.ListViewCardsItemClick(const Sender: TObject;
  const AItem: TListViewItem);
// var
// CardDetailsObject: TCardDetailsObject;
begin
  if Assigned(AItem) and Assigned(AItem.TagObject) and
    (AItem.TagObject is TCardDetailsObject) then
  begin
    // Retrieve the card details from the item's TagObject
    // CardDetailsObject := TCardDetailsObject(AItem.TagObject);
    // Call ShowCardDetails with the selected card details
    ShowCardDetails(AItem);
  end
  else
    ShowMessage('No card details are available for this item.');
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
  if BrIsLoaded = True then
    Exit;

  // FlowLayout1.Enabled := False;
  Edit1.Enabled := False;
  PopularCards := TStringList.Create;
  try
    SetupPopularCards(PopularCards);
    if PopularCards.Count > 0 then
      DisplayCardArtworks(PopularCards[Random(PopularCards.Count)]);
    BrIsLoaded := True;
    // WebBrowser1.Reload;
  finally
    PopularCards.Free;
  end;

  // FlowLayout1.Enabled := True;
  Edit1.Enabled := True;

  // DelayTimer.Enabled := False;

end;

procedure TForm1.DisplayCardArtworks(const CardName: string);
var
  ScryfallAPI: TScryfallAPI;
  AllCards: TArray<TCardDetails>;
  Card: TCardDetails;
  CardIndex: Integer;
begin
  ScryfallAPI := TScryfallAPI.Create;
  try
    // Fetch all card results using SearchAllCards
    AllCards := ScryfallAPI.SearchAllCards(CardName, '', '', '', False, False);

    if Length(AllCards) = 0 then
    begin
      ShowMessage('No results found for "' + CardName + '".');
      Exit;
    end;

    if Length(AllCards) > 800 then
    begin
      ShowMessage('Too many results. Please refine your search.');
      Exit;
    end;

    // Clear ListView and internal card list
    ListViewCards.Items.Clear;
    CardDataList.Clear;

    ProgressBar1.Min := 0;
    ProgressBar1.Max := Length(AllCards);
    ProgressBar1.Value := 0;
    ProgressBar1.Visible := True;

    for CardIndex := 0 to High(AllCards) do
    begin
      Card := AllCards[CardIndex];
      CardDataList.Add(Card); // Store card details in memory
      AddCardToListView(Card);

      ProgressBar1.Value := CardIndex + 1;
    end;

    // Label2.Text := Format('Found: %d cards', [Length(AllCards)]);

    // Automatically select and display the first item
    if ListViewCards.Items.Count > 0 then
    begin
      ListViewCards.Selected := ListViewCards.Items[0];
      ShowCardDetails(ListViewCards.Items[0]);
    end;

  finally
    ResizeListViewImage(64, 64);
    ProgressBar1.Visible := False;
    ScryfallAPI.Free;
  end;
end;

procedure TForm1.AddCardToListView(const Card: TCardDetails);
var
  ListViewItem: TListViewItem;
begin
  // Add a new item to the ListView
  ListViewItem := ListViewCards.Items.Add;

  // Set item text (card name)
  ListViewItem.Text := Card.CardName;

  // Store the card details in the item's TagObject
  ListViewItem.TagObject := TCardDetailsObject.Create(Card);

  // Load and assign the card art crop image as the item's background
  if Card.ImageUris.art_crop <> '' then
    LoadImageToListViewItemAsync(Card.ImageUris.art_crop, ListViewItem);
end;

procedure TForm1.LoadImageToListViewItemAsync(const URL: string;
ListViewItem: TListViewItem);
begin
  TTask.Run(
    procedure
    var
      Bitmap: TBitmap;
    begin
      Bitmap := TBitmap.Create;
      try
        // Download or load the image from cache
        LoadCachedOrDownloadImage(URL, Bitmap);

        // Update the ListView item's Bitmap on the main thread
        TThread.Synchronize(nil,
          procedure
          begin
            ListViewItem.Bitmap := Bitmap;
            // Assign the image as the background
          end);
      finally
        Bitmap.Free;
      end;
    end);
end;

procedure TForm1.DisplayImageFromURLAsync(const URL: string;
FlowLayout: TFlowLayout; const CardDetails: TCardDetails);
var
  Layout: TCardLayout; // Use TCardLayout for proper memory management
  ImageControl: TImageControl;
  CardDetailsObject: TCardDetailsObject;
  CName: TLabel;
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

  CName := TLabel.Create(Layout);
  CName.Parent := Layout;
  CName.Align := TAlignLayout.Bottom;
  CName.Text := CardDetails.CardName;

  CName.TextSettings.Font.Style := [TFontStyle.fsBold];
  CName.TextSettings.HorzAlign := TTextAlign.Center;
  CName.Margins.Bottom := -15;
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

// procedure TForm1.AddTextToHighlightRectangle(Rectangle: TRectangle;
// const Text: string);
// var
// TextControl: TText;
//
// begin
// // Create the text control
// TextControl := TText.Create(Rectangle);
// TextControl.Parent := Rectangle;
// TextControl.Align := TAlignLayout.Bottom;
// TextControl.Text := Text;
// TextControl.HitTest := False;
// TextControl.TextSettings.FontColor := TAlphaColors.White;
// TextControl.TextSettings.HorzAlign := TTextAlign.Center;
// TextControl.Margins.Bottom := -15;
// TextControl.TextSettings.Font.Size := 14;
// TextControl.TextSettings.Font.Style := [TFontStyle.fsBold];
// end;

procedure TForm1.ShowCardDetails(Sender: TObject);
var
  SelectedItem: TListViewItem;
  CardDetailsObject: TCardDetailsObject;
  SelectedCard: TCardDetails;
begin
  // Ensure the sender is a TListViewItem
  if Sender is TListViewItem then
  begin
    SelectedItem := TListViewItem(Sender);

    // Check if the TagObject contains card details
    if Assigned(SelectedItem.TagObject) and
      (SelectedItem.TagObject is TCardDetailsObject) then
    begin
      CardDetailsObject := TCardDetailsObject(SelectedItem.TagObject);
      SelectedCard := CardDetailsObject.CardDetails;
      FCardDetailsObject := CardDetailsObject;

      // Enable the high-res button
      ShowHighResButton.Enabled := SelectedCard.ImageUris.Normal <> '';
      ShowHighResButton.TagString := SelectedCard.ImageUris.Normal;
      CardTitle := SelectedCard.CardName;

      // Avoid immediately fetching and displaying rulings
      TThread.Queue(nil,
        procedure
        begin
          WebBrowser1.LoadFromStrings
            ('<html><body><p>Loading card details...</p></body></html>',
            Application.Name);
          DisplayCardInBrowser(SelectedCard, []);
          // Empty rulings by default
        end);
    end
    else
      ShowMessage('Error: No card details available for this item.');
  end
  else
    ShowMessage('Error: Sender is not a TListViewItem.');
end;

procedure TForm1.DisplayCardInBrowser(const CardDetails: TCardDetails;
const Rulings: TArray<TRuling>);
var
  HTMLContent: TStringBuilder;
  FormattedOracleText, FormattedManaCost, LegalityName, LegalityStatus,
    StatusClass: string;
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
    HTMLContent.AppendLine
      ('body { font-family: Arial, sans-serif; margin: 10px; background-color: #000000; color: white; }');
    HTMLContent.AppendLine('h1 { color: #1E90FF; }');
    HTMLContent.AppendLine('p { font-size: 14px; line-height: 1.5; }');
    HTMLContent.AppendLine
      ('img { width: 300px; display: block; margin-bottom: 10px; }');
    HTMLContent.AppendLine('.legalities { margin-top: 20px; }');
    HTMLContent.AppendLine
      ('.legalities th, .legalities td { padding: 5px; text-align: left; }');
    HTMLContent.AppendLine('.legal { color: green; }');
    HTMLContent.AppendLine('.not-legal { color: gray; }');
    HTMLContent.AppendLine('.banned { color: red; }');
    HTMLContent.AppendLine('.restricted { color: orange; }');
    HTMLContent.AppendLine('.unknown { color: black; }');
    HTMLContent.AppendLine('.rulings { margin-top: 20px; }');
    HTMLContent.AppendLine('.rulings h2 { color: #FFD700; }');
    HTMLContent.AppendLine('.rulings p { font-size: 12px; line-height: 1.4; }');
    HTMLContent.AppendLine('</style>');
    HTMLContent.AppendLine('<script>');
    // JavaScript to disable the right-click menu
    HTMLContent.AppendLine
      ('document.addEventListener("contextmenu", function(e) { e.preventDefault(); });');
    HTMLContent.AppendLine('</script>');
    HTMLContent.AppendLine('</head>');
    HTMLContent.AppendLine('<body>');

    // Card Name
    HTMLContent.AppendLine(Format('<h1>%s</h1>',
      [TNetEncoding.HTML.Encode(CardDetails.CardName)]));

    // Card Details
    HTMLContent.AppendLine(Format('<p><strong>Type:</strong> %s</p>',
      [TNetEncoding.HTML.Encode(CardDetails.TypeLine)]));
    HTMLContent.AppendLine(Format('<p><strong>Mana Cost:</strong> %s</p>',
      [FormattedManaCost]));

    // Include Power/Toughness if available
    if (CardDetails.Power <> '') and (CardDetails.Toughness <> '') then
    begin
      HTMLContent.AppendLine
        (Format('<p><strong>Power/Toughness:</strong> %s/%s</p>',
        [TNetEncoding.HTML.Encode(CardDetails.Power),
        TNetEncoding.HTML.Encode(CardDetails.Toughness)]));
    end;

    // Oracle Text
    HTMLContent.AppendLine(Format('<p>%s</p>', [FormattedOracleText]));

    // Legalities Section
    HTMLContent.AppendLine('<div class="legalities">');
    HTMLContent.AppendLine('<h2>Legalities</h2>');
    HTMLContent.AppendLine('<table>');
    HTMLContent.AppendLine('<tr><th>Format</th><th>Status</th></tr>');

    // Iterate over legalities array
    for i := Low(LegalitiesArray) to High(LegalitiesArray) do
    begin
      LegalityName := LegalitiesArray[i];

      // Access the corresponding field value directly
      LegalityStatus := GetLegalStatus(CardDetails.Legalities, LegalityName);

      // Only process if LegalityStatus is not empty
      if LegalityStatus <> '' then
      begin
        // Format the legality name for display
        LegalityName := StringReplace(LegalityName, '_', ' ', [rfReplaceAll]);
        if Length(LegalityName) > 0 then
          LegalityName := AnsiUpperCase(LegalityName[1]) +
            LowerCase(Copy(LegalityName, 2, MaxInt));

        // Determine the CSS class based on LegalityStatus
        StatusClass := GetStatusClass(LegalityStatus);

        // Add the row with the CSS class
        HTMLContent.AppendLine
          (Format('<tr><td>%s</td><td class="%s">%s</td></tr>',
          [LegalityName, StatusClass,
          TNetEncoding.HTML.Encode(LegalityStatus)]));
      end;
    end;

    HTMLContent.AppendLine('</table>');
    HTMLContent.AppendLine('</div>');

    // Add rulings section if available
    if Length(Rulings) > 0 then
    begin
      HTMLContent.AppendLine('<div class="rulings">');
      HTMLContent.AppendLine('<h2>Rulings</h2>');
      for i := 0 to High(Rulings) do
      begin
        HTMLContent.AppendLine(Format('<p><em>%s:</em> %s</p>',
          [TNetEncoding.HTML.Encode(Rulings[i].PublishedAt),
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

//procedure TForm1.ShowHighResButtonClick(Sender: TObject);
//var
//  HighResURL: string;
//  HighResImageForm: THighResImageForm; // Declare it here
//begin
//  HighResURL := ShowHighResButton.TagString;
//  // Ensure TagString is properly assigned elsewhere
//
//  if HighResURL <> '' then
//  begin
//    HighResImageForm.Show;
//  end
//  else
//    ShowMessage('No high-resolution image available.');
//end;


procedure TForm1.ShowHighResButtonClick(Sender: TObject);
var
  ImageURL: string;
  CardName: string;
begin
  ImageURL := ShowHighResButton.TagString;
  CardName := 'Sample Card';

  // Create and display the high-res form
  if not Assigned(HighResImageForm) then
    HighResImageForm := THighResImageForm.Create(nil);

  Self.Hide; // Hide the current form (optional)
  HighResImageForm.ShowImage(ImageURL, CardName);
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
      ShowMessage('Ruling: ' + Rulings[i].Comment + Rulings[i].Comment);

    end;
  finally
    ScryfallAPI.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Label1.Text := '';
  if Trim(Edit1.Text).IsEmpty <> True then
  begin
    // Label1.Text := '';
    DisplayCardArtworks(Trim(Edit1.Text.Trim));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // Filter for cards
  // FilterDisplayedCards(Trim(Edit2.Text));

end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  // FilterDisplayedCards(''); // Passing an empty string displays all cards
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
    CollectionForm.Show;
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
      art_crop)) then
    begin
      ImageBitmap.LoadFromFile
        (GetCachedImagePath(FCardDetailsObject.CardDetails.ImageUris.art_crop));

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

procedure TForm1.ResizeListViewImage(NewWidth, NewHeight: Single);
var
  AppearanceItem: TListItemImage;
begin
  // Loop through all items in the ListView
  for var i := 0 to ListViewCards.Items.Count - 1 do
  begin
    AppearanceItem := ListViewCards.Items[i].Objects.FindObjectT<TListItemImage>
      ('GlyphImage');
    if Assigned(AppearanceItem) then
    begin
      // Resize the image
      AppearanceItem.Width := NewWidth;
      AppearanceItem.Height := NewHeight;
    end;
  end;
end;

initialization

Randomize;

end.
