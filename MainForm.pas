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
  System.Hash, ScryfallAPIWrapperV2, SGlobalsZ, DataModuleUnit, HighResForm,

  CardCollectionForm, MLogic, System.JSON, REST.JSON, System.StrUtils,
  System.TypInfo, System.NetEncoding, Math, System.Threading,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox;

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
    ComboBoxSetCode: TComboBox;
    ComboBoxColors: TComboBox;
    ComboBoxRarity: TComboBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ShowHighResButtonClick(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure DelayTimerTimer(Sender: TObject);
    procedure ListViewCardsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure ListViewCardsButtonClick(const Sender: TObject;
      const AItem: TListItem; const AObject: TListItemSimpleControl);

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
var
  ScryfallAPIHold: TScryfallAPI;
  SetDetailsArray: TArray<TSetDetails>;
  SetDetails: TSetDetails;
begin
  HttpClient := THTTPClient.Create;
  CardDataList := TList<TCardDetails>.Create;
  // DataModule1.CreateCardDetailsTable;
  CardCount := 0;
  AppClose := False;
  DataModule1.SetupDatabaseConnection(GetDatabasePath);
  CopyDatabaseToInternalStorage;
  CopyTemplateToInternalStorage;
  // InitializeManaSymbolMap;
  FBase64ImageCache := TDictionary<string, string>.Create;
  BrIsLoaded := False;
  DelayTimer.Enabled := True;
  WebBrowser1.URL := 'about:blank';
  // ListViewCards.ItemAppearanceName := 'ImageListItem';
  ListViewCards.OnItemClick := ListViewCardsItemClick;

  ComboBoxSetCode.Items.Add('All Sets');

  ScryfallAPIHold := TScryfallAPI.Create;
  try
    SetDetailsArray := ScryfallAPIHold.GetAllSets; // Fetch all sets from Scryfall
    for SetDetails in SetDetailsArray do
    begin
      ComboBoxSetCode.Items.Add(SetDetails.Code + ' - ' + SetDetails.Name);
      // Example: "KHM - Kaldheim"
    end;
  finally
    ScryfallAPIHold.Free;
    ComboBoxSetCode.ItemIndex := 0;
    ComboBoxColors.ItemIndex := 0;
    ComboBoxRarity.ItemIndex := 0;
  end;

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

procedure TForm1.ListViewCardsButtonClick(const Sender: TObject;
  const AItem: TListItem; const AObject: TListItemSimpleControl);

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
begin
  if BrIsLoaded then
    Exit;

  TTask.Run(
    procedure
    var
      PopularCards: TStringList;
      RandomCardName: string;
    begin
      PopularCards := TStringList.Create;
      try
        SetupPopularCards(PopularCards);
        if PopularCards.Count > 0 then
          RandomCardName := PopularCards[Random(PopularCards.Count)];
      finally
        PopularCards.Free;
      end;

      TThread.Queue(nil,
        procedure
        begin
          if not RandomCardName.IsEmpty then
            DisplayCardArtworks(RandomCardName);
          BrIsLoaded := True;
          DelayTimer.Enabled := False;
        end);
    end);
end;

procedure TForm1.DisplayCardArtworks(const CardName: string);
var
  ScryfallAPI: TScryfallAPI;
  SelectedSetCode: string;
  SelectedColorCode: string;
  SelectedRareCode: string;

begin
  // Show the progress bar
  ProgressBar1.Visible := True;
  ProgressBar1.Value := 0;

  if ComboBoxSetCode.Selected.Text = 'All Sets' then
    SelectedSetCode := ''
  else
    SelectedSetCode := ComboBoxSetCode.Selected.Text.Split([' - '])[0];

  if ComboBoxRarity.Text = 'All Rarities' then
    SelectedRareCode := ''
  else
    SelectedRareCode := ComboBoxRarity.Text;

  // All Colors
  if ComboBoxColors.Text = 'All Colors' then
    SelectedColorCode := ''
  else
    SelectedColorCode := ComboBoxColors.Text;

  // Create the API instance
  ScryfallAPI := TScryfallAPI.Create;



  // Call the asynchronous method
  ScryfallAPI.SearchAllCardsAsync(CardName, SelectedSetCode, SelectedRareCode,
    SelectedColorCode, False, False,
    procedure(Success: Boolean; Cards: TArray<TCardDetails>; ErrorMsg: string)
    begin
      // Hide the progress bar
      ProgressBar1.Visible := False;

      try
        if not Success then
        begin
          ShowMessage('Error searching cards: ' + ErrorMsg);
          Exit;
        end;

        if Length(Cards) = 0 then
        begin
          ShowMessage('No results found for "' + CardName + '".');
          Exit;
        end;

        if Length(Cards) > 800 then
        begin
          ShowMessage('Too many results. Please refine your search.');
          Exit;
        end;

        // Clear the ListView and internal card list
        ListViewCards.Items.Clear;
        CardDataList.Clear;
        ProgressBar1.Max := Length(Cards);


          //ScryfallAPI.GetSetByCode(DCardSet.Code);
   //     DCardSet := ScryfallAPI.GetAllSets;


        // Add cards to the ListView
        for var CardIndex := 0 to High(Cards) do
        begin
          CardDataList.Add(Cards[CardIndex]); // Store card details in memory
          AddCardToListView(Cards[CardIndex]); // Add to ListView
          ProgressBar1.Value := CardIndex + 1;
        end;

        // Automatically select and display the first item
        if ListViewCards.Items.Count > 0 then
        begin
          ListViewCards.Selected := ListViewCards.Items[0];
          ShowCardDetails(ListViewCards.Items[0]);
        end;
      finally
        ScryfallAPI.Free; // Free the API object
      end;
    end);
end;

procedure TForm1.AddCardToListView(const Card: TCardDetails);
var
  ListViewItem: TListViewItem;
  // Button: TListItemButton;
begin
  // Add a new item to the ListView
  ListViewItem := ListViewCards.Items.Add;

  // Set item text (card name)
  ListViewItem.Text := Card.CardName;

  // Store the card details in the item's TagObject
  ListViewItem.TagObject := TCardDetailsObject.Create(Card);


  // Load and assign the card art crop image as the item's background
//  if Card.ImageUris.art_crop <> '' then
//    LoadImageToListViewItemAsync(Card.ImageUris.art_crop, ListViewItem);
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

      // Fetch rulings asynchronously
      TTask.Run(
        procedure
        var
          Rulings: TArray<TRuling>;
        begin
          try
            Rulings := []; //FetchRulingsAsync(SelectedCard.SFID);
            // Fetch rulings using card's Scryfall ID
          except
            on E: Exception do
              TThread.Queue(nil,
                procedure
                begin
                  ShowMessage('Error fetching rulings: ' + E.Message);
                end);
          end;

          // Display card details and rulings
          TThread.Queue(nil,
            procedure
            begin
              DisplayCardInBrowser(SelectedCard, Rulings);
            end);
        end);
    end
    else
      ShowMessage('Error: No card details available for this item.');
  end
  else
    ShowMessage('Error: Sender is not a TListViewItem.');
end;



function ReplacePlaceholder(const Template, Placeholder, Value: string): string;
begin
  Result := StringReplace(Template, '{{' + Placeholder + '}}', Value,
    [rfReplaceAll]);
end;

procedure TForm1.DisplayCardInBrowser(const CardDetails: TCardDetails;
  const Rulings: TArray<TRuling>);
var
  Template: string;
  Replacements: TDictionary<string, string>;
  CardImagesHtml: string;
  RarityClass, Layout: string;
  I: Integer;

begin
  // Determine the rarity class for highlighting
  if CardDetails.Rarity.ToLower = 'common' then
    RarityClass := 'common'
  else if CardDetails.Rarity.ToLower = 'uncommon' then
    RarityClass := 'uncommon'
  else if CardDetails.Rarity.ToLower = 'rare' then
    RarityClass := 'rare'
  else if CardDetails.Rarity.ToLower = 'mythic' then
    RarityClass := 'mythic'
  else
    RarityClass := ''; // Default class if no match

  // Get the card layout (e.g., transform, adventure, split, etc.)
  Layout := CardDetails.Layout.ToLower;

  // Load the HTML template
  Template := LoadTemplate('card_template.html');

  // Build the card images HTML based on the layout
  if (Layout = 'transform') or (Layout = 'modal_dfc') then
  begin
    // Double-faced card
    if Length(CardDetails.CardFaces) > 1 then
    begin
      CardImagesHtml := Format('<div class="flip-card" onclick="flipCard()">' +
        '<div class="card-face front"><img src="%s" alt="Front Face"></div>' +
        '<div class="card-face back"><img src="%s" alt="Back Face"></div>' +
        '</div>',
        [TNetEncoding.HTML.Encode(CardDetails.CardFaces[0].ImageUris.Normal),
         TNetEncoding.HTML.Encode(CardDetails.CardFaces[1].ImageUris.Normal)]);
    end;
  end
  else if Layout = 'split' then
  begin
    // Split card
    CardImagesHtml := Format('<div class="single-card"><img src="%s" alt="Split Card"></div>',
      [TNetEncoding.HTML.Encode(CardDetails.ImageUris.Normal)]);
  end
  else if Layout = 'adventure' then
  begin
    // Adventure card
    if Length(CardDetails.CardFaces) > 0 then
    begin
      CardImagesHtml := Format('<div class="single-card"><img src="%s" alt="Adventure Card"></div>',
        [TNetEncoding.HTML.Encode(CardDetails.ImageUris.Normal)]);
    end;
  end
  else if Layout = 'saga' then
  begin
    // Saga card
    CardImagesHtml := Format('<div class="single-card"><img src="%s" alt="Saga"></div>',
      [TNetEncoding.HTML.Encode(CardDetails.ImageUris.Normal)]);
  end
  else if Layout = 'token' then
  begin
    // Token or emblem
    CardImagesHtml := Format('<div class="single-card"><img src="%s" alt="Token"></div>',
      [TNetEncoding.HTML.Encode(CardDetails.ImageUris.Normal)]);
  end
  else
  begin
    // Default single-faced card
    CardImagesHtml := Format('<div class="single-card"><img src="%s" alt="Card Image"></div>',
      [TNetEncoding.HTML.Encode(CardDetails.ImageUris.Normal)]);
  end;

  // Build legalities HTML
// Build legalities rows
var
  LegalitiesRows: string;
begin
  LegalitiesRows := '';
  for I := Low(LegalitiesArray) to High(LegalitiesArray) do
  begin
    var LegalityName := LegalitiesArray[I];
    var LegalityStatus := GetLegalStatus(CardDetails.Legalities, LegalityName);
    if LegalityStatus <> '' then
    begin
      var StatusClass := '';
      if LegalityStatus.ToLower = 'legal' then
        StatusClass := 'legal'
      else if LegalityStatus.ToLower = 'not_legal' then
        StatusClass := 'not-legal'
      else if LegalityStatus.ToLower = 'banned' then
        StatusClass := 'banned'
      else if LegalityStatus.ToLower = 'restricted' then
        StatusClass := 'restricted';

      // Generate a single row
      LegalitiesRows := LegalitiesRows +
        Format('<tr>' +
                 '<td class="format-name">%s</td>' +
                 '<td class="status"><span class="%s">%s</span></td>' +
               '</tr>',
               [TNetEncoding.HTML.Encode(LegalityName), StatusClass, TNetEncoding.HTML.Encode(StatusClass)]);
    end;
  end;
end;


  // Power/Toughness or Loyalty
  var PowerToughnessHtml := '';
  if (CardDetails.Power <> '') and (CardDetails.Toughness <> '') then
  begin
    PowerToughnessHtml := Format('<p><strong>Power/Toughness:</strong> %s/%s</p>',
      [TNetEncoding.HTML.Encode(CardDetails.Power),
       TNetEncoding.HTML.Encode(CardDetails.Toughness)]);
  end
  else if CardDetails.Loyalty <> '' then
  begin
    PowerToughnessHtml := Format('<p><strong>Loyalty:</strong> %s</p>',
      [TNetEncoding.HTML.Encode(CardDetails.Loyalty)]);
  end;

  // Prepare replacements for placeholders
  Replacements := TDictionary<string, string>.Create;
  try
    Replacements.Add('{{CardName}}', TNetEncoding.HTML.Encode(CardDetails.CardName));
     if CardDetails.FlavorText <> '' then
     Replacements.Add('{{FlavorText}}', TNetEncoding.HTML.Encode(CardDetails.FlavorText))
     else
     Replacements.Add('{{FlavorText}}', '');

     //{{SetName}}
    Replacements.Add('{{SetIcon}}', CardDetails.SetIconURI); // Add set icon URI
    Replacements.Add('{{SetName}}', TNetEncoding.HTML.Encode(CardDetails.SetName)); // Set name
    Replacements.Add('{{CardImages}}', CardImagesHtml);
    Replacements.Add('{{TypeLine}}', TNetEncoding.HTML.Encode(CardDetails.TypeLine));
    Replacements.Add('{{ManaCost}}', ReplaceManaSymbolsWithImages(CardDetails.ManaCost));
    Replacements.Add('{{OracleText}}', ReplaceManaSymbolsWithImages(CardDetails.OracleText));
    Replacements.Add('{{PowerToughness}}', PowerToughnessHtml);
      // Replace the placeholder in the template
    Replacements.Add('{{Legalities}}', LegalitiesRows);
    Replacements.Add('{{Rarity}}', TNetEncoding.HTML.Encode(CardDetails.Rarity));
    Replacements.Add('{{RarityClass}}', RarityClass);
    Replacements.Add('{{USD}}', CardDetails.Prices.USD);
    Replacements.Add('{{USD_Foil}}', CardDetails.Prices.USD_Foil);
    Replacements.Add('{{EUR}}', CardDetails.Prices.EUR);
    Replacements.Add('{{Tix}}', CardDetails.Prices.Tix);

    // Replace placeholders in the template
    for var Key in Replacements.Keys do
      Template := Template.Replace(Key, Replacements[Key]);

    // Load the final HTML into the WebBrowser
    TThread.Queue(nil,
      procedure
      begin
        WebBrowser1.LoadFromStrings(Template, '');
      end);
  finally
    Replacements.Free;
  end;
end;


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
  I: Integer;
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
    for I := 0 to High(Rulings) do
    begin
      ShowMessage('Ruling: ' + Rulings[I].Comment + Rulings[I].Comment);

    end;
  finally
    ScryfallAPI.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  if Trim(Edit1.Text).IsEmpty <> True then
  begin

    DisplayCardArtworks(Trim(Edit1.Text.Trim));
  end;
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

initialization

Randomize;

end.
