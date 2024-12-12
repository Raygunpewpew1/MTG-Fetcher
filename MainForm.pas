//I really need to move the app data to relative paths/ the images, json data etc.


unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, System.IOUtils, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ExtCtrls, FMX.Ani, FMX.Edit, FMX.StdCtrls,
  FMX.WebBrowser, FireDAC.Stan.Intf, FireDAC.Phys.Intf,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet, FireDAC.Stan.Param, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, Data.DB, FMX.Skia, System.Net.HttpClient,
  System.Hash, ScryfallAPIWrapperV2, SGlobalsZ, WrapperHelper,

  MLogic, APIConstants, System.StrUtils,
  System.TypInfo, System.NetEncoding, Math, System.Threading,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox, CardDisplayHelpers,
  FMX.TabControl;

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
    ComboBoxSetCode: TComboBox;
    ComboBoxColors: TComboBox;
    ComboBoxRarity: TComboBox;
    CountLabel: TLabel;
    ButtonNextPage: TButton;
    StyleBook1: TStyleBook;
    LayoutMain: TLayout; // Main layout container
    LayoutControls: TLayout; // Layout for controls like ComboBox, Edit, Button
    LayoutContent: TLayout; // Layout for ListView and WebBrowser
    LayoutButtons: TLayout; // Layout for buttons like Save, Collection, etc.
    SplitterMain: TSplitter;
    Switch1: TSwitch;
    DisplayUniq: TLabel;
    ComboBox1: TComboBox; // Splitter between ListView and WebBrowser
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);

    procedure DelayTimerTimer(Sender: TObject);
    procedure ListViewCardsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure ListViewCardsButtonClick(const Sender: TObject;
      const AItem: TListItem; const AObject: TListItemSimpleControl);
    procedure WebBrowser1DidFinishLoad(ASender: TObject);
    procedure ButtonNextPageClick(Sender: TObject);

  private

    WebBrowserInitialized: Boolean;
    CurrentPage: Integer;
    HasMorePages: Boolean;
    SearchTerm: string;
    HttpClient: THTTPClient;
    FCardDetailsObject: TCardDetailsObject; // Private field
    CardTitle: string; // Private field
    CardDataList: TList<TCardDetails>;
    // Encapsulated within the form
    CardCount: Integer; // Encapsulated within the form
    AppClose: Boolean; // Encapsulated within the form
    FScryfallAPI: TScryfallAPI; // Instance of TScryfallAPI

    BrIsLoaded: Boolean;
    procedure DisplayCardArtworks(const CardName: string);

    procedure ShowCardDetails(Sender: TObject);

    // procedure SaveSelectedCardToDatabase;

    procedure DisplayCardInBrowser(const CardDetails: TCardDetails;
      const Rulings: TArray<TRuling>);

    procedure AddCardToListView(const Card: TCardDetails);
    procedure LoadNextPage(const CardName, SetCode, ColorCode,
      RarityCode: string);
    procedure LoadAllCatalogs;
    // procedure AdjustLayout;

  public
    // Public declarations
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

const
  SAboutBlank = 'about:blank';
  SCard_templateHtml = 'card_template.html';
  SCatalogsJson = 'catalogs.json';
  S = ' - ';
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

procedure TForm1.LoadAllCatalogs;
var
  Catalogs: TDictionary<string, TScryfallCatalog>;
  FileName: string;
begin
  FileName := SCatalogsJson;
  Catalogs := TDictionary<string, TScryfallCatalog>.Create;

  try
    // Try to load the catalogs from the file
    LoadCatalogsFromFile(FileName, Catalogs);

    // If the dictionary is empty (file not found or invalid), fetch from the API
    if Catalogs.Count = 0 then
    begin
      Catalogs := FScryfallAPI.FetchAllCatalogs;
      SaveCatalogsToFile(FileName, Catalogs);
    end;

    // Use the catalogs
    if Catalogs.ContainsKey(CatalogKeywordAbilities) then
    begin
      ComboBox1.Items.Clear;
      ComboBox1.Items.AddStrings(Catalogs[CatalogKeywordAbilities].Data);
      ComboBox1.ItemIndex := 0;
    end;

    // ShowMessage('Keyword Abilities: ' + String.Join(', ', Catalogs[CatalogKeywordAbilities].Data));
  finally
    Catalogs.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  SetDetailsArray: TArray<TSetDetails>;
  SetDetails: TSetDetails;

  // CardTypes: TScryfallCatalog;
  // CreaTypeStr: string;
begin
  WebBrowserInitialized := False;

  WebBrowser1.URL := SAboutBlank;
  LoadAllCatalogs;
  // PopulateComboBoxFromCatalog(ComboBox1, CatalogLandTypes);

  FScryfallAPI := TScryfallAPI.Create;
  HttpClient := THTTPClient.Create;
  CardDataList := TList<TCardDetails>.Create;
  // DataModule1.CreateCardDetailsTable;
  CardCount := 0;
  AppClose := False;
  // DataModule1.SetupDatabaseConnection(GetDatabasePath);
  // CopyDatabaseToInternalStorage;
  CopyTemplateToInternalStorage;
  // InitializeManaSymbolMap;
  FBase64ImageCache := TDictionary<string, string>.Create;
  BrIsLoaded := False;

  ListViewCards.OnItemClick := ListViewCardsItemClick;

  ComboBoxSetCode.Items.Add('All Sets');

  try
    //FScryfallAPI.PreloadAllSets;
    SetDetailsArray := FScryfallAPI.GetAllSets;
    // Fetch all sets from Scryfall
    for SetDetails in SetDetailsArray do
    begin
      ComboBoxSetCode.Items.Add(SetDetails.Code + S + SetDetails.Name);
      // Example: "KHM - Kaldheim"
    end;
  finally

    ComboBoxSetCode.ItemIndex := 0;
    ComboBoxColors.ItemIndex := 0;
    ComboBoxRarity.ItemIndex := 0;
  end;

 DelayTimer.Enabled := True;
  // Form1.StyleBook := HighResForm.HighResImageForm.StyleBook1;

  // AdjustLayout;

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
  FScryfallAPI.Free;
end;

procedure TForm1.ListViewCardsButtonClick(const Sender: TObject;
  const AItem: TListItem; const AObject: TListItemSimpleControl);

// var
// ImageURL: string;
// CardName: string;
begin
  // ImageURL := ShowHighResButton.TagString;
  // CardName := 'Sample Card';
  //
  // // Create and display the high-res form
  // if not Assigned(HighResImageForm) then
  // HighResImageForm := THighResImageForm.Create(nil);
  //
  // Self.Hide; // Hide the current form (optional)
  // HighResImageForm.ShowImage(ImageURL, CardName);

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

procedure TForm1.DelayTimerTimer(Sender: TObject);

begin
  if BrIsLoaded or WebBrowserInitialized then
    Exit;

    //DisplayCardInBrowser

TTask.Run(
    procedure
    var
      RandomCard: TCardDetails;
    begin
      try
        // Fetch a random card
        RandomCard := FScryfallAPI.GetRandomCard;
      //  WebBrowserInitialized := true;
        // Update the UI
        TThread.Synchronize(nil,
          procedure
          begin
          //  DisplayCardInBrowser(RandomCard, []); // Display in browser / not really needed

            AddCardToListView(RandomCard);       // Add to ListView
             BrIsLoaded := True;               // Mark as loaded
            DelayTimer.Enabled := False;         // Disable timer
             if ListViewCards.Items.Count > 0 then
             begin
                ListViewCards.Selected := ListViewCards.Items[0];
                ShowCardDetails(ListViewCards.Items[0]); // Full view in browsers and sets up possible saving
             end;
          end);
      except
        on E: Exception do
        begin
          TThread.Synchronize(nil,
            procedure
            begin
              ShowMessage('Error fetching random card: ' + E.Message);
            end);
        end;
      end;
    end).Start;



//  TTask.Run(
//    procedure
//    var
//      PopularCards: TStringList;
//      RandomCardName: string;
//    begin
//      PopularCards := TStringList.Create;
//      try
//        SetupPopularCards(PopularCards);
//        if PopularCards.Count > 0 then
//          RandomCardName := PopularCards[Random(PopularCards.Count)];
//      finally
//        PopularCards.Free;
//      end;
//
//      TThread.Queue(nil,
//        procedure
//        begin
//          if not RandomCardName.IsEmpty then
//            DisplayCardArtworks(RandomCardName);
//          BrIsLoaded := True;
//          DelayTimer.Enabled := False;
//        end);
//    end);
end;

procedure TForm1.DisplayCardArtworks(const CardName: string);
var
  SelectedSetCode: string;
  SelectedColorCode: string;
  SelectedRareCode: string;
begin
  LayoutControls.Enabled := False;
  // Reset pagination variables
  CurrentPage := 1;
  HasMorePages := False;

  // Show the progress bar
  ProgressBar1.Visible := True;
  ProgressBar1.Value := 0;

  // Get the selected set code
  if ComboBoxSetCode.Selected.Text = 'All Sets' then
    SelectedSetCode := ''
  else
    SelectedSetCode := ComboBoxSetCode.Selected.Text.Split([' - '])[0];

  // Get the selected rarity
  if ComboBoxRarity.Text = 'All Rarities' then
    SelectedRareCode := ''
  else
    SelectedRareCode := ComboBoxRarity.Text;
     if ComboBoxRarity.Text = 'Mythic Rare' then
       SelectedRareCode := 'Mythic';

  // Get the selected colors
  if ComboBoxColors.Text = 'All Colors' then
    SelectedColorCode := ''
  else
    SelectedColorCode := ComboBoxColors.Text;

  // Clear ListView and internal card list for a new search
  ListViewCards.Items.Clear;
  CardDataList.Clear;

  // Start loading the first page
  LoadNextPage(CardName, SelectedSetCode, SelectedColorCode, SelectedRareCode);

  // Set up "Next Page" button click event
  ButtonNextPage.OnClick := ButtonNextPageClick;
  ButtonNextPage.Enabled := False;
  // Initially disabled until `HasMorePages` is updated
end;

procedure TForm1.LoadNextPage(const CardName, SetCode, ColorCode,
  RarityCode: string);
Var
  IsChecked: Boolean;
begin
  IsChecked := Switch1.IsChecked;

  FScryfallAPI.SearchAllCardsAsync(CardName, SetCode, RarityCode, ColorCode,
    False, IsChecked, CurrentPage,
    procedure(Success: Boolean; Cards: TArray<TCardDetails>; HasMore: Boolean;
      ErrorMsg: string)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          // Hide the progress bar when done
          ProgressBar1.Visible := False;

          try
            // Handle errors
            if not Success then
            begin
              ShowMessage('Error searching cards: ' + ErrorMsg);
              Button1.Enabled := True;
              LayoutControls.Enabled := True;
              Exit;
            end;

            // Update ListView and internal card list
            ProgressBar1.Max := ProgressBar1.Max + Length(Cards);

            for var CardIndex := 0 to High(Cards) do
            begin
              if IsCardValid(Cards[CardIndex]) then
              begin
                CardDataList.Add(Cards[CardIndex]); // Store valid card details
                AddCardToListView(Cards[CardIndex]); // Add to ListView

                // Update the progress bar
                ProgressBar1.Value := ProgressBar1.Value + 1;
              end
              else
              begin
                LogError(Format('Skipping invalid card at index %d: %s',
                  [CardIndex, Cards[CardIndex].CardName]));
              end;
            end;

            // Update "Next Page" button state
            HasMorePages := HasMore;
            ButtonNextPage.Enabled := HasMorePages;

            // Automatically select and display the first item on the first page
            if (CurrentPage = 1) and (ListViewCards.Items.Count > 0) then
            begin
              ListViewCards.Selected := ListViewCards.Items[0];
              ShowCardDetails(ListViewCards.Items[0]);
            end;
          finally
            CountLabel.Text := 'Cards Found: ' +
              ListViewCards.ItemCount.ToString;
          end;
        end);

      // Enable the button
      LayoutControls.Enabled := True;
    end);
end;

procedure TForm1.AddCardToListView(const Card: TCardDetails);
var
  ListViewItem: TListViewItem;
begin
  // Skip invalid cards
  if Card.CardName.IsEmpty or Card.SFID.IsEmpty then
  begin
    LogError('Skipping card: missing "name" or "id".');
    Exit;
  end;

  // Create a new ListView item
  ListViewItem := ListViewCards.Items.Add;

  // Set item properties
  ListViewItem.Text := Card.CardName;
  ListViewItem.Detail := Card.SetCode.ToUpper;
  ListViewItem.TagObject := TCardDetailsObject.Create(Card);
  // Store the card object
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

      // Display card details immediately without set details
      // DisplayCardInBrowser(SelectedCard, []);

      // Fetch set details asynchronously
      if not SelectedCard.SetCode.IsEmpty and SelectedCard.SetIconURI.IsEmpty
      then
      begin
        TTask.Run(
          procedure
          var
            SetDetails: TSetDetails;
          begin
            try
              // Fetch the set details from the API
              SetDetails := FScryfallAPI.GetSetByCode(SelectedCard.SetCode);
              TThread.Synchronize(nil,
                procedure
                begin
                  // Update the card with fetched set details
                  SelectedCard.SetName := SetDetails.Name;
                  SelectedCard.SetIconURI := SetDetails.IconSVGURI;

                  // Refresh the browser with updated set details
                  DisplayCardInBrowser(SelectedCard, []);
                end);
            except
              on E: Exception do
                LogError('Failed to fetch set details: ' + E.Message);
            end;
          end);
      end;
    end
    else
      ShowMessage('Error: No card details available for this item.');
  end
  else

    ShowMessage('Error: Sender is not a TListViewItem.');
end;

procedure TForm1.WebBrowser1DidFinishLoad(ASender: TObject);
begin
  WebBrowser1.EvaluateJavaScript
    ('document.addEventListener("contextmenu", function (e) { e.preventDefault(); });'
    + 'function flipCard() {' +
    '  const card = document.querySelector(".flip-card");' + '  if (card) {' +
    '    card.classList.toggle("show-back");' + '  }' + '}');
end;

procedure TForm1.DisplayCardInBrowser(const CardDetails: TCardDetails;
const Rulings: TArray<TRuling>);
var
  Template: string;
  Replacements: TDictionary<string, string>;
begin
  try
    // Load the HTML template
    Template := LoadTemplate(SCard_templateHtml);
    // Ensure this file exists and includes proper placeholders

    // Initialize the replacements dictionary
    Replacements := TDictionary<string, string>.Create;
    try
      // sections of replacements using helper functions
      AddCoreReplacements(Replacements, CardDetails); // Basic card details
      AddImageReplacements(Replacements, CardDetails); // Image and layout logic
      AddLegalitiesReplacements(Replacements, CardDetails); // Legalities table
      AddPricesReplacements(Replacements, CardDetails); // Prices section
      AddBadgesReplacements(Replacements, CardDetails);
      // FullArt, Promo, Reserved badges
      AddKeywordsReplacement(Replacements, CardDetails); // Keywords section

      // Perform the replacements in the template
      for var Key in Replacements.Keys do
        Template := Template.Replace(Key, Replacements[Key]);

      // Display the final HTML in the web browser
      TThread.Queue(nil,
        procedure
        begin
          WebBrowser1.LoadFromStrings(Template, ''); // Load the processed HTML
        end);
    finally
      Replacements.Free; // Free the replacements dictionary
    end;
  except
    on E: Exception do
      ShowMessage('Error displaying card: ' + E.Message);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

  if Edit1.Text.Trim.IsEmpty <> True then
  begin
    SearchTerm := '';
    SearchTerm := Edit1.Text.Trim;
    DisplayCardArtworks(SearchTerm);
    LayoutControls.Enabled := False;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  // if Assigned(FCardDetailsObject) then
  // begin
  // if DataModule1.CheckCardExists(Trim(FCardDetailsObject.CardDetails.SFID))
  // then
  // ShowMessage('Card already exists in the database.')
  // else
  // SaveSelectedCardToDatabase;
  // end
  // else
  // ShowMessage('No card is selected.');
end;

procedure TForm1.ButtonNextPageClick(Sender: TObject);
begin
  if HasMorePages then
  begin
    Inc(CurrentPage); // Increment the page number
    LoadNextPage(SearchTerm, '', '', ''); // Load the next page using filters
  end
  else
    ShowMessage('No more pages to load.');
end;

initialization

Randomize;

end.
