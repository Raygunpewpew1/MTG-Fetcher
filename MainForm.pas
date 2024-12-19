{

  TODO Organize code, UI improvments
  Need to focus on Windows, less time worrying about Android.

}

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
  System.Hash, SGlobalsZ, ScryfallAPIWrapperV2, System.StrUtils,
  System.TypInfo, Math, System.Threading,
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
    DelayTimer: TTimer;
    StyleBook1: TStyleBook;
    LayoutMain: TLayout;
    LayoutControls: TLayout;
    LabelSearch: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    ComboBoxSetCode: TComboBox;
    ComboBoxRarity: TComboBox;
    ComboBoxColors: TComboBox;
    ButtonNextPage: TButton;
    Switch1: TSwitch;
    DisplayUniq: TLabel;
    ComboBox1: TComboBox;
    LayoutContent: TLayout;
    ListViewCards: TListView;
    SplitterMain: TSplitter;
    WebBrowser1: TWebBrowser;
    LayoutButtons: TLayout;
    Button4: TButton;
    Button5: TButton;
    ShowHighResButton: TButton;
    ProgressBar1: TProgressBar;
    CountLabel: TLabel; // Splitter between ListView and WebBrowser
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DelayTimerTimer(Sender: TObject);
    procedure ListViewCardsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure WebBrowser1DidFinishLoad(ASender: TObject);
    procedure ButtonNextPageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

  private

    WebBrowserInitialized: Boolean;
    CurrentPage: Integer;
    HasMorePages: Boolean;
    SearchTerm: string;
    HttpClient: THTTPClient;
    // FCardDetailsObject: TCardDetailsObject;
    CardDataList: TList<TCardDetails>;
    CardCount: Integer;
    AppClose: Boolean;
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

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

uses
  WrapperHelper, MLogic, APIConstants, CardDisplayHelpers, System.NetEncoding;

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

    { Other Catalogs to do
      CatalogCreatureTypes,
      CatalogPlaneswalkerTypes,
      CatalogArtifactTypes,
      CatalogEnchantmentTypes,
      CatalogLandTypes,
      CatalogSpellTypes,
      CatalogPowers,
      CatalogToughnesses,
      CatalogLoyalties,
      CatalogWatermarks,
      CatalogKeywordAbilities,
      CatalogKeywordActions,
      CatalogAbilityWords }

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
  WebBrowser1.LoadFromStrings('', '');

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
  // CopyTemplateToInternalStorage;

  BrIsLoaded := False;

  ListViewCards.OnItemClick := ListViewCardsItemClick;

  ComboBoxSetCode.Items.Add('All Sets');

  try
    // FScryfallAPI.PreloadAllSets;
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

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AppClose := True;
  FreeAndNil(HttpClient);
  FreeAndNil(CardDataList);
  FreeAndNil(FScryfallAPI);
end;

procedure TForm1.FormShow(Sender: TObject);
begin

  WebBrowser1.Navigate('about:blank');
end;

procedure TForm1.ListViewCardsItemClick(const Sender: TObject;
  const AItem: TListViewItem);

begin
  if Assigned(AItem) and Assigned(AItem.TagObject) and
    (AItem.TagObject is TCardDetailsObject) then
  begin

    // Call ShowCardDetails with the selected card details
    ShowCardDetails(AItem);
  end
  else
    ShowMessage('No card details are available for this item.');
end;

procedure TForm1.DelayTimerTimer(Sender: TObject);
begin
  if BrIsLoaded and WebBrowserInitialized then
    Exit;

  // Start the task to fetch a random card asynchronously
  TTask.Run(
    procedure
    var
      RandomCard: TCardDetails;
    begin
      try
        // Fetch a random card
        RandomCard := FScryfallAPI.GetRandomCard;

        // Synchronize with the main thread for UI updates
        TThread.Queue(nil,
          procedure
          begin
            AddCardToListView(RandomCard); // Add the card to ListView
            BrIsLoaded := True;
            DelayTimer.Enabled := False;

            // First item is selected and details are displayed
            if ListViewCards.Items.Count > 0 then
            begin
              ListViewCards.Selected := ListViewCards.Items[0];
              ShowCardDetails(ListViewCards.Items[0]);
            end;
          end);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Error fetching random card: ' + E.Message);
            end);
      end;
    end).Start;
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

  // Show the progress bar for giggles
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
  ClearListViewItems(ListViewCards);
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
                LogStuff(Format('Skipping invalid card at index %d: %s',
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

      // Enable the buttons
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
    LogStuff('Skipping card: missing "name" or "id".');
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
      // FCardDetailsObject := CardDetailsObject;


      // ShowHighResButton.Enabled := SelectedCard.ImageUris.Normal <> '';
      // ShowHighResButton.TagString := SelectedCard.ImageUris.Normal;
      // CardTitle := SelectedCard.CardName;

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
                LogStuff('Failed to fetch set details: ' + E.Message);
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
if SameText(WebBrowser1.URL, 'about:blank') then
  begin
    WebBrowserInitialized := True;

  end
  else
  begin
    WebBrowser1.EvaluateJavaScript(JScript);
  end;
  //
end;

procedure TForm1.DisplayCardInBrowser(const CardDetails: TCardDetails;
const Rulings: TArray<TRuling>);
begin
  TTask.Run(
    procedure
    var
      Template: string;
      Replacements: TDictionary<string, string>;
    begin
      try
        Template := HtmlTemplate; // Load HTML template
        Replacements := TDictionary<string, string>.Create;
        try
          // Prepare all replacements
          AddCoreReplacements(Replacements, CardDetails);
          AddImageReplacements(Replacements, CardDetails);
          AddLegalitiesReplacements(Replacements, CardDetails);
          AddPricesReplacements(Replacements, CardDetails);
          AddBadgesReplacements(Replacements, CardDetails);
          AddKeywordsReplacement(Replacements, CardDetails);

          for var Key in Replacements.Keys do
            Template := Template.Replace(Key, Replacements[Key]);
        finally
          Replacements.Free;
        end;

        TThread.Queue(nil,
          procedure
          begin
            WebBrowser1.LoadFromStrings(Template, '');
          end);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage('Error displaying card: ' + E.Message);
            end);
      end;
    end);
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
