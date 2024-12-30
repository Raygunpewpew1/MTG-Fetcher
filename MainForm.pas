unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, System.IOUtils, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ExtCtrls, FMX.Ani, FMX.Edit, FMX.StdCtrls,
  FMX.WebBrowser, FMX.Skia, System.Net.HttpClient,
  SGlobalsZ, ScryfallAPIWrapperV2,
  System.TypInfo, Math, System.Threading,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox, MLogic,
  System.Net.URLClient, System.Net.HttpClientComponent, MainFormLogicHelpers;

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
    CountLabel: TLabel;
    LabelProgress: TLabel;
    NetHTTPClient1: TNetHTTPClient;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DelayTimerTimer(Sender: TObject);
    procedure ListViewCardsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure WebBrowser1DidFinishLoad(ASender: TObject);
    procedure ButtonNextPageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);

  private

    WebBrowserInitialized: Boolean;
    CurrentPage: Integer;
    HasMorePages: Boolean;
    SearchTerm: string;
    HttpClient: THTTPClient;
    CardDataList: TList<TCardDetails>;
    CardCount: Integer;
    AppClose: Boolean;
    SavePathX: string;
    FScryfallAPI: TScryfallAPI; // Instance of TScryfallAPI

    BrIsLoaded: Boolean;
    procedure DisplayCardArtworks(const CardName: string);

    procedure ShowCardDetails(Sender: TObject);


    procedure DisplayCardInBrowser(const CardDetails: TCardDetails;
      const Rulings: TArray<TRuling>);

    procedure AddCardToListView(const Card: TCardDetails);
    procedure LoadNextPage(const CardName, SetCode, ColorCode,
      RarityCode: string);
    procedure LoadAllCatalogs;
    procedure DownloadBulkData;
    procedure OnDownloadProgress(const Sender: TObject;
      AContentLength, AReadCount: Int64; var Abort: Boolean);


  public

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

uses
  WrapperHelper, APIConstants, CardDisplayHelpers,
  JsonDataObjects, DataModuleUnit, Logger,Template;


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



procedure TForm1.DownloadBulkData;
var
  AppDataPath: string;
begin

  AppDataPath := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
  if not TDirectory.Exists(AppDataPath) then
    TDirectory.CreateDirectory(AppDataPath);

  SavePathX := TPath.Combine(AppDataPath, BulkDataPath);

  if FileExists(SavePathX) then
  begin
    LoadBulkData(SavePathX,CardDataList);
    Exit;
  end;

  ProgressBar1.Visible := True;
  LabelProgress.Visible := True;
  LabelProgress.Text := S_FETCHING_BULK_DATA_METADATA;

  // Execute download in a background thread
  TTask.Run(
    procedure
    var
      MetadataStream, DataStream: TStringStream;
      MetadataJson: TJsonObject;
      DownloadURI: string;
    begin
      MetadataStream := TStringStream.Create;
      DataStream := TStringStream.Create;
      try
        try

          TThread.Queue(nil,
            procedure
            begin
              LabelProgress.Text := S_FETCHING_BULK_DATA_METADATA;
            end);

          HttpClient.CustomHeaders['User-Agent'] := UserAgent;
          HttpClient.CustomHeaders['Accept'] := AcceptHeader;
          HttpClient.Get('https://api.scryfall.com/bulk-data', MetadataStream);

          MetadataJson := TJsonObject.Parse(MetadataStream.DataString)
            as TJsonObject;
          try
            if MetadataJson.Contains(S_DATA) then
            begin
              for var Obj in MetadataJson.A['data'] do
              begin
                if (Obj.S[S_TYPE] = S_ORACLE_CARDS) then
                begin
                  DownloadURI := Obj.S[S_DOWNLOAD_URI];
                  Break;
                end;
              end;
            end;

            if DownloadURI.IsEmpty then
              raise Exception.Create
                (S_DOWNLOAD_URI_FOR_DEFAULT_CARDS_NOT_FOUND);

            TThread.Queue(nil,
              procedure
              begin
                LabelProgress.Text := S_DOWNLOADING_BULK_DATA;
              end);

            HttpClient.OnReceiveData := OnDownloadProgress;
            HttpClient.Get(DownloadURI, DataStream);

            DataStream.SaveToFile(SavePathX);

            TThread.Queue(nil,
              procedure
              begin
                LabelProgress.Text := S_DOWNLOAD_COMPLETE_FILE_SAVED_TO +
                  SavePathX;
                LoadBulkData(SavePathX,CardDataList);
              end);

          finally
            MetadataJson.Free;
          end;

        except
          on E: Exception do
            TThread.Queue(nil,
              procedure
              begin
                LabelProgress.Text := S_ERROR_DOWNLOADING_BULK_DATA;
                ShowMessage(S_ERROR + E.Message);
              end);
        end;

      finally
        MetadataStream.Free;
        DataStream.Free;

        TThread.Queue(nil,
          procedure
          begin
            ProgressBar1.Visible := False;
          end);
      end;
    end);
end;

procedure TForm1.OnDownloadProgress(const Sender: TObject;
AContentLength, AReadCount: Int64; var Abort: Boolean);
begin
  if AContentLength > 0 then
    TThread.Queue(nil,
      procedure
      begin
        ProgressBar1.Max := 100;
        ProgressBar1.Value := (AReadCount / AContentLength) * 100;
      end);
end;

procedure TForm1.LoadAllCatalogs;
var
  Catalogs: TDictionary<string, TScryfallCatalog>;
  FileName: string;
begin
  FileName := SCatalogsJson;
  Catalogs := TDictionary<string, TScryfallCatalog>.Create;

  try

    LoadCatalogsFromFile(FileName, Catalogs);

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

  finally
    Catalogs.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  SetDetailsArray: TArray<TSetDetails>;
  SetDetails: TSetDetails;
begin
  WebBrowserInitialized := False;
  WebBrowser1.LoadFromStrings('', '');

  LoadAllCatalogs;
  FScryfallAPI := TScryfallAPI.Create;
  HttpClient := THTTPClient.Create;
  CardDataList := TList<TCardDetails>.Create;
  CardCount := 0;
  AppClose := False;

  CopyDatabaseToInternalStorage;

  BrIsLoaded := False;

  ListViewCards.OnItemClick := ListViewCardsItemClick;

  ComboBoxSetCode.Items.Add(S_ALL_SETS);

  try
    SetDetailsArray := FScryfallAPI.GetAllSets;
    for SetDetails in SetDetailsArray do
    begin
      ComboBoxSetCode.Items.Add(SetDetails.Code + S + SetDetails.Name);
      //  "KHM - Kaldheim"
    end;
  finally

    ComboBoxSetCode.ItemIndex := 0;
    ComboBoxColors.ItemIndex := 0;
    ComboBoxRarity.ItemIndex := 0;
  end;

  DownloadBulkData;

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

  WebBrowser1.Navigate(S_ABOUT_BLANK);
end;

procedure TForm1.ListViewCardsItemClick(const Sender: TObject;
const AItem: TListViewItem);

begin
  if Assigned(AItem) and Assigned(AItem.TagObject) and
    (AItem.TagObject is TCardDetailsObject) then
  begin

    ShowCardDetails(AItem);

  end
  else
    ShowMessage(S_NO_CARD_DETAILS_ARE_AVAILABLE_FOR_THIS);
end;

procedure TForm1.DelayTimerTimer(Sender: TObject);
begin
  if BrIsLoaded and WebBrowserInitialized then
    Exit;

  TTask.Run(
    procedure
    var
      RandomCard: TCardDetails;
    begin
      try
        // Fetch a random card
        RandomCard := FScryfallAPI.GetRandomCard;

        // Synchronize
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
              ShowMessage(S_ERROR_FETCHING_RANDOM_CARD + E.Message);
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
  if ComboBoxSetCode.Selected.Text = S_ALL_SETS then
    SelectedSetCode := ''
  else
    SelectedSetCode := ComboBoxSetCode.Selected.Text.Split([S])[0];


  if ComboBoxRarity.Text = S_ALL_RARITIES then
    SelectedRareCode := ''
  else
    SelectedRareCode := ComboBoxRarity.Text;
  if ComboBoxRarity.Text = S_MYTHIC_RARE then
    SelectedRareCode := S_MYTHIC;


  if ComboBoxColors.Text = S_ALL_COLORS then
    SelectedColorCode := ''
  else
    SelectedColorCode := ComboBoxColors.Text;


  ClearListViewItems(ListViewCards);
  CardDataList.Clear;

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
              ShowMessage(S_ERROR_SEARCHING_CARDS + ErrorMsg);
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

                ProgressBar1.Value := ProgressBar1.Value + 1;
              end
              else
              begin
                LogStuff(Format(S_SKIPPING_INVALID_CARD_AT_INDEX_D_S,
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
            CountLabel.Text := S_CARDS_FOUND +
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
    LogStuff(S_SKIPPING_CARD_MISSING_NAME_OR_ID);
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

  if Sender is TListViewItem then
  begin
    SelectedItem := TListViewItem(Sender);

    // Check if the TagObject contains card details
    if Assigned(SelectedItem.TagObject) and
      (SelectedItem.TagObject is TCardDetailsObject) then
    begin
      CardDetailsObject := TCardDetailsObject(SelectedItem.TagObject);
      SelectedCard := CardDetailsObject.CardDetails;

      LogStuff(SelectedCard.CardName + ' ' + SelectedCard.SetCode);

      if not SelectedCard.SetCode.IsEmpty then
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
  if SameText(WebBrowser1.URL, S_ABOUT_BLANK) then
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
        WebBrowser1.LoadFromStrings(SAboutBlank, '');
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
            Template := '';
          end);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage(S_ERROR_DISPLAYING_CARD + E.Message);
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

procedure TForm1.Button2Click(Sender: TObject);
var
  SelectedSetCode, SelectedColorCode, SearchTerm: string;
  RarityFilter: TRarity;
  FilteredCards: TList<TCardDetails>;
begin
  if CardDataList.IsEmpty then Exit;


  // Get the selected set code
  if ComboBoxSetCode.Selected.Text = S_ALL_SETS then
    SelectedSetCode := ''
  else
    SelectedSetCode := ComboBoxSetCode.Selected.Text.Split([S])[0];

  // Map rarity to TRarity enum
  if ComboBoxRarity.Text = S_COMMON then
    RarityFilter := rCommon
  else if ComboBoxRarity.Text = S_UNCOMMON then
    RarityFilter := rUncommon
  else if ComboBoxRarity.Text = S_RARE then
    RarityFilter := rRare
  else if ComboBoxRarity.Text = S_MYTHIC_RARE then
    RarityFilter := rMythic
  else if ComboBoxRarity.Text = S_SPECIAL then
    RarityFilter := rSpecial
  else
    RarityFilter := rCommon; // Default if no match

  // Get the selected colors
  if ComboBoxColors.Text = S_ALL_COLORS then
    SelectedColorCode := ''
  else
    SelectedColorCode := ComboBoxColors.Text.ToLower;

  // Get the search term
  SearchTerm := Edit1.Text.Trim;

  // Call the SearchCards function
  FilteredCards := SearchCards(CardDataList, SearchTerm, RarityFilter,
    ColorNameToCode(SelectedColorCode));
  ListViewCards.Items.Clear;

  for var Card in FilteredCards do
  begin

    AddCardToListView(Card);

  end;

  LabelProgress.Text := FilteredCards.Count.ToString;
  // CheckMemoryUsage;
  // LogStuff(IntToStr(GetHeapStatus.TotalAllocated div 1024) + ' KB allocated');
end;

procedure TForm1.ButtonNextPageClick(Sender: TObject);
begin
  if HasMorePages then
  begin
    Inc(CurrentPage); // Increment the page number
    LoadNextPage(SearchTerm, '', '', ''); // Load the next page using filters
  end
  else
    ShowMessage(S_NO_MORE_PAGES_TO_LOAD);
end;

//CardDataList



initialization

Randomize;

end.
