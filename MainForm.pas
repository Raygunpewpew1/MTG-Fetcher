unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls, FMX.Ani, FMX.Edit, FMX.StdCtrls,
  FMX.WebBrowser, FMX.Skia, SGlobalsZ, ScryfallData, System.TypInfo, Math,
  System.Threading, FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.ListBox, MLogic, FMX.ComboEdit, CardDisplayManager, ScryfallQueryBuilder,
  System.IOUtils, System.StrUtils, FMX.MultiView, FMX.Platform;

type
  TCardLayout = class(TLayout)
  public
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    DelayTimer: TTimer;
    // NetHTTPClient1: TNetHTTPClient;
    TimerDebounce: TTimer;
    StyleBook1: TStyleBook;
    MultiViewFilters: TMultiView;
    LayoutFilters: TLayout;
    ButtonNextPage: TButton;
    Button1: TButton;
    ComboBoxSetCode: TComboBox;
    ComboBoxRarity: TComboBox;
    ComboBoxAbility: TComboBox;
    ComboBoxEditSearch: TComboEdit;
    LayoutContent: TLayout;
    ListViewCards: TListView;
    WebBrowser1: TWebBrowser;
    LayoutActions: TLayout;
    Switch1: TSwitch;
    Button2: TButton;
    ListBoxColors: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DelayTimerTimer(Sender: TObject);
    procedure ListViewCardsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure WebBrowser1DidFinishLoad(ASender: TObject);
    procedure ButtonNextPageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerDebounceTimer(Sender: TObject);
    procedure ComboBoxEditSearchItemClick(const Sender: TObject;
      const AItem: TListBoxItem);
    procedure ComboBoxEditSearchKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure ComboBoxEditSearchChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);

  private
    WebBrowserInitialized: Boolean;
    FIsProgrammaticChange: Boolean;

    // SearchTerm: string;
    FCurrentAutocompleteTask: ITask;
    // HttpClient: THTTPClient;

    FCardDisplayManager: TCardDisplayManager;
    FScryfallAPI: TScryfallAPI;
    BrIsLoaded: Boolean;
    AmOnline: Boolean;

    procedure OnSearchComplete(Success: Boolean);
    procedure PopulateColorListBox;
    function GetSelectedColors: string;

  public
  end;

var
  Form1: TForm1;
  ColorCheckBoxes: TObjectList<TCheckBox>;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

uses
  APIConstants, JsonDataObjects, Logger, Template, CardDisplayHelpers,
  System.NetEncoding;

destructor TCardLayout.Destroy;
begin
  FreeAndNil(TagObject);
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  SetDetailsArray: TArray<TSetDetails>;
  ComboBoxMap: TDictionary<string, TComboBox>;
  CacheFileName: string;
begin

  FScryfallAPI := TScryfallAPI.Create;

  try
    AmOnline := FScryfallAPI.IsInternetAvailable;
  except
    on E: Exception do
    begin
      ShowMessage('Error checking internet connection: ' + E.Message);
     {$IF DEFINED(MSWINDOWS) OR DEFINED(MACOS)}
      Application.Terminate;
     {$ELSE}
      Halt;
     {$ENDIF}
    end;
  end;

  WebBrowserInitialized := False;
  FIsProgrammaticChange := False;
  WebBrowser1.LoadFromStrings(HtmlTemplate, '');
  ComboBoxMap := TDictionary<string, TComboBox>.Create;

  FCardDisplayManager := TCardDisplayManager.Create(ListViewCards, WebBrowser1,
    FScryfallAPI);

  FCardDisplayManager.OnProgressUpdate := procedure(Progress: Integer)
    begin
      // ProgressBar1.Value := Progress;
    end;
  // ComboBoxSetCode

  try
    ComboBoxMap.Add(CatalogKeywordAbilities, ComboBoxAbility);
    FCardDisplayManager.LoadAllCatalogs(ComboBoxMap);
  finally
    ComboBoxMap.Free;
  end;

  // HttpClient := THTTPClient.Create;

  AppClose := False;

  BrIsLoaded := False;
  ListViewCards.OnItemClick := ListViewCardsItemClick;
  ComboBoxSetCode.Items.Add(S_ALL_SETS);

  CacheFileName := GetCacheFilePath(SetCacheFile);

  if TFile.Exists(CacheFileName) then
  begin

    LogStuff('Loading set details from cache...');
    SetDetailsArray := LoadSetDetailsFromJson(CacheFileName);
  end
  else
  begin

    LogStuff('Fetching set details from Scryfall...');
    try
      SetDetailsArray := FScryfallAPI.GetAllSets;

      // Save to cache
      SaveSetDetailsToJson(CacheFileName, SetDetailsArray);
    except
      on E: Exception do
      begin
        LogStuff('Error fetching set details: ' + E.Message);
        Exit;
      end;
    end;
  end;

  try

    for var SetDetails in SetDetailsArray do
      ComboBoxSetCode.Items.Add(SetDetails.Code + ' - ' + SetDetails.Name);

  finally
    ComboBoxSetCode.ItemIndex := 0;
 //   ComboBoxColors.ItemIndex := 0;
    ComboBoxRarity.ItemIndex := 0;
  end;
  ColorCheckBoxes := TObjectList<TCheckBox>.Create(True); // Auto-own checkboxes
  PopulateColorListBox;
  DelayTimer.Enabled := True;
end;


function TForm1.GetSelectedColors: string;
var
  CheckBox: TCheckBox;
  Colors: string;
begin
  Colors := ''; // Initialize the result
  for CheckBox in ColorCheckBoxes do
  begin
    if CheckBox.IsChecked then
      Colors := Colors + CheckBox.TagString; // Append the color code
  end;
  Result := Colors; // Return the combined color string
end;





procedure TForm1.PopulateColorListBox;
  procedure AddColor(const ColorName: string; const TagValue: string);
  var
    ListBoxItem: TListBoxItem;
    CheckBox: TCheckBox;
  begin
    // Create a new ListBoxItem
    ListBoxItem := TListBoxItem.Create(ListBoxColors);
    ListBoxItem.Parent := ListBoxColors; // Attach to the ListBox
    ListBoxItem.Height := 40;           // Set item height for spacing

    // Create a CheckBox and add it to the ListBoxItem
    CheckBox := TCheckBox.Create(ListBoxItem);
    CheckBox.Parent := ListBoxItem;        // Attach to the ListBoxItem
    CheckBox.Align := TAlignLayout.Client; // Make the checkbox fill the item
    CheckBox.Text := ColorName;            // Set the checkbox label
    CheckBox.TagString := TagValue;        // Store the color code in TagString

    // Store the reference to the CheckBox
    ColorCheckBoxes.Add(CheckBox);
  end;

begin
  // Clear the ListBox and the checkbox list
  ListBoxColors.Clear;
  ColorCheckBoxes.Clear;

  // Populate the ListBox with color options
  AddColor('White', 'W');
  AddColor('Blue', 'U');
  AddColor('Black', 'B');
  AddColor('Red', 'R');
  AddColor('Green', 'G');
  AddColor('Colorless', 'C');
end;





procedure TForm1.OnSearchComplete(Success: Boolean);
begin
  Button1.Enabled := True;
  ButtonNextPage.Enabled := FCardDisplayManager.HasMorePages;

  if not Success then
    ShowMessage('Search failed. Please try again.')
  else if ListViewCards.Items.Count = 0 then
    ShowMessage('No cards found.')
  else
  begin
    ListViewCards.SearchVisible := True;
    ListViewCards.ItemIndex := 0;
    ListViewCards.OnItemClick(ListViewCards, ListViewCards.Items[0]);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  try
    if Assigned(ListViewCards) then
      ListViewCards.OnItemClick := nil;
    if Assigned(FScryfallAPI) then
    begin
      FScryfallAPI.Free;
      FScryfallAPI := nil;
      ColorCheckBoxes.Free;
    end;
  except
    on E: Exception do
    begin
      LogStuff('Error during Exit: ' + E.Message, ERROR);
    end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  WebBrowser1.Navigate(HtmlTemplate);
  TTask.Run(
    procedure
    begin
      TThread.Sleep(500); // Adjust timeout if needed
      TThread.Queue(nil,
        procedure
        begin
          if not WebBrowserInitialized then
          begin
            WebBrowserInitialized := True; // Fallback for initialization
            LogStuff('WebBrowser initialized via fallback.', WARNING);
          end;
        end);
    end);

  MultiViewFilters.ShowMaster;

end;

procedure TForm1.ListViewCardsItemClick(const Sender: TObject;
const AItem: TListViewItem);
var
  CardDetailsObj: TCardDetailsObject;
begin
  if not Assigned(AItem) or not Assigned(AItem.TagObject) then
  begin
    ShowMessage('Item or TagObject not assigned');
    Exit;
  end;

  // LogStuff('TagObject class: ' + AItem.TagObject.ClassName);
  // LogStuff('Is TCardDetailsObject: ' +
  // BoolToStr(AItem.TagObject is TCardDetailsObject, True));

  if AItem.TagObject is TCardDetailsObject then
  begin
    CardDetailsObj := TCardDetailsObject(AItem.TagObject);
    // LogStuff('Clicked on card: ' + CardDetailsObj.CardDetails.CardName, DEBUG);
    FCardDisplayManager.ShowCardDetails(CardDetailsObj.CardDetails);
  end
  else
    ShowMessage('TagObject is not TCardDetailsObject');
end;

procedure TForm1.TimerDebounceTimer(Sender: TObject);
var
  PartialQuery: string;
begin
  TimerDebounce.Enabled := False;
  PartialQuery := ComboBoxEditSearch.Text.Trim;

  if Length(PartialQuery) < 2 then
  begin
    TThread.Queue(nil,
      procedure
      begin
        ComboBoxEditSearch.Items.Clear;
      end);
    Exit;
  end;

  if Assigned(FCurrentAutocompleteTask) and
    (FCurrentAutocompleteTask.Status = TTaskStatus.Running) then
    FCurrentAutocompleteTask.Cancel;

  FCurrentAutocompleteTask := TTask.Run(
    procedure
    var
      Suggestions: TArray<string>;
    begin
      try
        Suggestions := FScryfallAPI.AutocompleteCards(PartialQuery);

        if TTask.CurrentTask.Status = TTaskStatus.Canceled then
          Exit;

        TThread.Queue(nil,
          procedure
          begin
            ComboBoxEditSearch.Items.BeginUpdate;
            try
              ComboBoxEditSearch.Items.Clear;
              ComboBoxEditSearch.Items.AddStrings(Suggestions);
            finally
              ComboBoxEditSearch.Items.EndUpdate;
            end;

            if Length(Suggestions) > 0 then
              ComboBoxEditSearch.DropDown
            else
              ComboBoxEditSearch.Items.Clear;
          end);
      except
        on E: Exception do
          LogStuff('Autocomplete error: ' + E.Message);
      end;
    end);
end;

procedure TForm1.WebBrowser1DidFinishLoad(ASender: TObject);
begin
  if SameText(WebBrowser1.URL, SAboutBlank) then
  begin
    WebBrowserInitialized := True;

  end
  else
  begin
    WebBrowser1.EvaluateJavaScript(JScript);

  end;
end;

procedure TForm1.DelayTimerTimer(Sender: TObject);
begin
  if BrIsLoaded or not WebBrowserInitialized then
  begin
    Exit;
  end;

  AmOnline := FScryfallAPI.IsInternetAvailable;
  if not AmOnline then
  begin
    DelayTimer.Enabled := False;
    ShowMessage('Internet not available');
    Exit;
  end;

  // Fetch a random card asynchronously
  TTask.Run(
    procedure
    var
      RandomCard: TCardDetails;
    begin
      try
        RandomCard := FScryfallAPI.GetRandomCard;
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(FCardDisplayManager) then
            begin

              FCardDisplayManager.AddCardToListView(RandomCard);
              BrIsLoaded := True; // Mark as loaded
              DelayTimer.Enabled := False; // Disable after successful run

              if ListViewCards.Items.Count > 0 then
              begin
                ListViewCards.Selected := ListViewCards.Items[0];
                FCardDisplayManager.ShowCardDetails(TCardDetailsObject
                  (ListViewCards.Items[0].TagObject).CardDetails);
              end;
            end;
          end);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              ShowMessage(S_ERROR_FETCHING_RANDOM_CARD + E.Message);
              LogStuff('Failed to fetch random card: ' + E.Message, ERROR);
              DelayTimer.Enabled := False;
              Exit;
            end);
      end;
    end).Start;
end;

function DecodeURL(const EncodedURL: string): string;
begin
  Result := TNetEncoding.URL.Decode(EncodedURL);
end;

procedure TestIncludeExtras;
var
  Query: TScryfallQuery;
  DecodedURL: string;
begin
  Query := TScryfallQuery.Create;
  try
    Query.IncludeExtras(True);
    DecodedURL := DecodeURL(Query.BuildURL);
    Assert(DecodedURL.Contains('&include_extras=true'),
      'IncludeExtras not included in URL.');
    LogStuff('Test passed: ' + DecodedURL, DEBUG);
  finally
    Query.Free;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Query: TScryfallQuery;
  SelectedRarity: TRarity;
  SelectedSetCode: string;
  SelectedColors: string;
  UniqueMode: string;
  // MaxBudget: Currency; // For budget-based filtering
begin
  if ComboBoxEditSearch.Text.Trim.IsEmpty then
    Exit;

  Button1.Enabled := False;
  MultiViewFilters.HideMaster;

  SelectedRarity := rAll; // Default to all rarities
  if ComboBoxRarity.Text <> S_ALL_RARITIES then
  begin
    if AnsiSameText(ComboBoxRarity.Text.Trim, S_MYTHIC_RARE) then
      SelectedRarity := rMythic
    else if AnsiSameText(ComboBoxRarity.Text.Trim, S_RARE) then
      SelectedRarity := rRare
    else if AnsiSameText(ComboBoxRarity.Text.Trim, S_UNCOMMON) then
      SelectedRarity := rUncommon
    else if AnsiSameText(ComboBoxRarity.Text.Trim, S_COMMON) then
      SelectedRarity := rCommon
    else if AnsiSameText(ComboBoxRarity.Text.Trim, S_SPECIAL) then
      SelectedRarity := rSpecial
    else
      raise Exception.Create('Invalid rarity selected.');
  end;
  Query := TScryfallQuery.Create;
  try
    if (ComboBoxSetCode.Selected <> nil) and
      (ComboBoxSetCode.Selected.Text <> S_ALL_SETS) then
      SelectedSetCode := ComboBoxSetCode.Selected.Text.Split([S])[0]
    else
      SelectedSetCode := '';

//    if ComboBoxColors.Text = S_ALL_COLORS then
//      SelectedColors := ''
//    else
//      SelectedColors := ComboBoxColors.Text;
    SelectedColors := GetSelectedColors; // Get selected colors from the ListBox

    // Determine the Unique mode
    UniqueMode := System.StrUtils.IfThen(Switch1.IsChecked, 'prints', '');

    // Example: Using the CreateBudgetQuery helper
    // MaxBudget := 10.00; // Example: Filter cards with a price below $10

    // Add additional filters based on user inputs
    Query.WithName(ComboBoxEditSearch.Text).WithSet(SelectedSetCode)
      .WithRarity(SelectedRarity).WithColors(SelectedColors).Unique(UniqueMode)
      .OrderBy('name', 'asc').IncludeExtras(False);

    // Pass the query to FCardDisplayManager
    FCardDisplayManager.ExecuteQuery(Query, OnSearchComplete);

    // Do not free Query here. It will be managed by FCardDisplayManager.
  except
    Query.Free;
    raise;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MultiViewFilters.ShowMaster;
end;

procedure TestCreateBudgetQuery;
var
  Query: TScryfallQuery;
begin
  try
    Query := TScryfallQueryHelper.CreateBudgetQuery(10.0);
    // Example budget: $10
    try
      // Display the generated query for debugging
      Query.LogQueryState('TestCreateBudgetQuery');

      // Execute the query (replace with your method to handle API calls)
      // E.g., ScryfallAPI.ExecuteQuery(Query);
    finally
      Query.Free;
    end;
  except
    on E: Exception do
    begin
      LogStuff('TestCreateBudgetQuery failed: ' + E.Message, ERROR);
    end;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  TestCreateBudgetQuery;
end;

// ProcessScryfallBulkFile('C:\Users\raygu\AppData\Roaming\MTGCardFetch\oracle-cards-20250112100215.json');
// "C:\Users\raygu\AppData\Roaming\MTGCardFetch\oracle-cards-20250112100215.json"

procedure TForm1.ButtonNextPageClick(Sender: TObject);
begin
  if not FCardDisplayManager.HasMorePages then
    Exit;

  ButtonNextPage.Enabled := False;
  MultiViewFilters.HideMaster;

  FCardDisplayManager.LoadNextPage(OnSearchComplete);
  // ListViewCards.ItemIndex := ListViewCards.ItemCount -1;
  // ListViewCards.OnItemClick(ListViewCards, ListViewCards.Items[ListViewCards.ItemCount -1]);

end;

procedure TForm1.ComboBoxEditSearchChange(Sender: TObject);
begin
//  TimerDebounce.Enabled := False;
//  TimerDebounce.Enabled := True;
end;

procedure TForm1.ComboBoxEditSearchItemClick(const Sender: TObject;
const AItem: TListBoxItem);
begin
  if Assigned(AItem) then
  begin
    FIsProgrammaticChange := True;
    try
      ComboBoxEditSearch.Text := AItem.Text;
      LogStuff(Format('User selected suggestion: "%s"', [AItem.Text]));
    finally
      FIsProgrammaticChange := False;
    end;
  end
  else
  begin
    LogStuff('Selected item is not assigned (nil).');
  end;
end;

procedure TForm1.ComboBoxEditSearchKeyDown(Sender: TObject; var Key: Word;
var KeyChar: WideChar; Shift: TShiftState);
begin
  if FIsProgrammaticChange then
    Exit;

  case Key of

    vkReturn:
      begin
        Button1Click(Sender);

      end;

    vkDown:
      begin
        if ComboBoxEditSearch.Items.Count > 0 then
          ComboBoxEditSearch.DropDown;
        Key := 0;
      end;
    vkEscape:
      begin
        ComboBoxEditSearch.Items.Clear;
        Key := 0;
      end;
  end;
end;

initialization

end.
