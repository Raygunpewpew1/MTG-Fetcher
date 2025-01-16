unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ExtCtrls, FMX.Ani, FMX.Edit, FMX.StdCtrls,
  FMX.WebBrowser, FMX.Skia,
  SGlobalsZ, ScryfallData,
  System.TypInfo, Math, System.Threading,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox, MLogic,

  FMX.ComboEdit, CardDisplayManager, ScryfallQueryBuilder,
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
    ComboBoxColors: TComboBox;
    ComboBoxRarity: TComboBox;
    ComboBoxAbility: TComboBox;
    ComboBoxEditSearch: TComboEdit;
    LayoutContent: TLayout;
    ListViewCards: TListView;
    WebBrowser1: TWebBrowser;
    LayoutActions: TLayout;
    Switch1: TSwitch;
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
    procedure TimerDebounceTimer(Sender: TObject);
    procedure ComboBoxEditSearchItemClick(const Sender: TObject;
      const AItem: TListBoxItem);
    procedure ComboBoxEditSearchKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure ComboBoxEditSearchChange(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);

  private
    WebBrowserInitialized: Boolean;
    FIsProgrammaticChange: Boolean;

    // SearchTerm: string;
    FCurrentAutocompleteTask: ITask;
    // HttpClient: THTTPClient;

    FCardDisplayManager: TCardDisplayManager;
    FScryfallAPI: TScryfallAPI;
    BrIsLoaded: Boolean;

    procedure OnSearchComplete(Success: Boolean);

  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

uses
  APIConstants, JsonDataObjects, Logger, Template, CardDisplayHelpers;

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
  WebBrowserInitialized := False;
  FIsProgrammaticChange := False;
  WebBrowser1.LoadFromStrings(S_ABOUT_BLANK, '');
  ComboBoxMap := TDictionary<string, TComboBox>.Create;

  FScryfallAPI := TScryfallAPI.Create;

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
    ComboBoxColors.ItemIndex := 0;
    ComboBoxRarity.ItemIndex := 0;
  end;

  DelayTimer.Enabled := True;
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
  ListViewCards.OnItemClick := nil;
  FreeAndNil(FCardDisplayManager);
  FreeAndNil(FScryfallAPI);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  WebBrowser1.Navigate(S_ABOUT_BLANK);
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
  if SameText(WebBrowser1.URL, S_ABOUT_BLANK) then
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
  if BrIsLoaded and WebBrowserInitialized then
    Exit;

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
            FCardDisplayManager.AddCardToListView(RandomCard);
            BrIsLoaded := True;
            DelayTimer.Enabled := False;

            if ListViewCards.Items.Count > 0 then
            begin
              ListViewCards.Selected := ListViewCards.Items[0];
              FCardDisplayManager.ShowCardDetails(TCardDetailsObject
                (ListViewCards.Items[0].TagObject).CardDetails);
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

procedure TForm1.Button1Click(Sender: TObject);
var
  Query: TScryfallQuery;
  SelectedRarity: TRarity;
  SelectedSetCode: string;
  SelectedColors: string;
  UniqueMode: string;
begin
  if ComboBoxEditSearch.Text.IsEmpty then
    Exit;

  Button1.Enabled := False;
  MultiViewFilters.HideMaster;
  // ProgressBar1.Value := 0;
  // LayoutControls.Enabled := False;

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
    // Handle the set code logic separately
    if (ComboBoxSetCode.Selected <> nil) and
      (ComboBoxSetCode.Selected.Text <> S_ALL_SETS) then
      SelectedSetCode := ComboBoxSetCode.Selected.Text.Split([S])[0]
    else
      SelectedSetCode := '';

    if ComboBoxColors.Text = S_ALL_COLORS then
      SelectedColors := ''
    else
      SelectedColors := ComboBoxColors.Text;

    // Determine the Unique mode using IfThen
    UniqueMode := System.StrUtils.IfThen(Switch1.IsChecked, 'prints', '');

    // Build the query
    Query.WithName(ComboBoxEditSearch.Text).WithSet(SelectedSetCode)
      .WithRarity(SelectedRarity).WithColors(SelectedColors).Unique(UniqueMode)
    // Use UniqueMode determined by IfThen
      .OrderBy('name');

    // Call your FCardDisplayManager.ExecuteQuery method
    FCardDisplayManager.ExecuteQuery(Query, OnSearchComplete);

  finally
    Query.Free;
  end;

  // LayoutControls.Enabled := True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MultiViewFilters.ShowMaster;
end;

procedure TForm1.Button4Click(Sender: TObject);
// var
// FoundCard: System.JSON.TJSONObject;
// STerm:string;
begin
  // STerm := ComboBoxEditSearch.Text.Trim;
  // FoundCard := FindCardByName('C:\Users\raygu\AppData\Roaming\MTGCardFetch\oracle-cards-20250112100215.json', STerm);
  // try
  // if Assigned(FoundCard) then
  // begin
  // // Print card details to the log
  // LogStuff('Card Found: ' + FoundCard.ToString);
  // end
  // else
  // begin
  // LogStuff('Card not found.');
  // end;
  // finally
  // FoundCard.Free; // Ensure the found card is freed to avoid memory leaks
  // end;
end;




// ProcessScryfallBulkFile('C:\Users\raygu\AppData\Roaming\MTGCardFetch\oracle-cards-20250112100215.json');
// "C:\Users\raygu\AppData\Roaming\MTGCardFetch\oracle-cards-20250112100215.json"

procedure TForm1.ButtonNextPageClick(Sender: TObject);
begin
  if not FCardDisplayManager.HasMorePages then
    Exit;

  ButtonNextPage.Enabled := False;
  FCardDisplayManager.LoadNextPage(OnSearchComplete);
end;

procedure TForm1.ComboBoxEditSearchChange(Sender: TObject);
begin
  // TimerDebounce.Enabled := False;
  // TimerDebounce.Enabled := True;
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
        Exit;
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
