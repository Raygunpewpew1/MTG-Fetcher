unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, FMX.Types, FMX.Controls,
  FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ExtCtrls, FMX.Ani, FMX.Edit, FMX.StdCtrls,
  FMX.WebBrowser, FMX.Skia, System.Net.HttpClient,
  SGlobalsZ, ScryfallAPIWrapperV2,
  System.TypInfo, Math, System.Threading,
  FMX.Controls.Presentation, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.ListBox, MLogic,
  System.Net.HttpClientComponent,
  FMX.ComboEdit, CardDisplayManager, System.Net.URLClient, CardFilters;

type

  TCardLayout = class(TLayout)
  public
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    DelayTimer: TTimer;
    LayoutMain: TLayout;
    LayoutControls: TLayout;
    LabelSearch: TLabel;
    Button1: TButton;
    ComboBoxSetCode: TComboBox;
    ComboBoxRarity: TComboBox;
    ComboBoxColors: TComboBox;
    ButtonNextPage: TButton;
    Switch1: TSwitch;
    DisplayUniq: TLabel;
    ListViewCards: TListView;
    SplitterMain: TSplitter;
    WebBrowser1: TWebBrowser;
    LayoutButtons: TLayout;
    ProgressBar1: TProgressBar;
    LabelProgress: TLabel;
    NetHTTPClient1: TNetHTTPClient;
    TimerDebounce: TTimer;
    ComboBoxEditSearch: TComboEdit;
    StyleBook1: TStyleBook;
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

  private
    WebBrowserInitialized: Boolean;
    FIsProgrammaticChange: Boolean;

    SearchTerm: string;
    FCurrentAutocompleteTask: ITask;
    HttpClient: THTTPClient;

    AppClose: Boolean;
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
  APIConstants, JsonDataObjects, Logger, Template;

destructor TCardLayout.Destroy;
begin
  FreeAndNil(TagObject);
  inherited;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  SetDetailsArray: TArray<TSetDetails>;
  SetDetails: TSetDetails;
begin
  WebBrowserInitialized := False;
  FIsProgrammaticChange := False;
  WebBrowser1.LoadFromStrings('', '');

  FScryfallAPI := TScryfallAPI.Create;
  LogStuff('api created');

  FCardDisplayManager := TCardDisplayManager.Create(ListViewCards, WebBrowser1,
    FScryfallAPI);

  FCardDisplayManager.OnProgressUpdate := procedure(Progress: Integer)
    begin
      ProgressBar1.Value := Progress;
    end;

  //FCardDisplayManager.LoadAllCatalogs(ComboBoxSetCode);

  HttpClient := THTTPClient.Create;

  AppClose := False;

  BrIsLoaded := False;
  ListViewCards.OnItemClick := ListViewCardsItemClick;
  ComboBoxSetCode.Items.Add(S_ALL_SETS);

  try
    SetDetailsArray := FScryfallAPI.GetAllSets;
    for SetDetails in SetDetailsArray do
    begin
      ComboBoxSetCode.Items.Add(SetDetails.Code + S + SetDetails.Name);
    end;
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
  //CountLabel.Text := S_CARDS_FOUND + ListViewCards.ItemCount.ToString;

  if not Success then
    ShowMessage('Search failed. Please try again.')
  else if ListViewCards.Items.Count = 0 then
    ShowMessage('No cards found.');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  AppClose := True;
  FreeAndNil(HttpClient);

  FreeAndNil(FScryfallAPI);
  FreeAndNil(FCardDisplayManager);
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

//  LogStuff('TagObject class: ' + AItem.TagObject.ClassName);
//  LogStuff('Is TCardDetailsObject: ' +
//    BoolToStr(AItem.TagObject is TCardDetailsObject, True));

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
  Builder: TCardFilterBuilder;
  SelectedRarity: TRarity;
  SelectedSetCode: string;
  SelectedColors: string;
begin
  if ComboBoxEditSearch.Text.IsEmpty then
    Exit;

  Button1.Enabled := False;
  ProgressBar1.Value := 0;
  LayoutControls.Enabled := False;

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

  Builder := TCardFilterBuilder.Create;
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

    Builder.WithSearchTerm(ComboBoxEditSearch.Text).WithSet(SelectedSetCode)
      .WithRarity(SelectedRarity).WithColors(SelectedColors)
      .ShowUniqueOnly(Switch1.IsChecked);

    FCardDisplayManager.ApplyFilter(Builder.Build, OnSearchComplete);
  finally
    Builder.Free;

  end;
  LayoutControls.Enabled := True;

end;

procedure TForm1.ButtonNextPageClick(Sender: TObject);
begin
  if not FCardDisplayManager.HasMorePages then
    Exit;

  ButtonNextPage.Enabled := False;
  FCardDisplayManager.LoadNextPage(OnSearchComplete);
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
