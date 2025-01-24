unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls, FMX.Ani, FMX.Edit, FMX.StdCtrls,
  FMX.WebBrowser, FMX.Skia, SGlobalsZ, ScryfallData, System.TypInfo, Math,
  System.Threading, FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.ListBox, MLogic, FMX.ComboEdit, CardDisplayManager,
  ScryfallQuery,
  System.IOUtils, System.StrUtils, FMX.MultiView, FMX.Platform;

type
  TCardLayout = class(TLayout)
  public
    destructor Destroy; override;
  end;

  TForm1 = class(TForm)
    DelayTimer: TTimer;

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

    procedure ComboBoxEditSearchKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure Button2Click(Sender: TObject);

  private
    WebBrowserInitialized: Boolean;
    FIsProgrammaticChange: Boolean;

    FCardDisplayManager: TCardDisplayManager;
    FScryfallAPI: TScryfallAPI;
    BrIsLoaded: Boolean;
    AmOnline: Boolean;

    procedure OnSearchComplete(Success: Boolean);

    // procedure PopulateColorListBox;
    // function GetSelectedColors: string;

  public
  end;

var
  Form1: TForm1;
  // ColorCheckBoxes: TObjectList<TCheckBox>;

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
  // WebBrowser1.LoadFromStrings(HtmlTemplate, '');
  ComboBoxMap := TDictionary<string, TComboBox>.Create;

  FCardDisplayManager := TCardDisplayManager.Create(ListViewCards, WebBrowser1,
    FScryfallAPI);

  // Link the ListBoxColors to the CardDisplayManager
  FCardDisplayManager.ListBoxColors := ListBoxColors;

  // Populate the color list
  FCardDisplayManager.PopulateColorListBox;

  FCardDisplayManager.OnProgressUpdate := procedure(Progress: Integer)
    begin

    end;

  try
    ComboBoxMap.Add(CatalogKeywordAbilities, ComboBoxAbility);
    FCardDisplayManager.LoadAllCatalogs(ComboBoxMap);
  finally
    ComboBoxMap.Free;
  end;

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

    ComboBoxRarity.ItemIndex := 0;
  end;


  // ColorCheckBoxes := TObjectList<TCheckBox>.Create(True);
  // PopulateColorListBox;
 // DelayTimer.Enabled := True;
end;

// function TForm1.GetSelectedColors: string;
// var
// CheckBox: TCheckBox;
// Colors: string;
// IncludeMulticolor: Boolean;
// begin
// Colors := '';
// IncludeMulticolor := False;
//
// for CheckBox in ColorCheckBoxes do
// begin
// if CheckBox.IsChecked then
// begin
// if CheckBox.TagString = 'M' then
// IncludeMulticolor := True
// else
// Colors := Colors + CheckBox.TagString;
//
// end;
// end;
//
// if IncludeMulticolor then
// begin
// if Colors.IsEmpty then
// Result := 'M'
// else
// Result := '>' + Colors;
// end
// else
// Result := Colors;
//
// end;
//
// procedure TForm1.PopulateColorListBox;
// procedure AddColor(const ColorName: string; const TagValue: string);
// var
// ListBoxItem: TListBoxItem;
// CheckBox: TCheckBox;
// begin
// Assert(not TagValue.IsEmpty, Format('TagValue for %s must not be empty.',
// [ColorName]));
//
// ListBoxItem := TListBoxItem.Create(ListBoxColors);
// ListBoxItem.Parent := ListBoxColors;
// ListBoxItem.Height := 20;
//
// CheckBox := TCheckBox.Create(ListBoxItem);
// CheckBox.Parent := ListBoxItem;
// CheckBox.Align := TAlignLayout.Client;
// CheckBox.Text := ColorName;
// CheckBox.TagString := TagValue;
//
// ColorCheckBoxes.Add(CheckBox);
// end;
//
// begin
// ListBoxColors.Clear;
// ColorCheckBoxes.Clear;
//
// AddColor('White', 'W');
// AddColor('Blue', 'U');
// AddColor('Black', 'B');
// AddColor('Red', 'R');
// AddColor('Green', 'G');
// AddColor('Colorless', 'C');
// AddColor('Multicolor', 'M');
// end;

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
      // ColorCheckBoxes.Free;
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

  WebBrowser1.Navigate('about:blank');
  MultiViewFilters.ShowMaster;
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
              BrIsLoaded := True;
              DelayTimer.Enabled := False;

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




//end;

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

  if AItem.TagObject is TCardDetailsObject then
  begin
    CardDetailsObj := TCardDetailsObject(AItem.TagObject);

    FCardDisplayManager.ShowCardDetails(CardDetailsObj.CardDetails);
  end
  else
    ShowMessage('TagObject is not TCardDetailsObject');
end;

procedure TForm1.WebBrowser1DidFinishLoad(ASender: TObject);
begin
  if Assigned(WebBrowser1) then
  begin

    WebBrowserInitialized := True;
   // WebBrowser1.EvaluateJavaScript(JScript);

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
              BrIsLoaded := True;
              DelayTimer.Enabled := False;

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

procedure TForm1.Button1Click(Sender: TObject);
var
  Query: TScryfallQuery;
  SelectedRarity: TRarity;
  SelectedSetCode: string;
  SelectedColors: string;
  UniqueMode: string;
begin
  if ComboBoxEditSearch.Text.Trim.IsEmpty then
    Exit;

  Button1.Enabled := False;
  MultiViewFilters.HideMaster;

  SelectedRarity := rAll;
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

    SelectedColors := FCardDisplayManager.GetSelectedColors;

    UniqueMode := System.StrUtils.IfThen(Switch1.IsChecked, 'prints', '');

    Query.WithName(ComboBoxEditSearch.Text).WithSet(SelectedSetCode)
      .WithRarity(SelectedRarity).WithColors(SelectedColors).Unique(UniqueMode)
      .OrderBy('released').IncludeExtras(False);

    FCardDisplayManager.ExecuteQuery(Query, OnSearchComplete);

  except
    Query.Free;
    raise;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MultiViewFilters.ShowMaster;
end;

procedure TForm1.ButtonNextPageClick(Sender: TObject);
begin
  if not FCardDisplayManager.HasMorePages then
    Exit;

  ButtonNextPage.Enabled := False;
  MultiViewFilters.HideMaster;

  FCardDisplayManager.LoadNextPage(OnSearchComplete);

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

      end;
    vkEscape:
      begin

      end;
  end;
end;

initialization

end.
