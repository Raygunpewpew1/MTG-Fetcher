unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Layouts, FMX.ExtCtrls, FMX.Ani, FMX.Edit, FMX.StdCtrls,
  FMX.WebBrowser, FMX.Skia, CardMainData, ScryfallData, System.TypInfo, Math,
  FMX.Controls.Presentation, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.ListBox, MLogic, FMX.ComboEdit, CardDisplayManager, ScryfallQuery,
  System.IOUtils, System.StrUtils, FMX.MultiView, FMX.Platform, CardMetaData,
  DataModuleUnit, ISmellToast;

type
  TForm1 = class(TForm)
    StyleBook1: TStyleBook;
    MultiViewFilters: TMultiView;
    LayoutFilters: TLayout;
    ComboBoxSetCode: TComboBox;
    ComboBoxRarity: TComboBox;
    ComboBoxAbility: TComboBox;
    LayoutContent: TLayout;
    ListViewCards: TListView;
    WebBrowser1: TWebBrowser;
    LayoutActions: TLayout;
    Switch1: TSwitch;
    Button2: TButton;
    ListBoxColors: TListBox;
    LoadingLayout: TLayout;
    AniIndicator1: TAniIndicator;
    Label1: TLabel;
    Button1: TButton;
    ButtonNextPage: TButton;
    ComboBoxEditSearch: TComboEdit;
    ButtonPrevPage: TButton;
    LabelPageNumber: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListViewCardsItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure WebBrowser1DidFinishLoad(ASender: TObject);
    procedure ButtonNextPageClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

    procedure ComboBoxEditSearchKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: WideChar; Shift: TShiftState);
    procedure Button2Click(Sender: TObject);
    procedure ListViewCardsButtonClick(const Sender: TObject;
      const AItem: TListItem; const AObject: TListItemSimpleControl);
    procedure ButtonPrevPageClick(Sender: TObject);

  private
    WebBrowserInitialized: Boolean;
    FIsProgrammaticChange: Boolean;
    BrIsLoaded: Boolean;
    AmOnline: Boolean;
    procedure OnSearchComplete(Success: Boolean; const ErrorMsg: string);
    function GetSelectedRarity: TRarity;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

uses
  APIConstants, JsonDataObjects, Logger, CardDisplayHelpers, System.NetEncoding;

var
  FScryfallAPI: TScryfallAPI;
  FCardDisplayManager: TCardDisplayManager;

function TForm1.GetSelectedRarity: TRarity;
var
  RarityStr: string;
  RarityMap: TDictionary<string, TRarity>;
begin
  RarityStr := Trim(ComboBoxRarity.Text);

  if AnsiSameText(RarityStr, S_ALL_RARITIES) then
    Exit(rAll);

  RarityMap := TDictionary<string, TRarity>.Create;
  try
    RarityMap.Add(LowerCase(S_MYTHIC_RARE), rMythic);
    RarityMap.Add(LowerCase(S_RARE), rRare);
    RarityMap.Add(LowerCase(S_UNCOMMON), rUncommon);
    RarityMap.Add(LowerCase(S_COMMON), rCommon);
    RarityMap.Add(LowerCase(S_SPECIAL), rSpecial);

    if not RarityMap.TryGetValue(LowerCase(RarityStr), Result) then
      raise Exception.Create('Invalid rarity selected.');
  finally
    RarityMap.Clear;
    RarityMap.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  SetDetailsArray: TArray<TSetDetails>;
  ComboBoxMap: TDictionary<string, TComboBox>;
  CacheFileName: string;
begin
  try
    AmOnline := FScryfallAPI.IsInternetAvailable;
    if not AmOnline then
    begin
      ShowMessage('Error checking internet connection, Shutting Down');
{$IF DEFINED(MSWINDOWS) OR DEFINED(MACOS)}
      Application.Terminate;
{$ELSE}
      Halt;
{$ENDIF}
    end;
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

  FCardDisplayManager := TCardDisplayManager.Create(ListViewCards, WebBrowser1, FScryfallAPI);

  ComboBoxMap := TDictionary<string, TComboBox>.Create;
  try
    ComboBoxMap.Add(CatalogKeywordAbilities, ComboBoxAbility);
    FCardDisplayManager.LoadAllCatalogs(ComboBoxMap);
  finally
    ComboBoxMap.Clear;
    ComboBoxMap.Free;
  end;

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
    ComboBoxSetCode.BeginUpdate;
    for var SetDetails in SetDetailsArray do
      ComboBoxSetCode.Items.Add(SetDetails.Code + ' - ' + SetDetails.Name);
  finally
  ComboBoxSetCode.EndUpdate;
    FreeSetDetailsArray(SetDetailsArray);
  end;

  ComboBoxSetCode.ItemIndex := 0;
  ComboBoxRarity.ItemIndex := 0;

//  ReportMemoryLeaksOnShutdown := True;

  DataModule1.SetupDatabase('MTGSCards.db');
end;


procedure TForm1.OnSearchComplete(Success: Boolean; const ErrorMsg: string);
var
  TotalPages: Integer;
begin
  // Hide loading UI
  LoadingLayout.Visible := False;
  AniIndicator1.Enabled := False;
  Button1.Enabled := True;

  // Update pagination buttons
  ButtonNextPage.Enabled := FCardDisplayManager.HasMorePages;
  ButtonPrevPage.Enabled := FCardDisplayManager.CurrentPage > 1;

  // Handle failure case
  if not Success then
  begin
    ShowMessage('Search failed: ' + ErrorMsg);
    Exit;
  end;

  // Success
  if ListViewCards.Items.Count > 0 then
  begin
    ListViewCards.SearchVisible := True;
    ListViewCards.ItemIndex := 0;

    // Calculate total pages
    TotalPages := Ceil(FCardDisplayManager.TotalCards / 175);

    // Update page number display
    LabelPageNumber.Text := Format('Page %d of %d',
      [FCardDisplayManager.CurrentPage, TotalPages]);

    ListViewCards.OnItemClick(ListViewCards, ListViewCards.Items[0]);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  try
    FCardDisplayManager.ClearListViewItems;

   if Assigned(FCardDisplayManager) then
       FreeAndNil(FCardDisplayManager);

    if Assigned(ListViewCards) then
      ListViewCards.OnItemClick := nil;
  except
    on E: Exception do
    begin
      LogStuff('Error during Exit: ' + E.Message, ERROR);
    end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin

  // Link the ListBoxColors to the CardDisplayManager
  FCardDisplayManager.ListBoxColors := ListBoxColors;

  // Populate the color list
  FCardDisplayManager.PopulateColorListBox;

  WebBrowser1.Navigate('about:blank');

  FCardDisplayManager.LoadRandomCard(
    procedure(Success: Boolean)
    begin
      if Success then
      begin

        if ListViewCards.Items.Count > 0 then
        begin
          ListViewCards.Selected := ListViewCards.Items[0];
          // TagObject is a TCardDetails instance:
          FCardDisplayManager.ShowCardDetails(TCardDetails(ListViewCards.Items
            [0].TagObject));
        end;
      end
      else
      begin
        ShowMessage('Failed to load random card.');
      end;
    end);

  // MultiViewFilters.ShowMaster;
end;

procedure TForm1.ListViewCardsButtonClick(const Sender: TObject;
const AItem: TListItem; const AObject: TListItemSimpleControl);
var
  // Rulings: TArray<TRuling>;
  CardDetails: TCardDetails;
begin
  if AItem.TagObject is TCardDetails then
  begin
    CardDetails := TCardDetails(AItem.TagObject);
    DataModule1.SaveCardJson(CardDetails);
    ShowSkiaToast('Card ' + CardDetails.CardName + ' Saved', Self);
    // Rulings := FCardDisplayManager.GetCardRulings(CardDetails.SFID);
    // FCardDisplayManager.DisplayCardInBrowser(CardDetails, Rulings);
    // // FCardDisplayManager.ShowCardDetails(CardDetails);
  end
  else
    ShowMessage('TagObject is not TCardDetails');
end;

procedure TForm1.ListViewCardsItemClick(const Sender: TObject;
const AItem: TListViewItem);
var
  CardDetails: TCardDetails;
begin
  if not Assigned(AItem) or not Assigned(AItem.TagObject) then
  begin
    ShowMessage('Item or TagObject not assigned');
    Exit;
  end;

  if AItem.TagObject is TCardDetails then
  begin
    CardDetails := TCardDetails(AItem.TagObject);
    // CardDetails.ToJSON;
    FCardDisplayManager.ShowCardDetails(CardDetails);
  end
  else
    ShowMessage('TagObject is not TCardDetailsObject');
end;

procedure TForm1.WebBrowser1DidFinishLoad(ASender: TObject);
begin
  if Assigned(WebBrowser1) then
  begin
    WebBrowserInitialized := True;
  end;
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
  SelectedName: string;
begin
  if ComboBoxEditSearch.Text.Trim.IsEmpty then
    Exit;

  Button1.Enabled := False;
  MultiViewFilters.HideMaster;
  LoadingLayout.Visible := True;
  AniIndicator1.Enabled := True;

  SelectedRarity := GetSelectedRarity;
  SelectedName := ComboBoxEditSearch.Text.Trim;

  Query := TScryfallQuery.Create;
  try
    if ComboBoxSetCode.Selected.Text <> S_ALL_SETS then
      SelectedSetCode := ComboBoxSetCode.Selected.Text.Split([S])[0]
    else
      SelectedSetCode := '';

    SelectedColors := FCardDisplayManager.GetSelectedColors;
    UniqueMode := System.StrUtils.IfThen(Switch1.IsChecked, 'prints', '');

    Query.WithName(SelectedName)
         .WithSet(SelectedSetCode)
         .WithRarity(SelectedRarity)
         .WithColors(SelectedColors)
         .Unique(UniqueMode)
         .OrderBy(FieldReleased)
         .IncludeExtras(False);

    // ExecuteQuery clones Query, so it's safe to free
    FCardDisplayManager.ExecuteQuery(Query, OnSearchComplete);

  finally
    Query.Free;
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

  LoadingLayout.Visible := True;
  AniIndicator1.Enabled := True;

  ButtonNextPage.Enabled := False;
  // MultiViewFilters.HideMaster;
  FCardDisplayManager.ClearListViewItems;
  FCardDisplayManager.LoadNextPage(OnSearchComplete);

end;

procedure TForm1.ButtonPrevPageClick(Sender: TObject);
begin

  if FCardDisplayManager.CurrentPage <= 1 then
    Exit;

  LoadingLayout.Visible := True;
  AniIndicator1.Enabled := True;

  FCardDisplayManager.ClearListViewItems;

  FCardDisplayManager.LoadPreviousPage(OnSearchComplete);
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

    // vkDown:
    // begin
    //
    // end;
    // vkEscape:
    // begin
    //
    // end;
  end;
end;

initialization

FScryfallAPI := TScryfallAPI.Create;

finalization

FScryfallAPI.Free;

end.
