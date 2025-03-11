unit CardDisplayManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView, FMX.WebBrowser,
  ScryfallData, System.Threading, FMX.Dialogs, FMX.ListBox, CardDisplayHelpers,
  Template, Logger, CardMainData, MLogic, APIConstants, ScryfallFilterType,
  ScryfallQuery, System.SyncObjs, FMX.StdCtrls, FMX.Types, Math, CardMetaData;

type
  TOnQueryComplete = reference to procedure(Success: Boolean;
    const ErrorMsg: string);

  TCardDisplayManager = class
  private
    FListView: TListView;
    FWebBrowser: TWebBrowser;
    FScryfallAPI: TScryfallAPI;
    // FCardDataList: TList<TCardDetails>;
    FOnProgressUpdate: TProc<Integer>;
    FCurrentPage: Integer;
    FHasMorePages: Boolean;
    FCurrentQuery: TScryfallQuery;
    FCriticalSection: TCriticalSection;
    FListBoxColors: TListBox;
    FColorCheckBoxes: TObjectList<TCheckBox>;
    FTotalCards: Integer;
    FCardDataList: TObjectList<TCardDetails>;

    procedure InitializeWebBrowser;

    procedure DisplayCardArtworks(const Query: TScryfallQuery;
      const OnComplete: TOnQueryComplete);

    procedure HandleSetDetails(const CardDetails: TCardDetails;
      const SetDetails: TSetDetails);

    // function ApplyLocalFilters(const Cards: TArray<TCardDetails>)
    // : TArray<TCardDetails>;
    procedure SafeFreeAndNil<T: class>(var Obj: T);

  public
    procedure ClearListViewItems;
    constructor Create(AListView: TListView; AWebBrowser: TWebBrowser;
      AScryfallAPI: TScryfallAPI); overload;
    destructor Destroy; override;
    procedure PopulateColorListBox;
    function GetSelectedColors: string;

    // Assign this to link the ListBoxColors from the form
    property ListBoxColors: TListBox read FListBoxColors write FListBoxColors;

    procedure ExecuteQuery(const Query: TScryfallQuery;
      const OnComplete: TOnQueryComplete);

    procedure LoadNextPage(const OnComplete: TOnQueryComplete);
    procedure LoadPreviousPage(const OnComplete: TOnQueryComplete);

    function GetCurrentQuery: TScryfallQuery;

    procedure ShowCardDetails(const CardDetails: TCardDetails);

    procedure DisplayCardInBrowser(const CardDetails: TCardDetails;
      const Rulings: TArray<TRuling>);

    procedure AddCardToListView(const Card: TCardDetails);
    procedure LoadAllCatalogs(const ComboBoxes: TDictionary<string, TComboBox>);
    procedure LoadRandomCard(const OnComplete: TProc<Boolean>);
    procedure SetupDefaultFilters(Query: TScryfallQuery);
    procedure UpdateProgress(Current, Total: Integer);
    function GetCardRulings(const UUID: string): TArray<TRuling>;

    property OnProgressUpdate: TProc<Integer> read FOnProgressUpdate
      write FOnProgressUpdate;
    property CurrentPage: Integer read FCurrentPage;
    property HasMorePages: Boolean read FHasMorePages;
    // property CardDataList: TList<TCardDetails> read FCardDataList;
    property TotalCards: Integer read FTotalCards write FTotalCards;
  end;

implementation

uses
  ScryfallDataHelper;

{ TCardDisplayManager }
constructor TCardDisplayManager.Create(AListView: TListView;
  AWebBrowser: TWebBrowser; AScryfallAPI: TScryfallAPI);
begin
  inherited Create;
  FListView := AListView;
  FWebBrowser := AWebBrowser;
  FScryfallAPI := AScryfallAPI;
  FCardDataList := TObjectList<TCardDetails>.Create(True);
  // FCardDataList := TList<TCardDetails>.Create;
  FCurrentPage := 1;
  FHasMorePages := False;
  FCurrentQuery := TScryfallQuery.Create;
  FCriticalSection := TCriticalSection.Create;
  FColorCheckBoxes := TObjectList<TCheckBox>.Create(True); // Owns objects
  InitializeWebBrowser;
end;

destructor TCardDisplayManager.Destroy;
begin
  FCardDataList.Free;
  // SafeFreeAndNil < TList < TCardDetails >> (FCardDataList);
  SafeFreeAndNil<TScryfallQuery>(FCurrentQuery);
  FCriticalSection.Free;
  FColorCheckBoxes.Free;
  inherited;
end;

function ProcessTemplate(const Template: string;
  const Replacements: TDictionary<string, string>): string;
var
  Key, Value: string;
begin
  Result := Template;
  for Key in Replacements.Keys do
  begin
    Value := Replacements[Key];
    Result := StringReplace(Result, Key, Value, [rfReplaceAll]);
  end;
end;

procedure AddAllReplacements(Replacements: TDictionary<string, string>;
  const CardDetails: TCardDetails; const Rulings: TArray<TRuling>);
begin
  AddCoreReplacements(Replacements, CardDetails);
  AddImageReplacements(Replacements, CardDetails);
  AddLegalitiesReplacements(Replacements, CardDetails);
  AddPricesReplacements(Replacements, CardDetails);
  AddBadgesReplacements(Replacements, CardDetails);
  AddKeywordsReplacement(Replacements, CardDetails);

  // Process rulings into HTML
  var
  RulingsHtml := '<div class="rulings-section"><h3>Card Rulings</h3>';
  if Length(Rulings) > 0 then
  begin
    for var Ruling in Rulings do
      RulingsHtml := RulingsHtml +
        Format('<div class="ruling-item"><p><strong>Source:</strong> %s</p>' +
        '<p><strong>Date:</strong> %s</p><p>%s</p></div>',
        [EncodeHTML(Ruling.Source), EncodeHTML(Ruling.PublishedAt),
        EncodeHTML(TWrapperHelper.GetUtf8String(Ruling.Comment))]);
  end;
  RulingsHtml := RulingsHtml + '</div>';
  Replacements.AddOrSetValue('{{Rulings}}', RulingsHtml);
end;

procedure TCardDisplayManager.PopulateColorListBox;

  procedure AddColor(const ColorName: string; const TagValue: string);
  var
    ListBoxItem: TListBoxItem;
    CheckBox: TCheckBox;
  begin
    Assert(not TagValue.IsEmpty, Format('TagValue for %s must not be empty.',
      [ColorName]));

    ListBoxItem := TListBoxItem.Create(FListBoxColors);
    ListBoxItem.Parent := FListBoxColors;
    ListBoxItem.Height := 20; // Adjust height as needed

    CheckBox := TCheckBox.Create(ListBoxItem);
    CheckBox.Parent := ListBoxItem;
    CheckBox.Align := TAlignLayout.Client;
    CheckBox.Text := ColorName;
    CheckBox.TagString := TagValue;

    FColorCheckBoxes.Add(CheckBox);
  end;

begin
  if not Assigned(FListBoxColors) then
    raise Exception.Create('FListBoxColors is not assigned.');

  FListBoxColors.Clear;
  FColorCheckBoxes.Clear;

  AddColor('White', 'W');
  AddColor('Blue', 'U');
  AddColor('Black', 'B');
  AddColor('Red', 'R');
  AddColor('Green', 'G');
  AddColor('Colorless', 'C');
  AddColor('Multicolor', 'M');
end;

function TCardDisplayManager.GetSelectedColors: string;
var
  CheckBox: TCheckBox;
  Colors: string;
  IncludeMulticolor: Boolean;
begin
  Colors := '';
  IncludeMulticolor := False;

  for CheckBox in FColorCheckBoxes do
  begin
    if CheckBox.IsChecked then
    begin
      if CheckBox.TagString = 'M' then
        IncludeMulticolor := True
      else
        Colors := Colors + CheckBox.TagString;
    end;
  end;

  if IncludeMulticolor then
    if Colors.IsEmpty then
      Result := 'M'
    else
      Result := '>' + Colors
  else
    Result := Colors;

  LogStuff('TCardDisplayManager.GetSelectedColors Output: ' + Result);
end;

procedure LogQueryState(Query: TScryfallQuery; const Context: string);
begin
  if Assigned(Query) then
    LogStuff(Format
      ('%s - Query State: Options: IncludeExtras: %s, UniqueMode: %s, Sort: %s, Direction: %s, Page: %d',
      [Context, BoolToStr(Query.Options.IncludeExtras, True),
      Query.Options.UniqueMode, Query.Options.Sort, Query.Options.Direction,
      Query.Options.Page]), DEBUG)
  else
    LogStuff(Format('%s - Query is nil.', [Context]), ERROR);
end;

procedure TCardDisplayManager.SafeFreeAndNil<T>(var Obj: T);
begin
  if Assigned(Obj) then
    FreeAndNil(Obj);
end;

procedure TCardDisplayManager.InitializeWebBrowser;
begin
  if Assigned(FWebBrowser) then
    FWebBrowser.LoadFromStrings
      ('<html><head></head><body><h2>Loading...</h2></body></html>',
      'text/html;charset=utf-8');
end;

procedure TCardDisplayManager.ClearListViewItems;
var
  i: Integer;
begin
  if not Assigned(FListView) then Exit;

  for i := 0 to FListView.Items.Count - 1 do
    FListView.Items[i].TagObject := nil; // Break external references

  FListView.Items.Clear;
  FCardDataList.Clear; // Now TObjectList can safely free objects
end;




procedure TCardDisplayManager.ExecuteQuery(const Query: TScryfallQuery;
  const OnComplete: TOnQueryComplete);
begin
  FWebBrowser.Navigate('about:blank');
  FCriticalSection.Enter;
  try
    // Clone and store the query
    FreeAndNil(FCurrentQuery);
    FCurrentQuery := Query.Clone;
    FCurrentPage := 1; // Reset pagination
    // LogStuff(Format('ExecuteQuery - After Cloning - Query State: %s',
    // [FCurrentQuery.Options.ToString]), DEBUG);
  finally
    FCriticalSection.Leave;
  end;
  ClearListViewItems;
  // Display card artworks
  DisplayCardArtworks(FCurrentQuery, OnComplete);
end;

function TCardDisplayManager.GetCurrentQuery: TScryfallQuery;
begin
  Result := FCurrentQuery;
end;

procedure TCardDisplayManager.SetupDefaultFilters(Query: TScryfallQuery);
begin
  // LogQueryState(Query, 'SetupDefaultFilters - Before');

  // Set default sort order if not already specified
  if Query.Options.Sort.IsEmpty then
    Query.OrderBy('name', 'asc');

  // Ensure the page number is set
  if Query.Options.Page < 1 then
    Query.SetPage(1);

  // LogQueryState(Query, 'SetupDefaultFilters - After');
end;

procedure TCardDisplayManager.LoadNextPage(const OnComplete: TOnQueryComplete);
var
  ClonedQuery: TScryfallQuery;
begin
  if not Assigned(FCurrentQuery) then
  begin
    OnComplete(False, 'No current query to load next page.');
    Exit;
  end;

  FWebBrowser.Navigate('about:blank');
  ClonedQuery := FCurrentQuery.Clone;
  try
    ClonedQuery.SetPage(FCurrentPage + 1);

    DisplayCardArtworks(ClonedQuery,
      procedure(Success: Boolean; const ErrorMsg: string)
      begin
        try
          if Success then
          begin
            Inc(FCurrentPage);
            OnComplete(True, '');
          end
          else
            OnComplete(False, ErrorMsg);
        finally
          ClonedQuery.Free; // Ensure it's freed
        end;
      end);
  except
    ClonedQuery.Free;
    raise;
  end;
end;


procedure TCardDisplayManager.LoadPreviousPage(const OnComplete: TOnQueryComplete);
var
  ClonedQuery: TScryfallQuery;
begin
  if not Assigned(FCurrentQuery) then
  begin
    LogStuff('LoadPreviousPage: FCurrentQuery is nil.', ERROR);
    OnComplete(False, 'No current query to load the previous page.');
    Exit;
  end;

  if FCurrentPage <= 1 then
  begin
    LogStuff('LoadPreviousPage: Already on the first page.', WARNING);
    OnComplete(False, 'You are already on the first page.');
    Exit;
  end;

  FWebBrowser.Navigate('about:blank');
  ClonedQuery := FCurrentQuery.Clone;
  try
    ClonedQuery.SetPage(FCurrentPage - 1); // Decrease the page number

    DisplayCardArtworks(ClonedQuery,
      procedure(Success: Boolean; const ErrorMsg: string)
      begin
        try
          if Success then
          begin
            Dec(FCurrentPage); // Decrease the page counter only on success
            LogStuff(Format('LoadPreviousPage: FCurrentPage decremented to %d.', [FCurrentPage]), DEBUG);
            OnComplete(True, '');
          end
          else
          begin
            LogStuff('LoadPreviousPage: Failed to load the previous page.', ERROR);
            OnComplete(False, ErrorMsg);
          end;
        finally
          ClonedQuery.Free; // Same as LoadNextPage, free it
        end;
      end);
  except
    ClonedQuery.Free;
    raise;
  end;
end;


procedure TCardDisplayManager.DisplayCardArtworks(const Query: TScryfallQuery;
const OnComplete: TOnQueryComplete);
begin
  // FCardDataList.Clear;
  SetupDefaultFilters(Query);

  FScryfallAPI.SearchAllCardsAsync(Query,
    procedure(Success: Boolean; Cards: TArray<TCardDetails>; HasMore: Boolean;
      TotalCards: Integer; ErrorMsg: string)
    begin
      // var
      // TotalPages: Integer;
      TThread.Synchronize(nil,
        procedure
        begin
          if not Success then
          begin
            OnComplete(False, ErrorMsg);
            Exit;
          end;

          // FTotalCards := TotalCards;
          // TotalPages := Ceil(FTotalCards / 175);
          FListView.BeginUpdate;
          try
            for var Card in Cards do
            begin
              // FCardDataList.Add(Card);
              AddCardToListView(Card);
            end;
          finally
            FListView.EndUpdate;
          end;

          FTotalCards := TotalCards;
          FHasMorePages := HasMore;
          OnComplete(True, '');
        end);
    end);
end;

procedure TCardDisplayManager.UpdateProgress(Current, Total: Integer);
begin
  if Assigned(FOnProgressUpdate) then
    FOnProgressUpdate(Round((Current / Total) * 100));
end;

procedure TCardDisplayManager.LoadRandomCard(const OnComplete: TProc<Boolean>);
begin
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
            ClearListViewItems;
            // FCardDataList.Clear;
            AddCardToListView(RandomCard);
            RandomCard.Free;
            OnComplete(True);
          end);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              LogStuff('Error fetching random card: ' + E.Message, ERROR);
              OnComplete(False);
            end);
      end;
    end);
end;

function TCardDisplayManager.GetCardRulings(const UUID: string)
  : TArray<TRuling>;
begin
  Result := FScryfallAPI.GetCardRulings(UUID);
end;

procedure TCardDisplayManager.AddCardToListView(const Card: TCardDetails);
var
  CardDetailsObj: TCardDetails;
begin
  if (not Assigned(FListView)) or Card.CardName.IsEmpty then Exit;

  CardDetailsObj := TCardDetails.Create;
  CardDetailsObj.Assign(Card);
  FCardDataList.Add(CardDetailsObj); // Stores card and owns it

  var ListViewItem := FListView.Items.Add;
  ListViewItem.Text := Card.CardName;
  ListViewItem.Detail := Card.Rarity.ToString;
  ListViewItem.ButtonText := 'Save Card';
  ListViewItem.TagObject := CardDetailsObj; // Just a reference, no ownership
end;


procedure TCardDisplayManager.HandleSetDetails(const CardDetails: TCardDetails;
const SetDetails: TSetDetails);
begin
  var
    RawSvg: string;
  RawSvg := GetSetIconAsRawSVG(SetDetails.IconSVGURI, SetDetails.Code);
  var
  UpdatedCard := CardDetails;
  UpdatedCard.SetName := SetDetails.Name;
  RawSvg := Trim(RawSvg);
  UpdatedCard.SetIconURI := Trim(RawSvg);

  DisplayCardInBrowser(UpdatedCard, []);
end;

procedure TCardDisplayManager.ShowCardDetails(const CardDetails: TCardDetails);
var
  CachedSets: TArray<TSetDetails>;
  SetDetails: TSetDetails;
  FoundInCache: Boolean;
begin
  if CardDetails.SetCode.IsEmpty then
    Exit;

  CachedSets := LoadSetDetailsFromJson(GetCacheFilePath(SetCacheFile));
  try
    FoundInCache := False;

    for var CachedSet in CachedSets do
    begin
      if CachedSet.Code = CardDetails.SetCode then
      begin
        SetDetails := CachedSet;
        FoundInCache := True;
        Break;
      end;
    end;

    if FoundInCache then
      HandleSetDetails(CardDetails, SetDetails)
    else
      TTask.Run(
        procedure
        begin
          try
            SetDetails := FScryfallAPI.GetSetByCode(CardDetails.SetCode);
            try
              TThread.Synchronize(nil,
                procedure
                begin
                  HandleSetDetails(CardDetails, SetDetails);
                end);
            finally
              SetDetails.Free;
            end;
          except
            on E: Exception do
              LogStuff('Failed to fetch set details: ' + E.Message, ERROR);
          end;
        end);

  finally
    FreeSetDetailsArray(CachedSets);
  end;
end;

procedure TCardDisplayManager.DisplayCardInBrowser(const CardDetails
  : TCardDetails; const Rulings: TArray<TRuling>);
begin
  TTask.Run(
    procedure
    var
      Template: string;
      Replacements: TDictionary<string, string>;
      FinalHTML: string;
    begin
      try
        // Load base HTML template
        Template := HtmlTemplate;
        Replacements := TDictionary<string, string>.Create;
        try
          // Add all required placeholders
          AddAllReplacements(Replacements, CardDetails, Rulings);

          // Process template with replacements
          if not Template.Contains('<script>') then
            FinalHTML := ProcessTemplate(Template, Replacements) + JScript
          else
            FinalHTML := ProcessTemplate(Template, Replacements);
        finally
          Replacements.Free;
        end;

        // Load the final HTML into the browser on the main UI thread
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(FWebBrowser) then
              FWebBrowser.LoadFromStrings(FinalHTML, 'text/html;charset=utf-8');
          end);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              LogStuff('Error displaying card: ' + E.ClassName + ', Message: ' +
                E.Message, ERROR);
            end);
      end;
    end);
end;

procedure TCardDisplayManager.LoadAllCatalogs(const ComboBoxes
  : TDictionary<string, TComboBox>);
var
  Catalogs: TDictionary<string, TScryfallCatalog>;
  FileName: string;
  CatalogName: string;
  SortedList: TStringList;
  Item: string;
  Catalog: TScryfallCatalog;
begin
  FileName := SCatalogsJson;
  Catalogs := TDictionary<string, TScryfallCatalog>.Create;
  try
    LoadCatalogsFromFile(FileName, Catalogs);

    if Catalogs.Count = 0 then
    begin
      for Catalog in Catalogs.Values do
        Catalog.Free;
      Catalogs := FScryfallAPI.FetchAllCatalogs;
      SaveCatalogsToFile(FileName, Catalogs);
    end;

    for CatalogName in ComboBoxes.Keys do
    begin
      if Catalogs.ContainsKey(CatalogName) then
      begin
        SortedList := TStringList.Create;
        try
          for Item in Catalogs[CatalogName].Data do
            SortedList.Add(Item);

          SortedList.Sort; // Alphabetically sort the items

          with ComboBoxes[CatalogName] do
          begin
            Items.BeginUpdate;
            try
              Items.Clear;
              Items.Assign(SortedList); // Assign sorted items
              ItemIndex := 0; // Ensure first item is selected
            finally
              Items.EndUpdate;
            end;
          end;
        finally
          SortedList.Free; // Free temporary sorted list
        end;
      end
      else
        LogStuff(Format('Catalog "%s" is missing from the fetched data.',
          [CatalogName]), ERROR);
    end;
  finally
    for Catalog in Catalogs.Values do
      Catalog.Free;
    Catalogs.Free;
  end;
end;

end.
