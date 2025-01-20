unit CardDisplayManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView, FMX.WebBrowser,
  ScryfallData, System.Threading, FMX.Dialogs, FMX.ListBox,
  CardDisplayHelpers, Template, Logger, SGlobalsZ, MLogic, APIConstants,
  ScryfallTypes, ScryfallQueryBuilder, System.SyncObjs;

type

  TCardDisplayManager = class
  private
    FListView: TListView;
    FWebBrowser: TWebBrowser;
    FScryfallAPI: TScryfallAPI;
    FCardDataList: TList<TCardDetails>;
    FOnProgressUpdate: TProc<Integer>;
    FCurrentPage: Integer;
    FHasMorePages: Boolean;
    FCurrentQuery: TScryfallQuery;
    FCriticalSection: TCriticalSection;

    procedure ClearListViewItems;
    procedure InitializeWebBrowser;
    procedure DisplayCardArtworks(const Query: TScryfallQuery;
      const OnComplete: TProc<Boolean>);
    procedure HandleSetDetails(const CardDetails: TCardDetails;
      const SetDetails: TSetDetails);
//    function ApplyLocalFilters(const Cards: TArray<TCardDetails>)
//      : TArray<TCardDetails>;
    procedure SafeFreeAndNil<T: class>(var Obj: T);

  public
    constructor Create(AListView: TListView; AWebBrowser: TWebBrowser;
      AScryfallAPI: TScryfallAPI);
    destructor Destroy; override;

    procedure ExecuteQuery(const Query: TScryfallQuery;
      const OnComplete: TProc<Boolean>);
    function GetCurrentQuery: TScryfallQuery;
    procedure LoadNextPage(const OnComplete: TProc<Boolean>);
    procedure ShowCardDetails(const CardDetails: TCardDetails);
    procedure DisplayCardInBrowser(const CardDetails: TCardDetails;
      const Rulings: TArray<TRuling>);
    procedure AddCardToListView(const Card: TCardDetails);
    procedure LoadAllCatalogs(const ComboBoxes: TDictionary<string, TComboBox>);
    procedure LoadRandomCard(const OnComplete: TProc<Boolean>);
    procedure SetupDefaultFilters(Query: TScryfallQuery);
    procedure UpdateProgress(Current, Total: Integer);

    property OnProgressUpdate: TProc<Integer> read FOnProgressUpdate
      write FOnProgressUpdate;
    property CurrentPage: Integer read FCurrentPage;
    property HasMorePages: Boolean read FHasMorePages;
    property CardDataList: TList<TCardDetails> read FCardDataList;
  end;

implementation

{ TCardDisplayManager }

constructor TCardDisplayManager.Create(AListView: TListView;
  AWebBrowser: TWebBrowser; AScryfallAPI: TScryfallAPI);
begin
  inherited Create;
  FListView := AListView;
  FWebBrowser := AWebBrowser;
  FScryfallAPI := AScryfallAPI;
  FCardDataList := TList<TCardDetails>.Create;
  FCurrentPage := 1;
  FHasMorePages := False;
  FCurrentQuery := TScryfallQuery.Create;
  FCriticalSection := TCriticalSection.Create;
  InitializeWebBrowser;
end;

destructor TCardDisplayManager.Destroy;
begin
  SafeFreeAndNil < TList < TCardDetails >> (FCardDataList);
  SafeFreeAndNil<TScryfallQuery>(FCurrentQuery);
  FCriticalSection.Free;
  inherited;
end;

procedure LogQueryState(Query: TScryfallQuery; const Context: string);
begin
  if Assigned(Query) then
    LogStuff(Format('%s - Query State: Options: IncludeExtras: %s, UniqueMode: %s, Sort: %s, Direction: %s, Page: %d',
      [Context,
       BoolToStr(Query.Options.IncludeExtras, True),
       Query.Options.UniqueMode,
       Query.Options.Sort,
       Query.Options.Direction,
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
    FWebBrowser.LoadFromStrings('', '');
end;

procedure TCardDisplayManager.ClearListViewItems;
begin
  if Assigned(FListView) then
    FListView.Items.Clear;
end;

procedure TCardDisplayManager.ExecuteQuery(const Query: TScryfallQuery; const OnComplete: TProc<Boolean>);
begin
  FCriticalSection.Enter;
  try
    // Clone and store the query
    SafeFreeAndNil<TScryfallQuery>(FCurrentQuery);
    FCurrentQuery := Query.Clone;
    FCurrentPage := 1; // Reset pagination
    LogStuff(Format('ExecuteQuery - After Cloning - Query State: %s', [FCurrentQuery.Options.ToString]), DEBUG);
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
//  LogQueryState(Query, 'SetupDefaultFilters - Before');

  // Set default sort order if not already specified
  if Query.Options.Sort.IsEmpty then
    Query.OrderBy('name', 'asc');

  // Ensure the page number is set
  if Query.Options.Page < 1 then
    Query.SetPage(1);

//  LogQueryState(Query, 'SetupDefaultFilters - After');
end;


procedure TCardDisplayManager.LoadNextPage(const OnComplete: TProc<Boolean>);
var
  ClonedQuery: TScryfallQuery;
begin
  if not Assigned(FCurrentQuery) then
  begin
    LogStuff('LoadNextPage: FCurrentQuery is nil.', ERROR);
    OnComplete(False);
    Exit;
  end;

  ClonedQuery := FCurrentQuery.Clone;
  try
    ClonedQuery.SetPage(FCurrentPage + 1);
    LogQueryState(ClonedQuery, 'LoadNextPage - Before DisplayCardArtworks');
    DisplayCardArtworks(ClonedQuery,
      procedure(Success: Boolean)
      begin
        if Success then
        begin
          Inc(FCurrentPage); // Increment the page only after successful fetch
          LogStuff(Format('LoadNextPage: FCurrentPage incremented to %d.', [FCurrentPage]), DEBUG);
        end
        else
          LogStuff('LoadNextPage: Failed to load next page.', ERROR);
        OnComplete(Success);
      end);
  finally
    // Do NOT free ClonedQuery here as it is used in DisplayCardArtworks
  end;
end;


procedure TCardDisplayManager.DisplayCardArtworks(const Query: TScryfallQuery;
  const OnComplete: TProc<Boolean>);
begin
  FCardDataList.Clear;
  SetupDefaultFilters(Query);

  FScryfallAPI.SearchAllCardsAsync(Query,
    procedure(Success: Boolean; Cards: TArray<TCardDetails>; HasMore: Boolean; ErrorMsg: string)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          if not Success then
          begin
            OnComplete(False);
            Exit;
          end;

          for var Card in Cards do
          begin
            FCardDataList.Add(Card);
            AddCardToListView(Card);
          end;

          FHasMorePages := HasMore;
          OnComplete(True);
        end);
    end);
end;



procedure TCardDisplayManager.UpdateProgress(Current, Total: Integer);
begin
  if Assigned(FOnProgressUpdate) then
    FOnProgressUpdate(Round((Current / Total) * 100));
end;

//function TCardDisplayManager.ApplyLocalFilters(const Cards
//  : TArray<TCardDetails>): TArray<TCardDetails>;
//begin
//  Result := Cards;
//end;

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
            FCardDataList.Clear;
            AddCardToListView(RandomCard);
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

procedure TCardDisplayManager.AddCardToListView(const Card: TCardDetails);
var
  ListViewItem: TListViewItem;
  CardDetailsObj: TCardDetailsObject;
  RareStr: string;
begin
  if not Assigned(FListView) or Card.CardName.IsEmpty or Card.SFID.IsEmpty then
  begin
    LogStuff(Format('AddCardToListView: Skipping card due to missing data. CardName: %s, SFID: %s',
      [Card.CardName, Card.SFID]), ERROR);
    Exit;
  end;
//  FListView.BeginUpdate;
  RareStr := RarityToString[Card.Rarity];
  RareStr := RareStr.Substring(0, 1).ToUpper + RareStr.Substring(1);

  CardDetailsObj := TCardDetailsObject.Create(Card);

  ListViewItem := FListView.Items.Add;
  ListViewItem.Text := Card.CardName;
  ListViewItem.Detail := RareStr;
  ListViewItem.TagObject := CardDetailsObj;
//  FListView.EndUpdate;
end;


procedure TCardDisplayManager.HandleSetDetails(const CardDetails: TCardDetails;
const SetDetails: TSetDetails);
begin
  var
  UpdatedCard := CardDetails;
  UpdatedCard.SetName := SetDetails.Name;
  UpdatedCard.SetIconURI := Format('data:image/svg+xml;base64,%s',
    [GetSetIconAsBase64(SetDetails.IconSVGURI, SetDetails.Code)]);

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
          TThread.Synchronize(nil,
            procedure
            begin
              HandleSetDetails(CardDetails, SetDetails);
            end);
        except
          on E: Exception do
            LogStuff('Failed to fetch set details: ' + E.Message, ERROR);
        end;
      end);
end;

procedure TCardDisplayManager.DisplayCardInBrowser(const CardDetails
  : TCardDetails; const Rulings: TArray<TRuling>);
begin
  TTask.Run(
    procedure
    var
      Template: string;
      Replacements: TDictionary<string, string>;
    begin
      try
        Template := HtmlTemplate;
        Replacements := TDictionary<string, string>.Create;
        try
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
            if Assigned(FWebBrowser) then
              FWebBrowser.LoadFromStrings(Template, '');
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

    for CatalogName in ComboBoxes.Keys do
    begin
      if Catalogs.ContainsKey(CatalogName) then
      begin
        ComboBoxes[CatalogName].Items.Clear;
        ComboBoxes[CatalogName].Items.AddStrings(Catalogs[CatalogName].Data);
        ComboBoxes[CatalogName].ItemIndex := 0;
      end
      else
        LogStuff(Format('Catalog "%s" is missing from the fetched data.',
          [CatalogName]), ERROR);
    end;
  finally
    Catalogs.Free;
  end;
end;

end.
