unit CardDisplayManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.WebBrowser,
  ScryfallData, System.Threading, FMX.Dialogs, FMX.ListBox,
  CardDisplayHelpers, Template, Logger, SGlobalsZ, MLogic, APIConstants,
  ScryfallTypes, ScryfallQueryBuilder,System.StrUtils;

type
  TCardDetailsObject = class(TObject)
  public
    CardDetails: TCardDetails;
    constructor Create(const ACardDetails: TCardDetails);
  end;

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


    procedure DisplayCardArtworks(const Query: TScryfallQuery; const OnComplete: TProc<Boolean>);
    procedure ClearListViewItems;
    procedure HandleSetDetails(const CardDetails: TCardDetails; const SetDetails: TSetDetails);
    function ApplyLocalFilters(const Cards: TArray<TCardDetails>): TArray<TCardDetails>;
    procedure InitializeWebBrowser;

  public
    constructor Create(AListView: TListView; AWebBrowser: TWebBrowser; AScryfallAPI: TScryfallAPI);
    destructor Destroy; override;

    // Primary query methods
    procedure ExecuteQuery(const Query: TScryfallQuery; const OnComplete: TProc<Boolean>);
    function GetCurrentQuery: TScryfallQuery;
    procedure LoadNextPage(const OnComplete: TProc<Boolean>);

    // Display and card handling methods
    procedure ShowCardDetails(const CardDetails: TCardDetails);
    procedure DisplayCardInBrowser(const CardDetails: TCardDetails; const Rulings: TArray<TRuling>);
    procedure AddCardToListView(const Card: TCardDetails);
    procedure LoadAllCatalogs(const ComboBoxes: TDictionary<string, TComboBox>);
    procedure LoadRandomCard(const OnComplete: TProc<Boolean>);

    // Helper methods
    procedure SetupDefaultFilters(Query: TScryfallQuery);
    procedure UpdateProgress(Current, Total: Integer);

    property OnProgressUpdate: TProc<Integer> read FOnProgressUpdate write FOnProgressUpdate;
    property CurrentPage: Integer read FCurrentPage;
    property HasMorePages: Boolean read FHasMorePages;
    property CardDataList: TList<TCardDetails> read FCardDataList;
  end;

implementation

{ TCardDetailsObject }

constructor TCardDetailsObject.Create(const ACardDetails: TCardDetails);
begin
  inherited Create;
  CardDetails := ACardDetails;
end;

{ TCardDisplayManager }

constructor TCardDisplayManager.Create(AListView: TListView; AWebBrowser: TWebBrowser;
  AScryfallAPI: TScryfallAPI);
begin
  inherited Create;
  FListView := AListView;
  FWebBrowser := AWebBrowser;
  FScryfallAPI := AScryfallAPI;
  FCardDataList := TList<TCardDetails>.Create;
  FCurrentPage := 1;
  FHasMorePages := False;
  if Assigned(FCurrentQuery) then
begin
  FCurrentQuery.Free; // Free the existing query
  FCurrentQuery := nil;
end;

FCurrentQuery := TScryfallQuery.Create;

  LogStuff('FCurrentQuery assigned. Address: ' + IntToStr(NativeInt(FCurrentQuery)));
  InitializeWebBrowser;
end;

destructor TCardDisplayManager.Destroy;
begin
  if Assigned(FCardDataList) then
    FreeAndNil(FCardDataList);

 if Assigned(FCurrentQuery) then
  FCurrentQuery.Free;
   FCurrentQuery := nil;
   LogStuff('TScryfallQuery destroyed. Address: ' + IntToStr(NativeInt(Self)));
  inherited;
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
  if Assigned(FCurrentQuery) then
    FCurrentQuery.Free; // Free the current query safely

  FCurrentQuery := Query.Clone; // Clone the passed query to ensure ownership
  FCurrentPage := 1;
  ClearListViewItems;
  DisplayCardArtworks(FCurrentQuery, OnComplete);
end;

function TCardDisplayManager.GetCurrentQuery: TScryfallQuery;
begin
  Result := FCurrentQuery;
end;

procedure TCardDisplayManager.SetupDefaultFilters(Query: TScryfallQuery);
begin
  Query.IncludeExtras(False)
       .OrderBy('name')
       .Page(FCurrentPage); // Call the Page method here
end;

procedure TCardDisplayManager.LoadNextPage(const OnComplete: TProc<Boolean>);
var
  ClonedQuery: TScryfallQuery;
begin
  if not Assigned(FCurrentQuery) then
  begin
    LogStuff('LoadNextPage: FCurrentQuery is nil.');
    OnComplete(False);
    Exit;
  end;

  if not FCurrentQuery.AreFiltersValid then
  begin
    LogStuff('LoadNextPage: FCurrentQuery filters are invalid.');
    OnComplete(False);
    Exit;
  end;

  ClonedQuery := FCurrentQuery.Clone; // Clone the current query for the next page
  try
    Inc(FCurrentPage);
    ClonedQuery.SetPage(FCurrentPage);
    DisplayCardArtworks(ClonedQuery, OnComplete);
  finally
    ClonedQuery.Free; // Free the cloned query after use
  end;
end;

procedure TCardDisplayManager.DisplayCardArtworks(const Query: TScryfallQuery; const OnComplete: TProc<Boolean>);
begin

  FCardDataList.Clear;

  SetupDefaultFilters(Query);

  FScryfallAPI.SearchAllCardsAsync(
    Query.GetSearchTerm,
    Query.GetSetCode,
    Query.GetRarityString,
    Query.GetColorCode,
    False,
    Query.GetShowUnique,
    Query.GetCurrentPage,
    procedure(Success: Boolean; Cards: TArray<TCardDetails>; HasMore: Boolean; ErrorMsg: string)
    var
      FilteredCards: TArray<TCardDetails>;
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          if not Success then
          begin
            if Assigned(OnComplete) then
              OnComplete(False);
            Exit;
          end;

          FilteredCards := ApplyLocalFilters(Cards);

          for var CardIndex := 0 to High(FilteredCards) do
          begin
            if IsCardValid(FilteredCards[CardIndex]) then
            begin
              FCardDataList.Add(FilteredCards[CardIndex]);
              AddCardToListView(FilteredCards[CardIndex]);
              UpdateProgress(CardIndex + 1, Length(FilteredCards));
            end
            else
              LogStuff(Format('Skipping invalid card at index %d: %s',
                [CardIndex, FilteredCards[CardIndex].CardName]));
          end;

          FHasMorePages := HasMore;

          if Assigned(OnComplete) then
            OnComplete(True);
        end);
    end);
end;

procedure TCardDisplayManager.UpdateProgress(Current, Total: Integer);
begin
  if Assigned(FOnProgressUpdate) then
    FOnProgressUpdate(Round((Current / Total) * 100));
end;

function TCardDisplayManager.ApplyLocalFilters(const Cards: TArray<TCardDetails>): TArray<TCardDetails>;
begin
  Result := Cards;  // Base implementation - extend as needed for local filtering
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
            FCardDataList.Clear;
            AddCardToListView(RandomCard);

            if Assigned(OnComplete) then
              OnComplete(True);
          end);
      except
        on E: Exception do
          TThread.Queue(nil,
            procedure
            begin
              LogStuff('Error fetching random card: ' + E.Message);
              if Assigned(OnComplete) then
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
    LogStuff('Skipping card - missing name or ID');
    Exit;
  end;

  CardDetailsObj := TCardDetailsObject.Create(Card);
  RareStr := RarityToString[Card.Rarity];
  RareStr := RareStr.Substring(0,1).ToUpper + RareStr.Substring(1);

  ListViewItem := FListView.Items.Add;
  ListViewItem.Text := Card.CardName;
  ListViewItem.Detail := RareStr;
  ListViewItem.TagObject := CardDetailsObj;
end;

procedure TCardDisplayManager.HandleSetDetails(const CardDetails: TCardDetails; const SetDetails: TSetDetails);
begin
  var UpdatedCard := CardDetails;
  UpdatedCard.SetName := SetDetails.Name;
  UpdatedCard.SetIconURI := Format('data:image/svg+xml;base64,%s', [
    GetSetIconAsBase64(SetDetails.IconSVGURI, SetDetails.Code)
  ]);

  DisplayCardInBrowser(UpdatedCard, []);
end;

procedure TCardDisplayManager.ShowCardDetails(const CardDetails: TCardDetails);
begin
  if CardDetails.SetCode.IsEmpty then
    Exit;

  var CachedSets := LoadSetDetailsFromJson(GetCacheFilePath(SetCacheFile));
  var SetDetails: TSetDetails;
  var FoundInCache := False;

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
            LogStuff('Failed to fetch set details: ' + E.Message);
        end;
      end);
end;

procedure TCardDisplayManager.DisplayCardInBrowser(const CardDetails: TCardDetails;
  const Rulings: TArray<TRuling>);
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
              ShowMessage('Error displaying card: ' + E.Message);
            end);
      end;
    end);
end;

procedure TCardDisplayManager.LoadAllCatalogs(const ComboBoxes: TDictionary<string, TComboBox>);
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
        LogStuff(Format('Catalog "%s" is missing from the fetched data.', [CatalogName]));
    end;
  finally
    Catalogs.Free;
  end;
end;

end.
