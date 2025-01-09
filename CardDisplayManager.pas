unit CardDisplayManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, FMX.WebBrowser,
  ScryfallAPIWrapperV2, System.Threading, FMX.Dialogs, FMX.ListBox,
  CardDisplayHelpers, Template, Logger, SGlobalsZ, MLogic, APIConstants, CardFilters;

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
    FCurrentFilter: TCardFilter;

    procedure DisplayCardArtworks(const Filter: TCardFilter; const OnComplete: TProc<Boolean>);
    procedure ClearListViewItems;
    procedure HandleSetDetails(const CardDetails: TCardDetails; const SetDetails: TSetDetails);
    function ApplyLocalFilters(const Cards: TArray<TCardDetails>): TArray<TCardDetails>;



  public
    constructor Create(AListView: TListView; AWebBrowser: TWebBrowser; AScryfallAPI: TScryfallAPI);
    destructor Destroy; override;

    // Primary filter methods
    procedure ApplyFilter(const Filter: TCardFilter; const OnComplete: TProc<Boolean>);
    function GetCurrentFilter: TCardFilter;
    procedure LoadNextPage(const OnComplete: TProc<Boolean>);

    // Display and card handling methods
    procedure ShowCardDetails(const CardDetails: TCardDetails);
    procedure DisplayCardInBrowser(const CardDetails: TCardDetails; const Rulings: TArray<TRuling>);
    procedure AddCardToListView(const Card: TCardDetails);
    procedure LoadAllCatalogs(const ComboBoxes: TDictionary<string, TComboBox>);

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
  FCurrentFilter := TCardFilter.Create;
end;

destructor TCardDisplayManager.Destroy;
begin
  FCardDataList.Free;
  inherited;
end;

procedure TCardDisplayManager.ClearListViewItems;
begin
  if Assigned(FListView) then
    FListView.Items.Clear;
end;

procedure TCardDisplayManager.ApplyFilter(const Filter: TCardFilter; const OnComplete: TProc<Boolean>);
begin
  FCurrentFilter := Filter;
  FCurrentPage := 1;
  DisplayCardArtworks(Filter, OnComplete);
end;

function TCardDisplayManager.GetCurrentFilter: TCardFilter;
begin
  Result := FCurrentFilter;
end;

function TCardDisplayManager.ApplyLocalFilters(const Cards: TArray<TCardDetails>): TArray<TCardDetails>;
begin
  Result := FilterCards(Cards, FCurrentFilter);
end;

procedure TCardDisplayManager.LoadNextPage(const OnComplete: TProc<Boolean>);
begin
  if FHasMorePages then
    Inc(FCurrentPage);

  FScryfallAPI.SearchAllCardsAsync(
    FCurrentFilter.SearchTerm,    // Using current filter
    FCurrentFilter.SetCode,
    RarityToString[FCurrentFilter.Rarity],
    FCurrentFilter.ColorCode,
    False,
    FCurrentFilter.ShowUnique,
    FCurrentPage,
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
              if Assigned(FOnProgressUpdate) then
                FOnProgressUpdate(CardIndex + 1);
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


procedure TCardDisplayManager.DisplayCardArtworks(const Filter: TCardFilter; const OnComplete: TProc<Boolean>);
begin
  FCurrentPage := 1;
  FHasMorePages := False;

  ClearListViewItems;
  FCardDataList.Clear;

  LoadNextPage(OnComplete);
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

  // Create the object
  CardDetailsObj := TCardDetailsObject.Create(Card);
 // LogStuff('Created TagObject of class: ' + CardDetailsObj.ClassName);
  RareStr := RarityToString[Card.Rarity];
  RareStr := RareStr.Substring(0,1).ToUpper + RareStr.Substring(1);

  ListViewItem := FListView.Items.Add;
  ListViewItem.Text := Card.CardName;
  ListViewItem.Detail := RareStr;
  ListViewItem.TagObject := CardDetailsObj;

  // Verify
//  LogStuff('Assigned TagObject of class: ' + ListViewItem.TagObject.ClassName);
end;


procedure TCardDisplayManager.HandleSetDetails(const CardDetails: TCardDetails;
  const SetDetails: TSetDetails);
var
  UpdatedCard: TCardDetails;
begin
  UpdatedCard := CardDetails;  // Create a copy
  UpdatedCard.SetName := SetDetails.Name;
  UpdatedCard.SetIconURI := SetDetails.IconSVGURI;
    UpdatedCard.SetIconURI := Format('data:image/svg+xml;base64,%s', [
    GetSetIconAsBase64(SetDetails.IconSVGURI, SetDetails.Code)
  ]);
  DisplayCardInBrowser(UpdatedCard, []);
end;

procedure TCardDisplayManager.ShowCardDetails(const CardDetails: TCardDetails);
begin
  if not CardDetails.SetCode.IsEmpty then
  begin
    TTask.Run(
      procedure
      var
        SetDetails: TSetDetails;
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
      end;
    end;
  finally
    Catalogs.Free;
  end;
end;

end.
