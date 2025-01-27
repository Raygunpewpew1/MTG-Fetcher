﻿unit CardDisplayManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView, FMX.WebBrowser,
  ScryfallData, System.Threading, FMX.Dialogs, FMX.ListBox,
  CardDisplayHelpers, Template, Logger, SGlobalsZ, MLogic, APIConstants,
  ScryfallTypes, ScryfallQuery, System.SyncObjs, FMX.StdCtrls, FMX.Types;

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
    FListBoxColors: TListBox;
    FColorCheckBoxes: TObjectList<TCheckBox>;

    procedure ClearListViewItems;
    procedure InitializeWebBrowser;
    procedure DisplayCardArtworks(const Query: TScryfallQuery;
      const OnComplete: TProc<Boolean>);
    procedure HandleSetDetails(const CardDetails: TCardDetails;
      const SetDetails: TSetDetails);
    // function ApplyLocalFilters(const Cards: TArray<TCardDetails>)
    // : TArray<TCardDetails>;
    procedure SafeFreeAndNil<T: class>(var Obj: T);

  public
    constructor Create(AListView: TListView; AWebBrowser: TWebBrowser;
      AScryfallAPI: TScryfallAPI); overload;
    destructor Destroy; override;
    procedure PopulateColorListBox;
    function GetSelectedColors: string;

    // Assign this to link the ListBoxColors from the form
    property ListBoxColors: TListBox read FListBoxColors write FListBoxColors;

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
    function GetCardRulings(const UUID: string): TArray<TRuling>;

    property OnProgressUpdate: TProc<Integer> read FOnProgressUpdate
      write FOnProgressUpdate;
    property CurrentPage: Integer read FCurrentPage;
    property HasMorePages: Boolean read FHasMorePages;
    property CardDataList: TList<TCardDetails> read FCardDataList;
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
  FCardDataList := TList<TCardDetails>.Create;
  FCurrentPage := 1;
  FHasMorePages := False;
  FCurrentQuery := TScryfallQuery.Create;
  FCriticalSection := TCriticalSection.Create;
  FColorCheckBoxes := TObjectList<TCheckBox>.Create(True); // Owns objects
  InitializeWebBrowser;
end;

destructor TCardDisplayManager.Destroy;
begin
  SafeFreeAndNil < TList < TCardDetails >> (FCardDataList);
  SafeFreeAndNil<TScryfallQuery>(FCurrentQuery);
  FCriticalSection.Free;
  FColorCheckBoxes.Free;
  inherited;
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
    FWebBrowser.LoadFromStrings('<html><head></head><body></body></html>',
      'file:///android_asset/');
end;

procedure TCardDisplayManager.ClearListViewItems;
begin
  if Assigned(FListView) then
    FListView.Items.Clear;
end;

procedure TCardDisplayManager.ExecuteQuery(const Query: TScryfallQuery;
  const OnComplete: TProc<Boolean>);
begin
  FCriticalSection.Enter;
  try
    // Clone and store the query
    SafeFreeAndNil<TScryfallQuery>(FCurrentQuery);
    FCurrentQuery := Query.Clone;
    FCurrentPage := 1; // Reset pagination
    LogStuff(Format('ExecuteQuery - After Cloning - Query State: %s',
      [FCurrentQuery.Options.ToString]), DEBUG);
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
          LogStuff(Format('LoadNextPage: FCurrentPage incremented to %d.',
            [FCurrentPage]), DEBUG);
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
    procedure(Success: Boolean; Cards: TArray<TCardDetails>; HasMore: Boolean;
      ErrorMsg: string)
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

function TCardDisplayManager.GetCardRulings(const UUID: string)
  : TArray<TRuling>;
begin
  Result := FScryfallAPI.GetCardRulings(UUID);
end;

procedure TCardDisplayManager.AddCardToListView(const Card: TCardDetails);
var
  ListViewItem: TListViewItem;
  CardDetailsObj: TCardDetailsObject;
  RareStr: string;
begin
  if not Assigned(FListView) or Card.CardName.IsEmpty or Card.SFID.IsEmpty then
  begin
    LogStuff(Format
      ('AddCardToListView: Skipping card due to missing data. CardName: %s, SFID: %s',
      [Card.CardName, Card.SFID]), ERROR);
    Exit;
  end;
  // FListView.BeginUpdate;
  RareStr := RarityToString[Card.Rarity];
  RareStr := RareStr.Substring(0, 1).ToUpper + RareStr.Substring(1);

  CardDetailsObj := TCardDetailsObject.Create(Card);

  ListViewItem := FListView.Items.Add;
  ListViewItem.Text := Card.CardName;
  ListViewItem.Detail := RareStr;
  ListViewItem.ButtonText := 'Rulings';
  ListViewItem.TagObject := CardDetailsObj;
  // FListView.EndUpdate;
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
  UpdatedCard.SetIconURI := RawSvg;

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
      RulingsHtml: string;
    begin
      try
        Template := HtmlTemplate;
        Replacements := TDictionary<string, string>.Create;
        try
          // Add core card replacements
          AddCoreReplacements(Replacements, CardDetails);
          AddImageReplacements(Replacements, CardDetails);
          AddLegalitiesReplacements(Replacements, CardDetails);
          AddPricesReplacements(Replacements, CardDetails);
          AddBadgesReplacements(Replacements, CardDetails);
          AddKeywordsReplacement(Replacements, CardDetails);

          // Add rulings if exist
          RulingsHtml := '<div class="rulings-section">';
          RulingsHtml := RulingsHtml + '<h3>Card Rulings</h3>';
          var Comments:string;
          if Length(Rulings) > 0 then

          begin
            for var Ruling in Rulings do
            begin
             Comments := TWrapperHelper.GetUtf8String(Ruling.Comment);
              RulingsHtml := RulingsHtml + Format('<div class="ruling-item">' +
                '<p><strong>Source:</strong> %s</p>' +
                '<p><strong>Date:</strong> %s</p>' + '<p>%s</p>' + '</div>',
                [EncodeHTML(Ruling.Source), EncodeHTML(Ruling.PublishedAt),
                EncodeHTML(Comments)]);
            end;
          end
          else
          begin
            RulingsHtml := RulingsHtml;
          end;
          RulingsHtml := RulingsHtml + '</div>';

          // Add rulings to replacements
          Replacements.AddOrSetValue('{{Rulings}}', RulingsHtml);

          // Replace placeholders in the template
          for var Key in Replacements.Keys do
            Template := Template.Replace(Key, Replacements[Key]);
        finally
          Replacements.Free;
        end;

        var
          FinalHTML: string;
        FinalHTML := Template + JScript;

        // Load the final HTML into the browser
        TThread.Queue(nil,
          procedure
          begin
            if Assigned(FWebBrowser) then
              FWebBrowser.LoadFromStrings(FinalHTML, 'file:///android_asset/');
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
