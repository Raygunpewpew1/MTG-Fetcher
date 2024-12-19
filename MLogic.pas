unit MLogic;

interface

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  System.RegularExpressions, System.Hash, System.Classes,
  FMX.Dialogs, SGlobalsZ, FMX.Graphics, System.Net.HttpClient, FMX.StdCtrls,
  System.Threading,FMX.ListView.Appearances,FMX.ListView;

procedure CopyDatabaseToInternalStorage;
function GetDatabasePath: string;
function GetTemplatePath: string;
procedure CopyTemplateToInternalStorage;
function LoadTemplate(const FileName: string;
  const DefaultTemplate: string = ''): string;
function GetAppDirectory: string;
function ParseTextWithSymbolsManual(const Input: string): TArray<string>;
function GetStatusClass(const LegalityStatus: string): string;
function IsCardValid(const Card: TCardDetails): Boolean;
procedure SaveCatalogsToFile(const FileName: string;
  const Catalogs: TDictionary<string, TScryfallCatalog>);
procedure LoadCatalogsFromFile(const FileName: string;
  var Catalogs: TDictionary<string, TScryfallCatalog>);
function GetLegalStatus(const Legalities: TCardLegalities;
  const FieldName: string): string;
procedure ClearListViewItems(ListView: TListView);
const
  TemplateFileName = 'card_template.html';
  DatabaseFileName = 'Collection.db';

var
  HttpClient: THTTPClient;

implementation

uses
  JsonDataObjects, System.NetEncoding, System.Types, APIConstants;

function GetAppDirectory: string;
begin
{$IF DEFINED(MSWINDOWS)}
  Result := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
{$ELSEIF DEFINED(ANDROID)}
  Result := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
{$ELSE}
  raise Exception.Create('Unsupported platform');
{$ENDIF}
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

function GetTargetPath(const FileName: string): string;
begin
  Result := TPath.Combine(GetAppDirectory, FileName);
end;

function GetSourcePath(const FileName: string): string;
begin
{$IF DEFINED(MSWINDOWS)}
  // For development on Windows
  Result := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), FileName);
{$ELSEIF DEFINED(ANDROID)}
  // On Android, files must be included in the app's assets folder
  Result := TPath.Combine(TPath.GetDocumentsPath, FileName);
{$ELSE}
  raise Exception.Create('Unsupported platform');
{$ENDIF}
end;

{ Centralized Template Path Logic }
function GetTemplatePath: string;
begin
  Result := TPath.Combine(GetAppDirectory, TemplateFileName);
end;

{ Copy Database to Internal Storage }
function GetDatabasePath: string;
begin
  Result := TPath.Combine(GetAppDirectory, DatabaseFileName);
end;

procedure CopyDatabaseToInternalStorage;
var
  SourcePath, DestinationPath: string;
begin
  DestinationPath := GetTargetPath(DatabaseFileName);
  // Use centralized target logic
  SourcePath := GetSourcePath(DatabaseFileName); // Use centralized source logic

  if not TFile.Exists(DestinationPath) then
  begin
    try
      if TFile.Exists(SourcePath) then
        TFile.Copy(SourcePath, DestinationPath)
      else
        raise Exception.CreateFmt('Database file not found: %s', [SourcePath]);
    except
      on E: Exception do
        ShowMessage('Error copying database: ' + E.Message);
    end;
  end;
end;

{ Copy Template to Internal Storage }
procedure CopyTemplateToInternalStorage;
var
  SourcePath, DestinationPath: string;
begin
  DestinationPath := GetTargetPath(TemplateFileName);
  SourcePath := GetSourcePath(TemplateFileName);

  if not TFile.Exists(DestinationPath) then
  begin
    try
      if TFile.Exists(SourcePath) then
        TFile.Copy(SourcePath, DestinationPath)
      else
        raise Exception.CreateFmt('Template file not found: %s', [SourcePath]);
    except
      on E: Exception do
        ShowMessage('Error copying template: ' + E.Message);
    end;
  end;
end;

function LoadTemplate(const FileName: string;
  const DefaultTemplate: string = ''): string;
var
  FullPath: string;
begin
  // Ensure the template exists in the target directory
  CopyTemplateToInternalStorage;

  // Use centralized target path logic
  FullPath := GetTargetPath(FileName);
  if TFile.Exists(FullPath) then
    Result := TFile.ReadAllText(FullPath, TEncoding.UTF8)
  else if not DefaultTemplate.IsEmpty then
    Result := DefaultTemplate
  else
    raise Exception.CreateFmt('Template file not found: %s', [FullPath]);
end;

{ Save Catalogs to File }
procedure SaveCatalogsToFile(const FileName: string;
  const Catalogs: TDictionary<string, TScryfallCatalog>);
var
  JsonCatalogs: TJsonObject;
  CatalogName: string;
  CatalogData: TJsonArray;
  FullFilePath: string;
begin
  FullFilePath := TPath.Combine(GetAppDirectory, FileName);
  JsonCatalogs := TJsonObject.Create;
  try
    for CatalogName in Catalogs.Keys do
    begin
      var
      Catalog := Catalogs[CatalogName];
      CatalogData := TJsonArray.Create;
      for var Item in Catalog.Data do
        CatalogData.Add(Item);

      JsonCatalogs.O[CatalogName] := TJsonObject.Create;
      JsonCatalogs.O[CatalogName].S[FieldName] := Catalog.Name;
      JsonCatalogs.O[CatalogName].A[FieldData] := CatalogData;
      JsonCatalogs.O[CatalogName].I[FeildCount] := Catalog.TotalItems;
    end;

    JsonCatalogs.SaveToFile(FullFilePath, False, TEncoding.UTF8, True);
  finally
    JsonCatalogs.Free;
  end;
end;

{ Load Catalogs from File }
procedure LoadCatalogsFromFile(const FileName: string;
  var Catalogs: TDictionary<string, TScryfallCatalog>);
var
  JsonCatalogs: TJsonObject;
  FullFilePath: string;
begin
  FullFilePath := TPath.Combine(GetAppDirectory, FileName);

  if not TFile.Exists(FullFilePath) then
  begin
    Catalogs.Clear;
    Exit;
  end;

  JsonCatalogs := TJsonObject.Create;
  try
    JsonCatalogs.LoadFromFile(FullFilePath);

    Catalogs.Clear;
    for var I := 0 to JsonCatalogs.Count - 1 do
    begin
      var
      CatalogName := JsonCatalogs.Names[I];
      var
      CatalogObj := JsonCatalogs.O[CatalogName];
      var
        Catalog: TScryfallCatalog;
      Catalog.Name := CatalogObj.S[FieldName];
      Catalog.TotalItems := CatalogObj.I[FeildCount];

      var
      CatalogDataArray := CatalogObj.A[FieldData];
      SetLength(Catalog.Data, CatalogDataArray.Count);
      for var J := 0 to CatalogDataArray.Count - 1 do
        Catalog.Data[J] := CatalogDataArray.S[J];

      Catalogs.Add(CatalogName, Catalog);
    end;
  finally
    JsonCatalogs.Free;
  end;
end;

procedure ClearListViewItems(ListView: TListView);
var
  I: Integer;
  Item: TListViewItem;
begin
  for I := 0 to ListView.Items.Count - 1 do
  begin
    Item := ListView.Items[I];
    if Assigned(Item.TagObject) then
    begin
      Item.TagObject.Free;
      Item.TagObject := nil;
    end;
  end;

  ListView.Items.Clear;
end;


function ParseTextWithSymbolsManual(const Input: string): TArray<string>;
var
  Regex: TRegEx;
  Matches: TMatchCollection;
  Match: TMatch;
  PartsList: TList<string>;
begin
  PartsList := TList<string>.Create;
  try
    // Regular expression to match either {symbol} or plain text
    Regex := TRegEx.Create('\{[^}]+\}|[^{]+');

    // Find all matches in the input string
    Matches := Regex.Matches(Input);
    for Match in Matches do
      PartsList.Add(Match.Value);

    Result := PartsList.ToArray;
  finally
    PartsList.Free;
  end;
end;

function GetStatusClass(const LegalityStatus: string): string;
begin
  if SameText(LegalityStatus, 'legal') then
    Result := 'legal'
  else if SameText(LegalityStatus, 'not_legal') then
    Result := 'not legal'
  else if SameText(LegalityStatus, 'banned') then
    Result := 'banned'
  else if SameText(LegalityStatus, 'restricted') then
    Result := 'restricted'
  else
    Result := 'unknown';
end;

function IsCardValid(const Card: TCardDetails): Boolean;
begin
  Result := not Card.CardName.IsEmpty and not Card.SFID.IsEmpty;
end;

function GetLegalStatus(const Legalities: TCardLegalities;
  const FieldName: string): string;
begin
  if FieldName = 'Standard' then
    Result := Legalities.Standard
  else if FieldName = 'Pioneer' then
    Result := Legalities.Pioneer
  else if FieldName = 'Modern' then
    Result := Legalities.Modern
  else if FieldName = 'Legacy' then
    Result := Legalities.Legacy
  else if FieldName = 'Commander' then
    Result := Legalities.Commander
  else if FieldName = 'Vintage' then
    Result := Legalities.Vintage
  else if FieldName = 'Pauper' then
    Result := Legalities.Pauper
  else if FieldName = 'Historic' then
    Result := Legalities.Historic
  else if FieldName = 'Explorer' then
    Result := Legalities.Explorer
  else if FieldName = 'Alchemy' then
    Result := Legalities.Alchemy
  else if FieldName = 'Brawl' then
    Result := Legalities.Brawl
  else if FieldName = 'Future' then
    Result := Legalities.Future
  else if FieldName = 'Oldschool' then
    Result := Legalities.Oldschool
  else if FieldName = 'Premodern' then
    Result := Legalities.Premodern
  else if FieldName = 'Duel' then
    Result := Legalities.Duel
  else if FieldName = 'Penny' then
    Result := Legalities.Penny
  else
    Result := '';
end;

initialization

finalization

end.
