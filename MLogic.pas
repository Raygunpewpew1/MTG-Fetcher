unit MLogic;

interface

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  System.RegularExpressions, System.Classes, FMX.Dialogs, SGlobalsZ,
  FMX.Graphics, System.Net.HttpClient, FMX.StdCtrls, FMX.ListView.Appearances,
  FMX.ListView, JsonDataObjects, Logger;

procedure CopyDatabaseToInternalStorage;
function GetAssetPath(const SubFolder, FileName: string): string;
function GetTemplatePath: string;
function GetAppDirectory: string;
function ParseTextWithSymbolsManual(const Input: string): TArray<string>;
function GetStatusClass(const LegalityStatus: string): string;
function IsCardValid(const Card: TCardDetails): Boolean;
procedure SaveCatalogsToFile(const FileName: string;
  const Catalogs: TDictionary<string, TScryfallCatalog>);
procedure LoadCatalogsFromFile(const FileName: string;
  var Catalogs: TDictionary<string, TScryfallCatalog>);
function GetLegalStatus(const Legalities: TCardLegalities; Format: TLegalityFormat): string;
procedure ClearListViewItems(ListView: TListView);
function ConvertColorCodeToName(const Code: string): string;
function MatchesColors(const CardColors: string;
  const SearchColors: string): Boolean;
function SafeJsonArrayToJSON(const JsonArray: TJsonArray): string;
function SafeJsonValue(JsonObject: TJsonObject; const Key: string): string;
function SafePriceValue(PricesObject: TJsonObject; const Key: string): string;
function GetJSONStringOrDefault(const JsonObject: TJsonObject;
  const Key: string; const DefaultValue: string = ''): string;
function GetCacheFilePath(const FileName: string): string;

var
  HttpClient: THTTPClient;

implementation

uses
  System.Types, APIConstants;

const
  MTGAppRootFolder = 'MTGCardFetch';

function EnsureDirectoryExists(const FolderPath: string): string;
begin
  if not TDirectory.Exists(FolderPath) then
  begin
    LogStuff('Creating directory: ' + FolderPath, DEBUG);
    TDirectory.CreateDirectory(FolderPath);
  end;
  Result := FolderPath;
end;

function GetAppDirectory: string;
begin
  {$IF DEFINED(MSWINDOWS)}
  Result := TPath.Combine(TPath.GetHomePath, MTGAppRootFolder);
  {$ELSEIF DEFINED(ANDROID)}
  Result := TPath.Combine(TPath.GetHomePath, MTGAppRootFolder);
  {$ELSE}
  raise Exception.Create('Unsupported platform');
  {$ENDIF}
  Result := EnsureDirectoryExists(Result);
end;

function GetAssetPath(const SubFolder, FileName: string): string;
var
  AppDataPath: string;
begin
  if SubFolder.IsEmpty then
    AppDataPath := GetAppDirectory
  else
    AppDataPath := TPath.Combine(GetAppDirectory, SubFolder);

  EnsureDirectoryExists(AppDataPath);
  Result := TPath.Combine(AppDataPath, FileName);
end;

function GetTemplatePath: string;
const
  TemplateFileName = 'template.html';
begin
  Result := GetAssetPath('', TemplateFileName);
end;

procedure CopyDatabaseToInternalStorage;
const
  DatabaseFileName = 'Collection.db';
var
  SourcePath, DestinationPath: string;
begin
  DestinationPath := GetAssetPath('mtgjson', DatabaseFileName);
  SourcePath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), DatabaseFileName);

  if not TFile.Exists(DestinationPath) then
  begin
    try
      if TFile.Exists(SourcePath) then
        TFile.Copy(SourcePath, DestinationPath)
      else
        raise Exception.CreateFmt('Database file not found: %s', [SourcePath]);
    except
      on E: Exception do
        LogStuff('Error copying database: ' + E.Message, ERROR);
    end;
  end;
end;

procedure SaveCatalogsToFile(const FileName: string;
  const Catalogs: TDictionary<string, TScryfallCatalog>);
var
  JsonCatalogs: TJsonObject;
  CatalogName: string;
  CatalogData: TJsonArray;
  FullFilePath: string;
begin
  FullFilePath := GetAssetPath('', FileName);
  JsonCatalogs := TJsonObject.Create;
  try
    for CatalogName in Catalogs.Keys do
    begin
      var Catalog := Catalogs[CatalogName];
      CatalogData := TJsonArray.Create;
      for var Item in Catalog.Data do
        CatalogData.Add(Item);

      JsonCatalogs.O[CatalogName] := TJsonObject.Create;
      JsonCatalogs.O[CatalogName].S[FieldName] := Catalog.Name;
      JsonCatalogs.O[CatalogName].A[FieldData] := CatalogData;
      JsonCatalogs.O[CatalogName].I[FieldCount] := Catalog.TotalItems;
    end;

    JsonCatalogs.SaveToFile(FullFilePath, False, TEncoding.UTF8, True);
  finally
    JsonCatalogs.Free;
  end;
end;

procedure LoadCatalogsFromFile(const FileName: string;
  var Catalogs: TDictionary<string, TScryfallCatalog>);
var
  JsonCatalogs: TJsonObject;
  FullFilePath: string;
begin
  FullFilePath := GetAssetPath('', FileName);

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
      var CatalogName := JsonCatalogs.Names[I];
      var CatalogObj := JsonCatalogs.O[CatalogName];
      var Catalog: TScryfallCatalog;
      Catalog.Name := CatalogObj.S[FieldName];
      Catalog.TotalItems := CatalogObj.I[FieldCount];

      var CatalogDataArray := CatalogObj.A[FieldData];
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
    Regex := TRegEx.Create('\{[^}]+\}|[^\{]+');
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

function GetLegalStatus(const Legalities: TCardLegalities; Format: TLegalityFormat): string;
begin
  Result := Legalities.GetStatus(Format);
end;



function ConvertColorCodeToName(const Code: string): string;
var
  I: Integer;
begin
  for I := Low(ColorMap) to High(ColorMap) do
    if SameText(Code, ColorMap[I].Code) then
      Exit(ColorMap[I].Name);

  Result := '';
end;

function MatchesColors(const CardColors: string;
  const SearchColors: string): Boolean;
var
  CardColorArray, SearchColorArray: TArray<string>;
  ConvertedCardColors: TArray<string>;
  SearchColor: string;
  I: Integer;
  Found: Boolean;
begin
  CardColorArray := CardColors.Split([','], TStringSplitOptions.ExcludeEmpty);
  SearchColorArray := SearchColors.ToLower.Split([',', ' '],
    TStringSplitOptions.ExcludeEmpty);

  SetLength(ConvertedCardColors, Length(CardColorArray));
  for I := Low(CardColorArray) to High(CardColorArray) do
    ConvertedCardColors[I] := ConvertColorCodeToName(CardColorArray[I]).ToLower;

  if Length(SearchColorArray) = 0 then
    Exit(True);

  for SearchColor in SearchColorArray do
  begin
    Found := False;
    for I := Low(ConvertedCardColors) to High(ConvertedCardColors) do
    begin
      if SameText(SearchColor, ConvertedCardColors[I]) then
      begin
        Found := True;
        Break;
      end;
    end;

    if not Found then
      Exit(False);
  end;

  Result := True;
end;

function SafeJsonArrayToJSON(const JsonArray: TJsonArray): string;
begin
  if Assigned(JsonArray) then
    Result := JsonArray.ToJSON
  else
    Result := '[]';
end;

function SafeJsonValue(JsonObject: TJsonObject; const Key: string): string;
begin
  if JsonObject.Contains(Key) then
  begin
    case JsonObject.Types[Key] of
      jdtString:
        Result := JsonObject.S[Key];
      jdtInt:
        Result := IntToStr(JsonObject.I[Key]);
      jdtLong:
        Result := IntToStr(JsonObject.L[Key]);
      jdtULong:
        Result := UIntToStr(JsonObject.U[Key]);
      jdtFloat:
        Result := FloatToStr(JsonObject.F[Key]);
      jdtDateTime, jdtUtcDateTime:
        Result := DateTimeToStr(JsonObject.D[Key]);
      jdtBool:
        Result := BoolToStr(JsonObject.B[Key], True);
      jdtArray:
        Result := JsonObject.A[Key].ToJSON;
      jdtObject:
        Result := JsonObject.O[Key].ToJSON;
    else
      Result := '';
    end;
  end
  else
    Result := '';
end;

function SafePriceValue(PricesObject: TJsonObject; const Key: string): string;
begin
  if Assigned(PricesObject) and PricesObject.Contains(Key) and
    not PricesObject.Values[Key].IsNull then
    Result := PricesObject.S[Key]
  else
    Result := '0.00';
end;

function GetJSONStringOrDefault(const JsonObject: TJsonObject;
  const Key: string; const DefaultValue: string = ''): string;
begin
  if JsonObject.Contains(Key) and not JsonObject.Values[Key].IsNull then
    Result := JsonObject.S[Key]
  else
    Result := DefaultValue;
end;

function GetCacheFilePath(const FileName: string): string;
begin
  Result := GetAssetPath('', FileName);
end;

initialization

finalization

end.

