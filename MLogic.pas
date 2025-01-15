unit MLogic;

interface

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  System.RegularExpressions, System.Classes, FMX.Dialogs, SGlobalsZ,
  FMX.Graphics, System.Net.HttpClient, FMX.StdCtrls, FMX.ListView.Appearances,
  FMX.ListView, JsonDataObjects, Logger;

procedure CopyDatabaseToInternalStorage;

function GetDatabasePathMTJSON: string;

function GetTemplatePath: string;

// procedure CopyTemplateToInternalStorage;


// function LoadTemplate(const FileName: string; const DefaultTemplate: string = ''): string;

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

function ConvertColorCodeToName(const Code: string): string;

function MatchesColors(const CardColors: string;
  const SearchColors: string): Boolean;

function GetDatabasePathLocal: string;

function GetJsonPathLocal: string;

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

function GetDatabasePathMTJSON: string;
const
  MTGAppRootFolder = 'MTGCardFetch';
  MTGAppSubFolder = 'mtgjson';
  DatabaseFileName = 'AllPrintings.sqlite';
var
  AppDataPath: string;
begin

  AppDataPath := TPath.Combine(TPath.GetHomePath,
    TPath.Combine(MTGAppRootFolder, MTGAppSubFolder));

  if not TDirectory.Exists(AppDataPath) then
    TDirectory.CreateDirectory(AppDataPath);

  Result := TPath.Combine(AppDataPath, DatabaseFileName);
end;

function GetDatabasePathLocal: string;
const
  MTGAppRootFolder = 'MTGCardFetch';
  DatabaseFileName = 'Collection.db';
var
  AppDataPath: string;
begin

  AppDataPath := TPath.Combine(TPath.GetHomePath,
    TPath.Combine(MTGAppRootFolder, DatabaseFileName));

  if not TDirectory.Exists(AppDataPath) then
    TDirectory.CreateDirectory(AppDataPath);

  Result := AppDataPath;
  // Result := TPath.Combine(AppDataPath, DatabaseFileName);
end;

function GetJsonPathLocal: string;
const
  MTGAppRootFolder = 'MTGCardFetch';
  JFileName = 'oracle-cards.json';
var
  AppDataPath: string;
begin

  AppDataPath := TPath.Combine(TPath.GetHomePath,
    TPath.Combine(MTGAppRootFolder, JFileName));

  if not TDirectory.Exists(AppDataPath) then
    TDirectory.CreateDirectory(AppDataPath);

  Result := AppDataPath;
end;

procedure CopyDatabaseToInternalStorage;
var
  SourcePath, DestinationPath: string;
begin
  DestinationPath := GetTargetPath(DatabaseFileName);

  SourcePath := GetSourcePath(DatabaseFileName);

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

// { Copy Template to Internal Storage }
// procedure CopyTemplateToInternalStorage;
// var
// SourcePath, DestinationPath: string;
// begin
// DestinationPath := GetTargetPath(TemplateFileName);
// SourcePath := GetSourcePath(TemplateFileName);
//
// if not TFile.Exists(DestinationPath) then
// begin
// try
// if TFile.Exists(SourcePath) then
// TFile.Copy(SourcePath, DestinationPath)
// else
// raise Exception.CreateFmt('Template file not found: %s', [SourcePath]);
// except
// on E: Exception do
// ShowMessage('Error copying template: ' + E.Message);
// end;
// end;
// end;
//
// function LoadTemplate(const FileName: string; const DefaultTemplate: string = ''): string;
// var
// FullPath: string;
// begin
// // Ensure the template exists in the target directory
// CopyTemplateToInternalStorage;
//
// // Use centralized target path logic
// FullPath := GetTargetPath(FileName);
// if TFile.Exists(FullPath) then
// Result := TFile.ReadAllText(FullPath, TEncoding.UTF8)
// else if not DefaultTemplate.IsEmpty then
// Result := DefaultTemplate
// else
// raise Exception.CreateFmt('Template file not found: %s', [FullPath]);
// end;

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
      JsonCatalogs.O[CatalogName].I[FieldCount] := Catalog.TotalItems;
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
      Catalog.TotalItems := CatalogObj.I[FieldCount];

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
  if FieldName = FieldStandard then
    Result := Legalities.Standard
  else if FieldName = FieldFuture then
    Result := Legalities.Future
  else if FieldName = FieldHistoric then
    Result := Legalities.Historic
  else if FieldName = FieldGladiator then
    Result := Legalities.Gladiator
  else if FieldName = FieldPioneer then
    Result := Legalities.Pioneer
  else if FieldName = FieldExplorer then
    Result := Legalities.Explorer
  else if FieldName = FieldModern then
    Result := Legalities.Modern
  else if FieldName = FieldLegacy then
    Result := Legalities.Legacy
  else if FieldName = FieldPauper then
    Result := Legalities.Pauper
  else if FieldName = FieldVintage then
    Result := Legalities.Vintage
  else if FieldName = FieldPenny then
    Result := Legalities.Penny
  else if FieldName = FieldCommander then
    Result := Legalities.Commander
  else if FieldName = FieldOathbreaker then
    Result := Legalities.Oathbreaker
  else if FieldName = FieldAlchemy then
    Result := Legalities.Alchemy
  else if FieldName = FieldBrawl then
    Result := Legalities.Brawl
  else if FieldName = FieldPauperCommander then
    Result := Legalities.PauperCommander
  else if FieldName = FieldDuel then
    Result := Legalities.Duel
  else if FieldName = FieldOldschool then
    Result := Legalities.Oldschool
  else if FieldName = FieldPremodern then
    Result := Legalities.Premodern
  else
    Result := '';
end;

function ConvertColorCodeToName(const Code: string): string;
var
  I: Integer;
begin
  for I := Low(ColorMap) to High(ColorMap) do
    if SameText(Code, ColorMap[I].Code) then
      Exit(ColorMap[I].Name);

  Result := ''; // Return empty string if no match is found
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
  // Split the input strings into arrays
  CardColorArray := CardColors.Split([','], TStringSplitOptions.ExcludeEmpty);
  SearchColorArray := SearchColors.ToLower.Split([',', ' '],
    TStringSplitOptions.ExcludeEmpty);

  // Convert card color codes to full names
  SetLength(ConvertedCardColors, Length(CardColorArray));
  for I := Low(CardColorArray) to High(CardColorArray) do
    ConvertedCardColors[I] := ConvertColorCodeToName(CardColorArray[I]).ToLower;

  // If no search colors are specified, match all
  if Length(SearchColorArray) = 0 then
    Exit(True);

  // Check if all search colors are present in the converted card colors
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
      Exit(False); // If any search color is not found, return False
  end;

  Result := True; // All search colors matched
end;

function SafeJsonArrayToJSON(const JsonArray: TJsonArray): string;
begin
  if Assigned(JsonArray) then
    Result := JsonArray.ToJSON
  else
    Result := '[]'; // Default to empty JSON array
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
        Result := UIntToStr(JsonObject.U[Key]); // Unsigned long integer value
      jdtFloat:
        Result := FloatToStr(JsonObject.F[Key]); // Float value
      jdtDateTime, jdtUtcDateTime:
        Result := DateTimeToStr(JsonObject.D[Key]); // DateTime value
      jdtBool:
        Result := BoolToStr(JsonObject.B[Key], True); // Boolean value
      jdtArray:
        Result := JsonObject.A[Key].ToJSON; // Array as JSON string
      jdtObject:
        Result := JsonObject.O[Key].ToJSON; // Object as JSON string
    else
      Result := '';
    end;
  end
  else
    Result := ''; // Return empty string if key doesn't exist
end;

function SafePriceValue(PricesObject: TJsonObject; const Key: string): string;
begin
  if Assigned(PricesObject) and PricesObject.Contains(Key) and
    not PricesObject.Values[Key].IsNull then
    Result := PricesObject.S[Key]
  else
    Result := '0.00'; // Default to '0.00' if the price is null or not there
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
var
  CacheFolder: string;
begin
  try
    CacheFolder := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
    if not TDirectory.Exists(CacheFolder) then
      TDirectory.CreateDirectory(CacheFolder);

    Result := TPath.Combine(CacheFolder, FileName);
  except
    on E: Exception do
    begin
      LogStuff('Error creating cache folder: ' + E.Message, ERROR);
      Result := '';
    end;
  end;
end;

initialization

finalization

end.
