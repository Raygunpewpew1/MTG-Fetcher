unit MLogic;

interface

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  System.RegularExpressions, System.Classes, FMX.Dialogs, SGlobalsX,
  FMX.Graphics, System.Net.HttpClient, FMX.StdCtrls, FMX.ListView.Appearances,
  FMX.ListView, JsonDataObjects, Logger;

procedure CopyDatabaseToInternalStorage;

function GetAssetPath(const SubFolder, FileName: string): string;

function GetTemplatePath: string;

function GetAppDirectory: string;

function ParseTextWithSymbolsManual(const Input: string): TArray<string>;

function GetStatusClass(const LegalityStatus: string): string;

function IsCardValid(const Card: TCardDetails): Boolean;

procedure SaveCatalogsToFile(const FileName: string; const Catalogs: TDictionary<string, TScryfallCatalog>);

procedure LoadCatalogsFromFile(const FileName: string; Catalogs: TDictionary<string, TScryfallCatalog>);

function GetLegalStatus(const Legalities: TCardLegalities; Format: TLegalityFormat): string;

procedure ClearListViewItems(ListView: TListView);

function ConvertColorCodeToName(const Code: string): string;

function MatchesColors(const CardColors: string; const SearchColors: string): Boolean;

function GetCacheFilePath(const FileName: string): string;

function GetSetIconAsRawSVG(const IconURL, SetCode: string): string;

procedure LoadSetIconCacheFromFile;

function LoadSetDetailsFromJson(const FileName: string): TArray<TSetDetails>;

procedure SaveSetDetailsToJson(const FileName: string; const SetDetailsArray: TArray<TSetDetails>);

var
  SetIconCache: TDictionary<string, string>;

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

procedure SaveCatalogsToFile(const FileName: string; const Catalogs: TDictionary<string, TScryfallCatalog>);
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

procedure LoadCatalogsFromFile(const FileName: string; Catalogs: TDictionary<string, TScryfallCatalog>);
var
  JsonCatalogs: TJsonObject;
  FullFilePath: string;
  CatalogName: string;
  JsonCatalogObj: TJsonObject;
  CatalogDataArray: TJsonArray;
  NewCat: TScryfallCatalog;
  TempData: TArray<string>;
  I, J: Integer;
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
    for I := 0 to JsonCatalogs.Count - 1 do
    begin
      CatalogName := JsonCatalogs.Names[I];
      JsonCatalogObj := JsonCatalogs.O[CatalogName];

      // Create a new instance for this catalog
      NewCat := TScryfallCatalog.Create;
      try
        NewCat.Name := JsonCatalogObj.S[FieldName];
        NewCat.TotalItems := JsonCatalogObj.I[FieldCount];

        CatalogDataArray := JsonCatalogObj.A[FieldData];
        SetLength(TempData, CatalogDataArray.Count);
        for J := 0 to CatalogDataArray.Count - 1 do
          TempData[J] := CatalogDataArray.S[J];
        NewCat.Data := TempData;

        Catalogs.Add(CatalogName, NewCat);
        // Do not free NewCat here because it's now owned by the dictionary.
      except
        NewCat.Free; // Free if something goes wrong before adding.
        raise;
      end;
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

function MatchesColors(const CardColors: string; const SearchColors: string): Boolean;
var
  CardColorArray, SearchColorArray: TArray<string>;
  ConvertedCardColors: TArray<string>;
  SearchColor: string;
  I: Integer;
  Found: Boolean;
begin
  CardColorArray := CardColors.Split([','], TStringSplitOptions.ExcludeEmpty);
  SearchColorArray := SearchColors.ToLower.Split([',', ' '], TStringSplitOptions.ExcludeEmpty);

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

function GetCacheFilePath(const FileName: string): string;
begin
  Result := GetAssetPath('', FileName);
end;

/// <summary>
/// Saves the entire in-memory SetIconCache to a JSON file on disk.
/// </summary>
procedure SaveSetIconCacheToFile;
var
  CacheJson: TJsonObject;
  Pair: TPair<string, string>;
  CachePath: string;
begin
  CacheJson := TJsonObject.Create;
  try
    for Pair in SetIconCache do
      CacheJson.S[Pair.Key] := Pair.Value;

    CachePath := GetCacheFilePath(SetIconCacheFile);
    CacheJson.SaveToFile(CachePath, False, TEncoding.UTF8, False);
    LogStuff('Set icon cache saved to disk: ' + CachePath);
  finally
    CacheJson.Free;
  end;
end;

/// <summary>
/// Loads the SetIconCache from a JSON file on disk (if it exists).
/// </summary>
procedure LoadSetIconCacheFromFile;
var
  CacheJson: TJsonObject;
  CachePath: string;
  I: Integer;
  Key: string;
begin
  CachePath := GetCacheFilePath(SetIconCacheFile);
  if not TFile.Exists(CachePath) then
    Exit;

  CacheJson := TJsonObject.Create;
  try
    try
      CacheJson.LoadFromFile(CachePath);
      for I := 0 to CacheJson.Count - 1 do
      begin
        Key := CacheJson.Names[I];
        SetIconCache.AddOrSetValue(Key, CacheJson.S[Key]);
      end;
      LogStuff('Set icon cache loaded from disk: ' + CachePath);
    except
      on E: Exception do
      begin
        LogStuff('Failed to load set icon cache: ' + E.Message);
        SetIconCache.Clear;
        TFile.Delete(CachePath);
      end;
    end;
  finally
    CacheJson.Free;
  end;
end;

function GetSetIconAsRawSVG(const IconURL, SetCode: string): string;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  MemoryStream: TMemoryStream;
  SVGContent: TBytes;
  RawSvg: string;
begin
  // If we've already cached this set code, just return it
  if SetIconCache.ContainsKey(SetCode) then
    Exit(SetIconCache[SetCode]);

  Result := '';
  HttpClient := THTTPClient.Create;
  MemoryStream := TMemoryStream.Create;
  try
    try
      Response := HttpClient.Get(IconURL, MemoryStream);
      if Response.StatusCode = 200 then
      begin
        MemoryStream.Position := 0;
        SetLength(SVGContent, MemoryStream.Size);
        MemoryStream.ReadBuffer(SVGContent, MemoryStream.Size);

        // Convert the raw bytes to a UTF8 string
        RawSvg := TEncoding.UTF8.GetString(SVGContent);

        // Store this raw SVG in the cache dictionary
        SetIconCache.AddOrSetValue(SetCode, RawSvg);
        Result := RawSvg;

        // Persist the updated cache to disk
        SaveSetIconCacheToFile;
      end
      else
      begin
        // Log or handle the error
        LogStuff(Format('Failed to download SVG. HTTP %d', [Response.StatusCode]), WARNING);
      end;
    except
      on E: Exception do
      begin
        LogStuff('Error fetching raw SVG: ' + E.Message, ERROR);
      end;
    end;
  finally
    HttpClient.Free;
    MemoryStream.Free;
  end;
end;

function LoadSetDetailsFromJson(const FileName: string): TArray<TSetDetails>;
var
  JsonObject: TJsonObject;
  JsonArray: TJsonArray;
  SetDetails: TSetDetails;
  I: Integer;
begin
  if not TFile.Exists(FileName) then
    Exit(nil);

  JsonObject := TJsonObject.Create;
  try
    JsonObject.LoadFromFile(FileName);
    if not JsonObject.Contains('sets') then
      Exit(nil);
    JsonArray := JsonObject.A['sets'];
    SetLength(Result, JsonArray.Count);
    for I := 0 to JsonArray.Count - 1 do
    begin
      SetDetails := TSetDetails.Create; // Allocate a new instance
      SetDetails.Code := JsonArray.O[I].S[FieldCode];
      SetDetails.Name := JsonArray.O[I].S[FieldName];
      SetDetails.IconSVGURI := JsonArray.O[I].S[FieldIconSvgUri];
      Result[I] := SetDetails;
    end;
  finally
    JsonObject.Free;
  end;
end;

procedure SaveSetDetailsToJson(const FileName: string; const SetDetailsArray: TArray<TSetDetails>);
var
  JsonObject: TJsonObject;
  JsonArray: TJsonArray;
  JsonSet: TJsonObject;
  SetDetails: TSetDetails;
begin
  JsonObject := TJsonObject.Create;
  try
    JsonArray := JsonObject.A[FieldSets];

    for SetDetails in SetDetailsArray do
    begin
      JsonSet := TJsonObject.Create;
      JsonSet.S[FieldCode] := SetDetails.Code;
      JsonSet.S[FieldName] := SetDetails.Name;
      JsonSet.S[FieldIconSvgUri] := SetDetails.IconSVGURI;
      JsonArray.AddObject(JsonSet);
    end;

    JsonObject.SaveToFile(FileName, False, TEncoding.UTF8, False);
  finally
    JsonObject.Free;
  end;
end;

initialization
  SetIconCache := TDictionary<string, string>.Create;
  LoadSetIconCacheFromFile;


finalization
  SaveSetIconCacheToFile;
  SetIconCache.Free;

end.

