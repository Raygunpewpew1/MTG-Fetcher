unit MLogic;

interface

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  System.RegularExpressions, System.Hash, System.Classes, System.NetEncoding,
  FMX.Dialogs, SGlobalsZ, FMX.Graphics, System.Net.HttpClient, FMX.StdCtrls,
  System.Threading, JsonDataObjects;

procedure CopyDatabaseToInternalStorage;
procedure InitializeManaSymbolMap;
function GetDatabasePath: string;
function GetTemplatePath: string;
procedure CopyTemplateToInternalStorage;
function LoadTemplate(const FileName: string; const DefaultTemplate: string = ''): string;
function GetAppDirectory: string;
function GetIconPath(const FileName: string): string;
function ParseTextWithSymbolsManual(const Input: string): TArray<string>;
function GetCacheDirectory: string;
function GetCachedImagePath(const URL: string): string;
function GetStatusClass(const LegalityStatus: string): string;
procedure SetupPopularCards(PopularCards: TStringList);
function ReplaceManaSymbolsWithImages(const OracleText: string): string;
function ImageToBase64(const ImagePath: string): string;
function ReplacePlaceholder(const Template, Placeholder, Value: string): string;
function IsCardValid(const Card: TCardDetails): Boolean;
procedure SaveCatalogsToFile(const FileName: string; const Catalogs: TDictionary<string, TScryfallCatalog>);
procedure LoadCatalogsFromFile(const FileName: string; var Catalogs: TDictionary<string, TScryfallCatalog>);
function GetLegalStatus(const Legalities: TCardLegalities; const FieldName: string): string;
const
  MTGAppFolder = 'MTGCardFetch';
  TemplateFileName = 'card_template.html';


var
  FManaSymbolMap: TDictionary<string, string>;
  FBase64ImageCache: TDictionary<string, string>;
  HttpClient: THTTPClient;

implementation

{ Centralized App Directory Logic }
function GetAppDirectory: string;
begin
  {$IF DEFINED(MSWINDOWS)}
  Result := TPath.Combine(TPath.GetDocumentsPath, MTGAppFolder);
  {$ELSEIF DEFINED(ANDROID)}
  Result := TPath.Combine(TPath.GetHomePath, MTGAppFolder);
  {$ELSE}
  raise Exception.Create('Unsupported platform');
  {$ENDIF}

  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

{ Centralized Template Path Logic }
function GetTemplatePath: string;
begin
  Result := TPath.Combine(GetAppDirectory, TemplateFileName);
end;

{ Copy Database to Internal Storage }
function GetDatabasePath: string;
begin
  Result := TPath.Combine(GetAppDirectory, 'Collection.db');
end;

procedure CopyDatabaseToInternalStorage;
var
  SourcePath, DestinationPath: string;
begin
  DestinationPath := GetDatabasePath;

  if not TFile.Exists(DestinationPath) then
  begin
    {$IFDEF ANDROID}
    SourcePath := TPath.Combine(TPath.GetDocumentsPath, 'Collection.db');
    {$ELSE}
    SourcePath := TPath.Combine(TPath.GetHomePath, 'Collection.db');
    {$ENDIF}

    try
      if TFile.Exists(SourcePath) then
        TFile.Copy(SourcePath, DestinationPath)
      else
        raise Exception.Create('Database file not found: ' + SourcePath);
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
  DestinationPath := GetTemplatePath;

  if not TFile.Exists(DestinationPath) then
  begin
    {$IFDEF ANDROID}
    SourcePath := TPath.Combine(TPath.GetDocumentsPath, TemplateFileName);
    {$ELSE}
    SourcePath := TPath.Combine(TPath.GetHomePath, TemplateFileName);
    {$ENDIF}

    try
      if TFile.Exists(SourcePath) then
        TFile.Copy(SourcePath, DestinationPath)
      else
        raise Exception.Create('Template file not found: ' + SourcePath);
    except
      on E: Exception do
        ShowMessage('Error copying template: ' + E.Message);
    end;
  end;
end;

{ Load Template }
function LoadTemplate(const FileName: string; const DefaultTemplate: string = ''): string;
var
  FullPath: string;
begin
  CopyTemplateToInternalStorage;

  FullPath := TPath.Combine(GetAppDirectory, FileName);
  if TFile.Exists(FullPath) then
    Result := TFile.ReadAllText(FullPath, TEncoding.UTF8)
  else if DefaultTemplate <> '' then
    Result := DefaultTemplate
  else
    raise Exception.Create('Template file not found: ' + FullPath);
end;

{ Cache Directory Management }
function GetCacheDirectory: string;
begin
  Result := TPath.Combine(GetAppDirectory, 'Cache');
  if not TDirectory.Exists(Result) then
    TDirectory.CreateDirectory(Result);
end;

{ Cached Image Path }
function GetCachedImagePath(const URL: string): string;
var
  Hash: string;
begin
  Hash := THashSHA2.GetHashString(URL);
  Result := TPath.Combine(GetCacheDirectory, Hash);
end;

{ Mana Symbol Path }
function GetIconPath(const FileName: string): string;
begin
  Result := TPath.Combine(GetAppDirectory, TPath.Combine('MTGIconsPNG', FileName));
end;

{ Placeholder Replacement }
function ReplacePlaceholder(const Template, Placeholder, Value: string): string;
begin
  Result := StringReplace(Template, '{{' + Placeholder + '}}', Value, [rfReplaceAll]);
end;

{ Image to Base64 Conversion }
function ImageToBase64(const ImagePath: string): string;
begin
  if FBase64ImageCache.TryGetValue(ImagePath, Result) then
    Exit;

  if not TFile.Exists(ImagePath) then
  begin
    Result := '';
    Exit;
  end;

  var Bytes: TBytes := TFile.ReadAllBytes(ImagePath);
  Result := TNetEncoding.Base64.EncodeBytesToString(Bytes);
  FBase64ImageCache.Add(ImagePath, Result);
end;

{ Popular Cards Setup }
procedure SetupPopularCards(PopularCards: TStringList);
begin
  PopularCards.Clear;
  PopularCards.AddStrings(['Black Lotus', 'Ancestral Recall', 'Mox Sapphire',
    'Mox Jet', 'Mox Ruby', 'Mox Pearl', 'Time Walk', 'Tarmogoyf', 'Force of Will']);
end;

{ Save Catalogs to File }
procedure SaveCatalogsToFile(const FileName: string; const Catalogs: TDictionary<string, TScryfallCatalog>);
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
      var Catalog := Catalogs[CatalogName];
      CatalogData := TJsonArray.Create;
      for var Item in Catalog.Data do
        CatalogData.Add(Item);

      JsonCatalogs.O[CatalogName] := TJsonObject.Create;
      JsonCatalogs.O[CatalogName].S['name'] := Catalog.Name;
      JsonCatalogs.O[CatalogName].A['data'] := CatalogData;
      JsonCatalogs.O[CatalogName].I['total_items'] := Catalog.TotalItems;
    end;

    JsonCatalogs.SaveToFile(FullFilePath, False, TEncoding.UTF8, True);
  finally
    JsonCatalogs.Free;
  end;
end;

{ Load Catalogs from File }
procedure LoadCatalogsFromFile(const FileName: string; var Catalogs: TDictionary<string, TScryfallCatalog>);
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
      var CatalogName := JsonCatalogs.Names[I];
      var CatalogObj := JsonCatalogs.O[CatalogName];
      var Catalog: TScryfallCatalog;
      Catalog.Name := CatalogObj.S['name'];
      Catalog.TotalItems := CatalogObj.I['total_items'];

      var CatalogDataArray := CatalogObj.A['data'];
      SetLength(Catalog.Data, CatalogDataArray.Count);
      for var J := 0 to CatalogDataArray.Count - 1 do
        Catalog.Data[J] := CatalogDataArray.S[J];

      Catalogs.Add(CatalogName, Catalog);
    end;
  finally
    JsonCatalogs.Free;
  end;
end;

procedure InitializeManaSymbolMap;
begin
  if Assigned(FManaSymbolMap) then
    Exit; // Already initialized

  FManaSymbolMap := TDictionary<string, string>.Create;

  // Populate mana symbols
  FManaSymbolMap.Add('{T}', '{T}.png');
  FManaSymbolMap.Add('{W}', '{W}.png');
  FManaSymbolMap.Add('{U}', '{U}.png');
  FManaSymbolMap.Add('{B}', '{B}.png');
  FManaSymbolMap.Add('{R}', '{R}.png');
  FManaSymbolMap.Add('{G}', '{G}.png');
  FManaSymbolMap.Add('{C}', '{C}.png');
  FManaSymbolMap.Add('{X}', '{X}.png');
  FManaSymbolMap.Add('{A}', '{A}.png');
  FManaSymbolMap.Add('{H}', '{H}.png');
  FManaSymbolMap.Add('{Y}', '{Y}.png');
  FManaSymbolMap.Add('{Z}', '{Z}.png');

  for var i := 0 to 20 do
    FManaSymbolMap.Add('{' + i.ToString + '}', '{' + i.ToString + '}.png');

  FManaSymbolMap.Add('{½}', '{½}.png');
  FManaSymbolMap.Add('{∞}', '{∞}.png');
  FManaSymbolMap.Add('{100}', '{100}.png');
  FManaSymbolMap.Add('{1000000}', '{1000000}.png');
  FManaSymbolMap.Add('{W/U}', '{W_U}.png');
  FManaSymbolMap.Add('{W/B}', '{W_B}.png');
  FManaSymbolMap.Add('{U/B}', '{U_B}.png');
  FManaSymbolMap.Add('{U/R}', '{U_R}.png');
  FManaSymbolMap.Add('{B/R}', '{B_R}.png');
  FManaSymbolMap.Add('{B/G}', '{B_G}.png');
  FManaSymbolMap.Add('{R/G}', '{R_G}.png');
  FManaSymbolMap.Add('{R/W}', '{R_W}.png');
  FManaSymbolMap.Add('{G/W}', '{G_W}.png');
  FManaSymbolMap.Add('{G/U}', '{G_U}.png');
  FManaSymbolMap.Add('{W/P}', '{W_P}.png');
  FManaSymbolMap.Add('{U/P}', '{U_P}.png');
  FManaSymbolMap.Add('{B/P}', '{B_P}.png');
  FManaSymbolMap.Add('{R/P}', '{R_P}.png');
  FManaSymbolMap.Add('{G/P}', '{G_P}.png');
  FManaSymbolMap.Add('{B/G/P}', '{B_G_P}.png');
  FManaSymbolMap.Add('{G/W/P}', '{G_W_P}.png');
  FManaSymbolMap.Add('{U/B/P}', '{U_B_P}.png');
  FManaSymbolMap.Add('{W/B/P}', '{W_B_P}.png');
  FManaSymbolMap.Add('{W/U/P}', '{W_U_P}.png');
  FManaSymbolMap.Add('{B/R/P}', '{B_R_P}.png');

  FManaSymbolMap.Add('{Q}', '{Q}.png');
  FManaSymbolMap.Add('{S}', '{S}.png');
  FManaSymbolMap.Add('{E}', '{E}.png');
  FManaSymbolMap.Add('{P}', '{P}.png');
  FManaSymbolMap.Add('{PW}', '{PW}.png');
  FManaSymbolMap.Add('{HW}', '{HW}.png');
  FManaSymbolMap.Add('{HR}', '{HR}.png');
  FManaSymbolMap.Add('{TK}', '{TK}.png');

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

function ReplaceManaSymbolsWithImages(const OracleText: string): string;
var
  Parts: TArray<string>;
  Part: string;
  ImageFileName: string;
  ImagePath, ImageBase64: string;
  ImageMimeType: string;
begin
  Result := ''; // Initialize the result string
  Parts := ParseTextWithSymbolsManual(OracleText);
  // Split OracleText into parts

  for Part in Parts do
  begin
    if Part.StartsWith('{') and Part.EndsWith('}') then
    begin
      // Look up the mana symbol image file
      if FManaSymbolMap.TryGetValue(Part, ImageFileName) then
        ImagePath := GetIconPath(ImageFileName)
      else
        ImagePath := GetIconPath('default.png'); // Fallback to default image

      // Validate the image file
      if not TFile.Exists(ImagePath) then
        ImagePath := GetIconPath('default.png');

      // Convert the image to Base64
      ImageBase64 := ImageToBase64(ImagePath);
      if ImageBase64 = '' then
        Continue; // Skip if conversion failed

      // Determine the MIME type based on the file extension
      if ImagePath.EndsWith('.png', True) then
        ImageMimeType := 'image/png'
      else if ImagePath.EndsWith('.jpg', True) or
        ImagePath.EndsWith('.jpeg', True) then
        ImageMimeType := 'image/jpeg'
      else if ImagePath.EndsWith('.gif', True) then
        ImageMimeType := 'image/gif'
      else
        ImageMimeType := 'application/octet-stream'; // Default MIME type

      // Construct the data URI
      ImagePath := Format('data:%s;base64,%s', [ImageMimeType, ImageBase64]);

      // Append the <img> tag with improved vertical alignment
      Result := Result +
        Format('<img src="%s" alt="%s" style="display:inline-block; width:16px; height:16px; vertical-align:middle; margin:0 2px;">',
        [ImagePath, TNetEncoding.HTML.Encode(Part)]);
    end
    else
    begin
      // Append plain text, properly encoded
      Result := Result + TNetEncoding.HTML.Encode(Part);
    end;
  end;
end;

function IsCardValid(const Card: TCardDetails): Boolean;
begin
  Result := not Card.CardName.IsEmpty and not Card.SFID.IsEmpty;
end;

function GetLegalStatus(const Legalities: TCardLegalities; const FieldName: string): string;
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
  FBase64ImageCache := TDictionary<string, string>.Create;
  InitializeManaSymbolMap;

finalization
  FBase64ImageCache.Free;
  FManaSymbolMap.Free;

end.
