unit MLogic;

interface

uses
  System.SysUtils, System.IOUtils, System.Generics.Collections,
  System.RegularExpressions, System.Hash, System.Classes,
  FMX.Dialogs, SGlobalsZ, FMX.Graphics, System.Net.HttpClient, FMX.StdCtrls,
  System.Threading;

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
//procedure SetupPopularCards(PopularCards: TStringList);
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
  DatabaseFileName = 'Collection.db';


var
  FManaSymbolMap: TDictionary<string, string>;
  FBase64ImageCache: TDictionary<string, string>;
  HttpClient: THTTPClient;

implementation
uses
JsonDataObjects,System.NetEncoding;

{ Need To Change This to relative paths }
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
  DestinationPath := GetTargetPath(DatabaseFileName); // Use centralized target logic
  SourcePath := GetSourcePath(DatabaseFileName);     // Use centralized source logic

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

function LoadTemplate(const FileName: string; const DefaultTemplate: string = ''): string;
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
  Result := TPath.Combine(GetTargetPath('MTGIconsPNG'), FileName);
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

//{ Popular Cards Setup }
//procedure SetupPopularCards(PopularCards: TStringList);
//begin
//  PopularCards.Clear;
//  PopularCards.AddStrings(['Black Lotus', 'Ancestral Recall', 'Mox Sapphire',
//    'Mox Jet', 'Mox Ruby', 'Mox Pearl', 'Time Walk', 'Tarmogoyf', 'Force of Will']);
//end;

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

procedure InitializeManaSymbolMap;  //need to change to other Symbol method to aviod this
const
  // Static mana symbols
  StaticSymbols: array [0 .. 26] of string = (
    '{T}', '{W}', '{U}', '{B}', '{R}', '{G}', '{C}', '{X}', '{A}', '{H}', '{Y}', '{Z}',
    '{½}', '{∞}', '{100}', '{1000000}', '{W/U}', '{W/B}', '{U/B}', '{U/R}', '{B/R}',
    '{B/G}', '{R/G}', '{R/W}', '{G/W}', '{G/U}', '{W/P}'
  );
  // Hybrid and phyrexian mana symbols
  HybridSymbols: array [0 .. 6] of string = (
    '{U/P}', '{B/P}', '{R/P}', '{G/P}', '{B/G/P}', '{G/W/P}', '{U/B/P}'
  );
  // Additional symbols
  ExtraSymbols: array [0 .. 7] of string = (
    '{W/B/P}', '{W/U/P}', '{B/R/P}', '{Q}', '{S}', '{E}', '{P}', '{PW}'
  );
  // Planeswalker and special symbols
  SpecialSymbols: array [0 .. 3] of string = ('{HW}', '{HR}', '{TK}', '{PW}');
var
  Symbol: string;
  i: Integer;
begin
  if Assigned(FManaSymbolMap) then
    Exit; // Already initialized

  FManaSymbolMap := TDictionary<string, string>.Create;

  // Add static symbols
  for Symbol in StaticSymbols do
    FManaSymbolMap.Add(Symbol, Symbol.Replace('/', '_') + '.png');

  // Add hybrid and phyrexian mana symbols
  for Symbol in HybridSymbols do
    FManaSymbolMap.Add(Symbol, Symbol.Replace('/', '_').Replace('/P', '_P') + '.png');

  // Add special symbols
  for Symbol in ExtraSymbols do
    FManaSymbolMap.Add(Symbol, Symbol.Replace('/', '_') + '.png');

  // Add numbers from 0 to 20
  for i := 0 to 20 do
    FManaSymbolMap.Add(Format('{%d}', [i]), Format('{%d}.png', [i]));

  // Add special numbers and symbols
for Symbol in SpecialSymbols do
begin
  if not FManaSymbolMap.ContainsKey(Symbol) then
    FManaSymbolMap.Add(Symbol, Symbol.Replace('/', '_') + '.png')
  else
   // LogError('Duplicate symbol detected: ' + Symbol); having issues here
end;
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
      ImageMimeType := 'image/png';

//      // Determine the MIME type
//      if ImagePath.EndsWith('.png', True) then
//        ImageMimeType := 'image/png'
//      else if ImagePath.EndsWith('.jpg', True) or
//        ImagePath.EndsWith('.jpeg', True) then
//        ImageMimeType := 'image/jpeg'
//      else if ImagePath.EndsWith('.gif', True) then
//        ImageMimeType := 'image/gif'
//      else
//        ImageMimeType := 'application/octet-stream'; // Default MIME type


        // Remove any stray <br> tags or unwanted characters
      //ImagePath := ImagePath.Replace('<br>', '').Replace(#13#10, '').Replace(#10, '').Replace(#13, '');

      ImagePath := Format('data:%s;base64,%s', [ImageMimeType, ImageBase64]);

      // Append the <img> tag
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
