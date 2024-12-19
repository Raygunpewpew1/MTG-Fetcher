unit Mana;

interface

uses
  System.Net.HttpClient, System.Classes, JsonDataObjects, System.SysUtils,
  System.Generics.Collections, System.RegularExpressions, System.IOUtils;

function ReplaceManaSymbolsWithImages(const OracleText: string): string;

implementation

uses
  WrapperHelper, System.NetEncoding, APIConstants;

var
  SymbolCache: TDictionary<string, string>;
  CacheFilePath: string;

function GetCacheFilePath: string;
var
  CacheFolder: string;
begin
  // Combine app data path and MTGCardFetch folder
  CacheFolder := TPath.Combine(TPath.GetHomePath, MTGAppFolder);

  // Ensure the directory exists
  if not TDirectory.Exists(CacheFolder) then
    TDirectory.CreateDirectory(CacheFolder);

  // Combine with the cache file name
  Result := TPath.Combine(CacheFolder, 'SymbolCache.json');
end;

procedure LoadCacheFromFile;
var
  CacheJson: TJsonObject;
  I: Integer;
  Key: string;
begin
  // Exit early if the file does not exist
  if not TFile.Exists(CacheFilePath) then
    Exit;

  CacheJson := TJsonObject.Create;
  try
    try
      // Load JSON file
      CacheJson.LoadFromFile(CacheFilePath);

      // Populate the symbol cache
      for I := 0 to CacheJson.Count - 1 do
      begin
        Key := CacheJson.Names[I];
        SymbolCache.AddOrSetValue(Key, CacheJson.S[Key]);
      end;
    except
      on E: Exception do
      begin
        LogStuff('Error in mana symbols, Deleted file:' + CacheFilePath);
        // Handle corrupted cache file
        SymbolCache.Clear; // Clear any partial cache
        TFile.Delete(CacheFilePath); // Delete corrupted file
      end;
    end;
  finally
    CacheJson.Free;
  end;
end;

procedure SaveCacheToFile;
var
  CacheJson: TJsonObject;
  Pair: TPair<string, string>;
begin
  CacheJson := TJsonObject.Create;
  try
    for Pair in SymbolCache do
      CacheJson.S[Pair.Key] := Pair.Value;

    CacheJson.SaveToFile(CacheFilePath, False, TEncoding.UTF8, True);
  finally
    LogStuff('Symbols saved and Cached');
    CacheJson.Free;
  end;
end;

procedure FetchAllSymbols(var SymbolsJSON: TJsonArray);
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JsonResponse: TJsonObject;
begin
  SymbolsJSON := nil;
  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(EndPointSymbology);
    if Response.StatusCode = 200 then
    begin
      JsonResponse := TJsonObject.Create;
      try
        JsonResponse.LoadFromStream(Response.ContentStream);
        SymbolsJSON := JsonResponse.A[FieldData].Clone as TJsonArray;
      finally
        JsonResponse.Free;
        LogStuff('Symbols Fetched/Found');
      end;
    end
    else
      raise Exception.Create('Failed to fetch symbols: ' + Response.StatusText);
  finally
    HttpClient.Free;
  end;
end;

function FetchSVGContent(const SVG_URL: string): string;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
begin
  Result := '';
  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(SVG_URL);
    if Response.StatusCode = 200 then
      Result := Response.ContentAsString(TEncoding.UTF8);
  finally
    HttpClient.Free;
  end;
end;

procedure PopulateSymbolCache;
var
  SymbolsJSON: TJsonArray;
  I: Integer;
  Symbol, SVG_URL, SVGContent: string;
  CacheUpdated: Boolean;
begin
  SymbolsJSON := nil;
  CacheUpdated := False;

  try
    FetchAllSymbols(SymbolsJSON); // Fetch all symbols from Scryfall
    for I := 0 to SymbolsJSON.Count - 1 do
    begin
      Symbol := SymbolsJSON.O[I].S[FeildSymbol];
      SVG_URL := SymbolsJSON.O[I].S[FeildSVGUri];

      if not SymbolCache.ContainsKey(Symbol) then
      begin
        // Fetch and cache the SVG content
        SVGContent := FetchSVGContent(SVG_URL);
        if not SVGContent.IsEmpty then
        begin
          SymbolCache.Add(Symbol, SVGContent);
          CacheUpdated := True;
        end;
      end;
    end;

    if CacheUpdated then
      SaveCacheToFile; // Save the updated cache
  finally
    SymbolsJSON.Free; // Free the JSON array
  end;
end;

function ReplaceManaSymbolsWithImages(const OracleText: string): string;
var
  Symbol: string;
  InlineSVG: string;
begin
  Result := OracleText;

  // Populate cache if it's empty
  if SymbolCache.Count = 0 then
    PopulateSymbolCache;

  // Replace symbols with properly aligned inline SVGs using <img>
  for Symbol in SymbolCache.Keys do
  begin
    InlineSVG := Format(SVG_TEMPLATE,
      [TNetEncoding.Base64.Encode(SymbolCache[Symbol]), Symbol]);

    Result := Result.Replace(Symbol, InlineSVG, [rfReplaceAll]);
  end;
end;

initialization

SymbolCache := TDictionary<string, string>.Create;
CacheFilePath := GetCacheFilePath;

// Load existing cache from file
LoadCacheFromFile;

finalization

SaveCacheToFile; // Save cache on shutdown
SymbolCache.Free;

end.
