unit Mana;

interface

uses
  System.Net.HttpClient, System.Classes, JsonDataObjects, System.SysUtils,
  System.Generics.Collections, System.IOUtils, Logger, Template, MLogic;

/// <summary>
///   Replaces mana symbols in the given Oracle text with inlined SVG images.
/// </summary>
function ReplaceManaSymbolsWithImages(const OracleText: string): string;

implementation

uses
  System.NetEncoding, APIConstants;

var
  SymbolCache: TDictionary<string, string>;
  CacheFilePath: string;


/// <summary>
///   Loads the symbol cache from SymbolCache.json if it exists.
///   If the cache file is corrupt, it is deleted and the cache is cleared.
/// </summary>
procedure LoadCacheFromFile;
var
  CacheJson: TJsonObject;
  I: Integer;
  Key: string;
begin
  if not TFile.Exists(CacheFilePath) then
    Exit; // Nothing to load

  CacheJson := TJsonObject.Create;
  try
    try
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
        LogStuff('Error loading mana symbol cache: ' + E.Message);
        LogStuff('Deleting file: ' + CacheFilePath);
        SymbolCache.Clear; // Clear any partial cache
        TFile.Delete(CacheFilePath); // Remove corrupt file
      end;
    end;
  finally
    CacheJson.Free;
  end;
end;

/// <summary>
///   Saves the in-memory symbol cache to SymbolCache.json on disk.
/// </summary>
procedure SaveCacheToFile;
var
  CacheJson: TJsonObject;
  Pair: TPair<string, string>;
begin
  CacheJson := TJsonObject.Create;
  try
    for Pair in SymbolCache do
      CacheJson.S[Pair.Key] := Pair.Value;

    CacheJson.SaveToFile(CacheFilePath, False, TEncoding.UTF8, False);
  finally
    CacheJson.Free;
    LogStuff('Symbol cache saved to disk.');
  end;
end;

/// <summary>
///   Fetches the entire list of mana symbols from Scryfall, returning them as a JSON array.
///   Raises an exception if the request fails.
/// </summary>
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
        LogStuff('Successfully fetched symbol list from Scryfall.');
      finally
        JsonResponse.Free;
      end;
    end
    else
      raise Exception.CreateFmt(
        'Failed to fetch symbols. HTTP status: %d. %s',
        [Response.StatusCode, Response.StatusText]
      );
  finally
    HttpClient.Free;
  end;
end;

/// <summary>
///   Fetches the SVG file content from the given URL. Returns an empty string on error.
/// </summary>
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
      Result := Response.ContentAsString(TEncoding.UTF8)
    else
      LogStuff(Format('Failed to fetch SVG from %s. HTTP %d: %s', [
        SVG_URL, Response.StatusCode, Response.StatusText
      ]));
  finally
    HttpClient.Free;
  end;
end;

/// <summary>
///   Fetches all mana symbols from Scryfall, then for each symbol not in our cache,
///   fetches its SVG content. Updates cache and saves to file if new symbols are added.
/// </summary>
procedure PopulateSymbolCache;
var
  SymbolsJSON: TJsonArray;
  I: Integer;
  Symbol, SVGUrl, SVGContent: string;
  CacheUpdated: Boolean;
  SymbolObj: TJsonObject;
begin
  SymbolsJSON := nil;
  CacheUpdated := False;
  try
    // Pull the entire symbol list
    FetchAllSymbols(SymbolsJSON);

    for I := 0 to SymbolsJSON.Count - 1 do
    begin
      SymbolObj := SymbolsJSON.O[I];
      Symbol := SymbolObj.S[FieldSymbol];
      SVGUrl := SymbolObj.S[FieldSVGUri];

      if not SymbolCache.ContainsKey(Symbol) then
      begin
        // Attempt to fetch the SVG content
        SVGContent := FetchSVGContent(SVGUrl);
        if not SVGContent.IsEmpty then
        begin
          SymbolCache.Add(Symbol, SVGContent);
          CacheUpdated := True;
        end
        else
        begin
          LogStuff(Format('No SVG content retrieved for symbol [%s] from [%s]', [Symbol, SVGUrl]));
        end;
      end;
    end;

    // Only save if something changed
    if CacheUpdated then
      SaveCacheToFile;
  finally
    SymbolsJSON.Free;
  end;
end;

/// <summary>
///   Replaces all recognized mana symbols in <paramref name="OracleText"/> with
///   inline base64-encoded SVG images using an <img> tag.
///   If the cache is empty, attempts to populate it first.
/// </summary>
function ReplaceManaSymbolsWithImages(const OracleText: string): string;
var
  Symbol, InlineSVG: string;
begin
  Result := OracleText;

  // If the symbol cache is empty, attempt to populate from Scryfall
  if SymbolCache.Count = 0 then
    PopulateSymbolCache;

  // Perform string replacements
  for Symbol in SymbolCache.Keys do
  begin
    InlineSVG := Format(
      SVG_TEMPLATE,
      [TNetEncoding.Base64.Encode(SymbolCache[Symbol]), Symbol]
    );

    Result := Result.Replace(Symbol, InlineSVG, [rfReplaceAll]);
  end;
end;

initialization
  SymbolCache := TDictionary<string, string>.Create;
  CacheFilePath := GetCacheFilePath(SymbolCacheFile);
  LoadCacheFromFile;

finalization
  SaveCacheToFile;
  SymbolCache.Free;

end.
