unit Mana;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.IOUtils,
  System.Net.HttpClient,
  JsonDataObjects,
  Logger,
  Template,
  MLogic,
  APIConstants;

type
  TManaSymbolManager = class
  private
    FSymbolCache: TDictionary<string, string>;
    FCacheFilePath: string;
  public
    constructor Create;
    destructor Destroy; override;

    /// <summary>
    /// Initializes the symbol manager by loading the cache and populating it if necessary.
    /// </summary>
    procedure Initialize;

    /// <summary>
    /// Replaces mana symbols in the given Oracle text with inlined SVG images.
    /// </summary>
    function ReplaceManaSymbolsWithImages(const OracleText: string): string;

  private
    /// <summary>
    /// Loads the symbol cache from the cache file.
    /// </summary>
    procedure LoadCacheFromFile;

    /// <summary>
    /// Saves the symbol cache to the cache file.
    /// </summary>
    procedure SaveCacheToFile;

    /// <summary>
    /// Fetches all mana symbols from Scryfall.
    /// </summary>
    function FetchAllSymbols: TJsonArray;

    /// <summary>
    /// Fetches the SVG content from a given URL.
    /// </summary>
    function FetchSVGContent(const SVG_URL: string): string;

    /// <summary>
    /// Populates the symbol cache by fetching symbols not already cached.
    /// </summary>
    procedure PopulateSymbolCache;
  end;

  /// <summary>
  /// Replaces mana symbols in the given Oracle text with inlined SVG images.
  /// </summary>
function ReplaceManaSymbolsWithImages(const OracleText: string): string;

implementation

{ TManaSymbolManager }

var
  ManaSymbolManager: TManaSymbolManager;

constructor TManaSymbolManager.Create;
begin
  inherited Create;
  FSymbolCache := TDictionary<string, string>.Create;
  FCacheFilePath := GetCacheFilePath(SymbolCacheFile);
end;

destructor TManaSymbolManager.Destroy;
begin
  try
    SaveCacheToFile;
  except
    on E: Exception do
      LogStuff('Error saving mana symbol cache during destruction: ' +
        E.Message, ERROR);
  end;
  FSymbolCache.Free;
  inherited Destroy;
end;

procedure TManaSymbolManager.Initialize;
begin
  LoadCacheFromFile;
  if FSymbolCache.Count = 0 then
    PopulateSymbolCache;
end;

function TManaSymbolManager.ReplaceManaSymbolsWithImages(const OracleText
  : string): string;
var
  Symbol, InlineSVG, RawSVG: string;
begin
  Result := OracleText;

  // Replace each mana symbol with its inline SVG
  for Symbol in FSymbolCache.Keys do
  begin
    RawSVG := FSymbolCache[Symbol];
    InlineSVG := Format(SVG_TEMPLATE, [RawSVG]);
    Result := Result.Replace(Symbol, InlineSVG, [rfReplaceAll]);
  end;
end;

procedure TManaSymbolManager.LoadCacheFromFile;
var
  CacheJson: TJsonObject;
  I: Integer;
  Key: string;
begin
  if not TFile.Exists(FCacheFilePath) then
    Exit; // No cache to load

  CacheJson := TJsonObject.Create;
  try
    try
      CacheJson.LoadFromFile(FCacheFilePath);

      // Populate the symbol cache
      for I := 0 to CacheJson.Count - 1 do
      begin
        Key := CacheJson.Names[I];
        if not FSymbolCache.ContainsKey(Key) then
          FSymbolCache.Add(Key, CacheJson.S[Key]);
      end;
      LogStuff('Symbol cache loaded from file.', DEBUG);
    except
      on E: Exception do
      begin
        LogStuff('Error loading mana symbol cache: ' + E.Message, ERROR);
        LogStuff('Deleting corrupt cache file: ' + FCacheFilePath, WARNING);
        FSymbolCache.Clear;
        TFile.Delete(FCacheFilePath);
      end;
    end;
  finally
    CacheJson.Free;
  end;
end;

procedure TManaSymbolManager.SaveCacheToFile;
var
  CacheJson: TJsonObject;
  Pair: TPair<string, string>;
begin
  CacheJson := TJsonObject.Create;
  try
    for Pair in FSymbolCache do
      CacheJson.S[Pair.Key] := Pair.Value;

    CacheJson.SaveToFile(FCacheFilePath, False, TEncoding.UTF8, False);
    LogStuff('Symbol cache saved to disk.', DEBUG);
  finally
    CacheJson.Free;
  end;
end;

function TManaSymbolManager.FetchAllSymbols: TJsonArray;
var
  HttpClient: THTTPClient;
  Response: IHTTPResponse;
  JsonResponse: TJsonObject;
begin

  HttpClient := THTTPClient.Create;
  try
    Response := HttpClient.Get(EndPointSymbology);
    if Response.StatusCode = 200 then
    begin
      JsonResponse := TJsonObject.Create;
      try
        JsonResponse.LoadFromStream(Response.ContentStream);
        Result := JsonResponse.A[FieldData].Clone as TJsonArray;
        LogStuff('Successfully fetched symbol list from Scryfall.', INFO);
      finally
        JsonResponse.Free;
      end;
    end
    else
      raise Exception.CreateFmt
        ('Failed to fetch symbols from Scryfall. HTTP status: %d. %s',
        [Response.StatusCode, Response.StatusText]);
  finally
    HttpClient.Free;
  end;
end;

function TManaSymbolManager.FetchSVGContent(const SVG_URL: string): string;
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
      LogStuff(Format
        ('Failed to fetch SVG content from %s. HTTP status: %d. %s',
        [SVG_URL, Response.StatusCode, Response.StatusText]), ERROR);
  finally
    HttpClient.Free;
  end;
end;

procedure TManaSymbolManager.PopulateSymbolCache;
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
    SymbolsJSON := FetchAllSymbols;
    if not Assigned(SymbolsJSON) then
      Exit;

    for I := 0 to SymbolsJSON.Count - 1 do
    begin
      SymbolObj := SymbolsJSON.O[I];
      Symbol := SymbolObj.S[FieldSymbol];
      SVGUrl := SymbolObj.S[FieldSVGUri];

      if not FSymbolCache.ContainsKey(Symbol) then
      begin
        // Fetch and store the SVG content
        SVGContent := FetchSVGContent(SVGUrl);
        if not SVGContent.IsEmpty then
        begin
          FSymbolCache.Add(Symbol, SVGContent);
          CacheUpdated := True;
        end
        else
          LogStuff(Format
            ('No SVG content retrieved for mana symbol [%s] from URL [%s]',
            [Symbol, SVGUrl]), WARNING);
      end;
    end;

    // Save cache if updated
    if CacheUpdated then
      SaveCacheToFile;
  finally
    SymbolsJSON.Free;
  end;
end;

/// <summary>
/// Global function to replace mana symbols with images.
/// </summary>
function ReplaceManaSymbolsWithImages(const OracleText: string): string;
begin
  Result := ManaSymbolManager.ReplaceManaSymbolsWithImages(OracleText);
end;

initialization

ManaSymbolManager := TManaSymbolManager.Create;
ManaSymbolManager.Initialize;

finalization

ManaSymbolManager.Free;

end.
