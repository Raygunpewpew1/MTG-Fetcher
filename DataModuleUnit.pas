unit DataModuleUnit;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.FMXUI.Wait, FireDAC.Stan.Param,
  FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, fmx.Dialogs,
  System.IOUtils, SGlobalsZ, System.Generics.Collections, System.StrUtils;

type
  TDataModule1 = class(TDataModule)
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
  private
    procedure LogError(const Msg: string);

    // procedure ListCardDetailsFields;
  public
    procedure SaveCardToCache(const Card: TCardDetails);
    // procedure CacheCards(const Cards: TArray<TCardDetails>);
    function GetCachedCards(const Query: string): TArray<TCardDetails>;
    procedure CreateCardDetailsTable;
    procedure SaveCardToDatabase(const CardDetails: TCardDetails;
      ImageStream: TMemoryStream; Quantity: Integer);
    function CheckCardExists(const SFID: string): Boolean;
    procedure SetupDatabaseConnection(const DBPath: string);

  end;

var
  DataModule1: TDataModule1;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}
{ TDataModule1 }

procedure TDataModule1.LogError(const Msg: string);
var
  LogFile: string;
begin
  LogFile := TPath.Combine(TPath.GetDocumentsPath, 'ErrorLog.txt');
  try
    TFile.AppendAllText(LogFile, Format('[%s] %s%s', [DateTimeToStr(Now), Msg,
      sLineBreak]), TEncoding.UTF8);
  except
    // Error handling for logging failure
  end;
end;

procedure TDataModule1.SaveCardToCache(const Card: TCardDetails);
var
  InsertQuery: TFDQuery;
begin
  InsertQuery := TFDQuery.Create(nil);
  try
    InsertQuery.Connection := FDConnection1;

    InsertQuery.SQL.Text := 'INSERT OR REPLACE INTO cached_cards (' +
      'id, name, set_code, set_name, mana_cost, type_line, oracle_text, rarity, '
      + 'power, toughness, loyalty, image_uri, back_face_image_uri, set_icon_uri, '
      + 'standard_legal, pioneer_legal, modern_legal, legacy_legal, commander_legal, '
      + 'vintage_legal, pauper_legal, historic_legal, explorer_legal, alchemy_legal, '
      + 'brawl_legal, future_legal, oldschool_legal, premodern_legal, duel_legal, penny_legal, '
      + 'price_usd, price_usd_foil, price_eur, price_tix) ' +
      'VALUES (:id, :name, :set_code, :set_name, :mana_cost, :type_line, :oracle_text, :rarity, '
      + ':power, :toughness, :loyalty, :image_uri, :back_face_image_uri, :set_icon_uri, '
      + ':standard_legal, :pioneer_legal, :modern_legal, :legacy_legal, :commander_legal, '
      + ':vintage_legal, :pauper_legal, :historic_legal, :explorer_legal, :alchemy_legal, '
      + ':brawl_legal, :future_legal, :oldschool_legal, :premodern_legal, :duel_legal, :penny_legal, '
      + ':price_usd, :price_usd_foil, :price_eur, :price_tix)';

    // Set parameters for core details
    InsertQuery.ParamByName('id').AsString := Card.SFID;
    InsertQuery.ParamByName('name').AsString := Card.CardName;
    InsertQuery.ParamByName('set_code').AsString := Card.SetCode;
    InsertQuery.ParamByName('set_name').AsString := Card.SetName;
    InsertQuery.ParamByName('mana_cost').AsString := Card.ManaCost;
    InsertQuery.ParamByName('type_line').AsString := Card.TypeLine;
    InsertQuery.ParamByName('oracle_text').AsString := Card.OracleText;
    InsertQuery.ParamByName('rarity').AsString := Card.Rarity;
    InsertQuery.ParamByName('power').AsString := Card.Power;
    InsertQuery.ParamByName('toughness').AsString := Card.Toughness;
    InsertQuery.ParamByName('loyalty').AsString := Card.Loyalty;
    InsertQuery.ParamByName('image_uri').AsString := Card.ImageUris.Normal;
    InsertQuery.ParamByName('back_face_image_uri').AsString :=
      Card.ImageUris.BackFace; // Save back face URI
    InsertQuery.ParamByName('set_icon_uri').AsString := Card.SetIconURI;

    // Set parameters for legalities and prices
    InsertQuery.ParamByName('standard_legal').AsString :=
      Card.Legalities.Standard;
    InsertQuery.ParamByName('pioneer_legal').AsString :=
      Card.Legalities.Pioneer;
    InsertQuery.ParamByName('modern_legal').AsString := Card.Legalities.Modern;
    InsertQuery.ParamByName('legacy_legal').AsString := Card.Legalities.Legacy;
    InsertQuery.ParamByName('commander_legal').AsString :=
      Card.Legalities.Commander;
    InsertQuery.ParamByName('vintage_legal').AsString :=
      Card.Legalities.Vintage;
    InsertQuery.ParamByName('pauper_legal').AsString := Card.Legalities.Pauper;
    InsertQuery.ParamByName('historic_legal').AsString :=
      Card.Legalities.Historic;
    InsertQuery.ParamByName('explorer_legal').AsString :=
      Card.Legalities.Explorer;
    InsertQuery.ParamByName('alchemy_legal').AsString :=
      Card.Legalities.Alchemy;
    InsertQuery.ParamByName('brawl_legal').AsString := Card.Legalities.Brawl;
    InsertQuery.ParamByName('future_legal').AsString := Card.Legalities.Future;
    InsertQuery.ParamByName('oldschool_legal').AsString :=
      Card.Legalities.Oldschool;
    InsertQuery.ParamByName('premodern_legal').AsString :=
      Card.Legalities.Premodern;
    InsertQuery.ParamByName('duel_legal').AsString := Card.Legalities.Duel;
    InsertQuery.ParamByName('penny_legal').AsString := Card.Legalities.Penny;

    InsertQuery.ParamByName('price_usd').AsString := Card.Prices.USD;
    InsertQuery.ParamByName('price_usd_foil').AsString := Card.Prices.USD_Foil;
    InsertQuery.ParamByName('price_eur').AsString := Card.Prices.EUR;
    InsertQuery.ParamByName('price_tix').AsString := Card.Prices.Tix;

    InsertQuery.ExecSQL;
  finally
    InsertQuery.Free;
  end;
end;

function TDataModule1.GetCachedCards(const Query: string): TArray<TCardDetails>;
var
  QueryData: TFDQuery;
  Card: TCardDetails;
  Results: TList<TCardDetails>;
begin
  Results := TList<TCardDetails>.Create;
  try
    QueryData := TFDQuery.Create(nil);
    try
      QueryData.Connection := FDConnection1;
      QueryData.SQL.Text :=
        'SELECT * FROM cached_cards WHERE name = :query COLLATE NOCASE';
      QueryData.ParamByName('query').AsString := Query.Trim;
      QueryData.Open;

      while not QueryData.Eof do
      begin
        Card.Clear;

        // Core card details
        Card.SFID := QueryData.FieldByName('id').AsString;
        Card.CardName := QueryData.FieldByName('name').AsString;
        Card.SetCode := QueryData.FieldByName('set_code').AsString;
        Card.SetName := QueryData.FieldByName('set_name').AsString;
        Card.ManaCost := QueryData.FieldByName('mana_cost').AsString;
        Card.TypeLine := QueryData.FieldByName('type_line').AsString;
        Card.OracleText := QueryData.FieldByName('oracle_text').AsString;
        Card.Rarity := QueryData.FieldByName('rarity').AsString;
        Card.Power := QueryData.FieldByName('power').AsString;
        Card.Toughness := QueryData.FieldByName('toughness').AsString;
        Card.Loyalty := QueryData.FieldByName('loyalty').AsString;
        Card.ImageUris.Normal := QueryData.FieldByName('image_uri').AsString;

        // Back face URI
        Card.ImageUris.BackFace := QueryData.FieldByName
          ('back_face_image_uri').AsString;

        // Additional attributes
        Card.SetIconURI := QueryData.FieldByName('set_icon_uri').AsString;

        // Legalities and Prices
        Card.Legalities.Standard := QueryData.FieldByName
          ('standard_legal').AsString;
        Card.Legalities.Pioneer := QueryData.FieldByName
          ('pioneer_legal').AsString;
        Card.Legalities.Modern := QueryData.FieldByName('modern_legal')
          .AsString;
        Card.Legalities.Legacy := QueryData.FieldByName('legacy_legal')
          .AsString;
        Card.Legalities.Commander := QueryData.FieldByName
          ('commander_legal').AsString;
        Card.Legalities.Vintage := QueryData.FieldByName
          ('vintage_legal').AsString;
        Card.Legalities.Pauper := QueryData.FieldByName('pauper_legal')
          .AsString;
        Card.Legalities.Historic := QueryData.FieldByName
          ('historic_legal').AsString;
        Card.Legalities.Explorer := QueryData.FieldByName
          ('explorer_legal').AsString;
        Card.Legalities.Alchemy := QueryData.FieldByName
          ('alchemy_legal').AsString;
        Card.Legalities.Brawl := QueryData.FieldByName('brawl_legal').AsString;
        Card.Legalities.Future := QueryData.FieldByName('future_legal')
          .AsString;
        Card.Legalities.Oldschool := QueryData.FieldByName
          ('oldschool_legal').AsString;
        Card.Legalities.Premodern := QueryData.FieldByName
          ('premodern_legal').AsString;
        Card.Legalities.Duel := QueryData.FieldByName('duel_legal').AsString;
        Card.Legalities.Penny := QueryData.FieldByName('penny_legal').AsString;

        // Prices
        Card.Prices.USD := QueryData.FieldByName('price_usd').AsString;
        Card.Prices.USD_Foil := QueryData.FieldByName('price_usd_foil')
          .AsString;
        Card.Prices.EUR := QueryData.FieldByName('price_eur').AsString;
        Card.Prices.Tix := QueryData.FieldByName('price_tix').AsString;

        Results.Add(Card);
        QueryData.Next;
      end;
    finally
      QueryData.Free;
    end;

    Result := Results.ToArray;
  finally
    Results.Free;
  end;
end;

procedure TDataModule1.CreateCardDetailsTable;
begin
  if not FDConnection1.Connected then
    FDConnection1.Connected := True;
  try
    FDConnection1.ExecSQL('DROP TABLE IF EXISTS CardDetails;');
    FDConnection1.ExecSQL('CREATE TABLE IF NOT EXISTS CardDetails (' +
      'ID INTEGER PRIMARY KEY AUTOINCREMENT, ' + 'CardName TEXT, ' +
      'TypeLine TEXT, ' + 'ManaCost TEXT, ' + 'OracleText TEXT, ' +
      'SetCode TEXT, ' + 'SetName TEXT, ' + 'Rarity TEXT, ' + 'Power TEXT, ' +
      'Toughness TEXT, ' + 'PrintsSearchUri TEXT, ' + 'ImageUriSmall TEXT, ' +
      'ImageUriNormal TEXT, ' + 'ImageUriLarge TEXT, ' + 'ImageUriPng TEXT, ' +
      'PriceUsd TEXT, ' + 'PriceUsdFoil TEXT, ' + 'PriceEur TEXT, ' +
      'PriceTix TEXT, ' + 'LegalStandard TEXT, ' + 'LegalPioneer TEXT, ' +
      'LegalModern TEXT, ' + 'LegalLegacy TEXT, ' + 'LegalCommander TEXT, ' +
      'ImageBlob BLOB, ' + 'Quantity INTEGER, ' + 'OracleID TEXT, ' +
      'SFID TEXT UNIQUE);');
  except
    on E: Exception do
    begin
      LogError('Error creating table: ' + E.Message);
      ShowMessage('Error creating table: ' + E.Message);
    end;
  end;
end;

procedure TDataModule1.SaveCardToDatabase(const CardDetails: TCardDetails;
  ImageStream: TMemoryStream; Quantity: Integer);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'INSERT INTO CardDetails (CardName, TypeLine, ManaCost, OracleText, ' +
      'SetCode, SetName, Rarity, Power, Toughness, PrintsSearchUri, ImageUriSmall, '
      + 'ImageUriNormal, ImageUriLarge, ImageUriPng, PriceUsd, PriceUsdFoil, PriceEur, '
      + 'PriceTix, LegalStandard, LegalPioneer, LegalModern, LegalLegacy, LegalCommander, '
      + 'ImageBlob, Quantity, SFID, OracleID) ' +
      'VALUES (:CardName, :TypeLine, :ManaCost, :OracleText, :SetCode, :SetName, '
      + ':Rarity, :Power, :Toughness, :PrintsSearchUri, :ImageUriSmall, :ImageUriNormal, '
      + ':ImageUriLarge, :ImageUriPng, :PriceUsd, :PriceUsdFoil, :PriceEur, :PriceTix, '
      + ':LegalStandard, :LegalPioneer, :LegalModern, :LegalLegacy, :LegalCommander, '
      + ':ImageBlob, :Quantity, :SFID, :OracleID)';

    FDConnection1.StartTransaction;
    try
      Query.ParamByName('CardName').AsString := CardDetails.CardName;
      Query.ParamByName('TypeLine').AsString := CardDetails.TypeLine;
      Query.ParamByName('ManaCost').AsString := CardDetails.ManaCost;
      Query.ParamByName('OracleText').AsString := CardDetails.OracleText;
      Query.ParamByName('SetCode').AsString := CardDetails.SetCode;
      Query.ParamByName('SetName').AsString := CardDetails.SetName;
      Query.ParamByName('Rarity').AsString := CardDetails.Rarity;
      Query.ParamByName('Power').AsString := CardDetails.Power;
      Query.ParamByName('Toughness').AsString := CardDetails.Toughness;
      Query.ParamByName('PrintsSearchUri').AsString :=
        CardDetails.PrintsSearchUri;
      Query.ParamByName('ImageUriSmall').AsString :=
        CardDetails.ImageUris.Small;
      Query.ParamByName('ImageUriNormal').AsString :=
        CardDetails.ImageUris.Normal;
      Query.ParamByName('ImageUriLarge').AsString :=
        CardDetails.ImageUris.Large;
      Query.ParamByName('ImageUriPng').AsString := CardDetails.ImageUris.PNG;
      Query.ParamByName('PriceUsd').AsString := CardDetails.Prices.USD;
      Query.ParamByName('PriceUsdFoil').AsString := CardDetails.Prices.USD_Foil;
      Query.ParamByName('PriceEur').AsString := CardDetails.Prices.EUR;
      Query.ParamByName('PriceTix').AsString := CardDetails.Prices.Tix;
      Query.ParamByName('LegalStandard').AsString :=
        CardDetails.Legalities.Standard;
      Query.ParamByName('LegalPioneer').AsString :=
        CardDetails.Legalities.Pioneer;
      Query.ParamByName('LegalModern').AsString :=
        CardDetails.Legalities.Modern;
      Query.ParamByName('LegalLegacy').AsString :=
        CardDetails.Legalities.Legacy;
      Query.ParamByName('LegalCommander').AsString :=
        CardDetails.Legalities.Commander;
      Query.ParamByName('OracleID').AsString := CardDetails.OracleID;
      Query.ParamByName('SFID').AsString := CardDetails.SFID;
      Query.ParamByName('Quantity').AsInteger := Quantity;

      if Assigned(ImageStream) and (ImageStream.Size > 0) then
      begin
        ImageStream.Position := 0;
        Query.ParamByName('ImageBlob').LoadFromStream(ImageStream, ftBlob);
      end
      else
        Query.ParamByName('ImageBlob').Clear;

      Query.ExecSQL;
      FDConnection1.Commit;
      ShowMessage('Card saved successfully.');
    except
      on E: Exception do
      begin
        FDConnection1.Rollback;
        LogError('Error saving card to database: ' + E.Message);
        ShowMessage('Error saving card to database: ' + E.Message);
      end;
    end;
  finally
    Query.Free;
  end;
end;

function TDataModule1.CheckCardExists(const SFID: string): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FDConnection1;
    Query.SQL.Text :=
      'SELECT COUNT(*) AS Count FROM CardDetails WHERE SFID = :SFID';
    Query.ParamByName('SFID').AsString := SFID;
    Query.Open;

    if not Query.IsEmpty then
      Result := Query.FieldByName('Count').AsInteger > 0;
  finally
    Query.Free;
  end;
end;

procedure TDataModule1.SetupDatabaseConnection(const DBPath: string);
begin
  try
    FDConnection1.Close;

    FDConnection1.Params.DriverID := 'SQLite';
    FDConnection1.Params.Database := DBPath;
    FDConnection1.Params.Add('LockingMode=Normal');
    FDConnection1.Params.Add('Synchronous=Normal');

    FDConnection1.DriverName := 'SQLite';
    FDConnection1.LoginPrompt := False;
    FDConnection1.Connected := True;

    // ShowMessage('Database connection established successfully.');
  except
    on E: Exception do
    begin
      LogError('Error setting up database connection: ' + E.Message);
      ShowMessage('Error setting up database connection: ' + E.Message);
    end;
  end;
end;

end.
