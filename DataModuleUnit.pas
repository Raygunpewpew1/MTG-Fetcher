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
  System.IOUtils, SGlobalsZ;

type
  TDataModule1 = class(TDataModule)
    FDQuery1: TFDQuery;
    FDConnection1: TFDConnection;
  private
    procedure LogError(const Msg: string);
//    procedure ListCardDetailsFields;
  public
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
    TFile.AppendAllText(LogFile,
      Format('[%s] %s%s', [DateTimeToStr(Now), Msg, sLineBreak]),
      TEncoding.UTF8);
  except
    // Error handling for logging failure
  end;
end;

//procedure TDataModule1.ListCardDetailsFields;
//begin
//  FDQuery1.SQL.Text := 'PRAGMA table_info(CardDetails);';
//  FDQuery1.Open;
//  try
//    while not FDQuery1.Eof do
//    begin
//      ShowMessage(
//        Format('Field: %s | Type: %s',
//          [FDQuery1.FieldByName('name').AsString,
//           FDQuery1.FieldByName('type').AsString]));
//      FDQuery1.Next;
//    end;
//  finally
//    FDQuery1.Close;
//  end;
//end;

procedure TDataModule1.CreateCardDetailsTable;
begin
  if not FDConnection1.Connected then
    FDConnection1.Connected := True;
  try
    FDConnection1.ExecSQL('DROP TABLE IF EXISTS CardDetails;');
    FDConnection1.ExecSQL('CREATE TABLE IF NOT EXISTS CardDetails (' +
      'ID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      'CardName TEXT, ' +
      'TypeLine TEXT, ' +
      'ManaCost TEXT, ' +
      'OracleText TEXT, ' +
      'SetCode TEXT, ' +
      'SetName TEXT, ' +
      'Rarity TEXT, ' +
      'Power TEXT, ' +
      'Toughness TEXT, ' +
      'PrintsSearchUri TEXT, ' +
      'ImageUriSmall TEXT, ' +
      'ImageUriNormal TEXT, ' +
      'ImageUriLarge TEXT, ' +
      'ImageUriPng TEXT, ' +
      'PriceUsd TEXT, ' +
      'PriceUsdFoil TEXT, ' +
      'PriceEur TEXT, ' +
      'PriceTix TEXT, ' +
      'LegalStandard TEXT, ' +
      'LegalPioneer TEXT, ' +
      'LegalModern TEXT, ' +
      'LegalLegacy TEXT, ' +
      'LegalCommander TEXT, ' +
      'ImageBlob BLOB, ' +
      'Quantity INTEGER, ' +
      'OracleID TEXT, ' +
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
      'SetCode, SetName, Rarity, Power, Toughness, PrintsSearchUri, ImageUriSmall, ' +
      'ImageUriNormal, ImageUriLarge, ImageUriPng, PriceUsd, PriceUsdFoil, PriceEur, ' +
      'PriceTix, LegalStandard, LegalPioneer, LegalModern, LegalLegacy, LegalCommander, ' +
      'ImageBlob, Quantity, SFID, OracleID) ' +
      'VALUES (:CardName, :TypeLine, :ManaCost, :OracleText, :SetCode, :SetName, ' +
      ':Rarity, :Power, :Toughness, :PrintsSearchUri, :ImageUriSmall, :ImageUriNormal, ' +
      ':ImageUriLarge, :ImageUriPng, :PriceUsd, :PriceUsdFoil, :PriceEur, :PriceTix, ' +
      ':LegalStandard, :LegalPioneer, :LegalModern, :LegalLegacy, :LegalCommander, ' +
      ':ImageBlob, :Quantity, :SFID, :OracleID)';

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
      Query.ParamByName('PrintsSearchUri').AsString := CardDetails.PrintsSearchUri;
      Query.ParamByName('ImageUriSmall').AsString := CardDetails.ImageUris.Small;
      Query.ParamByName('ImageUriNormal').AsString := CardDetails.ImageUris.Normal;
      Query.ParamByName('ImageUriLarge').AsString := CardDetails.ImageUris.Large;
      Query.ParamByName('ImageUriPng').AsString := CardDetails.ImageUris.PNG;
      Query.ParamByName('PriceUsd').AsString := CardDetails.Prices.USD;
      Query.ParamByName('PriceUsdFoil').AsString := CardDetails.Prices.USD_Foil;
      Query.ParamByName('PriceEur').AsString := CardDetails.Prices.EUR;
      Query.ParamByName('PriceTix').AsString := CardDetails.Prices.Tix;
      Query.ParamByName('LegalStandard').AsString := CardDetails.Legalities.Standard;
      Query.ParamByName('LegalPioneer').AsString := CardDetails.Legalities.Pioneer;
      Query.ParamByName('LegalModern').AsString := CardDetails.Legalities.Modern;
      Query.ParamByName('LegalLegacy').AsString := CardDetails.Legalities.Legacy;
      Query.ParamByName('LegalCommander').AsString := CardDetails.Legalities.Commander;
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
    Query.SQL.Text := 'SELECT COUNT(*) AS Count FROM CardDetails WHERE SFID = :SFID';
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
    FDConnection1.Params.Values['Database'] := DBPath;
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
