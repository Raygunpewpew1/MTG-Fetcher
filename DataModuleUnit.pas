unit DataModuleUnit;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.FMXUI.Wait,
  Data.DB, FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, FireDAC.Comp.DataSet,SGlobalsZ,FireDAC.Comp.UI;

type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;

  private

  public
    procedure SetupDatabase(const DBPath: string);
    procedure CreateDatabaseSchema;
    procedure SaveCardToDatabase(const CardDetails: TCardDetails);
    function CheckCardExists(const SFID: string): Boolean;
  end;

var
  DataModule1: TDataModule1;

implementation
uses
WrapperHelper, Mlogic;

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TDataModule1.SetupDatabase(const DBPath: string);
begin
  try
    FDConnection1.Connected := False;
    FDConnection1.Params.DriverID := 'SQLite';
    FDConnection1.Params.Database := DBPath;
    FDConnection1.Params.Add('LockingMode=Normal');
    FDConnection1.Params.Add('Synchronous=Normal');
    FDConnection1.LoginPrompt := False;
    FDConnection1.Connected := True;
  except
    on E: Exception do
    begin
      LogStuff('Error setting up database: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TDataModule1.CreateDatabaseSchema;
begin
  if not FDConnection1.Connected then
    Exit;

  try
    FDConnection1.ExecSQL('CREATE TABLE IF NOT EXISTS CardDetails (' +
      'ID INTEGER PRIMARY KEY AUTOINCREMENT, ' +
      'SFID TEXT UNIQUE NOT NULL, OracleID TEXT, CardName TEXT, ' +
      'TypeLine TEXT, ManaCost TEXT, OracleText TEXT, FlavorText TEXT, ' +
      'Power TEXT, Toughness TEXT, Loyalty TEXT, Rarity TEXT, ' +
      'SetCode TEXT, SetName TEXT, ReleasedAt TEXT, ImageUriSmall TEXT, ' +
      'ImageUriNormal TEXT, ImageUriLarge TEXT, ImageUriPng TEXT, Reserved INTEGER, ' +
      'Digital INTEGER, FullArt INTEGER, Textless INTEGER, StorySpotlight INTEGER, ' +
      'Quantity INTEGER DEFAULT 0, PriceUsd TEXT, PriceUsdFoil TEXT, PriceEur TEXT, PriceTix TEXT);');

    FDConnection1.ExecSQL('CREATE TABLE IF NOT EXISTS CardFaces (' +
      'ID INTEGER PRIMARY KEY AUTOINCREMENT, CardID INTEGER NOT NULL, ' +
      'FaceName TEXT, ManaCost TEXT, TypeLine TEXT, OracleText TEXT, Power TEXT, ' +
      'Toughness TEXT, Loyalty TEXT, ImageUriSmall TEXT, ImageUriNormal TEXT, ImageUriLarge TEXT, ' +
      'FOREIGN KEY (CardID) REFERENCES CardDetails(ID) ON DELETE CASCADE);');

    FDConnection1.ExecSQL('CREATE TABLE IF NOT EXISTS Legalities (' +
      'ID INTEGER PRIMARY KEY AUTOINCREMENT, CardID INTEGER NOT NULL, ' +
      'Format TEXT, Legality TEXT, FOREIGN KEY (CardID) REFERENCES CardDetails(ID) ON DELETE CASCADE);');

    FDConnection1.ExecSQL('CREATE TABLE IF NOT EXISTS Keywords (' +
      'ID INTEGER PRIMARY KEY AUTOINCREMENT, CardID INTEGER NOT NULL, ' +
      'Keyword TEXT, FOREIGN KEY (CardID) REFERENCES CardDetails(ID) ON DELETE CASCADE);');
  except
    on E: Exception do
    begin
      LogStuff('Error creating database schema: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TDataModule1.SaveCardToDatabase(const CardDetails: TCardDetails);
var
  CardID: Integer;
begin
  if not FDConnection1.Connected then
    Exit;

  FDConnection1.StartTransaction;
  try
    // Insert into CardDetails
    FDQuery1.SQL.Text := 'INSERT OR REPLACE INTO CardDetails (' +
      'SFID, OracleID, CardName, TypeLine, ManaCost, OracleText, FlavorText, ' +
      'Power, Toughness, Loyalty, Rarity, SetCode, SetName, ReleasedAt, ImageUriSmall, ' +
      'ImageUriNormal, ImageUriLarge, ImageUriPng, Reserved, Digital, FullArt, Textless, ' +
      'StorySpotlight, Quantity, PriceUsd, PriceUsdFoil, PriceEur, PriceTix) ' +
      'VALUES (:SFID, :OracleID, :CardName, :TypeLine, :ManaCost, :OracleText, :FlavorText, ' +
      ':Power, :Toughness, :Loyalty, :Rarity, :SetCode, :SetName, :ReleasedAt, :ImageUriSmall, ' +
      ':ImageUriNormal, :ImageUriLarge, :ImageUriPng, :Reserved, :Digital, :FullArt, :Textless, ' +
      ':StorySpotlight, :Quantity, :PriceUsd, :PriceUsdFoil, :PriceEur, :PriceTix);';

    FDQuery1.ParamByName('SFID').AsString := CardDetails.SFID;
    FDQuery1.ParamByName('OracleID').AsString := CardDetails.OracleID;
    FDQuery1.ParamByName('CardName').AsString := CardDetails.CardName;
    FDQuery1.ParamByName('TypeLine').AsString := CardDetails.TypeLine;
    FDQuery1.ParamByName('ManaCost').AsString := CardDetails.ManaCost;
    FDQuery1.ParamByName('OracleText').AsString := CardDetails.OracleText;
    FDQuery1.ParamByName('FlavorText').AsString := CardDetails.FlavorText;
    FDQuery1.ParamByName('Power').AsString := CardDetails.Power;
    FDQuery1.ParamByName('Toughness').AsString := CardDetails.Toughness;
    FDQuery1.ParamByName('Loyalty').AsString := CardDetails.Loyalty;
    FDQuery1.ParamByName('Rarity').AsString := CardDetails.Rarity;
    FDQuery1.ParamByName('SetCode').AsString := CardDetails.SetCode;
    FDQuery1.ParamByName('SetName').AsString := CardDetails.SetName;
    FDQuery1.ParamByName('ReleasedAt').AsString := CardDetails.ReleasedAt;
    FDQuery1.ParamByName('ImageUriSmall').AsString :=
      CardDetails.ImageUris.Small;
    FDQuery1.ParamByName('ImageUriNormal').AsString :=
      CardDetails.ImageUris.Normal;
    FDQuery1.ParamByName('ImageUriLarge').AsString :=
      CardDetails.ImageUris.Large;
    FDQuery1.ParamByName('ImageUriPng').AsString := CardDetails.ImageUris.PNG;
    FDQuery1.ParamByName('Reserved').AsInteger := Integer(CardDetails.Reserved);
    FDQuery1.ParamByName('Digital').AsInteger := Integer(CardDetails.Digital);
    FDQuery1.ParamByName('FullArt').AsInteger := Integer(CardDetails.FullArt);
    FDQuery1.ParamByName('Textless').AsInteger := Integer(CardDetails.Textless);
    FDQuery1.ParamByName('StorySpotlight').AsInteger :=
      Integer(CardDetails.StorySpotlight);
    FDQuery1.ParamByName('Quantity').AsInteger := 1; // Default to 1
    FDQuery1.ParamByName('PriceUsd').AsString := CardDetails.Prices.USD;
    FDQuery1.ParamByName('PriceUsdFoil').AsString :=
      CardDetails.Prices.USD_Foil;
    FDQuery1.ParamByName('PriceEur').AsString := CardDetails.Prices.EUR;
    FDQuery1.ParamByName('PriceTix').AsString := CardDetails.Prices.Tix;
    FDQuery1.ExecSQL;

    // Retrieve the CardID of the inserted record
    CardID := FDConnection1.GetLastAutoGenValue('CardDetails');

    // Insert into CardFaces
    for var Face in CardDetails.CardFaces do
    begin
      FDQuery1.SQL.Text :=
        'INSERT INTO CardFaces (CardID, FaceName, ManaCost, TypeLine, OracleText, ' +
        'Power, Toughness, Loyalty, ImageUriSmall, ImageUriNormal, ImageUriLarge) ' +
        'VALUES (:CardID, :FaceName, :ManaCost, :TypeLine, :OracleText, :Power, :Toughness, ' +
        ':Loyalty, :ImageUriSmall, :ImageUriNormal, :ImageUriLarge);';
      FDQuery1.ParamByName('CardID').AsInteger := CardID;
      FDQuery1.ParamByName('FaceName').AsString := Face.Name;
      FDQuery1.ParamByName('ManaCost').AsString := Face.ManaCost;
      FDQuery1.ParamByName('TypeLine').AsString := Face.TypeLine;
      FDQuery1.ParamByName('OracleText').AsString := Face.OracleText;
      FDQuery1.ParamByName('Power').AsString := Face.Power;
      FDQuery1.ParamByName('Toughness').AsString := Face.Toughness;
      FDQuery1.ParamByName('Loyalty').AsString := Face.Loyalty;
      FDQuery1.ParamByName('ImageUriSmall').AsString := Face.ImageUris.Small;
      FDQuery1.ParamByName('ImageUriNormal').AsString := Face.ImageUris.Normal;
      FDQuery1.ParamByName('ImageUriLarge').AsString := Face.ImageUris.Large;
      FDQuery1.ExecSQL;
    end;

    // Insert into Legalities
    for var FormatName in LegalitiesArray do
    begin
      var Legality := GetLegalStatus(CardDetails.Legalities, FormatName);
      if Legality <> '' then
      begin
        FDQuery1.SQL.Text := 'INSERT INTO Legalities (CardID, Format, Legality) '
          +
          'VALUES (:CardID, :Format, :Legality);';
        FDQuery1.ParamByName('CardID').AsInteger := CardID;
        FDQuery1.ParamByName('Format').AsString := FormatName;
        FDQuery1.ParamByName('Legality').AsString := Legality;
        FDQuery1.ExecSQL;
      end;
    end;

    // Insert into Keywords
    for var Keyword in CardDetails.Keywords do
    begin
      FDQuery1.SQL.Text :=
        'INSERT INTO Keywords (CardID, Keyword) VALUES (:CardID, :Keyword);';
      FDQuery1.ParamByName('CardID').AsInteger := CardID;
      FDQuery1.ParamByName('Keyword').AsString := Keyword;
      FDQuery1.ExecSQL;
    end;

    FDConnection1.Commit;
  except
    on E: Exception do
    begin
      FDConnection1.Rollback;
      LogStuff('Error saving card to database: ' + E.Message);
      raise;
    end;
  end;
end;

function TDataModule1.CheckCardExists(const SFID: string): Boolean;
begin
  FDQuery1.SQL.Text := 'SELECT COUNT(*) FROM CardDetails WHERE SFID = :SFID;';
  FDQuery1.ParamByName('SFID').AsString := SFID;
  FDQuery1.Open;
  try
    Result := FDQuery1.Fields[0].AsInteger > 0;
  finally
    FDQuery1.Close;
  end;
end;

end.
