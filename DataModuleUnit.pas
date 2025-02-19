unit DataModuleUnit;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Phys,
  FireDAC.Comp.Client, FireDAC.Comp.DataSet, Data.DB, FireDAC.Comp.UI,
  System.Variants, FMX.Dialogs, JsonDataObjects, FireDAC.Stan.Param,
  FireDAC.Phys.SQLite, Neon.Core.Types, Neon.Core.Persistence,
  Neon.Core.Persistence.JSON, System.JSON, CardMainData, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.FMXUI.Wait, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.Phys.SQLiteDef;

type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;

  private
  public
    procedure SetupDatabase(const DBPath: string);
    procedure SaveCardJson(MyCard: TCardDetails);
    function LoadCardJson(const OracleID: string): TCardDetails;

  end;

var
  DataModule1: TDataModule1;
  // FDQuery: TFDQuery;

implementation

{$R *.dfm}
{ TDataModule1 }

procedure TDataModule1.SetupDatabase(const DBPath: string);
begin
  FDConnection1.Connected := False;
  FDConnection1.Params.DriverID := 'SQLite';
  FDConnection1.Params.Database := DBPath;
  FDConnection1.LoginPrompt := False;
  FDGUIxWaitCursor1.Provider := 'FMX';
  // Just implanmenting the setup
  // old code didnt work
  try

    try
      FDConnection1.Connected := True;
    finally

    end;
  except
    on E: Exception do
      raise Exception.Create('Error connecting to database: ' + E.Message);
  end;
end;

procedure TDataModule1.SaveCardJson(MyCard: TCardDetails);
var
  LJSON: TJSONValue;
  JSONString: string;
  NeonConfig: INeonConfiguration;
  UIDCard: string;
  SQLString: string;
begin

  if not Assigned(MyCard) then
    raise Exception.Create('Card Data is nil!!!');

  UIDCard := MyCard.OracleID;

  NeonConfig := TNeonConfiguration.Default.SetMemberCase(TNeonCase.SnakeCase)
  // .SetMembers(TNeonMembers.Properties)
  // .SetIgnoreFieldPrefix(True)
  // .SetVisibility([ mvPublic])

  // .SetIgnoreEmptyValues(True)
;
  LJSON := TNeon.ObjectToJSON(MyCard, NeonConfig);
  try
    JSONString := LJSON.ToJSON;
  finally
    LJSON.Free;
  end;

  SQLString := '''
    INSERT INTO CardDetails(OracleID, Data)VALUES(: OracleID, : Data)
    ON CONFLICT(OracleID)
    DO UPDATE SET Data = excluded.Data;

  ''';

  try
    FDQuery1.Connection := FDConnection1;
    FDQuery1.SQL.Text := SQLString;
    FDQuery1.ParamByName('OracleID').AsString := UIDCard;
    FDQuery1.ParamByName('Data').AsString := JSONString;

    try
      FDQuery1.ExecSQL;
    except
      on E: Exception do
        raise Exception.Create('Error saving JSON to database: ' + E.Message);
    end;
  finally
    FDQuery1.Close;
  end;
end;

/// working on this
function TDataModule1.LoadCardJson(const OracleID: string): TCardDetails;
var
  LJSON: TJSONValue;
  JSONString: string;
  NeonConfig: INeonConfiguration;
begin
  Result := nil;

  // Query database for the given OracleID
  FDQuery1.SQL.Text := '''

    SELECT Data FROM CardDetails WHERE OracleID = : OracleID;

  ''';
  FDQuery1.ParamByName(' OracleID ').AsString := OracleID;
  FDQuery1.Open;

  // Exit if no data is found
  if FDQuery1.IsEmpty then
  begin
    FDQuery1.Close;
    Exit;
  end;

  JSONString := FDQuery1.FieldByName('Data').AsString;
  FDQuery1.Close;

  LJSON := TJSONObject.ParseJSONValue(JSONString);
  if not Assigned(LJSON) then
    raise Exception.Create('Failed to parse JSON for OracleID: ' + OracleID);

  try
    Result := TCardDetails.Create;

    NeonConfig := TNeonConfiguration.Default;

    TNeon.JSONToObject(Result, LJSON, NeonConfig);
  finally
    LJSON.Free;
  end;
end;

{ var
  JSONString: string;
  LJSON: TJSONValue;
  Card: TCardDetails;
  NeonConfig: TNeonConfiguration;
  begin
  // Retrieve your JSON string from the database. For example:
  JSONString := FDQuery1.FieldByName('Data').AsString;

  // Create an instance of TCardDetails
  Card := TCardDetails.Create;
  try
  // Optionally set up a Neon configuration
  NeonConfig := TNeonConfiguration.Default;

  // Parse the JSON string into a TJSONValue
  LJSON := TJSONObject.ParseJSONValue(JSONString);
  try
  // Use Neon to populate Card from the parsed JSON
  TNeon.JSONToObject(Card, LJSON, NeonConfig);
  finally
  LJSON.Free;
  end;

  // Card now contains the deserialized data.
  // ... use Card as needed
  finally
  Card.Free;
  end;
  end;
}

end.

