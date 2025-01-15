unit DataModuleUnit;

interface

uses
  System.SysUtils, System.Classes,
  FireDAC.Stan.Intf, FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet,
  Data.DB, FireDAC.Comp.UI, System.Variants,
  FMX.Dialogs, JsonDataObjects, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.FMXUI.Wait;

type
  TDataModule1 = class(TDataModule)
    FDConnection1: TFDConnection;
    FDQuery1: TFDQuery;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;

  private

  public

    procedure SetupDatabase(const DBPath: string);

  end;

var
  DataModule1: TDataModule1;

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

end.
