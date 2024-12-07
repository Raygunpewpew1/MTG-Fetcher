program mtgfetchV2;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  ScryfallAPIWrapperV2 in 'ScryfallAPIWrapperV2.pas',
  SGlobalsZ in 'SGlobalsZ.pas',
  DataModuleUnit in 'DataModuleUnit.pas' {DataModule1: TDataModule},
  MLogic in 'MLogic.pas',
  JsonDataObjects in 'JsonDataObjects.pas',
  CardDisplayHelpers in 'CardDisplayHelpers.pas',
  WrapperHelper in 'WrapperHelper.pas',
  APIConstants in 'APIConstants.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
