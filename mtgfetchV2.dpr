program mtgfetchV2;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  MainForm in 'MainForm.pas' {Form1},
  ScryfallData in 'ScryfallData.pas',
  SGlobalsZ in 'SGlobalsZ.pas',
  DataModuleUnit in 'DataModuleUnit.pas' {DataModule1: TDataModule},
  MLogic in 'MLogic.pas',
  JsonDataObjects in 'JsonDataObjects.pas',
  CardDisplayHelpers in 'CardDisplayHelpers.pas',
  WrapperHelper in 'WrapperHelper.pas',
  APIConstants in 'APIConstants.pas',
  mana in 'mana.pas',
  Logger in 'Logger.pas',
  Template in 'Template.pas',
  CardDisplayManager in 'CardDisplayManager.pas',
  ScryfallTypes in 'ScryfallTypes.pas',
  ScryfallQuery in 'ScryfallQuery.pas',
  CardDetailsHelper in 'CardDetailsHelper.pas';

 {$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
