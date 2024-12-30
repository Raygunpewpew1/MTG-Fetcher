program mtgfetchV2;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  MainForm in 'MainForm.pas' {Form1},
  ScryfallAPIWrapperV2 in 'ScryfallAPIWrapperV2.pas',
  SGlobalsZ in 'SGlobalsZ.pas',
  DataModuleUnit in 'DataModuleUnit.pas' {DataModule1: TDataModule},
  MLogic in 'MLogic.pas',
  JsonDataObjects in 'JsonDataObjects.pas',
  CardDisplayHelpers in 'CardDisplayHelpers.pas',
  WrapperHelper in 'WrapperHelper.pas',
  APIConstants in 'APIConstants.pas',
  Mana in 'Mana.pas',
  Logger in 'Logger.pas',
  MainFormLogicHelpers in 'MainFormLogicHelpers.pas',
  Template in 'Template.pas';

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  {$R *.res}
  Application.Run;

end.
