program mtgfetchV2;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  MainForm in 'MainForm.pas' {Form1},
  ScryfallAPIWrapperV2 in 'ScryfallAPIWrapperV2.pas',
  SGlobalsZ in 'SGlobalsZ.pas',
  DataModuleUnit in 'DataModuleUnit.pas' {DataModule1: TDataModule},
  HighResForm in 'HighResForm.pas' {HighResImageForm},
  CollectionForm in 'CollectionForm.pas' {Form2},
  MLogic in 'MLogic.pas',
  JSONHelper in 'JSONHelper.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(THighResImageForm, HighResImageForm);
  Application.CreateForm(THighResImageForm, HighResImageForm);
  Application.CreateForm(TForm2, Form2);
  Application.Run;

end.
