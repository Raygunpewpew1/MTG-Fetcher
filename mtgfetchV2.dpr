program mtgfetchV2;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form1},
  ScryfallAPIWrapperV2 in 'ScryfallAPIWrapperV2.pas',
  SGlobalsZ in 'SGlobalsZ.pas',
  DataModuleUnit in 'DataModuleUnit.pas' {DataModule1: TDataModule},
  CardCollectionform in 'CardCollectionform.pas' {CollectionsForm},
  HighResForm in 'HighResForm.pas' {HighResImageForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TCollectionsForm, CollectionsForm);
  Application.CreateForm(THighResImageForm, HighResImageForm);
  Application.Run;

end.
