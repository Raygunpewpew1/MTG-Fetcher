program mtgfetchV2;

uses
  System.SysUtils,
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  MainForm in 'MainForm.pas' {Form1},
  ScryfallData in 'ScryfallData.pas',
  DataModuleUnit in 'DataModuleUnit.pas' {DataModule1: TDataModule},
  MLogic in 'MLogic.pas',
  CardDisplayHelpers in 'CardDisplayHelpers.pas',
  ScryfallDataHelper in 'ScryfallDataHelper.pas',
  APIConstants in 'APIConstants.pas',
  mana in 'mana.pas',
  Logger in 'Logger.pas',
  Template in 'Template.pas',
  CardDisplayManager in 'CardDisplayManager.pas',
  ScryfallFilterType in 'ScryfallFilterType.pas',
  ScryfallQuery in 'ScryfallQuery.pas',
  CardMainData in 'CardMainData.pas',
  ISmellToast in 'ISmellToast.pas',
  CardMetaData in 'CardMetaData.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
 // FastMM_LogStateToFile(PWideChar(ExtractFilePath(ParamStr(0)) + 'FastMM_DetailedState.log'), nil, True);

//  FastMM_LogToFileEvents := [mmetUnexpectedMemoryLeakDetail, mmetUnexpectedMemoryLeakSummary];
//  FastMM_SetDebugModeStackTraceEntryCount(30);
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
