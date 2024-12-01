object DataModule1: TDataModule1
  Height = 750
  Width = 1000
  PixelsPerInch = 120
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 824
    Top = 536
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 824
    Top = 424
  end
end
