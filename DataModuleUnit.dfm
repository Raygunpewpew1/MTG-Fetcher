object DataModule1: TDataModule1
  Height = 1800
  Width = 2400
  PixelsPerInch = 288
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 1978
    Top = 1286
  end
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=C:\FMMagicFetch\Collection.db'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 1978
    Top = 1018
  end
end
