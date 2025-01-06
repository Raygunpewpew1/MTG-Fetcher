object DataModule1: TDataModule1
  Height = 750
  Width = 1000
  PixelsPerInch = 120
  object FDConnection1: TFDConnection
    Left = 856
    Top = 456
  end
  object FDQuery1: TFDQuery
    Connection = FDConnection1
    Left = 456
    Top = 312
  end
  object FDGUIxWaitCursor1: TFDGUIxWaitCursor
    Provider = 'FMX'
    Left = 536
    Top = 584
  end
end
