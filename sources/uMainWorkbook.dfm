inherited WorkbookMainForm: TWorkbookMainForm
  Caption = 'URQEdit'
  ClientHeight = 500
  ClientWidth = 650
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  ExplicitWidth = 658
  ExplicitHeight = 554
  PixelsPerInch = 96
  TextHeight = 13
  object pctrlMain: TPageControl [0]
    Left = 0
    Top = 0
    Width = 650
    Height = 481
    Align = alClient
    TabOrder = 0
    TabStop = False
    OnChange = pctrlMainChange
  end
  inherited StatusBar: TStatusBar
    Top = 481
    Width = 650
    ExplicitTop = 481
    ExplicitWidth = 650
  end
  inherited mnuMain: TMainMenu
    inherited mFile: TMenuItem
      Caption = '&Quest'
    end
  end
  inherited actlStandard: TActionList
    Top = 76
  end
end
