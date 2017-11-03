object ConfirmDialog: TConfirmDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Confirm'
  ClientHeight = 93
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 32
    Height = 32
  end
  object lblConfirmation: TLabel
    Left = 64
    Top = 8
    Width = 261
    Height = 44
    AutoSize = False
    WordWrap = True
  end
  object btnReplace: TButton
    Left = 38
    Top = 62
    Width = 75
    Height = 23
    Caption = '&Yes'
    Default = True
    ModalResult = 6
    TabOrder = 0
  end
  object btnSkip: TButton
    Left = 119
    Top = 62
    Width = 75
    Height = 23
    Caption = '&No'
    ModalResult = 7
    TabOrder = 1
  end
  object btnCancel: TButton
    Left = 198
    Top = 62
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
