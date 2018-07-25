object fSettings: TfSettings
  Left = 380
  Top = 326
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  BorderWidth = 8
  Caption = 'Settings'
  ClientHeight = 344
  ClientWidth = 425
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnOK: TBitBtn
    Left = 205
    Top = 312
    Width = 107
    Height = 30
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 318
    Top = 312
    Width = 107
    Height = 30
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 425
    Height = 306
    ActivePage = TabSheet1
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'URQ'
      object lblURQInt: TLabel
        Left = 8
        Top = 16
        Width = 51
        Height = 13
        Caption = 'URQ path:'
      end
      object btSelURQ: TSpeedButton
        Left = 375
        Top = 39
        Width = 34
        Height = 22
        Caption = '...'
        OnClick = btSelURQClick
      end
      object edSelURQ: TEdit
        Left = 3
        Top = 40
        Width = 366
        Height = 21
        TabOrder = 0
      end
    end
  end
  object btnDefault: TBitBtn
    Left = 0
    Top = 312
    Width = 107
    Height = 30
    Caption = 'Default'
    TabOrder = 3
    OnClick = btnDefaultClick
  end
  object OpenDialog: TOpenDialog
    Left = 252
    Top = 24
  end
end
