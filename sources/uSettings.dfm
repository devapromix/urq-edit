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
    Left = 102
    Top = 312
    Width = 107
    Height = 30
    Caption = #1043#1086#1090#1086#1074#1086
    Default = True
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000064000000640000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00317A360A2D753207FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF003985400A37833DFF317B37FB2E763307FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF004292490A408E47FF54A35CFF4F9F57FF327C38FE2E773408FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF004B9E530A499A51FF5BAC64FF77CA82FF74C87EFF51A059FF337D39FE2F78
      3508FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0053A9
      5C0A51A65AFF63B56DFF7ECE89FF7BCC87FF76CA81FF76C981FF52A25AFF347E
      3AFE30793508FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF005AB4650959B0
      63FF6BBD76FF84D290FF7AC985FF60B26AFF63B46DFF78C983FF78CB82FF53A3
      5CFF347F3AFD317A3608FFFFFF00FFFFFF00FFFFFF00FFFFFF005EB969465BB5
      66E479C986FF80CE8DFF51A65AFC4DA1566F499C518B5CAD67FF7CCC86FF79CB
      85FF54A45DFF35803BFC317B3708FFFFFF00FFFFFF00FFFFFF00FFFFFF005FBA
      6A3C5CB666E66DC079FF55AC5F6FFFFFFF00FFFFFF004A9D52915EAE68FF7DCD
      89FF7CCD87FF56A55FFF36813CFC327C3808FFFFFF00FFFFFF00FFFFFF00FFFF
      FF005FBB6A435CB76765FFFFFF00FFFFFF00FFFFFF00FFFFFF004B9E53915FAF
      69FF7FCE8AFF7ECE89FF57A660FF37823DFC337D3908FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF004B9F
      549160B06AFF81CF8DFF7FCF8BFF58A761FF398540FF347E3A08FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF004CA0559162B26CFF82D18FFF7AC885FF57A660FF38843F7BFFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF004DA1569163B36DFF5FAF69FF41914979FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF004EA257914A9D527FFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00}
    ModalResult = 1
    TabOrder = 0
    OnClick = btnOKClick
  end
  object btnCancel: TBitBtn
    Left = 215
    Top = 312
    Width = 107
    Height = 30
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    Glyph.Data = {
      36040000424D3604000000000000360000002800000010000000100000000100
      2000000000000004000064000000640000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF003F3DED413B38EB08FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00211FE3081E1CE241FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF004A47F0414F4CF2FF403EEDFD3C39EB08FFFFFF00FFFFFF00FFFFFF00FFFF
      FF002725E5082422E4FC312FEAFF1F1DE241FFFFFF00FFFFFF00FFFFFF005451
      F3415856F5FF6361FAFF5855F6FF413FEDFC3D3AEC08FFFFFF00FFFFFF00302D
      E7082C2AE6FC413FF1FF4C4AF6FF312FEAFF1F1DE241FFFFFF00FFFFFF005956
      F52B5B58F6FF6562FAFF7170FFFF5956F6FF4240EEFC3E3BEC083937EB083532
      E9FC4745F2FF6362FFFF4A48F4FF2F2DE9FF2220E32BFFFFFF00FFFFFF00FFFF
      FF005A57F52B5B59F6FF6663FAFF7471FFFF5A58F6FF4341EEFC3E3CECFD504D
      F4FF6867FFFF504EF5FF3634EBFF2A27E52BFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF005B58F62B5C5AF6FF6764FAFF7472FFFF7370FFFF706EFFFF6E6C
      FFFF5755F7FF3F3DEEFF3230E82BFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF005C59F62B5D5BF7FF7976FFFF5956FFFF5754FFFF7270
      FFFF4846F0FF3C39EB2BFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00615EF8085D5AF6FD7D79FFFF5E5BFFFF5B58FFFF7674
      FFFF4643EFFD413FED08FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF006967FB086663F9FC706DFBFF807EFFFF7E7BFFFF7C79FFFF7977
      FFFF5E5CF7FF4744EFFC4240EE08FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00716EFD086E6BFCFC7774FDFF8682FFFF7673FCFF6462F8FF605DF7FF6D6A
      FAFF7B79FFFF605DF7FF4845EFFC4341EE08FFFFFF00FFFFFF00FFFFFF007673
      FF087471FEFD7D7AFEFF8A87FFFF7C79FDFF6C69FBFF6361F92B5F5CF72B615E
      F8FF6E6CFAFF7D7AFFFF615FF7FF4946F0FC4441EE05FFFFFF00FFFFFF007774
      FF1F7A77FFFF817EFFFF817EFEFF7471FDFF6C69FB2BFFFFFF00FFFFFF00605D
      F72B625FF8FF6F6DFBFF7E7CFFFF625FF8FF4A47F06F4542EE02FFFFFF00FFFF
      FF007774FF1F7A77FFFF7976FEFF726FFD2BFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00615EF82B6461F8FF6A68F9FF5451F3A84F4DF229FFFFFF00FFFFFF00FFFF
      FF00FFFFFF007774FF1F7774FF2BFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00625FF82B5D5BF76F5956F53EFFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF006360F80AFFFFFF00FFFFFF00FFFFFF00}
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
  object OpenDialog: TOpenDialog
    Left = 252
    Top = 24
  end
end
