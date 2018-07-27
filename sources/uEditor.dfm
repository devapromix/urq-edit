object EditorForm: TEditorForm
  Left = 338
  Top = 199
  ActiveControl = SynEditor
  Caption = 'Editor'
  ClientHeight = 456
  ClientWidth = 612
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 145
    Top = 0
    Height = 456
    ExplicitLeft = 224
    ExplicitTop = 136
    ExplicitHeight = 100
  end
  object SynEditor: TSynEdit
    Left = 148
    Top = 0
    Width = 464
    Height = 456
    Align = alClient
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    PopupMenu = pmnuEditor
    TabOrder = 0
    OnEnter = SynEditorEnter
    OnExit = SynEditorExit
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.ShowModification = True
    SearchEngine = SynEditSearch1
    OnChange = SynEditorChange
    OnReplaceText = SynEditorReplaceText
    OnStatusChange = SynEditorStatusChange
    FontSmoothing = fsmNone
    RemovedKeystrokes = <
      item
        Command = ecDeleteLastChar
        ShortCut = 8200
      end
      item
        Command = ecLineBreak
        ShortCut = 8205
      end
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 145
    Height = 456
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object ListBox1: TListBox
      Left = 0
      Top = 0
      Width = 145
      Height = 456
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object pmnuEditor: TPopupMenu
    Left = 356
    Top = 36
    object lmiEditUndo: TMenuItem
      Action = CommandsDataModule.actEditUndo
    end
    object lmiEditRedo: TMenuItem
      Action = CommandsDataModule.actEditRedo
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object lmiEditCut: TMenuItem
      Action = CommandsDataModule.actEditCut
    end
    object lmiEditCopy: TMenuItem
      Action = CommandsDataModule.actEditCopy
    end
    object lmiEditPaste: TMenuItem
      Action = CommandsDataModule.actEditPaste
    end
    object lmiEditDelete: TMenuItem
      Action = CommandsDataModule.actEditDelete
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object lmiEditSelectAll: TMenuItem
      Action = CommandsDataModule.actEditSelectAll
    end
  end
  object SynEditSearch1: TSynEditSearch
    Left = 356
    Top = 64
  end
end
