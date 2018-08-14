object fEditor: TfEditor
  Left = 338
  Top = 199
  ActiveControl = SynEditor
  Caption = 'Editor'
  ClientHeight = 310
  ClientWidth = 580
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 145
    Top = 0
    Height = 310
    ExplicitLeft = 224
    ExplicitTop = 136
    ExplicitHeight = 100
  end
  object SynEditor: TSynEdit
    Left = 148
    Top = 0
    Width = 432
    Height = 310
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
    Height = 310
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 0
      Top = 0
      Width = 145
      Height = 310
      ActivePage = TabSheet1
      Align = alClient
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'KeyWords'
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object KeyWordsList: TTreeView
          Left = 0
          Top = 0
          Width = 137
          Height = 282
          Align = alClient
          AutoExpand = True
          DoubleBuffered = True
          Indent = 19
          ParentDoubleBuffered = False
          ReadOnly = True
          TabOrder = 0
          OnClick = KeyWordsListClick
          OnDeletion = KeyWordsListDeletion
          ExplicitLeft = -3
          ExplicitTop = 3
        end
      end
    end
  end
  object pmnuEditor: TPopupMenu
    Left = 356
    Top = 36
    object lmiEditUndo: TMenuItem
      Action = fCommands.actEditUndo
    end
    object lmiEditRedo: TMenuItem
      Action = fCommands.actEditRedo
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object lmiEditCut: TMenuItem
      Action = fCommands.actEditCut
    end
    object lmiEditCopy: TMenuItem
      Action = fCommands.actEditCopy
    end
    object lmiEditPaste: TMenuItem
      Action = fCommands.actEditPaste
    end
    object lmiEditDelete: TMenuItem
      Action = fCommands.actEditDelete
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object lmiEditSelectAll: TMenuItem
      Action = fCommands.actEditSelectAll
    end
  end
  object SynEditSearch1: TSynEditSearch
    Left = 452
    Top = 40
  end
end
