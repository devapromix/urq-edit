object MainForm: TMainForm
  Left = 186
  Top = 133
  ClientHeight = 312
  ClientWidth = 506
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = mnuMain
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 293
    Width = 506
    Height = 19
    Action = actUpdateStatusBarPanels
    Panels = <
      item
        Alignment = taCenter
        Width = 84
      end
      item
        Alignment = taCenter
        Width = 72
      end
      item
        Alignment = taCenter
        Width = 84
      end
      item
        Width = 50
      end>
  end
  object mnuMain: TMainMenu
    Left = 28
    Top = 32
    object mQuest: TMenuItem
      Caption = '&Quest'
      OnClick = mQuestClick
      object miFileNew: TMenuItem
        Action = actQuestNew
      end
      object miFileOpen: TMenuItem
        Action = actQuestOpen
      end
      object mRecentFiles: TMenuItem
        Caption = '&Recent Files'
        OnClick = mRecentFilesClick
        object miFileMRU1: TMenuItem
          Caption = '[MRU1]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU2: TMenuItem
          Caption = '[MRU2]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU3: TMenuItem
          Caption = '[MRU3]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU4: TMenuItem
          Caption = '[MRU4]'
          OnClick = OnOpenMRUFile
        end
        object miFileMRU5: TMenuItem
          Caption = '[MRU5]'
          OnClick = OnOpenMRUFile
        end
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object miFileSave: TMenuItem
        Action = CommandsDataModule.actFileSave
      end
      object miFileSaveAs: TMenuItem
        Action = CommandsDataModule.actFileSaveAs
      end
      object miFileClose: TMenuItem
        Action = CommandsDataModule.actFileClose
      end
      object miFileCloseAll: TMenuItem
        Action = actQuestCloseAll
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object miFilePrint: TMenuItem
        Action = CommandsDataModule.actFilePrint
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miFileExit: TMenuItem
        Action = actQuestExit
      end
    end
    object mEdit: TMenuItem
      Caption = '&Edit'
      object miEditUndo: TMenuItem
        Action = CommandsDataModule.actEditUndo
      end
      object miEditRedo: TMenuItem
        Action = CommandsDataModule.actEditRedo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object miEditCut: TMenuItem
        Action = CommandsDataModule.actEditCut
      end
      object miEditCopy: TMenuItem
        Action = CommandsDataModule.actEditCopy
      end
      object miEditPaste: TMenuItem
        Action = CommandsDataModule.actEditPaste
      end
      object miEditDelete: TMenuItem
        Action = CommandsDataModule.actEditDelete
      end
      object miEditSelectAll: TMenuItem
        Action = CommandsDataModule.actEditSelectAll
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object miEditFind: TMenuItem
        Action = CommandsDataModule.actSearchFind
      end
      object miEditFindNext: TMenuItem
        Action = CommandsDataModule.actSearchFindNext
      end
      object miEditFindPrev: TMenuItem
        Action = CommandsDataModule.actSearchFindPrev
      end
      object miEditReplace: TMenuItem
        Action = CommandsDataModule.actSearchReplace
      end
    end
    object mView: TMenuItem
      Caption = '&View'
      object miViewStatusbar: TMenuItem
        Action = actViewStatusbar
      end
      object N6: TMenuItem
        Action = acConfig
      end
    end
    object mHelp: TMenuItem
      Caption = '&Help'
      object mAbout: TMenuItem
        Action = actAbout
      end
    end
  end
  object actlStandard: TActionList
    Left = 76
    Top = 68
    object actQuestNew: TAction
      Category = 'Quest'
      Caption = '&New'
      ShortCut = 16462
      OnExecute = actQuestNewExecute
      OnUpdate = actFileNewOrOpenUpdate
    end
    object actQuestOpen: TAction
      Category = 'Quest'
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = actQuestOpenExecute
      OnUpdate = actFileNewOrOpenUpdate
    end
    object actQuestCloseAll: TAction
      Category = 'Quest'
      Caption = 'Close All Fi&les'
      Enabled = False
      ShortCut = 24691
      OnExecute = actQuestCloseAllExecute
      OnUpdate = actQuestCloseAllUpdate
    end
    object actQuestExit: TAction
      Category = 'Quest'
      Caption = 'E&xit'
      ShortCut = 32883
      OnExecute = actQuestExitExecute
    end
    object actViewStatusbar: TAction
      Category = 'View'
      Caption = '&Status Bar'
      OnExecute = actViewStatusbarExecute
      OnUpdate = actViewStatusbarUpdate
    end
    object actUpdateStatusBarPanels: TAction
      Caption = 'actUpdateStatusBarPanels'
      OnUpdate = actUpdateStatusBarPanelsUpdate
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = '&About...'
      OnExecute = actAboutExecute
    end
    object acConfig: TAction
      Category = 'View'
      Caption = 'Settings'
      Hint = 'Settings'
      OnExecute = acConfigExecute
    end
  end
end
