unit uMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, uEditAppIntfs, ComCtrls, System.Actions;

type
  TMainForm = class(TForm)
    mnuMain: TMainMenu;
    mQuest: TMenuItem;
    miFileExit: TMenuItem;
    miFileNew: TMenuItem;
    N1: TMenuItem;
    mEdit: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    miFileClose: TMenuItem;
    miEditUndo: TMenuItem;
    miEditRedo: TMenuItem;
    N2: TMenuItem;
    miEditCut: TMenuItem;
    miEditCopy: TMenuItem;
    miEditPaste: TMenuItem;
    miEditDelete: TMenuItem;
    miEditSelectAll: TMenuItem;
    N3: TMenuItem;
    miEditFind: TMenuItem;
    miEditFindNext: TMenuItem;
    miEditFindPrev: TMenuItem;
    miEditReplace: TMenuItem;
    StatusBar: TStatusBar;
    miViewStatusbar: TMenuItem;
    mView: TMenuItem;
    N4: TMenuItem;
    mRecentFiles: TMenuItem;
    miFileMRU5: TMenuItem;
    miFileMRU4: TMenuItem;
    miFileMRU3: TMenuItem;
    miFileMRU2: TMenuItem;
    miFileMRU1: TMenuItem;
    N5: TMenuItem;
    miFilePrint: TMenuItem;
    actlStandard: TActionList;
    actQuestNew: TAction;
    actQuestOpen: TAction;
    actQuestExit: TAction;
    actViewStatusbar: TAction;
    actUpdateStatusBarPanels: TAction;
    actQuestCloseAll: TAction;
    miFileCloseAll: TMenuItem;
    mHelp: TMenuItem;
    mAbout: TMenuItem;
    actAbout: TAction;
    actViewSettings: TAction;
    miViewSettings: TMenuItem;
    N7: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mQuestClick(Sender: TObject);
    procedure actFileNewOrOpenUpdate(Sender: TObject);
    procedure actQuestNewExecute(Sender: TObject);
    procedure actQuestOpenExecute(Sender: TObject);
    procedure actQuestExitExecute(Sender: TObject);
    procedure mRecentFilesClick(Sender: TObject);
    procedure actViewStatusbarUpdate(Sender: TObject);
    procedure actViewStatusbarExecute(Sender: TObject);
    procedure OnOpenMRUFile(Sender: TObject);
    procedure actUpdateStatusBarPanelsUpdate(Sender: TObject);
    procedure actQuestCloseAllExecute(Sender: TObject);
    procedure actQuestCloseAllUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actViewSettingsExecute(Sender: TObject);
  private
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  protected
    FMRUItems: array [1 .. 5] of TMenuItem;
    function CanCloseAll: Boolean;
    function CmdLineOpenFiles(AMultipleFiles: Boolean): Boolean;
    function DoCreateEditor(AFileName: string): IEditor; virtual;
    procedure DoOpenFile(AFileName: string);
    procedure ReadIniSettings;
    procedure WriteIniSettings;
  end;

implementation

{$R *.DFM}

uses
  IniFiles, uCommands, uLanguage, uAbout, uUtils, Winapi.ShellAPI, uSettings;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
  FMRUItems[1] := miFileMRU1;
  FMRUItems[2] := miFileMRU2;
  FMRUItems[3] := miFileMRU3;
  FMRUItems[4] := miFileMRU4;
  FMRUItems[5] := miFileMRU5;
  CommandsDataModule := TCommandsDataModule.Create(Self);
  ReadIniSettings;
  Language := TLanguage.Create(True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if GI_EditorFactory <> nil then
    GI_EditorFactory.CloseAll;
  Language.SaveDefault;
  FreeAndNil(Language);
  WriteIniSettings;
  FreeAndNil(CommandsDataModule);
  DragAcceptFiles(Handle, False);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  // Main menu
  mQuest.Caption := _('&Quest');
  mEdit.Caption := _('&Edit');
  mView.Caption := _('&View');
  mHelp.Caption := _('&Help');
  // Quest
  actQuestNew.Caption := _('&New');
  actQuestOpen.Caption := _('&Open...');
  actQuestCloseAll.Caption := _('Close All Fi&les');
  actQuestExit.Caption := _('E&xit');
  mRecentFiles.Caption := _('&Recent Files');
  CommandsDataModule.actFileSave.Caption := _('&Save');
  CommandsDataModule.actFileSaveAs.Caption := _('Save &As...');
  CommandsDataModule.actFileClose.Caption := _('&Close');
  CommandsDataModule.actFilePrint.Caption := _('&Print...');
  // Edit
  CommandsDataModule.actEditUndo.Caption := _('&Undo');
  CommandsDataModule.actEditRedo.Caption := _('&Redo');
  CommandsDataModule.actEditCut.Caption := _('Cu&t');
  CommandsDataModule.actEditCopy.Caption := _('&Copy');
  CommandsDataModule.actEditPaste.Caption := _('&Paste');
  CommandsDataModule.actEditDelete.Caption := _('De&lete');
  CommandsDataModule.actEditSelectAll.Caption := _('Select &All');
  // Search
  CommandsDataModule.actSearchFind.Caption := _('&Find...');
  CommandsDataModule.actSearchFindNext.Caption := _('Find &Next');
  CommandsDataModule.actSearchFindPrev.Caption := _('Find &Previous');
  CommandsDataModule.actSearchReplace.Caption := _('&Replace...');
  // View
  actViewStatusbar.Caption := _('&Status Bar');
  actViewSettings.Caption := _('Se&ttings');
  // Help
  actAbout.Caption := _('&About...');
  //
end;

// implementation

function TMainForm.CanCloseAll: Boolean;
begin
  Result := True;
end;

function TMainForm.CmdLineOpenFiles(AMultipleFiles: Boolean): Boolean;
var
  I, Cnt: Integer;
begin
  Cnt := ParamCount;
  if Cnt > 0 then
  begin
    if not AMultipleFiles and (Cnt > 1) then
      Cnt := 1;
    for I := 1 to Cnt do
      DoOpenFile(ParamStr(I));
    Result := True;
  end
  else
    Result := False;
end;

function TMainForm.DoCreateEditor(AFileName: string): IEditor;
begin
  Result := nil;
end;

procedure TMainForm.DoOpenFile(AFileName: string);
var
  I: Integer;
  LEditor: IEditor;
begin
  AFileName := ExpandFileName(AFileName);
  if AFileName <> '' then
  begin
    CommandsDataModule.RemoveMRUEntry(AFileName);
    // activate the editor if already open
    Assert(GI_EditorFactory <> nil);
    for I := GI_EditorFactory.GetEditorCount - 1 downto 0 do
    begin
      LEditor := GI_EditorFactory.Editor[I];
      if CompareText(LEditor.GetFileName, AFileName) = 0 then
      begin
        LEditor.Activate;
        Exit;
      end;
    end;
  end;
  // create a new editor, add it to the editor list, open the file
  LEditor := DoCreateEditor(AFileName);
  if LEditor <> nil then
    LEditor.OpenFile(AFileName);
end;

procedure TMainForm.ReadIniSettings;
var
  IniFile: TIniFile;
  X, Y, W, H: Integer;
  I: Integer;
  S: string;
begin
  IniFile := TIniFile.Create(Utils.GetPath('') + 'config.ini');
  try
    X := IniFile.ReadInteger('Main', 'Left', 0);
    Y := IniFile.ReadInteger('Main', 'Top', 0);
    W := IniFile.ReadInteger('Main', 'Width', 0);
    H := IniFile.ReadInteger('Main', 'Height', 0);
    if (W > 0) and (H > 0) then
      SetBounds(X, Y, W, H);
    if IniFile.ReadInteger('Main', 'Maximized', 0) <> 0 then
      WindowState := wsMaximized;
    StatusBar.Visible := IniFile.ReadInteger('Main', 'ShowStatusbar', 1) <> 0;
    // MRU files
    for I := 5 downto 1 do
    begin
      S := IniFile.ReadString('MRUFiles', Format('MRUFile%d', [I]), '');
      if S <> '' then
        CommandsDataModule.AddMRUEntry(S);
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
var
  FileName: array [0 .. MAX_PATH] of Char;
begin
  // Бросить файл в окно программы
  try
    if DragQueryFile(Msg.Drop, 0, FileName, MAX_PATH) > 0 then
    begin
      DoOpenFile(FileName);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure TMainForm.WriteIniSettings;
var
  IniFile: TIniFile;
  WP: TWindowPlacement;
  I: Integer;
  S: string;
begin
  IniFile := TIniFile.Create(Utils.GetPath('') + 'config.ini');
  try
    WP.length := SizeOf(TWindowPlacement);
    GetWindowPlacement(Handle, @WP);
    // form properties
    with WP.rcNormalPosition do
    begin
      IniFile.WriteInteger('Main', 'Left', Left);
      IniFile.WriteInteger('Main', 'Top', Top);
      IniFile.WriteInteger('Main', 'Width', Right - Left);
      IniFile.WriteInteger('Main', 'Height', Bottom - Top);
    end;
    IniFile.WriteInteger('Main', 'Maximized', Ord(WindowState = wsMaximized));
    IniFile.WriteInteger('Main', 'ShowStatusbar', Ord(StatusBar.Visible));
    // MRU files
    for I := 1 to 5 do
    begin
      S := CommandsDataModule.GetMRUEntry(I - 1);
      if S <> '' then
        IniFile.WriteString('MRUFiles', Format('MRUFile%d', [I]), S)
      else
        IniFile.DeleteKey('MRUFiles', Format('MRUFile%d', [I]));
    end;
  finally
    IniFile.Free;
  end;
end;

// action handler methods

procedure TMainForm.actViewSettingsExecute(Sender: TObject);
begin
  Utils.ShowForm(fSettings);
end;

procedure TMainForm.actAboutExecute(Sender: TObject);
begin
  Utils.ShowForm(AboutDialog);
end;

procedure TMainForm.actFileNewOrOpenUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GI_EditorFactory <> nil;
end;

procedure TMainForm.actQuestNewExecute(Sender: TObject);
begin
  DoOpenFile('');
end;

procedure TMainForm.actQuestOpenExecute(Sender: TObject);
var
  I: Integer;
begin
  with CommandsDataModule.dlgFileOpen do
  begin
    if Execute then
      for I := 0 to Files.Count - 1 do
        DoOpenFile(Files[I]);
  end;
end;

procedure TMainForm.actQuestCloseAllExecute(Sender: TObject);
var
  I: Integer;
begin
  if GI_EditorFactory <> nil then
  begin
    if not CanCloseAll then
      Exit;
    I := GI_EditorFactory.GetEditorCount - 1;
    // close all editor childs
    while I >= 0 do
    begin
      GI_EditorFactory.GetEditor(I).Close;
      Dec(I);
    end;
  end;
end;

procedure TMainForm.actQuestCloseAllUpdate(Sender: TObject);
begin
  actQuestCloseAll.Enabled := (GI_EditorFactory <> nil) and
    (GI_EditorFactory.GetEditorCount > 0);
end;

procedure TMainForm.actQuestExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.mRecentFilesClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := Low(FMRUItems) to High(FMRUItems) do
    if FMRUItems[I] <> nil then
    begin
      S := CommandsDataModule.GetMRUEntry(I - Low(FMRUItems));
      FMRUItems[I].Visible := S <> '';
      FMRUItems[I].Caption := S;
    end;
end;

procedure TMainForm.mQuestClick(Sender: TObject);
begin
  mRecentFiles.Enabled := CommandsDataModule.GetMRUEntries > 0;
end;

procedure TMainForm.actViewStatusbarUpdate(Sender: TObject);
begin
  actViewStatusbar.Checked := StatusBar.Visible;
end;

procedure TMainForm.actViewStatusbarExecute(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TMainForm.OnOpenMRUFile(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := Low(FMRUItems) to High(FMRUItems) do
    if Sender = FMRUItems[I] then
    begin
      S := CommandsDataModule.GetMRUEntry(I - 1);
      if S <> '' then
        DoOpenFile(S);
    end;
end;

procedure TMainForm.actUpdateStatusBarPanelsUpdate(Sender: TObject);
var
  PtCaret: TPoint;
begin
  actUpdateStatusBarPanels.Enabled := True;
  if GI_ActiveEditor <> nil then
  begin
    PtCaret := GI_ActiveEditor.GetCaretPos;
    if (PtCaret.X > 0) and (PtCaret.Y > 0) then
      StatusBar.Panels[0].Text := Format(' %6d:%3d ', [PtCaret.Y, PtCaret.X])
    else
      StatusBar.Panels[0].Text := '';
    if GI_ActiveEditor.GetModified then
      StatusBar.Panels[1].Text := _('Modified')
    else
      StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := GI_ActiveEditor.GetEditorState;
  end
  else
  begin
    StatusBar.Panels[0].Text := '';
    StatusBar.Panels[1].Text := '';
    StatusBar.Panels[2].Text := '';
  end;
end;

end.
