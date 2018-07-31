unit uMain;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ActnList, uEditAppIntfs, ComCtrls, System.Actions;

type
  TfMain = class(TForm)
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
    actQuestRun: TAction;
    N6: TMenuItem;
    Run1: TMenuItem;
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
    procedure actQuestRunExecute(Sender: TObject);
    procedure actQuestRunUpdate(Sender: TObject);
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

procedure TfMain.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(Handle, True);
  FMRUItems[1] := miFileMRU1;
  FMRUItems[2] := miFileMRU2;
  FMRUItems[3] := miFileMRU3;
  FMRUItems[4] := miFileMRU4;
  FMRUItems[5] := miFileMRU5;
  fCommands := TfCommands.Create(Self);
  ReadIniSettings;
  Language := TLanguage.Create(True);
end;

procedure TfMain.FormDestroy(Sender: TObject);
begin
  if GI_EditorFactory <> nil then
    GI_EditorFactory.CloseAll;
  Language.SaveDefault;
  FreeAndNil(Language);
  WriteIniSettings;
  FreeAndNil(fCommands);
  DragAcceptFiles(Handle, False);
end;

procedure TfMain.FormShow(Sender: TObject);
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
  actQuestRun.Caption := _('&Run');
  mRecentFiles.Caption := _('Recent &Files');
  fCommands.actFileSave.Caption := _('&Save');
  fCommands.actFileSaveAs.Caption := _('Save &As...');
  fCommands.actFileClose.Caption := _('&Close');
  fCommands.actFilePrint.Caption := _('&Print...');
  // Edit
  fCommands.actEditUndo.Caption := _('&Undo');
  fCommands.actEditRedo.Caption := _('&Redo');
  fCommands.actEditCut.Caption := _('Cu&t');
  fCommands.actEditCopy.Caption := _('&Copy');
  fCommands.actEditPaste.Caption := _('&Paste');
  fCommands.actEditDelete.Caption := _('De&lete');
  fCommands.actEditSelectAll.Caption := _('Select &All');
  // Search
  fCommands.actSearchFind.Caption := _('&Find...');
  fCommands.actSearchFindNext.Caption := _('Find &Next');
  fCommands.actSearchFindPrev.Caption := _('Find &Previous');
  fCommands.actSearchReplace.Caption := _('&Replace...');
  // View
  actViewStatusbar.Caption := _('&Status Bar');
  actViewSettings.Caption := _('Se&ttings');
  // Help
  actAbout.Caption := _('&About...');
  //
end;

// implementation

function TfMain.CanCloseAll: Boolean;
begin
  Result := True;
end;

function TfMain.CmdLineOpenFiles(AMultipleFiles: Boolean): Boolean;
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

function TfMain.DoCreateEditor(AFileName: string): IEditor;
begin
  Result := nil;
end;

procedure TfMain.DoOpenFile(AFileName: string);
var
  I: Integer;
  LEditor: IEditor;
begin
  AFileName := ExpandFileName(AFileName);
  if AFileName <> '' then
  begin
    fCommands.RemoveMRUEntry(AFileName);
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

procedure TfMain.ReadIniSettings;
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
        fCommands.AddMRUEntry(S);
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TfMain.WMDropFiles(var Msg: TWMDropFiles);
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

procedure TfMain.WriteIniSettings;
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
      S := fCommands.GetMRUEntry(I - 1);
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

procedure TfMain.actViewSettingsExecute(Sender: TObject);
begin
  Utils.ShowForm(fSettings);
end;

procedure TfMain.actAboutExecute(Sender: TObject);
begin
  Utils.ShowForm(fAbout);
end;

procedure TfMain.actFileNewOrOpenUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GI_EditorFactory <> nil;
end;

procedure TfMain.actQuestNewExecute(Sender: TObject);
begin
  DoOpenFile('');
end;

procedure TfMain.actQuestOpenExecute(Sender: TObject);
var
  I: Integer;
begin
  with fCommands.dlgFileOpen do
  begin
    if Execute then
      for I := 0 to Files.Count - 1 do
        DoOpenFile(Files[I]);
  end;
end;

procedure TfMain.actQuestRunExecute(Sender: TObject);
var
  FileName, IntURQPath: string;
begin
  // Run
  FileName := '';
  if (GI_ActiveEditor <> nil) then
    FileName := GI_ActiveEditor.GetFileName;
  IntURQPath := Trim(fSettings.edSelURQ.Text);
  if (IntURQPath = '') or not FileExists(IntURQPath) then
    Exit;
  ShellExecute(Application.Handle, 'open', PWideChar(IntURQPath),
    PWideChar(FileName), nil, SW_SHOWNORMAL);
end;

procedure TfMain.actQuestRunUpdate(Sender: TObject);
var
  IntURQPath: string;
begin
  IntURQPath := Trim(fSettings.edSelURQ.Text);
  actQuestRun.Enabled := (GI_ActiveEditor <> nil) and
    not GI_ActiveEditor.GetModified and (IntURQPath <> '') and
    FileExists(IntURQPath)
end;

procedure TfMain.actQuestCloseAllExecute(Sender: TObject);
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

procedure TfMain.actQuestCloseAllUpdate(Sender: TObject);
begin
  actQuestCloseAll.Enabled := (GI_EditorFactory <> nil) and
    (GI_EditorFactory.GetEditorCount > 0);
end;

procedure TfMain.actQuestExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfMain.mRecentFilesClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := Low(FMRUItems) to High(FMRUItems) do
    if FMRUItems[I] <> nil then
    begin
      S := fCommands.GetMRUEntry(I - Low(FMRUItems));
      FMRUItems[I].Visible := S <> '';
      FMRUItems[I].Caption := S;
    end;
end;

procedure TfMain.mQuestClick(Sender: TObject);
begin
  mRecentFiles.Enabled := fCommands.GetMRUEntries > 0;
end;

procedure TfMain.actViewStatusbarUpdate(Sender: TObject);
begin
  actViewStatusbar.Checked := StatusBar.Visible;
end;

procedure TfMain.actViewStatusbarExecute(Sender: TObject);
begin
  StatusBar.Visible := not StatusBar.Visible;
end;

procedure TfMain.OnOpenMRUFile(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  for I := Low(FMRUItems) to High(FMRUItems) do
    if Sender = FMRUItems[I] then
    begin
      S := fCommands.GetMRUEntry(I - 1);
      if S <> '' then
        DoOpenFile(S);
    end;
end;

procedure TfMain.actUpdateStatusBarPanelsUpdate(Sender: TObject);
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
