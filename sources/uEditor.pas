unit uEditor;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
  uEditAppIntfs, SynEdit, SynEditTypes, SynEditMiscProcs,
  SynEditMiscClasses, SynEditSearch, SynUnicode, ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.Dialogs;

type
  TEditorKind = (ekBorderless, ekInTabsheet, ekMDIChild);

  TEditor = class;

  TfEditor = class(TForm)
    SynEditor: TSynEdit;
    pmnuEditor: TPopupMenu;
    lmiEditCut: TMenuItem;
    lmiEditCopy: TMenuItem;
    lmiEditPaste: TMenuItem;
    lmiEditDelete: TMenuItem;
    N1: TMenuItem;
    lmiEditSelectAll: TMenuItem;
    lmiEditUndo: TMenuItem;
    lmiEditRedo: TMenuItem;
    N2: TMenuItem;
    SynEditSearch1: TSynEditSearch;
    Panel1: TPanel;
    Splitter1: TSplitter;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    KeyWordsList: TTreeView;
    procedure SynEditorReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure SynEditorChange(Sender: TObject);
    procedure SynEditorEnter(Sender: TObject);
    procedure SynEditorExit(Sender: TObject);
    procedure SynEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure FormCreate(Sender: TObject);
    procedure KeyWordsListDeletion(Sender: TObject; Node: TTreeNode);
    procedure KeyWordsListClick(Sender: TObject);
  private
    FEditor: TEditor;
    FKind: TEditorKind;
    FSearchFromCaret: Boolean;
    FSearchBackwards: Boolean;
    FSearchCaseSensitive: Boolean;
    FSearchSelectionOnly: Boolean;
    FSearchTextAtCaret: Boolean;
    FSearchWholeWords: Boolean;
    function DoAskSaveChanges: Boolean;
    procedure DoAssignInterfacePointer(AActive: Boolean);
    function DoSave: Boolean;
    function DoSaveFile: Boolean;
    function DoSaveAs: Boolean;
    procedure DoSearchReplaceText(AReplace: Boolean; ABackwards: Boolean);
    procedure DoUpdateCaption;
    procedure DoUpdateHighlighter;
    procedure ShowSearchReplaceDialog(AReplace: Boolean);
    procedure InitKeyWordsList;
    procedure FreeKeyWordsList;
    function IsLastCharDigit(const S: string): Boolean;
  public
    procedure DoActivate;
  end;

  TEditor = class(TInterfacedObject, IEditor, IEditCommands, IFileCommands, ISearchCommands)
  private
    // IEditor implementation
    procedure Activate;
    function AskSaveChanges: Boolean;
    procedure Close;
    function GetCaretPos: TPoint;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetModified: Boolean;
    procedure OpenFile(AFileName: string);
    // IEditCommands implementation
    function CanCopy: Boolean;
    function CanCut: Boolean;
    function IEditCommands.CanDelete = CanCut;
    function CanPaste: Boolean;
    function CanRedo: Boolean;
    function CanSelectAll: Boolean;
    function CanUndo: Boolean;
    procedure ExecCopy;
    procedure ExecCut;
    procedure ExecDelete;
    procedure ExecPaste;
    procedure ExecRedo;
    procedure ExecSelectAll;
    procedure ExecUndo;
    // IFileCommands implementation
    function CanClose: Boolean;
    function CanPrint: Boolean;
    function CanSave: Boolean;
    function CanSaveAs: Boolean;
    procedure IFileCommands.ExecClose = Close;
    procedure ExecPrint;
    procedure ExecSave;
    procedure ExecSaveAs;
    // ISearchCommands implementation
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function ISearchCommands.CanFindPrev = CanFindNext;
    function CanReplace: Boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
  private
    FFileName: string;
    FForm: TfEditor;
    FHasSelection: Boolean;
    FIsEmpty: Boolean;
    FIsReadOnly: Boolean;
    FModified: Boolean;
    FUntitledNumber: Integer;
    constructor Create(AForm: TfEditor);
    procedure DoSetFileName(AFileName: string);
  end;

implementation

{$R *.DFM}

uses
  IniFiles, Character, uCommands, uSearchText, uReplaceText, uConfirmReplace,
  uMainWorkbook, uLanguage, uConfirm, uUtils, uMain;

const
  WM_DELETETHIS = WM_USER + 42;

var
  gsSearchText: string;
  gsSearchTextHistory: string;
  gsReplaceText: string;
  gsReplaceTextHistory: string;

  { TEditor }

constructor TEditor.Create(AForm: TfEditor);
begin
  Assert(AForm <> nil);
  inherited Create;
  FForm := AForm;
  FUntitledNumber := -1;
end;

procedure TEditor.Activate;
begin
  if FForm <> nil then
    FForm.DoActivate;
end;

function TEditor.AskSaveChanges: Boolean;
begin
  if FForm <> nil then
    Result := FForm.DoAskSaveChanges
  else
    Result := True;
end;

function TEditor.CanClose: Boolean;
begin
  Result := FForm <> nil;
end;

procedure TEditor.Close;
begin
  if (FFileName <> '') and (FCommands <> nil) then
    FCommands.AddMRUEntry(FFileName);
  if FUntitledNumber <> -1 then
    FCommands.ReleaseUntitledNumber(FUntitledNumber);
  if FForm <> nil then
    FForm.Close;
end;

procedure TEditor.DoSetFileName(AFileName: string);
begin
  if AFileName <> FFileName then
  begin
    FFileName := AFileName;
    if FUntitledNumber <> -1 then
    begin
      FCommands.ReleaseUntitledNumber(FUntitledNumber);
      FUntitledNumber := -1;
    end;
  end;
end;

function TEditor.GetCaretPos: TPoint;
begin
  if FForm <> nil then
    Result := TPoint(FForm.SynEditor.CaretXY)
  else
    Result := Point(-1, -1);
end;

function TEditor.GetEditorState: string;
begin
  // Состояние редактора
  if FForm <> nil then
  begin
    if FForm.SynEditor.ReadOnly then
      Result := _('Read Only')
    else if FForm.SynEditor.InsertMode then
      Result := _('Insert')
    else
      Result := _('Overwrite');
  end
  else
    Result := '';
end;

function TEditor.GetFileName: string;
begin
  Result := FFileName;
end;

function TEditor.GetFileTitle: string;
begin
  // Название закладки редактора
  // Если открыт сущ. файл, то выводится его название
  if FFileName <> '' then
    Result := ExtractFileName(FFileName)
  else
  begin
    // ... иначе "Квест 1" и т.д.
    if FUntitledNumber = -1 then
      FUntitledNumber := FCommands.GetUntitledNumber;
    Result := _('Quest') + FUntitledNumber.ToString;
  end;
end;

function TEditor.GetModified: Boolean;
begin
  if FForm <> nil then
    Result := FForm.SynEditor.Modified
  else
    Result := False;
end;

procedure TEditor.OpenFile(AFileName: string);
begin
  FFileName := AFileName;
  if FForm <> nil then
  begin
    if (AFileName <> '') and FileExists(AFileName) then
      FForm.SynEditor.Lines.LoadFromFile(AFileName)
    else
      FForm.SynEditor.Lines.Clear;
    FForm.DoUpdateCaption;
    FForm.DoUpdateHighlighter;
  end;
end;

// IEditCommands implementation

function TEditor.CanCopy: Boolean;
begin
  Result := (FForm <> nil) and FHasSelection;
end;

function TEditor.CanCut: Boolean;
begin
  Result := (FForm <> nil) and FHasSelection and not FIsReadOnly;
end;

function TEditor.CanPaste: Boolean;
begin
  Result := (FForm <> nil) and FForm.SynEditor.CanPaste;
end;

function TEditor.CanRedo: Boolean;
begin
  Result := (FForm <> nil) and FForm.SynEditor.CanRedo;
end;

function TEditor.CanSelectAll: Boolean;
begin
  Result := FForm <> nil;
end;

function TEditor.CanUndo: Boolean;
begin
  Result := (FForm <> nil) and FForm.SynEditor.CanUndo;
end;

procedure TEditor.ExecCopy;
begin
  if FForm <> nil then
    FForm.SynEditor.CopyToClipboard;
end;

procedure TEditor.ExecCut;
begin
  if FForm <> nil then
    FForm.SynEditor.CutToClipboard;
end;

procedure TEditor.ExecDelete;
begin
  if FForm <> nil then
    FForm.SynEditor.SelText := '';
end;

procedure TEditor.ExecPaste;
begin
  if FForm <> nil then
    FForm.SynEditor.PasteFromClipboard;
end;

procedure TEditor.ExecRedo;
begin
  if FForm <> nil then
    FForm.SynEditor.Redo;
end;

procedure TEditor.ExecSelectAll;
begin
  if FForm <> nil then
    FForm.SynEditor.SelectAll;
end;

procedure TEditor.ExecUndo;
begin
  if FForm <> nil then
    FForm.SynEditor.Undo;
end;

// IFileCommands implementation

function TEditor.CanPrint: Boolean;
begin
  Result := False;
end;

function TEditor.CanSave: Boolean;
begin
  Result := (FForm <> nil) and (FModified or (FFileName = ''));
end;

function TEditor.CanSaveAs: Boolean;
begin
  Result := FForm <> nil;
end;

procedure TEditor.ExecPrint;
begin
  if FForm <> nil then
    // TODO
end;

procedure TEditor.ExecSave;
begin
  if FForm <> nil then
  begin
    if FFileName <> '' then
      FForm.DoSave
    else
      FForm.DoSaveAs
  end;
end;

procedure TEditor.ExecSaveAs;
begin
  if FForm <> nil then
    FForm.DoSaveAs;
end;

// ISearchCommands implementation

function TEditor.CanFind: Boolean;
begin
  Result := (FForm <> nil) and not FIsEmpty;
end;

function TEditor.CanFindNext: Boolean;
begin
  Result := (FForm <> nil) and not FIsEmpty and (gsSearchText <> '');
end;

function TEditor.CanReplace: Boolean;
begin
  Result := (FForm <> nil) and not FIsReadOnly and not FIsEmpty;
end;

procedure TEditor.ExecFind;
begin
  if FForm <> nil then
    FForm.ShowSearchReplaceDialog(False);
end;

procedure TEditor.ExecFindNext;
begin
  if FForm <> nil then
    FForm.DoSearchReplaceText(False, False);
end;

procedure TEditor.ExecFindPrev;
begin
  if FForm <> nil then
    FForm.DoSearchReplaceText(False, True);
end;

procedure TEditor.ExecReplace;
begin
  if FForm <> nil then
    FForm.ShowSearchReplaceDialog(True);
end;

{ TEditorTabSheet }

type
  TEditorTabSheet = class(TTabSheet)
  private
    procedure WMDeleteThis(var Msg: TMessage); message WM_DELETETHIS;
    constructor Create(AOwner: TComponent);
  end;

constructor TEditorTabSheet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.PopupMenu := fCommands.PopupMenu1;
end;

procedure TEditorTabSheet.WMDeleteThis(var Msg: TMessage);
begin
  Free;
end;

{ TEditorFactory }

type
  TEditorFactory = class(TInterfacedObject, IEditorFactory)
  private
    function CanCloseAll: Boolean;
    procedure CloseAll;
    function CreateBorderless(AOwner: TForm): IEditor;
    function CreateMDIChild(AOwner: TForm): IEditor;
    function CreateTabSheet(AOwner: TPageControl): IEditor;
    function GetEditorCount: Integer;
    function GetEditor(Index: Integer): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
  private
    FEditors: TInterfaceList;
    constructor Create;
    destructor Destroy; override;
  end;

constructor TEditorFactory.Create;
begin
  inherited Create;
  FEditors := TInterfaceList.Create;
end;

destructor TEditorFactory.Destroy;
begin
  FreeAndNil(FEditors);
  inherited Destroy;
end;

function TEditorFactory.CanCloseAll: Boolean;
var
  I: Integer;
  LEditor: IEditor;
begin
  // Попытка закрыть все вкладки
  I := FEditors.Count - 1;
  while I >= 0 do
  begin
    LEditor := IEditor(FEditors[I]);
    if not LEditor.AskSaveChanges then
      Exit(False);
    Dec(I);
  end;
  Result := True;
end;

procedure TEditorFactory.CloseAll;
var
  I: Integer;
begin
  // Закрыть все вкладки
  I := FEditors.Count - 1;
  while I >= 0 do
  begin
    IEditor(FEditors[I]).Close;
    Dec(I);
  end;
end;

function TEditorFactory.CreateBorderless(AOwner: TForm): IEditor;
var
  LForm: TfEditor;
begin
  LForm := TfEditor.Create(AOwner);
  with LForm do
  begin
    FEditor := TEditor.Create(LForm);
    Result := FEditor;
    FKind := ekBorderless;
    BorderStyle := bsNone;
    Parent := AOwner;
    Align := alClient;
    Visible := True;
  end;
  if Result <> nil then
    FEditors.Add(Result);
end;

function TEditorFactory.CreateMDIChild(AOwner: TForm): IEditor;
var
  LForm: TfEditor;
begin
  LForm := TfEditor.Create(AOwner);
  with LForm do
  begin
    FEditor := TEditor.Create(LForm);
    Result := FEditor;
    FKind := ekMDIChild;
    FormStyle := fsMDIChild;
  end;
  if Result <> nil then
    FEditors.Add(Result);
end;

function TEditorFactory.CreateTabSheet(AOwner: TPageControl): IEditor;
var
  Sheet: TTabSheet;
  LForm: TfEditor;
begin
  Sheet := TEditorTabSheet.Create(AOwner);
  try
    Sheet.PageControl := AOwner;
    LForm := TfEditor.Create(Sheet);
    with LForm do
    begin
      FEditor := TEditor.Create(LForm);
      Result := FEditor;
      FKind := ekInTabsheet;
      BorderStyle := bsNone;
      Parent := Sheet;
      Align := alClient;
      Visible := True;
      AOwner.ActivePage := Sheet;
      LForm.SetFocus;
    end;
    LForm.Realign;
    if Result <> nil then
      FEditors.Add(Result);
  except
    FreeAndNil(Sheet);
  end;
end;

function TEditorFactory.GetEditorCount: Integer;
begin
  Result := FEditors.Count;
end;

function TEditorFactory.GetEditor(Index: Integer): IEditor;
begin
  Result := IEditor(FEditors[Index]);
end;

procedure TEditorFactory.RemoveEditor(AEditor: IEditor);
var
  I: Integer;
begin
  I := FEditors.IndexOf(AEditor);
  if I > -1 then
    FEditors.Delete(I);
end;

{ TEditorForm }

procedure TfEditor.FormActivate(Sender: TObject);
begin
  DoAssignInterfacePointer(True);
end;

procedure TfEditor.FormDeactivate(Sender: TObject);
begin
  DoAssignInterfacePointer(False);
end;

procedure TfEditor.FormShow(Sender: TObject);
begin
  // Обновить заголовок в окне редактора
  DoUpdateCaption;
end;

procedure TfEditor.FreeKeyWordsList;
begin

end;

procedure TfEditor.InitKeyWordsList;
var
  ValuesList, SectionsList: TStrings;
  Name: string;
  I, J: Integer;
  RootNode: TTreeNode;
  P: PString;
begin
  KeyWordsList.Items.Clear;
  // Открываем файл для работы
  with TMemIniFile.Create(Utils.GetPath('') + 'keywords.ini', TEncoding.UTF8) do
    try
      SectionsList := TStringList.Create;
      try
        // Читаем все группы ключевых слов
        ReadSections(SectionsList);
        ValuesList := TStringList.Create;
        try
          // Перебираем группы
          for I := 0 to SectionsList.Count - 1 do
          begin
            ValuesList.Clear;
            // Загрузка в память всех ключевых слов из тек. группы
            ReadSectionValues(SectionsList[I], ValuesList);
            RootNode := KeyWordsList.Items.Add(nil, _(SectionsList[I]));
            for J := 0 to ValuesList.Count - 1 do
            begin
              // Само ключ. слово ...
              Name := ValuesList.Names[J];
              // ... и его шаблон
              New(P);
              P^ := ValuesList.Values[Name];
              KeyWordsList.Items.AddChildObject(RootNode, Name, P);
            end;
          end;
        finally
          FreeAndNil(ValuesList);
        end;
      finally
        FreeAndNil(SectionsList);
      end;
    finally
      Free;
    end;
end;

function TfEditor.IsLastCharDigit(const S: string): Boolean;
var
  C: Char;
begin
  C := S[S.Length];
  Result := C.IsDigit;
end;

procedure TfEditor.KeyWordsListClick(Sender: TObject);
var
  Word, Hint: string;
  Start, Finish, CaretLeft: Integer;
  Template: string;
const
  Enter = #13#10;
begin
  SynEditor.SetFocus;
  if KeyWordsList.Selected.Level > 0 then
  begin
    CaretLeft := 0;
    Template := '';
    Hint := string(KeyWordsList.Selected.Data^).Trim;
    // Выбранное из группы ключевое слово
    Word := KeyWordsList.Selected.Text.Trim;
    // Начало шаблона (если вообще есть шаблон для кл. слова)
    Start := Pos('{', Hint);
    if Start > 0 then
    begin
      // Конечная позиция шаблона
      Finish := Pos('}', Hint);
      // Обрабатываем шаблон кл. слова
      Template := Copy(Hint, Start + 1, Finish - 2);
      // Удал. лишнее...
      Delete(Hint, Start, Finish);
      // Число указывает, на сколько символов нужно вернуть каретку назад
      CaretLeft := StrToIntDef(Template[Length(Template)], 0);
      // Теперь это число уже не нужно - удаляем его
      if IsLastCharDigit(Template) then
        Delete(Template, Length(Template), 1);
      // Замена спец. символов по шаблону
      Template := Template.Replace('@@', #13#10);
      // Ключ. слово взято из обработ. шаблона
      Word := Template;
    end;
    // Добавляем ключ. слово в позицию курсора
    SynEditor.SelText := Word;
    // Отводим каретку по шаблону назад на N символов,
    // если такая команда была в шаблоне (число больше 0)
    if CaretLeft > 0 then
      SynEditor.CaretX := SynEditor.CaretX - CaretLeft;
  end;
end;

procedure TfEditor.KeyWordsListDeletion(Sender: TObject; Node: TTreeNode);
begin
  // Освобождаем память
  if Node.Data <> nil then
    Dispose(PString(Node.Data));
end;

procedure TfEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FKind = ekInTabsheet then
  begin
    PostMessage(Parent.Handle, WM_DELETETHIS, 0, 0);
    Action := caNone;
  end
  else
    Action := caFree;
end;

procedure TfEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  // Попытка закрыть окно
  if not(csDestroying in ComponentState) then
    CanClose := DoAskSaveChanges;
end;

procedure TfEditor.FormCreate(Sender: TObject);
begin
  // Загружаем список ключевых слов
  InitKeyWordsList;
end;

procedure TfEditor.FormDestroy(Sender: TObject);
var
  LEditor: IEditor;
begin
  FreeKeyWordsList;
  LEditor := FEditor;
  Assert(FEditor <> nil);
  FEditor.FForm := nil;
  Assert(GI_EditorFactory <> nil);
  GI_EditorFactory.RemoveEditor(LEditor);
end;

procedure TfEditor.SynEditorChange(Sender: TObject);
var
  Empty: Boolean;
  I: Integer;
begin
  Assert(FEditor <> nil);
  Empty := True;
  for I := SynEditor.Lines.Count - 1 downto 0 do
    if SynEditor.Lines[I] <> '' then
    begin
      Empty := False;
      Break;
    end;
  FEditor.FIsEmpty := Empty;
end;

procedure TfEditor.SynEditorEnter(Sender: TObject);
begin
  DoAssignInterfacePointer(True);
end;

procedure TfEditor.SynEditorExit(Sender: TObject);
begin
  DoAssignInterfacePointer(False);
end;

procedure TfEditor.SynEditorReplaceText(Sender: TObject; const ASearch, AReplace: string; Line, Column: Integer; var Action: TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else
  begin
    APos := SynEditor.ClientToScreen(SynEditor.RowColumnToPixels(SynEditor.BufferToDisplayPos(BufferCoord(Column, Line))));
    EditRect := ClientRect;
    EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
    EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

    if fConfirmReplace = nil then
      fConfirmReplace := TfConfirmReplace.Create(Application);
    fConfirmReplace.PrepareShow(EditRect, APos.X, APos.Y, APos.Y + SynEditor.LineHeight, ASearch);
    case Utils.ShowForm(fConfirmReplace) of
      mrYes:
        Action := raReplace;
      mrYesToAll:
        Action := raReplaceAll;
      mrNo:
        Action := raSkip;
    else
      Action := raCancel;
    end;
  end;
end;

procedure TfEditor.SynEditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  // Изм. сатус
  Assert(FEditor <> nil);
  if Changes * [scAll, scSelection] <> [] then
    FEditor.FHasSelection := SynEditor.SelAvail;
  if Changes * [scAll, scSelection] <> [] then
    FEditor.FIsReadOnly := SynEditor.ReadOnly;
  if Changes * [scAll, scModified] <> [] then
    FEditor.FModified := SynEditor.Modified;
end;

procedure TfEditor.DoActivate;
var
  TabSheet: TTabSheet;
  PageControl: TPageControl;
begin
  if FormStyle = fsMDIChild then
    BringToFront
  else if Parent is TTabSheet then
  begin
    TabSheet := TTabSheet(Parent);
    PageControl := TabSheet.PageControl;
    if PageControl <> nil then
      PageControl.ActivePage := TabSheet;
  end;
  DoUpdateCaption;
end;

function TfEditor.DoAskSaveChanges: Boolean;
begin
  if SynEditor.Modified then
  begin
    DoActivate;
    MessageBeep(MB_ICONQUESTION);
    Assert(FEditor <> nil);
    case DoConfirmDialog(ExtractFileName(FEditor.GetFileTitle)) of
      crYes:
        Result := DoSave;
      crNo:
        Result := True;
    else
      Result := False;
    end;
  end
  else
    Result := True;
end;

procedure TfEditor.DoAssignInterfacePointer(AActive: Boolean);
begin
  if AActive then
  begin
    GI_ActiveEditor := FEditor;
    GI_EditCmds := FEditor;
    GI_FileCmds := FEditor;
    GI_SearchCmds := FEditor;
  end
  else
  begin
    if GI_ActiveEditor = IEditor(FEditor) then
      GI_ActiveEditor := nil;
    if GI_EditCmds = IEditCommands(FEditor) then
      GI_EditCmds := nil;
    if GI_FileCmds = IFileCommands(FEditor) then
      GI_FileCmds := nil;
    if GI_SearchCmds = ISearchCommands(FEditor) then
      GI_SearchCmds := nil;
  end;
end;

function TfEditor.DoSave: Boolean;
begin
  Assert(FEditor <> nil);
  if FEditor.FFileName <> '' then
    Result := DoSaveFile
  else
    Result := DoSaveAs;
end;

function TfEditor.DoSaveFile: Boolean;
begin
  Assert(FEditor <> nil);
  try
    SynEditor.Lines.SaveToFile(FEditor.FFileName);
    SynEditor.Modified := False;
    Result := True;
  except
    Application.HandleException(Self);
    Result := False;
  end;
end;

function TfEditor.DoSaveAs: Boolean;
var
  NewName: string;
begin
  Assert(FEditor <> nil);
  NewName := FEditor.FFileName;
  if FCommands.GetSaveFileName(NewName, SynEditor.Highlighter) then
  begin
    FEditor.DoSetFileName(NewName);
    DoUpdateCaption;
    DoUpdateHighlighter;
    Result := DoSaveFile;
  end
  else
    Result := False;
end;

procedure TfEditor.DoSearchReplaceText(AReplace: Boolean; ABackwards: Boolean);
var
  Options: TSynSearchOptions;
begin
  if AReplace then
    Options := [ssoPrompt, ssoReplace, ssoReplaceAll]
  else
    Options := [];
  if ABackwards then
    Include(Options, ssoBackwards);
  if FSearchCaseSensitive then
    Include(Options, ssoMatchCase);
  if not FSearchFromCaret then
    Include(Options, ssoEntireScope);
  if FSearchSelectionOnly then
    Include(Options, ssoSelectedOnly);
  if FSearchWholeWords then
    Include(Options, ssoWholeWord);
  if SynEditor.SearchReplace(gsSearchText, gsReplaceText, Options) = 0 then
  begin
    MessageBeep(MB_ICONASTERISK);
    if ssoBackwards in Options then
      SynEditor.BlockEnd := SynEditor.BlockBegin
    else
      SynEditor.BlockBegin := SynEditor.BlockEnd;
    SynEditor.CaretXY := SynEditor.BlockBegin;
  end;

  if fConfirmReplace <> nil then
    FreeAndNil(fConfirmReplace);
end;

procedure TfEditor.DoUpdateCaption;
begin
  // Обновить заголовок программы
  Assert(FEditor <> nil);
  if (FKind = ekInTabsheet) then
    (Parent as TTabSheet).Caption := FEditor.GetFileTitle;
  Application.MainForm.Caption := FEditor.GetFileTitle + ' - ' + 'URQEdit';
end;

procedure TfEditor.DoUpdateHighlighter;
begin
  Assert(FEditor <> nil);
  // Выбираем хайлайтер для расширения файла
  if FEditor.FFileName <> '' then
    SynEditor.Highlighter := FCommands.GetHighlighterForFile(FEditor.FFileName)
  else
    // Текущий урковский хайлайтер по умолчанию
    SynEditor.Highlighter := FCommands.GetHighlighterForFile(FCommands.GetDefaultHighlighter);
end;

procedure TfEditor.ShowSearchReplaceDialog(AReplace: Boolean);
var
  Dialog: TfSearchText;
begin
  if AReplace then
    Dialog := TfReplaceText.Create(Self)
  else
    Dialog := TfSearchText.Create(Self);
  Utils.ShowForm(Dialog, False);
  with Dialog do
    try
      // assign search options
      SearchBackwards := FSearchBackwards;
      SearchCaseSensitive := FSearchCaseSensitive;
      SearchFromCursor := FSearchFromCaret;
      SearchInSelectionOnly := FSearchSelectionOnly;
      // start with last search text
      SearchText := gsSearchText;
      if FSearchTextAtCaret then
      begin
        // if something is selected search for that text
        if SynEditor.SelAvail and (SynEditor.BlockBegin.Line = SynEditor.BlockEnd.Line) then
          SearchText := SynEditor.SelText
        else
          SearchText := SynEditor.GetWordAtRowCol(SynEditor.CaretXY);
      end;
      SearchTextHistory := gsSearchTextHistory;
      if AReplace then
        with Dialog as TfReplaceText do
        begin
          ReplaceText := gsReplaceText;
          ReplaceTextHistory := gsReplaceTextHistory;
        end;
      SearchWholeWords := FSearchWholeWords;
      if ShowModal = mrOK then
      begin
        FSearchBackwards := SearchBackwards;
        FSearchCaseSensitive := SearchCaseSensitive;
        FSearchFromCaret := SearchFromCursor;
        FSearchSelectionOnly := SearchInSelectionOnly;
        FSearchWholeWords := SearchWholeWords;
        gsSearchText := SearchText;
        gsSearchTextHistory := SearchTextHistory;
        if AReplace then
          with Dialog as TfReplaceText do
          begin
            gsReplaceText := ReplaceText;
            gsReplaceTextHistory := ReplaceTextHistory;
          end;
        FSearchFromCaret := FSearchFromCaret;
        if gsSearchText <> '' then
        begin
          DoSearchReplaceText(AReplace, FSearchBackwards);
          FSearchFromCaret := True;
        end;
      end;
    finally
      FreeAndNil(Dialog);
    end;
end;

initialization

GI_EditorFactory := TEditorFactory.Create;

finalization

GI_EditorFactory := nil;

end.
