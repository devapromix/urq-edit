unit uEditAppIntfs;

{$I SynEdit.inc}

interface

uses
  Windows, Classes, Forms, ComCtrls;

type
  IEditor = interface
    procedure Activate;
    function AskSaveChanges: Boolean;
    function CanClose: Boolean;
    procedure Close;
    function GetCaretPos: TPoint;
    function GetEditorState: string;
    function GetFileName: string;
    function GetFileTitle: string;
    function GetModified: Boolean;
    procedure OpenFile(AFileName: string);
  end;

  IEditorFactory = interface
    function CanCloseAll: Boolean;
    procedure CloseAll;
    function CreateBorderless(AOwner: TForm): IEditor;
    function CreateMDIChild(AOwner: TForm): IEditor;
    function CreateTabSheet(AOwner: TPageControl): IEditor;
    function GetEditorCount: Integer;
    function GetEditor(Index: Integer): IEditor;
    procedure RemoveEditor(AEditor: IEditor);
    property Editor[Index: Integer]: IEditor read GetEditor;
  end;

  IEditCommands = interface
    function CanCopy: Boolean;
    function CanCut: Boolean;
    function CanDelete: Boolean;
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
  end;

  IFileCommands = interface
    function CanClose: Boolean;
    function CanPrint: Boolean;
    function CanSave: Boolean;
    function CanSaveAs: Boolean;
    procedure ExecClose;
    procedure ExecPrint;
    procedure ExecSave;
    procedure ExecSaveAs;
  end;

  ISearchCommands = interface
    function CanFind: Boolean;
    function CanFindNext: Boolean;
    function CanFindPrev: Boolean;
    function CanReplace: Boolean;
    procedure ExecFind;
    procedure ExecFindNext;
    procedure ExecFindPrev;
    procedure ExecReplace;
  end;

var
  GI_EditorFactory: IEditorFactory;

  GI_ActiveEditor: IEditor;

  GI_EditCmds: IEditCommands;
  GI_FileCmds: IFileCommands;
  GI_SearchCmds: ISearchCommands;

implementation

end.
