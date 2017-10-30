program URQEdit;

uses
  Forms,
  uMain in 'uMain.pas' {MainForm},
  uMainWorkbook in 'uMainWorkbook.pas' {WorkbookMainForm},
  uEditAppIntfs in 'uEditAppIntfs.pas',
  uEditor in 'uEditor.pas' {EditorForm},
  uCommands in 'uCommands.pas' {CommandsDataModule: TDataModule},
  uHighlighterProcs in 'uHighlighterProcs.pas',
  uSearchText in 'uSearchText.pas' {TextSearchDialog},
  uReplaceText in 'uReplaceText.pas' {TextReplaceDialog},
  uConfirmReplace in 'uConfirmReplace.pas' {ConfirmReplaceDialog},
  GNUGetText in 'GNUGetText.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'URQEdit';
  Application.CreateForm(TWorkbookMainForm, WorkbookMainForm);
  Application.Run;
end.

