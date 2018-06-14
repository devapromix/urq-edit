program URQEdit;

uses
  Forms,
  uMain in 'uMain.pas' {MainForm} ,
  uMainWorkbook in 'uMainWorkbook.pas' {WorkbookMainForm} ,
  uEditAppIntfs in 'uEditAppIntfs.pas',
  uEditor in 'uEditor.pas' {EditorForm} ,
  uCommands in 'uCommands.pas' {CommandsDataModule: TDataModule} ,
  uHighlighterProcs in 'uHighlighterProcs.pas',
  uSearchText in 'uSearchText.pas' {TextSearchDialog} ,
  uReplaceText in 'uReplaceText.pas' {TextReplaceDialog} ,
  uConfirmReplace in 'uConfirmReplace.pas' {ConfirmReplaceDialog} ,
  uLanguage in 'uLanguage.pas',
  uUtils in 'uUtils.pas',
  uConfirm in 'uConfirm.pas' {ConfirmDialog} ,
  uAbout in 'uAbout.pas' {AboutDialog};

{$R *.RES}

begin
  Randomize();
{$IFNDEF FPC}
{$IF COMPILERVERSION >= 18}
  ReportMemoryLeaksOnShutdown := True;
{$IFEND}
{$ENDIF}
  Application.Initialize;
  Application.Title := 'URQEdit';
  Application.CreateForm(TWorkbookMainForm, WorkbookMainForm);
  Application.CreateForm(TConfirmDialog, ConfirmDialog);
  Application.CreateForm(TAboutDialog, AboutDialog);
  Application.Run;

end.
