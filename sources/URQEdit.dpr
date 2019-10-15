program URQEdit;

uses
  Forms,
  uMain in 'uMain.pas' {fMain},
  uMainWorkbook in 'uMainWorkbook.pas' {fMainWorkbook},
  uEditAppIntfs in 'uEditAppIntfs.pas',
  uEditor in 'uEditor.pas' {fEditor},
  uCommands in 'uCommands.pas' {fCommands: TDataModule},
  uHighlighterProcs in 'uHighlighterProcs.pas',
  uSearchText in 'uSearchText.pas' {fSearchText},
  uReplaceText in 'uReplaceText.pas' {fReplaceText},
  uConfirmReplace in 'uConfirmReplace.pas' {fConfirmReplace},
  uLanguage in 'uLanguage.pas',
  uUtils in 'uUtils.pas',
  uConfirm in 'uConfirm.pas' {fConfirm},
  uAbout in 'uAbout.pas' {fAbout},
  uSettings in 'uSettings.pas' {fSettings};

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
  Application.CreateForm(TfMainWorkbook, fMainWorkbook);
  Application.CreateForm(TfConfirm, fConfirm);
  Application.CreateForm(TfAbout, fAbout);
  Application.CreateForm(TfSettings, fSettings);
  Application.CreateForm(TfSettings, fSettings);
  Application.Run;

end.
