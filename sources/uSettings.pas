unit uSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls;

type
  TfSettings = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    lblURQInt: TLabel;
    btSelURQ: TSpeedButton;
    edSelURQ: TEdit;
    OpenDialog: TOpenDialog;
    procedure btSelURQClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LoadConfig;
    procedure SaveConfig;
  end;

var
  fSettings: TfSettings;

implementation

{$R *.dfm}

uses uUtils, IniFiles, uLanguage;

const
  stURQIntFilters: string = 'Интерпретатор URQ|*.exe';

procedure TfSettings.btnCancelClick(Sender: TObject);
begin
  LoadConfig;
end;

procedure TfSettings.btnOKClick(Sender: TObject);
begin
  SaveConfig;
end;

procedure TfSettings.btSelURQClick(Sender: TObject);
begin
  // Выбор интерпретатора для запуска квестов
  OpenDialog.Filter := stURQIntFilters;
  if OpenDialog.Execute then
    edSelURQ.Text := Trim(OpenDialog.FileName);
end;

procedure TfSettings.FormCreate(Sender: TObject);
begin
  LoadConfig;
end;

procedure TfSettings.FormShow(Sender: TObject);
begin
  Caption := _('Settings');
  btnOK.Caption := _('OK');
  btnCancel.Caption := _('Cancel');
  TabSheet1.Caption := _('URQ');
  lblURQInt.Caption := _('Cancel');
end;

procedure TfSettings.LoadConfig;
var
  F: TIniFile;
begin
  F := TIniFile.Create(Utils.GetPath('') + 'config.ini');
  try
    // Интерпретатор
    edSelURQ.Text := Trim(F.ReadString('Main', 'URQInt', ''));
  finally
    FreeAndNil(F);
  end;
end;

procedure TfSettings.SaveConfig;
var
  F: TIniFile;
begin
  F := TIniFile.Create(Utils.GetPath('') + 'config.ini');
  try
    // Интерпретатор
    F.WriteString('Main', 'URQInt', Trim(edSelURQ.Text));
  finally
    FreeAndNil(F);
  end;
end;

end.
