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
    btnDefault: TBitBtn;
    procedure btSelURQClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
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

procedure TfSettings.btnCancelClick(Sender: TObject);
begin
  LoadConfig;
end;

procedure TfSettings.btnDefaultClick(Sender: TObject);
begin
  // Дефолтные настройки
  edSelURQ.Text := '';
end;

procedure TfSettings.btnOKClick(Sender: TObject);
begin
  SaveConfig;
end;

procedure TfSettings.btSelURQClick(Sender: TObject);
begin
  // Выбор интерпретатора для запуска квестов
  OpenDialog.Filter := _('Interpretator URQ|*.exe');
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
  btnDefault.Caption := _('Default');
  TabSheet1.Caption := _('URQ');
  lblURQInt.Caption := _('URQ path:');
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
