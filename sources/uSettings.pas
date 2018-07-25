unit uSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls;

type
  TfSettings = class(TForm)
    btOK: TBitBtn;
    btCancel: TBitBtn;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label4: TLabel;
    btSelURQ: TSpeedButton;
    edSelURQ: TEdit;
    OpenDialog: TOpenDialog;
    procedure btSelURQClick(Sender: TObject);
    procedure btOKClick(Sender: TObject);
    procedure btCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

uses uCommon, uUtils, IniFiles;

procedure TfSettings.btCancelClick(Sender: TObject);
begin
  LoadConfig;
end;

procedure TfSettings.btOKClick(Sender: TObject);
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
