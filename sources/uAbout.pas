unit uAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TAboutDialog = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutDialog: TAboutDialog;

implementation

{$R *.dfm}

uses uLanguage;

procedure TAboutDialog.FormShow(Sender: TObject);
begin
  Caption := _('About...');
end;

end.
