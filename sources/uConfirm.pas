unit uConfirm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TConfirmDialog = class(TForm)
    Image1: TImage;
    btnReplace: TButton;
    btnSkip: TButton;
    btnCancel: TButton;
    lblConfirmation: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  TConfirmResult = (crCancel, crYes, crNo);

function DoConfirmDialog(const FileName: string): TConfirmResult;

var
  ConfirmDialog: TConfirmDialog;

implementation

{$R *.dfm}

uses uLanguage, uUtils;

function DoConfirmDialog(const FileName: string): TConfirmResult;
begin
  Result := crCancel;
  with ConfirmDialog do
  begin
    lblConfirmation.Caption :=
      Format(_('The text in the "%s" file has changed. Do you want to save the modifications?'),
      [FileName]);
    Utils.ShowForm(ConfirmDialog);
    case ModalResult of
      mrYes:
        Result := crYes;
      mrNo:
        Result := crNo;
    end;
  end;
end;

procedure TConfirmDialog.FormCreate(Sender: TObject);
begin
  Image1.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
end;

procedure TConfirmDialog.FormShow(Sender: TObject);
begin
  Caption := _('Confirm');
  btnReplace.Caption := _('&Yes');
  btnSkip.Caption := _('&No');
  btnCancel.Caption := _('&Cancel');
end;

end.
