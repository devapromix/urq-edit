unit uConfirmReplace;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfConfirmReplace = class(TForm)
    btnReplace: TButton;
    lblConfirmation: TLabel;
    btnSkip: TButton;
    btnCancel: TButton;
    btnReplaceAll: TButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure PrepareShow(AEditorRect: TRect; X, Y1, Y2: Integer; AReplaceText: string);
  end;

var
  fConfirmReplace: TfConfirmReplace;

implementation

{$R *.DFM}

uses uLanguage;

{ TConfirmReplaceDialog }

procedure TfConfirmReplace.FormCreate(Sender: TObject);
begin
  Image1.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
end;

procedure TfConfirmReplace.FormDestroy(Sender: TObject);
begin
  fConfirmReplace := nil;
end;

procedure TfConfirmReplace.FormShow(Sender: TObject);
begin
  Caption := _('Confirm replace');
  btnReplace.Caption := _('&Yes');
  btnSkip.Caption := _('&No');
  btnCancel.Caption := _('&Cancel');
  btnReplaceAll.Caption := _('Yes to &all');
end;

procedure TfConfirmReplace.PrepareShow(AEditorRect: TRect; X, Y1, Y2: Integer; AReplaceText: string);
var
  NW, NH: Integer;
begin
  lblConfirmation.Caption := Format(_('Replace this occurence of "%s"?'), [AReplaceText]);
  NW := AEditorRect.Right - AEditorRect.Left;
  NH := AEditorRect.Bottom - AEditorRect.Top;

  if NW <= Width then
    X := AEditorRect.Left - (Width - NW) div 2
  else
  begin
    if X + Width > AEditorRect.Right then
      X := AEditorRect.Right - Width;
  end;
  if Y2 > AEditorRect.Top + MulDiv(NH, 2, 3) then
    Y2 := Y1 - Height - 4
  else
    Inc(Y2, 4);
  SetBounds(X, Y2, Width, Height);
end;

end.
