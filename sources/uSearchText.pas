unit uSearchText;

{$I SynEdit.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TfSearchText = class(TForm)
    Label1: TLabel;
    cbSearchText: TComboBox;
    rgSearchDirection: TRadioGroup;
    gbSearchOptions: TGroupBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchFromCursor: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    btnOK: TButton;
    btnCancel: TButton;
    cbRegularExpression: TCheckBox;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    function GetSearchBackwards: Boolean;
    function GetSearchCaseSensitive: Boolean;
    function GetSearchFromCursor: Boolean;
    function GetSearchInSelection: Boolean;
    function GetSearchText: string;
    function GetSearchTextHistory: string;
    function GetSearchWholeWords: Boolean;
    procedure SetSearchBackwards(Value: Boolean);
    procedure SetSearchCaseSensitive(Value: Boolean);
    procedure SetSearchFromCursor(Value: Boolean);
    procedure SetSearchInSelection(Value: Boolean);
    procedure SetSearchText(Value: string);
    procedure SetSearchTextHistory(Value: string);
    procedure SetSearchWholeWords(Value: Boolean);
    procedure SetSearchRegularExpression(const Value: Boolean);
    function GetSearchRegularExpression: Boolean;
  public
    property SearchBackwards: Boolean read GetSearchBackwards write SetSearchBackwards;
    property SearchCaseSensitive: Boolean read GetSearchCaseSensitive write SetSearchCaseSensitive;
    property SearchFromCursor: Boolean read GetSearchFromCursor write SetSearchFromCursor;
    property SearchInSelectionOnly: Boolean read GetSearchInSelection write SetSearchInSelection;
    property SearchText: string read GetSearchText write SetSearchText;
    property SearchTextHistory: string read GetSearchTextHistory write SetSearchTextHistory;
    property SearchWholeWords: Boolean read GetSearchWholeWords write SetSearchWholeWords;
    property SearchRegularExpression: Boolean read GetSearchRegularExpression write SetSearchRegularExpression;
  end;

implementation

{$R *.DFM}

uses uLanguage;

{ TTextSearchDialog }

procedure TfSearchText.FormShow(Sender: TObject);
begin
  Caption := _('Search Text');
  Label1.Caption := _('&Search for:');
  rgSearchDirection.Caption := _('Direction');
  rgSearchDirection.Items.Strings[0] := _('&Forward');
  rgSearchDirection.Items.Strings[1] := _('&Backward');
  gbSearchOptions.Caption := _('Options');
  cbSearchCaseSensitive.Caption := _('C&ase sensitivity');
  cbSearchWholeWords.Caption := _('&Whole words only');
  cbSearchFromCursor.Caption := _('Search from &caret');
  cbSearchSelectedOnly.Caption := _('&Selected text only');
  cbRegularExpression.Caption := _('&Regular expression');
  btnOK.Caption := _('OK');
  btnCancel.Caption := _('Cancel');
end;

function TfSearchText.GetSearchBackwards: Boolean;
begin
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TfSearchText.GetSearchCaseSensitive: Boolean;
begin
  Result := cbSearchCaseSensitive.Checked;
end;

function TfSearchText.GetSearchFromCursor: Boolean;
begin
  Result := cbSearchFromCursor.Checked;
end;

function TfSearchText.GetSearchInSelection: Boolean;
begin
  Result := cbSearchSelectedOnly.Checked;
end;

function TfSearchText.GetSearchRegularExpression: Boolean;
begin
  Result := cbRegularExpression.Checked;
end;

function TfSearchText.GetSearchText: string;
begin
  Result := cbSearchText.Text;
end;

function TfSearchText.GetSearchTextHistory: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to cbSearchText.Items.Count - 1 do
  begin
    if I >= 10 then
      break;
    if I > 0 then
      Result := Result + #13#10;
    Result := Result + cbSearchText.Items[I];
  end;
end;

function TfSearchText.GetSearchWholeWords: Boolean;
begin
  Result := cbSearchWholeWords.Checked;
end;

procedure TfSearchText.SetSearchBackwards(Value: Boolean);
begin
  rgSearchDirection.ItemIndex := Ord(Value);
end;

procedure TfSearchText.SetSearchCaseSensitive(Value: Boolean);
begin
  cbSearchCaseSensitive.Checked := Value;
end;

procedure TfSearchText.SetSearchFromCursor(Value: Boolean);
begin
  cbSearchFromCursor.Checked := Value;
end;

procedure TfSearchText.SetSearchInSelection(Value: Boolean);
begin
  cbSearchSelectedOnly.Checked := Value;
end;

procedure TfSearchText.SetSearchText(Value: string);
begin
  cbSearchText.Text := Value;
end;

procedure TfSearchText.SetSearchTextHistory(Value: string);
begin
  cbSearchText.Items.Text := Value;
end;

procedure TfSearchText.SetSearchWholeWords(Value: Boolean);
begin
  cbSearchWholeWords.Checked := Value;
end;

procedure TfSearchText.SetSearchRegularExpression(const Value: Boolean);
begin
  cbRegularExpression.Checked := Value;
end;

procedure TfSearchText.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  S: string;
  I: Integer;
begin
  if ModalResult = mrOK then
  begin
    S := cbSearchText.Text;
    if S <> '' then
    begin
      I := cbSearchText.Items.IndexOf(S);
      if I > -1 then
      begin
        cbSearchText.Items.Delete(I);
        cbSearchText.Items.Insert(0, S);
        cbSearchText.Text := S;
      end
      else
        cbSearchText.Items.Insert(0, S);
    end;
  end;
end;

end.
