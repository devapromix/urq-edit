unit uUtils;

interface

uses
  Forms;

type
  Utils = class(TObject)
  public
    class function GetPath(SubDir: string): string;
    class function ShowCenterForm(const AForm: TForm; AShow: Boolean = True): Integer;
  end;

implementation

uses SysUtils;

class function Utils.GetPath(SubDir: string): string;
begin
  Result := ExtractFilePath(ParamStr(0));
  Result := IncludeTrailingPathDelimiter(Result + SubDir);
end;

class function Utils.ShowCenterForm(const AForm: TForm; AShow: Boolean = True): Integer;
begin
  Result := -1;
  AForm.Left := Application.MainForm.Left + ((Application.MainForm.Width div 2) - (AForm.Width div 2));
  AForm.Top := Application.MainForm.Top + ((Application.MainForm.Height div 2) - (AForm.Height div 2));
  if AShow then
    Result := AForm.ShowModal;
end;

end.
