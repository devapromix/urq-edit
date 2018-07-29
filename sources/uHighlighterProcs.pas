unit uHighlighterProcs;

interface

uses
  Classes, SynEditHighlighter;

procedure GetHighlighters(AOwner: TComponent; AHighlighters: TStringList;
  AppendToList: Boolean);
function GetHighlightersFilter(AHighlighters: TStringList): string;
function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;

implementation

uses
  SysUtils, uLanguage;

procedure GetHighlighters(AOwner: TComponent; AHighlighters: TStringList;
  AppendToList: Boolean);
var
  I: Integer;
  Highlighter: TSynCustomHighlighter;
begin
  if Assigned(AOwner) and Assigned(AHighlighters) then
  begin
    if not AppendToList then
      AHighlighters.Clear;
    for I := AOwner.ComponentCount - 1 downto 0 do
    begin
      if not(AOwner.Components[I] is TSynCustomHighlighter) then
        Continue;
      Highlighter := AOwner.Components[I] as TSynCustomHighlighter;
      // only one highlighter for each language
      if AHighlighters.IndexOf(Highlighter.GetLanguageName) = -1 then
        AHighlighters.AddObject(Highlighter.GetLanguageName, Highlighter);
    end;
    AHighlighters.Sort;
  end;
end;

function GetHighlightersFilter(AHighlighters: TStringList): string;
var
  I: Integer;
  Highlighter: TSynCustomHighlighter;
begin
  Result := '';
  if Assigned(AHighlighters) then
    for I := 0 to AHighlighters.Count - 1 do
    begin
      if not(AHighlighters.Objects[I] is TSynCustomHighlighter) then
        Continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[I]);
      if Highlighter.DefaultFilter = '' then
        Continue;
      Result := Result + _(Highlighter.DefaultFilter);
      if Result[Length(Result)] <> '|' then
        Result := Result + '|';
    end;
end;

function GetHighlighterFromFileExt(AHighlighters: TStringList;
  Extension: string): TSynCustomHighlighter;
var
  ExtLen: Integer;
  I, J: Integer;
  Highlighter: TSynCustomHighlighter;
  Filter: string;
begin
  Extension := LowerCase(Extension);
  ExtLen := Length(Extension);
  if Assigned(AHighlighters) and (ExtLen > 0) then
  begin
    for I := 0 to AHighlighters.Count - 1 do
    begin
      if not(AHighlighters.Objects[I] is TSynCustomHighlighter) then
        Continue;
      Highlighter := TSynCustomHighlighter(AHighlighters.Objects[I]);
      Filter := LowerCase(Highlighter.DefaultFilter);
      J := Pos('|', Filter);
      if J > 0 then
      begin
        Delete(Filter, 1, J);
        J := Pos(Extension, Filter);
        if (J > 0) and ((J + ExtLen > Length(Filter)) or
          (Filter[J + ExtLen] = ';')) then
        begin
          Result := Highlighter;
          exit;
        end;
      end;
    end;
  end;
  Result := nil;
end;

end.
