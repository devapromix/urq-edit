unit SynHighlighterURQ;

{$I SynEdit.inc}

interface

uses
  SysUtils,
  Classes,
{$IFDEF SYN_CLX}
  QControls,
  QGraphics,
{$ELSE}
  Windows,
  Controls,
  Graphics,
{$ENDIF}
  SynEditTypes,
  SynEditHighlighter;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkSpace, tkLabel, tkString, tkUnknown);
  TRangeState = (rsUnKnown, rsBraceComment, rsCStyleComment, rsString);
  TProcTableProc = procedure of object;
  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 161;

type
  TSynURQSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    fRange: TRangeState;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0 .. MaxKey] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure BraceCommentOpenProc;
    procedure BraceCommentProc;
    procedure CommentProc;
    procedure LabelProc;
    procedure StringOpenProc;
    procedure StringProc;
    // KeyWords
    function Func15: TtkTokenKind;
    function Func16: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func89: TtkTokenKind;
    //
  protected
    //function GetIdentChars: TSynIdentChars; override;
    //function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEOL: Boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: string; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
  end;
  
const
  SYNS_LangURQlanguage = 'URQL';

var
  SYNS_Filter_URQLanguage: string = 'URQ files (*.qst)|*.qst';

implementation

uses
  SynEditStrConst, untLang;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynURQSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
  begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[15]  := Func15;
  fIdentFuncTable[16]  := Func16;
  fIdentFuncTable[19]  := Func19;
  fIdentFuncTable[23]  := Func23;
  fIdentFuncTable[24]  := Func24;
  fIdentFuncTable[33]  := Func33;
  fIdentFuncTable[34]  := Func34;
  fIdentFuncTable[36]  := Func36;
  fIdentFuncTable[41]  := Func41;
  fIdentFuncTable[42]  := Func42;
  fIdentFuncTable[47]  := Func47;
  fIdentFuncTable[49]  := Func49;
  fIdentFuncTable[52]  := Func52;
  fIdentFuncTable[54]  := Func54;
  fIdentFuncTable[57]  := Func57;
  fIdentFuncTable[62]  := Func62;
  fIdentFuncTable[65]  := Func65;
  fIdentFuncTable[67]  := Func67;
  fIdentFuncTable[68]  := Func68;
  fIdentFuncTable[80]  := Func80;
  fIdentFuncTable[83]  := Func83;
  fIdentFuncTable[89]  := Func89;
end;
//----------------------------------------------------------//
function TSynURQSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func16: TtkTokenKind;
begin
  if KeyComp('p') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func19: TtkTokenKind;
begin
  if KeyComp('and') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func23: TtkTokenKind;
begin
  if KeyComp('end') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func24: TtkTokenKind;
begin
  if KeyComp('inv+') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func33: TtkTokenKind;
begin
  if KeyComp('or') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func34: TtkTokenKind;
begin
  if KeyComp('cls') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func36: TtkTokenKind;
begin
  if KeyComp('btn') or KeyComp('clsb') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func41: TtkTokenKind;
begin
  if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func42: TtkTokenKind;
begin
  if KeyComp('pln') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func47: TtkTokenKind;
begin
  if KeyComp('then') then Result := tkKey
  else if KeyComp('save') then Result := tkKey
  else Result := tkIdentifier;
end;

function TSynURQSyn.Func49: TtkTokenKind;
begin
  if KeyComp('not') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func52: TtkTokenKind;
begin
  if KeyComp('proc') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func54: TtkTokenKind;
begin
  if KeyComp('play') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func57: TtkTokenKind;
begin
  if KeyComp('goto') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func62: TtkTokenKind;
begin
  if KeyComp('pause') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func65: TtkTokenKind;
begin
  if KeyComp('music') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func67: TtkTokenKind;
begin
  if KeyComp('quit') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func68: TtkTokenKind;
begin
  if KeyComp('include') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func80: TtkTokenKind;
begin
  if KeyComp('input') or KeyComp('instr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func83: TtkTokenKind;
begin
  if KeyComp('perkill') then Result := tkKey else Result := tkIdentifier;
end;

function TSynURQSyn.Func89: TtkTokenKind;
begin
  if KeyComp('invkill') then Result := tkKey else Result := tkIdentifier;
end;

//----------------------------------------------------------//
function TSynURQSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '+', '-', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynURQSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end
  else
    Result := False;
end;

function TSynURQSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynURQSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynURQSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      ';': fProcTable[I] := CommentProc;
      ':': fProcTable[I] := LabelProc;
      '#': fProcTable[I] := StringOpenProc;
      #1..#9,
      #11,
      #12,
      #14..#32 : fProcTable[I] := SpaceProc;
      'A'..'Z', 'a'..'z', '_', '+', '-': fProcTable[I] := IdentProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynURQSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end;

procedure TSynURQSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynURQSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynURQSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynURQSyn.BraceCommentOpenProc;
begin
  Inc(Run);
  fRange := rsBraceComment;
  BraceCommentProc;
  fTokenID := tkComment;
end;

procedure TSynURQSyn.BraceCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '}') then
        begin
          Inc(Run, 1);
          fRange := rsUnKnown;
          Break;
        end;
        if not (fLine[Run] in [#0, #10, #13]) then Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynURQSyn.LabelProc;
begin
  fTokenID := tkLabel;
  repeat
    if (fLine[Run] = ':')
    and ((fLine[Run + 1] = #32)
    or (fLine[Run + 2] = '/')) then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynURQSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    case fLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until fLine[Run] = #0;
end;

procedure TSynURQSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '$') then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynURQSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

constructor TSynURQSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clBlack;
  AddAttribute(fKeyAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Style := [fsBold];
  fStringAttri.Foreground := clGreen;
  AddAttribute(fStringAttri);

{  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clGreen;
  fStringAttri.Style := [fsBold];
  AddAttribute(fStringAttri);

  fTestAttri := TSynHighLighterAttributes.Create(SYNS_AttrTest);
  fTestAttri.Style := [fsUnderline, fsItalic];
  fTestAttri.Foreground := clBlue;
  fTestAttri.Background := clSilver;
  AddAttribute(fTestAttri);}

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  SYNS_Filter_URQlanguage := Lang.Get(112);
  fDefaultFilter := SYNS_Filter_URQlanguage;
  fRange := rsUnknown;
end;

procedure TSynURQSyn.SetLine(NewValue: string; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynURQSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end;

procedure TSynURQSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynURQSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsBraceComment: BraceCommentProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynURQSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynURQSyn.GetEOL: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynURQSyn.GetKeyWords: string;
begin
  Result := '';
end;

function TSynURQSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynURQSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynURQSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkLabel: Result := fKeyAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynURQSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynURQSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynURQSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '+', '-', 'a'..'z', 'A'..'Z'];
end;

function TSynURQSyn.GetSampleSource: string;
begin
  Result := '';
end;

function TSynURQSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_Filter_URQlanguage;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynURQSyn.GetLanguageName: string;
begin
  Result := SYNS_LangURQlanguage;
end;

procedure TSynURQSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynURQSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynURQSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynURQSyn);
{$ENDIF}
end.
