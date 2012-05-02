unit Template;

(* ***************************************************************************
  Template.pas - template expander for TZipResMaker for ZipMaster
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.

	This file is part of TZipMaster Version 1.9.1.x

All rights reserved.
For the purposes of Copyright and this license "DelphiZip" is the current
 authors, maintainers and developers of its code:
  Russell Peters and Roger Aelbrecht.

Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
* DelphiZip reserves the names "DelphiZip", "ZipMaster", "ZipBuilder",
   "DelZip" and derivatives of those names for the use in or about this
   code and neither those names nor the names of its authors or
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL DELPHIZIP, IT'S AUTHORS OR CONTRIBUTERS BE
 LIABLE FOR ANYDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

contact: problems AT delphizip DOT org
updates: http://www.delphizip.org
 *************************************************************************** *)
//modified 2012-04-03
{$I '..\..\ZipVers.inc'}
(*
    Line Syntax -    (...) are optional or additional args
      {KEY(repeat)(operator)( following only where marked ** )}
      {symbol(args)}
    repeat    (must immediately follow KEY)
      *     - append count
      +     - append count and increment, trigger repeat line while defined
    operator  (must immediately follow KEY or repeat)
      .     - KEY is Uppercase extension
      $     - KEY without extension
      none  - use KEY if defined
      =txt  - compare to txt
      <txt  - compare to txt
      <=txt - compare to txt
      >txt  - compare to txt
      >=txt - compare to txt
      <>txt - compare to txt
      space - use KEY if defined else use following    **
      @     - use following if KEY defined             **
      !     - use following if KEY not defined         **

    symbol
      ^     - tab (default 8)
        ^=n   - set tab to n (0, 2, 4, 8)
      %     - crlf
      ?     - conditional crlf - break line > 69
        ?\    - conditional append \ on crlf
        ?^    - conditional use tab after crlf or a space if no break
        ?\^   - both
      @     - the previous symbol
      *     - gives count
        *+n   - gives count + n
        *=n   - sets count
      [     - gives [
      ]     - gives ]
      #     - rem
      ~     - else
      +     - repeat
      <=b   - start of section - b = TRUE or FALSE
      >     - end of section
      |     - 'else' for section
      ,     - continued next line
      =     - define macro  {=}ident=txt to eol
      =     - include macro  {=ident=}

    info is of the form  KEYWORD=text
    predefined KEYWORDS
      PROJECTVER - 1900000
      STAMP YEAR AUTHOR
      FILENAME  - name of destination file
      NAME      - name of destination file without extension
      LICENCE
      VERSION   - 1.9.0.0000
      VERSION_C - 1,9,0,0000
      GUID
      MAJOR MINOR RELEASE BUILDZ - 1 9 0 0000
      BUILD     - build number
      PROJ      - 190
*)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes;

type
  TKeySymbols = (ksElse, ksCntr, ksRep, ksRem, ksPrev, ksCond, ksCR,
    ksTab, ksOpen, ksClose, ksNone);
  TKeyDefines = (dkPROJECTVER, dkSTAMP, dkDATE, dkTIME, dkYEAR, dkAUTHOR, dkFILENAME, dkNAME,
    dkLICENCE, dkVERSION, dkVERSION_C, dkGUID, dkPROJECT, dkMAJOR, dkMINOR,
    dkRELEASE, dkBUILDZ, dkBUILD, dkPROJ, dkNone);
  TConds      = (ckNone, ckEqual, ckGreater, ckLess, ckNEqual, ckGEqual, ckLEqual);

type
  TScanFlags = (sfIgnore, sfFalse, sfIf);
  TScanFlag  = set of TScanFlags;
  TCharSet   = set of AnsiChar;

const
  Opener = '{';
  Closer = '}';

type
  TTemplater         = class;
  TTemplateShowEvent = procedure(Sender: TTemplater; const msg: String) of object;

  TTemplater = class
  private
    fBusy:        Boolean;
    fCurrentLine: String;
    fDefines:     TStrings;
    fDest:        TStrings;
    fDidExpand:   Boolean;
    fErrCount:    Integer;
    fError:       String;
    fFilename:    String;
    fIgnoreLine:  Boolean;
    fLine:        Integer;
    fOnShowMsg:   TTemplateShowEvent;
    fPosition:    Integer;
    fRepCount:    Integer;
    fRepeating:   Boolean;
    fSource:      TStringList;
    fSubstLine:   String;
    FTabSize: Integer;
    fTest:        Boolean;
    FUseTabs: Boolean;
    fVersion:     Integer;
    InRegion:     Boolean;
    PreLevel:     Integer;
    PreLevels:    array of Integer;
    PreUse:       Integer;
    function AtKey(var key: String): TKeyDefines;
    function GetAtChar(ofs: Integer): Char;
    function GetAtEOS(ofs: Integer): Boolean;
    function GetCurrent(Index: Integer): Char;
    function GetEOS(posn: Integer): Boolean;
    function GetIsClose(posn: Integer): Boolean;
    function GetPredefined(kind: TKeyDefines): String;
    procedure SetDefines(const Value: TStrings);
    procedure SetDest(const Value: TStrings);
    procedure SetTabSize(const Value: Integer);
  protected
    fMacros: TStringList;
    procedure Add(const txt: String);
    procedure AddToDest(const txt: String);
    procedure Advance(ofs: Integer = 1);
    function Append(const txt: String): String;
    function AtClose(ofs: Integer = 0): Boolean;
    function AtCondition: TConds;
    function AtConstant(var key, Value: String): Char;
    function AtCopy(len: Integer; ofs: Integer = 0): String;
    function AtCountToNext: Integer;
    function AtMatch(const txt: String): Boolean; overload;
    function AtMatch(const Vals: array of String): Integer; overload;
    function AtNumber: Integer;
    function AtString(Allow: TSysCharSet): String;
    procedure CheckAtClose(ofs: Integer = 0);
    procedure CheckAtEOS;
    function DoComp(const Value: String): string;
    function DoDefine(const Keyword: String; var flags: TScanFlag): String;
    function DoKey(const Keyword: String; var flags: TScanFlag): String;
    procedure DoLine;
    function DoLoad(const template: String): Integer;
    function DoProcess: Integer;
    function DoStatement(const KeyWord: String; var flag: TScanFlag): String;
    function DoSymbol(const Keyword: String; var flags: TScanFlag): String;
    procedure Err(const Err, Key: String);
    procedure ExpandMacros;
    function GetLine(var LineNo: Integer): boolean;
    function IsBlanks(const txt: String): Boolean;
    function IsKeyDefines(const txt: String): TKeyDefines;
    function IsKeySymbol(k: Char): TKeySymbols;
    function IsSkip(flags: TScanFlag): Boolean;
    procedure Licence;
    function NextOpen: Integer;
    function PopLevel: Integer;
    function PreProcess: Boolean;
    function PushLevel: Integer;
    function SafeChar(c: Char): Char;
    function TabbedLength(const s: String): Integer;
    function Year(dt: TDateTime): Integer;
    property AtChar[ofs: Integer]: Char Read GetAtChar;
    property Current[Index: Integer]: Char Read GetCurrent;
    property CurrentLine: String Read fCurrentLine Write fCurrentLine;
    property Line: Integer Read fLine Write fLine;
    property Position: Integer Read fPosition Write fPosition;
    property Predefined[kind: TKeyDefines]: String Read GetPredefined;
    property Source: TStringList Read fSource;
    property SubstLine: String Read fSubstLine Write fSubstLine;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear;
    function Process(const template: String): Integer;
    property AtEOS[ofs: Integer]: Boolean Read GetAtEOS;
    property Defines: TStrings Read fDefines Write SetDefines;
    property Dest: TStrings Read fDest Write SetDest;
    property EOS[posn: Integer]: Boolean Read GetEOS;
    property ErrCount: Integer Read fErrCount;
    property Error: String Read fError Write fError;
    property Filename: String Read fFilename Write fFilename;
    property IsClose[posn: Integer]: Boolean Read GetIsClose;
    property TabSize: Integer read FTabSize write SetTabSize;
    property Test: Boolean Read fTest Write fTest;
    property UseTabs: Boolean read FUseTabs write FUseTabs;
    property Version: Integer Read fVersion Write fVersion;
    property OnShowMsg: TTemplateShowEvent Read fOnShowMsg Write fOnShowMsg;
  end;


implementation

uses
  ZMUtils, StrUtils;

const
  KeyChars = ['_', 'A'..'Z', '0'..'9'];
  KeyDefines: array [TKeyDefines] of String = (
    'PROJECTVER', 'STAMP', 'DATE', 'TIME', 'YEAR', 'AUTHOR', 'FILENAME', 'NAME',
    'LICENCE', 'VERSION', 'VERSION_C', 'GUID', 'PROJECT', 'MAJOR', 'MINOR', 'RELEASE',
    'BUILDZ', 'BUILD', 'PROJ', '');

  //  TKeySymbols = (ksElse, ksCntr, ksRep, ksRem, ksPrev, ksCond, ksCR,
  //    ksTab, ksOpen, ksClose, ksNone);
  KeySymbols: array [TKeySymbols] of Char =
    ('~', '*', '+', '#', '@', '?', '%', '^', Opener, Closer, #0);

type
  ETemplater = class(Exception)
  private
    fErr: String;
    fKey: String;
  public
    constructor Create(const msg: String; const what: String = '');
    property Err: String Read fErr;
    property Key: String Read fKey;
  end;


constructor TTemplater.Create;
begin
  inherited;
end;

destructor TTemplater.Destroy;
begin
  inherited;
end;

// add a line to destination, fix formatting
procedure TTemplater.Add(const txt: String);
var
  t:  String;
  x:  Integer;
  c:  Char;
  i:  Integer;
  n:  Integer;
  pi: Integer;
  pl: Integer;
  s:  String;
begin
  s := '';
  if fDidExpand and IsBlanks(txt) then
    exit;
  if txt = '' then
  begin
    AddToDest(txt);
    exit;
  end;
  i  := 1;
  pi := -1; // previous cr
  pl := 0;  // keep compiler happy
  x  := 0;
  while i <= Length(txt) do
  begin
    if (Length(s) >= 79) and (pi > 0) then
    begin
      // add before last conditional
      t := Copy(s, 1, pl);
      if (x and 2) <> 0 then
        t := t + ' \';
      AddToDest(t);
      i  := pi;// go back to it
      s  := '';
      pi := -1;
      if (x and 1) <> 0 then
        s := s + StringOfChar(' ', 8);
      x := 0;
    end;
    c := txt[i];
    Inc(i);
    if c > #6 then
    begin
      if (c = #9) and not UseTabs then  // tab
      begin
        n := Length(s);
        s := s + StringOfChar(' ', (1 + (n or (TabSize -1))) - n);
      end
      else
        s := s + c;
      continue;
    end;
    if c = #2 then  // unconditional crlf
    begin
      AddToDest(s);
      s  := '';
      pi := -1;
      x  := 0;
      continue;
    end;
    // must be conditional break
    if c > #3 then
    begin
      x := Ord(c) - 3;
    end;
    if TabbedLength(s) >= 79 then
    begin
      if pi > 0 then
      begin
        t := Copy(s, 1, pl);
        if (x and 2) <> 0 then
          t := t + ' \';
        i := pi;// go back to previous
      end
      else
      begin
        // over length but cannot help it
        t := s;
        if (x and 2) <> 0 then
          t := t + ' \';
      end;
      AddToDest(t);
      s  := '';
      pi := -1;
      if (x and 1) <> 0 then
      begin
        if UseTabs then
          s := S + #9
        else
          s := s + StringOfChar(' ', TabSize);
      end;
      x := 0;
    end
    else
    begin
      pi := i - 1;  // allow backing up
      pl := length(s);
      if ((x and 1) <> 0) and not CharInSet(s[pl], [#9, ' ']) then
        s := s + ' ';   // give space
    end;
  end;
  AddToDest(s);
end;

procedure TTemplater.AddToDest(const txt: String);
begin
  Dest.Add(txt);
  if Test and assigned(OnShowMsg) then
    OnShowMsg(self, txt);
end;

procedure TTemplater.Advance(ofs: Integer = 1);
begin
  fPosition := fPosition + ofs;
end;

procedure TTemplater.AfterConstruction;
begin
  inherited;
  Line  := -1;
  Position := -1;
  fError := '';
  fBusy := False;
  fSource := TStringList.Create;
  fDest := TStringList.Create;
  fDefines := TStringList.Create;
  fMacros := TStringList.Create;
  PreLevel := -1;
  SetLength(PreLevels, 8);
  TabSize := 8;
  UseTabs := False;
end;

function TTemplater.Append(const txt: String): String;
begin
  Result := SubstLine + txt;
end;

// return char at offset from present position
function TTemplater.GetAtChar(ofs: Integer): Char;
begin
  Result := Current[Position + ofs];
end;

// check close at offset from present position
function TTemplater.AtClose(ofs: Integer): Boolean;
begin
  Result := IsClose[Position + ofs];
end;

function TTemplater.AtCondition: TConds;
var
  c1: Char;
  c:  Char;
begin
  Result := ckNone;
  c := AtChar[0];
  if c = '=' then
    Result := ckEqual
  else
  if c = '<' then
  begin
    c1 := AtChar[1];
    if c1 = '>' then
      Result := ckNEqual
    else
    if c1 = '=' then
      Result := ckLEqual
    else
      Result := ckLess;
  end
  else
  if c = '>' then
  begin
    c1 := AtChar[1];
    if c1 = '=' then
      Result := ckGEqual
    else
      Result := ckGreater;
  end;
  //  end;
  if Result <> ckNone then
    Advance(1);
  if Result > ckLess then
    Advance(1);
end;

function TTemplater.AtConstant(var key, Value: String): Char;
var
  KIdx: Integer;
  kind: TKeyDefines;
begin
  Result := #0;
  // get key
  kind  := AtKey(key);
  Value := '';
  if kind = dkNone then
  begin
    if CharInSet(AtChar[0], ['+', '*']) then
    begin
      Key := Format('%s%d', [Key, fRepCount]);
      Result := AtChar[0];
      Advance(1);
    end;
    KIdx := Defines.IndexOfName(key);
    if KIdx >= 0 then
      Value := Defines.ValueFromIndex[KIdx];
  end
  else
    Value := Predefined[kind];
  if CharInSet(AtChar[0], ['.', '$']) then
  begin
    if Value <> '' then
    begin
      if AtChar[0] = '.' then
      begin
        // return the extension
        Value := ExtractFileExt(Value);
        if Length(Value) > 1 then
          Value := Copy(Value, 2, 55);
      end
      else
        Value := ExtractNameOfFile(Value);
      if (Result <> #0) and (Value = '') then
        Value := '--';  // not empty
    end
    else
       Value := '';
    Advance(1);
  end;
end;

// return string at offset from present position
function TTemplater.AtCopy(len: Integer; ofs: Integer = 0): String;
var
  p: Integer;
begin
  Result := '';
  p := Position + ofs;
  if p < 1 then
    exit;
  if not EOS[p] then
  begin
    if len < 0 then
      len := 1 + Length(CurrentLine) - p;
    Result := Copy(CurrentLine, p, len);
  end;
end;

// returns chars to eol or next open or close
function TTemplater.AtCountToNext: Integer;
begin
  Result := 0;
  while (not AtEOS[Result]) and not CharInSet(AtChar[Result], [Opener, Closer]) do
    Inc(Result);
end;

function TTemplater.GetAtEOS(ofs: Integer): Boolean;
begin
  Result := EOS[Position + ofs];
end;

// get key
function TTemplater.AtKey(var key: String): TKeyDefines;
begin
  key := AtString(KeyChars);
  Result := IsKeyDefines(key);
end;

function TTemplater.AtMatch(const txt: String): Boolean;
begin
  Result := AnsiSameText(AtCopy(Length(txt)), txt);
end;

function TTemplater.AtMatch(const Vals: array of String): Integer;
var
  txt: String;
  l: Integer;
  lmax: Integer;
  lmin: Integer;
  i: Integer;
  m: Integer;
begin
  Result := -1;
  //find shortest and longest
  lmin := 99;
  lmax := 0;
  for i := LOW(Vals) to HIGH(Vals) do
  begin
    l := Length(Vals[i]);
    if l < lmin then
      lmin := l;
    if l > lmax then
      lmax := 1;
  end;
  // get copy for shortest
  txt := AtCopy(lmin);
  // look for it
  for m := lmin to lmax do
  begin
    for i := LOW(Vals) to HIGH(Vals) do
    begin
      if Length(Vals[i]) <> m then
        continue;
      if AnsiSameText(txt, Vals[i]) then
      begin
        Result := i;
        break;
      end;
    end;
    if Result >= 0 then
      break;
    txt := txt + AtChar[m];
  end;
  if Result >= 0 then
    Advance(Length(Vals[Result]));
end;

function TTemplater.AtNumber: Integer;
begin
  Result := -1;
  while CharInSet(AtChar[0], ['0'..'9']) do
  begin
    if Result < 0 then
      Result := 0;
    Result := (Result * 10) + (Ord(AtChar[0]) - Ord('0'));
    Advance(1);
  end;
end;

function TTemplater.AtString(Allow: TSysCharSet): String;
var
  c: Char;
begin
  Result := '';
  while not AtEos[1] do
  begin
    c := AtChar[0];
    if not CharInSet(c, Allow) then
      break;
    Result := Result + c;
    Advance(1);
  end;
end;

procedure TTemplater.BeforeDestruction;
begin
  PreLevels := nil;
  FreeAndNil(fSource);
  FreeAndNil(fDest);
  FreeAndNil(fDefines);
  FreeAndNil(fMacros);
  inherited;
end;

procedure TTemplater.CheckAtClose(ofs: Integer = 0);
begin
  if not AtClose(ofs) then
    raise ETemplater.Create('Expected ' + Closer, 'found ' + AtCopy(2));
end;

procedure TTemplater.CheckAtEOS;
begin
  if AtEOS[0] then
    raise ETemplater.Create('Unexpected end of string ', '');
end;

procedure TTemplater.Clear;
begin
  fSource.Clear;
  fDest.Clear;
  fDefines.Clear;
  Line  := -1;
  Position := -1;
  fError := '';
  fBusy := False;
end;

function TTemplater.DoComp(const Value: String): string;
var
  ok: Boolean;
  Comp: Integer;
  txt: String;
  cond: TConds;
  nv: integer;
  nt: integer;
begin
  Result := Value;
  cond := AtCondition;
  if cond <> ckNone then
  begin
    txt := AtString(['A'..'Z', 'a'..'z', '0'..'9', '_', '-']);
    // no 'value' returns empty (false)
    if (Value <> '') {or not ((Result <> #0) and (Value = '##'))} then
    begin
      if TryStrToInt(Value, nv) and TryStrToInt(txt, nt) then
        comp := nv - nt
      else
        Comp := AnsiCompareText(Value, txt);   // non-case
      ok := True;
      case cond of
        //      ckNone: ;
        ckEqual: ok := Comp = 0;
        ckGreater: ok := Comp > 0;
        ckLess: ok  := Comp < 0;
        ckNEqual: ok := Comp <> 0;
        ckGEqual: ok := Comp >= 0;
        ckLEqual: ok := Comp <= 0;
      end;
      if not ok then
        Result := ''
      else
      if Value = '' then
        Result := 'True';  // return something
    end;
//    if (Result <> #0) and (Value = '') then
//      Value := '##';  // not empty - will be removed
  end;
end;

function TTemplater.DoDefine(const Keyword: String; var flags: TScanFlag): String;
var
  k: Char;
  key: String;
begin
  key := '';
  Advance(1);
  fDidExpand := True;
  k := AtChar[0];
  if CharInSet(k, ['A'..'Z']) then
    Result := DoKey(Keyword, flags)
  else
  if IsKeySymbol(k) <> ksNone then
    Result := DoSymbol(Keyword, flags)
  else
  begin
    if k < ' ' then
      raise ETemplater.Create('Invalid key', '#' + IntToStr(Ord(k)));
    raise ETemplater.Create('Invalid key', '"' + k + '"');
  end;
end;

function TTemplater.DoKey(const Keyword: String; var flags: TScanFlag): String;
var
  Counted: Char;
  txt: String;
  IsFalse: Boolean;
  f:  tScanFlag;
//  islo: boolean;
  KText: String;
  op: Char;
  key: String;
begin
  Result := '';
  txt := '';
  f := [];
  Counted := AtConstant(key, KText);
  if Counted <> #0 then
  begin
    if not IsSkip(flags) then
    begin
      if (KText = '') or (KText = '##') then
        fRepeating := False  // stop repeat on fail
      else
      if Counted = '+' then
      begin
        fRepeating := True;  // only set if incrementing
        Inc(fRepCount);
      end;
    end;
    if KText = '##' then
      KText := '';
  end;
  KText := DoComp(KText);
  //  get operator
  op := AtChar[0];
  if op = Closer then
  begin
    if not IsSkip(flags) then
      txt := KText; // just return the text if not skipping
  end
  else
  begin
    if not CharInSet(op, [#0, #9, ' ', '@', '!'{, '$', '&'}]) then
      ETemplater.Create('Invalid operator', key + ' ' + op);
    Advance(1);
    if IsSkip(flags) then
      f := [sfIgnore];
    if CharInSet(op, [#9, ' ']) then
    begin
      IsFalse := KText <> '';
      if IsFalse then
        txt := KText;   // return defined text
    end
    else
    begin
      f := f + [sfIf];
//      IsFalse := True;
//      if KText <> '' then
//      begin
//        case op of
//          '!': IsFalse := True;
//          '$': IsFalse := Pos('.', KText) > 0;  // has dot
//          '&': IsFalse :=  Pos('.', KText) < 1; // no dot
////          '$': IsFalse := not CharInSet(KText[1], ['A'..'Z']);
////          '&': IsFalse := not CharInSet(KText[1], ['a'..'z']);
//        end;
//      end;
      IsFalse := KText = '';
      if op = '!' then
        IsFalse := not IsFalse;
    end;
    if IsFalse then
      f := f + [sfFalse];
    txt := txt + DoStatement(key, f);
  end;
  CheckAtClose;
  Advance(1);
  if not IsSkip(flags) then
    Result := txt;
end;

procedure TTemplater.DoLine;
var
  flag: TScanFlag;
  txt:  String;
begin
  fDidExpand := False;
  fIgnoreLine := False;
  SubstLine := '';
  fPosition := 1;
  flag := [];
  fRepeating := False;
  try
    txt := DoStatement('', flag);
    if not AtEOS[0] then
      raise ETemplater.Create('Expected end of line', 'got ' + AtChar[0]);
    if not fIgnoreLine then
      Add(txt);
  except
    on E: ETemplater do
    begin
      if not assigned(OnShowMsg) then
        raise;
      Err(E.Err, E.Key);
    end;
  end;
end;

function TTemplater.DoLoad(const template: String): Integer;
var
  resstrm: TStream;
begin
  if ExtractFilePath(template) = '' then
  begin
    resstrm := OpenResStream(template, PChar('TEXT'));
    if resstrm <> nil then
    begin
      fSource.LoadFromStream(resstrm);
      resstrm.Free;
    end
    else
      fSource.LoadFromFile(Template);  // worth a try
  end
  else
    fSource.LoadFromFile(Template);
  Result := fSource.Count;
end;

function TTemplater.DoProcess: Integer;
var
  HadRem: Boolean;
  p: Integer;
  lno: Integer;
  Repeats: Integer;
  v: String;
begin
  p := Defines.IndexOfName('USETABS');
  if p >= 0 then
    UseTabs := AnsiSameText(Defines.Values['USETABS'], 'true');
  v := Defines.Values['TAB'];
  if v <> '' then
  begin
    p := StrToIntDef(v, 8);
    TabSize := p;
  end;
  fRepCount := 0;
  PreLevel := 0;
  PreUse := 0;  // bits 0 _ 1 = in if, 1 _ 1 = false, 2 _ 1 = parent was false
  InRegion := False;  // not in hidden region
  lno := 0;
  fMacros.Clear;
  while lno < fSource.Count do
//  for lno := 0 to fSource.Count - 1 do
  begin
    fLine := lno;
//    inc(lno);
    fPosition := 1;
    fRepeating := False;
//    fCurrentLine := fSource[Line];
//    // trim trailing rem
//    p := AnsiPos(Opener + '#' + Closer, CurrentLine);
//    if not InRegion and (p > 0) then
//    begin
//      fCurrentLine := Copy(fCurrentLine, 1, p - 1);
//      if IsBlanks(CurrentLine) then
//        continue;
//    end;
    //++++
    HadRem := GetLine(lno);
    if not InRegion and HadRem and IsBlanks(CurrentLine) then
      continue;
    //-+++
    // check pre
    p := AnsiPos(Opener, CurrentLine);
    if p = 1 then
    begin
      if PreProcess then
        continue;
      p := AnsiPos(Opener, CurrentLine);  // may have changed
    end;
    if InRegion or (p < 1) then
    begin
      if (PreUse < 2) then
        Add(fCurrentLine);
      continue;
    end;
    // ignoring it?
    if PreUse > 1 then
      continue;
    //++++
    ExpandMacros;
    repeats := 0;
    repeat
      Inc(Repeats);
      if Repeats > 99 then
      begin
        fRepCount := 0;
        fRepeating := False;
        raise ETemplater.Create('Too many repeats');
      end;
      fDidExpand := False;
      DoLine;
    until not fRepeating;
  end;
  Result := ErrCount;
end;

function TTemplater.DoStatement(const KeyWord: String; var flag: TScanFlag): String;
var
  p: Integer;
begin
  Result := '';
  while not AtEOS[0] do
  begin
    p := AtCountToNext;   // chars to [ or ]
    if p > 0 then
    begin
      if not IsSkip(flag) then
        Result := Result + AtCopy(p);  // copy the text
      Advance(p);
      continue;
    end;
    if AtClose(0) then
      break;
    Result := Result + DoDefine(Keyword, flag);
  end;
end;

function TTemplater.DoSymbol(const Keyword: String; var flags: TScanFlag): String;
var
  c: Integer;
  n: Integer;
  repposn: Integer;
  txt: String;
  sym: TKeySymbols;
  f: TScanFlag;
  ttxt: String;
begin
  Result := '';
  ttxt := '';
  sym  := IsKeySymbol(AtChar[0]);
  Advance(1);
  case Sym of
    ksElse:
    begin
      if not (sfIgnore in flags) then
      begin
        if not (sfIf in flags) then
          raise ETemplater.Create('Else not allowed');
        flags := flags - [sfIf];  // not in if any more
        // invert sfFalse
        if sfFalse in flags then
          flags := flags - [sfFalse]
        else
          flags := flags + [sfFalse];
      end;
    end;
    ksCntr:
    begin
      if AtChar[0] = '=' then
      begin
        Advance(1);
        fRepeating := False;  // stop infinite loop
        // get n
        n := AtNumber;
        if (n >= 0) and not IsSkip(flags) then
          fRepCount := n;
      end
      else
      if AtChar[0] = '+' then
      begin
        Advance(1);
        // get n
        n := AtNumber;
        if n >= 0 then
          ttxt := IntToStr(fRepCount + n);
      end
      else
        ttxt := IntToStr(fRepCount);
    end;
    ksRep:
    begin
      fRepeating := True;
      repposn := Position;
      while fRepCount < 999 do
      begin
        txt := DoStatement('', flags);
        if isSkip(flags) or (not fRepeating) then
          break;
        Position := repposn;
        Inc(fRepCount);
        if not IsSkip(flags) then
          ttxt := ttxt + txt;
      end;
      if fRepCount >= 999 then
      begin
        fRepCount := 0;
        fRepeating := False;
        raise ETemplater.Create('Too many repeats');
      end;
    end;
    ksRem:
    begin
      if AtClose(0) then
      begin
        if not IsSkip(flags) then
          flags := flags + [sfIgnore];
      end
      else
      begin
        // rem this statement only
        f := [sfIgnore];
        {Result :=} DoStatement(Keyword, f);
      end;
    end;
    ksPrev: ttxt := Keyword;
    ksCond:
    begin
      c := 3;
      if AtChar[0] = '\' then
      begin       // append \ on break
        c := c + 2;
        Advance(1);
      end;
      if AtChar[0] = '^' then
      begin       // start new line with tab
        c := c + 1;
        Advance(1);
      end;
      ttxt := Char(c);
    end;
    ksCR: ttxt  := #2;
    ksTab:
    begin
      if AtChar[0] = '=' then
      begin
        Advance(1);
        // get n
        n := AtNumber;
        if (n >= 0) and not IsSkip(flags) then
        begin
          if n > 0 then
          begin
            TabSize := n;
            UseTabs := True;
          end
          else
            UseTabs := false;
        end;
      end
      else
        ttxt := #9;
    end;
    ksOpen: ttxt := Opener;
    ksClose: ttxt := Closer;
  end;
  CheckAtClose;
  Advance(1);
  if not IsSkip(flags) then
    Result := ttxt;
end;

function TTemplater.GetEOS(posn: Integer): Boolean;
begin
  Result := posn > Length(CurrentLine);
end;

procedure TTemplater.Err(const Err, Key: String);
begin
  Inc(fErrCount);
  Error := Format('%s: %s [%d.%d:%s]', [Err, Key, Line, Position, CurrentLine]);
  if assigned(OnShowMsg) then
    OnShowMsg(self, Error);
end;

procedure TTemplater.ExpandMacros;
var
  ident: String;
  p: Integer;
  pre: string;
  tmppos: Integer;
  txt: string;
begin    // check macros {=IDENT=}
  tmppos := Position;
  p := AnsiPos(Opener + '=', CurrentLine);
  while p > 0 do
  begin
    Position := p + 2;
    pre := copy(CurrentLine, 1, p-1);
    ident := AtString(KeyChars);
    if (ident = '') or (AtChar[0] <> '=') or (AtChar[1] <> Closer) then
        raise ETemplater.Create('Bad macro');
    txt := fMacros.Values[ident];
    if txt = '' then
        raise ETemplater.Create('unknown or empty macro ' + ident);
    CurrentLine := pre + txt + AtCopy(-1, 2);
    p := AnsiPos(Opener + '=', CurrentLine);
  end;
  Position := tmppos;
end;

// return char at position
function TTemplater.GetCurrent(Index: Integer): Char;
begin
  Result := #0;
  if (Index > 0) and (Index <= Length(CurrentLine)) then
    Result := CurrentLine[Index];
end;

function TTemplater.GetPredefined(kind: TKeyDefines): String;
var
  sn: Integer;
  vers: Integer;
  Guid: TGuid;
begin
  sn := -1;
  vers := Version;
  Result := '';
  case kind of
    dkPROJECTVER: ;
    dkSTAMP: DateTimeToString(Result, 'd mmm yyyy t', Now);
    dkDATE: DateTimeToString(Result, 'd mmmm, yyyy', Now);
    dkTIME: DateTimeToString(Result, 'tt', Now);
    dkYEAR: sn := Year(Now);
    dkAUTHOR: Result := 'Russell Peters';
    dkFILENAME: Result := ExtractFileName(Filename);
    dkNAME: Result := ExtractNameOfFile(Filename);
    dkLICENCE:
    begin
      fIgnoreLine := True;
      Licence;
    end;
    dkVERSION: Result := VersStr(vers);
    dkVERSION_C: Result := ReplaceStr(VersStr(vers), '.', ',');
    dkGUID:
    begin
      CreateGUID(Guid);
      Result := GuidToString(Guid);
    end;
    dkPROJECT: sn := Vers;
    dkMAJOR: sn := Vers div 1000000;
    dkMINOR: sn := (Vers div 100000) mod 10;
    dkRELEASE: sn := (Vers div 10000) mod 10;
    dkBUILDZ: Result := Copy(VersStr(vers), 7, 4);
    dkBUILD: sn := Vers mod 10000;
    dkPROJ: sn  := Vers div 10000;
    dkNone: ;
  end;
  if sn >= 0 then
    Result := IntToStr(sn);
end;

function TTemplater.IsBlanks(const txt: String): Boolean;
var
  i: Integer;
begin
  Result := True;
  if Length(txt) > 0 then
  begin
    for i := 1 to Length(txt) do
      if not CharInSet(txt[i], [#9, #10, #13, ' ']) then
      begin
        Result := False;
        exit;
      end;
  end;
end;

function TTemplater.GetIsClose(posn: Integer): Boolean;
begin
  Result := False;
  if (posn > 0) and (posn <= (Length(CurrentLine))) then
    Result := (CurrentLine[posn] = Closer);
end;

function TTemplater.GetLine(var LineNo: Integer): boolean;
var
  p: Integer;
  SingleLine: boolean;
begin
  Result := False;
  fCurrentLine := fSource[Line];
  inc(LineNo);
  repeat
    SingleLine := True;
    // trim trailing rem
    p := AnsiPos(Opener + '#' + Closer, CurrentLine);
    if {not InRegion and} (p > 0) then
    begin
      fCurrentLine := Copy(fCurrentLine, 1, p - 1);
      Result := True;
    end;
    // continue line?
    p := AnsiPos(Opener + ',' + Closer, CurrentLine);
    if p > 0 then
    begin
      fCurrentLine := Copy(fCurrentLine, 1, p - 1);
      if LineNo < fSource.Count then
      begin
        fCurrentLine := fCurrentLine + fSource[LineNo];
        inc(LineNo);
        SingleLine := False;
      end;
    end;
  until SingleLine;
end;

function TTemplater.IsKeyDefines(const txt: String): TKeyDefines;
var
  i: TKeyDefines;
begin
  Result := dkNone;
  for i := dkPROJECTVER to dkNone do
    if KeyDefines[i] = txt then
    begin
      Result := i;
      break;
    end;
end;

function TTemplater.IsKeySymbol(k: Char): TKeySymbols;
var
  i: TKeySymbols;
begin
  Result := ksNone;
  for i := ksElse to ksNone do
    if KeySymbols[i] = k then
    begin
      Result := i;
      break;
    end;
end;

// check if ignore or false
function TTemplater.IsSkip(flags: TScanFlag): Boolean;
begin
  Result := (flags - [sfIf]) <> [];
end;

procedure _Licence(strs: TStrings; Stamp: string = ''; CStyle: Boolean = False);
const
  DividerLength = 50;
var
  S: string;
begin
  if CStyle then
    S := '/* '
  else
    S := '(* ';
  strs.Add(S + StringOfChar('*', DividerLength));
  strs.Add('TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.');
  strs.Add('  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.');
  strs.Add('Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler');
  strs.Add('Copyright (C) 1992-2008 Eric W. Engler');
  strs.Add('Copyright (C) 2009, 2010, 2011, 2012 Russell Peters and Roger Aelbrecht');
  strs.Add(' ');
  strs.Add('All rights reserved.');
  strs.Add('For the purposes of Copyright and this license "DelphiZip" is the current');
  strs.Add(' authors, maintainers and developers of its code:');
  strs.Add('  Russell Peters and Roger Aelbrecht.');
  strs.Add(' ');
  strs.Add('Redistribution and use in source and binary forms, with or without');
  strs.Add(' modification, are permitted provided that the following conditions are met:');
  strs.Add('* Redistributions of source code must retain the above copyright');
  strs.Add('   notice, this list of conditions and the following disclaimer.');
  strs.Add('* Redistributions in binary form must reproduce the above copyright');
  strs.Add('   notice, this list of conditions and the following disclaimer in the');
  strs.Add('   documentation and/or other materials provided with the distribution.');
  strs.Add('* DelphiZip reserves the names "DelphiZip", "ZipMaster", "ZipBuilder",');
  strs.Add('   "DelZip" and derivatives of those names for the use in or about this');
  strs.Add('   code and neither those names nor the names of its authors or');
  strs.Add('   contributors may be used to endorse or promote products derived from');
  strs.Add('   this software without specific prior written permission.');
  strs.Add(' ');
  strs.Add('THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"');
  strs.Add(' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE');
  strs.Add(' IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE');
  strs.Add(' ARE DISCLAIMED. IN NO EVENT SHALL DELPHIZIP, IT''S AUTHORS OR CONTRIBUTERS BE');
  strs.Add(' LIABLE FOR ANYDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR');
  strs.Add(' CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF');
  strs.Add(' SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS');
  strs.Add(' INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN');
  strs.Add(' CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE)');
  strs.Add(' ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE');
  strs.Add(' POSSIBILITY OF SUCH DAMAGE.');
  strs.Add(' ');
  strs.Add('contact: problems AT delphizip DOT org');
  strs.Add('updates: http://www.delphizip.org');
  if CStyle then
    S := ' */'
  else
    S := ' *)';
  strs.Add(StringOfChar('*', DividerLength) + S);
  if Stamp <> '' then
    strs.Add('//' + Stamp + ' ' + FormatDateTime('yyyy-mm-dd', Now));
  strs.Add(' ');
end;

procedure TTemplater.Licence;
begin
  _Licence(Dest, '', false);
end;

function TTemplater.NextOpen: Integer;
var
  c: Char;
  p: Integer;
begin
  Result := 0;
  P := Position;
  while P <= Length(CurrentLine) do
  begin
    c := CurrentLine[P];
    Inc(P);
    if (c = '{') then
    begin
      Result := P;
      break;
    end;
  end;
end;

function TTemplater.PopLevel: Integer;
begin
  if PreLevel >= 0 then
  begin
    PreUse := PreLevels[PreLevel];
    Dec(PreLevel);
  end;
  Result := PreLevel;
end;

// <0 _ off, 0 _ none (on), >0 _ on
function TTemplater.PreProcess: Boolean;
var
  c: Char;
  key: String;
  ktxt: string;
  mtxt: string;
  Value: String;
begin
  Result := False;
  if InRegion then
  begin
    if (AtChar[1] = '=') and (AtChar[2] = Closer) then
    begin
      // ignore define a macro to eol
      Result := True;
      exit;
    end;
    if (AtChar[1] = '>') and (AtChar[2] = Closer) then
    begin
      if PreUse < 2 then
        InRegion := False;  // end of region
      Result := True;
    end;
    exit;
  end;
  // not hidden
  if (AtChar[1] = '=') and (AtChar[2] = Closer) then
  begin
    Advance(3);
    // define a macro to eol
    ktxt := AtString(KeyChars);
    if (ktxt <> '') and (AtChar[0] = '=') then
    begin
      mtxt := AtCopy(-1, 0);//1);
      fMacros.Add(ktxt + {'=' +} mtxt);  // assumes correctly formatted
    end;
    Result := True;    // discard line
    exit;
  end;
  if AtChar[1] = '|' then
  begin
    Advance(2);
    // get 'variable'
    if AtConstant(key, Value) <> #0 then
      raise ETemplater.Create('repeat constants not allowed');
    Value := DoComp(Value);
    PushLevel;
    if PreUse > 1 then
      PreUse := 4     // in false
    else
      PreUse := 0;
    if Value = '' then
      PreUse := PreUse or 3     // in if/ifdef, false
    else
      PreUse := PreUse or 1;    // in if/ifdef, true
    Result := True;
    CheckAtClose;
  end
  else
  if (AtChar[1] = Closer) and (AtChar[2] <> Closer) then
  begin
    if PreUse = 0 then
      raise ETemplater.Create('not in if/ifdef');
    // end
    if PopLevel < 0 then
      PreUse := 0;
    Advance(1);
    Result := True;
  end;
  c := AtChar[1];
  if CharInSet(c, ['<', '>', ':', '~']) and (AtChar[2] = Closer) then
  begin
    Advance(3);
    Result := True;
    case c of
      '<':
      begin
        if (PreUse < 2) then
        begin
          InRegion := True;
        end;
      end;
      '>':
        begin
          if PreUse < 2 then
            raise ETemplater.Create('not in region');
        end;
      ':':   // use rest of line unaltered
      begin
        if PreUse < 2 then
          Add(Copy(fCurrentLine, 4, Length(fCurrentLine) - 3));
      end;
      '~':
      begin
        if (PreUse and 1) = 0 then
          raise ETemplater.Create('not allowed here');
        PreUse := (PreUse xor 2) or (PreUse and 4);
      end;
    end;
  end;
end;

function TTemplater.Process(const template: String): Integer;
begin
  Result := -1;
  if fBusy then
    exit;
  fBusy := True;
  try
    try
      Result := DoLoad(template);
      if Result > 0 then
        Result := DoProcess;
    except
      on E: ETemplater do
      begin
        Err(E.Err, E.Key);
      end;
      on E: Exception do
      begin
        Err('Exception ', E.Message);
      end;
    end;
  finally
    fBusy := False;
  end;
end;

function TTemplater.PushLevel: Integer;
var
  lvl: Integer;
begin
  lvl := PreLevel + 1;
  if lvl > HIGH(PreLevels) then
    SetLength(PreLevels, lvl + 4);
  PreLevel := lvl;
  PreLevels[lvl] := PreUse;
  if PreUse > 1 then
    PreUse := 4;    // parent was false - keep it

  Result := lvl;
end;

function TTemplater.SafeChar(c: Char): Char;
begin
  if c = #0 then
    Result := '?'
  else
    Result := c;
end;

procedure TTemplater.SetDefines(const Value: TStrings);
begin
  if fDefines <> Value then
  begin
    //    fDefines := Value;
  end;
end;

procedure TTemplater.SetDest(const Value: TStrings);
begin
  if fDest <> Value then
  begin
    //    fDest := Value;
  end;
end;

procedure TTemplater.SetTabSize(const Value: Integer);
begin
  if Value < 2 then
    FTabSize := 2
  else
  if Value < 4 then
    FTabSize := 4
  else
    FTabSize := 8;
end;

function TTemplater.TabbedLength(const s: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for I := 1 to Length(s) do
    if s[I] = #9 then
      Result := (Result or (TabSize - 1)) + 1
    else
      inc(Result);
end;

function TTemplater.Year(dt: TDateTime): Integer;
var
  y, m, d: Word;
begin
  DecodeDate(dt, y, m, d);
  Result := y;
end;

constructor ETemplater.Create(const msg: String; const what: String = '');
begin
  inherited Create(msg + ': ' + what);
  fErr := msg;
  fKey := what;
//  Message := msg + ': ' + what;
end;

end.
