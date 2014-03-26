unit scan;
(* ***************************************************************************
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.
 Copyright (C) 2013, 2014  by Russell J. Peters, Roger Aelbrecht.

   This file is part of TZipMaster Version 1.9.2
   
TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009, 2010, 2011 Russell Peters and Roger Aelbrecht

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
//modified 2012-04-02

interface

uses
  classes;

const
  IndentifierChars = ['_', 'a' .. 'z', 'A' .. 'Z', '0' .. '9'];
  IndentifierChar1 = ['_', 'a' .. 'z', 'A' .. 'Z'];
  SpaceChars       = [#0, #9, ' '];
  qte: char        = #39;
  LineMarker       = '{_LINE_}';

type
  TTokenizer = class
  private
    FLineNo: Integer;
    FOwnsStrs: Boolean;
    FPosn: Integer;
    FTheStrs: TStrings;
    FTK: char;
    function GetCurrentLine: string;
    function GetLine(Index: Integer): string;
    procedure SetTheStrs(const Value: TStrings);
  protected
  public
    constructor Create(Strs: tstrings);
    procedure AfterConstruction; override;
    function Alpha(var next: char): string;
    function At_Safe: Char;
    procedure BeforeDestruction; override;
    procedure Clear;
    function Count: integer;
    function FindIdent(const theIdent: string): Boolean;
    function FindSymbol(const Symbol: string): boolean;
    function LoadFromFile(const FName: string): Integer;
    function NextChar(Prep: Boolean = False): char;
    function Token(any: Boolean = False): string;
    function Number(var next: Char): integer; overload;
    function Number: integer; overload;
    function NumberAsString: string; overload;
    function NumberAsString(var next: char): string; overload;
    property CurrentLine: string read GetCurrentLine;
    property Line[Index: Integer]: string read GetLine; default;
    property LineNo: Integer read FLineNo write FLineNo;
    property Posn: Integer read FPosn write FPosn;
    property TheStrs: TStrings read FTheStrs write SetTheStrs;
    property TK: char read FTK write FTK;
  end;

function Alpha(var chars: string; const s: string; var psn: Integer): Char;
function At_safe(const s: string; psn: Integer): char;
// return next word (identifier/label) from psn
// k = terminator for current comment ( '*' = '*/' )
// function Token_C(var psn: integer; var k: char; const l: string): string;
// return next word (identifier/label) from psn
// k = terminator for current comment ( '*' = '*)' )
// function Token_pas(var psn: integer; var k: char; const l: string; any: Boolean = False): string;
function Token(var lno, psn: Integer; var k: char; src: TStrings;
  any: Boolean = False): string;
function Skip(var psn: Integer; const l: string): char;
function ID(var psn: Integer; const l: string): string;
function IDPas(var psn: Integer; const l: string): string; // get ID.ID
function Quoted_(var psn: Integer; const l: string; q: char): string;
function Quoted(var psn: Integer; const l: string; dq: Boolean): string;
function Number_(var psn: Integer; const l: string): string;
function Number(var psn: Integer; const l: string): Integer;
// return char at psn after skipping spaces and comments
function SkipPas(var psn: Integer; var k: char; const l: string;
  Prep: Boolean = False): char;
function NextPas(var lno, psn: Integer; var k: char; src: TStrings;
  Prep: Boolean = False): char;

implementation

uses
  SysUtils;

const
  Alphas: TSysCharSet = ['A'..'Z', 'a'..'z'];

{$ifndef UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := c in CharSet;
end;
{$endif}

function Alpha(var chars: string; const s: string; var psn: Integer): Char;
begin
  Result := skip(psn, s);
  while CharInSet(Result, Alphas) do
  begin
    chars := chars + Result;
    inc(psn);
    Result := At_safe(s, psn);
  end;
end;

function At_safe(const s: string; psn: Integer): char;
begin
  Result := #0;
  if (psn >= 1) and (psn <= length(s)) then
    Result := s[psn];
end;

// return next word (identifier/label) from psn
// k = terminator for current comment ( '*' = '*/' )
function Token_C(var psn: Integer; var k: char; const l: string): string;
var
  c: char;
  inquote, hadpre: Boolean;
begin
  Result := '';
  if psn < 1 then
    psn := 1;
  if psn > length(l) then
    exit;
  if k <> '*' then
    k := #0;
  inquote := False;
  hadpre := False;
  // find start of word
  while psn <= length(l) do
  begin
    c := l[psn];
    if inquote then
    begin
      inquote := c <> qte;
      inc(psn);
      continue;
    end;
    if k <> #0 then
    begin
      // in comment
      if c <> k then
      begin
        inc(psn);
        continue;
      end;
      // if k = '}' then
      // begin
      // k := #0;
      // inc(psn);
      // continue;
      // end;
      if (k = '*') and (At_safe(l, psn + 1) = '/') then
      begin
        inc(psn);
        k := #0;
      end;
      inc(psn);
      continue;
    end;
    // not in comment or string
    if c = qte then
    begin
      if Result <> '' then
        break; // end of token
      inquote := true;
      hadpre := False;
      inc(psn);
      continue;
    end;
    // if c = '{' then
    // begin
    // if Result <> '' then
    // break; // end of token
    // k := '}';
    // inc(psn);
    // continue;
    // end;
    if (c = '/') and (At_safe(l, psn + 1) = '*') then
    begin
      if Result <> '' then
        break; // end of token
      k := '*';
      inc(psn, 2);
      hadpre := False;
      continue;
    end;
    if (Result = '') then
    begin
      if (c = '/') and (At_safe(l, psn + 1) = '/') then
      begin
        psn := -1;
        break;
      end;
      if CharInSet(c, ['_', 'a' .. 'z', 'A' .. 'Z']) then
      begin
        if hadpre then
          Result := '#' + c
        else
          Result := c;
      end
      else
      begin
        if hadpre then
        begin
          if not CharInSet(c, [#9, ' ']) then
            hadpre := False; // only space allowed
        end
        else
          if c = '#' then
            hadpre := true;
      end;
    end
    else // in word
    begin
      if not CharInSet(c, ['_', 'a' .. 'z', 'A' .. 'Z', '0' .. '9']) then
        break; // end of word
      Result := Result + c;
    end;
    inc(psn);
  end;
end;

// return char at psn after skipping spaces and comments
// k = terminator for current comment ( '*' = '*)' )
function SkipPas(var psn: Integer; var k: char; const l: string;
  Prep: Boolean = False): char;
const
  qte: char = #39;
var
  inquote: Boolean;
begin
  Result := #0;
  if psn < 1 then
    psn := 1;
  if psn > length(l) then
    exit;
  if not CharInSet(k, ['}', '*']) then
    k := #0;
  inquote := False;
  Dec(psn);
  // find start of word
  while psn <= length(l) do
  begin
    inc(psn);
    // next char
    Result := l[psn];
    if inquote then
    begin
      // ignore all between quotes
      inquote := Result <> qte;
      continue;
    end;
    if k <> #0 then
    begin
      // in comment
      if Result <> k then
      begin
        // ignore
        inquote := Result = qte;
        continue;
      end;
      if (Result = '}') and (k = Result) then
      begin
        // end of rem
        k := #0;
        continue;
      end;
      if (k = '*') and (At_safe(l, psn + 1) = ')') then
      begin
        inc(psn);
        k := #0;
      end;
      continue;
    end;
    // not in comment or string
    if not CharInSet(Result, [#9, ' ', '{', '(', '/']) then
      break;
    if (Result = '{') and ((not Prep) or (At_safe(l, psn + 1) <> '$')) then
      k := '}';
    if (Result = '(') and (At_safe(l, psn + 1) = '*') then
    begin
      k := '*';
      inc(psn);
    end;
    if (Result = '/') and (At_safe(l, psn + 1) = '/') then
    begin
      k := #0;
      Result := #0;
      psn := -1; // end of line
      break;
    end;
    if not CharInSet(Result, [#9, ' ']) then
      break;
  end;
end;

// return next word (identifier/label) from psn
// k = terminator for current comment ( '*' = '*)' )
function Token_pas(var psn: Integer; var k: char; const l: string;
  any: Boolean = False): string;
const
  qte: char = #39;
var
  c: char;
  cs: string;
  inquote: Boolean;
begin
  Result := '';
  if psn < 1 then
    psn := 1;
  if psn > length(l) then
    exit;
  if not CharInSet(k, [#0, '}', '*']) then
    k := #0;
  inquote := False;
  // find start of word
  while psn <= length(l) do
  begin
    c := l[psn];
    if inquote then
    begin
      inquote := c <> qte;
      inc(psn);
      continue;
    end;
    if k <> #0 then
    begin
      // comment
      if c <> k then
      begin
        inc(psn);
        continue;
      end;
      if k = '}' then
      begin
        k := #0;
        inc(psn);
        continue;
      end;
      if (k = '*') and (At_safe(l, psn + 1) = ')') then
      begin
        inc(psn);
        k := #0;
      end;
      inc(psn);
      continue;
    end;
    // not in comment or string
    if c = qte then
    begin
      if Result <> '' then
        break; // end of token
      inquote := true;
      inc(psn);
      continue;
    end;
    if c = '{' then
    begin
      if Result <> '' then
        break; // end of token
      if Any and (At_safe(l, psn + 1) = '_') then
      begin
        cs := Copy(l, psn, 8);
//        if SameStr(cs, LineMarker) then
        if CompareStr(cs, LineMarker) = 0 then
        begin
          Result := LineMarker;
          inc(psn, Length(LineMarker)); // after '}'
          break;
        end;
      end;
      k := '}';
      inc(psn);
      continue;
    end;
    if (c = '(') and (At_safe(l, psn + 1) = '*') then
    begin
      if Result <> '' then
        break; // end of token
      k := '*';
      inc(psn, 2);
      continue;
    end;
    if (Result = '') then
    begin
      if (c = '/') and (At_safe(l, psn + 1) = '/') then
      begin
        psn := -1;
        break;
      end;
      if CharInSet(c, IndentifierChar1) then
        Result := c // ; // At start of Indentifier
      else
        if any and not CharInSet(c, SpaceChars) then
        begin
          Result := c;
          inc(psn);
          break;
        end;
    end
    else // in word
    begin
      if not CharInSet(c, IndentifierChars) then
        break; // end of word
      Result := Result + c;
    end;
    inc(psn);
  end;
end;

function NextPas(var lno, psn: Integer; var k: char; src: TStrings;
  Prep: Boolean = False): char;
begin
  Result := #0;
  if lno < 0 then
  begin
    lno := 0;
    psn := 1;
    k := #0;
  end;
  if psn < 1 then
    psn := 1;
  while lno < src.Count do
  begin
    Result := SkipPas(psn, k, src[lno], Prep);
    if Result <> #0 then
      break;
    inc(lno);
    psn := 1;
  end;
end;

function TokenPas(var lno, psn: Integer; var k: char; src: TStrings;
  any: Boolean = False): string;
const
  qte: char = #39;
var
  c: char;
  pre: String;
  st: string;
begin
  Result := '';
  repeat
    c := NextPas(lno, psn, k, src, any);
    if c = #0 then
      exit; // nothing found
    Result := IDPas(psn, src[lno]);
    if Result <> '' then
      break;
    st := Quoted_(psn, src[lno], qte);
    if c = '{' then
    begin
      // preprocessor
      pre := c;
      st := src[lno];
      while psn < length(st) do
      begin
        inc(psn);
        c := st[psn];
        pre := pre + c;
        if c = '}' then
          break;
      end;
    end;
    if any then
    begin
      Result := st;
      if Result = '' then
        Result := pre;
      if Result = '' then
        Result := Number_(psn, src[lno]);
      if Result = '' then
      begin
        // return anything else
        Result := c;
        inc(psn);
      end;
    end;
    // ignore anything other than identifier
    if (st = '') then // already skipped quoted string
      inc(psn);
  until Result <> '';
end;

function Token(var lno, psn: Integer; var k: char; src: TStrings;
  { c: boolean; } any: Boolean = False): string;
begin
  Result := '';
  if lno < 0 then
  begin
    lno := 0;
    psn := 1;
    k := #0;
  end;
  if psn < 1 then
    psn := 1;
  while lno < src.Count do
  begin
    // if c then
    // Result := Token_C(psn, k, src[lno])
    // else
    Result := Token_pas(psn, k, src[lno], any);
    if Result <> '' then
      break;
    inc(lno);
    psn := 1;
  end;
end;

function Skip(var psn: Integer; const l: string): char;
begin
  if psn < 1 then
    psn := 1;
  while (psn <= length(l)) do
  begin
    Result := l[psn];
    if not CharInSet(Result, [#9, ' ']) then
      exit;
    inc(psn);
  end;
  Result := #0;
end;

function IDPas(var psn: Integer; const l: string): string;
var
  c: char;
  k: char;
  Posn: Integer;
  lbl2: String;
begin
  Result := ID(psn, l);
  if Result <> '' then
  begin
    Posn := psn;
    k := #0;
    c := SkipPas(Posn, k, l, true);
    if c = '.' then
    begin
      inc(Posn);
      SkipPas(Posn, k, l, true);
      lbl2 := IDPas(Posn, l);
      if lbl2 <> '' then
      begin
        Result := Result + '.' + lbl2;
        psn := Posn;
      end;
    end;
  end;
end;

function ID(var psn: Integer; const l: string): string;
var
  c: char;
begin
  Result := '';
  if not CharInSet(Skip(psn, l), IndentifierChar1) then
    exit;
  while psn <= length(l) do
  begin
    c := l[psn];
    if not CharInSet(c, IndentifierChars) then
      break;
    Result := Result + c;
    inc(psn);
  end;
end;

function Quoted_(var psn: Integer; const l: string; q: char): string;
var
  c: char;
begin
  Result := '';
  c := l[psn];
  if c <> q then
    exit;
  Result := c;
  inc(psn);
  while psn <= length(l) do
  begin
    c := l[psn];
    Result := Result + c;
    inc(psn);
    if c = q then
      break; // eos
  end;
end;

function Quoted(var psn: Integer; const l: string; dq: Boolean): string;
var
  q { , c } : char;
begin
  // Result := '';
  if dq then
    q := '"'
  else
    q := #39;
  { c := } Skip(psn, l);
  Result := Quoted_(psn, l, q);
  (* TODO: extracted code
    if c <> q then
    exit;
    Result := c;
    inc(psn);
    while psn <= length(l) do
    begin
    c := l[psn];
    Result := Result + c;
    inc(psn);
    if c = q then
    break;  // eos
    end;
  *)
end;

function Number_(var psn: Integer; const l: string): string;
var
  c: char;
begin
  Result := '';
  if not CharInSet(Skip(psn, l), ['$', '0' .. '9']) then
    exit;
  if l[psn] <> '$' then
  begin
    while psn <= length(l) do
    begin
      c := l[psn];
      if not CharInSet(c, ['0' .. '9']) then
        exit;
      Result := Result + c;
      inc(psn);
    end;
  end
  else
  begin
    // is hex
    inc(psn);
    if not CharInSet(l[psn], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z']) then
      exit;
    Result := '$';
    while psn <= length(l) do
    begin
      c := l[psn];
      if not CharInSet(c, ['0' .. '9', 'A' .. 'Z', 'a' .. 'z']) then
        exit;
      Result := Result + c;
      inc(psn);
    end;
  end;
end;

function Number(var psn: Integer; const l: string): Integer;
var
  s: string;
begin
  Result := 0;
  s := Number_(psn, l);
  if s <> '' then
    Result := StrToInt(s);
end;

constructor TTokenizer.Create(Strs: tstrings);
begin
  FOwnsStrs := Strs <> nil;
  FTheStrs := Strs;
end;

procedure TTokenizer.AfterConstruction;
begin
  inherited;
  if not FOwnsStrs then
    FTheStrs := TStringList.Create;
end;

function TTokenizer.Alpha(var next: char): string;
begin
  {Next :=} scan.Alpha(Result, CurrentLine, FPosn);
  next := NextChar(False);
end;

function TTokenizer.At_Safe: Char;
begin
  Result := scan.At_safe(CurrentLine, FPosn);
end;

procedure TTokenizer.BeforeDestruction;
begin
  if not FOwnsStrs then
    FTheStrs.Free;
  inherited;
end;

procedure TTokenizer.Clear;
begin
  FTheStrs.Clear;
end;

function TTokenizer.Count: integer;
begin
  Result := FTheStrs.Count;
end;

function TTokenizer.FindIdent(const theIdent: string): Boolean;
var
  txt: string;
begin
  repeat
    txt := Token(False);
  until (txt = '') or (CompareText(txt, theIdent) = 0);
  Result := txt <> '';
end;

function TTokenizer.FindSymbol(const Symbol: string): boolean;
var
  txt: string;
begin
  repeat
    txt := Token(True);
  until (txt = '') or (CompareStr(txt, Symbol) = 0);
  Result := txt <> '';
end;

function TTokenizer.GetCurrentLine: string;
begin
  Result := FTheStrs[LineNo];
end;

function TTokenizer.GetLine(Index: Integer): string;
begin
  Result := '';
  if (Index >= 0) and (Index < FTheStrs.Count) then
    Result := FTheStrs[index];
end;

function TTokenizer.LoadFromFile(const FName: string): Integer;
begin
  FTheStrs.LoadFromFile(FName);
  Result := Count;
end;

procedure TTokenizer.SetTheStrs(const Value: TStrings);
begin
//  FTheStrs := Value;
end;

function TTokenizer.Token(any: Boolean = False): string;
begin
  Result := scan.Token(FLineNo, FPosn, FTK, TheStrs, any);
end;

function TTokenizer.NextChar(Prep: Boolean): char;
begin
  Result := NextPas(FLineNo, FPosn, FTK, TheStrs, Prep);
end;

function TTokenizer.Number(var next: Char): integer;
begin
  Result := scan.Number(FPosn, CurrentLine);
  next := NextChar(False);
end;

function TTokenizer.Number: integer;
begin
  Result := scan.Number(FPosn, CurrentLine);
end;

function TTokenizer.NumberAsString: string;
begin
  Result := scan.Number_(FPosn, CurrentLine);
end;

function TTokenizer.NumberAsString(var next: char): string;
begin
  Result := scan.Number_(FPosn, CurrentLine);
  next := NextChar(False);
end;

end.
