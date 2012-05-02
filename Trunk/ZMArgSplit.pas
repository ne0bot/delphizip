unit ZMArgSplit;

//  ZMArgSplit.pas - Split command strings

(* ***************************************************************************
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

{$I   '.\ZipVers.inc'}

interface

uses
  Classes, ZipMstr;

//const
//  ZPasswordArg: Char = '<';
//  ZSwitch: Char      = '/';
//  ZSpecArg: Char     = '>';
//  MAX_ARG_LENGTH     = 2048;
//  ZFILE_SEPARATOR    = '>>';

type
  TZASErrors = (zasNone, zasIgnored, zasInvalid, zasDuplicate, zasUnknown,
    zasDisallowed, zasBad);
  TZArgOpts = (zaoLastSpec, zaoWildSpec, zaoMultiSpec);
  TZArgOptions = set of TZArgOpts;

type
  TZMArgSplitter = class(TZMWorkBin)
  private
    FAllow: string;
    fArgs: TStringList;
    FError: TZASErrors;
    FErrorMsg: string;
    FFound: string;
    FMain: string;
    FOptions: TZArgOptions;
    FRaw: string;
    procedure AnotherArg(Option: Char; const arg: string);
    procedure SetAllow(const Value: string);
    procedure SetRaw(const Value: string);
  protected
    function CheckCompression(var arg: string): TZASErrors;
    function CheckExcludes(var arg: string): TZASErrors;
    function CheckFolder(var arg: string): TZASErrors;
    function CheckInclude(var arg: string): TZASErrors;
    function CheckXlatePath(var arg: string): TZASErrors;
    function GetArg(var idx: Integer; AllowPW: Boolean): string;
    function HandleMain(s: string): Boolean;
    function HandleSwitch(TheSwitch: String): Boolean;
    function Index(Option: Char): Integer;
    procedure SplitRaw;
    function UpperOp(Option: Char): Char;
  public
    procedure AfterConstruction; override;
    function arg(Option: Char): string; overload;
    function arg(Index: Integer): String; overload;
    procedure BeforeDestruction; override;
    procedure Clear;
    function Has(Option: Char): Boolean;
    property Allow: string read FAllow write SetAllow;
    property Error: TZASErrors read FError write FError;
    property ErrorMsg: string read FErrorMsg;
    property Found: string read FFound;
    property Main: string read FMain;
    property Options: TZArgOptions read FOptions write FOptions;
    property Raw: string read FRaw write SetRaw;
  end;

implementation

uses
  SysUtils, Windows, ZMUtils, ZMStructs;

const
  __UNIT__ = 2 shl 23;

const
  KnownArgs  = ['>', '<', 'C', 'E', 'F', 'N', 'S', 'X'];
  DupAllowed = [];
  {
    switches
    >>spec                  select spec
    <password               use password (must be last)
    C:n                     set compression to n
    E:[|][spec[|spec...]]   exclude spec if starts | added to global
    F:folder                set folder
    N[+ or -]               no rename with AddNewName (default +)
    S[+ or -]               sub-directories (default+)
    X:[old]::[new]          translate old to new
  }
  MAX_ARG_LENGTH     = 2048;

procedure TZMArgSplitter.AfterConstruction;
begin
  inherited;
  fArgs := TStringList.Create;
end;

procedure TZMArgSplitter.AnotherArg(Option: Char; const arg: string);
begin
  FFound := FFound + Option;
  fArgs.Add(arg);
end;

function TZMArgSplitter.arg(Option: Char): string;
var
  idx: Integer;
begin
  Result := '';
  idx := Index(Option);
  if idx < 0 then
    Exit; // not valid - how was it found?
  Result := fArgs[idx];
end;

function TZMArgSplitter.arg(Index: Integer): String;
begin
  Result := '';
  if (Index >= 0) and (Index < fArgs.Count) then
    Result := fArgs[Index];
end;

procedure TZMArgSplitter.BeforeDestruction;
begin
  fArgs.Free;
  inherited;
end;

// must be 0..9
function TZMArgSplitter.CheckCompression(var arg: string): TZASErrors;
begin
  if (Length(arg) = 1) and (arg[1] >= '0') and (arg[1] <= '9') then
    Result := zasNone
  else
    Result := zasBad;
end;

// allowed empty
function TZMArgSplitter.CheckExcludes(var arg: string): TZASErrors;
begin
  Result := zasNone;
  arg := WinPathDelimiters(arg);
end;

// empty returns current dir
function TZMArgSplitter.CheckFolder(var arg: string): TZASErrors;
var
  tmp: string;
begin
  Result := zasNone;
  if (arg = '') or (arg = '.') then
    tmp := GetCurrentDir
  else
    tmp := WinPathDelimiters(arg);
  if NameIsBad(tmp, False) then
    Result := zasBad
  else
    arg := tmp;
end;

// must have file spec
function TZMArgSplitter.CheckInclude(var arg: string): TZASErrors;
var
  first: string;
  rest: string;
  tmp: string;
begin
  Result := zasNone;
  tmp := WinPathDelimiters(arg);
  first := tmp;
  if (zaoMultiSpec in Options) and (Pos('|', first) > 0) then
  begin
    while first <> '' do
    begin
      first := ZSplitString('|', first, rest);
      if NameIsBad(first, zaoWildSpec in Options) then
      begin
        Result := zasBad;
        Exit;
      end;
      first := rest;
    end;
    arg := tmp;
  end
  else
  begin
    if NameIsBad(tmp, zaoWildSpec in Options) then
      Result := zasBad
    else
      arg := tmp;
  end;
end;

// format :[orig_path]::[new_path]
// paths must not include drive
function TZMArgSplitter.CheckXlatePath(var arg: string): TZASErrors;
var
  new_path: string;
  nposn: Integer;
  orig_path: string;
  tmp: string;
begin
  if Length(arg) < 2 then
  begin
    Result := zasInvalid;
    Exit;
  end;
  tmp := arg;
  nposn := Pos('::', tmp);
  if nposn < 1 then
  begin
    Result := zasInvalid;
    Exit;
  end;
  tmp := SetSlash(tmp, psdInternal);
  orig_path := Trim(Copy(tmp, 1, nposn - 1));
  new_path := Trim(Copy(tmp, nposn + 2, MAX_ARG_LENGTH));
  if (orig_path = '') and (new_path = '') then
  begin
    Result := zasIgnored;
    Exit;
  end;
  // TODO check paths valid if not empty
  arg := orig_path + '::' + new_path;  // rebuild it ??
  Result := zasNone; // good
end;

procedure TZMArgSplitter.Clear;
begin
    FAllow := '';
    fArgs.Clear;
    FError := zasNone;
    FErrorMsg := '';
    FFound := '';
    FMain := '';
    FRaw := '';
end;

function TZMArgSplitter.GetArg(var idx: Integer; AllowPW: Boolean): string;
var
  ch: Char;
  Spaces: Integer;
  lastchar: Integer;
  Start: Integer;
  len: Integer;
  nxt: Integer;
begin
  Result := '';
  if idx < 1 then
    idx := 1;
  len := Length(FRaw);
  if idx > len then
  begin
    idx := -1; // passed end
    Exit;
  end;
  Spaces := 0;
  // skip leading
  while idx <= len do
  begin
    if FRaw[idx] > ' ' then
      break;
    Inc(idx);
  end;
  if idx > len then
  begin
    idx := -1;
    Exit; // nothing valid found
  end;
  if (AllowPW and (FRaw[idx] = ZPasswordArg)) then
  begin
    // pass to end of line
    Result := Copy(FRaw, idx, len - pred(idx));
    idx := -1;
    Exit;
  end;

  // advance to next, find the length ignoring trailing space
  nxt := idx;
  Start := idx;
  while nxt <= len do
  begin
    ch := FRaw[nxt];
    if (ch = ZSwitch) and (Spaces > 0) then
      break; // at next switch
    if AllowPW and (ch = ZPasswordArg) then
      break; // at next (Comment)
    if (ch <= ' ') then
    begin
      // skip but count space
      Inc(nxt);
      Inc(Spaces);
      continue;
    end;

    if (ch = '"') then
    begin
      // copy previous
      Result := Result + Copy(FRaw, Start, nxt - Start);
      Start := nxt;
      Inc(Start); // past leading quote
      // find end of quote
      while nxt <= len do
      begin
        Inc(nxt);
        if FRaw[nxt] = '"' then
          break;
      end;
      Result := Result + Copy(FRaw, Start, nxt - Start);
      Inc(nxt);
      Start := nxt; // end quote
      Spaces := 0;
      continue;
    end;

    // just a character
    Inc(nxt);
    Spaces := 0;
  end;
  // copy previous
  lastchar := nxt - Spaces;
  if (lastchar > Start) then
    Result := Result + Copy(FRaw, Start, lastchar - Start);
  idx := idx + (nxt - idx);

  if idx > len then
    idx := -1;
end;

function TZMArgSplitter.HandleMain(s: string): Boolean;
var
  delimpos: Integer;
  I: Integer;
  tmp: string;
begin
  Result := False;
  FMain := s;
  if Pos(ZSpecArg, FAllow) < 1 then
    Exit;   // don't check
  if Pos(ZFILE_SEPARATOR, s) < 1 then
    Exit; // no sub spec
  // trim unwanted spaces
  while Pos(' >>', s) > 0 do
  begin
    I := Pos(' >>', s);
    s := Copy(s, 1, I-1) + Copy(s, I+1, 1024);
  end;
  while Pos('>> ', s) > 0 do
  begin
    I := Pos('>> ', s);
    s := Copy(s, 1, I+1) + Copy(s, I+3, 1024);
  end;
  if zaoLastSpec in Options then
  begin
    // Split at last
    SplitQualifiedName(s, s, tmp);
    FMain := s;
  end
  else
  begin
    // split at first
    delimpos := Pos(ZFILE_SEPARATOR, s);
    tmp := s;
    I := delimpos - 1;
    while (I > 0) and (s[I] <= ' ') do
      Dec(I);   // trim
    FMain := Copy(s, 1, I);
    I := delimpos + Length(ZFILE_SEPARATOR);
    while (I < Length(tmp)) and (s[I] <= ' ') do
      Inc(I);   // trim
    tmp := Copy(tmp, I, MAX_ARG_LENGTH);
  end;
  Error := CheckInclude(tmp);
  Result := Error <> zasNone;
  if not Result then
    AnotherArg('>', tmp)
  else
    FErrorMsg := tmp;
end;

function TZMArgSplitter.HandleSwitch(TheSwitch: String): Boolean;
var
  arg: string;
  c: Char;
  Opt: Char;
  sw: Integer;
begin
  Result := True;
  Error := zasDisallowed;
  FErrorMsg := TheSwitch;
  Opt := UpperOp(TheSwitch[2]); // option
  sw := Pos(Opt, FAllow) - 1;
  if sw < 0 then
    Exit;
{$IFDEF UNICODE}
  if (Pos(Opt, FFound) > 0) and not CharInSet(Opt, DupAllowed) then
{$ELSE}
  if (Pos(Opt, FFound) > 0) and not (Opt in DupAllowed) then
{$ENDIF}
  begin
    // duplicate
    Error := zasDuplicate;
    Exit; // fatal
  end;
  Error := zasInvalid;
  // we have wanted switch
  if (Opt = 'S') or (Opt = 'N') then
  begin
    // can be /s or /s+ or /s-
    if Length(TheSwitch) = 2 then
    begin
      TheSwitch := TheSwitch + '+'
    end;
    c := TheSwitch[3];
    if (Length(TheSwitch) = 3) and ((c = '+') or (c = '-')) then
    begin
      arg := c;
      Error := zasNone;
    end;
  end
  else
  begin
    // have form /?:
    if (Length(TheSwitch) < 4) or (TheSwitch[3] <> ':') then
    begin
      if Opt = 'E' then
      begin
        Error := zasNone;
        arg := SPEC_SEP;    // allow /E as short for /E:| (use default);
      end;
    end
    else
    begin
      arg := Trim(Copy(TheSwitch, 4, MAX_ARG_LENGTH));
      case Opt of
        'C':
          Error := CheckCompression(arg);
        'E':
          Error := CheckExcludes(arg);
        'F':
          Error := CheckFolder(arg);
        'X':
          Error := CheckXlatePath(arg);
      end;
    end;
  end;
//  if Error = zasNone then
  if Error <= zasIgnored then
  begin
    if Error <> zasIgnored then
      AnotherArg(Opt, arg);
    Result := False;
    FErrorMsg := '';
    Error := zasNone;
  end;
end;

function TZMArgSplitter.Has(Option: Char): Boolean;
begin
  Result := Pos(UpperOp(Option), FFound) > 0;
end;

function TZMArgSplitter.Index(Option: Char): Integer;
begin
  Option := UpperOp(Option);
  Result := Pos(Option, FFound) - 1;
end;

procedure TZMArgSplitter.SetAllow(const Value: string);
var
  ch: char;
  up: string;
  I: Integer;
begin
  up := UpperCase(Value);
  if FAllow <> up then
  begin
    for I := 1 to Length(up) do
    begin
      ch := up[I];
{$IFDEF UNICODE}
      if not CharInSet(ch, KnownArgs) then
{$ELSE}
      if not (ch in KnownArgs) then
{$ENDIF}
      begin
        Error := zasInvalid;
        FErrorMsg := up[I];
        Exit;
      end;
    end;
    FAllow := up;
    SplitRaw;
  end;
end;

procedure TZMArgSplitter.SetRaw(const Value: string);
begin
  if FRaw <> Value then
  begin
    FRaw := Value;
    SplitRaw;
  end;
end;

procedure TZMArgSplitter.SplitRaw;
var
  AllowPW: Boolean;
  c: Char;
  idx: Integer;
  s: string;
  stt: Integer;
begin
  fArgs.Clear;
  FFound := '';
  FMain := '';
  FErrorMsg := '';
  FError := zasNone;
  AllowPW := Pos(ZPasswordArg, FAllow) > 0;
  // split raw
  idx := 1;
  while idx > 0 do
  begin
    stt := idx;
    s := GetArg(idx, AllowPW);
    if s <> '' then
    begin
      c := s[1];
      if c = ZSwitch then
      begin
        if Length(s) < 2 then
        begin
          // invalid
          Error := zasInvalid;
          FErrorMsg := Copy(FRaw, stt, MAX_ARG_LENGTH);
          break; // fatal
        end;
        if HandleSwitch(s) then
          break; // fatal
      end
      else
        if c = ZPasswordArg then
          AnotherArg(ZPasswordArg, Copy(s, 2, MAX_ARG_LENGTH))
        else
          if HandleMain(s) then
            Break;  // fatal
    end;
  end;
end;

function TZMArgSplitter.UpperOp(Option: Char): Char;
begin
  if (Option >= 'a') and (Option <= 'z') then
    Result := Char(Ord(Option) - 32)
  else
    Result := Option;
end;

end.

