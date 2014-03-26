unit Idents;

//  Idents.pas - reader for ZipMsgUS.rc and ZipMsg.h

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
//modified 2011-10-16

interface

uses
  classes, windows, ZMMsg, Template, LocList;

const
  MinID = 1;//6 * (DT_Language div 16);
  // MaxID = 15 + (16 * (TM_SystemError div 16));
  MaxID      = 15 + (16 * (11699 div 16));
  NamedRes   = $40000000;
  UnknownStr = $20000000;

type
{$IFDEF UNICODE}
  TZMString     = string; // unicode
  TZMWideString = string;
  TZMRawBytes   = RawByteString;
{$ELSE}
{$IFNDEF VERD6up}
  UTF8String = type string;
{$ENDIF}
  TZMString     = AnsiString; // Ansi/UTF8 depending upon UseUTF8
  TZMWideString = WideString;
  TZMRawBytes   = AnsiString;
{$ENDIF}
  // const
  // MIN_ID = 10095;
  // MAX_ID = 11630;

const
  Err_Ok         = 0;
  Err_Nothing    = 1;
  Err_Bad        = 2;
  Err_NoIDs      = 3;
  Err_BadIDs     = 4;
  Err_NoStrings  = 5;
  Err_BadStrings = 6;
  Err_Syntax     = 7;
  Err_Missing    = 8;
  Err_Unknown    = -1;

type
  TIdFlag  = (idNone, idMissing, idDiscard, idActive, idShared, idSfx);//, idRenumbered);
  TIdFlags = set of TIdFlag;

type
  TRCTokens = (rcNone, rcComma, rcEOS, rcEnd, rcStart, rcSymbol, rcIdentifier,
    rcLitNumber, rcQString);

  TTocValues = record
    Num: Integer;
    Str: string;
    WS: TZMWideString;
  end;

  TIdEntry = class(TObject)
  private
    FComment: string;
    fFlag: TIdFlag;
    fId: Integer;
    fMsg: WideString;
  public
    destructor Destroy; OVERRIDE;
    property Comment: string read FComment write FComment;
    property Flag: TIdFlag READ fFlag WRITE fFlag;
    property Id: Integer READ fId WRITE fId;
    property Msg: WideString READ fMsg WRITE fMsg;
  end;

type
  TIdentifiers = class
  private
    FCountryInfo: TLocInfo;
    FCP: Integer;
    FUsingLCID: Cardinal;
    function GetComment(Idx: Integer): string;
    function IsValidDefString(const wstr: TZMWideString): boolean;
    procedure SetComment(Idx: Integer; const Value: string);
    procedure SetCountryInfo(const Value: TLocInfo);
    procedure SetUsingLCID(const Value: Cardinal);
    procedure TemplaterMsg(Sender: TTemplater; const msg: String);
    function _Get_Ident(var TxtP: PAnsiChar): String;
    function _Get_LitNumber(var TxtP: PAnsiChar): Integer;
    function _Get_Lit_Hex(var TxtP: PAnsiChar): Integer;
    function _Get_Lit_Octal(var TxtP: PAnsiChar): Integer;
    function _Get_QString(var TxtP: PAnsiChar): TZMWideString;
    procedure _Skip_Comment(var TxtP: PAnsiChar);
    procedure _Skip_EOL(var TxtP: PAnsiChar);
  protected
    fList: TStringList;
    /// /    function WriteToStream(dest: TStream; what: TidFlags;
    /// /      wid: Boolean): Integer;
    procedure FixCP(N, M: Integer);
    function GetToken(var TxtP: PAnsiChar; var Value: TTocValues): TRCTokens;
// TODO: Renumbered
//  function Renumbered(Id: Integer): WideString;
    function TheLocaleAsStr: string;
  public
    constructor Create;
    destructor Destroy; OVERRIDE;
    function AddId(const N: String; Id: Integer): Integer;
    function AddIdent(const N: String; Id: Integer; aFlag: TIdFlag; const aMsg:
        WideString; const aComment: string): Integer;
    procedure Clear;
    function CountOf(flgs: TIdFlags): Integer;
    procedure EmptyMsgs;
    function GetCount: Integer;
    function GetFlag(idx: Integer): TIdFlag;
    function GetId(idx: Integer): Integer;
    function GetMsg(idx: Integer): WideString;
    function GetName(idx: Integer): String;
    function IndexAt(N: Integer; what: TIdFlags): Integer;
    function IndexOfId(Id: Integer): Integer;
    function IndexOfName(const N: String): Integer;
    function LoadHFile(const f: String): Integer;
    function MsgOfId(Id: Integer): String;
    function NameOfId(Id: Integer): String;
    function ReadRCFile(const f: String; def: boolean): Integer;
    function ReadRESFile(const f: String): Integer;
    procedure SetFlag(idx: Integer; const Value: TIdFlag);
    procedure SetId(idx: Integer; const Value: Integer);
    procedure SetMsg(idx: Integer; const Value: WideString);
    procedure SetName(idx: Integer; const Value: String);
    function WriteHFile(const Where: string): integer;
    function WriteRCFile(const Where: string): Integer;
    property Comment[Idx: Integer]: string read GetComment write SetComment;
    property Count: Integer READ GetCount;
    property CountryInfo: TLocInfo read FCountryInfo write SetCountryInfo;
    property CP: Integer read FCP write FCP;
    property Flag[idx: Integer]: TIdFlag READ GetFlag WRITE SetFlag;
    property Id[idx: Integer]: Integer READ GetId WRITE SetId;
    property Msg[idx: Integer]: WideString READ GetMsg WRITE SetMsg;
    property Name[idx: Integer]: String READ GetName WRITE SetName;
    property UsingLCID: Cardinal read FUsingLCID write SetUsingLCID;
  end;

implementation

uses
  SysUtils, Main;//, ToolHelper;
 (*
type
  TLangs = record
    s: String;
    l: Integer;
    c: Integer;
  end;

const
  std = 1252;

  LangCPs: array [0 .. 28] of TLangs = (
    (s: 'BG: Bulgarian'; l: $0402; c: 1251),
    (s: 'BR: Brazilian Portuguese'; l: $0416; c: std),
    (s: 'CN: Chinese'; l: $0804; c: 936),
    (s: 'CT: Catalan'; l: $0403; c: std),
    (s: 'CZ: Czech'; l: $0405; c: 1250),
    (s: 'DE: German'; l: $0407; c: std),
    (s: 'DK: Danish'; l: $0406; c: std),
    (s: 'ES: Spanish'; l: $040A; c: std),
    (s: 'FI: Finnish'; l: $040B; c: std),
    (s: 'FR: French'; l: $040C; c: std),
    (s: 'GR: Greek'; l: $0408; c: 1253),
    (s: 'HU: Hungarian'; l: $040E; c: 1250),
    (s: 'ID: Indonesian'; l: $0421; c: std),
    (s: 'IT: Italian'; l: $0410; c: std),
    (s: 'JP: Japanese'; l: $0411; c: 932),
    (s: 'KO: Korean'; l: $0412; c: 949),
    (s: 'MY: Malaysian'; l: $043E; c: 874),
    (s: 'NL: Dutch'; l: $0413; c: std),
    (s: 'NO: Norwegian'; l: $0414; c: std),
    (s: 'PL: Polish'; l: $0415; c: 1250),
    (s: 'RO: Romanian'; l: $0418; c: std),
    (s: 'RU: Russian'; l: $0419; c: 1251),
    (s: 'S1: Spanish'; l: $0C0A; c: std),
    (s: 'SI: Slovenian'; l: $0424; c: 1250),
    (s: 'SL: Slovenian'; l: $0424; c: 1250),
    (s: 'SW: Swedish'; l: $041D; c: std),   // should be SV
    (s: 'TW: Taiwanese'; l: $0404; c: 950),
    (s: 'US: US English'; l: $0409; c: std),
    (s: 'YU: Serbian'; l: $081A; c: 1250)
    // , (s: 'AU: AU English'; l: $0C09; c: std)
    );

function FindLangCP(Lang: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := Low(LangCPs) to High(LangCPs) do
    if LangCPs[I].l = Lang then
    begin
      Result := LangCPs[I].c;
      Break;
    end;
end;
 *)
function MBToWideEx(const MB: PAnsiChar; CP: cardinal; len: Integer)
  : TZMWideString;
var
  cnt: Integer;
  wcnt: Integer;
begin
  Result := '';
  if len = 0 then
    exit;
  wcnt := MultiByteToWideChar(CP, 0, MB, len, nil, 0);
  if wcnt > 0 then
  begin
    SetLength(Result, wcnt);
    cnt := MultiByteToWideChar(CP, 0, MB, len, pWideChar(Result), wcnt);
    if cnt < 1 then
      Result := ''; // oops - something went wrong
  end
  else
    RaiseLastOSError;
end;

function WideToMB(const WStr: TZMWideString; CP: Cardinal): string;
var
  cnt: Integer;
  Len: Integer;
  Tmp: AnsiString;
begin
  Tmp := '';
  Len := Length(WStr);
//  cnt := 0;
  if Len > 0 then
  begin
    cnt := WideCharToMultiByte(CP, 0, PWideChar(WStr), Len, nil, 0, nil, nil);
    if cnt > 0 then
    begin
      SetLength(Tmp, cnt);
      cnt := WideCharToMultiByte(CP, 0, PWideChar(WStr), Len,
        PAnsiChar(Tmp), cnt, nil, nil);
      if cnt < 1 then
        Tmp := '';  // oops - something went wrong
    end
    else
      RaiseLastOSError;
  end;
  Result := string(Tmp);
end;

function HexStr(hx: Cardinal): string;
begin
  Result := IntToHex(hx, 4);
  while (Length(Result) > 1) and (Result[1] = '0') do
    Delete(Result, 1, 1);
  if hx > 9 then
    Result := '0x' + Result;
end;

constructor TIdentifiers.Create;
begin
  fList := TStringList.Create;
  fCountryInfo := TLocInfo.Create;
end;

destructor TIdentifiers.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  fCountryInfo.Free;
  inherited;
end;

{ TIdentifiers }

function TIdentifiers.AddId(const N: String; Id: Integer): Integer;
var
  obj: TIdEntry;
begin
  obj := TIdEntry.Create;
  obj.Id := Id;
  obj.Flag := idActive;
  obj.Msg := '';
  Result := fList.AddObject(N, obj);
end;

{ TIdentifiers }

function TIdentifiers.AddIdent(const N: String; Id: Integer; aFlag: TIdFlag;
    const aMsg: WideString; const aComment: string): Integer;
var
  obj: TIdEntry;
begin
  obj := TIdEntry.Create;
  obj.Id := Id;
  obj.Flag := aFlag;
  obj.Msg := aMsg;
  obj.Comment := aComment;
  Result := fList.AddObject(N, obj);
end;

procedure TIdentifiers.Clear;
var
  I: Integer;
  obj: TIdEntry;
begin
  for I := 0 to pred(fList.Count) do
  begin
    obj := TIdEntry(fList.Objects[I]);
    if assigned(obj) then
    begin
      fList.Objects[I] := Nil;
      obj.Msg := '';
      obj.Comment := '';
      obj.Free;
    end;
  end;
  fList.Clear;
end;

function TIdentifiers.CountOf(flgs: TIdFlags): Integer;
var
  I: Integer;
  obj: TIdEntry;
begin
  Result := 0;
  for I := 0 to pred(fList.Count) do
  begin
    obj := TIdEntry(fList.Objects[I]);
    if assigned(obj) and (obj.Flag in flgs) and (obj.Msg <> '') then
      Result := Result + 1;
  end;
end;

procedure TIdentifiers.EmptyMsgs;
var
  I: Integer;
  obj: TIdEntry;
begin
  for I := 0 to pred(fList.Count) do
  begin
    obj := TIdEntry(fList.Objects[I]);
    if assigned(obj) then
    begin
      obj.Msg := '';
    end;
  end;
end;

procedure TIdentifiers.FixCP(N, M: Integer);
var
  Loc: Integer;
//  NewCP: Integer;
begin
  Loc := (M shl 10) + N;
  if Loc <> 0 then
  begin
    UsingLCID := Loc;   // updates CP also
  end;
end;

function TIdentifiers.GetComment(Idx: Integer): string;
var
  obj: TIdEntry;
begin
  Result := '';
  if (idx >= 0) and (idx < fList.Count) then
  begin
    obj := TIdEntry(fList.Objects[idx]);
    if assigned(obj) then
      Result := obj.Comment;
  end;
end;

function TIdentifiers.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TIdentifiers.GetFlag(idx: Integer): TIdFlag;
var
  obj: TIdEntry;
begin
  Result := idNone;
  if (idx >= 0) and (idx < fList.Count) then
  begin
    obj := TIdEntry(fList.Objects[idx]);
    if assigned(obj) then
      Result := obj.Flag;
  end;
end;

function TIdentifiers.GetId(idx: Integer): Integer;
var
  obj: TIdEntry;
begin
  Result := 0;
  if (idx >= 0) and (idx < fList.Count) then
  begin
    obj := TIdEntry(fList.Objects[idx]);
    if assigned(obj) then
      Result := obj.Id;
  end;
end;

function TIdentifiers.GetMsg(idx: Integer): WideString;
var
  obj: TIdEntry;
begin
  Result := '';
  if (idx >= 0) and (idx < fList.Count) then
  begin
    obj := TIdEntry(fList.Objects[idx]);
    if assigned(obj) then
      Result := obj.Msg;
  end;
end;

function TIdentifiers.GetName(idx: Integer): String;
begin
  Result := '';
  if (idx >= 0) and (idx < fList.Count) then
  begin
    Result := fList[idx];
  end;
end;

function TIdentifiers.GetToken(var TxtP: PAnsiChar; var Value: TTocValues)
  : TRCTokens;
var
  Ch: AnsiChar;
begin
  Result := rcNone;
  Value.Num := -1;
  Value.Str := '';
  Value.WS := '';
  while Result = rcNone do
  begin
    Ch := TxtP^;
    Inc(TxtP);
    case Ch of
      #0:
        Result := rcEOS;
      'A' .. 'Z', 'a' .. 'z', '_':
        begin
          Value.Str := _Get_Ident(TxtP);
          Result := rcIdentifier;
        end;
      '0':
        begin
          if (TxtP^ = 'x') or (TxtP^ = 'X') then
            Value.Num := _Get_Lit_Hex(TxtP)
          else
            Value.Num := _Get_Lit_Octal(TxtP);
          Result := rcLitNumber;
        end;
      '1' .. '9':
        begin
          Value.Num := _Get_LitNumber(TxtP);
          Result := rcLitNumber;
        end;
      '"':
        begin
          Value.WS := _Get_QString(TxtP);
          Result := rcQString;
        end;
      '/':
        begin
          if TxtP^ = '/' then
            _Skip_EOL(TxtP)
          else
            if TxtP^ = '*' then
              _Skip_Comment(TxtP)
            else
            begin
              Value.Str := Char(Ch);
              Result := rcSymbol;
            end;
        end;
      '#':
        _Skip_EOL(TxtP); // ignore
      ',':
        Result := rcComma;
      #9, ' ':
        ; // nothing _ ignore
      #10:
        begin
          if TxtP^ = #13 then
            Inc(TxtP);
          // Inc(LineNo);
        end;
      '{':
        Result := rcStart;
      '}':
        Result := rcEnd;
      '+', '-', '*':
        begin
          Value.Str := Char(Ch);
          Result := rcSymbol;
        end;
    end;
  end;
end;

function TIdentifiers.IndexAt(N: Integer; what: TIdFlags): Integer;
begin
  for Result := 0 to pred(Count) do
  begin
    if (Flag[Result] in what) and (Msg[Result] <> '') then
    begin
      if N = 0 then
        exit;
      dec(N);
    end;
  end;
  Result := -1;
end;

function TIdentifiers.IndexOfId(Id: Integer): Integer;
var
  I: Integer;
  obj: TIdEntry;
begin
  Result := -1;
  for I := 0 to pred(fList.Count) do
  begin
    obj := TIdEntry(fList.Objects[I]);
    if assigned(obj) and (obj.Id = Id) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TIdentifiers.IndexOfName(const N: String): Integer;
begin
  Result := fList.IndexOf(N);
end;

function TIdentifiers.IsValidDefString(const wstr: TZMWideString): boolean;
var
  I: Integer;
  wch: WideChar;
begin
  Result := True;
  for I := 1 to Length(wstr) do
  begin
    wch := wstr[I];
    if ord(wch) > 126 then
    begin
      Result := False;
      Break;
    end;
  end;
end;

function TIdentifiers.LoadHFile(const f: String): Integer;
var
  Cmnt: string;
  Flg: TIdFlag;
  I: Integer;
  idList: TStringList;
//  ix: Integer;
  j: Integer;
  s: String;
  slen: Integer;
  sn: String;
  sv: String;
  sw: Char;
  v: Integer;
begin
//  Result := 0;
  try
    Clear;
    idList := TStringList.Create;
    idList.LoadFromFile(f);
    // clean file
    if idList.Count < 190 then
    begin
      Result := -1;
      exit;
    end;
    for I := 0 to pred(idList.Count) do
    begin
      s := trim(idList[I]);
      slen := Length(s);
      if (slen < 15) or (Uppercase(copy(s, 1, 8)) <> '#DEFINE ') then
        continue;
      sn := '';
      j := 8;
      while j <= slen do
      begin
        if CharInSet(s[j], [#9, ' ']) then
        begin
          if sn <> '' then
            Break;
        end
        else
          sn := sn + s[j];
        Inc(j);
      end;
      sv := '';
      while j <= slen do
      begin
        if CharInSet(s[j], [#9, ' ']) then
        begin
          if sv <> '' then
            Break;
        end
        else
          sv := sv + s[j];
        Inc(j);
      end;
      v := StrToIntDef(sv, -1);
      if (sn <> '') and (v >= MinID) and (v <= MaxID) then
      begin
        // check for switch
        Cmnt := '';
        sw := #0;
        while (j <= (slen - 3)) and CharInSet(s[j], [#9, ' ']) do
          Inc(j);
        if (j <= (slen - 2)) and (s[j] = '/') and (s[j + 1] = '/') then
        begin
          sw := s[j + 2];
          Cmnt := Copy(s, j, 255);
        end;
        case sw of
//          '!':
//            Flg := idShared;
//          '*':
//            Flg := idSfx;
          '-':
            Flg := idDiscard;
//          'A'..'Z':
//            Flg := idRenumbered;
        else
          Flg := idActive;
        end;
        AddIdent(sn, v, Flg, '', Cmnt);
      end;
    end;
    Result := fList.Count;
  finally
    FreeAndNil(idList);
  end;
end;

(*
  // tot, siz1, tbl1, tbl2, 0
  function TIdentifiers.SaveAsBlock(strm: TStream; wid: Boolean): Integer;
  var
  sz1, sz2: Integer;
  ms:  TMemoryStream;
  w:   Word;
  begin
  Result := -1;
  if CountOf([idActive .. idSFX]) < 1 then
  exit;
  ms := TMemoryStream.Create;
  try
  sz1 := WriteToStream(ms, [idActive], wid);
  sz2 := WriteToStream(ms, [idShared, idSFX], wid);
  w := sz1 + sz2 + 2;   // total words excluding total size
  strm.WriteBuffer(w, sizeof(Word));
  Result := w + 1;
  w := sz1;
  strm.WriteBuffer(w, sizeof(Word));
  ms.Position := 0;
  strm.CopyFrom(ms, ms.Size);
  w := 0; // end mark
  strm.WriteBuffer(w, sizeof(Word));
  finally
  FreeAndNil(ms);
  end;
  end; *)

function TIdentifiers.MsgOfId(Id: Integer): String;
var
  I: Integer;
begin
  I := IndexOfId(Id);
  if I < 0 then
    Result := ''
  else
    Result := Msg[I]; // converts to Ansi
end;

function TIdentifiers.NameOfId(Id: Integer): String;
var
  I: Integer;
begin
  I := IndexOfId(Id);
  if I < 0 then
    Result := ''
  else
    Result := Name[I];
end;

// handles LANGUAGE and code page differences
function TIdentifiers.ReadRCFile(const f: String; def: boolean): Integer;
var
  Err: string;
  ident: string;
  idx: Integer;
  M: Integer;
  Missing: Integer;
  N: Integer;
  NeedTok: TRCTokens;
  Tok: TRCTokens;
  TokValue: TTocValues;
  RCText: TMemoryStream;
  TxtP: PAnsiChar;
  wstr: TZMWideString;
begin
  if Count < 190 then
  begin
    ToolSupp.Show('no identifiers');
    Result := Err_NoIDs;
    exit;
  end;
  Missing := 11630; // first ID for missing IDs
  RCText := TMemoryStream.Create;
  try
    RCText.LoadFromFile(f);
//    Clear;
    EmptyMsgs;
    Result := 0;
	CP := 0; // use default unless stated
    NeedTok := rcIdentifier;
    Tok := rcNone;
    TxtP := PAnsiChar(RCText.Memory);
    while TxtP^ <> #0 do
    begin
      Tok := GetToken(TxtP, TokValue);
      if Tok = rcEOS then
        Break; // end of stream
      if Tok <> rcIdentifier then
        continue; // ignore
      if CompareText(TokValue.Str, 'STRINGTABLE') <> 0 then
        continue;
      Tok := GetToken(TxtP, TokValue);
      if Tok <> rcStart then
      begin
        // must be language
        if (Tok <> rcIdentifier) or {and}
          (CompareText(TokValue.Str, 'LANGUAGE') <> 0) then
        begin
          ToolSupp.Show('Expecting LANGUAGE');
          Result := Err_Syntax;
          Break;
        end;
        // need Locale, sub-locale
        Tok := GetToken(TxtP, TokValue);
        if Tok <> rcLitNumber then
        begin
          ToolSupp.Show('Expecting locale');
          Result := Err_Syntax;
          Break;
        end;
        N := TokValue.Num;
        Tok := GetToken(TxtP, TokValue);
        if Tok <> rcComma then
        begin
          ToolSupp.Show('Expected ,');
          Result := Err_Syntax;
          Break;
        end;
        Tok := GetToken(TxtP, TokValue);
        if Tok <> rcLitNumber then
        begin
          ToolSupp.Show('Expecting locale');
          Result := Err_Syntax;
          Break;
        end;
        M := TokValue.Num;
        if CP = 0 then
          FixCP(N, M);
        Tok := GetToken(TxtP, TokValue);
      end;
      if Tok <> rcStart then
      begin
        ToolSupp.Show('Expected {');
        Result := Err_Syntax;
      end;
      Break;
    end;
    if Tok = rcStart then
    begin
      idx := -1;
      while TxtP^ <> #0 do
      begin
        Tok := GetToken(TxtP, TokValue);
        if (Tok = rcEOS) or (Tok = rcEnd) then
          Break; // end of stream
//        if Tok <> NeedTok then
        if (Tok <> NeedTok) and not ((Tok = rcQString) and (NeedTok = rcComma)) then
        begin
          case NeedTok of
            rcIdentifier:
              Err := 'needed Identifier';
            rcComma:
              Err := 'needed ,';
            rcQString:
              Err := 'needed "string"';
          else
            Err := 'syntax error';
          end;
          ToolSupp.Show(Err);
          Result := Err_Syntax;
          Break;
        end;
        if Tok = rcComma then
        begin
          NeedTok := rcQString;
          continue;
        end;
        // have identier or string
        if Tok = rcIdentifier then
        begin
          NeedTok := rcComma;
          ident := TokValue.Str;
          idx := IndexOfName(ident);
          if idx < 0 then
          begin
            ToolSupp.Show('unknown identifier ' + ident);
//            idx := AddId(ident, Missing);//11630);
            idx := AddIdent(ident, Missing, idMissing, '', '// missing');
            Inc(Missing);
//            Flag[idx] := idMissing;
//            Result := Err_BadStrings;
            continue;
          end;
        end
        else
        begin
          NeedTok := rcIdentifier;
          wstr := TokValue.WS;
          if def then
          begin
            if not IsValidDefString(wstr) then
            begin
              ToolSupp.Show('Invalid default string ' + ident + ' ' + wstr);
              Result := Err_BadStrings;
              Break;
            end;
          end;
          if idx >= 0 then
            Msg[idx] := wstr;
          // RCStrings.Add(FList[idx], wstr);
          idx := -1;
        end;
      end;
    end;
  finally
    RCText.Free;
  end;
end;

function TIdentifiers.ReadRESFile(const f: String): Integer;
var
  c: array [0 .. 1] of cardinal;
  dat: cardinal;
  fs: TFileStream;
  fsz: Int64;
  head: array of Word;
  I: cardinal;
  ix: Integer;
  j: cardinal;
  l: cardinal;
  rid: cardinal;
  s: String;
  sbuf: array of Char;
  sz: cardinal;
  WS: WideString;
begin
  Result := 0;
  EmptyMsgs;
  fs := Nil;
  try
    fs := TFileStream.Create(f, fmOpenRead);
    fsz := fs.Size;
    SetLength(head, 100);
    while (fs.Position + 8) < fsz do
    begin
      fs.Position := (fs.Position + 3) and $7FFFFFFC; // dword align
      fs.Read(c[0], 8);
      sz := c[1] + c[0] - 8;
      if High(head) < Integer(sz div 2) then
        SetLength(head, (sz div 2) + 8);
      fs.Read(head[0], sz);
      if head[0] <> $FFFF then
      begin
        Result := -1;
        ToolSupp.Show('bad res type');
        continue;
      end;
      if head[1] <> 6 then
        continue; // not string table
      if head[2] <> $FFFF then
      begin
        Result := Result or NamedRes;
        ToolSupp.Show('named ids');
        continue;
      end;
      rid := pred(head[3]) * 16;
      dat := (c[1] - 8) div 2;
      WS := '';
      j := 0;
      for I := 0 to 15 do
      begin
        l := head[dat + j];
        Inc(j);
//        if l < 1 then
//          WS := ''
//        else
        if l > 0 then
        begin
          SetLength(WS, l);
          move(head[dat + j], WS[1], l * sizeof(WideChar));
          Inc(j, l);
          ix := IndexOfId(rid + I);
          if ix < 0 then // missing
          begin
            Result := Result or UnknownStr;
            s := '??_' + IntToStr(rid + I);
//            ix := AddId(s, rid + I);
//            Flag[ix] := idMissing;
            ix := AddIdent(s, rid + I, idMissing, '', '// missing');
          end;
          Msg[ix] := WS;
          Inc(Result);
        end;
      end;
    end;
  finally
    head := Nil;
    sbuf := Nil;
    FreeAndNil(fs);
  end;
end;

// TODO: Renumbered
//function TIdentifiers.Renumbered(Id: Integer): WideString;
//var
//I: Integer;
//obj: TIdEntry;
//begin
//Result := '';
//I := 0;
//case Id of
//  ZC_OK:
//    I := 10152; // PW_Ok;
//  ZC_Cancel:
//    I := 10153; // PW_Cancel;
//  ZC_CancelAll:
//    I := 10157; // PW_CancelAll;
//  ZC_Abort:
//    I := 10158; // PW_Abort;
//end;
//if I > 0 then
//begin
//  I := IndexOfId(I);
//  if I > 0 then
//  begin
//    obj := TIdEntry(fList.Objects[I]);
//    if assigned(obj) then
//      Result := obj.Msg;
//  end;
//end;
//end;

procedure TIdentifiers.SetComment(Idx: Integer; const Value: string);
var
  obj: TIdEntry;
begin
  if (idx >= 0) and (idx < fList.Count) then
  begin
    obj := TIdEntry(fList.Objects[idx]);
    if assigned(obj) then
      obj.Comment := Value;
  end;
end;

procedure TIdentifiers.SetCountryInfo(const Value: TLocInfo);
begin
  FCountryInfo := Value;
end;


procedure TIdentifiers.SetFlag(idx: Integer; const Value: TIdFlag);
var
  obj: TIdEntry;
begin
  if (idx >= 0) and (idx < fList.Count) then
  begin
    obj := TIdEntry(fList.Objects[idx]);
    if assigned(obj) then
      obj.Flag := Value;
  end;
end;

procedure TIdentifiers.SetId(idx: Integer; const Value: Integer);
var
  obj: TIdEntry;
begin
  if (idx >= 0) and (idx < fList.Count) then
  begin
    obj := TIdEntry(fList.Objects[idx]);
    if assigned(obj) then
      obj.Id := Value;
  end;
end;

procedure TIdentifiers.SetMsg(idx: Integer; const Value: WideString);
var
  obj: TIdEntry;
begin
  if (idx >= 0) and (idx < fList.Count) then
  begin
    obj := TIdEntry(fList.Objects[idx]);
    if assigned(obj) then
      obj.Msg := Value;
  end;
end;

procedure TIdentifiers.SetName(idx: Integer; const Value: String);
begin
  if (idx >= 0) and (idx < fList.Count) then
  begin
    fList[idx] := Value;
  end;
end;

procedure TIdentifiers.SetUsingLCID(const Value: Cardinal);
var
  Idx: Integer;
  NewCP: Integer;
begin
  if FUsingLCID <> Value then
  begin
    FUsingLCID := Value;
    Idx := CountryInfo.IndexOfID(Value);
    NewCP := 0;
    if Idx >= 0 then
      NewCP := CountryInfo.Page[Idx];
    ToolSupp.Show('Found Language ' + IntToHex(Value, 4) + ' code page ' +
      IntToStr(NewCP) + '  :' + CountryInfo.Country[Idx]);
    if NewCP <> 0 then
      CP := NewCP;
  end;
end;

procedure TIdentifiers.TemplaterMsg(Sender: TTemplater; const msg: String);
begin
  ToolSupp.Show('** ' + msg);
end;

function TIdentifiers.TheLocaleAsStr: string;
var
  PrimaryID: Cardinal;
  SubID: Cardinal;
begin
  PrimaryID := UsingLCID and $3FF;
  SubID := UsingLCID shr 10;
  Result := HexStr(PrimaryID) + ', ' + HexStr(SubID);
  Result := Result + ' // 0x' + IntToHex(UsingLCID, 4);
end;

function TIdentifiers.WriteHFile(const Where: string): integer;
var
  Lines: TStringList;
  I: Integer;
  L: Integer;
  MaxLen: Integer;
  pn: string;
  S: string;
  Spaces: Integer;
  Tab: Integer;
begin
//  Result := -1;
  pn := ExtractFileName(where);
  ToolSupp.Show('Writing ' + pn);
  Lines := TStringList.Create;
  try
    Lines.Add('#ifndef ZipMsgH');
    Lines.Add('  #define ZipMsgH');
    ToolSupp.Licence(Lines, 'Generated', True);

    Lines.Add('// note special comments //! shared, //* sfx, //- discard, {//= moved}');
    Lines.Add(' ');
    MaxLen := 0;
    for I := 0 to Count - 1 do
    begin
      L := Length(Name[I]);
      if L > MaxLen then
        MaxLen := L;
    end;
    Tab := 10 + MaxLen + 2;
    for I := 0 to Count - 1 do
    begin
      S := '  #define ';
      S := S + Name[I];
      Spaces := Tab - Length(S);
      S := S + StringOfChar(' ', Spaces);
      S := S + IntToStr(Id[I]);
      if Comment[I] <> '' then
        S := S + ' ' + Comment[I];
      Lines.Add(S);
    end;
    Lines.Add(' ');
    S := '  #define DT_Last';
    Spaces := Tab - Length(S);
    S := S + StringOfChar(' ', Spaces);
    S := S + IntToStr(Id[Count - 1]);
    Lines.Add(S);

    Lines.Add(' ');
    Lines.Add('#endif');
{$IFDEF CompilerVersion >= 18.0}
    Lines.SaveToFile(GlobalSyms(where), TEncoding.Default);
{$ELSE}
    Lines.SaveToFile(ToolSupp.Path(where));
{$ENDIF}
    Result := 0;
  finally
    Lines.Free;
  end;
end;

function TIdentifiers.WriteRCFile(const Where: string): Integer;
var
  pn: String;
  templater: TTemplater;
  tpt: String;
  i: Integer;
  N: integer;
  WS: TZMWideString;
{$IFDEF CompilerVersion >= 18.0}
  ReqEncoding: TEncoding;
{$ENDIF}
begin
  if Count < 190 then
  begin
    ToolSupp.Show('no identifiers');
    Result := Err_NoIDs;
    exit;
  end;
  pn := ExtractFileName(where);
  ToolSupp.Show('Writing ' + pn);
  tpt := ToolSupp.Path('templates/Lang_rc.tpt');
  templater := TTemplater.Create;
  try
    templater.OnShowMsg := TemplaterMsg;
    templater.FileName := pn;
    templater.Version := 191;
    templater.Defines.Add('LANG=' + Msg[IndexOfName('DT_Language')]);
    templater.Defines.Add('LANGUAGE_VALUE=' + TheLocaleAsStr);
    N := 0;
    for i := 0 to Count - 1 do
    begin
      WS := Msg[I];
      if WS <> '' then
      begin
        if Flag[I] in [idActive{, idRenumbered}, idShared] then
        begin
          templater.Defines.Add('IDENTS_'+ IntToStr(N)+ '=' + Name[I]);
{$IFDEF CompilerVersion >= 18.0}
          templater.Defines.Add('MSGS_'+ IntToStr(N)+ '=' + WS);
{$ELSE}
          templater.Defines.Add('MSGS_'+ IntToStr(N)+ '=' + WideToMB(WS, CP));
{$ENDIF}
          Inc(N);
        end;
      end;
    end;
    Result := templater.Process(tpt);

    ToolSupp.BackupFile(where);
{$IFDEF CompilerVersion >= 18.0}
  ReqEncoding := TEncoding.GetEncoding(CP);
  try
    templater.dest.SaveToFile(GlobalSyms(where), ReqEncoding);//TEncoding.Default);
  finally
    ReqEncoding.Free;
  end;
{$ELSE}
    templater.dest.SaveToFile(ToolSupp.Path(where));
{$ENDIF}
//    Result := 0;
  finally
    FreeAndNil(templater);
  end;
end;

function TIdentifiers._Get_Ident(var TxtP: PAnsiChar): String;
begin
  Result := '';
  dec(TxtP);
  while CharInSet(TxtP^, ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_']) do
  begin
    Result := Result + Char(TxtP^);
    Inc(TxtP);
  end;
end;

function TIdentifiers._Get_LitNumber(var TxtP: PAnsiChar): Integer;
var
  c: AnsiChar;
  d: Integer;
  Done: boolean;
begin
  Result := 0;
  Done := False;
  dec(TxtP); // need first digit
  while not Done do
  begin
    c := TxtP^;
    if c in ['0' .. '9'] then
    begin
      d := ord(c) - ord('0');
      Result := (Result * 10) + d;
      Inc(TxtP);
    end
    else
      Done := True;
  end;
end;

function TIdentifiers._Get_Lit_Hex(var TxtP: PAnsiChar): Integer;
var
  c: AnsiChar;
  d: Integer;
  Done: boolean;
begin
  Result := 0;
  Inc(TxtP); // skip 'x'
  Done := False;
  d := 0;
  while not Done do
  begin
    c := TxtP^;
    case c of
      '0' .. '9':
        d := ord(c) - ord('0');
      'A' .. 'F':
        d := 10 + ord(c) - ord('A');
      'a' .. 'f':
        d := 10 + ord(c) - ord('a');
      'h', 'H':
        begin
          Inc(TxtP);
          Done := True;
        end
    else
      Done := True;
    end;
    if not Done then
    begin
      Result := (Result * 16) + d;
      Inc(TxtP);
    end;
  end;
end;

function TIdentifiers._Get_Lit_Octal(var TxtP: PAnsiChar): Integer;
var
  c: AnsiChar;
  d: Integer;
  Done: boolean;
begin
  Result := 0;
  Done := False;
  while not Done do
  begin
    c := TxtP^;
    if c in ['0' .. '7'] then
    begin
      d := ord(c) - ord('0');
      Result := (Result * 8) + d;
      Inc(TxtP);
    end
    else
      Done := True;
  end;
end;

function TIdentifiers._Get_QString(var TxtP: PAnsiChar): TZMWideString;
var
  Ch: AnsiChar;
  Stt: PAnsiChar;
begin
  Result := '';
  Stt := TxtP;
  while TxtP^ <> #0 do
  begin
    Ch := TxtP^;
    Inc(TxtP);
    if Ch = '"' then
      Break; // end of quoted
    if (Ch = '\') and (TxtP^ = '"') then
      Inc(TxtP); // keep both
  end;
  if TxtP > Stt then
    Result := MBToWideEx(Stt, CP, (TxtP - Stt) - 1)
end;

procedure TIdentifiers._Skip_Comment(var TxtP: PAnsiChar);
var
  Ch, ch0, ch1: AnsiChar;
begin
  Ch := 'x';
  repeat
    ch0 := Ch;
    Inc(TxtP);
    Ch := TxtP^;
    if Ch = #0 then
      Break;
    if (Ch = '''') or (Ch = '"') then
    begin // ignore Literal Chars except terminated at eol
      ch1 := Ch;
      repeat
        Inc(TxtP);
        Ch := TxtP^;
      until (Ch in [#0, #10, #13]) or (Ch = ch1);
    end;
  until (ch0 = '*') and (Ch = '/');
  Inc(TxtP);
end;

procedure TIdentifiers._Skip_EOL(var TxtP: PAnsiChar);
begin
  while not(TxtP^ in [#0, #10, #13]) do
    Inc(TxtP);
end;

function LCIDToCodePage(ALcid: LCID): Cardinal;
var
  Buf: array[0..10] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDefaultAnsiCodePage, Buf, 10);
  Result := StrToIntDef(Buf, 0);
end;

{ TIdEntry }

destructor TIdEntry.Destroy;
begin
  fMsg := '';
  inherited;
end;

end.
