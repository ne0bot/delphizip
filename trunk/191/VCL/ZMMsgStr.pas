unit ZMMsgStr;

//  ZMMsgStr.pas - global string language handler
//	This file is part of TZipMaster Version 1.9.

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
//modified 2011-11-20

{$I   '.\ZipVers.inc'}

interface

uses
  Classes, ZipMstr, ZMCompat;

var
  OnZMStr: TZMLoadStrEvent;

function LoadZipStr(id: Integer): String;

// '*' = Auto, '' = default US,  language  (number = language id)
function SetZipMsgLanguage(const zl: String): String;
// get language at index (<0 - default, 0 - current)
function GetZipMsgLanguage(idx: Integer): String;
function SetZipMsgLanguageID(ReqID: Cardinal): Cardinal;
function GetZipMsgLanguageID: Cardinal;
// info (-1) = language id, 0 = name, other values as per windows LOCALE_
function GetZipMsgLanguageInfo(idx: Integer; info: Cardinal): String;

function LanguageIdent(const seg: string): string;
function LocaleInfo(loc: Integer; info: Cardinal): String;

{$IFNDEF UNICODE}
  function UsingUTF8: Boolean;
  function SetZipUseUTF8(UseUTF8: Boolean): Boolean;
{$ENDIF}

// prevent changes whilst busy
procedure SetZipBusy(IsBusy: Boolean);

implementation

{$I   '.\ZMConfig191.inc'}

uses
  Windows, SysUtils,
{$IFDEF UNICODE}
//  AnsiStrings,
{$ELSE}
  ZMUTF8,
{$ENDIF}
  ZMUtils, ZMMsg, ZMDefMsgs, ZMInflt;

const
  __UNIT__ = 23 shl 23;

{$IFNDEF VERD6up}
type
  PCardinal = ^Cardinal;
{$ENDIF}

(* format of strings blok
  Size: Word                            [0]
  Stamp: Cardinal // time stamp DOS date[1]
  FlagUTF: Word   // non-zero = UTF16   [3]
  Ranges                                [4]
  {
    Max: Word
    Min: Word
    BaseOffset: Word
  }[]
  RangeEnd: Word // $FFFF
  Offsets: Word[] // contains offset to start of msg
  Msgs: Char[]  // zero terminated messages Ansi/UTF16
*)
type
  TZMRangeEntry = packed record
    MaxVal: Word;
    MinVal: Word;
    Base: Word;
  end;
  TZMRangeArray = packed array of TZMRangeEntry;
  TOffsetsArray = packed array of Word;

  TZMLangBlok = packed array of AnsiChar;

type
  TLangBlokHeader = packed record
    LID: Word;
    DataSize: Word;
    LIdent: array [0..3] of AnsiChar;
  end;
  TLangBlokHeaderEx = packed record
    LID: Word;
    DataSize: Word;
    LIdent: array [0..3] of AnsiChar;
    CRC: DWORD;
  end;

const
  SResourceMissingFor = 'Resource missing for ';
  SUSDefault = 'US: default';
  lchars = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_'];
  Uchars = ['a' .. 'z'];
  Digits = ['0' .. '9'];
  Letters = ['A' .. 'Z'];

var
{$IFDEF USE_COMPRESSED_STRINGS}
  DefRes: TZMLangBlok; // default strings
{$ENDIF}
  SelRes: TZMLangBlok; // selected strings
  SelId: Cardinal;
  SelName: string;
  Setting: Boolean;
  GUsingUTF8: Boolean;

function LanguageIdent(const seg: string): string;
var
  c: Char;
  i: Integer;
begin
  Result := '';
  for i := 1 to length(seg) do
  begin
    c := seg[i];
    if not CharInSet(c, lchars) then
    begin
      if (Result <> '') or (c <> ' ') then
        break;
    end
    else
    begin
      if (Result = '') and CharInSet(c, Digits) then
        break; // must not start with digit
      if CharInSet(c, Uchars) then
        c := Char(Ord(c) - $20); // convert to upper
      Result := Result + c;
    end;
  end;
end;

// format is
// id: word, data_size: word, label: AnsiChar[4], data: byte[]
// stream at id
// returns id
function LoadFromStream(var blk: TZMLangBlok; src: TStream): Cardinal;
var
  Header: TLangBlokHeaderEx;
  r: Integer;
  so: TMemoryStream;
  szw: Integer;
begin
  blk := nil; // empty it
  Result := 0;
  if src.Read(Header, sizeof(TLangBlokHeaderEx)) <> sizeof(TLangBlokHeaderEx) then
    exit;
  if src.Size < (Header.DataSize + (3 * sizeof(Word))) then
    exit;
  try
    so := TMemoryStream.Create;
//    r := LZ77Extract(so, src, Header.DataSize);
    r := UndeflateQ(so, src, Header.DataSize - sizeof(DWORD),
      8, Header.CRC);
    if (r = 0) and (so.Size < 20000) then
    begin
      szw := (Integer(so.Size));
      SetLength(blk, szw);
      so.Position := 0;
      if so.Read(blk[0], so.Size) = so.Size then
        Result := Header.LID
      else
        blk := nil;
    end;
  finally
    FreeAndNil(so);
  end;
end;

// format is
// id: word, data_size: word, label: char[4], data: byte[];... ;0
// positions stream to point to id
function FindLIDInStream(src: TStream; var SegName: string; var LangId: Word):
    Boolean;
var
  Base: Word;
  c: AnsiChar;
  FirstMaybe: Integer;
  Header: TLangBlokHeader;
  I: Integer;
  p: Int64;
  ss: Int64;
begin
  Result := False;
  if not assigned(src) then
    exit;
  FirstMaybe := -1; // first close match position
  p := src.Position;
  ss := src.Size - sizeof(TLangBlokHeader); // max position possible
  while (not Result) and (p < ss) do
  begin
    src.Position := p;
    src.ReadBuffer(Header, sizeof(TLangBlokHeader));
    Base := Header.LID and $3FF; // base LCID
    if (FirstMaybe < 0) and (Base = (LangId and $3FF)) then
      FirstMaybe := Integer(p);  // in case no exact match
    Result := (LangId = Header.LID) or ((LangId < $400) and (Base = LangId));
    if not Result then
      p := src.Position + Header.DataSize; // skip data to next entry
  end;
  if (not Result) and (FirstMaybe >= 0) then
  begin
    Result := True;
    p := FirstMaybe;
    src.Position := FirstMaybe;
    src.ReadBuffer(Header, sizeof(TLangBlokHeader));
  end;
  if Result then
  begin
    SegName := '';
    for I := 0 to 3 do
    begin
      c := Header.LIdent[I];
      if c = #0 then
        Break;
      SegName := SegName + Char(c);
    end;
    LangId := Header.LID;
    src.Position := p;
  end;
end;

// format is
// id: word, data_size: word, label: char[4], data: byte[];... ;0
// positions stream to point to id
function FindLangInStream(src: TStream; var SegName: string; var LangId: Word):
    Boolean;
var
  c: AnsiChar;
  Ch: Char;
  CloseMatch: Integer;
  CloseMaybe: Integer;
  Good: boolean;
  Header: TLangBlokHeader;
  p: Int64;
  Req: packed array [0..3] of AnsiChar;
  ss: Int64;
  I: Integer;
  M: Integer;
  ReqLen: Integer;
begin
  Result := False;
  if not assigned(src) then
    exit;
  ReqLen := 0;
  for I := 1 to 4 do
  begin
    c := #0;
    if I <= Length(SegName) then
    begin
      Ch := SegName[I];
      if CharInSet(ch, Uchars) then
        ch := Char(Ord(ch) - $20);
      Good := CharInSet(Ch, Letters);
      if (I > 2) and not Good then
        Good := CharInSet(Ch, Digits);
      if Good then
      begin
        c := AnsiChar(ch);
        Inc(ReqLen);
      end;
    end;
    Req[I-1] := c;
  end;
  if ReqLen < 2 then
    exit;
  CloseMatch := -1;
  CloseMaybe := -1; // first close match position
  p := src.Position;
  ss := src.Size - sizeof(TLangBlokHeader); // max position possible
  while (not Result) and (p < ss) do
  begin
    src.Position := p;
    src.ReadBuffer(Header, sizeof(TLangBlokHeader));
    Result := (Req[0] = Header.LIdent[0]) and (Req[1] = Header.LIdent[1]);
    if Result then
    begin
      if Result then
      begin
        M := 2;
        if ReqLen > 2 then
        begin
          Result := Req[2] = Header.LIdent[2];
          if Result then
          begin
            Inc(M);
            if ReqLen = 4 then
            begin
              Result := Req[3] = Header.LIdent[3];
              if Result then
                Inc(M);
            end;
          end;
        end;
        if (M <> ReqLen) and (M > CloseMatch) then
        begin
          CloseMatch := M;
          CloseMaybe := Integer(p);  // in case no exact match
        end;
      end;
    end;
    if not Result then
      p := src.Position + Header.DataSize; // skip data to next entry
  end;
  if (not Result) and (CloseMaybe >= 0) then
  begin
    Result := True;
    p := CloseMaybe;
    src.Position := CloseMaybe;
    src.ReadBuffer(Header, sizeof(TLangBlokHeader));
  end;
  if Result then
  begin
    SegName := '';
    for I := 0 to 3 do
    begin
      c := Header.LIdent[I];
      if c = #0 then
        Break;
      SegName := SegName + Char(c);
    end;
    LangId := Header.LID;
    src.Position := p;
  end;
end;

// format is
// lcid: word, data_size: word, label: AnsiChar[4], data: byte[];
// positions stream to point to id
function IdInStream(src: TStream; var idx: Cardinal; var lang: String): Boolean;
var
  Heder: TLangBlokHeader;
  p: Int64;
//  s: Ansistring;
  ss: Int64;
begin
  Result := False;
  if (idx < 1) or not assigned(src) then
    exit;
  p := src.Position;
  ss := src.Size - (sizeof(TLangBlokHeader) + 20); // header + 20 bytes
  if p > ss then
    exit;
  repeat
    src.ReadBuffer(Heder, sizeof(TLangBlokHeader)); // id, dsize, nsize
    if idx <= 1 then
      break;
    Dec(idx);
    p := p + Heder.DataSize + sizeof(TLangBlokHeader); // next header
    if p < ss then
      src.Position := p
    else
      exit;
  until False;
  lang := String(PAnsiChar(@Heder.LIdent));
  idx := Heder.LID;
  src.Position := p;
  Result := True;
end;

{$IFNDEF VERD6up}
function TryStrToInt(const s: String; var v: Integer): Boolean;
begin
  if (s = '') or not CharInSet(s[1], ['0' .. '9', '$']) then
    Result := False
  else
  begin
    Result := True;
    try
      v := StrToInt(s);
    except
      on EConvertError do
        Result := False;
    end;
  end;
end;
{$ENDIF}

function SetZipMsgLanguage(const zl: String): String;
var
  id: Word;
  len: Integer;
  LangName: string;
  newBlock: TZMLangBlok;
  newId: Cardinal;
  newres: TZMLangBlok;
  res: TResourceStream;
begin
  if zl = '' then
    exit;
  res := nil;
  LangName := LanguageIdent(zl);
  if (length(LangName) < 2) then
  begin
    if zl = '*' then
    begin
      id := GetUserDefaultLCID;
      SetZipMsgLanguageID(id);
      Result := SelName;
      if Result = '' then
        Result := 'US';
    end;
    Exit;
  end;
  if (LangName <> 'US') and (id <> $0409) then // use default US
  begin
    try
      Setting := True;
      SelRes := nil;  // reset to default
      SelId := 0;
      SelName := '';
      Result := '';
      id := 0;
      res := OpenResStream(DZRES_Str, RT_RCData);
      if assigned(res) and FindLangInStream(res, LangName, id) then
      begin
        newId := LoadFromStream(newBlock, res);
        if newId > 0 then
        begin
          len := length(newBlock);
          SetLength(newres, len);
          Move(newBlock[0], newres[0], len);
          Result := String(LangName);
          SelRes := newres;
          SelName := LangName;
          SelId := newId;
        end;
      end;
    finally
      Setting := False;
      FreeAndNil(res);
    end;
  end;
end;

function LocaleInfo(loc: Integer; info: Cardinal): String;
var
  s: String;
begin
  if (loc <= 0) or (loc = $400) then
    loc := LOCALE_USER_DEFAULT;
  SetLength(s, 1024);
  GetLocaleInfo(loc and $FFFF, info, PChar(s), 1023);
  Result := PChar(s); // remove any trailing #0
end;

// get language at Idx (<0 - default, 0 - current)
// info (-1) = language id, 0 = name, other values as per windows LOCALE_
function GetZipMsgLanguageInfo(idx: Integer; info: Cardinal): String;
var
  id: Cardinal;
  res: TResourceStream;
  s: String;
begin
  id := $0409;
  Result := SUSDefault; // default US English
  if (idx = 0) and (SelRes <> nil) then
  begin
    Result := String(SelName);
    id := SelId;
  end;
  if idx > 0 then
  begin
    res := nil;
    Result := '><';
    id := idx and $FF;
    try
      res := OpenResStream(DZRES_Str, RT_RCData);
      if assigned(res) and IdInStream(res, id, s) then
        Result := s;
    finally
      FreeAndNil(res);
    end;
  end;
  if Result <> '><' then
  begin
    if info = 0 then
      Result := '$' + IntToHex(id, 4)
    else if info <> Cardinal(-1) then
      Result := LocaleInfo(id, info);
  end;
end;

function SetZipMsgLanguageID(ReqID: Cardinal): Cardinal;
var
  id: Word;
  len: Integer;
  LangName: string;
  newBlock: TZMLangBlok;
  newId: Cardinal;
  newres: TZMLangBlok;
  res: TResourceStream;
begin
  Result := SelId;
  if Setting then
    exit;
  res := nil;
  try
    Setting := True;
    SelRes := nil; // reset to default
    SelId := 0;
    SelName := '';
    Result := $0409;
    id := ReqID;
    if (ReqID = $0409) or (ReqID > $0FFFF) then
    begin
      Exit; // default
    end;
    LangName := '';
    res := OpenResStream(DZRES_Str, RT_RCData);
    if assigned(res) and FindLIDInStream(res, LangName, id) then
    begin
      newId := LoadFromStream(newBlock, res);
      if newId > 0 then
      begin
        len := length(newBlock);
        SetLength(newres, len);
        Move(newBlock[0], newres[0], len);
        SelRes := newres;
        SelName := LangName;
        SelId := newId;
        Result := SelId;
      end;
    end;
  finally
    Setting := False;
    FreeAndNil(res);
  end;
end;

function GetZipMsgLanguageID: Cardinal;
begin
  if SelRes <> nil then
    Result := SelId
  else
    Result := $0409;
end;

// get language at index (<0 - current, 0 - default, >0 - index)
function GetZipMsgLanguage(idx: Integer): String;
begin
  Result := GetZipMsgLanguageInfo(idx, Cardinal(-1));
end;

function ZMLoadStr(const Table: TZMLangBlok; Id: Cardinal): string;
{$Q-}
var
  AMsg: PAnsiChar;
  IsAnsi: boolean;
  Offset: Integer;
  P: PWORD;
  PTable: pointer;
  WMsg: PWideChar;
begin
  Result := ''; // not found
  if Length(Table) < 50 then
    Exit;
  PTable := @Table[0];
  P := PWORD(PTable);
  Inc(P, 3); // => Ansi/UTF16 flag
  IsAnsi := P^ = 0;
  Inc(P);// => Ranges[0].MaxVal
  while Id > P^ do
    Inc(P, SizeOf(TZMRangeEntry) div SizeOf(WORD));
  if P^ = $FFFF then
    Exit; // not found
  Inc(P); // => MinVal
  Offset := Id - P^;
  if Offset < 0 then
    Exit; // not in range
  Inc(P); // => Base
  Offset := P^ + Offset; // index of Offsets entry
  P := PWORD(PTable);
  Inc(P, Offset);
  Offset := P^;
  if Offset <> 0 then
  begin
    if IsAnsi then
    begin
      AMsg := PAnsiChar(PTable);
      Inc(AMsg, Offset);
      Result := string(AMsg);
    end
    else
    begin
      WMsg := PWideChar(PTable);
      Inc(WMsg, Offset);
      Result := string(WMsg);
    end;
  end;
end;

{$IFNDEF USE_COMPRESSED_STRINGS}
function FindConst(id: Integer): String;
var
  p: pResStringRec;

  function Find(idx: Integer): pResStringRec;
  var
    wi: Word;
    i: Integer;
  begin
    Result := nil;
    wi := Word(idx);
    for i := 0 to high(ResTable) do
      if ResTable[i].i = wi then
      begin
        Result := PResStringRec(ResTable[i]).s;
        break;
      end;
  end;

begin { FindConst }
  Result := '';
  p := Find(id);
  if p <> nil then
    Result := LoadResString(p);
end;
{$ELSE}

// format is
//// id: word, data_size: word, label_size: word, label: char[], data: byte[];... ;0
// id: word, data_size: word, label: AnsiChar[4], data: byte[];... ;0
// block/stream format comp_size: word, size: word, data[comp_size]:bytes ????
function LoadCompressedDef(const src): Integer;
var
  ms: TMemoryStream;
  DataSize: Word;
  pw: PWord;
begin
  Result := -1;
  pw := @src;
  if pw^ = $0409 then
  begin
    inc(pw);
    DataSize := pw^;
    Inc(DataSize, SizeOf(TLangBlokHeader));
    ms := TMemoryStream.Create;
    try
      ms.Write(src, DataSize); // copy to stream
      ms.Position := 0;
      Result := LoadFromStream(DefRes, ms);
    finally
      FreeAndNil(ms);
    end;
  end;
end;
{$ENDIF}

// returns String
function LoadZipStr(id: Integer): String;
var
  blk: TZMLangBlok;
  d: String;
  MsgId: Integer;
  tmpOnZipStr: TZMLoadStrEvent;
begin
  Result := '';
  MsgId := id and MSG_ID_MASK;  // remove possible extended info
  blk := SelRes;
  if blk <> nil then
    Result := ZMLoadStr(blk, MsgId);
{$IFDEF USE_COMPRESSED_STRINGS}
  if Result = '' then
  begin
    if DefRes = nil then
      LoadCompressedDef(CompBlok);
    blk := DefRes;
    Result := ZMLoadStr(blk, MsgId);
  end;
{$ELSE}
  if Result = '' then
    Result := FindConst(MsgId);
{$ENDIF}
  tmpOnZipStr := OnZMStr;
  if assigned(tmpOnZipStr) then
  begin
    d := Result;
    tmpOnZipStr(MsgId, d);
    if d <> '' then
      Result := d;
  end;
  if Result = '' then
    Result := SResourceMissingFor + IntToStr(id);
end;

function UsingUTF8: Boolean;
begin
  result := GUsingUTF8;
end;

function SetZipUseUTF8(UseUTF8: Boolean): Boolean;
begin
  {$IFDEF UNICODE}
    result := False;
  {$ELSE}
    {$IFNDEF SUPPORT_OLD_WINDOWS}
      result := GUsingUTF8;
      // Win 95, 98, ME does not have UTF8 support
      GUsingUTF8 := UseUTF8 and (Win32PlatForm = Ver_Platform_Win32_NT);
    {$ELSE}
      result := False;
    {$ENDIF}
  {$ENDIF}
end;

// prevent changes whilst busy
procedure SetZipBusy(IsBusy: Boolean);
begin
  { TODO -oRussell -c : implement 6/11/2010 9:17:22 AM }
end;

initialization
  OnZMStr := nil;
  Setting := False;
  GUsingUTF8 := False;
{$IFDEF USE_COMPRESSED_STRINGS}
  DefRes := nil;
{$ENDIF}
  SelRes := nil;

finalization
{$IFDEF USE_COMPRESSED_STRINGS}
  DefRes := nil; // force destruction
{$ENDIF}
  SelRes := nil;

end.
