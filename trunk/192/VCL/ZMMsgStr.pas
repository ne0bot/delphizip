unit ZMMsgStr;

//  ZMMsgStr.pas - global string language handler
//	This file is part of TZipMaster Version 1.9.2.x

(* ***************************************************************************
TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009, 2010, 2011, 2012, 2013 Russell Peters and Roger Aelbrecht

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
//modified 2013-01-29

{$I   '.\ZipVers.inc'}

interface

uses
  {$IFDEF VERDXE2up}
    System.Classes,
  {$ELSE}
    Classes,
  {$ENDIF}
  ZipMstr, ZMBody, ZMlister, ZMZipReader, ZMUnzipOpr;

var
  OnZMStr: TZMLoadStrEvent;

function LoadZipStr(id: Integer): String;

//  [zip>>]'0';  // uses user default language or primary [in zip]
//  [zip>>]'*';  // uses user default language or primary [in zip]
//  [zip>>]'1023';  // uses language 1023 or primary [in zip]
//  [zip>>]'$XXXX';  // uses language XXXX or primary [in zip]
//  [zip>>]'LL';  // uses named language [in zip]
//  [zip>>]'pattern';  // uses first entry matching 'pattern' [in zip]
//  'file';  // file 'file'
// returns >0 strings loaded or <=0 error
//function SetZipMsgLanguage(const zl: String; Body: TZMBody): Integer;
{$IFDEF UNICODE}
{$DEFINE _NO_UTF8_}
{$ELSE}
{$IFDEF SUPPORT_OLD_WINDOWS}
{$DEFINE _NO_UTF8_}
{$ENDIF}
{$ENDIF}

{$IFNDEF _NO_UTF8_}
  function UsingUTF8: Boolean;
  function SetZipUseUTF8(UseUTF8: Boolean): Boolean;
{$ENDIF}

type
  TZMLangOpr = class(TZMUnzipOpr)
  private
    function FindById(ZM: TZMZipReader; HexId: String): TZMEntryBase;
    function LoadFromZip(DstStrs: TStrings; const ById, Req, LangsZip: string):
        Integer;
    function LoadLang(DstStrs: TStrings; const Lang: string; LangsZip: String):
        Integer;
    function LoadZipEntry(DstStrs: TStrings; Entry: TZMEntryBase): Integer;
  public
    // in Delphi can be protected or private
    function MergeLanguage(DstStrs: TStrings; const ReqName: string): Integer;
    function SetLanguage(const zl: String): integer;
  end;

function CurrentZipLanguage: string;

implementation

uses
  {$IFDEF VERDXE2up}
    WinApi.Windows, System.SysUtils, System.SyncObjs,
  {$ELSE}
    Windows, SysUtils,
    {$IFNDEF VERpre6}SyncObjs,{$ENDIF}
    {$IFNDEF UNICODE}
        ZMCompat,
    {$ENDIF}
  {$ENDIF}
  ZMUtils, ZMMsg, ZMDefMsgs, {ZMInflt,}ZMZStream, ZMUTF8;

const
  __UNIT__ = 19 shl 23;

const
  SResourceMissingFor = 'Resource missing for ';

var
  MsgStrs: TStrings;
  Setting: Boolean;
  CurrentLanguage: String;
{$IFNDEF _NO_UTF8_}
  GUsingUTF8: Boolean;
{$ENDIF}
{$IFDEF VERpre6}
    CSection: TRTLCriticalSection;
{$ELSE}
    Guard: TCriticalSection;
{$ENDIF}

function CurrentZipLanguage: string;
begin
  Result := CurrentLanguage;
end;

//function FindIndentifier(const Ident: string): integer;
//var
//  Hash: Cardinal;
//begin
//  Hash := HashFuncNoCase(Ident);
//  for Result := 0 to High(IdentHashes) do
//    if IdentHashes[Result] = Hash then
//      Exit;
//  Result := -1;
//end;

procedure FillEmptyStrs(DstStrs: TStrings);
var
  I: Integer;
begin
  DstStrs.Clear;
  DstStrs.Capacity := MAX_ID + 1;
  for I := 0 to MAX_ID do
    DstStrs.Add('');
end;

function GetLanguageID(AStr: AnsiString): Integer;
const
  NeededU: AnsiString = 'ZA_ID';
  NeededL: AnsiString = 'za_id';
var
  ACh: AnsiChar;
  L: string;
  Posn: Integer;
  Matching: Integer;
  MaxPosn: Integer;
begin
  Result := 0;
  Matching := 1;
  Posn := 0;
  if (Length(AStr) > 1) and (AStr[1] = '1') then
  begin
    Result := $0409;
    Exit;
  end;
  MaxPosn := Length(AStr) - (5 + Length(NeededU));
  while Posn <= MaxPosn do
  begin
    Inc(Posn);
    ACh := AStr[Posn];
    if (ACh = NeededU[Matching]) or (ACh = NeededL[Matching]) then
    begin
      Inc(Matching);
      if Matching > Length(NeededU) then
        Break; // found
      Continue;
    end;
    Matching := 1;
  end;
  if Matching > Length(NeededU) then
  begin
    while (Posn < Length(AStr)) and (AStr[Posn] <> '"') do
      Inc(Posn);
    Inc(Posn);
    L := '';
    while (Posn < Length(AStr)) and (AStr[Posn] <> '"') do
    begin
      L := L + Char(AStr[Posn]);
      Inc(Posn);
    end;
    if Length(L) > 3 then
    begin
      if (L[1] = '0') and ((L[2] = 'x') or (L[2] = 'X')) then
        L := '$' + Copy(L, 3, 4);
      Result := 0;
      if not TryStrToInt(L, Result) then
        Result := -1;
    end;
  end;
end;

function MergeStrs(DstStrs: TStrings; const ws: TZMWideString): Integer;
var
  sl: TStringList;
  I: Integer;
  Id: Integer;
  IdStr: string;
//  MaxId: Integer;
  psn: Integer;
  qs: string;
  qsp: PChar;
  s: string;
begin
  Result := 0;
//  MaxId := MAX_ID;
  sl := TStringList.Create;
  try
  {$IFNDEF _NO_UTF8_}
    if GUsingUTF8 then
      sl.Text := WideToUTF8(ws)
    else
  {$ENDIF}
      sl.Text := ws; // will convert if needed
    for I := 0 to sl.Count - 1 do
    begin
      s :=  sl[I];
      psn := Pos(',', s);
      if psn < 2 then
        continue;
      IdStr := Trim(Copy(s, 1, psn - 1));
      if IdStr = '' then
        continue;
      Id := FindIndentifier(IdStr);
      if Id < 0 then
      begin
        if not TryStrToInt(IdStr, Id) then
          Continue; // not found or invalid
      end
      else
        Inc(Id); // adjust index
      if Id > MAX_ID{MaxId} then
        continue;
      qs := Trim(Copy(s, psn + 1, Length(s)));
      s := qs;
      if (qs <> '') and (qs[1] = '"') then
      begin
        qsp := PChar(qs);
        s := AnsiExtractQuotedStr(qsp, '"');
      end;
      s := StringReplace(s, '\n', #13#10, [rfReplaceAll]);
      {Msg}DstStrs[Id] := s;
      Inc(Result);
    end;
  finally
    sl.Free;
  end;
end;

function LoadStrs(DstStrs: TStrings; ss: TStream; Body: TZMLangOpr): Integer;
var
  AStr: AnsiString;
  ws: TZMWideString;
  LID: Integer;
  pw: PWideChar;
begin
  Result := -1;
  if (ss.Size > 20000) or (ss.Size < 30) then
  begin
    if Body <> nil then
      Body.Reporter.Inform('Invalid language file');//, False);
    Exit;
  end;
  // read
  SetLength(AStr, ss.Size);
  ss.Position := 0;
  if ss.Read(PAnsiChar(AStr)^, ss.Size) <> ss.Size then
  begin
    Result := -21;
    Exit;
  end;
  // make guess at encoding
  if (AStr[2] = #0) or ((AStr[1] = #$FF) and (AStr[2] = #$FE)) then
  begin
    // Unicode
    pw := PWideChar(PAnsiChar(AStr));
    if AStr[2] = #$FE then
      Inc(pw);
    ws := pw;
  end
  else
  begin
    // Ansii
    // find ZA_ID
    LID := GetLanguageID(AStr);
    if LID <= 0 then
    begin
      // assume default
      ws := TZMWideString(AStr);
    end
    else
      ws := StrToWideEx(AStr, LID, Length(AStr));
  end;
  Result := MergeStrs(DstStrs, ws);
end;

procedure SwapMsgStrs(var DefStrs: TStrings);
var
  TmpStrs: TStrings;
begin
  if DefStrs <> nil then
  begin
    Setting := True;
{$IFDEF VERpre6}
  EnterCriticalSection(CSection);
{$ELSE}
  Guard.Enter;
{$ENDIF}
    TmpStrs := MsgStrs;
    MsgStrs := DefStrs;
    DefStrs := TmpStrs;
{$IFDEF VERpre6}
  LeaveCriticalSection(CSection);
{$ELSE}
    Guard.Leave;
{$ENDIF}
    Setting := False;
  end;
end;

//procedure ReadCompressedStrings(si: TMemoryStream);
//var
//  I: Integer;
//  K: Cardinal;
//begin
//  for I := 0 to High(CompBlok) do
//  begin
//    K := CompBlok[I];
//    si.WriteBuffer(K, SizeOf(Cardinal));
//  end;
//end;

//  [zip>>]'0';  // uses user default language or primary [in zip]
//  [zip>>]'*';  // uses user default language or primary [in zip]
//  [zip>>]'1023';  // uses language 1023 or primary [in zip]
//  [zip>>]'$XXXX';  // uses language XXXX or primary [in zip]
//  [zip>>]'LL';  // uses named language [in zip]
//  [zip>>]'pattern';  // uses first entry matching 'pattern' [in zip]
//  'file';  // file 'file'
function SetZipMsgLanguage(const zl: String; Opr: TZMLangOpr): integer;
var
//  I: Integer;
  CRC: Dword;
  DefStrs: TStrings;
  r: Integer;
  si: TMemoryStream;
  so: TMemoryStream;
//  K: Cardinal;
  ReqName: string;
begin
  DefStrs := TStringList.Create;
  try
    DefStrs.Capacity := MAX_ID + 1;
    si := TMemoryStream.Create;
    try
      (*TODO: extracted code
      for I := 0 to High(CompBlok) do
      begin
        K := CompBlok[I];
        si.WriteBuffer(K, SizeOf(Cardinal));
      end;
      *)
      ReadCompressedStrings(si);
      si.Position := 0;
      si.ReadBuffer(CRC, sizeof(DWord));
      so := TMemoryStream.Create;
      try
        r := UndeflateQ(so, si, si.Size - sizeof(DWORD), 8, CRC);
        if (r = 0) and (so.Size < 20000) then
        begin
          // success
          so.Position := 0;
          FillEmptyStrs(DefStrs);
          Result := LoadStrs(DefStrs, so, Opr);
          if (Result > 0) and (zl <> '') and (Opr <> nil) then
          begin
            ReqName := Unquote(zl);
            REsult := Opr.MergeLanguage(DefStrs, ReqName);
          end;
        end
        else
          Result := -1;
        if Result < 0 then
        begin
          if Opr <> nil then
            Opr.Reporter.Inform('Error unzipping language strings');
          FreeAndNil(DefStrs);
        end;
      finally
        FreeAndNil(so);
      end;
    finally
      si.Free;
    end;
  finally
    SwapMsgStrs(DefStrs);
    DefStrs.Free;
  end;
  if Result >= 0 then
    CurrentLanguage := zl;
end;

function LoadZipStr(id: Integer): String;
var
  d: String;
  MsgId: Integer;
  tmpOnZipStr: TZMLoadStrEvent;
begin
  Result := '';
  MsgId := AbsErr(id);  // remove possible extended info
  if (MsgID > 0) and (MsgID <= MAX_ID) then
  begin
    if Not Setting then
    begin
      if MsgStrs = nil then
        SetZipMsgLanguage('', nil);
      if (MsgStrs <> nil) and (MsgID < MsgStrs.Count) then
        Result := MsgStrs[MsgID];
    end;
  end;
  tmpOnZipStr := OnZMStr;
  if assigned(tmpOnZipStr) then
  begin
    d := Result;
    tmpOnZipStr(MsgId, d);
    if d <> '' then
      Result := d;
  end;
  if Result = '' then
    Result := SResourceMissingFor + IntToStr(MsgID);
end;

{$IFNDEF _NO_UTF8_}
function UsingUTF8: Boolean;
begin
  result := GUsingUTF8;
end;

function SetZipUseUTF8(UseUTF8: Boolean): Boolean;
begin
  // Win 95, 98, ME do not have UTF8 support
  GUsingUTF8 := UseUTF8 and (Win32PlatForm = Ver_Platform_Win32_NT);
  result := GUsingUTF8;
end;
{$ENDIF}

function TZMLangOpr.FindById(ZM: TZMZipReader; HexId: String): TZMEntryBase;// TZMDirEntry;
const
  Hexit = ['0' .. '9', 'a' .. 'f', 'A' .. 'F'];
var
  Ch: Char;
  Entry: TZMEntryBase;
  Full: TZMEntryBase;
  Near: TZMEntryBase;
  Num: string;
  PrimaryHigh: Integer;
  ToFind: string;
begin
  Result := nil;
  if Length(HexId) <> 4 then
    Exit;
  PrimaryHigh := Ord(HexId[2]) and 3; // high 2 bits of primary language
  Near := nil;
  Full := nil;
  Entry := nil;
  repeat
    // find any likely match of primary language
    ToFind := '??' + Copy(HexId, 3, 2) + '-*.txt';
    Entry := ZM.SearchNameEx(ToFind, True, Entry);
    if Entry = nil then
      break;
    Num := Copy(Entry.FileName, 1, 4); // get found ident as string
    // verify first 2 characters are hex
    if not(CharInSet(Num[1], Hexit) and CharInSet(Num[2], Hexit)) then
      continue;
    if CompareText(Num, HexId) = 0 then
    begin
      // found full match
      Full := Entry;
      break;
    end;
    // test high bits of primary
    Ch := Num[2];
    if (Ord(Ch) and 3) = PrimaryHigh then
    begin
      // Found primary match
      if Near = nil then
        Near := Entry;
    end;
  until False;
  if Full <> nil then
    Result := Full
  else
  if Near <> nil then
    Result := Near;
end;

function TZMLangOpr.LoadFromZip(DstStrs: TStrings; const ById, Req, LangsZip:
    string): Integer;
var
  Entry: TZMEntryBase;
  idx: Integer;
  ResZip: TStream;
  TheZip: TZMZipReader;
begin
  ResZip := nil;
  TheZip := TZMZipReader.Create(Lister);
  try
    if LangsZip = '' then
    begin
      ResZip := OpenResStream(DZRES_Lng, RT_RCData);
      ResZip.Position := 0;
      TheZip.Stream := ResZip;
    end
    else
      TheZip.ArchiveName := LangsZip;
    Reporter.Trace('Loading Language zip');//, True);
    Result := TheZip.Open(false, false);
    if (Result >= 0) and (TheZip.Count > 0) then
    begin
      idx := -1;
      if ById <> '' then
        Entry := FindById(TheZip, ById)
      else
        Entry := TheZip.SearchName(Req, True, idx); // find first matching pattern
//        Entry := ZM.Find(Req, idx); // find first matching pattern
      if Entry <> nil then
        Result := LoadZipEntry(DstStrs, Entry)
      else
        Reporter.Inform('Could not find language: ' + Req);//, False);
    end
    else
    if Body <> nil then
    begin
      if Result < 0 then
        Reporter.Inform('Error: '+ IntToStr(Result) + ' loading zipped langauge files');//, False);
    end;
  finally
    TheZip.Free;
    if ResZip <> nil then
      ResZip.Free;
  end;
end;

// filename is 'xxxx-lang.txt'
function TZMLangOpr.LoadLang(DstStrs: TStrings; const Lang: string; LangsZip:
    String): Integer;
var
  ById: string;
  IdToFind: Integer;
  Posn: Integer;
  Req: string;
begin
  Result := -1;
  if Lang = '' then
    Exit;
  Req := Trim(Lang);
  Posn := Pos(':', Req);
  if Posn > 2 then
    Req := Trim(Copy(Req, 1, Posn - 1));
  if Req = '*' then
    Req := '0';
  if (not IsWild(Req)) and (Pos('-', Req) < 5) and (ExtractFileExt(Req) = '') then
  begin
    if TryStrToInt(Req, IdToFind) then
    begin
      if IdToFind <= 0 then
      begin
        IdToFind := GetUserDefaultLCID;
        if (IdToFind >= $FFFF) or ((LangsZip = '') and (IdToFind = $0409)) then
          Exit;  // use default
      end;
      ById := IntToHex(IdToFind, 4);
    end
    else
      Req := '????-' + Req + '.txt';
  end;
  Result := LoadFromZip(DstStrs, ById, Req, LangsZip);
end;

function TZMLangOpr.LoadZipEntry(DstStrs: TStrings; Entry: TZMEntryBase):
    Integer;
var
  ls: TMemoryStream;
begin
  Reporter.Trace('Loading selected language');//, true);
  ls := TMemoryStream.Create;
  try
    Result := UnzipAStream(ls, Entry);
    if (Result = 0) and (ls.Size > 10) then
    begin
      ls.Position := 0;
      Result := LoadStrs(DstStrs, ls, Self);
    end
    else
      Reporter.Inform('Error unzipping language strings');//, False);
  finally
    ls.Free;
  end;
end;

function TZMLangOpr.MergeLanguage(DstStrs: TStrings; const ReqName: string):
    Integer;
var
  AStream: TStream;
  EntryName: string;
  ZipName: string;
begin
  Result := 0;
  if ReqName = '' then
    exit; // use default
  if (Pos('>>', ReqName) < 1) and (ExtractFileExt(ReqName) <> '') then
  begin
    // load specified file
    if FileExists(ReqName) then
    begin
      AStream := TFileStream.Create(ReqName, fmOpenRead);
      try
        AStream.Position := 0;
        Result := LoadStrs(DstStrs, AStream, Self);
      finally
        AStream.Free;
      end;
    end
    else
      Result := LoadLang(DstStrs, ReqName, ''); // from resources
  end
  else
  begin
    if (Pos('>>', ReqName) > 1) then
    begin
      ZipName := ZSplitString('>>', ReqName, EntryName);
      Result := LoadLang(DstStrs, EntryName, ZipName);
    end
    else
      Result := LoadLang(DstStrs, ReqName, ''); // from resources
  end;
end;

function TZMLangOpr.SetLanguage(const zl: String): integer;
begin
  Result := SetZipMsgLanguage(zl, self);
  if Result <= 0 then
    Reporter.Inform(Format('Error %d setting language: %s', [Result, zl]));//, False);
end;

initialization
  OnZMStr := nil;
  Setting := False;
  MsgStrs := nil;
  CurrentLanguage := '';
{$IFNDEF _NO_UTF8_}
  GUsingUTF8 := False;
{$ENDIF}
{$IFDEF VERpre6}
  InitializeCriticalSection(CSection);
{$ELSE}
  Guard := TCriticalSection.Create;
{$ENDIF}

finalization
  OnZMStr := nil;
  MsgStrs.Free;
{$IFDEF VERpre6}
  DeleteCriticalSection(CSection);
{$ELSE}
  FreeAndNil(Guard);
{$ENDIF}

end.
