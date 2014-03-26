unit SFXMakeStr;

(* ***************************************************************************
  SFXMakeStr.pas - make sfx strings for TZipResMaker for ZipMaster
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.
 Copyright (C) 2013, 2014  by Russell J. Peters, Roger Aelbrecht.

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

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, PathParser, ZipMstr,
  ZMSFXInt;

type
  WordArray = array of Word;

type
  TSFXStrMaker = class
  private
    FCP:       Integer;
    FLang:     String;
    FLangID:   Integer;
    FStrTable: WordArray;
    function GetStrIdent(Index: String): Integer;
    function GetStrIdentValue(Index: Integer): Integer;
    procedure SetLangID(const Value: Integer);
    procedure SetStrIdent(Index: String; const Value: Integer);
    procedure SetStrTable(const Value: WordArray);
  protected
    StrIdents: TStringList;
    function CheckIdentStrs(MustExist: Boolean): Integer;
    function CompressStrTable(outs: TStream; const table: RawByteString): Integer;
    function DecodeString(const astr: String; cp: Integer): TZMWideString;
    function DequoteStr(const qstr: String; var cnt: Integer): String;
    procedure KillIdents;
    function MakeLang(dest: TStream; const lid: String): Integer;
    function MakeStrTable(IsDefault: boolean): RawByteString;
    function Path(const fs: String): String;
    function PrepareLang(var ll: Word; ms: TStream; const lid: String;
      MakeBin: Boolean): Longword;
    function PrepareLang1(var ll: Word; ms: TStream; const lid: String): Longword;
    function ReadIdents(const fn: String): Integer;
    function ReadRawStrings(const ll: String): Integer;
    procedure Show(const msg: String; Err: Boolean = False);
    function StrIdentIndex(const index: String): Integer;
    function WriteUSTable(const fn: String; const tbl: RawByteString): Integer;
    function WStr(ident: Word): WideString;
    property CP: Integer Read FCP Write FCP;
    property Lang: String Read FLang;
    property LangID: Integer Read FLangID Write SetLangID;
    property StrIdent[Index: String]: Integer Read GetStrIdent Write SetStrIdent;
    property StrIdentValue[Index: Integer]: Integer Read GetStrIdentValue;
    property StrTable: WordArray Read FStrTable Write SetStrTable;
  public
    constructor Create;
    destructor Destroy; override;
    function LangName(const fname: string): string;
    function AddSFXLanguages(const dest: string; Langs: TStrings): Integer;
    function WriteDefTable: Integer;
  end;


implementation

{$WARN SYMBOL_PLATFORM OFF}
uses
  StrUtils, Main, ZMUtils, ZMUTF8, ToolHelper;

const
  NUM_LANGS = 12;
  Langs: array[1..NUM_LANGS] of String = ('BR', 'CAT', 'DE', 'DK', 'ES', 'FR',
    'HU', 'IT', 'KO', 'NL', 'PL', 'US');

  IncName = '$(ZM_SFX)\ZMSFXDefStrs19.pas';
  Apo: Char = #$27;
  BinExt  = '.bin';
  TestSFXsize = $BE00;
  TestSFXName = '$(ZM_SFX)\SFXTest.exe';

const
  LOWEST_ID  = 1024;
  HIGHEST_ID = 1200;


type
  TMsgStr = class(TObject)
  private
    FU8Str: UTF8String;
    function GetWStr: WideString;
    procedure SetWStr(const Value: WideString);
  public
    constructor Create;//(const w: WideString);
    destructor Destroy; override;
    property U8Str: UTF8String Read FU8Str Write FU8Str;
    property WStr: WideString Read GetWStr Write SetWStr;
  end;

type
  TLangs = record
    s: String;
    l: Integer;
    c: Integer;
  end;

const
  std = 1252;
{  LangCPs: array [0..26] of TLangs = (
    (s: 'BR: Brazilian'; p: std),
    (s: 'CT: Catalan'; p: std),
    (s: 'DK: Danish'; p: std),
    (s: 'NL: Dutch'; p: std), //0413
    (s: 'FI: Finnish'; p: std),
    (s: 'FR: French'; p: std),
    (s: 'DE: German'; p: std),
    (s: 'HU: Hungarian'; p: 1250),
    (s: 'NO: Norwegian'; p: std),
    (s: 'PO: Polish'; p: 1250),
    (s: 'SW: Swedish'; p: std),
    (s: 'S1: Spanish'; p: std),
    (s: 'ES: Spanish'; p: std),
    (s: 'US: default US'; p: std),
    (s: 'SI: Slovenian'; p: 1250),
    (s: 'SL: Slovenian'; p: 1250),//0424
    (s: 'CN: Chinese'; p: 936),
    (s: 'CZ: Czech'; p: 1250),
    (s: 'GR: Greek'; p: 1253),
    (s: 'IT: Italian'; p: std),
    (s: 'JP: Japanese'; p: 932),
    (s: 'MY: Malaysian'; p: 874),
    (s: 'RO: Romanian'; p: std),
    (s: 'RU: Russian'; p: 1251),
    (s: 'YU: Serbian'; p: 1250),
    (s: 'TW: Taiwanese'; p: 950),
    (s: 'KO: Korean'; p: 949)
    );   }


  SFXCPs: array [0..12] of TLangs = (
    (s: 'BR: Brazilian Portuguese'; l: $0416; c: std),
    (s: 'CAT: Catalan'; l: $0403; c: std),
    (s: 'DK: Danish'; l: $0406; c: std),
    (s: 'NL: Dutch'; l: $0413; c: std),
    (s: 'FR: French'; l: $040c; c: std),
    (s: 'DE: German'; l: $0407; c: std),
    (s: 'HU: Hungarian'; l: $040e; c: 1250),
    (s: 'IT: Italian'; l: $0410; c: std),
    (s: 'KO: Korean'; l: $0412; c: 949),
    (s: 'PL: Polish'; l: $0415; c: 1250),
    (s: 'ES: Spanish'; l: $040a; c: std),
    (s: 'US: US English'; l: $0409; c: std)
    , (s: 'AU: AU English'; l: $0C09; c: std)
    );


type
  RC_String = record
    i: String;
    v: String;
  end;

const
  NUM_RCS = 18;
  RCStrings: array [1..NUM_RCS] of RC_String =
    ((i: 'FILEEXISTS_TITLE'; v: 'SFX_Ttl_File'),
    //(i:'FILEEXISTS_MESSAGE'; v: ),
    (i: 'YES'; v: 'SFX_Btn_Yes'),
    (i: 'NO'; v: 'SFX_Btn_No'),
    (i: 'DONT_ASK_AGAIN'; v: 'SFX_Btn_DontAsk'),
    (i: 'MAIN_TITLE'; v: 'SFX_Ttl_Main'),
    (i: 'EXTRACT_TO'; v: 'SFX_Btn_ExtractTo'),
    (i: 'FILES'; v: 'SFX_Btn_Files'),
    (i: 'EXISTING_FILES'; v: 'SFX_Btn_Existing'),
    (i: 'OVERWRITE_CONFIRM'; v: 'SFX_Btn_OvrAsk'),
    (i: 'OVERWRITE_SKIP'; v: 'SFX_Btn_OvrSkip'),
    (i: 'OVERWRITE_DO'; v: 'SFX_Btn_OvrAll'),
    (i: 'ABOUT_BUTTON'; v: 'SFX_Btn_About'),
    (i: 'SHOW_FILES_BUTTON'; v: 'SFX_Btn_ShowFiles'),
    (i: 'START_BUTTON'; v: 'SFX_Btn_Start'),
    (i: 'CLOSE_BUTTON'; v: 'SFX_Btn_Close'),
    //(i:'RUN_AFTER_EXTRACTION'; v: ),
    //(i:'INSTALL_AFTER_EXTRACTION'; v: ),
    (i: 'PASSWORD_TITLE'; v: 'SFX_Ttl_PWrd'),
    //(i:'PASSWORD_MESSAGE'; v: ),
    (i: 'OK'; v: 'SFX_Btn_Ok'),
    (i: 'CANCEL'; v: 'SFX_Btn_Cancel'));


// return default codepage
function Find_CP(const sid: String; tbl: array of TLangs): Integer;
var
  i: Integer;
  p: Integer;
  s: String;
  u: String;
begin
  Result := -1;
  if sid <> '' then
  begin
    u := Uppercase(sid);
    for i := 0 to high(tbl) do
    begin
      s := tbl[i].s;
      p := AnsiPos(':', s);
      if p > 1 then
        s := copy(s, 1, p - 1);
      if s = u then
      begin
        Result := tbl[i].c;
        break;
      end;
    end;
  end;
end;

// return default locale
function Find_Locale(const sid: String; tbl: array of TLangs): Integer;
var
  i: Integer;
  p: Integer;
  s: String;
  u: String;
begin
  Result := -1;
  if sid <> '' then
  begin
    u := Uppercase(sid);
    for i := 0 to high(tbl) do
    begin
      s := tbl[i].s;
      p := AnsiPos(':', s);
      if p > 1 then
        s := copy(s, 1, p - 1);
      if s = u then
      begin
        Result := tbl[i].l;
        break;
      end;
    end;
  end;
end;


constructor TSFXStrMaker.Create;
begin
  StrIdents := nil;
end;

destructor TSFXStrMaker.Destroy;
begin
  KillIdents;
  inherited;
end;

function TSFXStrMaker.CheckIdentStrs(MustExist: Boolean): Integer;
var
  i: Integer;
  wo: TMsgStr;
begin
  Result := 0;  // count missing
  for I := 0 to StrIdents.Count - 1 do
  begin
    wo := nil;
    if StrIdents.Objects[i] is TMsgStr then
      wo := StrIdents.Objects[i] as TMsgStr;
    // check identifier was used
    //    if (wo = nil) or (MustExist and (Length(wo.U8Str) = 0)) then
    // identifier not needed
    if MustExist and ((wo = nil) or (Length(wo.U8Str) = 0)) then
    begin
      Show(' no string ' + StrIdents.Names[i] + StrIdents.ValueFromIndex[i]);
      Inc(Result);
    end;
  end;
  if Result <> 0 then
    Show(Format('missing %d strings', [Result]), True);
end;

function TSFXStrMaker.CompressStrTable(outs: TStream; const table:
    RawByteString): Integer;
var
  sz: Integer;
  tbl: TMemoryStream;
  Zip: TZipMaster;
begin
  Result := -1;
  sz := Length(table);
  if sz < 500 then
    exit;     // empty
  tbl := TMemoryStream.Create;
  try
    if tbl.Write(PAnsiChar(Table)^, sz) <> sz then
    begin
      Show('error writing table', True);
      Result := -2;
      exit;
    end;
    tbl.Position := 0;
    Result := 0;
    Zip := TZipMaster.Create(nil);
    try
      //      Zip.Logging := zlSafe;
      //      Zip.LogName := 'ziplog.txt';
      //      Zip.Trace := True;
      //      Zip.Verbose := True;
      Zip.DLLDirectory := Path('$(ZM_Dll)');
      Zip.Dll_Load := True;
      if not Zip.Dll_Load then
      begin
        Show('Could not load dll', True);
        Result := -3;
        exit;
      end;

      if Zip.AddStreamToStream(tbl) <> nil then
      begin
        // keep crc
        Result := Zip.ZipStream.Size - 2; {bytes}
        if Result > 0 then
        begin
          // strip off method
          Zip.ZipStream.Position := 2;
          outs.CopyFrom(Zip.ZipStream, Result);
        end;
      end;
    finally
      Zip.Free;
    end;
  finally
    tbl.Free;
  end;
end;

function TSFXStrMaker.DecodeString(const astr: string; cp: Integer):
    TZMWideString;
var
  c: integer;
  len: integer;
  p: integer;
  t: string;
  t2: string;
  tmp: string;
{$IFNDEF UNICODE}
  wc: integer;
{$ENDIF}
begin
  Result := '';
  tmp := '';
  if astr = '' then
    exit;
  if astr[1] = Apo then
  begin
    t2 := astr;
    if t2[2] = ' ' then
      t2 := astr;
    repeat
      c := -1;
      len := Length(t2);
      tmp := tmp + DequoteStr(t2, c);
      if c < len then
      begin
        t2 := Trim(Copy(t2, c, len - (c -1)));
        if t2 = ';' then
          t2 := ''
        else
        while (t2 <> '') and CharInSet(t2[1], ['+', '#']) do
        begin
          len := Length(t2);
          if t2[1] = '+' then
            t2 := Trim(Copy(t2, 2, len -1)) // remove it
          else
          begin
            t := '';
            p := 2;
            c := 0;
            while (p <= len) and CharInSet(t2[p], ['0'..'9']) do
            begin
              c := (c * 10) + (ord(t2[p]) - ord('0'));
              inc(p);
            end;
            tmp := tmp + char(c);
            t2 := Trim(Copy(t2, p, len - (p-1)));
          end;
        end;
      end
      else
        t2 := '';
    until t2 = '';
  end
  else
    tmp := astr;
{$IFDEF UNICODE}
  Result := tmp;
{$ELSE}
  wc := MultiByteToWideChar(cp, 0, pAnsiChar(tmp), Length(tmp), nil, 0);
  if wc > 0 then
  begin
    SetLength(Result, wc);
    if MultiByteToWideChar(cp, 0, pAnsiChar(tmp), Length(tmp), pWideChar(Result), wc) < 1 then
     Result := '';
  end;
  if Result = '' then
    Result := '????';
{$ENDIF}
end;

function TSFXStrMaker.DequoteStr(const qstr: String; var cnt: Integer): String;
var
  c: Char;
  len: Integer;
  p: Integer;
begin
  len := length(qstr);
  Result := '';
  cnt := 2;
  if qstr = '''' then
    exit;
  cnt := 0;
  if qstr = '' then
    exit;
  if qstr[1] <> Apo then
  begin
    Result := qstr;
    cnt := len;
    exit;
  end;
  // find end of quote
  p := 2;
  while p <= len do
  begin
    c := qstr[p];
    Inc(p);
    if c <> Apo then
    begin
      Result := Result + c;
      continue;
    end;
    if (p < len) and (qstr[p] = Apo) then
    begin
      Result := Result + c;
      Inc(p);
      continue;
    end
    else
      break;
  end;
  cnt := p;
end;

function TSFXStrMaker.GetStrIdent(Index: String): Integer;
var
  s: String;
  v: String;
begin
  Result := -1;
  if StrIdents = nil then
    exit;
  if Length(Index) < 8 then
    exit;
  s := Index;
  if Copy(Index, 1, 4) = '_STR' then
    s := 'SFX' + copy(index, 5, Length(Index) - 4);
  if s[1] = '_' then
    s := copy(s, 2, Length(s) - 1);
  v := StrIdents.Values[s];
  if (v <> '') and not TryStrToInt(v, Result) then
    Result := -2;
end;

function TSFXStrMaker.GetStrIdentValue(Index: Integer): Integer;
var
  v: String;
begin
  v := StrIdents.ValueFromIndex[Index];
  Result := StrToIntDef(v, -1);
end;

procedure TSFXStrMaker.KillIdents;
var
  I: Integer;
begin
  if StrIdents <> nil then
  begin
    for I := 0 to StrIdents.Count - 1 do
    begin
      if StrIdents.Objects[i] <> nil then
        StrIdents.Objects[i].Free;
    end;
    FreeAndNil(StrIdents);
  end;
end;

function TSFXStrMaker.LangName(const fname: string): string;  
var
  fn: String;
  id: Integer;
  j: Integer;
  ll: String;
  p: Integer;
  strs: TStringList;
  tmp: String;
  v: String;
begin
  Result := '???';
  fn := ExtractFileName(fname);
  ll := Uppercase(copy(fn, 8, length(fn) - (7 + 4)));
  if ll = '' then  // ignore empty
     exit;
  if ll = 'US' then  // ignore hidden, default
  begin
    Result := 'default';
    exit;
  end;
  ID := Find_Locale(ll, SFXCPs);
  strs := TStringList.Create;
  try
    strs.LoadFromFile(fname);    
    v := strs.Values['NAME'];   
    if v <> '' then
    begin
      Result := ':' + v;
      exit;
    end;
    v := strs.Values['LANG_ID'];
    j  := 0;
    if v <> '' then
    begin
      j := -1;
      v := '$' + DequoteStr(v, j);
      j := AnsiPos(' ', v);
      if j > 0 then
      begin
        tmp := '$' + Trim(Copy(v, j + 1, 4));
        v := Copy(v, 1, j - 1);
        if TryStrToInt(tmp, j) and TryStrToInt(v, p) then
          ID := (j * 256) + p;
      end
      else
        ID := StrToIntDef(v, -1);
    end;
    if ID > 0 then
    begin
      SetLength(tmp, 125);
    if GetLocaleInfo(ID, LOCALE_SLANGUAGE, PChar(tmp), 120) <> 0 then
      Result := PChar(tmp);
    end;
  finally
    strs.Free;
  end;
end;

// make language to stream
function TSFXStrMaker.MakeLang(dest: TStream; const lid: String): Integer;
var
  cz: Integer;
  Hed: TSFXStringsData;
  IsDef: Boolean;
  K: Cardinal;
  Loc: Word;
  ms: TMemoryStream;
  tbl: RawByteString;
  x: Integer;
begin
//  Result := -2;
//  if AnsiSameText(lid, 'US') then
//    exit;
  Show('Making ' + lid);
  KillIdents;
  IsDef := AnsiSameText(lid, 'US');
  Result := ReadIdents(SFXStrings);
  if Result <= 0 then
  begin
    Result := -3;
    Show('Could not load identifiers', True);
    exit;
  end;
  Result := ReadRawStrings(lid);
  Show(' Read returned ' + IntToStr(Result));
  if Result < 0 then
    exit;
  Show('Language = ' + lid);
  CheckIdentStrs(IsDef);//False);
  //  fn := Path(BinName + Lang + BinExt);
  tbl := MakeStrTable(IsDef);
  if Length(tbl) < 100 then
  begin
    Show('error making table', True);
    Result := -99;
    exit;
  end;
  ms := TMemoryStream.Create;
  try
    cz := CompressStrTable(ms, tbl);
    if cz < 100 then
    begin
      Show('error compressing table', True);
      Result := -999;
      if cz < 0 then
        Result := cz;
      exit;
    end;
    Result := -20;
    ms.Position := 0;
    cz  := cz - sizeof(DWORD); // crc
    Loc := Word(LangID);
    Hed.CSize := cz; {bytes}
    Hed.USize := length(tbl); {Bytes}
    if ms.Read(Hed.CRC, sizeof(DWORD)) <> sizeof(DWORD) then
      exit;
    try
      if dest.Write(Loc, sizeof(Word)) <> sizeof(Word) then
        raise Exception.Create('write failed');
      if dest.Write(Hed, sizeof(TSFXStringsData)) <> sizeof(TSFXStringsData) then
        raise Exception.Create('write failed');
      // data
      if dest.CopyFrom(ms, cz) <> cz then
        raise Exception.Create('copy failed');
      // align dword boundary
      x := (Hed.CSize) + sizeof(TSFXStringsData);
      x := x and (sizeof(DWORD) - 1);
      if x <> 0 then
      begin
        x := sizeof(DWORD) - x;
        k := 0;
        if dest.Write(k, x) <> x then
          raise Exception.Create('write failed');
      end;
      Result := dest.Size;
      dest.Position := 0;
      Show(' MakeLang returns ' + IntToStr(Result));
    except
      on E: Exception do
      begin
        Show('Exception creating ' + lid + ' ' + E.Message, true);
        Result := -1;
      end;
    end;
  finally
    ms.Free;
  end;
end;


// TODO: MakeLanguagePack
////{$define WANT_TEST}
//function TSFXStrMaker.MakeLanguagePack(dest: TMemoryStream; Langs: TStrings;
//MakeBin: Boolean): Integer;    (*
//const
//bsize: Integer = (3 * sizeof(Word)) + sizeof(DWORD);
//var
//ts: TMemoryStream;
//x, i, c, r: Integer;
//SHead: TSFXStringsHeader;
//STail: TSFXStringsEndOfHeader;
//Blocks: array of TSFXStringsEntry;
//SSize: DWORD;
//ll, ofs: Word;
//{$ifdef WANT_TEST}
//fs: TFileStream;
//sz: Int64;
//{$endif}  *)
//begin
//Result := -1; (*
//ts := TMemoryStream.Create;
//try
//  SHead.Signature := SFX_STRINGS_SIG;
//  STail.Signature := SFX_STRINGS_END_SIG;
//  x := Langs.Count;
//  if x > 0 then
//  begin
//    SHead.Count := x;
//    // calc size and initial language entries
//    SetLength(Blocks, x);
//    ofs := sizeof(TSFXStringsHeader) + (x * sizeof(TSFXStringsEntry));
//    SSize := 0;
//    c := 0;
//    for I := 0 to Langs.Count - 1 do
//    begin
//      Blocks[c].DOfs := ofs + Word(ts.Position and $FFFF);
//      r := PrepareLang(ll, ts, Langs[i], MakeBin);
//      Inc(SSize, r);
//      //        Inc(SSize, XFerLang(ll, ts, Langs[i]));
//      Blocks[c].LangID := ll;
//      Inc(SSize, sizeof(TSFXStringsEntry));
//      Inc(c);
//    end;
//    SHead.Size := SizeOf(TSFXStringsHeader) + Word(ts.Size) +
//      (x * sizeof(TSFXStringsEntry));
//    if SHead.Size <> (SSize + SizeOf(TSFXStringsHeader)) then
//    begin
//      Show('Woops - cannot add up!', True);
//      exit;
//    end;
//    STail.HeaderSize := SHead.Size;
//    // write header
//    if dest.Write(SHead, sizeof(TSFXStringsHeader)) <> sizeof(TSFXStringsHeader) then
//    begin
//      Show('Could not write strings header!', True);
//      exit;
//    end;
//    // update offsets and write blocks
//    for I := 0 to x - 1 do
//    begin
//      if dest.Write(Blocks[i], sizeof(TSFXStringsEntry)) <>
//        sizeof(TSFXStringsEntry) then
//      begin
//        Show('Could not write strings block!', True);
//        exit;
//      end;
//    end;
//    // write data
//    ts.Position := 0;
//    if dest.CopyFrom(ts, ts.Size) <> ts.Size then
//    begin
//      Show('Could not copy block data!', True);
//      exit;
//    end;
//    // write end of strings
//    if dest.Write(STail, sizeof(TSFXStringsEndOfHeader)) <>
//      sizeof(TSFXStringsEndOfHeader) then
//    begin
//      Show('Could not write strings end of header!', True);
//      exit;
//    end;
//  end//;
//  else
//    Show('no languages selected', True);
//
//{$ifdef WANT_TEST}
//  // write 'old' test file headers + data
//  fs := TFileStream.Create(Path('$(ZM_SFX)\test-r.exe'), fmOpenRead);
//  try
//    fs.Position := TestSFXSize;
//    sz := fs.Size - TestSFXSize;
//    if dest.CopyFrom(fs, sz) <> sz then
//    begin
//      Show('Could not copy old test!', true);
//      exit;
//    end;
//  finally
//    fs.Free;
//  end;
//  // write as new test file
//  dest.Position := 0;
//  fs := TFileStream.Create(Path(TestSFXName), fmCreate);
//  try
//    if fs.CopyFrom(dest, dest.Size) <> dest.Size then
//    begin
//      Show('Could not write new test!', true);
//      exit;
//    end;
//      Show('Wrote ' + Path(TestSFXName) + ' = ' + IntToStr(fs.Size));
//  finally
//    fs.Free;
//  end;
//{$endif}
//  Result := 0;
//finally
//  if ts <> nil then
//    ts.Free;
//end;   *)
//end;

function TSFXStrMaker.AddSFXLanguages(const dest: string; Langs: TStrings):
    Integer;
//const
//  bsize: Integer = (3 * sizeof(Word)) + sizeof(DWORD);
var
  empty: TSFX_LanguageData;
  hUpdateRes: THandle;
  //  ret: Boolean;
  i: Integer;
  id: integer;
  ll: Word;
  r: Integer;
  ts: TMemoryStream;
begin
   Result := 0;
// Open the file to which you want to add the dialog box resource.
  hUpdateRes := BeginUpdateResource(PChar(dest), FALSE);
  if (hUpdateRes = 0) then
  begin
    Show('Could not open file for writing!', True);
    exit;
  end;
  ts := TMemoryStream.Create;
  try
    id := SFX_LANG_BASE + 1;
    for i := 0 to Langs.Count - 1 do
    begin
      ts.Size := 0;  // empty it
      ll := 0;  // dummy value
      if ts.Write(ll, sizeof(WORD)) <> sizeof(WORD) then
      begin
        Show('Could not write to stream!', True);
        Result := -2;
        break;
      end;
      r := PrepareLang(ll, ts, Langs[i], false);
      if r > 0 then
      begin
//        ret := False;
        ts.Position := 0;
        if ts.Write(ll, sizeof(WORD)) <> sizeof(WORD) then
        begin
          Show('Could not write to stream!', True);
          Result := -2;
          break;
        end;
          // Add the compressed strings - assume did not exist
        if not UpdateResource(hUpdateRes, RT_RCDATA, PChar(id),
             MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), ts.Memory, ts.Size) then
        begin
          Show('Could not add resource! [' + IntToStr(id) + ']', True);
          Result := -1;
          break;
        end;
        inc(Result);  // count languages added
        inc(id);
      end;
    end;
  finally
    ts.Free;
  end;

  if Result > 0 then
  begin
    ZeroMemory(@empty, sizeof(TSFX_LanguageData));
//    empty.LangID := -1;
    // mark end in case more existed
    if not UpdateResource(hUpdateRes, RT_RCDATA, PChar(id),
         MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL),
         @empty, sizeof(TSFX_LanguageData)) then
    begin
      Show('Could not add terminator [' + IntToStr(id) + ']', True);
      exit;
    end;
    // Write changes to FOOT.EXE and then close it.
    if ( not EndUpdateResource(hUpdateRes, FALSE)) then
    begin
      Show('Could not write changes to file!', True);
      exit;
    end;
  end;
end;


// format id:byte,utf8string,0;.....;0
function TSFXStrMaker.MakeStrTable(IsDefault: boolean): RawByteString;
var
  I: Integer;
  l: Integer;
  l8: Integer;
  long8: Integer;
  longest: Integer;
  ovr: Integer;
  ovr8: Integer;
  shortest: Integer;
  sz: Integer;
  tbl: RawByteString;
  ustr: UTF8String;
  wo: TMsgStr;
begin
  Show('Making New Strings');
  Result := '';
  if (StrIdents = nil) or (StrIdents.Count < 20) then
  begin
    Show('Idents empty', True);
    exit;
  end;
  ovr := 1 + (StrIdents.Count * 2);
  sz  := 0;
  ovr8 := 0;
  long8 := 0;
  Longest := -1;
  Shortest := HIGH(Integer);
  I := 0;
//  for I := 0 to StrIdents.Count - 1 do
  while I < StrIdents.Count do
  begin
    if (not IsDefault) and SameStr('SFX_MSG_ABOUT', copy(StrIdents.Names[i], 1, 13)) then
    begin
      StrIdents.delete(i);
      continue;
    end;
    if not (StrIdents.Objects[i] is TMsgStr) then
    begin
      Show(' missing string ' + StrIdents.Names[i] + '  ' +
        StrIdents.ValueFromIndex[i], True);
      exit;
    end;
    wo := StrIdents.Objects[i] as TMsgStr;
    ustr := wo.U8Str;
    l  := Length(ustr);// + 1;
    if ValidUTF8(ustr, l) > 0 then
    begin
      // only want UTF8 overheads
      l8 := l - Length(wo.WStr);
      if l8 > long8 then
        long8 := l8;
      Inc(ovr8, l8);
    end;
    Inc(sz, l);
    if l > longest then
      longest := l;
    if l < Shortest then
      Shortest := l;
    if l > 0 then
      tbl := tbl + AnsiChar(StrIdentValue[i]) + ustr + #0;
    inc(i);
  end;
  tbl := tbl + #0;
  Show(Format('size = %d bytes, entries = %d ', [Length(tbl), StrIdents.Count]));
  Show(Format('  characters = %d,  [%d .. %d] ', [sz, shortest, longest]));
  Show(Format('  overhead = %d bytes, + UTF8  = %d bytes, [max: %d] ',
    [ovr, ovr8, long8]));
  Result := tbl;
end;

function TSFXStrMaker.Path(const fs: String): String;
begin
  Result := ToolSupp.Path(fs);
end;

function TSFXStrMaker.PrepareLang1(var ll: Word; ms: TStream;
  const lid: String): Longword;
var
  ls: TMemoryStream;
  t: Word;
begin
  ll := 0;
  Result := 0;
  ls := TMemoryStream.Create;
  try
    if MakeLang(ls, lid) < 0 then
    begin
      Show('Could not make language: ' + lid);
      exit;
    end;
    if ls.Read(t, sizeof(Word)) <> sizeof(Word) then
    begin
      Show('Could not read ll header LangID: ' + lid, True);
      exit;
    end;
    ll := t;
    if ms.CopyFrom(ls, ls.Size - sizeof(Word)) <> (ls.Size - sizeof(Word)) then
    begin
      Show('Could not copy data: ' + lid, True);
    end
    else
      Result := Longword(ls.Size) - sizeof(Word);
  finally
    ls.Free;
  end;
end;

function TSFXStrMaker.PrepareLang(var ll: Word; ms: TStream;
  const lid: String; MakeBin: Boolean): Longword;
var
  f: String;
  fs: TFileStream;
  pos0: Int64;
  sz: Int64;
begin
  fs := nil;
  f  := Path(BinName + lid + BinExt);
  pos0 := ms.Position;
  //  if lid = 'AU' then
  //    Result := 0
  //  else
  Result := PrepareLang1(ll, ms, lid);
  if Result = 0 then
  begin
    // read from file
    Show('Reading "' + lid + '" from: ' + f);
    if not FileExists(f) then
    begin
      Show('Could not find: ' + f, True);
      exit;
    end;
    fs := TFileStream.Create(f, fmOpenRead);
    try
      ms.Position := pos0;
      if ms.CopyFrom(fs, fs.Size) = fs.Size then
        Result := fs.Size;
      if Result <> 0 then
        Show('IMPORTED: ' + lid);
    finally
      FreeAndNil(fs);
    end;
  end
  else
  if MakeBin then
  begin
    // copy to file
    ForceDirectory(Path('$(ZM_RC-Bin)'));
    if ToolSupp.CanWrite(f) then
    begin
      ToolSupp.BackupFile(f);
      fs := TFileStream.Create(f, fmCreate);
      try
        sz := ms.Size - pos0;
        ms.Position := pos0;
        fs.CopyFrom(ms, sz);
      finally
        FreeAndNil(fs);
      end;
    end
    else
      Show('Could not write: ' + f, True);
  end;
  if Result = 0 then
    Show('Failed to make or read language: ' + lid, True);
end;


function TSFXStrMaker.ReadIdents(const fn: String): Integer;
const
  USections: array [1..7] of String = (
    'UNIT', 'INTERFACE', 'TYPE', 'CONST', 'VAR', 'IMPLEMENTATION', 'END.');
var
  DefNames: TStringList;
  fname: String;
  i: Integer;
  Ident: String;
  l: String;
  ln: Integer;
  p: Integer;
  sec: Integer;
  v: Integer;

  function IsSection(ln: Integer; var lt: String): Integer;
  var
    ii: Integer;
    ss: String;
  begin
    Result := -1;
    if (ln < 0) or (ln >= DefNames.Count) then
      exit;
    lt := Uppercase(Trim(DefNames[ln]));
    ii := Pos(' ', lt);
    if ii > 0 then
      ss := copy(lt, 1, pred(ii))
    else
      ss := lt;
    for Result := 1 to HIGH(USections) do
      if ss = USections[Result] then
        exit;
    Result := 0;
  end;

begin
  Result := -1;//'?';  // invalid
  fname  := Path(fn);
  if not FileExists(fname) then
  begin
    Show('Could not open: ' + fname, True);
    exit;
  end;
  try
    DefNames := TStringList.Create;
    DefNames.LoadFromFile(fname);
    if (DefNames.Count > 10) then
    begin
      ln := -1;
      // need 'UNIT'
      repeat
        Inc(ln);
        sec := IsSection(ln, l);
        if sec < 0 then
          exit;
      until sec = 1;
      l := copy(l, 6, length(l) - 5);
      if Trim(l) <> 'ZMSFXSTRINGS;' then
        exit;
      repeat
        Inc(ln);
        sec := IsSection(ln, l);
        if sec < 0 then
          exit;
      until sec <> 0;
      // need 'Interface'
      if sec <> 2 then
        exit;
      repeat
        repeat
          Inc(ln);
          sec := IsSection(ln, l);
          if sec < 0 then
            exit;
        until sec <> 0;
        // need 'CONST'
        if (sec <= 2) or (sec > 4) then
          exit;
      until sec = 4;
      repeat
        Result := -10;
        repeat
          Inc(ln);
          sec := IsSection(ln, l);
        until (sec <> 0) or ((Length(l) > 1) and (l[1] <> '/'));
        if sec = 0 then
        begin
          Result := -11;
          p := AnsiPos(';', l);
          if p < 5 then
            exit;
          Result := -12;
          l := Copy(l, 1, p - 1);
          v := -1;
          Ident := '';
          i := 1;
          while (i < Length(l)) and CharInSet(l[i], ['_', 'A'..'Z', 'a'..'z', '0'..'9']) do
          begin
            Ident := Ident + l[i];
            Inc(i);
          end;
          while (i < Length(l)) and CharInSet(l[i], [' ', #9]) do
            Inc(i);
          if (i >= Length(l)) or (l[i] <> '=') then
            exit;
          Result := -13;
          l := Trim(copy(l, i + 1, Length(l) - i));
          if TryStrToInt(l, v) then
            StrIdent[Ident] := v;
        end;
      until Sec <> 0;//False;
      if StrIdents = nil then
        Result := 0
      else
        Result := StrIdents.Count;// t;
    end;
  finally
    FreeAndNil(DefNames);
  end;
end;

function TSFXStrMaker.ReadRawStrings(const ll: String): Integer;
var
  ec: Char;
  fname: String;
  i: Integer;
  id: String;
  ident: Integer;
  idx: Integer;
  j: Integer;
  p: Integer;
  s: String;
  strs: TStringList;
  tmp: String;
  v: String;
  w: WideString;
begin
  Result := -1;
  fname  := Path(LangStrs + ll + LangExt);
  if not FileExists(fname) then
    exit;
  if (StrIdents = nil) or (StrIdents.Count < 2) then
    ReadIdents(SFXStrings);
  fLang := ll;
  LangID := Find_Locale(ll, SFXCPs);
  cp := Find_CP(ll, SFXCPs);
//  if CP < 0 then
//  begin
//    Show('Unknown language: ' + ll, True);
//    exit;
//  end;
  strs := TStringList.Create;
  try
    strs.LoadFromFile(fname);
    j  := 0;
    ec := #0;
    for i := 0 to strs.Count - 1 do
    begin
      s := Trim(strs[i]);
      if (Length(s) < 2) or (Copy(s, 1, 2) = '//') then
        continue;
      if ec <> '+' then
      begin
        if not CharInSet(s[1], ['_', 'A'..'Z']) then
          continue;
        p := AnsiPos('=', s);
        if p < 2 then
          continue;
        v  := Trim(copy(s, p + 1, Length(s) - p));
        id := Trim(copy(s, 1, p - 1));
        ident := StrIdent[id];
        if ident < 0 then
        begin
          if AnsiSameText(id, 'Lang_Id') then
          begin
            j := -1;
            v := '$' + DequoteStr(v, j);
            j := AnsiPos(' ', v);
            if j > 0 then
            begin
              tmp := '$' + Trim(Copy(v, j + 1, 4));
              v := Copy(v, 1, j - 1);
              if TryStrToInt(tmp, j) and TryStrToInt(v, p) then
              begin
                j := (j * 256) + p;
              end
              else
                j := -1;
            end
            else
              j := StrToIntDef(v, -1);
            if j <> -1 then
            begin
              Show('Locale = ' + IntToHex(j, 4));
              SetLength(tmp, 55);
              if GetLocaleInfo(j, LOCALE_IDEFAULTANSICODEPAGE, PChar(tmp), 25) <> 0 then
              begin
                if j <> LangId then
                begin
                  Show('Locale = ' + IntToStr(j));
                  LangId := j;
                end;
                j := StrToIntDef(tmp, -1);
                if j <> CP then
                begin
                  Show('CodePage = ' + IntToStr(j));
                  CP := j;
                end;
              end;
            end;
            continue;
          end
          else
          begin
            for J := 1 to NUM_RCS do
            begin
              if AnsiSameText(RCStrings[j].i, id) then
              begin
                ident := StrIdent[RCStrings[j].v];
                id := RCStrings[j].v;
                break;
              end;
            end;
            if ident < 0 then
            begin
              Show('Unused string ' + id + ' [' + IntToStr(i) + ']', True);
              continue;
            end;
          end;
        end;
        v  := Trim(copy(s, p + 1, Length(s) - p));
        ec := v[Length(v)];
        if ec = '+' then
          continue;
      end
      else
      begin
        v  := v + s;
        ec := s[Length(s)];
        if ec = '+' then
          continue;
      end;                                                  
      if CP < 0 then
      begin
        Show('Unidentified language: ' + ll, True);
        Result := -2;
        break;
      end;
      //            Show('[' + IntToStr(ident)+'] = ' + v);
      if (v = '') or (v = ';') then
        w := ''
      else
        w := DecodeString(v, CP);
      //            Show('[' + IntToStr(ident)+'] = "' + w +'"');
      idx := StrIdentIndex(id);
      if idx < 0 then
      begin
        Show('Woops - something went wrong', True);
        continue;
      end;
      if assigned(StrIdents.Objects[idx]) then
      begin
        Show('id: ' + id + ' already defined as: ' +
          TMsgStr(StrIdents.Objects[idx]).WStr, True);
        continue;
      end;
      StrIdents.Objects[idx] := TMsgStr.Create;
      Inc(j);
      TMsgStr(StrIdents.Objects[idx]).WStr := w;
    end;
    Result := j;
  finally
    strs.Free;
  end;
end;

procedure TSFXStrMaker.SetLangID(const Value: Integer);
begin
  FLangID := Value;
end;

procedure TSFXStrMaker.SetStrIdent(Index: String; const Value: Integer);
var
  i: Integer;
  s: String;
begin
  if StrIdents = nil then
    StrIdents := TStringList.Create;
  if Length(Index) < 8 then
    exit;
  s := Index;
  if Copy(Index, 1, 4) = '_STR' then
    s := 'SFX' + copy(index, 5, Length(Index) - 4);
  if s[1] = '_' then
    s := copy(s, 2, Length(s) - 1);
  i := StrIdents.IndexOf(s);
  if i < 0 then
    StrIdents.Add(s + '=' + IntToStr(Value))
  else
    StrIdents.Values[s] := IntToStr(Value);
end;

procedure TSFXStrMaker.SetStrTable(const Value: WordArray);
begin
  FStrTable := Value;
end;

procedure TSFXStrMaker.Show(const msg: String; Err: Boolean = False);
begin
  ToolSupp.Show(msg, err);
end;

function TSFXStrMaker.StrIdentIndex(const index: String): Integer;
var
  i: Integer;
  s: String;
begin
  Result := -1;
  if Length(Index) < 8 then
    exit;
  s := Index;
  if Copy(Index, 1, 4) = '_STR' then
    s := 'SFX' + copy(index, 5, Length(Index) - 4);
  if s[1] = '_' then
    s := copy(s, 2, Length(s) - 1);
  for I := 0 to StrIdents.Count - 1 do
    if AnsiCompareText(s, StrIdents.Names[i]) = 0 then
    begin
      Result := i;
      break;
    end;
end;

function TSFXStrMaker.WriteDefTable: Integer;
var
  bad: boolean;
  fn: String;
//  tbl: RawByteString;
  fs: TFileStream;
  ll: Word;
  ms: TMemoryStream;
begin
  Show('Making ' + 'US');
  KillIdents;
  Result := ReadIdents(SFXStrings);
  if Result <= 0 then
  begin
    Result := -3;
    Show('Could not load identifiers', True);
    exit;
  end;

  Result := ReadRawStrings('US');
  Show(' Read returned ' + IntToStr(Result));
  if Result < 0 then
    exit;
  if CheckIdentStrs(True) = 0 then
  begin
//    fn  := Path(IncName);
//    tbl := MakeStrTable(True);
//    if tbl <> '' then
//      Result := WriteUSTable(fn, tbl);
//    fs := nil;
    bad := false;
    ms := TMemoryStream.Create;
    try
      ll := 0;  // dummy value
      if ms.Write(ll, sizeof(WORD)) <> sizeof(WORD) then
      begin
        Show('Could not write to stream!', True);
        Result := -2;
        exit;
      end;
      if PrepareLang(ll, ms, 'US', False) > 0 then
      begin
        fn := Path('$(ZM_SFX)\DefStr.bin');
        ToolSupp.BackupFile(fn);
        bad := true;
        fs := TFileStream.Create(fn, fmCreate);
        try
        ms.Position := 0;
        if ms.Write(ll, sizeof(WORD)) <> sizeof(WORD) then
        begin
          Show('Could not write to stream!', True);
          Result := -2;
          exit;
        end;
          ms.Position := 0;
          fs.copyFrom(ms, ms.Size);
          bad := fs.Size <> ms.Size;
        finally
          fs.Free;
        end;
      end;
    finally
      ms.Free;
    end;
    if bad then
      ToolSupp.RestoreFile(fn);
  end;
end;

function TSFXStrMaker.WriteUSTable(const fn: String; const tbl: RawByteString):
    Integer;
var
  CSTot: Integer;
  idx: Integer;
  pv: pCardinal;
  s: String;
  siz: Integer;
  t: RawByteString;
  un: String;
  ustrs: TStringList;
begin
  ustrs := nil;
  t := tbl;
  while (Length(t) and (sizeof(Cardinal) - 1)) <> 0 do
    t := t + #0;
  siz := Length(t);
  CSTot := (siz) div sizeof(Cardinal);

  un := ExtractFileName(fn);
  try
    ustrs := TStringList.Create;
    ustrs.Add('Unit ZMSFXDefStrs;');
    ustrs.Add(' ');
    ustrs.Add('(* Built by ZipResMaker');
    ustrs.Add('   DO NOT MODIFY');
    ustrs.Add('  ' + un + ' - default messages table');
    ustrs.Add('  TZipSFX VCL by Chris Vleghert and Eric W. Engler');
    ustrs.Add('');
    ustrs.Add('  ' + VCL_VER_STRING);//ToolSupp.VerString[vsVer]);//v1.90');
    ustrs.Add('  * Copyright 1997, Microchip Systems / Carl Bunton                *');
    ustrs.Add('  * e-mail: Twojags@cris.com                                       *');
    ustrs.Add('  * Web-page: http://www.concentric.net/~twojags                   *');
    ustrs.Add('  *                                                                *');
    ustrs.Add('  * modified by Markus Stephany                                    *');
    ustrs.Add('     and by  Russell Peters');
    ToolSupp.Licence(ustrs);
    //    LicenceMessage(ustrs);
    ustrs.Add(' ');
    ustrs.Add('Interface');
    ustrs.Add(' ');
    ustrs.Add('const');
    ustrs.Add(Format(' StrBlok: array [0..%d] of Cardinal = (', [CSTot]));
    s  := '  ';
    pV := pCardinal(PAnsiChar(tbl));
    for idx := 0 to CSTot - 1 do
    begin
      s := s + '$' + IntToHex(pV^, 8) + ', ';
      if length(s) > 60 then
      begin
        ustrs.add(s);
        s := '  ';
      end;
      Inc(pV);
    end;
    s := s + '$' + IntToHex(pV^, 8) + ' );';
    if Length(s) > 4 then
      ustrs.add(s);
    Result := -1;
    ustrs.Add(' ');
    ustrs.Add('implementation');
    ustrs.Add(' ');
    ustrs.Add('end.');
    ToolSupp.BackupFile(fn);
    ustrs.SaveToFile(Path(fn));
    Result := 0;
  finally
    FreeAndNil(ustrs);
  end;
end;

function TSFXStrMaker.WStr(ident: Word): WideString;
var
  i: Integer;
  id: Word;
  p: Integer;
  sz: Word;
begin
  Result := '';
  p := 0;
  if (StrTable = nil) or (ident = 0) then
    exit;
  id := StrTable[p];
  while (id <> 0) and (id <> ident) do
  begin
    Inc(p, 2 + StrTable[p + 1]);
    id := StrTable[p];
  end;
  if id <> ident then
    exit;
  Inc(p);
  sz := StrTable[p];
  SetLength(Result, sz);
  for I := 1 to sz do
    Result[i] := Widechar(StrTable[p + i]);
end;

constructor TMsgStr.Create;
begin
  fU8Str := '';
end;

destructor TMsgStr.Destroy;
begin
  inherited;
end;

function TMsgStr.GetWStr: WideString;
begin
  Result := UTF8ToWide(fU8Str, -1);
end;

procedure TMsgStr.SetWStr(const Value: WideString);
begin
  if Length(Value) > 0 then
    fU8Str := WideToUTF8(Value, -1)
  else
    fU8Str := '';
end;

end.
