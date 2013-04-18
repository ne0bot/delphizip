unit UWriter;

(* ***************************************************************************
  UWriter.pas - writer for message string units
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

interface

uses
  classes, Main, Idents, StrMaker;

type
  WBlocks = packed array of Cardinal;

const
  MIN_ID = 1;//0101;
  MAX_ID = 11620;

const
  Err_Ok = 0;
  Err_Nothing = 1;
  Err_Bad = 2;
  Err_NoIDs = 3;
  Err_BadIDs = 4;
  Err_NoStrings = 5;
  Err_BadStrings = 6;
  Err_Compression = 7;
  Err_Compile = 8;
  Err_Unknown = -1;

(* format
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
  TRangeEntry = packed record
    MaxVal: Word;
    MinVal: Word;
    Base: Word;
  end;
  TRangeArray = packed array of TRangeEntry;
  TOffsetsArray = packed array of Word;

// CS?? compressed block All
// CX?? compressed shared and SFX
// MS?? strings ZipMaster only
// XS?? strings shared and SFX
type
  TUnitWriter = class(TBlokMaker)
  private
    fIdents: TIdentifiers;
    fErr:  Integer;
    fposn: Integer;
    fBlock: WBlocks;
    function GetCSTot: Integer;
    function GetIDTot: Integer;
  protected
    property Idents: TIdentifiers read fIdents;
    property Block: WBlocks read fBlock;
    property IDTot: Integer read GetIDTot;
    property CSTot: Integer read GetCSTot;
  public
    constructor Create;
    destructor Destroy; OVERRIDE;
    function PrepareCompDefBlock: integer;
    function WriteDefMsgs(const where: string): Integer;
    function WriteZipMsg(const where: string): integer;
    property Err: Integer read fErr write fErr;
  end;

implementation

uses Windows, SysUtils, StrUtils, ToolHelper;

{$WARN SYMBOL_DEPRECATED OFF}

function NormPasStr(const s: String): String;
var
  r: String;
  p: Integer;
begin
  Result := '';
  r := s;
  p := AnsiPos('\n', s);
  while p > 0 do
  begin
    Result := Result + AnsiQuotedStr(copy(r, 1, pred(p)), '''') + '#10';
    r := copy(r, p + 2, Length(r) - (p + 1));
    p := AnsiPos('\n', r);
  end;
  if r <> '' then
    Result := Result + AnsiQuotedStr(r, '''');
end;

function NormCStr(const s: String): String;
begin
  Result := AnsiQuotedStr(s, '"');
end;

{ TUnitWriter }

function TUnitWriter.GetCSTot: Integer;
begin
  Result := succ(High(Block));
end;


function TUnitWriter.GetIDTot: Integer;
begin
  Result := Idents.CountOf([idActive, idShared, idSfx]);
end;

constructor TUnitWriter.Create;
begin
  inherited;
  fBlock := Nil;
  fIdents := Nil;
  fErr := 0;
  fPosn := 0;
end;

destructor TUnitWriter.Destroy;
begin
  FreeAndNil(fIdents);
  fBlock := Nil;
  inherited;
end;

function TUnitWriter.WriteDefMsgs(const where: string): Integer;
var
  ustrs: TStringList;
  idx, ix, i: integer;
  s: string;
begin
  ustrs := nil;
//  Result := -1;
  try
    ustrs := TStringList.Create;
    ustrs.Add('Unit ZMDefMsgs;');
    ustrs.Add(' ');
    ustrs.Add('(* Built by ZipResMaker');
    ustrs.Add('   DO NOT MODIFY');
    ustrs.Add('  ZMDefMsgs.pas - default messages and compressed tables');
    ustrs.Add('  TZipMaster VCL by Chris Vleghert and Eric W. Engler');
    ustrs.Add('');
    ustrs.Add('  ' + VCL_VER_STRING);//ToolSupp.VerString[vsVer]);//v1.90');
    ustrs.Add('      Copyright (C) 2009, 2010, 2011, 2012, 2013  Russell Peters');
    ToolSupp.Licence(ustrs);
    ustrs.Add(' ');
    ustrs.Add('  modified '+ DateTimeToStr(Now));
    ustrs.Add('---------------------------------------------------------------------------*)');
    ustrs.Add(' ');
    ustrs.Add('Interface');
    ustrs.Add(' ');
    ustrs.Add('Uses');
    ustrs.Add('  ZMMsg;');
    ustrs.Add(' ');
    ustrs.Add('{$I  ''.\ZMConfig191.inc''}');
    ustrs.Add(' ');
    ustrs.Add('{$IFNDEF USE_COMPRESSED_STRINGS}');
    ustrs.Add(' ');
    ustrs.Add('type');
    ustrs.Add('  TZipResRec = packed record');
    ustrs.Add('    i: Word;');
    ustrs.Add('    s: pResStringRec;');
    ustrs.Add('  end;');
    ustrs.Add(' ');
    ustrs.Add('ResourceString');
    for i := 0 to pred(IDTot) do
    begin
      ix := Idents.IndexAt(i, [idActive{, idShared, idSfx}]);
      s := '  _' + Idents.Name[ix] + ' = ' + NormPasStr(Idents.Msg[ix]) + ';';
      ustrs.Add(s);
    end;
    ustrs.Add(' ');
    ustrs.Add('const');
    ustrs.Add(Format('ResTable: array [0..%d] of TZipResRec = (',[pred(IDTot)]));
    s := '  ';
    for i := 0 to pred(IDTot) do
    begin
      ix := Idents.IndexAt(i, [idActive, idShared, idSfx]);
      s := '    (i: ' + Idents.Name[ix] + '; s: @_' + Idents.Name[ix] + ')';
      if i < pred(IDTot) then
        s := s + ','
      else
        s := s + ');';
      ustrs.Add(s);
    end;
    ustrs.Add(' ');
    ustrs.Add('{$ELSE}');
    ustrs.Add(' ');
    ustrs.Add('const');
    ustrs.Add(Format(' CompBlok: array [0..%d] of Cardinal = (',[pred(CSTot)]));
    s := '  ';
    for idx := 0 to CSTot-2 do
    begin
      s := s + '$'+IntToHex(Block[idx], 8)+', ';
      if length(s) > 60 then
      begin
        ustrs.add(s);
        s := '  ';
      end;
    end;
    s := s + '$'+IntToHex(Block[pred(CSTot)], 8)+' );';
    if Length(s) > 4 then
        ustrs.add(s);
    ustrs.Add('{$ENDIF}');
    Result := -1;
    ustrs.Add(' ');
    ustrs.Add('implementation');
    ustrs.Add(' ');
    ustrs.Add('end.');
    ToolSupp.BackupFile(where);
    ustrs.SaveToFile(where);
    Result := 0;
  finally
    FreeAndNil(ustrs);
  end;
end;

function TUnitWriter.WriteZipMsg(const where: string): integer;
var
  ustrs: TStringList;
  idx, ix: integer;
  s: string;
begin
  ustrs := nil;
//  Result := -1;
  try
    ustrs := TStringList.Create;
    ustrs.Add('Unit ZMMsg;');
    ustrs.Add(' ');
    ustrs.Add('(* Built by ZipResMaker');
    ustrs.Add('   DO NOT MODIFY');
    ustrs.Add('  ZMMsg.pas - Message Identifiers');
    ustrs.Add('  TZipMaster VCL by Chris Vleghert and Eric W. Engler');
    ustrs.Add('');
    ustrs.Add('  ' + VCL_VER_STRING);
    ustrs.Add('      Copyright (C) 2008, 2009, 2010, 2011, 2012, 2013  Russell Peters');
    ustrs.Add(' ');
    ToolSupp.Licence(ustrs);
    ustrs.Add('  modified '+ DateTimeToStr(Now));
    ustrs.Add('---------------------------------------------------------------------------*)');
    ustrs.Add(' ');
    ustrs.Add(' ');
    ustrs.Add('Interface');
    ustrs.Add(' ');
    ustrs.Add('Const');
    for idx := 0 to pred(Idents.CountOf([idActive, idShared{, idSfx}])) do
    begin
      ix := Idents.IndexAt(idx, [idActive, idShared, idSfx]);
      s := Format('  %s = %d;',[ Idents.Name[ix],Idents.Id[ix]]);
      ustrs.Add(s);
    end;
    ustrs.Add(' ');
    ustrs.Add('const');
    ustrs.Add(' MSG_ID_MASK = $1FF;');
    ustrs.Add(' ');
    ustrs.Add('// name of compressed resource data');
    ustrs.Add('const ');
    ustrs.Add('  DZRES_Str = ''DZResStr191'';  // compressed language strings');
    ustrs.Add(
      '  DZRES_SFX = ''DZResSFX191'';  // stored UPX Dll version as string');
    ustrs.Add('  DZRES_Dll = ''DZResDll191'';  // stored UPX Dll');

    ustrs.Add(' ');
    ustrs.Add('implementation');
    ustrs.Add(' ');
    ustrs.Add('end.');
    ToolSupp.BackupFile(where);
    ustrs.SaveToFile(where);
    Result := 0;
  finally
    FreeAndNil(ustrs);
  end;
end;

function FindOffsetsIndex(Table: TRangeArray; Req: Integer): Integer;
var
  Idx: Integer;
begin
  Result := 0; // not found
  Idx := 0;
  while Req > Table[Idx].MaxVal do
    Inc(Idx);
  if Table[Idx].MaxVal = $FFFF then
    Exit; // not found
  Result := Req - Table[Idx].MinVal;
  if Result < 0 then
    Exit; // not in range
  Result := Table[Idx].Base + Result; // index of Offsets entry
end;

function TUnitWriter.PrepareCompDefBlock: integer;
var
  blksiz: Integer;
  i: Integer;
  K: Cardinal;
  siz: Integer;
  TableStream: TMemoryStream;
begin
  TableStream := TMemoryStream.Create;
  try
    i := WriteCompLangBlok(TableStream, '$(ZM_Lang)\ZipMsgUS.rc', True);
    if i > 0 then
    begin
      ToolSupp.Show('Compressed Table size = ' + IntToStr(i));
      blksiz := Integer(TableStream.Size);
      K := 0; // pad so can read whole cardinals
      TableStream.Write(K, sizeof(Cardinal));
      blksiz := (blksiz + sizeof(Cardinal) - 1) div sizeof(Cardinal);
      SetLength(fBlock, blksiz);
      siz := blksiz * SizeOf(Cardinal);
      TableStream.Position := 0;
      if TableStream.Read(fBlock[0], siz) <> siz then
      begin
        TableStream.Size := 0;
        ToolSupp.Show('Compression error');
        Err := Err_Unknown;
        Result := Err;
        exit;
      end;
      Result := 0;
    end
    else
    begin
      TableStream.Size := 0;
      ToolSupp.Show('Compression error');
      Err := Err_Unknown;
      Result := Err;
      exit;
    end;
  finally
    TableStream.Free;
  end;
end;

end.
