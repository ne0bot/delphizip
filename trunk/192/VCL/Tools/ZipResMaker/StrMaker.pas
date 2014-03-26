unit StrMaker;

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
// modified 2011-10-16

interface

uses
  Classes, Idents, ZipMstr;

type
  WBlocks = packed array of Cardinal;

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

  TRangeArray   = packed array of TRangeEntry;
  TOffsetsArray = packed array of Word;

type
  TBlokMaker = class(TIdentifiers)
  private
    fErr: Integer;
    function FixNewLine(Msg: WideString): WideString;
    function _MakeRanges(var Ranges: TRangeArray;
      var OffsetsCount: Integer): Integer;
  protected
    function CompressLangBlock(OutStream: TStream; Table: TMemoryStream): Integer;
    function LoadIdentifiers: Integer;
    function MakeTable(Table: TStream; const FileName: string; AsAnsi: boolean):
        Integer;
  public
    constructor Create;
    destructor Destroy; OVERRIDE;
    procedure Diag(const Msg: String);
    function LangLabel: string;
    function WriteCompLangBlok(CompStream: TStream; const RCFile: string;
      IsDefault: boolean): Integer;
    function ZLoadStr(const Table: pointer; Id: Cardinal): string;
    property Err: Integer read fErr write fErr;
  end;

implementation

uses
  Windows, SysUtils, StrUtils, Main, ZMMsg, ToolHelper;

{$WARN SYMBOL_DEPRECATED OFF}

function NormPasStr(const s: String): String;
var
  p: Integer;
  r: String;
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

constructor TBlokMaker.Create;
begin
  inherited;
  fErr := 0;
end;

destructor TBlokMaker.Destroy;
begin
  inherited;
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

// lcid: word, data_size: word, label: AnsiChar[4], data: byte[];
function TBlokMaker.CompressLangBlock(OutStream: TStream; Table:
    TMemoryStream): Integer;
var
  K: Cardinal;
  Lbl: AnsiString;
  posn: Int64;
  strm: TMemoryStream;
  ssize: Integer;
  Zip: TZipMaster;
begin
  Result := -1;
  posn := OutStream.Position;
  strm := TMemoryStream.Create;
  try
    Table.Position := 0;
    Zip := TZipMaster.Create(nil);
    try
      if Zip.AddStreamToStream(Table) <> nil then
      begin
        // keep crc
        ssize := Zip.ZipStream.Size - SizeOf(Word);
        if ssize > 0 then
        begin
          Zip.ZipStream.Position := SizeOf(Word);;
          strm.CopyFrom(Zip.ZipStream, ssize);
        end;
      end
      else
      begin
        ToolSupp.Show('Compression error');
        strm.Size := 0;
      end;
    finally
      Zip.Free;
    end;
    if strm.Size > 0 then
    begin
      K := UsingLCID;
      OutStream.WriteBuffer(K, sizeof(Word));
      K := strm.Size and $7FFF;
      OutStream.WriteBuffer(K, sizeof(Word));
      Lbl := AnsiString(LangLabel);
      if Length(Lbl) > 4 then
        SetLength(Lbl, 4)
      else
        while Length(Lbl) < 4 do
          Lbl := Lbl + #0; // pad to 4 bytes
      OutStream.WriteBuffer(pAnsiChar(Lbl)^, 4);
      strm.Position := 0;
      OutStream.CopyFrom(strm, strm.Size);
      Result := Integer(OutStream.Size - posn);
    end;
  finally
    strm.Free;
  end;
end;

procedure TBlokMaker.Diag(const Msg: String);
begin
  ToolSupp.Show(Msg);
end;

function TBlokMaker.FixNewLine(Msg: WideString): WideString;
var
  i: Integer;
  Len: Integer;
begin
  Result := '';
  Len := Length(Msg);
  i := 0;
  while i < Len do
  begin
    Inc(i);
    if (Msg[i] = '\') and (i < Len) and (Msg[i + 1] = 'n') then
    begin
      Result := Result + #13#10;
      Inc(i);
    end
    else
      Result := Result + Msg[i];
  end;
end;

function TBlokMaker.LangLabel: string;
const
  Allowed = ['a' .. 'z', 'A' .. 'Z', '0' .. '9'];
var
  desc: string;
  i: Integer;
begin
  Result := '';
  desc := MsgOfId(ZX_Language);
  i := 1;
  while (i <= Length(desc)) and CharInSet(desc[i], Allowed) do
    Inc(i);
  Dec(i);
  if i > 0 then
    Result := Uppercase(copy(desc, 1, i));
end;

function TBlokMaker.MakeTable(Table: TStream; const FileName: string; AsAnsi:
    boolean): Integer;
var
  AString: AnsiString;
  CurrentId: Integer;
  CurrentMsg: TZMWideString;
  IdentIdx: Integer;
  InitSize: Integer;
  K: Cardinal;
  MemStream: TMemoryStream;
  MsgOffset: Integer;
  MsgsBase: Integer;
  n: Integer;
  Offsets: TOffsetsArray;
  OffsetsBase: Integer;
  OffsetsCount: Integer;
  OffsetsIdx: Integer;
  RangeCount: Integer;
  Ranges: TRangeArray;
  rcf: string;
  W: Word;
begin
  Err := 0;
  InitSize := Table.Position;
  rcf := ToolSupp.Path(FileName);
  if ReadRCFile(rcf, AsAnsi{True}) <> 0 then
  begin
    ToolSupp.Show('error reading identifiers : ' + rcf);
    Err := Err_NoIDs;
    Result := -Err;
    Exit;
  end;
  RangeCount := _MakeRanges(Ranges, OffsetsCount);
  // number of words (4 pre + ranges + end_mark)
  OffsetsBase := 5 + (RangeCount * 3);
  // Dump Ranges
  // preamble first
  W := 0;
  Table.WriteBuffer(W, sizeof(Word));
  K := Cardinal(DateTimeToFileDate(Now));
  Table.WriteBuffer(K, sizeof(Cardinal));
  if not AsAnsi then
    W := 1;
  Table.WriteBuffer(W, sizeof(Word));
  // the ranges table
  Table.WriteBuffer(Ranges[0], RangeCount * sizeof(TRangeEntry));
  W := $FFFF; // mark end
  Table.WriteBuffer(W, sizeof(Word));
  // verify
  n := (Integer(Table.Position) - InitSize) div sizeof(Word);
  Assert(n = OffsetsBase, ' error - wrong Offsets table base');
  // make offsets table
  SetLength(Offsets, OffsetsCount);
  ZeroMemory(@Offsets[0], OffsetsCount * sizeof(Word));
  MsgsBase := OffsetsBase + OffsetsCount; // WideChar offset
  if AsAnsi then
    MsgsBase := MsgsBase * sizeof(Word); // AnsiChar offset
  // save msgs to memory stream
  MemStream := TMemoryStream.Create;
  try
    W := 0;
    // 2nd pass
    for IdentIdx := 0 to Count - 1 do
    begin
      if not(Flag[IdentIdx] in [idActive{, idShared}]) then
        continue; // skip discarded etc
      CurrentMsg := Msg[IdentIdx];
      if CurrentMsg = '' then
        continue; // skip empty
      CurrentId := Id[IdentIdx];
      n := Integer(MemStream.Size);
      if not AsAnsi then
        n := n div sizeof(Word); // Word aligned
      MsgOffset := MsgsBase + n;
      OffsetsIdx := FindOffsetsIndex(Ranges, CurrentId) - OffsetsBase;
      if (OffsetsIdx < 0) or (OffsetsIdx >= OffsetsCount) then
      begin
        ToolSupp.Show('Unknown identifier : ' + IntToStr(CurrentId));
        Err := Err_BadIDs;
        Result := -Err;
        Exit;
      end;
      if Offsets[OffsetsIdx] <> 0 then
      begin
        ToolSupp.Show('Duplicate identifier : ' + IntToStr(CurrentId));
        Err := Err_BadIDs;
        Result := -Err;
        Exit;
      end;
      CurrentMsg := FixNewLine(CurrentMsg);
      // write Msg
      if AsAnsi then
      begin
        AString := AnsiString(CurrentMsg);
        MemStream.WriteBuffer(AString[1], Length(AString));
        MemStream.WriteBuffer(W, sizeof(AnsiChar));
      end
      else
      begin
        MemStream.WriteBuffer(CurrentMsg[1], Length(CurrentMsg) *
          sizeof(WideChar));
        MemStream.WriteBuffer(W, sizeof(WideChar));
      end;
      Offsets[OffsetsIdx] := MsgOffset;
    end;
    // write offsets table
    Table.WriteBuffer(Offsets[0], OffsetsCount * sizeof(Word));
    // the messages
    MemStream.Position := 0;
    Table.CopyFrom(MemStream, MemStream.Size);
    // finished
    Result := Integer(Table.Size) - InitSize;
  finally
    MemStream.Free;
  end;
end;

function TBlokMaker.LoadIdentifiers: Integer;
begin
  Result := 0;
  Clear;  // remove any old lists
  if CountryInfo.LoadFromFile(ToolSupp.Path('$(ZM_Lang)\Countries.txt')) < 170 then
  begin
    ToolSupp.Show('error reading countries');
    Err := Err_NoIDs;
    Result := Err;
    Exit;
  end;
  if LoadHFile(ToolSupp.Path('$(ZM_Lang)\ZipMsg.h')) < 170 then
  begin
    ToolSupp.Show('error reading identifiers');
    Err := Err_NoIDs;
    Result := Err;
  end;
end;

function TBlokMaker.WriteCompLangBlok(CompStream: TStream; const RCFile: string;
  IsDefault: boolean): Integer;
var
  i: Integer;
  InitPosn: Integer;
  InitSize: Integer;
  K: Cardinal;
  rcf: string;
  siz: Integer;
  TableStream: TMemoryStream;
begin
  Result := LoadIdentifiers;
  if Result <> 0 then
    Exit;
  InitSize := CompStream.Size;
  InitPosn := CompStream.Position;
  rcf := ToolSupp.Path(RCFile);
  TableStream := TMemoryStream.Create;
  try
    i := MakeTable(TableStream, rcf, IsDefault);
    if i <= 0 then
    begin
      ToolSupp.Show(' failed to create string TableStream, result = ' + IntToStr(i));
      Err := Err_Unknown;
      Result := Err;
      Exit;
    end;

    Result := CompressLangBlock(CompStream, TableStream);
    if Result <= 0 then
    begin
      CompStream.Position := InitPosn;
      CompStream.Size := InitSize;
      ToolSupp.Show('Compression error');
      Err := Err_Unknown;
      Result := 0;
      Exit;
    end;
    K := 0;
    siz := (Result + (SizeOf(Cardinal) - 1) div SizeOf(Cardinal)) - Result;
    CompStream.WriteBuffer(K, siz); // pad to cardinal size
    Result := Result + siz;
    ToolSupp.Show(rcf + ' language TableStream size = ' + IntToStr(i) +
      ' compressed ' + IntToStr(Result));
  finally
    TableStream.Free;
  end;
end;

function TBlokMaker.ZLoadStr(const Table: pointer; Id: Cardinal): string;
var
  AMsg: pAnsiChar;
  IsAnsi: boolean;
  Offset: Integer;
  p: PWORD;
  WMsg: PWideChar;
begin
  Result := ''; // not found
  p := PWORD(Table);
  Inc(p, 3); // => Ansi/UTF16 flag
  IsAnsi := p^ = 0;
  Inc(p); // => Ranges[0].MaxVal
  while Id > p^ do
    Inc(p, sizeof(TRangeEntry) div sizeof(Word)); // 3);
  if p^ = $FFFF then
    Exit; // not found
  Inc(p); // => MinVal
  Offset := Id - p^;
  if Offset < 0 then
    Exit; // not in range
  Inc(p); // => Base
  Offset := p^ + Offset; // index of Offsets entry
  p := PWORD(Table);
  Inc(p, Offset);
  Offset := p^;
  if Offset <> 0 then
  begin
    if IsAnsi then
    begin
      AMsg := pAnsiChar(Table);
      Inc(AMsg, Offset);
      Result := string(AMsg);
    end
    else
    begin
      WMsg := PWideChar(Table);
      Inc(WMsg, Offset);
      Result := string(WMsg);
    end;
  end;
end;

function TBlokMaker._MakeRanges(var Ranges: TRangeArray;
  var OffsetsCount: Integer): Integer;
const
  MAX_GAP = 4; // max empty entries
var
  i: Integer;
  IdentIdx: Integer;
  n: Integer;
  RangeLevel: Integer;
  RCurrent: Integer;
  RPrev: Integer;
begin
  // First pass - create ranges (assumes Indents sorted by ID)
  SetLength(Ranges, 50);
  RangeLevel := 0;
  OffsetsCount := 0;
  Ranges[RangeLevel].MinVal := Id[0];
  Ranges[RangeLevel].MaxVal := 0;
  Ranges[RangeLevel].Base := 0;
  RPrev := Ranges[RangeLevel].MinVal;
  RangeLevel := 0;
  for IdentIdx := 0 to Count - 1 do
  begin
    if not(Flag[IdentIdx] in [idActive{, idShared}]) then
      continue; // skip discarded etc
    if Msg[IdentIdx] = '' then
      continue; // skip empty

    RCurrent := Id[IdentIdx];
    if RCurrent > (RPrev + (MAX_GAP + 1)) then
    begin
      // end of range
      Ranges[RangeLevel].MaxVal := RPrev;
      // sum number of entires in offsets Ranges
      n := ((Ranges[RangeLevel].MaxVal - Ranges[RangeLevel].MinVal) + 1);
      OffsetsCount := OffsetsCount + n;
      Inc(RangeLevel);
      if RangeLevel > High(Ranges) then
        SetLength(Ranges, RangeLevel + 10); // allow more room
      Ranges[RangeLevel].Base := OffsetsCount;
      Ranges[RangeLevel].MinVal := RCurrent;
    end;
    RPrev := RCurrent;
  end;
  Ranges[RangeLevel].MaxVal := RPrev;
  // sum number of entires in offsets Ranges
  n := ((Ranges[RangeLevel].MaxVal - Ranges[RangeLevel].MinVal) + 1);
  OffsetsCount := OffsetsCount + n;
  // correct base offsets
  n := 5 + ((RangeLevel + 1) * 3);
  // number of words (4 pre + ranges + end_mark)
  for i := 0 to RangeLevel do
    Ranges[i].Base := Ranges[i].Base + n;
  Inc(RangeLevel); // number of levels
  if RangeLevel > High(Ranges) then
    SetLength(Ranges, RangeLevel + 1); // allow more room
  Ranges[RangeLevel].MaxVal := $FFFF; // mark end
  Result := RangeLevel;
end;

end.
