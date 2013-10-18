unit ZMIRec;

//  ZMIRec.pas - Represents the 'Directory entry' of a Zip file

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
//modified 2011-11-20

{$INCLUDE   '.\ZipVers.inc'}
{$IFDEF VER180}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Classes, Windows, ZipMstr, ZMStructs, ZMCompat;

type
  TZMRecStrings = (zrsName, zrsComment, zrsIName);
  TZMSelects    = (zzsClear, zzsSet, zzsToggle);
//  TZipSelects          = (zzsClear, zzsSet, zzsToggle);
  TZMStrEncOpts = (zseDOS, zseXName, zseXComment);
  TZMStrEncodes = set of TZMStrEncOpts;

// ZipDirEntry status bit constants
const
  zsbHashed = $100;     // hash calculated
  zsbLocalDone = $200;  // local data prepared
  zsbLocal64 = $400;    // local header required zip64

  zsbEncMask = $70000;  // mask for bits holding how entry is encoded
  zsbRenamed = $80000;  // written to different name

type
  TZSExtOpts = (zsxUnkown, zsxName, zsxComment, zsxName8, zsxComment8);
  TZStrExts = set of TZSExtOpts;

type
  THowToEnc = (hteOEM, hteAnsi, hteUTF8);


type
  TZMIRec = class(TZMDirRec)
  private
    fComprMethod:    Word;            //compression method(2)
    fComprSize:      Int64;           //compressed file size  (8)
    fCRC32:          Longword;        //Cyclic redundancy check (4)
    fDiskStart:      Cardinal;        //starts on disk number xx(4)
    fExtFileAtt:     Longword;        //external file attributes(4)
    FHash:           Cardinal;
    fIntFileAtt:     Word;            //internal file attributes(2)
    FLocalData:      TZMRawBytes;
    fModifDateTime:  Longword;        // dos date/time          (4)
    fRelOffLocal:    Int64;
    FSelectArgs: string;
    fStatusBits:     Cardinal;
    fUnComprSize:    Int64;           //uncompressed file size (8)
    fVersionNeeded:    Word;          // version needed to extract(2)
    FXName: TZMString;
    function GetHash: Cardinal;
    function GetIsEncoded: TZMEncodingOpts;
    function GetSelected: Boolean;
    function GetStatusBit(Mask: Cardinal): Cardinal;
    function GetXName: TZMString;
    procedure SetIsEncoded(const Value: TZMEncodingOpts);
    procedure SetSelected(const Value: Boolean);
  protected
    function FindDataTag(tag: Word; var idx, siz: Integer): Boolean;
    function GetCompressedSize: Int64; override;
    function GetCompressionMethod: Word; override;
    function GetCRC32: Cardinal; override;
    function GetDateTime: Cardinal; override;
    function GetDirty: Boolean;
    function GetExtFileAttrib: Longword; override;
    function GetExtraData(Tag: Word): TZMRawBytes; override;
    function GetFileNameLength: Word; override;
    function GetIntFileAttrib: Word; override;
    function GetRelOffLocalHdr: Int64; override;
    function GetStartOnDisk: Word; override;
    function GetStatusBits: Cardinal; override;
    function GetUncompressedSize: Int64; override;
    function GetVersionNeeded: Word; override;
    function IsZip64: Boolean;
    procedure MarkDirty;
    procedure SetDateStamp(Value: TDateTime);
    function WriteDataDesc: Integer; virtual; abstract;
    property LocalData: TZMRawBytes read FLocalData write FLocalData;
  public
    constructor Create;
    procedure AfterConstruction; override;
    procedure AssignFrom(const zr: TZMIRec); virtual;
    function CentralSize: Cardinal;
    function ChangeAttrs(nAttr: Cardinal): Integer; override;
    function ClearStatusBit(const Values: Cardinal): Cardinal;
    function HasChanges: Boolean;
    function HasDataDesc: Boolean;
    function Process: Int64; virtual;
    function ProcessSize: Int64; virtual;
    function SafeHeaderName(const IntName: TZMString): TZMString;
    function SeekLocalData: Integer; virtual; abstract;
    function Select(How: TZMSelects): Boolean;
    function SetStatusBit(const Value: Cardinal): Cardinal;
    function TestStatusBit(const mask: Cardinal): Boolean;
    property CompressedSize: Int64 Read fComprSize Write fComprSize;
    property ComprMethod: Word Read fComprMethod Write fComprMethod;
    property CRC32: Longword Read fCRC32 Write fCRC32;
    property DiskStart: Cardinal Read fDiskStart Write fDiskStart;
    property Encoded: TZMEncodingOpts Read GetEncoded;
    property ExtFileAttrib: Longword Read fExtFileAtt Write fExtFileAtt;
    property ExtraFieldLength: Word read GetExtraFieldLength;
    property FileComment: TZMString Read GetFileComment;
    property FileName: TZMString Read GetFileName;
    property Hash: Cardinal read GetHash;
    property IntFileAttrib: Word Read fIntFileAtt Write fIntFileAtt;
    //1 the cached value in the status
    property IsEncoded: TZMEncodingOpts read GetIsEncoded write SetIsEncoded;
    property ModifDateTime: Longword Read fModifDateTime Write fModifDateTime;
    property RelOffLocal: Int64 Read fRelOffLocal Write fRelOffLocal;
    property SelectArgs: string read FSelectArgs write FSelectArgs;
    property Selected: Boolean Read GetSelected Write SetSelected;
    property StatusBit[Mask: Cardinal]: Cardinal read GetStatusBit;
    property StatusBits: Cardinal Read GetStatusBits Write fStatusBits;
    property UncompressedSize: Int64 read fUnComprSize write fUnComprSize;
    property VersionNeeded: Word Read fVersionNeeded Write fVersionNeeded;
    property XName: TZMString read GetXName write FXName;
  end;

function IsInvalidIntName(const FName: TZMString): Boolean;

implementation
uses
  SysUtils, ZMMsg, ZMXcpt, ZMMsgStr, ZMUtils,
  ZMUTF8, ZMMatch, ZMCore, ZMDelZip;

const
  __UNIT__ = 19 shl 23;

const
  MAX_BYTE = 255;

type
  Txdat64 = packed record
    tag:  Word;
    siz:  Word;
    vals: array [0..4] of Int64;  // last only cardinal
  end;

const
  ZipCenRecFields: array [0..17] of Integer =
    (4, 1, 1, 2, 2, 2, 2, 2, 4, 4, 4, 2, 2, 2, 2, 2, 4, 4);

// make safe version of external comment
function SafeComment(const xcomment: String): string;
var
  c: Char;
  i: integer;
Begin
  if StrHasExt(xcomment) then
    Result := StrToOEM(xcomment)
  else
    Result := xcomment;
  for i := 1 to Length(Result) do
  begin
    c := Result[i];
    if (c < ' ') or (c > #126) then
      Result[i] := '_';
  end;
End;

{ TZMIRec }
constructor TZMIRec.Create;
begin
  inherited Create;
end;

procedure TZMIRec.AssignFrom(const zr: TZMIRec);
begin
  inherited;
  if (zr <> self) and (zr is TZMIRec) then
  begin
    VersionNeeded := zr.VersionNeeded;
    ComprMethod := zr.ComprMethod;
    ModifDateTime := zr.ModifDateTime;
    CRC32 := zr.CRC32;
    CompressedSize := zr.CompressedSize;
    UncompressedSize := zr.UncompressedSize;
    DiskStart := zr.DiskStart;
    IntFileAttrib := zr.IntFileAttrib;
    ExtFileAttrib := zr.ExtFileAttrib;
    RelOffLocal := zr.RelOffLocal;
    StatusBits := zr.StatusBits;
    fHash := zr.FHash;
    FXName := zr.FXName;
  end;
end;

function TZMIRec.CentralSize: Cardinal;
begin
  Result := SizeOf(TZipCentralHeader);
  Inc(Result, FileNameLength + ExtraFieldLength + FileCommentLen);
end;

function TZMIRec.ChangeAttrs(nAttr: Cardinal): Integer;
begin
  Result := 0; // always allowed
  if nAttr <> GetExtFileAttrib then
  begin
    ExtFileAttrib := nAttr;
    MarkDirty;
  end;
end;

// returns previous values
function TZMIRec.ClearStatusBit(const Values: Cardinal): Cardinal;
begin
  Result := StatusBits and Values;
  StatusBits := StatusBits and not Values;
end;

function TZMIRec.FindDataTag(tag: Word; var idx, siz: Integer): Boolean;
begin
  Result := False;
  if XData(ExtraField, tag, idx, siz) then
    Result := True;
end;

function IsOnlyDOS(const hstr: TZMRawBytes): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(hstr) do
    if (hstr[i] > #126) or (hstr[i] < #32) then
    begin
      Result := False;
      Break;
    end;
end;

function TZMIRec.GetCompressedSize: Int64;
begin
  Result := fComprSize;
end;

function TZMIRec.GetCompressionMethod: Word;
begin
  Result := fComprMethod;
end;

function TZMIRec.GetCRC32: Cardinal;
begin
  Result := fCRC32;
end;

function TZMIRec.GetDateTime: Cardinal;
begin
  Result := fModifDateTime;
end;

function TZMIRec.GetDirty: Boolean;
begin
  Result := TestStatusBit(zsbDirty);
end;

function TZMIRec.GetExtFileAttrib: Longword;
begin
  Result := fExtFileAtt;
end;

// returns the 'data' without the tag
function TZMIRec.GetExtraData(Tag: Word): TZMRawBytes;
var
  i: Integer;
  sz: Integer;
  x: TZMRawBytes;
begin
  Result := '';
  x := GetExtraField;
  if XData(x, Tag, i, sz) then
    Result := Copy(x, i + 4, sz - 4);
end;

function TZMIRec.GetFileNameLength: Word;
begin
  Result := Length(HeaderName);
end;

function TZMIRec.GetHash: Cardinal;
begin
  if not TestStatusBit(zsbHashed) then
  begin
    fHash := HashFuncNoCase(FileName);
    SetStatusBit(zsbHashed);
  end;
  Result := fHash;
end;

function TZMIRec.GetIntFileAttrib: Word;
begin
  Result := fIntFileAtt;
end;

function TZMIRec.GetIsEncoded: TZMEncodingOpts;
var
  n: Integer;
begin
  n := StatusBit[zsbEncMask] shr 16;
  if n > ord(zeoUPath) then
    n := 0;
  if n = 0 then
  begin
    // unknown - work it out and cache result
    Result := Encoded;
    SetIsEncoded(Result);
  end
  else
    Result := TZMEncodingOpts(n);
end;

function TZMIRec.GetRelOffLocalHdr: Int64;
begin
  Result := fRelOffLocal;
end;

function TZMIRec.GetSelected: Boolean;
begin
  Result := TestStatusBit(zsbSelected);
end;

function TZMIRec.GetStartOnDisk: Word;
begin
  Result := fDiskStart;
end;

function TZMIRec.GetStatusBit(Mask: Cardinal): Cardinal;
begin
  Result := StatusBits and mask;
end;

function TZMIRec.GetStatusBits: Cardinal;
begin
  Result := fStatusBits;
end;

function TZMIRec.GetUncompressedSize: Int64;
begin
  Result := fUnComprSize;
end;

function TZMIRec.GetVersionNeeded: Word;
begin
  Result := fVersionNeeded;
end;

function TZMIRec.HasChanges: Boolean;
begin
  Result := (StatusBits and zsbDirty) <> 0;
end;

// test for invalid characters
function IsInvalidIntName(const FName: TZMString): Boolean;
var
  c: Char;
  clen: Integer;
  i: Integer;
  len: Integer;
  n: Char;
  p: Char;
begin
  Result := True;
  len := Length(FName);
  if (len < 1) or (len >= MAX_PATH) then
    exit;                                   // empty or too long
  c := FName[1];
  if (c = PathDelim) or (c = '.') or (c = ' ') then
    exit;                                   // invalid from root or below
  i := 1;
  clen := 0;
  p := #0;
  while i <= len do
  begin
    Inc(clen);
    if clen > 255 then
      exit; // component too long
    c := FName[i];
    if i < len then
      n := FName[i + 1]
    else
      n := #0;
    case c of
      WILD_MULTI, DriveDelim, WILD_CHAR, '<', '>', '|', #0:
        exit;
      #1..#31:
        exit; // invalid
      PathDelimAlt:
      begin
        if p = ' ' then
          exit;   // bad - component has Trailing space
        if (n = c) or (n = '.') or (n = ' ') then
          exit; // \\ . leading space invalid
        clen := 0;
      end;
      '.':
      begin
        n := FName[succ(i)];
        if (n = PathDelim) or (n < ' ') then
          exit;
      end;
      ' ':
        if i = len then
          exit;   // invalid
    end;
    p := c;
    Inc(i);
  end;
  Result := False;
end;

procedure TZMIRec.AfterConstruction;
begin
  inherited;
  fStatusBits := 0;
end;

function TZMIRec.GetXName: TZMString;
begin
  Result := FXName;
  if Result = '' then
    Result := FileName;
end;

function TZMIRec.HasDataDesc: Boolean;
begin
  Result := (Flag and FLAG_DATADESC_BIT) <> 0;
end;

function TZMIRec.IsZip64: Boolean;
begin
  Result := (UncompressedSize >= MAX_UNSIGNED) or
    (CompressedSize >= MAX_UNSIGNED) or
    (RelOffLocal >= MAX_UNSIGNED) or (DiskStart >= MAX_WORD);
end;

procedure TZMIRec.MarkDirty;
begin
  SetStatusBit(zsbDirty);
end;

// process the record (base type does nothing)
// returns bytes written, <0 _ error
function TZMIRec.Process: Int64;
begin
  Result := 0;  // default, nothing done
end;

// size of data to process - excludes central directory (virtual)
function TZMIRec.ProcessSize: Int64;
begin
  Result := 0;// default nothing to process
end;

function TZMIRec.SafeHeaderName(const IntName: TZMString): TZMString;
const
  BadChars : TSysCharSet = [#0..#31, ':', '<', '>', '|', '*', '?', #39, '\'];
var
  c: Char;
  i: integer;
Begin
  Result := '';
  for i := 1 to Length(IntName) do
  begin
    c := IntName[i];
    if (c <= #255) and (AnsiChar(c) in BadChars) then
    begin
      if c = '\' then
        Result := Result + PathDelimAlt
      else
        Result := Result + '#$' + IntToHex(Ord(c),2);
    end
    else
      Result := Result + c;
  end;
end;

// returns the new value
function TZMIRec.Select(How: TZMSelects): Boolean;
begin
  case How of
    zzsClear:
      Result := False;
    zzsSet:
      Result := True;
//    zzsToggle:
    else
      Result := not TestStatusBit(zsbSelected);
  end;
  SetSelected(Result);
end;

procedure TZMIRec.SetDateStamp(Value: TDateTime);
begin
  DateTimeToFileDate(Value);
end;

procedure TZMIRec.SetIsEncoded(const Value: TZMEncodingOpts);
var
  n: Integer;
begin
  n := Ord(Value) shl 16;
  ClearStatusBit(zsbEncMask); // clear all
  SetStatusBit(n);            // set new value
end;

procedure TZMIRec.SetSelected(const Value: Boolean);
begin
  if Selected <> Value then
  begin
    if Value then
      SetStatusBit(zsbSelected)
    else
    begin
      ClearStatusBit(zsbSelected);
      SelectArgs := '';
    end;
  end;
end;

// returns previous values
function TZMIRec.SetStatusBit(const Value: Cardinal): Cardinal;
begin
  Result := StatusBits and Value;
  StatusBits := StatusBits or Value;
end;

function TZMIRec.TestStatusBit(const mask: Cardinal): Boolean;
begin
  Result := (StatusBits and mask) <> 0;
end;

end.
