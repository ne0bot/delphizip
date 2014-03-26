unit ZMZStream;

// ZMZStream.pas - Misc. zip stream handling

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
// modified 2013-03-26

{$INCLUDE   '.\ZipVers.inc'}
{$Q-}  // range check off

{ .$define ZDEBUG_UNZ }

interface

uses
{$IFDEF VERDXE2up}
  System.Classes, WinApi.Windows,
{$ELSE}
  Windows, Classes,
{$ENDIF}
  ZipMstr, ZMZLibEx;

const
  UnzBufferSize = $8000;

type
  TZProgressFunc = function(Sender: TStream{TObject}; count: Cardinal)
    : boolean of object;

const
  RAND_HEAD_LEN = 12; (* Length of encryption random header. *)

type
  TZMCryptKeys = array [0 .. 2] of DWORD;
  PZMCryptKeys = ^TZMCryptKeys;
  TZMCryptHeader = packed array [0 .. RAND_HEAD_LEN - 1] of Byte;
  PZMCryptHeader = ^TZMCryptHeader;

type
  TZMCryptorBase = class
  private
    FCRC32Table: PZCRC32Table;
  protected
    function GetCryptBuf: PByte; virtual; abstract;
    function GetCryptBufSize: Integer; virtual; abstract;
    function GetState: Integer; virtual; abstract;
    procedure SetState(const Value: Integer); virtual; abstract;
    property CRC32Table: PZCRC32Table read FCRC32Table;
  public
    procedure AfterConstruction; override;
//  procedure BeforeDestruction; override;
    function DecodeBuffer(var OutBuffer; const InBuffer; count: longint): Integer;
        virtual; abstract;
    function EncodeBuffer(var OutBuffer; const InBuffer; count: longint): Integer;
      virtual; abstract;
    function ReadHeader(Source: TStream): Integer; virtual; abstract;
    function Unlock(const PassPhrase: AnsiString): Integer; virtual; abstract;
    function WriteHeader(Destination: TStream): Integer; virtual; abstract;
    property CryptBuf: PByte read GetCryptBuf;
    property CryptBufSize: Integer read GetCryptBufSize;
    property State: Integer read GetState write SetState;
  end;

type
  TZMZipCryptor = class(TZMCryptorBase)
  private
    FCryptBuf: PByte;
    FCryptHeader: TZMCryptHeader;
    FMyReference: Cardinal;
    FReady: Integer;
    Keys: TZMCryptKeys;
    function DecodeByte(c: Byte): Byte;
    function EncodeByte(c: Byte): Byte;
    procedure init_keys(passwd: AnsiString);
    function MakeHeader(const PassPhrase: AnsiString; RefValue: DWORD): Integer;
    procedure update_keys(c: Byte);
  protected
    function GetCryptBuf: PByte; override;
    function GetCryptBufSize: Integer; override;
    function GetState: Integer; override;
    procedure SetState(const Value: Integer); override;
    property Ready: Integer read FReady write FReady;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function DecodeBuffer(var OutBuffer; const InBuffer; count: longint): Integer;
        override;
    function DecodeInit(SrcStream: TStream; RefValue: Cardinal): Integer;
    function EncodeBuffer(var OutBuffer; const InBuffer; count: longint): Integer; override;
    function EncodeInit(const PassPhrase: AnsiString; RefValue: DWORD): Integer;
    function ReadHeader(Source: TStream): Integer; override;
    function Unlock(const PassPhrase: AnsiString): Integer; override;
    function WriteHeader(Destination: TStream): Integer; override;
  end;

type
  TZMCryptStreamBase = class(TStream)
  private
    FCryptor: TZMCryptorBase;
    FIOwnIt: boolean;
    FMyStream: TStream;
  public
    constructor Create(AStream: TStream; Cryptor: TZMCryptorBase;
      OwnsStream: boolean = False);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Cryptor: TZMCryptorBase read FCryptor;
    property MyStream: TStream read FMyStream;
  end;

// MyStream is source
type
  TZMDecryptStream = class(TZMCryptStreamBase)
  public
    function Read(var Buffer; count: longint): longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; count: longint): longint; override;
  end;

// MyStream is destination
type
  TZMEncryptStream = class(TZMCryptStreamBase)
  public
//  procedure AfterConstruction; override;
    function Read(var Buffer; count: longint): longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; count: longint): longint; override;
  end;

type
  TZMCRCStream = class(TStream)
  private
    FIOwnIt: boolean;
    FMyStream: TStream;
    FPCRC: PDWORD;
  public
    constructor Create(AStream: TStream; var CRC: DWORD;
      OwnsStream: boolean = False);
    procedure BeforeDestruction; override;
    function Read(var Buffer; count: longint): longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; count: longint): longint; override;
  end;

type
  TZMProgressStream = class(TStream)
  private
    FCounter: Cardinal;
    FInterval: Cardinal;
    FIOwnIt: boolean;
    FMyStream: TStream;
    FProc: TZProgressFunc;
    procedure DoCount(count: Integer);
    function DoProgress(Amount: Cardinal): Boolean;
  public
    constructor Create(AStream: TStream; Proc: TZProgressFunc = nil; MinInterval:
        Cardinal = 8096; OwnsStream: boolean = False);
    procedure BeforeDestruction; override;
    function Read(var Buffer; count: longint): longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; count: longint): longint; override;
  end;

type
  TZMProgressLimiter = class(TZMProgressStream)
  private
    FHaveRead: Int64;
    FLimit: Int64;
  public
    constructor Create(SrcStream: TStream; ReadLimit: Int64; Proc: TZProgressFunc =
        nil);
    function Read(var Buffer; count: longint): longint; override;
  end;

type
  TZMCompressionStream = class(TZCompressionStream)
  private
    FIOwnIt: boolean;
    FMyStream: TStream;
  public
    constructor Create(SourceStream: TStream; Level: TZCompressionLevel;
        OwnsStream: boolean = False);
    procedure BeforeDestruction; override;
  end;

type
  TZMDecompressionStream = class(TZDecompressionStream)
  private
    FIOwnIt: boolean;
    FMyStream: TStream;
  public
    constructor Create(DestStream: TStream; OwnsStream: boolean = False);
    procedure BeforeDestruction; override;
  end;

function UndeflateQ(OutStream, InStream: TStream; Length: Int64; Method: Word;
  var CRC: Cardinal): Integer;

function ExtractZStream(OutStream, InStream: TStream; Length: Int64): Integer;

function DeflateStream(OutStream, InStream: TStream; Length: Int64;
  Method: Word; var CRC: Cardinal): Integer;
function UndeflateStream(OutStream, InStream: TStream; Length: Int64;
  Method: Word; var CRC: Cardinal): Integer;

function CopyStream(OutStream, InStream: TStream; count: Int64): Integer;

implementation

uses
{$IFDEF VERDXE2up}
  System.SysUtils,
{$ELSE}
  SysUtils,
{$ENDIF}
  ZMMsg, ZMXcpt, ZMStructs, ZLibExApi, ZMBody;

const
  __UNIT__ = 13;

const
  CRYPT_BUFFER_SIZE = 8 * 1024;
  ZipMagic = 134775813;

var
  XSSeed: UInt64 = 0;
  XSInitialed: boolean = false;

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

//2463534242;
function XSRandom: Cardinal;
// Marsaglia, George (July 2003). "Xorshift RNGs".
// Journal of Statistical Software Vol. 8 (Issue  14).
var
  Temp: UInt64;
begin
  Temp := XSSeed xor (XSSeed shl 13);
  Temp := Temp xor (Temp shr 17);
  Temp := Temp xor (Temp shl 5);
  XSSeed := Temp;
  result := (Temp shr 1) and $FFFFFFFF;
end;

procedure InitXSSeed;
var
  Temp: FILETIME;
  SysTime: SYSTEMTIME;
begin
  GetSystemTime(SysTime);
  SystemTimeToFileTime(SysTime, Temp);
  XSSeed := (GetTickCount shr 1) xor UInt64(Temp);
  XSInitialed := True;
end;


// returns <0 _ error , 0 _ success
function CopyStream(OutStream, InStream: TStream; count: Int64): Integer;
const
  BufSizeMax = $F000;
var
  Buf: PByte;
  BufSize: Integer;
  ReadCnt: Integer;
  WriteCnt: Integer;
begin
  result := 0;
  BufSize := BufSizeMax;
  if count < BufSize then
    BufSize := count;
  GetMem(Buf, BufSize);
  try
    repeat
      ReadCnt := InStream.Read(Buf^, BufSize);
      if ReadCnt <= 0 then
        Break;

      WriteCnt := OutStream.Write(Buf^, ReadCnt);
      if WriteCnt <> ReadCnt then
      begin
        ReadCnt := WriteCnt; // error
        if ReadCnt >= 0 then
          ReadCnt := -ZM_Error(325, ZS_WriteError);
        Break;
      end;
      if ReadCnt < BufSize then
        break;   // finish on short read
      Dec(count, ReadCnt);
      if count < BufSize then
        BufSize := count;
    until count <= 0;
    if ReadCnt < 0 then
      result := Integer(ReadCnt); // return error
  finally
    FreeMem(Buf);
  end;
end;

function UndeflateStream(OutStream, InStream: TStream; Length: Int64;
  Method: Word; var CRC: Cardinal): Integer;
var
  DecompStream: TZDecompressionStream;
  CRCStream: TZMCRCStream;
  Stream: TStream;
begin
  if (Method <> METHOD_STORED) and (Method <> METHOD_DEFLATED) then
  begin
    result := -ZM_Error(350, ZS_Unsupported);
    Exit;
  end;
  Stream := InStream;
  DecompStream := nil;
  CRCStream := TZMCRCStream.Create(OutStream, CRC);
  try
    try
      if Method = METHOD_DEFLATED then
      begin
        DecompStream := TZMDecompressionStream.Create(InStream);
        Stream := DecompStream;
      end;
      result := CopyStream(CRCStream, Stream, Length);
    finally
      DecompStream.Free;
      CRCStream.Free;
    end;
  except
    on E: EZlibError do
    begin
      Result := -ZM_Error(371, ZLibErrs[-(E.ErrorCode + 1)]);
    end;
    on ews: EZipMaster do // All WriteSpan specific errors.
    begin
      Result := -ews.ExtErr;
    end;
    on E: Exception do
    begin
      // The remaining errors, should not occur.
      Result := -ZM_Error(380, ZS_ErrorUnknown);
    end;
  end;
end;

function DeflateStream(OutStream, InStream: TStream; Length: Int64;
  Method: Word; var CRC: Cardinal): Integer;
var
  CompStream: TZCompressionStream;
  CRCStream: TZMCRCStream;
  Stream: TStream;
begin
  if (Method <> METHOD_STORED) and (Method <> METHOD_DEFLATED) then
  begin
    result := -ZM_Error(394, ZS_Unsupported);
    Exit;
  end;
  Stream := OutStream;
  try
    CompStream := nil;
    CRCStream := TZMCRCStream.Create(InStream, CRC);
    try
      if Method = METHOD_DEFLATED then
      begin
        CompStream := TZMCompressionStream.Create(OutStream, zcLevel8);
        Stream := CompStream;
      end;
      result := CopyStream(Stream, CRCStream, Length);
    finally
      CompStream.Free;
      CRCStream.Free;
    end;
  except
    on E: EZlibError do
    begin
      Result := -ZM_Error(415, ZLibErrs[-(E.ErrorCode + 1)]);
    end;
    on ews: EZipMaster do // All WriteSpan specific errors.
    begin
      Result := -ews.ExtErr;
    end;
    on E: Exception do
    begin
      // The remaining errors, should not occur.
      Result := -ZM_Error(424, ZS_ErrorUnknown);
    end;
  end;
end;

function UndeflateQ(OutStream, InStream: TStream; Length: Int64; Method: Word;
  var CRC: Cardinal): Integer;
var
  ReqCRC: DWORD;
begin
  ReqCRC := CRC;
  result := UndeflateStream(OutStream, InStream, MAXINT, Method, CRC);
  if (result >= 0) and (ReqCRC <> CRC) then
    result := -ZM_Error(437, ZS_BadCRC)
  else
    result := 0;
end;

function ExtractZStream(OutStream, InStream: TStream; Length: Int64): Integer;
var
  Heder: TZM_StreamHeader;
begin
  if InStream.Read(Heder, sizeof(TZM_StreamHeader)) <> sizeof(TZM_StreamHeader)
  then
    result := -ZM_Error(448, ZS_ReadError)
  else
    result := UndeflateQ(OutStream, InStream, Length - sizeof(TZM_StreamHeader),
      Heder.Method, Heder.CRC);
end;

{ TZMCryptStreamBase }
constructor TZMCryptStreamBase.Create(AStream: TStream; Cryptor: TZMCryptorBase;
  OwnsStream: boolean = False);
begin
  inherited Create;
  FCryptor := Cryptor;
  FMyStream := AStream;
  FIOwnIt := OwnsStream;
end;

procedure TZMCryptStreamBase.AfterConstruction;
begin
  inherited;
end;

procedure TZMCryptStreamBase.BeforeDestruction;
begin
  if FIOwnIt then
    FMyStream.Free;
  FCryptor.Free;
  inherited;
end;

{ TZMDecryptStream }

function TZMDecryptStream.Read(var Buffer; count: longint): longint;
var
  CryptBuf: PByte;
  CryptCnt: Integer;
  CryptSize: Integer;
  OutBuf: PByte;
  ToRead: Integer;
  ReadCnt: Integer;
begin
  Result := 0;
  if Cryptor.State <= 0 then
    exit;
  CryptBuf := Cryptor.CryptBuf;
  CryptSize := Cryptor.CryptBufSize;
  OutBuf := PByte(@Buffer);
  while Count > 0 do
  begin
    ToRead := CryptSize;
    if Count < ToRead then
      ToRead := count;
    // read encrypted
    {Result}ReadCnt := FMyStream.Read(CryptBuf^, ToRead);
    if ReadCnt <= 0 then
    begin
      Result := ReadCnt;
      Break;
    end;
    // decode it into Buffer
    CryptCnt := Cryptor.DecodeBuffer(OutBuf^, CryptBuf^, ReadCnt);
    if CryptCnt <> ReadCnt then
    begin
      Result := CryptCnt; // error
      if Result >= 0 then
        Result := -ZM_Error(512, ZS_CryptError);
      Break;
    end;
    Inc(Result, CryptCnt);
    if CryptCnt < ToRead then
      Break; // finish on short read
    Inc(OutBuf, CryptCnt);
    Dec(count, CryptCnt);
  end;
end;

function TZMDecryptStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result := FMyStream.Seek(Offset, Origin);
end;

function TZMDecryptStream.Write(const Buffer; count: longint): longint;
begin
  Result := -ZM_Error(530, ZS_InternalError);
end;

constructor TZMCRCStream.Create(AStream: TStream; var CRC: DWORD;
  OwnsStream: boolean = False);
begin
  inherited Create;
  FMyStream := AStream;
  FPCRC := PDWORD(@CRC);
  CRC := 0;
  FIOwnIt := OwnsStream;
end;

procedure TZMCRCStream.BeforeDestruction;
begin
  if FIOwnIt then
    FMyStream.Free;
  inherited;
end;

function TZMCRCStream.Read(var Buffer; count: Integer): longint;
begin
  result := FMyStream.Read(Buffer, count);
  if result > 0 then
    FPCRC^ := ZLibExApi.CRC32(FPCRC^, Buffer, result);
end;

function TZMCRCStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // cannot seek - invalidates CRC
//  result := FMyStream.Seek(Offset, Origin);
  Result := -ZM_Error(561, ZS_InternalError);
end;

function TZMCRCStream.Write(const Buffer; count: Integer): longint;
begin
  result := FMyStream.Write(Buffer, count);
  if result > 0 then
    FPCRC^ := ZLibExApi.CRC32(FPCRC^, Buffer, result);
end;

{ TZMProgressStream }
constructor TZMProgressStream.Create(AStream: TStream; Proc: TZProgressFunc =
    nil; MinInterval: Cardinal = 8096; OwnsStream: boolean = False);
begin
  inherited Create;
  FMyStream := AStream;
  FProc := Proc;
  FInterval := MinInterval;
  FCounter := 0;
  FIOwnIt := OwnsStream;
end;

procedure TZMProgressStream.BeforeDestruction;
begin
  if FIOwnIt then
    FMyStream.Free;
  if FCounter > 0 then
    DoProgress(FCounter); // final count
  inherited;
end;

procedure TZMProgressStream.DoCount(count: Integer);
begin
  if count > 0 then
  begin
    FCounter := FCounter + Cardinal(count);
    if FCounter > FInterval then
    begin
      if DoProgress(FCounter) then
        raise EZMAbort.Create;
      FCounter := 0;
    end;
  end
  else
    if (FCounter > 0) then
    begin
      if DoProgress(FCounter) then
        raise EZMAbort.Create;
      FCounter := 0;
    end;
end;

function TZMProgressStream.DoProgress(Amount: Cardinal): Boolean;
begin
  if @FProc <> nil then
    result := FProc(FMyStream, Amount)
  else
    result := False;
end;

function TZMProgressStream.Read(var Buffer; count: Integer): longint;
begin
  result := FMyStream.Read(Buffer, count);
  DoCount(Result);
end;

function TZMProgressStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  // cannot seek - invalidates Counter
  if (Offset <> 0) and (Origin <> soCurrent) then
    result := -1
  else
    result := FMyStream.Seek(Offset, Origin);
end;

function TZMProgressStream.Write(const Buffer; count: Integer): longint;
begin
  result := FMyStream.Write(Buffer, count);
  DoCount(Result);
end;

constructor TZMCompressionStream.Create(SourceStream: TStream; Level:
    TZCompressionLevel; OwnsStream: boolean = False);
begin
  inherited Create(SourceStream, Level, -15, 8, zsDefault);
  FMyStream := SourceStream;
  FIOwnIt := OwnsStream;
end;

{ TZMCompressionStream }

procedure TZMCompressionStream.BeforeDestruction;
var
  DestStream: TStream;
begin
  DestStream := nil;
  if FIOwnIt then
  begin
    DestStream := FMyStream;
    FMyStream := nil;
//    FMyStream.Free;
  end;
  inherited;  // flush and cleanup myself
  if DestStream <> nil then
    DestStream.Free;
end;

constructor TZMDecompressionStream.Create(DestStream: TStream; OwnsStream:
    boolean = False);
begin
  inherited Create(DestStream, -15);
  FMyStream := DestStream;
  FIOwnIt := OwnsStream;
end;

{ TZMDecompressionStream }

procedure TZMDecompressionStream.BeforeDestruction;
var
  SourceStream: TStream;
begin
  SourceStream := nil;
  if FIOwnIt then
  begin
    SourceStream := FMyStream;
    FMyStream := nil;
  end;
  inherited;  // flush and cleanup myself before source
  if SourceStream <> nil then
    SourceStream.Free;
end;

{ TZMEncryptStream }
function TZMEncryptStream.Read(var Buffer; count: longint): longint;
begin
  Result := -ZM_Error(697, ZS_InternalError);
end;

function TZMEncryptStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  result := FMyStream.Seek(Offset, Origin);
end;

function TZMEncryptStream.Write(const Buffer; count: longint): longint;
var
  CryptBuf: PByte;
  CryptSize: Integer;
  CryptCnt: Integer;
  InBuf: PByte;
  WriteCnt: Integer;
  ToWrite: Integer;
begin
  Result := 0;
  if Cryptor.State <= 0 then
    exit;
  CryptBuf := Cryptor.CryptBuf;
  CryptSize := Cryptor.CryptBufSize;
  InBuf := PByte(@Buffer);
  while Count > 0 do
  begin
    ToWrite := CryptSize;
    if Count < ToWrite then
      ToWrite := count;
    CryptCnt := Cryptor.EncodeBuffer(CryptBuf^, InBuf^, ToWrite);
    if CryptCnt <= 0 then
    begin
      Result := CryptCnt; // error
      Break;
    end;
    WriteCnt := FMyStream.Write(CryptBuf^, CryptCnt);
    if CryptCnt <> WriteCnt then
    begin
      Result := WriteCnt;  // error
      if Result >= 0 then
        Result := -ZM_Error(736, ZS_WriteError);
      Break;
    end;
    Inc(Result, CryptCnt);
    if CryptCnt < ToWrite then
      break;    // finish
    Inc(InBuf, CryptCnt);
    Dec(count, CryptCnt);
  end;
end;

{ TZMCryptorBase }
procedure TZMCryptorBase.AfterConstruction;
begin
  inherited;
  FCRC32Table := PZCRC32Table(get_crc_table);
end;

// TODO: BeforeDestruction
//procedure TZMCryptorBase.BeforeDestruction;
//begin
////  FreeMem(FCryptBuf);
//inherited;
//end;

{ TZMZipCryptor }

procedure TZMZipCryptor.AfterConstruction;
begin
  inherited;
  GetMem(FCryptBuf, CryptBufSize);
  FReady := 0; // not verified
  if not XSInitialed then
   InitXSSeed;
end;

procedure TZMZipCryptor.BeforeDestruction;
begin
  FreeMem(FCryptBuf);
  inherited;
end;

function TZMZipCryptor.DecodeBuffer(var OutBuffer; const InBuffer; count:
    longint): Integer;
var
  b: Byte;
  i: Integer;
  InBuf: PByte;
  OutBuf: PByte;
  T: Byte;
  temp: Cardinal;
begin
  InBuf := PByte(@InBuffer);
  OutBuf := PByte(@OutBuffer);
  for i := 0 to count - 1 do
  begin
    // decode byte
    temp := (Keys[2] and $FFFF) or 2;
    T := (((temp * (temp xor 1)) shr 8) and $FF);
    b := Byte(InBuf^ xor T);
    OutBuf^ := b;
  // update keys
    Keys[0] := CRC32Table^[DWORD((Keys[0] xor b) and $0FF)] xor
      DWORD((Keys[0] shr 8) and $0FFFFFF);
    Keys[1] := Keys[1] + (Keys[0] and $FF);
    Keys[1] := (Keys[1] * ZipMagic) + 1;
    Keys[2] := CRC32Table^[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
    xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
    Inc(InBuf);
    Inc(OutBuf);
  end;
  result := Count;
end;

function TZMZipCryptor.DecodeByte(c: Byte): Byte;
var
  temp: Cardinal;
  T: Byte;
begin
  // decode byte
  temp := (Keys[2] and $FFFF) or 2;
  T := (((temp * (temp xor 1)) shr 8) and $0FF);
  result := Byte(c xor T);
  // update keys
  Keys[0] := CRC32Table^[DWORD((Keys[0] xor result) and $0FF)] xor
    DWORD((Keys[0] shr 8) and $0FFFFFF);
  Keys[1] := Keys[1] + (Keys[0] and $FF);
  Keys[1] := (Keys[1] * ZipMagic) + 1;
  Keys[2] := CRC32Table^[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
    xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
end;

function TZMZipCryptor.DecodeInit(SrcStream: TStream;
  RefValue: Cardinal): Integer;
begin
  FMyReference := RefValue;
  if ReadHeader(SrcStream) <> RAND_HEAD_LEN then
    FReady := -1; // bad

  result := Ready;
end;

function TZMZipCryptor.EncodeBuffer(var OutBuffer; const InBuffer; count:
    longint): Integer;
var
  b: Byte;
  i: Integer;
  InBuf: PByte;
  OutBuf: PByte;
  T: Byte;
  temp: Cardinal;
begin
  InBuf := PByte(@InBuffer);
  OutBuf := PByte(@OutBuffer);
  for i := 0 to count - 1 do
  begin
    b := InBuf^;
    temp := (Keys[2] and $FFFF) or 2;
    T := ((temp * (temp xor 1)) shr 8) and $FF;
    OutBuf^ := Byte(b xor T);
    // update keys
    Keys[0] := CRC32Table[DWORD((Keys[0] xor b) and $0FF)] xor
      DWORD((Keys[0] shr 8) and $0FFFFFF);
    Keys[1] := Keys[1] + (Keys[0] and $FF);
    Keys[1] := (Keys[1] * ZipMagic) + 1;
    Keys[2] := CRC32Table[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
      xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
    Inc(InBuf);
    Inc(OutBuf);
  end;
  result := Count;
end;

function TZMZipCryptor.EncodeByte(c: Byte): Byte;
var
  T: Byte;
  temp: DWORD;
begin
  // decrypt byte
  temp := (Keys[2] and $FFFF) or 2;
  T := (((temp * (temp xor 1)) shr 8) and $FF);
  result := Byte(c xor T);
  // update keys
  Keys[0] := CRC32Table[DWORD((Keys[0] xor c) and $0FF)] xor
    DWORD((Keys[0] shr 8) and $0FFFFFF);
  Keys[1] := Keys[1] + (Keys[0] and $FF);
  Keys[1] := (Keys[1] * ZipMagic) + 1;
  Keys[2] := CRC32Table[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
    xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
end;

function TZMZipCryptor.EncodeInit(const PassPhrase: AnsiString; RefValue:
    DWORD): Integer;
begin
  Result := MakeHeader(PassPhrase, RefValue);
  if Result < 0 then
    FReady := -1
  else
    FReady := 2;
end;

function TZMZipCryptor.GetCryptBuf: PByte;
begin
  Result := FCryptBuf;
end;

function TZMZipCryptor.GetCryptBufSize: Integer;
begin
  Result := CRYPT_BUFFER_SIZE;
end;

function TZMZipCryptor.GetState: Integer;
begin
  result := FReady;
end;

procedure TZMZipCryptor.init_keys(passwd: AnsiString);
var
  i: Integer;
begin
  Keys[0] := 305419896;
  Keys[1] := 591751049;
  Keys[2] := 878082192;
  for i := 1 to Length(passwd) do
    update_keys(Ord(passwd[i]));
end;

function TZMZipCryptor.MakeHeader(const PassPhrase: AnsiString;
  RefValue: DWORD): Integer;
var
  Header: array [0 .. RAND_HEAD_LEN - 3] of Byte;
  i: Integer;
begin
  // First generate RAND_HEAD_LEN - 2 random bytes.
  // We encrypt the output of rand() to get less predictability,
  // since rand() is often poorly implemented.
  init_keys(PassPhrase);
  for i := 0 to RAND_HEAD_LEN - 3 do
  begin
    Header[i] := EncodeByte(XSRandom and $0FF);
  end;
  init_keys(PassPhrase);
  // Encrypt random header (last two bytes is high word of crc)
  for i := 0 to RAND_HEAD_LEN - 3 do
    FCryptHeader[i] := EncodeByte(Header[i]);
  FCryptHeader[RAND_HEAD_LEN - 2] := EncodeByte((RefValue shr 16) and $FF);
  FCryptHeader[RAND_HEAD_LEN - 1] := EncodeByte((RefValue shr 24) and $FF);
  result := RAND_HEAD_LEN;
end;

function TZMZipCryptor.ReadHeader(Source: TStream): Integer;
begin
  result := Source.Read(FCryptHeader, RAND_HEAD_LEN);
end;

procedure TZMZipCryptor.SetState(const Value: Integer);
begin
  // read-only
end;

function TZMZipCryptor.Unlock(const PassPhrase: AnsiString): Integer;
var
  hh: array [0 .. RAND_HEAD_LEN - 1] of Byte; // decrypted header copy
  b: Word;
  n: Integer;
begin
  result := FReady;
  if (result = 0) and (PassPhrase <> '') then
  begin
    // still locked
    (* set DecryptKeys and save the encrypted header *)
    init_keys(PassPhrase);
    Move(FCryptHeader, hh, RAND_HEAD_LEN);

    (* check password *)
    for n := 0 to RAND_HEAD_LEN - 1 do
      hh[n] := Byte(DecodeByte(hh[n]));
    b := hh[RAND_HEAD_LEN - 1];

    if b = (FMyReference and $0FF) then
    begin
      FReady := 1; // unlocked
      result := 1;
    end;
  end;
end;

procedure TZMZipCryptor.update_keys(c: Byte);
begin
  Keys[0] := CRC32Table[(c xor Keys[0]) and $0FF] xor (Keys[0] shr 8);
  Keys[1] := Keys[1] + (Keys[0] and $0FF);
  Keys[1] := (Keys[1] * ZipMagic) + 1;
  Keys[2] := CRC32Table[DWORD(Keys[2] xor Keys[1] shr 24) and $0FF]
    xor (DWORD(Keys[2] shr 8) and $0FFFFFF);
end;

function TZMZipCryptor.WriteHeader(Destination: TStream): Integer;
begin
  Result := 0;
  if FReady = 2 then
  begin
    result := Destination.Write(FCryptHeader, RAND_HEAD_LEN);
    if Result <> RAND_HEAD_LEN then
      FReady := -1
    else
      FReady := 1;
  end
  else
    FReady := -1;
  if (Result >= 0) and (Result <> RAND_HEAD_LEN) then
    Result := -ZM_Error(1006, ZS_CryptError);
end;

constructor TZMProgressLimiter.Create(SrcStream: TStream; ReadLimit: Int64;
    Proc: TZProgressFunc = nil);
begin
  inherited Create(SrcStream, Proc, 8096, False);
  FLimit := ReadLimit;
end;

function TZMProgressLimiter.Read(var Buffer; count: longint): longint;
var
  CanRead: longint;
begin
  if (FHaveRead + count) > FLimit then
    CanRead := FLimit - FHaveRead
  else
    CanRead := count;
  Result := inherited Read(Buffer, CanRead);
  if Result > 0 then
    FHaveRead := FHaveRead + Result;
end;

end.
