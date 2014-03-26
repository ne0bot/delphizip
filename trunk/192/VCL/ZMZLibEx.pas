unit ZMZLibEx;
{ *************************************************************************************************
  *  ZLibEx.pas                                                                                    *
  *                                                                                                *
  *  copyright (c) 2000-2012 base2 technologies                                                    *
  *  copyright (c) 1995-2002 Borland Software Corporation                                          *
  ************************************************************************************************* }
(* Adapted for ZipMaster 1.9.2 Russell Peters *)
// modified 2013-04-15

interface

uses
  Classes, SysUtils, Windows, ZLibExApi, ZMMsg;
                          
{$I ZLibEx.inc}

type
{$IFNDEF UNICODE}
  RawByteString = AnsiString;

  UnicodeString = WideString;
  UnicodeChar = WideChar;

{$ELSE ifdef Version2010Plus}
  UnicodeChar = WideChar;

{$ENDIF}
{$IFNDEF Version2009Plus}
  NativeInt = Integer;
  NativeUInt = Cardinal;

{$ENDIF}
  TStreamPos = {$IFDEF Version6Plus} Int64 {$ELSE} Longint {$ENDIF};

  TZCompressionLevel = (zcNone, zcFastest, zcDefault, zcMax, zcLevel1, zcLevel2,
    zcLevel3, zcLevel4, zcLevel5, zcLevel6, zcLevel7, zcLevel8, zcLevel9);

  TZStrategy = (zsDefault, zsFiltered, zsHuffman, zsRLE, zsFixed);

  TZError = (zeError, zeStreamError, zeDataError, zeMemoryError, zeBufferError,
    zeVersionError);

  TZFlush = (zfNoFlush, zfPartialFlush, zfSyncFlush, zfFullFlush, zfFinish,
    zfBlock, zfTrees);

const
  ZLevels: array [TZCompressionLevel] of Integer = (Z_NO_COMPRESSION, // zcNone
    Z_BEST_SPEED, // zcFastest
    Z_DEFAULT_COMPRESSION, // zcDefault
    Z_BEST_COMPRESSION,    // zcMax
    1,                     // zcLevel1
    2,                     // zcLevel2
    3,                     // zcLevel3
    4,                     // zcLevel4
    5,                     // zcLevel5
    6,                     // zcLevel6
    7,                     // zcLevel7
    8,                     // zcLevel8
    9                      // zcLevel9
    );

  ZStrategies: array [TZStrategy] of Integer = (Z_DEFAULT_STRATEGY, // zsDefault
    Z_FILTERED,     // zsFiltered
    Z_HUFFMAN_ONLY, // zsHuffman
    Z_RLE,          // zsRLE
    Z_FIXED         // zsFixed
    );

  ZErrors: array [TZError] of Integer = (Z_ERRNO, // zeError
    Z_STREAM_ERROR,                               // zeStreamError
    Z_DATA_ERROR,                                 // zeDataError
    Z_MEM_ERROR,                                  // zeMemoryError
    Z_BUF_ERROR,                                  // zeBufferError
    Z_VERSION_ERROR                               // zeVersionError
    );

  ZFlushes: array [TZFlush] of Integer = (Z_NO_FLUSH, // zfNoFlush
    Z_PARTIAL_FLUSH,                                  // zfPartialFlush
    Z_SYNC_FLUSH,                                     // zfSyncFlush
    Z_FULL_FLUSH,                                     // zfFullFlush
    Z_FINISH,                                         // zfFinish
    Z_BLOCK,                                          // zfBlock
    Z_TREES                                           // zfTrees
    );

type
  { ** TZ*Function ******************************************************************************* }

  TZReadFunction = function(param: Pointer; var buffer; size: Integer): Integer;

  TZWriteFunction = function(param: Pointer; const buffer;
    size: Integer): Integer;

  { ** TZInformation ***************************************************************************** }

  TZInformation = packed record
    CompressedFlags: Longint;
    CompressedSize: TStreamPos;
    CompressedCrc: Longint;
    CompressedAdler: Longint;

    UncompressedFlags: Longint;
    UncompressedSize: TStreamPos;
    UncompressedCrc: Longint;
    UncompressedAdler: Longint;
  end;

  { ** TCustomZStream **************************************************************************** }
  TCustomZStream = class(TStream)
  private
    FStream: TStream;
    FStreamPos: TStreamPos;
    FOnProgress: TNotifyEvent;

    FZStream: TZStreamRec;
    FBuffer: array [Word] of Byte;

    function GetStreamPosition: TStreamPos;
    procedure SetStreamPosition(value: TStreamPos);
  protected
    constructor Create(stream: TStream);

    function StreamRead(var buffer; count: Longint): Longint;
    function StreamWrite(const buffer; count: Longint): Longint;
    function StreamSeek(offset: Longint; origin: Word): Longint;

    procedure StreamReadBuffer(var buffer; count: Longint);
    procedure StreamWriteBuffer(const buffer; count: Longint);

    procedure DoProgress; dynamic;

    property StreamPosition: TStreamPos read GetStreamPosition
      write SetStreamPosition;

    property OnProgress: TNotifyEvent read FOnProgress write FOnProgress;
  end;

  { ** TZCompressionStream *********************************************************************** }

  TZCompressionStream = class(TCustomZStream)
  private
    function GetCompressionRate: Single;
  public
    constructor Create(dest: TStream;
      compressionLevel: TZCompressionLevel = zcDefault); overload;

    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel;
      windowBits, memLevel: Integer; strategy: TZStrategy); overload;

// TODO: Destroy
//  destructor Destroy; override;
    procedure BeforeDestruction; override;

    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;

    property CompressionRate: Single read GetCompressionRate;
    property OnProgress;
  end;

  { ** TZDecompressionStream ********************************************************************* }

  TZDecompressionStream = class(TCustomZStream)
  public
    constructor Create(source: TStream); overload;
    constructor Create(source: TStream; windowBits: Integer); overload;

// TODO: Destroy
//  destructor Destroy; override;
    procedure BeforeDestruction; override;

    function Read(var buffer; count: Longint): Longint; override;
    function Write(const buffer; count: Longint): Longint; override;
    function Seek(offset: Longint; origin: Word): Longint; override;

    property OnProgress;
  end;
  // {************************************************************************************************}

type
  EZLibErrorClass = class of EZlibError;

  EZlibError = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(code: Integer; const dummy: string = ''); overload;
    constructor Create(error: TZError; const dummy: string = ''); overload;

    property ErrorCode: Integer read FErrorCode write FErrorCode;
  end;

  EZCompressionError = class(EZlibError);
  EZDecompressionError = class(EZlibError);


type
  TZCRC32Table = packed array [0..255] of Cardinal;
  PZCRC32Table = ^TZCRC32Table;

const
  ZLibErrs: array [0..5] of integer = (ZS_ReadError, ZS_ReadError,
    ZS_ZipDataError, ZS_UnknownError, ZS_UnknownError, ZS_Unsupported);

implementation

uses
  ZMXcpt;

const
  SZInvalid = 'Invalid ZStream operation!';

const
  __UNIT__ = 32;

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

//function GetCRCTable: PZCRC32Table;
//begin
//  Result := PZCRC32Table(get_crc_table);
//end;

  { ************************************************************************************************ }
function ZCompressCheck(code: Integer): Integer;
begin
  result := code;

  if code < 0 then
  begin
    raise EZCompressionError.Create(code);
  end;
end;

function ZDecompressCheck(code: Integer;
  raiseBufferError: Boolean = True): Integer;
begin
  result := code;

  if code < 0 then
  begin
    if (code <> Z_BUF_ERROR) or raiseBufferError then
    begin
      raise EZDecompressionError.Create(code);
    end;
  end;
end;

{ ** zlib deflate routines *********************************************************************** }

function ZDeflateInit(var stream: TZStreamRec;
  level: TZCompressionLevel): Integer;
begin
  result := deflateInit_(stream, ZLevels[level], ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;

function ZDeflateInit2(var stream: TZStreamRec; level: TZCompressionLevel;
  windowBits, memLevel: Integer; strategy: TZStrategy): Integer;
begin
  result := deflateInit2_(stream, ZLevels[level], Z_DEFLATED, windowBits,
    memLevel, ZStrategies[strategy], ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function ZDeflate(var stream: TZStreamRec; flush: TZFlush): Integer;
begin
  result := deflate(stream, ZFlushes[flush]);
end;

function ZDeflateEnd(var stream: TZStreamRec): Integer;
begin
  result := deflateEnd(stream);
end;

function ZDeflateReset(var stream: TZStreamRec): Integer;
begin
  result := deflateReset(stream);
end;

{ ** zlib inflate routines *********************************************************************** }

function ZInflateInit(var stream: TZStreamRec): Integer;
begin
  result := inflateInit_(stream, ZLIB_VERSION, SizeOf(TZStreamRec));
end;

function ZInflateInit2(var stream: TZStreamRec; windowBits: Integer): Integer;
begin
  result := inflateInit2_(stream, windowBits, ZLIB_VERSION,
    SizeOf(TZStreamRec));
end;

function ZInflate(var stream: TZStreamRec; flush: TZFlush): Integer;
begin
  result := inflate(stream, ZFlushes[flush]);
end;

function ZInflateEnd(var stream: TZStreamRec): Integer;
begin
  result := inflateEnd(stream);
end;

function ZInflateReset(var stream: TZStreamRec): Integer;
begin
  result := inflateReset(stream);
end;

{ ** TCustomZStream ****************************************************************************** }

constructor TCustomZStream.Create(stream: TStream);
begin
  inherited Create;

  FStream := stream;
  FStreamPos := stream.Position;
end;

function TCustomZStream.StreamRead(var buffer; count: Longint): Longint;
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  result := FStream.Read(buffer, count);

  FStreamPos := FStreamPos + result;
end;

function TCustomZStream.StreamWrite(const buffer; count: Longint): Longint;
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  result := FStream.Write(buffer, count);

  FStreamPos := FStreamPos + result;
end;

function TCustomZStream.StreamSeek(offset: Longint; origin: Word): Longint;
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  result := FStream.Seek(offset, origin);

  FStreamPos := FStream.Position;
end;

procedure TCustomZStream.StreamReadBuffer(var buffer; count: Longint);
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  FStream.ReadBuffer(buffer, count);

  FStreamPos := FStreamPos + count;
end;

procedure TCustomZStream.StreamWriteBuffer(const buffer; count: Longint);
begin
  if FStream.Position <> FStreamPos then
    FStream.Position := FStreamPos;

  FStream.WriteBuffer(buffer, count);

  FStreamPos := FStreamPos + count;
end;

procedure TCustomZStream.DoProgress;
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self);
end;

function TCustomZStream.GetStreamPosition: TStreamPos;
begin
  result := FStream.Position;
end;

procedure TCustomZStream.SetStreamPosition(value: TStreamPos);
begin
  FStream.Position := value;
  FStreamPos := FStream.Position;
end;

{ ** TZCompressionStream ************************************************************************* }

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel);
begin
  inherited Create(dest);

  FZStream.next_out := PByte(@FBuffer);
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(ZDeflateInit(FZStream, compressionLevel));
end;

constructor TZCompressionStream.Create(dest: TStream;
  compressionLevel: TZCompressionLevel; windowBits, memLevel: Integer;
  strategy: TZStrategy);
begin
  inherited Create(dest);

  FZStream.next_out := PByte(@FBuffer);
  FZStream.avail_out := SizeOf(FBuffer);

  ZCompressCheck(ZDeflateInit2(FZStream, compressionLevel, windowBits, memLevel,
    strategy));
end;

// TODO: Destroy
//destructor TZCompressionStream.Destroy;
//begin
//FZStream.next_in := nil;
//FZStream.avail_in := 0;
//
//try
//  while ZCompressCheck(ZDeflate(FZStream, zfFinish)) <> Z_STREAM_END do
//  begin
//    StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
//
//    FZStream.next_out := PByte(@FBuffer);
//    FZStream.avail_out := SizeOf(FBuffer);
//  end;
//
//  if FZStream.avail_out < SizeOf(FBuffer) then
//  begin
//    StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
//  end;
//finally
//  ZDeflateEnd(FZStream);
//end;
//
//inherited Destroy;
//end;

procedure TZCompressionStream.BeforeDestruction;
begin
  FZStream.next_in := nil;
  FZStream.avail_in := 0;

  try
    while ZCompressCheck(ZDeflate(FZStream, zfFinish)) <> Z_STREAM_END do
    begin
      StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);

      FZStream.next_out := PByte(@FBuffer);
      FZStream.avail_out := SizeOf(FBuffer);
    end;

    if FZStream.avail_out < SizeOf(FBuffer) then
    begin
      StreamWriteBuffer(FBuffer, SizeOf(FBuffer) - FZStream.avail_out);
    end;
  finally
    ZDeflateEnd(FZStream);
  end;

  inherited;
end;

function TZCompressionStream.Read(var buffer; count: Longint): Longint;
begin
  raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.Write(const buffer; count: Longint): Longint;
var
  writeCount: Longint;
begin
  result := count;

  FZStream.next_in := @buffer;
  FZStream.avail_in := count;

  while FZStream.avail_in > 0 do
  begin
    ZCompressCheck(ZDeflate(FZStream, zfNoFlush));

    if FZStream.avail_out = 0 then
    begin
      writeCount := StreamWrite(FBuffer, SizeOf(FBuffer));

      if writeCount = SizeOf(FBuffer) then
      begin
        FZStream.next_out := PByte(@FBuffer);
        FZStream.avail_out := SizeOf(FBuffer);

        DoProgress;
      end
      else
      begin
        StreamPosition := StreamPosition - writeCount;

        result := Cardinal(count) - FZStream.avail_in;

        FZStream.avail_in := 0;
      end;
    end;
  end;
end;

function TZCompressionStream.Seek(offset: Longint; origin: Word): Longint;
begin
  if (offset = 0) and (origin = soFromCurrent) then
  begin
    result := FZStream.total_in;
  end
  else
    raise EZCompressionError.Create(SZInvalid);
end;

function TZCompressionStream.GetCompressionRate: Single;
begin
  if FZStream.total_in = 0 then
    result := 0
  else
    result := (1.0 - (FZStream.total_out / FZStream.total_in)) * 100.0;
end;

{ ** TZDecompressionStream *********************************************************************** }

constructor TZDecompressionStream.Create(source: TStream);
begin
  inherited Create(source);

  FZStream.next_in := PByte(@FBuffer);
  FZStream.avail_in := 0;

  ZDecompressCheck(ZInflateInit(FZStream));
end;

constructor TZDecompressionStream.Create(source: TStream; windowBits: Integer);
begin
  inherited Create(source);

  FZStream.next_in := PByte(@FBuffer);
  FZStream.avail_in := 0;

  ZDecompressCheck(ZInflateInit2(FZStream, windowBits));
end;

// TODO: Destroy
//destructor TZDecompressionStream.Destroy;
//begin
//ZInflateEnd(FZStream);
//
//inherited Destroy;
//end;

procedure TZDecompressionStream.BeforeDestruction;
begin
  ZInflateEnd(FZStream);
  inherited;
end;

function TZDecompressionStream.Read(var buffer; count: Longint): Longint;
var
  zresult: Integer;
begin
  FZStream.next_out := PByte(@buffer);
  FZStream.avail_out := count;

  zresult := Z_OK;
  Result := 0;

  while (FZStream.avail_out > 0) and (zresult <> Z_STREAM_END) do
  begin
    if FZStream.avail_in = 0 then
    begin
      FZStream.avail_in := StreamRead(FBuffer, SizeOf(FBuffer));

      if FZStream.avail_in = 0 then
      begin
        Result := Cardinal(count) - FZStream.avail_out;

        Exit;
      end;

      FZStream.next_in := PByte(@FBuffer);

      DoProgress;
    end;

    zresult := ZDecompressCheck(ZInflate(FZStream, zfNoFlush));
  end;

  if (zresult = Z_STREAM_END) and (FZStream.avail_in > 0) then
  begin
    StreamPosition := StreamPosition - TStreamPos(FZStream.avail_in);

    FZStream.avail_in := 0;
  end;
  if Result = 0 then
    Result := Cardinal(count) - FZStream.avail_out;
end;

function TZDecompressionStream.Write(const buffer; count: Longint): Longint;
begin
  raise EZDecompressionError.Create(SZInvalid);
end;

function TZDecompressionStream.Seek(offset: Longint; origin: Word): Longint;
var
  buf: array [0 .. 8191] of Byte;
  i: Integer;
begin
  if (offset = 0) and (origin = soFromBeginning) then
  begin
    ZDecompressCheck(ZInflateReset(FZStream));

    FZStream.next_in := PByte(@FBuffer);
    FZStream.avail_in := 0;

    StreamPosition := 0;
  end
  else
    if ((offset >= 0) and (origin = soFromCurrent)) or
      (((Cardinal(offset) - FZStream.total_out) > 0) and
      (origin = soFromBeginning)) then
    begin
      if origin = soFromBeginning then
        Dec(offset, FZStream.total_out);

      if offset > 0 then
      begin
        for i := 1 to offset div SizeOf(buf) do
          ReadBuffer(buf, SizeOf(buf));
        ReadBuffer(buf, offset mod SizeOf(buf));
      end;
    end
    else
      if (offset = 0) and (origin = soFromEnd) then
      begin
        while read(buf, SizeOf(buf)) > 0 do;
      end
      else
        raise EZDecompressionError.Create(SZInvalid);

  result := FZStream.total_out;
end;

{ ** EZLibError ********************************************************************************** }

constructor EZlibError.Create(code: Integer; const dummy: string);
begin
  inherited Create(z_errmsg[2 - code]);

  FErrorCode := code;
end;

constructor EZlibError.Create(error: TZError; const dummy: string);
begin
  Create(ZErrors[error], dummy);
end;

end.
