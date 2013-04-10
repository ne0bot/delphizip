unit ZMWorkFile;

//  ZMWorkFile.pas - basic in/out for zip files
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

{$I  '.\ZipVers.inc'}
{$IFDEF VER180}
 {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

  (*
  if Len < 0 then must process on this segment
  ????Full - gives error if not processed non-split
  ????Check - gives error if not all done
  // Len = int64
  function Seek(offset: Int64; From: integer): Int64; virtual;
  procedure CopyTo(var dest: TZMWorkZip; Len: Int64; ErrId: Integer); virtual;
  // only operate on < 2G at a time
  procedure CopyToFull(var dest: TZMWorkZip; Len, ErrId: Integer); virtual;
  function Read(var Buffer; ReadLen: Integer): Integer; virtual;
  procedure ReadCheck(var Buffer; Len, ErrId: Integer); virtual;
  procedure ReadFull(var Buffer; ReadLen, DSErrIdent: Integer); virtual;
  function Write(const Buffer; Len: Integer): Integer; virtual;
  function WriteCheck(const Buffer; Len, ErrId: Integer): Integer; virtual;
  procedure WriteFull(const Buffer; Len, ErrIdent: Integer); virtual;
*)
interface

uses
  Classes, Windows, SysUtils, ZipMstr, ZMDelZip, ZMCore;

// file signitures read by OpenEOC
type
  TZipFileSigs = (zfsNone, zfsLocal, zfsMulti, zfsDOS);

type
  TZipWrites = (zwDefault, zwSingle, zwMultiple);

const
  ProgressActions: array [TZipShowProgress] of TActionCodes =
    (zacTick, zacProgress, zacXProgress);
  MustFitError = -10999;
  MustFitFlag = $20000; // much bigger than any 'fixed' field
  MustFitMask = $1FFFF; // removes flag limits 'fixed' length

type
  TByteBuffer = array of Byte;

type
  TZMWorkFile = class(TZMCore)
  private
    FAlias: String;
    fBytesRead: Int64;
    fBytesWritten: Int64;
    fFileName: String;
    FHandle: Integer;
    fIsOpen: Boolean;
    fIsTemp: Boolean;
    FOpenMode: Cardinal;
    fShowProgress: TZipShowProgress;
    fStampDate: Cardinal;
    function GetExists: Boolean;
//    function GetFile_Size: Int64;
//    procedure SetFile_Size(const Value: Int64);
    procedure SetHandle(const Value: Integer);
  protected
    function Copy_File(Source: TZMWorkFile): Integer;
    function DoRead(var Buffer; Len: Integer): Integer;
    function DoWrite(const Buffer; Len: Integer): Integer;
    function GetPosition: Int64; virtual;
    procedure SetFileName(const Value: String); virtual;
    procedure SetPosition(const Value: Int64); virtual;
  public
    procedure AfterConstruction; override;
    procedure AssignFrom(Src: TZMWorkFile); virtual;
    procedure BeforeDestruction; override;
    function CheckRead(var Buffer; Len: Integer): Boolean; overload;
    procedure CheckRead(var Buffer; Len, ErrId: Integer); overload;
    function CheckReads(var Buffer; const Lens: array of Integer): Boolean;
      overload;
    procedure CheckReads(var Buffer; const Lens: array of Integer;
      ErrId: Integer); overload;
    function CheckSeek(offset: Int64; from, ErrId: Integer): Int64;
    function CheckWrite(const Buffer; Len: Integer): Boolean; overload;
    procedure CheckWrite(const Buffer; Len, ErrId: Integer); overload;
    function CheckWrites(const Buffer; const Lens: array of Integer): Boolean;
      overload;
    procedure CheckWrites(const Buffer; const Lens: array of Integer;
      ErrId: Integer); overload;
    function CopyFrom(Source: TZMWorkFile; Len: Int64): Int64;
    function FileDate: Cardinal;
    procedure File_Close; virtual;
    function File_Create(const theName: String): Boolean; virtual;
    function File_CreateTemp(const Prefix, Where: String): Boolean;
    function File_Open(Mode: Cardinal): Boolean; virtual;
    function File_Rename(const NewName: string; const Safe: Boolean = false)
      : Boolean;
    function File_Reopen(Mode: Cardinal): Integer; virtual;
    function GetFileInformation(var FileInfo: _BY_HANDLE_FILE_INFORMATION): Boolean;
    function IsEndOfFile: Boolean;
    function LastWriteTime(var last_write: TFileTime): Boolean;
    function Name(Expanded: Boolean = False): string; virtual;
    procedure ProgReport(prog: TActionCodes; xprog: Integer; const Name: String;
        size: Int64);
    function Read(var Buffer; Len: Integer): Integer; virtual;
    function Reads(var Buffer; const Lens: array of Integer): Integer; virtual;
    function ReadTo(strm: TStream; Count: Integer): Integer;
    function Seek(offset: Int64; from: Integer): Int64;
    function SetEndOfFile: Boolean;
    function Write(const Buffer; Len: Integer): Integer; virtual;
    function WriteFrom(strm: TStream; Count: Integer): Int64;
    function Writes(const Buffer; const Lens: array of Integer): Integer; virtual;
    property Alias: String read FAlias write FAlias;
    property BytesRead: Int64 read fBytesRead write fBytesRead;
    property BytesWritten: Int64 read fBytesWritten write fBytesWritten;
    property Exists: Boolean read GetExists;
    property FileName: String read fFileName write SetFileName;
//    property File_Size: Int64 read GetFile_Size write SetFile_Size;
    property Handle: Integer read FHandle write SetHandle;
    property IsOpen: Boolean read fIsOpen;
    property IsTemp: Boolean read fIsTemp write fIsTemp;
    property OpenMode: Cardinal read FOpenMode;
    property Position: Int64 read GetPosition write SetPosition;
    property ShowProgress
      : TZipShowProgress read fShowProgress write fShowProgress;
    // if non-zero set fileDate
    property StampDate: Cardinal read fStampDate write fStampDate;
  end;

implementation

uses
  ZMMsgStr, ZMUtils, ZMMsg, ZMXcpt, ZMWFuncs;

const
  __UNIT__ = 30 shl 23;

procedure TZMWorkFile.AfterConstruction;
begin
  inherited;
  fHandle := -1;
  fBytesWritten := 0;
  fBytesRead := 0;
  fOpenMode := 0;
end;

// Src should not be open but not enforced
procedure TZMWorkFile.AssignFrom(Src: TZMWorkFile);
begin
  if (Src <> Self) and (Src <> nil) then
  begin
    fBytesRead := Src.fBytesRead;
    fBytesWritten := Src.fBytesWritten;
    fFileName := Src.fFileName;
    fHandle := -1;  // don't acquire handle
    fIsOpen := False;
    fIsTemp := Src.fIsTemp;
    fOpenMode := Src.fOpenMode;
    fShowProgress := Src.fShowProgress;
    fStampDate := Src.fStampDate;
    FAlias := Src.FAlias;
  end;
end;

procedure TZMWorkFile.BeforeDestruction;
begin
  File_Close;
  if IsTemp and _Z_FileExists(fFileName) then
  begin
    if Verbosity >= zvTrace then
      Diag('Trace: Deleting ' + Name(True));
    _Z_DeleteFile(fFileName);
  end;
  inherited;
end;

function TZMWorkFile.CheckRead(var Buffer; Len: Integer): Boolean;
begin
  if Len < 0 then
    Len := -Len;
  Result := Read(Buffer, Len) = Len;
end;

procedure TZMWorkFile.CheckRead(var Buffer; Len, ErrId: Integer);
const
  __ERR_DS_ReadError = __UNIT__ + (233 shl 10) + DS_ReadError;
begin
  if Len < 0 then
    Len := -Len;
  if not CheckRead(Buffer, Len) then
  begin
    if ErrId = 0 then
      ErrId := __ERR_DS_ReadError;
    raise EZipMaster.CreateMsgDisp(ErrId, True);
  end;
end;

function TZMWorkFile.CheckReads(var Buffer; const Lens: array of Integer)
  : Boolean;
var
  c: Integer;
  i: Integer;
begin
  c := 0;
  for i := Low(Lens) to High(Lens) do
    c := c + Lens[i];
  Result := Reads(Buffer, Lens) = c;
end;

procedure TZMWorkFile.CheckReads(var Buffer; const Lens: array of Integer;
  ErrId: Integer);
const
  __ERR_DS_ReadError = __UNIT__ + (258 shl 10) + DS_ReadError;
begin
  if not CheckReads(Buffer, Lens) then
  begin
    if ErrId = 0 then
      ErrId := __ERR_DS_ReadError;
    raise EZipMaster.CreateMsgDisp(ErrId, True);
  end;
end;

function TZMWorkFile.CheckSeek(offset: Int64; from, ErrId: Integer): Int64;
const
  __ERR_DS_FailedSeek = __UNIT__ + (273 shl 10) + DS_FailedSeek;
begin
  Result := Seek(offset, from);
  if Result < 0 then
  begin
    if ErrId = 0 then
      raise EZipMaster.CreateMsgDisp(DS_SeekError, True);
    if ErrId = -1 then
      ErrId := __ERR_DS_FailedSeek;
    raise EZipMaster.CreateMsgDisp(ErrId, True);
  end;
end;

function TZMWorkFile.CheckWrite(const Buffer; Len: Integer): Boolean;
begin
  if Len < 0 then
    Len := -Len;
  Result := Write(Buffer, Len) = Len;
end;

procedure TZMWorkFile.CheckWrite(const Buffer; Len, ErrId: Integer);
const
  __ERR_DS_WriteError = __UNIT__ + (292 shl 10) + DS_WriteError;
begin
  if not CheckWrite(Buffer, Len) then
  begin
    if ErrId = 0 then
      ErrId := __ERR_DS_WriteError;
    raise EZipMaster.CreateMsgDisp(ErrId, True);
  end;
end;

function TZMWorkFile.CheckWrites(const Buffer; const Lens: array of Integer)
  : Boolean;
var
  c: Integer;
  i: Integer;
begin
  c := 0;
  for i := Low(Lens) to High(Lens) do
    c := c + Lens[i];
  Result := Writes(Buffer, Lens) = c;
end;

// must read from current part
procedure TZMWorkFile.CheckWrites(const Buffer; const Lens: array of Integer;
  ErrId: Integer);
const
  __ERR_DS_WriteError = __UNIT__ + (318 shl 10) + DS_WriteError;
begin
  if not CheckWrites(Buffer, Lens) then
  begin
    if ErrId = 0 then
      ErrId := __ERR_DS_WriteError;
    raise EZipMaster.CreateMsgDisp(ErrId, True);
  end;
end;

function TZMWorkFile.CopyFrom(Source: TZMWorkFile; Len: Int64): Int64;
const
  __ERR_DS_ReadError = __UNIT__ + (350 shl 10) + DS_ReadError;
  __ERR_DS_WriteError = __UNIT__ + (361 shl 10) + DS_WriteError;
const
  BufSize = 10 * 1024;   // constant is somewhere
var
  Buffer: array of byte;
  SizeR: Integer;
  ToRead: Integer;
  wb: pByte;
begin
  SetLength(Buffer, BufSize);
  wb := PByte(Buffer);
  Result := 0;

  while Len > 0 do
  begin
    ToRead := BufSize;
    if Len < BufSize then
      ToRead := Len;
    SizeR := Source.Read(wb^, ToRead);
    if SizeR <> ToRead then
    begin
      if SizeR < 0 then
        Result := SizeR
      else
        Result := -__ERR_DS_ReadError;
      exit;
    end;
    if SizeR > 0 then
    begin
      ToRead := Write(wb^, SizeR);
      if SizeR <> ToRead then
      begin
        if ToRead < 0 then
          Result := ToRead
        else
          Result := -__ERR_DS_WriteError;
        exit;
      end;
      Len := Len - SizeR;
      Result := Result + SizeR;
      ProgReport(zacProgress, PR_Copying, Source.FileName, SizeR);
    end;
  end;
end;

function TZMWorkFile.Copy_File(Source: TZMWorkFile): Integer;
var
  fsize: Int64;
  r: Int64;
begin
  try
    if not Source.IsOpen then
      Source.File_Open(fmOpenRead);
    Result := 0;
    fsize := Source.Seek(0, 2);
    Source.Seek(0, 0);
    ProgReport(zacXItem, PR_Copying, Source.FileName, fsize);
    r := self.CopyFrom(Source, fsize);
    if r < 0 then
      Result := Integer(r);
  except
    Result := -9; // general error
  end;
end;

function TZMWorkFile.DoRead(var Buffer; Len: Integer): Integer;
const
  __ERR_DS_ReadError = __UNIT__ + (401 shl 10) + DS_ReadError;
begin
  if Len < 0 then
    Len := -Len;
  Result := FileRead(fHandle, Buffer, Len);
  if Result > 0 then
    BytesRead := BytesRead + Len
  else if Result < 0 then
    Result := -__ERR_DS_ReadError;
end;

function TZMWorkFile.DoWrite(const Buffer; Len: Integer): Integer;
const
  __ERR_DS_WriteError = __UNIT__ + (414 shl 10) + DS_WriteError;
begin
  if Len < 0 then
    Len := (-Len) and MustFitMask;
  Result := FileWrite(fHandle, Buffer, Len);
  if Result > 0 then
    BytesWritten := BytesWritten + Len
  else if Result < 0 then
    Result := -__ERR_DS_WriteError;
end;

function TZMWorkFile.FileDate: Cardinal;
begin
  if IsOpen then
    Result := FileGetDate(fHandle)
  else
    Result := Cardinal(-1);
end;

procedure TZMWorkFile.File_Close;
var
  err: cardinal;
  th: Integer;
begin
  if fHandle <> -1 then
  begin
    th := fHandle;
    fHandle := -1;
    // if open for writing set date
    if (StampDate <> 0) and
       ((OpenMode and (SysUtils.fmOpenReadWrite or SysUtils.fmOpenWrite)) <> 0) then
    begin
      if FileSetDate(th, StampDate) <> 0 then
      begin
        // failed
        err := GetLastError;
        Diag('Warning: Set file Date' + fFileName + ' to ' + DateTimeToStr
            (FileDateToLocalDateTime(StampDate))+ ' failed ' + SysErrorMsg(err));
      end;
      if Verbosity >= zvTrace then
        Diag('Trace: Set file Date ' + fFileName + ' to ' + DateTimeToStr
            (FileDateToLocalDateTime(StampDate)));
    end;
    FileClose(th);
    if Verbosity >= zvTrace then
      Diag('Trace: Closed ' + Name(True));
  end;
  fIsOpen := false;
end;

function TZMWorkFile.File_Create(const theName: String): Boolean;
var
  n: String;
begin
  File_Close;
  Result := false;
  if theName <> '' then
  begin
    if FileName = '' then
      FileName := theName;
    n := theName;
  end
  else
    n := FileName;
  if n = '' then
    exit;
  if Verbosity >= zvTrace then
    Diag('Trace: Creating ' + n);
  fHandle := FileCreate(n);
  if fHandle <> -1 then
    AddCleanupFile(n)
  else
    Diag('Trace: FileCreate Failed: ' + n + '  ' + SysErrorMsg);
  fBytesWritten := 0;
  fBytesRead := 0;
  Result := fHandle <> -1;
  fIsOpen := Result;
  fOpenMode := SysUtils.fmOpenReadWrite;
end;

function TZMWorkFile.File_CreateTemp(const Prefix, Where: String): Boolean;
var
  Buf: String;
  Len: DWORD;
  tmpDir: String;
begin
  Result := false;
  if Length(TempDir) = 0 then
  begin
    if Length(Where) <> 0 then
    begin
      tmpDir := ExtractFilePath(Where);
      if (Length(tmpDir) > 1) and ((tmpDir[1] = '\') and (tmpDir[2] = '\') or
        (tmpDir[1] = '/') and (tmpDir[2] = '/')) then
        tmpDir := ''  // use system instead of lan
      else
        tmpDir := ExpandFileName(tmpDir);
    end;
    if Length(tmpDir) = 0 then // Get the system temp dir
    begin
      // 1. The path specified by the TMP environment variable.
      // 2. The path specified by the TEMP environment variable, if TMP is not defined.
      // 3. The current directory, if both TMP and TEMP are not defined.
      Len := GetTempPath(0, PChar(tmpDir));
      SetLength(tmpDir, Len);
      GetTempPath(Len, PChar(tmpDir));
    end;
  end
  else // Use Temp dir provided by ZipMaster
    tmpDir := TempDir;
  tmpDir := DelimitPath(tmpDir, True);
  SetLength(Buf, MAX_PATH + 12);
  if GetTempFileName(PChar(tmpDir), PChar(Prefix), 0, PChar(Buf)) <> 0 then
  begin
    FileName := PChar(Buf);
    IsTemp := True; // delete when finished
    if Verbosity >= zvTrace then
      Diag('Trace: Created temporary ' + Name(True));
    fBytesWritten := 0;
    fBytesRead := 0;
    Result := File_Open(fmOpenReadWrite);
  end //;
  else
    if Verbosity >= zvTrace then
    begin
      Diag('Trace: Creating temporary name failed ' + Name(True) + '  ' +
        SysErrorMsg);
    end;
end;

function TZMWorkFile.File_Open(Mode: Cardinal): Boolean;
//const
//  AccessMode: array[0..2] of LongWord = (
//    GENERIC_READ,
//    GENERIC_WRITE,
//    GENERIC_READ or GENERIC_WRITE);
//  ShareMode: array[0..4] of LongWord = (
//    0,
//    0,
//    FILE_SHARE_READ,
//    FILE_SHARE_WRITE,
//    FILE_SHARE_READ or FILE_SHARE_WRITE);
begin
  File_Close;
  if Verbosity >= zvTrace then
    Diag('Trace: Opening ' + Name(True));
  fHandle := _Z_FileOpen(fFileName, Mode);
  Result := fHandle <> -1;
  if (not Result) then
    Diag('Trace: FileOpen Failed: ' + Name(True) + '  ' + SysErrorMsg);
  fIsOpen := Result;
  fOpenMode := Mode;
end;

function TZMWorkFile.File_Rename(const NewName: string;
  const Safe: Boolean = false): Boolean;
begin
  if Verbosity >= zvTrace then
    Diag('Trace: Rename ' + Name(True) + ' to ' + NewName);
  IsTemp := false;
  if IsOpen then
    File_Close;
  if _Z_FileExists(FileName) then
  begin
    if _Z_FileExists(NewName) then
    begin
      if Verbosity >= zvTrace then
        Diag('Trace: Erasing ' + NewName);
      if (_Z_EraseFile(NewName, not Safe) <> 0) then
        Diag('Trace: Erase failed ' + NewName + '  ' + SysErrorMsg);
    end;
  end;
  Result := _Z_RenameFile(FileName, NewName);
  if Result then
    fFileName := NewName;  // success
end;

function TZMWorkFile.File_Reopen(Mode: Cardinal): Integer;
const
  __ERR_DS_FileOpen = __UNIT__ + (596 shl 10) + DS_FileOpen;
begin
  Result := 0;
  if (not IsOpen) or (OpenMode <> Mode) then
  begin
    File_Close;
    if Verbosity >= zvTrace then
      Diag('Trace: Reopening ' + Name());
    if not File_Open(Mode) then
    begin
      Diag('Could not File_Reopen: ' + Name(True) + '  ' + SysErrorMsg);
      Result := -__ERR_DS_FileOpen;
    end;
  end;
end;

function TZMWorkFile.GetExists: Boolean;
begin
  Result := IsOpen;
  if Result or _Z_FileExists(FileName) then
    Result := True;
end;

function TZMWorkFile.GetFileInformation(var FileInfo:
    _BY_HANDLE_FILE_INFORMATION): Boolean;
begin
  Result := IsOpen;
  if Result then
    Result := GetFileInformationByHandle(Handle, FileInfo);
  if not Result then
    ZeroMemory(@FileInfo, sizeof(_BY_HANDLE_FILE_INFORMATION));
end;

//function TZMWorkFile.GetFile_Size: Int64;
//var
//  cposn: Int64;
//begin
//  if IsOpen then
//  begin
//    cposn := Position;
//    Result := FileSeek(Handle, 0, soFromEnd);
//    if cposn <> Result then
//      Position := cposn;
//  end
//  else
//    Result := -1;
//end;

function TZMWorkFile.GetPosition: Int64;
begin
  if IsOpen then
    Result := FileSeek(fHandle, 0, soFromCurrent) // from current
  else
    Result := -1;
end;

// return true if end of segment
function TZMWorkFile.IsEndOfFile: Boolean;
var
  cposn: Int64;
begin
  if IsOpen then
  begin
    cposn := Position;
    Result := cposn = FileSeek(Handle, 0, soFromEnd);
    if not Result then
      Position := cposn;
  end
  else
    Result := True;
end;

function TZMWorkFile.LastWriteTime(var last_write: TFileTime): Boolean;
var
  BHFInfo: TByHandleFileInformation;
begin
  Result := false;
  last_write.dwLowDateTime := 0;
  last_write.dwHighDateTime := 0;
  if IsOpen then
  begin
    Result := GetFileInformationByHandle(fHandle, BHFInfo);
    if Result then
      last_write := BHFInfo.ftLastWriteTime;
  end;
end;

function TZMWorkFile.Name(Expanded: Boolean = False): string;
begin
  Result := FileName;
  if Alias <> '' then
  begin
    Result := Alias;
    if Expanded then
      Result := Result +'<' + FileName + '>';
  end;
end;

procedure TZMWorkFile.ProgReport(prog: TActionCodes; xprog: Integer; const
    Name: String; size: Int64);
var
  actn: TActionCodes;
  msg: String;
begin
  actn := prog;
  if (Name = '') and (xprog >= PR_Archive) then
    msg := ZipLoadStr(xprog)
  else
    msg := Name;
  case ShowProgress of
    zspNone:
      case prog of
        zacItem:
          actn := zacNone;
        zacProgress:
          actn := zacTick;
        zacEndOfBatch:
          actn := zacTick;
        zacCount:
          actn := zacNone;
        zacSize:
          actn := zacTick;
        zacXItem:
          actn := zacNone;
        zacXProgress:
          actn := zacTick;
      end;
    zspExtra:
      case prog of
        zacItem:
          actn := zacNone; // do nothing
        zacProgress:
          actn := zacXProgress;
        zacCount:
          actn := zacNone; // do nothing
        zacSize:
          actn := zacXItem;
      end;
  end;
  if actn <> zacNone then
    ReportProgress(actn, xprog, msg, size);
end;

function TZMWorkFile.Read(var Buffer; Len: Integer): Integer;
begin
  Result := DoRead(Buffer, Len);
end;

function TZMWorkFile.Reads(var Buffer; const Lens: array of Integer): Integer;
var
  c: Integer;
  i: Integer;
begin
  c := 0;
  for i := Low(Lens) to High(Lens) do
    c := c + Lens[i];
  Result := DoRead(Buffer, c);
end;

function TZMWorkFile.ReadTo(strm: TStream; Count: Integer): Integer;
const
  __ERR_DS_WriteError = __UNIT__ + (766 shl 10) + DS_WriteError;
  __ERR_DS_FileError = __UNIT__ + (770 shl 10) + DS_FileError;
const
  bsize = 20 * 1024;
var
  done: Integer;
  sz: Integer;
  wbufr: array of Byte;
begin
  Result := 0;
  SetLength(wbufr, bsize);
  while Count > 0 do
  begin
    sz := bsize;
    if sz > Count then
      sz := Count;
    done := DoRead(wbufr[0], sz);
    if done > 0 then
    begin
      if strm.write(wbufr[0], done) <> done then
        done := -__ERR_DS_WriteError;
    end;
    if done <> sz then
    begin
      Result := -__ERR_DS_FileError;
      if done < 0 then
        Result := done;
      break;
    end;
    Count := Count - sz;
    Result := Result + sz;
  end;
end;

function TZMWorkFile.Seek(offset: Int64; from: Integer): Int64;
begin
  if IsOpen then
    Result := FileSeek(fHandle, offset, from)
  else
    Result := -1;
end;

function TZMWorkFile.SetEndOfFile: Boolean;
begin
  if IsOpen then
    Result := Windows.SetEndOfFile(Handle)
  else
    Result := false;
end;

procedure TZMWorkFile.SetFileName(const Value: String);
begin
  if fFileName <> Value then
  begin
    if IsOpen then
      File_Close;
    fFileName := Value;
  end;
end;

//procedure TZMWorkFile.SetFile_Size(const Value: Int64);
//var
//  cposn: Int64;
//begin
//  if IsOpen and (Value >= 0) and (Value < File_Size)  then
//  begin
//    cposn := Position;
//    Position := Value;
//    SetEndOfFile;   // truncate
//    if cposn < Value then
//      Position := cposn;  // restore position
//  end;
//end;

procedure TZMWorkFile.SetHandle(const Value: Integer);
begin
  File_Close;
  fHandle := Value;
  fIsOpen := fHandle <> -1;
end;

procedure TZMWorkFile.SetPosition(const Value: Int64);
begin
  if IsOpen then
    Seek(Value, 0);
end;

function TZMWorkFile.Write(const Buffer; Len: Integer): Integer;
begin
  Result := DoWrite(Buffer, Len);
end;

function TZMWorkFile.WriteFrom(strm: TStream; Count: Integer): Int64;
const
  __ERR_DS_FileError = __UNIT__ + (864 shl 10) + DS_FileError;
const
  bsize = 20 * 1024;
var
  done: Integer;
  maxsize: Integer;
  sz: Integer;
  wbufr: array of Byte;
begin
  Result := 0;
  SetLength(wbufr, bsize);
  maxsize := strm.size - strm.Position;
  if Count > maxsize then
    Count := maxsize;
  while Count > 0 do
  begin
    sz := bsize;
    if sz > Count then
      sz := Count;
    done := strm.Read(wbufr[0], sz);
    if done > 0 then
      done := DoWrite(wbufr[0], done);
    if done <> sz then
    begin
      Result := -__ERR_DS_FileError;
      if done < 0 then
        Result := done;
      break;
    end;
    Count := Count - sz;
    Result := Result + sz;
  end;
end;

function TZMWorkFile.Writes(const Buffer; const Lens: array of Integer)
  : Integer;
var
  c: Integer;
  i: Integer;
begin
  c := 0;
  for i := Low(Lens) to High(Lens) do
    c := c + Lens[i];
  Result := DoWrite(Buffer, c);
end;

end.
