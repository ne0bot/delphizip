unit ZMStream;

//  ZMStream.pas - basic in/out for zip files

(* ***************************************************************************
TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009, 2010, 2011, 2012 Russell Peters and Roger Aelbrecht

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
//modified 2013-02-06

{$I   '.\ZipVers.inc'}

interface

uses
  {$IFDEF VERDXE2up}
    System.Classes, WinApi.Windows, System.SysUtils,
  {$ELSE}
    Classes, Windows, SysUtils,
  {$ENDIF}
   ZipMstr, ZMBody, ZMDrv, ZMZipBase;

type
  TZMStream = class(TZMStreamBase)
  private
    FBody: TZMBody;
    FFileAttrs: cardinal;
    FHandle: Integer;
    FOpenMode: Integer;
    FRealFileName: string;
    function GetLastWritten: Cardinal;
    procedure SetBody(const Value: TZMBody);
    procedure SetHandle(const Value: Integer);
  protected
    function GetIsOpen: Boolean; override;
    function GetMyType: Integer; override;
    function GetOpenMode: Word; override;
    function GetPosition: Int64; virtual;
    function GetRealFileName: string; override;
    procedure PrepareFile; virtual;
    procedure SetPosition(const Value: Int64); virtual;
    property Body: TZMBody read FBody write SetBody;
    property Handle: Integer read FHandle write SetHandle;
    property OpenMode: Integer read FOpenMode write FOpenMode;
  public
    constructor Create(MyOwner: TZMZipBase; const FileName: string; Mode: Word);
    procedure AfterConstruction; override;
    procedure AssignFrom(Src: TZMStream); virtual;
    procedure BeforeDestruction; override;
    function FileDate: Cardinal;
    procedure File_Close; override;
    procedure File_Create(const theName: String); override;
    function File_GetDate: Cardinal; override;
    procedure File_Open(const FileName: string; Mode: Word); override;
    function File_SetDate(DOSdate: Cardinal): Boolean; override;
    function File_SetTime(CreateTime, AccessTime, WriteTime: TFileTime): boolean;
        override;
    function FinaliseWrite: integer; virtual;
    function FixFileAttrs: integer; virtual;
    function FixFileDate: integer; virtual;
    function GetFileInformation(var FileInfo: _BY_HANDLE_FILE_INFORMATION): Boolean;  override;
    function IsEndOfFile: Boolean; override;
    function LastWriteTime(var last_write: TFileTime): Boolean; override;
    function Read(var Buffer; Len: Integer): Integer; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function SetEndOfFile: Boolean; override;
    function Write(const Buffer; Len: Integer): Integer; override;
    property FileAttrs: cardinal read FFileAttrs write FFileAttrs;
    property LastWritten: Cardinal read GetLastWritten;
    property Position: Int64 read GetPosition write SetPosition;
    property RealFileName: string read GetRealFileName;
  end;


implementation

uses
  ZMMsg, ZMXcpt, ZMWFuncs, ZMStructs, ZMCtx, ZMDlg,
  ZMUtils;

const
  __UNIT__ = 20;// shl 23;

const
  MaxDiskBufferSize = (4 * 1024 * 1024); // floppies only

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

{ TZMStream }
constructor TZMStream.Create(MyOwner: TZMZipBase; const FileName: string; Mode:
    Word);
begin
  inherited Create(MyOwner);
  FBody := Owner.Body;
  FOpenMode := Mode;
  FRealFileName := FileName;
end;

procedure TZMStream.AfterConstruction;
begin
  inherited;
  FHandle := -1;
  PrepareFile;
end;

procedure TZMStream.AssignFrom(Src: TZMStream);
begin
  if (Src <> Self) and (Src <> nil) then
  begin
    FRealFileName := Src.FRealFileName;
    FHandle := -1;  // don't acquire handle
    IsTemp := Src.IsTemp;
    FOpenMode := Src.FOpenMode;
    StampDate := Src.StampDate;
  end;
end;

procedure TZMStream.BeforeDestruction;
begin
  File_Close;
  if IsTemp and _Z_FileExists(RealFileName) then
  begin
    Reporter.Trace('Deleting ' + RealFileName);
    File_Delete(RealFileName);
  end;
  inherited;
end;

function TZMStream.FileDate: Cardinal;
begin
  Result := Cardinal(-1);
  if FHandle <> -1 then
    Result := FileGetDate(Handle);
end;

procedure TZMStream.File_Close;
var
  th: Integer;
begin
  if FHandle <> -1 then
  begin
    th := FHandle;
    FHandle := -1;
    FileClose(th);
    Reporter.Trace('Closed ' + RealFileName);
  end;
end;

procedure TZMStream.File_Create(const theName: String);
begin
  if Handle <> -1 then
    File_Close;
  FRealFileName := theName;
  if theName <> '' then
  begin
    FHandle := FileCreate(theName);
    if FHandle = -1 then
      Reporter.InformSys('FileCreate Failed: ' + theName);
    FOpenMode := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.fmOpenReadWrite;
  end;
end;

function TZMStream.File_GetDate: Cardinal;
begin
  Result := FileGetDate(Handle);
end;

procedure TZMStream.File_Open(const FileName: string; Mode: Word);
begin
  if Handle <> -1 then
    File_Close;
  FRealFileName := FileName;
  FHandle := _Z_FileOpen(RealFileName, Mode);
  if Handle < 0 then
    Reporter.InformSys('FileOpen Failed: ' + RealFileName)
  else
    Reporter.Trace('Opened ' + RealFileName);
  FOpenMode := Mode;
end;

function TZMStream.File_SetDate(DOSdate: Cardinal): Boolean;
begin
  Result := False;
  if IsOpen then
  begin
    if FileSetDate(Handle, DOSDate) <> 0 then
    begin
      // failed
      Reporter.InformSys('Warning: Set file Date' + RealFileName + ' to ' +
        DateTimeToStr(FileDateToLocalDateTime(DOSDate)) + ' failed ');
    end
    else
    begin
      Result := True;
      if Reporter.Verbosity >= zvTrace then
        Reporter.Trace('Set file Date ' + RealFileName + ' to ' +
          DateTimeToStr(FileDateToLocalDateTime(DOSDate)));
    end;
  end;
end;

function TZMStream.File_SetTime(CreateTime, AccessTime, WriteTime: TFileTime):
    boolean;
begin
  Result := SetFileTime(Handle, @CreateTime, @AccessTime, @WriteTime);
end;

function TZMStream.FinaliseWrite: integer;
begin
  Result := 0;
  if StampDate <> 0 then
    FixFileDate;
  if FileAttrs <> 0 then
    FixFileAttrs;
end;

function TZMStream.FixFileAttrs: integer;
begin
  Result := 0;
  // set attributes
  File_Close;
  if not SetFileAttributes(PChar(RealFileName), FileAttrs and $7F) then
  begin
    Reporter.InformSys('Failed to set file attributes: ' + RealFileName);
    Result := -ZS_SetFileAttributes;
  end;
end;

function TZMStream.FixFileDate: integer;
begin
  Result := 0;
  if not File_SetDate(StampDate) then
    Result := -ZS_SetDateError;
end;

function TZMStream.GetFileInformation(var FileInfo:
    _BY_HANDLE_FILE_INFORMATION): Boolean;
begin
  Result := false;
  if Handle <> -1 then
    Result := GetFileInformationByHandle(Handle, FileInfo);
  if not Result then
    ZeroMemory(@FileInfo, sizeof(_BY_HANDLE_FILE_INFORMATION));
end;

function TZMStream.GetIsOpen: Boolean;
begin
  Result := (Handle <> -1);
end;

function TZMStream.GetLastWritten: Cardinal;
var
  ft: TFileTime;
begin
  Result := 0;
  if (Handle <> -1) and LastWriteTime(ft) then
    Result := FileTimeToLocalDOSTime(ft);
end;

function TZMStream.GetMyType: Integer;
begin
  Result := ZM_INT_STREAM;
end;

function TZMStream.GetOpenMode: Word;
begin
  Result := FOpenMode;
end;

function TZMStream.GetPosition: Int64;
begin
  if Handle <> -1 then
    Result := FileSeek(fHandle, 0, soFromCurrent)
  else
    Result := -1;
end;

function TZMStream.GetRealFileName: string;
begin
  Result := FRealFileName;
end;

// return true if end of segment
function TZMStream.IsEndOfFile: Boolean;
var
  cposn: Int64;
begin
  Result := True;
  if Handle <> -1 then
  begin
    cposn := Position;
    Result := cposn = FileSeek(Handle, 0, soFromEnd);
    if not Result then
      Position := cposn;
  end;
end;

function TZMStream.LastWriteTime(var last_write: TFileTime): Boolean;
var
  BHFInfo: TByHandleFileInformation;
begin
  Result := false;
  last_write.dwLowDateTime := 0;
  last_write.dwHighDateTime := 0;
  if Handle <> -1 then
  begin
    Result := GetFileInformationByHandle(Handle, BHFInfo);
    if Result then
      last_write := BHFInfo.ftLastWriteTime;
  end;
end;

procedure TZMStream.PrepareFile;
begin
  if OpenMode = fmCreate then
    File_Create(FRealFileName)
  else
    File_Open(FRealFileName, OpenMode);
end;

function TZMStream.Read(var Buffer; Len: Integer): Integer;
begin
  Assert(Len >= 0, 'TZMStream read len < 0');
  Result := -1;
  if Handle <> -1 then
    Result := FileRead(Handle, Buffer, Len);
end;

function TZMStream.Seek(const offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := -1;
  if Handle <> -1 then
    Result := FileSeek(Handle, offset, WORD(Origin));
end;

procedure TZMStream.SetBody(const Value: TZMBody);
begin
  // never change
end;

function TZMStream.SetEndOfFile: Boolean;
begin
  Result := False;
  if Handle <> -1 then
    Result := {$IFDEF VERDXE2up}WinApi.{$ENDIF}Windows.SetEndOfFile(Handle);
end;

procedure TZMStream.SetHandle(const Value: Integer);
begin
  if FHandle <> Value then
  begin
    if Handle <> -1 then
      File_Close;
    FHandle := Value;
  end;
end;

procedure TZMStream.SetPosition(const Value: Int64);
begin
  if Handle <> -1 then
    Seek(Value, 0);
end;

function TZMStream.Write(const Buffer; Len: Integer): Integer;
begin
  Result := -1;
  Assert(Len >= 0, 'TZMStream write Len < 0');
  if Handle <> -1 then
    Result := FileWrite(Handle, Buffer, Len);
end;

end.

