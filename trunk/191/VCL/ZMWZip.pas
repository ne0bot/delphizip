unit ZMWZip;

//  ZMWZip.pas - basic in/out for zip files

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
{$IFDEF VER180}
 {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

interface

uses
  Classes, Windows, SysUtils, ZipMstr, ZMCore, ZMWorkFile, ZMDelZip, ZMDrv;

//type
//  TZipWrites = (zwDefault, zwSingle, zwMultiple);

type
  TZMWorkZip = class(TZMWorkFile)
  private
    fAllowedSize: Int64;
    fDiskNr: Integer;
    FFile_Size: Int64;
//    FFlushing: boolean;
    fInfo: Cardinal;
    fLastWrite: TFileTime;
    FRealFileName: String;
    fRealFileSize: Int64;
    FReqFileName: String;
    fSig: TZipFileSigs;
    fTotalDisks: Integer;
    fWorkDrive: TZMWorkDrive;
    FZipDiskAction: TZMDiskAction;
    FZipDiskStatus: TZMZipDiskStatus;
    WBuf: array of Byte;
    function GetKeepFreeOnAllDisks: Cardinal;
    function GetKeepFreeOnDisk1: Cardinal;
    function GetLastWritten: Cardinal;
    function GetMaxVolumeSize: Int64;
    function GetMinFreeVolumeSize: Cardinal;
    procedure SetFile_Size(const Value: Int64);
    procedure SetKeepFreeOnAllDisks(const Value: Cardinal);
    procedure SetKeepFreeOnDisk1(const Value: Cardinal);
    procedure SetMaxVolumeSize(const Value: Int64);
    procedure SetWorkDrive(const Value: TZMWorkDrive);
    function WriteSplit(const Buffer; ToWrite: Integer): Integer;
  protected
    fBufferPosition: Integer;
    fConfirmErase: Boolean;
//    fDiskBuffer: TByteBuffer;
    FDiskWritten: Cardinal;
    fIsMultiPart: Boolean;
    FNewDisk: Boolean;
    FNumbering: TZipNumberScheme;
    fSavedFileInfo: _BY_HANDLE_FILE_INFORMATION;
    function ChangeNumberedName(const FName: String; NewNbr: Cardinal; Remove:
        boolean): string;
    procedure CheckForDisk(writing, UnformOk: Boolean);
    procedure ClearFloppy(const dir: String);
    procedure FlushDiskBuffer;
    function GetPosition: Int64; override;
    function HasSpanSig(const FName: String): boolean;
    function IsRightDisk: Boolean;
    procedure NewFlushDisk;
    function NewSegment: Boolean;
    function OldVolName(Part: Integer): String;
    procedure SetFileName(const Value: String); override;
    procedure SetPosition(const Value: Int64); override;
    function VolName(Part: Integer): String;
    function ZipFormat(const NewName: String): Integer;
    property AllowedSize: Int64 Read fAllowedSize Write fAllowedSize;
    property RealFileName: String read FRealFileName;
  public
    procedure AfterConstruction; override;
    function AskAnotherDisk(const DiskFile: String): Integer;
    function AskOverwriteSegment(const DiskFile: String; DiskSeq: Integer): Integer;
    procedure AssignFrom(Src: TZMWorkFile); override;
    procedure BeforeDestruction; override;
    procedure ClearFileInformation;
    function CreateMVFileNameEx(const FileName: String;
      StripPartNbr, Compat: Boolean): String;
    procedure File_Close; override;
    function File_Create(const theName: String): Boolean; override;
    function File_Open(Mode: Cardinal): Boolean; override;
    function FinishWrite: Integer;
    procedure GetNewDisk(DiskSeq: Integer; AllowEmpty: Boolean);
    function MapNumbering(Opts: TZMSpanOpts): TZMSpanOpts;
    function Name(Expanded: Boolean = False): string; override;
    function Read(var Buffer; Len: Integer): Integer; override;
    function Reads(var Buffer; const Lens: array of Integer): Integer; override;
    function SaveFileInformation: Boolean;
    function SeekDisk(Nr: Integer): Integer;
    function VerifyFileInformation(IgnoreWriteTime: boolean = False): Boolean;
    function WBuffer(size: Integer): pByte;
    function Write(const Buffer; Len: Integer): Integer; override;
    function Writes(const Buffer; const Lens: array of Integer): Integer; override;
    property DiskNr: Integer read fDiskNr write fDiskNr;
    property File_Size: Int64 read FFile_Size write SetFile_Size;
    property info: Cardinal read fInfo write fInfo;
    property IsMultiPart: Boolean read fIsMultiPart write fIsMultiPart;
    property KeepFreeOnAllDisks: Cardinal read GetKeepFreeOnAllDisks write
        SetKeepFreeOnAllDisks;
    property KeepFreeOnDisk1: Cardinal read GetKeepFreeOnDisk1 write
      SetKeepFreeOnDisk1;
    property LastWritten: Cardinal read GetLastWritten;
    property MaxVolumeSize: Int64 read GetMaxVolumeSize write SetMaxVolumeSize;
    property MinFreeVolumeSize: Cardinal read GetMinFreeVolumeSize;
    property NewDisk: Boolean Read FNewDisk Write FNewDisk;
    property Numbering: TZipNumberScheme Read FNumbering Write FNumbering;
    property RealFileSize: Int64 read fRealFileSize write fRealFileSize;
    property ReqFileName: String read FReqFileName write FReqFileName;
    property Sig: TZipFileSigs read fSig write fSig;
    property TotalDisks: Integer read fTotalDisks write fTotalDisks;
    property WorkDrive: TZMWorkDrive read fWorkDrive write SetWorkDrive;
  end;

const
//  zfi_None: Cardinal = 0;
//  zfi_Open: Cardinal = 1;
//  zfi_Create: Cardinal = 2;
  zfi_Dirty: Cardinal = 4;
  zfi_MakeMask: Cardinal = $07;
  zfi_Error: Cardinal = 8;
//  zfi_NotFound: cardinal = $10;     // named file not found
//  zfi_NoLast: cardinal = $20;       // last file not found
  zfi_Loading: cardinal = $40;
  zfi_Cancelled: cardinal = $80;    // loading was cancelled
//  zfi_FileMask: cardinal = $F0;

implementation

uses
  Forms, Controls, Dialogs, ZMMsgStr, ZMCtx, ZMCompat, ZMDlg,
  ZMStructs, ZMUtils, ZMMsg, ZMXcpt, ZMWFuncs;

const
  __UNIT__ = 31 shl 23;

const
  MAX_PARTS = 999;
  MaxDiskBufferSize = (4 * 1024 * 1024); // floppies only

const
  SZipSet = 'ZipSet_';
  SPKBACK = 'PKBACK#';

  (* ? FormatFloppy
    *)
function FormatFloppy(WND: HWND; const Drive: String): Integer;
const
  SHFMT_ID_DEFAULT = $FFFF;
  { options }
  SHFMT_OPT_FULL = $0001;
  // SHFMT_OPT_SYSONLY = $0002;
  { return values }
  // SHFMT_ERROR = $FFFFFFFF;
  // -1 Error on last format, drive may be formatable
  // SHFMT_CANCEL = $FFFFFFFE;    // -2 last format cancelled
  // SHFMT_NOFORMAT = $FFFFFFFD;    // -3 drive is not formatable
type
  TSHFormatDrive = function(WND: HWND; Drive, fmtID, Options: DWORD): DWORD;
    stdcall;
var
  SHFormatDrive: TSHFormatDrive;
var
  drv: Integer;
  hLib: THandle;
  OldErrMode: Integer;
begin
  Result := -3; // error
  if not((Length(Drive) > 1) and (Drive[2] = ':') and CharInSet
      (Drive[1], ['A' .. 'Z', 'a' .. 'z'])) then
    exit;
  if GetDriveType(PChar(Drive)) <> DRIVE_REMOVABLE then
    exit;
  drv := Ord(Upcase(Drive[1])) - Ord('A');
  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  hLib := LoadLibrary('Shell32');
  if hLib <> 0 then
  begin
    @SHFormatDrive := GetProcAddress(hLib, 'SHFormatDrive');
//    if @SHFormatDrive <> nil then
    if assigned(SHFormatDrive) then
      try
        Result := SHFormatDrive(WND, drv, SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);
      finally
        FreeLibrary(hLib);
      end;
    SetErrorMode(OldErrMode);
  end;
end;

procedure TZMWorkZip.AfterConstruction;
begin
  inherited;
//  fDiskBuffer := nil;
//  fBufferPosition := -1;
  fInfo := 0;
  fIsMultiPart := false;
  fNumbering := znsNone;
  fWorkDrive := TZMWorkDrive.Create;
  ClearFileInformation;
end;

function TZMWorkZip.AskAnotherDisk(const DiskFile: String): Integer;
var
  MsgQ: String;
  tmpStatusDisk: TZMStatusDiskEvent;
begin
  MsgQ := ZipLoadStr(DS_AnotherDisk);
  FZipDiskStatus := FZipDiskStatus + [zdsSameFileName];
  tmpStatusDisk := Master.OnStatusDisk;
  if Assigned(tmpStatusDisk) and not(zaaYesOvrwrt in AnswerAll) then
  begin
    FZipDiskAction := zdaOk; // The default action
    tmpStatusDisk(Master, 0, DiskFile, FZipDiskStatus, FZipDiskAction);
    case FZipDiskAction of
      zdaCancel:
        Result := idCancel;
      zdaReject:
        Result := idNo;
      zdaErase:
        Result := idOk;
      zdaYesToAll:
        begin
          Result := idOk;
        end;
      zdaOk:
        Result := idOk;
    else
      Result := idOk;
    end;
  end
  else
    Result := ZipMessageDlgEx(ZipLoadStr(FM_Confirm), MsgQ,
      zmtWarning + DHC_SpanOvr, [mbOk, mbCancel]);
end;

function TZMWorkZip.AskOverwriteSegment(const DiskFile: String; DiskSeq:
    Integer): Integer;
var
  MsgQ: String;
  tmpStatusDisk: TZMStatusDiskEvent;
begin
  // Do we want to overwrite an existing file?
  if FileExists(DiskFile) then
    if (File_Age(DiskFile) = StampDate) and (Pred(DiskSeq) < DiskNr)
      then
    begin
      MsgQ := ZipFmtLoadStr(DS_AskPrevFile, [DiskSeq]);
      FZipDiskStatus := FZipDiskStatus + [zdsPreviousDisk];
    end
    else
    begin
      MsgQ := ZipFmtLoadStr(DS_AskDeleteFile, [DiskFile]);
      FZipDiskStatus := FZipDiskStatus + [zdsSameFileName];
    end
    else if not WorkDrive.DriveIsFixed then
      if (WorkDrive.VolumeSize <> WorkDrive.VolumeSpace) then
        FZipDiskStatus := FZipDiskStatus + [zdsHasFiles]
        // But not the same name
      else
        FZipDiskStatus := FZipDiskStatus + [zdsEmpty];
  tmpStatusDisk := Master.OnStatusDisk;
  if Assigned(tmpStatusDisk) and not(zaaYesOvrwrt in AnswerAll) then
  begin
    FZipDiskAction := zdaOk; // The default action
    tmpStatusDisk(Master, DiskSeq, DiskFile, FZipDiskStatus,
      FZipDiskAction);
    case FZipDiskAction of
      zdaCancel:
        Result := idCancel;
      zdaReject:
        Result := idNo;
      zdaErase:
        Result := idOk;
      zdaYesToAll:
        begin
          Result := idOk;
          AnswerAll := AnswerAll + [zaaYesOvrwrt];
        end;
      zdaOk:
        Result := idOk;
    else
      Result := idOk;
    end;
  end
  else if ((FZipDiskStatus * [zdsPreviousDisk, zdsSameFileName]) <> []) and not
    ((zaaYesOvrwrt in AnswerAll) or Unattended) then
  begin
    Result := ZipMessageDlgEx(ZipLoadStr(FM_Confirm), MsgQ,
      zmtWarning + DHC_SpanOvr, [mbYes, mbNo, mbCancel, mbYesToAll]);
    if Result = mrYesToAll then
    begin
      AnswerAll := AnswerAll + [zaaYesOvrwrt];
      Result := idOk;
    end;
  end
  else
    Result := idOk;
end;

// Src should not be open but not enforced
procedure TZMWorkZip.AssignFrom(Src: TZMWorkFile);
var
  zsrc: TZMWorkZip;
begin
  if (Src <> Self) and (Src <> nil) then
  begin
    inherited;
    if Src is TZMWorkZip then
    begin
      zsrc := TZMWorkZip(Src);
//      fDiskBuffer := nil;
//      fBufferPosition := -1;
      Move(zsrc.fSavedFileInfo, fSavedFileInfo, SizeOf(fSavedFileInfo));
      fAllowedSize := zsrc.fAllowedSize;
      fDiskNr := zsrc.fDiskNr;
      fFile_Size := zsrc.fFile_Size;
      fInfo := zsrc.fInfo;
      fLastWrite := zsrc.fLastWrite;
      fNumbering := zsrc.fNumbering;
      fRealFileName := zsrc.fRealFileName;
      fReqFileName := zsrc.FReqFileName;
      fSig := zsrc.fSig;
      fTotalDisks := zsrc.fTotalDisks;
      fWorkDrive.AssignFrom(zsrc.WorkDrive);
      FZipDiskAction := zsrc.FZipDiskAction;
      FZipDiskStatus := zsrc.FZipDiskStatus;
    end;
  end;
end;

procedure TZMWorkZip.BeforeDestruction;
begin
  File_Close;
  if IsTemp and _Z_FileExists(fRealFileName) then
  begin
    if Verbosity >= zvTrace then
      Diag('Trace: Deleting ' + fRealFileName);
    _Z_DeleteFile(fRealFileName);
    IsTemp := False;  // avoid deleting again
  end;
  FreeAndNil(fWorkDrive);
//  fDiskBuffer := nil; // ++ discard contents
  WBuf := nil;
  inherited;
end;

// uses 'real' number
function TZMWorkZip.ChangeNumberedName(const FName: String; NewNbr: Cardinal;
    Remove: boolean): string;
const
  __ERR_DS_TooManyParts = __UNIT__ + (399 shl 10) + DS_TooManyParts;
var
  ext: string;
  StripLen: Integer;
begin
  if DiskNr > 999 then
    raise EZipMaster.CreateMsgDisp(__ERR_DS_TooManyParts, True);
  ext := ExtractFileExt(FName);
  StripLen := 0;
  if Remove then
    StripLen := 3;
  Result := Copy(FName, 1, Length(FName) - Length(ext) - StripLen)
    + Copy(IntToStr(1000 + NewNbr), 2, 3) + ext;
end;

procedure TZMWorkZip.CheckForDisk(writing, UnformOk: Boolean);
const
  __ERR_DS_NoUnattSpan = __UNIT__ + (437 shl 10) + DS_NoUnattSpan;
  __ERR_DS_TooManyParts = __UNIT__ + (455 shl 10) + DS_TooManyParts;
  __ERR_GE_Abort = __UNIT__ + (492 shl 10) + GE_Abort;
  __ERR_DS_Canceled = __UNIT__ + (494 shl 10) + DS_Canceled;
var
  OnGetNextDisktmp: TZMGetNextDiskEvent;
  AbortAction: Boolean;
  MsgFlag: Integer;
  MsgStr: String;
  Res: Integer;
  SizeOfDisk: Int64;
  totDisks: Integer;
begin
  if TotalDisks <> 1 then // check
    IsMultiPart := True;
  if WorkDrive.DriveIsFixed then
  begin
    // If it is a fixed disk we don't want a new one.
    NewDisk := false;
    CheckCancel;
    exit;
  end;
  KeepAlive;       // just ProcessMessages
  // First check if we want a new one or if there is a disk (still) present.
  while (NewDisk or (not WorkDrive.HasMedia(UnformOk))) do
  begin
    if Unattended then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_NoUnattSpan, True);

    MsgFlag := zmtWarning + DHC_SpanNxtW; // or error?
    if DiskNr < 0 then // want last disk
    begin
      MsgStr := ZipLoadStr(DS_InsertDisk);
      MsgFlag := zmtError + DHC_SpanNxtR;
    end
    else if writing then
    begin
      // This is an estimate, we can't know if every future disk has the same space available and
      // if there is no disk present we can't determine the size unless it's set by MaxVolumeSize.
      SizeOfDisk := WorkDrive.VolumeSize - KeepFreeOnAllDisks;
      if (MaxVolumeSize <> 0) and (MaxVolumeSize < WorkDrive.VolumeSize) then
        SizeOfDisk := MaxVolumeSize;

      TotalDisks := DiskNr + 1;
      if TotalDisks > MAX_PARTS then
        raise EZipMaster.CreateMsgDisp(__ERR_DS_TooManyParts, True);
      if SizeOfDisk > 0 then
      begin
        totDisks := Trunc((File_Size + 4 + KeepFreeOnDisk1) / SizeOfDisk);
        if TotalDisks < totDisks then
          TotalDisks := totDisks;
        MsgStr := ZipFmtLoadStr
          (DS_InsertVolume, [DiskNr + 1, TotalDisks]);
      end
      else
        MsgStr := ZipFmtLoadStr(DS_InsertAVolume, [DiskNr + 1]);
    end
    else
    begin // reading - want specific disk
      if TotalDisks = 0 then
        MsgStr := ZipFmtLoadStr(DS_InsertAVolume, [DiskNr + 1])
      else
        MsgStr := ZipFmtLoadStr(DS_InsertVolume, [DiskNr + 1, TotalDisks]);
    end;

    MsgStr := MsgStr + ZipFmtLoadStr(DS_InDrive, [WorkDrive.DriveStr]);
    OnGetNextDisktmp := Master.OnGetNextDisk;
    if Assigned(OnGetNextDisktmp) then
    begin
      AbortAction := false;
      OnGetNextDisktmp(Master, DiskNr + 1, TotalDisks, Copy
          (WorkDrive.DriveStr, 1, 1), AbortAction);
      if AbortAction then
        Res := idAbort
      else
        Res := idOk;
    end
    else
      Res := ZipMessageDlgEx('', MsgStr, MsgFlag, mbOkCancel);

    if Res <> idOk then
    begin
      Cancel := __ERR_GE_Abort;
      info := info or zfi_Cancelled;
      raise EZipMaster.CreateMsgDisp(__ERR_DS_Canceled, false);
    end;
    NewDisk := false;
    KeepAlive;
  end;
end;

procedure TZMWorkZip.ClearFileInformation;
begin
  ZeroMemory(@fSavedFileInfo, sizeof(_BY_HANDLE_FILE_INFORMATION));
end;

procedure TZMWorkZip.ClearFloppy(const dir: String);
var
  Fname: String;
  SRec: _Z_TSearchRec;
begin
  if _Z_FindFirst(dir + WILD_ALL, faAnyFile, SRec) = 0 then
    repeat
      Fname := dir + SRec.Name;
      if ((SRec.Attr and faDirectory) <> 0) and (SRec.Name <> DIR_THIS) and
        (SRec.Name <> DIR_PARENT) then
      begin
        Fname := Fname + PathDelim;
        ClearFloppy(Fname);
        if Verbosity >= zvTrace then
          ReportMsg(TM_Erasing, [Fname])
        else
          KeepAlive;
        // allow time for OS to delete last file
        _Z_RemoveDir(Fname);
      end
      else
      begin
        if Verbosity >= zvTrace then
          ReportMsg(TM_Deleting, [Fname])
        else
          KeepAlive;
        _Z_DeleteFile(Fname);
      end;
    until _Z_FindNext(SRec) <> 0;
    _Z_FindClose(SRec);
end;

function TZMWorkZip.CreateMVFileNameEx(const FileName: String;
  StripPartNbr, Compat: Boolean): String;
var
  ext: String;
begin // changes FileName into multi volume FileName
  if Compat then
  begin
    if DiskNr <> (TotalDisks - 1) then
    begin
      if DiskNr < 9 then
        ext := '.z0'
      else
        ext := '.z';
      ext := ext + IntToStr(succ(DiskNr));
    end
    else
      ext := EXT_ZIP;
    Result := ChangeFileExt(FileName, ext);
  end
  else
    Result := ChangeNumberedName(FileName, DiskNr + 1, StripPartNbr);
end;

procedure TZMWorkZip.File_Close;
begin
//  if fDiskBuffer <> nil then
//    FlushDiskBuffer;
  inherited;
end;

function TZMWorkZip.File_Create(const theName: String): Boolean;
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
  fRealFileName := n;
  Result := inherited File_Create(n);
end;

function TZMWorkZip.File_Open(Mode: Cardinal): Boolean;
begin
  Result := inherited File_Open(Mode);
  fRealFileName := FileName;
end;

// rename last part after Write
function TZMWorkZip.FinishWrite: Integer;
const
  __ERR_DS_NoRenamePart = __UNIT__ + (645 shl 10) + DS_NoRenamePart;
var
  fn: String;
  LastName: String;
  MsgStr: String;
  Res: Integer;
  OnStatusDisk: TZMStatusDiskEvent;
begin
  // change extn of last file
  LastName := RealFileName;
  File_Close;
  Result := 0;

  if IsMultiPart then
  begin
    if ((Numbering = znsExt) and not AnsiSameText(ExtractFileExt(LastName), EXT_ZIP)) or
      ((Numbering = znsName) and (DiskNr = 0)) then
    begin
      Result := -1;
      fn := FileName;
      if (FileExists(fn)) then
      begin
        MsgStr := ZipFmtLoadStr(DS_AskDeleteFile, [fn]);
        FZipDiskStatus := FZipDiskStatus + [zdsSameFileName];
        Res := idYes;
        if not(zaaYesOvrwrt in AnswerAll) then
        begin
          OnStatusDisk := Master.OnStatusDisk;
          if Assigned(OnStatusDisk) then // 1.77
          begin
            FZipDiskAction := zdaOk; // The default action
            OnStatusDisk(Master, DiskNr, fn, FZipDiskStatus,
              FZipDiskAction);
            if FZipDiskAction = zdaYesToAll then
            begin
              AnswerAll := AnswerAll + [zaaYesOvrwrt];
              FZipDiskAction := zdaOk;
            end;
            if FZipDiskAction = zdaOk then
              Res := idYes
            else
              Res := idNo;
          end
          else
            Res := ZipMessageDlgEx(MsgStr, ZipLoadStr(FM_Confirm)
                , zmtWarning + DHC_WrtSpnDel, [mbYes, mbNo]);
        end;
        if (Res = idNo) then
          ReportMsg(__ERR_DS_NoRenamePart, [LastName]);
        if (Res = idYes) then
          _Z_DeleteFile(fn); // if it exists delete old one
      end;
      if _Z_FileExists(LastName) then // should be there but ...
      begin
        _Z_RenameFile(LastName, fn);
        Result := 0;
        if Verbosity >= zvVerbose then
          Diag(Format('renamed %s to %s', [LastName, fn]));
      end;
    end;
  end;
end;

procedure TZMWorkZip.FlushDiskBuffer;
//var
//  did: Integer;
//  Len: Integer;
begin
//  if fDiskBuffer <> nil then
//  begin
//    Len := fBufferPosition;
//    fBufferPosition := -1; // stop retrying on error
//    FFlushing := True;
//    KeepAlive;
//    CheckCancel;
//    if Len > 0 then
//    begin
////      repeat
////        did := DoWrite(fDiskBuffer[0], Len);
//        did := Write(fDiskBuffer[0], Len);
//        if did <> Len then
//        begin
//          raise EZipMaster.CreateMsgDisp(DS_WriteError, True);
////          NewFlushDisk; // abort or try again on new disk
//        end;
////      until (did = Len);
//    end;
//    fDiskBuffer := nil;
//  end;
end;

function TZMWorkZip.GetKeepFreeOnAllDisks: Cardinal;
begin
  Result := fInternal.fKeepFreeOnAllDisks;
end;

function TZMWorkZip.GetKeepFreeOnDisk1: Cardinal;
begin
  Result := fInternal.fKeepFreeOnDisk1;
end;

function TZMWorkZip.GetLastWritten: Cardinal;
var
  ft: TFileTime;
begin
  Result := 0;
  if IsOpen and LastWriteTime(ft) then
    Result := FileTimeToLocalDOSTime(ft);
end;

function TZMWorkZip.GetMaxVolumeSize: Int64;
begin
  Result := fInternal.fMaxVolumeSize;
end;

function TZMWorkZip.GetMinFreeVolumeSize: Cardinal;
begin
  Result := fInternal.fMinFreeVolumeSize;
end;

procedure TZMWorkZip.GetNewDisk(DiskSeq: Integer; AllowEmpty: Boolean);
const
  __ERR_DS_NoInFile = __UNIT__ + (743 shl 10) + DS_NoInFile;
begin
  File_Close;
  // Close the file on the old disk first.
  if (TotalDisks <> 1) or (DiskSeq <> 0) then
    IsMultiPart := True;
  DiskNr := DiskSeq;
  while True do
  begin
    repeat
      NewDisk := True;
      File_Close;
      CheckForDisk(false, spTryFormat in SpanOptions);
      if AllowEmpty and WorkDrive.HasMedia(spTryFormat in SpanOptions) then
      begin
        if WorkDrive.VolumeSpace = -1 then
          exit; // unformatted
        if WorkDrive.VolumeSpace = WorkDrive.VolumeSize then
          exit; // empty
      end;
    until IsRightDisk;

    if Verbosity >= zvVerbose then
      Diag(ZipFmtLoadStr(TM_GetNewDisk, [FileName]));
    if File_Open(fmShareDenyWrite or fmOpenRead) then
      break; // found
    if WorkDrive.DriveIsFixed then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_NoInFile, True)
    else
      ShowZipMessage(__ERR_DS_NoInFile, '');
  end;
end;

function TZMWorkZip.GetPosition: Int64;
begin
//  if fDiskBuffer <> nil then
//    Result := fBufferPosition
//  else
    Result := inherited GetPosition;
end;

function TZMWorkZip.HasSpanSig(const FName: String): boolean;
var
  fs: TFileStream;
  Sg: Cardinal;
begin
  Result := False;
  if FileExists(FName) then
  begin
    fs := TFileStream.Create(FName, fmOpenRead);
    try
      if (fs.Size > (sizeof(TZipLocalHeader) + sizeof(Sg))) and
        (fs.Read(Sg, sizeof(Sg)) = sizeof(Sg)) then
        Result :=  (Sg = ExtLocalSig) and (fs.Read(Sg, sizeof(Sg)) = sizeof(Sg)) and
          (Sg = LocalFileHeaderSig);
    finally
      fs.Free;
    end;
  end;
end;

function TZMWorkZip.IsRightDisk: Boolean;
var
  fn: String;
  VName: string;
begin
  Result := True;
  if (Numbering < znsName) and (not WorkDrive.DriveIsFixed) then
  begin
    VName := WorkDrive.DiskName;
    Diag('Checking disk ' + VName + ' need ' + VolName(DiskNr));
    if (AnsiSameText(VName, VolName(DiskNr)) or AnsiSameText(VName, OldVolName(DiskNr))) and
        FileExists(FileName) then
    begin
      Numbering := znsVolume;
      Diag('found volume ' + VName);
      exit;
    end;
  end;
  fn := FileName;
  if Numbering = znsNone then // not known yet
  begin
    FileName := CreateMVFileNameEx(FileName, True, True);
    // make compat name
    if FileExists(FileName) then
    begin
      Numbering := znsExt;
      exit;
    end;
    FileName := fn;
    FileName := CreateMVFileNameEx(FileName, True, false);
    // make numbered name
    if FileExists(FileName) then
    begin
      Numbering := znsName;
      exit;
    end;
    if WorkDrive.DriveIsFixed then
      exit; // always true - only needed name
    FileName := fn; // restore
    Result := false;
    exit;
  end;
  // numbering scheme already known
  if Numbering = znsVolume then
  begin
    Result := false;
    exit;
  end;
  FileName := CreateMVFileNameEx(FileName, True, Numbering = znsExt);
  // fixed drive always true only needed new filename
  if (not WorkDrive.DriveIsFixed) and (not FileExists(FileName)) then
  begin
    FileName := fn; // restore
    Result := false;
  end;
end;

function TZMWorkZip.MapNumbering(Opts: TZMSpanOpts): TZMSpanOpts;
var
  spans: TZMSpanOpts;
begin
  Result := Opts;
  if Numbering <> znsNone then
  begin
    // map numbering type only if known
    spans := Opts - [spCompatName] + [spNoVolumeName];
    case Numbering of
      znsVolume:
        spans := spans - [spNoVolumeName];
      znsExt:
        spans := spans + [spCompatName];
    end;
    Result := spans;
  end;
end;

function TZMWorkZip.Name(Expanded: Boolean = False): string;
begin
  if ReqFileName = '' then
    Result := inherited Name(Expanded)
  else
  begin
    Result := ReqFileName;
    if Alias <> '' then
    begin
      if Expanded then
        Result := Alias +'<' + Result + '>'
      else
        Result := Alias;
    end;
  end;
end;

procedure TZMWorkZip.NewFlushDisk;
begin
  // need to allow another disk, check size, open file, name disk etc
  raise EZipMaster.CreateMsgDisp(DS_WriteError, True);
end;

function TZMWorkZip.NewSegment: Boolean; // true to 'continue'
const
  __ERR_DS_Canceled = __UNIT__ + (926 shl 10) + DS_Canceled;
  __ERR_DS_Canceled1 = __UNIT__ + (928 shl 10) + DS_Canceled;
  __ERR_DS_Canceled2 = __UNIT__ + (950 shl 10) + DS_Canceled;
  __ERR_DS_Canceled3 = __UNIT__ + (966 shl 10) + DS_Canceled;
  __ERR_DS_Canceled4 = __UNIT__ + (1027 shl 10) + DS_Canceled;
  __ERR_DS_NoVolume = __UNIT__ + (1041 shl 10) + DS_NoVolume;
var
  DiskFile: String;
  DiskSeq: Integer;
  MsgQ: String;
  Res: Integer;
  SegName: String;
  OnGetNextDisk: TZMGetNextDiskEvent;
  OnStatusDisk: TZMStatusDiskEvent;
begin
  Result := false;
  // If we write on a fixed disk the filename must change.
  // We will get something like: FileNamexxx.zip where xxx is 001,002 etc.
  // if CompatNames are used we get FileName.zxx where xx is 01, 02 etc.. last .zip
  if Numbering = znsNone then
  begin
    if spCompatName in SpanOptions then
      Numbering := znsExt
    else if WorkDrive.DriveIsFixed or (spNoVolumeName in SpanOptions) then
      Numbering := znsName
    else
      Numbering := znsVolume;
  end;
  DiskFile := FileName;
  if Numbering <> znsVolume then
    DiskFile := CreateMVFileNameEx(DiskFile, false, Numbering = znsExt);
  CheckForDisk(True, spWipeFiles in SpanOptions);

  OnGetNextDisk := Master.OnGetNextDisk;
  // Allow clearing of removeable media even if no volume names
  if (not WorkDrive.DriveIsFixed) and (spWipeFiles in SpanOptions) and
    ((FZipDiskAction = zdaErase) or not Assigned(OnGetNextDisk)) then
  begin
    // Do we want a format first?
    if Numbering = znsVolume then
      SegName := VolName(DiskNr)
      // default name
    else
      SegName := SZipSet + IntToStr(succ(DiskNr));
    // Ok=6 NoFormat=-3, Cancel=-2, Error=-1
    case ZipFormat(SegName) of
      // Start formating and wait until BeforeClose...
      - 1:
        raise EZipMaster.CreateMsgDisp(__ERR_DS_Canceled, True);
      -2:
        raise EZipMaster.CreateMsgDisp(__ERR_DS_Canceled1, false);
    end;
  end;
  if WorkDrive.DriveIsFixed or (Numbering <> znsVolume) then
    DiskSeq := DiskNr + 1
  else
  begin
    DiskSeq := StrToIntDef(Copy(WorkDrive.DiskName, 9, 3), 1);
    if DiskSeq < 0 then
      DiskSeq := 1;
  end;
  FZipDiskStatus := [];
  Res := AskOverwriteSegment(DiskFile, DiskSeq);
  if (Res = idYes) and (WorkDrive.DriveIsFixed) and
    (spCompatName in SpanOptions) and _Z_FileExists(ReqFileName) then
  begin
    Res := AskOverwriteSegment(ReqFileName, DiskSeq);
    if (Res = idYes) then
      _Z_EraseFile(ReqFileName, HowToDelete = htdFinal);
  end;
  if (Res = 0) or (Res = idCancel) or ((Res = idNo) and WorkDrive.DriveIsFixed)
    then
    raise EZipMaster.CreateMsgDisp(__ERR_DS_Canceled2, false);

  if Res = idNo then
  begin // we will try again...
    FDiskWritten := 0;
    NewDisk := True;
    Result := True;
    exit;
  end;
  // Create the output file.
  if not File_Create(DiskFile) then
  begin // change proposed by Pedro Araujo
    MsgQ := ZipLoadStr(DS_NoOutFile);
    Res := ZipMessageDlgEx('', MsgQ, zmtError + DHC_SpanNoOut,
      [mbRetry, mbCancel]);
    if Res <> idRetry then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_Canceled3, false);
    FDiskWritten := 0;
    NewDisk := True;
    Result := True;
    exit;
  end;

  // Get the free space on this disk, correct later if neccessary.
  WorkDrive.VolumeRefresh;

  // Set the maximum number of bytes that can be written to this disk(file).
  // Reserve space on/in all the disk/file.
  if (DiskNr = 0) and (KeepFreeOnDisk1 > 0) or (KeepFreeOnAllDisks > 0) then
  begin
    if (KeepFreeOnDisk1 mod WorkDrive.VolumeSecSize) <> 0 then
      KeepFreeOnDisk1 := succ(KeepFreeOnDisk1 div WorkDrive.VolumeSecSize)
        * WorkDrive.VolumeSecSize;
    if (KeepFreeOnAllDisks mod WorkDrive.VolumeSecSize) <> 0 then
      KeepFreeOnAllDisks := succ
        (KeepFreeOnAllDisks div WorkDrive.VolumeSecSize)
        * WorkDrive.VolumeSecSize;
  end;
  AllowedSize := WorkDrive.VolumeSize - KeepFreeOnAllDisks;
  if (MaxVolumeSize > 0) and (MaxVolumeSize < AllowedSize) then
    AllowedSize := MaxVolumeSize;
  // Reserve space on/in the first disk(file).
  if DiskNr = 0 then
    AllowedSize := AllowedSize - KeepFreeOnDisk1;

  // Do we still have enough free space on this disk.
  if AllowedSize < MinFreeVolumeSize then // No, too bad...
  begin
    OnStatusDisk := Master.OnStatusDisk;
    File_Close;
    _Z_DeleteFile(DiskFile);
    if Assigned(OnStatusDisk) then // v1.60L
    begin
      if Numbering <> znsVolume then
        DiskSeq := DiskNr + 1
      else
      begin
        DiskSeq := StrToIntDef(Copy(WorkDrive.DiskName, 9, 3), 1);
        if DiskSeq < 0 then
          DiskSeq := 1;
      end;
      FZipDiskAction := zdaOk; // The default action
      FZipDiskStatus := [zdsNotEnoughSpace];
      OnStatusDisk(Master, DiskSeq, DiskFile, FZipDiskStatus,
        FZipDiskAction);
      if FZipDiskAction = zdaCancel then
        Res := idCancel
      else
        Res := idRetry;
    end
    else
    begin
      MsgQ := ZipLoadStr(DS_NoDiskSpace);
      Res := ZipMessageDlgEx('', MsgQ, zmtError + DHC_SpanSpace,
        [mbRetry, mbCancel]);
    end;
    if Res <> idRetry then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_Canceled4, false);
    FDiskWritten := 0;

    NewDisk := True;
    // If all this was on a HD then this wouldn't be useful but...
    Result := True;
  end
  else
  begin
    // ok. it fits and the file is open
    // Set the volume label of this disk if it is not a fixed one.
    if not(WorkDrive.DriveIsFixed or (Numbering <> znsVolume)) then
    begin
      if not WorkDrive.RenameDisk(VolName(DiskNr)) then
        raise EZipMaster.CreateMsgDisp(__ERR_DS_NoVolume, True);
    end;
//    // if it is a floppy buffer it
//    if (not WorkDrive.DriveIsFixed) and (AllowedSize <= MaxDiskBufferSize)// then
//      and not FFlushing then // 18/06/2012 9:22:56 AM
//    begin
//      SetLength(fDiskBuffer, AllowedSize);
//      fBufferPosition := 0;
//    end;
  end;
end;

function TZMWorkZip.OldVolName(Part: Integer): String;
begin
  Result := SPKBACK + ' ' + Copy(IntToStr(1001 + Part), 2, 3);
end;

function TZMWorkZip.Read(var Buffer; Len: Integer): Integer;
const
  __ERR_DS_ReadError = __UNIT__ + (1088 shl 10) + DS_ReadError;
  __ERR_DS_ReadError1 = __UNIT__ + (1108 shl 10) + DS_ReadError;
var
  bp: PAnsiChar;
  SizeR: Integer;
  ToRead: Integer;
begin
  try
    if IsMultiPart then
    begin
      ToRead := Len;
      if Len < 0 then
        ToRead := -Len;
      bp := @Buffer;
      Result := 0;
      while ToRead > 0 do
      begin
        SizeR := inherited Read(bp^, ToRead);
        if SizeR <> ToRead then
        begin
          // Check if we are at the end of a input disk.
          if SizeR < 0 then
          begin
            Result := SizeR;
            exit;
          end;
          // if  error or (len <0 and read some) or (end segment)
          if ((Len < 0) and (SizeR <> 0)) or not IsEndOfFile then
          begin
            Result := -__ERR_DS_ReadError;
            exit;
          end;
          // It seems we are at the end, so get a next disk.
          GetNewDisk(DiskNr + 1, false);
        end;
        if SizeR > 0 then
        begin
          Inc(bp, SizeR);
          ToRead := ToRead - SizeR;
          Result := Result + SizeR;
        end;
      end;
    end
    else
      Result := inherited Read(Buffer, Len);
  except
    on E: EZipMaster do
      Result := -E.ResId;
    on E: Exception do
      Result := -__ERR_DS_ReadError1;
  end;
end;

function TZMWorkZip.Reads(var Buffer; const Lens: array of Integer): Integer;
var
  i: Integer;
  pb: PAnsiChar;
  r: Integer;
begin
  Result := 0;
  if IsMultiPart then
  begin
    pb := @Buffer;
    for i := Low(Lens) to High(Lens) do
    begin
      r := Read(pb^, -Lens[i]);
      if r < 0 then
      begin
        Result := r;
        break;
      end;
      Result := Result + r;
      Inc(pb, r);
    end;
  end
  else
    Result := inherited Reads(Buffer, Lens);
end;

function TZMWorkZip.SaveFileInformation: Boolean;
begin
  Result := GetFileInformation(fSavedFileInfo);
end;

function TZMWorkZip.SeekDisk(Nr: Integer): Integer;
begin
  if DiskNr <> Nr then
    GetNewDisk(Nr, false);
  Result := Nr;
end;

procedure TZMWorkZip.SetFileName(const Value: String);
begin
  if FileName <> Value then
  begin
    inherited SetFileName(Value);
    WorkDrive.DriveStr := Value;
  end;
end;

procedure TZMWorkZip.SetFile_Size(const Value: Int64);
begin
  if FFile_Size <> Value then
  begin
    FFile_Size := Value;
  end;
end;

procedure TZMWorkZip.SetKeepFreeOnAllDisks(const Value: Cardinal);
begin
  fInternal.fKeepFreeOnAllDisks := Value;
end;

procedure TZMWorkZip.SetKeepFreeOnDisk1(const Value: Cardinal);
begin
  fInternal.fKeepFreeOnDisk1 := Value;
end;

procedure TZMWorkZip.SetMaxVolumeSize(const Value: Int64);
begin
  fInternal.fMaxVolumeSize := Value;
end;

procedure TZMWorkZip.SetPosition(const Value: Int64);
begin
  Seek(Value, 0);
end;

procedure TZMWorkZip.SetWorkDrive(const Value: TZMWorkDrive);
begin
  if fWorkDrive <> Value then
  begin
    fWorkDrive := Value;
  end;
end;

function TZMWorkZip.VerifyFileInformation(IgnoreWriteTime
  : Boolean = False): Boolean;
var
  info: _BY_HANDLE_FILE_INFORMATION;
begin
  GetFileInformation(info);
  if IgnoreWriteTime then
    Result := True
  else
    Result := (info.ftLastWriteTime.dwLowDateTime = fSavedFileInfo.
      ftLastWriteTime.dwLowDateTime) and
      (info.ftLastWriteTime.dwHighDateTime = fSavedFileInfo.ftLastWriteTime.
      dwHighDateTime);
  if Result then
    Result := (info.ftCreationTime.dwLowDateTime =
      fSavedFileInfo.ftCreationTime.dwLowDateTime) and
      (info.ftCreationTime.dwHighDateTime =
        fSavedFileInfo.ftCreationTime.dwHighDateTime) and
        (info.nFileSizeLow = fSavedFileInfo.nFileSizeLow) and
      (info.nFileSizeHigh = fSavedFileInfo.nFileSizeHigh) and
      (info.nFileIndexLow = fSavedFileInfo.nFileIndexLow) and
      (info.nFileIndexHigh = fSavedFileInfo.nFileIndexHigh) and
      (info.dwFileAttributes = fSavedFileInfo.dwFileAttributes) and
      (info.dwVolumeSerialNumber = fSavedFileInfo.dwVolumeSerialNumber);
end;

function TZMWorkZip.VolName(Part: Integer): String;
begin
  Result := SPKBACK + Copy(IntToStr(1001 + Part), 2, 3);
end;

function TZMWorkZip.WBuffer(size: Integer): pByte;
begin
  if size < 1 then
    WBuf := nil
  else if HIGH(WBuf) < size then
  begin
    size := size or $3FF;
    SetLength(WBuf, size + 1); // reallocate
  end;
  Result := @WBuf[0];
end;

function TZMWorkZip.Write(const Buffer; Len: Integer): Integer;
begin
  if IsMultiPart then
    Result := WriteSplit(Buffer, Len)
  else
    Result := inherited Write(Buffer, Len);
end;

function TZMWorkZip.Writes(const Buffer; const Lens: array of Integer): Integer;
var
  c: Integer;
  i: Integer;
begin
  if IsMultiPart then
  begin
    c := 0;
    for i := Low(Lens) to High(Lens) do
      c := c + Lens[i];
    Result := WriteSplit(Buffer, -c);
  end
  else
    Result := inherited Writes(Buffer, Lens);
end;

function TZMWorkZip.WriteSplit(const Buffer; ToWrite: Integer): Integer;
const
  __ERR_DS_NoWrite = __UNIT__ + (1332 shl 10) + DS_NoWrite;
  __ERR_DS_UnknownError = __UNIT__ + (1359 shl 10) + DS_UnknownError;
var
  Buf: PAnsiChar;
  Len: Cardinal;
  MaxLen: Cardinal;
  MinSize: Cardinal;
  MustFit: Boolean;
  Res: Integer;
begin { WriteSplit }
  try
    Result := 0;
    MustFit := false;
    if ToWrite >= 0 then
    begin
      Len := ToWrite;
      MinSize := 0;
    end
    else
    begin
      Len := -ToWrite;
      MustFit := (Len and MustFitFlag) <> 0;
      Len := Len and MustFitMask;
      MinSize := Len;
    end;
    Buf := @Buffer;
    KeepAlive;
    CheckCancel;

    // Keep writing until error or Buffer is empty.
    while True do
    begin
      // Check if we have an output file already opened, if not: create one,
      // do checks, gather info.
      if (not IsOpen) then
      begin
        NewDisk := DiskNr <> 0; // allow first disk in drive
        if NewSegment then
        begin
          NewDisk := True;
          continue;
        end;
      end;

      // Check if we have at least MinSize available on this disk,
      // headers are not allowed to cross disk boundaries. ( if zero than don't care.)
      if (MinSize <> 0) and (MinSize > AllowedSize) then
      begin // close this part
        // all parts must be same stamp
        if StampDate = 0 then
          StampDate := LastWritten;
        File_Close;
        FDiskWritten := 0;
        NewDisk := True;
        DiskNr := DiskNr + 1; // RCV270299
        if not MustFit then
          continue;
        Result := MustFitError;
        break;
      end;

      // Don't try to write more bytes than allowed on this disk.
      MaxLen := HIGH(Integer);
      if AllowedSize < MaxLen then
        MaxLen := Integer(AllowedSize);
      if Len < MaxLen then
        MaxLen := Len;
//      if (fDiskBuffer <> nil) and not FFlushing then // 18/06/2012 9:22:32 AM
//      begin
//        Move(Buf^, fDiskBuffer[fBufferPosition], MaxLen);
//        Res := MaxLen;
//        Inc(fBufferPosition, MaxLen);
//      end
//      else
        Res := inherited Write(Buf^, MaxLen);
      if Res < 0 then
        raise EZipMaster.CreateMsgDisp(__ERR_DS_NoWrite, True);
      // A write error (disk removed?)

      Inc(FDiskWritten, Res);
      Inc(Result, Res);
      AllowedSize := AllowedSize - MaxLen;
      if MaxLen = Len then
        break;

      // We still have some data left, we need a new disk.
      if StampDate = 0 then
        StampDate := LastWritten;
      File_Close;
      AllowedSize := 0;
      FDiskWritten := 0;
      DiskNr := DiskNr + 1;
      NewDisk := True;
      Inc(Buf, MaxLen);
      Dec(Len, MaxLen);
    end; { while(True) }
  except
    on E: EZipMaster do
    begin
      Result := -E.ResId;
    end;
    on E: Exception do
    begin
      Result := -__ERR_DS_UnknownError;
    end;
  end;
end;

function TZMWorkZip.ZipFormat(const NewName: String): Integer;
var
  msg: String;
  Res: Integer;
  Vol: String;
begin
  if NewName <> '' then
    Vol := NewName
  else
    Vol := WorkDrive.DiskName;
  if Length(Vol) > 11 then
    Vol := Copy(Vol, 1, 11);
  Result := -3;
  if WorkDrive.DriveIsFloppy then
  begin
    if (spTryFormat in SpanOptions) then
      Result := FormatFloppy(Application.Handle, WorkDrive.DriveStr);
    if Result = -3 then
    begin
      if ConfirmErase then
      begin
        msg := ZipFmtLoadStr(FM_Erase, [WorkDrive.DriveStr]);
        Res := ZipMessageDlgEx(ZipLoadStr(FM_Confirm), msg,
          zmtWarning + DHC_FormErase, [mbYes, mbNo]);
        if Res <> idYes then
        begin
          Result := -3; // no  was -2; // cancel
          exit;
        end;
      end;
      ClearFloppy(WorkDrive.DriveStr);
      Result := 0;
    end;
    WorkDrive.HasMedia(false);
    if (Result = 0) and (Numbering = znsVolume) then
      WorkDrive.RenameDisk(Vol);
  end;
end;

end.
