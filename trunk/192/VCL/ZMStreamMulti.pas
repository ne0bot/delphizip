Unit ZMStreamMulti;

//  ZMStreamMulti.pas - basic in/out for multi-part zip files

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
//modified 2012-09-01

{$I   '.\ZipVers.inc'}

interface

uses
  {$ifdef VERDXE2up}
    System.Classes, WinApi.Windows, System.SysUtils, VCL.Controls, VCL.Forms,
    VCL.Dialogs, VCL.Graphics,
  {$ELSE}
    Classes, Windows, SysUtils, Controls, Forms, Dialogs, Graphics,
  {$ENDIF}
  ZipMstr, ZMBody, ZMStream, ZMZipBase, ZMZipMulti, ZMDrv;

const
  FM_CreateMulti = $FFEE;
  MustFitError = -10999;

type
  TZMStreamMulti = class(TZMStream)
  private
    FAllowedSize: Int64;
    FDiskWritten: Cardinal;
    FFile_Size: Int64;
    FHowToDelete: TZMDeleteOpts;
    FLastWrite: TFileTime;
    FMaster: TCustomZipMaster;
    FMultiOwner: TZMZipMulti;
    FReqFileName: String;
    FSig: TZipFileSigs;
    FSpan: TZMSpanParameters;
    FUnattended: boolean;
    FWorkDrive: TZMWorkDrive;
    FZipDiskAction: TZMDiskAction;
    FZipDiskStatus: TZMZipDiskStatus;
    function AskOverwriteSegment(const DiskFile: String; DiskSeq: Integer): Integer;
    procedure ClearFloppy(const dir: String);
    function GetAnswerAll: boolean;
    function GetArchiveName: string;
    function GetDiskNr: Integer;
    function GetIsMultiPart: Boolean;
    function GetNewDisk: Boolean;
    function GetNumbering: TZipNumberScheme;
    function GetTotalDisks: Integer;
    function NewSegment: Boolean;
    procedure SetAnswerAll(const Value: boolean);
    procedure SetArchiveName(const Value: string);
    procedure SetDiskNr(const Value: Integer);
    procedure SetIsMultiPart(const Value: Boolean);
    procedure SetNewDisk(const Value: Boolean);
    procedure SetNumbering(const Value: TZipNumberScheme);
    procedure SetTotalDisks(const Value: Integer);
    function WriteSplit(const Buffer; ToWrite: Integer; Contiguous: boolean):
        Integer;
  private
    procedure CheckCancel;
    function GetCancel: Integer;
    function KeepAlive: boolean;
    procedure SetCancel(const Value: Integer);
    function VolName(Part: Integer): String;
    function ZipFmtLoadStr(id: Integer; const Args: array of const): TZMString;
    function ZipFormat(const NewName: String): Integer;
    function ZipLoadStr(id: Integer): TZMString;
    function ZipMessageDlgEx(const title, msg: string; context: Integer;
      btns: TMsgDlgButtons): TModalResult;
    property AnswerAll: boolean read GetAnswerAll write SetAnswerAll;
    property Cancel: Integer read GetCancel write SetCancel;
    property HowToDelete: TZMDeleteOpts read FHowToDelete;
    property Master: TCustomZipMaster read FMaster;
    property MultiOwner: TZMZipMulti read FMultiOwner;
    property Span: TZMSpanParameters read FSpan;// write SetSpan;
    property Unattended: boolean read FUnattended;
  protected
    function GetMyType: Integer; override;
    procedure PrepareFile; override;
    procedure SetPosition(const Value: Int64); override;
  public
    constructor Create(Core: TZMZipBase; const FileName: string; Mode: Word);
    procedure AfterConstruction; override;
    function AskAnotherDisk(const DiskFile: String): Integer;
    procedure AssignFrom(Src: TZMStream); override;
    function CreateMVFileNameEx(const FileName: String;
      StripPartNbr, Compat: Boolean): String;
    function Read(var Buffer; Len: Integer): Integer; override;
    function Write(const Buffer; Len: Integer): Integer; override;
    function WriteContiguous(const Buffer; Len: Integer): Integer;
    property ArchiveName: string read GetArchiveName write SetArchiveName;
    property DiskNr: Integer read GetDiskNr write SetDiskNr;
    property IsMultiPart: Boolean read GetIsMultiPart write SetIsMultiPart;
    property NewDisk: Boolean read GetNewDisk write SetNewDisk;
    property Numbering: TZipNumberScheme read GetNumbering write SetNumbering;
    property ReqFileName: String read FReqFileName write FReqFileName;
    property TotalDisks: Integer read GetTotalDisks write SetTotalDisks;
    property WorkDrive: TZMWorkDrive read FWorkDrive;
  end;

implementation

uses
  ZMCtx, {$IFNDEF UNICODE}ZMCompat,{$ENDIF} ZMDlg,
  ZMStructs, ZMUtils, ZMMsg, ZMXcpt, ZMWFuncs;

const
  __UNIT__ = 21;

const
  MAX_PARTS = 999;
  MaxDiskBufferSize = (4 * 1024 * 1024); // floppies only

const
  SZipSet = 'ZipSet_';
  SPKBACK = 'PKBACK#';

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

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
    if @SHFormatDrive <> nil then
      try
        Result := SHFormatDrive(WND, drv, SHFMT_ID_DEFAULT, SHFMT_OPT_FULL);
      finally
        FreeLibrary(hLib);
      end;
    SetErrorMode(OldErrMode);
  end;
end;

// only called to create single
constructor TZMStreamMulti.Create(Core: TZMZipBase; const FileName: string;
    Mode: Word);
begin
  inherited;
end;

procedure TZMStreamMulti.AfterConstruction;
begin
  inherited;
//  FNumbering := znsNone;
  FMultiOwner := Owner as TZMZipMulti;
  FWorkDrive := MultiOwner.WorkDrive;
  FSpan := MultiOwner.Span;
  FMaster := MultiOwner.Master;
end;

function TZMStreamMulti.AskAnotherDisk(const DiskFile: String): Integer;
begin
  Result := MultiOwner.AskAnotherDisk(DiskFile);
end;

function TZMStreamMulti.AskOverwriteSegment(const DiskFile: String; DiskSeq:
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
      MsgQ := ZipFmtLoadStr(ZS_AskPrevFile, [DiskSeq]);
      FZipDiskStatus := FZipDiskStatus + [zdsPreviousDisk];
    end
    else
    begin
      MsgQ := ZipFmtLoadStr(ZS_AskDeleteFile, [DiskFile]);
      FZipDiskStatus := FZipDiskStatus + [zdsSameFileName];
    end
    else if not WorkDrive.DriveIsFixed then
      if (WorkDrive.VolumeSize <> WorkDrive.VolumeSpace) then
        FZipDiskStatus := FZipDiskStatus + [zdsHasFiles]
        // But not the same name
      else
        FZipDiskStatus := FZipDiskStatus + [zdsEmpty];
  tmpStatusDisk := Master.OnStatusDisk;
  if Assigned(tmpStatusDisk) and not AnswerAll then
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
          AnswerAll := True;
        end;
      zdaOk:
        Result := idOk;
    else
      Result := idOk;
    end;
  end
  else if ((FZipDiskStatus * [zdsPreviousDisk, zdsSameFileName]) <> []) and not
    (AnswerAll or Unattended) then
  begin
    Result := ZipMessageDlgEx(ZipLoadStr(ZS_Confirm), MsgQ,
      zmtWarning + DHC_SpanOvr, [mbYes, mbNo, mbCancel, mbYesToAll]);
    if Result = mrYesToAll then
    begin
      AnswerAll := True;
      Result := idOk;
    end;
  end
  else
    Result := idOk;
end;

// Src should not be open but not enforced
procedure TZMStreamMulti.AssignFrom(Src: TZMStream);
var
  zsrc: TZMStreamMulti;
begin
  if (Src <> Self) and (Src <> nil) then
  begin
    inherited;
    if Src is TZMStreamMulti then
    begin
      zsrc := TZMStreamMulti(Src);
      FAllowedSize := zsrc.FAllowedSize;
      FFile_Size := zsrc.FFile_Size;
      FLastWrite := zsrc.FLastWrite;
//      FNumbering := zsrc.FNumbering;
      FReqFileName := zsrc.FReqFileName;
      FSig := zsrc.FSig;
      FWorkDrive.AssignFrom(zsrc.WorkDrive);
      FZipDiskAction := zsrc.FZipDiskAction;
      FZipDiskStatus := zsrc.FZipDiskStatus;
    end;
  end;
end;

procedure TZMStreamMulti.CheckCancel;
begin
  KeepAlive;
  if Cancel <> 0 then
    raise EZipMaster.CreateMsgDisp(Cancel, True);
end;

procedure TZMStreamMulti.ClearFloppy(const dir: String);
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
//        if Verbosity >= zvTrace then
          Body.Reporter.ReportMsg(ZS_Deleting{TM_Erasing}, [Fname]);
//        else
//          KeepAlive;
        // allow time for OS to delete last file
        _Z_RemoveDir(Fname);
      end
      else
      begin
//        if Verbosity >= zvTrace then
          Body.Reporter.ReportMsg(ZS_Deleting, [Fname]);
//        else
//          KeepAlive;
        File_Delete(FName);//_Z_DeleteFile(Fname);
      end;
    until _Z_FindNext(SRec) <> 0;
    _Z_FindClose(SRec);
end;

function TZMStreamMulti.CreateMVFileNameEx(const FileName: String;
  StripPartNbr, Compat: Boolean): String;
begin // changes FileName into multi volume FileName
  Result := MultiOwner.CreateMVFileNameEx(FileName, StripPartNbr, Compat);
end;

function TZMStreamMulti.GetAnswerAll: boolean;
begin
  Result := Body.AnswerAll;
end;

function TZMStreamMulti.GetArchiveName: string;
begin
  Result := MultiOwner.ArchiveName;
end;

function TZMStreamMulti.GetCancel: Integer;
begin
  Result := Body.Cancel;
end;

function TZMStreamMulti.GetDiskNr: Integer;
begin
  Result := MultiOwner.DiskNr;
end;

function TZMStreamMulti.GetIsMultiPart: Boolean;
begin
  Result := MultiOwner.IsMultiPart;
end;

function TZMStreamMulti.GetMyType: Integer;
begin
  Result := ZM_MULTI_STREAM;
end;

function TZMStreamMulti.GetNewDisk: Boolean;
begin
  Result := MultiOwner.NewDisk;
end;

function TZMStreamMulti.GetNumbering: TZipNumberScheme;
begin
  Result := MultiOwner.Numbering;
end;

function TZMStreamMulti.GetTotalDisks: Integer;
begin
  Result := MultiOwner.TotalDisks;
end;

function TZMStreamMulti.KeepAlive: boolean;
begin
  Result := Body.KeepAlive;
end;

function TZMStreamMulti.NewSegment: Boolean;
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
    if spCompatName in Span.Options then
      Numbering := znsExt
    else if WorkDrive.DriveIsFixed or (spNoVolumeName in Span.Options) then
      Numbering := znsName
    else
      Numbering := znsVolume;
  end;
  DiskFile := ArchiveName;
  if Numbering <> znsVolume then
    DiskFile := CreateMVFileNameEx(DiskFile, false, Numbering = znsExt);
  MultiOwner.CheckForDisk(True, spWipeFiles in Span.Options);

  OnGetNextDisk := Master.OnGetNextDisk;
  // Allow clearing of removeable media even if no volume names
  if (not WorkDrive.DriveIsFixed) and (spWipeFiles in Span.Options) and
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
        raise EZipMaster.CreateMsgDisp(ZM_Error(455, ZS_Canceled), True);
      -2:
        raise EZipMaster.CreateMsgDisp(ZM_Error(457, ZS_Canceled), false);
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
    (spCompatName in Span.Options) and _Z_FileExists(ReqFileName) then
  begin
    Res := AskOverwriteSegment(ReqFileName, DiskSeq);
    if (Res = idYes) then
      _Z_EraseFile(ReqFileName, HowToDelete = htdFinal);
  end;
  if (Res = 0) or (Res = idCancel) or ((Res = idNo) and WorkDrive.DriveIsFixed)
    then
    raise EZipMaster.CreateMsgDisp(ZM_Error(479, ZS_Canceled), false);

  if Res = idNo then
  begin // we will try again...
    FDiskWritten := 0;
    NewDisk := True;
    Result := True;
    exit;
  end;
  // Create the output file.
  File_Create(DiskFile);
  if Handle < 0 then
  begin // change proposed by Pedro Araujo
    MsgQ := ZipLoadStr(ZS_NoOutFile);
    Res := ZipMessageDlgEx('', MsgQ, zmtError + DHC_SpanNoOut,
      [mbRetry, mbCancel]);
    if Res <> idRetry then
      raise EZipMaster.CreateMsgDisp(ZM_Error(496, ZS_Canceled), false);
    FDiskWritten := 0;
    NewDisk := True;
    Result := True;
    exit;
  end;

  // Get the free space on this disk, correct later if neccessary.
  WorkDrive.VolumeRefresh;

  // Set the maximum number of bytes that can be written to this disk(file).
  // Reserve space on/in all the disk/file.
  if (DiskNr = 0) and (Span.KeepFreeOnDisk1 > 0) or (Span.KeepFreeOnAllDisks > 0) then
  begin
    if (Span.KeepFreeOnDisk1 mod WorkDrive.VolumeSecSize) <> 0 then
      Span.KeepFreeOnDisk1 := succ(Span.KeepFreeOnDisk1 div WorkDrive.VolumeSecSize)
        * WorkDrive.VolumeSecSize;
    if (Span.KeepFreeOnAllDisks mod WorkDrive.VolumeSecSize) <> 0 then
      Span.KeepFreeOnAllDisks := succ
        (Span.KeepFreeOnAllDisks div WorkDrive.VolumeSecSize)
        * WorkDrive.VolumeSecSize;
  end;
  FAllowedSize := WorkDrive.VolumeSpace - Span.KeepFreeOnAllDisks;
  if (Span.MaxVolumeSize > 0) and (Span.MaxVolumeSize < FAllowedSize) then
    FAllowedSize := Span.MaxVolumeSize;
  // Reserve space on/in the first disk(file).
  if DiskNr = 0 then
    FAllowedSize := FAllowedSize - Span.KeepFreeOnDisk1;

  // Do we still have enough free space on this disk.
  if FAllowedSize < Span.MinFreeVolumeSize then // No, too bad...
  begin
    OnStatusDisk := Master.OnStatusDisk;
    File_Close;
    File_Delete(DiskFile);//_Z_DeleteFile(DiskFile);
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
      MsgQ := ZipLoadStr(ZS_NoDiskSpace);
      Res := ZipMessageDlgEx('', MsgQ, zmtError + DHC_SpanSpace,
        [mbRetry, mbCancel]);
    end;
    if Res <> idRetry then
      raise EZipMaster.CreateMsgDisp(ZM_Error(557, ZS_Canceled), false);
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
        raise EZipMaster.CreateMsgDisp(ZM_Error(571, ZS_NoVolume), True);
    end;
  end;
end;

procedure TZMStreamMulti.PrepareFile;
begin
  if OpenMode <> FM_CreateMulti then
    inherited;
end;

function TZMStreamMulti.Read(var Buffer; Len: Integer): Integer;
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
            Result := -ZM_Error(610, ZS_ReadError);
            exit;
          end;
          // It seems we are at the end, so get a next disk.
          MultiOwner.SeekDisk(DiskNr + 1, false);
//          GetNewDisk(DiskNr + 1, false);
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
      Result := -E.ExtErr;
    on E: EZMAbort do
      raise;
    on E: Exception do
      Result := -ZM_Error(633, ZS_ReadError);
  end;
end;

//procedure TZMStreamMulti.ReportMsg(id: Integer; const Args: array of const);
//begin
//  Body.ReportMsg(id, Args);
//end;

procedure TZMStreamMulti.SetAnswerAll(const Value: boolean);
begin
  Body.AnswerAll := Value;
end;

procedure TZMStreamMulti.SetArchiveName(const Value: string);
begin
  MultiOwner.ArchiveName := Value;
end;

procedure TZMStreamMulti.SetCancel(const Value: Integer);
begin
  Body.Cancel := Value;
end;

procedure TZMStreamMulti.SetDiskNr(const Value: Integer);
begin
  MultiOwner.DiskNr := Value;
end;

procedure TZMStreamMulti.SetIsMultiPart(const Value: Boolean);
begin
  MultiOwner.IsMultiPart := Value;
end;

procedure TZMStreamMulti.SetNewDisk(const Value: Boolean);
begin
  MultiOwner.NewDisk := Value;
end;

procedure TZMStreamMulti.SetNumbering(const Value: TZipNumberScheme);
begin
  MultiOwner.Numbering := Value;
end;

procedure TZMStreamMulti.SetPosition(const Value: Int64);
begin
  Seek(Value, 0);
end;

procedure TZMStreamMulti.SetTotalDisks(const Value: Integer);
begin
  MultiOwner.TotalDisks := Value;
end;

//procedure TZMStreamMulti.ShowZipMessage(Ident: Integer; const UserStr: string);
//begin
//  Body.Reporter.ShowZipMessage(Ident, UserStr);
//end;

function TZMStreamMulti.VolName(Part: Integer): String;
begin
  Result := SPKBACK + Copy(IntToStr(1001 + Part), 2, 3);
end;

function TZMStreamMulti.Write(const Buffer; Len: Integer): Integer;
begin
  if IsMultiPart then
    Result := WriteSplit(Buffer, Len, false)
  else
    Result := inherited Write(Buffer, Len);
end;

function TZMStreamMulti.WriteContiguous(const Buffer; Len: Integer): Integer;
begin
  if IsMultiPart then
    Result := WriteSplit(Buffer, Len, True)
  else
    Result := inherited Write(Buffer, Len);
end;

function TZMStreamMulti.WriteSplit(const Buffer; ToWrite: Integer; Contiguous:
    boolean): Integer;
var
  Buf: PAnsiChar;
  Len: Cardinal;
  MaxLen: Cardinal;
  MinSize: Cardinal;
  MustFit: Boolean;
  Res: Integer;
begin { WriteSplit }
  Result := 0;
  if ToWrite = 0 then
    Exit;
  Assert(ToWrite > 0, 'ToWrite < 0');
  try
    Len := ToWrite;
    MustFit := Contiguous;
    if MustFit then
      MinSize := Len
    else
      MinSize := 0;
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
      if (MinSize <> 0) and (MinSize > FAllowedSize) then
      begin // close this part
        // all parts must be same stamp
        if StampDate = 0 then
          StampDate := LastWritten;
        FinaliseWrite;
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
      if FAllowedSize < MaxLen then
        MaxLen := Integer(FAllowedSize);
      if Len < MaxLen then
        MaxLen := Len;
      Res := inherited Write(Buf^, MaxLen);
      if Res < 0 then
        raise EZipMaster.CreateMsgDisp(ZM_Error(779, ZS_NoWrite), True);
      // A write error (disk removed?)

      Inc(FDiskWritten, Res);
      Inc(Result, Res);
      FAllowedSize := FAllowedSize - MaxLen;
      if MaxLen = Len then
        break;

      // We still have some data left, we need a new disk.
      if StampDate = 0 then
        StampDate := LastWritten;
      FinaliseWrite;
      File_Close;
      FAllowedSize := 0;
      FDiskWritten := 0;
      DiskNr := DiskNr + 1;
      NewDisk := True;
      Inc(Buf, MaxLen);
      Dec(Len, MaxLen);
    end; { while(True) }
  except
    on E: EZipMaster do
      Result := -E.ExtErr;
    on E: EZMAbort do
      raise;
    on E: Exception do
      Result := -ZM_Error(806, ZS_UnknownError);
  end;
end;

function TZMStreamMulti.ZipFmtLoadStr(id: Integer; const Args: array of const)
  : TZMString;
begin
  Result := Body.ZipFmtLoadStr(id, Args);
end;

function TZMStreamMulti.ZipFormat(const NewName: String): Integer;
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
    if (spTryFormat in Span.Options) then
      Result := FormatFloppy(Application.Handle, WorkDrive.DriveStr);
    if Result = -3 then
    begin
      if Body.ConfirmErase then
      begin
        msg := ZipFmtLoadStr(ZS_Erase, [WorkDrive.DriveStr]);
        Res := ZipMessageDlgEx(ZipLoadStr(ZS_Confirm), msg,
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

function TZMStreamMulti.ZipLoadStr(id: Integer): TZMString;
begin
  Result := Body.ZipLoadStr(id);
end;

function TZMStreamMulti.ZipMessageDlgEx(const title, msg: string; context: Integer;
  btns: TMsgDlgButtons): TModalResult;
begin
  Result := Body.ZipMessageDlgEx(title, msg, context, btns);
end;


end.

