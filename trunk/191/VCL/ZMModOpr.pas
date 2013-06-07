unit ZMModOpr;

//  ZMModOpr.pas - Operations modifying existing entries

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

interface

uses
  SysUtils, Windows, Classes, Graphics,
  ZipMstr, ZMCompat, ZMCore, ZMZipFile, ZMLister, ZMFileOpr;

type
  TSFXOps = (sfoNew, sfoZip, sfoExe);

type
  TZMModOpr = class(TZMFileOpr)
  private
    function GetZipComment: AnsiString;
    function Prepare(MustExist: Boolean; SafePart: boolean = false): TZMZipFile;
    function RecreateSingle(Intermed, theZip: TZMZipFile): Integer;
    procedure SetZipComment(const Value: AnsiString);
  protected
    procedure Finished(WasGood: boolean); override;
    function Recreate(Intermed, theZip: TZMZipFile): Integer;
    //1 Rewrite via an intermediate
    function Remake(CurZip: TZMZipFile; ReqCnt: Integer; All: boolean): Integer;
    procedure Started; override;
    procedure VerifySource(SrcZip: TZMZipFile);
    property ZipComment: AnsiString read GetZipComment write SetZipComment;
  public
    constructor Create(theComponent: TCustomZipMaster);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ChangeFileDetails(func: TZMChangeFunction; var data): Integer;
    procedure Rename(RenameList: TList; NewDateTime: Integer; How: TZMRenameOpts =
        htrDefault);
    procedure Set_ZipComment(const zComment: AnsiString);
  end;


implementation

uses
  Dialogs, ZMStructs, ZMDelZip, ZMXcpt, ZMUtils, ZMDlg, ZMCtx,
  ZMMsgStr, ZMMsg, ZMWorkFile, ZMWZip, ZMDrv, ZMMatch, ZMIRec,
  ZMEOC, ZMWFuncs, ZMCentral;

const
  __UNIT__ = 22 shl 23;

const
  BufSize = 10240;
  //8192;   // Keep under 12K to avoid Winsock problems on Win95.
  // If chunks are too large, the Winsock stack can
  // lose bytes being sent or received.

type
  pRenData = ^TRenData;

  TRenData = record
    Owner: TZMCore;
    RenList: TList;
    DTime:   Integer;
    How:  TZMRenameOpts;
    cnt:     Integer;
  end;

// 'ForEach' function to rename files
function RenFunc(rec: TZMDirRec; var data): Integer;
var
  ChangeName: boolean;
  FileName: String;
  How:  TZMRenameOpts;
  i: Integer;
  k: Integer;
  ncomment: String;
  newname: String;
  newStamp: integer;
  pData: pRenData;
  pRenRec: PZMRenameRec;
  RenSource: TZMString;
begin
  filename := rec.FileName;
  pData := @data;
  How := pData.How;
  Result := 0;
  for i := 0 to pData^.RenList.Count - 1 do
  begin
    pRenRec := PZMRenameRec(pData^.RenList[i]);
    RenSource := pRenRec.Source;
    newname := pRenRec.Dest;
    ncomment := pRenRec.Comment;
    newStamp := pRenRec.DateTime;
    ChangeName := (newname <> SPEC_SEP) and (CompareStr(filename, newname) <> 0);
    if How = htrFull then
    begin
      if FileNameMatch(pRenRec.Source, FileName) then
        k := -1
      else
        k := 0;
    end
    else
    begin
      k := Pos(UpperCase(RenSource), UpperCase(FileName));
    end;
    if k <> 0 then
    begin
      inc(pData^.cnt);   // I am selected
      if not ChangeName then
        Result := 0
      else
      begin
        if k > 0 then
        begin
          newname := FileName;
          System.Delete(newname, k, Length(RenSource));
          Insert(pRenRec.Dest, newname, k);
        end;
        Result := rec.ChangeName(newname);
        if Result = 0 then
          filename := rec.FileName;
      end;
      if Result = 0 then
      begin
        if ncomment <> '' then
        begin
          if ncomment[1] = #0 then
            ncomment := '';
          Result := rec.ChangeComment(ncomment);
        end;
      end;
      if Result = 0 then
      begin
        if newStamp = 0 then
          newStamp := pData^.DTime;
        if newStamp <> 0 then
          Result := rec.ChangeDate(newStamp);
      end;
      if How <> htrDefault then
        break;
    end;
  end;
end;

constructor TZMModOpr.Create(theComponent: TCustomZipMaster);
begin
  inherited;
end;

procedure TZMModOpr.AfterConstruction;
begin
  inherited;
  fIsDestructing := False;
end;

(*? TZMModOpr.BeforeDestruction
1.73 3 July 2003 RP stop callbacks
*)
procedure TZMModOpr.BeforeDestruction;
begin
  fIsDestructing := True;                   // stop callbacks
  inherited;
end;

(* TZMModOpr.ChangeFileDetails
  Add zipped files from source ZipMaster selected from source FSpecArgs
  When finished
    FSpecArgs will contain source files copied
    FSpecArgsExcl will contain source files skipped  (data = error code)
*)
function TZMModOpr.ChangeFileDetails(func: TZMChangeFunction; var data):
    Integer;
const
  __ERR_GE_InvalidArguments = __UNIT__ + (233 shl 10) + GE_InvalidArguments;
  __ERR_GE_NoSkipping = __UNIT__ + (246 shl 10) + GE_NoSkipping;
  __ERR_AZ_NothingToDo = __UNIT__ + (254 shl 10) + AZ_NothingToDo;
  __ERR_GE_NoSkipping1 = __UNIT__ + (279 shl 10) + GE_NoSkipping;
  __ERR_GE_ExceptErr = __UNIT__ + (301 shl 10) + GE_ExceptErr;
var
  Changes: Integer;
  CurZip: TZMZipFile;
  idx: Integer;
  rec: TZMZRec;
  SelCnt: Integer;
  SkipCnt: Integer;
  SkippedFiles: TStringList;
begin
  Result := 0;
//  if (IncludeSpecs.Count < 1) or (@func = nil) then
  if (IncludeSpecs.Count < 1) or not assigned(func) then
    raise EZipMaster.CreateMsgDisp(__ERR_GE_InvalidArguments, true);
  SkippedFiles := TStringList.Create;
  try
    if Verbosity >= zvVerbose then
      Diag('StartUp ChangeFileDetails');
    CurZip := Prepare(true);  // prepare the current zip
    SelCnt := CurZip.SelectFiles(IncludeSpecs, ExcludeSpecs, SkippedFiles);
    IncludeSpecs.Clear; // will contain files processed
    ExcludeSpecs.Clear; // will contain source files skipped
    SkipCnt := SkippedFiles.Count;
    for idx := 0 to SkippedFiles.Count - 1 do
    begin
      if ReportSkippingEx(SkippedFiles[idx], stNotFound, 0, ExcludeSpecs) then
        Result := -__ERR_GE_NoSkipping
      else
        Dec(SkipCnt);  // user chose to ignore
    end;
    if (Result = 0) and ((SelCnt <= 0) or (SkipCnt <> 0)) then
    begin
      if Verbosity >= zvVerbose then
        Diag('nothing selected');
      ShowZipMessage(__ERR_AZ_NothingToDo, '');
      Result := -__ERR_AZ_NothingToDo;
    end;
  finally
    SkippedFiles.Free;
  end;
  // process selected files
  Changes := 0;
  idx := -1;  // from beginning
  try
    while Result = 0 do
    begin
      idx := CurZip.NextSelected(idx);
      if idx < 0 then
        break; // no more - finished
      rec := CurZip[idx];
      if Verbosity >= zvVerbose then
        Diag('Changing: ' + rec.FileName);
      Result := func(rec, data);
      if Result <> 0 then
      begin
        if Verbosity >= zvVerbose then
          Diag(Format('error [%d] for: %s',[Result, rec.FileName]));

        if ReportSkippingEx(rec.FileName, stCannotDo, Result, ExcludeSpecs) then
          Result := -__ERR_GE_NoSkipping1
        else
          Result := 0;   // ignore error
      end;
      if Result = 0 then
      begin
        IncludeSpecs.Add(rec.FileName);
        if rec.HasChanges then
        begin
          if Verbosity >= zvVerbose then
            Diag('Changed: ' + rec.FileName);
          inc(Changes);
        end;
        CheckCancel;
      end;
    end;
  except
    on E: EZipMaster do
    begin
      Result := -E.ResId;
    end;
    on E: Exception do
      Result := -__ERR_GE_ExceptErr;
  end;
  if (Result = 0) and (Changes > 0) then
  begin
    if Verbosity >= zvVerbose then
      Diag('saving changes');
    Remake(CurZip, -1, True);
    SuccessCnt := Changes;
    Lister.Current := nil;
    // Update the Zip Directory by calling List method
    // for spanned exe avoid swapping to last disk
    Lister.Reload := zlrReload;
  end;
  if Verbosity >= zvVerbose then
    Diag('finished ChangeFileDetails');
end;

procedure TZMModOpr.Finished(WasGood: boolean);
begin
  inherited;
end;

function TZMModOpr.GetZipComment: AnsiString;
begin
    Result := fInternal.fZipComment
end;

(* TZMModOpr.Prepare
  Prepare destination and get SFX stub as needed
*)
function TZMModOpr.Prepare(MustExist: Boolean; SafePart: boolean = false):
    TZMZipFile;
const
  __ERR_DS_NoUnattSpan = __UNIT__ + (338 shl 10) + DS_NoUnattSpan;
begin
  Result := Lister.CurrentZip(MustExist, SafePart);
  if Master.Unattended and not Result.WorkDrive.DriveIsFixed then
    raise EZipMaster.CreateMsgDisp(__ERR_DS_NoUnattSpan, true);
  if (Uppercase(ExtractFileExt(Result.ReqFileName)) = EXT_EXE) then
  begin
    Result.UseSFX := true;
    Result.Stub := NewSFXStub;
    Result.UseSFX := true;
  end;
end;

(* ? TZMModOpr.Recreate
recreate the 'theZip' file from the intermediate result
to make as SFX
- theZip.UseSFX is set
- theZip.Stub must hold the stub to use
*)
function TZMModOpr.Recreate(Intermed, theZip: TZMZipFile): Integer;
const
  __ERR_GE_Unknown = __UNIT__ + (377 shl 10) + GE_Unknown;
  __ERR_CF_SFXCopyError = __UNIT__ + (383 shl 10) + CF_SFXCopyError;
var
  czip: TZMZipFile;
  DestZip: TZMZipCopy;
  detchSFX: Boolean;
  detchsz: Integer;
  existed: Boolean;
  OrigKeepFreeDisk1: Cardinal;
  r: Integer;
  tmp: String;
  wantNewDisk: Boolean;
begin
  detchsz := 0;
  detchSFX := false;
  existed := (zfi_Loaded and theZip.info) <> 0;
  if theZip.MultiDisk or ((not existed) and (zwoDiskSpan in theZip.WriteOptions)) then
  begin
    if Verbosity >= zvVerbose then
      Diag('Recreate multi-part: ' + theZip.ReqFileName);
    if theZip.UseSFX then
      detchSFX := true;
    Result := -__ERR_GE_Unknown;
    Intermed.File_Close;
    czip := theZip;
    // theZip must have proper stub
    if detchSFX and not assigned(czip.stub) then
    begin
      Result := -__ERR_CF_SFXCopyError; // no stub available - cannot convert
      exit;
    end;
    wantNewDisk := true; // assume need to ask for new disk
    if existed then
    begin
      czip.GetNewDisk(0, true); // ask to enter the first disk again
      czip.File_Close;
      wantNewDisk := false;
    end;
    tmp := theZip.ReqFileName;
    if detchSFX then
    begin
      if Verbosity >= zvVerbose then // Verbose or Trace then
        Diag('Recreate detached SFX');
      // allow room detchSFX stub
      detchsz := DetachedSize(Intermed);
      tmp := ChangeFileExt(tmp, EXT_ZIP); // name of the zip files
    end;
    // now create the spanned archive similar to theZip from Intermed
    OrigKeepFreeDisk1 := KeepFreeOnDisk1;
    DestZip := TZMZipCopy.Create(Master, nil);
    try
      DestZip.FileName := tmp;
      DestZip.ReqFileName := theZip.ReqFileName;
      KeepFreeOnDisk1 := KeepFreeOnDisk1 + Cardinal(detchsz);
      DestZip.ShowProgress := zspExtra;
      DestZip.TotalDisks := 0;
      if detchSFX and (DestZip.Numbering = znsExt) then
        DestZip.Numbering := znsName//;
      else
        DestZip.Numbering := theZip.Numbering;  // number same as source
      DestZip.PrepareWrite(zwMultiple);
      DestZip.NewDisk := wantNewDisk;
      DestZip.File_Size := Intermed.File_Size; // to calc TotalDisks
      Intermed.File_Open(fmOpenRead);
      DestZip.StampDate := Intermed.FileDate;
      AnswerAll := AnswerAll + [zaaYesOvrwrt];
      r := DestZip.WriteFile(Intermed, true);
      DestZip.File_Close;
      if r < 0 then
        raise EZipMaster.CreateMsgDisp(-r, true);
      if detchSFX then
      begin
        DestZip.FileName := DestZip.CreateMVFileNameEx(tmp, false, false);
        DestZip.GetNewDisk(0, false);
        DestZip.AssignStub(czip);
        DestZip.FileName := tmp; // restore base name
        if WriteDetached(DestZip) >= 0 then
          Result := 0;
      end
      else
        Result := 0;
    finally
      Intermed.File_Close;
      KeepFreeOnDisk1 := OrigKeepFreeDisk1;
      DestZip.Free;
    end;
    theZip.Invalidate;  // must reload
  end
  else
    // not split
    Result := RecreateSingle(Intermed, theZip); // just copy it
end;

(* ? TZMModOpr.RecreateSingle
Recreate the 'current' file from the intermediate result
to make as SFX
- Current.UseSFX is set
- Current.Stub must hold the stub to use
*)
function TZMModOpr.RecreateSingle(Intermed, theZip: TZMZipFile): Integer;
const
  __ERR_DS_FileError = __UNIT__ + (468 shl 10) + DS_FileError;
var
  DestZip: TZMZipCopy;
begin
  theZip.File_Close;
  if Verbosity >= zvVerbose then
    Diag('Replacing: ' + theZip.ReqFileName);
  Result := _Z_EraseFile(theZip.ReqFileName, HowToDelete = htdAllowUndo);
  if Result > 0 then // ignore file does not exist
  begin
    if Verbosity >= zvVerbose then
      Diag('EraseFile failed for: ' + theZip.ReqFileName);
    raise EZipMaster.CreateMsgDisp(__ERR_DS_FileError, true);
  end;
  // rename/copy Intermed
  AnswerAll := AnswerAll + [zaaYesOvrwrt];
  if assigned(theZip.stub) and theZip.UseSFX and (Intermed.Sig <> zfsDOS)
    then
  begin // rebuild with sfx
    if Verbosity >= zvVerbose then
      Diag('Rebuild with SFX');
    Intermed.File_Close;
    Intermed.File_Open(fmOpenRead);
    Result := Intermed.Open(false, false);
    if Result < 0 then
      exit;
    DestZip := TZMZipCopy.Create(Master, nil);
    try
      DestZip.AssignStub(theZip);
      DestZip.UseSFX := true;
      DestZip.StampDate := Intermed.StampDate; // will be 'orig' or now
      DestZip.DiskNr := 0;
      DestZip.ZipComment := theZip.ZipComment; // keep orig
      DestZip.ShowProgress := zspExtra;
      DestZip.File_Create(theZip.ReqFileName);
      Result := DestZip.WriteFile(Intermed, true);
      Intermed.File_Close;
      DestZip.File_Close;
      if Result < 0 then
        raise EZipMaster.CreateMsgDisp(-Result, true);
    finally
      DestZip.Free;
    end;
  end
  else
  begin
    theZip.File_Close;
    Result := -DS_FileError;
    if Intermed.File_Rename(theZip.ReqFileName) then
      Result := 0;
  end;
  theZip.Invalidate; // changed - must reload
end;

// write to intermediate then recreate as original
function TZMModOpr.Remake(CurZip: TZMZipFile; ReqCnt: Integer; All: boolean):
    Integer;
const
  __ERR_DS_NoOutFile = __UNIT__ + (524 shl 10) + DS_NoOutFile;
  __ERR_AZ_InternalError = __UNIT__ + (535 shl 10) + AZ_InternalError;
var
  Intermed: TZMZipCopy;
  Res: Integer;
begin
  Result := 0;
  Intermed := TZMZipCopy.Create(Master, nil);
  try
    if not Intermed.File_CreateTemp(PRE_INTER, '') then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_NoOutFile, True);
    Intermed.ShowProgress := zspFull;
    Intermed.ZipComment := CurZip.ZipComment;
    CurZip.File_Reopen(fmOpenRead);
    Res := Intermed.WriteFile(CurZip, All);
    CurZip.File_Close;
    Intermed.File_Close;
    if Res < 0 then
      raise EZipMaster.CreateMsgDisp(-Res, true);
    Result := Intermed.Count; // number of remaining files
    if (ReqCnt >= 0) and (Result <> ReqCnt) then
      raise EZipMaster.CreateMsgDisp(__ERR_AZ_InternalError, true);
    // Recreate like orig
    Res := Recreate(Intermed, CurZip);
    if Res < 0 then
      raise EZipMaster.CreateMsgDisp(-Res, true);
  finally
    Intermed.Free; // also delete temp file
  end;
end;

(*? TZMModOpr.Rename
 Function to read a Zip archive and change one or more file specifications.
 Source and Destination should be of the same type. (path or file)
 If NewDateTime is 0 then no change is made in the date/time fields.
*)
procedure TZMModOpr.Rename(RenameList: TList; NewDateTime: Integer; How:
    TZMRenameOpts = htrDefault);
const
  __ERR_AD_InvalidName = __UNIT__ + (564 shl 10) + AD_InvalidName;
var
  i: Integer;
  RenDat: TRenData;
  RenRec: PZMRenameRec;
  res: Integer;
begin
  for i := 0 to RenameList.Count - 1 do
  begin
    RenRec := RenameList.Items[i];
    if IsWild(RenRec.Source) then
       raise EZipMaster.CreateMsgDisp(__ERR_AD_InvalidName, true);
    RenRec^.Source := SetSlash(RenRec^.Source, psdExternal);
    RenRec^.Dest := SetSlash(RenRec^.Dest, psdExternal);
  end;
  RenDat.Owner := Self;
  RenDat.RenList := RenameList;
  RenDat.DTime := NewDateTime;
  RenDat.How := How;
  RenDat.cnt := 0;
  if IncludeSpecs.Count < 1 then
    IncludeSpecs.Add('*.*');
  res := ChangeFileDetails(RenFunc, RenDat);
  if res < 0 then
    raise EZipMaster.CreateMsgDisp(-res, true);
  SuccessCnt := RenDat.cnt;
end;

procedure TZMModOpr.SetZipComment(const Value: AnsiString);
begin
    fInternal.fZipComment := Value;
end;

procedure TZMModOpr.Set_ZipComment(const zComment: AnsiString);
const
  __ERR_GE_NoZipSpecified = __UNIT__ + (614 shl 10) + GE_NoZipSpecified;
  __ERR_DS_FileOpen = __UNIT__ + (625 shl 10) + DS_FileOpen;
  __ERR_DS_FailedSeek = __UNIT__ + (628 shl 10) + DS_FailedSeek;
  __ERR_DS_EOCBadRead = __UNIT__ + (629 shl 10) + DS_EOCBadRead;
  __ERR_DS_EOCBadRead1 = __UNIT__ + (631 shl 10) + DS_EOCBadRead;
  __ERR_DS_FailedSeek1 = __UNIT__ + (633 shl 10) + DS_FailedSeek;
  __ERR_DS_EOCBadWrite = __UNIT__ + (634 shl 10) + DS_EOCBadWrite;
  __ERR_DS_EOCBadWrite1 = __UNIT__ + (636 shl 10) + DS_EOCBadWrite;
var
  EOC: TZipEndOfCentral;
  len: Integer;
  wz: TZMZipFile;
  zcom: AnsiString;
begin
  if Lister = nil then
    exit;
  wz := TZMZipFile.Create(Master, nil);
  try
    try
      if Length(Lister.ZipFileName) <> 0 then
      begin
        wz.SpanOptions := wz.SpanOptions - [spExactName];
        wz.FileName := Lister.ZipFileName;
        wz.Open(true, true);// ignore errors
      end
      else
        raise EZipMaster.CreateMsgDisp(__ERR_GE_NoZipSpecified, true);
      Lister.Reload := zlrReload; // force reload
      // opened by OpenEOC() only for Read
      if wz.IsOpen then     // file exists
      begin
        wz.File_Close;
        // must reopen for read/write
        zcom := zComment;
        len := Length(zCom);
        wz.File_Open(fmShareDenyWrite or fmOpenReadWrite);
        if not wz.IsOpen then
          raise EZipMaster.CreateMsgDisp(__ERR_DS_FileOpen, True);
        if wz.MultiDisk and (wz.StampDate = 0) then
          wz.StampDate := wz.LastWritten;  // keep date of set
        wz.CheckSeek(wz.EOCOffset, 0, __ERR_DS_FailedSeek);
        wz.CheckRead(EOC, SizeOf(EOC), __ERR_DS_EOCBadRead);
        if (EOC.HeaderSig <> EndCentralDirSig) then
          raise EZipMaster.CreateMsgDisp(__ERR_DS_EOCBadRead1, True);
        EOC.ZipCommentLen := len;
        wz.CheckSeek(-Sizeof(EOC), 1, __ERR_DS_FailedSeek1);
        wz.CheckWrite(EOC, sizeof(EOC), __ERR_DS_EOCBadWrite);
        if len > 0 then
          wz.CheckWrite(zCom[1], len, __ERR_DS_EOCBadWrite1);
        // if SetEOF fails we get garbage at the end of the file, not nice but
        // also not important.
        wz.SetEndOfFile;
      end;
    except
      on ews: EZipMaster do
      begin
        ShowExceptionError(ews);
        ZipComment := '';
      end;
    end;
  finally
    wz.Free;
  end;
end;

procedure TZMModOpr.Started;
begin
  inherited;
end;

procedure TZMModOpr.VerifySource(SrcZip: TZMZipFile);
const
  __ERR_AZ_NothingToDo = __UNIT__ + (665 shl 10) + AZ_NothingToDo;
  __ERR_DS_Canceled = __UNIT__ + (667 shl 10) + DS_Canceled;
  __ERR_AD_InvalidZip = __UNIT__ + (669 shl 10) + AD_InvalidZip;
begin
  if not assigned(SrcZip) then
    raise EZipMaster.CreateMsgDisp(__ERR_AZ_NothingToDo, true);
  if (SrcZip.info and zfi_Cancelled) <> 0 then
    raise EZipMaster.CreateMsgDisp(__ERR_DS_Canceled, true);
  if (SrcZip.info and zfi_loaded) = 0 then
    raise EZipMaster.CreateMsgDisp(__ERR_AD_InvalidZip, true);
end;

end.

