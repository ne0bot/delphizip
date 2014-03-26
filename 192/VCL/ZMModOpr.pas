unit ZMModOpr;

// ZMModOpr.pas - Operations modifying existing entries

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
// modified 2013-02-06

{$I   '.\ZipVers.inc'}

interface

uses
  {$IFDEF VERDXE2up}
    System.Classes, System.SysUtils, WinApi.Windows, VCL.Graphics,
  {$ELSE}
    SysUtils, Windows, Classes, Graphics,
  {$ENDIF}
  ZipMstr, ZMCompat, ZMLister, ZMBody, ZMFileOpr, ZMZipReader, ZMZipWriter,
  ZMEntryReader;

type
  TSFXOps = (sfoNew, sfoZip, sfoExe);

type
  TZMBaseOpr = class(TZMFileOpr)
  private
    FInterimZip: TZMZipWriter;
    function RecreateSingle(Intermed, theZip: TZMZipReader): Integer;
    procedure SetInterimZip(const Value: TZMZipWriter);
  protected
    procedure CreateInterimZip; virtual;
    function FinalizeInterimZip(OrigZip: TZMZipReader): Integer; virtual;
    procedure PrepareInterimZip; virtual;
    function PrepareZip(Zip: TZMZipReader): Integer;
    function Recreate(Intermed, theZip: TZMZipReader): Integer;
    // 1 Rewrite via an intermediate
    function Remake(CurZip: TZMZipReader; ReqCnt: Integer;
      All: boolean): Integer;
    procedure VerifySource(SrcZip: TZMZipReader);
  public
    procedure BeforeDestruction; override;
    property InterimZip: TZMZipWriter read FInterimZip write SetInterimZip;
  end;

type
  TZMChangeOpr = class(TZMBaseOpr)
  protected
    procedure CreateInterimZip; override;
  public
    function ChangeFileDetails(func: TZMChangeFunction; var data): Integer;
    function Rename(RenameList: TList; NewDateTime: Integer; How: TZMRenameOpts =
        htrDefault): Integer;
    procedure Set_ZipComment(const zComment: AnsiString);
  end;

type
  TZMRecChangables = (zrcAttributes, zrcComment, zrcDOSDate, zrcEncoding,
    zrcExtraField, zrcFileName);
  TZMRecChanges = set of TZMRecChangables;

type
  TZMChangeRec = class(TZMDirRec)
  private
    FChanged: TZMRecChanges;
    FExtFileAttrib: Cardinal;
    FExtraField: TZMRawBytes;
    FFileComment: TZMString;
    FFileName: TZMString;
    FIsEncoded: TZMEncodingOpts;
    FModifDateTime: Cardinal;
    FMyMaster: TCustomZipMaster;
    FMyRec: TZMEntryCopier;
    function GetChanges(Index: TZMRecChangables): boolean;
    function SameExtra(ExtraField, ExtraField1: TZMRawBytes): boolean;
    procedure SetChanged(const Value: TZMRecChanges);
    procedure SetChanges(Index: TZMRecChangables; const Value: boolean);
    property Changes[Index: TZMRecChangables]: boolean read GetChanges
      write SetChanges;
  protected
    procedure AssignEntry(Entry: TZMEntryCopier);
    function GetCompressedSize: Int64; override;
    function GetCompressionMethod: Word; override;
    function GetCRC32: Cardinal; override;
    function GetDateTime: Cardinal; override;
    function GetEncoded: TZMEncodingOpts; override;
    function GetEncrypted: boolean; override;
    function GetExtFileAttrib: Longword; override;
    function GetExtraField: TZMRawBytes; override;
    function GetExtraFieldLength: Word; override;
    function GetFileComment: TZMString; override;
    function GetFileCommentLen: Word; override;
    function GetFileName: TZMString; override;
    function GetFileNameLength: Word; override;
    function GetFlag: Word; override;
    function GetHeaderName: TZMRawBytes; override;
    function GetIntFileAttrib: Word; override;
    function GetMaster: TComponent; override;
    function GetRelOffLocalHdr: Int64; override;
    function GetStartOnDisk: Word; override;
    function GetStatusBits: Cardinal; override;
    function GetUncompressedSize: Int64; override;
    function GetVersionMadeBy: Word; override;
    function GetVersionNeeded: Word; override;
  public
    constructor Create(TheMaster: TCustomZipMaster;
      TheRec: TZMEntryCopier); overload;
    function ChangeAttrs(nAttr: Cardinal): Integer; override;
    function ChangeComment(const ncomment: TZMString): Integer; override;
    function ChangeData(ndata: TZMRawBytes): Integer; override;
    function ChangeDate(ndosdate: Cardinal): Integer; override;
    function ChangeEncoding: Integer; override;
    function ChangeName(const nname: TZMString; NoCheck: boolean = False)
      : Integer; override;
    property Changed: TZMRecChanges read FChanged write SetChanged;
  end;

implementation

uses
  {$IFDEF VERDXE2up}
    VCL.Dialogs,
  {$ELSE}
    Dialogs,
  {$ENDIF}
  ZMStructs, ZMDelZip, ZMXcpt, ZMUtils, ZMDlg, ZMCtx, ZMMisc,
  ZMMsg, ZMZipBase, ZMDrv, ZMMatch, ZMWFuncs, ZMZipMulti;//, ZMZStream, ZMZLibEx;

const
  __UNIT__ = 18;// shl 23;

type
  pRenData = ^TRenData;

  TRenData = record
    Owner: TZMBaseOpr;
    RenList: TList;
    DTime: Integer;
    How: TZMRenameOpts;
    cnt: Integer;
  end;

type
  TZMZipChanger = class(TZMZipCopier)
  public
    function ChangeDetails(func: TZMChangeFunction; var data): Integer;
    function PrepareEntries(Src: TZMZipReader): Integer;
  end;

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

  // 'ForEach' function to rename files
function RenFunc(rec: TZMDirRec; var data): Integer;
var
  ChangingName: boolean;
  FileName: String;
  How: TZMRenameOpts;
  i: Integer;
  k: Integer;
  ncomment: String;
  newname: String;
  newStamp: Integer;
  pData: pRenData;
  pRenRec: PZMRenameRec;
  RenSource: TZMString;
begin
  FileName := rec.FileName;
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
    ChangingName := (newname <> SPEC_SEP) and
      (CompareStr(FileName, newname) <> 0);
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
      inc(pData^.cnt); // I am selected
      if not ChangingName then
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
          FileName := rec.FileName;
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

procedure TZMBaseOpr.BeforeDestruction;
begin
  if FInterimZip <> nil then
    FInterimZip.Free;
  inherited;
end;

procedure TZMBaseOpr.CreateInterimZip;
begin
  InterimZip := nil;
end;

function TZMBaseOpr.FinalizeInterimZip(OrigZip: TZMZipReader): Integer;
begin
  if OrigZip = nil then
    OrigZip := Lister.Current;
  OrigZip.File_Reopen(fmOpenRead);
  Result := InterimZip.Commit(zwoZipTime in WriteOptions);
  OrigZip.File_Close;
  InterimZip.File_Close;
  if Result < 0 then
    raise EZipMaster.CreateMsgDisp(-Result, true);
  PrepareZip(OrigZip);
  // Recreate like orig
  Result := Recreate(InterimZip, OrigZip);
  if Result < 0 then
    raise EZipMaster.CreateMsgDisp(-Result, true);
end;

procedure TZMBaseOpr.PrepareInterimZip;
var
  CurZip: TZMZipReader;
  Err: Integer;
begin
  CurZip := Lister.Current;
  CreateInterimZip;
  ShowProgress := zspFull;
  InterimZip.ZipComment := CurZip.ZipComment;
  if (CurZip.OpenRet >= 0) and CurZip.MultiDisk then
  begin
    Err := InterimZip.RefuseWriteSplit;
    if Err <> 0 then
      raise EZipMaster.CreateMsgDisp(ZM_Error(313, Err), true);
  end;
  if (not(zwoDiskSpan in Body.WriteOptions)) and
    (UpperCase(ExtractFileExt(CurZip.ReqFileName)) = EXT_EXE) then
  begin
    InterimZip.UseSFX := true;
    InterimZip.Stub := NewSFXStub;
  end;
end;

function TZMBaseOpr.PrepareZip(Zip: TZMZipReader): Integer;
begin
  Result := Zip.RefuseWriteSplit;
  if Result <> 0 then
  begin
    Result := -ZM_Error(328, Result);
    Exit;
  end;
  if (UpperCase(ExtractFileExt(Zip.ReqFileName)) = EXT_EXE) then
  begin
    Zip.UseSFX := true;
    Zip.Stub := NewSFXStub;
  end;
  Result := 0;
end;

(* ? TZMBaseOpr.Recreate
  recreate the 'theZip' file from the intermediate result
  to make as SFX
  - theZip.UseSFX is set
  - theZip.Stub must hold the stub to use
*)
function TZMBaseOpr.Recreate(Intermed, theZip: TZMZipReader): Integer;
var
  czip: TZMZipReader;
  DestZip: TZMZipCopier;
  detchSFX: boolean;
  detchsz: Integer;
  existed: boolean;
  OrigKeepFreeDisk1: Cardinal;
  r: Integer;
  tmp: String;
  wantNewDisk: boolean;
begin
  detchsz := 0;
  detchSFX := False;
  existed := (zfi_Loaded and theZip.info) <> 0;
  if theZip.MultiDisk or ((not existed) and (zwoDiskSpan in Lister.WriteOptions))
  then
  begin
    Reporter.Trace('Recreate multi-part: ' + theZip.ReqFileName, ZM_Error(363, 0));
    if theZip.UseSFX then
      detchSFX := true;
    Result := -ZM_Error(366, ZS_Unknown);
    Intermed.File_Close;
    czip := theZip;
    // theZip must have proper stub
    if detchSFX and not assigned(czip.Stub) then
    begin
      Result := -ZM_Error(372, ZS_SFXCopyError); // no stub available - cannot convert
      Exit;
    end;
    wantNewDisk := true; // assume need to ask for new disk
    if existed then
    begin
      czip.SeekDisk(0, true); // ask to enter the first disk again
      czip.File_Close;
      wantNewDisk := False;
    end;
    tmp := theZip.ReqFileName;
    if detchSFX then
    begin
//      if Verbosity >= zvVerbose then // Verbose or Trace then
        Reporter.Trace('Recreate detached SFX', ZM_Error(386, 0));
      // allow room detchSFX stub
      detchsz := DetachedSize(Intermed);
      tmp := ChangeFileExt(tmp, EXT_ZIP); // name of the zip files
    end;
    // now create the spanned archive similar to theZip from Intermed
    OrigKeepFreeDisk1 := Span.KeepFreeOnDisk1;
    DestZip := TZMZipCopier.Create(Lister);
    try
      DestZip.ArchiveName := tmp;
      DestZip.WorkDrive.DriveStr := tmp;
      DestZip.ReqFileName := theZip.ReqFileName;
      Span.KeepFreeOnDisk1 := Span.KeepFreeOnDisk1 + Cardinal(detchsz);
      ShowProgress := zspExtra;
      DestZip.TotalDisks := 0;
      if detchSFX and (DestZip.Numbering = znsExt) then
        DestZip.Numbering := znsName
      else
        DestZip.Numbering := theZip.Numbering; // number same as source
      DestZip.PrepareWrite(zwMultiple);
      DestZip.NewDisk := wantNewDisk;
      DestZip.File_Size := Intermed.File_Size; // to calc TotalDisks
      Intermed.File_Open('', fmOpenRead);
      DestZip.StampDate := Intermed.FileDate;
      AnswerAll := true;
      r := DestZip.WriteFile(Intermed, true);
      DestZip.File_Close;
      if r < 0 then
        raise EZipMaster.CreateMsgDisp(-r, true);
      if detchSFX then
      begin
        if DestZip.WorkDrive.DriveIsFloppy then
          DestZip.ArchiveName := tmp
        else
          DestZip.ArchiveName := DestZip.CreateMVFileNameEx(tmp, False, False);
        DestZip.SeekDisk(0, False);
        DestZip.AssignStub(czip);
        DestZip.ArchiveName := tmp; // restore base name
        if WriteDetached(DestZip) >= 0 then
          Result := 0;
      end
      else
        Result := 0;
    finally
      Intermed.File_Close;
      Span.KeepFreeOnDisk1 := OrigKeepFreeDisk1;
      DestZip.Free;
    end;
    theZip.Invalidate; // must reload
  end
  else
    // not split
    Result := RecreateSingle(Intermed, theZip); // just copy it
end;

(* ? TZMBaseOpr.RecreateSingle
  Recreate the 'current' file from the intermediate result
  to make as SFX
  - Current.UseSFX is set
  - Current.Stub must hold the stub to use
*)
function TZMBaseOpr.RecreateSingle(Intermed, theZip: TZMZipReader): Integer;
var
  DestZip: TZMZipCopier;
begin
  theZip.File_Close;
  Reporter.Trace('Replacing: ' + theZip.ReqFileName);
  Result := _Z_EraseFile(theZip.ReqFileName, HowToDelete = htdAllowUndo);
  if Result > 0 then // ignore file does not exist
  begin
    Reporter.Inform('EraseFile failed for: ' + theZip.ReqFileName);
    raise EZipMaster.CreateMsgDisp(ZM_Error(457, ZS_FileError), true);
  end;
  // rename/copy Intermed
  AnswerAll := true;
  if assigned(theZip.Stub) and theZip.UseSFX and (Intermed.Sig <> zfsDOS) then
  begin // rebuild with sfx
    Reporter.Trace('Rebuild with SFX', ZM_Error(463, 0));
    Intermed.File_Close;
    Intermed.File_Open('', fmOpenRead);
    Result := Intermed.Open(False, False);
    if Result < 0 then
      Exit;
    DestZip := TZMZipCopier.Create(Lister);
    try
      DestZip.AssignStub(theZip);
      DestZip.UseSFX := true;
      DestZip.StampDate := Intermed.StampDate; // will be 'orig' or now
      DestZip.DiskNr := 0;
      DestZip.ZipComment := theZip.ZipComment; // keep orig
      ShowProgress := zspExtra;
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
    Result := -ZS_FileError;
    if Intermed.File_Rename(theZip.ReqFileName) then
      Result := 0;
  end;
  theZip.Invalidate; // changed - must reload
end;

// write to intermediate then recreate as original
function TZMBaseOpr.Remake(CurZip: TZMZipReader; ReqCnt: Integer;
  All: boolean): Integer;
var
  Intermed: TZMZipCopier;
  Res: Integer;
begin
  Result := 0;
  Intermed := TZMZipCopier.Create(Lister);
  try
    if not Intermed.File_CreateTemp(PRE_INTER, '') then
      raise EZipMaster.CreateMsgDisp(ZM_Error(508, ZS_NoOutFile), true);
    ShowProgress := zspFull;
    Intermed.ZipComment := CurZip.ZipComment;
    CurZip.File_Reopen(fmOpenRead);
    Res := Intermed.WriteFile(CurZip, All);
    CurZip.File_Close;
    Intermed.File_Close;
    if Res < 0 then
      raise EZipMaster.CreateMsgDisp(-Res, true);
    Result := Intermed.Count; // number of remaining files
    if (ReqCnt >= 0) and (Result <> ReqCnt) then
      raise EZipMaster.CreateMsgDisp(ZM_Error(519, ZS_InternalError), true);
    // Recreate like orig
    Res := Recreate(Intermed, CurZip);
    if Res < 0 then
      raise EZipMaster.CreateMsgDisp(-Res, true);
  finally
    Intermed.Free; // also delete temp file
  end;
end;

procedure TZMBaseOpr.SetInterimZip(const Value: TZMZipWriter);
begin
  if Value <> FInterimZip then
  begin
    FInterimZip.Free;
    FInterimZip := Value;
  end;
end;

procedure TZMBaseOpr.VerifySource(SrcZip: TZMZipReader);
begin
  if not assigned(SrcZip) then
    raise EZipMaster.CreateMsgDisp(ZM_Error(541, ZS_NothingToDo), true);
  if (SrcZip.info and zfi_Cancelled) <> 0 then
    raise EZipMaster.CreateMsgDisp(ZM_Error(543, ZS_Canceled), true);
  if (SrcZip.info and zfi_Loaded) = 0 then
    raise EZipMaster.CreateMsgDisp(ZM_Error(545, ZS_InvalidZip), true);
end;

function TZMZipChanger.ChangeDetails(func: TZMChangeFunction; var data)
  : Integer;
var
  Changes: Integer;
  ChangeRec: TZMChangeRec;
  CRec: TZMEntryCopier;
  i: Integer;
  Reason: TZMSkipTypes;
  RecName: string;
begin
  Result := 0;
  Changes := 0;
  try
    ChangeRec := TZMChangeRec.Create(Master, nil);
    try
      for i := 0 to ToDoList.Count - 1 do
      begin
        CRec := TZMEntryCopier(ToDoList[i]); // Selected;
        ChangeRec.AssignEntry(CRec);
        RecName := CRec.FileName;
        Result := func(ChangeRec, data);
        CheckCancel;
        if Result <> 0 then
        begin
          Reporter.Inform(Format('error [%d] for: %s',
            [AbsErr(Result), CRec.FileName]));

          Reason := stCannotDo;
          if AbsErr(Result) = ZS_DuplFileName then
            Reason := stDupName;
          if Reporter.ReportSkippingEx(RecName, Reason, Result, ExcludeSpecs) then
          begin
            Result := -ZM_Error(580, ZS_NoSkipping);
            break; // fatal
          end;
          Result := 0; // ignore error
          Continue;
        end;
        if ChangeRec.Changed <> [] then
        begin
          if zrcFileName in ChangeRec.Changed then
          begin
            if FHashTable <> nil then
              HTRemove(CRec); // remove old name
            CRec._FileName := '';
            CRec.FileName := ChangeRec.FileName;
            CRec.Status[zsbRenamed] := true;
            if FHashTable <> nil then
              HTAdd(CRec, true); // add new name
            RecName := RecName + ' :: ' + CRec.FileName;
          end;
          if zrcComment in ChangeRec.Changed then
          begin
            CRec._FileComment := '';
            CRec.FileComment := ChangeRec.FileComment;
            CRec.SetStatusBit(zsbVChanged);
          end;
          if zrcAttributes in ChangeRec.Changed then
          begin
            CRec.ExtFileAttrib := ChangeRec.ExtFileAttrib;
          end;
          if zrcDOSDate in ChangeRec.Changed then
          begin
            CRec.ModifDateTime := ChangeRec.DateTime;
            CRec.SetStatusBit(zsbVChanged);
          end;
          if zrcEncoding in ChangeRec.Changed then
          begin

            CRec.SetStatusBit(zsbVChanged);
          end;
          if zrcExtraField in ChangeRec.Changed then
          begin
            CRec.ExtraField := ChangeRec.ExtraField;
            CRec.SetStatusBit(zsbVChanged);
          end;
          CRec.SetStatusBit(zsbDirty);
          inc(Changes);
          Reporter.Trace('Changed: ' + RecName);
          CRec.Status[zsbHail] := True;
        end
        else
          ExcludeSpecs.Add(RecName);
      end;
    finally
      ChangeRec.Free;
    end;
  except
    on E: EZipMaster do
      Result := -E.ResId;
    on E: EZMAbort do
      raise;
    on E: Exception do
      Result := -ZM_Error(641, ZS_ExceptErr);
  end;
  if Result = 0 then
    Result := Changes;
end;

// returns count of selected entries
function TZMZipChanger.PrepareEntries(Src: TZMZipReader): Integer;
var
  CopyRec: TZMEntryWriter;
  rec: TZMEntryBase;
begin
  Result := 0;
  HTAutoSize(Src.Count);
  rec := Src.FirstRec;
  while rec <> nil do
  begin
    if rec.StatusBit[zsbError or zsbDiscard] = 0 then
    begin
      if Reporter.Verbosity > zvVerbose then
        Reporter.Trace('including: ' + rec.FileName);
      CopyRec := TZMEntryCopier.Create(Self); // make a copy
      CopyRec.AssignFrom(rec);
      CopyRec.Link := rec; // link to original
      Add(CopyRec);
      HTAdd(CopyRec, true); // add all names
      if rec.TestStatusBit(zsbSelected) then
      begin
        ToDoList.Add(CopyRec);
        inc(Result);
      end;
    end;
    rec := rec.Next;
  end;
end;

(* TZMChangeOpr.ChangeFileDetails
  Add zipped files from source ZipMaster selected from source FSpecArgs
  When finished
  FSpecArgs will contain source files copied
  FSpecArgsExcl will contain source files skipped  (data = error code)
*)
function TZMChangeOpr.ChangeFileDetails(func: TZMChangeFunction;
  var data): Integer;
var
  CurZip: TZMZipReader;
  Did: Integer;
  i: Integer;
  MyChanger: TZMZipChanger;
  SelCnt: Integer;
  SkipCnt: Integer;
  SkippedFiles: TStringList;
begin
  Result := 0;
  Reporter.Trace('StartUp ChangeFileDetails');
  if Reporter.Verbosity > zvTrace then // Reporter.Logger <> nil then
    Reporter.Logger.LogSpecs('');
  if @func = nil then
    raise EZipMaster.CreateMsgDisp(ZM_Error(699, ZS_InvalidArguments), true);
  if IncludeSpecs.Count < 1 then
    IncludeSpecs.Add('*.*');
  // copy all entries to InterimZip
  CurZip := CurrentZip(true, False);
  SkippedFiles := TStringList.Create;
  try
    SelCnt := CurZip.SelectFiles(IncludeSpecs, ExcludeSpecs, SkippedFiles);
    Body.ClearIncludeSpecs;//IncludeSpecs.Clear; // will contain files processed
    Body.ClearExcludeSpecs;//ExcludeSpecs.Clear; // will contain source files skipped
    SkipCnt := SkippedFiles.Count;
    for i := 0 to SkippedFiles.Count - 1 do
    begin
      if Reporter.ReportSkippingEx(SkippedFiles[i], stNotFound, 0, ExcludeSpecs) then
        Result := -ZM_Error(713, ZS_NoSkipping)
      else
        Dec(SkipCnt); // user chose to ignore
    end;
    if (Result = 0) and ((SelCnt <= 0) or (SkipCnt <> 0)) then
    begin
      Reporter.Inform('nothing selected');
      Result := -ZM_Error(720, ZS_NothingToDo);
      Reporter.ShowZipMessage(Result, '');
    end;
  finally
    SkippedFiles.Free;
  end;
  if Result < 0 then
    Exit;
  PrepareInterimZip;
  MyChanger := InterimZip as TZMZipChanger;
  MyChanger.PrepareEntries(CurZip);
  // process selected files and copy rest
  Result := MyChanger.ChangeDetails(func, data);
  Did := Result;
  if Result > 0 then
    Result := FinalizeInterimZip(CurZip); // write results

  if Result < 0 then
    Reporter.ShowZipMessage(Result, '')
//    ShowZipMsg(Result, true)
  else
    SuccessCnt := Did;
  // Update the Zip Directory by calling List method
  // for spanned exe avoid swapping to last disk
  Reload := zlrReload;
  Reporter.Trace('finished ChangeFileDetails');
  if Reporter.Verbosity > zvTrace then // Reporter.Logger <> nil then
    Reporter.Logger.LogSpecs('');
end;

procedure TZMChangeOpr.CreateInterimZip;
begin
  InterimZip := TZMZipChanger.Create(Lister);
  if not InterimZip.File_CreateTemp(PRE_INTER, '') then
    raise EZipMaster.CreateMsgDisp(ZM_Error(754, ZS_NoOutFile), true);
end;

(* ? TZMChangeOpr.Rename
  Function to read a Zip archive and change one or more file specifications.
  Source and Destination should be of the same type. (path or file)
  If NewDateTime is 0 then no change is made in the date/time fields.
*)
function TZMChangeOpr.Rename(RenameList: TList; NewDateTime: Integer; How:
    TZMRenameOpts = htrDefault): Integer;
var
  i: Integer;
  RenDat: TRenData;
  RenRec: PZMRenameRec;
//  Res: Integer;
begin
  for i := 0 to RenameList.Count - 1 do
  begin
    RenRec := RenameList.Items[i];
    if IsWild(RenRec.Source) then
      raise EZipMaster.CreateMsgDisp(ZM_Error(774, ZS_InvalidName), true);
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
  Result := ChangeFileDetails(RenFunc, RenDat);
//  if Res < 0 then
//    raise EZipMaster.CreateMsgDisp(-Res, true);
  if Result >= 0 then
    SuccessCnt := RenDat.cnt;
end;

procedure TZMChangeOpr.Set_ZipComment(const zComment: AnsiString);
var
  EOC: TZipEndOfCentral;
  len: Integer;
  wz: TZMZipWriter;
  zcom: AnsiString;
begin
  if Lister = nil then
    Exit;
  wz := TZMZipWriter.Create(Lister);
  try
    try
      if Length(ZipFileName) <> 0 then
      begin
        Span.Options := Span.Options - [spExactName];
        wz.ArchiveName := ZipFileName;
        wz.Open(true, true); // ignore Errors
      end
      else
        raise EZipMaster.CreateMsgDisp(ZM_Error(810, ZS_NoZipSpecified), true);
      Reload := zlrReload; // force reload
      // opened by OpenEOC() only for Read
      if wz.IsOpen then // file exists
      begin
        wz.File_Close;
        // must reopen for read/write
        zcom := zComment;
        len := Length(zcom);
        wz.File_Open('', fmShareDenyWrite or fmOpenReadWrite);
        if not wz.IsOpen then
          raise EZipMaster.CreateMsgDisp(ZM_Error(821, ZS_FileOpen), true);
        if wz.MultiDisk and (wz.StampDate = 0) then
          wz.StampDate := wz.LastWritten; // keep date of set
        wz.CheckSeek(wz.EOCOffset, soBeginning, ZM_Error(824, ZS_FailedSeek));
        wz.CheckRead(EOC, SizeOf(EOC), ZM_Error(825, ZS_EOCBadRead));
        if (EOC.HeaderSig <> EndCentralDirSig) then
          raise EZipMaster.CreateMsgDisp(ZM_Error(827, ZS_EOCBadRead), true);
        EOC.ZipCommentLen := len;
        wz.CheckSeek(-SizeOf(EOC), soCurrent, ZM_Error(829, ZS_FailedSeek));
        wz.CheckWrite(EOC, SizeOf(EOC), ZM_Error(830, ZS_EOCBadWrite));
        if len > 0 then
          wz.CheckWrite(zcom[1], len, ZM_Error(832, ZS_EOCBadWrite));
        // if SetEOF fails we get garbage at the end of the file, not nice but
        // also not important.
        wz.SetEndOfFile;
        wz.FixFileDate;
      end;
    except
      on ews: EZipMaster do
        Reporter.ShowExceptionError(ews)
      else
        raise;
    end;
  finally
    wz.Free;
  end;
end;

constructor TZMChangeRec.Create(TheMaster: TCustomZipMaster;
  TheRec: TZMEntryCopier);
begin
  inherited Create;
  FMyMaster := TheMaster;
  AssignEntry(TheRec);
end;

procedure TZMChangeRec.AssignEntry(Entry: TZMEntryCopier);
begin
  FMyRec := Entry;
  if FMyRec <> nil then
  begin
    FExtFileAttrib := Entry.ExtFileAttrib;
    FExtraField := Entry.ExtraField;
    FFileComment := Entry.FileComment;
    FFileName := Entry.FileName;
    FIsEncoded := Entry.IsEncoded;
    FModifDateTime := Entry.ModifDateTime;
  end
  else
  begin
    FExtFileAttrib := 0;
    FExtraField := '';
    FFileComment := '';
    FFileName := '';
    FIsEncoded := zeoAuto;
    FModifDateTime := 0;
  end;
  FChanged := [];
end;

{ TZMChangeRec }
function TZMChangeRec.ChangeAttrs(nAttr: Cardinal): Integer;
begin
  if nAttr <> ExtFileAttrib then
  begin
    FExtFileAttrib := nAttr;
    Changes[zrcAttributes] := nAttr <> FMyRec.ExtFileAttrib;
  end;
  Result := 0;
end;

function TZMChangeRec.ChangeComment(const ncomment: TZMString): Integer;
begin
  if CompareStr(ncomment, FFileComment) <> 0 then
  begin
    FFileComment := ncomment;
    Changes[zrcComment] := CompareStr(FFileComment, FMyRec.FileComment) <> 0;
  end;
  Result := 0;
end;

function TZMChangeRec.ChangeData(ndata: TZMRawBytes): Integer;
var
  NowData: TZMRawBytes;
  OldData: TZMRawBytes;
begin
  Result := 0; // always allowed
  if not SameExtra(ndata, FExtraField) then
  begin
    // preserve required tags
    OldData := XDataKeep(FExtraField, [Zip64_data_tag, UPath_Data_Tag,
      UCmnt_Data_Tag]);
    // do not allow changing fields
    NowData := XDataRemove(ndata, [Zip64_data_tag, UPath_Data_Tag,
      UCmnt_Data_Tag]);
    // will it fit?
    if (Length(OldData) + Length(NowData) + Length(GetFileComment) +
      Length(GetFileName)) < MAX_WORD then
    begin
      FExtraField := OldData + NowData;
      Changes[zrcExtraField] := not SameExtra(FExtraField, FMyRec.ExtraField);
    end
    else
      Result := -ZM_Error(924, ZS_CEHDataSize);
  end;
end;

function TZMChangeRec.ChangeDate(ndosdate: Cardinal): Integer;
begin
//  Result := -__ERR_ZS_NoProtected;
  if Encrypted then
  begin
    Result := -ZM_Error(933, ZS_NoProtected);
    Exit;
  end;
  try
    // test if valid date/time will throw error if not
    FileDateToDateTime(ndosdate);
  except
    on E: EZMAbort do
      raise
    else
    begin
      Result := -ZM_Error(944, ZS_InvalidDateTime);
      FMyRec.MyFile.Reporter.Inform('Invalid date ' + GetFileName);
      Exit;
    end;
  end;
  Result := 0;
  if ndosdate <> GetDateTime then
  begin
    FModifDateTime := ndosdate;
    Changes[zrcDOSDate] := FModifDateTime <> FMyRec.ModifDateTime;
  end;
end;

function TZMChangeRec.ChangeEncoding: Integer;
begin
  Changes[zrcEncoding] := true;
  Result := 0;
end;

function TZMChangeRec.ChangeName(const nname: TZMString;
  NoCheck: boolean = False): Integer;
var
  Dup: TZMEntryBase;
  iname: TZMString;
begin
  Result := FMyRec.ToIntForm(nname, iname);
  if Result = 0 then
  begin
    if IsFolder(iname) <> FMyRec.IsDirOnly then
    begin
      Result := -ZM_Error(974, ZS_NoChangeDir);
      Exit; // dirOnly status must be same
    end;
    if CompareStr(iname, FFileName) <> 0 then
    begin
      if not NoCheck then
      begin
        Dup := FMyRec.MyFile.FindName(iname, FMyRec);
        if Dup <> nil then
        begin
          FMyRec.MyFile.Reporter.Inform('Name already exists: "' + iname + '"');
          Result := -ZM_Error(985, ZS_DuplFileName);
          Exit;
        end;
      end;
      FFileName := iname;
      Changes[zrcFileName] := True;
    end;
  end;
end;

function TZMChangeRec.GetChanges(Index: TZMRecChangables): boolean;
begin
  Result := Index in FChanged;
end;

function TZMChangeRec.GetCompressedSize: Int64;
begin
  Result := FMyRec.CompressedSize;
end;

function TZMChangeRec.GetCompressionMethod: Word;
begin
  Result := FMyRec.CompressionMethod;
end;

function TZMChangeRec.GetCRC32: Cardinal;
begin
  Result := FMyRec.CRC32;
end;

function TZMChangeRec.GetDateTime: Cardinal;
begin
  Result := FModifDateTime;
end;

function TZMChangeRec.GetEncoded: TZMEncodingOpts;
begin
  Result := FIsEncoded;
end;

function TZMChangeRec.GetEncrypted: boolean;
begin
  Result := FMyRec.Encrypted;
end;

function TZMChangeRec.GetExtFileAttrib: Longword;
begin
  Result := FExtFileAttrib;
end;

function TZMChangeRec.GetExtraField: TZMRawBytes;
begin
  Result := FExtraField;
end;

function TZMChangeRec.GetExtraFieldLength: Word;
begin
  Result := Length(FExtraField);
end;

function TZMChangeRec.GetFileComment: TZMString;
begin
  Result := FFileComment;
end;

function TZMChangeRec.GetFileCommentLen: Word;
begin
  Result := Length(FFileComment);
end;

function TZMChangeRec.GetFileName: TZMString;
begin
  Result := FFileName;
end;

function TZMChangeRec.GetFileNameLength: Word;
begin
  Result := Length(FFileName);
end;

function TZMChangeRec.GetFlag: Word;
begin
  Result := FMyRec.Flag;
end;

function TZMChangeRec.GetHeaderName: TZMRawBytes;
begin
  Result := FMyRec._FileName;
end;

function TZMChangeRec.GetIntFileAttrib: Word;
begin
  Result := FMyRec.IntFileAttrib;
end;

function TZMChangeRec.GetMaster: TComponent;
begin
  Result := FMyMaster;
end;

function TZMChangeRec.GetRelOffLocalHdr: Int64;
begin
  Result := FMyRec.RelOffLocalHdr;
end;

function TZMChangeRec.GetStartOnDisk: Word;
begin
  Result := FMyRec.StartOnDisk;
end;

function TZMChangeRec.GetStatusBits: Cardinal;
begin
  Result := FMyRec.StatusBits;
end;

function TZMChangeRec.GetUncompressedSize: Int64;
begin
  Result := FMyRec.UncompressedSize;
end;

function TZMChangeRec.GetVersionMadeBy: Word;
begin
  Result := FMyRec.VersionMadeBy;
end;

function TZMChangeRec.GetVersionNeeded: Word;
begin
  Result := FMyRec.VersionNeeded;
end;

function TZMChangeRec.SameExtra(ExtraField, ExtraField1: TZMRawBytes): boolean;
var
  i: Integer;
begin
  Result := False;
  if Length(ExtraField) <> Length(ExtraField1) then
    Exit;
  for i := 1 to Length(ExtraField) do
    if ExtraField[i] <> ExtraField1[i] then
      Exit;
  Result := true;
end;

procedure TZMChangeRec.SetChanged(const Value: TZMRecChanges);
var
  C: TZMRecChangables;
  V: TZMRecChanges;
begin
  if FChanged <> Value then
  begin
    V := Value;
    // don't allow setting changes that have not happened
    for C := zrcAttributes to zrcFileName do
      if (C in V) and not(C in FChanged) then
        V := V - [C];
    // allow clearing changes
    if (zrcAttributes in FChanged) and not(zrcAttributes in V) then
      FExtFileAttrib := FMyRec.ExtFileAttrib;
    if (zrcComment in FChanged) and not(zrcComment in V) then
      FFileComment := FMyRec.FileComment;
    if (zrcDOSDate in FChanged) and not(zrcDOSDate in V) then
      FModifDateTime := FMyRec.ModifDateTime;
    if (zrcEncoding in FChanged) and not(zrcEncoding in V) then
      FIsEncoded := FMyRec.IsEncoded;
    if (zrcExtraField in FChanged) and not(zrcExtraField in V) then
      FExtraField := FMyRec.ExtraField;
    if (zrcFileName in FChanged) and not(zrcFileName in V) then
      FFileName := FMyRec.FileName;
    FChanged := V;
  end;
end;

procedure TZMChangeRec.SetChanges(Index: TZMRecChangables;
  const Value: boolean);
begin
  if Changes[Index] <> Value then
  begin
    if Value then
      FChanged := FChanged + [Index]
    else
      FChanged := FChanged - [Index];
  end;
end;

end.
