unit ZMZippedOpr;

//  ZMZippedOpr.pas - MergeZipped/CopyZipped operations

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
//modified 2012-03-15

{$I   '.\ZipVers.inc'}
{$IFDEF VER180}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

interface

uses
  SysUtils, Windows, Classes, Graphics,
  ZipMstr, ZMCompat, ZMCore, ZMModOpr, ZMZipFile, ZMLister,
  ZMCentral, ZMArgSplit, ZMWZip, ZMIRec;

// ------------------------------------------------------------------------

type
  TSFXOps = (sfoNew, sfoZip, sfoExe);

type
  TMOArgOptions = record
    Arg: TZMString;
    Excludes: TZMString;
    NFlag: Boolean;
    XArg: TZMString;
  end;

type
  TZM_ConflictEntry = class(TZMConflictEntry)
  private
    FRec: TZMZRec;
    FZipName: TZMString;
    procedure SetRec(const Value: TZMZRec);
    procedure SetZipName(const Value: TZMString);
  protected
    function GetEntry: TZMDirEntry; override;
    function GetRec: TZMZRec;
    function GetZipName: TZMString; override;
  public
    property Rec: TZMZRec read GetRec write SetRec;
    property ZipName: TZMString read GetZipName write SetZipName;
  end;

type
  // provides support for auto-open of source zips
  TZMZipMerge = class(TZMZipCopy)
  private
    FLastOpened: TZMWorkZip;
    procedure SetLastOpened(const Value: TZMWorkZip);
  protected
    function CommitRec(Rec: TZMZRec): Int64; override;
    property LastOpened: TZMWorkZip read FLastOpened write SetLastOpened;
  public
    procedure AfterConstruction; override;
  end;

  TZMMergeRec = class(TZMCopyRec)
  private
    FKeep: Boolean;
    FNextEntry: Int64;
  public
    procedure AfterConstruction; override;
    procedure AssignFrom(const ARec: TZMIRec); override;
    function Process: Int64; override;
    property Keep: Boolean read FKeep write FKeep;
    property NextEntry: Int64 read FNextEntry write FNextEntry;
  end;

type
  TZMZipList = class;

  TZMZippedOpr = class(TZMModOpr)
  private
    procedure AssignArgOptions(var Locals: TMOArgOptions;
      const Args: TMOArgOptions);
    procedure ClearAppend;
    function CommitAppend: Integer;
    function CopyZippedDelete: Integer;
    function DeleteSelectedFiles(Zip: TZMZipFile): Integer;
    function ExtractChildZip(ParentIndex: Integer; MyName: string): Integer;
    procedure FixResults;
    function FlattenExcludes(Excludes: TStrings): string;
    function IncludeAZip(const SourceName: string): Integer;
    function MergeAnotherZip(const SourceName: String): Integer;
    function MergeAnotherZip1(const SourceName: String): Integer;
    function MergeIntermediate(SelectedCount: Integer;
      const Opts: TZMMergeOpts): Integer;
    function MergeIntermedZip(RefZip: TZMZipFile; ZipIndex: Integer;
      Opts: TZMMergeOpts): Integer;
    function MergePrepareName(var ZName: TZMString; ZRec: TZMZRec): Integer;
    function MergeResolveConflict(RefRec, NRec: TZMCopyRec;
      const Opts: TZMMergeOpts): Integer;
    function MergeSafeName(ZipIndex: Integer; const Name: string): string;
    function MergeZippedFiles1(Opts: TZMMergeOpts; TheDest: TZMZipFile)
      : Integer;
    function Prepare(MustExist: Boolean; SafePart: Boolean = false): TZMZipFile;
    function PrepareAppend: Boolean;
    function PrepareDest(DestLister: TZMLister): TZMZipFile;
    function PrepareZip(Zip: TZMZipFile): Integer;
    function ProcessDelete(SrcZip: TZMZipFile; Arg, Excludes: string): Integer;
    function ProcessDeleteList(var SelectCount: Integer): Integer;
    function ProcessInclude(SrcZip: TZMZipFile; Args: TMOArgOptions): Integer;
    function ProcessIncludeList(var SelectCount: Integer): Integer;
    function ResolveConfirm(const ExistName: TZMString; ExistRec: TZMZRec;
      const ConflictName: TZMString; ConflictRec: TZMZRec;
      const NewName: TZMString): TZMResolutions;
    procedure SetArgsOptionsFromSplitter(var Args: TMOArgOptions;
      const ParentExcludes: TZMString);
    procedure SetupAppendRecs(Allow: Boolean);
  protected
    FOutZip: TZMZipCopy;
    FSkippedFiles: TStringList;
    FSplitter: TZMArgSplitter;
    FZipList: TZMZipList;
    function Delete1: Integer;
    function DeleteByList: Integer;
    procedure DoMergeEvent(Sender: TObject;
      Existing, Conflicting: TZMConflictEntry; var Resolve: TZMResolutions);
    procedure Finished(WasGood: Boolean); override;
    function MergeWrite: Integer;
    procedure Started; override;
  public
    constructor Create(theComponent: TCustomZipMaster);
    procedure AddZippedFiles(SrcLister: TZMLister; Merge: TZMMergeOpts);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function CleanZipName(const FileName: string): string;
    procedure CopyZippedFiles(DstLister: TZMLister; DeleteFromSource: Boolean;
      OverwriteDest: TZMMergeOpts); overload;
    function Delete: Integer;
    function MergeAnotherZipInZip(const SourceName: String): Integer;
    function MergeZippedFiles(Opts: TZMMergeOpts; TheDest: TZMZipFile): Integer;
  end;

  TZMZipList = class(TZMWorkBin)
  private
    FProtectZero: Boolean;
    FZips: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TZMZipFile;
  protected
    property ProtectZero: Boolean read FProtectZero write FProtectZero;
  public
    function Add(aZip: TZMZipFile): Integer;
    procedure BeforeDestruction; override;
    procedure Clear;
    procedure CloseAll;
    function Find(const aZipFile: TZMString): Integer;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TZMZipFile read GetItems; default;
  end;

implementation

uses
  Dialogs, ZMStructs, ZMDelZip, ZMXcpt, ZMUtils, ZMDlg, ZMCtx,
  ZMMsgStr, ZMMsg, ZMWorkFile, ZMDrv, ZMMatch,
  ZMEOC, ZMWFuncs, Controls;

const
  __UNIT__ = 34 shl 23;

const
  DeleteIncludeListThreshold = 20;
  MergeIncludeListThreshold  = 10;
  PrepareAppendThreshold     = 10;

const
  SkipConsts: array [TZMSkipTypes] of Integer = (Ord(stOnFreshen),
    Ord(stNoOverwrite), Ord(stFileExists), Ord(stBadPassword), Ord(stBadName),
    Ord(stCompressionUnknown), Ord(stUnknownZipHost), Ord(stZipFileFormatWrong),
    Ord(stGeneralExtractError), Ord(stUser), Ord(stCannotDo), Ord(stNotFound),
    Ord(stNoShare), Ord(stNoAccess), Ord(stNoOpen), Ord(stDupName),
    Ord(stReadError), Ord(stSizeChange), Ord(stNothingToDo));

constructor TZMZippedOpr.Create(theComponent: TCustomZipMaster);
begin
  inherited;
end;

procedure TZMZippedOpr.AddZippedFiles(SrcLister: TZMLister;
  Merge: TZMMergeOpts);
const
  __ERR_GE_NoZipSpecified = __UNIT__ + (241 shl 10) + GE_NoZipSpecified;
  __ERR_CF_SourceIsDest = __UNIT__ + (244 shl 10) + CF_SourceIsDest;
var
  AString: string;
  DestName: string;
  err: Integer;
  I: Integer;
  SrcName: string;
begin
  // validate dest
  DestName := Lister.ZipFileName;
  SrcName := SrcLister.ZipFileName;
  if (DestName = '') or (SrcName = '') then
    raise EZipMaster.CreateMsgDisp(__ERR_GE_NoZipSpecified, true);
  // Are source and destination different?
  if IsSameFile(SrcName, DestName) then
    raise EZipMaster.CreateMsgDisp(__ERR_CF_SourceIsDest, true);
  IncludeSpecs.Clear;
  ExcludeSpecs.Clear;
  // prepare list of file specs to copy
  AString := FlattenExcludes(SrcLister.ExcludeSpecs);
  if AString <> '' then
  begin
    AString := '/e:' + AString;
    IncludeSpecs.Add(AString);
  end;
  IncludeSpecs.Add(SrcName + ' >> ' + SrcLister.IncludeSpecs[0]);
  for I := 1 to SrcLister.IncludeSpecs.Count - 1 do
    IncludeSpecs.Add('>> ' + SrcLister.IncludeSpecs[I]);
  err := MergeZippedFiles1(Merge, nil);
  // did it work?
  if err >= 0 then
  begin
    if not Lister.IsDetachedSFX(DestName) then
    begin
      // try to load the destination
      Lister.LoadZip(DestName, false);
    end;
  end;
  if (err >= 0) then
    FixResults; // fix result lists
end;

procedure TZMZippedOpr.AfterConstruction;
begin
  inherited;
  fIsDestructing := false;
  FSkippedFiles := nil;
  FZipList := TZMZipList.Create(Self);
  FSkippedFiles := TStringList.Create;
  FSplitter := TZMArgSplitter.Create(Self);
end;

procedure TZMZippedOpr.BeforeDestruction;
begin
  FSkippedFiles.Free;
  FZipList.Free;
  inherited;
end;

procedure TZMZippedOpr.ClearAppend;
begin
  SetupAppendRecs(false); // clear flags
end;

function TZMZippedOpr.CommitAppend: Integer;
var
  DstZip: TZMZipFile;
begin
  DstZip := FZipList[0];
  FOutZip.FileName := DstZip.FileName;
  Result := FOutZip.File_Reopen(fmOpenReadWrite);
  if Result >= 0 then
    Result := FOutZip.Commit(zwoZipTime in WriteOptions);
end;

function TZMZippedOpr.CopyZippedDelete: Integer;
var
  SavedDone: TStringList;
  SavedSkipped: TStringList;
begin
  // delete the copied files
  SavedSkipped := nil;
  SavedDone := TStringList.Create;
  try
    // save done and SavedSkipped files
    SavedDone.AddStrings(IncludeSpecs);
    SavedDone.Assign(IncludeSpecs);
    SavedSkipped := TStringList.Create;
    SavedSkipped.Assign(ExcludeSpecs);
    ExcludeSpecs.Clear;
    Result := Delete1; // delete from current zip
    IncludeSpecs.Clear;
    IncludeSpecs.Assign(SavedDone); // restore done files
    ExcludeSpecs.Clear;
    ExcludeSpecs.Assign(SavedSkipped);
  finally
    SavedDone.Free;
    SavedSkipped.Free;
  end;
  // Update the Zip Directory by calling List method
  // for spanned exe avoid swapping to last disk
  Lister.Reload := zlrReload; // force reload;
end;

(*
  Enter FSpecArgs and FSpecArgsExcl specify files to be copied
  Exit FSpecArgs = files copied
  FSpecArgsExcl = files SavedSkipped
*)
procedure TZMZippedOpr.CopyZippedFiles(DstLister: TZMLister;
  DeleteFromSource: Boolean; OverwriteDest: TZMMergeOpts);
const
  __ERR_GE_NoZipSpecified = __UNIT__ + (355 shl 10) + GE_NoZipSpecified;
  __ERR_CF_SourceIsDest = __UNIT__ + (358 shl 10) + CF_SourceIsDest;
var
  AString: string;
  DestName: string;
  DstZip: TZMZipFile;
  err: Integer;
  I: Integer;
  SrcName: string;
begin
  // validate dest
  DestName := DstLister.ZipFileName;
  SrcName := Lister.ZipFileName;
  if (DestName = '') or (SrcName = '') then
    raise EZipMaster.CreateMsgDisp(__ERR_GE_NoZipSpecified, true);
  // Are source and destination different?
  if IsSameFile(SrcName, DestName) then
    raise EZipMaster.CreateMsgDisp(__ERR_CF_SourceIsDest, true);
  IncludeSpecs[0] := SrcName + ' >> ' + IncludeSpecs[0];
  for I := 1 to IncludeSpecs.Count - 1 do
    IncludeSpecs[I] := ' >> ' + IncludeSpecs[I];
  // prepare list of file specs to copy
  AString := FlattenExcludes(ExcludeSpecs);
  if AString <> '' then
  begin
    AString := '/e:' + AString;
    IncludeSpecs.Insert(0, AString);
  end;
  ExcludeSpecs.Clear;
  // load destination zip to work on
  Lister.Set_ZipFileName(DestName, zloSilent);
  if ErrCode <> 0 then
  begin
    if ErrCode < 0 then
      ErrCode := -ErrCode;
    if (ErrCode and MSG_ID_MASK) <> DS_NoInFile then
      exit;
    ErrCode := 0;
  end;
  DstZip := PrepareDest(DstLister);
  err := MergeZippedFiles1(OverwriteDest, DstZip);
  // did it work?
  if err >= 0 then
  begin
    if not Lister.IsDetachedSFX(DestName) then
      DstLister.LoadZip(DestName, false);  // try to load the destination
  end;
  // restore source  //? on errors too?
  Lister.Set_ZipFileName(SrcName, zloSilent);
  if (err >= 0) then
  begin
    // fix result lists
    FixResults;
    if DeleteFromSource then
      CopyZippedDelete;  // delete the copied files
  end;
end;

function TZMZippedOpr.Delete: Integer;
const
  __ERR_DL_NothingToDel = __UNIT__ + (407 shl 10) + DL_NothingToDel;
begin
  if (Lister <> nil) and (Lister.ZipFileName <> '') then
  begin
    // do it the old way
    if (Lister.Current.Count < 1) or (IncludeSpecs.Count = 0) then
      Result := -__ERR_DL_NothingToDel
    else
      Result := Delete1;
    // Update the Zip Directory by calling List method
    // for spanned exe avoid swapping to last disk
    if ((-Result) and MSG_ID_MASK) <> DL_NothingToDel then
      Lister.Reload := zlrReload;
  end
  else
    Result := DeleteByList;
  if Result < 0 then
    ShowZipMessage(-Result, '')
  else
    SuccessCnt := Result;
end;

(* ? TZMModOpr.Delete1
  Deletes files specified in FSpecArgs from current Zip
  exit: FSpecArgs = files deleted,
  FSpecArgsExcl = files skipped
  Result = >=0 number of files deleted, <0 error
*)
function TZMZippedOpr.Delete1: Integer;
const
  __ERR_GE_NoSkipping = __UNIT__ + (450 shl 10) + GE_NoSkipping;
  __ERR_DL_NothingToDel = __UNIT__ + (456 shl 10) + DL_NothingToDel;
var
  BeforeCnt: Integer;
  CurZip: TZMZipFile;
  DelCnt: Integer;
  idx: Integer;
  SkippedFiles: TStringList;
begin
  CurZip := Prepare(true); // prepare the Current zip
  Result := 0;
  SkippedFiles := TStringList.Create;
  try
    DelCnt := CurZip.SelectFiles(IncludeSpecs, ExcludeSpecs, SkippedFiles);
    IncludeSpecs.Clear; // will contain files deleted
    ExcludeSpecs.Clear; // will contain files skipped
    for idx := 0 to SkippedFiles.Count - 1 do
    begin
      if ReportSkippingEx(SkippedFiles[idx], stNotFound, 0, ExcludeSpecs) then
        Result := -__ERR_GE_NoSkipping;
    end;
  finally
    SkippedFiles.Free;
  end;
  if (Result = 0) and (DelCnt <= 0) then
    Result := -__ERR_DL_NothingToDel;
  if Result = 0 then
  begin
    ASSERT(DelCnt = CurZip.SelCount, 'selcount wrong 1');
    if (CurZip.Count - DelCnt) < 1 then
    begin
      // no files left
      CurZip.File_Close;
      _Z_DeleteFile(CurZip.FileName);
      Result := DelCnt; // number of files deleted
    end
    else
    begin
      idx := -1; // from beginning
      while true do
      begin
        idx := CurZip.NextSelected(idx);
        if idx < 0 then
          break; // no more - finished
        IncludeSpecs.Add(CurZip[idx].FileName);
      end;
      BeforeCnt := CurZip.Count;
      CurZip.Select('*', zzsToggle); // select entries to keep
      ASSERT((CurZip.Count - DelCnt) = CurZip.SelCount, 'selcount wrong 2');
      // write the result
      Result := Remake(CurZip, CurZip.Count - DelCnt, false);
      if Result >= 0 then
        Result := BeforeCnt - Result; // if no error
    end;
  end;
  CurZip.Invalidate;
  Lister.Current := nil; // force reload
end;

function TZMZippedOpr.DeleteByList: Integer;
const
  __ERR_AZ_NothingToDo = __UNIT__ + (502 shl 10) + AZ_NothingToDo;
  __ERR_AZ_NothingToDo1 = __UNIT__ + (514 shl 10) + AZ_NothingToDo;
var
  I: Integer;
  SelectCount: Integer;
  ShowXProgress: Boolean;
  Zip: TZMZipFile;
begin
  ShowProgress := zspFull; // zspNone;
  if IncludeSpecs.Count < 1 then
    raise EZipMaster.CreateMsgDisp(__ERR_AZ_NothingToDo, true);
  FZipList.Clear;
  FSkippedFiles.Clear;
  FZipList.ProtectZero := false;
  // add zips to list and select their files
  Result := ProcessDeleteList(SelectCount);
  if Result < 0 then
    raise EZipMaster.CreateMsgDisp(-Result, true);
  if SelectCount < 1 then
  begin
    if Verbosity >= zvVerbose then
      Diag('nothing selected');
    ShowZipMessage(__ERR_AZ_NothingToDo1, '');
    Result := -__ERR_AZ_NothingToDo1;
    exit;
  end;
  IncludeSpecs.Clear; // will contain files deleted
  ExcludeSpecs.Clear; // will contain files skipped
  ShowXProgress := FZipList.Count > MergeIncludeListThreshold;
  if ShowXProgress then
    ReportProgress(zacXItem, PR_Processing, '', FZipList.Count);
  for I := 0 to FZipList.Count - 1 do
  begin
    if ShowXProgress then
      ReportProgress(zacXProgress, PR_Processing, '', 1);
    CheckCancel;
    Zip := FZipList[I];
    Result := DeleteSelectedFiles(Zip);
  end;
end;

function TZMZippedOpr.DeleteSelectedFiles(Zip: TZMZipFile): Integer;
const
  __ERR_DL_NothingToDel = __UNIT__ + (544 shl 10) + DL_NothingToDel;
var
  BeforeCnt: Integer;
  DelCnt: Integer;
  idx: Integer;
begin
  Result := PrepareZip(Zip); // prepare the Current zip
  DelCnt := Zip.SelCount;
  if (Result = 0) and (DelCnt <= 0) then
    Result := -__ERR_DL_NothingToDel;
  if Result = 0 then
  begin
    if (Zip.Count - Zip.SelCount) < 1 then
    begin
      // no files left
      Zip.File_Close;
      _Z_DeleteFile(Zip.FileName);
      Result := DelCnt; // number of files deleted
    end
    else
    begin
      idx := -1; // from beginning
      while true do
      begin
        idx := Zip.NextSelected(idx);
        if idx < 0 then
          break; // no more - finished
        IncludeSpecs.Add(Zip[idx].FileName);
      end;
      BeforeCnt := Zip.Count;
      Zip.Select('*', zzsToggle); // write entries not selected for deletion
      ASSERT((Zip.Count - DelCnt) = Zip.SelCount, 'selcount wrong 2');
      Result := Remake(Zip, Zip.Count - DelCnt, false); // write the result
      if Result >= 0 then
        Result := BeforeCnt - Result; // if no error
    end;
  end;
end;

// do TZMCopyZippedOverwriteEvent from TZMMergeZippedConflictEvent
procedure TZMZippedOpr.DoMergeEvent(Sender: TObject;
  Existing, Conflicting: TZMConflictEntry; var Resolve: TZMResolutions);
var
  tmpCopyZippedConflict: TZMCopyZippedOverwriteEvent;
  WantSrc: Boolean;
begin
  WantSrc := false; // default keep existing
  // Do we have a event assigned for this then don't ask.
  tmpCopyZippedConflict := Master.OnCopyZippedOverwrite;
  if assigned(tmpCopyZippedConflict) then
    tmpCopyZippedConflict(Master, Conflicting.Entry, Existing.Entry, WantSrc)
  else
    if ZipMessageDlgEx('', Format(ZipLoadStr(CF_OverwriteYN),
      [Conflicting.ZipName, Existing.ZipName]), zmtConfirmation + DHC_CpyZipOvr,
      [mbYes, mbNo]) = idYes then
      WantSrc := true;
  if WantSrc then
    Resolve := zmrConflicting
  else
    Resolve := zmrExisting;
end;

procedure TZMZippedOpr.Finished(WasGood: Boolean);
begin
  inherited;
end;

procedure TZMZippedOpr.FixResults;
const
  Unwanted = '<<stFileExists';
var
  AString: string;
  I: Integer;
  Line: string;
  zn: string;
begin
  for I := 0 to IncludeSpecs.Count - 1 do
  begin
    SplitQualifiedName(IncludeSpecs[I], zn, AString);
    IncludeSpecs[I] := AString;
  end;
  I := 0;
  while I < ExcludeSpecs.Count do
  begin
    Line := ExcludeSpecs[I];
    if Pos(Unwanted, Line) > 0 then
    begin
      ExcludeSpecs.Delete(I);
      Continue;
    end;
    SplitQualifiedName(Line, zn, AString);
    if AString = '' then
      AString := Line; // no entry name so use zip name
    ExcludeSpecs[I] := AString;
    Inc(I);
  end;
end;

function TZMZippedOpr.FlattenExcludes(Excludes: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  // flatten the list
  for I := 0 to Excludes.Count - 1 do
  begin
    if Excludes[I] = '' then
      Continue;
    if Result <> '' then
      Result := Result + SPEC_SEP;;
    Result := Result + Excludes[I];
  end;
end;

// opens a zip file for merging and adds to FZipList
// return <0 _ error, >= 0 _ index in FZipList
function TZMZippedOpr.MergeAnotherZip(const SourceName: String): Integer;
begin
  if Pos(ZFILE_SEPARATOR, SourceName) > 0 then
    Result := MergeAnotherZipInZip(SourceName) // handle Zip in Zip
  else
    Result := MergeAnotherZip1(SourceName);
end;

// opens a zip file for merging and adds to FZipList
// return <0 _ error, >= 0 _ index in FZipList
function TZMZippedOpr.MergeAnotherZip1(const SourceName: String): Integer;
const
  __ERR_AZ_SameAsSource = __UNIT__ + (681 shl 10) + AZ_SameAsSource;
var
  Dzip: TZMZipFile;
  SrcZip: TZMZipFile;
begin
  // have not opened file yet
  SrcZip := TZMZipFile.Create(Master, Self);
  try
    begin
      SrcZip.FileName := SourceName;
      Result := SrcZip.Open(false, false);
      if Result >= 0 then
      begin
        Dzip := FZipList[0];
        if (SrcZip.WorkDrive.DriveLetter = Dzip.WorkDrive.DriveLetter) and
          (not Dzip.WorkDrive.DriveIsFixed) and
          (Dzip.MultiDisk or SrcZip.MultiDisk or
          (zwoDiskSpan in WriteOptions)) then
          Result := -__ERR_AZ_SameAsSource
        else
        begin
          Result := FZipList.Add(SrcZip); // add to list and return index
          SrcZip.File_Close;
          if Result > 0 then
            SrcZip := nil; // accepted so do not free
        end;
      end;
    end;
  finally
    SrcZip.Free;
  end;
end;

// handle Zip in Zip
function TZMZippedOpr.MergeAnotherZipInZip(const SourceName: String): Integer;
var
  MyName: string;
  MyOwner: string;
begin
  SplitQualifiedName(SourceName, MyOwner, MyName);
  // find MyOwner
  Result := FZipList.Find(MyOwner);
  if Result < 0 then
  begin
    // not found _ maybe MyOwner not added yet
    Result := MergeAnotherZip(MyOwner); // recursively add parents
  end;
  if Result > 0 then
  begin
    // we have owner in FZipList
    // open MyOwner and extract MyName to temporary
    Result := ExtractChildZip(Result, MyName);
  end;
end;

// create the intermediate from DstZip and selected entries
function TZMZippedOpr.MergeIntermediate(SelectedCount: Integer;
  const Opts: TZMMergeOpts): Integer;
var
  DstZip: TZMZipFile;
  RefZip: TZMZipFile;
  RefZipIndex: Integer;
  SelCount: Integer;
begin
  Result := 0;
  DstZip := FZipList[0];
  FOutZip := TZMZipMerge.Create(Self.Master, Self);
  // replicate all records and settings
  FOutZip.Select('*', zzsSet); // want all (initially)
  FOutZip.Capacity := DstZip.Count + SelectedCount; // make sure there is room
  FOutZip.QuickSearches := false; // do it after adding new entries
  ReportProgress(zacXItem, PR_Merging, '', FZipList.Count);
  // add the possibly altered selected entries
  for RefZipIndex := 0 to FZipList.Count - 1 do
  begin
    ReportProgress(zacXProgress, PR_Processing, '', 1);
    CheckCancel;
    RefZip := FZipList[RefZipIndex];
    SelCount := RefZip.SelCount;
    if Verbosity >= zvVerbose then
      Diag('Processing ' + IntToStr(SelCount) + ' files: ' + RefZip.Name);
    if SelCount < 1 then
      Continue;
    Result := MergeIntermedZip(RefZip, RefZipIndex, Opts);
  end;
  if Result >= 0 then
  begin
    FOutZip.QuickSearches := true;
    Result := FOutZip.HashContents(0, true); // also detects duplicate names
  end;
end;

function TZMZippedOpr.MergeIntermedZip(RefZip: TZMZipFile; ZipIndex: Integer;
  Opts: TZMMergeOpts): Integer;
var
  Cnt: Integer;
  Index: Integer;
  NewRec: TZMMergeRec;
  RefIndex: Integer;
  RefRec: TZMCopyRec;
  skp: TZMSkipTypes;
  ZName: string;
  ZRec: TZMZRec;
begin
  Result := 0; // keep compiler happy
  // process each selected entry
  Index := -1;
  // while true do
  Cnt := 0;
  while not Master.Cancel do
  begin
    Inc(Cnt);
    if (Cnt and 63) = 0 then
      CheckCancel;
    Result := 0;
    Index := RefZip.NextSelected(Index);
    if Index < 0 then
      break;
    ZRec := RefZip[Index];
    ZName := ZRec.FileName;
    if ZipIndex > 0 then
    begin
      Result := MergePrepareName(ZName, ZRec);
      if Result < 0 then
        break; // error
      if ZName = '' then
        Continue; // entry ignored by user
    end;
    // make new CopyRec
    NewRec := TZMMergeRec.Create(FOutZip); // make a entry
    NewRec.AssignFrom(ZRec);
    NewRec.Link := ZRec; // link to original
    if Result = 1 then
    begin
      NewRec.SetStatusBit(zsbRenamed);
      Result := NewRec.ChangeName(ZName);
      if Result < 0 then
      begin
        NewRec.Free; // stop leak
        Diag('Failed rename [' + IntToStr(-Result) + '] ' + ZRec.FileName +
          ' to ' + ZName);
        skp := stUser;
        case ((-Result) and MSG_ID_MASK) of
          AD_BadFileName:
            skp := stBadName;
          AD_DuplFileName:
            skp := stDupName;
        end;
        if skp = stUser then
          break; // unknown error - fatal
        if ReportSkippingEx(QualifiedName(RefZip.Name, ZName), skp, -Result,
          FSkippedFiles) then
          break; // fatal
        Continue; // ignore
      end;
      Diag('Renamed ' + ZRec.FileName + ' to ' + NewRec.FileName);
    end;
    // does it exist
    RefIndex := -1;
    repeat
      RefRec := TZMCopyRec(FOutZip.FindNameEx(ZName, RefIndex, false));
      // only check non-discarded entries
    until ((RefRec = nil) or (RefRec.StatusBit[zsbDiscard] = 0));
    Result := 0;
    if RefRec <> nil then
    begin
      // duplicate found - resolve it
      Result := MergeResolveConflict(RefRec, NewRec, Opts);
    end;
    if Result = 0 then
      Result := FOutZip.Add(NewRec);
    if Result < 0 then
    begin
      NewRec.Free; // stop leak
      break;
    end;
  end;
end;

// returns <0 = error, 0 = no change, 1 = changed, 2 = ignored by user
function TZMZippedOpr.MergePrepareName(var ZName: TZMString;
  ZRec: TZMZRec): Integer;
const
  __ERR_GE_NoSkipping = __UNIT__ + (880 shl 10) + GE_NoSkipping;
var
  Changed: Boolean;
  FileName: string;
  old: string;
  sep: Integer;
  subst: string;
  tmpOnSetAddName: TZMSetAddNameEvent;
  XArg: string;
  ZipName: string;
begin
  Result := 0;
  ZName := ZRec.FileName;
  XArg := ZRec.SelectArgs;
  ZipName := ZRec.MyFile.Name; // FileName;
  if (XArg <> '') and (XArg[1] = SPEC_SEP) then
    XArg := Copy(XArg, 2, Length(XArg) - 1) // remove flag
  else
  begin
    tmpOnSetAddName := Master.OnSetAddName;
    if assigned(tmpOnSetAddName) then
    begin
      Changed := false;
      FileName := ZName;
      tmpOnSetAddName(Master, FileName, QualifiedName(ZipName,
        FileName), Changed);
      if Changed then
      begin
        Result := 1; // changed
        // verify valid
        if FileName = '' then
        begin
          if ReportSkippingEx(QualifiedName(ZipName, ZRec.FileName), stUser, 0,
            FSkippedFiles) then
            Result := -__ERR_GE_NoSkipping
          else
            Result := 2; // ignore entry
          ZName := FileName;
          exit;
        end;
        FileName := SetSlash(FileName, psdExternal);
        ZName := FileName;
      end;
    end;
  end;
  if XArg <> '' then
  begin
    old := XArg;
    sep := Pos('::', old);
    subst := Copy(old, sep + 2, 2048);
    old := Copy(old, 1, sep - 1);
    if old = '' then
      FileName := subst + ZName
    else
    begin
      old := SetSlash(old, psdExternal);
      if Pos(old, ZName) <= 0 then
      begin
        // no change
        Result := 0;
        exit;
      end;
      FileName := StringReplace(ZName, old, subst,
        [rfReplaceAll, rfIgnoreCase]);
    end;
    FileName := SetSlash(FileName, psdExternal);
    Result := 1; // changed
    // verify valid
    ZName := FileName;
  end;
end;

// return <0 = error, 0 = use NRec, 1 = discard NRec
function TZMZippedOpr.MergeResolveConflict(RefRec, NRec: TZMCopyRec;
  const Opts: TZMMergeOpts): Integer;
const
  __ERR_GE_NoSkipping = __UNIT__ + (962 shl 10) + GE_NoSkipping;
  __ERR_GE_NoSkipping1 = __UNIT__ + (975 shl 10) + GE_NoSkipping;
  __ERR_GE_NoSkipping2 = __UNIT__ + (991 shl 10) + GE_NoSkipping;
var
  ConflictZipName: string;
  NewName: string;
  RefZipName: string;
  Resolve: TZMResolutions;
begin
  RefZipName := RefRec.Link.MyFile.Name;
  ConflictZipName := NRec.Link.MyFile.Name;
  if Verbosity > zvVerbose then
    Diag('Found conflict for ' + QualifiedName(RefZipName, RefRec.FileName) +
      ' in ' + ConflictZipName);
  Resolve := zmrConflicting;
  NewName := MergeSafeName(1, RefRec.FileName);
  case Opts of
    zmoConfirm:
      Resolve := ResolveConfirm(RefZipName, RefRec, ConflictZipName,
        NRec, NewName);
    zmoAlways:
      ;
    zmoNewer:
      if RefRec.ModifDateTime >= NRec.ModifDateTime then
        Resolve := zmrExisting;
    zmoOlder:
      if RefRec.ModifDateTime <= NRec.ModifDateTime then
        Resolve := zmrExisting;
    zmoNever:
      Resolve := zmrExisting;
    zmoRename:
      Resolve := zmrRename;
  end;
  Result := 0;
  case Resolve of
    zmrExisting:
      begin
        // zmrExisting _ keep RefRec, skip (delete) NRec
        if ReportSkippingEx(QualifiedName(ConflictZipName, NRec.FileName),
          stFileExists, 0, FSkippedFiles) then
          Result := -__ERR_GE_NoSkipping
        else
        begin
          NRec.Selected := false;
          NRec.SetStatusBit(zsbDiscard);
          Result := 1; // discard NRec
        end;
      end;
    zmrConflicting:
      begin
        // zmrConflicting _ skip (delete) RefRef, keep NRec
        if ReportSkippingEx(QualifiedName(RefZipName, RefRec.FileName),
          stFileExists, 0, FSkippedFiles) then
          Result := -__ERR_GE_NoSkipping1
        else
        begin
          RefRec.Selected := false;
          RefRec.SetStatusBit(zsbDiscard);
        end;
      end;
    zmrRename:
      begin
        // zmrRename _ keep RefRec, rename and keep NRec
        NRec.SetStatusBit(zsbRenamed);
        Result := NRec.ChangeName(NewName);
        if (Result < 0) then
        begin
          if ReportSkippingEx(QualifiedName(ConflictZipName, NewName),
            stFileExists, 0, FSkippedFiles) then
            Result := -__ERR_GE_NoSkipping2
          else
            Result := 1; // ignore
        end
        else
        begin
          NRec.Selected := true; // false;
          NRec.SetStatusBit(zsbRenamed);
          Result := 0;
        end;
      end;
  end;
end;

function TZMZippedOpr.MergeSafeName(ZipIndex: Integer;
  const Name: string): string;
var
  EName: string;
  Extn: string;
  idx: Integer;
  N: Cardinal;
begin
  EName := ExtractFilePath(Name) + ExtractNameOfFile(Name);
  Extn := ExtractFileExt(Name);
  N := 0;
  repeat
    if N = 0 then
      Result := Format('%s[%d]%s', [EName, ZipIndex, Extn])
    else
      Result := Format('%s[%d.%x]%s', [EName, ZipIndex, N, Extn]);
    Inc(N);
    idx := -1; // from begining
  until (FOutZip.FindName(Result, idx) = nil);
end;

function TZMZippedOpr.MergeWrite: Integer;
const
  __ERR_GE_NoSkipping = __UNIT__ + (1092 shl 10) + GE_NoSkipping;
  __ERR_DS_NoOutFile = __UNIT__ + (1115 shl 10) + DS_NoOutFile;
  __ERR_DS_NoOutFile1 = __UNIT__ + (1154 shl 10) + DS_NoOutFile;
  __ERR_AZ_InternalError = __UNIT__ + (1176 shl 10) + AZ_InternalError;
var
  DstZip: TZMZipFile;
  existed: Boolean;
  I: Integer;
  Rec: TZMCopyRec;
  RenamedCnt: Integer;
  ReplaceCnt: Integer;
  Rubbish: Integer;
  S: string;
  SourceZip: TZMWorkZip;
  SrcName: string;
  TotalCnt: Integer;
  WillSplit: Boolean;
begin
  // find total files in dest and list of files added/replaced
  DstZip := FZipList[0];
  IncludeSpecs.Clear; // add names of files to be added
  TotalCnt := 0;
  ReplaceCnt := 0;
  Rubbish := 0;
  RenamedCnt := 0;
  for I := 0 to FOutZip.Count - 1 do
  begin
    CheckCancel;
    Rec := TZMCopyRec(FOutZip[I]);
    if (Rec = nil) or (Rec.StatusBit[zsbError or zsbDiscard or zsbSelected] <>
      zsbSelected) then
    begin
      Inc(Rubbish);
      Continue;
    end;
    SourceZip := Rec.Link.MyFile;
    if Rec.StatusBit[zsbDiscard] <> 0 then
    begin
      if SourceZip = DstZip then
        Inc(ReplaceCnt);
      Inc(Rubbish);
      Continue;
    end;
    Inc(TotalCnt);
    if SourceZip = DstZip then
      Continue;
    // add names to list
    SrcName := SourceZip.FileName;
    S := QualifiedName(SourceZip.Name, Rec.Link.FileName);
    if Rec.StatusBit[zsbRenamed] <> 0 then
    begin
      S := Rec.FileName + '<<' + S;
      Inc(RenamedCnt);
    end;
    IncludeSpecs.Add(S);
  end;
  if Verbosity >= zvVerbose then
    Diag(Format('Total=%d, Replaced=%d, New=%d, Discarded=%d, Renamed=%d',
      [TotalCnt, ReplaceCnt, IncludeSpecs.Count, Rubbish, RenamedCnt]));
  if (TotalCnt < 1) or (IncludeSpecs.Count < 1) then
  begin
    if Verbosity >= zvVerbose then
      Diag('nothing to do');
    if ReportSkippingEx(DstZip.Name, stNothingToDo, 0, FSkippedFiles) then
      Result := -__ERR_GE_NoSkipping
    else
      Result := 0;
    exit;
  end;
  // can we just append to original
  existed := (zfi_Loaded and DstZip.info) <> 0;
  WillSplit := DstZip.MultiDisk or
    ((not existed) and (zwoDiskSpan in DstZip.WriteOptions));

  if (not WillSplit) or (not(zwoSafe in DstZip.WriteOptions)) then
  begin
    if not existed then
    begin
      // write new file
      FOutZip.FileName := DstZip.FileName;
      FOutZip.ZipComment := Master.ZipComment; // keep orig
      FOutZip.ShowProgress := zspFull;
      if assigned(DstZip.stub) and DstZip.UseSFX then
      begin
        FOutZip.AssignStub(DstZip);
        FOutZip.UseSFX := true;
      end;
      Result := -__ERR_DS_NoOutFile;
      FOutZip.File_Create(DstZip.FileName);
      if FOutZip.IsOpen then
      begin
        Result := FOutZip.Commit(zwoZipTime in WriteOptions);
        FOutZip.File_Close;
        FZipList.CloseAll; // close all source files
      end;
      if (Result < 0) and (Verbosity >= zvVerbose) then
        Diag(Format('Merging new file failed: %d', [-Result]));
      exit;
    end
    else
    begin
      // try to append to existing zip
      if PrepareAppend then
      begin
        // commit it
        Result := CommitAppend;
        if (Verbosity >= zvVerbose) and (Result >= 0) then
          Diag('Merging append successful');
        if ((-Result) and MSG_ID_MASK) <> DS_NoAppend then
          exit;
        // fix to allow safe write
        ClearAppend;
        if Verbosity >= zvVerbose then
          Diag(Format('Merging append failed: %d', [-Result]));
      end;
    end;
  end;

  // write to intermediate
  if WillSplit then
    FOutZip.File_CreateTemp(PRE_INTER, '')
  else
    // begin
    FOutZip.File_CreateTemp(PRE_INTER, DstZip.FileName);
  if not FOutZip.IsOpen then
  begin
    Result := -__ERR_DS_NoOutFile1;
    exit;
  end;
  if not WillSplit then
  begin
    // initial temporary destination
    if assigned(DstZip.stub) and DstZip.UseSFX then
    begin
      FOutZip.AssignStub(DstZip);
      FOutZip.UseSFX := true;
    end;
    FOutZip.DiskNr := 0;
  end;
  FOutZip.ZipComment := DstZip.ZipComment; // keep orig
  FOutZip.ShowProgress := zspFull;
  // now we write it
  Result := FOutZip.Commit(zwoZipTime in WriteOptions);
  FOutZip.File_Close;
  FZipList.CloseAll; // close all source files
  if Result >= 0 then
  begin
    if (FOutZip.Count - Rubbish) <> TotalCnt then
      Result := -__ERR_AZ_InternalError
    else
    begin
      // all correct so Recreate source
      Result := Recreate(FOutZip, DstZip);
    end;
  end;
end;

// IncludeSpecs = Zips and files to include
// zipname
// ExcludeSpecs = files to exclude
function TZMZippedOpr.MergeZippedFiles(Opts: TZMMergeOpts;
  TheDest: TZMZipFile): Integer;
const
  __ERR_GE_InvalidArguments = __UNIT__ + (1200 shl 10) + GE_InvalidArguments;
  __ERR_GE_NoZipSpecified = __UNIT__ + (1202 shl 10) + GE_NoZipSpecified;
var
  DstZip: TZMZipFile;
  SelectCount: Integer;
begin
  ShowProgress := zspFull;
  FOutZip := nil; // will be used to write the output
  if IncludeSpecs.Count < 1 then
    raise EZipMaster.CreateMsgDisp(__ERR_GE_InvalidArguments, true);
  if Master.ZipFileName = '' then
    raise EZipMaster.CreateMsgDisp(__ERR_GE_NoZipSpecified, true);
  FZipList.Clear;
  FSkippedFiles.Clear;
  if TheDest <> nil then
  begin
    DstZip := TheDest;
  end
  else
    DstZip := Prepare(false, true);
  DstZip.Select('*', zzsSet); // initial want all
  FZipList.ProtectZero := true; // do not destroy DstZip
  FZipList.Add(DstZip);
  // add source zips to list and select their files
  Result := ProcessIncludeList(SelectCount);
  if Result < 0 then
    raise EZipMaster.CreateMsgDisp(-Result, true);
  IncludeSpecs.Clear; // for new/updated files
  if SelectCount >= 1 then
  begin
    // add all selected to OutZip
    Result := MergeIntermediate(SelectCount, Opts);
    if Result < 0 then
      raise EZipMaster.CreateMsgDisp(-Result, ((-Result) and MSG_ID_MASK) <> GE_Abort);
    // we have processed list now resolve merge conflicts
    // write the results
    if Result >= 0 then
      Result := MergeWrite;
    // Update the Zip Directory by calling List method
    // for spanned exe avoid swapping to last disk
    Lister.Reload := zlrReload; // force reload
  end;
  if Result < 0 then
    raise EZipMaster.CreateMsgDisp(-Result, ((-Result) and MSG_ID_MASK) <> GE_Abort);
  // it was successful
  SuccessCnt := IncludeSpecs.Count;
  FZipList.Clear;
  ExcludeSpecs.Clear;
  ExcludeSpecs.Assign(FSkippedFiles); // does it copy object pointers?
  FSkippedFiles.Clear;
end;

function TZMZippedOpr.MergeZippedFiles1(Opts: TZMMergeOpts;
  TheDest: TZMZipFile): Integer;
var
  tmpMergeZippedConflictEvent: TZMMergeZippedConflictEvent;
begin
  if Opts = zmoConfirm then
  begin
    tmpMergeZippedConflictEvent := Master.OnMergeZippedConflict;
    try
      Master.OnMergeZippedConflict := DoMergeEvent;
      Result := MergeZippedFiles(Opts, TheDest);
    finally
      Master.OnMergeZippedConflict := tmpMergeZippedConflictEvent;
    end;
  end
  else
    Result := MergeZippedFiles(Opts, TheDest);
end;

(* TZMZippedOpr.Prepare
  Prepare destination and get SFX stub as needed
*)
function TZMZippedOpr.Prepare(MustExist: Boolean; SafePart: Boolean = false)
  : TZMZipFile;
var
  err: Integer;
begin
  Result := Lister.CurrentZip(MustExist, SafePart);
  err := PrepareZip(Result);
  if err < 0 then
    raise EZipMaster.CreateMsgDisp(-err, true);
end;

// prepare to commit by appending to orig file, return false if not possible
function TZMZippedOpr.PrepareAppend: Boolean;
var
  DstZip: TZMZipFile;
  HighKept: Int64;
  I: Integer;
  LocalOfs: Int64;
  LowDiscard: Int64;
  OrigCnt: Integer;
  Rec: TZMMergeRec;
  ShowXProgress: Boolean;
begin
  Result := false;
  DstZip := FZipList[0];
  OrigCnt := DstZip.Count;
  if OrigCnt < 1 then
    exit;
  // check can append
  ShowXProgress := OrigCnt > PrepareAppendThreshold;
  if ShowXProgress then
    ReportProgress(zacXItem, PR_Processing, '', IncludeSpecs.Count);
  LowDiscard := DstZip.SOCOfs;
  HighKept := -1;
  for I := 0 to OrigCnt - 1 do
  begin
    if ShowXProgress then
      ReportProgress(zacXProgress, PR_Processing, '', 1);
    CheckCancel;
    Rec := TZMMergeRec(FOutZip[I]);
    if (Rec = nil) or (Rec.StatusBit[zsbError] <> 0) then
      Continue;
    LocalOfs := Rec.RelOffLocalHdr;
    if Rec.StatusBit[zsbDiscard] <> 0 then
    begin
      if LocalOfs < LowDiscard then
      begin
        LowDiscard := LocalOfs;
        if HighKept > LowDiscard then
          exit; // would produce a hole
      end;
    end
    else
    begin
      if LocalOfs > HighKept then
      begin
        HighKept := LocalOfs;
        if HighKept > LowDiscard then
          exit; // would produce a hole
      end;
    end;
  end;
  if Verbosity >= zvVerbose then
    Diag('Should be able to append');
  SetupAppendRecs(true);
  Result := true;
end;

function TZMZippedOpr.PrepareDest(DestLister: TZMLister): TZMZipFile;
var
  err: Integer;
begin
  Result := DestLister.CurrentZip(false, false);
  err := PrepareZip(Result);
  if err < 0 then
    raise EZipMaster.CreateMsgDisp(-err, true);
end;

function TZMZippedOpr.PrepareZip(Zip: TZMZipFile): Integer;
const
  __ERR_DS_NoUnattSpan = __UNIT__ + (1347 shl 10) + DS_NoUnattSpan;
begin
  Result := -__ERR_DS_NoUnattSpan;
  if Master.Unattended and not Zip.WorkDrive.DriveIsFixed then
    exit;
  if (Uppercase(ExtractFileExt(Zip.ReqFileName)) = EXT_EXE) then
  begin
    Zip.UseSFX := true;
    Zip.stub := NewSFXStub;
    Zip.UseSFX := true;
  end;
  Result := 0;
end;

function TZMZippedOpr.ProcessDelete(SrcZip: TZMZipFile;
  Arg, Excludes: string): Integer;
const
  __ERR_GE_NoSkipping = __UNIT__ + (1372 shl 10) + GE_NoSkipping;
begin
  if Verbosity >= zvVerbose then
    Diag('Selecting: ' + Arg + ' in: ' + SrcZip.FileName);
  Result := SrcZip.SelectFile(Arg, Excludes, zzsSet);
  if Result < 1 then
  begin
    // none found
    if ReportSkippingEx(QualifiedName(SrcZip.FileName, Arg), stNotFound, 0,
      FSkippedFiles) then
      Result := -__ERR_GE_NoSkipping;
  end;
end;

// returns <0 _ error
(*
  switches
  >>spec  before any source zip is specified sets the default select spec
  >>spec  select files in current zip according to spec
  /E:[|][spec[|spec]...]   set excludes, if starts with | it appends to globals otherwise use spec
  /E  same as /E:|    (use globals)
  changes to excludes occur at current line and continue until changed - does not change already selected files.
  when used on same line as '>>' it only applies to that line and modifies the 'current' excludes.
*)
function TZMZippedOpr.ProcessDeleteList(var SelectCount: Integer): Integer;
const
  __ERR_DS_NoInFile = __UNIT__ + (1416 shl 10) + DS_NoInFile;
  __ERR_GE_InvalidParameter = __UNIT__ + (1427 shl 10) + GE_InvalidParameter;
  __ERR_GE_NoSkipping = __UNIT__ + (1460 shl 10) + GE_NoSkipping;
var
  DefaultArg: TZMString;
  EffectiveExcludes: TZMString;
  GlobalExcludes: TZMString;
  I: Integer;
  LocalArg: string;
  LocalExcludes: string;
  ShowXProgress: Boolean;
  Skip: TZMSkipTypes;
  SourceName: string;
  xc: string;
  Zip: TZMZipFile;
  ZipsIndex: Integer;
begin
  Result := 0;
  SelectCount := 0;
  GlobalExcludes := FlattenExcludes(ExcludeSpecs);
  EffectiveExcludes := GlobalExcludes;
  FSplitter.Allow := '>E';
  DefaultArg := '';
  // locate source zips and their files
  SourceName := '';
  ShowXProgress := IncludeSpecs.Count > DeleteIncludeListThreshold;
  if ShowXProgress then
    ReportProgress(zacXItem, PR_Processing, '', IncludeSpecs.Count);
  ZipsIndex := -__ERR_DS_NoInFile; // no current yet
  for I := 0 to IncludeSpecs.Count - 1 do
  begin
    if ShowXProgress then
      ReportProgress(zacXProgress, PR_Processing, '', 1);
    CheckCancel;
    if Result < 0 then
      break;
    Result := 0;
    FSplitter.Raw := IncludeSpecs[I];
    if FSplitter.Error <> zasNone then
      raise EZipMaster.CreateMsgStr(__ERR_GE_InvalidParameter, FSplitter.Raw);
    if FSplitter.Main <> '' then
    begin
      SourceName := FSplitter.Main;
      ZipsIndex := FZipList.Find(SourceName);
      if ZipsIndex < 0 then
      begin
        // have not opened file yet
        if FileExists(SourceName) then
        begin
          Zip := TZMZipFile.Create(Master, Self);
          Zip.FileName := SourceName;
          ZipsIndex := Zip.Open(false, false);
          if ZipsIndex >= 0 then
            ZipsIndex := FZipList.Add(Zip) // add to list and return index
          else
            Zip.Free; // dispose of it (will happen anyway)
        end;
        if ZipsIndex < 0 then
        begin
          // file does not exist or could not be opened
          if Verbosity >= zvVerbose then
            Diag(Format('Skipped missing or bad zip [%d] %s',
              [-ZipsIndex, SourceName]));
//          Skip := stReadError;
          case ((-ZipsIndex) and MSG_ID_MASK) of
            DS_NoInFile: Skip := stNotFound;
            DS_FileOpen: Skip := stNoOpen;
            else
              Skip := stReadError;
          end;
          if ReportSkippingEx(SourceName, Skip, 0, FSkippedFiles) then
          begin
            Result := -__ERR_GE_NoSkipping;
            break;
          end;
          Result := 0; // we can ignore it
          Continue;
        end;
      end;
    end
    else // no zip specified
    begin
      // ignore empty lines
      if FSplitter.Found = '' then
        Continue;
      // any zips specified yet
      if ZipsIndex < 0 then
      begin
        // none yet, set defaults
        if FSplitter.Has('E') then
        begin
          xc := FSplitter.Arg('E');
          if (xc <> '') and (xc[1] = SPEC_SEP) then
            EffectiveExcludes := GlobalExcludes + xc
          else
            EffectiveExcludes := xc;
          GlobalExcludes := EffectiveExcludes;
        end;
        if FSplitter.Has(ZSPECARG) then
        begin
          DefaultArg := FSplitter.Arg(ZSPECARG);
        end;
        Continue;
      end;
    end;
    if (Result < 0) or (ZipsIndex < 0) then
      break; // must have open file
    if FSplitter.Has(ZSPECARG) or (FSplitter.Main <> '') then
    begin
      // include a spec in the current zip
      Zip := FZipList[ZipsIndex];
      LocalExcludes := EffectiveExcludes;
      if FSplitter.Has(ZSPECARG) then
      begin
        // include a spec in the current zip
        LocalArg := FSplitter.Arg(ZSPECARG);
      end;
      // check local overrides
      if FSplitter.Has('E') then
      begin
        xc := FSplitter.Arg('E');
        if (xc <> '') and (xc[1] = SPEC_SEP) then
          LocalExcludes := EffectiveExcludes + xc
        else
          LocalExcludes := xc;
      end;
      Result := ProcessDelete(Zip, LocalArg, LocalExcludes);
      if Result > 0 then
        SelectCount := SelectCount + Result;
      Continue;
    end;
    // Set effective
    if FSplitter.Has('E') then
    begin
      xc := FSplitter.Arg('E');
      if (xc <> '') and (xc[1] = SPEC_SEP) then
        EffectiveExcludes := GlobalExcludes + xc
      else
        EffectiveExcludes := xc;
    end;
  end;
end;

type
  TZMergeSelRec = record
    Owner: TZMZippedOpr;
    NXArg: TZMString;
    FromDate: Cardinal;
    ZipName: TZMString;
    list: TStrings;
  end;

  PZMergeSelRec = ^TZMergeSelRec;

function SelectedFunc(Rec: TZMZRec; var Data): Boolean;
var
  SelRecP: PZMergeSelRec;
begin
  SelRecP := PZMergeSelRec(@Data);
  if SelRecP^.FromDate <> 0 then
  begin
    Result := SelRecP^.FromDate <= Rec.ModifDateTime;
    if not Result then
      exit; // reject _ too old
  end;
  Rec.SelectArgs := SelRecP^.NXArg; // save for later processing
  Result := true;
end;

procedure TZMZippedOpr.AssignArgOptions(var Locals: TMOArgOptions;
  const Args: TMOArgOptions);
begin
  Locals.Arg := Args.Arg;
  Locals.Excludes := Args.Excludes;
  Locals.NFlag := Args.NFlag;
  Locals.XArg := Args.XArg;
end;

// rebuild file name without spaces
function TZMZippedOpr.CleanZipName(const FileName: string): string;
var
  Ancestors: string;
  Done: Boolean;
  Generation: string;
begin
  Result := '';
  Ancestors := FileName;
  repeat
    SplitQualifiedName(Ancestors, Ancestors, Generation);
    Done := Generation = '';
    if Done then
      Generation := Ancestors;
    if Result <> '' then
      Result := Generation + ZFILE_SEPARATOR + Result
    else
      Result := Generation;
  until Done;
end;

// Extract MyName from FZipList[ParentIndex] and add to FZipList if successful
function TZMZippedOpr.ExtractChildZip(ParentIndex: Integer;
  MyName: string): Integer;
const
  __ERR_DS_NoInFile = __UNIT__ + (1600 shl 10) + DS_NoInFile;
  __ERR_DS_NoWrite = __UNIT__ + (1655 shl 10) + DS_NoWrite;
var
  idx: Integer;
  MyFile: TZMZipFile;
  Parent: TZMZipFile;
  ParentName: string;
  TempZipMaster: TZipMaster;
begin
  Result := -__ERR_DS_NoInFile;
  // be safe
  if (ParentIndex <= 0) or (MyName = '') then
    exit;
  Parent := FZipList[ParentIndex];
  ParentName := Parent.FileName;
  if ParentName = '' then
    exit;
  TempZipMaster := TZipMaster.Create(nil);
  try
    TempZipMaster.OnMessage := Master.OnMessage;
    TempZipMaster.OnPasswordError := Master.OnPasswordError;
    TempZipMaster.OnProgress := Master.OnProgress;
    TempZipMaster.Password := Master.Password;
    TempZipMaster.PasswordReqCount := Master.PasswordReqCount;
    TempZipMaster.OnCRC32Error := Master.OnCRC32Error;
    TempZipMaster.Verbose := Master.Verbose;
    TempZipMaster.Trace := Master.Trace;
    TempZipMaster.TempDir := Master.TempDir;
    TempZipMaster.Active := true;
    TempZipMaster.ZipFileName := ParentName;
    if TempZipMaster.ErrCode = 0 then
    begin
      // loaded ok - see if exists
      idx := -1;
      if TempZipMaster.Find(MyName, idx) <> nil then
      begin
        // create a temporary file
        MyFile := TZMZipFile.Create(Master, Self);
        try
          MyFile.Alias :=
            QualifiedName(FZipList[ParentIndex].Name(false), MyName);
          MyFile.File_CreateTemp('Zcy', '');
          TempZipMaster.Pipes.Add(THandleStream.Create(MyFile.Handle),
            MyName, true);
          Result := TempZipMaster.Extract;
          if TempZipMaster.SuccessCnt = 1 then
          begin
            // good
            Result := MyFile.Open(false, false);
            MyFile.File_Close;
            if Result >= 0 then
            begin
              Result := FZipList.Add(MyFile); // add to list and return index
              MyFile.File_Close;
              if Result > 0 then
                MyFile := nil; // do not free
            end;
          end
          else
          begin
            // extracting zip failed
            MyFile.Position := 0;
            MyFile.SetEndOfFile;
            MyFile.File_Close;
            Result := -__ERR_DS_NoWrite; // error does not matter
          end;
        finally
          MyFile.Free;
        end;
      end;
    end;
  finally
    TempZipMaster.Free;
  end;
end;

function TZMZippedOpr.IncludeAZip(const SourceName: string): Integer;
const
  __ERR_CF_SourceIsDest = __UNIT__ + (1678 shl 10) + CF_SourceIsDest;
  __ERR_CF_SourceIsDest1 = __UNIT__ + (1689 shl 10) + CF_SourceIsDest;
  __ERR_GE_NoSkipping = __UNIT__ + (1707 shl 10) + GE_NoSkipping;
var
  Skip: TZMSkipTypes;
begin
  Result := FZipList.Find(SourceName); // already in list?
  if Result = 0 then
  begin
    Result := -__ERR_CF_SourceIsDest;
    exit;
  end;
  if Result < 0 then
  begin
    // have not opened file yet _ not in list
    Result := MergeAnotherZip(SourceName);
    if ((-Result) and MSG_ID_MASK) = AZ_SameAsSource then
      exit;
    if Result = 0 then
    begin
      Result := -__ERR_CF_SourceIsDest1;
      exit;
    end;
    if Result < 0 then
    begin
      // file does not exist or could not be opened
      if Verbosity >= zvVerbose then
        Diag(Format('Skipped missing or bad zip [%d] %s',
          [-Result, SourceName]));
      case ((-Result) and MSG_ID_MASK) of
        DS_NoInFile:
          Skip := stNotFound;
        DS_FileOpen:
          Skip := stNoOpen;
      else
        Skip := stReadError;
      end;
      if ReportSkippingEx(SourceName, Skip, 0, FSkippedFiles) then
        Result := -__ERR_GE_NoSkipping
      else
        Result := 0; // we can ignore it
    end;
  end;
end;

// select files in Source, return files selected or <0 = error
function TZMZippedOpr.ProcessInclude(SrcZip: TZMZipFile;
  Args: TMOArgOptions): Integer;
const
  __ERR_GE_NoSkipping = __UNIT__ + (1739 shl 10) + GE_NoSkipping;
var
  MergeRec: TZMergeSelRec;
begin
  if Verbosity >= zvVerbose then
    Diag('Including: ' + QualifiedName(SrcZip.Name, Args.Arg));
  MergeRec.Owner := Self;
  MergeRec.ZipName := SrcZip.FileName;
  MergeRec.FromDate := DateTimeToFileDate(Master.AddFrom);
  MergeRec.list := FSkippedFiles;
  if Args.NFlag then
    MergeRec.NXArg := SPEC_SEP + Args.XArg
  else
    MergeRec.NXArg := Args.XArg;
  Result := SrcZip.SelectFileEx(Args.Arg, Args.Excludes, zzsSet, SelectedFunc,
    MergeRec);
  if Result < 1 then
  begin
    // none found
    if ReportSkippingEx(QualifiedName(SrcZip.Name(), Args.Arg), stNotFound, 0,
      FSkippedFiles) then
      Result := -__ERR_GE_NoSkipping;
  end;
end;

// returns <0 _ error
(*
  [zipname] [switch] [[switch] ...]
  switches
  /N[+ or -]  flags not to use AddNewName (default N- _ use AddNewName)
  /X:[old]::[new]  replace 'old' with 'new' - must result in valid internal name
  >>spec  before any source zip is specified sets the default select spec
  >>spec  select files in current zip according to spec
  /E:[|][spec[|spec]...]   set excludes, if starts with | it appends to
  globals otherwise use spec
  changes to excludes occur at current line and continue until changed
  - does not change already included files.
  when used on same line as '>>' it only applies to that line
  and modifies the 'current' excludes.
*)
function TZMZippedOpr.ProcessIncludeList(var SelectCount: Integer): Integer;
const
  __ERR_DS_NoInFile = __UNIT__ + (1780 shl 10) + DS_NoInFile;
  __ERR_GE_InvalidParameter = __UNIT__ + (1794 shl 10) + GE_InvalidParameter;
var
  DefaultExcludes: TZMString;
  Effectives: TMOArgOptions;
  I: Integer;
  Locals: TMOArgOptions;
  ShowXProgress: Boolean;
  ZipsIndex: Integer;
begin
  Result := 0;
  SelectCount := 0;
  DefaultExcludes := FlattenExcludes(ExcludeSpecs);
  Effectives.Excludes := DefaultExcludes;
  Effectives.Arg := '*.*';
  Effectives.NFlag := false; // N- or none
  Effectives.XArg := '';
  FSplitter.Allow := '>ENX';
  FSplitter.Options := [zaoLastSpec, zaoWildSpec, zaoMultiSpec];
  // locate source zips and their files
  ZipsIndex := -__ERR_DS_NoInFile; // no current yet
  ShowXProgress := IncludeSpecs.Count > MergeIncludeListThreshold;
  if ShowXProgress then
    ReportProgress(zacXItem, PR_Processing, '', IncludeSpecs.Count);
  for I := 0 to IncludeSpecs.Count - 1 do
  begin
    if Result < 0 then
      break;
    if ShowXProgress then
      ReportProgress(zacXProgress, PR_Processing, '', 1);
    CheckCancel;
    Result := 0;
    FSplitter.Raw := IncludeSpecs[I];
    if FSplitter.Error <> zasNone then
      raise EZipMaster.CreateMsgStr(__ERR_GE_InvalidParameter, FSplitter.Raw);
    if FSplitter.Main <> '' then
    begin
      // we are specifying a zip to process
      ZipsIndex := IncludeAZip(FSplitter.Main);
      Result := ZipsIndex;
      if Result < 0 then
        break; // error
      if Result = 0 then
        Continue; // skipped it
      // process spec and/or switches
    end
    else // no zip specified
    begin
      // ignore empty lines
      if FSplitter.Found = '' then
        Continue;
      // any zips specified yet
      if ZipsIndex < 0 then
      begin
        // none yet, set defaults
        SetArgsOptionsFromSplitter(Effectives, DefaultExcludes);
        Continue;
      end;
    end;
    if (Result < 0) or (ZipsIndex < 0) then
      break; // must have open file
    if FSplitter.Has(ZSPECARG) or (FSplitter.Main <> '') then
    begin
      // using local settings from splitter
      AssignArgOptions(Locals, Effectives);
      SetArgsOptionsFromSplitter(Locals, Effectives.Excludes);
      // include a spec in the current zip
      Result := ProcessInclude(FZipList[ZipsIndex], Locals);
      if Result > 0 then
        SelectCount := SelectCount + Result;
      Continue;
    end;
    // Only have switches _ Set effectives
    SetArgsOptionsFromSplitter(Effectives, DefaultExcludes);
  end;
end;

function TZMZippedOpr.ResolveConfirm(const ExistName: TZMString;
  ExistRec: TZMZRec; const ConflictName: TZMString; ConflictRec: TZMZRec;
  const NewName: TZMString): TZMResolutions;
var
  ConflictEntry: TZM_ConflictEntry;
  ExistEntry: TZM_ConflictEntry;
  Response: Integer;
  tmpZippedConflict: TZMMergeZippedConflictEvent;
begin
  // Do we have a event assigned for this then don't ask.
  tmpZippedConflict := Master.OnMergeZippedConflict;
  if assigned(tmpZippedConflict) then
  begin
    ConflictEntry := nil;
    ExistEntry := TZM_ConflictEntry.Create;
    try
      ExistEntry.Rec := ExistRec;
      ExistEntry.ZipName := ExistName;
      ConflictEntry := TZM_ConflictEntry.Create;
      ConflictEntry.Rec := ConflictRec;
      ConflictEntry.ZipName := ConflictName;
      Result := zmrRename;
      tmpZippedConflict(Master, ExistEntry, ConflictEntry, Result);
    finally
      ExistEntry.Free;
      ConflictEntry.Free;
    end;
    exit;
  end;
  Response := ZipMessageDlgEx(ZipLoadStr(CF_FileConflict),
    Format(ZipLoadStr(CF_Merge), [QualifiedName(ExistName, ExistRec.XName),
    QualifiedName(ConflictName, ConflictRec.XName), NewName]),
    zmtConfirmation + DHC_CpyZipOvr, [mbYes, mbNo, mbIgnore]);
  if Response = mrOk then
    Result := zmrRename
  else
    if Response = mrNo then
      Result := zmrExisting
    else
      Result := zmrConflicting;
end;

procedure TZMZippedOpr.SetArgsOptionsFromSplitter(var Args: TMOArgOptions;
  const ParentExcludes: TZMString);
var
  xc: string;
begin
  if FSplitter.Has(ZSPECARG) then
  begin
    Args.Arg := FSplitter.Arg(ZSPECARG);
    if Args.Arg = '' then
      Args.Arg := '*.*';
  end;
  if FSplitter.Has('E') then
  begin
    xc := FSplitter.Arg('E');
    if (xc <> '') and (xc[1] = SPEC_SEP) then
    begin
      if xc <> SPEC_SEP then
        Args.Excludes := ParentExcludes + xc;
    end
    else
      Args.Excludes := xc;
  end;
  if FSplitter.Has('N') then
    Args.NFlag := FSplitter.Arg('N') = '+';
  if FSplitter.Has('X') then
    Args.XArg := FSplitter.Arg('X');
end;

procedure TZMZippedOpr.SetupAppendRecs(Allow: Boolean);
var
  DstZip: TZMZipFile;
  I: Integer;
  OrigCnt: Integer;
  Rec: TZMMergeRec;
begin
  DstZip := FZipList[0];
  OrigCnt := DstZip.Count;
  if OrigCnt < 1 then
    exit;
  for I := 0 to OrigCnt - 1 do
  begin
    CheckCancel;
    Rec := TZMMergeRec(FOutZip[I]);
    if (Rec = nil) or (Rec.StatusBit[zsbError or zsbDiscard or zsbSelected] <>
      zsbSelected) then
      Continue;
    Rec.Keep := Allow;
  end;
end;

procedure TZMZippedOpr.Started;
begin
  inherited;
end;

function TZMZipList.Add(aZip: TZMZipFile): Integer;
begin
  if FZips = nil then
    FZips := TList.Create;
  Result := FZips.Add(aZip) // add to list and return index
end;

procedure TZMZipList.BeforeDestruction;
begin
  Clear;
  FZips.Free;
  inherited;
end;

procedure TZMZipList.Clear;
var
  f: Integer;
  I: Integer;
  z: pointer;
begin
  if FZips <> nil then
  begin
    if FProtectZero then
      f := 1 // do not destroy first object
    else
      f := 0;
    for I := f to FZips.Count - 1 do
    begin
      z := FZips.Items[I];
      if z <> nil then
        TObject(z).Free;
    end;
    FZips.Clear;
  end;
end;

// close all source files
procedure TZMZipList.CloseAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].File_Close;
end;

// find already open zip file
// return index or <0 = error
function TZMZipList.Find(const aZipFile: TZMString): Integer;
const
  __ERR_DS_NoInFile = __UNIT__ + (1988 shl 10) + DS_NoInFile;
var
  I: Integer;
  z: TZMZipFile;
begin
  Result := -__ERR_DS_NoInFile;
  if FZips = nil then
    exit;
  // does it exist
  for I := 0 to FZips.Count - 1 do
  begin
    if FZips[I] = nil then
      Continue;
    z := Items[I];
    if FileNameComp(z.Name, aZipFile) = 0 then
    begin
      Result := I; // found
      break;
    end;
  end;
end;

function TZMZipList.GetCount: Integer;
begin
  Result := FZips.Count;
end;

function TZMZipList.GetItems(Index: Integer): TZMZipFile;
begin
  Result := nil;
  if (Index >= 0) and (Index < FZips.Count) and (FZips[Index] <> nil) then
    Result := TObject(FZips[Index]) as TZMZipFile;
end;

{ TZM_ConflictEntry }

function TZM_ConflictEntry.GetEntry: TZMDirEntry;
begin
  Result := FRec;
end;

function TZM_ConflictEntry.GetRec: TZMZRec;
begin
  Result := TZMZRec(FRec);
end;

function TZM_ConflictEntry.GetZipName: TZMString;
begin
  Result := FZipName;
end;

procedure TZM_ConflictEntry.SetRec(const Value: TZMZRec);
begin
  FRec := Value;
end;

procedure TZM_ConflictEntry.SetZipName(const Value: TZMString);
begin
  FZipName := Value;
end;

{ TZMZipMerge }

procedure TZMZipMerge.AfterConstruction;
begin
  inherited;
  LastOpened := nil;
end;

function TZMZipMerge.CommitRec(Rec: TZMZRec): Int64;
var
  err: Integer;
  InFile: TZMWorkZip;
begin
  if (not(Rec is TZMMergeRec)) or (TZMMergeRec(Rec).Link = nil) then
  begin
    Result := inherited CommitRec(Rec);
    exit;
  end;
  if TZMMergeRec(Rec).Keep then
  begin
    Result := Rec.Process;
  end
  else
  begin
    Result := Rec.ProcessSize;
    if Result > 0 then
    begin
      InFile := TZMCopyRec(Rec).Link.MyFile;
      if (not InFile.IsOpen) and (InFile <> LastOpened) then
      begin
        if Verbosity > zvVerbose then
          Diag('Opening ' + InFile.Name(true));
        if FLastOpened <> nil then
          FLastOpened.File_Close;
        LastOpened := InFile;
        err := InFile.File_Reopen(fmOpenRead); // open it
        if err < 0 then
        begin
          Result := err;
          exit;
        end;
      end;
      Result := Rec.Process;
    end;
  end;
end;

procedure TZMZipMerge.SetLastOpened(const Value: TZMWorkZip);
begin
  if FLastOpened <> Value then
  begin
    if FLastOpened <> nil then
      FLastOpened.File_Close;
    FLastOpened := Value;
  end;
end;

procedure TZMMergeRec.AfterConstruction;
begin
  inherited;
  Keep := false;
  NextEntry := -1;
end;

procedure TZMMergeRec.AssignFrom(const ARec: TZMIRec);
begin
  inherited;
  if (ARec <> Self) and (ARec is TZMMergeRec) then
  begin
    Keep := TZMMergeRec(ARec).Keep;
    NextEntry := TZMMergeRec(ARec).NextEntry;
  end;
end;

function TZMMergeRec.Process: Int64;
const
  __ERR_DS_SeekError = __UNIT__ + (2139 shl 10) + DS_SeekError;
  __ERR_DS_NoAppend = __UNIT__ + (2145 shl 10) + DS_NoAppend;
var
  Nxt: Int64;
begin
  if not Keep then
  begin
    Result := inherited Process;
    exit;
  end;
  // verify at correct header
  Result := SeekLocalData;
  if Result >= 0 then
  begin
    Nxt := RelOffLocalHdr + ProcessSize;
    // Good - Position to next entry
    if MyFile.Seek(Nxt, 0) = Nxt then
      Result := 0
    else
      Result := -__ERR_DS_SeekError;
  end
  else
    if (MyFile.Verbosity > zvVerbose) then
      Diag('Kept local headers different ' + FileName);
  if Result < 0 then
    Result := -__ERR_DS_NoAppend;
end;

end.
