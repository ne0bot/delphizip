unit ZMMergeOpr;

//  ZMMergeOpr.pas - MergeZipped operation

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
//modified 2013-04-17

{$I   '.\ZipVers.inc'}

interface

uses
  {$IFDEF VERDXE2up}
    System.Classes, System.SysUtils, WinApi.Windows, VCL.Graphics,
  {$ELSE}
    SysUtils, Windows, Classes, Graphics, ZMCompat,
  {$ENDIF}
  ZipMstr, ZMLister, ZMBody, ZMModOpr, ZMMisc, ZMArgSplit, ZMZipBase,
  ZMZipReader, ZMZipWriter, ZMEntryReader, ZMUnzipOpr;
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
  // provides support for auto-open of source zips
  TZMZipMerger = class(TZMZipCopier)
  private
    FLastOpened: TZMZipBase;
    procedure SetLastOpened(const Value: TZMZipBase);
  protected
    function CommitRec(Rec: TZMEntryWriter): Int64; override;
    property LastOpened: TZMZipBase read FLastOpened write SetLastOpened;
  public
    procedure AfterConstruction; override;
  end;

  TZMEntryMerger = class(TZMEntryCopier)
  private
    FKeep: Boolean;
  protected
    function GetTitle: string; override;
  public
    procedure AfterConstruction; override;
    function Process: Int64; override;
    property Keep: Boolean read FKeep write FKeep;
  end;

type
  TZMMergeOpr = class(TZMUnzipOpr)
  private
    procedure AssignArgOptions(var Locals: TMOArgOptions;
      const Args: TMOArgOptions);
    procedure ClearAppend;
    function CommitAppend: Integer;
    function ExtractChildZip(ParentIndex: Integer; const MyName: string): Integer;
    function FlattenExcludes(Excludes: TStrings): string;
    function IncludeAZip(const SourceName: string): Integer;
    function MergeAnotherZip(const SourceName: String): Integer;
    function MergeAnotherZip1(const SourceName: String): Integer;
    function MergeIntermediate(SelectedCount: Integer;
      const Opts: TZMMergeOpts): Integer;
    function MergeIntermedZip(RefZip: TZMZipReader; ZipIndex: Integer; Opts:
        TZMMergeOpts): Integer;
    function MergePrepareName(var ZName: TZMString; ZRec: TZMEntryBase): Integer;
    function MergeResolveConflict(RefRec, NRec: TZMEntryCopier; const Opts:
        TZMMergeOpts): Integer;
    function MergeSafeName(ZipIndex: Integer; const Name: string): string;
    function Prepare(MustExist: Boolean; SafePart: Boolean = false): TZMZipReader;
    function PrepareAppend: Boolean;
    function ProcessInclude(SrcZip: TZMZipReader; const Args: TMOArgOptions):
        Integer;
    function ProcessIncludeList(var SelectCount: Integer): Integer;
    function ResolveConfirm(const ExistName: TZMString; ExistRec: TZMEntryBase;
        const ConflictName: TZMString; ConflictRec: TZMEntryBase; const NewName:
        TZMString): TZMResolutions;
    procedure SetArgsOptionsFromSplitter(var Args: TMOArgOptions;
      const ParentExcludes: TZMString);
    procedure SetupAppendRecs(Allow: Boolean);
  protected
    FOutZip: TZMZipCopier;
    FSkippedFiles: TStringList;
    FSplitter: TZMArgSplitter;
    FZipList: TZMZipList;
    procedure CreateInterimZip; override;
    function FinalizeInterimZip(OrigZip: TZMZipReader): Integer; override;
    function MergeWrite: Integer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function CleanZipName(const FileName: string): string;
    function MergeAnotherZipInZip(const SourceName: String): Integer;
    function MergeZippedFiles(Opts: TZMMergeOpts; TheDest: TZMZipWriter): Integer;
  end;

implementation

uses
  {$IFDEF VERDXE2up}
    VCL.Dialogs, VCL.Controls,
  {$ELSE}
    Dialogs, Controls,
  {$ENDIF}
   ZMStructs, ZMDelZip, ZMXcpt, ZMUtils, ZMDlg, ZMCtx,
  ZMMsg, ZMDrv, ZMMatch, ZMWFuncs, ZMZipMulti;

const
  __UNIT__ = 16;

type
  TZMMergeArgs = class(TZMSelectArgs)
  private
    FFromDate: Cardinal;
    Flist: TStrings;
    FNFlag: boolean;
    FXArg: TZMString;
    FZipName: TZMString;
  public
    function Accept(Rec: TZMEntryBase): Boolean; override;
    procedure AfterConstruction; override;
    procedure Assign(Other: TZMSelectArgs); override;
    property XArg: TZMString read FXArg write FXArg;
    property FromDate: Cardinal read FFromDate write FFromDate;
    property ZipName: TZMString read FZipName write FZipName;
    property list: TStrings read Flist write Flist;
    property NFlag: boolean read FNFlag write FNFlag;
  end;

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

const
  DeleteIncludeListThreshold = 20;
  MergeIncludeListThreshold  = 10;
  PrepareAppendThreshold     = 10;

procedure TZMMergeOpr.AfterConstruction;
begin
  inherited;
  FSkippedFiles := nil;
  FZipList := TZMZipList.Create;
  FSkippedFiles := TStringList.Create;
  FSplitter := TZMArgSplitter.Create;
end;

procedure TZMMergeOpr.BeforeDestruction;
begin
  FSkippedFiles.Free;
  FZipList.Free;
  FSplitter.Free;
  inherited;
end;

procedure TZMMergeOpr.ClearAppend;
begin
  SetupAppendRecs(false); // clear flags
end;

function TZMMergeOpr.CommitAppend: Integer;
var
  DstZip: TZMZipReader;
begin
  DstZip := FZipList[0];
  DstZip.File_Close; // don't aquire handle
  FOutZip.Stream := DstZip.ReleaseStream;
  FOutZip.ArchiveName := DstZip.ArchiveName;
  Result := FOutZip.File_Reopen(fmOpenReadWrite);
  if Result >= 0 then
    Result := FOutZip.Commit(zwoZipTime in WriteOptions);
end;

function TZMMergeOpr.FlattenExcludes(Excludes: TStrings): string;
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
// return <0 _ error, >= 0 _ Index in FZipList
function TZMMergeOpr.MergeAnotherZip(const SourceName: String): Integer;
begin
  if Pos(ZFILE_SEPARATOR, SourceName) > 0 then
    Result := MergeAnotherZipInZip(SourceName) // handle Zip in Zip
  else
    Result := MergeAnotherZip1(SourceName);
end;

// opens a zip file for merging and adds to FZipList
// return <0 _ error, >= 0 _ Index in FZipList
function TZMMergeOpr.MergeAnotherZip1(const SourceName: String): Integer;
var
  Dzip: TZMZipReader;
  SrcZip: TZMZipReader;
begin
  // have not opened file yet
  SrcZip := TZMZipReader.Create(Lister);
  try
    begin
      SrcZip.ArchiveName := SourceName;
      Result := SrcZip.Open(false, false);
      if Result >= 0 then
      begin
        Dzip := FZipList[0];
        if (SrcZip.WorkDrive.DriveLetter = Dzip.WorkDrive.DriveLetter) and
          (not Dzip.WorkDrive.DriveIsFixed) and
          (Dzip.MultiDisk or SrcZip.MultiDisk or
          (zwoDiskSpan in WriteOptions)) then
          Result := -ZM_Error(248, ZS_SameAsSource)
        else
        begin
          Result := FZipList.Add(SrcZip); // add to list and return Index
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
function TZMMergeOpr.MergeAnotherZipInZip(const SourceName: String): Integer;
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
function TZMMergeOpr.MergeIntermediate(SelectedCount: Integer;
  const Opts: TZMMergeOpts): Integer;
var
  MaxCount: Integer;
  RefZip: TZMZipReader;
  RefZipIndex: Integer;
  SelCount: Integer;
begin
  Result := 0;
  PrepareInterimZip;
  FOutZip := InterimZip as TZMZipMerger;
  // replicate all records and settings
  FOutZip.Select('*', zzsSet); // want all (initially)
  Progress.NewXtraItem(zxMerging, FZipList.Count);
  // add the possibly altered selected entries
  // [0] is 'DstZip'
  MaxCount := 0;
  for RefZipIndex := 0 to FZipList.Count - 1 do
    MaxCount := MaxCount + FZipList[RefZipIndex].SelCount;
  FOutZip.HTAutoSize(MaxCount);
  for RefZipIndex := 0 to FZipList.Count - 1 do
  begin
    Progress.AdvanceXtra(1);
    CheckCancel;
    RefZip := FZipList[RefZipIndex];
    SelCount := RefZip.SelCount;
    Reporter.Trace('Processing ' + IntToStr(SelCount) + ' files: ' + RefZip.Name);
    if SelCount < 1 then
      Continue;
    Result := MergeIntermedZip(RefZip, RefZipIndex, Opts);
  end;
end;

// Merge selected entries from RefZip into FOutZip
function TZMMergeOpr.MergeIntermedZip(RefZip: TZMZipReader; ZipIndex: Integer;
    Opts: TZMMergeOpts): Integer;
var
  Cnt: Integer;
  NewRec: TZMEntryMerger;
  Rec: TZMEntryBase;
  RefRec: TZMEntryCopier;
  skp: TZMSkipTypes;
  ZName: string;
  ZRec: TZMEntryBase;
begin
  Result := 0; // keep compiler happy
  // process each selected entry
  Rec := RefZip.FirstSelected;
  Cnt := 0;
  while (Rec <> nil) do
  begin
    Inc(Cnt);
    if (Cnt and 63) = 0 then
      CheckCancel;
    Result := 0;
    ZRec := Rec;
    Rec := RefZip.NextSelected(Rec);
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
    NewRec := TZMEntryMerger.Create(FOutZip); // make a entry
    NewRec.AssignFrom(ZRec);
    NewRec.Link := ZRec; // link to original
    if Result = 1 then
    begin
      NewRec.SetStatusBit(zsbRenamed);
      Result := NewRec.ChangeName(ZName);
      if Result < 0 then
      begin
        NewRec.Free; // stop leak
        Reporter.Inform('Failed rename [' + IntToStr(-Result) + '] ' + ZRec.FileName +
          ' to ' + ZName);
        skp := stUser;
        case AbsErr(Result) of
          ZS_BadFileName:
            skp := stBadName;
          ZS_DuplFileName:
            skp := stDupName;
        end;
        if skp = stUser then
          break; // unknown error - fatal
        if Reporter.ReportSkippingEx(QualifiedName(RefZip.Name, ZName), skp, -Result,
          FSkippedFiles) then
          break; // fatal
        Continue; // ignore
      end;
      Reporter.Inform('Renamed ' + ZRec.FileName + ' to ' + NewRec.FileName);
    end;
    // does it exist
    RefRec := TZMEntryCopier(FOutZip.FindName(ZName, nil));
    Result := 0;
    if RefRec <> nil then
    begin
      // duplicate found - resolve it
      Result := MergeResolveConflict(RefRec, NewRec, Opts);
    end;
    if Result = 0 then
    begin
      Result := FOutZip.Add(NewRec);  // use NewRec
      FOutZip.HTAdd(NewRec, false);
    end
    else
      NewRec.Free; // stop leak
    if Result < 0 then
      break;
  end;
end;

// returns <0 = error, 0 = no change, 1 = changed, 2 = ignored by user
function TZMMergeOpr.MergePrepareName(var ZName: TZMString; ZRec:
    TZMEntryBase): Integer;
var
  Args: TZMMergeArgs;
  Changed: Boolean;
  FileName: string;
  old: string;
  sep: Integer;
  subst: string;
  tmpOnSetAddName: TZMSetAddNameEvent;
  ZipName: string;
begin
  Result := 0;
  ZName := ZRec.FileName;
  Args := TZMMergeArgs(ZRec.SelectArgs);

  ZipName := ZRec.MyFile.Name;
  if (Args = nil) or not Args.NFlag then
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
          if Reporter.ReportSkippingEx(QualifiedName(ZipName, ZRec.FileName), stUser, 0,
            FSkippedFiles) then
            Result := -ZM_Error(437, ZS_NoSkipping)
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
  if (Args <> nil) and (Args.XArg <> '') then
  begin
    old := Args.XArg;
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
function TZMMergeOpr.MergeResolveConflict(RefRec, NRec: TZMEntryCopier; const
    Opts: TZMMergeOpts): Integer;
var
  ConflictZipName: string;
  NewName: string;
  RefZipName: string;
  Resolve: TZMResolutions;
begin
  RefZipName := RefRec.Link.MyFile.Name;
  ConflictZipName := NRec.Link.MyFile.Name;
  Reporter.Trace('Found conflict for ' +
    QualifiedName(RefZipName, RefRec.FileName) + ' in ' + ConflictZipName, ZM_Error(487, 0));
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
        if Reporter.ReportSkippingEx(QualifiedName(ConflictZipName, NRec.FileName),
          stFileExists, 0, FSkippedFiles) then
          Result := -ZM_Error(514, ZS_NoSkipping)
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
        if Reporter.ReportSkippingEx(QualifiedName(RefZipName, RefRec.FileName),
          stFileExists, 0, FSkippedFiles) then
          Result := -ZM_Error(527, ZS_NoSkipping)
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
          if Reporter.ReportSkippingEx(QualifiedName(ConflictZipName, NewName),
            stFileExists, 0, FSkippedFiles) then
            Result := -ZM_Error(543, ZS_NoSkipping)
          else
            Result := 1; // ignore
        end
        else
        begin
          NRec.Selected := true;
          NRec.SetStatusBit(zsbRenamed);
          Result := 0;
        end;
      end;
  end;
end;

function TZMMergeOpr.MergeSafeName(ZipIndex: Integer;
  const Name: string): string;
var
  EName: string;
  Extn: string;
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
  until (FOutZip.FindName(Result, nil) = nil);
end;

function TZMMergeOpr.MergeWrite: Integer;
var
  DstZip:  TZMZipReader;// orig dest zip (read only)
  existed: Boolean;
  Includes: Integer;
  Rec: TZMEntryCopier;
  RenamedCnt: Integer;
  ReplaceCnt: Integer;
  Rubbish: Integer;
  SourceZip: TZMZipReader;
  TotalCnt: Integer;
  WillSplit: Boolean;
  ZRec: TZMEntryBase;
begin
  // find total files in dest and list of files added/replaced
  DstZip := FZipList[0];
  Body.ClearIncludeSpecs; // add names of files to be added
  Includes := 0;
  TotalCnt := 0;
  ReplaceCnt := 0;
  Rubbish := 0;
  RenamedCnt := 0;
  ZRec := FOutZip.FirstRec;
  while ZRec <> nil do
  begin
    CheckCancel;
    Rec := TZMEntryCopier(ZRec);
    ZRec := ZRec.Next;
    if Rec.StatusBit[zsbError or zsbDiscard or zsbSelected] <> zsbSelected then
    begin
      Inc(Rubbish);
      Continue;
    end;
    SourceZip := TZMZipReader(Rec.Link.MyFile);
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
    // count and mark for later adding to FSpecArgs
    if Rec.StatusBit[zsbRenamed] <> 0 then
      Inc(RenamedCnt);
    Rec.Status[zsbHail] := True;
    Inc(Includes);
  end;
  Reporter.Trace(Format('Total=%d, Replaced=%d, New=%d, Discarded=%d, Renamed=%d',
      [TotalCnt, ReplaceCnt, Includes, Rubbish, RenamedCnt]),ZM_Error(627, 0));
  if (TotalCnt < 1) or (Includes < 1) then
  begin
    Reporter.Inform('nothing to do', ZM_Error(630, 0));
    if Reporter.ReportSkippingEx(DstZip.Name, stNothingToDo, 0, FSkippedFiles) then
      Result := -ZM_Error(632, ZS_NoSkipping)
    else
      Result := 0;
    exit;
  end;
  // can we just append to original
  existed := (zfi_Loaded and DstZip.info) <> 0;
  WillSplit := DstZip.MultiDisk or
    ((not existed) and (zwoDiskSpan in WriteOptions));

  if (not WillSplit) or (not(zwoSafe in WriteOptions)) then
  begin
    if not existed then
    begin              // need to create the new file
      // write new file
      FOutZip.ArchiveName := DstZip.ArchiveName;
      FOutZip.ZipComment := Lister.ZipComment; // keep orig
      ShowProgress := zspFull;
      if assigned(DstZip.stub) and DstZip.UseSFX then
      begin
        FOutZip.AssignStub(DstZip);
        FOutZip.UseSFX := true;
      end;
      Result := -ZM_Error(655, ZS_NoOutFile);
      FOutZip.File_Create(DstZip.ArchiveName);
      if FOutZip.IsOpen then
      begin
        Result := FOutZip.Commit(zwoZipTime in WriteOptions);
        FOutZip.File_Close;
        FZipList.CloseAll; // close all source files
      end;
      if Result < 0 then
        Reporter.Inform(Format('Merging new file failed: %d', [-Result]),
          ZM_Error(665, 0));
      exit;
    end
    else
    begin
      // try to append to existing zip
      if PrepareAppend then
      begin
        // commit it
        Result := CommitAppend;
        if Result >= 0 then
          Reporter.Trace('Merging append successful');
        if AbsErr(Result) <> ZS_NoAppend then
          exit;
        // fix to allow safe write
        ClearAppend;
        Reporter.Inform(Format('Merging append failed: %d', [-Result]),
          ZM_Error(682, 0));
      end;
    end;
  end;

  // write to intermediate
  if WillSplit then
    FOutZip.File_CreateTemp(PRE_INTER, '')
  else
    FOutZip.File_CreateTemp(PRE_INTER, DstZip.ArchiveName);
  if not FOutZip.IsOpen then
  begin
    Result := -ZM_Error(694, ZS_NoOutFile);
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
  ShowProgress := zspFull;
  // now we write it
  Result := FOutZip.Commit(zwoZipTime in WriteOptions);
  FOutZip.File_Close;
  FZipList.CloseAll; // close all source files
  if Result >= 0 then
  begin
    if (FOutZip.Count - Rubbish) <> TotalCnt then
      Result := -ZM_Error(716, ZS_InternalError)
    else
      Result := Recreate(FOutZip, DstZip); // all correct so Recreate source
  end;
end;

// IncludeSpecs = Zips and files to include
// zipname
// ExcludeSpecs = files to exclude
function TZMMergeOpr.MergeZippedFiles(Opts: TZMMergeOpts; TheDest:
    TZMZipWriter): Integer;
var
  DstZip: TZMZipReader;
  SelectCount: Integer;
begin
  if Reporter.Verbosity > zvTrace then
    Reporter.Logger.LogSpecs('');
  ShowProgress := zspFull;
  FOutZip := nil; // will be used to write the output
  if IncludeSpecs.Count < 1 then
    raise EZipMaster.CreateMsgDisp(ZM_Error(736, ZS_InvalidArguments), true);
  if ZipFileName = '' then
    raise EZipMaster.CreateMsgDisp(ZM_Error(738, ZS_NoZipSpecified), true);
  FZipList.Clear;
  FSkippedFiles.Clear;
  if TheDest <> nil then
    DstZip := TheDest
  else
    DstZip := Prepare(false, true); // TODO -c : don't add sfx stub to reader
  DstZip.Select('*', zzsSet); // initial want all
  FZipList.ProtectZero := true; // do not destroy DstZip
  FZipList.Add(DstZip);
  // add source zips to list and select their files
  Result := ProcessIncludeList(SelectCount);
  if Result < 0 then
    raise EZipMaster.CreateMsgDisp(Result, true);
  Body.ClearIncludeSpecs; // for new/updated files
  if SelectCount >= 1 then
  begin
    // add all selected to OutZip
    Result := MergeIntermediate(SelectCount, Opts);
    if Result < 0 then
      raise EZipMaster.CreateMsgDisp(Result, AbsErr(Result) <> ZS_Abort);
    // we have processed list now resolve merge conflicts
    // write the results
    if Result >= 0 then
      Result := MergeWrite;
    // Update the Zip Directory by calling List method
    // for spanned exe avoid swapping to last disk
    Reload := zlrReload; // force reload
  end;
  if Result < 0 then
    raise EZipMaster.CreateMsgDisp(Result, AbsErr(Result) <> ZS_Abort);
  // it was successful
  SuccessCnt := IncludeSpecs.Count;
  FZipList.Clear;
  ExcludeSpecs.Clear;
  ExcludeSpecs.Assign(FSkippedFiles);
  FSkippedFiles.Clear;
  if Reporter.Verbosity > zvTrace then
    Reporter.Logger.LogSpecs('');
end;

(* TZMMergeOpr.Prepare
  Prepare destination and get SFX stub as needed
*)
function TZMMergeOpr.Prepare(MustExist: Boolean; SafePart: Boolean = false):
    TZMZipReader;
var
  err: Integer;
begin
  Result := CurrentZip(MustExist, SafePart);
  err := PrepareZip(Result);
  if err < 0 then
    raise EZipMaster.CreateMsgDisp(-err, true);
end;

// prepare to commit by appending to orig file, return false if not possible
function TZMMergeOpr.PrepareAppend: Boolean;
var
  DstZip: TZMZipReader;
  HighKept: Int64;
  I: Integer;
  LocalOfs: Int64;
  LowDiscard: Int64;
  OrigCnt: Integer;
  Rec: TZMEntryMerger;
  ShowXProgress: Boolean;
  ZRec: TZMEntryBase;
begin
  Result := false;
  DstZip := FZipList[0];
  OrigCnt := DstZip.Count;
  if OrigCnt < 1 then
    exit;
  // check can append
  ShowXProgress := OrigCnt > PrepareAppendThreshold;
  if ShowXProgress then
    Progress.NewXtraItem(zxProcessing, IncludeSpecs.Count);
  LowDiscard := DstZip.SOCOfs;
  HighKept := -1;
  ZRec := FOutZip.FirstRec;
  I := 0;
  while (Zrec <> nil) and (I < OrigCnt) do
  begin
    if ShowXProgress then
      Progress.AdvanceXtra(1);
    CheckCancel;
    Rec := TZMEntryMerger(ZRec);
    ZRec := ZRec.Next;
    Inc(I);
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
  Reporter.Trace('Should be able to append');
  SetupAppendRecs(true);
  Result := true;
end;

procedure TZMMergeOpr.AssignArgOptions(var Locals: TMOArgOptions;
  const Args: TMOArgOptions);
begin
  Locals.Arg := Args.Arg;
  Locals.Excludes := Args.Excludes;
  Locals.NFlag := Args.NFlag;
  Locals.XArg := Args.XArg;
end;

// rebuild file name without spaces
function TZMMergeOpr.CleanZipName(const FileName: string): string;
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

// does not create a temperary file
procedure TZMMergeOpr.CreateInterimZip;
begin
  InterimZip := TZMZipMerger.Create(Lister);
end;

// Extract MyName from FZipList[ParentIndex] and add to FZipList if successful
function TZMMergeOpr.ExtractChildZip(ParentIndex: Integer;
  const MyName: string): Integer;
var
  Entry: TZMEntryBase;
  idx: Integer;
  MyFile: TZMZipReader;
  Parent: TZMZipReader;
  ParentName: string;
  TheZip: TZMZipReader;
begin
//  Result := -__ERR_ZS_NoInFile;
  // be safe
  if (ParentIndex <= 0) or (MyName = '') then
  begin
    Result := -ZM_Error(930, ZS_NoInFile);
    exit;
  end;
  Parent := FZipList[ParentIndex];
  ParentName := Parent.ArchiveName;
  if ParentName = '' then
  begin
    Result := -ZM_Error(937, ZS_NoInFile);
    exit;
  end;
  TheZip := TZMZipReader.Create(Lister);
  try
    TheZip.ArchiveName := ParentName;
    Reporter.Trace('Loading parent zip', ZM_Error(943, 0));
    Result := TheZip.Open(false, false);
    if Result >= 0 then
    begin
      // loaded ok - see if file exists
      idx := -1;
      Entry := TheZip.SearchName(MyName, True, idx); // find first matching pattern
      if Entry <> nil then
      begin
        // create a temporary file
        MyFile := TZMZipReader.Create(Lister);
        try
          MyFile.Alias :=
            QualifiedName(FZipList[ParentIndex].Name(false), MyName);
          MyFile.File_CreateTemp('Zcy', '');
          Result := UnzipAStream(MyFile.Stream, Entry);
          if Result = 0 then
          begin
            // good
            MyFile.File_Close;
            Result := MyFile.Open(false, false);
            MyFile.File_Close;
            if Result >= 0 then
            begin
              Result := FZipList.Add(MyFile); // add to list and return Index
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
            Result := -ZM_Error(979, ZS_NoWrite); // error does not matter
          end;
        finally
          MyFile.Free;
        end;
      end;
    end;
  finally
    TheZip.Free;
  end;
end;

function TZMMergeOpr.FinalizeInterimZip(OrigZip: TZMZipReader): Integer;
begin
  if (InterimZip.ArchiveName = '') and
    not InterimZip.File_CreateTemp(PRE_INTER, '') then
    raise EZipMaster.CreateMsgDisp(ZM_Error(995, ZS_NoOutFile), true);
  Result := inherited FinalizeInterimZip(OrigZip);
end;

function TZMMergeOpr.IncludeAZip(const SourceName: string): Integer;
var
  Skip: TZMSkipTypes;
begin
  Result := FZipList.Find(SourceName); // already in list?
  if Result = 0 then
  begin
    Result := -ZM_Error(1008, ZS_SourceIsDest);
    exit;
  end;
  if Result < 0 then
  begin
    // have not opened file yet _ not in list
    Result := MergeAnotherZip(SourceName);
    if AbsErr(Result) = ZS_SameAsSource then
      exit;
    if Result = 0 then
    begin
      Result := -ZM_Error(1019, ZS_SourceIsDest);
      exit;
    end;
    if Result < 0 then
    begin
      // file does not exist or could not be opened
      Reporter.Inform(Format('Skipped missing or bad zip [%d] %s',
          [-Result, SourceName]));
      case AbsErr(Result) of
        ZS_NoInFile:
          Skip := stNotFound;
        ZS_FileOpen:
          Skip := stNoOpen;
      else
        Skip := stReadError;
      end;
      if Reporter.ReportSkippingEx(SourceName, Skip, 0, FSkippedFiles) then
        Result := -ZM_Error(1036, ZS_NoSkipping)
      else
        Result := 0; // we can ignore it
    end;
  end;
end;

// select files in Source, return files selected or <0 = error
function TZMMergeOpr.ProcessInclude(SrcZip: TZMZipReader; const Args:
    TMOArgOptions): Integer;
var
  MergeArgs: TZMMergeArgs;
begin
  if Reporter.Verbosity > zvVerbose then
    Reporter.Trace('Including: ' + QualifiedName(SrcZip.Name, Args.Arg));
  MergeArgs := TZMMergeArgs.Create;
  MergeArgs.ZipName := SrcZip.ArchiveName;
  MergeArgs.FromDate := DateTimeToFileDate(Lister.AddFrom);
  MergeArgs.list := FSkippedFiles;
  MergeArgs.XArg := Args.XArg;
  MergeArgs.NFlag := Args.NFlag;
  Result := SrcZip.SelectRec(Args.Arg, Args.Excludes, zzsSet, MergeArgs);
  if Result < 1 then
  begin
    // none found
    MergeArgs.Free;
    if Reporter.ReportSkippingEx(QualifiedName(SrcZip.Name(), Args.Arg), stNotFound, 0,
      FSkippedFiles) then
      Result := -ZM_Error(1066, ZS_NoSkipping);
  end//;
  else
    SrcZip.AddSelectArgs(MergeArgs);
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
function TZMMergeOpr.ProcessIncludeList(var SelectCount: Integer): Integer;
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
  ZipsIndex := -ZM_Error(1104, ZS_NoInFile); // no current yet
  ShowXProgress := IncludeSpecs.Count > MergeIncludeListThreshold;
  if ShowXProgress then
    Progress.NewXtraItem(zxProcessing, IncludeSpecs.Count);
  for I := 0 to IncludeSpecs.Count - 1 do
  begin
    if Result < 0 then
      break;
    if ShowXProgress then
      Progress.AdvanceXtra(1);
    CheckCancel;
    Result := 0;
    FSplitter.Raw := IncludeSpecs[I];
    if FSplitter.Error <> zasNone then
      raise EZipMaster.CreateMsgStr(ZM_Error(1118, ZS_InvalidParameter), FSplitter.Raw);
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

function TZMMergeOpr.ResolveConfirm(const ExistName: TZMString; ExistRec:
    TZMEntryBase; const ConflictName: TZMString; ConflictRec: TZMEntryBase;
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
    ExistEntry := TZM_ConflictEntry.Create(Master, ExistRec);
    try
      ExistEntry.ZipName := ExistName;
      ConflictEntry := TZM_ConflictEntry.Create(Master, ConflictRec);
      ConflictEntry.ZipName := ConflictName;
      Result := zmrRename;
      tmpZippedConflict(Master, ExistEntry, ConflictEntry, Result);
    finally
      ExistEntry.Free;
      ConflictEntry.Free;
    end;
    exit;
  end;
  Response := ZipMessageDlgEx(ZipLoadStr(ZS_FileConflict),
    Format(ZipLoadStr(ZS_Merge), [QualifiedName(ExistName, ExistRec.XName),
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

procedure TZMMergeOpr.SetArgsOptionsFromSplitter(var Args: TMOArgOptions;
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

procedure TZMMergeOpr.SetupAppendRecs(Allow: Boolean);
var
  DstZip: TZMZipReader;
  I: Integer;
  OrigCnt: Integer;
  Rec: TZMEntryMerger;
  ZRec: TZMEntryBase;
begin
  DstZip := FZipList[0];
  OrigCnt := DstZip.Count;
  if OrigCnt < 1 then
    exit;
  I := 0;
  ZRec := FOutZip.FirstRec;
  while (ZRec <> nil) and (I < OrigCnt) do
  begin
    CheckCancel;
    Rec := TZMEntryMerger(ZRec);
    ZRec := ZRec.Next;
    Inc(I);
    if (Rec = nil) or (Rec.StatusBit[zsbError or zsbDiscard or zsbSelected] <>
      zsbSelected) then
      Continue;
    Rec.Keep := Allow;
  end;
end;

{ TZMZipMerger }

procedure TZMZipMerger.AfterConstruction;
begin
  inherited;
  LastOpened := nil;
end;

function TZMZipMerger.CommitRec(Rec: TZMEntryWriter): Int64;
var
  err: Integer;
  InFile: TZMZipBase;
  MergeRec: TZMEntryMerger;
begin
  if (not(Rec is TZMEntryMerger)) or (TZMEntryMerger(Rec).Link = nil) then
  begin
    Result := inherited CommitRec(Rec);
    exit;
  end;
  MergeRec := TZMEntryMerger(Rec);
  if MergeRec.Keep then
  begin
    Result := MergeRec.Process;
  end
  else
  begin
    Result := MergeRec.ProcessSize;
    if Result > 0 then
    begin
      InFile := MergeRec.Link.MyFile;
      if (not InFile.IsOpen) and (InFile <> LastOpened) then
      begin
        Body.Reporter.Trace('Opening ' + InFile.Name(true));
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
      Result := MergeRec.Process;
    end;
  end;
end;

procedure TZMZipMerger.SetLastOpened(const Value: TZMZipBase);
begin
  if FLastOpened <> Value then
  begin
    if FLastOpened <> nil then
      FLastOpened.File_Close;
    FLastOpened := Value;
  end;
end;

procedure TZMEntryMerger.AfterConstruction;
begin
  inherited;
  Keep := false;
end;

function TZMEntryMerger.GetTitle: string;
begin
  Result := QualifiedName(Link.MyFile.Name, Link.FileName) + ' :: ' + FileName;
end;

function TZMEntryMerger.Process: Int64;
var
  LOH: TZipLocalHeader;
  Nxt: Int64;
begin
  if not Keep then
  begin
    Result := inherited Process;
    exit;
  end;
  // verify at correct header
  Result := SeekLocalData(LOH, _FileName);
  if Result >= 0 then
  begin
    Nxt := RelOffLocalHdr + ProcessSize;
    // Good - Position to next entry
    if MyFile.Seek(Nxt, soBeginning) = Nxt then
      Result := 0
    else
      Result := -ZM_Error(1344, ZS_SeekError);
  end
  else
  if Result < 0 then
    Result := -ZM_Error(1348, ZS_NoAppend);
end;

// return false = reject, too old
function TZMMergeArgs.Accept(Rec: TZMEntryBase): Boolean;
begin
  Result := true;
  if FromDate <> 0 then
    Result := FromDate <= Rec.ModifDateTime;
end;

procedure TZMMergeArgs.AfterConstruction;
begin
  inherited;
end;

procedure TZMMergeArgs.Assign(Other: TZMSelectArgs);
var
  Src: TZMMergeArgs;
begin
  inherited;
  if Other is TZMMergeArgs then
  begin
    Src := TZMMergeArgs(Other);
    FFromDate := Src.FromDate;
    Flist := Src.list;
    FNFlag := Src.NFlag;
    FXArg := Src.XArg;
    FZipName := Src.ZipName;
  end;
end;

end.
