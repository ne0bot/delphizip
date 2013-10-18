unit ZMZipFile;

//  ZMZipFile.pas - Represents the selectable Directory of a Zip file

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
//modified 2011-11-21

{$INCLUDE   '.\ZipVers.inc'}

interface

uses
  Classes, Windows, ZipMstr, ZMCore, ZMIRec, ZMHash, ZMWorkFile,
  ZMWZip, ZMCentral;

type
  TZMZipFile = class(TZMCentral)
  private
    FEOCFileTime: TFileTime;
    FFirst: Integer;
    FHashList: TZMDirHashList;
    FOpenRet: Integer;
    FQuickSearches: Boolean;
    FShowAll: Boolean;
    FStub: TMemoryStream;
    FUseSFX: Boolean;
    procedure SetQuickSearches(const Value: Boolean);
    procedure SetShowAll(const Value: Boolean);
    procedure SetStub(const Value: TMemoryStream);
  protected
    function BeforeCommit: Integer; virtual;
    function CalcSizes(var NoEntries: Integer; var ToProcess: Int64;
      var CenSize: Cardinal): Integer;
    procedure ClearEntries; override;
    function CommitCentral: Int64;
    function CommitRec(Rec: TZMZRec): Int64; virtual;
    function EOCSize(Is64: Boolean): Cardinal;
    procedure InferNumbering;
    procedure MarkDirty;
    function Open1(EOConly: Boolean): Integer;
    procedure SetCapacity(Value: Integer); override;
  public
    function Add(rec: TZMZRec): Integer; override;
    procedure AfterConstruction; override;
    procedure AssignFrom(Src: TZMWorkFile); override;
    procedure AssignStub(from: TZMZipFile);
    procedure BeforeDestruction; override;
    procedure ClearCachedNames; override;
    function Commit(MarkLatest: Boolean): Integer;
    function CommitAppend(Last: Integer; MarkLatest: Boolean): Integer;
    procedure DiscardEntry(Index: Integer);
    function Entry(Chk: Cardinal; Idx: Integer): TZMZRec;
    function FindNameEx(const pattern: TZMString; var Idx: Integer;
      IsWild: Boolean): TZMZRec; override;
    function HasDupName(const rec: TZMZRec): Integer; override;
    // 1 Mark as Contents Invalid
    procedure Invalidate;
    function Open(EOConly, NoLoad: Boolean): Integer;
    function PrepareWrite(typ: TZipWrites): Boolean;
    function TrimEntries(First: Integer): integer; override;
    function File_Reopen(Mode: Cardinal): Integer; override;
    // 1 Returns the number of duplicates
    function HashContents(what: Integer; AllowDuplicates: boolean): Integer;
    function RenameEntry(Index: Integer; const NewName: TZMString): integer;
    function VerifyOpen: Integer;
    property EOCFileTime: TFileTime Read FEOCFileTime;
    property First: Integer read FFirst;
    property OpenRet: Integer Read FOpenRet Write FOpenRet;
    property QuickSearches: Boolean read FQuickSearches write SetQuickSearches;
    property ShowAll: Boolean Read FShowAll Write SetShowAll;
    property Stub: TMemoryStream Read FStub Write SetStub;
    property UseSFX: Boolean Read FUseSFX Write FUseSFX;
  end;

type
  TZMCopyRec = class(TZMZRec)
  private
    FLink: TZMZRec;
  public
    constructor Create(theOwner: TZMCentral);
    procedure AfterConstruction; override;
    procedure AssignFrom(const ARec: TZMIRec); override;
    function Process: Int64; override;
    function ProcessSize: Int64; override;
    property Link: TZMZRec read FLink write FLink;
  end;

type
  TZMZipCopy = class(TZMZipFile)
  private
  protected
    function AffixZippedFile(rec: TZMZRec): Integer;
    function CloneRec(Rec: TZMZRec): TZMZRec; override;
  public
    function AffixZippedFiles(Src: TZMZipFile; All: Boolean): Integer;
    procedure AfterConstruction; override;
    function WriteFile(InZip: TZMZipFile; All: Boolean): Int64;
    function AppendCommit(MarkLatest: Boolean; FirstToWrite: Integer = 0): Integer;
  end;

  // const
  // BadIndex = -HIGH(Integer);
  //
  // const
  // zfi_Loaded: cardinal = $1000;     // central loaded
  // zfi_DidLoad: cardinal = $2000;    // central loaded
  // zfi_Invalid: Cardinal = $8000;    // needs reload

implementation

uses
  SysUtils, ZMMsg, ZMXcpt, ZMStructs, ZMDelZip,
  ZMUtils, ZMMatch , ZMEOC{$IFDEF VERpre6} , ZMCompat{$ENDIF};

const
  __UNIT__ = 33 shl 23;

const
  AllSpec: String = '*.*';
  AnySpec: String = '*';

function TZMZipFile.Add(rec: TZMZRec): Integer;
begin
  Result := inherited Add(rec);
  if not FHashList.Empty then
    FHashList.Add(Result, False);
end;

procedure TZMZipFile.AfterConstruction;
begin
  inherited;
  FHashList := TZMDirHashList.Create(Self);
  FQuickSearches := True; // default
end;

procedure TZMZipFile.AssignFrom(Src: TZMWorkFile);
var
  theSrc: TZMZipFile;
begin
  inherited;
  if (Src is TZMZipFile) and (Src <> Self) then
  begin
    theSrc := TZMZipFile(Src);
    FEOCFileTime := theSrc.FEOCFileTime;
    FFirst := theSrc.FFirst;
    FOpenRet := theSrc.FOpenRet;
    FShowAll := theSrc.FShowAll;
    FStub := nil;
    FUseSFX := False;
    if theSrc.UseSFX and assigned(theSrc.FStub) then
    begin
      FStub := TMemoryStream.Create;
      theSrc.FStub.Position := 0;
      if FStub.CopyFrom(theSrc.FStub, theSrc.FStub.size)
        = theSrc.FStub.size then
        FUseSFX := True
      else
        FreeAndNil(FStub);
    end;
    FQuickSearches := TZMZipFile(Src).QuickSearches;
    if QuickSearches then
      HashContents(0, True);
  end;
end;

procedure TZMZipFile.AssignStub(from: TZMZipFile);
begin
  FreeAndNil(FStub);
  FStub := from.Stub;
  from.FStub := nil;
end;

function TZMZipFile.BeforeCommit: Integer;
begin
  Result := 0;
end;

procedure TZMZipFile.BeforeDestruction;
begin
  FreeAndNil(FHashList);
  FreeAndNil(FStub);
  inherited;
end;

function TZMZipFile.CalcSizes(var NoEntries: Integer; var ToProcess: Int64;
  var CenSize: Cardinal): Integer;
var
  i: Integer;
  rec: TZMZRec;
begin
  Result := 0;
  for i := 0 to Count - 1 do
  begin
    rec := Items[i];
    ToProcess := ToProcess + rec.ProcessSize;
    CenSize := CenSize + rec.CentralSize;
    Inc(NoEntries);
  end;
end;

procedure TZMZipFile.ClearCachedNames;
begin
  FHashList.Clear; // this will disable quick find
  inherited;
end;

procedure TZMZipFile.ClearEntries;
begin
  if FHashList <> nil then
    FHashList.Clear;
  inherited;
  FFirst := -1;
end;

function TZMZipFile.Commit(MarkLatest: Boolean): Integer;
const
  __ERR_DS_NoWrite = __UNIT__ + (310 shl 10) + DS_NoWrite;
var
  I: Integer;
  Latest: Cardinal;
  NoEntries: Cardinal;
  Rec: TZMZRec;
  S: Cardinal;
  ToProcess: Int64;
  TotalProcess: Int64;
  Written: Int64;
  Wrote: Int64;
begin
  if Verbosity > zvVerbose then
    Diag('Commit file');
  Latest := 0;
  Wrote := 0;
  Result := BeforeCommit;
  if Result < 0 then
    exit;
  // calculate sizes
  NoEntries := 0;
  ToProcess := 0;
  for I := 0 to Count - 1 do
  begin
    CheckCancel;
    Rec := TZMZRec(Items[I]);
    if (Rec = nil) or (Rec.StatusBit[zsbError or zsbDiscard] <> 0) then
      Continue;
    ToProcess := ToProcess + Rec.ProcessSize;
    Inc(NoEntries);
    if MarkLatest and (Rec.ModifDateTime > Latest) then
      Latest := Rec.ModifDateTime;
  end;
  // mostly right ToProcess = total compressed sizes
  TotalProcess := ToProcess;
  if UseSFX and assigned(Stub) and (Stub.size > 0) then
    TotalProcess := TotalProcess + Stub.size;
  ProgReport(zacCount, PR_Writing, '', NoEntries + 1);
  ProgReport(zacSize, PR_Writing, '', TotalProcess);
  if Verbosity > zvVerbose then
  begin
    Diag(' to process ' + IntToStr(NoEntries) + ' entries');
    Diag(' size = ' + IntToStr(TotalProcess));
  end;
  Result := 0;
  if MarkLatest then
    StampDate := Latest;
  try
    // if out is going to split should write proper signal
    if IsMultiPart then
    begin
      S := ExtLocalSig;
      Result := Write(S, -4);
      if (Result <> 4) and (Result > 0) then
        Result := -__ERR_DS_NoWrite;
      Sig := zfsMulti;
    end
    else // write stub if required
      if UseSFX and assigned(Stub) and (Stub.size > 0) then
      begin
        // write the sfx stub
        ProgReport(zacItem, PR_SFX, '', Stub.size);
        Stub.Position := 0;
        Result := WriteFrom(Stub, Stub.size);
        if Result > 0 then
        begin
          Wrote := Stub.size;
          ProgReport(zacProgress, PR_SFX, '', Stub.size);
          if ShowProgress = zspFull then
            ProgDetail.Written(Wrote);
          Sig := zfsDOS; // assume correct
        end;
        ProgReport(zacItem, PR_SFX, '', -1); // end of item
      end
      else
        Sig := zfsLocal;
    if (Result >= 0) and (ToProcess > 0) then
    begin
      // write each local header and data
      for I := 0 to Count - 1 do
      begin
        CheckCancel;
        Rec := TZMZRec(Items[I]);
        if (Rec = nil) or (Rec.StatusBit[zsbError or zsbDiscard] <> 0) then
          Continue;
        Written := CommitRec(Rec);
        if Written < 0 then
        begin
          Result := Written;
          Break;
        end;
        Wrote := Wrote + Written;
        if ShowProgress = zspFull then
          TotalWritten := Wrote;
      end;
    end;
    // finished locals and data
    if Result >= 0 then
    begin
      // write central
      Written := CommitCentral;
      if Written >= 0 then
      begin
        Result := NoEntries;
        File_Size := Written + Wrote;
      end;
    end;
  finally
    ProgReport(zacEndOfBatch, 7, '', 0);
  end;
end;

function TZMZipFile.CommitAppend(Last: Integer; MarkLatest: Boolean): Integer;
var
  i: Integer;
  latest: Cardinal;
  NoEntries: Integer;
  r: Integer;
  rec: TZMZRec;
  ToProcess: Int64;
  TotalProcess: Int64;
  w64: Int64;
  wrote: Int64;
begin
  Diag('CommitAppend file');
  latest := 0;
  wrote := 0;
  // calculate sizes
  NoEntries := 0;
  ToProcess := 0;
  for i := 0 to Count - 1 do
  begin
    CheckCancel;
    rec := TZMZRec(Items[i]);
    Assert(assigned(rec), ' no rec');
    if i >= Last then
    begin
      ToProcess := ToProcess + rec.ProcessSize;
      Inc(NoEntries);
    end;
    if MarkLatest and (rec.ModifDateTime > latest) then
      latest := rec.ModifDateTime;
  end;
  // mostly right ToProcess = total compressed sizes
  TotalProcess := ToProcess;
  if UseSFX and assigned(Stub) and (Stub.size > 0) and (First < 0) then
    TotalProcess := TotalProcess + Stub.size;
  ProgReport(zacCount, PR_Writing, '', NoEntries + 1);
  ProgReport(zacSize, PR_Writing, '', TotalProcess);
  Diag(' to process ' + IntToStr(NoEntries) + ' entries');
  Diag(' size = ' + IntToStr(TotalProcess));
  Result := 0;
  if MarkLatest then
    StampDate := latest;
  try
    // write stub if required
    if UseSFX and assigned(Stub) and (Stub.size > 0) and (First < 0) then
    begin
      // write the sfx stub
      ProgReport(zacItem, PR_SFX, '', Stub.size);
      Stub.Position := 0;
      Result := WriteFrom(Stub, Stub.size);
      if Result > 0 then
      begin
        wrote := Stub.size;
        ProgReport(zacProgress, PR_SFX, '', Stub.size);
        if ShowProgress = zspFull then
          ProgDetail.Written(wrote);
        Sig := zfsDOS; // assume correct
      end;
    end
    else
      Sig := zfsLocal;
    if (Result >= 0) and (ToProcess > 0) then
    begin
      for i := Last + 1 to Count - 1 do
      begin
        CheckCancel;
        rec := TZMZRec(Items[i]);
        w64 := CommitRec(Rec);
        if w64 < 0 then
        begin
          Result := w64;
          Break;
        end;
        wrote := wrote + w64;
        if ShowProgress = zspFull then
          TotalWritten := wrote;
      end;
    end;
    // finished locals and data
    if Result >= 0 then
    begin
      // write central
      if Verbosity >= zvVerbose then
        ReportMsg(GE_Copying, [ZipLoadStr(DS_CopyCentral)]);
      r := WriteCentral; // uses XProgress
      if r >= 0 then
        wrote := wrote + r;
      Diag(' wrote = ' + IntToStr(wrote));
      if r > 0 then
      begin
        Result := 0;
        File_Size := wrote;
        Diag('  finished ok');
      end;
    end;
  finally
    ProgReport(zacEndOfBatch, 7, '', 0);
  end;
end;

function TZMZipFile.CommitCentral: Int64;
var
  Wrote: Int64;
begin
  // write central
  if Verbosity > zvVerbose then
    ReportMsg(GE_Copying, [ZipLoadStr(DS_CopyCentral)]);
  Result := WriteCentral; // uses XProgress
  if Result >= 0 then
  begin
    Wrote := Result;
    Result := FinishWrite;
    if Result >= 0 then
    begin
      Result := Result + Wrote;
      if Verbosity > zvVerbose then
        Diag('  finished ok');
    end;
  end;
end;

// write the record, return bytes written <0 on error
function TZMZipFile.CommitRec(Rec: TZMZRec): Int64;
begin
  Result := Rec.ProcessSize;
  if Result > 0 then
    Result := Rec.Process;
end;

// mark entry as discarded
procedure TZMZipFile.DiscardEntry(Index: Integer);
var
  ZRec: TZMZRec;
  Status: Cardinal;
begin
  if (Index < 0) or (Index >= Count) then
    Exit;
  ZRec := Items[Index];
  ZRec.SetStatusBit(zsbDiscard);
  Status := ZRec.ClearStatusBit(zsbSelected);
  if Status <> 0 then
    SelCount := SelCount - 1;
end;

function TZMZipFile.Entry(Chk: Cardinal; Idx: Integer): TZMZRec;
begin
  Result := nil;
  if (Chk = CheckNo) and (Idx >= 0) and (Idx < Count) then
    Result := Items[Idx];
end;

// Zip64 size aproximate only
function TZMZipFile.EOCSize(Is64: Boolean): Cardinal;
begin
  Result := Cardinal(sizeof(TZipEndOfCentral) + Length(ZipComment));
  if Is64 then
    Result := Result + sizeof(TZip64EOCLocator) + sizeof(TZipEOC64) +
      (3 * sizeof(Int64));
end;

function TZMZipFile.File_Reopen(Mode: Cardinal): Integer;
const
  __ERR_GE_FileChanged = __UNIT__ + (541 shl 10) + GE_FileChanged;
var
  WasWriting: boolean;
begin
  WasWriting := (OpenMode and (SysUtils.fmOpenReadWrite or SysUtils.fmOpenWrite)) <> 0;
  Result := inherited File_Reopen(Mode);
  if (Result = 0) and ((info and zfi_Loaded) <> 0) and
    not VerifyFileInformation(WasWriting) then
  begin
    Diag('File has changed! ' + RealFileName);
    // close it?
    Result := -__ERR_GE_FileChanged; // just complain at moment
  end;
end;

function TZMZipFile.FindNameEx(const pattern: TZMString; var Idx: Integer;
  IsWild: Boolean): TZMZRec;
begin
  Result := nil;
  if (pattern <> '') then
  begin
    // if it wild or multiple we must try to match - else only if same hash
    if (not IsWild) and (Idx < 0) and (FHashList.size > 0) then
    begin
      Idx := FHashList.Find(pattern, True); // include discarded entries
      if Idx >= 0 then
        Result := Items[Idx]; // do it quickly
    end
    else
      Result := inherited FindNameEx(pattern, idx, IsWild);
  end;
  if Result = nil then
    Idx := BadIndex;
end;

// searches for record with same name
function TZMZipFile.HasDupName(const rec: TZMZRec): Integer;
begin
  if FHashList.size = 0 then
  begin
    Result := inherited HasDupName(rec);
    exit; // not in empty list
  end;
  Result := FHashList.Find(rec.FileName, False);
  if (Result >= 0) and (Verbosity >= zvTrace) then
    ReportMessage(0, 'Duplicate FileName: ' + rec.FileName);
end;

// zsbDirty    = $1;
// zsbSelected = $2;
// zsbSkipped  = $4;
// zsbIgnore   = $8;
// zsbDirOnly  = $10;
// zsbInvalid  = $20;
// what = -1 _ all
// else ignore rubbish
// what = 0 _ any non rubbish
function TZMZipFile.HashContents(what: Integer; AllowDuplicates: boolean):
    Integer;
const
  Skip = zsbInvalid or zsbIgnore or zsbSkipped;
var
  i: Integer;
  rec: TZMZRec;
  use: Boolean;
begin
  Result := 0;
  if QuickSearches then
  begin
    FHashList.Clear;
    FHashList.AutoSize(Count); // make required size
    for i := 0 to Count - 1 do
    begin
      rec := Items[i];
      if rec = nil then
        continue;
      use := what = -1;
      if (not use) then
      begin
        if (rec.StatusBit[Skip] <> 0) then
          continue;
        use := (what = 0) or (rec.StatusBit[what] <> 0);
      end;
      if use and (FHashList.Add(i, AllowDuplicates) <> nil) then
          Inc(Result); // count duplicates
    end;
  end;
end;

// Use after EOC found and FileName is last part
// if removable has proper numbered volume name we assume it is numbered volume
procedure TZMZipFile.InferNumbering;
var
  fname: string;
  num: Integer;
  numStr: string;
begin
  // only if unknown
  if (Numbering = znsNone) and (TotalDisks > 1) then
  begin
    if WorkDrive.DriveIsFloppy and SameText(WorkDrive.DiskName,
      VolName(DiskNr)) then
      Numbering := znsVolume
    else
    begin
      numStr := '';
      fname := ExtractNameOfFile(FileName);
      Numbering := znsExt;
      if Length(fname) > 3 then
      begin
        numStr := Copy(fname, Length(fname) - 2, 3);
        num := StrToIntDef(numStr, -1);
        if num = (DiskNr + 1) then
        begin
          // ambiguous conflict
          if WorkDrive.DriveIsFixed and
            HasSpanSig(ChangeNumberedName(FileName, 1, True)) then
              Numbering := znsName; // unless there is an orphan
        end;
      end;
    end;
  end;
end;

//
procedure TZMZipFile.Invalidate;
begin
  info := info or zfi_Invalid;
end;

procedure TZMZipFile.MarkDirty;
begin
  info := info or zfi_Dirty;
end;

function TZMZipFile.Open(EOConly, NoLoad: Boolean): Integer;
const
  __ERR_DS_CECommentLen = __UNIT__ + (686 shl 10) + DS_CECommentLen;
  __ERR_LI_WrongZipStruct = __UNIT__ + (688 shl 10) + LI_WrongZipStruct;
var
  r: Integer;
begin
  // verify disk loaded
  ClearFileInformation;
  info := (info and zfi_MakeMask) or zfi_Loading;
  if WorkDrive.DriveIsFixed or WorkDrive.HasMedia(False) then
  begin
    Result := Open1(EOConly);
    if (Result >= 0) then
    begin
      LastWriteTime(FEOCFileTime);
      InferNumbering;
      if not(EOConly or NoLoad) then
      begin
        info := info or zfi_EOC;
        if (Result and EOCBadComment) <> 0 then
          ShowZipMessage(__ERR_DS_CECommentLen, '');
        if (Result and EOCBadStruct) <> 0 then
          ShowZipMessage(__ERR_LI_WrongZipStruct, '');
        r := Load;
        if r <> 0 then
          Result := r
        else
        begin
          info := info or zfi_Loaded or zfi_DidLoad;
          SaveFileInformation; // get details
        end;
      end;
    end;
  end
  else
    Result := -DS_NoInFile;
  OpenRet := Result;
  if Verbosity >= zvTrace then
  begin
    if Result < 0 then
      Diag('Open = ' + ZipLoadStr(-Result))
    else
      Diag('Open = ' + IntToStr(Result));
  end;
end;

function TZMZipFile.Open1(EOConly: Boolean): Integer;
const
  __ERR_DS_NoInFile = __UNIT__ + (753 shl 10) + DS_NoInFile;
var
  fn: string;
  SfxType: Integer;
  size: Integer;
begin
  SfxType := 0; // keep compiler happy
  ReqFileName := FileName;
  fn := FileName;
  Result := OpenEOC(EOConly);
  if (((-Result) and MSG_ID_MASK) = DS_FileOpen) and WorkDrive.DriveIsFixed then
    Exit;
  if (Result >= 0) and (Sig = zfsDOS) then
  begin
    Stub := nil;
    SfxType := CheckSFXType(handle, fn, size);
    if SfxType >= cstSFX17 then
    begin
      if Seek(0, 0) <> 0 then
        exit;
      Stub := TMemoryStream.Create;
      try
        if ReadTo(Stub, size) <> size then
        begin
          Stub := nil;
        end;
      except
        Stub := nil;
      end;
    end;
  end;
  if not(spExactName in SpanOptions) then
  begin
    if (Result >= 0) and (SfxType >= cstDetached) then
    begin // it is last part of detached sfx
      File_Close;
      // Get proper path and name
      FileName := IncludeTrailingBackslash(ExtractFilePath(ReqFileName)) + fn;
      // find last part
      Result := -__ERR_DS_NoInFile;
    end;
    if Result < 0 then
      Result := OpenLast(EOConly, Result);
  end;
end;

function TZMZipFile.PrepareWrite(typ: TZipWrites): Boolean;
begin
  case typ of
    zwSingle:
      Result := False;
    zwMultiple:
      Result := True;
  else
    Result := zwoDiskSpan in WriteOptions;
  end;
  IsMultiPart := Result;
  if Result then
  begin
    DiskNr := 0;
    File_Close;
  end
  else
    DiskNr := -1;
end;


function TZMZipFile.RenameEntry(Index: Integer; const NewName: TZMString):
    integer;
const
  __ERR_DS_UnknownError = __UNIT__ + (788 shl 10) + DS_UnknownError;
var
  ZRec: TZMZRec;
begin
  Result := -__ERR_DS_UnknownError;
  if (Index < 0) or (Index >= Count) then
    Exit;
  ZRec := Items[Index];
  Result := ZRec.ChangeName(NewName);
  if Result = 0 then
    QuickSearches := False; // will need to rehash
end;

function TZMZipFile.TrimEntries(First: Integer): integer;
begin
  Result := inherited TrimEntries(First);
  if QuickSearches then
    HashContents(0, False);  // remove removed entries
end;

procedure TZMZipFile.SetCapacity(Value: Integer);
begin
  inherited;
  if QuickSearches then
    FHashList.AutoSize(Value);
end;

procedure TZMZipFile.SetQuickSearches(const Value: Boolean);
begin
  if Value <> FQuickSearches then
  begin
    if Value then
      FHashList.AutoSize(Count)
    else
      FHashList.Clear;
    FQuickSearches := Value;
  end;
end;

procedure TZMZipFile.SetShowAll(const Value: Boolean);
begin
  FShowAll := Value;
end;

procedure TZMZipFile.SetStub(const Value: TMemoryStream);
begin
  if FStub <> Value then
  begin
    if assigned(FStub) then
      FStub.Free;
    FStub := Value;
  end;
end;

function TZMZipFile.VerifyOpen: Integer;
const
  __ERR_DS_FileOpen = __UNIT__ + (845 shl 10) + DS_FileOpen;
  __ERR_DS_FileChanged = __UNIT__ + (854 shl 10) + DS_FileChanged;
var
  ft: TFileTime;
begin
  Result := __ERR_DS_FileOpen;
  if not IsOpen and not File_Open(fmOpenRead or fmShareDenyWrite) then
    exit;
  if LastWriteTime(ft) then
  begin
    Result := 0;

    LastWriteTime(FEOCFileTime);
    if CompareFileTime(EOCFileTime, ft) <> 0 then
      Result := -__ERR_DS_FileChanged;
  end;
end;

constructor TZMCopyRec.Create(theOwner: TZMCentral);
begin
  inherited Create(theOwner);
end;

procedure TZMCopyRec.AfterConstruction;
begin
  inherited;
  FLink := nil;
end;

procedure TZMCopyRec.AssignFrom(const ARec: TZMIRec);
var
  Src: TZMCopyRec;
begin
  inherited;
  if (ARec <> self) and (ARec is TZMCopyRec) then
  begin
    Src := TZMCopyRec(ARec);
    FLink := Src.FLink;
  end;
end;

// process record, return bytes written; <0 = -error
function TZMCopyRec.Process: Int64;
const
  __ERR_DS_DataCopy = __UNIT__ + (924 shl 10) + DS_DataCopy;
var
  did: Int64;
  InRec: TZMZRec;
  InWorkFile: TZMWorkZip;
  stNr: Integer;
  stt: Int64;
  ToWrite: Int64;
  wrt: Int64;
begin
  // ASSERT(assigned(Owner), 'no owner');
  if MyFile.Verbosity >= zvVerbose then
    MyFile.ReportMsg(GE_Copying, [FileName]);
  InRec := Link;
  InWorkFile := TZMZRec(InRec).MyFile;
  if MyFile.Verbosity > zvVerbose then
    Diag('Copying local');
  Result := InRec.SeekLocalData;
  if Result < 0 then
    exit; // error
  stNr := MyFile.DiskNr;
  stt := MyFile.Position;
  Result := WriteAsLocal1(ModifDateTime, CRC32);
  if Result < 0 then
    exit; // error
  wrt := Result;
  MyFile.ProgReport(zacProgress, PR_Copying, '', wrt);
  // Diag('  finished copy local');
  // ok so update positions
  RelOffLocal := stt;
  DiskStart := stNr;
  ToWrite := CompressedSize;
  // Diag('copying zipped data');
  MyFile.ProgReport(zacItem, PR_Copying, '', ToWrite);
  did := MyFile.CopyFrom(InWorkFile, ToWrite);
  if did <> ToWrite then
  begin
    if did < 0 then
      Result := did // write error
    else
      Result := -__ERR_DS_DataCopy;
    exit;
  end;
  wrt := wrt + did;
  if (Flag and 8) <> 0 then
  begin
    did := WriteDataDesc;
    if did < 0 then
    begin
      Result := did; // error
      exit;
    end;
    wrt := wrt + did;
    MyFile.ProgReport(zacProgress, PR_Copying, '', did);
  end;
  MyFile.ProgReport(zacItem, PR_Copying, FileName, -1); // end of item
  Result := wrt;
end;

// return bytes to be processed
function TZMCopyRec.ProcessSize: Int64;
begin
  Result := CompressedSize + LocalSize;
  if HasDataDesc then
    Result := Result + sizeof(TZipDataDescriptor);
end;

// Add a copy of source record if name is unique
function TZMZipCopy.AffixZippedFile(rec: TZMZRec): Integer;
const
  __ERR_AD_DuplFileName = __UNIT__ + (958 shl 10) + AD_DuplFileName;
var
  nrec: TZMCopyRec;
begin
  Result := -__ERR_AD_DuplFileName;
  if HasDupName(rec) < 0 then
  begin
    // accept it
    nrec := TZMCopyRec.Create(Self); // make a copy
    nrec.AssignFrom(rec);
    nrec.Link := rec; // link to original
    Result := Add(nrec);
  end;
end;

// return >=0 number added <0 error
function TZMZipCopy.AffixZippedFiles(Src: TZMZipFile; All: Boolean): Integer;
var
  cnt: Integer;
  i: Integer;
  r: Integer;
  rec: TZMZRec;
begin
  Result := 0;
  // calculate capacity required
  if All then
    cnt := Count + Src.Count
  else
    cnt := Count + Src.SelCount;
  if cnt > Capacity then
    Capacity := cnt;
  for i := 0 to Src.Count - 1 do
  begin
    rec := Src[i];
    if not assigned(rec) then
      continue;
    if All or rec.TestStatusBit(zsbSelected) then
    begin
      if Verbosity > zvVerbose then
      Diag('including: ' + rec.FileName);
      r := AffixZippedFile(rec);
      if (r >= 0) then
        Inc(Result) // added
      else
      begin
        // error
        if r < 0 then
          Result := r;
      end;
    end
    else
      if Verbosity > zvVerbose then
      Diag('ignoring: ' + rec.FileName);
  end;
end;

procedure TZMZipCopy.AfterConstruction;
begin
  inherited;
  QuickSearches := False;
end;

function TZMZipCopy.AppendCommit(MarkLatest: Boolean; FirstToWrite: Integer):
    Integer;
var
  DstZip: TZMZipFile;
  ExistingEntries: Cardinal;
  FirstOffset: Int64;
  I: Integer;
  Latest: Cardinal;
  NoEntries: Cardinal;
  Rec: TZMCopyRec;
  RecsWritten: Cardinal;
  ToProcess: Int64;
  TotalProcess: Int64;
  Written: Int64;
  Wrote: Int64;
begin
  if Verbosity > zvVerbose then
    Diag('Append Commit file');
  if IsMultiPart or UseSFX or (SOCOfs = 0) or (FirstToWrite < 1) then
  begin
    Result := -1; // cannot append
    Exit;
  end;
  Latest := 0;
  Wrote := 0;
  Result := BeforeCommit;
  if Result < 0 then
    exit;
  // calculate sizes
  NoEntries := 0;
  ToProcess := 0;
  ExistingEntries := 0;
  RecsWritten := 0;
  Result := -1;
  DstZip := nil;
  for I := 0 to FirstToWrite - 1 do
  begin
    CheckCancel;
    Rec := TZMCopyRec(Items[I]);
    if (Rec = nil) or (Rec.StatusBit[zsbError or zsbDiscard or zsbRenamed] <> 0) then
      Exit;
    if ((I > 0) and (Rec.RelOffLocal = 0)) or (Rec.DiskStart <> 0) then
      Exit;
    if DstZip = nil then
      DstZip := TZMZipFile(Rec.Link.MyFile)
    else
      if DstZip <> TZMZipFile(Rec.Link.MyFile) then
        Exit;
    Inc(ExistingEntries);
    if MarkLatest and (Rec.ModifDateTime > Latest) then
      Latest := Rec.ModifDateTime;
  end;
  for I := FirstToWrite to Count - 1 do
  begin
    CheckCancel;
    Rec := TZMCopyRec(Items[I]);
    if (Rec = nil) or (Rec.StatusBit[zsbError or zsbDiscard] <> 0) then
      Continue;
    if DstZip = TZMZipFile(Rec.Link.MyFile) then
      Exit;
    ToProcess := ToProcess + Rec.ProcessSize;
    Inc(NoEntries);
    if MarkLatest and (Rec.ModifDateTime > Latest) then
      Latest := Rec.ModifDateTime;
  end;
  // mostly right ToProcess = total compressed sizes
  TotalProcess := ToProcess;
  // we are now comitted to appending
  DstZip.File_Close;
  FileName := DstZip.FileName;
  if FirstToWrite = DstZip.Count then
    FirstOffset := DstZip.SOCOfs
  else
    FirstOffset := DstZip[FirstToWrite].RelOffLocal;
  for I := 0 to FirstToWrite - 1 do
  begin
    CheckCancel;
    Rec := TZMCopyRec(Items[I]);
    Rec.Link := nil;  // unlink
  end;
  Result := File_Reopen(fmOpenReadWrite);
  if Result < 0 then
    Exit; // could not reopen

  ProgReport(zacCount, PR_Writing, '', NoEntries + 1);
  ProgReport(zacSize, PR_Writing, '', TotalProcess);
  if Verbosity > zvVerbose then
  begin
    Diag(' to process ' + IntToStr(NoEntries) + ' new entries');
    Diag(' size = ' + IntToStr(TotalProcess));
  end;
  Result := 0;
  if MarkLatest then
  begin
    // Diag(' Latest date = ' + DateTimeToStr(FileDateToLocalDateTime(Latest)));
    StampDate := Latest;
  end;
  try
    Sig := zfsLocal;
    if (Result >= 0) and (ToProcess > 0) then
    begin
      if Verbosity > zvVerbose then
        Diag('  Appending records');
      CheckSeek(FirstOffset, 0, 0);
      for I := FirstToWrite to Count - 1 do
      begin
        CheckCancel;
        Rec := TZMCopyRec(Items[I]);
        if (Rec = nil) or (Rec.StatusBit[zsbError or zsbDiscard] <> 0) then
          Continue;
        Written := CommitRec(Rec);
        if Written < 0 then
        begin
          Result := Written;
          Break;
        end;
        Wrote := Wrote + Written;
        if ShowProgress = zspFull then
          TotalWritten := Wrote;
      end;
    end;
    // finished locals and data
    if Result >= 0 then
    begin
      // write central
      Written := CommitCentral;
      Assert((ExistingEntries + NoEntries) = CentralEntries, 'Wrong Central Entries count');
      if Written >= 0 then
      begin
        Result := RecsWritten;
        File_Size := Written + Wrote;
      end;
    end;
  finally
    ProgReport(zacEndOfBatch, 7, '', 0);
  end;
end;

function TZMZipCopy.CloneRec(Rec: TZMZRec): TZMZRec;
begin
  Result := TZMCopyRec.Create(self);
  Result.AssignFrom(Rec);
end;

// copies selected files from InZip
function TZMZipCopy.WriteFile(InZip: TZMZipFile; All: Boolean): Int64;
begin
  Assert(assigned(InZip), 'no input');
  Diag('Write file');
  Result := InZip.VerifyOpen; // verify unchanged and open
  if Result >= 0 then
  begin
    ZipComment := InZip.ZipComment;
    Result := AffixZippedFiles(InZip, All);
    if Result >= 0 then
      Result := Commit(zwoZipTime in WriteOptions);
  end;
end;

end.
