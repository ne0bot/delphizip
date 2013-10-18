unit ZMCentral;

//  ZMCentral.pas - Represents the 'Central Directory' of a Zip file

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
//modified 2011-11-17

interface

{$INCLUDE   '.\ZipVers.inc'}

uses
  Classes, Windows, ZipMstr, ZMCore, ZMIRec, ZMWorkFile,
    ZMWZip, ZMCompat, ZMEOC;

type
  TZCChanges     = (zccNone, zccBegin, zccCount, zccAdd, zccEdit, zccDelete,
    zccEnd, zccCheckNo);
  TZCChangeEvent = procedure(Sender: TObject; idx: Integer;
    change: TZCChanges) of object;

type
  TVariableData = array of Byte;

type
  TZMCXFields = (zcxUncomp, zcxComp, zcxOffs, zcxStart);

type
  TIRecArray = array of TZMIRec;

type
  TZMZRec = class;
  // returns True selected
  TZMSelectedFunction = function(rec: TZMZRec; var Data): Boolean;

  TZMCentral = class(TZMEOC)
  private
    FCapacity: Integer;
    FCheckNo: Cardinal;
    FCount: Integer;
    FSelCount: integer;
    FOnChange:    TZCChangeEvent;
    FRecList: TIRecArray;
    FSFXOfs:      Cardinal;
    FSOCOfs: Int64;
    function GetItems(Idx: Integer): TZMZRec;
    procedure SetCount(const Value: Integer);
    procedure SetItems(Idx: Integer; const Value: TZMZRec);
  protected
    procedure ClearEntries; virtual;
    function CloneRec(Rec: TZMZRec): TZMZRec; virtual;
    function SelectEntry(Rec: TZMZRec; How: TZMSelects): Boolean;
    procedure SetCapacity(Value: Integer); virtual;
    function WriteCentral: Integer;
  public
    function Add(rec: TZMZRec): Integer; virtual;
    procedure AfterConstruction; override;
    procedure AssignFrom(Src: TZMWorkFile); override;
    procedure BeforeDestruction; override;
    procedure ClearCachedNames; virtual;
    procedure ClearSelection;
    function FindName(const Pattern: TZMString; var Idx: Integer): TZMZRec;
        overload;
    function FindName(const Pattern: TZMString; var Idx: Integer; const myself:
        TZMZRec): TZMZRec; overload;
    function FindNameEx(const Pattern: TZMString; var Idx: Integer; IsWild:
        boolean): TZMZRec; virtual;
    function HasDupName(const Rec: TZMZRec): Integer; virtual;
    function Load: Integer;
    function NextSelected(Current: Integer): integer;
    function TrimEntries(First: Integer): Integer; virtual;
    function Select(const Pattern: TZMString; How: TZMSelects): Integer;
    function SelectFile(const Pattern, Reject: TZMString; How: TZMSelects): Integer;
    function SelectFileEx(const Pattern, Reject: TZMString; How: TZMSelects;
        Func: TZMSelectedFunction; var Data): Integer;
    function SelectFiles(const want, reject: TStrings;
      skipped: TStrings): Integer;
    property Capacity: Integer read FCapacity write SetCapacity;
    property CheckNo: Cardinal read FCheckNo write FCheckNo;
    property Count: Integer read FCount write SetCount;
    property Items[Idx: Integer]: TZMZRec Read GetItems Write SetItems; default;
    property SelCount: integer read FSelCount write FSelCount;
    property SFXOfs: Cardinal Read FSFXOfs Write FSFXOfs;   // add to 'fields' ?
    property SOCOfs: Int64 read FSOCOfs write FSOCOfs;
    property OnChange: TZCChangeEvent Read FOnChange Write FOnChange;
  end;

  TZMZRec = class(TZMIRec)
  private
    FExtraField:     TZMRawBytes;
    FFileComLen:     Word;            //(2)
    FFileName:       TZMString;       // cache for external filename
    FFileNameLen:    Word;            //(2)
    FFlag:           Word;            //generalPurpose bitflag(2)
    FHeaderComment:  TZMRawBytes;  // internal comment
    FHeaderName:     TZMRawBytes;
    FLocalData:      TZMRawBytes;
    FMyFile: TZMCentral;
    FOrigHeaderName: TZMRawBytes;
    FVersionMadeBy: word;
    function GetEncodeAs: TZMEncodingOpts;
    function GetEncoding: TZMEncodingOpts;
    function GetHeaderComment: TZMRawBytes;
  protected
    procedure Diag(const Msg: TZMString);
    procedure FixMinimumVers(IsZ64: boolean);
    function FixStrings(const NewName, NewComment: TZMString): Integer;
    function FixXData64: Integer;
    function GetDataString(Cmnt: Boolean): UTF8String;
    function GetEncoded: TZMEncodingOpts; override;
    function GetEncrypted: Boolean; override;
    function GetExtraData(Tag: Word): TZMRawBytes; override;
    function GetExtraField: TZMRawBytes; override;
    function GetExtraFieldLength: Word; override;
    function GetFileComment: TZMString; override;
    function GetFileCommentLen: Word; override;
    function GetFileName: TZMString; override;
    function GetFlag: Word; override;
    function GetHeaderName: TZMRawBytes; override;
    function GetVersionMadeBy: Word; override;
    //1 convert internal Filename/Comment from utf
    function Int2UTF(Field: TZMRecStrings; NoUD: Boolean = False): TZMString;
//    //1 return true if Zip64 fields used
    procedure PrepareLocalData;
    procedure SetEncrypted(const Value: Boolean);
    procedure SetExtraData(Tag: Word; const Data: TZMRawBytes);
    function StripDrive(const FName: TZMString; NoPath: Boolean): TZMString;
    function StrToHeader(const AString: TZMString; How: THowToEnc): TZMRawBytes;
    function StrToSafe(const AString: TZMString; ToOem: boolean): AnsiString;
    function StrToUTF8Header(const AString: TZMString): TZMRawBytes;
    function StrTo_UTF8(const AString: TZMString): UTF8String;
    function ToIntForm(const ExtName: TZMString; var IntName: TZMString): Integer;
    function WriteAsLocal1(Stamp, Crc: Cardinal): Integer;
    function WriteDataDesc: Integer; override;
    property LocalData: TZMRawBytes read FLocalData write FLocalData;
//    //1 Header name before rename - needed to verify local header
    property OrigHeaderName: TZMRawBytes read FOrigHeaderName;
  public
    constructor Create(TheOwner: TZMCentral);
    procedure AssignFrom(const ZRec: TZMIRec); override;
    procedure BeforeDestruction; override;
    function ChangeAttrs(NewAttr: Cardinal): Integer; override;
    function ChangeComment(const NewComment: TZMString): Integer; override;
    function ChangeData(NewData: TZMRawBytes): Integer; override;
    function ChangeDate(NewDosDate: Cardinal): Integer; override;
    function ChangeEncoding: Integer; override;
    function ChangeName(const NewName: TZMString): Integer; override;
    procedure ClearCachedName;
    function LocalSize: Cardinal;
    function Read: Integer;
    function SeekLocalData: Integer; override;
    function Write: Integer;
    property EncodeAs: TZMEncodingOpts Read GetEncodeAs;
    property Encoding: TZMEncodingOpts Read GetEncoding;
    property Encrypted: Boolean Read GetEncrypted Write SetEncrypted;
    property ExtraData[Tag: Word]: TZMRawBytes read GetExtraData write
        SetExtraData;
    property ExtraField: TZMRawBytes read FExtraField write FExtraField;
    property ExtraFieldLength: Word read GetExtraFieldLength;
    property FileComLen: Word Read FFileComLen Write FFileComLen;
    property FileComment: TZMString Read GetFileComment;
    property FileCommentLen: Word Read FFileComLen Write FFileComLen;
    property FileName: TZMString Read GetFileName;
    property FileNameLen: Word Read FFileNameLen Write FFileNameLen;
    property FileNameLength: Word Read FFileNameLen Write FFileNameLen;
    property Flag: Word Read FFlag Write FFlag;
    property HeaderComment: TZMRawBytes read GetHeaderComment;
    property HeaderName: TZMRawBytes read GetHeaderName write FHeaderName;
    property MyFile: TZMCentral read FMyFile;
    property VersionMadeBy: word read FVersionMadeBy write FVersionMadeBy;
  end;

const
  BadIndex = -HIGH(Integer);

const
  zfi_Loaded: cardinal = $1000;     // central loaded
  zfi_DidLoad: cardinal = $2000;    // central loaded
  zfi_Invalid: Cardinal = $8000;    // needs reload

implementation

uses
  SysUtils, ZMMsg, ZMXcpt, ZMMsgStr, ZMStructs, ZMDelZip,
  ZMUtils, ZMMatch, ZMUTF8;

const
  __UNIT__ = 4 shl 23;

const
  AllSpec: String = '*.*';
  AnySpec: String = '*';

function TZMCentral.Add(Rec: TZMZRec): Integer;
begin
  if FCount >= Capacity then
    Capacity := FCount + 64; // allow it to grow
  Result := FCount;
  Inc(FCount);
  Items[Result] := Rec;
end;

procedure TZMCentral.AfterConstruction;
begin
  inherited;
  FCount := 0;
  FCapacity := 0;
end;

procedure TZMCentral.AssignFrom(Src: TZMWorkFile);
var
  Cnt: Integer;
  I: Integer;
  Rec: TZMZRec;
  TmpSrc : TZMCentral;
begin
  inherited; // assign everything up to 'EOC' level
  if (Src is TZMCentral) and (Src <> Self) then
  begin
    CheckNo := NextLoadCheckNo;
    TmpSrc := TZMCentral(Src);
    FOnChange := TmpSrc.FOnChange;
    FSFXOfs := TmpSrc.FSFXOfs;
    FSOCOfs := TmpSrc.FSOCOfs;
    // add records from Src
    Cnt := TmpSrc.Count;
    Capacity := Cnt;
    FSelCount := 0;
    for I := 0 to Cnt - 1 do
    begin
      Rec := TmpSrc[I];
      if (Rec = nil) or
        (Rec.StatusBit[zsbError or zsbDiscard] <> 0) then
        Continue;
      if Rec <> nil then
        Rec := CloneRec(Rec);
      Add(Rec);
      if Rec.Selected then
        Inc(FSelCount);
    end;
  end;
end;

procedure TZMCentral.BeforeDestruction;
begin
  ClearEntries;
  FRecList := nil;
  inherited;
end;

procedure TZMCentral.ClearCachedNames;
var
  i: Integer;
  tmp: TObject;
begin
  for i := 0 to Count - 1 do
  begin
    tmp := Items[i];
    if tmp is TZMZRec then
      TZMZRec(tmp).ClearCachedName;
  end;
end;

procedure TZMCentral.ClearEntries;
var
  i: Integer;
  tmp: TObject;
begin
  for i := 0 to pred(Count) do
  begin
    tmp := FRecList[i];
    if tmp <> nil then
    begin
      FRecList[i] := nil;
      tmp.Free;
    end;
  end;
  FRecList := nil;
  FCount := 0;
  FSelCount := 0;
  FCapacity := 0;
end;

procedure TZMCentral.ClearSelection;
var
  i: Integer;
  t: TZMZRec;
begin
  FSelCount := 0;
  for i := 0 to Count - 1 do
  begin
    t := Items[i];
    t.Selected := False;
  end;
end;

function TZMCentral.CloneRec(Rec: TZMZRec): TZMZRec;
begin
  Result := TZMZRec.Create(self);
  Result.AssignFrom(Rec);
end;

function TZMCentral.FindName(const Pattern: TZMString; var Idx: Integer):
    TZMZRec;
begin
  Result := FindNameEx(Pattern, Idx, not CanHash(pattern));
end;

function TZMCentral.FindName(const Pattern: TZMString; var Idx: Integer; const
    myself: TZMZRec): TZMZRec;
begin
  if myself = nil then
    Result := FindNameEx(Pattern, idx, not CanHash(pattern))
  else
  begin
    myself.SetStatusBit(zsbIgnore);  // prevent 'finding' myself
    Result := FindNameEx(Pattern, Idx, not CanHash(Pattern));
    myself.ClearStatusBit(zsbIgnore);
  end;
end;

function TZMCentral.FindNameEx(const Pattern: TZMString; var Idx: Integer;
    IsWild: boolean): TZMZRec;
var
  hash: Cardinal;
  rec: TZMZRec;
begin
  Result := nil;   // keep compiler happy
  hash := 0;       // keep compiler happy
  if (pattern <> '') then
  begin
    if not IsWild then
      hash := HashFuncNoCase(Pattern);
    inc(Idx);  // start at next
    if Idx >= 0 then
    begin
      while Idx < Count do
      begin
        rec := Items[Idx];
        if (rec.StatusBit[zsbIgnore] = 0) and (IsWild or (rec.Hash = hash)) and
          FileNameMatch(Pattern, rec.Filename) then
        begin
          Result := rec;
          Exit;
        end;
        Inc(idx);
      end;
    end;
  end;
  if Result = nil then
    Idx := BadIndex;
end;

function TZMCentral.GetItems(Idx: Integer): TZMZRec;
begin
  IndexCheck(Idx, fCount);
  Result := TZMZRec(FRecList[Idx]);
end;

// searches for record with same name
function TZMCentral.HasDupName(const rec: TZMZRec): Integer;
var
  dup: TZMZRec;
begin
  Result := -1;
  rec.SetStatusBit(zsbIgnore);
  try
    repeat
      dup := FindNameEx(rec.FileName, Result, False);
    until (dup = nil) or (dup.StatusBit[zsbDiscard] = 0); // ignore discarded
    if dup <> nil then
      Diag('Duplicate FileName: ' + rec.FileName);
  finally
    rec.ClearStatusBit(zsbIgnore);
  end;
end;

function TZMCentral.Load: Integer;
const
  __ERR_DS_UnknownError = __UNIT__ + (437 shl 10) + DS_UnknownError;
  __ERR_LI_WrongZipStruct = __UNIT__ + (476 shl 10) + LI_WrongZipStruct;
  __ERR_LI_ReadZipError = __UNIT__ + (477 shl 10) + LI_ReadZipError;
  __ERR_LI_ReadZipError1 = __UNIT__ + (487 shl 10) + LI_ReadZipError;
  __ERR_DS_CEHBadRead = __UNIT__ + (478 shl 10) + DS_CEHBadRead;
var
  i: Integer;
  LiE: Integer;
  OffsetDiff: Int64;
  r: Integer;
  rec: TZMZRec;
  sgn: Cardinal;
begin
  if not IsOpen then
  begin
    Result := -DS_FileOpen;
    exit;
  end;
  Result := -__ERR_DS_UnknownError;
  if (info and zfi_EOC) = 0 then
    exit; // should not get here if eoc has not been read
  LiE := 1;
  OffsetDiff := 0;
  ClearEntries;
  fCheckNo := NextLoadCheckNo;
  if Assigned(OnChange) then
    OnChange(Self, CheckNo, zccBegin);
  SOCOfs := CentralOffset;
  try
    OffsetDiff := CentralOffset;
    // Do we have to request for a previous disk first?
    if DiskNr <> CentralDiskNo then
    begin
      SeekDisk(CentralDiskNo);
      File_Size := Seek(0, 2);
    end
    else
    if not Z64 then
    begin
      // Due to the fact that v1.3 and v1.4x programs do not change the archives
      // EOC and CEH records in case of a SFX conversion (and back) we have to
      // make this extra check.
      OffsetDiff := File_Size - (Integer(CentralSize) +
        SizeOf(TZipEndOfCentral) + ZipCommentLen);
    end;
    SOCOfs := OffsetDiff;
    // save the location of the Start Of Central dir
    SFXOfs := Cardinal(OffsetDiff);
    if SFXOfs <> SOCOfs then
      SFXOfs := 0;
    // initialize this - we will reduce it later
    if File_Size = 22 then
      SFXOfs := 0;

    if CentralOffset <> OffsetDiff then
    begin
      // We need this in the ConvertXxx functions.
      ShowZipMessage(__ERR_LI_WrongZipStruct, '');
      CheckSeek(CentralOffset, 0, __ERR_LI_ReadZipError);
      CheckRead(sgn, 4, __ERR_DS_CEHBadRead);
      if sgn = CentralFileHeaderSig then
      begin
        SOCOfs := CentralOffset;
        // TODO warn - central size error
      end;
    end;

    // Now we can go to the start of the Central directory.
    CheckSeek(SOCOfs, 0, __ERR_LI_ReadZipError1);
    ProgReport(zacItem, PR_Loading, '', TotalEntries);
    // Read every entry: The central header and save the information.
{$IFDEF DEBUG}
      if Verbosity >= zvTrace then
        Diag(Format('List - expecting %d files', [TotalEntries]));
{$ENDIF}
    Capacity := TotalEntries;
    rec := nil;
    if Assigned(OnChange) then
      OnChange(Self, TotalEntries, zccCount);
    for i := 0 to (TotalEntries - 1) do
    begin
      FreeAndNil(rec);
      rec := TZMZRec.Create(Self);
      r := rec.Read;
      if r < 0 then
      begin
        FreeAndNil(rec);
        raise EZipMaster.CreateMsgDisp(r, True);
      end;
      if r > 0 then
        Z64 := True;
{$IFDEF DEBUG}
        if Verbosity >= zvTrace then //Trace then
          Diag(Format('List - [%d] "%s"', [i, rec.FileName]));
{$ENDIF}
      Add(rec);
      // Notify user, when needed, of the NextSelected entry in the ZipDir.
      if Assigned(OnChange) then
        OnChange(Self, i, zccAdd);   // change event to give TZipDirEntry

      // Calculate the earliest Local Header start
      if SFXOfs > rec.RelOffLocal then
        SFXOfs := rec.RelOffLocal;
      rec := nil; // used
      ProgReport(zacProgress, PR_Loading, '', 1);
      CheckCancel;
    end;  // for
    LiE := 0;                             // finished ok
    Result := 0;
    info := (info and not (zfi_MakeMask)) or zfi_Loaded;
  finally
    ProgReport(zacEndOfBatch, PR_Loading, '', 0);
    if LiE = 1 then
    begin
      FileName := '';
      SFXOfs := 0;
      File_Close;
    end
    else
    begin
      CentralOffset := SOCOfs;  // corrected
      // Correct the offset for v1.3 and 1.4x
      SFXOfs := SFXOfs + Cardinal(OffsetDiff - CentralOffset);
    end;

    // Let the user's program know we just refreshed the zip dir contents.
    if Assigned(OnChange) then
      OnChange(Self, Count, zccEnd);
  end;
end;

// return BadIndex when no more
function TZMCentral.NextSelected(Current: Integer): integer;
var
  k: Cardinal;
  mask: cardinal;
  rec: TZMZRec;
begin
  Result := BadIndex;
  mask := zsbSkipped or zsbSelected;
  if not UseDirOnlyEntries then
     mask := mask or zsbDirOnly;
  if Current >= -1 then
  begin
    while Current < Count -1 do
    begin
      inc(Current);
      rec := TZMZRec(Items[Current]);
      if rec <> nil then
      begin
        k := rec.StatusBit[mask];
        if k = zsbSelected then
        begin
          Result := Current;
          break;
        end;
      end;
    end;
  end;
end;

function TZMCentral.TrimEntries(First: Integer): Integer;
var
  tmp: TObject;
begin
  Result := 0;
  if First < 0 then
    First := 0;
  while FCount > First do
  begin
    Dec(FCount);
    tmp := FRecList[FCount];
    if tmp <> nil then
    begin
      FRecList[FCount] := nil;
      if TZMIRec(tmp).Selected then
        Dec(FSelCount);
      tmp.Free;
    end;
    Inc(Result);
  end;
end;

// select entries matching external Pattern - return number of selected entries
function TZMCentral.Select(const Pattern: TZMString; How: TZMSelects): Integer;
var
  i: Integer;
  srch: Integer;
  t: TZMZRec;
  wild: Boolean;
begin
  Result := 0;
  // if it wild or multiple we must try to match - else only if same hash
  wild := not CanHash(Pattern);
  if (Pattern = '') or
    (wild and ((Pattern = AllSpec) or (Pattern = AnySpec))) then
  begin
    // do all
    for i := 0 to Count - 1 do
    begin
      t := Items[i];
      if SelectEntry(t, How) then
        Inc(Result);
    end;
  end
  else
  begin
    // select specific Pattern
    i := -1;
    srch := 1;
    while srch <> 0 do
    begin
      t := FindNameEx(Pattern, i, wild);
      if t = nil then
        Break;
      if SelectEntry(t, How) then
        Inc(Result);
      if srch > 0 then
      begin
        if wild then
          srch := -1 // search all
        else
          srch := 0; // done
      end;
    end;
  end;
end;

// SelectFile entries matching external Pattern
function TZMCentral.SelectFile(const Pattern, Reject: TZMString; How:
    TZMSelects): Integer;
var
  i: Integer;
  exc: string;
  ptn: string;
  aRec: TZMZRec;
  wild: Boolean;
begin
  Result := 0;
  exc := Reject; // default excludes
  ptn := Pattern; // need to remove switches
  // split Pattern into Pattern and switches
  // if it wild or multiple we must try to match - else only if same hash
  wild := not CanHash(ptn);
  if (ptn = '') or (wild and ((ptn = AllSpec) or (ptn = AnySpec))) then
  begin
    // do all
    for i := 0 to Count - 1 do
    begin
      aRec := Items[i];
      if (exc <> '') and (FileNameMatch(exc, aRec.FileName)) then
        continue;
      SelectEntry(aRec, How);
      Inc(Result);
    end;
  end
  else
  begin
    // SelectFile specific Pattern
    i := -1;
    while True do
    begin
      aRec := FindNameEx(ptn, i, wild);
      if aRec = nil then
        Break; // no matches
      if (exc = '') or not(FileNameMatch(exc, aRec.FileName)) then
      begin
        SelectEntry(aRec, How);
        Inc(Result);
      end;
      if not wild then
        Break; // old find first
    end;
  end;
end;

// Func returns True to accept file
function TZMCentral.SelectFileEx(const Pattern, Reject: TZMString; How:
    TZMSelects; Func: TZMSelectedFunction; var Data): Integer;
var
  i: Integer;
  exc: string;
  ptn: string;
  aRec: TZMZRec;
  wild: Boolean;
begin
  Result := 0;
  exc := Reject; // default excludes
  ptn := Pattern; // need to remove switches
  // split Pattern into Pattern and switches
  // if it wild or multiple we must try to match - else only if same hash
  wild := not CanHash(ptn);
  if (ptn = '') or (wild and ((ptn = AllSpec) or (ptn = AnySpec))) then
  begin
    // do all
    for i := 0 to Count - 1 do
    begin
      aRec := Items[i];
      if (exc <> '') and (FileNameMatch(exc, aRec.FileName)) then
        continue;
      // do extra checking or processing
      if Assigned(Func) and not Func(aRec, Data) then
          Continue;
      SelectEntry(aRec, How);
      Inc(Result);
    end;
  end
  else
  begin
    // Select specific Pattern
    i := -1;
    while True do
    begin
      aRec := FindNameEx(ptn, i, wild);
      if aRec = nil then
        Break; // no matches
      if (exc = '') or not(FileNameMatch(exc, aRec.FileName)) then
      begin
        if Assigned(Func) and not Func(aRec, Data) then
            Continue;
        SelectEntry(aRec, How);
        Inc(Result);
      end;
      if not wild then
        Break; // old find first
    end;
  end;
end;


function TZMCentral.SelectEntry(Rec: TZMZRec; How: TZMSelects): Boolean;
begin
  Result := Rec.Select(How);
  if Result then
    Inc(FSelCount)
  else
    dec(FSelCount);
end;

function TZMCentral.SelectFiles(const want, reject: TStrings;
  skipped: TStrings): Integer;
var
  a: Integer;
  SelectsCount: Integer;
  exc: string;
  i: Integer;
  NoSelected: Integer;
  spec: String;
begin
  Result := 0;
  ClearSelection; // clear all
  SelectsCount := want.Count;
  if (SelectsCount < 1) or (Count < 1) then
    exit;
  exc := '';
  // combine rejects into a string
  if (reject <> nil) and (reject.Count > 0) then
  begin
    exc := reject[0];
    for i := 1 to reject.Count - 1 do
      exc := exc + ZSwitchFollows + reject[i];
  end;
  // attempt to select each wanted spec
  for a := 0 to SelectsCount - 1 do
  begin
    spec := want[a];
    NoSelected := SelectFile(spec, exc, zzsSet);//, '');
    if NoSelected < 1 then
    begin
      // none found
      if Verbosity >= zvVerbose then
        Diag('Skipped filespec ' + spec);
      if assigned(skipped) then
        skipped.Add(spec);
    end;
    if NoSelected > 0 then
      Result := Result + NoSelected;
    if NoSelected >= Count then
      Break; // all have been done
  end;
end;


procedure TZMCentral.SetCapacity(Value: Integer);
begin
  Value := (Value or 15) + 1; // 64 byte blocks
  if FCapacity < 0 then
    FCapacity := 0;
  if Value > FCapacity then
  begin
    SetLength(FRecList, Value);
    while FCapacity < Value do
    begin
      FRecList[FCapacity] := nil;
      Inc(FCapacity);
    end;
  end;
end;

procedure TZMCentral.SetCount(const Value: Integer);
begin
  // not allowed
end;

procedure TZMCentral.SetItems(Idx: Integer; const Value: TZMZRec);
const
  __ERR_GE_RangeError = __UNIT__ + (830 shl 10) + GE_RangeError;
var
  tmp: TObject;
begin
  if (Idx < 0) or (Idx >= FCount) then
    raise EZipMaster.CreateMsgFmt(__ERR_GE_RangeError, [Idx, FCount]);
//    raise ERangeError.Create('RecList index out of range');
  tmp := Items[Idx];
  if tmp <> Value then
  begin
    FRecList[Idx] := Value;
    tmp.Free;
  end;
end;

// returns bytes written or <0 _ error
function TZMCentral.WriteCentral: Integer;
var
  i: Integer;
  rec: TZMZRec;
  wrote: Integer;
begin
  Result := 0;
  wrote  := 0;
  CentralOffset := Position;
  CentralDiskNo := DiskNr;
  TotalEntries := 0;
  CentralEntries := 0;
  CentralSize := 0;
  ProgReport(zacXItem, PR_CentrlDir, '', Count);
  for i := 0 to Count - 1 do
  begin
    rec := TZMZRec(Items[i]);
    if rec.StatusBit[zsbError or zsbDiscard] = 0 then
    begin
      // no processing error
      if Verbosity >= zvTrace then
        Diag('Writing central [' + IntToStr(i) + '] ' + rec.FileName);
      // check for deleted?
      Result := rec.Write;
      if Result < 0 then
        break;      // error
      if Position <= Result then    // started new part
        CentralEntries := 0;
      wrote := wrote + Result;
      CentralSize  := CentralSize + Cardinal(Result);
      TotalEntries := TotalEntries + 1;
      CentralEntries := CentralEntries + 1;
      ProgReport(zacXProgress, PR_CentrlDir, '', 1);
    end
    else
      if Verbosity > zvVerbose then
        Diag('skipped Writing central ['+ IntToStr(i) + '] ' + rec.FileName);
  end;
  // finished Central
  if Result >= 0 then
  begin
//    FlushDiskBuffer; // 18/06/2012 9:38:10 AM
    Result := WriteEOC;
    if Result >= 0 then
    begin
      ProgReport(zacXProgress, PR_CentrlDir, '', 1);
      Result := wrote + Result;
      if (Result > 0) and (Verbosity > zvVerbose) then
        Diag('  finished ok');
    end;
  end;
end;

const
  MAX_BYTE = 255;

type
  Txdat64 = packed record
    tag:  Word;
    siz:  Word;
    vals: array [0..4] of Int64;  // last only cardinal
  end;

const
  ZipCenRecFields: array [0..17] of Integer =
    (4, 1, 1, 2, 2, 2, 2, 2, 4, 4, 4, 2, 2, 2, 2, 2, 4, 4);

// make safe version of external comment
function SafeComment(const xcomment: String): string;
var
  c: Char;
  i: integer;
Begin
  if StrHasExt(xcomment) then
    Result := StrToOEM(xcomment)
  else
    Result := xcomment;
  for i := 1 to Length(Result) do
  begin
    c := Result[i];
    if (c < ' ') or (c > #126) then
      Result[i] := '_';
  end;
End;

{ TZMZRec }
constructor TZMZRec.Create(TheOwner: TZMCentral);
begin
  inherited Create;
  FMyFile := TheOwner;
end;

procedure TZMZRec.AssignFrom(const ZRec: TZMIRec);
var
  src: TZMZRec;
begin
  inherited;
  if (ZRec <> self) and (ZRec is TZMZRec) then
  begin
    src := TZMZRec(ZRec);
    VersionMadeBy := src.VersionMadeBy;
    Flag  := src.Flag;
    FileNameLength := src.FileNameLength;
    FileCommentLen := src.FileCommentLen;
    FOrigHeaderName := src.OrigHeaderName;
    FHeaderName := ZRec.HeaderName;
    FHeaderComment := src.HeaderComment;
    fExtraField := src.fExtraField;
  end;
end;

procedure TZMZRec.BeforeDestruction;
begin
  FExtraField := '';
  FHeaderName := '';
  FHeaderComment := '';
  inherited;
end;

function TZMZRec.ChangeAttrs(NewAttr: Cardinal): Integer;
begin
  Result := 0; // always allowed
  if NewAttr <> GetExtFileAttrib then
  begin
    ExtFileAttrib := NewAttr;
    MarkDirty;
  end;
end;

function TZMZRec.ChangeComment(const NewComment: TZMString): Integer;
begin
  Result := 0; // always allowed
  if NewComment <> GetFileComment then
    Result := FixStrings(FileName, NewComment);
end;

function TZMZRec.ChangeData(NewData: TZMRawBytes): Integer;
const
  __ERR_CD_CEHDataSize = __UNIT__ + (998 shl 10) + CD_CEHDataSize;
var
  NowData: TZMRawBytes;
  OldData: TZMRawBytes;
begin
  Result := 0; // always allowed
  if NewData <> GetExtraField then
  begin
    // preserve required tags
    OldData := XDataKeep(ExtraField, [Zip64_data_tag, UPath_Data_Tag, UCmnt_Data_Tag]);
    // do not allow changing fields
    NowData := XDataRemove(NewData, [Zip64_data_tag, UPath_Data_Tag, UCmnt_Data_Tag]);
    // will it fit?
    if (Length(OldData) + Length(NowData) + Length(GetFileComment) +
          Length(GetFileName)) < MAX_WORD then
    begin
      fExtraField := OldData + NowData;
      MarkDirty;
    end
    else
      Result := -__ERR_CD_CEHDataSize;
  end;
end;

function TZMZRec.ChangeDate(NewDosDate: Cardinal): Integer;
const
  __ERR_RN_InvalidDateTime = __UNIT__ + (1014 shl 10) + RN_InvalidDateTime;
  __ERR_CD_NoProtected = __UNIT__ + (1007 shl 10) + CD_NoProtected;
begin
  Result := -__ERR_CD_NoProtected;
  if Encrypted then
    exit;
  try
    // test if valid date/time will throw error if not
    FileDateToDateTime(NewDosDate);
  except
    Result := -__ERR_RN_InvalidDateTime;
    if MyFile.Verbosity >= zvVerbose then
      Diag('Invalid date ' + GetFileName);
    exit;
  end;
  Result := 0;
  if NewDosDate <> GetDateTime then
  begin
    ModifDateTime := NewDosDate;
    MarkDirty;
  end;
end;

function TZMZRec.ChangeEncoding: Integer;
begin
  Result := FixStrings(FileName, FileComment);
end;

function TZMZRec.ChangeName(const NewName: TZMString): Integer;
const
  __ERR_CD_NoChangeDir = __UNIT__ + (1041 shl 10) + CD_NoChangeDir;
var
  iname: TZMString;
begin
  Result := ToIntForm(NewName, iname);
  if Result = 0 then
  begin
    Result := -__ERR_CD_NoChangeDir;
    if IsFolder(iname) <> IsFolder(HeaderName) then
      exit; // dirOnly status must be same
    if iname <> FileName then
      Result := FixStrings(iname, FileComment);
  end;
end;

procedure TZMZRec.ClearCachedName;
begin
  FFileName := '';  // force reconvert - settings have changed
  ClearStatusBit(zsbHashed);
  IsEncoded := zeoAuto; // force re-evaluate
end;

procedure TZMZRec.Diag(const Msg: TZMString);
begin
  if MyFile.Verbosity >= zvVerbose then
    MyFile.ShowMsg('Trace: ' + Msg, 0, False);
end;

procedure TZMZRec.FixMinimumVers(IsZ64: boolean);
const
  OS_FAT: Word = (FS_FAT * 256);
  WZIP         = (FS_NTFS * 256) + 50;
var
  NewNeed: Word;
begin
  if ((VersionMadeBy and VerMask) <= ZIP64_VER) and
    ((VersionNeeded and VerMask) <= ZIP64_VER) then
  begin
    // Enc := IsEncoded;
    if IsZ64 then
      VersionMadeBy := (VersionMadeBy and OSMask) or ZIP64_VER
    else
      if (VersionMadeBy and VerMask) = ZIP64_VER then
      begin
        // zip64 no longer needed
        VersionMadeBy := (VersionMadeBy and OSMask) or OUR_VEM;
      end;
    // correct bad encodings - marked ntfs should be fat
    if VersionMadeBy = WZIP then
      VersionMadeBy := OS_FAT or OUR_VEM;

    case ComprMethod of
      0:
        NewNeed := 10; // stored
      1 .. 8:
        NewNeed := 20;
      9:
        NewNeed := 21; // enhanced deflate
      10:
        NewNeed := 25; // DCL
      12:
        NewNeed := 46; // BZip2
    else
      NewNeed := ZIP64_VER;
    end;
    if ((Flag and 32) <> 0) and (NewNeed < 27) then
      NewNeed := 27;
    if IsZ64 and (NewNeed < ZIP64_VER) then
      NewNeed := ZIP64_VER;
    // keep needed os
    VersionNeeded := (VersionNeeded and OSMask) + NewNeed;
  end;
end;

function TZMZRec.FixStrings(const NewName, NewComment: TZMString): Integer;
const
  __ERR_AD_DuplFileName = __UNIT__ + (1136 shl 10) + AD_DuplFileName;
  __ERR_CD_CEHDataSize = __UNIT__ + (1213 shl 10) + CD_CEHDataSize;
var
  dup: TZMIRec;
  enc: TZMEncodingOpts;
  HasXComment: Boolean;
  HasXName: Boolean;
  hcomment: TZMRawBytes;
  IX: Integer;
  need64: Boolean;
  NeedU8Bit: Boolean;
  newdata: Boolean;
  NewHeaderName: TZMRawBytes;
  NewIntName: string;
  NewMadeFS: Word;
  UComment: UTF8String;
  UData: TZMRawBytes;
  uheader: TUString_Data_Header;
  UName: UTF8String;
  xlen: Integer;
begin
  enc := EncodeAs;
  NewMadeFS := (FS_FAT * 256) or OUR_VEM;
  UName  := '';
  UComment := '';
  NeedU8Bit := False;
  Result := -__ERR_AD_DuplFileName;
  ix := -1;  // from start
  repeat
    dup := (MyFile as TZMCentral).FindName(NewName, ix, self);
  until (dup = nil) or (dup.StatusBit[zsbDiscard] = 0); // ignore discarded
  if dup <> nil then
    exit; // duplicate external name
  NewIntName := SafeHeaderName(NewName);
  // default convert new name and comment to OEM
  NewHeaderName  := StrToHeader(NewIntName, hteOEM);
  hcomment := StrToHeader(NewComment, hteOEM);
  // make entry name
  HasXName := StrHasExt(NewName);
  HasXComment := StrHasExt(NewComment);
  // form required strings
  if HasXName or HasXComment then
  begin
    if enc = zeoAuto then
    begin
      enc := zeoUPATH;  // unless both extended
      if HasXName and HasXComment then
        enc := zeoUTF8;
    end;
    // convert strings
    if enc = zeoUTF8 then
    begin
      NewHeaderName  := StrToHeader(NewIntName, hteUTF8);
      hcomment := StrToHeader(NewComment, hteUTF8);
      NeedU8Bit := True;
    end
    else
    begin
      if enc = zeoUPath then
      begin
        // we want UPATH or/and UCOMMENT
        if HasXName then
          UName  := StrTo_UTF8(NewIntName);
        if HasXComment then
          UComment := StrTo_UTF8(NewComment);
      end
      else
      if enc = zeoNone then
      begin
        // we want Ansi name and comment - NTFS
        NewHeaderName  := StrToHeader(NewIntName, hteAnsi);
        hcomment := StrToHeader(NewComment, hteAnsi);
        if StrHasExt(NewHeaderName) or StrHasExt(hcomment) then
          NewMadeFS := (FS_NTFS * 256) or OUR_VEM; // wasn't made safe FAT
      end;
    end;
  end;
  // we now have the required strings
  // remove old extra strings
  UData := XDataRemove(GetExtraField, [UPath_Data_Tag, UCmnt_Data_Tag]);
  newdata := Length(UData) <> ExtraFieldLength;
  if UName <> '' then
  begin
    uheader.tag := UPath_Data_Tag;
    uheader.totsiz := sizeof(TUString_Data_Header) + Length(UName) - (2 * sizeof(Word));
    uheader.version := 1;
    uheader.origcrc := CalcCRC32(NewHeaderName[1], length(NewHeaderName), 0);
    XDataAppend(UData, uheader, sizeof(uheader), UName[1], length(UName));
    newdata := True;
  end;

  if UComment <> '' then
  begin
    // append UComment
    uheader.tag := UCmnt_Data_Tag;
    uheader.totsiz := sizeof(TUString_Data_Header) + Length(UComment) -
      (2 * sizeof(Word));
    uheader.version := 1;
    uheader.origcrc := CalcCRC32(hcomment[1], length(hcomment), 0);
    XDataAppend(UData, uheader, sizeof(uheader), UComment[1], length(UComment));
    newdata := True;
  end;
  // will it fit?
  Result := -__ERR_CD_CEHDataSize;
  xlen := Length(HeaderComment) + Length(NewHeaderName) + Length(UData);
  if xlen < MAX_WORD then
  begin
    // ok - make change
    FHeaderName  := NewHeaderName;
    FFileNameLen := Length(NewHeaderName);
    FHeaderComment := hcomment;
    FFileComLen := Length(hcomment);

    if newdata then
      ExtraField := UData;

    if NeedU8Bit then
      FFlag := FFlag or FLAG_UTF8_BIT
    else
      FFlag := FFlag and (not FLAG_UTF8_BIT);
    ClearCachedName;
    IsEncoded := zeoAuto;         // unknown
    need64 := (UncompressedSize >= MAX_UNSIGNED) or (CompressedSize >= MAX_UNSIGNED);
    // set versions to minimum required
    FVersionMadeBy := NewMadeFS;
    FixMinimumVers(need64);
    MarkDirty;
    Result := 0;
  end;
end;

 // 'fixes' the special Zip64  fields from extra data
 // return <0 error, 0 none, 1 Zip64
function TZMZRec.FixXData64: Integer;
const
  __ERR_DS_Zip64FieldError = __UNIT__ + (1257 shl 10) + DS_Zip64FieldError;
var
  idx: Integer;
  p: PAnsiChar;
  wsz: Integer;
begin
  Result := 0;
  if (VersionNeeded and VerMask) < ZIP64_VER then
    exit;
  if not XData(FExtraField, Zip64_data_tag, idx, wsz) then
    Exit;
  p := @fExtraField[idx];
  Result := -__ERR_DS_Zip64FieldError;  // new msg
  Inc(p, 4);  // past header
  Dec(wsz, 4);  // discount header
  if UncompressedSize = MAX_UNSIGNED then
  begin
    if wsz < 8 then
      exit;   // error
    UncompressedSize := pInt64(p)^;
    Inc(p, sizeof(Int64));
    Dec(wsz, sizeof(Int64));
  end;
  if CompressedSize = MAX_UNSIGNED then
  begin
    if wsz < 8 then
      exit;    // error
    CompressedSize := pInt64(p)^;
    Inc(p, sizeof(Int64));
    Dec(wsz, sizeof(Int64));
  end;
  if RelOffLocal = MAX_UNSIGNED then
  begin
    if wsz < 8 then
      exit;    // error
    RelOffLocal := pInt64(p)^;
    Inc(p, sizeof(Int64));
    Dec(wsz, sizeof(Int64));
  end;
  if DiskStart = MAX_WORD then
  begin
    if wsz < 4 then
      exit;   // error
    DiskStart := pCardinal(p)^;
  end;
  Result := 1;
end;

// will return empty if not exists or invalid
function TZMZRec.GetDataString(Cmnt: Boolean): UTF8String;
var
  crc: Cardinal;
  field: TZMRawBytes;
  idx: Integer;
  pH: PUString_Data_Header;
  pS: PAnsiChar;
  siz: Integer;
  tag: Word;
begin
  Result := '';
  if Cmnt then
  begin
    tag := UCmnt_Data_Tag;
    Field := HeaderComment;
    if field = '' then
      Exit; // no point checking
  end
  else
  begin
    tag := UPath_Data_Tag;
    field := HeaderName;
  end;
  if FindDataTag(tag, idx, siz) then
  begin
    pS := @ExtraField[idx];
    pH := PUString_Data_Header(pS);
    if pH^.version = 1 then
    begin
      crc := CalcCRC32(field[1], Length(field), 0);
      if pH^.origcrc = crc then
      begin
        siz := siz - sizeof(TUString_Data_Header);
        Inc(pS, sizeof(TUString_Data_Header));
        if (siz > 0) and (ValidUTF8(pS, siz) >= 0) then
        begin
          SetLength(Result, siz);
          move(pS^, Result[1], siz);
        end;
      end;
    end;
  end;
end;

function TZMZRec.GetEncodeAs: TZMEncodingOpts;
begin
  Result := (MyFile as TZMCentral).EncodeAs;
end;

{
  Encoded as OEM for
    DOS (default)                       FS_FAT
    OS/2                                FS_HPFS
    Win95/NT with Nico Mak's WinZip     FS_NTFS && host = 5.0
  UTF8 is flag is set
  except (someone always has to be different)
    PKZIP (Win) 2.5, 2.6, 4.0 - mark as FS_FAT but local is Windows ANSI (1252)
    PKZIP (Unix) 2.51 - mark as FS_FAT but are current code page
}
function TZMZRec.GetEncoded: TZMEncodingOpts;
const
  WZIP = $0B32;//(FS_NTFS * 256) + 50;
  OS_HPFS = FS_HPFS * 256;
  OS_FAT = FS_FAT * 256;
begin
  Result := zeoNone;

  if (Flag and FLAG_UTF8_BIT) <> 0 then
    Result := zeoUTF8
  else
  if (GetDataString(false) <> '') or (GetDataString(True) <> '') then
    Result := zeoUPath
  else
  if ((VersionMadeBy and OSMask) = OS_FAT) or
      ((VersionMadeBy and OSMask) = OS_HPFS) or
      (VersionMadeBy = WZIP) then
    Result := zeoOEM;
end;

function TZMZRec.GetEncoding: TZMEncodingOpts;
begin
  Result := (MyFile as TZMCentral).Encoding;
end;

function TZMZRec.GetEncrypted: Boolean;
begin
  Result := (FFlag and FLAG_CRYPT_BIT) <> 0;
end;

// returns the 'data' without the tag
function TZMZRec.GetExtraData(Tag: Word): TZMRawBytes;
var
  i: Integer;
  sz: Integer;
  x: TZMRawBytes;
begin
  Result := '';
  x := GetExtraField;
  if XData(x, Tag, i, sz) then
    Result := Copy(x, i + 4, sz - 4);
end;

function TZMZRec.GetExtraField: TZMRawBytes;
begin
  Result := fExtraField;
end;

function TZMZRec.GetExtraFieldLength: Word;
begin
  Result := Length(fExtraField);
end;

function TZMZRec.GetFileComment: TZMString;
begin
  Result := Int2UTF(zrsComment, False);
end;

function TZMZRec.GetFileCommentLen: Word;
begin
  Result := Length(HeaderComment);
end;

// returns the external filename interpretting the internal name by Encoding
// still in internal form
function TZMZRec.GetFileName: TZMString;
begin
  if FFileName = '' then
    FFileName := Int2UTF(zrsName, False);
  Result := FFileName;
end;

function TZMZRec.GetFlag: Word;
begin
  Result := FFlag;
end;

function TZMZRec.GetHeaderComment: TZMRawBytes;
begin
  Result := FHeaderComment;
end;

function TZMZRec.GetHeaderName: TZMRawBytes;
begin
  Result := FHeaderName;
end;

function TZMZRec.GetVersionMadeBy: Word;
begin
  Result := FVersionMadeBy;
end;

function TZMZRec.Int2UTF(Field: TZMRecStrings; NoUD: Boolean = False):
    TZMString;
var
  Enc: TZMEncodingOpts;
  fld: TZMRawBytes;
begin
  if Field = zrsComment then
    fld := HeaderComment
  else
    fld := HeaderName;
  Result := '';
  Enc := Encoding;
  if Enc = zeoAuto then
  begin
    Enc := IsEncoded; // cached Encoded; // how entry is encoded
    if NoUD and (Enc = zeoUPath) then
      Enc := zeoOEM;  // use header Field
  end;
  if (Enc = zeoUPath) or StrHasExt(fld) then
  begin
{$IFDEF UNICODE}
    case Enc of
      // use UTF8 extra data string if available
      zeoUPath: Result := UTF8ToWide(GetDataString(Field = zrsComment));
      zeoNone:  // treat as Ansi (from somewhere)
        Result := StrToUTFEx(fld, MyFile.Encoding_CP, -1);
      zeoUTF8:    // treat Field as being UTF8
        Result := PUTF8ToWideStr(PAnsiChar(fld), Length(fld));
      zeoOEM:    // convert to OEM
        Result := StrToUTFEx(fld, CP_OEMCP, -1);
    end;
{$ELSE}
    if UsingUtf8 then
    begin
      case Enc of
        // use UTF8 extra data string if available
        zeoUPath: Result := GetDataString(Field = zrsComment);
        zeoNone:  // treat as Ansi (from somewhere)
            Result := StrToUTFEx(fld, TZMCentral(MyFile).Encoding_CP, -1);
        zeoUTF8:    // treat Field as being UTF8
            Result := fld;
        zeoOEM:    // convert to OEM
            Result := StrToUTFEx(fld, CP_OEMCP, -1);
      end;
    end
    else
    begin
      case Enc of
        // use UTF8 extra data string if available
        zeoUPath: Result := UTF8ToSafe(GetDataString(Field = zrsComment), false);
        zeoNone:  // treat as Ansi (from somewhere)
            Result := StrToWideEx(fld, TZMCentral(MyFile).Encoding_CP, -1);  // will be converted
        zeoUTF8:    // treat Field as being UTF8
            Result := UTF8ToSafe(fld, false);
        zeoOEM:    // convert to OEM
            Result := StrToWideEx(fld, CP_OEMCP, -1);  // will be converted
      end;
    end;
{$ENDIF}
  end;
  if length(Result) = 0 then
    Result := String(fld); // better than nothing
  if Field = zrsName then
    Result := SetSlash(Result, psdExternal);
end;

// also calculate required version and create extra data
function TZMZRec.LocalSize: Cardinal;
begin
  Result := SizeOf(TZipLocalHeader);
  PrepareLocalData;    // form local extra data
  Inc(Result, FileNameLength + Length(LocalData));
end;

procedure TZMZRec.PrepareLocalData;
var
  xd: Txdat64;
  Need64: Boolean;
begin
  LocalData := '';  // empty
  ClearStatusBit(zsbLocal64);
  // check for Zip64
  Need64 := (UncompressedSize >= MAX_UNSIGNED) or (CompressedSize >= MAX_UNSIGNED);
  FixMinimumVers(Need64);
  if Need64 then
  begin
    SetStatusBit(zsbLocal64);
    xd.tag := Zip64_data_tag;
    xd.siz := 16;
    xd.vals[0] := UncompressedSize;
    xd.vals[1] := CompressedSize;
    SetLength(fLocalData, 20);
    Move(xd.tag, PAnsiChar(LocalData)^, 20);
  end;
  // remove unwanted 'old' tags
  if ExtraFieldLength > 0 then
    LocalData := LocalData + XDataRemove(ExtraField,
      [Zip64_data_tag, Ntfs_data_tag, UCmnt_Data_Tag]);
  SetStatusBit(zsbLocalDone);
end;

(*? TZMIRec.Read
  Reads directory entry
  returns
  >=0 = ok   (1 = Zip64)
  <0 = -error
*)
function TZMZRec.Read: Integer;
const
  __ERR_DS_CEHBadRead = __UNIT__ + (1578 shl 10) + DS_CEHBadRead;
  __ERR_DS_CEHWrongSig = __UNIT__ + (1583 shl 10) + DS_CEHWrongSig;
  __ERR_DS_CECommentLen = __UNIT__ + (1607 shl 10) + DS_CECommentLen;
  __ERR_DS_CENameLen = __UNIT__ + (1609 shl 10) + DS_CENameLen;
  __ERR_LI_ReadZipError = __UNIT__ + (1612 shl 10) + LI_ReadZipError;
var
  CH: TZipCentralHeader;
  ExtraLen: Word;
  n: TZMRawBytes;
  r: Integer;
  v: Integer;
  wf: TZMWorkZip;
begin
  wf := MyFile;
  ASSERT(assigned(wf), 'no WorkFile');
  //  Diag('Write central');
  Result := -1;
  if not wf.IsOpen then
    exit;
  StatusBits := zsbInvalid;
  //  Diag('read central' );
  r := wf.Reads(CH, ZipCenRecFields);
  if r <> SizeOf(TZipCentralHeader) then
  begin
    Result := -__ERR_DS_CEHBadRead;
    exit;
  end;
  if CH.HeaderSig <> CentralFileHeaderSig then
  begin
    Result := -__ERR_DS_CEHWrongSig;
    exit;
  end;
  VersionMadeBy := CH.VersionMadeBy;
  VersionNeeded := CH.VersionNeeded;
  Flag := CH.Flag;
  ComprMethod := CH.ComprMethod;
  ModifDateTime := CH.ModifDateTime;
  CRC32 := CH.CRC32;
  FileNameLength := CH.FileNameLen;
  ExtraLen := CH.ExtraLen;
  FileCommentLen := CH.FileComLen;
  DiskStart := CH.DiskStart;
  IntFileAttrib := CH.IntFileAtt;
  ExtFileAttrib := CH.ExtFileAtt;
  RelOffLocal := CH.RelOffLocal;
  CompressedSize := CH.ComprSize;
  UncompressedSize := CH.UncomprSize;
  // read variable length fields
  v := FileNameLen + ExtraLen + FileComLen;
  SetLength(n, v);
  r := wf.Reads(n[1], [FileNameLen, ExtraLen, FileComLen]);
  if r <> v then
  begin
    Result := -__ERR_DS_CECommentLen;
    if r < FileNameLen then
      Result := -__ERR_DS_CENameLen
    else
    if r < (FileNameLen + ExtraLen) then
      Result := -__ERR_LI_ReadZipError;
    exit;
  end;
  if FileComLen > 0 then
    FHeaderComment := copy(n, FileNameLen + ExtraLen + 1, FileComLen);
  if ExtraLen > 0 then
    fExtraField := copy(n, FileNameLen + 1, ExtraLen);
  SetLength(n, FileNameLen);
  FHeaderName := n;
  FOrigHeaderName := n;
  ClearStatusBit(zsbInvalid);   // record is valid
  if (n <> '') and (n[Length(n)] = PathDelimAlt) then
    SetStatusBit(zsbDirOnly);   // dir only entry
  Result := FixXData64;
end;

function TZMZRec.SeekLocalData: Integer;
const
  __ERR_DS_FileOpen = __UNIT__ + (1646 shl 10) + DS_FileOpen;
  __ERR_DS_LOHBadRead = __UNIT__ + (1649 shl 10) + DS_LOHBadRead;
  __ERR_DS_LOHWrongName = __UNIT__ + (1671 shl 10) + DS_LOHWrongName;
  __ERR_DS_UnknownError = __UNIT__ + (1690 shl 10) + DS_UnknownError;
const
  // no signature
  LOHFlds: array [0..9] of Integer = (2, 2, 2, 2, 2, 4, 4, 4, 2, 2);
var
  did: Int64;
  i: Integer;
  LOH: TZipLocalHeader;
  t: Integer;
  v: TZMRawBytes;
begin
  ASSERT(assigned(MyFile), 'no MyFile');
  //  Diag('Seeking local');
  Result := -__ERR_DS_FileOpen;
  if not MyFile.IsOpen then
    exit;
  Result := -__ERR_DS_LOHBadRead;
  try
    MyFile.SeekDisk(DiskStart);
    MyFile.Position := RelOffLocal;
    did := MyFile.Read(LOH, 4);
    if (did = 4) and (LOH.HeaderSig = LocalFileHeaderSig) then
    begin         // was local header
      did := MyFile.Reads(LOH.VersionNeeded, LOHFlds);
      if did = (sizeof(TZipLocalHeader) - 4) then
  begin
        if LOH.FileNameLen = Length(OrigHeaderName) then
        begin
          t := LOH.FileNameLen + LOH.ExtraLen;
          SetLength(v, t);
          did := MyFile.Reads(v[1], [LOH.FileNameLen, LOH.ExtraLen]);
          if (did = t) then
          begin
            Result := 0;
            for i := 1 to LOH.FileNameLen do
            begin
              if v[i] <> OrigHeaderName[i] then
              begin
                Result := -__ERR_DS_LOHWrongName;
                break;
              end;
            end;
          end;
        end;
        v := '';
      end;
    end;
    if ((-Result) and MSG_ID_MASK) = DS_LOHBadRead then            //&=
      Diag('could not read local header: ' + FileName);
  except
    on E: EZipMaster do
    begin
      Result := -E.ResId;
      exit;
    end;
    on E: Exception do
    begin
      Result := -__ERR_DS_UnknownError;
      exit;
    end;
  end;
end;

procedure TZMZRec.SetEncrypted(const Value: Boolean);
begin
  if Value then
    Flag := Flag or 1
  else
    Flag := Flag and $FFFE;
end;

// assumes data contains the data with no header
procedure TZMZRec.SetExtraData(Tag: Word; const data: TZMRawBytes);
var
  after: Integer;
  afterLen: integer;
  nidx: Integer;
  ix: Integer;
  newXData: TZMRawBytes;
  dataSize: Word;
  sz: Integer;
  v: Integer;
  x: TZMRawBytes;
begin
  x := GetExtraField;
  XData(x, Tag, ix, sz); // find existing Tag
  v := Length(x) - sz;   // size after old tag removed
  if Length(data) > 0 then
    v := v + Length(data) + 4;
  if v > MAX_WORD then     // new length too big?
    exit;     // maybe give error
  dataSize := Length(data);
  SetLength(newXData, v);
  nidx := 1;  // next index into newXData
  if (dataSize > 0) then
  begin
    // prefix required tag
    newXData[1] := AnsiChar(Tag and MAX_BYTE);
    newXData[2] := AnsiChar(Tag shr 8);
    newXData[3] := AnsiChar(dataSize and MAX_BYTE);
    newXData[4] := AnsiChar(dataSize shr 8);
    // add the data
    Move(data[1], newXData[5], dataSize);
    Inc(nidx, dataSize + 4);
  end;
  if ix >= 1 then
  begin
    // had existing data
    if ix > 1 then
    begin
      // append data from before existing tag
      Move(x[1], newXData[nidx], ix - 1);
      Inc(nidx, ix);
    end;
    after := ix + sz; // index after replaced tag
    if after < Length(x) then
    begin
      // append data from after existing
      afterLen := Length(x) + 1 - after;
      Move(x[after], newXData[nidx], afterLen);
    end;
  end
  else
  begin
    // did not exist
    if Length(x) > 0 then
      Move(x[1], newXData[nidx], Length(x)); // append old extra data
  end;
  ExtraField := newXData;
end;

// converts to internal delimiter
function TZMZRec.StripDrive(const FName: TZMString; NoPath: Boolean): TZMString;
var
  nam: Integer;
  posn: Integer;
begin
  Result := SetSlash(FName, psdExternal);
  // Remove drive: or //host/share
  posn := 0;
  if length(Result) > 1 then
  begin
    if Result[2{1}] = ':' then  // 8/08/2013 10:48:30 AM
    begin
      posn := 2;
      if (Length(Result) > 2) and (Result[3] = PathDelim{Alt}) then
        posn := 3;
    end
    else
    if (Result[1] = PathDelim{Alt}) and (Result[2] = PathDelim) then // 8/08/2013 10:52:20 AM
    begin
      posn := 3;
      while (posn < Length(Result)) and (Result[posn] <> PathDelim) do
        Inc(posn);
      Inc(posn);
      while (posn < Length(Result)) and (Result[posn] <> PathDelim{Alt}) do // 8/08/2013 10:53:05 AM
        Inc(posn);
      if posn >= Length(Result) then
      begin
        // error - invalid host/share
        Diag('Invalid filespec: ' + Result);
        Result := '';
        exit;
  end;
    end;
  end;
  Inc(posn);
  // remove leading ./
  if ((posn + 1) < Length(Result)) and (Result[posn] = '.') and
    (Result[posn + 1] = PathDelim) then
    posn := posn + 2;
  // remove path if not wanted
  if NoPath then
  begin
    nam := LastPos(Result, PathDelim);
    if nam > posn then
      posn := nam + 1;
  end;
  Result := Copy(Result, posn, MAX_PATH);
end;

function TZMZRec.StrToHeader(const AString: TZMString; How: THowToEnc):
    TZMRawBytes;
begin
{$IFDEF UNICODE}
  if how = hteUTF8 then
    Result  := TZMRawBytes(WideToUTF8(AString, -1))
  else
    Result  := TZMRawBytes(WideToSafe(AString, How = hteOEM));
{$ELSE}
  if UsingUTF8 then
  begin
    if how = hteUTF8 then
      Result  := TZMRawBytes(AString)
    else
      Result  := TZMRawBytes(WideToSafe(UTF8ToWide(AString), How = hteOEM));
  end
  else
  begin
    case how of
      hteOEM: Result := TZMRawBytes(StrToOEM(AString));
      hteAnsi: Result := TZMRawBytes(AString);
      hteUTF8: Result := TZMRawBytes(StrToUTF8(AString));
  end;
end;
{$ENDIF}
end;

function TZMZRec.StrToSafe(const AString: TZMString; ToOem: boolean):
    AnsiString;
begin
{$IFDEF UNICODE}
  Result := WideToSafe(AString, ToOem);
{$ELSE}
  if UsingUTF8 then
    Result := UTF8ToSafe(AString, ToOem)
  else
    Result := WideToSafe(AString, ToOem);
{$ENDIF}
end;

function TZMZRec.StrToUTF8Header(const AString: TZMString): TZMRawBytes;
begin
{$IFDEF UNICODE}
  Result := UTF8String(AString);
{$ELSE}
  if UsingUTF8 then
    Result := AsUTF8Str(AString) // make sure UTF8
  else
    Result  := StrToUTF8(AString);
{$ENDIF}
end;

function TZMZRec.StrTo_UTF8(const AString: TZMString): UTF8String;
begin
{$IFDEF UNICODE}
  Result := UTF8String(AString);
{$ELSE}
  if UsingUTF8 then
    Result := AsUTF8Str(AString) // make sure UTF8
  else
    Result  := StrToUTF8(AString);
{$ENDIF}
end;

function TZMZRec.ToIntForm(const ExtName: TZMString; var IntName: TZMString):
    Integer;
const
  __ERR_AD_BadFileName = __UNIT__ + (1895 shl 10) + AD_BadFileName;
var
  temp: TZMString;
begin
  Result := 0;
  IntName := StripDrive(ExtName, False);
  // truncate if too long
  if Length(IntName) > MAX_PATH then
  begin
    temp := IntName;
    SetLength(IntName, MAX_PATH);
    Diag('Truncated ' + temp + ' to ' + IntName);
  end;
  if IsInvalidIntName(IntName) then
    Result := -__ERR_AD_BadFileName;
end;

 // write the central entry on it's MyFile
 // return bytes written (< 0 = -Error)
function TZMZRec.Write: Integer;
const
  __ERR_DS_CEHBadWrite = __UNIT__ + (1996 shl 10) + DS_CEHBadWrite;
var
  CH: PZipCentralHeader;
  l: Integer;
  Need64: Boolean;
  ni: TZMRawBytes;
  p: pByte;
  pb: pByte;
  r: Integer;
  siz: Word;
  vals: array [0 .. 4] of Int64;
  wf: TZMWorkZip;
  x: TZMRawBytes;
begin
  wf := MyFile;
  ASSERT(assigned(wf), 'no WorkFile');
  //  Diag('Write central');
  Result := -1;
  if not wf.IsOpen then
    exit;
  FOrigHeaderName := HeaderName;  // might have changed
  pb := wf.WBuffer(sizeof(TZipCentralHeader));
  CH := PZipCentralHeader(pb);
  ni := HeaderName;
  CH^.HeaderSig := CentralFileHeaderSig;
  CH^.VersionMadeBy := VersionMadeBy;
  CH^.VersionNeeded := VersionNeeded;  // assumes local was written - may be updated
  CH^.Flag := Flag;
  CH^.ComprMethod := ComprMethod;
  CH^.ModifDateTime := ModifDateTime;
  CH^.CRC32 := CRC32;
  CH^.FileNameLen := length(ni);
  CH^.FileComLen := Length(HeaderComment);
  CH^.IntFileAtt := IntFileAttrib;
  CH^.ExtFileAtt := ExtFileAttrib;

  siz := 0;
  if (UncompressedSize >= MAX_UNSIGNED) then
  begin
    vals[0] := UncompressedSize;
    siz := 8;
    CH^.UncomprSize := MAX_UNSIGNED;
  end
  else
    CH^.UncomprSize := Cardinal(UncompressedSize);

  if (CompressedSize >= MAX_UNSIGNED) then
  begin
    vals[siz div 8] := CompressedSize;
    Inc(siz, 8);
    CH^.ComprSize := MAX_UNSIGNED;
  end
  else
    CH^.ComprSize := Cardinal(CompressedSize);

  if (RelOffLocal >= MAX_UNSIGNED) then
  begin
    vals[siz div 8] := RelOffLocal;
    Inc(siz, 8);
    CH^.RelOffLocal := MAX_UNSIGNED;
  end
  else
    CH^.RelOffLocal := Cardinal(RelOffLocal);

  if (DiskStart >= MAX_WORD) then
  begin
    vals[siz div 8] := DiskStart;
    Inc(siz, 4);
    CH^.DiskStart := MAX_WORD;
  end
  else
    CH^.DiskStart := Word(DiskStart);
  Need64 := False;
  if siz > 0 then
  begin
    SetLength(x, siz);
    move(vals[0], x[1], siz);
    Need64 := True;
    if (VersionNeeded and MAX_BYTE) < ZIP64_VER then
    begin
      FixMinimumVers(True);
      CH^.VersionNeeded := VersionNeeded;
      CH^.VersionMadeBy := VersionMadeBy;
  end;
    ExtraData[Zip64_data_tag] := x;
  end
  else
    ExtraData[Zip64_data_tag] := ''; // remove old 64 data
  if (StatusBit[zsbLocalDone] = 0) or (Need64) then
  FixMinimumVers(Need64);
  CH^.VersionMadeBy := VersionMadeBy;
  CH^.VersionNeeded := VersionNeeded;
  x := '';
  CH^.ExtraLen := ExtraFieldLength;
  Result := -__ERR_DS_CEHBadWrite;
  l  := sizeof(TZipCentralHeader) + CH^.FileNameLen + CH^.ExtraLen +
    CH^.FileComLen;
  pb := wf.WBuffer(l);
  p  := pb;
  Inc(p, sizeof(TZipCentralHeader));
  move(ni[1], p^, CH^.FileNameLen);
  Inc(p, CH^.FileNameLen);
  if CH^.ExtraLen > 0 then
  begin
    move(ExtraField[1], p^, CH^.ExtraLen);
    Inc(p, CH^.ExtraLen);
  end;
  if CH^.FileComLen > 0 then
    move(HeaderComment[1], p^, CH^.FileComLen);
  r := wf.Write(pb^, -l);
  if r = l then
  begin
    //    Diag('  Write central ok');
    Result := r;
    ClearStatusBit(zsbDirty);
  end//;
  else
  if r < 0 then
    Result := r;
end;

// write local header using specified stamp and crc
// return bytes written (< 0 = -Error)
function TZMZRec.WriteAsLocal1(Stamp, Crc: Cardinal): Integer;
const
  __ERR_DS_LOHBadWrite = __UNIT__ + (2100 shl 10) + DS_LOHBadWrite;
var
  cd: TZMRawBytes;
  fnlen: Integer;
  i: Integer;
  LOH: PZipLocalHeader;
  need64: Boolean;
  ni: TZMRawBytes;
  p: pByte;
  pb: pByte;
  t: Integer;
  wf: TZMWorkZip;
begin
  wf := MyFile;
  ASSERT(assigned(wf), 'no WorkFile');
  if StatusBit[zsbLocalDone] = 0 then
    PrepareLocalData;
  LOH := PZipLocalHeader(wf.WBuffer(sizeof(TZipLocalHeader)));
  if ((Flag and 9) = 8) then
    Flag := Flag and $FFF7; // remove extended local data if not encrypted
  ni := HeaderName;
  fnlen := length(ni);
  LOH^.HeaderSig := LocalFileHeaderSig;
  LOH^.VersionNeeded := VersionNeeded;   // may be updated
  LOH^.Flag := Flag;
  LOH^.ComprMethod := ComprMethod;
  LOH^.ModifDateTime := Stamp;
  LOH^.CRC32 := crc;
  LOH^.FileNameLen := fnlen;
  cd := LocalData;
  LOH^.ExtraLen := Length(cd); // created by LocalSize
  need64 := (LOH^.ExtraLen > 0) and (StatusBit[zsbLocal64] <> 0);
  if need64 then
  begin
    LOH^.UnComprSize := MAX_UNSIGNED;
    LOH^.ComprSize := MAX_UNSIGNED;
  end
  else
  begin
    if (Flag and 8) <> 0 then
    begin
      LOH^.UnComprSize := 0;
      LOH^.ComprSize := 0;
      if (VersionNeeded and MAX_BYTE) < ZIP64_VER then
      begin
        FixMinimumVers(True);
        LOH^.VersionNeeded := VersionNeeded;
      end;
    end
    else
    begin
      LOH^.UnComprSize := Cardinal(UncompressedSize);
      LOH^.ComprSize := Cardinal(CompressedSize);
    end;
  end;
  t := fnlen + Length(cd);
  pb := wf.WBuffer(sizeof(TZipLocalHeader) + t);
  p  := pb;
  Inc(p, sizeof(TZipLocalHeader));
  i := Sizeof(TZipLocalHeader);  // i = destination index
  Move(ni[1], p^, fnlen);
  i := i + fnlen;
  Inc(p, fnlen);
  // copy any extra data
  if Length(cd) > 0 then
  begin
    Move(cd[1], p^, Length(cd));
    Inc(i, Length(cd));
  end;
  Result := wf.Write(pb^, -i);  // must fit
  if Result = i then
    ClearStatusBit(zsbDirty)
  else
    Result := -__ERR_DS_LOHBadWrite;
end;

// return bytes written (< 0 = -Error)
function TZMZRec.WriteDataDesc: Integer;
const
  __ERR_DS_DataDesc = __UNIT__ + (2125 shl 10) + DS_DataDesc;
var
  d: TZipDataDescriptor;
  d64: TZipDataDescriptor64;
  r: Integer;
  wf: TZMWorkZip;
begin
  wf := MyFile;
  ASSERT(assigned(wf), 'no WorkFile');
  //  Diag('Write central');
  Result := -1;
  if not wf.IsOpen then
    exit;
//  ASSERT(assigned(OutZip), 'no WorkFile');
  if (Flag and 8) <> 0 then
  begin
    Result := 0;
    exit;
  end;
  Result := -__ERR_DS_DataDesc;
  if (VersionNeeded and MAX_BYTE) < ZIP64_VER then
  begin
    d.DataDescSig := ExtLocalSig;
    d.CRC32 := CRC32;
    d.ComprSize := Cardinal(CompressedSize);
    d.UnComprSize := Cardinal(UncompressedSize);
    r := wf.Write(d, -sizeof(TZipDataDescriptor));
    if r = sizeof(TZipDataDescriptor) then
      Result := r;
  end
  else
  begin
    d64.DataDescSig := ExtLocalSig;
    d64.CRC32 := CRC32;
    d64.ComprSize := CompressedSize;
    d64.UnComprSize := UncompressedSize;
    r := wf.Write(d64, -sizeof(TZipDataDescriptor64));
    if r = sizeof(TZipDataDescriptor64) then
      Result := r;
  end;
end;

end.
