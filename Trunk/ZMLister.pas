unit ZMLister;

//  ZMLister.pas - Loads and 'lists' zips

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
  Windows, SysUtils, Classes, Graphics, ZipMstr, ZMSFXInt,
  ZMStructs, ZMCompat, ZMZipFile, ZMCore, ZMCenDir;

type
  // None = no reload, Reload = reload non-detached, Any = reload any, Clear = clear
  TZMReloads = (zlrNone, zlrClear, zlrLoaded, zlrReload, zlrAny);

type
  TZMLister = class(TZMCore)
  private
    fCentralDir: TZMCenDir;
    fReload: TZMReloads;
    function GetCurrent: TZMZipFile;
    function GetCurrentIsValid: boolean;
    function GetZipComment: AnsiString;
    function GetZipFileName: TZMString;
    function MapSFXSettings17(pheder: PByte; stub: TMemoryStream): Integer;
    function MapSFXSettings19(pheder: PByte; stub: TMemoryStream): Integer;
    function ReadSFXStr17(var p: PByte; len: Byte): AnsiString;
    procedure SetCurrent(const Value: TZMZipFile);
    procedure SetCurrentIsValid(const Value: boolean);
    procedure SetZipComment(const Value: AnsiString);
  protected
    procedure Finished(WasGood: boolean); override;
    function IsDetachSFX(zfile: TZMZipFile): boolean;
    function LoadSFXStr(ptbl: PByte; ident: Byte): string;
    function MapOptionsFrom17(opts: Word): TZMSFXOpts;
    function MapOptionsFromStub(opts: Word): TZMSFXOpts;
    function MapOverwriteModeFromStub(ovr: Word): TZMOvrOpts;
    // 1 return true if it was there
    function MapSFXSettings(stub: TMemoryStream): Integer;
    procedure Started; override;
    function TrimDetached(stub: TMemoryStream): Boolean;
    procedure UpdateAuxProperties;
  public
    constructor Create(theMaster: TCustomZipMaster);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ClearCachedNames;
    function CurrentZip(MustExist: boolean; SafePart: boolean = false)
      : TZMZipFile;
    function ForEach(func: TZMForEachFunction; var data): Integer;
    function IsDetachedSFX(const filename: string): boolean;
    procedure List;
    procedure LoadZip(const ZipName: string; NoEvent: boolean);
    procedure Set_ZipFileName(const zname: string; Load: TZLoadOpts);
    procedure UseDirOnlyEntriesChanged(Value: boolean);
    property CentralDir: TZMCenDir read fCentralDir;
    property Current: TZMZipFile read GetCurrent write SetCurrent;
    property CurrentIsValid: boolean read GetCurrentIsValid write SetCurrentIsValid;
    property Reload: TZMReloads read fReload write fReload;
    property ZipComment: AnsiString read GetZipComment write SetZipComment;
    property ZipFileName: TZMString read GetZipFileName;
  end;

implementation

uses
  Dialogs, ZMMsg, ZMUtils, ZMXcpt, ZMMsgStr, ZMWZip,
  ZMIRec, ZMUTF8, ZMInflt, ZMWorkFile, ZMCentral, ZMDelZip;

const
  __UNIT__ = 20 shl 23;

const
  MinStubSize = 12000;


constructor TZMLister.Create(theMaster: TCustomZipMaster);
begin
  inherited Create(theMaster, nil);
end;

{ TZMLister }

procedure TZMLister.AfterConstruction;
begin
  inherited;
  fCentralDir := TZMCenDir.Create(self);
  fCentralDir.UseDirOnlyEntries := UseDirOnlyEntries;
end;

procedure TZMLister.BeforeDestruction;
begin
  fCentralDir.Free;
  inherited;
end;

procedure TZMLister.ClearCachedNames;
var
  cz: TZMZipFile;
begin
  cz := CentralDir.TheCurrent;
  if cz <> nil then
    cz.ClearCachedNames;  // update for any changed settings
end;

function TZMLister.GetCurrent: TZMZipFile;
begin
  Result := CentralDir.Current;
end;

function TZMLister.CurrentZip(MustExist: boolean; SafePart: boolean = false)
  : TZMZipFile;
const
  __ERR_GE_NoZipSpecified = __UNIT__ + (162 shl 10) + GE_NoZipSpecified;
  __ERR_DS_NoValidZip = __UNIT__ + (165 shl 10) + DS_NoValidZip;
  __ERR_GE_Abort = __UNIT__ + (169 shl 10) + GE_Abort;
begin
  if ZipFileName = '' then
    raise EZipMaster.CreateMsgDisp(__ERR_GE_NoZipSpecified, true);
  Result := CentralDir.Current;
  if MustExist and ((zfi_Loaded and Result.info) = 0) then
    raise EZipMaster.CreateMsgDisp(__ERR_DS_NoValidZip, true);
  if SafePart and ((zfi_Cancelled and Result.info) <> 0) then
  begin
    if Result.AskAnotherDisk(ZipFileName) = idCancel then
      raise EZipMaster.CreateMsgDisp(__ERR_GE_Abort, false);
    Result.info := 0; // clear error
  end;

  if Result.filename = '' then
  begin
    // creating new file
    Result.filename := ZipFileName;
    Result.ReqFileName := ZipFileName;
  end;
end;

procedure TZMLister.Finished(WasGood: boolean);
var
  czip: TZMZipFile;
  tmpDirUpdate: TNotifyEvent;
begin
  if WasGood then
  begin
    case Reload of
      // zlrNone:
      // ;
      zlrClear:
        begin
          CentralDir.Current := nil; // close and remove any old file
          tmpDirUpdate := Master.OnDirUpdate;
          if assigned(tmpDirUpdate) then
            tmpDirUpdate(Master);
        end;
      zlrReload:
        if not IsDetachedSFX(ZipFileName) then
          LoadZip(ZipFileName, false);
      zlrAny:
        LoadZip(ZipFileName, false);
    end;
    if Reload > zlrClear then
      UpdateAuxProperties;  // update Aux properties from current
  end
  else
  begin
    czip := CentralDir.Current;
    if czip.info <> 0 then
      czip.info := (czip.info and zfi_Cancelled) or zfi_Error;
  end;
end;

function TZMLister.ForEach(func: TZMForEachFunction; var data): Integer;
const
  __ERR_GE_InvalidArguments = __UNIT__ + (232 shl 10) + GE_InvalidArguments;
  __ERR_AZ_NothingToDo = __UNIT__ + (243 shl 10) + AZ_NothingToDo;
  __ERR_GE_NoSkipping = __UNIT__ + (260 shl 10) + GE_NoSkipping;
var
  BadSkip: boolean;
  CurZip: TZMZipFile;
  good: Integer;
  idx: Integer;
  rec: TZMDirEntry;
  SelCnt: Integer;
  SkippedFiles: TStringList;
begin
  Result := 0;
  good := 0;
  if IncludeSpecs.Count < 1 then
    raise EZipMaster.CreateMsgDisp(__ERR_GE_InvalidArguments, true);
  SkippedFiles := TStringList.Create;
  try
    if Verbosity >= zvVerbose then
      Diag('StartUp ForEach');
    CurZip := CurrentZip(true);
    SelCnt := CurZip.SelectFiles(IncludeSpecs, ExcludeSpecs, SkippedFiles);
    if SelCnt <= 0 then
    begin
      if Verbosity >= zvVerbose then
        Diag('nothing selected');
      ShowZipMessage(__ERR_AZ_NothingToDo, '');
      Result := -__ERR_AZ_NothingToDo;
      Exit;
    end;
    IncludeSpecs.Clear; // will contain files processed
    ExcludeSpecs.Clear; // will contain files skipped
    BadSkip := false;
    for idx := 0 to SkippedFiles.Count - 1 do
    begin
      if ReportSkippingEx(SkippedFiles[idx], stNotFound, 0, ExcludeSpecs) then
        BadSkip := true;
    end;
  finally
    SkippedFiles.Free;
  end;
  if BadSkip then
  begin
    ShowZipMessage(__ERR_GE_NoSkipping, '');
    Exit;
  end;
  CurZip.ShowProgress := zspFull;
  CurZip.ProgReport(zacCount, PR_Processing, '', SelCnt);
  CurZip.ProgReport(zacSize, PR_Processing, '', SelCnt);
  idx := -1;
  while true do
  begin
    idx := CurZip.NextSelected(idx);
    if idx < 0 then
      Break;
    rec := CurZip[idx];
    if Verbosity >= zvVerbose then
      Diag('Processing: ' + rec.filename);
    CurZip.ProgReport(zacProgress, PR_Processing, '', 1);
    Result := func(rec, data);
    if Result <> 0 then
    begin
      ExcludeSpecs.Add(rec.filename);
      Break;
    end;
    Inc(good);
    IncludeSpecs.Add(rec.filename);
    CurZip.ProgReport(zacItem, PR_Loading, '', 1);
    CheckCancel;
  end;
  CurZip.ProgReport(zacEndOfBatch, PR_Processing, '', 0);
  SuccessCnt := good;
  if Verbosity >= zvVerbose then
    Diag('finished ForEach');
end;

function TZMLister.GetCurrentIsValid: boolean;
begin
  Result := (ZipFileName <> '') and (Current <> nil) and
            ((Current.info and zfi_Loaded) <> 0);
end;

function TZMLister.GetZipComment: AnsiString;
begin
    Result := fInternal.fZipComment
end;

function TZMLister.GetZipFileName: TZMString;
begin
    Result := fInternal.fZipFileName
end;

function TZMLister.IsDetachedSFX(const filename: string): boolean;
var
  ext: string;
  fn: string;
  wz: TZMZipFile;
begin
  Result := false;
  fn := filename;
  if fn = '' then
    fn := ZipFileName;
  ext := ExtractFileExt(fn);
  if AnsiSameText(ext, '.exe') then
  begin
    wz := TZMZipFile.Create(Master, nil);
    try
      wz.filename := fn;
      if (wz.OpenEOC(true) >= 0) and IsDetachSFX(wz) then
        Result := true;
    finally
      wz.Free;
    end;
  end;
end;

// if is detached sfx - set stub excluding the detached header
function TZMLister.IsDetachSFX(zfile: TZMZipFile): boolean;
const
  MaxStubSize = 80000;
var
  cstt: Integer;
  ms: TMemoryStream;
begin
  Result := false;
  try
    zfile.stub := nil; // remove old
    ms := nil;
    if (zfile.IsOpen) and (zfile.DiskNr = 0) and (zfile.Sig = zfsDOS) then
    begin
      // check invalid values
      if (zfile.EOCOffset <= zfile.CentralSize) or
        (zfile.CentralSize < sizeof(TZipCentralHeader)) then
        Exit;
      cstt := zfile.EOCOffset - zfile.CentralSize;
      // must have SFX stub but we only check for biggest practical header
      if (cstt < MinStubSize) or (cstt > MaxStubSize) then
        Exit;
      if zfile.Seek(0, 0) <> 0 then
        Exit;
      ms := TMemoryStream.Create;
      try
        if zfile.ReadTo(ms, cstt + 4) = (cstt + 4) then
        begin
          Result := TrimDetached(ms);
        end;
      finally
        ms.Free;
      end;
    end;
  except
    Result := false;
    FreeAndNil(ms);
  end;
end;

procedure TZMLister.List;
begin
  LoadZip(ZipFileName, false);
end;

// table format - ident: byte, strng[]: byte, 0: byte; ...;0
function TZMLister.LoadSFXStr(ptbl: PByte; ident: Byte): string;
var
  Id: Byte;
begin
  Result := '';
  if (ptbl = nil) or (ident = 0) then
    Exit;
  Id := ptbl^;
  while (Id <> 0) and (Id <> ident) do
  begin
    while ptbl^ <> 0 do
      Inc(ptbl);
    Inc(ptbl);
    Id := ptbl^;
  end;
  if Id = ident then
  begin
    Inc(ptbl);
{$IFDEF UNICODE}
    Result := PUTF8ToStr(pAnsiChar(ptbl), -1);
{$ELSE}
    if UsingUTF8 then
      Result := UTF8String(pAnsiChar(ptbl))
    else
      Result := PUTF8ToStr(pAnsiChar(ptbl), -1);
{$ENDIF}
  end;
end;

procedure TZMLister.LoadZip(const ZipName: string; NoEvent: boolean);
const
  __ERR_DS_NoInFile = __UNIT__ + (431 shl 10) + DS_NoInFile;
var
  r: Integer;
  tmpDirUpdate: TNotifyEvent;
begin
  CentralDir.Current := nil; // close and remove any old file
  if ZipName <> '' then
  begin
    CentralDir.Current.filename := ZipName;
    r := CentralDir.Current.Open(false, false);
    if r >= 0 then
    begin
      CentralDir.Current.File_Close;
      ZipComment := CentralDir.ZipComment;
      Reload := zlrLoaded; // update Aux properties
    end
    else
    begin
      if ((-r) and MSG_ID_MASK) = DS_NoInFile then
      begin
        // just report no file - may be intentional
        ExtErrCode := __ERR_DS_NoInFile;
        ErrCode := DS_NoInFile;
        ErrMessage := ZipLoadStr(DS_NoInFile);
      end
      else
        ShowZipMsg(-r, true);
    end;
  end;
  if not NoEvent then
  begin
    tmpDirUpdate := Master.OnDirUpdate;
    if assigned(tmpDirUpdate) then
      tmpDirUpdate(Master);
  end;
end;

function TZMLister.MapOptionsFrom17(opts: Word): TZMSFXOpts;
begin
  Result := [];
  if (so_AskCmdLine_17 and opts) <> 0 then
    Result := Result + [soAskCmdLine];
  if (so_AskFiles_17 and opts) <> 0 then
    Result := Result + [soAskFiles];
  if (so_HideOverWriteBox_17 and opts) <> 0 then
    Result := Result + [soHideOverWriteBox];
  if (so_AutoRun_17 and opts) <> 0 then
    Result := Result + [soAutoRun];
  if (so_NoSuccessMsg_17 and opts) <> 0 then
    Result := Result + [soNoSuccessMsg];
  if (so_ExpandVariables_17 and opts) <> 0 then
    Result := Result + [soExpandVariables];
  if (so_InitiallyHideFiles_17 and opts) <> 0 then
    Result := Result + [soInitiallyHideFiles];
  if (so_ForceHideFiles_17 and opts) <> 0 then
    Result := Result + [soForceHideFiles];
  if (so_CheckAutoRunFileName_17 and opts) <> 0 then
    Result := Result + [soCheckAutoRunFileName];
  if (so_CanBeCancelled_17 and opts) <> 0 then
    Result := Result + [soCanBeCancelled];
  if (so_CreateEmptyDirs_17 and opts) <> 0 then
    Result := Result + [soCreateEmptyDirs];
end;

function TZMLister.MapOptionsFromStub(opts: Word): TZMSFXOpts;
begin
  Result := [];
  if (so_AskCmdLine and opts) <> 0 then
    Result := Result + [soAskCmdLine];
  if (so_AskFiles and opts) <> 0 then
    Result := Result + [soAskFiles];
  if (so_HideOverWriteBox and opts) <> 0 then
    Result := Result + [soHideOverWriteBox];
  if (so_AutoRun and opts) <> 0 then
    Result := Result + [soAutoRun];
  if (so_NoSuccessMsg and opts) <> 0 then
    Result := Result + [soNoSuccessMsg];
  if (so_ExpandVariables and opts) <> 0 then
    Result := Result + [soExpandVariables];
  if (so_InitiallyHideFiles and opts) <> 0 then
    Result := Result + [soInitiallyHideFiles];
  if (so_ForceHideFiles and opts) <> 0 then
    Result := Result + [soForceHideFiles];
  if (so_CheckAutoRunFileName and opts) <> 0 then
    Result := Result + [soCheckAutoRunFileName];
  if (so_CanBeCancelled and opts) <> 0 then
    Result := Result + [soCanBeCancelled];
  if (so_CreateEmptyDirs and opts) <> 0 then
    Result := Result + [soCreateEmptyDirs];
  if (so_SuccessAlways and opts) <> 0 then
    Result := Result + [soSuccessAlways];
end;

{
  function TZMLister.MapOptionsToStub(opts: TZMSFXOpts): Word;
  begin
  Result := 0;
  if soAskCmdLine in opts then
  Result := Result or so_AskCmdLine;
  if soAskFiles in opts then
  Result := Result or so_AskFiles;
  if soHideOverWriteBox in opts then
  Result := Result or so_HideOverWriteBox;
  if soAutoRun in opts then
  Result := Result or so_AutoRun;
  if soNoSuccessMsg in opts then
  Result := Result or so_NoSuccessMsg;
  if soExpandVariables in opts then
  Result := Result or so_ExpandVariables;
  if soInitiallyHideFiles in opts then
  Result := Result or so_InitiallyHideFiles;
  if soForceHideFiles in opts then
  Result := Result or so_ForceHideFiles;
  if soCheckAutoRunFileName in opts then
  Result := Result or so_CheckAutoRunFileName;
  if soCanBeCancelled in opts then
  Result := Result or so_CanBeCancelled;
  if soCreateEmptyDirs in opts then
  Result := Result or so_CreateEmptyDirs;
  if soSuccessAlways in opts then
  Result := Result or so_SuccessAlways;
  end;
}
function TZMLister.MapOverwriteModeFromStub(ovr: Word): TZMOvrOpts;
begin
  case ovr of
    som_Overwrite:
      Result := ovrAlways;
    som_Skip:
      Result := ovrNever;
  else
    Result := ovrConfirm;
  end;
end;

function TZMLister.MapSFXSettings(stub: TMemoryStream): Integer;
const
  __ERR_DS_SFXBadRead = __UNIT__ + (570 shl 10) + DS_SFXBadRead;
type
  T_header = packed record
    Sig: DWORD;
    Size: Word;
    x: Word;
  end;
  P_header = ^T_header;
var
  i: Integer;
  NumSections: Integer;
  p: PByte;
  phed: P_header;
  sz: Cardinal;
  tmp: Cardinal;
begin
  Result := 0;
  if (stub <> nil) and (stub.Size > MinStubSize) then
  begin
    sz := 0;
    p := stub.Memory;
    if (PImageDosHeader(p).e_magic <> IMAGE_DOS_SIGNATURE) then
      Exit;
    Result := -__ERR_DS_SFXBadRead; // 'unknown sfx'
    Inc(p, PImageDosHeader(p)._lfanew);
    if PCardinal(p)^ <> IMAGE_PE_SIGNATURE then
      Exit; // not exe
    Inc(p, sizeof(Cardinal));
    NumSections := PImageFileHeader(p).NumberOfSections;
    Inc(p, sizeof(TImageFileHeader) + sizeof(TImageOptionalHeader));
    for i := 1 to NumSections do
    begin
      tmp := PImageSectionHeader(p)^.PointerToRawData + PImageSectionHeader(p)
        ^.SizeOfRawData;
      if tmp > sz then
        sz := tmp;
      Inc(p, sizeof(TImageSectionHeader));
    end;
    // sz = end of stub
    p := stub.Memory;
    Inc(p, sz);
    phed := P_header(p);
    if phed.Sig = SFX_HEADER_SIG then
      Result := MapSFXSettings19(p, stub)
    else
      if phed.Sig = SFX_HEADER_SIG_17 then
        Result := MapSFXSettings17(p, stub);
  end;
end;

function TZMLister.MapSFXSettings17(pheder: PByte; stub: TMemoryStream)
  : Integer;
const
  __ERR_DS_SFXBadRead = __UNIT__ + (614 shl 10) + DS_SFXBadRead;
type
  T_header = packed record
    Sig: DWORD;
    Size: Word;
    x: Word;
  end;
  P_header = ^T_header;
var
  ico: TIcon;
  p: PByte;
  PSFXHeader: PSFXFileHeader_17;
  X_Caption, X_Path, X_CmdLine, X_RegFailPath, X_StartMsg: AnsiString;
begin
  Result := -__ERR_DS_SFXBadRead;
  PSFXHeader := PSFXFileHeader_17(pheder);
  p := pheder;
  Inc(p, sizeof(TSFXFileHeader_17)); // point to strings
  X_Caption := ReadSFXStr17(p, PSFXHeader^.CaptionSize);
  X_Path := ReadSFXStr17(p, PSFXHeader^.PathSize);
  X_CmdLine := ReadSFXStr17(p, PSFXHeader^.CmdLineSize);
  X_RegFailPath := ReadSFXStr17(p, PSFXHeader^.RegFailPathSize);
  X_StartMsg := ReadSFXStr17(p, PSFXHeader^.StartMsgSize);

  // read icon
  try
    ico := GetFirstIcon(stub);
    // should test valid
    SFXIcon := ico;
    ico.Free;
  except
    on E: EZMException do
    begin
      Result := -E.ResId;
      Exit;
    end
    else
      Exit;
  end;
  SFXOptions := MapOptionsFrom17(PSFXHeader^.Options);
  SFXOverwriteMode := MapOverwriteModeFromStub(PSFXHeader^.DefOVW);
  if (PSFXHeader^.StartMsgType and (MB_OKCANCEL or MB_YESNO)) <> 0 then
  begin
    if (PSFXHeader^.StartMsgType and MB_OKCANCEL) <> 0 then
      X_StartMsg := '1|' + X_StartMsg
    else
      if (PSFXHeader^.StartMsgType and MB_YESNO) <> 0 then
        X_StartMsg := '2|' + X_StartMsg;
  end;
  SFXMessage := string(X_StartMsg);
  SFXCaption := string(X_Caption);
  SFXDefaultDir := string(X_Path);
  SFXCommandLine := string(X_CmdLine);
  SFXRegFailPath := string(X_RegFailPath);
  Result := 0; // all is well
end;

function TZMLister.MapSFXSettings19(pheder: PByte; stub: TMemoryStream)
  : Integer;
const
  __ERR_DS_SFXBadRead = __UNIT__ + (671 shl 10) + DS_SFXBadRead;
var
  cmnds: PByte;
  cstream: TMemoryStream;
  ico: TIcon;
  msg: string;
  delta: Integer;
  p: PByte;
  phed: PSFXFileHeader;
  psdat: PSFXStringsData;
begin
  Result := -__ERR_DS_SFXBadRead;
  phed := PSFXFileHeader(pheder);
  cstream := nil;
  cmnds := PByte(@phed^.StartMsgType);
  Inc(cmnds, sizeof(Word));
  try
    // get command strings
    if (so_CompressedCmd and phed^.Options) <> 0 then
    begin
      // needs dll!!!!
      p := cmnds;
      cmnds := nil;
      psdat := PSFXStringsData(p);
      Inc(p, sizeof(TSFXStringsData)); // point to compressed data
      delta := Cardinal(p) - Cardinal(stub.Memory);
      if stub.Seek(delta, soFromBeginning) = delta then
      begin
        cstream := TMemoryStream.Create;
        if (UndeflateQ(cstream, stub, psdat.CSize, METHOD_DEFLATED, psdat.crc)
          = 0) and (cstream.Size = psdat.USize) then
          cmnds := cstream.Memory // ok
{$ifdef _DEBUG}
        else ShowZipMessage(__ERR_AZ_InternalError, " : Undeflate error");
{$ELSE}
        ;
{$endif}
      end;
    end;
    if cmnds <> nil then
    begin
      // read icon
      try
        ico := GetFirstIcon(stub);
        // should test valid
        SFXIcon := ico;
        ico.Free;
      except
        on E: EZMException do
        begin
          Result := -E.ResId;
          Exit;
        end
        else
          Exit;
      end;
      // we have strings
      SFXCaption := LoadSFXStr(cmnds, sc_Caption);
      SFXDefaultDir := LoadSFXStr(cmnds, sc_Path);
      SFXCommandLine := LoadSFXStr(cmnds, sc_CmdLine);
      SFXRegFailPath := LoadSFXStr(cmnds, sc_RegFailPath);
      msg := LoadSFXStr(cmnds, sc_StartMsg);
      SFXOptions := MapOptionsFromStub(phed^.Options);
      SFXOverwriteMode := MapOverwriteModeFromStub(phed^.DefOVW);
      if (phed^.StartMsgType and (MB_OKCANCEL or MB_YESNO)) <> 0 then
      begin
        if (phed^.StartMsgType and MB_OKCANCEL) <> 0 then
          msg := '1|' + msg
        else
          if (phed^.StartMsgType and MB_YESNO) <> 0 then
            msg := '2|' + msg;
      end;
      SFXMessage := msg;
      Result := 0; // all is well
    end;
  finally
    if cstream <> nil then
      cstream.Free;
  end;
end;

function TZMLister.ReadSFXStr17(var p: PByte; len: Byte): AnsiString;
var
  i: Integer;
begin
  Result := '';
  if len > 0 then
  begin
    SetLength(Result, len);
    for i := 1 to len do
    begin
      Result[i] := AnsiChar(p^);
      Inc(p);
    end;
  end;
end;

procedure TZMLister.SetCurrent(const Value: TZMZipFile);
begin
  CentralDir.Current := Value;
end;

procedure TZMLister.SetCurrentIsValid(const Value: boolean);
begin
  //
end;

procedure TZMLister.SetZipComment(const Value: AnsiString);
begin
    fInternal.fZipComment := Value;
end;

procedure TZMLister.Set_ZipFileName(const zname: string; Load: TZLoadOpts);
begin
  fInternal.fZipFileName := zname;
  if Load <> zloNoLoad then
    LoadZip(zname, Load = zloSilent); // automatically load the file
end;

procedure TZMLister.Started;
begin
  Reload := zlrNone;
end;

function TZMLister.TrimDetached(stub: TMemoryStream): Boolean;
type
  T_header = packed record
    Sig: DWORD;
    Size: Word;
    x: Word;
  end;
  P_header = ^T_header;
var
  i: Integer;
  NumSections: Integer;
  p: PByte;
  phed: P_header;
  sz: Cardinal;
begin
  Result := false;
  if (stub <> nil) and (stub.Size > MinStubSize) then
  begin
    sz := 0;
    p := stub.Memory;
    if (PImageDosHeader(p).e_magic <> IMAGE_DOS_SIGNATURE) then
      exit;
    Inc(p, PImageDosHeader(p)._lfanew);
    if PCardinal(p)^ <> IMAGE_PE_SIGNATURE then
      exit; // not exe
    Inc(p, sizeof(Cardinal));
    NumSections := PImageFileHeader(p).NumberOfSections;
    Inc(p, sizeof(TImageFileHeader) + sizeof(TImageOptionalHeader));
    for i := 1 to NumSections do
    begin
      with PImageSectionHeader(p)^ do
        if PointerToRawData + SizeOfRawData > sz then
          sz := PointerToRawData + SizeOfRawData;
      Inc(p, sizeof(TImageSectionHeader));
    end;
    // sz = end of stub
    p := stub.Memory;
    Inc(p, sz);
    phed := P_header(p);
    if phed.Sig <> SFX_HEADER_SIG then
      exit; // bad
    sz := sz + phed.Size;
    // posn := sz;
    Inc(p, phed.Size);
    phed := P_header(p);
    if (phed.Sig = CentralFileHeaderSig) then
    begin
      stub.Size := sz; // remove file header
      Result := true;
    end;
  end;
end;

// was GetAuxProperties
procedure TZMLister.UpdateAuxProperties;
var
  r: Integer;
  czip: TZMZipFile;
begin
  if not fInternal.fNoReadAux then
  begin
    czip := CentralDir.TheCurrent;
    // only if it exists
    if czip <> nil then
    begin
      if (czip.info and zfi_DidLoad) <> 0 then
      begin
        if czip.stub <> nil then
        begin
          // read Aux Settings from stub into component
          r := MapSFXSettings(czip.stub);
          if r <> 0 then
            Exit; // not easy to show warning
        end;
        if czip.MultiDisk then
        begin
          SpanOptions := czip.MapNumbering(SpanOptions);
          // set multi-disk
           WriteOptions := WriteOptions + [zwoDiskSpan];
        end
        else
          WriteOptions := WriteOptions - [zwoDiskSpan];
        czip.info := czip.info and (not zfi_DidLoad); // don't clear again
        AuxChanged := false;  // clear AuxChanged
      end;
    end;
  end;
end;

procedure TZMLister.UseDirOnlyEntriesChanged(Value: boolean);
begin
  CentralDir.UseDirOnlyEntries := Value;
end;

end.
