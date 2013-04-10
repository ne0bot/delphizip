unit ZMFileOpr;

//  ZMFileOpr.pas - SFX and Span operations
(*
  Derived from
  * SFX for DelZip v1.7
  * Copyright 2002-2005
  * written by Markus Stephany
*)

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
  ZMStructs, ZMCompat, ZMZipFile, ZMCore, ZMLister;

type
  TZMFileOpr = class(TZMCore)
  private
    Detached: Boolean;
    FLister: TZMLister;
    FSFXBinStream: TMemoryStream;
    OutSize: Integer;
    function BrowseResDir(ResStart, Dir: PIRD; Depth: Integer): PIRDatE;
    function CreateStubStream: Boolean;
    function GetKeepFreeOnDisk1: Cardinal;
    function GetMaxVolumeSize: Int64;
    function LoadFromBinFile(var stub: TStream; var Specified: Boolean)
      : Integer;
    function LoadFromResource(var stub: TStream; const sfxtyp: string): Integer;
    function PrepareStub: Integer;
    function SearchResDirEntry(ResStart: PIRD; entry: PIRDirE;
      Depth: Integer): PIRDatE;
    procedure SetKeepFreeOnDisk1(const Value: Cardinal);
    procedure SetMaxVolumeSize(const Value: Int64);
    function WriteMulti(Src: TZMZipFile; Dest: TZMZipCopy;
      UseXProgress: Boolean): Integer;
  protected
    function DetachedSize(zf: TZMZipFile): Integer;
    procedure Finished(WasGood: boolean); override;
    function MapOptionsToStub(opts: TZMSFXOpts): Word;
    function MapOverwriteModeToStub(mode: TZMOvrOpts): Word;
    function NewSFXFile(const ExeName: string): Integer;
    function NewSFXStub: TMemoryStream;
    function RecreateMVArchive(const TmpZipName: string;
      Recreate: Boolean): Boolean;
    function ReleaseSFXBin: TMemoryStream;
    procedure Started; override;
    function WriteDetached(zf: TZMZipFile): Integer;
    function WriteEOC(Current: TZMZipFile; OutFile: Integer): Integer;
    property KeepFreeOnDisk1: Cardinal read GetKeepFreeOnDisk1 write
        SetKeepFreeOnDisk1;
    property Lister: TZMLister read FLister;
    property MaxVolumeSize: Int64 read GetMaxVolumeSize write SetMaxVolumeSize;
  public
    constructor Create(AMaster: TCustomZipMaster);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function ConvertToSFX(const OutName: string; theZip: TZMZipFile): Integer;
    function ConvertToSpanSFX(const OutFileName: TZMString;
      theZip: TZMZipFile): Integer;
    function ConvertToZIP: Integer;
    function Deflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var crc: Cardinal): Integer; virtual;
    function ReadSpan(const InFileName: string; var OutFilePath: string;
      UseXProgress: Boolean): Integer;
    function WriteSpan(const InFileName, OutFileName: string;
      UseXProgress: Boolean): Integer;
  end;

implementation

uses
  Dialogs, ZMMsg, ZMDrv, ZMDelZip,
  ZMUtils, ZMXcpt, ZMMsgStr, ZMEOC, ZMWorkFile, ZMWZip,
  ZMIRec, ZMUTF8, ZMMatch, ShellAPI, ZMWFuncs, ZMCentral;

const
  __UNIT__ = 16 shl 23;

const
  SPKBACK001 = 'PKBACK#001';
  { File Extensions }
  ExtZip    = 'zip';
  DotExtZip = '.' + ExtZip;
  ExtExe    = 'exe';
  DotExtExe = '.' + ExtExe;
  ExtBin    = 'bin';
  ExtZSX    = 'zsx';
  { Identifiers }
  DzSfxID = 'DZSFX';

const
  MinStubSize = 12000;
  MaxStubSize = 80000;
  BufSize     = 10240;
  // 8192;   // Keep under 12K to avoid Winsock problems on Win95.
  // If chunks are too large, the Winsock stack can
  // lose bytes being sent or received.

function WriteCommand(Dest: TMemoryStream; const cmd: string; ident: Integer)
  : Integer; forward;

type
  TZMLoader = class(TZMZipFile)
  private
    fForZip: TZMZipFile;
    fname: string;
    procedure SetForZip(const Value: TZMZipFile);
  protected
    function AddStripped(const Rec: TZMIRec): Integer;
    function BeforeCommit: Integer; override;
    function PrepareDetached: Integer;
    function StripEntries: Integer;
  public
    procedure AfterConstruction; override;
    property ForZip: TZMZipFile read fForZip write SetForZip;
  end;

type
  TFileNameIs = (fiExe, fiZip, fiOther, fiEmpty);

const
  SFXBinDefault: string = 'ZMSFX19.bin';
  SFXBufSize: Word      = $2000;

// get the kind of filename
function GetFileNameKind(const sFile: TFileName): TFileNameIs;
var
  sExt: string;
begin
  if sFile = '' then
    Result := fiEmpty
  else
  begin
    sExt := LowerCase(ExtractFileExt(sFile));
    if sExt = DotExtZip then
      Result := fiZip
    else
      if sExt = DotExtExe then
        Result := fiExe
      else
        Result := fiOther;
  end;
end;

{ TZMFileOpr }
constructor TZMFileOpr.Create(AMaster: TCustomZipMaster);
begin
  inherited Create(AMaster, nil);
end;

procedure TZMFileOpr.AfterConstruction;
begin
  inherited;
  FSFXBinStream := nil;
  ActiveWorker := Self; // allow cleanup
  FLister := TZMLister(theLister);
end;

procedure TZMFileOpr.BeforeDestruction;
begin
  FreeAndNil(FSFXBinStream);
  inherited;
end;

function TZMFileOpr.BrowseResDir(ResStart, Dir: PIRD; Depth: Integer): PIRDatE;
var
  i: Integer;
  SingleRes: PIRDirE;
  x: PByte;
begin
  Result := nil;
  x := PByte(Dir);
  Inc(x, sizeof(IMAGE_RESOURCE_DIRECTORY));
  SingleRes := PIRDirE(x);

  for i := 1 to Dir.NumberOfNamedEntries + Dir.NumberOfIdEntries do
  begin
    Result := SearchResDirEntry(ResStart, SingleRes, Depth);
    if Result <> nil then
      Break; // Found the one w're looking for.
  end;
end;

function TZMFileOpr.ConvertToSFX(const OutName: string;
  theZip: TZMZipFile): Integer;
const
  __ERR_CF_CopyFailed = __UNIT__ + (268 shl 10) + CF_CopyFailed;
var
  nn: string;
  oz: TZMZipCopy;
  useTemp: Boolean;
begin
  Diag('ConvertToSFX');
  if theZip = nil then
    theZip := Lister.CurrentZip(true); // use Current
  Detached := false;
  Result := PrepareStub;
  if (Result < 0) or not assigned(FSFXBinStream) then
  begin
    // result:= some error;
    exit;
  end;
  if OutName = '' then
    nn := ChangeFileExt(theZip.FileName, DotExtExe)
  else
    nn := OutName;
  useTemp := _Z_FileExists(nn);
  oz := TZMZipCopy.Create(Master, nil);
  try
    if useTemp then
      oz.File_CreateTemp(ExtZSX, '')
    else
      oz.File_Create(nn);
    oz.stub := FSFXBinStream;
    FSFXBinStream := nil;
    oz.UseSFX := true;
    Result := oz.WriteFile(theZip, true);
    theZip.File_Close;
    if (Result >= 0) then
    begin
      if useTemp and not oz.File_Rename(nn, HowToDelete <> htdFinal) then
        raise EZipMaster.CreateMsg2Str(__ERR_CF_CopyFailed, oz.FileName, nn);
      Result := 0;
      Lister.Set_ZipFileName(nn, zloFull);
    end;
  finally
    oz.Free;
  end;
end;

function TZMFileOpr.ConvertToSpanSFX(const OutFileName: TZMString;
  theZip: TZMZipFile): Integer;
const
  __ERR_SF_DetachedHeaderTooBig = __UNIT__ + (350 shl 10) + SF_DetachedHeaderTooBig;
var
  DiskFile: string;
  DiskSerial: Cardinal;
  Dummy1: Cardinal;
  Dummy2: Cardinal;
  FileListSize: Cardinal;
  FreeOnDisk1: Cardinal;
  KeepFree: Cardinal;
  LDiskFree: Cardinal;
  MsgStr: string;
  OrgKeepFree: Cardinal;
  OutDrv: TZMWorkDrive;
  PartFileName: string;
  RightDiskInserted: Boolean;
  SFXName: string;
  SplitZip: TZMZipCopy;
  VolName: array [0 .. MAX_PATH - 1] of Char;
begin
  Detached := true;
  // prepare stub
  Result := PrepareStub;
  if (Result >= 0) and assigned(FSFXBinStream) then
  begin
    SplitZip := nil;
    if theZip = nil then
      theZip := Lister.Current; // use Current
    PartFileName := ChangeFileExt(OutFileName, DotExtZip);
    // delete the existing sfx stub
    if _Z_FileExists(OutFileName) then
      _Z_DeleteFile(OutFileName);
    SFXName := ExtractFileName(ChangeFileExt(OutFileName, DotExtZip));
    FileListSize := DetachedSize(theZip);
    OrgKeepFree := fInternal.fKeepFreeOnDisk1;
    OutDrv := TZMWorkDrive.Create;
    try
      // get output parameters
      OutDrv.DriveStr := OutFileName;
      OutDrv.HasMedia(true); // set media details

      // calulate the size of the sfx stub
      Result := 0; // is good (at least until it goes bad)

      if (not OutDrv.DriveIsFixed) and (MaxVolumeSize = 0) then
        MaxVolumeSize := OutDrv.VolumeSize;
      // first test if multiple parts are really needed
      if (MaxVolumeSize <= 0) or ((theZip.File_Size + FSFXBinStream.Size) <
        MaxVolumeSize) then
      begin
        Diag('Too small for span sfx');
        Detached := false;
        Result := ConvertToSFX(OutFileName, theZip);
      end
      else
      begin
        FileListSize := FileListSize + sizeof(Integer) +
          sizeof(TZipEndOfCentral);
        if KeepFreeOnDisk1 <= 0 then
          KeepFree := 0
        else
          KeepFree := KeepFreeOnDisk1;
        KeepFree := KeepFree + FileListSize;
        if OutDrv.VolumeSize > MAXINT then
          LDiskFree := MAXINT
        else
          LDiskFree := Cardinal(OutDrv.VolumeSize);
        { only one set of ' span' params }
        if (MaxVolumeSize > 0) and (MaxVolumeSize < LDiskFree) then
          LDiskFree := MaxVolumeSize;
        if (FileListSize > LDiskFree) then
          Result := -__ERR_SF_DetachedHeaderTooBig;

        if Result = 0 then // << moved
        begin
          if (KeepFree mod OutDrv.VolumeSecSize) <> 0 then
            FreeOnDisk1 := ((KeepFree div OutDrv.VolumeSecSize) + 1) *
              OutDrv.VolumeSecSize
          else
            FreeOnDisk1 := KeepFree;

          // let the spanslave of the Worker do the spanning <<< bad comment - remove
          KeepFreeOnDisk1 := FreeOnDisk1;
          SplitZip := TZMZipCopy.Create(Master, nil);
          SplitZip.FileName := PartFileName;
          Result := WriteMulti(theZip, SplitZip, true);
          // if all went well - rewrite the loader correctly
          if (Result >= 0) and not OutDrv.DriveIsFixed then
          begin
            // for removable disk we need to insert the first again
            RightDiskInserted := false;
            while not RightDiskInserted do
            begin // ask to insert the first disk
              MsgStr := ZipFmtLoadStr(DS_InsertAVolume, [1]) +
                ZipFmtLoadStr(DS_InDrive, [OutDrv.DriveStr]);

              MessageDlg(MsgStr, mtInformation, [mbOK], 0);
              // check if right disk is inserted
              if SplitZip.Numbering = znsVolume then
              begin
                GetVolumeInformation(PChar(@OutDrv.DriveStr), VolName, MAX_PATH,
                  @DiskSerial, Dummy1, Dummy2, nil, 0);
                if (StrComp(VolName, SPKBACK001) = 0) then
                  RightDiskInserted := true;
              end
              else
              begin
                DiskFile := Copy(PartFileName, 1, Length(PartFileName) -
                  Length(ExtractFileExt(PartFileName))) + '001.zip';
                if FileExists(DiskFile) then
                  RightDiskInserted := true;
              end;
            end;
          end;
          // write the loader
          if Result >= 0 then
            Result := WriteDetached(SplitZip);
        end;
      end;
    finally
      FreeAndNil(SplitZip);
      FreeAndNil(OutDrv);
      // restore original value
      KeepFreeOnDisk1 := OrgKeepFree;
    end;
  end;
  if Result < 0 then
    CleanupFiles(true);
end;

function TZMFileOpr.ConvertToZIP: Integer;
const
  __ERR_CF_CopyFailed = __UNIT__ + (433 shl 10) + CF_CopyFailed;
var
  cz: TZMZipFile;
  nn: string;
  oz: TZMZipCopy;
  useTemp: Boolean;
begin
  Diag('ConvertToZip');
  cz := Lister.CurrentZip(true);
  nn := ChangeFileExt(cz.FileName, DotExtZip);
  useTemp := _Z_FileExists(nn);
  oz := TZMZipCopy.Create(Master, nil);
  try
    if useTemp then
      oz.File_CreateTemp(ExtZSX, '')
    else
      oz.File_Create(nn);
    Result := oz.WriteFile(cz, true);
    cz.File_Close;
    if (Result >= 0) then
    begin
      if useTemp and not oz.File_Rename(nn, HowToDelete <> htdFinal) then
        raise EZipMaster.CreateMsg2Str(__ERR_CF_CopyFailed, oz.FileName, nn);
      Result := 0;
      Lister.Set_ZipFileName(nn, zloFull);
    end;
  finally
    oz.Free;
  end;
end;

function TZMFileOpr.CreateStubStream: Boolean;
const
  __ERR_DS_CopyError = __UNIT__ + (513 shl 10) + DS_CopyError;
  __ERR_SF_NoZipSFXBin = __UNIT__ + (527 shl 10) + SF_NoZipSFXBin;
const
  MinVers = 1900000;
var
  binname: string;
  BinStub: TStream;
  BinVers: Integer;
  err: Boolean;
  ResStub: TStream;
  ResVers: Integer;
  stub: TStream;
  stubname: string;
  UseBin: Boolean;
  XPath: string;
begin
  // what type of bin will be used
  stub := nil;
  ResStub := nil;
  BinStub := nil;
  BinVers := -1;
  FreeAndNil(FSFXBinStream); // dispose of existing (if any)
  try
    // load it either from resource (if bcsfx##.res has been linked to the executable)
    // or by loading from file in SFXPath and check both versions if available
    // ResVersion := '';
    stubname := DZRES_SFX;
    binname := SFXBinDefault;
    err := false; // resource stub not found
    XPath := SFXPath;
    if (Length(XPath) > 1) and (XPath[1] = '>') and
      (XPath[Length(XPath)] = '<') then
    begin
      // must use from resource
      stubname := Copy(XPath, 2, Length(XPath) - 2);
      if stubname = '' then
        stubname := DZRES_SFX;
      ResVers := LoadFromResource(ResStub, stubname);
      if ResVers < MinVers then
        err := true;
    end
    else
    begin
      // get from resource if it exists
      ResVers := LoadFromResource(ResStub, DZRES_SFX);
      // load if exists from file
      BinVers := LoadFromBinFile(BinStub, UseBin);
      if UseBin then
        ResVers := 0;
    end;
    if not err then
    begin
      // decide which will be used
      if (BinVers >= MinVers) and (BinVers >= ResVers) then
        stub := BinStub
      else
      begin
        if ResVers >= MinVers then
          stub := ResStub
        else
          err := true;
      end;
    end;
    if stub <> nil then
    begin
      FSFXBinStream := TMemoryStream.Create();
      try
        if FSFXBinStream.CopyFrom(stub, stub.Size - sizeof(Integer)) <>
          (stub.Size - sizeof(Integer)) then
          raise EZipMaster.CreateMsgDisp(__ERR_DS_CopyError, true);
        FSFXBinStream.Position := 0;
        if assigned(SFXIcon) then
          ReplaceIcon(FSFXBinStream, SFXIcon);
        FSFXBinStream.Position := 0;
      except
        FreeAndNil(FSFXBinStream);
      end;
    end;
  finally
    FreeAndNil(ResStub);
    FreeAndNil(BinStub);
  end;
  if err then
    raise EZipMaster.CreateMsgStr(__ERR_SF_NoZipSFXBin, stubname);
  Result := FSFXBinStream <> nil;
end;

function TZMFileOpr.Deflate(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var crc: Cardinal): Integer;
const
  __ERR_AZ_InternalError = __UNIT__ + (537 shl 10) + AZ_InternalError;
begin
  Diag('Deflate not available');
  Result := -__ERR_AZ_InternalError;
end;

function TZMFileOpr.DetachedSize(zf: TZMZipFile): Integer;
var
  Data: TZMRawBytes;
  Has64: Boolean;
  i: Integer;
  ix: Integer;
  rec: TZMIRec;
  sz: Integer;
begin
  Result := -1;
  ASSERT(assigned(zf), 'no input');
  // Diag('Write file');
  if not assigned(zf) then
    exit;
  if FSFXBinStream = nil then
  begin
    Result := PrepareStub;
    if Result < 0 then
      exit;
  end;
  Result := FSFXBinStream.Size;

  Has64 := false;
  // add approximate central directory size
  for i := 0 to zf.Count - 1 do
  begin
    rec := zf[i];
    Result := Result + sizeof(TZipCentralHeader) + rec.FileNameLength;
    if rec.ExtraFieldLength > 4 then
    begin
      ix := 0;
      sz := 0;
      Data := rec.ExtraField;
      if XData(Data, Zip64_data_tag, ix, sz) then
      begin
        Result := Result + sz;
        Has64 := true;
      end;
      if XData(Data, UPath_Data_Tag, ix, sz) then
        Result := Result + sz;
      if XData(Data, NTFS_data_tag, ix, sz) and (sz >= 36) then
        Result := Result + sz;
    end;
  end;
  Result := Result + sizeof(TZipEndOfCentral);
  if Has64 then
  begin
    // also has EOC64
    Inc(Result, sizeof(TZip64EOCLocator));
    Inc(Result, zf.Z64VSize);
  end;
end;

procedure TZMFileOpr.Finished(WasGood: boolean);
begin
  inherited;
end;

function TZMFileOpr.GetKeepFreeOnDisk1: Cardinal;
begin
    Result := fInternal.fKeepFreeOnDisk1
end;

function TZMFileOpr.GetMaxVolumeSize: Int64;
begin
    Result := fInternal.fMaxVolumeSize
end;

function TZMFileOpr.LoadFromBinFile(var stub: TStream;
  var Specified: Boolean): Integer;
var
  BinExists: Boolean;
  binpath: string;
  path: string;
  XPath: string;
begin
  Result := -1;
  Specified := false;
  XPath := SFXPath;
  path := XPath;
  // if no name specified use default
  if ExtractFileName(XPath) = '' then
    path := path + SFXBinDefault;
  binpath := path;
  if (Length(XPath) > 1) and
    ((XPath[1] = '.') or (ExtractFilePath(XPath) <> '')) then
  begin
    // use specified
    Specified := true;
    if XPath[1] = '.' then // relative to program
      binpath := PathConcat(ExtractFilePath(ParamStr(0)), path);
    BinExists := FileExists(binpath);
  end
  else
  begin
    // Try the application directory.
    binpath := DelimitPath(ExtractFilePath(ParamStr(0)), true) + path;
    BinExists := FileExists(binpath);
    if not BinExists then
    begin
      // Try the current directory.
      binpath := path;
      BinExists := FileExists(binpath);
    end;
  end;
  if BinExists then
  begin
    try
      stub := TFileStream.Create(binpath, fmOpenRead);
      if (stub.Size > MinStubSize) and (stub.Size < MaxStubSize) then
      begin
        stub.ReadBuffer(Result, sizeof(Integer));
      end;
      Diag('found stub: ' + XPath + ' ' + VersStr(Result));
    except
      Result := -5;
    end;
  end;
end;

function TZMFileOpr.LoadFromResource(var stub: TStream;
  const sfxtyp: string): Integer;
var
  rname: string;
begin
  Result := -2;
  rname := sfxtyp;
  stub := OpenResStream(rname, RT_RCDATA);
  if (stub <> nil) and (stub.Size > MinStubSize) and
    (stub.Size < MaxStubSize) then
  begin
    stub.ReadBuffer(Result, sizeof(Integer));
    Diag('resource stub: ' + VersStr(Result));
  end;
end;

function TZMFileOpr.MapOptionsToStub(opts: TZMSFXOpts): Word;
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

function TZMFileOpr.MapOverwriteModeToStub(mode: TZMOvrOpts): Word;
begin
  case mode of
    ovrAlways:
      Result := som_Overwrite;
    ovrNever:
      Result := som_Skip;
  else
    Result := som_Ask;
  end;
end;

function TZMFileOpr.NewSFXFile(const ExeName: string): Integer;
const
  __ERR_DS_FileError = __UNIT__ + (731 shl 10) + DS_FileError;
  __ERR_DS_WriteError = __UNIT__ + (746 shl 10) + DS_WriteError;
var
  eoc: TZipEndOfCentral;
  fs: TFileStream;
begin
  Diag('Write empty SFX');
  fs := nil;
  Result := PrepareStub;
  if Result <> 0 then
    exit;
  try
    Result := -__ERR_DS_FileError;
    eoc.HeaderSig := EndCentralDirSig;
    eoc.ThisDiskNo := 0;
    eoc.CentralDiskNo := 0;
    eoc.CentralEntries := 0;
    eoc.TotalEntries := 0;
    eoc.CentralSize := 0;
    eoc.CentralOffset := 0;
    eoc.ZipCommentLen := 0;
    FSFXBinStream.WriteBuffer(eoc, sizeof(eoc));
    Result := 0;
    FSFXBinStream.Position := 0;
    fs := TFileStream.Create(ExeName, fmCreate);
    Result := fs.CopyFrom(FSFXBinStream, FSFXBinStream.Size);
    if Result <> FSFXBinStream.Size then
      Result := -__ERR_DS_WriteError
    else
      Result := 0;
    Diag('finished write empty SFX');
  finally
    FreeAndNil(fs);
    FreeAndNil(FSFXBinStream);
  end;
end;

function TZMFileOpr.NewSFXStub: TMemoryStream;
begin
  Result := nil;
  if PrepareStub = 0 then
    Result := ReleaseSFXBin;
end;

function TZMFileOpr.PrepareStub: Integer;
const
  __ERR_GE_Unknown = __UNIT__ + (783 shl 10) + GE_Unknown;
  __ERR_SF_StringTooLong = __UNIT__ + (825 shl 10) + SF_StringTooLong;
  __ERR_AZ_InternalError = __UNIT__ + (869 shl 10) + AZ_InternalError;
  __ERR_GE_Unknown1 = __UNIT__ + (886 shl 10) + GE_Unknown;
var
  cdata: TSFXStringsData;
  dflt: TZMDeflates;
  ds: TMemoryStream;
  err: Integer;
  i: Integer;
  l: Integer;
  ms: TMemoryStream;
  SFXBlkSize: Integer;
  SFXHead: TSFXFileHeader;
  SFXMsg: string;
  SFXMsgFlags: Word;
  Want: Integer;
begin
  Result := -__ERR_GE_Unknown;
  if not CreateStubStream then
    exit;
  SFXMsg := SFXMessage;
  SFXMsgFlags := MB_OK;
  If (Length(SFXMsg) >= 1) Then
  Begin
    Want := 1; // want the lot
    If (Length(SFXMsg) > 1) And (SFXMsg[2] = '|') Then
    Begin
      Case SFXMsg[1] Of
        '1':
          SFXMsgFlags := MB_OKCANCEL Or MB_ICONINFORMATION;
        '2':
          SFXMsgFlags := MB_YESNO Or MB_ICONQUESTION;
        '|':
          Want := 2;
      End;
      If SFXMsgFlags <> MB_OK Then
        Want := 3;
    End;
    If Want > 1 Then
      SFXMsg := Copy(SFXMsg, Want, 2048);
  End;
  try
    // create header
    SFXHead.Signature := SFX_HEADER_SIG;
    SFXHead.Options := MapOptionsToStub(SFXOptions);
    SFXHead.DefOVW := MapOverwriteModeToStub(SFXOverwriteMode);
    SFXHead.StartMsgType := SFXMsgFlags;
    ds := nil;
    ms := TMemoryStream.Create;
    try
      WriteCommand(ms, SFXCaption, sc_Caption);
      WriteCommand(ms, SFXCommandLine, sc_CmdLine);
      WriteCommand(ms, SFXDefaultDir, sc_Path);
      WriteCommand(ms, SFXMsg, sc_StartMsg);
      WriteCommand(ms, SFXRegFailPath, sc_RegFailPath);
      l := 0;
      ms.WriteBuffer(l, 1);
      // check string lengths
      if ms.Size > 4000 then
        raise EZipMaster.CreateMsgDisp(__ERR_SF_StringTooLong, true);

      if ms.Size > 100 then
      begin
        cdata.USize := ms.Size;
        ms.Position := 0;
        ds := TMemoryStream.Create;
        dflt := ZMDeflate;
        err := Deflate(ds, ms, ms.Size, dflt, cdata.crc);
        if err = 0 then
        begin
          cdata.CSize := ds.Size;
          if (dflt = ZMDeflate) and
            (ms.Size > (cdata.CSize + sizeof(cdata))) then
          begin
            // use compressed
            ms.Size := 0;
            ds.Position := 0;
            ms.WriteBuffer(cdata, sizeof(cdata));
            ms.CopyFrom(ds, ds.Size);
            SFXHead.Options := SFXHead.Options or so_CompressedCmd;
          end;
        end;
      end;
      // DWord Alignment.
      i := ms.Size and 3;
      if i <> 0 then
        ms.WriteBuffer(l, 4 - i); // dword align
      SFXBlkSize := sizeof(TSFXFileHeader) + ms.Size;
      // // create header
      SFXHead.Size := Word(SFXBlkSize);

      FSFXBinStream.Seek(0, soFromEnd);
      FSFXBinStream.WriteBuffer(SFXHead, sizeof(SFXHead));
      l := SFXBlkSize - sizeof(SFXHead);
      i := ms.Size;
      if i > 0 then
      begin
        ms.Position := 0;
        FSFXBinStream.CopyFrom(ms, i);
        Dec(l, i);
      end;
      // check DWORD align
      if l <> 0 then
        raise EZipMaster.CreateMsgDisp(__ERR_AZ_InternalError, true);

      Result := 0;
    finally
      ms.Free;
      ds.Free;
    end;
  except
    on E: EZipMaster do
    begin
      FreeAndNil(FSFXBinStream);
      ShowExceptionError(E);
      Result := -E.ResId;
    end
    else
    begin
      FreeAndNil(FSFXBinStream);
      Result := -__ERR_GE_Unknown1;
    end;
  end;
end;

function TZMFileOpr.ReadSpan(const InFileName: string; var OutFilePath: string;
  UseXProgress: Boolean): Integer;
const
  __ERR_DS_NoTempFile = __UNIT__ + (913 shl 10) + DS_NoTempFile;
  __ERR_DS_NoOutFile = __UNIT__ + (931 shl 10) + DS_NoOutFile;
  __ERR_DS_ErrorUnknown = __UNIT__ + (957 shl 10) + DS_ErrorUnknown;
var
  fd: TZMZipCopy;
  fs: TZMZipFile;
begin
  ShowProgress := zspNone;
  fd := nil;
  fs := nil;
  Result := 0;

  try
    try
      // If we don't have a filename we make one first.
      if ExtractFileName(OutFilePath) = '' then
      begin
        OutFilePath := Master.MakeTempFileName('', '');
        if OutFilePath = '' then
          Result := -__ERR_DS_NoTempFile;
      end
      else
      begin
        _Z_EraseFile(OutFilePath, HowToDelete = htdFinal);
        OutFilePath := ChangeFileExt(OutFilePath, EXT_ZIP);
      end;

      if Result = 0 then
      begin
        fs := TZMZipFile.Create(Master, nil);
        // Try to get the last disk from the user if part of Volume numbered set
        fs.FileName := InFileName;
        Result := fs.Open(false, false);
      end;
      if Result >= 0 then
      begin
        // InFileName opened successfully
        Result := -__ERR_DS_NoOutFile;
        fd := TZMZipCopy.Create(Master, nil);
        if fd.File_Create(OutFilePath) then
        begin
          if UseXProgress then
            fd.ShowProgress := zspExtra
          else
            fd.ShowProgress := zspFull;
          if UseXProgress then
            fd.EncodeAs := zeoUTF8;
          // preserve file names for internal operations
          Result := fd.WriteFile(fs, true);
        end;
      end;
      if Result < 0 then
        ShowZipMessage(-Result, '');
    except
      on ers: EZipMaster do
      begin
        // All ReadSpan specific errors.
        ShowExceptionError(ers);
        Result := -ers.ResId;
      end;
      on E: Exception do
      begin
        // The remaining errors, should not occur.
        Result := -__ERR_DS_ErrorUnknown;
        ShowZipMessage(Result, E.Message);
      end;
    end;
  finally
    FreeAndNil(fs);
    if (fd <> nil) and (fd.IsOpen) then
    begin
      fd.File_Close;
      if Result <> 0 then
      begin
        // An error somewhere, OutFile is not reliable.
        _Z_DeleteFile(OutFilePath);
        OutFilePath := '';
      end;
    end;
    FreeAndNil(fd);
  end;
end;

// recreate main file (ZipFileName) from temporary file (TmpZipName)
function TZMFileOpr.RecreateMVArchive(const TmpZipName: string;
  Recreate: Boolean): Boolean;
const
  __ERR_DS_NoOutFile = __UNIT__ + (1024 shl 10) + DS_NoOutFile;
var
  OutPath: string;
  r: Integer;
  tmp: string;
  tzip: TZMZipFile;
begin
  Result := false;
  try
    tzip := TZMZipFile.Create(Master, nil);

    tzip.FileName := Lister.Current.FileName;
    tzip.DiskNr := -1;
    tzip.IsMultiPart := true;
    if Recreate then
    begin
      try
        tzip.GetNewDisk(0, true); // ask to enter the first disk again
        tzip.File_Close;
      except
        on E: Exception do
        begin
          _Z_DeleteFile(TmpZipName); // delete the temp file
          raise; // throw last exception again
        end;
      end;
    end;

    if AnsiSameText('.exe', ExtractFileExt(Lister.ZipFileName)) then
    begin // make 'detached' SFX
      OutPath := Lister.ZipFileName; // remember it
      Lister.Set_ZipFileName(TmpZipName, zloFull); // reload
      // create an header first to now its size
      tmp := ExtractFileName(OutPath);
      r := ConvertToSpanSFX(OutPath, Lister.Current);
      if r >= 0 then
      begin
        _Z_DeleteFile(TmpZipName);
        Lister.Set_ZipFileName(OutPath, zloNoLoad); // restore it
      end
      else
      begin
        SuccessCnt := 0; // failed
        ShowZipMessage(__ERR_DS_NoOutFile, 'Error ' + IntToStr(r));
      end;
    end { if SameText(...) }
    else
    begin
      if Recreate then
        // reproduce orig numbering
        SpanOptions := Lister.Current.MapNumbering(SpanOptions);
      if WriteSpan(TmpZipName, Lister.ZipFileName, true) <{>} 0 then
        SuccessCnt := 0;
      _Z_DeleteFile(TmpZipName);
    end;
  finally
    FreeAndNil(tzip);
  end;
end;

function TZMFileOpr.ReleaseSFXBin: TMemoryStream;
begin
  Result := FSFXBinStream;
  FSFXBinStream := nil;
end;

function TZMFileOpr.SearchResDirEntry(ResStart: PIRD; entry: PIRDirE;
  Depth: Integer): PIRDatE;
var
  x: PByte;
begin
  Result := nil;
  if entry.un1.NameIsString <> 0 then
    exit; // No named resources.
  if (Depth = 0) and (entry.un1.Id <> 3) then
    exit; // Only icon resources.
  if (Depth = 1) and (entry.un1.Id <> 1) then
    exit; // Only icon with ID 0x1.
  if entry.un2.DataIsDirectory = 0 then
  begin
    x := PByte(ResStart);
    Inc(x, entry.un2.OffsetToData);
    Result := PIRDatE(x);
  end
  else
  begin
    x := PByte(ResStart);
    Inc(x, entry.un2.OffsetToDirectory);
    Result := BrowseResDir(ResStart, PIRD(x), Depth + 1);
  end;
end;

procedure TZMFileOpr.SetKeepFreeOnDisk1(const Value: Cardinal);
begin
    fInternal.fKeepFreeOnDisk1 := Value;
end;

procedure TZMFileOpr.SetMaxVolumeSize(const Value: Int64);
begin
    fInternal.fMaxVolumeSize := Value;
end;

Procedure TZMFileOpr.Started;
Begin
  Inherited;
End;

function TZMFileOpr.WriteDetached(zf: TZMZipFile): Integer;
const
  __ERR_DS_FileError = __UNIT__ + (1095 shl 10) + DS_FileError;
var
  xf: TZMLoader;
begin
  Diag('Write detached SFX stub');
  Result := -__ERR_DS_FileError;
  xf := TZMLoader.Create(Master, nil);
  try
    xf.ForZip := zf;
    if xf.File_Create(ChangeFileExt(zf.FileName, DotExtExe)) then
      Result := xf.Commit(false);
  finally
    xf.Free;
  end;
end;

function TZMFileOpr.WriteEOC(Current: TZMZipFile; OutFile: Integer): Integer;
var
  r: Integer;
begin
  Current.handle := OutFile;
  Current.Position := FileSeek(OutFile, 0, soFromCurrent);
  r := Current.WriteEOC();
  OutSize := FileSeek(OutFile, 0, soFromEnd);
  Current.handle := -1; // closes OutFile
  Result := r;
end;

function TZMFileOpr.WriteMulti(Src: TZMZipFile; Dest: TZMZipCopy;
  UseXProgress: Boolean): Integer;
const
  __ERR_DS_NoInFile = __UNIT__ + (1127 shl 10) + DS_NoInFile;
  __ERR_DS_NoOutFile = __UNIT__ + (1129 shl 10) + DS_NoOutFile;
  __ERR_DS_ErrorUnknown = __UNIT__ + (1155 shl 10) + DS_ErrorUnknown;
begin
  try
    if ExtractFileName(Src.FileName) = '' then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_NoInFile, true);
    if ExtractFileName(Dest.FileName) = '' then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_NoOutFile, true);
    Result := Src.Open(false, false);
    if Result < 0 then
      raise EZipMaster.CreateMsgDisp(-Result, true);
    Dest.StampDate := Src.StampDate;
    if UseXProgress then
      Dest.ShowProgress := zspExtra
    else
      Dest.ShowProgress := zspFull;
    Dest.TotalDisks := 0;
    Dest.PrepareWrite(zwMultiple);
    Dest.File_Size := Src.File_Size; // to calc TotalDisks
    Result := Dest.WriteFile(Src, true);
    Dest.File_Close;
    Src.File_Close;
    if Result < 0 then
      raise EZipMaster.CreateMsgDisp(-Result, true);
  except
    on ews: EZipMaster do // All WriteSpan specific errors.
    begin
      ShowExceptionError(ews);
      Result := -ews.ResId;
    end;
    on E: Exception do
    begin
      // The remaining errors, should not occur.
      Result := -__ERR_DS_ErrorUnknown;
      ShowZipMessage(Result, E.Message);
    end;
  end;
end;

function TZMFileOpr.WriteSpan(const InFileName, OutFileName: string;
  UseXProgress: Boolean): Integer;
const
  __ERR_DS_NoUnattSpan = __UNIT__ + (1177 shl 10) + DS_NoUnattSpan;
var
  fd: TZMZipCopy;
  fs: TZMZipFile;
begin
  Result := -1;
  fd := nil;
  fs := TZMZipFile.Create(Master, nil);
  try
    fs.FileName := InFileName;
    fd := TZMZipCopy.Create(Master, nil);
    fd.FileName := OutFileName;
    if Unattended and not fd.WorkDrive.DriveIsFixed then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_NoUnattSpan, true);
    Result := WriteMulti(fs, fd, UseXProgress);
  finally
    fs.Free;
    if fd <> nil then
      fd.Free;
  end;
end;

function ReadSFXStr17(var p: PByte; len: Byte): AnsiString;
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

function WriteCommand(Dest: TMemoryStream; const cmd: string;
  ident: Integer): Integer;
var
  ucmd: UTF8String;
  z: Byte;
begin
  Result := 0;
  if Length(cmd) > 0 then
  begin
    ucmd := AsUTF8Str(cmd);
    Dest.Write(ident, 1);
    Result := Dest.Write(PAnsiChar(ucmd)^, Length(ucmd)) + 2;
    z := 0;
    Dest.Write(z, 1);
  end;
end;

function TZMLoader.AddStripped(const Rec: TZMIRec): Integer;
const
  __ERR_AZ_InternalError = __UNIT__ + (1292 shl 10) + AZ_InternalError;
var
  Data: TZMRawBytes;
  idx: Integer;
  ixN: Integer;
  ixU: Integer;
  ixZ: Integer;
  NData: TZMRawBytes;
  ni: TZMRawBytes;
  NRec: TZMZRec;
  siz: Integer;
  szN: Integer;
  szU: Integer;
  szZ: Integer;
begin
  ixZ := 0;
  szZ := 0;
  ixU := 0;
  szU := 0;
  ixN := 0;
  szN := 0;
  NRec := TZMZRec.Create(self);
  NRec.VersionMadeBy := Rec.VersionMadeBy;
  NRec.VersionNeeded := Rec.VersionNeeded;
  NRec.Flag := Rec.Flag;
  NRec.ComprMethod := Rec.ComprMethod;
  NRec.ModifDateTime := Rec.ModifDateTime;
  NRec.CRC32 := Rec.CRC32;
  NRec.CompressedSize := Rec.CompressedSize;
  NRec.UncompressedSize := Rec.UncompressedSize;
  NRec.FileCommentLen := 0;
  NRec.DiskStart := Rec.DiskStart;
  NRec.IntFileAttrib := Rec.IntFileAttrib;
  NRec.ExtFileAttrib := Rec.ExtFileAttrib;
  NRec.RelOffLocal := Rec.RelOffLocal;
  NRec.StatusBits := Rec.StatusBits;
  NData := '';
  siz := 0;
  ni := Rec.HeaderName;
  if Rec.ExtraFieldLength > 4 then
  begin
    Data := Rec.ExtraField;
    if XData(Data, Zip64_data_tag, ixZ, szZ) then
      siz := siz + szZ;
    if XData(Data, UPath_Data_Tag, ixU, szU) then
      siz := siz + szU;
    if XData(Data, NTFS_data_tag, ixN, szN) and (szN >= 36) then
      siz := siz + szN;
  end;
  NRec.HeaderName := ni;
  NRec.FileNameLength := Length(ni);
  if siz > 0 then
  begin
    // copy required extra data fields
    SetLength(NData, siz);
    idx := 1;
    if szZ > 0 then
      move(Data[ixZ], NData[idx], szZ);
    Inc(idx, szZ);
    if szU > 0 then
      move(Data[ixU], NData[idx], szU);
    Inc(idx, szU);
    if szN >= 36 then
      move(Data[ixN], NData[idx], szN);
    NRec.ExtraField := NData;
    NData := '';
  end;
  Result := Add(NRec);
  if Result < 0 then
  begin
    NRec.Free; // could not add it
    Result := -__ERR_AZ_InternalError;
  end;
end;

procedure TZMLoader.AfterConstruction;
begin
  inherited;
  ForZip := nil;
  fname := '';
  DiskNr := MAX_WORD - 1;
end;

function TZMLoader.BeforeCommit: Integer;
const
  __ERR_AZ_NothingToDo = __UNIT__ + (1313 shl 10) + AZ_NothingToDo;
begin
  Result := inherited BeforeCommit;
  // Prepare detached header
  if Result = 0 then
  begin
    if Count < 0 then
      raise EZipMaster.CreateMsgDisp(__ERR_AZ_NothingToDo, true);
    StampDate := ForZip.StampDate;
    Result := PrepareDetached;
  end;
end;

function TZMLoader.PrepareDetached: Integer;
var
  SFXWorker: TZMFileOpr;
begin
  if not assigned(stub) then
  begin
    SFXWorker := ActiveWorker as TZMFileOpr;
    Result := SFXWorker.PrepareStub;
    if Result < 0 then
      exit; // something went wrong
    stub := SFXWorker.ReleaseSFXBin; // we now own it
  end;
  UseSFX := true;
  Result := 0;
end;

procedure TZMLoader.SetForZip(const Value: TZMZipFile);
begin
  if ForZip <> Value then
  begin
    fForZip := Value;
    ClearEntries;
    StripEntries;
    DiskNr := ForZip.DiskNr + 1;
  end;
end;

function TZMLoader.StripEntries: Integer;
const
  __ERR_AZ_NothingToDo = __UNIT__ + (1352 shl 10) + AZ_NothingToDo;
var
  i: Integer;
begin
  Result := -__ERR_AZ_NothingToDo;
  // fill list from ForFile
  for i := 0 to ForZip.Count - 1 do
  begin
    Result := AddStripped(ForZip[i]);
    if Result < 0 then
      Break;
  end;
end;

end.
