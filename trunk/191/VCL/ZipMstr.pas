unit ZipMstr;

//  ZipMstr.pas - main component

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
//modified 2013-03-05
{$I   '.\ZipVers.inc'}
{$I   '.\ZMConfig191.inc'}

interface

uses
  Classes, Forms, SysUtils, Graphics, Dialogs, Windows, Controls,
  ZMXcpt, ZMStructs;

const
  ZIPMASTERBUILD: string =  '1.9.1.0016';
  ZIPMASTERDATE: string  =  '30/03/2013';
  ZIPMASTERPRIV: Integer = 1910016;
{$IFDEF WIN64}
  DELZIPVERSION          = 191;
  MIN_DLL_BUILD          = 1910111;
{$ELSE}
  DELZIPVERSION          = 190;
  MIN_DLL_BUILD          = 1900111;
{$ENDIF}

const
  ZMPWLEN = 80;

type
{$IFDEF UNICODE}
  TZMString = string; // unicode
  TZMWideString = string;
  TZMRawBytes = RawByteString;
{$ELSE}
{$IFNDEF VERD6up}
  UTF8String = type string;
{$ENDIF}
  TZMString = AnsiString; // Ansi/UTF8 depending upon UseUTF8
  TZMWideString = WideString;
  TZMRawBytes = AnsiString;
{$ENDIF}
{$IFDEF UNICODE}
{$DEFINE _NO_UTF8_}
{$ELSE}
{$IFDEF SUPPORT_OLD_WINDOWS}
{$DEFINE _NO_UTF8_}
{$ENDIF}
{$ENDIF}
               
//procedure register;

type
  TZMStates = (zsDisabled, zsIdle, zsReentered, zsBusy, zsReentry);

  // options when editing a zip
  TZMAddOptsEnum = (AddDirNames, AddRecurseDirs, AddMove, AddFreshen, AddUpdate,
    AddHiddenFiles, AddArchiveOnly, AddResetArchive, AddEncrypt, AddEmptyDirs,
    AddVolume, AddFromDate, AddVersion, AddNTFS);
  TZMAddOpts = set of TZMAddOptsEnum;

  // the EncodeAs values (writing) -
  // zeoUPATH - convert to Ansi but have UTF8 proper name in data
  // zeoUTF  - convert to UTF8
  // zeoOEM  - convert to OEM
  // zeoNone - store 'as is' (Ansi on Windows)
  // 'default' (zeoAuto) - [in order of preference]
  // is Ansi - use zeoNone
  // can be converted to Ansi - use zeoUPath (unless comment also extended)
  // use zeoUTF8

  // Encoded (reading)
  // zeoUPATH- use UPATH if available
  // zeoUTF  - assume name is UTF8 - convert to Ansi/Unicode
  // zeoOEM  - assume name is OEM - convert to Ansi/Unicode
  // zeoNone - assume name is Ansi - convert to Ansi/Unicode
  // zeoAuto - unless flags/versions say otherwise, or it has UTF8 name in data,
  // treat it as OEM (FAT) / Ansi (NTFS)
  TZMEncodingOpts = (zeoAuto, zeoNone, zeoOEM, zeoUTF8, zeoUPath);

  // When changing this enum also change the pointer array in the function AddSuffix,
  // and the initialisation of ZipMaster.
  TZMAddStoreSuffixEnum = (assGIF, assPNG, assZ, assZIP, assZOO, assARC, assLZH,
    assARJ, assTAZ, assTGZ, assLHA, assRAR, assACE, assCAB, assGZ, assGZIP,
    assJAR, assEXE, assEXT, assJPG, assJPEG, ass7Zp, assMP3, assWMV, assWMA,
    assDVR, assAVI);

  TZMAddStoreExts = set of TZMAddStoreSuffixEnum;

  TZMNameFixes = (znfFatal, znfSkip, znfAuto, znfFixed);

  TZMSpanOptsEnum = (spNoVolumeName, spCompatName, spWipeFiles, spTryFormat,
    spAnyTime, spExactName);
  TZMSpanOpts = set of TZMSpanOptsEnum;

  // options for when reading a zip file
  TZMExtrOptsEnum = (ExtrDirNames, ExtrOverWrite, ExtrFreshen, ExtrUpdate,
    ExtrTest, ExtrForceDirs, ExtrNTFS);
  TZMExtrOpts = set of TZMExtrOptsEnum;

  // options for when writing a zip file
  TZMWriteOptsEnum = (zwoDiskSpan, zwoZipTime, zwoForceDest, zwoSafe);
  TZMWriteOpts = set of TZMWriteOptsEnum;

  // other options
  TZMMergeOpts = (zmoConfirm, zmoAlways, zmoNewer, zmoOlder, zmoNever, zmoRename);
  TZMOvrOpts = (ovrAlways, ovrNever, ovrConfirm);

  TZMReplaceOpts = (rplConfirm, rplAlways, rplNewer, rplNever);

  TZMDeleteOpts = (htdFinal, htdAllowUndo);

  TZMRenameOpts = (htrDefault, htrOnce, htrFull);

  TZMSkipTypes = (stOnFreshen, stNoOverwrite, stFileExists, stBadPassword,
    stBadName, stCompressionUnknown, stUnknownZipHost, stZipFileFormatWrong,
    stGeneralExtractError, stUser, stCannotDo, stNotFound,
    stNoShare, stNoAccess, stNoOpen, stDupName, stReadError, stSizeChange,
    stNothingToDo);
  TZMSkipAborts = set of TZMSkipTypes;

  TZMZipDiskStatusEnum = (zdsEmpty, zdsHasFiles, zdsPreviousDisk,
    zdsSameFileName, zdsNotEnoughSpace);
  TZMZipDiskStatus = set of TZMZipDiskStatusEnum;
  TZMDiskAction = (zdaYesToAll, zdaOk, zdaErase, zdaReject, zdaCancel);

  TZMDeflates = (zmStore, zmStoreEncrypt, zmDeflate, zmDeflateEncrypt);
  TZMVerbosity = (zvOff, zvVerbose, zvTrace, zvNoisy);

type
  TZMSFXOpt = (soAskCmdLine,
    // allow user to prevent execution of the command line
    soAskFiles, // allow user to prevent certain files from extraction
    soHideOverWriteBox, // do not allow user to choose the overwrite mode
    soAutoRun, // start extraction + evtl. command line automatically
    // only if sfx filename starts with "!" or is "setup.exe"
    soNoSuccessMsg, // don't show success message after extraction
    soExpandVariables, // expand environment variables in path/cmd line...
    soInitiallyHideFiles, // dont show file listview on startup
    soForceHideFiles, // do not allow user to show files list
    // (no effect if shfInitiallyShowFiles is set)
    soCheckAutoRunFileName, // can only autorun if !... or setup.exe
    soCanBeCancelled, // extraction can be cancelled
    soCreateEmptyDirs, // recreate empty directories
    soSuccessAlways
    // always give success message even if soAutoRun or soNoSuccessMsg
    );

  // set of TSFXOption
  TZMSFXOpts = set of TZMSFXOpt;

type
  TZMProgressType = (NewFile, ProgressUpdate, EndOfItem, EndOfBatch,
    TotalFiles2Process, TotalSize2Process, NewExtra, ExtraUpdate);

type
  TZMProgressDetails = class(TObject)
  protected
    function GetBytesWritten: Int64; virtual; abstract;
    function GetDelta: Int64; virtual; abstract;
    function GetItemName: TZMString; virtual; abstract;
    function GetItemNumber: Integer; virtual; abstract;
    function GetItemPerCent: Integer;
    function GetItemPosition: Int64; virtual; abstract;
    function GetItemSize: Int64; virtual; abstract;
    function GetOrder: TZMProgressType; virtual; abstract;
    function GetStop: Boolean; virtual; abstract;
    function GetTotalCount: Int64; virtual; abstract;
    function GetTotalPerCent: Integer;
    function GetTotalPosition: Int64; virtual; abstract;
    function GetTotalSize: Int64; virtual; abstract;
    procedure SetStop(const Value: Boolean); virtual; abstract;
  public
    property BytesWritten: Int64 read GetBytesWritten;
    property Delta: Int64 read GetDelta;
    property ItemName: TZMString read GetItemName;
    property ItemNumber: Integer read GetItemNumber;
    property ItemPerCent: Integer read GetItemPerCent;
    property ItemPosition: Int64 read GetItemPosition;
    property ItemSize: Int64 read GetItemSize;
    property Order: TZMProgressType read GetOrder;
    property Stop: Boolean read GetStop write SetStop;
    property TotalCount: Int64 read GetTotalCount;
    property TotalPerCent: Integer read GetTotalPerCent;
    property TotalPosition: Int64 read GetTotalPosition;
    property TotalSize: Int64 read GetTotalSize;
  end;

  // ZipDirEntry status bit constants
const
  zsbDirty    = $1;
  zsbSelected = $2;
  zsbSkipped  = $4;
  zsbIgnore   = $8;
  zsbDirOnly  = $10;
  zsbInvalid  = $20;
  zsbError    = $40; // processing error
  zsbDiscard  = $80;

const
  DefNoSkips    = [stDupName, stReadError];
  ZMInitialCRC  = $FFFFFFFF;

type
  // used internally to ensure cleanup
  TZMWorkBin = class
  private
    FBinned: TList;
    FOwner: TZMWorkBin;
    procedure SetOwner(const Value: TZMWorkBin);
    procedure InsertIntoBin(WorkBin: TZMWorkBin);
    procedure KillBinned;
    procedure RemoveFromBin(WorkBin: TZMWorkBin);
  protected
    property Owner: TZMWorkBin read FOwner write SetOwner;
  public
    constructor Create; reintroduce; overload;
    constructor Create(theOwner: TZMWorkBin); overload; virtual;
    procedure BeforeDestruction; override;
  end;

type
  // abstract class representing a zip central record
  TZMDirEntry = class
  private
    function GetIsDirOnly: boolean;
  protected
    function GetCompressedSize: Int64; virtual; abstract;
    function GetCompressionMethod: Word; virtual; abstract;
    function GetCRC32: Cardinal; virtual; abstract;
    function GetDateStamp: TDateTime;
    function GetDateTime: Cardinal; virtual; abstract;
    function GetEncoded: TZMEncodingOpts; virtual; abstract;
    function GetEncrypted: boolean; virtual; abstract;
    function GetExtFileAttrib: Longword; virtual; abstract;
    function GetExtraData(Tag: Word): TZMRawBytes; virtual;
    function GetExtraField: TZMRawBytes; virtual; abstract;
    function GetExtraFieldLength: Word; virtual; abstract;
    function GetFileComment: TZMString; virtual; abstract;
    function GetFileCommentLen: Word; virtual; abstract;
    function GetFileName: TZMString; virtual; abstract;
    function GetFileNameLength: Word; virtual; abstract;
    function GetFlag: Word; virtual; abstract;
    function GetHeaderName: TZMRawBytes; virtual; abstract;
    function GetIntFileAttrib: Word; virtual; abstract;
    function GetRelOffLocalHdr: Int64; virtual; abstract;
    function GetStartOnDisk: Word; virtual; abstract;
    function GetStatusBits: Cardinal; virtual; abstract;
    function GetUncompressedSize: Int64; virtual; abstract;
    function GetVersionMadeBy: Word; virtual; abstract;
    function GetVersionNeeded: Word; virtual; abstract;
    function XData(const x: TZMRawBytes; Tag: Word;
      var idx, size: Integer): boolean;
  public
    property CompressedSize: Int64 read GetCompressedSize;
    property CompressionMethod: Word read GetCompressionMethod;
    property CRC32: Cardinal read GetCRC32;
    property DateStamp: TDateTime read GetDateStamp;
    property DateTime: Cardinal read GetDateTime;
    property Encoded: TZMEncodingOpts read GetEncoded;
    property Encrypted: boolean read GetEncrypted;
    property ExtFileAttrib: Longword read GetExtFileAttrib;
    property ExtraData[Tag: Word]: TZMRawBytes read GetExtraData;
    property ExtraField: TZMRawBytes read GetExtraField;
    property ExtraFieldLength: Word read GetExtraFieldLength;
    property FileComment: TZMString read GetFileComment;
    property FileCommentLen: Word read GetFileCommentLen;
    property FileName: TZMString read GetFileName;
    property FileNameLength: Word read GetFileNameLength;
    property Flag: Word read GetFlag;
    property HeaderName: TZMRawBytes read GetHeaderName;
    property IntFileAttrib: Word read GetIntFileAttrib;
    property IsDirOnly: boolean read GetIsDirOnly;
    property RelOffLocalHdr: Int64 read GetRelOffLocalHdr;
    property StartOnDisk: Word read GetStartOnDisk;
    property StatusBits: Cardinal read GetStatusBits;
    property UncompressedSize: Int64 read GetUncompressedSize;
    property VersionMadeBy: Word read GetVersionMadeBy;
    property VersionNeeded: Word read GetVersionNeeded;
  end;

  TZMDirRec = class(TZMDirEntry)
  public
    function ChangeAttrs(nAttr: Cardinal): Integer; virtual; abstract;
    function ChangeComment(const ncomment: TZMString): Integer;
      virtual; abstract;
    function ChangeData(ndata: TZMRawBytes): Integer; virtual; abstract;
    function ChangeDate(ndosdate: Cardinal): Integer; virtual; abstract;
    function ChangeEncoding: Integer; virtual; abstract;
    function ChangeName(const nname: TZMString): Integer; virtual; abstract;
    function ChangeStamp(ndate: TDateTime): Integer;
  end;

type
  TZMForEachFunction = function(rec: TZMDirEntry; var Data): Integer;
  TZMChangeFunction = function(rec: TZMDirRec; var Data): Integer;

type
  TZMRenameRec = record
    Source: string;
    Dest: string;
    Comment: string;
    DateTime: Integer;
  end;
  PZMRenameRec = ^TZMRenameRec;

type
  TZMConflictEntry = class
  protected
    function GetEntry: TZMDirEntry; virtual; abstract;
    function GetZipName: TZMString; virtual; abstract;
  public
    property Entry: TZMDirEntry read GetEntry;
    property ZipName: TZMString read GetZipName;
  end;

type
  TZMResolutions = (zmrRename, zmrExisting, zmrConflicting);

type
  // internal use only
  TZMRenderMethod = procedure(const Data: Pointer) of object;
  // internal use only
  TZMPerformMethod = function(var arg): Integer of object;

  // structure used to 'identify' streams
type
  TZMSStats = packed record
    size: Int64;
    Date: Cardinal;
    Attrs: Cardinal;
  end;

type
  TZMStreamOp = (zsoIdentify, zsoOpen, zsoClose);

type
  TZMBadNameEvent = procedure(Sender: TObject; var FileName: TZMString;
    const ExtName: TZMString; var Action: TZMNameFixes) of object;
  TZMCheckTerminateEvent = procedure(Sender: TObject; var abort: boolean)
    of object;
  TZMCopyZippedOverwriteEvent = procedure(Sender: TObject;
    src, dst: TZMDirEntry; var DoOverwrite: boolean) of object;
  TZMCRC32ErrorEvent = procedure(Sender: TObject; const ForFile: TZMString;
    FoundCRC, ExpectedCRC: Longword; var DoExtract: boolean) of object;
  TZMExtractOverwriteEvent = procedure(Sender: TObject;
    const ForFile: TZMString; IsOlder: boolean; var DoOverwrite: boolean;
    DirIndex: Integer) of object;
  TZMMergeZippedConflictEvent = procedure(Sender: TObject; Existing, Conflicting:
      TZMConflictEntry; var Resolve: TZMResolutions) of object;
  TZMSkippedEvent = procedure(Sender: TObject; const ForFile: TZMString;
    SkipType: TZMSkipTypes; var ExtError: Integer) of object;
  TZMFileCommentEvent = procedure(Sender: TObject; const ForFile: TZMString;
    var FileComment: TZMString; var IsChanged: boolean) of object;
  TZMFileExtraEvent = procedure(Sender: TObject; const ForFile: TZMString;
    var Data: TZMRawBytes; var IsChanged: boolean) of object;
  TZMGetNextDiskEvent = procedure(Sender: TObject;
    DiskSeqNo, DiskTotal: Integer; Drive: string; var AbortAction: boolean)
    of object;
  TZMLoadStrEvent = procedure(Ident: Integer; var DefStr: string) of object;
  TZMMessageEvent = procedure(Sender: TObject; ErrCode: Integer;
    const ErrMsg: TZMString) of object;
  TZMNewNameEvent = procedure(Sender: TObject; SeqNo: Integer) of object;
  TZMPasswordErrorEvent = procedure(Sender: TObject; IsZipAction: boolean;
    var NewPassword: string; const ForFile: TZMString;
    var RepeatCount: Longword; var Action: TMsgDlgBtn) of object;
  TZMProgressEvent = procedure(Sender: TObject; details: TZMProgressDetails)
    of object;
  TZMSetAddNameEvent = procedure(Sender: TObject; var FileName: TZMString;
    const ExtName: TZMString; var IsChanged: boolean) of object;
  TZMSetExtNameEvent = procedure(Sender: TObject; var FileName: TZMString;
    const BaseDir: TZMString; var IsChanged: boolean) of object;
  TZMStatusDiskEvent = procedure(Sender: TObject; PreviousDisk: Integer;
    PreviousFile: string; Status: TZMZipDiskStatus; var Action: TZMDiskAction)
    of object;
  TZMTickEvent = procedure(Sender: TObject) of object;
  TZMDialogEvent = procedure(Sender: TObject; const title: string;
    var msg: string; var Result: Integer; btns: TMsgDlgButtons) of object;
  TZMSetCompLevel = procedure(Sender: TObject; const ForFile: TZMString;
    var level: Integer; var IsChanged: boolean) of object;
  TZMStreamEvent = procedure(Sender: TObject; opr: TZMStreamOp;
    snumber: Integer; var strm: TStream; var stat: TZMSStats; var done: boolean)
    of object;
  TZMStateChange = procedure(Sender: TObject; state: TZMStates;
    var NoCursor: boolean) of object;

type
  TZMPipe = class
  protected
    function GetAttributes: Cardinal; virtual; abstract;
    function GetDOSDate: Cardinal; virtual; abstract;
    function GetFileName: string; virtual; abstract;
    function GetOwnsStream: boolean; virtual; abstract;
    function GetSize: Integer; virtual; abstract;
    function GetStream: TStream; virtual; abstract;
    procedure SetAttributes(const Value: Cardinal); virtual; abstract;
    procedure SetDOSDate(const Value: Cardinal); virtual; abstract;
    procedure SetFileName(const Value: string); virtual; abstract;
    procedure SetOwnsStream(const Value: boolean); virtual; abstract;
    procedure SetSize(const Value: Integer); virtual; abstract;
    procedure SetStream(const Value: TStream); virtual; abstract;
  public
    property Attributes: Cardinal read GetAttributes write SetAttributes;
    property DOSDate: Cardinal read GetDOSDate write SetDOSDate;
    property FileName: string read GetFileName write SetFileName;
    property OwnsStream: boolean read GetOwnsStream write SetOwnsStream;
    property size: Integer read GetSize write SetSize;
    property Stream: TStream read GetStream write SetStream;
  end;

  TZMPipeList = class
  protected
    function GetCount: Integer; virtual; abstract;
    function GetPipe(index: Integer): TZMPipe; virtual; abstract;
    procedure SetCount(const Value: Integer); virtual; abstract;
    procedure SetPipe(index: Integer; const Value: TZMPipe); virtual; abstract;
  public
    function Add(aStream: TStream; const FileName: string; Own: boolean)
      : Integer; virtual; abstract;
    procedure Clear; virtual; abstract;
    property Count: Integer read GetCount write SetCount;
    property Pipe[index: Integer]: TZMPipe read GetPipe write SetPipe; default;
  end;

type
  TZMInternal = record
    fAddCompLevel: Integer;
    fAddFrom: TDateTime;
    FAddOptions: TZMAddOpts;
    fAddStoreSuffixes: TZMAddStoreExts;
    FAuxChanged: boolean;
    fCancel: Integer;
    fConfirmErase: boolean;
    fDLLDirectory: string;
    fDLLLoad: boolean;
    fEncodeAs: TZMEncodingOpts;
    fEncoding: TZMEncodingOpts;
    fEncoding_CP: Cardinal;
    fEncrypt: boolean;
    fErrCode: Integer;
    fErrMessage: TZMString;
    fExcludeSpecs: TStringList;
    fExtAddStoreSuffixes: string;
    fExtErrCode: Cardinal;
    fExtrBaseDir: string;
    fExtrOptions: TZMExtrOpts;
    fHandle: HWND;
    fHowToDelete: TZMDeleteOpts;
    fIncludeSpecs: TStringList;
    fKeepFreeOnAllDisks: Cardinal;
    fKeepFreeOnDisk1: Cardinal;
    fMaxVolumeSize: Int64;
    fMinFreeVolumeSize: Cardinal;
    fNoReadAux: boolean;
    fNoSkipping: TZMSkipAborts;
    fNotMainThread: boolean;
    fPassword: string;
    fPasswordReqCount: Longword;
    FPipes: TZMPipeList;
    fRootDir: string;
    fSFXCaption: TZMString;
    fSFXCommandLine: TZMString;
    fSFXDefaultDir: string;
    fSFXIcon: TIcon;
    fSFXMessage: TZMString;
    fSFXOptions: TZMSFXOpts;
    fSFXOverwriteMode: TZMOvrOpts;
    fSFXPath: string;
    fSFXRegFailPath: string;
    fSpanOptions: TZMSpanOpts;
    fSuccessCnt: Integer;
    fTempDir: string;
    fTotalSizeToProcess: Int64;
    fUnattended: boolean;
    fUseDelphiBin: boolean;
    fUseDirOnlyEntries: boolean;
    fVerbosity: TZMVerbosity;
    fWriteOptions: TZMWriteOpts;
    fZipComment: AnsiString;
    fZipFileName: string;
    fZipStream: TMemoryStream;
  end;
  PZMInternal = ^TZMInternal;

type
{$IFDEF VERD2005up}
  TCustomZipMaster = class;

  TZipMasterEnumerator = class
  private
    FIndex: Integer;
    FOwner: TCustomZipMaster;
  public
    constructor Create(AMaster: TCustomZipMaster);
    function GetCurrent: TZMDirEntry;
    function MoveNext: boolean;
    property Current: TZMDirEntry read GetCurrent;
  end;
{$ENDIF}

  TZMWorkBase = class;
  // the main component
{$IFDEF VERDXE2up} [ComponentPlatformsAttribute(pidWin32 or pidWin64)] {$ENDIF}
  TCustomZipMaster = class(TComponent)
  private
    { Private versions of property variables }
    BusyFlag: Integer;
    fActive: Integer;
    fActiveWorker: TZMWorkBase;
    fBlockedCnt: Integer;
    FCurWaitCount: Integer;
    fDelaying: Integer;
    FFSpecArgs: TStrings;
    fFSpecArgsExcl: TStrings;
    fInternal: TZMInternal;
    FLanguage: string;
    FLanguageID: cardinal;
    FOnBadName: TZMBadNameEvent;
    fOnCheckTerminate: TZMCheckTerminateEvent;
    fOnCopyZippedOverwrite: TZMCopyZippedOverwriteEvent;
    fOnCRC32Error: TZMCRC32ErrorEvent;
    fOnDirUpdate: TNotifyEvent;
    fOnExtractOverwrite: TZMExtractOverwriteEvent;
    fOnFileComment: TZMFileCommentEvent;
    fOnFileExtra: TZMFileExtraEvent;
    fOnGetNextDisk: TZMGetNextDiskEvent;
    FOnMergeZippedConflict: TZMMergeZippedConflictEvent;
    fOnMessage: TZMMessageEvent;
    fOnNewName: TZMNewNameEvent;
    fOnPasswordError: TZMPasswordErrorEvent;
    fOnProgress: TZMProgressEvent;
    fOnSetAddName: TZMSetAddNameEvent;
    fOnSetCompLevel: TZMSetCompLevel;
    fOnSetExtName: TZMSetExtNameEvent;
    FOnSkipped: TZMSkippedEvent;
    fOnStateChange: TZMStateChange;
    fOnStatusDisk: TZMStatusDiskEvent;
    fOnStream: TZMStreamEvent;
    fOnTick: TZMTickEvent;
    fOnZipDialog: TZMDialogEvent;
    FPipes: TZMPipeList;
    FSaveCursor: TCursor;
    fSniffer: Cardinal;
    fSniffNo: Integer;
    FState: TZMStates;
    procedure AuxWasChanged;
    procedure ClearCachedNames_Action(const data: Pointer);
    procedure ClearErr;
    function Dll_ForcedBuild(Worker: TZMWorkBase): integer;
    procedure DLL_LoadDelayed;
    function DoDLL_Load(const Value: boolean): boolean;
    function DoSetZipComment(const Value: AnsiString): boolean;
    function FindSniffer: Cardinal;
    procedure Finish(WasGood: boolean);
    function GetActive: boolean;
    function GetAddCompLevel: Integer;
    function GetAddFrom: TDateTime;
    function GetAddOptions: TZMAddOpts;
    function GetAddStoreSuffixes: TZMAddStoreExts;
    function GetBuild: Integer;
//  { Property get/set functions }
    function GetCancel: boolean;
    function GetConfirmErase: boolean;
    function GetCount: Integer;
    function GetDirEntry(idx: Integer): TZMDirEntry;
    function GetDirOnlyCnt: Integer;
    function GetDLLDirectory: string;
    function GetDLL_Build: Integer;
    function GetDLL_Load: boolean;
    function GetDLL_Path: string;
    function GetDLL_Version: string;
    function GetDLL_Version1(ForceLoad: boolean): string;
    function GetEncodeAs: TZMEncodingOpts;
    function GetEncoding: TZMEncodingOpts;
    function GetEncoding_CP: Cardinal;
    function GetErrCode: Integer;
    function GetErrMessage: TZMString;
    function GetExtAddStoreSuffixes: string;
    function GetExtErrCode: cardinal;
    function GetExtrBaseDir: string;
    function GetHandle: HWND;
    function GetHowToDelete: TZMDeleteOpts;
    function GetIsSpanned: boolean;
    function GetKeepFreeOnAllDisks: Cardinal;
    function GetKeepFreeOnDisk1: Cardinal;
    function GetLanguage: string;
    function GetLanguageID: cardinal;
    {class} function GetLanguageInfo(idx: Integer; info: Cardinal): string;
    function GetMaxVolumeSize: Int64;
    function GetMaxVolumeSizeKb: Integer;
    function GetMinFreeVolumeSize: Cardinal;
    function GetNoReadAux: boolean;
    function GetNoSkipping: TZMSkipAborts;
    function GetNotMainThread: boolean;
    function GetOnLoadStr: TZMLoadStrEvent;
    function Get_Password: String;
    function GetPasswordReqCount: Longword;
    function GetRootDir: string;
    function GetSFXCaption: TZMString;
    function GetSFXCommandLine: TZMString;
    function GetSFXDefaultDir: string;
    function GetSFXIcon: TIcon;
    function GetSFXMessage: TZMString;
    function GetSFXOffset: Integer;
    function GetSFXOptions: TZMSFXOpts;
    function GetSFXOverwriteMode: TZMOvrOpts;
    function GetSFXPath: string;
    function GetSFXRegFailPath: string;
    function GetSpanOptions: TZMSpanOpts;
    function GetSuccessCnt: Integer;
    function GetTempDir: string;
    function GetTotalSizeToProcess: Int64;
    function GetTrace: boolean;
    function GetUnattended: boolean;
    function GetUseDirOnlyEntries: boolean;
{$IFNDEF _NO_UTF8_}
    function GetUseUTF8: boolean;
{$ENDIF}
    function GetVerbose: boolean;
    function GetVersion: string;
    function GetWriteOptions: TZMWriteOpts;
    function GetZipComment: AnsiString;
    function GetZipEOC: Int64;
    function GetZipFileName: string;
    function GetZipFileSize: Int64;
    function GetZipSOC: Int64;
    function GetZipStream: TMemoryStream;
    function KeepAlive: boolean;
    procedure Render(Action: TZMRenderMethod; const Data: Pointer);
    function ReportSkipping(const FName: string; err: Integer;
      typ: TZMSkipTypes): boolean;
    procedure SetActive(Value: boolean);
    procedure SetAddCompLevel(const Value: Integer);
    procedure SetAddFrom(const Value: TDateTime);
    procedure SetAddOptions(const Value: TZMAddOpts);
    procedure SetAddStoreSuffixes(const Value: TZMAddStoreExts);
    procedure SetCancel(Value: boolean);
    procedure SetConfirmErase(const Value: boolean);
    procedure SetDLLDirectory(const Value: string);
    procedure SetDLL_Load(const Value: boolean);
    procedure SetEncodeAs(const Value: TZMEncodingOpts);
    procedure SetEncoding(const Value: TZMEncodingOpts);
    procedure SetEncoding_CP(Value: Cardinal);
    procedure SetErrCode(Value: Integer);
    procedure SetExtAddStoreSuffixes(const Value: string);
    procedure SetExtErrCode(const Value: cardinal);
    procedure SetExtrBaseDir(const Value: string);
    procedure SetExtrOptions(const Value: TZMExtrOpts);
    procedure SetFSpecArgs(const Value: TStrings);
    procedure SetFSpecArgsExcl(const Value: TStrings);
    procedure SetHandle(const Value: HWND);
    procedure SetHowToDelete(const Value: TZMDeleteOpts);
    procedure SetKeepFreeOnAllDisks(const Value: Cardinal);
    procedure SetKeepFreeOnDisk1(const Value: Cardinal);
    procedure SetLanguage(const Value: string);
    procedure SetLanguageID(const Value: cardinal);
    procedure SetMaxVolumeSize(const Value: Int64);
    procedure SetMaxVolumeSizeKb(const Value: Integer);
    procedure SetMinFreeVolumeSize(const Value: Cardinal);
    procedure SetNoReadAux(const Value: boolean);
    procedure SetNoSkipping(const Value: TZMSkipAborts);
    procedure SetNotMainThread(const Value: boolean);
    procedure SetOnLoadStr(const Value: TZMLoadStrEvent);
    procedure Set_Password(const Value: string);
    procedure SetPasswordReqCount(Value: Longword);
    procedure SetPipes(const Value: TZMPipeList);
    procedure SetRootDir(const Value: string);
    procedure SetSFXCaption(const Value: TZMString);
    procedure SetSFXCommandLine(const Value: TZMString);
    procedure SetSFXDefaultDir(const Value: string);
    procedure SetSFXIcon(Value: TIcon);
    procedure SetSFXMessage(const Value: TZMString);
    procedure SetSFXOptions(const Value: TZMSFXOpts);
    procedure SetSFXOverwriteMode(const Value: TZMOvrOpts);
    procedure SetSFXPath(const Value: string);
    procedure SetSFXRegFailPath(const Value: string);
    procedure SetSpanOptions(const Value: TZMSpanOpts);
    procedure SetTempDir(const Value: string);
    procedure SetTrace(const Value: boolean);
    procedure SetUnattended(const Value: boolean);
    procedure SetUseDirOnlyEntries(const Value: boolean);
    procedure SetUseDirOnlyEntries_Action(const Data: Pointer);
{$IFNDEF _NO_UTF8_}
    procedure SetUseUTF8(const Value: boolean);
    procedure SetUseUTF8_Action(const Data: Pointer);
{$ENDIF}
    procedure SetVerbose(const Value: boolean);
    procedure SetVersion(const Value: string);
    procedure SetWriteOptions(const Value: TZMWriteOpts);
    procedure SetZipComment(const Value: AnsiString);
    procedure SetZipFileName(const Value: string);
    procedure Set_ZipCommentDelayed;
  protected
    fLister: TZMWorkBase;
    fLoadCheckNo: Integer;
    function CanStart: boolean;
    procedure DoDelays;
    procedure done(Good: boolean = True);
    procedure DoneBad(E: Exception);
    function IsActive: boolean;
    procedure Loaded; override;
    function Permitted: boolean;
    procedure ReEntered;
    function ReEntry: boolean;
    procedure ReportMessage(err: Integer; const msg: TZMString; NoLock: boolean);
    procedure ReportToSniffer(err: Cardinal; const msg: TZMString);
    procedure Set_Lister(aLister: TZMWorkBase);
    procedure ShowMsg(const msg: TZMString; ResID: Integer; display: boolean);
    procedure Start;
    procedure StartWaitCursor;
    procedure StateChanged(newState: TZMStates);
    function Stopped: boolean;
    procedure StopWaitCursor;
    function SysErrorMsg(ErrNo: cardinal = Cardinal(-1)): string;
    function ZipFmtLoadStr(id: Integer; const Args: array of const ): TZMString;
    function ZipMessageDialog(const title: string; var msg: string;
      context: Integer; btns: TMsgDlgButtons): TModalResult;
    function _GetAddPassword(var Response: TMsgDlgBtn): string;
    function _GetExtrPassword(var Response: TMsgDlgBtn): string;
    function _GetPassword(const DialogCaption, MsgTxt: string; ctx: Integer;
      pwb: TMsgDlgButtons; var ResultStr: string): TMsgDlgBtn;
    property LanguageID: cardinal read GetLanguageID write SetLanguageID;
  public
    procedure AbortDLL;
    function Add: Integer;
    function AddStreamToFile(const FileName: string;
      FileDate, FileAttr: Dword): Integer;
    function AddStreamToStream(InStream: TMemoryStream): TMemoryStream;
    function AddZippedFiles(SrcZipMaster: TCustomZipMaster; merge: TZMMergeOpts):
        Integer; {$IFNDEF VERpre6} deprecated; {$ENDIF}
    procedure AfterConstruction; override;
    function AppendSlash(const sDir: string): string;
    procedure BeforeDestruction; override;
    function ChangeFileDetails(func: TZMChangeFunction; var Data): Integer;
    procedure Clear;
    function ConvertToSFX: Integer;
    function ConvertToSpanSFX(const OutFile: string): Integer;
    function ConvertToZIP: Integer;
    function CopyZippedFiles(DestZipMaster: TCustomZipMaster; DeleteFromSource:
        boolean; OverwriteDest: TZMMergeOpts): Integer; {$IFNDEF VERpre6} deprecated; {$ENDIF}
    function Copy_File(const InFileName, OutFileName: string): Integer;
    function Deflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal): Integer;
    function Delete: Integer;
    function EraseFile(const FName: string; How: TZMDeleteOpts): Integer;
    function Extract: Integer;
    function ExtractFileToStream(const FileName: string): TMemoryStream;
    function ExtractStreamToStream(InStream: TMemoryStream; OutSize: Longword)
      : TMemoryStream;
    function Find(const fspec: TZMString; var idx: Integer): TZMDirEntry;
    function ForEach(func: TZMForEachFunction; var Data): Integer;
    function FullVersionString: string;
    function GetAddPassword: string; overload;
    function GetAddPassword(var Response: TMsgDlgBtn): string; overload;
{$IFDEF VERD2005up}
    function GetEnumerator: TZipMasterEnumerator;
{$ENDIF}
    function GetExtrPassword: string; overload;
    function GetExtrPassword(var Response: TMsgDlgBtn): string; overload;
    function GetPassword(const DialogCaption, MsgTxt: string;
      pwb: TMsgDlgButtons; var ResultStr: string): TMsgDlgBtn;
    function IndexOf(const FName: TZMString): Integer;
    function IsZipSFX(const SFXExeName: string): Integer;
    function List: Integer;
    function MakeTempFileName(const Prefix, Extension: string): string;
    function MergeZippedFiles(Opts: TZMMergeOpts): Integer;
    function QueryZip(const FName: TFileName): Integer;
    function ReadSpan(const InFileName: string;
      var OutFilePath: string): Integer;
    function Rename(RenameList: TList; DateTime: Integer;
      How: TZMRenameOpts = htrDefault): Integer;
    procedure ShowExceptionError(const ZMExcept: Exception);
    procedure ShowZipFmtMessage(id: Integer; const Args: array of const;
      display: boolean);
    procedure ShowZipMessage(Ident: Integer; const UserStr: string);
    function Undeflate(OutStream, InStream: TStream; Length: Int64;
      var Method: TZMDeflates; var CRC: Cardinal): Integer;
    function WriteSpan(const InFileName, OutFileName: string): Integer;
    function ZipLoadStr(id: Integer): string;

    property Active: boolean read GetActive write SetActive default True;
    property AddCompLevel: Integer read GetAddCompLevel write SetAddCompLevel
        default 9;
    property AddFrom: TDateTime read GetAddFrom write SetAddFrom;
    property AddOptions: TZMAddOpts read GetAddOptions write SetAddOptions;
    property AddStoreSuffixes: TZMAddStoreExts read GetAddStoreSuffixes write
        SetAddStoreSuffixes;
    property BlockedCnt: Integer read fBlockedCnt;
    property Build: Integer read GetBuild;
    property Cancel: boolean read GetCancel write SetCancel;
    property ConfirmErase: boolean read GetConfirmErase write SetConfirmErase
        default True;
    property Count: Integer read GetCount;
    property DirEntry[idx: Integer]: TZMDirEntry read GetDirEntry; default;
    property DirOnlyCnt: Integer read GetDirOnlyCnt;
    property DLLDirectory: string read GetDLLDirectory write SetDLLDirectory;
    property DLL_Build: Integer read GetDLL_Build;
    property DLL_Load: boolean read GetDLL_Load write SetDLL_Load;
    property DLL_Path: string read GetDLL_Path;
    property DLL_Version: string read GetDLL_Version;
    property EncodeAs: TZMEncodingOpts read GetEncodeAs write SetEncodeAs;
    // 1 Filename and comment character encoding
    property Encoding: TZMEncodingOpts read GetEncoding write SetEncoding default
        zeoAuto;
    // 1 codepage to use to decode filename
    property Encoding_CP: Cardinal read GetEncoding_CP write SetEncoding_CP;
    property ErrCode: Integer read GetErrCode write SetErrCode;
    property ErrMessage: TZMString read GetErrMessage;
    property ExtAddStoreSuffixes: string read GetExtAddStoreSuffixes write
        SetExtAddStoreSuffixes;
    property ExtErrCode: cardinal read GetExtErrCode write SetExtErrCode;
    property ExtrBaseDir: string read GetExtrBaseDir write SetExtrBaseDir;
    property ExtrOptions: TZMExtrOpts read fInternal.fExtrOptions write SetExtrOptions;
    property FSpecArgs: TStrings read FFSpecArgs write SetFSpecArgs;
    property FSpecArgsExcl: TStrings read fFSpecArgsExcl write SetFSpecArgsExcl;
    property Handle: HWND read GetHandle write SetHandle;
    property HowToDelete: TZMDeleteOpts read GetHowToDelete write SetHowToDelete
        default htdAllowUndo;
    property IsSpanned: boolean read GetIsSpanned;
    property KeepFreeOnAllDisks: Cardinal read GetKeepFreeOnAllDisks write
        SetKeepFreeOnAllDisks;
    property KeepFreeOnDisk1: Cardinal read GetKeepFreeOnDisk1 write
        SetKeepFreeOnDisk1;
    property Language: string read GetLanguage write SetLanguage;
    property LanguageInfo[idx: Integer; info: Cardinal]: string
      read GetLanguageInfo;
    property MaxVolumeSize: Int64 read GetMaxVolumeSize write SetMaxVolumeSize;
    property MaxVolumeSizeKb: Integer read GetMaxVolumeSizeKb write
        SetMaxVolumeSizeKb;
    property MinFreeVolumeSize: Cardinal read GetMinFreeVolumeSize write
        SetMinFreeVolumeSize default 65536;
    property NoReadAux: boolean read GetNoReadAux write SetNoReadAux;
    property NoSkipping: TZMSkipAborts read GetNoSkipping write SetNoSkipping
        default DefNoSkips;
    property NotMainThread: boolean read GetNotMainThread write SetNotMainThread;
    property Password: string read Get_Password write Set_Password;
    property PasswordReqCount: Longword read GetPasswordReqCount write
        SetPasswordReqCount default 1;
    property Pipes: TZMPipeList read FPipes write SetPipes;
    property RootDir: string read GetRootDir write SetRootDir;
    property SFXCaption: TZMString read GetSFXCaption write SetSFXCaption;
    property SFXCommandLine: TZMString read GetSFXCommandLine write
        SetSFXCommandLine;
    property SFXDefaultDir: string read GetSFXDefaultDir write SetSFXDefaultDir;
    property SFXIcon: TIcon read GetSFXIcon write SetSFXIcon;
    property SFXMessage: TZMString read GetSFXMessage write SetSFXMessage;
    property SFXOffset: Integer read GetSFXOffset;
    property SFXOptions: TZMSFXOpts read GetSFXOptions write SetSFXOptions;
    property SFXOverwriteMode: TZMOvrOpts read GetSFXOverwriteMode write
        SetSFXOverwriteMode default ovrConfirm;
    property SFXPath: string read GetSFXPath write SetSFXPath;
    property SFXRegFailPath: string read GetSFXRegFailPath write SetSFXRegFailPath;
    property SpanOptions: TZMSpanOpts read GetSpanOptions write SetSpanOptions;
    property State: TZMStates read FState;
    property SuccessCnt: Integer read GetSuccessCnt;
    property TempDir: string read GetTempDir write SetTempDir;
    property TotalSizeToProcess: Int64 read GetTotalSizeToProcess;
    property Trace: boolean read GetTrace write SetTrace;
    property Unattended: boolean read GetUnattended write SetUnattended;
    property UseDirOnlyEntries: boolean read GetUseDirOnlyEntries write
        SetUseDirOnlyEntries default False;
{$IFNDEF _NO_UTF8_}
    property UseUTF8: boolean read GetUseUTF8 write SetUseUTF8;
{$ENDIF}
    property Verbose: boolean read GetVerbose write SetVerbose;
    property Version: string read GetVersion write SetVersion;
    property WriteOptions: TZMWriteOpts read GetWriteOptions write SetWriteOptions;
    property ZipComment: AnsiString read GetZipComment write SetZipComment;
    property ZipEOC: Int64 read GetZipEOC;
    property ZipFileName: string read GetZipFileName write SetZipFileName;
    property ZipFileSize: Int64 read GetZipFileSize;
    property ZipSOC: Int64 read GetZipSOC;
    property ZipStream: TMemoryStream read GetZipStream;
    { Events }
    property OnBadName: TZMBadNameEvent read FOnBadName write FOnBadName;
    property OnCheckTerminate: TZMCheckTerminateEvent read fOnCheckTerminate
      write fOnCheckTerminate;
    property OnCopyZippedOverwrite: TZMCopyZippedOverwriteEvent
      read fOnCopyZippedOverwrite write fOnCopyZippedOverwrite;
    property OnCRC32Error: TZMCRC32ErrorEvent read fOnCRC32Error
      write fOnCRC32Error;
    property OnDirUpdate: TNotifyEvent read fOnDirUpdate write fOnDirUpdate;
    property OnExtractOverwrite: TZMExtractOverwriteEvent
      read fOnExtractOverwrite write fOnExtractOverwrite;
    property OnFileComment: TZMFileCommentEvent read fOnFileComment
      write fOnFileComment;
    property OnFileExtra: TZMFileExtraEvent read fOnFileExtra
      write fOnFileExtra;
    property OnGetNextDisk: TZMGetNextDiskEvent read fOnGetNextDisk
      write fOnGetNextDisk;
    property OnLoadStr: TZMLoadStrEvent read GetOnLoadStr write SetOnLoadStr;
    property OnMergeZippedConflict: TZMMergeZippedConflictEvent read
        FOnMergeZippedConflict write FOnMergeZippedConflict;
    property OnMessage: TZMMessageEvent read fOnMessage write fOnMessage;
    property OnNewName: TZMNewNameEvent read fOnNewName write fOnNewName;
    property OnPasswordError: TZMPasswordErrorEvent read fOnPasswordError
      write fOnPasswordError;
    property OnProgress: TZMProgressEvent read fOnProgress write fOnProgress;
    property OnSetAddName: TZMSetAddNameEvent read fOnSetAddName
      write fOnSetAddName;
    property OnSetCompLevel: TZMSetCompLevel read fOnSetCompLevel
      write fOnSetCompLevel;
    property OnSetExtName: TZMSetExtNameEvent read fOnSetExtName
      write fOnSetExtName;
    property OnSkipped: TZMSkippedEvent read FOnSkipped write FOnSkipped;
    property OnStateChange: TZMStateChange read fOnStateChange
      write fOnStateChange;
    property OnStatusDisk: TZMStatusDiskEvent read fOnStatusDisk
      write fOnStatusDisk;
    property OnStream: TZMStreamEvent read fOnStream write fOnStream;
    property OnTick: TZMTickEvent read fOnTick write fOnTick;
    property OnZipDialog: TZMDialogEvent read fOnZipDialog write fOnZipDialog;
  end;

  TZMWorkBase = class(TZMWorkBin)
  private
    function GetActiveWorker: TZMWorkBase;
    function GetExcludeSpecs: TStrings;
    function GetIncludeSpecs: TStrings;
    function GetNextLoadCheckNo: Integer;
    function GetTheLister: TZMWorkBase;
    function GetVerbosity: TZMVerbosity;
    procedure SetActiveWorker(const Value: TZMWorkBase);
    procedure SetExcludeSpecs(const Value: TStrings);
    procedure SetIncludeSpecs(const Value: TStrings);
  protected
    fInternal: PZMInternal;
    fMaster: TCustomZipMaster;
    procedure CancelSet(Value: Integer); virtual;
    // 1 Gets called by master before not busy
    procedure Finished(WasGood: boolean); virtual; abstract;
    procedure IndexCheck(Index, Count: Integer);
    procedure Started; virtual; abstract;
    property ActiveWorker: TZMWorkBase read GetActiveWorker
      write SetActiveWorker;
    property TheLister: TZMWorkBase read GetTheLister;
  public
    constructor Create(anOwner: TZMWorkBin); reintroduce; overload;
    constructor Create(theMaster: TCustomZipMaster; anOwner: TZMWorkBin);
        overload;
    function KeepAlive: boolean;
    procedure ReportMessage(err: Integer; const msg: TZMString);
    procedure ReportMsg(id: Integer; const Args: array of const);
    function ReportSkipping(const FName: string; err: Integer;
      typ: TZMSkipTypes): boolean;
    procedure ReportToSniffer(err: Cardinal; const msg: TZMString);
    procedure ShowExceptionError(const ZMExcept: Exception);
    procedure ShowMsg(const msg: TZMString; ResID: Integer; display: boolean);
    procedure ShowZipFmtMessage(id: Integer; const Args: array of const;
      display: boolean);
    procedure ShowZipMessage(Ident: Integer; const UserStr: string);
    function SysErrorMsg(ErrNo: cardinal = Cardinal(-1)): string;
    function ZipFmtLoadStr(id: Integer; const Args: array of const): TZMString;
    function ZipMessageDialog(const title: string; var msg: string;
      context: Integer; btns: TMsgDlgButtons): TModalResult;
    property ExcludeSpecs: TStrings read GetExcludeSpecs write SetExcludeSpecs;
    property IncludeSpecs: TStrings read GetIncludeSpecs write SetIncludeSpecs;
    property Master: TCustomZipMaster read fMaster;
    property NextLoadCheckNo: Integer read GetNextLoadCheckNo;
    property Verbosity: TZMVerbosity read GetVerbosity;
  end;
        
type
  TZipMaster = class(TCustomZipMaster)
  published
    property Active default True;
    property AddCompLevel default 9;
    property AddFrom;
    property AddOptions;
    property AddStoreSuffixes;
    property ConfirmErase default True;
    property DLLDirectory;
    property DLL_Load;
    // 1 Filename and comment character encoding
    property Encoding default zeoAuto;
    property ExtAddStoreSuffixes;
    property ExtrBaseDir;
    property ExtrOptions;
    property FSpecArgs;
    property FSpecArgsExcl;
    property HowToDelete;
    property KeepFreeOnAllDisks;
    property KeepFreeOnDisk1;
    property Language;
    property LanguageID;
    property MaxVolumeSize;
    property MaxVolumeSizeKb;
    property MinFreeVolumeSize default 65536;
    property NoReadAux;
    property NoSkipping default DefNoSkips;
    { Events }
    property OnBadName;
    property OnCheckTerminate;
    property OnCopyZippedOverwrite;
    property OnCRC32Error;
    property OnDirUpdate;
    property OnExtractOverwrite;
    property OnFileComment;
    property OnFileExtra;
    property OnGetNextDisk;
    property OnLoadStr;
    property OnMessage;
    property OnNewName;
    property OnPasswordError;
    property OnProgress;
    property OnSetAddName;
    property OnSetCompLevel;
    property OnSetExtName;
    property OnSkipped;
    property OnStatusDisk;
    property OnStream;
    property OnTick;
    property OnZipDialog;
    property Password;
    property PasswordReqCount default 1;
    // SFX
    property RootDir;
    property SFXCaption;
    property SFXCommandLine;
    property SFXDefaultDir;
    property SFXIcon;
    property SFXMessage;
    property SFXOptions;
    property SFXOverwriteMode;
    property SFXPath;
    property SFXRegFailPath;
    property SpanOptions;
    property TempDir;
    property Trace;
    property Unattended;
    property UseDirOnlyEntries;
{$IFNDEF _NO_UTF8_}
    property UseUTF8;
{$ENDIF}
    property Verbose;
    property Version;
    property WriteOptions;
    property ZipComment;
    property ZipFileName;
  end;

  // default file extensions that are best 'stored'
const
  ZMDefAddStoreSuffixes = [assGIF .. assJAR, assJPG .. ass7Zp,
    assMP3 .. assAVI];

//procedure Register;

implementation

uses
  Messages,
  ZMCompat, ZMUtils, ZMCore, ZMMsg, ZMMatch, ZMMsgStr,
  ZMUTF8, ZMDlg, ZMDelZip, ZMCtx, ZMLister, ZMWFuncs,
  ZMZippedOpr, ZMDllOpr, ZMModOpr, ZMFileOpr, ZMDllLoad, ZMZipFile;

{$R   'res\zmres191_str.res'}

const
  __UNIT__ = 1 shl 23;

const
  DelayingLanguage = 1;
  DelayingLanguageID = 2;
  DelayingFileName = 4;
  DelayingComment  = 8;
  DelayingDLL      = 16;

const
  SZipMasterSniffer = 'ZipMaster Sniffer';
  STZipSniffer      = 'TSnifferForm';//'TZipSniffer';
  WM_SNIFF_START    = WM_APP + $3F42;
  WM_SNIFF_STOP     = WM_APP + $3F44;
  SNIFF_MASK        = $FFFFFF;
  RESOURCE_ERROR
    : string = 'ZMRes_???.res is probably not linked to the executable' + #10
    + 'Missing String ID is: %d ';

//procedure Register;
//begin
//  RegisterComponents('DelphiZip', [TZipMaster]);
//end;

{ TZMProgressDetails }
function TZMProgressDetails.GetItemPerCent: Integer;
begin
  if (ItemSize > 0) and (ItemPosition > 0) then
    Result := (100 * ItemPosition) div ItemSize
  else
    Result := 0;
end;

function TZMProgressDetails.GetTotalPerCent: Integer;
begin
  if (TotalSize > 0) and (TotalPosition > 0) then
    Result := (100 * TotalPosition) div TotalSize
  else
    Result := 0;
end;

{ TZMDirEntry }
function TZMDirEntry.GetDateStamp: TDateTime;
begin
  Result := FileDateToLocalDateTime(GetDateTime);
end;

// return first data for Tag
function TZMDirEntry.GetExtraData(Tag: Word): TZMRawBytes;
var
  i: Integer;
  sz: Integer;
begin
  Result := ExtraField;
  if (ExtraFieldLength >= 4) and XData(Result, Word(Tag), i, sz) then
    Result := Copy(Result, 5, sz - 4)
  else
    Result := '';
end;

function TZMDirEntry.GetIsDirOnly: boolean;
begin
  Result := (StatusBits and zsbDirOnly) <> 0;
end;

function TZMDirEntry.XData(const x: TZMRawBytes; Tag: Word;
  var idx, size: Integer): boolean;
var
  i: Integer;
  l: Integer;
  wsz: Word;
  wtg: Word;
begin
  Result := False;
  idx := 0;
  size := 0;
  i := 1;
  l := Length(x);
  while i <= l - 4 do
  begin
    wtg := pWord(@x[i])^;
    wsz := pWord(@x[i + 2])^;
    if wtg = Tag then
    begin
      Result := (i + wsz + 4) <= l + 1;
      if Result then
      begin
        idx := i;
        size := wsz + 4;
      end;
      break;
    end;
    i := i + wsz + 4;
  end;
end;

{ TZMDirRec }
function TZMDirRec.ChangeStamp(ndate: TDateTime): Integer;
begin
  Result := ChangeDate(DateTimeToFileDate(ndate));
end;

{$IFDEF VERD2005up}
{ TZipMasterEnumerator }
constructor TZipMasterEnumerator.Create(AMaster: TCustomZipMaster);
begin
  inherited Create;
  FIndex := -1;
  FOwner := AMaster;
end;

function TZipMasterEnumerator.GetCurrent: TZMDirEntry;
begin
  Result := FOwner[FIndex];
end;

function TCustomZipMaster.GetEnumerator: TZipMasterEnumerator;
begin
  Result := TZipMasterEnumerator.Create(Self);
end;

function TZipMasterEnumerator.MoveNext: boolean;
begin
  Result := FIndex < (FOwner.Count - 1);
  if Result then
    Inc(FIndex);
end;
{$ENDIF}

{ TCustomZipMaster }
function TheLister(aLister: TZMWorkBase): TZMLister;
begin
  if aLister is TZMLister then
    Result := TZMLister(aLister)
  else
    Result := nil;
end;

procedure TCustomZipMaster.AbortDLL;
begin
  if fActiveWorker is TZMDLLOpr then
    TZMDLLOpr(fActiveWorker).CancelSet(GE_Abort);
end;

function TCustomZipMaster.Add: Integer;
var
  DllWorker: TZMDLLOpr;
begin
  if Permitted then
    try
      Start;
      DllWorker := TZMDLLOpr.Create(Self);
      DllWorker.Add;
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
  Result := ErrCode;
end;

function TCustomZipMaster.AddStreamToFile(const FileName: string;
  FileDate, FileAttr: Dword): Integer;
var
  DllWorker: TZMDLLOpr;
begin
  if Permitted then
    try
      Start;
      DllWorker := TZMDLLOpr.Create(Self);
      DllWorker.AddStreamToFile(FileName, FileDate, FileAttr);
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
  Result := ErrCode;
end;

function TCustomZipMaster.AddStreamToStream(InStream: TMemoryStream)
  : TMemoryStream;
var
  DllWorker: TZMDLLOpr;
begin
  Result := nil;
  if Permitted then
    try
      Start;
      DllWorker := TZMDLLOpr.Create(Self);
      DllWorker.AddStreamToStream(InStream);
      if SuccessCnt = 1 then
        Result := ZipStream;
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
end;

procedure TCustomZipMaster.AfterConstruction;
begin
  inherited;
  fDelaying := 0;
  BusyFlag := 0;
  FCurWaitCount := 0;
  fInternal.fNotMainThread := GetCurrentThreadID <> MainThreadID;
  fInternal.FNoReadAux := False;
  fInternal.FAuxChanged := False;
  FFSpecArgs := TStringList.Create;
  fFSpecArgsExcl := TStringList.Create;
  fInternal.fIncludeSpecs := TStringList.Create;
  fInternal.fExcludeSpecs := TStringList.Create;
  FPipes := TZMPipeListImp.Create;
  fInternal.fAddCompLevel := 9; // default to tightest compression
  fInternal.fAddStoreSuffixes := ZMDefAddStoreSuffixes;
  fInternal.fEncoding := zeoAuto;
  fInternal.fEncrypt := False;
  fInternal.fAddFrom := 0;
  fInternal.fHandle := Application.Handle;
  fInternal.fHowToDelete := htdAllowUndo;
  fInternal.FPassword := '';
  fInternal.fPasswordReqCount := 1;
  fInternal.fUnattended := False;
  fInternal.FUseDirOnlyEntries := False;
  fInternal.fUseDelphiBin := True;
  fInternal.fMinFreeVolumeSize := 65536;
  fInternal.fMaxVolumeSize := 0;
  fInternal.fKeepFreeOnAllDisks := 0;
  fInternal.fKeepFreeOnDisk1 := 0;
  fInternal.fConfirmErase := False;
  fInternal.FNoSkipping := DefNoSkips;
  fLister := nil;
  fActiveWorker := nil;
  fActive := 2;
end;

function TCustomZipMaster.AppendSlash(const sDir: string): string;
begin
  Result := DelimitPath(sDir, True);
end;

procedure TCustomZipMaster.AuxWasChanged;
begin
  if (not NoReadAux) or (csDesigning in ComponentState) or
    (csLoading in ComponentState) then
    fInternal.FAuxChanged := True;
end;

procedure TCustomZipMaster.BeforeDestruction;
begin
  Cancel := True; // stop any activity
  fActive := 0;
  fOnMessage := nil; // stop any messages being sent
  fOnStateChange := nil;
  fOnStream := nil;
  fOnTick := nil;
  fOnZipDialog := nil;
  fActiveWorker.Free;
  FFSpecArgs.Free;
  fFSpecArgsExcl.Free;
  FPipes.Free;
  fLister.Free;
  fInternal.fIncludeSpecs.Free;
  fInternal.fExcludeSpecs.Free;
  fInternal.fZipStream.Free;
  _DLL_Remove(Self); // remove from list
  inherited;
end;

function TCustomZipMaster.CanStart: boolean;
begin
  if not IsActive then // not Active
    Result := False
  else
    Result := Stopped;
end;

function TCustomZipMaster.ChangeFileDetails(func: TZMChangeFunction;
  var Data): Integer;
var
  ModWorker: TZMModOpr;
begin
  Result := 0;
  if Permitted then
    try
      Start;
      ModWorker := TZMModOpr.Create(Self);
      Result := ModWorker.ChangeFileDetails(@func, Data);
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
end;

procedure TCustomZipMaster.Clear;
var
  aLister: TZMWorkBase;
begin
  if Permitted then
  begin
    if fLister <> nil then
    begin
      aLister := fLister;
      fLister := nil;
      aLister.Free;
    end;
    Pipes.Clear;
    FFSpecArgs.Clear;
    fFSpecArgsExcl.Clear;
    fInternal.fIncludeSpecs.Clear;
    fInternal.fExcludeSpecs.Clear;
    fInternal.fPasswordReqCount := 1;
    fInternal.fTotalSizeToProcess := 0;
    if fInternal.fZipStream <> nil then
      fInternal.fZipStream.Clear;
    done;
  end;
end;

procedure TCustomZipMaster.ClearErr;
begin
  fInternal.fErrMessage := '';
  fInternal.fErrCode := 0;
  fInternal.fExtErrCode := 0;
end;

function TCustomZipMaster.ConvertToSFX: Integer;
var
  FileWorker: TZMFileOpr;
begin
  Result := 0;
  if Permitted then
    try
      Start;
      FileWorker := TZMDLLOpr.Create(Self);
      Result := FileWorker.ConvertToSFX('', nil);
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
  if ErrCode <> 0 then
    Result := ErrCode;
end;

function TCustomZipMaster.ConvertToSpanSFX(const OutFile: string): Integer;
var
  FileWorker: TZMFileOpr;
begin
  Result := 0;
  if Permitted then
    try
      Start;
      FileWorker := TZMDLLOpr.Create(Self);
      Result := FileWorker.ConvertToSpanSFX(OutFile, nil);
      if Result > 0 then
        Result := 0; //? keep old return value
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
  if ErrCode <> 0 then
    Result := ErrCode;
end;

function TCustomZipMaster.ConvertToZIP: Integer;
var
  FileWorker: TZMFileOpr;
begin
  Result := 0;
  if Permitted then
    try
      Start;
      FileWorker := TZMFileOpr.Create(Self);
      Result := FileWorker.ConvertToZIP;
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
  if ErrCode <> 0 then
    Result := ErrCode;
end;

function CopyBuffer(InFile, OutFile: Integer; ReadLen: Int64): Integer;
const
  __ERR_DS_CopyError = __UNIT__ + (1515 shl 10) + DS_CopyError;
  __ERR_DS_CopyError1 = __UNIT__ + (1522 shl 10) + DS_CopyError;
const
  BufSize = 10 * 1024;
var
  Buffer: array of Byte;
  SizeR: Integer;
  ToRead: Cardinal;
begin
  // both files are already open
  Result := 0;
  if ReadLen = 0 then
    Exit;
  ToRead := BufSize;
  try
    SetLength(Buffer, BufSize);
    repeat
      if ReadLen >= 0 then
      begin
        ToRead := BufSize;
        if ReadLen < ToRead then
          ToRead := ReadLen;
      end;
      SizeR := FileRead(InFile, Buffer[0], ToRead);
      if (SizeR < 0) or (FileWrite(OutFile, Buffer[0], SizeR) <> SizeR) then
      begin
        Result := __ERR_DS_CopyError;
        break;
      end;
      if (ReadLen > 0) then
        ReadLen := ReadLen - Cardinal(SizeR);
    until ((ReadLen = 0) or (SizeR <> Integer(ToRead)));
  except
    Result := __ERR_DS_CopyError1;
  end;
  // leave both files open
end;

// maybe move to utils
function TCustomZipMaster.Copy_File(const InFileName,
  OutFileName: string): Integer;
const
  __ERR_DS_FileOpen = __UNIT__ + (1544 shl 10) + DS_FileOpen;
  __ERR_DS_NoOutFile = __UNIT__ + (1556 shl 10) + DS_NoOutFile;
  __ERR_DS_SetDateError = __UNIT__ + (1568 shl 10) + DS_SetDateError;
  __ERR_DS_NoOutFile1 = __UNIT__ + (1573 shl 10) + DS_NoOutFile;
  __ERR_DS_UnknownError = __UNIT__ + (1580 shl 10) + DS_UnknownError;
var
  InFile: THandle;//Integer;
  In_Size: Int64;
  OutFile: THandle;//Integer;
  Out_Size: Int64;
begin
  In_Size := -1;
  Out_Size := -1;
  Result := -__ERR_DS_FileOpen;

  if not _Z_FileExists(InFileName) then
    Exit;
  InFile := _Z_FileOpen(InFileName, fmOpenRead or fmShareDenyWrite);
  if InFile <> Invalid_Handle then
  begin
    if _Z_FileExists(OutFileName) then
    begin
      OutFile := _Z_FileOpen(OutFileName, fmOpenWrite or fmShareExclusive);
      if OutFile = Invalid_Handle then
      begin
        Result := -__ERR_DS_NoOutFile; // might be read-only or source
        File_Close(InFile);
        Exit;
      end;
      File_Close(OutFile);
      _Z_EraseFile(OutFileName, HowToDelete = htdFinal);
    end;
    OutFile := _Z_FileCreate(OutFileName);
    if OutFile <> Invalid_Handle then
    begin
      Result := CopyBuffer(InFile, OutFile, -1);
      if (Result = 0) and (FileSetDate(OutFile, FileGetDate(InFile)) <> 0) then
        Result := -__ERR_DS_SetDateError;
      Out_Size := FileSeek(OutFile, Int64(0), soFromEnd);
      File_Close(OutFile);
    end
    else
      Result := __ERR_DS_NoOutFile1;
    In_Size := FileSeek(InFile, Int64(0), soFromEnd);
    File_Close(InFile);
  end;
  // An extra check if the filesizes are the same.
  if (Result = 0) and ((In_Size = -1) or (Out_Size = -1) or
    (In_Size <> Out_Size)) then
    Result := -__ERR_DS_UnknownError;
  // Don't leave a corrupted outfile lying around. (SetDateError is not fatal!)
  if (Result <> 0) and (((-Result) and MSG_ID_MASK) <> DS_SetDateError) then
    _Z_DeleteFile(OutFileName);
end;

function TCustomZipMaster.Deflate(OutStream, InStream: TStream; Length: Int64;
  var Method: TZMDeflates; var CRC: Cardinal): Integer;
var
  DllWorker: TZMDLLOpr;
begin
  if Permitted then
    try
      Start;
      DllWorker := TZMDLLOpr.Create(Self);
      ErrCode := DllWorker.Deflate(OutStream, InStream, Length, Method, CRC);
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
  Result := ErrCode;
end;

function TCustomZipMaster.Delete: Integer;
var
  CopyWorker: TZMZippedOpr;
begin
  if Permitted then
    try
      Start;
      CopyWorker := TZMZippedOpr.Create(Self);
      CopyWorker.Delete;
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
  Result := ErrCode;
end;

procedure TCustomZipMaster.DoDelays;
var
  delay: Integer;
begin
  if Permitted then
    try
      Start;
      delay := fDelaying;
      fDelaying := 0;
      if (delay and (DelayingLanguage or DelayingLanguageID)) <> 0 then
      begin
        if (delay and DelayingLanguage) <> 0 then
          SetZipMsgLanguage(FLanguage)//;
        else
          SetZipMsgLanguageID(FLanguageID);
      end;
      if (delay and DelayingFileName) <> 0 then
      begin
        Set_Lister(TZMLister.Create(Self)); // create new
        TheLister(fLister).Set_ZipFileName(fInternal.fZipFileName, zloFull);
      end;
      if (ErrCode = 0) and ((delay and DelayingComment) <> 0) then
        Set_ZipCommentDelayed;
      if (ErrCode = 0) and ((delay and DelayingDLL) <> 0) then
      begin
        DLL_LoadDelayed;
      end;
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
end;

procedure TCustomZipMaster.done(Good: boolean = True);
begin
  Finish(Good);
end;

procedure TCustomZipMaster.DoneBad(E: Exception);
const
  __ERR_DS_UnknownError = __UNIT__ + (1673 shl 10) + DS_UnknownError;
begin
  done(False);
  Pipes.Clear;
  if E is EZMException then // Catch all Zip specific errors.
    ShowExceptionError(EZMException(E))
  else
    ShowZipMessage(__ERR_DS_UnknownError, E.Message);
  // the error ErrMessage of an unknown error is displayed ...
end;

function TCustomZipMaster.EraseFile(const FName: string;
  How: TZMDeleteOpts): Integer;
begin
  Result := _Z_EraseFile(FName, How = htdFinal);
end;

function TCustomZipMaster.Extract: Integer;
var
  DllWorker: TZMDLLOpr;
begin
  if Permitted then
    try
      Start;
      DllWorker := TZMDLLOpr.Create(Self);
      DllWorker.Extract;
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
  Result := ErrCode;
end;

function TCustomZipMaster.ExtractFileToStream(const FileName: string)
  : TMemoryStream;
var
  DllWorker: TZMDLLOpr;
begin
  Result := nil;
  if Permitted then
    try
      Start;
      DllWorker := TZMDLLOpr.Create(Self);
      DllWorker.ExtractFileToStream(FileName);
      if SuccessCnt = 1 then
        Result := ZipStream;
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
end;

function TCustomZipMaster.ExtractStreamToStream(InStream: TMemoryStream;
  OutSize: Longword): TMemoryStream;
var
  DllWorker: TZMDLLOpr;
begin
  Result := nil;
  if Permitted then
    try
      Start;
      DllWorker := TZMDLLOpr.Create(Self);
      DllWorker.ExtractStreamToStream(InStream, OutSize);
      if SuccessCnt = 1 then
        Result := ZipStream;
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
end;

function TCustomZipMaster.Find(const fspec: TZMString; var idx: Integer)
  : TZMDirEntry;
var
  c: Integer;
begin
  if idx < 0 then
    idx := -1;
  c := pred(Count);
  while idx < c do
  begin
    Inc(idx);
    Result := GetDirEntry(idx);
    if FileNameMatch(fspec, Result.FileName) then
      Exit;
  end;
  idx := -1;
  Result := nil;
end;

function TCustomZipMaster.FindSniffer: Cardinal;
var
  flgs: Cardinal;
  Noise: Integer;
  res: Integer;
begin
  Result := FindWindow(PChar(STZipSniffer), PChar(SZipMasterSniffer));
  if Result <> 0 then
  begin
    if Verbose then
      Noise := 1
    else
      Noise := 0;
    if Trace then
      Noise := Noise or 2;
    res := SendMessage(Result, WM_SNIFF_START, Longint(Handle), Noise);
    if res < 0 then
    begin
      Result := 0; // invalid
      Exit;
    end;
    // in range so hopefully valid response
    flgs := Cardinal(res) shr 24;
    if flgs >= 8 then
    begin
      Result := 0; // invalid
      Exit;
    end;
    // treat it as valid
    if flgs > 3 then
    begin
      // force it
      fInternal.fVerbosity := TZMVerbosity(flgs and 3);
    end;
    fSniffNo := res and SNIFF_MASK; // operation number
  end;
end;

procedure TCustomZipMaster.Finish(WasGood: boolean);
begin
  // tell the 'workers' we have finished
  if fActiveWorker <> nil then
  begin
    fActiveWorker.Finished(WasGood);
    FreeAndNil(fActiveWorker);
  end;
  if fLister <> nil then
    fLister.Finished(WasGood);
  if WasGood then
  begin
    // restore the 'locked' spec lists
    FSpecArgs.Assign(fInternal.fIncludeSpecs);
    FSpecArgsExcl.Assign(fInternal.fExcludeSpecs);
  end;
  Dec(BusyFlag);
  if Trace then
    ShowMsg('Trace: done = ' + IntToStr(BusyFlag), 0, False);
  fSniffer := 0;
  SetZipBusy(False); // unlock language and UsingUTF8
  if BusyFlag = 0 then
  begin
    if State <> zsReentry then
      StateChanged(zsIdle)
    else
      StateChanged(zsReentered);
    // Are we waiting to go inactive?
    if fActive < 0 then
    begin
      fActive := 0;
      StateChanged(zsDisabled);
    end;
  end;
end;

function TCustomZipMaster.ForEach(func: TZMForEachFunction; var Data)
  : Integer;
var
  lister: TZMLister;
begin
  Result := 0;
  if Permitted then
    try
      Start;
      lister := fLister as TZMLister;
      Result := lister.ForEach(@func, Data);
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
end;

function TCustomZipMaster.FullVersionString: string;
begin
  Result := 'ZipMaster ' + Version;
  Result := Result + ', DLL ' + GetDLL_Version1(True);
end;

function TCustomZipMaster.GetActive: boolean;
begin
  Result := fActive <> 0;
end;

function TCustomZipMaster.GetAddPassword: string;
var
  Resp: TMsgDlgBtn;
begin
  Result := _GetAddPassword(Resp);
  if State < zsBusy then
    Password := Result;
end;

function TCustomZipMaster.GetAddPassword(var Response: TMsgDlgBtn): string;
begin
  Result := _GetAddPassword(Response);
  if State < zsBusy then
    Password := Result;
end;

function TCustomZipMaster.GetBuild: Integer;
begin
  Result := ZIPMASTERPRIV;
end;

function TCustomZipMaster.GetCancel: boolean;
begin
  Result := fInternal.fCancel <> 0;
end;

function TCustomZipMaster.GetCount: Integer;
begin
  if IsActive and (fLister <> nil) then
    Result := TheLister(fLister).CentralDir.Count
  else
    Result := 0;
end;

function TCustomZipMaster.GetDirEntry(idx: Integer): TZMDirEntry;
begin
  if IsActive and (fLister <> nil) then
    Result := TheLister(fLister).CentralDir[idx]
  else
    Result := nil;
end;

function TCustomZipMaster.GetDirOnlyCnt: Integer;
begin
  if IsActive and (fLister <> nil) then
    Result := TheLister(fLister).CentralDir.DirOnlyCount
  else
    Result := 0;
end;

function TCustomZipMaster.GetDLL_Build: Integer;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Result := 0
  else
    Result := _DLL_Build;
end;

function TCustomZipMaster.GetDLL_Load: boolean;
begin
  if not((csDesigning in ComponentState) or (csLoading in ComponentState)) then
    fInternal.fDLLLoad := _DLL_Loaded(Self);
  Result := fInternal.fDLLLoad
end;

function TCustomZipMaster.GetDLL_Path: string;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Result := ''
  else
    Result := _DLL_Path;
end;

function TCustomZipMaster.GetDLL_Version: string;
begin
  Result := GetDLL_Version1(False);
end;

function TCustomZipMaster.GetDLL_Version1(ForceLoad: Boolean): string;
var
  Worker: TZMCore;
  Ver: Integer;
begin
  Result := '';
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Exit;
  Ver := _DLL_Build;
  if (Ver < (DELZIPVERSION * 1000)) and ForceLoad then
  begin
    if fActiveWorker = nil then
    begin
        Worker := TZMCore.Create(Self, nil);
        try
          if Permitted then
          begin
            try
              Start;
              Ver := Dll_ForcedBuild(Worker);
              done;
            except
              on E: Exception do
                DoneBad(E);
            end;
          end;
        finally
          Worker.Free;
        end;
    end
    else
      Ver := Dll_ForcedBuild(fActiveWorker);
  end;
  if Ver <= 0 then
    Result := '?.?.?.????'
  else
    Result := VersStr(Ver, False);
end;

function TCustomZipMaster.GetErrCode: Integer;
begin
  Result := fInternal.fErrCode;
  if not IsActive then
    Result := GE_Inactive;
end;

function TCustomZipMaster.GetErrMessage: TZMString;
const
  __ERR_GE_Inactive = __UNIT__ + (1993 shl 10) + GE_Inactive;
begin
  if IsActive then
    Result := fInternal.fErrMessage
  else
    Result := ZipLoadStr(__ERR_GE_Inactive);
end;

function TCustomZipMaster.GetExtrPassword: string;
var
  Resp: TMsgDlgBtn;
begin
  Result := _GetExtrPassword(Resp);
  if State < zsBusy then
    Password := Result;
end;

function TCustomZipMaster.GetExtrPassword(var Response: TMsgDlgBtn): string;
begin
  Result := _GetExtrPassword(Response);
  if State < zsBusy then
    Password := Result;
end;

function TCustomZipMaster.GetIsSpanned: boolean;
begin
  if IsActive and (fLister <> nil) then
    Result := TheLister(fLister).CentralDir.MultiDisk
  else
    Result := False;
end;

function TCustomZipMaster.GetLanguage: string;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Result := FLanguage
  else
    Result := GetZipMsgLanguage(0);
end;

{class} function TCustomZipMaster.GetLanguageInfo(idx: Integer;
  info: Cardinal): string;
begin
  Result := GetZipMsgLanguageInfo(idx, info);
end;

function TCustomZipMaster.GetNoReadAux: boolean;
begin
  Result := fInternal.FNoReadAux;
  if not((csDesigning in ComponentState) or (csLoading in ComponentState)) then
    Result := Result or fInternal.FAuxChanged;
end;

function TCustomZipMaster.GetOnLoadStr: TZMLoadStrEvent;
begin
  Result := OnZMStr;
end;

function TCustomZipMaster.GetPassword(const DialogCaption, MsgTxt: string;
  pwb: TMsgDlgButtons; var ResultStr: string): TMsgDlgBtn;
begin
  Result := _GetPassword(DialogCaption, MsgTxt, DHC_Password, pwb, ResultStr);
end;

function TCustomZipMaster.GetSFXOffset: Integer;
begin
  if IsActive and (fLister <> nil) then
    Result := TheLister(fLister).CentralDir.SFXOffset
  else
    Result := 0;
end;

function TCustomZipMaster.GetSuccessCnt: Integer;
begin
  Result := fInternal.fSuccessCnt;
end;

function TCustomZipMaster.GetTotalSizeToProcess: Int64;
begin
  Result := fInternal.fTotalSizeToProcess;
end;

{$IFNDEF _NO_UTF8_}
function TCustomZipMaster.GetUseUTF8: boolean;
begin
  Result := UsingUTF8;
end;
{$ENDIF}

function TCustomZipMaster.GetVersion: string;
begin
  Result := ZIPMASTERBUILD;
end;

function TCustomZipMaster.GetZipEOC: Int64;
begin
  if IsActive and (fLister <> nil) then
    Result := TheLister(fLister).CentralDir.EOCOffset
  else
    Result := 0;
end;

function TCustomZipMaster.GetZipFileSize: Int64;
begin
  if IsActive and (fLister <> nil) then
    Result := TheLister(fLister).CentralDir.ZipFileSize
  else
    Result := 0;
end;

function TCustomZipMaster.GetZipSOC: Int64;
begin
  if IsActive and (fLister <> nil) then
    Result := TheLister(fLister).CentralDir.SOCOffset
  else
    Result := 0;
end;

function TCustomZipMaster.GetZipStream: TMemoryStream;
begin
  if fInternal.fZipStream = nil then
    fInternal.fZipStream := TMemoryStream.Create;
  Result := fInternal.fZipStream;
end;

function TCustomZipMaster.IndexOf(const FName: TZMString): Integer;
var
  fn: TZMString;
begin
  fn := FName;
  for Result := 0 to pred(Count) do
    if FileNameMatch(fn, GetDirEntry(Result).FileName) then
      Exit;
  Result := -1;
end;

function TCustomZipMaster.IsActive: boolean;
begin
  Result := (fActive <> 0);
  if Result and ((csDesigning in ComponentState) or
    (csLoading in ComponentState)) then
    Result := False; // never Active while loading or designing
end;

function TCustomZipMaster.IsZipSFX(const SFXExeName: string): Integer;
begin
  Result := ZMUtils.IsZipSFX(SFXExeName);
end;

function TCustomZipMaster.KeepAlive: boolean;
var
  DoStop: boolean;
  tmpCheckTerminate: TZMCheckTerminateEvent;
  tmpTick: TZMTickEvent;
begin
  Result := fInternal.fCancel <> 0;
  tmpTick := OnTick;
  if assigned(tmpTick) then
    tmpTick(Self);
  tmpCheckTerminate := OnCheckTerminate;
  if assigned(tmpCheckTerminate) then
  begin
    DoStop := fInternal.fCancel <> 0;
    tmpCheckTerminate(Self, DoStop);
    if DoStop then
      fInternal.fCancel := DS_Canceled;
  end
  else
    if not fInternal.fNotMainThread then
      Application.ProcessMessages;
end;

function TCustomZipMaster.List: Integer;
var
  lister: TZMLister;
begin
  if Permitted then
    try
      Start;
      if fLister = nil then
      begin
        lister := TZMLister.Create(Self);
        Set_Lister(lister);
      end;
      TheLister(fLister).List;
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
  Result := ErrCode;
end;

procedure TCustomZipMaster.Loaded;
begin
  inherited;
  if IsActive then
    DoDelays;
end;

(* ? MakeTempFileName
  Make a temporary filename like: C:\...\zipxxxx.zip
  Prefix and extension are default: 'zip' and '.zip'
*)
function TCustomZipMaster.MakeTempFileName(const Prefix,
  Extension: string): string;
var
  buf: string;
  ext: string;
  len: Dword;
  pre: string;
  tmpDir: string;
begin
  Result := '';
  if Prefix = '' then
    pre := 'zip'
  else
    pre := Prefix;
  if Extension = '' then
    ext := EXT_ZIPL
  else
    ext := Extension;
  if Length(TempDir) = 0 then // Get the system temp dir
  begin
    // 1. The path specified by the TMP environment variable.
    // 2. The path specified by the TEMP environment variable, if TMP is not defined.
    // 3. The current directory, if both TMP and TEMP are not defined.
    len := GetTempPath(0, PChar(tmpDir));
    SetLength(tmpDir, len);
    GetTempPath(len, PChar(tmpDir));
  end
  else // Use Temp dir provided by ZipMaster
    tmpDir := DelimitPath(fInternal.fTempDir, True);
  SetLength(buf, MAX_PATH + 12);
  if GetTempFileName(PChar(tmpDir), PChar(pre), 0, PChar(buf)) <> 0 then
  begin
    buf := PChar(buf);
    SysUtils.DeleteFile(buf);
    // Needed because GetTempFileName creates the file also.
    Result := ChangeFileExt(buf, ext);
    // And finally change the extension.
  end;
end;


function TCustomZipMaster.MergeZippedFiles(Opts: TZMMergeOpts): Integer;
var
  CopyWorker: TZMZippedOpr;
begin
  if Permitted then
    try
      Start;
      CopyWorker := TZMZippedOpr.Create(Self);
      CopyWorker.MergeZippedFiles(Opts, nil);
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
  Result := ErrCode;
end;

// if Permitted returns true Finish or Done or DoneBad must be called
function TCustomZipMaster.Permitted: boolean;
begin
  Result := False;
  if IsActive then
  begin
    Inc(BusyFlag);
    if BusyFlag <> 1 then
    begin
      Dec(BusyFlag);
      ReEntered;
    end
    else
      Result := True;
  end;
  if Result then
    StateChanged(zsBusy);
end;

function TCustomZipMaster.QueryZip(const FName: TFileName): Integer;
begin
  Result := ZMUtils.QueryZip(FName);
end;

function TCustomZipMaster.ReadSpan(const InFileName: string;
  var OutFilePath: string): Integer;
var
  FileWorker: TZMFileOpr;
begin
  Result := 0;
  if Permitted then
    try
      Start;
      FileWorker := TZMFileOpr.Create(Self);
      Result := FileWorker.ReadSpan(InFileName, OutFilePath, False);
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
end;

procedure TCustomZipMaster.ReEntered;
begin
  Inc(fBlockedCnt);
  StateChanged(zsReentry);
  if Verbose then
    ShowMsg('Trace: Re-entry', 0, False);
end;

function TCustomZipMaster.ReEntry: boolean;
begin
  Result := State >= zsBusy;
  if Result then
    ReEntered;
end;

function TCustomZipMaster.Rename(RenameList: TList; DateTime: Integer;
  How: TZMRenameOpts = htrDefault): Integer;
var
  ModWorker: TZMModOpr;
begin
  if Permitted then
    try
      Start;
      ModWorker := TZMModOpr.Create(Self);
      ModWorker.Rename(RenameList, DateTime, How);
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
  Result := ErrCode;
end;

// return proper ErrCode for dll error
function DllToErrCode(DLL_error: Cardinal): Integer;
begin
  Result := DLL_error and 255;
  if Result <> 0 then
    Result := DZ_RES_GOOD + Result;
  if Result > DZ_ERR_DUPNAME then
    Result := DZ_RES_ERROR;
end;

function TCustomZipMaster.AddZippedFiles(SrcZipMaster: TCustomZipMaster;
    merge: TZMMergeOpts): Integer;
const
  __ERR_GE_NoZipSpecified = __UNIT__ + (2351 shl 10) + GE_NoZipSpecified;
  __ERR_GE_WasBusy = __UNIT__ + (2368 shl 10) + GE_WasBusy;
var
  CopyWorker: TZMZippedOpr;
begin
  if Permitted then
    try
      Start;
      if (not assigned(SrcZipMaster)) or (SrcZipMaster.ZipFileName = '') then
        raise EZipMaster.CreateMsgDisp(__ERR_GE_NoZipSpecified, True);
      if SrcZipMaster.Permitted then
      begin
        CopyWorker := TZMZippedOpr.Create(Self);
        try
          SrcZipMaster.Start;
          CopyWorker.AddZippedFiles(SrcZipMaster.fLister as TZMLister, merge);
          SrcZipMaster.done;
        except
          on E: Exception do
          begin
            SrcZipMaster.DoneBad(E);
            raise EZipMaster.CreateMsgDisp(SrcZipMaster.ErrCode, False);
          end;
        end;
      end
      else
        raise EZipMaster.CreateMsgStr(__ERR_GE_WasBusy, 'Source');
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
  Result := ErrCode;
end;

procedure TCustomZipMaster.ClearCachedNames_Action(const data: Pointer);
begin
  TheLister(fLister).ClearCachedNames;
end;

function TCustomZipMaster.CopyZippedFiles(DestZipMaster: TCustomZipMaster;
    DeleteFromSource: boolean; OverwriteDest: TZMMergeOpts): Integer;
var
  CopyWorker: TZMZippedOpr;
begin
  if (not assigned(DestZipMaster)) or (DestZipMaster.ZipFileName = '') then
  begin
    Result := CF_NoDest;
    ShowZipMessage(Result, '');
    Exit;
  end;
  // destination must not be busy and must not be allowed to become busy
  if DestZipMaster.Permitted then
  begin
    try
      DestZipMaster.Start; // lock it
      if Permitted then
        try
          Start;
          CopyWorker := TZMZippedOpr.Create(Self);
          CopyWorker.CopyZippedFiles(DestZipMaster.fLister as TZMLister,
            DeleteFromSource, OverwriteDest);
          done;
        except
          on E: Exception do
          begin
            DoneBad(E);
          end;
        end;
      DestZipMaster.done; // release it
    except
      on E: Exception do
        DestZipMaster.DoneBad(E);
    end;
    Result := ErrCode;
  end
  else
  begin
    Result := GE_WasBusy;
    ShowZipFmtMessage(Result, [DestZipMaster.ZipFileName], True);
  end;
end;

function TCustomZipMaster.Dll_ForcedBuild(Worker: TZMWorkBase): integer;
begin
  _DLL_Load(Worker);
  Result := _DLL_Build;
  _DLL_Unload(Worker);
end;

procedure TCustomZipMaster.DLL_LoadDelayed;
begin
  DoDLL_Load(fInternal.fDLLLoad);
end;

function TCustomZipMaster.DoDLL_Load(const Value: boolean): boolean;
var
  DllWorker: TZMDLLOpr;
begin
  Result := False;
    if Permitted then
    begin
      Result := True;
      try
        Start;
        DllWorker := TZMDLLOpr.Create(Self);
        if DllWorker.DLL_Load <> Value then
          DllWorker.DLL_Load := Value;
        fInternal.fDLLLoad := DllWorker.DLL_Load; // true if it loaded
        done;
      except
        on E: Exception do
          DoneBad(E);
      end
    end;
end;

function TCustomZipMaster.DoSetZipComment(const Value: AnsiString): Boolean;
const
  __ERR_GE_NoProcess = __UNIT__ + (2476 shl 10) + GE_NoProcess;
var
  ModWorker: TZMModOpr;
  lister: TZMLister;
begin
  Result := False;
  if Permitted then
  begin
    Result := True;
    try
      Start;
      if (fLister <> nil) and (IsSpanned or FileExists(ZipFileName)) then
      begin
        lister := TheLister(fLister);
        if not lister.CurrentIsValid then
          raise EZMException.CreateMsgDisp(__ERR_GE_NoProcess, False);
        if lister.ZipComment <> Value then
        begin
          ModWorker := TZMModOpr.Create(Self);
          ModWorker.Set_ZipComment(Value);
        end;
        fInternal.fZipComment := lister.ZipComment;
      end
      else
        fInternal.fZipComment := Value;
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end
  end;
end;

function TCustomZipMaster.GetAddCompLevel: Integer;
begin
    Result := fInternal.fAddCompLevel;
end;

function TCustomZipMaster.GetAddFrom: TDateTime;
begin
    Result := fInternal.fAddFrom;
end;

function TCustomZipMaster.GetAddOptions: TZMAddOpts;
begin
    Result := fInternal.fAddOptions;
end;

function TCustomZipMaster.GetAddStoreSuffixes: TZMAddStoreExts;
begin
    Result := fInternal.fAddStoreSuffixes;
end;

function TCustomZipMaster.GetConfirmErase: boolean;
begin
    Result := fInternal.fConfirmErase;
end;

function TCustomZipMaster.GetDLLDirectory: string;
begin
    Result := fInternal.fDLLDirectory;
end;

function TCustomZipMaster.GetEncodeAs: TZMEncodingOpts;
begin
    Result := fInternal.fEncodeAs;
end;

function TCustomZipMaster.GetEncoding: TZMEncodingOpts;
begin
    Result := fInternal.fEncoding;
end;

function TCustomZipMaster.GetEncoding_CP: Cardinal;
begin
    Result := fInternal.fEncoding_CP;
end;

function TCustomZipMaster.GetExtAddStoreSuffixes: string;
begin
    Result := fInternal.fExtAddStoreSuffixes;
end;

function TCustomZipMaster.GetExtErrCode: cardinal;
begin
  Result := fInternal.fExtErrCode;
end;

function TCustomZipMaster.GetExtrBaseDir: string;
begin
  Result := fInternal.fExtrBaseDir;
end;

function TCustomZipMaster.GetHandle: HWND;
begin
    Result := fInternal.fHandle;
end;

function TCustomZipMaster.GetHowToDelete: TZMDeleteOpts;
begin
    Result := fInternal.fHowToDelete;
end;

function TCustomZipMaster.GetKeepFreeOnAllDisks: Cardinal;
begin
  Result := fInternal.fKeepFreeOnAllDisks;
end;

function TCustomZipMaster.GetKeepFreeOnDisk1: Cardinal;
begin
  Result := fInternal.fKeepFreeOnDisk1;
end;

function TCustomZipMaster.GetLanguageID: cardinal;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    Result := FLanguageID
  else
    Result := GetZipMsgLanguageID;
end;

function TCustomZipMaster.GetMaxVolumeSize: Int64;
begin
  Result := fInternal.fMaxVolumeSize;
end;

function TCustomZipMaster.GetMaxVolumeSizeKb: Integer;
begin
  Result := fInternal.fMaxVolumeSize div 1024;
end;

function TCustomZipMaster.GetMinFreeVolumeSize: Cardinal;
begin
  Result := fInternal.fMinFreeVolumeSize;
end;

function TCustomZipMaster.GetNoSkipping: TZMSkipAborts;
begin
    Result := fInternal.fNoSkipping;
end;

function TCustomZipMaster.GetNotMainThread: boolean;
begin
    Result := fInternal.fNotMainThread;
end;

function TCustomZipMaster.Get_Password: String;
begin
    Result := fInternal.fPassword;
end;

function TCustomZipMaster.GetPasswordReqCount: Longword;
begin
    Result := fInternal.fPasswordReqCount;
end;

function TCustomZipMaster.GetRootDir: string;
begin
    Result := fInternal.fRootDir;
end;

function TCustomZipMaster.GetSFXCaption: TZMString;
begin
    Result := fInternal.fSFXCaption;
end;

function TCustomZipMaster.GetSFXCommandLine: TZMString;
begin
    Result := fInternal.fSFXCommandLine;
end;

function TCustomZipMaster.GetSFXDefaultDir: string;
begin
    Result := fInternal.fSFXDefaultDir;
end;

function TCustomZipMaster.GetSFXIcon: TIcon;
begin
    Result := fInternal.fSFXIcon;
end;

function TCustomZipMaster.GetSFXMessage: TZMString;
begin
    Result := fInternal.fSFXMessage;
end;

function TCustomZipMaster.GetSFXOptions: TZMSFXOpts;
begin
    Result := fInternal.fSFXOptions;
end;

function TCustomZipMaster.GetSFXOverwriteMode: TZMOvrOpts;
begin
    Result := fInternal.fSFXOverwriteMode;
end;

function TCustomZipMaster.GetSFXPath: string;
begin
    Result := fInternal.fSFXPath;
end;

function TCustomZipMaster.GetSFXRegFailPath: string;
begin
    Result := fInternal.fSFXRegFailPath;
end;

function TCustomZipMaster.GetSpanOptions: TZMSpanOpts;
begin
    Result := fInternal.fSpanOptions;
end;

function TCustomZipMaster.GetTempDir: string;
begin
    Result := fInternal.fTempDir;
end;

function TCustomZipMaster.GetTrace: boolean;
begin
  Result := (Ord(fInternal.fVerbosity) and 2) <> 0;
end;

function TCustomZipMaster.GetUnattended: boolean;
begin
    Result := fInternal.fUnattended;
end;

function TCustomZipMaster.GetUseDirOnlyEntries: boolean;
begin
    Result := fInternal.fUseDirOnlyEntries;
end;

function TCustomZipMaster.GetVerbose: boolean;
begin
  Result := (Ord(fInternal.fVerbosity) and 1) <> 0;
end;

function TCustomZipMaster.GetWriteOptions: TZMWriteOpts;
begin
    Result := fInternal.fWriteOptions;
end;

function TCustomZipMaster.GetZipComment: AnsiString;
begin
    Result := fInternal.fZipComment;
end;

function TCustomZipMaster.GetZipFileName: string;
begin
    Result := fInternal.fZipFileName;
end;

// render an action when permitted
procedure TCustomZipMaster.Render(Action: TZMRenderMethod;
  const Data: Pointer);
begin
  if Permitted then
    try
      Action(Data);
      Finish(True);
    except
      Finish(False);
    end;
end;

procedure TCustomZipMaster.ReportMessage(err: Integer; const msg: TZMString;
    NoLock: boolean);
var
  ECode: Integer;
  tmpMessage: TZMMessageEvent;
begin
  if err < 0 then
    err := - err;
  ECode := err and $1F;
  if fSniffer <> 0 then
    ReportToSniffer(err, msg);
  if NoLock and (ECode <> 0) and (ErrCode = 0) then // only catch first
  begin
    fInternal.fErrCode := ECode;
    fInternal.fExtErrCode := err;
    fInternal.fErrMessage := msg;
  end;
  tmpMessage := OnMessage;
  if assigned(tmpMessage) then
    tmpMessage(Self, ECode, msg);
  KeepAlive; // process messages or check terminate
end;

function TCustomZipMaster.ReportSkipping(const FName: string; err: Integer;
  typ: TZMSkipTypes): boolean;
var
  ti: Integer;
  tmpMessage: TZMMessageEvent;
  tmpSkipped: TZMSkippedEvent;
begin
  Result := False;
  if typ in NoSkipping then
  begin
    if err = 0 then
      err := GE_NoSkipping;
  end;
  ti := err;
  if ti < 0 then
    ti := -ti;
  if (ti <> 0) and (typ in NoSkipping) then
    ti := -ti; // default to abort
  tmpSkipped := OnSkipped;
  if assigned(tmpSkipped) then
    tmpSkipped(Self, FName, typ, ti)
  else
    if Verbose or Trace then
    begin
      tmpMessage := OnMessage;
      if assigned(tmpMessage) then
        tmpMessage(Self, GE_Unknown, ZipFmtLoadStr(GE_Skipped,
          [FName, Ord(typ)]));
    end;
  if ti < 0 then
    Result := True; // Skipping not allowed
  if fSniffer <> 0 then
    ReportToSniffer(0, Format('[Skipped] IN=%d,%d OUT=%d',
      [err, Ord(typ), Ord(Result)]));
end;

procedure TCustomZipMaster.ReportToSniffer(err: Cardinal;
  const msg: TZMString);
var
  aCopyData: TCopyDataStruct;
  msg8: UTF8String;
begin
  if fSniffer = 0 then // should not happen
    Exit;
  // always feed Sniffer with UTF8
{$IFDEF UNICODE}
  msg8 := StrToUTF8(msg);
{$ELSE}
  if UsingUTF8 then
    msg8 := msg
  else
    msg8 := StrToUTF8(msg);
{$ENDIF}
  aCopyData.dwData := Cardinal(err);
  aCopyData.cbData := (Length(msg8) + 1) * sizeof(AnsiChar);
  aCopyData.lpData := @msg8[1];
  if SendMessage(fSniffer, WM_COPYDATA, fSniffNo, Longint(@aCopyData)) = 0 then
    fSniffer := 0; // could not process it -don't try again
end;

(* TCustomZipMaster19.SetActive
  sets the following values
  0 - not active
  1 - active
  -1 - active in design/loading state (no Active functions allowed)
*)
procedure TCustomZipMaster.SetActive(Value: boolean);
var
  was: Integer;
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
  begin
    if Value then
      fActive := 1 // set but ignored
    else
      fActive := 0;
    Exit;
  end;
  if Value <> (fActive > 0) then
  begin
    was := fActive;
    if Value then
    begin
      fActive := 1;
      // reject change active to inactive to active while busy
      if was = 0 then
      begin
        // changed to 'active'
        StateChanged(zsIdle);
        if (fDelaying <> 0) and (BusyFlag = 0) then
          DoDelays;
      end;
    end
    else
    begin
      if BusyFlag <> 0 then
        fActive := -3 // clear when 'done'
      else
      begin
        fActive := 0; // now inactive
        StateChanged(zsDisabled);
      end;
    end;
  end;
end;

procedure TCustomZipMaster.SetAddCompLevel(const Value: Integer);
begin
  if not ReEntry then
    fInternal.fAddCompLevel := Value;
end;

procedure TCustomZipMaster.SetAddFrom(const Value: TDateTime);
begin
  if not ReEntry then
    fInternal.fAddFrom := Value;
end;

procedure TCustomZipMaster.SetAddOptions(const Value: TZMAddOpts);
begin
  if not ReEntry then
    fInternal.fAddOptions := Value;
end;

procedure TCustomZipMaster.SetAddStoreSuffixes(const Value: TZMAddStoreExts);
begin
  if not ReEntry then
    fInternal.fAddStoreSuffixes := Value;
end;

procedure TCustomZipMaster.SetCancel(Value: boolean);
begin
  if Value <> Cancel then
  begin
    if not Cancel then
    begin
      if fActiveWorker <> nil then
        fActiveWorker.CancelSet(DS_Canceled);
      if fLister <> nil then
        fLister.CancelSet(DS_Canceled);
      fInternal.fCancel := DS_Canceled;
    end
    else
      fInternal.fCancel := 0;
  end;
end;

procedure TCustomZipMaster.SetConfirmErase(const Value: boolean);
begin
  if not ReEntry then
    fInternal.fConfirmErase := Value;
end;

procedure TCustomZipMaster.SetDLLDirectory(const Value: string);
begin
  if not ReEntry then
    fInternal.fDLLDirectory := Value;
end;

procedure TCustomZipMaster.SetDLL_Load(const Value: boolean);
begin
  if Value <> fInternal.fDLLLoad then
  begin
    if not DoDLL_Load(Value) and not IsActive then // not Active
    begin
      fInternal.fDLLLoad := Value;
      fDelaying := fDelaying or DelayingDLL; // delay until Active
    end;
  end;
end;

procedure TCustomZipMaster.SetEncodeAs(const Value: TZMEncodingOpts);
begin
  if fInternal.FEncodeAs <> Value then
  begin
    if not ReEntry then
      fInternal.FEncodeAs := Value;
  end;
end;

procedure TCustomZipMaster.SetEncoding(const Value: TZMEncodingOpts);
begin
  if fInternal.fEncoding <> Value then
  begin
    fInternal.fEncoding := Value;
    if fLister <> nil then
      Render(ClearCachedNames_Action, nil);
  end;
end;

procedure TCustomZipMaster.SetEncoding_CP(Value: Cardinal);
var
  info: TCPInfo;
begin
  if not GetCPInfo(Value, info) then
    Value := 0;
  if fInternal.fEncoding_CP <> Value then
  begin
    fInternal.fEncoding_CP := Value;
    if fLister <> nil then
      Render(ClearCachedNames_Action, nil);
  end;
end;

procedure TCustomZipMaster.SetErrCode(Value: Integer);
begin
  if not ReEntry then
    fInternal.fErrCode := Value;
end;

procedure TCustomZipMaster.SetExtAddStoreSuffixes(const Value: string);
var
  c: Char;
  i: Integer;
  tempStr: string;
begin
  if not ReEntry then
  begin
    if Value <> '' then
    begin
      c := ':';
      i := 1;
      while i <= Length(Value) do
      begin
        c := Value[i];
        if c <> '.' then
          tempStr := tempStr + '.';
        while (c <> ':') and (i <= Length(Value)) do
        begin
          c := Value[i];
          if (c = ';') or (c = ':') or (c = ',') then
            c := ':';
          tempStr := tempStr + c;
          Inc(i);
        end;
      end;
      if c <> ':' then
        tempStr := tempStr + ':';
      fInternal.fAddStoreSuffixes := fInternal.fAddStoreSuffixes + [assEXT];
      fInternal.fExtAddStoreSuffixes := Lowercase(tempStr);
    end
    else
    begin
      fInternal.fAddStoreSuffixes := fInternal.fAddStoreSuffixes - [assEXT];
      fInternal.fExtAddStoreSuffixes := '';
    end;
  end;
end;

procedure TCustomZipMaster.SetExtErrCode(const Value: cardinal);
begin
  if not ReEntry then
    fInternal.fExtErrCode := Value;
end;

procedure TCustomZipMaster.SetExtrBaseDir(const Value: string);
begin
  if not ReEntry then
    fInternal.fExtrBaseDir := Value;
end;

procedure TCustomZipMaster.SetExtrOptions(const Value: TZMExtrOpts);
begin
  if not ReEntry then
    fInternal.fExtrOptions := Value;
end;

procedure TCustomZipMaster.SetFSpecArgs(const Value: TStrings);
begin
  if not ReEntry then
  begin
    if Value <> FFSpecArgs then
      FFSpecArgs.Assign(Value);
  end;
end;

procedure TCustomZipMaster.SetFSpecArgsExcl(const Value: TStrings);
begin
  if not ReEntry then
  begin
    if Value <> fFSpecArgsExcl then
      fFSpecArgsExcl.Assign(Value);
  end;
end;

procedure TCustomZipMaster.SetHandle(const Value: HWND);
begin
  //
end;

procedure TCustomZipMaster.SetHowToDelete(const Value: TZMDeleteOpts);
begin
  if not ReEntry then
    fInternal.fHowToDelete := Value;
end;

procedure TCustomZipMaster.SetKeepFreeOnAllDisks(const Value: Cardinal);
begin
  if not ReEntry then
    fInternal.fKeepFreeOnAllDisks := Value;
end;

procedure TCustomZipMaster.SetKeepFreeOnDisk1(const Value: Cardinal);
begin
  if not ReEntry then
    fInternal.fKeepFreeOnDisk1 := Value;
end;

procedure TCustomZipMaster.SetLanguage(const Value: string);
begin
  if Permitted then
    try
      FLanguage := Value;
      Start;
      SetZipMsgLanguage(Value);
      done;
    except
      on E: Exception do
        DoneBad(E);
    end
  else
    if not IsActive then // not Active
    begin
      FLanguage := Value;
      fDelaying := fDelaying or DelayingLanguage; // delay until Active
    end;
end;

procedure TCustomZipMaster.SetLanguageID(const Value: cardinal);
begin
  if Permitted then
    try
      FLanguageID := Value;
      Start;
      SetZipMsgLanguageID(Value);
      done;
    except
      on E: Exception do
        DoneBad(E);
    end
  else
    if not IsActive then // not Active
    begin
      FLanguageID := Value;
      fDelaying := fDelaying or DelayingLanguageID; // delay until Active
    end;
end;

procedure TCustomZipMaster.SetMaxVolumeSize(const Value: Int64);
begin
  if not ReEntry then
  begin
    if Value > 0 then
      fInternal.fMaxVolumeSize := Value
    else
      fInternal.fMaxVolumeSize := 0;
  end;
end;

procedure TCustomZipMaster.SetMaxVolumeSizeKb(const Value: Integer);
begin

  if not ReEntry then
  begin
    if Value > 0 then
      fInternal.fMaxVolumeSize := Value * 1024
    else
      fInternal.fMaxVolumeSize := 0;
  end;
end;

procedure TCustomZipMaster.SetMinFreeVolumeSize(const Value: Cardinal);
begin
  if not ReEntry then
    fInternal.fMinFreeVolumeSize := Value;
end;

procedure TCustomZipMaster.SetNoReadAux(const Value: boolean);
begin
  // must check changes in composite value
  if not ReEntry then
  begin
    if fInternal.fNoReadAux <> Value then
    begin
      fInternal.FNoReadAux := Value;
      fInternal.FAuxChanged := False; // reset
    end;
  end;
end;

procedure TCustomZipMaster.SetNoSkipping(const Value: TZMSkipAborts);
begin
  if not ReEntry then
    fInternal.FNoSkipping := Value;
end;

procedure TCustomZipMaster.SetNotMainThread(const Value: boolean);
begin
  if not ReEntry then
    fInternal.fNotMainThread := Value;
end;

procedure TCustomZipMaster.SetOnLoadStr(const Value: TZMLoadStrEvent);
begin
  OnZMStr := Value;
end;

procedure TCustomZipMaster.Set_Password(const Value: string);
begin
  if Password <> Value then
  begin
    if not ReEntry then
      fInternal.FPassword := Value;
  end;
end;

procedure TCustomZipMaster.SetPasswordReqCount(Value: Longword);
begin
  if Value > 15 then
    Value := 15;
  if Value <> PasswordReqCount then
  begin
    if not ReEntry then
      fInternal.fPasswordReqCount := Value;
  end;
end;

procedure TCustomZipMaster.SetPipes(const Value: TZMPipeList);
begin
  //
end;

procedure TCustomZipMaster.SetRootDir(const Value: string);
begin
  if not ReEntry then
    fInternal.fRootDir := Value;
end;

procedure TCustomZipMaster.SetSFXCaption(const Value: TZMString);
begin
  if not ReEntry then
  begin
    if SFXCaption <> Value then
    begin
      fInternal.FSFXCaption := Value;
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSFXCommandLine(const Value: TZMString);
begin
  if not ReEntry then
  begin
    if SFXCommandLine <> Value then
    begin
      fInternal.FSFXCommandLine := Value;
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSFXDefaultDir(const Value: string);
begin
  if not ReEntry then
  begin
    if SFXDefaultDir <> Value then
    begin
      fInternal.FSFXDefaultDir := Value;
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSFXIcon(Value: TIcon);
begin
  if not ReEntry then
  begin
    if Value <> fInternal.fSFXIcon then
    begin
      if assigned(Value) and not Value.Empty then
      begin
        if not assigned(fInternal.fSFXIcon) then
          fInternal.fSFXIcon := TIcon.Create;
        fInternal.fSFXIcon.Assign(Value);
      end
      else
        FreeAndNil(fInternal.fSFXIcon);
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSFXMessage(const Value: TZMString);
begin
  if not ReEntry then
  begin
    if SFXMessage <> Value then
    begin
      fInternal.FSFXMessage := Value;
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSFXOptions(const Value: TZMSFXOpts);
begin
  if not ReEntry then
  begin
    if SFXOptions <> Value then
    begin
      fInternal.FSFXOptions := Value;
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSFXOverwriteMode(const Value: TZMOvrOpts);
begin
  if not ReEntry then
  begin
    if SFXOverwriteMode <> Value then
    begin
      fInternal.FSFXOverwriteMode := Value;
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSFXPath(const Value: string);
begin
  if not ReEntry then
  begin
    if SFXPath <> Value then
    begin
      fInternal.FSFXPath := Value;
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSFXRegFailPath(const Value: string);
begin
  if not ReEntry then
  begin
    if SFXRegFailPath <> Value then
    begin
      fInternal.FSFXRegFailPath := Value;
      AuxWasChanged;
    end;
  end;
end;

procedure TCustomZipMaster.SetSpanOptions(const Value: TZMSpanOpts);
begin
  if not ReEntry then
  begin
    if SpanOptions <> Value then
    begin
      if (Value * [spNoVolumeName, spCompatName]) <>
        (SpanOptions * [spNoVolumeName, spCompatName]) then
        AuxWasChanged;
      fInternal.FSpanOptions := Value;
    end;
  end;
end;

procedure TCustomZipMaster.SetTempDir(const Value: string);
begin
  if not ReEntry then
    fInternal.fTempDir := DelimitPath(Value, True);
end;

procedure TCustomZipMaster.SetTrace(const Value: boolean);
var
  v: Integer;
begin
  if not ReEntry then
  begin
    if Value then
      v := 2
    else
      v := 0;
    fInternal.fVerbosity := TZMVerbosity((Ord(fInternal.fVerbosity) and 1) or v);
  end;
end;

procedure TCustomZipMaster.SetUnattended(const Value: boolean);
begin
  if not ReEntry then
    fInternal.fUnattended := Value;
end;

procedure TCustomZipMaster.SetUseDirOnlyEntries(const Value: boolean);
begin
  if Value <> UseDirOnlyEntries then
  begin
    fInternal.FUseDirOnlyEntries := Value;
    if fLister <> nil then
      Render(SetUseDirOnlyEntries_Action, @Value);
  end;
end;

procedure TCustomZipMaster.SetUseDirOnlyEntries_Action(const Data: Pointer);
begin
  TheLister(fLister).UseDirOnlyEntriesChanged(PBoolean(Data)^);
end;

{$IFNDEF _NO_UTF8_}
procedure TCustomZipMaster.SetUseUTF8(const Value: boolean);
begin
  if Value <> UsingUTF8 then
  begin
    Render(SetUseUTF8_Action, @Value);
    if fLister <> nil then
      Render(ClearCachedNames_Action, nil);
  end;
end;

procedure TCustomZipMaster.SetUseUTF8_Action(const Data: Pointer);
begin
  SetZipUseUTF8(PBoolean(Data)^);
end;
{$ENDIF}

procedure TCustomZipMaster.SetVerbose(const Value: boolean);
var
  v: Integer;
begin
  if not ReEntry then
  begin
    if Value then
      v := 1
    else
      v := 0;
    fInternal.fVerbosity := TZMVerbosity((Ord(fInternal.fVerbosity) and 2) or v);
  end;
end;

procedure TCustomZipMaster.SetVersion(const Value: string);
begin
  // Read only
end;

procedure TCustomZipMaster.SetWriteOptions(const Value: TZMWriteOpts);
begin
  if WriteOptions <> Value then
  begin
    if (zwoDiskSpan in Value) <> (zwoDiskSpan in WriteOptions) then
      AuxWasChanged;
    fInternal.FWriteOptions := Value;
  end;
end;

procedure TCustomZipMaster.SetZipComment(const Value: AnsiString);
begin
  if Value <> fInternal.FZipComment then
  begin
    if not DoSetZipComment(Value) then
    begin
      if (not IsActive) and (Value <> ZipComment) then // not Active
      begin
        fInternal.FZipComment := Value;
        fDelaying := fDelaying or DelayingComment;
      end;
    end;
  end;
end;

procedure TCustomZipMaster.SetZipFileName(const Value: string);
begin
  if Permitted then
    try
      fInternal.fZipFileName := Value;
      Set_Lister(nil);
      Start;
      Set_Lister(TZMLister.Create(Self));
      TheLister(fLister).Set_ZipFileName(Value, zloFull);
      done;
    except
      on E: Exception do
        DoneBad(E);
    end
  else
    if (not IsActive) and (Value <> ZipFileName) then // not Active
    begin
      fInternal.fZipFileName := Value;
      fDelaying := fDelaying or DelayingFileName;
    end;
end;

procedure TCustomZipMaster.Set_Lister(aLister: TZMWorkBase);
begin
  if aLister <> fLister then
  begin
    if fLister <> nil then
      fLister.Free;
    fLister := aLister;
  end;
end;

procedure TCustomZipMaster.Set_ZipCommentDelayed;
begin
  DoSetZipComment(fInternal.fZipComment);
end;

procedure TCustomZipMaster.ShowExceptionError(const ZMExcept: Exception);
const
  __ERR_GE_ExceptErr = __UNIT__ + (3471 shl 10) + GE_ExceptErr;
var
  display: boolean;
  msg: TZMString;
  ResID: Integer;
begin
  if ZMExcept is EZMException then
  begin
    ResID := EZMException(ZMExcept).ExtErr;
    display := EZMException(ZMExcept).DisplayMsg;
    msg := EZMException(ZMExcept).TheMessage;
  end
  else
  begin
    ResID := __ERR_GE_ExceptErr;
    display := True;
    msg := ZMExcept.Message;
  end;
  ShowMsg(msg, ResID, display);
end;

procedure TCustomZipMaster.ShowMsg(const msg: TZMString; ResID: Integer;
  display: boolean);
var
  m: TZMString;
  MsgID: Integer;
begin
  fInternal.fErrMessage := msg;
  if ResID < 0 then
    ResID := -ResID;
  MsgID := ResID and MSG_ID_MASK;
  fInternal.fExtErrCode := ResID;
  fInternal.fErrCode := MsgID;
  if display and (not Unattended) and (MsgID <> GE_Abort) and
    (MsgID <> DS_Canceled) then
  begin
    m := msg;
    ZipMessageDialog('', m, zmtInformation + DHC_ZipMessage, [mbOK]);
  end;

  ReportMessage(ResID, msg, State < zsBusy);
end;

procedure TCustomZipMaster.ShowZipFmtMessage(id: Integer;
  const Args: array of const; display: boolean);
var
  s: TZMString;
begin
  if id < 0 then
    id := -id;
  s := ZipLoadStr(id);

  if s <> '' then
    s := Format(s, Args);
  ShowMsg(s, id, display);
end;

procedure TCustomZipMaster.ShowZipMessage(Ident: Integer;
  const UserStr: string);
var
  msg: TZMString;
begin
  if Ident < 0 then
    Ident := -Ident;
  msg := ZipLoadStr(Ident);
  if msg = '' then
    msg := Format(RESOURCE_ERROR, [Ident]);
  msg := msg + UserStr;
  ShowMsg(msg, Ident, True);
end;

procedure TCustomZipMaster.Start;
var
  s: string;
begin
  fBlockedCnt := 0;
  SetZipBusy(True); // lock language and UsingUTF8
  if GetCurrentThreadID <> MainThreadID then
    fInternal.fNotMainThread := True;
  ClearErr;
  fInternal.fSuccessCnt := 0;
  fSniffer := FindSniffer;
  if fSniffer <> 0 then
  begin
    s := '';
    if Owner <> nil then
    begin
      s := Owner.Name + '.';
//      if s <> '' then
//        s := s + '.';
    end;
//    if name = '' then
//      s := '<unknown>.'
//    else
//      s := s + name;
//    if NotMainThread then
//      s := '*' + s;
//    ReportToSniffer(0, 'Starting ' + s);
    ReportToSniffer(0, Format('Starting %u.%u:%s<%p>(%s)',
        [MainThreadID, GetCurrentThreadID, s, pointer(self), Name]));
  end;
  // 'lock' spec lists
  fInternal.fIncludeSpecs.Assign(FSpecArgs);
  fInternal.fExcludeSpecs.Assign(FSpecArgsExcl);
  // tell the 'workers' we have started
  if fLister <> nil then
    fLister.Started;
  if fActiveWorker <> nil then
    fActiveWorker.Started;
end;

procedure TCustomZipMaster.StartWaitCursor;
begin
  if FCurWaitCount = 0 then
  begin
    FSaveCursor := Screen.Cursor;
    Screen.Cursor := crHourGlass;
  end;
  Inc(FCurWaitCount);
end;

procedure TCustomZipMaster.StateChanged(newState: TZMStates);
var
  NoCursor: boolean;
begin
  NoCursor := NotMainThread;
  if assigned(OnStateChange) then
    OnStateChange(Self, newState, NoCursor);
  if not NoCursor then
  begin
    if (newState < zsBusy) <> (State < zsBusy) then
    begin
      // 'busy' state has changed
      if State < zsBusy then
        StartWaitCursor   // now 'busy'
      else
        StopWaitCursor;   // now 'idle'
    end;
  end;
  FState := newState;
end;

function TCustomZipMaster.Stopped: boolean;
begin
  if BusyFlag = 0 then
    Result := True
  else
  begin
    Result := False;
    ReEntered;
  end;
end;

procedure TCustomZipMaster.StopWaitCursor;
begin
  if FCurWaitCount > 0 then
  begin
    Dec(FCurWaitCount);
    if FCurWaitCount < 1 then
      Screen.Cursor := FSaveCursor;
  end;
end;

function TCustomZipMaster.SysErrorMsg(ErrNo: cardinal = Cardinal(-1)): string;
const
  SysErrMsg = ' [0x%x] %s';
begin
  Result := '';
  if ErrNo = Cardinal(-1) then
    ErrNo := GetLastError;
  Result := Format(SysErrMsg, [ErrNo, SysErrorMessage(ErrNo)]);
end;

function TCustomZipMaster.Undeflate(OutStream, InStream: TStream;
  Length: Int64; var Method: TZMDeflates; var CRC: Cardinal): Integer;
var
  DllWorker: TZMDLLOpr;
begin
  if Permitted then
    try
      Start;
      DllWorker := TZMDLLOpr.Create(Self);
      DllWorker.Undeflate(OutStream, InStream, Length, Method, CRC);
      done;
    except
      on E: Exception do
        DoneBad(E);
    end;
  Result := ErrCode;
end;

function TCustomZipMaster.WriteSpan(const InFileName,
  OutFileName: string): Integer;
var
  FileWorker: TZMFileOpr;
begin
  Result := 0;
  if Permitted then
    try
      Start;
      FileWorker := TZMFileOpr.Create(Self);
      Result := FileWorker.WriteSpan(InFileName, OutFileName, False);
      if Result > 0 then
        Result := 0;
      done;
    except
      on E: Exception do
      begin
        DoneBad(E);
      end;
    end;
  if ErrCode <> 0 then
    Result := ErrCode;
end;

function TCustomZipMaster.ZipFmtLoadStr(id: Integer;
  const Args: array of const ): TZMString;
begin
  Result := ZipLoadStr(id);

  if Result <> '' then
    Result := Format(Result, Args);
end;

function TCustomZipMaster.ZipLoadStr(id: Integer): string;
begin
  if id < 0 then
    id := -id;
  Result := LoadZipStr(id);
{$IFNDEF UNICODE}
  if (Result <> '') and UsingUTF8 then
    Result := StrToUTF8(Result);
{$ENDIF}
end;

function TCustomZipMaster.ZipMessageDialog(const title: string;
  var msg: string; context: Integer; btns: TMsgDlgButtons): TModalResult;
var
  ctx: Integer;
  dlg: TZipDialogBox;
  s: string;
  t: string;
  tmpZipDialog: TZMDialogEvent;
begin
  t := title;
  if title = '' then
    t := Application.title;
  if Trace or Verbose then
    t := Format('%s   (%d)', [t, context and MAX_WORD]);
  tmpZipDialog := OnZipDialog;
  if assigned(tmpZipDialog) then
  begin
    s := msg;
    ctx := context;
    tmpZipDialog(Self, t, s, ctx, btns);
    if (ctx > 0) and (ctx <= Ord(mrYesToAll)) then
    begin
      msg := s;
      Result := TModalResult(ctx);
      Exit;
    end;
  end;
  dlg := TZipDialogBox.CreateNew2(Application, context);
  try
    dlg.Build(t, msg, btns);
    dlg.ShowModal();
    Result := dlg.ModalResult;
    if dlg.DlgType = zmtPassword then
    begin
      if (Result = mrOk) then
        msg := dlg.PWrd
      else
        msg := '';
    end;
  finally
    FreeAndNil(dlg);
  end;
end;

function TCustomZipMaster._GetAddPassword(var Response: TMsgDlgBtn): string;
const
  __ERR_PW_UnatAddPWMiss = __UNIT__ + (3743 shl 10) + PW_UnatAddPWMiss;
  __ERR_GE_WrongPassword = __UNIT__ + (3755 shl 10) + GE_WrongPassword;
var
  p1: string;
  p2: string;
begin
  p2 := '';
  if Unattended then
    ShowZipMessage(__ERR_PW_UnatAddPWMiss, '')
  else
  begin
    Response := _GetPassword(ZipLoadStr(PW_Caption),
      ZipLoadStr(PW_MessageEnter), DHC_AddPwrd1, mbOkCancel, p1);
    if (Response = mbOK) and (p1 <> '') then
    begin
      Response := _GetPassword(ZipLoadStr(PW_Caption),
        ZipLoadStr(PW_MessageConfirm), DHC_AddPwrd2, mbOkCancel, p2);
      if (Response = mbOK) and (p2 <> '') then
        if AnsiCompareStr(p1, p2) <> 0 then
        begin
          ShowZipMessage(__ERR_GE_WrongPassword, '');
          p2 := '';
        end;
    end;
  end;
  Result := p2;
end;

function TCustomZipMaster._GetExtrPassword(var Response: TMsgDlgBtn): string;
begin
  Result := '';
  if Unattended then
    ShowZipMessage(PW_UnatExtPWMiss, '')
  else
    Response := _GetPassword(ZipLoadStr(PW_Caption),
      ZipLoadStr(PW_MessageEnter), DHC_ExtrPwrd,
      [mbOK, mbCancel, mbAll], Result);
end;

function TCustomZipMaster._GetPassword(const DialogCaption, MsgTxt: string;
  ctx: Integer; pwb: TMsgDlgButtons; var ResultStr: string): TMsgDlgBtn;
var
  GModalResult: TModalResult;
  msg: string;
begin
  msg := MsgTxt;
  ResultStr := '';
  GModalResult := ZipMessageDialog(DialogCaption, msg,
    zmtPassword + (ctx and MAX_WORD), pwb);
  case GModalResult of
    mrOk:
      begin
        ResultStr := msg;
        Result := mbOK;
      end;
    mrCancel:
      Result := mbCancel;
    mrAll:
      Result := mbNoToAll;
  else
    Result := mbAbort;
  end;
end;

{ TZMWorkBase }
// stop public constructor
constructor TZMWorkBase.Create(anOwner: TZMWorkBin);
begin
  raise Exception.Create('Programming Error');
end;

constructor TZMWorkBase.Create(theMaster: TCustomZipMaster; anOwner:
    TZMWorkBin);
begin
  inherited Create(anOwner);
  fMaster := theMaster;
  fInternal := @theMaster.fInternal;
end;

procedure TZMWorkBase.CancelSet(Value: Integer);
begin
  // force cancel
end;

function TZMWorkBase.GetActiveWorker: TZMWorkBase;
begin
  Result := Master.fActiveWorker;
end;

function TZMWorkBase.GetExcludeSpecs: TStrings;
begin
  Result := Master.fInternal.fExcludeSpecs;
end;

function TZMWorkBase.GetIncludeSpecs: TStrings;
begin
  Result := Master.fInternal.fIncludeSpecs;
end;

function TZMWorkBase.GetNextLoadCheckNo: Integer;
begin
  // increment for each zip loaded
  Master.fLoadCheckNo := Master.fLoadCheckNo + 1;
  Result := Master.fLoadCheckNo;
end;

function TZMWorkBase.GetTheLister: TZMWorkBase;
begin
  Result := Master.fLister;
end;

function TZMWorkBase.GetVerbosity: TZMVerbosity;
begin
  Result := Master.fInternal.fVerbosity;
end;

procedure TZMWorkBase.IndexCheck(Index, Count: Integer);
const
  __ERR_GE_RangeError = __UNIT__ + (3856 shl 10) + GE_RangeError;
begin
  if (index < 0) or (index >= Count) then
    raise EZipMaster.CreateMsgFmt(__ERR_GE_RangeError, [index, Count - 1]);
end;

function TZMWorkBase.KeepAlive: boolean;
begin
  Result := Master.KeepAlive;
end;

procedure TZMWorkBase.ReportMessage(err: Integer; const msg: TZMString);
begin
  Master.ReportMessage(err, msg, True);
end;

procedure TZMWorkBase.ReportMsg(id: Integer; const Args: array of const );
var
  msg: TZMString;
  p: Integer;
begin
  msg := ZipFmtLoadStr(id, Args);
  if msg <> '' then
  begin
    p := 0;
    case msg[1] of
      '#':
        p := TM_Trace;
      '!':
        p := TM_Verbose;
    end;
    if p <> 0 then
    begin
      msg := Master.ZipLoadStr(p) + Copy(msg, 2, Length(msg) - 1);
    end;
  end;
  ReportMessage(0, msg);
end;

function TZMWorkBase.ReportSkipping(const FName: string; err: Integer;
  typ: TZMSkipTypes): boolean;
begin
  Result := Master.ReportSkipping(FName, err, typ);
end;

procedure TZMWorkBase.ReportToSniffer(err: Cardinal; const msg: TZMString);
begin
  Master.ReportToSniffer(err, msg);
end;

procedure TZMWorkBase.SetActiveWorker(const Value: TZMWorkBase);
begin
  if Master.fActiveWorker <> Value then
  begin
    // kill any old worker
    if Master.fActiveWorker <> nil then
      Master.fActiveWorker.Free;
    Master.fActiveWorker := Value;
  end;
end;

procedure TZMWorkBase.SetExcludeSpecs(const Value: TStrings);
begin
  //
end;

procedure TZMWorkBase.SetIncludeSpecs(const Value: TStrings);
begin
  //
end;

procedure TZMWorkBase.ShowExceptionError(const ZMExcept: Exception);
begin
  Master.ShowExceptionError(ZMExcept);
end;

procedure TZMWorkBase.ShowMsg(const msg: TZMString; ResID: Integer;
  display: boolean);
begin
  Master.ShowMsg(msg, ResID, display);
end;

procedure TZMWorkBase.ShowZipFmtMessage(id: Integer; const Args: array of const;
  display: boolean);
begin
  Master.ShowZipFmtMessage(id, Args, display);
end;

procedure TZMWorkBase.ShowZipMessage(Ident: Integer; const UserStr: string);
begin
  Master.ShowZipMessage(Ident, UserStr);
end;

function TZMWorkBase.SysErrorMsg(ErrNo: cardinal = Cardinal(-1)): string;
begin
  Result := Master.SysErrorMsg(ErrNo);
end;

function TZMWorkBase.ZipFmtLoadStr(id: Integer; const Args: array of const )
  : TZMString;
begin
  Result := Master.ZipFmtLoadStr(id, Args);
end;

function TZMWorkBase.ZipMessageDialog(const title: string; var msg: string;
  context: Integer; btns: TMsgDlgButtons): TModalResult;
begin
  Result := Master.ZipMessageDialog(title, msg, context, btns);
end;

{ TZMWorkBin }

// stop public default constructor
constructor TZMWorkBin.Create;
begin
  raise Exception.Create('Programming Error');
end;

constructor TZMWorkBin.Create(theOwner: TZMWorkBin);
begin
  inherited Create;
  FOwner := nil;  // not needed Delphi
  if theOwner <> nil then
    theOwner.InsertIntoBin(Self);
end;

procedure TZMWorkBin.BeforeDestruction;
var
  tmp: TZMWorkBin;
begin
  // free our binned items
  if FBinned <> nil then
    KillBinned;
  // remove from owner
  if FOwner <> nil then
  begin
    tmp := FOwner;
    FOwner := nil; // stop recursive free
    tmp.RemoveFromBin(Self);
  end;
  inherited;
end;

procedure TZMWorkBin.InsertIntoBin(WorkBin: TZMWorkBin);
begin
  if WorkBin <> nil then
  begin
    if FBinned = nil then
      FBinned := TList.Create;
    WorkBin.FOwner := Self;
    FBinned.Add(WorkBin);
  end;
end;

procedure TZMWorkBin.KillBinned;
var
  tmp: TZMWorkBin;
  I: Integer;
begin
  if FBinned <> nil then
  begin
    for I := FBinned.Count -1 downto 0 do
    begin
      if TObject(FBinned[I]) is TZMWorkBin then
      begin
        tmp := TZMWorkBin(FBinned[I]);
        tmp.FOwner := nil; // stop recursive free
        tmp.Free;
      end;
    end;
    FBinned.Free;
    FBinned := nil;
  end;
end;

procedure TZMWorkBin.RemoveFromBin(WorkBin: TZMWorkBin);
begin
  FBinned.Remove(WorkBin);
  // only free it if we own it
  if WorkBin.Owner = Self then
    WorkBin.Free;
  if FBinned.Count = 0 then
  begin
    FBinned.Free;
    FBinned := nil;
  end;
end;

procedure TZMWorkBin.SetOwner(const Value: TZMWorkBin);
var
  tmp: TZMWorkBin;
begin
  if FOwner <> Value then
  begin
    if FOwner <> nil then   // remove from old bin
    begin
      tmp := FOwner;
      FOwner := nil;  // don't free it
      tmp.RemoveFromBin(Self);
    end;
    FOwner := nil;
    if Value <> nil then    // insert myself into new owner
      Value.InsertIntoBin(Self);
  end;
end;

end.
