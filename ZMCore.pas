unit ZMCore;

//  ZMCore.pas - event triggering
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
//modified 2013-02-14

interface

{.$DEFINE DEBUG_PROGRESS}

uses
  Classes, SysUtils, Controls, Forms, Dialogs, Graphics,
  ZipMstr, ZMXcpt, ZMDelZip, ZMStructs, ZMCompat;

//const
//  zprFile = 0;  // not used
//  zprArchive = 1;  // dll
//  zprCopyTemp = 2; // dll
//  zprSFX = 3;      // not used
//  zprHeader = 4;   // not used
//  zprFinish = 5;     // not used
//  zprCompressed = 6;  // wrongly used
//  zprCentral = 7;  // dll
//  zprChecking = 8;
//  zprLoading = 9;  // dll
//  zprJoining = 10;
//  zprSplitting = 11;
//  zprWriting = 12;
//  zprPreCalc = 13; // dll
  {
  _PR_Archive = '*Resetting Archive bit';
  _PR_CopyZipFile = '*Copying Zip File';
  _PR_SFX = '*SFX';
  _PR_Header = '*??';
  _PR_Finish = '*Finalising';
  _PR_Copying = '*Copying';
  _PR_CentrlDir = '*Central Directory';
  _PR_Checking = '*Checking';
  _PR_Loading = '*Loading Directory';
  _PR_Joining = '*Joining split zip file';
  _PR_Splitting = '*Splitting zip file';
  _PR_Writing = '*Writing zip file';
  _PR_PreCalc = '*Precalculating CRC';
  _PR_Processing = '*Processing';
  }
const
  EXT_EXE = '.EXE';
  EXT_EXEL = '.exe';
  EXT_ZIP = '.ZIP';
  EXT_ZIPL = '.zip';
  PRE_INTER = 'ZI$';
  PRE_SFX = 'ZX$';

type
  TZLoadOpts = (zloNoLoad, zloFull, zloSilent);

type
//  TZMVerbosity = (zvOff, zvVerbose, zvTrace);
  TZMEncodingDir = (zedFromInt, zedToInt);
  TZipShowProgress = (zspNone, zspFull, zspExtra);

  TZipAllwaysItems = (zaaYesOvrwrt);
  TZipAnswerAlls = set of TZipAllwaysItems;

type
  TZipNameType = (zntExternal, zntInternal);

type
  TZipNumberScheme = (znsNone, znsVolume, znsName, znsExt);

type
  TProgDetails = class(TZMProgressDetails)
  private
    FDelta: Int64;
    FInBatch: Boolean;
    FItemCount: Int64;
    FItemName: TZMString;
    FItemNumber: Integer;
    FItemPosition: Int64;
    FItemSize: Int64;
    FProgType: TZMProgressType;
    FStop: Boolean;
    FTotalPosition: Int64;
    FTotalSize: Int64;
    FWritten: Int64;
  protected
    function GetBytesWritten: Int64; override;
    function GetDelta: Int64; override;
    function GetItemName: TZMString; override;
    function GetItemNumber: Integer; override;
    function GetItemPosition: Int64; override;
    function GetItemSize: Int64; override;
    function GetOrder: TZMProgressType; override;
    function GetStop: Boolean; override;
    function GetTotalCount: Int64; override;
    function GetTotalPosition: Int64; override;
    function GetTotalSize: Int64; override;
    procedure SetStop(const Value: Boolean); override;
  public
    procedure Advance(adv: Int64);
    procedure AdvanceXtra(adv: Cardinal);
    procedure Clear;
    procedure SetCount(Count: Int64);
    procedure SetEnd;
    procedure SetItem(const FName: TZMString; FSize: Int64);
    procedure SetItemXtra(const xmsg: TZMString; FSize: Int64);
    procedure SetSize(FullSize: Int64);
    procedure Written(bytes: Int64);
    property BytesWritten: Int64 read GetBytesWritten write FWritten;
    property InBatch: Boolean Read FInBatch;
    property ItemName: TZMString read GetItemName write FItemName;
    property ItemNumber: Integer read GetItemNumber write FItemNumber;
    property ItemPosition: Int64 read GetItemPosition write FItemPosition;
    property ItemSize: Int64 read GetItemSize write FItemSize;
    property Order: TZMProgressType read GetOrder write FProgType;
    property TotalCount: Int64 read GetTotalCount write FItemCount;
    property TotalPosition: Int64 read GetTotalPosition write FTotalPosition;
    property TotalSize: Int64 read GetTotalSize write FTotalSize;
  end;

type
  TZCentralValues = (zcvDirty, zcvEmpty, zcvError, zcvBadStruct, zcvBusy);
  TZCentralStatus = set of TZCentralValues;

type
  TZMPipeImp = class(TZMPipe)
  private
    FAttributes: Cardinal;
    FDOSDate: Cardinal;
    FFileName: string;
    FOwnsStream: boolean;
    FSize: Integer;
    FStream: TStream;
  protected
    function GetAttributes: Cardinal; override;
    function GetDOSDate: Cardinal; override;
    function GetFileName: string; override;
    function GetOwnsStream: boolean; override;
    function GetSize: Integer; override;
    function GetStream: TStream; override;
    procedure SetAttributes(const Value: Cardinal); override;
    procedure SetDOSDate(const Value: Cardinal); override;
    procedure SetFileName(const Value: string); override;
    procedure SetOwnsStream(const Value: boolean); override;
    procedure SetSize(const Value: Integer); override;
    procedure SetStream(const Value: TStream); override;
  public
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TZMPipeImp);
    procedure BeforeDestruction; override;
  end;

  TZMPipeListImp = class(TZMPipeList)
  private
    FList: TList;
  protected
    function GetCount: Integer; override;
    function GetPipe(Index: Integer): TZMPipe; override;
    procedure SetCount(const Value: Integer); override;
    procedure SetPipe(Index: Integer; const Value: TZMPipe); override;
  public
    function Add(aStream: TStream; const FileName: string; Own: boolean): integer; override;
    procedure AfterConstruction; override;
    procedure AssignTo(Dest: TZMPipeListImp);
    procedure BeforeDestruction; override;
    procedure Clear; override;
    function HasStream(Index: Integer): boolean;
    function KillStream(Index: Integer): boolean;
  end;

const
  MAX_PIPE = 9;

type
  TZMCore = class(TZMWorkBase)
  private
    FAnswerAll: TZipAnswerAlls;
    FCancel: Integer;
    FFileCleanup: TStringList;
    FShowProgress: TZipShowProgress;
    FWinXP: Boolean;
    function GetAddOptions: TZMAddOpts;
    function GetAuxChanged: Boolean;
    function GetConfirmErase: Boolean;
    function GetEncodeAs: TZMEncodingOpts;
    function GetEncoding: TZMEncodingOpts;
    function GetEncoding_CP: Cardinal;
    function GetErrCode: Integer;
    function GetErrMessage: TZMString;
    function GetExtErrCode: Cardinal;
    function GetHandle: Cardinal;
    function GetHowToDelete: TZMDeleteOpts;
    function GetNoSkipping: TZMSkipAborts;
    function GetSFXCaption: TZMString;
    function GetSFXCommandLine: TZMString;
    function GetSFXDefaultDir: string;
    function GetSFXIcon: TIcon;
    function GetSFXMessage: TZMString;
    function GetSFXOptions: TZMSFXOpts;
    function GetSFXOverwriteMode: TZMOvrOpts;
    function GetSFXPath: string;
    function GetSFXRegFailPath: string;
    function GetSpanOptions: TZMSpanOpts;
    function GetSuccessCnt: Integer;
    function GetTempDir: String;
    function GetTotalWritten: Int64;
    function GetUnattended: boolean;
    function GetUseDirOnlyEntries: boolean;
    function GetWriteOptions: TZMWriteOpts;
    function GetZipStream: TMemoryStream;
    procedure SetAddOptions(const Value: TZMAddOpts);
    procedure SetAuxChanged(const Value: Boolean);
    procedure SetCancel(Value: Integer);
    procedure SetEncodeAs(const Value: TZMEncodingOpts);
    procedure SetEncoding(const Value: TZMEncodingOpts);
    procedure SetEncoding_CP(const Value: Cardinal);
    procedure SetErrCode(Value: Integer);
    procedure SetErrMessage(const Value: TZMString);
    procedure SetExtErrCode(const Value: Cardinal);
    procedure SetProgDetail(const Value: TProgDetails);
    procedure SetSFXCaption(const Value: TZMString);
    procedure SetSFXCommandLine(const Value: TZMString);
    procedure SetSFXDefaultDir(const Value: string);
    procedure SetSFXIcon(const Value: TIcon);
    procedure SetSFXMessage(const Value: TZMString);
    procedure SetSFXOptions(const Value: TZMSFXOpts);
    procedure SetSFXOverwriteMode(const Value: TZMOvrOpts);
    procedure SetSFXPath(const Value: string);
    procedure SetSFXRegFailPath(const Value: string);
    procedure SetSpanOptions(const Value: TZMSpanOpts);
    procedure SetSuccessCnt(const Value: Integer);
    procedure SetTotalWritten(const Value: Int64);
    procedure SetWriteOptions(const Value: TZMWriteOpts);
    procedure SetZipStream(const Value: TMemoryStream);
  protected
    FEventErr: String;
    FIsDestructing: Boolean;
    FProgDetails: TProgDetails;
    procedure CancelSet(Value: Integer); override;
    procedure Finished(WasGood: boolean); override;
    function GetTotalSizeToProcess: Int64;
    procedure Started; override;
    property AuxChanged: Boolean read GetAuxChanged write SetAuxChanged;
    property Unattended: boolean read GetUnattended;
    property ZipStream: TMemoryStream read GetZipStream write SetZipStream;
  public
    procedure AddCleanupFile(const fn: TZMString; always: Boolean = False);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure CheckCancel;
    procedure CleanupFiles(IsError: Boolean);
    procedure Diag(const msg: String);
    procedure Kill; virtual;
    procedure OnDirUpdate;
    procedure OnNewName(idx: Integer);
    function RemoveFileCleanup(const fn: String): Boolean;
    procedure ReportProgress(ActionCode: TActionCodes; ErrorCode: Integer; msg:
        TZMString; File_Size: Int64);
    function ReportSkippingEx(const spec: TZMString; Reason: TZMSkipTypes; err:
        Integer; list: TStrings): Boolean;
    procedure ShowZipFmtMsg(id: Integer; const Args: array of const ;
      display: Boolean);
    procedure ShowZipMsg(Ident: Integer; display: Boolean);
    function ZipLoadStr(id: Integer): TZMString;
    function ZipMessageDlgEx(const title, msg: String; context: Integer;
      btns: TMsgDlgButtons): TModalResult;
    property AddOptions: TZMAddOpts read GetAddOptions write SetAddOptions;
    property AnswerAll: TZipAnswerAlls Read FAnswerAll Write FAnswerAll;
    property Cancel: Integer Read FCancel Write SetCancel;
    property ConfirmErase: Boolean read GetConfirmErase default True;
    property EncodeAs: TZMEncodingOpts read GetEncodeAs write SetEncodeAs;
    property Encoding: TZMEncodingOpts read GetEncoding write SetEncoding;
    property Encoding_CP: Cardinal read GetEncoding_CP write SetEncoding_CP;
    property ErrCode: Integer read GetErrCode write SetErrCode;
    property ErrMessage: TZMString read GetErrMessage write SetErrMessage;
    property ExtErrCode: Cardinal read GetExtErrCode write SetExtErrCode;
    property Handle: Cardinal read GetHandle;
    property HowToDelete: TZMDeleteOpts read GetHowToDelete;
    property NoSkipping: TZMSkipAborts read GetNoSkipping;
    property ProgDetail: TProgDetails Read FProgDetails Write SetProgDetail;
    property SFXCaption: TZMString read GetSFXCaption write SetSFXCaption;
    property SFXCommandLine: TZMString read GetSFXCommandLine write
        SetSFXCommandLine;
    property SFXDefaultDir: string read GetSFXDefaultDir write SetSFXDefaultDir;
    property SFXIcon: TIcon read GetSFXIcon write SetSFXIcon;
    property SFXMessage: TZMString read GetSFXMessage write SetSFXMessage;
    property SFXOptions: TZMSFXOpts read GetSFXOptions write SetSFXOptions;
    property SFXOverwriteMode: TZMOvrOpts read GetSFXOverwriteMode write
        SetSFXOverwriteMode default ovrConfirm;
    property SFXPath: string read GetSFXPath write SetSFXPath;
    property SFXRegFailPath: string read GetSFXRegFailPath write SetSFXRegFailPath;
    property ShowProgress
      : TZipShowProgress Read FShowProgress Write FShowProgress;
    property SpanOptions: TZMSpanOpts read GetSpanOptions write SetSpanOptions;
    property SuccessCnt: Integer read GetSuccessCnt write SetSuccessCnt;
    property TempDir: String read GetTempDir;
    property TotalWritten: Int64 read GetTotalWritten write SetTotalWritten;
    property UseDirOnlyEntries: boolean read GetUseDirOnlyEntries;
    property WinXP: Boolean Read FWinXP;
    property WriteOptions: TZMWriteOpts read GetWriteOptions write SetWriteOptions;
  end;

procedure IndexCheck(index, Count: Integer);

implementation

{$INCLUDE   '.\ZipVers.inc'}

uses Windows, Messages, ZMUtils, ZMDlg, ZMMsg, ZMCtx, ZMMsgStr,
  ZMUTF8, ZMMatch, ZMWFuncs, TypInfo;

const
  __UNIT__ = 6 shl 23;

const
  SZipMasterSniffer = 'ZipMaster Sniffer';
  STZipSniffer = 'TZipSniffer';
  WM_SNIFF_START = WM_APP + $3F42;
  WM_SNIFF_STOP = WM_APP + $3F44;
  SNIFF_MASK = $FFFFFF;
  RESOURCE_ERROR: String =
    'ZMRes_???.res is probably not linked to the executable' + #10 +
    'Missing String ID is: %d ';


procedure IndexCheck(index, Count: Integer);
const
  __ERR_GE_RangeError = __UNIT__ + (370 shl 10) + GE_RangeError;
begin
if (index < 0) or (index >= Count) then
  raise EZipMaster.CreateMsgFmt(__ERR_GE_RangeError, [index, Count - 1]);
end;

  { TProgDetails }
procedure TProgDetails.Advance(adv: Int64);
begin
  FDelta := adv;
  FTotalPosition := FTotalPosition + adv;
  FItemPosition := FItemPosition + adv;
  FProgType := ProgressUpdate;
end;

procedure TProgDetails.AdvanceXtra(adv: Cardinal);
begin
  FDelta := adv;
  Inc(FItemPosition, adv);
  FProgType := ExtraUpdate;
end;

procedure TProgDetails.Clear;
begin
  FProgType := EndOfBatch;
  FDelta := 0;
  FItemCount := 0;
  FWritten := 0;
  FTotalSize := 0;
  FTotalPosition := 0;
  FItemSize := 0;
  FItemPosition := 0;
  FItemName := '';
  FItemNumber := 0;
  FStop := False;
end;

function TProgDetails.GetBytesWritten: Int64;
begin
  Result := FWritten;
end;

function TProgDetails.GetDelta: Int64;
begin
  Result := FDelta;
end;

function TProgDetails.GetItemName: TZMString;
begin
  Result := FItemName;
end;

function TProgDetails.GetItemNumber: Integer;
begin
  Result := FItemNumber;
end;

function TProgDetails.GetItemPosition: Int64;
begin
  Result := FItemPosition;
end;

function TProgDetails.GetItemSize: Int64;
begin
  Result := FItemSize;
end;

function TProgDetails.GetOrder: TZMProgressType;
begin
  Result := FProgType;
end;

function TProgDetails.GetStop: Boolean;
begin
  Result := FStop;
end;

function TProgDetails.GetTotalCount: Int64;
begin
  Result := FItemCount;
end;

function TProgDetails.GetTotalPosition: Int64;
begin
  Result := FTotalPosition;
end;

function TProgDetails.GetTotalSize: Int64;
begin
  Result := FTotalSize;
end;

procedure TProgDetails.SetCount(Count: Int64);
begin
  Clear;
  FItemCount := Count;
  FItemNumber := 0;
  FProgType := TotalFiles2Process;
end;

procedure TProgDetails.SetEnd;
begin
  FItemName := '';
  FItemSize := 0;
  FInBatch := False;
  FProgType := EndOfBatch;
  FStop := False;
end;

procedure TProgDetails.SetItem(const FName: TZMString; FSize: Int64);
begin
  if FSize >= 0 then
  begin
    Inc(FItemNumber);
    FItemName := FName;
    FItemSize := FSize;
    FItemPosition := 0;
    FProgType := NewFile;
  end
  else
    FProgType := EndOfItem;
end;

procedure TProgDetails.SetItemXtra(const xmsg: TZMString; FSize: Int64);
begin
  FItemName := xmsg;
  FItemSize := FSize;
  FItemPosition := 0;
  FProgType := NewExtra;
end;

procedure TProgDetails.SetSize(FullSize: Int64);
begin
  FStop := False;
  FTotalSize := FullSize;
  FTotalPosition := 0;
  FItemName := '';
  FItemSize := 0;
  FItemPosition := 0;
  FProgType := TotalSize2Process;
  FWritten := 0;
  FInBatch := True; // start of batch
end;

procedure TProgDetails.SetStop(const Value: Boolean);
begin
  FStop := Value;
end;

procedure TProgDetails.Written(bytes: Int64);
begin
  FWritten := bytes;
end;

procedure TZMCore.AddCleanupFile(const fn: TZMString; always: Boolean = False);
var
  f: String;
  obj: TObject;
begin
  f := ExpandFileName(fn); // need full path incase current dir changes
  obj := nil;
  if always then
    obj := TObject(self);
  FFileCleanup.AddObject(f, obj);
end;

procedure TZMCore.AfterConstruction;
begin
  inherited;
  FProgDetails := TProgDetails.Create;
  FFileCleanup := TStringList.Create;
  FWinXP := IsWinXP; // set flag;
end;

procedure TZMCore.BeforeDestruction;
begin
  FCancel := DS_Canceled;
  FreeAndNil(FFileCleanup);
  FreeAndNil(FProgDetails);
  inherited;
end;

procedure TZMCore.CancelSet(Value: Integer);
begin
  FCancel := Value;
end;

procedure TZMCore.CheckCancel;
begin
  KeepAlive;
  if FCancel <> 0 then
    raise EZipMaster.CreateMsgDisp(FCancel, True);
end;

procedure TZMCore.CleanupFiles(IsError: Boolean);
var
  AlwaysClean: Boolean;
  fn: TZMString;
  i: Integer;
begin
  if (FFileCleanup.Count > 0) then
  begin
    for i := FFileCleanup.Count - 1 downto 0 do
    begin
      fn := FFileCleanup[i];
      if Length(fn) < 2 then
        continue;
      AlwaysClean := FFileCleanup.Objects[i] <> nil;
      if IsError or AlwaysClean then
      begin
        if CharInSet(fn[Length(fn)], ['/', '\']) then
        begin
          fn := ExcludeTrailingBackslash(fn);
          if _Z_DirExists(fn) then
            _Z_RemoveDir(fn);
        end
        else
        begin
          if FileExists(fn) then
            _Z_DeleteFile(fn);
        end;
      end;
    end;
    FFileCleanup.Clear;
  end;
end;

procedure TZMCore.Diag(const msg: String);
begin
  if Verbosity >= zvVerbose then
    ReportMessage(0, 'Trace: ' + msg); // quicker
end;

procedure TZMCore.Finished(WasGood: boolean);
begin
  CleanupFiles(not WasGood);
end;

function TZMCore.GetAddOptions: TZMAddOpts;
begin
  Result := fInternal.fAddOptions;
end;

function TZMCore.GetAuxChanged: Boolean;
begin
    Result := fInternal.fAuxChanged
end;

function TZMCore.GetConfirmErase: Boolean;
begin
  Result := fInternal.fConfirmErase;
end;

function TZMCore.GetEncodeAs: TZMEncodingOpts;
begin
  Result := fInternal.fEncodeAs;
end;

function TZMCore.GetEncoding: TZMEncodingOpts;
begin
  Result := fInternal.fEncoding;
end;

function TZMCore.GetEncoding_CP: Cardinal;
begin
    Result := fInternal.fEncoding_CP
end;

function TZMCore.GetErrCode: Integer;
begin
  Result := fInternal.fErrCode;
end;

function TZMCore.GetErrMessage: TZMString;
begin
  Result := fInternal.fErrMessage;
end;

function TZMCore.GetExtErrCode: Cardinal;
begin
  Result := fInternal.fExtErrCode;
end;

function TZMCore.GetHandle: Cardinal;
begin
  Result := fInternal.fHandle;
end;

function TZMCore.GetHowToDelete: TZMDeleteOpts;
begin
  Result := fInternal.fHowToDelete;
end;

function TZMCore.GetNoSkipping: TZMSkipAborts;
begin
  Result := fInternal.fNoSkipping;
end;

function TZMCore.GetSFXCaption: TZMString;
begin
    Result := fInternal.fSFXCaption
end;

function TZMCore.GetSFXCommandLine: TZMString;
begin
    Result := fInternal.fSFXCommandLine
end;

function TZMCore.GetSFXDefaultDir: string;
begin
    Result := fInternal.fSFXDefaultDir
end;

function TZMCore.GetSFXIcon: TIcon;
begin
    Result := fInternal.fSFXIcon
end;

function TZMCore.GetSFXMessage: TZMString;
begin
    Result := fInternal.fSFXMessage
end;

function TZMCore.GetSFXOptions: TZMSFXOpts;
begin
    Result := fInternal.fSFXOptions
end;

function TZMCore.GetSFXOverwriteMode: TZMOvrOpts;
begin
    Result := fInternal.fSFXOverwriteMode
end;

function TZMCore.GetSFXPath: string;
begin
    Result := fInternal.fSFXPath
end;

function TZMCore.GetSFXRegFailPath: string;
begin
    Result := fInternal.fSFXRegFailPath
end;

function TZMCore.GetSpanOptions: TZMSpanOpts;
begin
  Result := fInternal.fSpanOptions;
end;

function TZMCore.GetSuccessCnt: Integer;
begin
  Result := fInternal.fSuccessCnt;
end;

function TZMCore.GetTempDir: String;
begin
  Result := fInternal.fTempDir;
end;

function TZMCore.GetTotalSizeToProcess: Int64;
begin
  Result := TProgDetails(FProgDetails).TotalSize;
end;

function TZMCore.GetTotalWritten: Int64;
begin
  Result := ProgDetail.BytesWritten;
end;

function TZMCore.GetUnattended: boolean;
begin
    Result := fInternal.fUnattended
end;

function TZMCore.GetUseDirOnlyEntries: boolean;
begin
  Result := fInternal.fUseDirOnlyEntries;
end;

function TZMCore.GetWriteOptions: TZMWriteOpts;
begin
  Result := fInternal.fWriteOptions;
end;

function TZMCore.GetZipStream: TMemoryStream;
begin
    Result := Master.ZipStream;
end;

procedure TZMCore.Kill;
begin
  FCancel := DS_Canceled;
end;

procedure TZMCore.OnDirUpdate;
begin
  if assigned(Master.OnDirUpdate) then
    Master.OnDirUpdate(Master);
end;

procedure TZMCore.OnNewName(idx: Integer);
begin
  if assigned(Master.OnNewName) then
    Master.OnNewName(Master, idx);
end;

function TZMCore.RemoveFileCleanup(const fn: String): Boolean;
var
  f: String;
  i: Integer;
begin
  Result := False;
  f := ExpandFileName(fn);
  for i := FFileCleanup.Count - 1 downto 0 do
    if AnsiSameText(FFileCleanup[i], f) then
    begin
      FFileCleanup.Delete(i);
      Result := True;
      break;
    end;
end;

procedure TZMCore.ReportProgress(ActionCode: TActionCodes; ErrorCode: Integer;
    msg: TZMString; File_Size: Int64);
var
  Details: TProgDetails;
  SendDetails: Boolean;
  tmpProgress: TZMProgressEvent;
begin
  if FIsDestructing then
    exit;
  if ActionCode <= zacXProgress then
  begin
    Details := FProgDetails as TProgDetails;
    SendDetails := True;
    case ActionCode of
      zacTick: { 'Tick' Just checking / processing messages }
        begin
          KeepAlive;
          SendDetails := False;
        end;

      zacItem: { progress type 1 = StartUp any operation on a new file }
        Details.SetItem(msg, File_Size);

      zacProgress: { progress type 2 = increment bar }
        Details.Advance(File_Size);

      zacEndOfBatch: { end of a batch of 1 or more files }
        begin
          if Details.InBatch then
            Details.SetEnd
          else
            SendDetails := False;
        end;

      zacCount: { total number of files to process }
      begin
        Details.SetCount(File_Size);
        fInternal^.fTotalSizeToProcess := 0;
      end;

      zacSize: { total size of all files to be processed }
      begin
        Details.SetSize(File_Size);
        fInternal^.fTotalSizeToProcess := File_Size;
      end;

      zacXItem: { progress type 15 = Start new extra operation }
        begin
          if (ErrorCode >= 0) and (ErrorCode < 16) then
            ErrorCode := PR_Archive + ErrorCode - 1;
          msg := Master.ZipLoadStr(ErrorCode);
          Details.SetItemXtra(msg, File_Size);
        end;

      zacXProgress: { progress type 16 = increment bar for extra operation }
        Details.AdvanceXtra(File_Size);
    end; { end case }
{$IFDEF DEBUG_PROGRESS}
    if Verbosity >= zvVerbose then
      case ActionCode of
        zacItem:
        begin
          if Details.Order = EndOfItem then
            Diag(Format('#End of Item - "%s"', [Details.ItemName]))
          else
            Diag(Format('#Item - "%s" %d', [Details.ItemName, Details.ItemSize]));
        end;
        zacProgress:
          Diag(Format('#Progress - [inc:%d] ipos:%d isiz:%d, tpos:%d tsiz:%d',
              [File_Size, Details.ItemPosition, Details.ItemSize,
              Details.TotalPosition, Details.TotalSize]));
        zacEndOfBatch:
          if SendDetails then
            Diag('#End Of Batch')
          else
            Diag('#End Of Batch with no batch');
        zacCount:
          Diag(Format('#Count - %d', [Details.TotalCount]));
        zacSize:
          Diag(Format('#Size - %d', [Details.TotalSize]));
        zacXItem:
          Diag(Format('#XItem - %s size = %d', [Details.ItemName, File_Size]));
        zacXProgress:
          Diag(Format('#XProgress - [inc:%d] pos:%d siz:%d',
              [File_Size, Details.ItemPosition, Details.ItemSize]));
      end;
{$ENDIF}
    tmpProgress := Master.OnProgress;
    if SendDetails and (assigned(tmpProgress)) then
      tmpProgress(Master, Details);
  end;

  KeepAlive;
end;

function TZMCore.ReportSkippingEx(const spec: TZMString; Reason: TZMSkipTypes;
    err: Integer; list: TStrings): Boolean;
begin
  if Verbosity >= zvVerbose then
    Diag('Skipped filespec ' + spec);
  if list <> nil then
    list.Add(spec + '<<' + GetEnumName(TypeInfo(TZMSkipTypes), Ord(Reason)));
  Result := ReportSkipping(spec, err, Reason);
end;

procedure TZMCore.SetAddOptions(const Value: TZMAddOpts);
begin
  fInternal.FAddOptions := Value;
end;

procedure TZMCore.SetAuxChanged(const Value: Boolean);
begin
  fInternal.fAuxChanged := Value;
end;

procedure TZMCore.SetCancel(Value: Integer);
begin
  FCancel := Value;
  fInternal.FCancel := Value;
end;

procedure TZMCore.SetEncodeAs(const Value: TZMEncodingOpts);
begin
    fInternal.fEncodeAs := Value;
end;

procedure TZMCore.SetEncoding(const Value: TZMEncodingOpts);
begin
    fInternal.fEncoding := Value;
end;

procedure TZMCore.SetEncoding_CP(const Value: Cardinal);
begin
    fInternal.fEncoding_CP := Value;
end;

(* ? TZMCore.SetErrCode
Some functions return -error - normalise these values
*)
procedure TZMCore.SetErrCode(Value: Integer);
begin
  if Value < 0 then
    Value := -Value;
  fInternal.fErrCode := Value;
end;

procedure TZMCore.SetErrMessage(const Value: TZMString);
begin
  fInternal.fErrMessage := Value;
end;

procedure TZMCore.SetExtErrCode(const Value: Cardinal);
begin
  fInternal.fExtErrCode := Value;
end;

procedure TZMCore.SetProgDetail(const Value: TProgDetails);
begin
  // do not change
end;

procedure TZMCore.SetSFXCaption(const Value: TZMString);
begin
    fInternal.fSFXCaption := Value;
end;

procedure TZMCore.SetSFXCommandLine(const Value: TZMString);
begin
    fInternal.fSFXCommandLine := Value;
end;

procedure TZMCore.SetSFXDefaultDir(const Value: string);
begin
    fInternal.fSFXDefaultDir := Value;
end;

procedure TZMCore.SetSFXIcon(const Value: TIcon);
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
  end;
end;

procedure TZMCore.SetSFXMessage(const Value: TZMString);
begin
    fInternal.fSFXMessage := Value;
end;

procedure TZMCore.SetSFXOptions(const Value: TZMSFXOpts);
begin
    fInternal.fSFXOptions := Value;
end;

procedure TZMCore.SetSFXOverwriteMode(const Value: TZMOvrOpts);
begin
    fInternal.fSFXOverwriteMode := Value;
end;

procedure TZMCore.SetSFXPath(const Value: string);
begin
    fInternal.fSFXPath := Value;
end;

procedure TZMCore.SetSFXRegFailPath(const Value: string);
begin
    fInternal.fSFXRegFailPath := Value;
end;

procedure TZMCore.SetSpanOptions(const Value: TZMSpanOpts);
begin
  fInternal.fSpanOptions := Value;
end;

procedure TZMCore.SetSuccessCnt(const Value: Integer);
begin
  fInternal.fSuccessCnt := Value;
end;

procedure TZMCore.SetTotalWritten(const Value: Int64);
begin
  ProgDetail.Written(Value);
end;

procedure TZMCore.SetWriteOptions(const Value: TZMWriteOpts);
begin
  fInternal.fWriteOptions := Value;
end;

procedure TZMCore.SetZipStream(const Value: TMemoryStream);
begin
  if fInternal.fZipStream <> Value then
  begin
    if fInternal.fZipStream <> nil then
      fInternal.fZipStream.Free;
    fInternal.fZipStream := Value;
  end;
end;

(* ? TZMCore.ShowZipFmtMsg
  1.79 added
*)
procedure TZMCore.ShowZipFmtMsg(id: Integer; const Args: array of const ;
  display: Boolean);
begin
  if id < 0 then
    id := -id;
  ShowMsg(ZipFmtLoadStr(id, Args), id, display);
end;

procedure TZMCore.ShowZipMsg(Ident: Integer; display: Boolean);
var
  msg: String;
begin
  if Ident < 0 then
    Ident := -Ident;
  msg := ZipLoadStr(Ident);
  if msg = '' then
    msg := Format(RESOURCE_ERROR, [Ident]);
  ShowMsg(msg, Ident, display);
end;

procedure TZMCore.Started;
begin
  FCancel := 0;
end;

function TZMCore.ZipLoadStr(id: Integer): TZMString;
begin
  Result := Master.ZipLoadStr(id);
end;

function TZMCore.ZipMessageDlgEx(const title, msg: String; context: Integer;
  btns: TMsgDlgButtons): TModalResult;
var
  m: String;
begin
  m := msg;
  Result := ZipMessageDialog(title, m, context, btns);
end;

procedure TZMPipeImp.AfterConstruction;
begin
  inherited;
  FStream := nil;
  fSize := 0;
  fDOSDate := Cardinal(DateTimeToFileDate(now));
  fAttributes := 0;
end;

procedure TZMPipeImp.AssignTo(Dest: TZMPipeImp);
begin
  if Dest <> self then
  begin
    Dest.Stream := FStream;
    FStream := nil;
    Dest.Size := FSize;
    Dest.DOSDate := fDOSDate;
    Dest.Attributes := FAttributes;
    Dest.OwnsStream := FOwnsStream;
  end;
end;

procedure TZMPipeImp.BeforeDestruction;
begin
  if OwnsStream and (FStream <> nil) then
    FStream.Free;
  inherited;
end;

function TZMPipeImp.GetAttributes: Cardinal;
begin
  Result := FAttributes;
end;

function TZMPipeImp.GetDOSDate: Cardinal;
begin
  Result := FDOSDate;
end;

function TZMPipeImp.GetFileName: string;
begin
  Result := FFileName;
end;

function TZMPipeImp.GetOwnsStream: boolean;
begin
  Result := FOwnsStream;
end;

function TZMPipeImp.GetSize: Integer;
begin
  Result := FSize;
end;

function TZMPipeImp.GetStream: TStream;
begin
  Result := FStream;
end;

procedure TZMPipeImp.SetAttributes(const Value: Cardinal);
begin
  FAttributes := Value;
end;

procedure TZMPipeImp.SetDOSDate(const Value: Cardinal);
begin
  FDOSDate := Value;
end;

procedure TZMPipeImp.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
  end;
end;

procedure TZMPipeImp.SetOwnsStream(const Value: boolean);
begin
  FOwnsStream := Value;
end;

procedure TZMPipeImp.SetSize(const Value: Integer);
begin
  if Value <> FSize then
  begin
    if FStream = nil then
      FSize := 0
    else
    begin
      if Value > FStream.Size then
        FSize := Integer(FStream.Size)
      else
        FSize := Value;
    end;
  end;
end;

procedure TZMPipeImp.SetStream(const Value: TStream);
begin
  if FStream <> Value then
  begin
    if Value = nil then
      FStream.Free;
    FStream := Value;
    if Value <> nil then
    begin
      FSize := Integer(FStream.Size);
      FStream.Position := 0;
    end;
  end;
end;

function TZMPipeListImp.Add(aStream: TStream; const FileName: string; Own:
    boolean): integer;
var
  tmpPipe: TZMPipe;
begin
  Result := FList.Count;
  tmpPipe := Pipe[Result];
  tmpPipe.Stream := aStream;
  tmpPipe.FileName := FileName;
  tmpPipe.OwnsStream := Own;
end;

procedure TZMPipeListImp.AfterConstruction;
begin
  inherited;
  FList := TList.Create;
end;

procedure TZMPipeListImp.AssignTo(Dest: TZMPipeListImp);
var
  I: Integer;
begin
  if (Dest <> nil) and (Dest <> Self) then
  begin
    Dest.Clear;
    for I := 0 to Count - 1 do
      Dest.FList.Add(FList[i]);
    FList.Clear;
  end;
end;

procedure TZMPipeListImp.BeforeDestruction;
begin
  Clear;
  FList.Free;
  inherited;
end;

procedure TZMPipeListImp.Clear;
var
  i: Integer;
  tmp: TZMPipeImp;
begin
  if (FList <> nil) and (FList.Count > 0) then
  begin
    for I := 0 to FList.Count - 1 do
    begin
     if TObject(FList[i]) is TZMPipeImp then
      begin
        tmp := TZMPipeImp(FList[i]);
        FList[i] := nil;
        tmp.Free;
      end;
    end;
    FList.Clear;
  end;
end;

function TZMPipeListImp.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TZMPipeListImp.GetPipe(Index: Integer): TZMPipe;
const
  __ERR_GE_RangeError = __UNIT__ + (1253 shl 10) + GE_RangeError;
var
  tmpPipe: TZMPipeImp;
begin
  if (Index <0) or (Index > MAX_PIPE) then
    raise EZipMaster.CreateMsgFmt(__ERR_GE_RangeError, [Index, MAX_PIPE]);
  if Index >= FList.Count then
    FList.Count := Index + 1;
   if not (TObject(FList[Index]) is TZMPipeImp) then
   begin
     // need a new one
     tmpPipe := TZMPipeImp.Create;
     FList[Index] := tmpPipe;
   end;
   Result := TZMPipeImp(FList[Index]);
end;

function TZMPipeListImp.HasStream(Index: Integer): boolean;
begin
  Result := (Index >= 0) and (Index < count) and (Pipe[Index].Stream <> nil);
end;

function TZMPipeListImp.KillStream(Index: Integer): boolean;
var
  tmp: TZMPipe;
begin
  Result := False;
  if (Index >= 0) and (Index < count) then
  begin
    tmp := Pipe[Index];
    if tmp.OwnsStream and (tmp.Stream <> nil) then
      tmp.Stream := nil;
  end;
end;

procedure TZMPipeListImp.SetCount(const Value: Integer);
const
  __ERR_GE_RangeError = __UNIT__ + (1290 shl 10) + GE_RangeError;
var
  I: Integer;
begin
  if (Value <0) or (Value > MAX_PIPE) then
    raise EZipMaster.CreateMsgInt(__ERR_GE_RangeError, Value);
  if Value > FList.Count then
  begin
    I := FList.Count;
    while I < Value do
      FList.Add(nil);
  end;
end;

procedure TZMPipeListImp.SetPipe(Index: Integer; const Value: TZMPipe);
const
  __ERR_GE_RangeError = __UNIT__ + (1306 shl 10) + GE_RangeError;
var
  tmpPipe: TZMPipeImp;
begin
  if (Index <0) or (Index > MAX_PIPE) then
    raise EZipMaster.CreateMsgInt(__ERR_GE_RangeError, Index);
  if Index >= FList.Count then
    FList.Count := Index + 1;
  if not (TObject(FList[Index]) is TZMPipeImp) then
    FList[Index] := Value
  else
  begin
    tmpPipe := TZMPipeImp(FList[Index]);
    if Value <> tmpPipe then
    begin
      tmpPipe.Free;
      FList[Index] := Value;
    end;
  end;
end;

end.
