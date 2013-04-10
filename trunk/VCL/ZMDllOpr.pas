unit ZMDllOpr;

//  ZMDllOpr.pas - Dll operations and functions

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
//modified 2011-12-22

interface

{$INCLUDE    '.\ZipVers.inc'}

uses
  Classes, Windows, Controls, Graphics, Dialogs,
  ZMDelZip, ZipMstr, ZMCompat, ZMCore, ZMModOpr, ZMLister;

// {$DEFINE ZDEBUG}

type
  TZMDLLOpr = class;

  TDZCallback = class
  private
    fHoldSize: Integer;
    PCB: PZCallBackStruct;
    function GetActionCode: Integer;
    function GetArg1: Cardinal;
    function GetArg2: Cardinal;
    function GetArg3: Integer;
    function GetFile_Size: Int64;
    function GetIsZip: Boolean;
    function GetMsg: TZMString;
    function GetMsg2: TZMString;
    function GetOwner: TZMDLLOpr;
    function GetWritten: Int64;
    procedure SetArg1(const Value: Cardinal);
    procedure SetArg2(const Value: Cardinal);
    procedure SetArg3(const Value: Integer);
    procedure SetFile_Size(const Value: Int64);
    procedure SetMsg(const Value: TZMString);
  protected
    fHeldData: PByte;
    function Assign(ZCallBackRec: PZCallBackStruct): Integer;
    function CopyData(dst: PByte; MaxSize: Integer): Boolean;
    function HoldData(const src: PByte; size: Cardinal): PByte;
    function HoldString(const src: TZMString): PByte;
    function GetMsgStr(const msg: PByte): TZMString;
    procedure SetComment(const AStr: AnsiString);
    procedure SetData(src: PByte; size: Integer);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Clear;
    property ActionCode: Integer Read GetActionCode;
    property Arg1: Cardinal Read GetArg1 Write SetArg1;
    property Arg2: Cardinal Read GetArg2 Write SetArg2;
    property Arg3: Integer Read GetArg3 Write SetArg3;
    property File_Size: Int64 Read GetFile_Size Write SetFile_Size;
    property IsZip: Boolean Read GetIsZip;
    property msg: TZMString read GetMsg write SetMsg;
    property Msg2: TZMString read GetMsg2;
    property Owner: TZMDLLOpr Read GetOwner;
    property Written: Int64 Read GetWritten;
  end;

  TZMDLLOpr = class(TZMModOpr)
  private
    fAutoAttr: Cardinal;
    fAutoDate: Cardinal;
    FCB: TDZCallback;
    fDLLOperKey: Cardinal;
    fDLLTargetName: String;
    // 1 data for dll held until next callback or fini
    fHeldData: Pointer;
    FPipes: TZMPipeListImp;
    function DLLStreamClose(ZStreamRec: PZStreamRec): Integer;
    function DLLStreamCreate(ZStreamRec: PZStreamRec): Integer;
    function DLLStreamIdentify(ZStreamRec: PZStreamRec): Integer;
    function DllToErrCode(DLL_error: Integer): integer;
    procedure DLL_Comment(var Result: Integer);
    procedure DLL_CRCError(var Result: Integer);
    procedure DLL_Data(var Result: Integer);
    procedure DLL_ExtName(var Result: Integer);
    procedure DLL_Message(var Result: Integer);
    procedure DLL_Overwrite(var Result: Integer);
    procedure DLL_Password(var Result: Integer);
    procedure DLL_Progress(Action: TActionCodes; var Result: Integer);
    procedure DLL_SetAddName(var Result: Integer);
    procedure DLL_Skipped(var Result: Integer);
    function GetAddCompLevel: Integer;
    function GetAddFrom: TDateTime;
    function GetAddStoreSuffixes: TZMAddStoreExts;
    function GetDllErrCode: Cardinal;
    function GetDLL_Load: Boolean;
    function GetExtAddStoreSuffixes: String;
    function GetExtrBaseDir: String;
    function GetExtrOptions: TZMExtrOpts;
    function GetPassword: String;
    function GetPasswordReqCount: Integer;
    function GetRootDir: String;
    procedure GrabPipes;
    function IsDestWritable(const fname: String; AllowEmpty: Boolean): Boolean;
    function RejoinMVArchive(var TmpZipName: String): Integer;
    procedure ReportMessage1(err: Integer; const msg: TZMString);
    procedure SetCB(const Value: TDZCallback);
    procedure SetDllErrCode(const Value: Cardinal);
    procedure SetDLL_Load(const Value: Boolean);
    procedure SetExtrOptions(const Value: TZMExtrOpts);
    procedure SetPasswordReqCount(const Value: Integer);
    procedure SetRootDir(const Value: String);
  protected
    fAutoStream: TStream;
    function AddStoreExtStr(Options: TZMAddStoreExts): String;
    function AllocDLLCommand(const FileName: String): pDLLCommands;
    procedure CancelSet(Value: Integer); override;
    procedure DestroyDLLCmd(var rec: pDLLCommands);
    function DLLCallback(ZCallBackRec: PZCallBackStruct): Integer;
    function DLLStreamOp(op: TZStreamActions; ZStreamRec: PZStreamRec): Integer;
    procedure DLL_Arg(var Result: Integer);
    procedure ExtAdd;
    procedure ExtExtract;
    procedure Finished(WasGood: boolean); override;
    function SetupUnzCmd(const Value: String): pDLLCommands;
    function SetupZipCmd(const Value: String): pDLLCommands;
    procedure Started; override;
    property AddCompLevel: Integer read GetAddCompLevel;
    property AddFrom: TDateTime read GetAddFrom;
    property AddStoreSuffixes: TZMAddStoreExts read GetAddStoreSuffixes;
    property CB: TDZCallback read FCB write SetCB;
    property DllErrCode: Cardinal read GetDllErrCode write SetDllErrCode;
    property DLLTargetName: String read fDLLTargetName write fDLLTargetName;
    property ExtAddStoreSuffixes: String read GetExtAddStoreSuffixes;
    property ExtrBaseDir: String read GetExtrBaseDir;
    property ExtrOptions: TZMExtrOpts read GetExtrOptions write SetExtrOptions;
    property Password: String read GetPassword;
    property PasswordReqCount: Integer read GetPasswordReqCount write
        SetPasswordReqCount;
    property RootDir: String read GetRootDir write SetRootDir;
  public
    procedure AbortDLL;
    function Add: Integer;
    procedure AddStreamToFile(const FileName: String;
      FileDate, FileAttr: Dword);
    procedure AddStreamToStream(InStream: TMemoryStream);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Deflate(OutStream, InStream: TStream; Length: Int64; var Method:
        TZMDeflates; var crc: Cardinal): Integer; override;
    procedure Extract;
    procedure ExtractFileToStream(const FileName: String);
    procedure ExtractStreamToStream(InStream: TMemoryStream; OutSize: Longword);
    procedure Undeflate(OutStream, InStream: TStream; Length: Int64; var Method:
        tzMDeflates; var crc: Cardinal);
    property DLL_Load: Boolean read GetDLL_Load write SetDLL_Load;
    property Pipes: TZMPipeListImp read FPipes;
  end;

implementation

uses
  SysUtils, Forms, ZMMsg, ZMXcpt, ZMUtils, ZMMsgStr, ZMCtx,
  ZMDlg, ZMZipFile, ZMCenDir, ZMDrv, ZMStructs, ZMUTF8,
  ZMDllLoad, ZMIRec, ZMWFuncs, ZMWorkFile;

const
  __UNIT__ = 12 shl 23;

(* ? ZCallback
  1.76 01 May 2004 RP change return type and value to return flag for exception
  1.76 24 April 2004 RP use DLLCallback
  1.73 ( 1 June 2003) changed for new callback
  { Dennis Passmore (Compuserve: 71640,2464) contributed the idea of passing an
  instance handle to the DLL, and, in turn, getting it back from the callback.
  This lets us referance variables in the TZMDLLOpr class from within the
  callback function.  Way to go Dennis!
  Modified by Russell Peters }
*)
function ZCallback(ZCallBackRec: PZCallBackStruct): Longint; stdcall;
begin
  Result := CALLBACK_ERROR;
  if ZCallBackRec^.Check = ZCallBack_Check then
  begin
    with TObject(ZCallBackRec^.Caller) as TZMDLLOpr do
      Result := DLLCallback(ZCallBackRec);
  end;
end;

function ZStreamCallback(ZStreamRec: PZStreamRec): Longint; stdcall;
{$IFNDEF VERD6up}
const
  _ERR_DS_SeekError = __UNIT__ + (310 shl 10) + DS_SeekError;   
{$ENDIF}
var
  cnt: Integer;
  op: TZStreamActions;
  Strm: TStream;
begin
  Result := CALLBACK_ERROR;
  try
    if ZStreamRec^.Check = ZStream_Check then
    begin
      with ZStreamRec^ do
      begin
        op := TZStreamActions(OpCode);
        Result := 0;
        case op of
          zsaIdentify .. zsaClose:
            with TObject(ZStreamRec^.Caller) as TZMDLLOpr do
              Result := DLLStreamOp(op, ZStreamRec);
          zsaPosition: // reposition
            begin
{$IFNDEF VERD6up}
              if Integer(ArgLL) <> ArgLL then
                raise EZipMaster.CreateMsgDisp(_ERR_DS_SeekError, true);
{$ENDIF}
              Strm := TObject(StrmP) as TStream;
              ArgLL := Strm.Seek(ArgLL, ArgI);
              if ArgLL >= 0 then
                Result := CALLBACK_TRUE;
            end;
          zsaRead: // read
            begin
              Strm := TObject(StrmP) as TStream;
              cnt := ArgI;
              if (Strm.Position + cnt) > Strm.size then
                cnt := Integer(Strm.size - Strm.Position);
              ArgI := Strm.Read(BufP^, cnt);
              if ArgI = cnt then
                Result := CALLBACK_TRUE;
            end;
          zsaWrite: // Write
            begin
              Strm := TObject(StrmP) as TStream;
              cnt := ArgI;
              ArgI := Strm.Write(BufP^, cnt);
              if ArgI = cnt then
                Result := CALLBACK_TRUE;
            end;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      // clear any exceptions
      Result := CALLBACK_ERROR;
    end;
  end;
end;

procedure TZMDLLOpr.AbortDLL;
begin
  if fDLLOperKey <> 0 then
    _DLL_Abort(Master, fDLLOperKey);
end;

(* ? TZMDLLOpr.Add
*)
function TZMDLLOpr.Add: Integer;
begin
  fAutoStream := nil;
  ExtAdd;
  Result := ErrCode;
end;

(* ? TZMDLLOpr.AddStoreExtStr
*)
function TZMDLLOpr.AddStoreExtStr(Options: TZMAddStoreExts): String;
const
  SuffixStrings: array [TZMAddStoreSuffixEnum] of PChar =
    ('gif', 'png', 'z', 'zip', 'zoo', 'arc', 'lzh', 'arj', 'taz', 'tgz', 'lha',
    'rar', 'ace', 'cab', 'gz', 'gzip', 'jar', 'exe', '', 'jpg', 'jpeg', '7zp',
    'mp3', 'wmv', 'wma', 'dvr-ms', 'avi');
var
  o: TZMAddStoreSuffixEnum;
begin
  Result := '';
  for o := Low(TZMAddStoreSuffixEnum) to High(TZMAddStoreSuffixEnum) do
    if (o <> assEXT) and (o in Options) then
      Result := Result + '.' + String(SuffixStrings[o]) + ':';
  if assEXT in Options then
    Result := Result + ExtAddStoreSuffixes;
end;

(* ? TZMDLLOpr.AddStreamToFile
  //'FileName' is the name you want to use in the zip file to
  // store the contents of the stream under.
*)
procedure TZMDLLOpr.AddStreamToFile(const FileName: String;
  FileDate, FileAttr: Dword);
const
  __ERR_AD_NothingToZip = __UNIT__ + (345 shl 10) + AD_NothingToZip;
//  __ERR_AD_InvalidName = __UNIT__ + (349 shl 10) + AD_InvalidName;
  __ERR_BadName = __UNIT__ + (351 shl 10) + AD_BadFileName; // 23/08/2012 8:48:50 AM
var
  FatDate: Word;
  FatTime: Word;
  fn: String;
  ft: TFileTime;
  st: TSystemTime;
begin
  fn := Trim(FileName);
  if (Length(FileName) = 0) and (IncludeSpecs.Count > 0) then
    fn := Trim(IncludeSpecs[0]);
  if (fn = '') or (ZipStream.size = 0) then
  begin
    ShowZipMessage(__ERR_AD_NothingToZip, '');
    exit;
  end;
  // 24/08/2012 strip drive etc like 1.79
  if ExtractFileDrive(fn) <> '' then
    fn := Copy(fn, 3, Length(fn) - 2);
  if (fn <> '') and ((fn[1] = '/') or (fn[1] = '\')) then
    fn := Copy(fn, 2, Length(fn) -1);
//  if IsWild(fn) then
  if NameIsBad(fn, false) then  // 23/08/2012 8:48:57 AM
  begin
    ShowZipMessage(__ERR_BadName, ' : ' + fn); // 23/08/2012 8:49:03 AM
//    ShowZipMessage(__ERR_AD_InvalidName, '');
    exit;
  end;
  IncludeSpecs.Clear();

  IncludeSpecs.Add('0:' + fn);
  if FileDate = 0 then
  begin
    GetLocalTime(st);
    SystemTimeToFileTime(st, ft);
    FileTimeToDosDateTime(ft, FatDate, FatTime);
    FileDate := (Dword(FatDate) shl 16) + FatTime;
  end;
  fAutoStream := ZipStream;
  fAutoDate := FileDate;
  fAutoAttr := FileAttr;
  ExtAdd;
end;

(* ? TZMDLLOpr.AddStreamToStream
*)
procedure TZMDLLOpr.AddStreamToStream(InStream: TMemoryStream);
const
  __ERR_AD_InIsOutStream = __UNIT__ + (381 shl 10) + AD_InIsOutStream;
  __ERR_DS_NoEncrypt = __UNIT__ + (388 shl 10) + DS_NoEncrypt;
  __ERR_AD_NothingToZip = __UNIT__ + (408 shl 10) + AD_NothingToZip;
var
  Header: TZM_StreamHeader;
  Method: TZMDeflates;
begin
  if InStream = ZipStream then
  begin
    ShowZipMessage(__ERR_AD_InIsOutStream, '');
    exit;
  end;
  if assigned(InStream) and (InStream.size > 0) then
  begin
    if AddEncrypt in AddOptions then
    begin
      ShowZipMessage(__ERR_DS_NoEncrypt, '');
      exit;
    end;
    ZipStream.size := 0;
    Method := zmDeflate;
    Header.Method := METHOD_DEFLATED;
    Header.CRC := 0;
    ZipStream.WriteBuffer(Header, SizeOf(Header));
    Deflate(ZipStream, InStream, -1, Method, Header.CRC);
    if SuccessCnt = 1 then
    begin
      ZipStream.Position := 0;
      if Method <> zmDeflate then
        Header.Method := METHOD_STORED; // was stored
      ZipStream.WriteBuffer(Header, SizeOf(Header));
    end
    else
      ZipStream.size := 0;
  end
  else
    ShowZipMessage(__ERR_AD_NothingToZip, '');
end;

procedure TZMDLLOpr.AfterConstruction;
begin
  inherited;
  fDLLOperKey := 0;
  fHeldData := nil;
  fCB := TDZCallback.Create;
  FPipes := TZMPipeListImp.Create;
end;

function TZMDLLOpr.AllocDLLCommand(const FileName: String): pDLLCommands;
var
  Opts: Cardinal;
begin
  Result := AllocMem(SizeOf(TDLLCommands));
  DLLTargetName := FileName;
  ZeroMemory(Result, SizeOf(TDLLCommands));
  Result^.fVersion := DELZIPVERSION; // version we expect the DLL to be
  Result^.fCaller := self; // point to our VCL instance; returned in Report

  Result^.ZCallbackFunc := ZCallback; // pass addr of function to be called from DLL
  Result^.ZStreamFunc := ZStreamCallback;
  Result^.fEncodedAs := Ord(Encoding); // how to interpret existing names
  Result^.fFromPage := Encoding_CP;

  if Verbosity >= zvTrace then
    Result^.fVerbosity := -1
  else if Verbosity >= zvVerbose then
    Result^.fVerbosity := 1
  else
    Result^.fVerbosity := 0;
  { if tracing, we want verbose also }

  // used for dialogs (like the pwd dialogs)
  if Unattended then
    Result^.fHandle := 0
  else
    Result^.fHandle := Handle;
  Result^.fSS := nil;

  Opts := DLL_OPT_Quiet; // no DLL error reporting

  Result^.fOptions := Opts;
end;

procedure TZMDLLOpr.BeforeDestruction;
begin
  fIsDestructing := true; // stop callbacks
  AbortDLL;
  if fHeldData <> nil then
  begin
    FreeMem(fHeldData); // release held data
    fHeldData := nil;
  end;
  FreeAndNil(fCB);
  FPipes.Clear;
  FreeAndNil(FPipes);
  inherited;
end;

procedure TZMDLLOpr.CancelSet(Value: Integer);
begin
  AbortDLL; // is this too soon
end;

function TZMDLLOpr.Deflate(OutStream, InStream: TStream; Length: Int64; var
    Method: TZMDeflates; var crc: Cardinal): Integer;
const
  __ERR_DS_NoInStream = __UNIT__ + (489 shl 10) + DS_NoInStream;
  __ERR_DS_NoOutStream = __UNIT__ + (492 shl 10) + DS_NoOutStream;
  __ERR_AD_InIsOutStream = __UNIT__ + (495 shl 10) + AD_InIsOutStream;
  __ERR_AD_UnattPassword = __UNIT__ + (501 shl 10) + AD_UnattPassword;
  __ERR_LD_NoDLL = __UNIT__ + (514 shl 10) + LD_NoDLL;
var
  Args: TZSSArgs;
  CmdRecP: pDLLCommands;
  i: Integer;
  ncrypt: boolean;
begin
  Result := -__ERR_DS_NoInStream;
  if not assigned(InStream) then
    exit;
  Result := -__ERR_DS_NoOutStream;
  if not assigned(OutStream) then
    exit;
  Result := -__ERR_AD_InIsOutStream;
  if InStream = ZipStream then
    exit;
  CmdRecP := nil;
  ncrypt := (Method = zmStoreEncrypt) or (Method = zmDeflateEncrypt);
  // We can not do an Unattended Add if we don't have a password.
  Result := -__ERR_AD_UnattPassword;
  if Unattended and ncrypt and (Password = '') then
    exit;
  if Length < 0 then
    Length := InStream.size;
  if (Method = zmDeflate) or (Method = zmDeflateEncrypt) then
    Args.Method := 8
  else
    Args.Method := 0;
  Args.fSSInput := InStream;
  Args.fSSOutput := OutStream;
  Args.size := Length;
  Args.crc := crc;
  Result := -__ERR_LD_NoDLL;
  if _DLL_Load(self) <= 0 then
    exit;
  try
    if ncrypt then
      AddOptions := AddOptions + [addEncrypt]
    else
      AddOptions := AddOptions - [addEncrypt];
    CmdRecP := SetupZipCmd('');
    CmdRecP^.fSS := @Args;
    fEventErr := ''; // added
    { pass in a ptr to parms }
    i := _DLL_Exec(self, CmdRecP, fDLLOperKey);
  finally
    _DLL_Unload(self);
    DestroyDLLCmd(CmdRecP);
  end;
  if i = 1 then
  begin // success
    Result := 0;
    SuccessCnt := 1;
    if Args.Method = 8 then
      Method := zmDeflate
    else
      Method := zmStore;
    crc := Args.crc;
  end;
end;

procedure TZMDLLOpr.DestroyDLLCmd(var rec: pDLLCommands);
begin
  if rec <> nil then
  begin
    FreeMem(rec);
    rec := nil;
  end;
end;

(* ? TZMDLLOpr.DLLCallback
*)
function TZMDLLOpr.DLLCallback(ZCallBackRec: PZCallBackStruct): Integer;
var
  Action: TActionCodes;
begin
  Result := CALLBACK_UNHANDLED;
  if fIsDestructing then // in destructor return
  begin
    exit;
  end;
  CB.Assign(ZCallBackRec);
  Action := TActionCodes(CB.ActionCode and 63);
  try
    case Action of
      zacMessage:
        DLL_Message(Result);
      zacItem .. zacXProgress:
        DLL_Progress(Action, Result);
      zacNewName:
        // request for a new path+name just before zipping or extracting
        DLL_SetAddName(Result);
      zacPassword:
        // New or other password needed during Extract()
        DLL_Password(Result);
      zacCRCError:
        // CRC32 error, (default action is extract/test the file)
        DLL_CRCError(Result);
      zacOverwrite:
        // Extract(UnZip) Overwrite ask
        DLL_Overwrite(Result);
      zacSkipped:
        // Extract(UnZip) and Skipped
        DLL_Skipped(Result);
      zacComment:
        // Add(Zip) FileComments.
        DLL_Comment(Result);
      zacData:
        // Set Extra Data
        DLL_Data(Result);
      zacExtName:
        // request for a new path+name just before zipping or extracting
        DLL_ExtName(Result);
      zacKey:
        begin
          fDLLOperKey := CB.Arg1;
          Result := 0;
        end;
      zacArg:
        DLL_Arg(Result);
    else
      Result := CALLBACK_IGNORED; // unknown
    end; { end case }
    if (Action < zacKey) and (Action > zacMessage) then
    begin
      KeepAlive;
    end;
    if Cancel <> 0 then
    begin
      Result := CALLBACK_CANCEL;
      ReportToSniffer(0, '[CANCEL sent]');
    end;
  except
    on E: Exception do
    begin
      if fEventErr = '' then
        // catch first exception only
        fEventErr := ' #' + IntToStr(Ord(Action)) + ' "' + E.Message + '"';
      Cancel := GE_Except;
      Result := CALLBACK_EXCEPTION;
      ReportToSniffer(0, '[CALLBACK Exception sent] ' + fEventErr);
    end;
  end;
end;

function TZMDLLOpr.DLLStreamClose(ZStreamRec: PZStreamRec): Integer;
var
  IsDone: Boolean;
  SNumber: Integer;
  Strm: TStream;
  tmpOnStream: TZMStreamEvent;
  zstats: TZMSStats;
begin
  zstats.size := 0;
  zstats.Date := 0;
  zstats.Attrs := 0;
  Result := CALLBACK_UNHANDLED;
  if TObject(ZStreamRec^.StrmP) is TStream then
  begin
    Strm := TStream(ZStreamRec^.StrmP);
    if Strm = ZipStream then
    begin
      fAutoStream := nil;
      ZStreamRec^.StrmP := nil;
      Result := CALLBACK_TRUE;
    end
    else
    begin
          IsDone := False;
      tmpOnStream := Master.OnStream;
      SNumber := ZStreamRec^.Number;
      if assigned(tmpOnStream) then
      begin
        if (Strm <> ZipStream) then
        begin
          tmpOnStream(Master, zsoClose, SNumber, Strm, zstats, IsDone);
          if IsDone then
          begin
            Result := CALLBACK_TRUE;
            ZStreamRec^.StrmP := Strm;
          end;
        end;
      end;
    if (not IsDone) and FPipes.HasStream(SNumber) then
      begin
        FPipes.KillStream(SNumber);
        Result := CALLBACK_TRUE;
      end;
    end;
  end;
end;

function TZMDLLOpr.DLLStreamCreate(ZStreamRec: PZStreamRec): Integer;
var
  IsDone: Boolean;
  pipe: TZMPipe;
  SNumber: Integer;
  Strm: TStream;
  tmpOnStream: TZMStreamEvent;
  zstats: TZMSStats;
begin
  zstats.size := 0;
  zstats.Date := 0;
  zstats.Attrs := 0;
  Result := CALLBACK_UNHANDLED;
  ZStreamRec^.StrmP := nil;
  if assigned(fAutoStream) then
  begin
    Result := CALLBACK_TRUE;
    ZStreamRec^.StrmP := fAutoStream;
    fAutoStream.Position := 0;
  end
  else
  begin
    IsDone := False;
    tmpOnStream := Master.OnStream;
    SNumber := ZStreamRec^.Number;
    if assigned(tmpOnStream) then
    begin
      IsDone := False;
      tmpOnStream(Master, zsoOpen, SNumber, Strm, zstats, IsDone);
      if IsDone and assigned(Strm) then
      begin
        Result := CALLBACK_TRUE;
        ZStreamRec^.StrmP := Strm;
      end;
    end;
    if (not IsDone) and FPipes.HasStream(SNumber) then
    begin
      pipe := FPipes[SNumber];
      Result := CALLBACK_TRUE;
      ZStreamRec^.StrmP := pipe.Stream;
    end;
  end;
end;

function TZMDLLOpr.DLLStreamIdentify(ZStreamRec: PZStreamRec): Integer;
var
  IsDone: Boolean;
  pipe: TZMPipe;
  SNumber: Integer;
  Strm: TStream;
  tmpOnStream: TZMStreamEvent;
  zstats: TZMSStats;
begin
  zstats.size := 0;
  zstats.Date := 0;
  zstats.Attrs := 0;
  Result := CALLBACK_UNHANDLED;
  if assigned(fAutoStream) then
  begin
    Result := CALLBACK_TRUE;
    ZStreamRec^.ArgLL := fAutoStream.size;
    ZStreamRec^.ArgD := fAutoDate;
    ZStreamRec^.ArgA := fAutoAttr;
  end
  else
  begin
      IsDone := False;
    tmpOnStream := Master.OnStream;
    SNumber := ZStreamRec^.Number;
    if assigned(tmpOnStream) then
    begin
      tmpOnStream(Master, zsoIdentify, SNumber, Strm, zstats, IsDone);
      if IsDone then
      begin
        Result := CALLBACK_TRUE;
        ZStreamRec^.ArgLL := zstats.size;
        ZStreamRec^.ArgD := zstats.Date;
        ZStreamRec^.ArgA := zstats.Attrs;
      end;
    end;
    if (not IsDone) and FPipes.HasStream(SNumber) then
    begin
      pipe := FPipes[SNumber];
      Result := CALLBACK_TRUE;
      ZStreamRec^.ArgLL := pipe.size;
      ZStreamRec^.ArgD := pipe.DOSDate;
      ZStreamRec^.ArgA := pipe.Attributes;
    end;
  end;
end;

// ALL interface structures BYTE ALIGNED
(* stream operation arg usage
  zacStIdentify,
  //      IN BufP = name
  IN Number = number
  OUT ArgLL = size, ArgD = Date, ArgA = Attrs
  zacStCreate,
  //      IN BufP = name
  IN Number = number
  OUT StrmP = stream
  zacStClose,
  IN Number = number
  IN StrmP = stream
  OUT StrmP = stream (= NULL)
  zacStPosition,
  IN Number = number
  IN StrmP = stream, ArgLL = offset, ArgI = from
  OUT ArgLL = position
  zacStRead,
  IN Number = number
  IN StrmP = stream, BufP = buf, ArgI = count
  OUT ArgI = bytes read
  zacStWrite
  IN Number = number
  IN StrmP = stream, BufP = buf, ArgI = count
  OUT ArgI = bytes written
*)
function TZMDLLOpr.DLLStreamOp(op: TZStreamActions; ZStreamRec: PZStreamRec)
  : Integer;
begin
  Result := CALLBACK_UNHANDLED;
  case op of
    zsaIdentify: // get details for named stream
        Result := DLLStreamIdentify(ZStreamRec);
    zsaCreate: // Assign a stream
        Result := DLLStreamCreate(ZStreamRec);
    zsaClose: // defaults to freeing stream if not ZipStream
        Result := DLLStreamClose(ZStreamRec);
  end;
  if Verbosity >= zvVerbose then
  begin
    Diag(Format('Stream operation %d on %d returns %d',
        [Ord(op), ZStreamRec^.Number, Result]));
  end;
end;

// return proper ErrCode for dll error
function TZMDLLOpr.DllToErrCode(DLL_error: Integer): integer;
begin
  Result := DLL_error and $3F;
  if Result <> 0 then
    Result := DZ_RES_GOOD + Result;
  if Result > DZ_ERR_DUPNAME then
    Result := DZ_RES_ERROR;
end;

(* Arg1 = argument
  0 = filename
  1 = password
  2 = RootDir
  3 = ExtractDir
  4 = Zip comment
  5 = FSpecArgs      Arg3 = index
  6 = FSpecArgsExcl  Arg3 = index
*)
procedure TZMDLLOpr.DLL_Arg(var Result: Integer);
var
  Arg: TCBArgs;
  idx: Integer;
  sr: TZMString;
begin
  if CB.Arg1 <= Cardinal(Ord( HIGH(TCBArgs))) then
  begin
    Arg := TCBArgs(CB.Arg1);
    idx := CB.Arg3;
    sr := '';
    if (Arg in [zcbFSpecArgs, zcbFSpecArgsExcl]) and (idx < 0) then
      Result := CALLBACK_ERROR
    else
    if Arg = zcbComment then
    begin // always Ansi
      CB.SetComment(ZipComment);
      Result := CALLBACK_TRUE;
    end
    else
    begin
      Result := CALLBACK_TRUE;
      case Arg of
        zcbFilename:
          sr := DLLTargetName;
        zcbPassword:
          sr := Password;
        zcbRootDir:
          sr := RootDir;
        zcbExtractDir:
          sr := ExtrBaseDir;
        zcbFSpecArgs:
          begin
            if idx >= IncludeSpecs.Count then
              Result := CALLBACK_UNHANDLED
            else
              sr := IncludeSpecs[idx];
            CB.Arg3 := IncludeSpecs.Count;
          end;
        zcbFSpecArgsExcl:
          begin
            if idx >= ExcludeSpecs.Count then
              Result := CALLBACK_UNHANDLED
            else
              sr := ExcludeSpecs[idx];
            CB.Arg3 := ExcludeSpecs.Count;
          end;
        zcbSpecials:
          sr := AddStoreExtStr(AddStoreSuffixes);
        zcbTempPath:
          sr := TempDir;
      end;
      CB.msg := sr;
    end;
  end
  else
    Result := CALLBACK_ERROR;
end;

procedure TZMDLLOpr.DLL_Comment(var Result: Integer);
var
  FileComment: TZMString;
  IsChanged: Boolean;
  ti: Integer;
  tmpFileComment: TZMFileCommentEvent;
begin
  tmpFileComment := Master.OnFileComment;
  if assigned(tmpFileComment) then
  begin
    FileComment := CB.Msg2;
    IsChanged := False;
    tmpFileComment(Master, CB.msg, FileComment, IsChanged);
    if IsChanged then
    begin
      Result := CALLBACK_TRUE;
      ti := Length(FileComment);
      if ti > 255 then
      begin
        ti := 255;
        FileComment := Copy(FileComment, 1, 255);
      end;
      CB.msg := FileComment;
      CB.Arg1 := ti;
    end;
  end;
  if (Cancel <> 0) and (Result >= CALLBACK_IGNORED) then
    Result := CALLBACK_CANCEL;
end;

procedure TZMDLLOpr.DLL_CRCError(var Result: Integer);
var
  DoExtract: Boolean;
  tmpCRC32Error: TZMCRC32ErrorEvent;
begin
  DoExtract := true;
  tmpCRC32Error := Master.OnCRC32Error;
  if assigned(tmpCRC32Error) then
  begin
    tmpCRC32Error(Master, CB.msg, CB.Arg1, CB.Arg2, DoExtract);
    if DoExtract then
      Result := CALLBACK_TRUE
    else
      Result := CALLBACK_3;
  end;
end;

procedure TZMDLLOpr.DLL_Data(var Result: Integer);
var
  dat: TZMRawBytes;
  DataChanged: Boolean;
  DatSize: Int64;
  IsChanged: Boolean;
  LevelChanged: Boolean;
  lvl: Integer;
  tmpFileExtra: TZMFileExtraEvent;
  tmpSetCompLevel: TZMSetCompLevel;
  xlen: Integer;
begin
  tmpFileExtra := Master.OnFileExtra;
  tmpSetCompLevel := Master.OnSetCompLevel;
  LevelChanged := False;
  DataChanged := False;
  if assigned(tmpSetCompLevel) then
  begin
    IsChanged := False;
    lvl := Integer(CB.Arg2);
    tmpSetCompLevel(Master, CB.msg, lvl, IsChanged);
    if IsChanged and (lvl in [0 .. 9]) then
    begin
      CB.Arg2 := lvl;
      LevelChanged := true;
    end;
  end;
  if assigned(tmpFileExtra) then
  begin
    DatSize := CB.Arg1; // old size
    SetLength(dat, DatSize);
    if DatSize > 0 then
      CB.CopyData(PByte(@dat[1]), DatSize);
    IsChanged := False;
    tmpFileExtra(Master, CB.msg, dat, IsChanged);
    if IsChanged then
    begin
      DataChanged := true;
      xlen := Length(dat);
      if xlen > 2047 then // limit
        xlen := 2047;
      CB.SetData(PByte(@dat[1]), xlen);
    end;
  end;
  if DataChanged then
  begin
    if LevelChanged then
      Result := CALLBACK_3
    else
      Result := CALLBACK_TRUE;
  end
  else
  begin
    if LevelChanged then
      Result := CALLBACK_2;
  end;
end;

procedure TZMDLLOpr.DLL_ExtName(var Result: Integer);
var
  BaseDir: TZMString;
  IsChanged: Boolean;
  msg: TZMString;
  OldFileName: TZMString;
  tmpSetExtName: TZMSetExtNameEvent;

  function IsPathOnly(const f: String): Boolean;
  var
    c: Char;
  begin
    Result := False;
    if f <> '' then
    begin
      c := f[Length(f)];
      if (c = PathDelim) or (c = PathDelimAlt) then
        Result := true;
    end;
  end;

begin
  tmpSetExtName := Master.OnSetExtName;
  if assigned(tmpSetExtName) then
  begin
    msg := CB.Msg2;
    BaseDir := SetSlashW(msg, psdExternal);
    msg := CB.msg;
    OldFileName := msg;
    IsChanged := False;
    tmpSetExtName(Master, OldFileName, BaseDir, IsChanged);
    if IsChanged and (OldFileName <> msg) and
      (IsPathOnly(OldFileName) = IsPathOnly(msg)) then
    begin
      CB.msg := OldFileName;
      Result := CALLBACK_TRUE;
    end;
  end;
end;

procedure TZMDLLOpr.DLL_Message(var Result: Integer);
const
  __ERR_GE_EventEx = __UNIT__ + (1060 shl 10) + GE_EventEx;
  __ERR_DLL = __UNIT__ + (1064 shl 10);
var
  ECode: Integer;
  Erm: TZMString;
  ErrorCode: Integer;
  EType: Integer;
  ExtCode: Cardinal;
begin
  Erm := CB.msg;
  ErrorCode := CB.Arg1;
  if ErrorCode <> 0 then
    ExtCode := Cardinal(ErrorCode) or $80000000
  else
    ExtCode := 0;
  if (ErrorCode <> 0) and (DllErrCode = 0) then
    DllErrCode := ExtCode;   // remember last error
  ECode := DllToErrCode(ErrorCode);
  EType := ErrorCode and DZM_Type_Mask;
  if (EType >= DZM_Message) and ((ErrorCode and DZM_MessageBit) <> 0) then
    Erm := ZipLoadStr(ECode) + Erm;
  if (ECode <> 0) and (ErrCode = 0) then // W'll always keep the last ErrorCode
  begin
    if (fEventErr <> '') and (ECode = _DZ_ERR_ABORT) then
      Erm := ZipFmtLoadStr(__ERR_GE_EventEx, [fEventErr]);
  end;
  ReportToSniffer(ExtCode, Erm);
  if ECode <> 0 then
    ECode := ECode + __ERR_DLL;
  ReportMessage1(ECode, Erm);
end;

procedure TZMDLLOpr.DLL_Overwrite(var Result: Integer);
var
  DoOverwrite: Boolean;
  tmpExtractOverwrite: TZMExtractOverwriteEvent;
begin
  tmpExtractOverwrite := Master.OnExtractOverwrite;
  if assigned(tmpExtractOverwrite) then
  begin
    DoOverwrite := CB.Arg1 <> 0;
    tmpExtractOverwrite(Master, CB.msg, CB.Arg3 <> 2, DoOverwrite, CB.Arg2);
    if DoOverwrite then
      Result := CALLBACK_TRUE
    else
      Result := CALLBACK_2;
    ReportToSniffer
        (0, Format('[Overwrite] IN=%d,%d OUT=%d', [CB.Arg1, CB.Arg2, Result]));
  end;
end;

procedure TZMDLLOpr.DLL_Password(var Result: Integer);
var
  IsZip: Boolean;
  pwd: String;
  Response: TmsgDlgBtn;
  RptCount: Longword;
  tmpPasswordError: TZMPasswordErrorEvent;
begin
  pwd := '';
  RptCount := CB.Arg1;
  Response := mbOK;
  IsZip := CB.IsZip;
  tmpPasswordError := Master.OnPasswordError;
  if assigned(tmpPasswordError) then
  begin
    tmpPasswordError(Master, IsZip, pwd, CB.msg, RptCount, Response);
    if Response <> mbOK then
      pwd := '';
  end
  else if IsZip then
    pwd := Master.GetAddPassword(Response)
  else
    pwd := Master.GetExtrPassword(Response);

  if pwd <> '' then
  begin
    CB.msg := pwd;
    Result := CALLBACK_TRUE;
  end
  else
  begin // no password
    RptCount := 0;
    Result := CALLBACK_2;
  end;
  if RptCount > 15 then
    RptCount := 15;
  CB.Arg1 := RptCount;
  if Response = mbCancel then // Cancel
  begin
    Result := CALLBACK_2;
  end
  else if Response = mbNoToAll then // Cancel all
  begin
    Result := CALLBACK_3;
  end
  else if Response = mbAbort then // Abort
  begin
    Cancel := GE_Abort;
    Result := CALLBACK_ABORT;
  end;
end;

procedure TZMDLLOpr.DLL_Progress(Action: TActionCodes; var Result: Integer);
var
  ErrorCode: Integer;
  File_Size: Int64;
  M: String;
begin
  ErrorCode := 0;
  File_Size := 0;
  M := '';
  if (Action > zacTick) and (Action <= zacXProgress) then
    File_Size := CB.File_Size;
  if (Action = zacItem) or (Action = zacXItem) then
    M := CB.msg;
  case Action of
    zacItem .. zacEndOfBatch:
        ProgDetail.Written(CB.Written);
    zacCount:
      File_Size := CB.Arg1;
    zacXItem, zacXProgress:
        ErrorCode := CB.Arg1;
  end;
  ReportProgress(Action, ErrorCode, M, File_Size);
  Result := 0;
  if (Action = zacItem) and (File_Size = -1) and (ProgDetail.Stop)  then
    Result := CALLBACK_TRUE;
end;

procedure TZMDLLOpr.DLL_SetAddName(var Result: Integer);
var
  IsChanged: Boolean;
  M: String;
  M2: String;
  OldFileName: TZMString;
  OrigName: TZMString;
  tmpSetAddName: TZMSetAddNameEvent;
begin
  tmpSetAddName := Master.OnSetAddName;
  if assigned(tmpSetAddName) then
  begin
    M := CB.msg; // saves OldFileName
    M2 := CB.Msg2;
    if assigned(tmpSetAddName) then
    begin
      OrigName := SetSlashW(M2, psdExternal);
      OldFileName := M;
      IsChanged := False;

      tmpSetAddName(Master, OldFileName, OrigName, IsChanged);
      if IsChanged then
      begin
        CB.msg := OldFileName;
        Result := CALLBACK_TRUE;
      end;
    end;
  end;
end;

procedure TZMDLLOpr.DLL_Skipped(var Result: Integer);
var
  ErrorCode: Integer;
  ti: Integer;
begin
  ErrorCode := CB.Arg1; // error
  if ErrorCode <> 0 then
    DllErrCode := DllToErrCode(ErrorCode);
  ti := CB.Arg2; // type
  if ReportSkipping(CB.msg, DllToErrCode(ErrorCode), TZMSkipTypes(pred(ti and MAX_BYTE))) then
    Result := CALLBACK_TRUE;
end;

(* ? TZMDLLOpr.ExtAdd
*)
procedure TZMDLLOpr.ExtAdd;
const
  __ERR_AD_UnattPassword = __UNIT__ + (1235 shl 10) + AD_UnattPassword;
  __ERR_GE_NoZipSpecified = __UNIT__ + (1239 shl 10) + GE_NoZipSpecified;
  __ERR_AD_NothingToZip = __UNIT__ + (1243 shl 10) + AD_NothingToZip;
  __ERR_DS_NoUnattSpan = __UNIT__ + (1254 shl 10) + DS_NoUnattSpan;
  __ERR_AD_NothingToZip1 = __UNIT__ + (1257 shl 10) + AD_NothingToZip;
  __ERR_AD_NoDestDir = __UNIT__ + (1267 shl 10) + AD_NoDestDir;
  __ERR_DS_NotChangeable = __UNIT__ + (1272 shl 10) + DS_NotChangeable;
  __ERR_AD_AutoSFXWrong = __UNIT__ + (1295 shl 10) + AD_AutoSFXWrong;
  __ERR_GE_FatalZip = __UNIT__ + (1330 shl 10) + GE_FatalZip;
var
  CmdRecP: pDLLCommands;
  curz: TZMZipFile;
  MultiDisk: Boolean;
  ret: Integer;
  TmpZipName: String;
begin
//  { Make sure we can't get back in here while work is going on }
  CmdRecP := nil;
  MultiDisk := zwoDiskSpan in WriteOptions;
  // We can not do an Unattended Add if we don't have a password.
  if Unattended and (AddEncrypt in AddOptions) and
      (Password = '') then
    raise EZipMaster.CreateMsgDisp(__ERR_AD_UnattPassword, true);
  try
    GrabPipes;
    if Lister.ZipFileName = '' then // make sure we have a zip filename
      raise EZipMaster.CreateMsgDisp(__ERR_GE_NoZipSpecified, true);
    if (IncludeSpecs.Count = 0) then
    begin
      if not((AddFreshen in AddOptions) or (AddUpdate in AddOptions)) then
        raise EZipMaster.CreateMsgDisp(__ERR_AD_NothingToZip, true);
      AddOptions := (AddOptions - [AddUpdate]) + [AddFreshen];
      IncludeSpecs.Add(WILD_ALL); // do freshen all
    end;

    curz := Lister.Current;
    if curz.FileName = '' then
      curz.FileName := Lister.ZipFileName;
    curz.WorkDrive.HasMedia(False);
    // drive must exist and be changeable
    if Unattended and (not curz.WorkDrive.DriveIsFixed) and MultiDisk then
      raise EZipMaster.CreateMsgDisp(__ERR_DS_NoUnattSpan, true);

    if (curz.Count = 0) and ((AddFreshen in AddOptions)) then
      raise EZipMaster.CreateMsgDisp(__ERR_AD_NothingToZip1, true);

    // make certain destination can exist
    { We must allow a zipfile to be specified that doesn't already exist,
      so don't check here for existance. }
    if (curz.WorkDrive.DriveIsFixed or not MultiDisk) then
    begin
      if zwoForceDest in WriteOptions then
        _Z_ForceDirectory(ExtractFilePath(Lister.ZipFileName));
      if not _Z_DirExists(ExtractFilePath(Lister.ZipFileName)) then
        raise EZipMaster.CreateMsgStr(__ERR_AD_NoDestDir, ExtractFilePath(Lister.ZipFileName)
          );
    end;

    if not IsDestWritable(Lister.ZipFileName, MultiDisk) then
      raise EZipMaster.CreateMsgStr(__ERR_DS_NotChangeable, Lister.ZipFileName);

    if _DLL_Load(self) <= 0 then
      exit; // could not load valid dll
    TmpZipName := Lister.ZipFileName; // default
    // If we are using disk spanning, first create a temporary file
    if (MultiDisk) then
    begin
      ret := RejoinMVArchive(TmpZipName);
      if ret <> 0 then
      begin
        _DLL_Unload(self);
        raise EZipMaster.CreateMsgDisp(ErrCode, true);
      end;
    end;
    if not MultiDisk and AnsiSameText(EXT_EXE, ExtractFileExt(Lister.ZipFileName))
      and not FileExists(Lister.ZipFileName) then
    begin
      { This is the first "add" operation following creation of a new
        .EXE archive.  We need to add the SFX code now, before we add
        the files. }
      ret := NewSFXFile(Lister.ZipFileName);
      if ret <> 0 then
        raise EZipMaster.CreateMsgInt(__ERR_AD_AutoSFXWrong, AbsErr(ret));
    end;
  except
    on ews: EZipMaster do
    begin
      ShowExceptionError(ews);
      exit;
    end;
    else
      exit;
  end;
  Cancel := 0;

  try
    try
      CmdRecP := SetupZipCmd(TmpZipName);
      fEventErr := ''; // added
      { pass in a ptr to parms }
      SuccessCnt := _DLL_Exec(self, CmdRecP, fDLLOperKey);
      fEventErr := ''; // added
      if MultiDisk then
      begin
        if (SuccessCnt < 0) or RecreateMVArchive(TmpZipName,
             (Lister.CentralDir.Count > 0) and ((AddFreshen in AddOptions)
              or (AddUpdate in AddOptions))) then
          _Z_DeleteFile(TmpZipName);
      end;
    except
      on ews: EZipMaster do
      begin
        if fEventErr <> '' then
          ews.Message := ews.Message + fEventErr;
        ShowExceptionError(ews);
      end
      else
        ShowZipMessage(__ERR_GE_FatalZip, '');
    end;
  finally
    IncludeSpecs.Clear;
    ExcludeSpecs.Clear;
    FPipes.Clear;
    DestroyDLLCmd(CmdRecP);
  end; { end try finally }

  _DLL_Unload(self);
  Cancel := 0;
  // Update the Zip Directory by calling List method
  // for spanned exe avoid swapping to last disk
  if SuccessCnt > 0 then
      Lister.Reload := zlrReload // force reload
  else
    Lister.Reload := zlrClear;
end;

(* ? TZMDLLOpr.ExtExtract
  *)
procedure TZMDLLOpr.ExtExtract;
const
  __ERR_GE_NoZipSpecified = __UNIT__ + (1374 shl 10) + GE_NoZipSpecified;
  __ERR_DS_FileOpen = __UNIT__ + (1381 shl 10) + DS_FileOpen;
  __ERR_EX_NoExtrDir = __UNIT__ + (1394 shl 10) + EX_NoExtrDir;
  __ERR_EX_UnattPassword = __UNIT__ + (1430 shl 10) + EX_UnattPassword;
var
  CmdRecP: pDLLCommands;
  DLLVers: Integer;
  good: boolean;
  OldPRC: Integer;
  TmpBaseDir: String;
  TmpS: String;
  TmpZipName: String;
begin
  OldPRC := PasswordReqCount;
  DLLVers := 0;
  TmpZipName := '';
  CmdRecP := nil;
  good := True;
  Cancel := 0;
  try
    if (Lister.ZipFileName = '') then
      raise EZipMaster.CreateMsgDisp(__ERR_GE_NoZipSpecified, true);
    if Lister.CentralDir.Count = 0 then
      Lister.List; // try again
    if Lister.CentralDir.Count = 0 then
    begin
      good := False; // stop from doing anything
      if ErrCode = 0 then // only show once
        raise EZipMaster.CreateMsgDisp(__ERR_DS_FileOpen, true);
    end;
    Cancel := 0; // might have been set in List
    if good then
    begin
      TmpBaseDir := '';
      // expand and check ExtrBaseDir
      if (ExtrBaseDir <> '') and not(ExtrTest in ExtrOptions) then
      begin
        TmpBaseDir := ExpandUNCFileName(DelimitPath(ExtrBaseDir, true));
        if ExtrForceDirs in ExtrOptions then
          _Z_ForceDirectory(TmpBaseDir);
        if not _Z_DirExists(TmpBaseDir) then
          raise EZipMaster.CreateMsgStr(__ERR_EX_NoExtrDir, TmpBaseDir);
      end;

      TmpZipName := Lister.ZipFileName;

      // We do a check if we need UnSpanning first, this depends on
      // The number of the disk the EOC record was found on. ( provided by List() )
      // If we have a spanned set consisting of only one disk we don't use ReadSpan().
      if Lister.CentralDir.TotalDisks > 1 then
      begin
        if TempDir = '' then
        begin
          SetLength(TmpS, MAX_PATH + 2);
          GetTempPath(MAX_PATH, PChar(TmpS));
          TmpZipName := PChar(TmpS); // convert from NULL terminated
          TmpS := '';
        end;
        TmpZipName := DelimitPath(TempDir, true);
        good := ReadSpan(Lister.ZipFileName, TmpZipName, true) >= 0;
        // if we returned without an error, TmpZipName contains a real name.
      end;
    end; // if fUnzBusy then

    if good then
      DLLVers := _DLL_Load(self);
    if DLLVers > 0 then
      try
        GrabPipes;
        CmdRecP := SetupUnzCmd(TmpZipName);
        fEventErr := ''; // added
        // We have to be carefull doing an unattended Extract when a password is needed
        // for some file in the archive.
        if Unattended and (Password = '') and not assigned
          (Master.OnPasswordError) then
        begin
          PasswordReqCount := 0;
          ReportMsg(__ERR_EX_UnAttPassword, []);
        end;
        SuccessCnt := _DLL_Exec(self, CmdRecP, fDLLOperKey);
      finally
        _DLL_Unload(self);
        IncludeSpecs.Clear;
        FPipes.Clear;
        { If UnSpanned we still have this temporary file hanging around. }
        if Lister.CentralDir.TotalDisks > 1 then
          _Z_DeleteFile(TmpZipName);
        DestroyDLLCmd(CmdRecP);

        if Unattended and (Password = '') and not assigned
          (Master.OnPasswordError) then
          PasswordReqCount := OldPRC;
      end;
  except
    on ews: EZipMaster do
    begin
      if fEventErr <> '' then
        ews.Message := ews.Message + fEventErr;
      ShowExceptionError(ews);
      SuccessCnt := 0;
    end;
  end;
  { no need to call the List method; contents unchanged }
end;

procedure TZMDLLOpr.Extract;
begin
  fAutoStream := nil;
  ExtExtract;
end;

(* ? TZMDLLOpr.ExtractFileToStream
  1.73 15 July 2003 RA add check on FileName in FSpecArgs + return on busy
  *)
procedure TZMDLLOpr.ExtractFileToStream(const FileName: String);
const
  __ERR_AD_InvalidName = __UNIT__ + (1480 shl 10) + AD_InvalidName;
  __ERR_AD_NothingToZip = __UNIT__ + (1482 shl 10) + AD_NothingToZip;
var
  fn: String;
begin
  fn := Trim(FileName);
  if (Length(FileName) = 0) and (IncludeSpecs.Count > 0) then
    fn := Trim(IncludeSpecs[0]);
  if (fn = '') or IsWild(fn) then
  begin
    if fn <> '' then
      ShowZipMessage(__ERR_AD_InvalidName, '')
    else
      ShowZipMessage(__ERR_AD_NothingToZip, '');
    exit;
  end;
  IncludeSpecs.Clear();
  IncludeSpecs.Add('0:' + fn);
  fAutoStream := ZipStream;
  fAutoDate := 0;
  fAutoAttr := 0;
  ZipStream.Clear();
  ExtExtract;
  fAutoStream := nil;
  if SuccessCnt <> 1 then
    ZipStream.Clear();
end;

(* ? TZMDLLOpr.ExtractStreamToStream
  1.73 14 July 2003 RA initial SuccessCnt
  *)
procedure TZMDLLOpr.ExtractStreamToStream(InStream: TMemoryStream;
  OutSize: Longword);
const
  __ERR_AZ_NothingToDo = __UNIT__ + (1516 shl 10) + AZ_NothingToDo;
  __ERR_AD_InIsOutStream = __UNIT__ + (1521 shl 10) + AD_InIsOutStream;
  __ERR_DS_Unsupported = __UNIT__ + (1534 shl 10) + DS_Unsupported;
  __ERR_DS_BadCRC = __UNIT__ + (1544 shl 10) + DS_BadCRC;
var
  crc: Cardinal;
  Header: TZM_StreamHeader;
  Method: TZMDeflates;
  realsize: Int64;
begin
  ZipStream.Clear();
  if not assigned(InStream) then
  begin
    ShowZipMessage(__ERR_AZ_NothingToDo, '');
    exit;
  end;
  if InStream = ZipStream then
  begin
    ShowZipMessage(__ERR_AD_InIsOutStream, '');
    exit;
  end;
  realsize := InStream.Size - SizeOf(TZM_StreamHeader);
  if realsize > 0 then
  begin
    InStream.ReadBuffer(Header, SizeOf(TZM_StreamHeader));
    case Header.Method of
      METHOD_DEFLATED or TZMDeflateEncrypt: Method := zmDeflateEncrypt;
      METHOD_DEFLATED: Method := zmDeflate;
      METHOD_STORED: Method := zmStore;
    else
      begin
        ShowZipMessage(__ERR_DS_Unsupported, '');
        ZipStream.size := 0;
        Exit;
      end;
    end;
    crc := Header.CRC;
    Undeflate(ZipStream, InStream, realsize, Method, crc);
    if SuccessCnt = 1 then
    begin      if crc <> Header.CRC then
      begin
        ShowZipMessage(__ERR_DS_BadCRC, '');
        ZipStream.size := 0;
      end;
    end
    else
      ZipStream.size := 0;
  end;
end;

function TZMDLLOpr.GetDLL_Load: Boolean;
begin
  Result := _DLL_Loaded(Master);
{$IFDEF ZDEBUG}
  Diag('DLL_Load = ' + IntToStr(Ord(Result)));
{$ENDIF}
end;

procedure TZMDLLOpr.Finished(WasGood: boolean);
begin
  inherited;
  if not WasGood then
  begin
    FPipes.Clear;
    ZipStream := nil;
  end;
end;

function TZMDLLOpr.GetAddCompLevel: Integer;
begin
    Result := fInternal.fAddCompLevel
end;

function TZMDLLOpr.GetAddFrom: TDateTime;
begin
    Result := fInternal.fAddFrom
end;

function TZMDLLOpr.GetAddStoreSuffixes: TZMAddStoreExts;
begin
    Result := fInternal.fAddStoreSuffixes
end;

function TZMDLLOpr.GetDllErrCode: Cardinal;
begin
  Result := fInternal.fExtErrCode;
end;

function TZMDLLOpr.GetExtAddStoreSuffixes: String;
begin
    Result := fInternal.fExtAddStoreSuffixes
end;

function TZMDLLOpr.GetExtrBaseDir: String;
begin
    Result := fInternal.fExtrBaseDir
end;

function TZMDLLOpr.GetExtrOptions: TZMExtrOpts;
begin
    Result := fInternal.fExtrOptions
end;

function TZMDLLOpr.GetPassword: String;
begin
    Result := fInternal.fPassword
end;

function TZMDLLOpr.GetPasswordReqCount: Integer;
begin
    Result := fInternal.fPasswordReqCount
end;

function TZMDLLOpr.GetRootDir: String;
begin
    Result := fInternal.fRootDir
end;

procedure TZMDLLOpr.GrabPipes;
const
  __ERR_AD_BadFileName = __UNIT__ + (1638 shl 10) + AD_BadFileName;
var
  i: Integer;
  fn: String;
  MasterPipes: TZMPipeListImp;
begin
  MasterPipes := Master.Pipes as TZMPipeListImp;
  MasterPipes.AssignTo(Pipes);
  //  Add names to start of FSpecArgs
  if Pipes.Count > 0 then
  begin
    for I := 0 to Pipes.Count - 1 do
    begin
      fn := Pipes[I].FileName;
      if (fn <> '') and IsInvalidIntName(fn) then
        raise EZipMaster.CreateMsgDisp(__ERR_AD_BadFileName, true);
      while (fn <> '') and (fn[1] = '\') do
        fn := Copy(fn, 2, MAX_PATH);
      if fn = '' then
        fn := '#stream' + IntToStr(I) + '#';
      fn := IntToStr(I) + ':' + fn;
      IncludeSpecs.Insert(I, fn);
    end;
  end;
end;

(*? TZMWorker.IsDestWritable
1.79  2005 Jul 9
*)
function TZMDLLOpr.IsDestWritable(const fname: String; AllowEmpty: Boolean):
    Boolean;
var
  hFile: Integer;
  sr: _Z_TSearchRec;
  wd: TZMWorkDrive;
  xname: String;
begin
  Result := False;
  wd := TZMWorkDrive.Create;
  try
    xname := ExpandUNCFileName(fname);
    // test if destination can be written
    wd.DriveStr := xname;
    if not wd.HasMedia(false) then
    begin
      Result := AllowEmpty and (wd.DriveType = DRIVE_REMOVABLE);
      // assume can put in writable disk
      exit;
    end;
    if WinXP or (wd.DriveType <> DRIVE_CDROM) then
    begin
      if _Z_FindFirst(xname, faAnyFile, sr) = 0 then
      begin
        Result := (sr.Attr and faReadOnly) = 0;
        _Z_FindClose(sr);
        if Result then
        begin
          // exists and is not read-only - test locked
          hFile := _Z_FileOpen(xname, fmOpenWrite);
          Result := hFile > -1;
          if Result then
            SysUtils.FileClose(hFile);
        end;
        exit;
      end;
      // file did not exist - try to create it
      hFile := _Z_FileCreate(xname);
      if hFile > -1 then
      begin
        Result := True;
        FileClose(hFile);
        _Z_DeleteFile(xname);
      end;
    end;
  finally
    wd.Free;
  end;
end;

function TZMDLLOpr.RejoinMVArchive(var TmpZipName: String): Integer;
const
  __ERR_DS_NotChangeable = __UNIT__ + (1742 shl 10) + DS_NotChangeable;
var
  Attrs: Integer;
  curz: TZMZipFile;
  drt: Integer;
  tempzip: TZMZipCopy;
  tmpMessage: TZMMessageEvent;
  zname: String;
begin
  zname := Lister.ZipFileName;
  TmpZipName := Master.MakeTempFileName('', '');
  if Verbosity >= zvVerbose then
  begin
    tmpMessage := Master.OnMessage;
    if assigned(tmpMessage) then
      tmpMessage(Master, 0, ZipFmtLoadStr(GE_TempZip, [TmpZipName]));
  end;
  Result := 0;
  if Lister.Current.TotalEntries > 0 then
  begin
    if (AddFreshen in AddOptions) or (AddUpdate in AddOptions) then
    begin
      // is it detached SFX
      if Lister.Current.MultiDisk and (Lister.Current.Sig = zfsDOS)
        then
        // load the actual zip instead of the loader (without events)
        Lister.LoadZip(ChangeFileExt(zname, EXT_ZIPL), true);

      curz := Lister.Current;
      // test if output can eventually be produced
      drt := curz.WorkDrive.DriveType;
      // we can't re-write on a CD-ROM

      if (drt = DRIVE_CDROM) then
      begin
        Attrs := FileGetAttr(zname);
        if Attrs and faReadOnly <> 0 then
        begin
          ShowZipFmtMsg(__ERR_DS_NotChangeable, [zname], true);
          Result := -7;
          exit;
        end;
      end;
      // rebuild a temp archive
      Result := -DS_FileError;
      tempzip := TZMZipCopy.Create(Master, nil);
      try
        if tempzip.File_Create(TmpZipName) then
        begin
          tempzip.ShowProgress := zspExtra;
          if curz.File_Open(fmOpenRead) then
          begin
            tempzip.EncodeAs := zeoUTF8;
            Result := tempzip.WriteFile(curz, true);
          end;
        end;
      finally
        tempzip.Free;
        curz.File_Close;
      end;
    end;
    if Result < 0 then
    begin
      ErrCode := Result;
      exit;
    end;
    AnswerAll := AnswerAll + [zaaYesOvrwrt];
  end;
  Result := 0;
end;

procedure TZMDLLOpr.ReportMessage1(err: Integer; const msg: TZMString);
var
  tmpMessage: TZMMessageEvent;
begin
  if (err <> 0) and (ErrCode = 0) then // only catch first
  begin
    ErrCode := err and MSG_ID_MASK;
    ErrMessage := msg;
  end;
  tmpMessage := Master.OnMessage;
  if assigned(tmpMessage) then
    tmpMessage(Master, err, msg);
  KeepAlive; // process messages or check terminate
end;

procedure TZMDLLOpr.SetCB(const Value: TDZCallback);
begin
  if fCB <> Value then
  begin
    fCB := Value;
  end;
end;

procedure TZMDLLOpr.SetDllErrCode(const Value: Cardinal);
begin
  fInternal.fExtErrCode := Value;
end;

procedure TZMDLLOpr.SetDLL_Load(const Value: Boolean);
begin
{$IFDEF ZDEBUG}
  Diag('set DLL_Load to ' + IntToStr(Ord(Value)));
{$ENDIF}
  if Value <> _DLL_Loaded(Master) then
  begin
    if Value then
      _DLL_Load(self)
    else
      _DLL_Unload(self);
{$IFDEF ZDEBUG}
    Diag('changed DLL_Load to ' + IntToStr(Ord(Value)));
{$ENDIF}
  end;
end;

procedure TZMDLLOpr.SetExtrOptions(const Value: TZMExtrOpts);
begin
    fInternal.fExtrOptions := Value;
end;

procedure TZMDLLOpr.SetPasswordReqCount(const Value: Integer);
begin
    fInternal.fPasswordReqCount := Value;
end;

procedure TZMDLLOpr.SetRootDir(const Value: String);
begin
  fInternal.fRootDir := Value;
end;

function TZMDLLOpr.SetupUnzCmd(const Value: String): pDLLCommands;
var
  Opts: Cardinal;
begin
  Result := AllocDLLCommand(Value);
  if Result <> nil then
  begin
    Opts := Result^.fOptions;
    if ExtrNTFS in ExtrOptions then
      Opts := Opts or DLL_OPT_NTFSStamps;
    if ExtrDirNames in ExtrOptions then
      Opts := Opts or DLL_OPT_Directories;
    if ExtrOverWrite in ExtrOptions then
      Opts := Opts or DLL_OPT_Overwrite;
    if ExtrUpdate in ExtrOptions then
      Opts := Opts or DLL_OPT_Update
    else if ExtrFreshen in ExtrOptions then
      Opts := Opts or DLL_OPT_Freshen;
    { Update has precedence over freshen }

    if ExtrTest in ExtrOptions then
      Opts := Opts or DLL_OPT_OpIsTest
    else
      Opts := Opts or DLL_OPT_OpIsUnz;

    Result^.fPwdReqCount := PasswordReqCount;
    Result^.fOptions := Opts;
    Result^.fCheck := DLLCOMMANDCHECK;
  end;
end;

function TZMDLLOpr.SetupZipCmd(const Value: String): pDLLCommands;
var
  Opts: Cardinal;
begin
  Result := AllocDLLCommand(Value);
  if Result <> nil then
  begin
    Opts := Result^.fOptions;
    Result^.fEncodedAs := 0; // how to interpret existing names
    if Encoding = zeoOEM then
      Result^.fEncodedAs := Ord(zeoOEM)
    else if Encoding = zeoUTF8 then
      Result^.fEncodedAs := Ord(zeoUTF8);
    Result^.fEncodeAs := Ord(EncodeAs); // how to encode new names

    if AddArchiveOnly in AddOptions then
      Opts := Opts or DLL_OPT_ArchiveFilesOnly;
    if AddResetArchive in AddOptions then
      Opts := Opts or DLL_OPT_ResetArchiveBit;

    if HowToDelete = htdAllowUndo then
      Opts := Opts or DLL_OPT_HowToMove;
    if AddVersion in AddOptions then
      Opts := Opts or DLL_OPT_Versioning;
    if AddVolume in AddOptions then
      Opts := Opts or DLL_OPT_Volume;

    { if True, exclude files earlier than specified date }
    { Date to include files after; only used if fDate=TRUE }
    if AddFromDate in AddOptions then
      Result^.fDate := DateTimeToFileDate(AddFrom);
    // Compression level (0 - 9, 0=none and 9=best)
    Result^.fLevel := AddCompLevel;
//    if not(AddSafe in AddOptions) then
    if not (zwoSafe in WriteOptions) then
      Opts := Opts or DLL_OPT_Grow;
    { if True, Allow appending to a zip file (-g) }
    if AddNTFS in AddOptions then
      Opts := Opts or DLL_OPT_NTFSStamps;

    // distinguish bet. Add and Delete
    Opts := Opts or DLL_OPT_OpIsZip;

    // make zipfile's timestamp same as newest file
    if zwoZipTime in WriteOptions then
      Opts := Opts or DLL_OPT_LatestTime;

    if AddMove in AddOptions then
      Opts := Opts or DLL_OPT_Move; // dangerous, beware!

    if AddUpdate in AddOptions then
      Opts := Opts or DLL_OPT_Update
    else if AddFreshen in AddOptions then
      Opts := Opts or DLL_OPT_Freshen;
    // { Update has precedence over freshen }

    { DLL will prompt for password }
    if AddEncrypt in AddOptions then
      Opts := Opts or DLL_OPT_Encrypt;
    { NOTE: if user wants recursion, then he probably also wants
      AddDirNames, but we won't demand it. }
    if AddRecurseDirs in AddOptions then
      Opts := Opts or DLL_OPT_Recurse;
    if AddHiddenFiles in AddOptions then
      Opts := Opts or DLL_OPT_System;
    if not (AddEmptyDirs in AddOptions) then
      Opts := Opts or DLL_OPT_NoDirEntries;
    { don't store dirnames with filenames }
    if not(AddDirNames in AddOptions) then
      Opts := Opts or DLL_OPT_JunkDir;

    Result^.fOptions := Opts;
    Result^.fCheck := DLLCOMMANDCHECK;
  end;
end;

procedure TZMDLLOpr.Started;
begin
  // do nothing
end;

procedure TZMDLLOpr.Undeflate(OutStream, InStream: TStream; Length: Int64; var
    Method: tzMDeflates; var crc: Cardinal);
const
  __ERR_DS_NoInStream = __UNIT__ + (1963 shl 10) + DS_NoInStream;
  __ERR_DS_NoOutStream = __UNIT__ + (1968 shl 10) + DS_NoOutStream;
  __ERR_AD_InIsOutStream = __UNIT__ + (1973 shl 10) + AD_InIsOutStream;
  __ERR_EX_UnAttPassword = __UNIT__ + (1980 shl 10) + EX_UnAttPassword;
  __ERR_LD_NoDLL = __UNIT__ + (1998 shl 10) + LD_NoDLL;
var
  Args: TZSSArgs;
  CmdRecP: pDLLCommands;
  i: Integer;
  ncrypt: boolean;
begin
  if not assigned(InStream) then
  begin
    ShowZipMessage(__ERR_DS_NoInStream, '');
    exit;
  end;
  if not assigned(OutStream) then
  begin
    ShowZipMessage(__ERR_DS_NoOutStream, '');
    exit;
  end;
  if InStream = ZipStream then
  begin
    ShowZipMessage(__ERR_AD_InIsOutStream, '');
    exit;
  end;
  ncrypt := (Method = zmStoreEncrypt) or (Method = zmDeflateEncrypt);
  // We can not do an Unattended Add if we don't have a password.
  if Unattended and ncrypt and (Password = '') then
  begin
    ShowZipMessage(__ERR_EX_UnAttPassword, '');
    exit;
  end;
  if Length < 0 then
    Length := InStream.size;
  CmdRecP := nil;
  if (Method = zmDeflate) or (Method = zmDeflateEncrypt) then
    Args.Method := METHOD_DEFLATED
  else
    Args.Method := METHOD_STORED;
  if ncrypt then
    Args.Method := Args.Method or TZMDeflateEncrypt;
  Args.fSSInput := InStream;
  Args.fSSOutput := OutStream;
  Args.size := Length;
  Args.crc := crc;
  if _DLL_Load(self) <= 0 then
  begin
    ShowZipMessage(__ERR_LD_NoDLL, DelZipDLL_Name);
    exit;
  end;
  try
    Cancel := 0;
    CmdRecP := SetupUnzCmd('<UNDEFLATE>'); // do not localize
    CmdRecP^.fSS := @Args;
    fEventErr := ''; // added
    { pass in a ptr to parms }
    i := _DLL_Exec(self, CmdRecP, fDLLOperKey);
  finally
    _DLL_Unload(self);
    DestroyDLLCmd(CmdRecP);
  end;
  if i = 1 then
  begin // success
    SuccessCnt := 1;
    if Args.Method = METHOD_DEFLATED then
      Method := zmDeflate
    else
      Method := zmStore;
    crc := Args.crc;
  end;
end;

procedure TDZCallback.AfterConstruction;
begin
  inherited;
  PCB := nil;
  fHeldData := nil;
  fHoldSize := 0;
end;

function TDZCallback.Assign(ZCallBackRec: PZCallBackStruct): Integer;
begin
  PCB := ZCallBackRec;
  if PCB = nil then
    Result := 1
  else
    Result := 0;
end;

procedure TDZCallback.BeforeDestruction;
begin
  if fHeldData <> nil then
    FreeMem(fHeldData);
  fHeldData := nil;
  inherited;
end;

procedure TDZCallback.Clear;
begin
  if fHeldData <> nil then
    FreeMem(fHeldData);
  fHeldData := nil;
  fHoldSize := 0;
  PCB := nil; // ??
end;

function TDZCallback.CopyData(dst: PByte; MaxSize: Integer): Boolean;
var
  sz: Integer;
begin
  Result := False;
  sz := Arg1;
  if sz > MaxSize then
    sz := MaxSize;
  if sz > 0 then
  begin
    move(PCB^.Msg2P^, dst^, sz);
    Result := true;
  end;
end;

function TDZCallback.GetActionCode: Integer;
begin
  Result := PCB^.ActionCode;
end;

function TDZCallback.GetArg1: Cardinal;
begin
  Result := PCB^.Arg1;
end;

function TDZCallback.GetArg2: Cardinal;
begin
  Result := PCB^.Arg2;
end;

function TDZCallback.GetArg3: Integer;
begin
  Result := PCB^.Arg3;
end;

function TDZCallback.GetFile_Size: Int64;
begin
  Result := PCB^.File_Size;
end;

function TDZCallback.GetIsZip: Boolean;
begin
  Result := PCB^.IsOperationZip;
end;

function TDZCallback.GetMsg: TZMString;
begin
  Result := GetMsgStr(PCB^.MsgP);
end;

function TDZCallback.GetMsg2: TZMString;
begin
  Result := GetMsgStr(PCB^.Msg2P);
end;

function TDZCallback.GetMsgStr(const msg: PByte): TZMString;
{$IFNDEF UNICODE}
var
  utemp: UTF8String;
{$ENDIF}
begin
  Result := '';
  if msg <> nil then
  begin
{$IFDEF UNICODE}
    if PCB^.HaveWide <> 0 then
      Result := PWideChar(msg)
    else
      Result := PUTF8ToWideStr(PAnsiChar(msg), -1);
{$ELSE}
    if UsingUtf8 then
    begin
      if PCB^.HaveWide <> 0 then
        Result := PWideToUTF8(PWideChar(msg), -1)
      else
      begin
        utemp := PAnsiChar(msg);
        Result := StrToUTF8(utemp);
      end;
    end
    else
    begin
      if PCB^.HaveWide <> 0 then
        Result := PWideChar(msg) // will convert wide -> ansi
      else
        Result := PAnsiChar(msg);
    end;
{$ENDIF}
  end;
end;

function TDZCallback.GetOwner: TZMDLLOpr;
begin
  Result := TObject(PCB^.Caller) as TZMDLLOpr;
end;

function TDZCallback.GetWritten: Int64;
begin
  Result := PCB^.Written;
end;

function TDZCallback.HoldData(const src: PByte; size: Cardinal): PByte;
var
  len: Integer;
  p: PByte;
begin
  if src = nil then
  begin
    // free buffer
    FreeMem(fHeldData);
    fHeldData := nil;
    fHoldSize := 0;
    Result := fHeldData;
    exit;
  end;
  if fHeldData = nil then
    fHoldSize := 0;
  len := size + sizeof(Integer);
  if (fHeldData = nil) or (len >= fHoldSize) then
  begin
    if fHeldData <> nil then
      FreeMem(fHeldData);
    fHeldData := nil;
    len := (len or 511) + 1;  // increments of 512
    GetMem(fHeldData, len);
    fHoldSize := len;
  end;
  p := fHeldData;
  if size > 0 then
  begin
    move(src^, fHeldData^, size);
    Inc(p, size);
  end;
  PCardinal(p)^ := 0; // mark end
  Result := fHeldData;
end;

function TDZCallback.HoldString(const src: TZMString): PByte;
var
  len: Integer;
begin
  len := Length(src) * sizeof(Char);
  if len > 0 then
    Result := HoldData(PByte(PChar(src)), len)
  else
    Result := HoldData(PByte(@len), 0);  // avoid freeing hold area
end;

procedure TDZCallback.SetArg1(const Value: Cardinal);
begin
  PCB^.Arg1 := Value;
end;

procedure TDZCallback.SetArg2(const Value: Cardinal);
begin
  PCB^.Arg2 := Value;
end;

procedure TDZCallback.SetArg3(const Value: Integer);
begin
  PCB^.Arg3 := Value;
end;

procedure TDZCallback.SetComment(const AStr: AnsiString);
begin
  PCB^.HaveWide := 0;
  PCB^.MsgP := HoldData(PByte(PAnsiChar(AStr)), Length(AStr));
  PCB^.Arg1 := Cardinal(Length(AStr));
end;

procedure TDZCallback.SetData(src: PByte; size: Integer);
begin
  if size > 2048 then
    size := 2048;
  PCB^.MsgP := HoldData(src, size);
  PCB^.Arg1 := Cardinal(size);
end;

procedure TDZCallback.SetFile_Size(const Value: Int64);
begin
  PCB^.File_Size := Value;
end;

procedure TDZCallback.SetMsg(const Value: TZMString);
begin
{$IFDEF UNICODE}
  PCB^.HaveWide := 1; // Unicode
{$ELSE}
  if UsingUtf8 and (ValidUTF8(Value, -1) > 0) then
    PCB^.HaveWide := 2 // UTF8
  else
    PCB^.HaveWide := 0; // Ansi
{$ENDIF}
  PCB^.MsgP := HoldString(Value);
end;

end.
