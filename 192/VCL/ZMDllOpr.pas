unit ZMDllOpr;

//  ZMDllOpr.pas - Dll operations and functions

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
//modified 2013-02-06

interface

{$INCLUDE    '.\ZipVers.inc'}

uses
  {$IFDEF VERDXE2up}
    System.Classes, WinApi.Windows, VCL.Controls, VCL.Graphics, VCL.Dialogs,
  {$ELSE}
    Classes, Windows, Controls, Graphics, Dialogs,
  {$ENDIF}
  ZMDelZip, ZipMstr, ZMCompat, ZMBody, ZMModOpr, ZMLister;

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
    function HoldData(const src: PByte; Size: Cardinal): PByte;
    function HoldString(const src: TZMString): PByte;
    function GetMsgStr(const msg: PByte): TZMString;
    procedure SetComment(const AStr: AnsiString);
    procedure SetData(src: PByte; Size: Integer);
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

  TZMDLLOpr = class(TZMBaseOpr)
  private
    FAutoAttr: Cardinal;
    FAutoDate: Cardinal;
    FCB: TDZCallback;
    FDLLOperKey: Cardinal;
    FDLLTargetName: String;
    FEventErr: string;
    // 1 data for dll held until next callback or fini
    FHeldData: Pointer;
    FIsDestructing: Boolean;
    Warnings: Integer;
    function DLLStreamClose(ZStreamRec: PZStreamRec): Integer;
    function DLLStreamCreate(ZStreamRec: PZStreamRec): Integer;
    function DLLStreamIdentify(ZStreamRec: PZStreamRec): Integer;
    function DllToErrCode(DLL_error: Integer): integer;
    procedure DLL_Comment(var Result: Integer);
    procedure DLL_Data(var Result: Integer);
    procedure DLL_ExtName(var Result: Integer);
    procedure DLL_Message(var Result: Integer);
    procedure DLL_Password(var Result: Integer);
    procedure DLL_Progress(Action: TActionCodes; var Result: Integer);
    procedure DLL_SetAddName(var Result: Integer);
    procedure DLL_Skipped(var Result: Integer);
    function GetAddCompLevel: Integer;
    function GetAddFrom: TDateTime;
    function GetAddOptions: TZMAddOpts;
    function GetAddStoreSuffixes: TZMAddStoreExts;
    function GetDLL_Load: Boolean;
    function GetExtAddStoreSuffixes: String;
    function GetPassword: String;
    function GetPasswordReqCount: Integer;
    function GetRootDir: String;
    function GetZipStream: TMemoryStream;
    function IsDestWritable(const fname: String; AllowEmpty: Boolean): Boolean;
    function RejoinMVArchive(var TmpZipName: String): Integer;
    procedure SetAddOptions(const Value: TZMAddOpts);
    procedure SetCB(const Value: TDZCallback);
    procedure SetDLL_Load(const Value: Boolean);
    procedure SetPasswordReqCount(const Value: Integer);
    procedure SetRootDir(const Value: String);
    procedure SetZipStream(const Value: TMemoryStream);
  protected
    fAutoStream: TStream;
    function AddStoreExtStr(Options: TZMAddStoreExts): String;
    function AllocDLLCommand(const FileName: String): pDLLCommands;
    procedure CancelSet(Value: Integer);
    procedure DestroyDLLCmd(var rec: pDLLCommands);
    function DLLCallback(ZCallBackRec: PZCallBackStruct): Integer;
    function DLLStreamOp(op: TZStreamActions; ZStreamRec: PZStreamRec): Integer;
    procedure DLL_Arg(var Result: Integer);
    procedure ExtAdd;
    function SetupZipCmd(const Value: String): pDLLCommands;
    property AddCompLevel: Integer read GetAddCompLevel;
    property AddFrom: TDateTime read GetAddFrom;
    property AddStoreSuffixes: TZMAddStoreExts read GetAddStoreSuffixes;
    property CB: TDZCallback read FCB write SetCB;
    property DLLTargetName: String read FDLLTargetName write FDLLTargetName;
    property ExtAddStoreSuffixes: String read GetExtAddStoreSuffixes;
    property Password: String read GetPassword;
    property PasswordReqCount: Integer read GetPasswordReqCount write
        SetPasswordReqCount;
    property RootDir: String read GetRootDir write SetRootDir;
  public
    procedure AbortDLL;
    function Add: Integer;
    function AddStreamToFile(const FileName: String; FileDate, FileAttr: Dword):
        Integer;
// TODO: AddStreamToStream
//  procedure AddStreamToStream(InStream: TMemoryStream);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Deflate(OutStream, InStream: TStream; Length: Int64; var Method:
        TZMDeflates; var CRC: Cardinal): Integer;
    property AddOptions: TZMAddOpts read GetAddOptions write SetAddOptions;
    property DLL_Load: Boolean read GetDLL_Load write SetDLL_Load;
    property ZipStream: TMemoryStream read GetZipStream write SetZipStream;
  end;

implementation

uses
  {$IFDEF VERDXE2up}
    System.SysUtils,
  {$ELSE}
    SysUtils,
  {$ENDIF}
   Forms, ZMMsg, ZMXcpt, ZMUtils, ZMMsgStr, ZMCtx,
  ZMDlg, ZMDrv, ZMStructs, ZMUTF8, ZMZipReader,
  ZMDllLoad, ZMWFuncs, ZMZipBase, ZMEntryReader, ZMZipWriter;

const
  __UNIT__ = 6;// shl 23;

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

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
                raise EZipMaster.CreateMsgDisp(ZM_Error(249, ZS_SeekError), true);
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
              if (Strm.Position + cnt) > Strm.Size then
                cnt := Integer(Strm.Size - Strm.Position);
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
  if FDLLOperKey <> 0 then
    _DLL_Abort(Master, FDLLOperKey);
end;

function TZMDLLOpr.Add: Integer;
begin
  if Reporter.Verbosity > zvTrace then // Reporter.Logger <> nil then
    Reporter.Logger.LogSpecs('');
  fAutoStream := nil;
  ExtAdd;
  Result := Reporter.Code;
end;

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
  // 'FileName' is the name you want to use in the zip file to
  // store the contents of the stream under.
*)
function TZMDLLOpr.AddStreamToFile(const FileName: String; FileDate, FileAttr:
    Dword): Integer;
var
  FatDate: Word;
  FatTime: Word;
  fn: String;
  ft: TFileTime;
  st: TSystemTime;
begin
//  Result := 0;
  fn := Trim(FileName);
  if (Length(fn) = 0) and (IncludeSpecs.Count > 0) then
    fn := Trim(IncludeSpecs[0]);
  if (fn = '') or (ZipStream.Size = 0) then
  begin
    Result := -ZM_Error(336, ZS_NothingToZip);
//    Reporter.ShowZipMessage(ZM_Error(336, ZS_NothingToZip), '');
    exit;
  end;
//  fn := DriveFolders.MakeFullPath(fn);
  Result := DriveFolders.ExpandPath(fn, fn);
  if Result < 0 then
    Exit;
  // strip drive etc like 1.79
  if ExtractFileDrive(fn) <> '' then
    fn := Copy(fn, 3, Length(fn) - 2);
  if (fn <> '') and ((fn[1] = '/') or (fn[1] = '\')) then
    fn := Copy(fn, 2, Length(fn) -1);
  if NameIsBad(fn, false) then
  begin
    Result := -ZM_Error(347, ZS_BadFileName);
    Reporter.ShowZipMessage(Result{ZM_Error(347, ZS_BadFileName)}, fn);
//    exit;
  end;
  if Result = 0 then
  begin
    Body.ClearIncludeSpecs;

    IncludeSpecs.Add('0:' + fn);
    if FileDate = 0 then
    begin
      GetLocalTime(st);
      SystemTimeToFileTime(st, ft);
      FileTimeToDosDateTime(ft, FatDate, FatTime);
      FileDate := (Dword(FatDate) shl 16) + FatTime;
    end;
    fAutoStream := ZipStream;
    FAutoDate := FileDate;
    FAutoAttr := FileAttr;
    ExtAdd;
    Result := -Reporter.ExtCode; // ????
  end;
end;

// TODO: AddStreamToStream
//procedure TZMDLLOpr.AddStreamToStream(InStream: TMemoryStream);
//var
//Header: TZM_StreamHeader;
//Method: TZMDeflates;
//begin
//if InStream = ZipStream then
//begin
//  Reporter.ShowZipMessage(ZM_Error(371, ZS_InIsOutStream), '');
//  exit;
//end;
//if assigned(InStream) and (InStream.Size > 0) then
//begin
//  if AddEncrypt in AddOptions then
//  begin
//    Reporter.ShowZipMessage(ZM_Error(378, ZS_NoEncrypt), '');
//    exit;
//  end;
//  ZipStream.Size := 0;
//  Method := zmDeflate;
//  Header.Method := METHOD_DEFLATED;
//  Header.CRC := 0;
//  ZipStream.WriteBuffer(Header, SizeOf(Header));
//  Deflate(ZipStream, InStream, -1, Method, Header.CRC);
//  if SuccessCnt = 1 then
//  begin
//    ZipStream.Position := 0;
//    if Method <> zmDeflate then
//      Header.Method := METHOD_STORED; // was stored
//    ZipStream.WriteBuffer(Header, SizeOf(Header));
//  end
//  else
//    ZipStream.Size := 0;
//end
//else
//  Reporter.ShowZipMessage(ZM_Error(398, ZS_NothingToZip), '');
//end;

procedure TZMDLLOpr.AfterConstruction;
begin
  inherited;
  FDLLOperKey := 0;
  FHeldData := nil;
  fCB := TDZCallback.Create;
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
  Result^.fEncodedAs := Ord(Lister.Encoding); // how to interpret existing names
  Result^.fFromPage := Lister.Encoding_CP;

  if Reporter.Verbosity >= zvTrace then
    Result^.fVerbosity := -1
  else
  if Reporter.Verbosity >= zvVerbose then
    Result^.fVerbosity := 1
  else
    Result^.fVerbosity := 0;
  { if tracing, we want verbose also }

  // used for dialogs (like the pwd dialogs)
  if Reporter.Unattended then
    Result^.fHandle := 0
  else
    Result^.fHandle := Master.Handle;
  Result^.fSS := nil;

  Opts := DLL_OPT_Quiet; // no DLL error reporting

  Result^.fOptions := Opts;
end;

procedure TZMDLLOpr.BeforeDestruction;
begin
  FIsDestructing := true; // stop callbacks
  AbortDLL;
  if FHeldData <> nil then
  begin
    FreeMem(FHeldData); // release held data
    FHeldData := nil;
  end;
  FreeAndNil(fCB);
  inherited;
end;

procedure TZMDLLOpr.CancelSet(Value: Integer);
begin
  AbortDLL; // is this too soon
end;

function TZMDLLOpr.Deflate(OutStream, InStream: TStream; Length: Int64; var
    Method: TZMDeflates; var CRC: Cardinal): Integer;
var
  Args: TZSSArgs;
  CmdRecP: pDLLCommands;
  i: Integer;
  ncrypt: boolean;
begin
//  Result := -__ERR_ZS_NoInStream;
  if not assigned(InStream) then
  begin
    Result := -ZM_Error(477, ZS_NoInStream);
    exit;
  end;
//  Result := -__ERR_ZS_NoOutStream;
  if not assigned(OutStream) then
  begin
    Result := -ZM_Error(483, ZS_NoOutStream);
    exit;
  end;
//  Result := -__ERR_ZS_InIsOutStream;
  if InStream = ZipStream then
  begin
    Result := - ZM_Error(489, ZS_InIsOutStream);
    exit;
  end;
  CmdRecP := nil;
  ncrypt := (Method = zmStoreEncrypt) or (Method = zmDeflateEncrypt);
  // We can not do an Unattended Add if we don't have a password.
//  Result := -__ERR_ZS_UnattPassword;
  if Reporter.Unattended and ncrypt and (Password = '') then
  begin
    Result := -ZM_Error(498, ZS_UnattPassword);
    exit;
  end;
  if Length < 0 then
    Length := InStream.Size;
  if (Method = zmDeflate) or (Method = zmDeflateEncrypt) then
    Args.Method := 8
  else
    Args.Method := 0;
  Args.fSSInput := InStream;
  Args.fSSOutput := OutStream;
  Args.Size := Length;
  Args.crc := CRC;
//  Result := -__ERR_ZS_NoDLL;
  if _DLL_Load(Lister) <= 0 then
  begin
    Result := -ZM_Error(514, ZS_NoDll);
    exit;
  end;
  try
    if ncrypt then
      AddOptions := AddOptions + [AddEncrypt]
    else
      AddOptions := AddOptions - [AddEncrypt];
    CmdRecP := SetupZipCmd('');
    CmdRecP^.fSS := @Args;
    FEventErr := ''; // added
    { pass in a ptr to parms }
    i := _DLL_Exec(Lister, CmdRecP, FDLLOperKey);
  finally
    _DLL_Unload(Lister);
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
    CRC := Args.CRC;
  end//;
  else
    Result := -ZM_Error(542, DllToErrCode(i));
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
  if FIsDestructing then // in destructor return
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
      zacCRCError:;
      zacOverwrite: ;
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
          FDLLOperKey := CB.Arg1;
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
    if Reporter.Logger <> nil then
      Reporter.Logger.Log(ZM_Error(611, 0), '[CANCEL sent]');
    end;
  except
    on E: Exception do
    begin
      if FEventErr = '' then
        // catch first exception only
        FEventErr := ' #' + IntToStr(Ord(Action)) + ' "' + E.Message + '"';
      Cancel := ZS_Except;
      Result := CALLBACK_EXCEPTION;
    if Reporter.Logger <> nil then
      Reporter.Logger.Log(ZM_Error(622, 0), '[CALLBACK Exception sent] ' + FEventErr);
    end;
  end;
end;

function TZMDLLOpr.DLLStreamClose(ZStreamRec: PZStreamRec): Integer;
var
  Strm: TStream;
begin
  Result := CALLBACK_UNHANDLED;
  if TObject(ZStreamRec^.StrmP) is TStream then
  begin
    Strm := TStream(ZStreamRec^.StrmP);
    if Strm = ZipStream then
    begin
      fAutoStream := nil;
      ZStreamRec^.StrmP := nil;
      Result := CALLBACK_TRUE;
    end;
  end;
end;

function TZMDLLOpr.DLLStreamCreate(ZStreamRec: PZStreamRec): Integer;
begin
  Result := CALLBACK_UNHANDLED;
  ZStreamRec^.StrmP := nil;
  if assigned(fAutoStream) then
  begin
    Result := CALLBACK_TRUE;
    ZStreamRec^.StrmP := fAutoStream;
    fAutoStream.Position := 0;
  end;
end;

function TZMDLLOpr.DLLStreamIdentify(ZStreamRec: PZStreamRec): Integer;
begin
  Result := CALLBACK_UNHANDLED;
  if assigned(fAutoStream) then
  begin
    Result := CALLBACK_TRUE;
    ZStreamRec^.ArgLL := fAutoStream.Size;
    ZStreamRec^.ArgD := FAutoDate;
    ZStreamRec^.ArgA := FAutoAttr;
  end;
end;

// ALL interface structures BYTE ALIGNED
(* stream operation arg usage
  zacStIdentify,
  //      IN BufP = name
  IN Number = number
  OUT ArgLL = Size, ArgD = Date, ArgA = Attrs
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
  Reporter.Trace(Format('Stream operation %d on %d returns %d',
        [Ord(op), ZStreamRec^.Number, Result]), ZM_Error(708, 0));
end;

// return proper ErrCode for dll error
function TZMDLLOpr.DllToErrCode(DLL_error: Integer): integer;
begin
  Result := DLL_error and $3F;
  if Result <> 0 then
    Result := ZD_RES_GOOD + Result;
  if Result > ZD_ERR_SKIPPED then
    Result := ZD_RES_ERROR;
end;

(* Arg1 = argument
  0 = filename
  1 = password
  2 = RootDir
  3 = ExtractDir
  4 = Zip comment
  5 = FSpecArgs      Arg3 = Index
  6 = FSpecArgsExcl  Arg3 = Index
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
      CB.SetComment(Lister.ZipComment);
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
          ;//sr := ExtrBaseDir;
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
          sr := Lister.TempDir;
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
    DatSize := CB.Arg1; // old Size
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
var
  ECode: Integer;
  Erm: TZMString;
  ErrorCode: Integer;
  EType: Integer;
  ExtCode: Integer;
  Show: Boolean;
  tmpMessage: TZMMessageEvent;
begin
  Erm := CB.msg;
  ErrorCode := CB.Arg1;
  ExtCode := 0;
  ECode := 0;
  EType := 0;
  if ErrorCode <> 0 then
  begin
    ExtCode := ErrorCode or $40000000;
    If (Error and $700) > (Reporter.ExtCode and $700) then
      Reporter.ExtCode := ExtCode; // remember last error

    ECode := DllToErrCode(ErrorCode);
    EType := ErrorCode and DZM_Type_Mask;
    if EType = DZM_Warning then
      Inc(Warnings);
    if (EType >= DZM_Message) and ((ErrorCode and DZM_MessageBit) <> 0) then
      Erm := ZipLoadStr(ECode) + Erm;
    if (ECode <> 0) and (Reporter.Code = 0) then
    // W'll always keep the last ErrorCode
    begin
      if (FEventErr <> '') and (ECode = _DZ_ERR_ABORT) then
        Erm := ZipFmtLoadStr(ZM_Error(948, ZS_EventEx), [FEventErr]);
    end;
    Reporter.Message := Erm;
  end;
    if Reporter.Logger <> nil then
      Reporter.Logger.Log(ExtCode, Erm);
//  ReportToSniffer(ExtCode, Erm);
  tmpMessage := Master.OnMessage;
  if assigned(tmpMessage) then
  begin
    Show := false;
    case EType of
      DZM_General, DZM_Error, DZM_Warning, DZM_Message:
        Show := true;
      DZM_Verbose:
        if Reporter.Verbosity >= zvVerbose then
          Show := true;
      DZM_Trace:
        if Reporter.Verbosity >= zvTrace then
          Show := true;
    end;
    if Show then
    begin
      if ECode <> 0 then
        ECode := ZM_Error(972, ECode);
      tmpMessage(Master, ECode, Erm);
    end;
  end;
  KeepAlive; // process messages or check terminate
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
    Cancel := ZS_Abort;
    Result := CALLBACK_ABORT;
  end;
end;

procedure TZMDLLOpr.DLL_Progress(Action: TActionCodes; var Result: Integer);
begin
  case Action of
    zacItem .. zacEndOfBatch:
        Progress.Written(CB.Written);
  end;
  case Action of
    zacTick: KeepAlive;
    zacItem: Progress.NewItem(CB.msg, CB.File_Size);
    zacProgress: Progress.Advance(CB.File_Size);
    zacEndOfBatch: Progress.EndBatch;
    zacCount: Progress.TotalCount := CB.Arg1;
    zacSize: Progress.TotalSize := CB.File_Size;
    zacXItem: Progress.NewXtraItem(CB.msg, CB.File_Size);
    zacXProgress: Progress.AdvanceXtra(CB.File_Size);
  end;
  Result := 0;
  if (Action = zacItem) and (CB.File_Size = -1) and (Progress.Stop)  then
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
    Reporter.ExtCode := DllToErrCode(ErrorCode);
  ti := CB.Arg2; // type
  if Reporter.ReportSkipping(CB.msg, DllToErrCode(ErrorCode),
    TZMSkipTypes(ti and 31)) then
    Result := CALLBACK_TRUE;
end;

procedure TZMDLLOpr.ExtAdd;
var
  CmdRecP: pDLLCommands;
  curz: TZMZipReader;
  MultiDisk: Boolean;
  ret: Integer;
  TmpZipName: String;
begin
  Warnings := 0;
//  { Make sure we can't get back in here while work is going on }
  CmdRecP := nil;
  MultiDisk := zwoDiskSpan in WriteOptions;
  // We can not do an Unattended Add if we don't have a password.
  if Reporter.Unattended and (AddEncrypt in AddOptions) and
      (Password = '') then
    raise EZipMaster.CreateMsgDisp(ZM_Error(1111, ZS_UnattPassword), true);
  try
    if ZipFileName = '' then // make sure we have a zip filename
      raise EZipMaster.CreateMsgDisp(ZM_Error(1114, ZS_NoZipSpecified), true);
    if (IncludeSpecs.Count = 0) then
    begin
      if not((AddFreshen in AddOptions) or (AddUpdate in AddOptions)) then
        raise EZipMaster.CreateMsgDisp(ZM_Error(1118, ZS_NothingToZip), true);
      AddOptions := (AddOptions - [AddUpdate]) + [AddFreshen];
      IncludeSpecs.Add(WILD_ALL); // do freshen all
    end;

    curz := Current;
    if curz.ArchiveName = '' then
      curz.ArchiveName := ZipFileName;
    curz.WorkDrive.HasMedia(False);
    // drive must exist and be changeable
    ret := curz.RefuseWriteSplit;
    if ret <> 0 then
      raise EZipMaster.CreateMsgDisp(ZM_Error(1130, ret), true);

    if (curz.Count = 0) and ((AddFreshen in AddOptions)) then
      raise EZipMaster.CreateMsgDisp(ZM_Error(1133, ZS_NothingToZip), true);

    // make certain destination can exist
    { We must allow a zipfile to be specified that doesn't already exist,
      so don't check here for existance. }
    if (curz.WorkDrive.DriveIsFixed or not MultiDisk) then
    begin
      if zwoForceDest in WriteOptions then
        _Z_ForceDirectory(ExtractFilePath(ZipFileName));
      if not _Z_DirExists(ExtractFilePath(ZipFileName)) then
        raise EZipMaster.CreateMsgStr(ZM_Error(1143, ZS_NoDestDir), ExtractFilePath(ZipFileName)
          );
    end;

    if not IsDestWritable(ZipFileName, MultiDisk) then
      raise EZipMaster.CreateMsgStr(ZM_Error(1148, ZS_NotChangeable), ZipFileName);

    if _DLL_Load(Lister) <= 0 then
      exit; // could not load valid dll
    TmpZipName := ZipFileName; // default
    // If we are using disk spanning, first create a temporary file
    if (MultiDisk) then
    begin
      ret := RejoinMVArchive(TmpZipName);
      if ret <> 0 then
      begin
        _DLL_Unload(Lister);
        raise EZipMaster.CreateMsgDisp(Reporter.Code, true);
      end;
    end;
    if not MultiDisk and AnsiSameText(EXT_EXE, ExtractFileExt(ZipFileName))
      and not FileExists(ZipFileName) then
    begin
      { This is the first "add" operation following creation of a new
        .EXE archive.  We need to add the SFX code now, before we add
        the files. }
      ret := NewSFXFile(ZipFileName);
      if ret <> 0 then
        raise EZipMaster.CreateMsgInt(ZM_Error(1171, ZS_AutoSFXWrong), ret);
    end;
  except
    on ews: EZipMaster do
    begin
      Reporter.ShowExceptionError(ews);
      exit;
    end;
    else
      exit;
  end;
  Cancel := 0;

  try
    try
      CmdRecP := SetupZipCmd(TmpZipName);
      FEventErr := ''; // added
      { pass in a ptr to parms }
      SuccessCnt := _DLL_Exec(Lister, CmdRecP, FDLLOperKey);
      FEventErr := ''; // added
      if MultiDisk then
      begin
        if (SuccessCnt < 0) or RecreateMVArchive(TmpZipName,
             (Lister.Count > 0) and ((AddFreshen in AddOptions)
              or (AddUpdate in AddOptions))) then
          File_Delete(TmpZipName);
      end;
    except
      on ews: EZipMaster do
      begin
        if FEventErr <> '' then
          ews.Message := ews.Message + FEventErr;
        Reporter.ShowExceptionError(ews);
      end
      else
        Reporter.ShowZipMessage(ZM_Error(1206, ZS_FatalZip), '');
    end;
  finally
    Body.ClearIncludeSpecs;//IncludeSpecs.Clear;
    Body.ClearExcludeSpecs;//ExcludeSpecs.Clear;
    DestroyDLLCmd(CmdRecP);
  end; { end try finally }

  _DLL_Unload(Lister);
  Cancel := 0;
  // Update the Zip Directory by calling List method
  // for spanned exe avoid swapping to last disk
  if SuccessCnt > 0 then
      Reload := zlrReload // force reload
  else
    Reload := zlrClear;
  if (Reporter.Code = 0) and (Warnings > 0) then
  begin
    Reporter.Code := ZD_RES_WARNING;
    Reporter.Message := 'Finished with ' + IntToStr(Warnings) + ' warnings';
    Reporter.Inform(Reporter.Message);
  end;
end;

function TZMDLLOpr.GetDLL_Load: Boolean;
begin
  Result := _DLL_Loaded(Master);
{$IFDEF ZDEBUG}
  Reporter.Trace('DLL_Load = ' + IntToStr(Ord(Result)));
{$ENDIF}
end;

function TZMDLLOpr.GetAddCompLevel: Integer;
begin
    Result := Lister.AddCompLevel
end;

function TZMDLLOpr.GetAddFrom: TDateTime;
begin
    Result := Lister.AddFrom
end;

function TZMDLLOpr.GetAddOptions: TZMAddOpts;
begin
  Result := Body.AddOptions;
end;

function TZMDLLOpr.GetAddStoreSuffixes: TZMAddStoreExts;
begin
    Result := Lister.AddStoreSuffixes
end;

function TZMDLLOpr.GetExtAddStoreSuffixes: String;
begin
    Result := Lister.ExtAddStoreSuffixes
end;

function TZMDLLOpr.GetPassword: String;
begin
    Result := Lister.Password
end;

function TZMDLLOpr.GetPasswordReqCount: Integer;
begin
    Result := Lister.PasswordReqCount
end;

function TZMDLLOpr.GetRootDir: String;
begin
    Result := Lister.RootDir
end;

function TZMDLLOpr.GetZipStream: TMemoryStream;
begin
  Result := Body.ZipStream;
end;

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
    if {$IFNDEF UNICODE}Lister.WinXP or{$ENDIF}(wd.DriveType <> DRIVE_CDROM) then
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
            {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FileClose(hFile);
        end;
        exit;
      end;
      // file did not exist - try to create it
      hFile := _Z_FileCreate(xname);
      if hFile > -1 then
      begin
        Result := True;
        {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FileClose(hFile);
        File_Delete(xname);
      end;
    end;
  finally
    wd.Free;
  end;
end;

function TZMDLLOpr.RejoinMVArchive(var TmpZipName: String): Integer;
var
  Attrs: Integer;
  curz: TZMZipReader;
  drt: Integer;
  tempzip: TZMZipCopier;
  tmpMessage: TZMMessageEvent;
  zname: String;
begin
  zname := ZipFileName;
  TmpZipName := Master.MakeTempFileName('', '');
  if Reporter.Verbosity >= zvVerbose then
  begin
    tmpMessage := Master.OnMessage;
    if assigned(tmpMessage) then
      tmpMessage(Master, 0, ZipFmtLoadStr(ZS_TempZip, [TmpZipName]));
  end;
  Result := 0;
  if Current.TotalEntries > 0 then
  begin
    if (AddFreshen in AddOptions) or (AddUpdate in AddOptions) then
    begin
      // is it detached SFX
      if Current.MultiDisk and (Current.Sig = zfsDOS)
        then
        // load the actual zip instead of the loader (without events)
        Lister.LoadZip(ChangeFileExt(zname, EXT_ZIPL), true);

      curz := Current;
      // test if output can eventually be produced
      drt := curz.WorkDrive.DriveType;
      // we can't re-write on a CD-ROM

      if (drt = DRIVE_CDROM) then
      begin
        Attrs := FileGetAttr(zname);
        if Attrs and faReadOnly <> 0 then
        begin
          Result := -ZM_Error(1371, ZS_NotChangeable);
          Reporter.ShowZipFmtMessage(Result, [zname], true);
          exit;
        end;
      end;
      // rebuild a temp archive
      Result := -ZS_FileError;
      tempzip := TZMZipCopier.Create(Lister);
      try
        if tempzip.File_Create(TmpZipName) then
        begin
          ShowProgress := zspExtra;
          if curz.File_Open('', fmOpenRead) then
          begin
            tempzip.EncodeAs := zeoUTF8; // change for creating this zip only
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
      Reporter.Code := Result;
      exit;
    end;
    AnswerAll := True;
  end;
  Result := 0;
end;

procedure TZMDLLOpr.SetAddOptions(const Value: TZMAddOpts);
begin
  Body.AddOptions := Value;
end;

procedure TZMDLLOpr.SetCB(const Value: TDZCallback);
begin
  if fCB <> Value then
  begin
    fCB := Value;
  end;
end;

procedure TZMDLLOpr.SetDLL_Load(const Value: Boolean);
begin
{$IFDEF ZDEBUG}
  Reporter.Trace('set DLL_Load to ' + IntToStr(Ord(Value)));
{$ENDIF}
  if Value <> _DLL_Loaded(Master) then
  begin
    if Value then
      _DLL_Load(Lister)
    else
      _DLL_Unload(Lister);
{$IFDEF ZDEBUG}
    Reporter.Trace('changed DLL_Load to ' + IntToStr(Ord(Value)));
{$ENDIF}
  end;
end;

procedure TZMDLLOpr.SetPasswordReqCount(const Value: Integer);
begin
    Lister.PasswordReqCount := Value;
end;

procedure TZMDLLOpr.SetRootDir(const Value: String);
begin
  Lister.RootDir := Value;
end;

function TZMDLLOpr.SetupZipCmd(const Value: String): pDLLCommands;
var
  Opts: Cardinal;
  AddOpts: TZMAddOpts;
begin
  Result := AllocDLLCommand(Value);
  if Result <> nil then
  begin
    AddOpts := AddOptions;
    Opts := Result^.fOptions;
    Result^.fEncodedAs := 0; // how to interpret existing names
    if Lister.Encoding = zeoOEM then
      Result^.fEncodedAs := Ord(zeoOEM)
    else if Lister.Encoding = zeoUTF8 then
      Result^.fEncodedAs := Ord(zeoUTF8);
    Result^.fEncodeAs := Ord(Lister.EncodeAs); // how to encode new names

    if AddArchiveOnly in AddOpts then
      Opts := Opts or DLL_OPT_ArchiveFilesOnly;
    if AddResetArchive in AddOpts then
      Opts := Opts or DLL_OPT_ResetArchiveBit;

    if HowToDelete = htdAllowUndo then
      Opts := Opts or DLL_OPT_HowToMove;
    if AddVersion in AddOpts then
      Opts := Opts or DLL_OPT_Versioning;
    if AddVolume in AddOpts then
      Opts := Opts or DLL_OPT_Volume;

    { if True, exclude files earlier than specified date }
    { Date to include files after; only used if fDate=TRUE }
    if AddFromDate in AddOpts then
      Result^.fDate := DateTimeToFileDate(AddFrom);
    // Compression level (0 - 9, 0=none and 9=best)
    Result^.fLevel := AddCompLevel;
    if not (zwoSafe in Lister.WriteOptions) then
      Opts := Opts or DLL_OPT_Grow;
    { if True, Allow appending to a zip file (-g) }
    if AddNTFS in AddOpts then
      Opts := Opts or DLL_OPT_NTFSStamps;

    // distinguish bet. Add and Delete
    Opts := Opts or DLL_OPT_OpIsZip;

    // make zipfile's timestamp same as newest file
    if zwoZipTime in WriteOptions then
      Opts := Opts or DLL_OPT_LatestTime;

    if AddMove in AddOpts then
      Opts := Opts or DLL_OPT_Move; // dangerous, beware!

    if AddUpdate in AddOpts then
      Opts := Opts or DLL_OPT_Update
    else if AddFreshen in AddOpts then
      Opts := Opts or DLL_OPT_Freshen;
    // { Update has precedence over freshen }

    { DLL will prompt for password }
    if AddEncrypt in AddOpts then
      Opts := Opts or DLL_OPT_Encrypt;
    { NOTE: if user wants recursion, then he probably also wants
      AddDirNames, but we won't demand it. }
    if AddRecurseDirs in AddOpts then
      Opts := Opts or DLL_OPT_Recurse;
    if AddHiddenFiles in AddOpts then
      Opts := Opts or DLL_OPT_System;
    if not (AddEmptyDirs in AddOpts) then
      Opts := Opts or DLL_OPT_NoDirEntries;
    { don't store dirnames with filenames }
    if not(AddDirNames in AddOpts) then
      Opts := Opts or DLL_OPT_JunkDir;

    Result^.fOptions := Opts;
    Result^.fCheck := DLLCOMMANDCHECK;
  end;
end;

procedure TZMDLLOpr.SetZipStream(const Value: TMemoryStream);
begin
  //
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

function TDZCallback.HoldData(const src: PByte; Size: Cardinal): PByte;
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
  len := Size + sizeof(Integer);
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
  if Size > 0 then
  begin
    move(src^, fHeldData^, Size);
    Inc(p, Size);
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

procedure TDZCallback.SetData(src: PByte; Size: Integer);
begin
  if Size > 2048 then
    Size := 2048;
  PCB^.MsgP := HoldData(src, Size);
  PCB^.Arg1 := Cardinal(Size);
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
