unit SnifferMain2;
(* ***************************************************************************
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.

   This file is part of TZipMaster Version 1.9.
   
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
//modified 2012-04-02


interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, Menus,
  StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ToolWin, ImgList, AppEvnts;

const
  ZIPSNIFFERBUILD: String =  '1.9.0.0093';
  ZIPSNIFFERDATE: String  =  '23/06/2010';
  ZIPSNIFFERPRIV: Integer = 1900093;
      
const
  SZipMasterSniffer = 'ZipMaster Sniffer';
  STZipSniffer = 'TSnifferForm';
  WM_SNIFF_START = WM_APP + $3F42;
  WM_SNIFF_STOP = WM_APP + $3F44;

type
  ByteArray = Array of byte;
{$IFDEF UNICODE}
  TZString = string;
  TZWString = string;
{$ELSE}
  UTF8String = type string;
  TZString = UTF8String;
  TZWString = WideString;
{$ENDIF}

type
  TSnifferForm = class(TForm)
    ActionList1: TActionList;
    CopyItem: TMenuItem;
    CutItem: TMenuItem;
    Edit1: TMenuItem;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditPaste1: TEditPaste;
    File1: TMenuItem;
    FileClose1: TWindowClose;
    FileCloseItem: TMenuItem;
    FileExit1: TAction;
    FileExitItem: TMenuItem;
    FileNew1: TAction;
    FileNewItem: TMenuItem;
    FileOpen1: TAction;
    FileOpenItem: TMenuItem;
    FileSave1: TAction;
    FileSaveAs1: TAction;
    FileSaveAsItem: TMenuItem;
    FileSaveItem: TMenuItem;
    Help1: TMenuItem;
    HelpAbout1: TAction;
    HelpAboutItem: TMenuItem;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;
    PasteItem: TMenuItem;
    StatusBar: TStatusBar;
    tbbTrace: TToolButton;
    tbbVerbose: TToolButton;
    tbbWatch: TToolButton;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    Window1: TMenuItem;
    WindowArrangeAll1: TWindowArrange;
    WindowArrangeItem: TMenuItem;
    WindowCascade1: TWindowCascade;
    WindowCascadeItem: TMenuItem;
    WindowMinimizeAll1: TWindowMinimizeAll;
    WindowMinimizeItem: TMenuItem;
    WindowTileHorizontal1: TWindowTileHorizontal;
    WindowTileItem: TMenuItem;
    WindowTileItem2: TMenuItem;
    WindowTileVertical1: TWindowTileVertical;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ApplicationEvents1: TApplicationEvents;
    SaveLog1: TMenuItem;
    N2: TMenuItem;
    Action1: TAction;
    Contents1: TMenuItem;
    N3: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure Contents1Click(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure SaveLog1Click(Sender: TObject);
    procedure tbbTraceClick(Sender: TObject);
    procedure tbbVerboseClick(Sender: TObject);
    procedure tbbWatchClick(Sender: TObject);
    procedure ToolButton14Click(Sender: TObject);
  private
    fBuffer: ByteArray;
    fBufPosn: Integer;
    fBufSize: Integer;
    hLog: integer;
    { Private declarations }
    fCurr_Sniff: Integer;
    fLogName: string;
    fLogNo: Integer;
    fLogPath: string;
    fTraceState: Integer;
    fVerboseState: Integer;
    fWatchState: Boolean;
    MsgChild: TForm;
    MsgCnt: Integer;
    function CreateMDIChild(const Name: string): TForm;
    function GetNext_Sniff: Integer;
    procedure SetBuffer(const Value: ByteArray);
    procedure SetBufSize(const Value: Integer);
    procedure SetTraceState(const Value: Integer);
    procedure SetVerboseState(const Value: Integer);
    procedure SetWatchState(const Value: Boolean);
    procedure UpdateTrace;
    procedure UpdateVerbose;
    procedure UpdateWatch;
  protected
    function AppHelp(Command: Word; Data: NativeInt; var CallHelp: boolean):
        Boolean;
    procedure CloseLog;
    procedure FlushBuffer(Close: boolean);
    procedure SendMsg(const msg: UTF8String);
    procedure SendToBuffer(const msg: UTF8String);
    procedure OnSniffDone(var Msg: TMessage); message WM_SNIFF_STOP;
    procedure OnSniffStart(var Msg: TMessage); message WM_SNIFF_START;
    function PrepareLog: Boolean;
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    property Buffer: ByteArray read fBuffer write SetBuffer;
    property BufPosn: Integer read fBufPosn write fBufPosn;
    property BufSize: Integer read fBufSize write SetBufSize;
    property Curr_Sniff: Integer read fCurr_Sniff;
    property LogName: string read fLogName write fLogName;
    property LogPath: string read fLogPath write fLogPath;
    property Next_Sniff: Integer read GetNext_Sniff;
    property TraceState: Integer read fTraceState write SetTraceState;
    property VerboseState: Integer read fVerboseState write SetVerboseState;
    property WatchState: Boolean read fWatchState write SetWatchState;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure ReleaseMe(const Child: TForm);
  end;

var
  SnifferForm: TSnifferForm;

implementation

{$R *.dfm}

uses CHILDWIN, about, Vcl.HtmlHelpViewer;

{$WARN SYMBOL_PLATFORM off}

const
  CRLF = #13#10;
  SNIFF_MASK = $FFFFFF;

  UnitNames: array [1..43] of string = (
        'COMMON.CPP    ', 'CRC32.CPP     ', 'CRCTAB.CPP    ', 'CRYPT.CPP     ',
        'DZFRAME.CPP   ', 'DZOPER.CPP    ', 'ENTER.CPP     ', 'HELPERS.CPP   ',
        'INGMTCH.CPP   ', 'UINFLATE.CPP  ', 'UNZCRYPT.CPP  ', 'UNZEXEC.CPP   ',
        'UNZFIO.CPP    ', 'UNZINF.CPP    ', 'UNZOP.CPP     ', 'UNZPROC.CPP   ',
        'UNZSS.CPP     ', 'UNZSUP.CPP    ', 'UNZWIN32.CPP  ', 'UNZXPLODE.CPP ',
        'UNZXTRCT.CPP  ', 'UTIL.CPP      ', 'ZBITS.CPP     ', 'ZCRYPT.CPP    ',
        'ZDEFLATE.CPP  ', 'ZIPDFLT.CPP   ', 'ZIPFILE.CPP   ', 'ZIPFIO.CPP    ',
        'ZIPFNC.CPP    ', 'ZIPMAIN.CPP   ', 'ZIPOP.CPP     ', 'ZIPPRC.CPP    ',
        'ZIPREAD.CPP   ', 'ZIPSEL.CPP    ', 'ZIPSS.CPP     ', 'ZIPUP.CPP     ',
        'ZIPWIN32.CPP  ', 'ZMATCH.CPP    ', 'ZSTRINGS.CPP  ', 'ZTREES.CPP    ',
        'DZImport.cpp  ', 'DZ_Strw.cpp   ', 'DZRaw.cpp     ');

  ZMUnitNames: array [1..35] of string = (
        'ZipMstr     ', 'ZMArgSplit  ', 'ZMCenDir    ', 'ZMCentral  ',
        'ZMCompat    ', 'ZMCore      ', 'ZMCtx       ', 'ZMDefMsgs  ',
        'ZMDelZip    ', 'ZMDlg       ', 'ZMDllLoad   ', 'ZMDllOpr   ',
        'ZMDrv       ', 'ZMEOC       ', 'ZMExtrLZ77  ', 'ZMFileOpr  ',
        'ZMHash      ', 'ZMInflt     ', 'ZMIRec      ', 'ZMLister   ',
        'ZMMatch     ', 'ZMModOpr    ', 'ZMMsg       ', 'ZMMsgStr   ',
        'ZMSFXInt    ', 'ZMStructs   ', 'ZMUTF8      ', 'ZMUtils    ',
        'ZMWFuncs    ', 'ZMWorkFile  ', 'ZMWUtils    ', 'ZMWZip     ',
        'ZMXcpt      ', 'ZMZipFile   ', 'ZMZippedOpr ');
const
  IDH_ZipSniffer  =  1000;
  IDH_General 		=  1001;
  IDH_ToolButtons =  1002;
  IDH_MenuItems   =  1003;

function To_UTF8(const wstr: TZWString): UTF8String;
var
  cnt: integer;
  wcnt: integer;
begin
  wcnt := Length(wstr);
  cnt := WideCharToMultiByte(CP_UTF8, 0, pWideChar(wstr), wcnt, nil, 0, nil, nil);
  if cnt > 0 then
  begin
    SetLength(Result, cnt);
    {cnt :=} WideCharToMultiByte(CP_UTF8, 0, pWideChar(wstr), wcnt,
      PAnsiChar(@Result[1]), cnt, nil, nil);
  end;
end;

{ TFormF }

                                   
function UTF8ToStr(const raw: pAnsiChar; len: integer): TZString;
var
  wcnt:  integer;
  wtemp: TZWString;
begin
  Result := '';
  wcnt := MultiByteToWideChar(CP_UTF8, 0, raw, len, nil, 0);
  if wcnt > 0 then
  begin
    SetLength(wtemp, wcnt);
    {wcnt :=} MultiByteToWideChar(CP_UTF8, 0, raw, len,
      pWideChar(wtemp), wcnt);
  end
  else
    exit;  // invalid
{$IFDEF UNICODE}
  Result := PWideChar(@wtemp[1]);
{$ELSE}
  Result := To_UTF8(wtemp);
{$ENDIF}
end;

function UTF8ToWideStr(const raw: pAnsiChar; len: integer): TZWString;
var
//  cnt:   integer;
  wcnt:  integer;
  wtemp: TZWString;
begin
  Result := '';
  wcnt := MultiByteToWideChar(CP_UTF8, 0, raw, len, nil, 0);
  if wcnt > 0 then
  begin
    SetLength(wtemp, wcnt);
    {wcnt :=} MultiByteToWideChar(CP_UTF8, 0, raw, len,
      pWideChar(wtemp), wcnt);
    Result := pWideChar(@wtemp[1]);
  end;
end;
            
function UTF8ToWide(const astr: UTF8String; len: integer): TZWString;
begin
  Result := '';
  if len < 0 then
    len := Length(astr);
  Result := UTF8ToWideStr(PAnsiChar(@astr[1]), len);
end;

procedure TSnifferForm.FormCreate(Sender: TObject);
begin
  Application.OnHelp := AppHelp;
  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'ZipSniffer.chm';
end;

procedure TSnifferForm.AfterConstruction;
begin
  inherited;
  fBufSize := 16 * 1024;
  fBufPosn := 0;
  hLog := -1;
  fLogPath := '.\';
  fLogNo := 0;
  SetLength(fBuffer, fBufSize);
  MsgCnt := 0;
  MsgChild := nil;
  fWatchState := True;
end;

function TSnifferForm.AppHelp(Command: Word; Data: NativeInt; var CallHelp:
    boolean): Boolean;
begin
  Result := {True;//}False;
  HtmlHelp(Handle, PChar(Application.HelpFile), HH_HELP_CONTEXT, Data);
  CallHelp := False;
end;

//function TSnifferForm.AppHelp(
//  Command: Word; Data: Longint; var CallHelp: Boolean) : Boolean;
//var helpstring : String;
//begin
//  helpstring := Screen.ActiveForm.Name + '.hlp';
//  MessageDlg(
//    'Using ' + helpstring + ' as the help file.',
//    mtInformation, [mbOK], 0);
//  Application.HelpFile := helpstring;
//  CallHelp := True;
//end;


procedure TSnifferForm.ApplicationEvents1Idle(Sender: TObject; var Done:
    Boolean);
begin
  UpdateWatch;
  UpdateTrace;
  UpdateVerbose;
end;

procedure TSnifferForm.BeforeDestruction;
begin
  fBuffer := nil;
  inherited;
end;

procedure TSnifferForm.CloseLog;
var
  htmp: integer;
begin
  htmp := hLog;
  hLog := -1;
  if htmp <> -1 then
    FileClose(htmp);
end;

procedure TSnifferForm.Contents1Click(Sender: TObject);
begin
  Application.HelpContext(1001);
end;

function TSnifferForm.CreateMDIChild(const Name: string): TForm;
var
  Child: TMDIChild;
begin
  { create a new MDI child window }
  Child := TMDIChild.Create(Application);
  Child.Caption := Name;
  Child.Main := self;
  if FileExists(Name) then
    Child.Load(Name);
  Result := Child;
end;

procedure TSnifferForm.FileExit1Execute(Sender: TObject);
begin
  FlushBuffer(True);
  Close;
end;

procedure TSnifferForm.FileNew1Execute(Sender: TObject);
begin
  CreateMDIChild('NONAME' + IntToStr(MDIChildCount + 1));
end;

procedure TSnifferForm.FileOpen1Execute(Sender: TObject);
begin
  if OpenDialog.Execute then
    CreateMDIChild(OpenDialog.FileName);
end;

procedure TSnifferForm.FlushBuffer(Close: boolean);
var
   done: cardinal;
begin
  // flush the buffer
  if PrepareLog then
  begin
    if (not WriteFile(hLog, Buffer[0], BufPosn, done, nil)) or
      (done <> Cardinal(BufPosn)) then
    begin
      { TODO : give error }
      CloseLog;
    end;
    if Close then
      CloseLog;
  end; 
  BufPosn := 0;
end;

procedure TSnifferForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FlushBuffer(True);
  CloseLog;
end;

procedure TSnifferForm.FormShow(Sender: TObject);
begin
  UpdateTrace;
  UpdateVerbose;
end;

function TSnifferForm.GetNext_Sniff: Integer;
begin
  fCurr_Sniff := (fCurr_Sniff + 1) and Sniff_Mask;
  if fCurr_Sniff = 0 then
    fCurr_Sniff := 1;
  Result := fCurr_Sniff;
end;

procedure TSnifferForm.HelpAbout1Execute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TSnifferForm.OnSniffDone(var Msg: TMessage);
var
  nx: integer;
begin
  nx := msg.LParam;
  if InSendMessage then
    ReplyMessage(1);
{$IFDEF UNICODE}
  SendMsg(To_UTF8(Format('%X> FINISHED', [nx and SNIFF_MASK])));
{$ELSE}
  SendMsg(Format('%X> FINISHED', [nx and SNIFF_MASK]));
{$ENDIF}
end;                                                                

procedure TSnifferForm.OnSniffStart(var Msg: TMessage);
var
  lp: Integer;
  s: string;
  sender: HWND;
  nl: integer;
  ovr, nx: Integer;
{$IFDEF UNICODE}
  astr: AnsiString;
{$ENDIF}
begin
  ovr := 0;
  if TraceState <> 0 then
  begin
    if TraceState = 2 then
      ovr := $6000000   // force trace
    else
      ovr := $4000000;  // force off
  end
  else
  if VerboseState <> 0 then
  begin
    if VerboseState = 2 then
      ovr := $5000000  // force verbose
    else
      ovr := $4000000; // force off
  end;
  Sender := Msg.WParam;
  lp := msg.LParam;
  nx := Next_Sniff;
  ovr := ovr or nx;
  if InSendMessage then
    ReplyMessage(ovr);  
  if sender <> 0 then
  begin
    SetLength(s, 256);
    nl := GetWindowText(sender, pChar(s), 255);
    if nl > 0 then
      s := pChar(s)
    else
{$IFDEF UNICODE}
    begin
      SetLength(astr, 256);
      nl := GetWindowTextA(sender, pAnsiChar(astr), 255);
      if nl > 0 then
        s := String(AnsiString(pAnsiChar(astr)))
      else
        s := '';
    end;
{$ELSE}
      s := '';
{$ENDIF}
  end;
//  s := IntToHex(nx, 6) + s + ' = ' + '  [' + IntToStr(lp and 3) + ']';
  s := Format('%X> = %s (%d)',[nx, s, lp and 3]);
{$IFDEF UNICODE}
  SendMsg(To_UTF8(s));
{$ELSE}
  SendMsg(s);
{$ENDIF}
end;

function TSnifferForm.PrepareLog: Boolean;
const BOM: array [0..2] of byte = ($EF, $BB, $BF);
var done: Cardinal;
begin
  Result := true;
  if hLog <> -1 then
    exit;  // already open
  inc(fLogNo);
  LogName := IncludeTrailingBackslash(LogPath) + 'dzlog' + IntToHex(fLogNo,4) + '.txt';
  hLog := FileCreate(LogName);
//  Result := hLog <> -1;
  if hLog <> -1 then
  begin
    if WriteFile(hLog, BOM, 3, done, nil) and (done = 3) then
      Result := true;
    if not Result then
      CloseLog;
  end;
end;

procedure TSnifferForm.ReleaseMe(const Child: TForm);
begin
  if Child = MsgChild then
    MsgChild := nil;
end;

procedure TSnifferForm.SaveLog1Click(Sender: TObject);
begin
  FlushBuffer(True);
end;
  {
procedure TSnifferForm.btnClearClick(Sender: TObject);
begin
  Memo2.Lines.Clear;
  fCurr_Sniff := 0; // reset
end;

procedure TSnifferForm.btnSaveLogClick(Sender: TObject);
begin
  SaveDialog1.Title := 'Save Log File';
   if SaveDialog1.Execute then
   begin
     if FileExists(SaveDialog1.FileName) then
        DeleteFile(SaveDialog1.FileName);
     Memo2.Lines.SaveToFile(SaveDialog1.FileName);
   end;
end;
   }
procedure TSnifferForm.SendMsg(const msg: UTF8String);
begin
  SendToBuffer(msg);
  if WatchState then
  begin
    if MsgChild = nil then
      MsgChild := CreateMDIChild('NONAME' + IntToStr(MDIChildCount + 1));
    if MsgChild is TMDIChild then
    begin
      if not TMDIChild(MsgChild).AppendLine(msg) then
        WatchState := False;
    end;
  end;
end;

procedure TSnifferForm.SendToBuffer(const msg: UTF8String);
var
  len: integer;
  s: UTF8String;
begin
  s := msg + CRLF;
  len := Length(s);
  if BufPosn + len > BufSize then
    FlushBuffer(false);
  Move(s[1], Buffer[BufPosn], len);
  BufPosn := BufPosn + len;
end;

procedure TSnifferForm.SetBuffer(const Value: ByteArray);
begin
  // do not allow changes
//  if fBuffer <> Value then
//  begin
//    fBuffer := Value;
//  end;
end;

procedure TSnifferForm.SetBufSize(const Value: Integer);
begin
  if fBufSize <> Value then
  begin
    FlushBuffer(false);
    SetLength(fBuffer, Value);
    fBufSize := Value;
  end;
end;

// states 0 = no force, 1= force off, 2= force on
procedure TSnifferForm.SetTraceState(const Value: Integer);
begin
  if fTraceState <> Value then
  begin
    fTraceState := Value;
    UpdateTrace;
  end;
end;

// states 0 = no force, 1= force off, 2= force on
procedure TSnifferForm.SetVerboseState(const Value: Integer);
begin
  if fVerboseState <> Value then
  begin
    fVerboseState := Value;
    UpdateVerbose;
  end;
end;

procedure TSnifferForm.SetWatchState(const Value: Boolean);
begin
  if fWatchState <> Value then
  begin
    fWatchState := Value;
  end;
end;

procedure TSnifferForm.tbbTraceClick(Sender: TObject);
begin
  TraceState := TraceState + 1;
end;

procedure TSnifferForm.tbbVerboseClick(Sender: TObject);
begin
  VerboseState := VerboseState + 1;
end;

procedure TSnifferForm.tbbWatchClick(Sender: TObject);
begin
  WatchState := not WatchState;
end;

procedure TSnifferForm.ToolButton14Click(Sender: TObject);
begin
  FlushBuffer(True);
end;

procedure TSnifferForm.UpdateTrace;
const
  TStates: array [0..2] of string = ('Trace ?', 'Trace off', 'Trace on');
begin
  if fTraceState > 2 then
    fTraceState := 0;
  tbbTrace.ImageIndex := 18 + fTraceState;
  tbbTrace.Indeterminate := fTraceState = 0;
  StatusBar.Panels[3].Text := TStates[fTraceState];
end;

procedure TSnifferForm.UpdateVerbose;
const
  VStates: array [0..2] of string = ('Verbose ?', 'Verbose off', 'Verbose on');
begin
  if fVerboseState > 2 then
    fVerboseState := 0;
  tbbVerbose.ImageIndex := 21 + fVerboseState;
  tbbVerbose.Indeterminate := fVerboseState = 0;
  StatusBar.Panels[2].Text := VStates[fVerboseState];
end;

procedure TSnifferForm.UpdateWatch;
var
  n: Integer;
  t: String;
begin
  if WatchState then
  begin
    n := 24;
    t := 'Watch';
  end
  else
  begin
    n := 25;
    t := 'log';
  end;
  tbbWatch.ImageIndex := n;
  tbbWatch.Down := WatchState;
  StatusBar.Panels[1].Text := t;
end;

procedure TSnifferForm.WMCopyData(var Msg: TWMCopyData);
var
  ustr: UTF8String;
  s: string;
  nx: Integer;
  err: Cardinal;
  fno, lno: integer;
  tno: Integer;
begin
  nx := Integer(Msg.From);
  SetLength(ustr, Msg.CopyDataStruct.cbData);
  Move(Msg.CopyDataStruct.lpData^, ustr[1], Msg.CopyDataStruct.cbData);
  err := Msg.CopyDataStruct.dwData;
  if InSendMessage then
    ReplyMessage(1);
  ustr := pAnsiChar(ustr);  // remove trailing nul
  if (err and $80000000) <> 0 then
  begin
    fno := (err shr 24) and $7F;
    lno := (err shr 12) and $FFF;
    tno := (err shr 8) and 7;
    if (fno > 0) and (fno <= HIGH(UnitNames)) then
     s := Format('%X> %s %d %d %d:',[nx, UnitNames[fno], lno, tno, err and $FF])
    else
      s := Format('%X> %d %d %X:',[nx, fno, lno, err and $FF]);
// format is [SniffNo] Unit Line Type Error
  end
  else
  if err > $1FF then
  begin
    // component error
    fno := (err shr 23) and $3F;
    lno := (err shr 10) and $1FFF;
    if (fno > 0) and (fno <= HIGH(ZMUnitNames)) then
     s := Format('%X> %s %d %X:',[nx, ZMUnitNames[fno], lno, err and $1F])
    else
      s := Format('%X> %d %d %X:',[nx, fno, lno, err and $1F]);
// format is [SniffNo] Unit Line Error
  end
  else
    s := Format('%X> %X:',[nx, err]);
{$IFDEF UNICODE}
  SendMsg(To_UTF8(s) + ustr);
{$ELSE}
  SendMsg(s + ustr);
{$ENDIF}
  inc(MsgCnt);
  StatusBar.Panels[0].Text := IntToStr(MsgCnt);
end;

end.
