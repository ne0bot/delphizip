unit SnifferMain2;
(* ***************************************************************************
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.
 Copyright (C) 2013, 2014  by Russell J. Peters, Roger Aelbrecht.

   This file is part of TZipMaster Version 1.9.

TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009-2014 Russell Peters and Roger Aelbrecht

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
//modified 2014-01-03
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

interface

uses Windows, SysUtils, Classes, Contnrs, Graphics, Forms, Controls,
  Menus, StdCtrls, Dialogs, Buttons, Messages, ExtCtrls, ComCtrls, StdActns,
  ActnList, ToolWin, ImgList, AppEvnts, {$ifdef UNICODE}Actions,{$endif} GridWin3;

const
  ZIPSNIFFERBUILD: String =  '1.9.2.0002';
  ZIPSNIFFERDATE: String  =  '03/02/2014';
  ZIPSNIFFERPRIV: Integer = 1920002;

const
  SZipMasterSniffer = 'ZipMaster Sniffer V2';
  STZipSniffer = 'TZipSniffer';
  WM_SNIFF_START = WM_APP + $3F42;
//  WM_SNIFF_STOP = WM_APP + $3F44;

//type
////  ByteArray = Array of byte;
//{$IFDEF UNICODE}
//  TZString = string;
//  TZWString = string;
//{$ELSE}
//  UTF8String = type string;
//  TZString = UTF8String;
//  TZWString = WideString;
//{$ENDIF}
type
 TForceVerbosity = (fvDefault, fvOff, fvVerbose, fvTrace, fvNoisy);

type
  TZMCryData = packed record
    Size: DWORD;
    Ticks: DWord;
    Flags: DWord;
    SniffNo: DWORD;
    Err: DWORD;
    MBuf: Char;
  end;
  PZMCryData = ^TZMCryData;

type
  TFields = array[0..5] of string;

type
  TZipSniffer = class(TForm)
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
    tbtnBreak: TToolButton;
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
    tbbPlay: TToolButton;
    ApplicationEvents1: TApplicationEvents;
    SaveLog1: TMenuItem;
    N2: TMenuItem;
    Action1: TAction;
    Contents1: TMenuItem;
    N3: TMenuItem;
    Verbosity1: TMenuItem;
    ForceTrace1: TMenuItem;
    ForceOff1: TMenuItem;
    ForceVerbose1: TMenuItem;
    ForceFull1: TMenuItem;
    ForceDefault1: TMenuItem;
    CleanWindow: TAction;
    Logs1: TMenuItem;
    Folder1: TMenuItem;
    ActionLogFolder: TAction;
    BaseName1: TMenuItem;
    ActionLogBaseName: TAction;
    N4: TMenuItem;
    LogNew: TMenuItem;
    ActionLog_New: TAction;
    ActionLog_Close: TAction;
    LogClose: TMenuItem;
    N5: TMenuItem;
    ActionLog_Pause: TAction;
    LogPause: TMenuItem;
    LockWindow: TAction;
    ActionStop: TAction;
    ActionPlay: TAction;
    tbbRecord: TToolButton;
    Action2: TAction;
    ActionRecord: TAction;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ActionAuto: TAction;
    tbnNoisy: TToolButton;
    tbnTrace: TToolButton;
    tbnVerbose: TToolButton;
    ToolButton14: TToolButton;
    procedure CleanWindowExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure Contents1Click(Sender: TObject);
    procedure FileExit1Execute(Sender: TObject);
    procedure FileNew1Execute(Sender: TObject);
    procedure FileOpen1Execute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure HelpAbout1Execute(Sender: TObject);
    procedure ActionLogBaseNameExecute(Sender: TObject);
    procedure ActionLog_CloseExecute(Sender: TObject);
    procedure ActionLogFolderExecute(Sender: TObject);
    procedure ActionLog_NewExecute(Sender: TObject);
    procedure ActionLog_PauseExecute(Sender: TObject);
    procedure ForceVerbosityClick(Sender: TObject);
    procedure SaveLog1Click(Sender: TObject);
    procedure tbtnBreakClick(Sender: TObject);
    procedure tbbPlayClick(Sender: TObject);
    procedure tbbRecordClick(Sender: TObject);
    procedure VBtnClick(Sender: TObject);
    procedure ToolButton19Click(Sender: TObject);
  private
    FActiveChild: TForm;
    FBuffer: array of AnsiChar;
    FBufPosn: Integer;
    hLog: integer;
    FBufSize: Integer;
    fCurr_Sniff: Integer;
    FLogBase: string;
    fLogName: string;
    fLogNo: Integer;
    FLogPath: string;
    FPlayState: boolean;
    FRecordState: boolean;
    FShowTicks: boolean;
    FVBtnState: TForceVerbosity;
    FWatchState: Boolean;
{$IFDEF DEBUG_QUEUE}
    MaxQueueCount: Integer;
{$ENDIF}
    MsgChild: TForm;
    MsgCnt: Integer;
    MsgQueue: TQueue;
    ScrollState: boolean;
    Updater: TThread;
    procedure AddToBuffer(const s: String);
    function CreateMDIChildGridWin(const Name: string): TMDIGridWin;
    function GetNext_Sniff: Integer;
    function GetVerbosity: TForceVerbosity;
    procedure PushAsData(const s: string; nx: Integer);
    procedure SetBufSize(const Value: Integer);
    procedure SetLogBase(const Value: string);
    procedure SetLogPath(const Value: string);
    procedure SetShowTicks(const Value: boolean);
    procedure SetVbtn(ClickTag: Integer);
    procedure SetVBtns(level: TForceVerbosity);
    procedure SetVBtnState(const Value: TForceVerbosity);
    procedure SetVerbosity(const Value: TForceVerbosity);
    procedure SetWatchState(const Value: Boolean);
    procedure ThreadDone(Sender: TObject);
    procedure UpdateButtons;
    function VBtn(Tag: Integer): TToolButton;
  protected
{$ifdef UNICODE}
    function AppHelp(Command: Word; Data: NativeInt;
             var CallHelp: boolean): Boolean;
{$endif}
    procedure CloseLog;
    procedure FlushBuffer(Close: boolean);
    procedure OnSniffStart(var Msg: TMessage); message WM_SNIFF_START;
    function PrepareLog: Boolean;
    procedure UpdateGrid;
    procedure WMCopyData(var Msg: TWMCopyData); message WM_COPYDATA;
    property BufPosn: Integer read FBufPosn write FBufPosn;
    property BufSize: Integer read FBufSize write SetBufSize;
    property Curr_Sniff: Integer read fCurr_Sniff;
    property LogBase: string read FLogBase write SetLogBase;
    property LogName: string read fLogName write fLogName;
    property LogPath: string read FLogPath write SetLogPath;
    property Next_Sniff: Integer read GetNext_Sniff;
    property ShowTicks: boolean read FShowTicks write SetShowTicks;
    property VBtnState: TForceVerbosity read FVBtnState write SetVBtnState;
    property Verbosity: TForceVerbosity read GetVerbosity write SetVerbosity;
    property WatchState: Boolean read FWatchState write SetWatchState;
  public
    procedure AddGridData(Data: TGData);
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure EatMsgs;
    procedure ReleaseMe(const Child: TForm);
    procedure SendToBuffer(const msgs: TFields); overload;
    property ActiveChild: TForm read FActiveChild write FActiveChild;
    property PlayState: boolean read FPlayState write FPlayState;
    property RecordState: boolean read FRecordState write FRecordState;
  end;

var
  SnifferForm: TZipSniffer;

implementation

{$R *.dfm}

{$WARN SYMBOL_PLATFORM off}
{$WARN UNIT_PLATFORM OFF}

uses
  {$ifdef UNICODE}HtmlHelpViewer,{$endif} FileCtrl, about, SnifferThrd;


const
  CRLF = #13#10;
  SNIFF_MASK = $FFFFFF;
  VBtnTagV = 10;
  VBtnTagT = 11;
  VBtnTagN = 12;

const
  VerStates: array [0..4] of string = (
    'Default', 'Off', 'Verbose', 'Trace', 'Noisy');

const
  IDH_ZipSniffer  =  1000;
  IDH_General 		=  1001;
  IDH_ToolButtons =  1002;
  IDH_MenuItems   =  1003;

procedure TZipSniffer.CleanWindowExecute(Sender: TObject);
begin
  if MsgChild is TMDIGridWin then
    TMDIGridWin(MsgChild).CleanWindow;
end;

procedure TZipSniffer.AddGridData(Data: TGData);
var
  R: Integer;
  s: String;
  TheGrid: TMDIGridWin;
begin
  if not(MsgChild is TMDIGridWin) or
    ((MsgChild is TMDIGridWin) and (TMDIGridWin(MsgChild).IsReadOnly)) then
  begin
    s := FormatDateTime('yymmdd-hhmmss', Now);
    MsgChild := CreateMDIChildGridWin(s);
  end;
  TheGrid := MsgChild as TMDIGridWin;
  R := TheGrid.AppendData(Data);
  if R < 0 then
    PlayState := false;
  StatusBar.Panels[5].Text := IntToStr(R);
end;

procedure TZipSniffer.AddToBuffer(const s: String);
var
  len: integer;
begin
  len := Length(s) * SizeOf(Char);
  if BufPosn + len > BufSize then
    FlushBuffer(false);
  Move(s[1], FBuffer[BufPosn], len);
  BufPosn := BufPosn + len;
end;
procedure tdDisposeProc(aData : pointer);
Begin
  FreeMem(aData);
End;

procedure TZipSniffer.FormCreate(Sender: TObject);
begin
{$ifdef UNICODE}
  Application.OnHelp := AppHelp;
  Application.HelpFile := ExtractFilePath(Application.ExeName) + 'ZipSniffer.chm'; 
{$endif}
  FVBtnState := fvVerbose;
  VBtnState := fvDefault;
end;

procedure TZipSniffer.AfterConstruction;
begin
  inherited;
  fBufSize := 32{16} * 1024;
  fBufPosn := 0;
  hLog := -1;
  fLogPath := '.\';
  fLogNo := 0;
  SetLength(fBuffer, fBufSize);
  MsgCnt := 0;
  MsgChild := nil;
  FWatchState := True;
  fRecordState := True;
  fPlayState := True;
//  FVerValue := fvDefault;//-1; // cause update
  FVBtnState := fvVerbose; // force update
  Verbosity := fvDefault;
  LogPath := '';
  MsgQueue := TQueue.Create;
  Updater := nil;
end;

{$ifdef UNICODE}
function TZipSniffer.AppHelp(Command: Word; Data: NativeInt;
          var CallHelp: boolean): Boolean;
begin
  Result := False;
  HtmlHelp(Handle, PChar(Application.HelpFile), HH_HELP_CONTEXT, Data);
  CallHelp := False;
end;
{$endif}

procedure TZipSniffer.ApplicationEvents1Idle(Sender: TObject; var Done:
    Boolean);
begin
{$IFDEF DEBUG_QUEUE}
  StatusBar.Panels[0].Text := IntToStr(MsgCnt) + '  ' + IntToStr(MsgQueue.Count)
     + '  ' + IntToStr(MaxQueueCount);
{$ELSE}
  StatusBar.Panels[0].Text := IntToStr(MsgCnt);
{$ENDIF}
  if MsgQueue.Count > 0 then
    EatMsgs;
  UpdateButtons;
  Done := MsgQueue.Count = 0;
  if Done then
    UpdateGrid;
  if MsgChild is TMDIGridWin then
  begin
    ToolButton19.Enabled := True;
    TMDIGridWin(MsgChild).AutoScroll := not ScrollState;
  end
  else
    ToolButton19.Enabled := False;
end;

procedure TZipSniffer.BeforeDestruction;
begin
  fBuffer := nil;
  MsgQueue.Free;
  inherited;
end;

procedure TZipSniffer.CloseLog;
var
  htmp: integer;
begin
  htmp := hLog;
  hLog := -1;
  if htmp <> -1 then
    FileClose(htmp);
end;

procedure TZipSniffer.Contents1Click(Sender: TObject);
begin
  Application.HelpContext(1001);
end;

function TZipSniffer.CreateMDIChildGridWin(const Name: string): TMDIGridWin;
begin
  { create a new MDI child window }
  Result := TMDIGridWin.Create(Application);
  Result.Caption := Name;
  Result.Main := self;
end;

procedure TZipSniffer.FileExit1Execute(Sender: TObject);
begin
  FlushBuffer(True);
  Close;
end;

procedure TZipSniffer.FileNew1Execute(Sender: TObject);
var
  s: String;
begin
  if (MsgChild is TMDIGridWin) then
    TMDIGridWin(MsgChild).IsReadOnly := True;
  s := FormatDateTime('yymmdd-hhmmss', Now);
  MsgChild := CreateMDIChildGridWin(s);
end;

procedure TZipSniffer.FileOpen1Execute(Sender: TObject);
var
  Child: TMDIGridWin;
begin
  OpenDialog.InitialDir := LogPath;
  OpenDialog.DefaultExt := '.txt';
  if OpenDialog.Execute then
  begin
    if (MsgChild is TMDIGridWin) then
      TMDIGridWin(MsgChild).IsReadOnly := True;
    Child := CreateMDIChildGridWin(OpenDialog.FileName);
    Child.Load(OpenDialog.FileName);
  end;
end;

procedure TZipSniffer.FlushBuffer(Close: boolean);
var
  done: cardinal;
begin
  if BufPosn > 0 then
  begin
    // flush the buffer
    if PrepareLog then
    begin
      if (not WriteFile(hLog, FBuffer[0], BufPosn, done, nil)) or
        (done <> Cardinal(BufPosn)) then
      begin
        { TODO : give error }
        CloseLog;
      end//;
      else
      if Close then
        CloseLog;
    end;
  end
  else
  if Close and (hLog <> -1) then
    CloseLog;
  BufPosn := 0;
end;

function TZipSniffer.GetVerbosity: TForceVerbosity;
begin
  Result := fvDefault;
  if ForceOff1.Checked then
    Result := fvOff
  else
  if ForceVerbose1.Checked then
    Result := fvVerbose
  else
  if ForceTrace1.Checked then
    Result := fvTrace
  else
  if ForceFull1.Checked then
    Result := fvNoisy;
end;

procedure TZipSniffer.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FlushBuffer(True);
  CloseLog;
end;

procedure TZipSniffer.FormShow(Sender: TObject);
begin
  UpdateButtons;
end;

function TZipSniffer.GetNext_Sniff: Integer;
begin
  fCurr_Sniff := (fCurr_Sniff + 1) and Sniff_Mask;
  if fCurr_Sniff = 0 then
    fCurr_Sniff := 1;
  Result := fCurr_Sniff;
end;

function GetTempDirectory: String;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
end;

procedure TZipSniffer.EatMsgs;
var
  s: String;
begin
  if (Updater <> nil) or (MsgQueue.Count <= 0) then
    Exit;
  if not (MsgChild is TMDIGridWin) or ((MsgChild is TMDIGridWin) and
        (TMDIGridWin(MsgChild).IsReadOnly)) then
    begin
      s := FormatDateTime('yymmdd-hhmmss', Now);
      MsgChild := CreateMDIChildGridWin(s);
    end;
  Updater := TThrdUpdater.Create(Self, MsgQueue);
  Updater.OnTerminate := ThreadDone;
end;

procedure TZipSniffer.HelpAbout1Execute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TZipSniffer.ActionLogBaseNameExecute(Sender: TObject);
begin
//
end;

procedure TZipSniffer.ActionLog_CloseExecute(Sender: TObject);
begin
  FlushBuffer(True);
end;

procedure TZipSniffer.ActionLogFolderExecute(Sender: TObject);
const
  SELDIRHELP = 1000;
var
  dir: String;
begin
  dir := GetTempDirectory;
  LogPath := dir;
  if SelectDirectory(dir, [sdAllowCreate, sdPerformCreate, sdPrompt], SELDIRHELP)
  then
    LogPath := dir;
end;

procedure TZipSniffer.ActionLog_NewExecute(Sender: TObject);
begin
  FlushBuffer(True);
end;

procedure TZipSniffer.ActionLog_PauseExecute(Sender: TObject);
begin
  RecordState := not RecordState;
  ActionLog_Pause.Checked := not RecordState;
  tbbRecord.Down := not RecordState;
end;

procedure TZipSniffer.ForceVerbosityClick(Sender: TObject);
var
  VTag: Integer;
  V: TForceVerbosity;
begin
  if Sender is TMenuItem then
  begin
    VTag := TMenuItem(Sender).Tag - 15;
    if (VTag >= Ord(fvDefault)) and (VTag <= Ord(fvNoisy)) then
    begin
      V := TForceVerbosity(VTag);
      Verbosity := V;
      VBtnState := V;
    end;
  end;
end;

procedure TZipSniffer.OnSniffStart(var Msg: TMessage);
const
  VerbStr: array [0..3] of string = ('', 'Verbose', 'Trace', 'Noisy');
var
  lp: Integer;
  nl: integer;
  nx: Integer;
  ovr: Integer;
  s: string;
  sender: HWND;
{$IFDEF UNICODE}
  astr: AnsiString;
{$ENDIF}
  v: Integer;
begin
  if not WatchState then
  begin
    if InSendMessage then
      ReplyMessage(0);
    exit;
  end;
  ovr := 0;
  case Verbosity of
    fvDefault: ovr := 0;
    fvOff: ovr := 4;
    fvVerbose: ovr := 5;
    fvTrace: ovr := 6;
    fvNoisy: ovr := 7;
  end;
  ovr := ovr shl 24;
  Sender := Msg.WParam;
  lp := msg.LParam;
  nx := Next_Sniff;
  ovr := ovr or nx;
  if InSendMessage then
    ReplyMessage(ovr);
  s := '';
  if sender <> 0 then
  begin
    SetLength(s, 256);
    nl := GetWindowText(sender, pChar(s), 255);
    if nl > 0 then
      s := pChar(s)
    else
      s := '';
{$IFDEF UNICODE}
    if s = '' then
    begin
      SetLength(astr, 256);
      nl := GetWindowTextA(sender, pAnsiChar(astr), 255);
      if nl > 0 then
        s := String(AnsiString(pAnsiChar(astr)));
    end;
{$ENDIF}
  end;
  if s = '' then
    s := '<unknown>';
  v := lp and 3;
  if v > 0 then
    s := s + ' (' + VerbStr[v] +')';
  s := 'Logging on: ' + s;
  PushAsData(s, nx);
end;

function TZipSniffer.PrepareLog: Boolean;
{$IFDEF UNICODE}
const BOM: array [0..1] of byte = ($FF, $FE);
{$ELSE}
const BOM: array [0..2] of byte = ($EF, $BB, $BF);
{$ENDIF}
var
  done: Cardinal;
  s: string;
begin
  Result := true;
  if hLog <> -1 then
    exit;  // already open
  if LogBase = '' then
    LogBase := 'zmlog';
  inc(fLogNo);
  s := FormatDateTime('yymmdd-hhmmss', Now);
  LogName := LogPath + LogBase + s + '.txt';
  hLog := FileCreate(LogName);
  if hLog <> -1 then
  begin
{$IFDEF UNICODE}
    if WriteFile(hLog, BOM, 2, done, nil) and (done = 2) then
{$ELSE}
    if WriteFile(hLog, BOM, 3, done, nil) and (done = 3) then
{$ENDIF}
      Result := true;
    if not Result then
    begin
      BufPosn := 0; // cannot write it
      CloseLog;
    end;
  end;
end;

procedure TZipSniffer.PushAsData(const s: string; nx: Integer);
var
  DatSize: DWord;
  Len: Integer;
  MsgData: PZMCryData;
  P: PAnsiChar;
  w: DWord;
begin
  {$IFDEF UNICODE}
    w := 2;
  {$ELSE}
    W := 0;
  {$ENDIF}
    Len := Length(s) + 1; // need end null
    DatSize := Len * sizeof(Char);
    DatSize := ((DatSize + SizeOf(TZMCryData)) or 63) + 1;
    GetMem(MsgData, DatSize);
    MsgData.Size := DatSize;
    MsgData.Ticks := GetTickCount;
    MsgData.SniffNo := nx;
    MsgData.Flags := w;
    MsgData.Err := 0;
    P := PAnsiChar(@MsgData.MBuf);
    Move(PChar(@s[1])^, P^, Len * sizeof(Char));
    MsgQueue.Push(MsgData);
    inc(MsgCnt);
{$IFDEF DEBUG_QUEUE}
    if MsgQueue.Count > MaxQueueCount then
      MaxQueueCount := MsgQueue.Count;
{$ENDIF}
    EatMsgs;
end;

procedure TZipSniffer.ReleaseMe(const Child: TForm);
begin
  if Child = MsgChild then
    MsgChild := nil;
end;

procedure TZipSniffer.SaveLog1Click(Sender: TObject);
begin
  FlushBuffer(True);
end;

procedure TZipSniffer.SendToBuffer(const msgs: TFields);
var
  I: Integer;
  s: String;
begin
  s := msgs[0];
  for I := 1 to 5 do
    s := s + #9 + msgs[I];
  AddToBuffer(s + CRLF);
end;

procedure TZipSniffer.SetBufSize(const Value: Integer);
begin
  if FBufSize <> Value then
  begin
    FlushBuffer(false);
    SetLength(FBuffer, Value);
    FBufSize := Value;
  end;
end;

procedure TZipSniffer.SetLogBase(const Value: string);
var
  p: string;
begin
  p := Value;
  if p = '' then
    p := 'zmlog';
  if LogBase <> p then
  begin
    FLogBase := p;
    StatusBar.Panels[3].Text := p;
  end;
end;

procedure TZipSniffer.SetLogPath(const Value: string);
var
  p: string;
begin
  p := Value;
  if p = '' then
    p := GetTempDirectory;
  p := IncludeTrailingBackslash(p);
  if LogPath <> p then
  begin
    FLogPath := p;
    StatusBar.Panels[4].Text := p;
  end;
end;

procedure TZipSniffer.SetShowTicks(const Value: boolean);
begin
  FShowTicks := Value;
end;

procedure TZipSniffer.SetVbtn(ClickTag: Integer);
begin
  case ClickTag of
    VBtnTagV: // V
    begin
      VBtn(VBtnTagN).ImageIndex := 30;
      VBtn(VBtnTagT).ImageIndex := 33;
      VBtn(VBtnTagV).ImageIndex := 35;
    end;
    VBtnTagT: // T
    begin
      VBtn(VBtnTagN).ImageIndex := 30;
      VBtn(VBtnTagT).ImageIndex := 32;
      VBtn(VBtnTagV).ImageIndex := 36;
    end;
    VBtnTagN: // N
    begin
      VBtn(VBtnTagN).ImageIndex := 29;
      VBtn(VBtnTagT).ImageIndex := 33;
      VBtn(VBtnTagV).ImageIndex := 36;
    end;
  end;
end;

procedure TZipSniffer.SetVBtns(level: TForceVerbosity);
begin
  case level of
    fvDefault:
    begin
      VBtn(VBtnTagN).ImageIndex := 30;
      VBtn(VBtnTagT).ImageIndex := 33;
      VBtn(VBtnTagV).ImageIndex := 36;
    end;
    fvOff:
    begin
      VBtn(VBtnTagN).ImageIndex := 31;
      VBtn(VBtnTagT).ImageIndex := 34;
      VBtn(VBtnTagV).ImageIndex := 37;
    end;
    fvVerbose: SetVbtn(VBtnTagV);
    fvTrace: SetVbtn(VBtnTagT);
    fvNoisy: SetVbtn(VBtnTagN);
  end;
  VBtnState := level;
end;

procedure TZipSniffer.SetVBtnState(const Value: TForceVerbosity);
begin
  if FVBtnState <> Value then
  begin
    FVBtnState := Value;
    SetVBtns(Value);
  end;
end;

procedure TZipSniffer.SetVerbosity(const Value: TForceVerbosity);
var
  S: string;
  V: Integer;
begin
  V := 1 shl Ord(Value);
  ForceDefault1.Checked := (V and 1) <> 0;
  ForceOff1.Checked := (V and 2) <> 0;
  ForceVerbose1.Checked := (V and 4) <> 0;
  ForceTrace1.Checked := (V and 8) <> 0;
  ForceFull1.Checked := (V and 16) <> 0;
  SetVBtns(Value);
  case Value of
    fvDefault: S := 'Default';
    fvOff: S := 'Off';
    fvVerbose: S := 'Verbose';
    fvTrace: S := 'Trace';
    fvNoisy: S := 'Noisy';
  end;
  StatusBar.Panels[2].Text := S;
end;

procedure TZipSniffer.SetWatchState(const Value: Boolean);
begin
  if FWatchState <> Value then
  begin
    FWatchState := Value;
  end;
end;

procedure TZipSniffer.tbtnBreakClick(Sender: TObject);
begin
//  WatchState := not WatchState;
end;

procedure TZipSniffer.tbbPlayClick(Sender: TObject);
begin
  PlayState := not PlayState;
end;

procedure TZipSniffer.tbbRecordClick(Sender: TObject);
begin
  RecordState := not RecordState;
  ActionLog_Pause.Checked := not RecordState;
end;

procedure TZipSniffer.VBtnClick(Sender: TObject);
var
  ClickTag: Integer;
  ns: TForceVerbosity;
  BtnVerbosity: TForceVerbosity;
begin
  if Sender is TToolButton then
  begin
    ClickTag := TToolButton(Sender).Tag;
    if VBtnState = fvOff then
      ns := fvDefault
    else
    begin
      BtnVerbosity := TForceVerbosity(Ord(fvVerbose) + ClickTag - VBtnTagV);
      if BtnVerbosity = VBtnState then
        ns := fvOff // clicked a set button
      else
        ns := BtnVerbosity;
    end;
    Verbosity := ns;
  end;
end;

procedure TZipSniffer.UpdateButtons;
var
  n: Integer;
  t: String;
begin
  if WatchState then
  begin
    t := 'Active';
  end
  else
  begin
    t := 'Inactive';
  end;
  StatusBar.Panels[1].Text := t;
  if PlayState then
  begin
    n := 21;
  end
  else
  begin
    n := 22;
  end;
  tbbPlay.ImageIndex := n;
  tbbPlay.Down := PlayState;
  if RecordState then
  begin
    n := 23;
  end
  else
  begin
    n := 24;
  end;
  tbbRecord.ImageIndex := n;
  tbbRecord.Down := RecordState;
end;

procedure TZipSniffer.WMCopyData(var Msg: TWMCopyData);
var
  CData: PZMCryData;
  DatSize: DWord;
  MsgData: PZMCryData;
begin
  if not WatchState then
  begin
    if InSendMessage then
      ReplyMessage(0);
    exit;
  end;
  MsgData := nil;
  DatSize := Msg.CopyDataStruct.cbData;
  if DatSize >= SizeOf(TZMCryData) then
  begin
    CData := Msg.CopyDataStruct.lpData;
    GetMem(MsgData, 1 + (DatSize or 511));
    Move(CData^, MsgData^, DatSize);
    CData^.Size := DatSize;
  end;
  if InSendMessage then
    ReplyMessage(2);
  if MsgData <> nil then
    MsgQueue.Push(MsgData);
  inc(MsgCnt);
{$IFDEF DEBUG_QUEUE}
  if MsgQueue.Count > MaxQueueCount then
    MaxQueueCount := MsgQueue.Count;
{$ENDIF}
  EatMsgs;
end;


procedure TZipSniffer.ThreadDone(Sender: TObject);
begin
  Updater := nil;
end;

procedure TZipSniffer.ToolButton19Click(Sender: TObject);
var
  g: Integer;
begin
  ScrollState := not ScrollState;
  ToolButton19.Down := not ScrollState;
  if ScrollState then
    g := 26
  else
    g := 25;
  ToolButton19.ImageIndex := g;
  if MsgChild is TMDIGridWin then
    TMDIGridWin(MsgChild).AutoScroll := not ScrollState;
end;

procedure TZipSniffer.UpdateGrid;
begin
  if MsgChild is TMDIGridWin then
    TMDIGridWin(MsgChild).UpdateGrid;
end;

function TZipSniffer.VBtn(Tag: Integer): TToolButton;
begin
  case (Tag - VBtnTagV) of
    0: Result := tbnVerbose; // V
    1: Result := tbnTrace; // T
    2: Result := tbnNoisy; // N
    else
      raise Exception.Create('Invalid Tag: ' + IntToStr(Tag));
  end;
end;

end.
