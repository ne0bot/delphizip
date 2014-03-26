unit Main;

(* ***************************************************************************
  Main.pas - main form TZipResMaker for ZipMaster
 Copyright (C) 2009 - 2014 by Russell J. Peters, Roger Aelbrecht.

	This file is part of TZipMaster Version 1.9.2.x

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
//modified 2014-01-25
{$I '..\..\ZipVers.inc'}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ComCtrls, Menus, ExtCtrls, PathParser, Opers,
  Base, {ToolHelper,} AppEvnts, Template;

const
  ZIPRESMAKERBUILD: String =  '1.9.2.0004';
  ZIPRESMAKERDATE: String  =  '25/01/2014';
  ZIPRESMAKERPRIV: Integer = 1920004;
  
type
  ZPaths = (zphRoot, zphDll, zphSFX, zphLang, zphRes, zphBin,
    zphBRCC, zphUPX);

type
  TForm1 = class(TForm, IGUILink)
    A1:           TMenuItem;
    btnBrowsDest: TButton;
    BtnBrowseBCB: TButton;
    btnBrowseDll: TButton;
    btnBrowseLang: TButton;
    btnBrowseRoot: TButton;
    btnBrowseSFX: TButton;
    btnBrowseSFXBin: TButton;
    btnBrowseUPX: TButton;
    btnBuildRes:  TButton;
    BtnSFXAll:    TButton;
    btnSFXNone:   TButton;
    btnSFXRefresh: TButton;
    Build1:       TMenuItem;
    DefTags:      TMenuItem;
    btnSFXMake:   TButton;
    cbDlls:       TCheckBox;
    cbSFXUPX:     TCheckBox;
    cbUseUPX:     TCheckBox;
    cbSFXRes:     TCheckBox;
    edBRCC:       TEdit;
    edDll:        TEdit;
    edDllVer:     TEdit;
    edLang:       TEdit;
    edRes:        TEdit;
    edRoot:       TEdit;
    edSFX:        TEdit;
    edSFXBin:     TEdit;
    edUPX:        TEdit;
    Exit1:        TMenuItem;
    File1:        TMenuItem;
    Help1:        TMenuItem;
    Label1:       TLabel;
    Label10:      TLabel;
    Label2:       TLabel;
    Label3:       TLabel;
    Label4:       TLabel;
    Label5:       TLabel;
    Label7:       TLabel;
    Label8:       TLabel;
    Label9:       TLabel;
    lbSFXLangs:   TCheckListBox;
    MainMenu1:    TMainMenu;
    Memo1:        TMemo;
    N1:           TMenuItem;
    Pages:        TPageControl;
    Panel1:       TPanel;
    Panel3:       TPanel;
    Panel12:      TPanel;
    pnlDllUPX:    TPanel;
    Resources1:   TMenuItem;
    Script1:      TMenuItem;
    StaticText12: TStaticText;
    StaticText13: TStaticText;
    StaticText14: TStaticText;
    StaticText15: TStaticText;
    StaticText10: TStaticText;
    StaticText11: TStaticText;
    StaticText9:  TStaticText;
    StaticText6:  TStaticText;
    StaticText5:  TStaticText;
    StaticText4:  TStaticText;
    StaticText3:  TStaticText;
    StaticText2:  TStaticText;
    StaticText1:  TStaticText;
    TabSheet1:    TTabSheet;
    TabSheet3:    TTabSheet;
    TabSheet4:    TTabSheet;
    TabSheet5:    TTabSheet;
    TabSheet6:    TTabSheet;
    btnMakeUnits: TButton;
    OpenDialog1:  TOpenDialog;
    lblSFXVer:    TLabel;
    Separate1:    TMenuItem;
    N2:           TMenuItem;
    Combined1:    TMenuItem;
    lblDllVers:   TLabel;
    ApplicationEvents1: TApplicationEvents;
    cbSFXURES: TCheckBox;
    cbSFXUUPX: TCheckBox;
    lblSFXUVer: TLabel;
    lblSFXVer1: TLabel;
    lblSFXUVer1: TLabel;
    rgSFXAll: TRadioGroup;
    Label11: TLabel;
    lblSFXCompress: TLabel;
    procedure A1Click(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
//  procedure btnAllLangClick(Sender: TObject);
    procedure btnBrowsDestClick(Sender: TObject);
    procedure BtnBrowseBCBClick(Sender: TObject);
    procedure btnBrowseDllClick(Sender: TObject);
    procedure btnBrowseLangClick(Sender: TObject);
    procedure btnBrowseRootClick(Sender: TObject);
    procedure btnBrowseSFXBinClick(Sender: TObject);
    procedure btnBrowseSFXClick(Sender: TObject);
    procedure btnBrowseUPXClick(Sender: TObject);
    procedure btnBuildResClick(Sender: TObject);
    procedure btnBuildSFXDefStrClick(Sender: TObject);
    procedure btnMakeUnitsClick(Sender: TObject);
//    procedure btnNoneLangClick(Sender: TObject);
//  procedure btnRefreshLangClick(Sender: TObject);
    procedure BtnSFXAllClick(Sender: TObject);
    procedure btnSFXLangNewClick(Sender: TObject);
    procedure btnSFXNoneClick(Sender: TObject);
    procedure btnSFXRefreshClick(Sender: TObject);
    procedure btnSFXMakeClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Combined1Click(Sender: TObject);
    procedure edBRCCChange(Sender: TObject);
    procedure edDllChange(Sender: TObject);
    procedure DefTagsClick(Sender: TObject);
    procedure edLangChange(Sender: TObject);
    procedure edResChange(Sender: TObject);
    procedure edRootChange(Sender: TObject);
    procedure edSFXBinChange(Sender: TObject);
    procedure edSFXChange(Sender: TObject);
    procedure edUPXChange(Sender: TObject);
    procedure edRCBinChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure PagesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure Separate1Click(Sender: TObject);
    procedure TabSheet1Show(Sender: TObject);
  private
    fBusy:       Boolean;
    FSaveCursor: TCursor;
    SysPaths:    TPathParser;
    procedure BrowseClick(p: ZPaths; const title: String);
    function GetBuild(const pth: String): Integer;
    function GetPathText(z: ZPaths): String;
    procedure OnLocalFolder(var ident: String; var Handled: Boolean);
    function GetZipBuild: Integer;
//  function LangOfRC(const FileName: string): string;
    function Path(const where: String): String;
    function PathEdit(p: ZPaths): TEdit;
    procedure SetBusy(const Value: Boolean);
    procedure SetPathText(z: ZPaths; const Value: String);
    procedure SFXLangList(var arg: TStrings);
    function SFXMake: integer;
    property PathText[z: ZPaths]: String read GetPathText write SetPathText;
  protected
    fEdChange: Boolean;
    function BadPath: TEdit;
    function BrowsePath(const ForWhat: String; const def: String): String;
    function DefFileName(const fn, DefName: String): String;
    function ProjectPath(pth: ZPaths): String;
    procedure RefreshSFXLangs(const where: String);
    procedure SetChecks(lb: TCheckListBox; state: Boolean);
    procedure SetPaths;
    procedure ChangedToPage(TabIndex: Integer);
    function TempFileName(const prefix: String): String;
//  procedure UpdateList(const fs: String);
    function UseUPX: boolean;
  public
    procedure AfterConstruction; override;
    procedure Show(const s: String; Err: Boolean = False);
    {IGUILink}
    procedure ErrorMsg(const msg: string);
    function GetOptionBool(Index: Integer; var arg: boolean): boolean;
    function GetOptionInt(Index: Integer; var arg: Integer): boolean;
    function GetOptionString(Index: Integer; var arg: string): boolean;
    function GetOptionStrings(Index: Integer; arg: TStrings): boolean;
    procedure ShowMsg(const msg: string);
    function Syms(const arg: string): string;
    { Public declarations }
    function WantDlls: Boolean;
//  function WantLangs: Boolean;
    property Busy: Boolean Read fBusy Write SetBusy;
    property DllBuild: Integer Read GetZipBuild;
    property EdChange: Boolean Read FEdChange Write FEdChange;
  end;

var
  Form1:    TForm1;

implementation

{$R *.dfm}

{$INCLUDE '..\..\ZipVers.inc'}
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

uses  ShellApi, TypInfo, FileCtrl, Registry, IniFiles, ABOUT,
  ZMUtils, ZMDelZip, ZMMsg, ZipMstr, SFXOprs, StrOprs, DLLOprs,
  ResOprs;

{ TForm1 }

type
  TResBlock = array of Word;

const
  //  zphRoot, zphDll, zphSFX, zphLang, zphRC, zphRes, zphBin, zphBRCC, zphUPX
  defPaths: array [zphRoot..zphUPX] of String = (
    '', '$(ZM_Root)\DLL', '$(ZM_Root)\SFX', '$(ZM_Root)\Res\Lang',
    '$(ZM_Root)\Res', '$(ZM_Root)\RES', '$(BCB)\BIN', '');
  PathNames: array [zphRoot..zphUPX] of String = (
    'Root', 'Dll', 'SFX', 'Lang', 'Res', 'SFXBin', 'BRCC32', 'UPX');
  PathVerify: array [zphRoot..zphUPX] of String = (
    'ZipMstr.pas', 'DelZip192.dll', DefSFXExe, 'ZipMsgUS.rc', '', '',
    'BRCC32.exe', 'UPX.EXE');

type
  TLangs = record
    s: String;
    p: Integer;
  end;

function AddLBItem(lb: TCheckListBox; const n: String): Integer;
begin
  for Result := 0 to lb.Count - 1 do
    if AnsiSameText(n, lb.Items[Result]) then
      exit;
  Result := lb.Items.Add(n);
end;

function FindRes(const blk: TResBlock; id: Integer): String;
var
  bp: Integer;
  DatSiz: Cardinal;
  fid: Integer;
  HedSiz: Cardinal;
  hp: Integer;
  l: Cardinal;
  rid: Integer;
  sz: Cardinal;
  ws: WideString;
begin
  Result := '';
  fid := id div 16;
  try
    bp := 0;
    while (bp + 9) < HIGH(Blk) do
    begin
      bp := (bp + 1) and $7fffE;  // dword align
      DatSiz := (blk[bp + 1] shl 16) + blk[bp];
      HedSiz := (blk[bp + 3] shl 16) + blk[bp + 2];
      if (HedSiz + DatSiz) < 8 then
        break;
      //      Assert((HedSiz + DatSiz) >= 8, 'header error');
      sz := (HedSiz + DatSiz) - 8;
      hp := bp + 4;
      Inc(bp, 4 + (sz div 2));
      if blk[hp] <> $ffff then
        continue;  // bad res type
      if blk[hp + 1] <> 6 then
        continue;      // not string table
      if blk[hp + 2] <> $ffff then
        continue;
      rid := pred(blk[hp + 3]);
      if fid <> rid then
        continue;
      rid := rid * 16;
      Inc(hp, (HedSiz - 8) div 2);
      ws := '';
      while rid < id do
      begin
        l := blk[hp];
        Inc(hp, l + 1);
        Inc(rid);
      end;
      l := blk[hp];
      if l <> 0 then
      begin
        SetLength(ws, l);
        move(blk[hp + 1], ws[1], l * sizeof(Widechar));
        Result := ws;
        Result := StringReplace(Result, #10, #13#10, [rfReplaceAll]);
        break;
      end;
      break;
    end;
  except
    Result := '';
  end;
end;

procedure TForm1.A1Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TForm1.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
var
  good: Boolean;
begin
  // only when needed
  if EdChange then
  begin
    good := BadPath = nil;
    btnBuildRes.Enabled := good;
    Build1.Enabled := good;
    EdChange := False;
  end;
  if separate1.Checked and combined1.Checked then
    Combined1.Checked := False;
  if not (separate1.Checked or combined1.Checked) then
    separate1.Checked := True;
end;

function VerStr(v: Integer): String;
begin
  Result := VersStr(v, false);
end;

function TForm1.BadPath: TEdit;
var
  BadFolder: Boolean;
  clr: Integer;
  good: Boolean;
  i: ZPaths;
  Optional: Boolean;
  s: String;
  ver: Integer;
begin
  Result := nil;
  for I := zphRoot to zphUPX do
  begin
    good := False;
    Optional := i in [zphUPX];
    if (i = zphUPX) and (edUPX.Text <> '') then
      Optional := False;
    BadFolder := not DirExists(Path(PathText[i]));
    if not BadFolder then
    begin
      s := PathVerify[i];
      if s <> '' then
      begin
        s := DelimitPath(Path(PathText[i]), True) + s;
        good := FileExists(s);
        if (i = zphSFX) then
        begin
          if good then
          begin
            ver := ExeVers(Path('$(ZM_SFX)\' + DefSFXExe));
            if ver < MinSFXVers then
            begin
              lblSFXVer.Caption := 'too old or invalid';
              good := False;
            end
            else
              lblSFXVer.Caption := {'version: ' +} VerStr(ver);
          end
          else
          begin
            lblSFXVer.Caption := 'missing';
            good := True;  // does not have to exist
          end;
        end;
        if (i = zphDll) then
        begin
          if good then
          begin
            Ver  := DllBuild;
            edDllVer.Text := VerStr(Ver);
            lblDllVers.Caption := VerStr(Ver);
            good := (Ver >= MinDllVers);
          end
          else
          begin
            edDllVer.Text := 'none';
            lblDllVers.Caption := 'none';
          end;
          cbDlls.Enabled := good;
          Label7.Caption := DelZipDll_Name + ' version';
        end;
      end
      else
        good := True;
    end;
    if good then
    begin
      clr := clWindowText;
    end
    else
    begin
      clr := clGrayText;
      if (Result = nil) and not Optional then
        Result := PathEdit(i);
    end;
    if i = zphUPX then
    begin
      pnlDllUPX.Visible := good;
      cbSFXUPX.Visible  := good;
      cbSFXUUPX.Visible  := good;
      lblSFXCompress.Visible := good;
    end;
    TEdit(PathEdit(i)).Font.Color := clr;
  end;
end;

procedure TForm1.BrowseClick(p: ZPaths; const title: String);
var
  ed: TEdit;
begin
  ed := PathEdit(p);
  ed.Text := BrowsePath(title, PathConcat(edRoot.Text, defPaths[p]));
  ed.SetFocus;
end;

function TForm1.BrowsePath(const ForWhat, def: String): String;
var
  dir: String;
begin
  dir := Path(ForWhat);
  Result := ExtractFilePath(dir);
  if Result = '' then
    Result := Path('$(ZM_ROOT)');
  if Result = '' then
    Result := 'c:\';
  if not FileCtrl.SelectDirectory(Result, [sdAllowCreate, sdPerformCreate, sdPrompt], -1) then
    Result := '';
  if Result = '' then
    Result := def
  else
    Result := DelimitPath(Result, True);
end;

//procedure TForm1.btnAllLangClick(Sender: TObject);
//begin
//SetChecks(lbLangs, True);
//end;

procedure TForm1.btnBrowsDestClick(Sender: TObject);
begin
  edRes.Text := BrowsePath('Resources', PathConcat(edRoot.Text, defPaths[zphRes]));
  edRes.SetFocus;
end;

procedure TForm1.BtnBrowseBCBClick(Sender: TObject);
begin
  BrowseClick(zphBRCC, 'BRCC32.exe');
end;

procedure TForm1.btnBrowseDllClick(Sender: TObject);
begin
  BrowseClick(zphDll, 'Dll');
end;

procedure TForm1.btnBrowseLangClick(Sender: TObject);
begin
  BrowseClick(zphLang, 'Languages');
end;

procedure TForm1.btnBrowseRootClick(Sender: TObject);
begin
  edRoot.Text := BrowsePath('Root',
    ExpandFileName(PathConcat(ExtractFilePath(Application.ExeName),
    defPaths[zphRoot])));
  edRoot.SetFocus;
  SetPaths;
end;

procedure TForm1.btnBrowseSFXBinClick(Sender: TObject);
begin
  BrowseClick(zphBin, 'SFX Binary');
end;

procedure TForm1.btnBrowseSFXClick(Sender: TObject);
begin
  BrowseClick(zphSFX, 'ZMSFX.exe');
end;

procedure TForm1.btnBrowseUPXClick(Sender: TObject);
begin
  edUPX.Text := BrowsePath('UPX', '');
  edUPX.SetFocus;
end;

procedure TForm1.btnBuildResClick(Sender: TObject);
var
  ResOps: IResOpers;
begin
  if not Busy then
    try
      Busy := True;
      ResOps := TResOperations.Create(Self);
      ResOps.BuildCompRes;
    finally
      Busy := False;
    end;
end;

procedure TForm1.btnBuildSFXDefStrClick(Sender: TObject);
var
  SFXOps: ISFXOpers;
begin
  SFXOps := TSFXOperations.Create(Self);
  SFXOps.WriteDefTable;
end;

procedure TForm1.btnMakeUnitsClick(Sender: TObject);
var
  Maker: IStrOpers;
begin
  Show('Reading default strings and identifiers');
  Maker := TStrMaker.Create(Self);
  Maker.MakeUnits;
end;

//procedure TForm1.btnNoneLangClick(Sender: TObject);
//begin
//  SetChecks(lbLangs, False);
//end;

//procedure TForm1.btnRefreshLangClick(Sender: TObject);
//begin
//UpdateList(Path('$(ZM_Lang)\ZipMsg*.rc'));
//end;

procedure TForm1.BtnSFXAllClick(Sender: TObject);
begin
  SetChecks(lbSFXLangs, True);
end;

procedure TForm1.btnSFXLangNewClick(Sender: TObject);
begin
  OpenDialog1.DefaultExt := '.txt';
end;

procedure TForm1.btnSFXNoneClick(Sender: TObject);
begin
  SetChecks(lbSFXLangs, False);
end;

procedure TForm1.btnSFXRefreshClick(Sender: TObject);
begin
  RefreshSFXLangs('$(ZM_lang)\SFXStr_*.txt');
end;

{$WARN SYMBOL_DEPRECATED OFF}
procedure TForm1.btnSFXMakeClick(Sender: TObject);
begin
  SFXMake;
end;

function TForm1.SFXMake: integer;
var
  SFXOps: ISFXOpers;
begin
  Result := -9;
  if Busy then
    exit;
  SFXOps := TSFXOperations.Create(Self);
  Busy := True;
  try
    Result := SFXOps.Make;
  finally
    Busy := False;
    btnSFXMake.Enabled := True;
  end;
end;

procedure TForm1.AfterConstruction;
begin
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  v: String;
begin
  v := '$(BCB)';
  Show(v + ' ->> ' + Path(v));
  v := '$(BDS)';
  Show(v + ' ->> ' + Path(v));
  v := '$(BDSPROJECTSDIR)';
  Show(v + ' ->> ' + Path(v));
  v := '$(Personal)';
  Show(v + ' ->> ' + Path(v));
  v := '$(windows)';
  Show(v + ' ->> ' + Path(v));
  v := '$(ohno)';
  Show(v + ' ->> ' + Path(v), True);
  v := '$(ZM_RC_BIN)';
  Show(v + ' ->> ' + Path(v));
  v := '$(ZM_RC)';
  Show(v + ' ->> ' + Path(v));
  v := '$(ZM_Res)';
  Show(v + ' ->> ' + Path(v));
  v := BinName;//'$(ZM_Res)';
  Show(v + ' ->> ' + Path(v));
  PathText[zphDLL] := 'd:\code\ZM180\DLL';
  Show(PathText[zphDLL]);
  PathText[zphRes] := '$(ZM_ROOT)\res';
  Show(PathText[zphRes]);
end;

procedure TForm1.DefTagsClick(Sender: TObject);
begin
  btnBuildRes.Visible  := False;
  TabSheet6.TabVisible := True;
  Pages.ActivePageIndex := {3;//}4;
end;

procedure TForm1.edBRCCChange(Sender: TObject);
begin
  fEdChange := True;
end;

procedure TForm1.edDllChange(Sender: TObject);
begin
  fEdChange := True;
end;

procedure TForm1.edLangChange(Sender: TObject);
begin
  fEdChange := True;
  //  HasFile(Sender, 'zipmsgUS.res');
//  btnRefreshLangClick(Sender);
  btnSFXRefreshClick(Sender);
end;

procedure TForm1.edResChange(Sender: TObject);
begin
  fEdChange := True;
end;

procedure TForm1.edRootChange(Sender: TObject);
begin
  fEdChange := True;
end;

procedure TForm1.edSFXBinChange(Sender: TObject);
var
  f: String;
begin
  fEdChange := True;
  f := Path(edSFXBin.Text);
  if ExtractFileName(f) <> '' then
    f := ExtractFilePath(f);
  if DirExists(f) then
    edSFXBin.Font.Color := clWindowText
  else
    edSFXBin.Font.Color := clGrayText;
end;

procedure TForm1.edSFXChange(Sender: TObject);
begin
  fEdChange := True;
end;

procedure TForm1.edUPXChange(Sender: TObject);
begin
  fEdChange := True;
end;

procedure TForm1.ErrorMsg(const msg: string);
begin
  Show(msg, True);
end;

procedure TForm1.edRCBinChange(Sender: TObject);
begin
  fEdChange := True;
end;

function TForm1.DefFileName(const fn, DefName: String): String;
begin
  Result := Path(fn);
  if ExtractFileName(Result) = '' then
    Result := IncludeTrailingBackSlash(Result) + DefName;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
  i: Integer;
  Ini: TIniFile;
  n: String;
  zp: ZPaths;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.INI'));
  try
    if {False then //} GetAsyncKeyState(VK_CONTROL) >= 0 then
    begin
      ini.WriteBool('Resources', 'Separate', Separate1.Checked);
      ini.WriteBool('Resources', 'Combined', Combined1.Checked);
      for zp := zphRoot to zphUPX do
        ini.WriteString('Paths', PathNames[zp], PathEdit(zp).Text);
      ini.WriteBool('Dlls', 'Include', cbDlls.Checked);
      ini.WriteBool('Dlls', 'UPX', cbUseUPX.Checked);
      ini.EraseSection('Languages');
//      ini.WriteInteger('Languages', 'KnownCount', lbLangs.Count);
//      for i := 0 to pred(lbLangs.Count) do
//      begin
//        if lbLangs.Checked[i] then
//          n := '+'
//        else
//          n := '-';
//        n := n + lbLangs.Items[i];
//        ini.WriteString('Languages', 'Item' + IntToStr(i), n);
//      end;
      ini.EraseSection('SFXLanguages');
      ini.WriteInteger('SFXLanguages', 'KnownCount', lbSFXLangs.Count);
      for i := 0 to pred(lbSFXLangs.Count) do
      begin
        if lbSFXLangs.Checked[i] then
          n := '+'
        else
          n := '-';
        n := n + lbSFXLangs.Items[i];
        ini.WriteString('SFXLanguages', 'Item' + IntToStr(i), n);
      end;
      ini.WriteInteger('SFX', 'ALL', rgSFXAll.ItemIndex);
      ini.WriteBool('SFX', 'RES', cbSFXRes.Checked);
      ini.WriteBool('SFX', 'UPX', cbSFXUPX.Checked);
      ini.WriteBool('SFX', 'URES', cbSFXURes.Checked);
      ini.WriteBool('SFX', 'UUPX', cbSFXUUPX.Checked);
//      ini.WriteBool('SFX', 'MakeBin', cbMakeSFXBin.Checked);
      ini.WriteString('SFX', 'StubName', edSFXBin.Text);
    end;
  finally
    Ini.Free;
  end;
  FreeAndNil(SysPaths);
end;

procedure TForm1.FormCreate(Sender: TObject);

  function GetRegistryValue(KeyName, Name: String): String;
  var
    Registry: TRegistry;
  begin
    Registry := TRegistry.Create(KEY_READ);
    try
      Registry.RootKey := HKEY_CURRENT_USER;
      // False because we do not want to create it if it doesn't exist
      Registry.OpenKey(KeyName, False);
      Result := Registry.ReadString(Name);
    finally
      Registry.Free;
    end;
  end;

var
  fini: String;
  i: Integer;
  ini: TIniFile;
  k: Integer;
  ks: short;
  rs: String;
  s: String;
  t: String;
  x: Integer;
  zp: ZPaths;
begin
  Pages.ActivePageIndex := 0;
  TabSheet6.TabVisible := False;
  pnlDllUPX.Visible := False;
  s  := ExpandFileName(ExtractFilePath(Application.ExeName) + '..\..\');
  SysPaths := TPathParser.Create(True, OnLocalFolder);
  fini := ChangeFileExt(Application.ExeName, '.ini');
  ks := GetAsyncKeyState(VK_CONTROL);
  if FileExists(fini) and (ks >= 0) then
  begin
    ini := TIniFile.Create(fini);
    try
      // must set before setting paths
//      lbLangs.Clear;
//      k := ini.ReadInteger('Languages', 'KnownCount', 0);
//      for i := 0 to pred(k) do
//      begin
//        t := ini.ReadString('Languages', 'Item' + IntToStr(i), '');
//        if length(t) < 2 then
//          continue;
//        //        x := lbLangs.Items.Add(copy(t, 2, length(t) - 1));
//        x := AddLBItem(lbLangs, copy(t, 2, length(t) - 1));
//        lbLangs.Checked[x] := t[1] = '+';
//        lbLangs.ItemEnabled[x] := False;
//      end;
      lbSFXLangs.Clear;
      k := ini.ReadInteger('SFXLanguages', 'KnownCount', 0);
      for i := 0 to pred(k) do
      begin
        t := ini.ReadString('SFXLanguages', 'Item' + IntToStr(i), '');
        if length(t) < 2 then
          continue;
        //        x := lbSFXLangs.Items.Add(copy(t, 2, length(t) - 1));
        x := AddLBItem(lbSFXLangs, copy(t, 2, length(t) - 1));
        lbSFXLangs.Checked[x] := t[1] = '+';
        lbSFXLangs.ItemEnabled[x] := False;
      end;
      cbDlls.Checked := ini.ReadBool('DLLs', 'Include', True);
      cbUseUPX.Checked := ini.ReadBool('DLLs', 'UPX', True);
      cbSFXUPX.Checked := ini.ReadBool('SFX', 'UPX', True);
      cbSFXRes.Checked := ini.ReadBool('SFX', 'RES', True);
      cbSFXUUPX.Checked := ini.ReadBool('SFX', 'UUPX', True);
      cbSFXURes.Checked := ini.ReadBool('SFX', 'URES', True);
//      cbMakeSFXBin.Checked := ini.ReadBool('SFX', 'MakeBin', False);
      rgSFXAll.ItemIndex := ini.ReadInteger('SFX', 'ALL', 0);
      edSFXBin.Text  := ini.ReadString('SFX', 'StubName', '');
      Separate1.Checked := ini.ReadBool('Resources', 'Separate', False);
      Combined1.Checked := ini.ReadBool('Resources', 'Combined', True);
      // will cause Languages to update
      for zp := zphRoot to zphUPX do
        PathEdit(zp).Text :=
          ini.ReadString('Paths', PathNames[zp],
          DelimitPath(defPaths[zp], True));
    finally
      ini.Free;
    end;
  end
  else
  begin
    if ks >= 0 then
      rs := GetRegistryValue('Software\DelphiZip\ZipMaster19', 'InstallPath');
    if rs <> '' then
      edRoot.Text := rs
    else
      edRoot.Text := s;
    SetPaths;
  end;
  fEdChange := True;    // check valid
end;

function TForm1.GetBuild(const pth: String): Integer;
var
  fspec: String;
  hndl: HWND;
  OldMode: Cardinal;
  PrivFunc: function: DWord; stdcall;
begin
  Result := 0;
  fspec  := pth;
  if not FileExists(fspec) then  // must exist where stated
    exit;
  oldMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
    hndl := LoadLibrary(PChar(fspec));
    if hndl > HInstance_Error then
    begin
      @PrivFunc := GetProcAddress(hndl, PChar(DelZipDll_Privfunc));
      if Assigned(@PrivFunc) then
        Result := PrivFunc;
      FreeLibrary(hndl);
    end;
  finally
    SetErrorMode(oldMode);
  end;
end;

 function TForm1.GetOptionBool(Index: Integer; var arg: boolean): boolean;
begin
  Result := True;
  case Index of
    BWANTSFX: arg := cbSFXRes.Enabled and cbSFXRes.Checked;
    BWANTUSFX: arg := cbSFXURes.Enabled and cbSFXURes.Checked;
    BSFXUPX: arg := cbSFXUPX.Visible and cbSFXUPX.Checked;
    BSFXUUPX: arg := cbSFXUUPX.Visible and cbSFXUUPX.Checked;
    BWantASFX: arg := rgSFXAll.ItemIndex = 0;
    BCombinedRes: arg := Combined1.Checked;
    BSeparateRes: arg := Separate1.Checked;
    BWantDLLs: arg := WantDLLs;
    BUseUPX: arg := UseUPX;
    else
      Result := False;
  end;
end;

function TForm1.GetOptionInt(Index: Integer; var arg: Integer): boolean;
begin
  Result := True;
  case Index of
     KDLLBuild: arg := DllBuild;
    else
      Result := False;
  end;
end;

function TForm1.GetOptionString(Index: Integer; var arg: string): boolean;
begin
  Result := True;
  case Index of
    SBINPATHTEXT: arg := PathText[zphBin];
    else
      Result := False;
  end;
end;

function TForm1.GetOptionStrings(Index: Integer; arg: TStrings): boolean;
begin
  Result := True;
  case Index of
    LSFXLangs: SFXLangList(arg);
    else
      Result := False;
  end;
end;

//  TDrive = (dRoot, dLangs, dStrs, dVCL);
 //  ZPaths = (zphRoot, zphRes, zphDll, zphLang, zphVCL, zphSFX, zphUPX);
procedure TForm1.OnLocalFolder(var ident: String; var Handled: Boolean);
var
  I: ZPaths;
  n: String;
begin
  if AnsiSameText(ident, 'ZM_Zip') then
    ident := 'ZM_Res';
  for I := Low(ZPaths) to High(ZPaths) do
  begin
    n := 'ZM_' + PathNames[I];
    if AnsiSameText(ident, n) then
    begin
      ident := ExcludeTrailingBackslash(ProjectPath(i));
      Handled := True;
      break;
    end;
  end;
  //  if not found then
  //    raise Exception.Create('$(' + ident + ') not found');
  //    Show('$(' + ident + ') not found');
end;

function TForm1.GetZipBuild: Integer;
begin
  Result := GetBuild(Path('$(ZM_DLL)\' + DelZipDll_Name));
end;

// TODO: LangOfRC
//function TForm1.LangOfRC(const FileName: string): string;
//var
//S: string;
//SList: TStringList;
//begin
//Result := '';
//if not FileExists(FileName) then
//  Exit;
//SList := TStringList.Create;
//try
//  SList.LoadFromFile(FileName);
//  S := Trim(SList[0]);
//  if (Length(S) > 2) and (S[1] = '/') and (S[2] = '/') then
//    Result := Trim(Copy(S, 3, 400));
//finally
//  SList.Free;
//end;
//end;

procedure TForm1.Combined1Click(Sender: TObject);
begin
  Combined1.Checked := not Combined1.Checked;
  if not Combined1.Checked then
    Separate1.Checked := True;
end;

function TForm1.GetPathText(z: ZPaths): String;
begin
  Result := PathEdit(z).Text;
  if (z > zphRoot) and (Result <> '') then
  begin
    if (Result[1] <> '\') and (Result[1] <> '$') and
      ((Length(Result) > 1) and (Result[2] <> ':')) then
    begin
      // relative to root
      Result := DelimitPath('$(ZM_Root)', True) + Result;
    end;
  end;
end;

procedure TForm1.PagesChange(Sender: TObject);
begin
  if Busy and (Pages.TabIndex <> pred(Pages.PageCount)) then
    Pages.TabIndex := pred(Pages.PageCount);
  btnBuildRes.Visible := Pages.TabIndex <> 4;
  ChangedToPage(Pages.TabIndex);
end;

procedure TForm1.PagesChanging(Sender: TObject; var AllowChange: Boolean);
var
  ed: TEdit;
begin
  if (Sender as TPageControl).ActivePage <> TabSheet1 then
    AllowChange := True
  else
  begin
    ed := BadPath;
    AllowChange := ed = nil;
    if not AllowChange then
    begin
      ed.SetFocus;
    end;
  end;
end;

function TForm1.Path(const where: String): String;
begin
  Result := SysPaths.Parse(where);
  if (Result <> '') and (Result[1] = '<') then
    Show('Invalid Path: ' + where);
end;

//  ZPaths = (zphRoot, zphDll, zphSFX, zphLang, zphRC, zphRes, zphBin,
//    zphBRCC, zphUPX);
function TForm1.PathEdit(p: ZPaths): TEdit;
begin
  case p of
    zphRoot:
      Result := edRoot;
    zphDll:
      Result := edDll;
    zphSFX:
      Result := edSFX;
    zphLang:
      Result := edLang;
    zphRes:
      Result := edRes;
    zphBRCC:
      Result := edBRCC;
    zphBin:
      Result := edSFXBin;
    zphUPX:
      Result := edUPX;
    else
      Result := nil;//edRCBin;
  end;
end;

function TForm1.ProjectPath(pth: ZPaths): String;
var
  ed: TEdit;
  t: String;
begin
  ed := PathEdit(pth);
//if pth = zphRC then
//  Show('rc');
  t  := PathText[pth];
  if t = '' then
  begin
    t := defPaths[pth];
    if pth <> zphRoot then
      t := PathConcat(edRoot.Text, t)
    else
      t := PathConcat(ExtractFilePath(Application.ExeName), t);
    t := ExpandFileName(DelimitPath(t, True));
    ed.Text := t;
  end;
  if AnsiPos('$(', t) <> 0 then
    Result := Path(t)
  else
    Result := t;
end;

procedure TForm1.RefreshSFXLangs(const where: String);
var
  f: String;
  fs: String;
  idx: Integer;
  ii: Integer;
  l: String;
  lst: TCheckListBox;
  p: String;
  sr: TSearchRec;
  SFXOps: ISFXOpers;
begin
  SFXOps := TSFXOperations.Create(Self);
  lst := lbSFXLangs;
  for ii := 0 to pred(lst.Count) do
    lst.ItemEnabled[ii] := False;
  fs := Path(where);
  p := ExtractFilePath(fs);
  if FindFirst(fs, faReadOnly + faArchive, sr) = 0 then
  begin
    repeat
      if (sr.Attr and faDirectory) <> 0 then
        continue;
      if sr.Name[1] = '.' then
        continue;
      f := Uppercase(copy(sr.Name, 8, length(sr.Name) - (7 + 4)));
      if (f = '') or (f = 'US') then  // ignore empty, hidden, default
        continue;
      l := SFXOps.LangName(DelimitPath(p, True) + sr.Name);
      idx := AddLBItem(lbSFXLangs, f + ' : ' + l);
      if idx >= 0 then
      begin
        lst.ItemEnabled[idx] := True;
        lst.Checked[idx] := True;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;


procedure TForm1.Separate1Click(Sender: TObject);
begin
  Separate1.Checked := not Separate1.Checked;
  if not Separate1.Checked then
    Combined1.Checked := True;
end;

procedure TForm1.SetBusy(const Value: Boolean);
begin
  if fBusy <> Value then
  begin
    fBusy := Value;
    btnBuildRes.Visible := not Busy;
    btnSFXMake.Visible := not Busy;
    Build1.Visible := not Busy;
    if Busy then
      Pages.ActivePageIndex := pred(Pages.PageCount);
    if Busy then
    begin
      FSaveCursor := Screen.Cursor;
      Screen.Cursor := crHourGlass;
    end
    else
      Screen.Cursor := FSaveCursor;
  end;
end;

procedure TForm1.SetChecks(lb: TCheckListBox; state: Boolean);
var
  i: Integer;
begin
  for i := 0 to pred(lb.Count) do
    if lb.ItemEnabled[i] then
      lb.Checked[i] := state;
end;

procedure TForm1.SetPaths;
var
  p: ZPaths;
  s: String;
begin
  s := ExpandFileName(edRoot.Text);

  for p := zphDLL to zphUPX do
    PathText[p] := DelimitPath(defPaths[p], True);
  PathEdit(zphUPX).Text := '';
  if not DirExists(Path('$(ZM_DLL)')) then
  begin
    if DirExists(Path('$(ZM_ROOT)\DLL')) then
      PathEdit(zphDll).Text := ExpandFileName(Path('$(ZM_ROOT)\DLL'));
  end;
end;

procedure TForm1.SetPathText(z: ZPaths; const Value: String);
var
  s: String;
  t: String;
begin
  t := Value;
  if (z > zphRoot) and (t <> '') then
  begin
    if (t[1] <> '\') and ((Length(t) > 1) and (t[2] = ':')) then
    begin
      // Try to convert to relative to root
      s := DelimitPath(Path('$(ZM_Root)'), False);
      if (Length(s) <= Length(t)) and AnsiSameText(s, copy(t, 1, Length(s))) then
      begin
        s := copy(t, Length(s) + 1, 500);
        t := '$(ZM_Root)' + s;
      end;
    end;
    // trim macro
    if (Length(t) >= 11) and AnsiSameText('$(ZM_Root)\', copy(t, 1, 11)) then
      t := copy(t, 12, 500);
  end;
  PathEdit(z).Text := t;
end;

procedure TForm1.Show(const s: String; Err: Boolean = False);
var
  m: String;
begin
  Pages.ActivePageIndex := pred(Pages.PageCount);
  if Err then
    m := 'ERROR: ' + s
  else
    m := s;
  Memo1.Lines.Add(m);
end;

procedure TForm1.ShowMsg(const msg: string);
begin
  Show(msg, False);
end;

function TForm1.Syms(const arg: string): string;
begin
  Result := Path(arg);
end;

procedure TForm1.ChangedToPage(TabIndex: Integer);
var
  Ver: integer;
begin
  if Pages.TabIndex = 3 then
  begin
    ver := ExeVers(Path('$(ZM_SFX)\' + DefSFXExe));
    if ver < MinSFXVers then
    begin
      lblSFXVer1.Caption := '-bad-';
      cbSFXUPX.AllowGrayed := True;
      cbSFXRes.AllowGrayed := True;
      cbSFXUPX.Enabled := false;
      cbSFXRes.Enabled := false;
    end
    else
    begin
      lblSFXVer1.Caption := VersStr(ver);
      cbSFXUPX.Enabled := True;
      cbSFXRes.Enabled := True;
    end;
    ver := ExeVers(Path('$(ZM_SFX)\' + DefSFXUExe));
    if ver < MinSFXVers then
    begin
      lblSFXUVer1.Caption := '-bad-';
      cbSFXUUPX.AllowGrayed := True;
      cbSFXURes.AllowGrayed := True;
      cbSFXUUPX.Enabled := false;
      cbSFXURes.Enabled := false;
    end
    else
    begin
      lblSFXUVer1.Caption := VersStr(ver);
      cbSFXUUPX.Enabled := True;
      cbSFXURes.Enabled := True;
    end;
  end;
end;

procedure TForm1.SFXLangList(var arg: TStrings);
var
  I: Integer;
  s: string;
begin
  arg.Clear;
  for I := 0 to lbSFXLangs.Count - 1 do
  begin
    if not (lbSFXLangs.ItemEnabled[i] and lbSFXLangs.Checked[i]) then
      continue;
    s := lbSFXLangs.Items[i];
    arg.Add(s);
  end;
end;

procedure TForm1.TabSheet1Show(Sender: TObject);
begin
  EdChange := True; // refresh
end;

function TForm1.TempFileName(const prefix: String): String;
var
  fn: String;
  pth: String;
begin
  Result := Path('$(Temporary)\ZRM_temp.xxx');
  pth := Path('$(Temporary)');
  SetLength(fn, MAX_PATH + 10);
  if GetTempFileName(PChar(pth), PChar(Prefix), 1, PChar(fn)) <> 0 then
    Result := PChar(fn)
  else
    Show('GetTempFileName error', True);
  if FileExists(Result) then
    DeleteFile(Result);
end;

//procedure TForm1.UpdateList(const fs: String);
//var
//f: String;
//idx: Integer;
//ii: Integer;
//l: String;
//lst: TCheckListBox;
//p: String;
//sr: TSearchRec;
////  lid: Integer;
//
//function InTbl(const x: String): Integer;
//var
//  ii, cp: Integer;
//  ss: String;
//begin
//  Result := -1;
//  for ii := 0 to pred(lst.Count) do
//  begin
//    ss := lst.Items[ii];
//    cp := AnsiPos(':', ss);
//    if cp > 0 then
//      ss := copy(ss, 1, cp - 1);
//    if Uppercase(ss) = x then
//    begin
//      Result := ii;
//      break;
//    end;
//  end;
//end;
//
//begin
//lst := lbLangs;
//for ii := 0 to pred(lst.Count) do
//  lst.ItemEnabled[ii] := False;
//p := ExtractFilePath(fs);
//if FindFirst(fs, faReadOnly + faArchive, sr) = 0 then
//begin
//  repeat
//    if (sr.Attr and faDirectory) <> 0 then
//      continue;
//    if sr.Name[1] = '.' then
//      continue;
//    f := Uppercase(copy(sr.Name, 7, length(sr.Name) - (6 + 4)));
//    if (f = '') or (f = 'US') then  // ignore empty, hidden, default
//      continue;
//    idx := InTbl(f);
//    if idx < 0 then
//    begin
////        l := LangOfRes(p + sr.Name);
//      l := LangOfRC(p + sr.Name);
//      if l <> '' then
//        f := l
//      else
//        f := f + ': ???';
//      idx := AddLBItem(lst, f);
//      lst.Checked[idx] := True;
//    end;
//    if idx >= 0 then
//      lst.ItemEnabled[idx] := True;
//  until FindNext(sr) <> 0;
//  FindClose(sr);
//end;
//end;

function TForm1.WantDlls: Boolean;
begin
  Result := cbDlls.Enabled and cbDlls.Checked;
end;

// TODO: WantLangs
//function TForm1.WantLangs: Boolean;
////var
////  i: Integer;
//begin
////  Result := True;
////  for i := 0 to pred(lbLangs.Count) do
////    if lbLangs.ItemEnabled[i] and lbLangs.Checked[i] then
////      exit;
//Result := False;
//end;

function TForm1.UseUPX: boolean;
begin
  Result := cbUseUPX.Checked and FileExists(Path('$(ZM_UPX)\upx.exe'));
end;

end.

