unit Main;
(* ***************************************************************************
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.
 Copyright (C) 2013, 2014  by Russell J. Peters, Roger Aelbrecht.

   This file is part of TZipMaster Version 1.9.2

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
//modified 2014-01-03

interface

uses
  Windows, Classes, Messages, Variants,
  Graphics, Controls, Forms, Dialogs, Grids,
  ComCtrls, Menus, Contnrs, scan, StdCtrls, ImgList;

const
  FIXERRORLINESBUILD: string = '1.9.2.0002';
  FIXERRORLINESDATE: string = '03/01/2014';
  FIXERRORLINESPRIV: Integer = 1920002;

const
  Max_Used = 4;

type
  TRefTypes = (rtNone, rtInvalid, rtGood, rtBad, rtUnit);
  TRefPosition = record
    LineNo: Integer;
    Posn: Integer;
    Current: Integer;
    Typ: TRefTypes;
    Node: TTreeNode;
//    procedure Clear;
  end;
  TRefArray = array of TRefPosition;

type
  TMsgType = (mtNone=0, mtUnknown=3, mtGood, mtWarn, mtError, mtCritical);

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    StatusBar1: TStatusBar;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Fix1: TMenuItem;
    UnitsTree: TTreeView;
    ImageList1: TImageList;
    Verify1: TMenuItem;
    Folder1: TMenuItem;
    Refresh1: TMenuItem;
    UnitsList1: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Fix1Click(Sender: TObject);
    procedure Folder1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure UnitsList1Click(Sender: TObject);
    procedure UnitsTreeClick(Sender: TObject);
    procedure UnitsTreeCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure UnitsTreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Verify1Click(Sender: TObject);
  private
    AfterErrorsLine: Integer;
    ConstsList: TStringList;
    ErrorsList: TObjectList;
    FImpLineNo: Integer;
    FixButton: TButton;
    FUnitLine: Integer;
    FUnitNumber: Integer;
    FUpdateUnit: Integer;
    FVCLDir: string;
    IdentList: TStringList;
    Starting: Boolean;
    VerifyButton: TButton;
    UnitStrs: TStringList;
    UnitToks: TTokenizer;
    procedure BackupFile(const FileName: string);
    function CheckHasExt(const n: string): Boolean;
    procedure CheckUnits;
    function ErrorMsg(err: Integer): string;
    function FindMarker: string;
    function FindUnits(const Spec: string): Integer;
    procedure FindZipMaster;
    function Prepare(const UnitFile: string; UnitNode: TTreeNode): Integer;
    procedure SetVCLDir(const Value: string);
    function VerifyUnitNumber: Integer;
    procedure WriteUnitsInc;
    property ImpLineNo: Integer read FImpLineNo write FImpLineNo;
  protected
    UnitList: TStringList;
    function AddInfo(ParentNode: TTreeNode; const Msg: string; Typ: TMsgType =
        mtNone): TTreeNode;
    procedure AddUnit(const UnitName: string; HasExt: Boolean);
    function FixUnit(const FileName: string; ParentNode: TTreeNode; FixIt:
        Boolean): Integer;
    property UnitNumber: Integer read FUnitNumber write FUnitNumber;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function FixLineRefs(ParentNode: TTreeNode): Integer;
    function FixUnitNumber(const Filename: string; ParentNode: TTreeNode): Integer;
    property VCLDir: string read FVCLDir write SetVCLDir;
  published
  end;

var
  Form1: TForm1;

implementation

uses
  SysUtils, FileCtrl, ZMUtils;

{$R *.dfm}
// const
// __UNIT__ = 1;

const
  ZipMain = 'zipmstr.pas';
  MinBuild = 1920000;
  CheckMain = 'ZIPMASTERPRIV: Integer = ';
  CheckUnit = '__UNIT__ = ';

const
  // ImageList.StateIndex=0 has some bugs, so we add one dummy image to position 0
  cFlatUnCheck = 1;
  cFlatChecked = 2;

  (*
    bits 31..0
    0uFF FFFF  FLLL LLLL  LLLL LLuE  EEEE EEEE
    E _ error
    L _ line number
    F _ file number
    u _ unassigned
  *)
const
//  ERROR_PREFIX = '__ERR_';
  UNIT_NAME = '__UNIT__';
//  FUNC_NAME = 'ZM_Error';

const
  BakExt = '.bak';
  MTyp : array[TRefTypes] of TMsgType = (mtNone, mtError, mtGood, mtWarn, mtWarn);
//  TRefTypes = (rtNone, rtInvalid, rtGood, rtBad);
         
{$ifndef UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := c in CharSet;
end;
{$endif}

function LastPos(const S: string; ch: Char; before: Integer = MAXINT): Integer;
var
  i: Integer;
begin
  Result := 0; // not found
  for i := 1 to Length(S) do
  begin
    if i >= before then
      break;
    if S[i] = ch then
      Result := i;
  end;
end;

procedure ToggleTreeViewCheckBoxes(Node: TTreeNode;
  cUnChecked, cChecked: Integer);
begin
  if Assigned(Node) then
  begin
    if Node.StateIndex = cUnChecked then
      Node.StateIndex := cChecked
    else
      if Node.StateIndex = cChecked then
        Node.StateIndex := cUnChecked;
  end; // if Assigned(Node)
end; (* ToggleTreeViewCheckBoxes *)

procedure TForm1.FormCreate(Sender: TObject);
var
  Lft: Integer;
begin
  Lft := StatusBar1.Panels.Items[0].Width + StatusBar1.Panels.Items[1].Width;
  VerifyButton := TButton.Create(Self);
  VerifyButton.Parent := StatusBar1;
  VerifyButton.SetBounds(Lft, 0, StatusBar1.Panels.Items[2].Width, StatusBar1.ClientHeight);
  VerifyButton.Caption := 'Verify';
  VerifyButton.OnClick := Verify1Click;
  Lft := Lft + StatusBar1.Panels.Items[2].Width + StatusBar1.Panels.Items[3].Width;
  FixButton := TButton.Create(Self);
  FixButton.Parent := StatusBar1;
  FixButton.SetBounds(Lft, 0, StatusBar1.Panels.Items[4].Width, StatusBar1.ClientHeight);
  FixButton.Caption := 'Fix';
  FixButton.OnClick := Fix1Click;
end;

function TForm1.AddInfo(ParentNode: TTreeNode; const Msg: string; Typ: TMsgType
    = mtNone): TTreeNode;
begin
  Result := nil;
  if ParentNode = nil then
    exit;
  Result := nil;
  if ParentNode.HasChildren then
    Result := ParentNode.getFirstChild;
  if Result = nil then
    Result := UnitsTree.Items.AddChild(ParentNode, Msg)
  else
    Result := UnitsTree.Items.Add(Result, Msg);
  if Typ >= mtUnknown then
  begin
    while Result <> nil do
    begin
      Result.ImageIndex := Ord(Typ);
      Result := Result.Parent;
      if (Typ < mtWarn) then
        break;
    end;
  end;
end;

procedure TForm1.AddUnit(const UnitName: string; HasExt: Boolean);
const
  checks: array [Boolean] of Integer = (cFlatUnCheck, cFlatChecked);
var
  Node: TTreeNode;
begin
  if UnitsTree.Items.Count < 1 then
  begin
    Node := UnitsTree.Items.Add(nil, 'Units');
    Node.StateIndex := -1;
    Node.ImageIndex := 0;
  end
  else
    Node := UnitsTree.Items[0];
  Node := UnitsTree.Items.AddChild(Node, UnitName);
  Node.StateIndex := checks[HasExt];
  Node.ImageIndex := 3;
  if not HasExt then
    Node.ImageIndex := 0;
end;

procedure TForm1.AfterConstruction;
begin
  inherited;
  UnitStrs := TStringList.Create;
  ConstsList := TStringList.Create;
  ErrorsList := TObjectList.Create;
  IdentList := TStringList.Create;
  UnitList := TStringList.Create;
  UnitList.CaseSensitive := False;
  UnitToks := TTokenizer.Create(UnitStrs);
  Starting := True;
end;

procedure TForm1.BackupFile(const FileName: string);
begin
  if FileExists(FileName) then
  begin
    if FileExists(FileName + BakExt) then
      SysUtils.DeleteFile(FileName + BakExt);
    SysUtils.RenameFile(FileName, FileName + BakExt);
  end;
end;

procedure TForm1.BeforeDestruction;
begin
  UnitToks.Free;
  UnitStrs.Free;
  ConstsList.Free;
  ErrorsList.Free;
  IdentList.Free;
  inherited;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  //
end;

function TForm1.CheckHasExt(const n: string): Boolean;
const
  Required: array [0 .. 2] of string = ('interface', 'implementation', 'const');
var
  lno: Integer;
  Req: Integer;
  S: string;
  UnitText: TStringList;
begin
  Result := False;
  UnitText := TStringList.Create;
  try
    UnitText.LoadFromFile(n);
    lno := 0;
    Req := 0;
    while lno < UnitText.Count do
    begin
      S := Trim(UnitText[lno]);
      Inc(lno);
      if CompareText(S, Required[Req]) = 0 then
      begin
        Inc(Req);
        if Req = 3 then
          break;
      end;
    end;
    if Req = 3 then
    begin
      S := Copy(Trim(UnitText[lno]), 1, Length(CheckUnit));
      Result := CompareStr(S, CheckUnit) = 0;
    end;
  finally
    UnitText.Free;
  end;
end;

function TForm1.ErrorMsg(err: Integer): string;
begin
  Result := '';
  if err < 0 then
  begin
    case err of
      - 6:
        Result := 'Could not find, or invalid, __UNIT__';
      -7:
        Result := 'Could not find implementation';
      -8:
        Result := 'error loading unit';
      -9:
        Result := 'file not found';
      -17:
        Result := 'Could not find ZMMsg interface';
      -18:
        Result := 'error loading ZMMsg';
      -19:
        Result := 'ZMMsg not found';
    else
      Result := 'unknown error: ' + IntToStr(err);
    end;
  end;
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

function TForm1.FindUnits(const Spec: string): Integer;
var
  fn: string;
  n: string;
  path: string;
  r: string;
  sr: TSearchRec;
//  I: Integer;
begin
  Result := 0;
//  PrepareTree; // clear it
  Application.ProcessMessages;
  if Spec = '' then
    exit;
  fn := ExtractFileName(Spec);
  if (fn <> '') and (ExtractFileExt(Spec) <> '') then
    r := Spec
  else
  begin
    r := IncludeTrailingPathDelimiter(Spec) + '*.pas';
  end;
  path := IncludeTrailingPathDelimiter(ExtractFilePath(r));
  if (FindFirst(r, faAnyFile, sr) = 0) then
  begin
    repeat
      if (sr.Attr and faDirectory) = 0 then
      begin
        if (sr.Name <> '') and (sr.Name[1] <> '.') and (Pos('-', sr.Name) < 1) then
//        if (sr.Name <> '.') and (sr.Name <> '..') and
//          (AnsiCompareFilename(sr.Name, '.svn') <> 0) and
//          (AnsiCompareFilename(sr.Name, '.git') <> 0) and
//          (AnsiCompareFilename(sr.Name, '__history') <> 0) then
        begin
          n := path + sr.Name;
          AddUnit(n, CheckHasExt(n));
          Inc(Result);
          UnitList.Add(sr.Name);
        end;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  Application.ProcessMessages;
  UnitsTree.FullExpand;
  UnitList.Sort;//ed := True;
//  for I := 0 to UnitList.Count - 1 do
//    UnitList[I] := UnitList[I] + '=' + IntToStr(I + 1);
//  CheckUnits;
end;

procedure TForm1.FindZipMaster;
var
  aDir: string;
  path: string;
begin
  aDir := ExtractFilePath(Application.ExeName);
  FVCLDir := '';
  if FileExists(aDir + ZipMain) then
    FVCLDir := aDir
  else
  begin
    path := ExpandFileName(aDir + '..\..\' + ZipMain);
    if FileExists(path) then
      FVCLDir := ExtractFilePath(path)
    else
    begin
      path := ExpandFileName(aDir + '..\..\..\' + ZipMain);
      if FileExists(path) then
        FVCLDir := ExtractFilePath(path)
      else
      begin
        path := ExpandFileName(aDir + '..\..\..\..\' + ZipMain);
        if FileExists(path) then
          FVCLDir := ExtractFilePath(path);
      end;
    end;
  end;
  StatusBar1.Panels[0].Text := FVCLDir;
  if VCLDir <> '' then
//    FindUnits(FVCLDir + 'ZMZipReader.pas');
    FindUnits(VCLDir);
end;

procedure TForm1.Fix1Click(Sender: TObject);
var
  err: Integer;
  fn: string;
  Node: TTreeNode;
  thisNode: TTreeNode;
begin
  if UnitsTree.Items.Count < 2 then
    exit;
  Node := UnitsTree.Items[0];
  if Node = nil then
    exit;
  Node := Node.getFirstChild;
  while Node <> nil do
  begin
    thisNode := Node;
    Node := Node.getNextSibling;
    if thisNode.StateIndex <> cFlatChecked then
      Continue;
    fn := thisNode.Text;
    err := FixUnit(fn, thisNode, True);
    if (err >= 0) and (thisNode.ImageIndex < Ord(mtWarn)) then
    begin
      thisNode.ImageIndex := Ord(mtGood);
      thisNode.StateIndex := cFlatUnCheck;
    end;
  end;
end;

function TForm1.FixUnit(const FileName: string; ParentNode: TTreeNode; FixIt:
    Boolean): Integer;
var
  FixedUnit: Integer;
  SubNode: TTreeNode;
begin
  try
    // remove old children
    while ParentNode.HasChildren do
    begin
      SubNode := ParentNode.getFirstChild;
      SubNode.Delete;
    end;
    ParentNode.ImageIndex := Ord(mtUnknown);
    Result := Prepare(FileName, ParentNode);
    if Result < 0 then
    begin
      AddInfo(ParentNode, ErrorMsg(Result));
      exit;
    end;
    Result := FixUnitNumber(Filename, ParentNode);
    if Result < 0 then
    begin
      AddInfo(ParentNode, ErrorMsg(Result));
      exit;
    end;
    FixedUnit := Result;
    Result := FixLineRefs(ParentNode);
    if Result < 0 then
    begin
      AddInfo(ParentNode, ErrorMsg(Result));
      exit;
    end;
    Result := Result + FixedUnit;
    if Fixit and (Result > 0) then
    begin
      BackupFile(FileName);
      UnitStrs.SaveToFile(FileName);
    end;
  except
    on E: Exception do
    begin
      AddInfo(ParentNode, ' Exception: ' + E.Message, mtCritical);
      Result := -1;
    end;
  end;
end;

procedure TForm1.CheckUnits;
var
  err: Integer;
  fn: string;
  Node: TTreeNode;
  thisNode: TTreeNode;
begin
  if UnitsTree.Items.Count < 2 then
    exit;
  Node := UnitsTree.Items[0];
  if Node = nil then
    exit;
  Node := Node.getFirstChild;
  if (Node <> nil) and (Node.ImageIndex > Ord(mtGood)) then
    Node.ImageIndex := Ord(mtGood);
  while Node <> nil do
  begin
    thisNode := Node;
    Node := Node.getNextSibling;
    if thisNode.StateIndex <> cFlatChecked then
      Continue;
    fn := thisNode.Text;
    err := FixUnit(fn, thisNode, False);
    if thisNode.ImageIndex < Ord(mtWarn) then
    begin
      if (err = 0) then
      begin
        thisNode.ImageIndex := Ord(mtGood);
        thisNode.StateIndex := cFlatUnCheck;
      end;
      if (err > 0) then
        thisNode.ImageIndex := Ord(mtWarn);
    end;
  end;
end;

function TForm1.FindMarker: string;
var
  Ch: Char;
begin
  Result := '';
  Ch := #0;
  repeat
    repeat
      Result := UnitToks.Token(True);
    until (Result = '') or (CompareStr(Result, '{_LINE_}') = 0);
    if Result = '' then
      break;
    Ch := UnitToks.NextChar(False);
  until CharInSet(Ch, ['1'..'9']);
end;

function TForm1.FixLineRefs(ParentNode: TTreeNode): Integer;
var
  ch: Char;
  CLine: string;
  CLineNo: Integer;
  CurValue: Integer;
  NLine: string;
  Posn: Integer;
  ThisLine: Integer;
  ThisPosn: Integer;
  tok: string;
begin
  Result := 0;
  tok := FindMarker;
  while tok <> '' do
  begin
    ThisLine := UnitToks.LineNo;
    ch := UnitToks.NextChar(False);
    if CharInSet(ch, ['1' .. '9']) then
    begin
      ThisPosn := UnitToks.Posn;
      // get current 'value'
      CurValue := UnitToks.Number(ch);
      if (ch <> ',') and (ch <> ')')then
      begin
        // is invalid
        AddInfo(ParentNode, 'Invalid reference: line ' + IntToStr(ThisLine + 1), mtError);
      end
      else
      if CurValue <> (ThisLine + 1) then
      begin
        // is bad
        AddInfo(ParentNode, 'Reference was ' + IntToStr(CurValue) +
           ' update to ' + IntToStr(ThisLine + 1), mtWarn);
        CLineNo := ThisLine;
        CLine := UnitStrs[CLineNo];
        NLine := Copy(CLine, 1, ThisPosn -1);
        NLine := NLine + IntToStr(CLineNo + 1);
        Posn := ThisPosn;
        while (Posn < Length(CLine)) and CharInSet(CLine[Posn], ['0'..'9']) do
           Inc(Posn);
        NLine := NLine + Copy(CLine, Posn, 4095);
        UnitStrs[CLineNo] := NLine;
        Inc(Result);
      end;
    end;
    tok := FindMarker;
  end;
end;

function TForm1.FixUnitNumber(const Filename: string; ParentNode: TTreeNode):
    Integer;
var
  ch: Char;
  n: Integer;
  Posn: Integer;
  S: string;
  uname: string;
//  UnitNo: Integer;
begin
  uname := ExtractFileName(FileName);
  UnitNumber := UnitList.IndexOf(uname) + 1;
  Result := -6;
  Posn := UnitToks.Posn;
  n := UnitToks.Number(ch);
  if ch <> ';' then
    Exit;
  if n = UnitNumber then
  begin
    AddInfo(ParentNode, 'Unit number : ' + IntToStr(n));
    Result := 0;
    Exit;
  end;
  Result := 1;  // fixed something
  AddInfo(ParentNode, 'Unit number : ' + IntToStr(n) + 'needs updating to ' +
    IntToStr(UnitNumber), mtWarn);
  S := Copy(UnitToks.CurrentLine, 1, posn -1);
  S := S + IntToStr(UnitNumber) + ';';
  UnitStrs[UnitToks.LineNo] := S;

//  if n > 0 then
//  if (ch = ';') and (UnitNo = n) then
//  begin
//    UnitNumber := n;
//    Result := 0;
//  end
//  else
//  begin
//    AddInfo(ParentNode, 'Needs updating : ' + UnitToks.CurrentLine, mtWarn);
//    UnitNumber := UnitNo;//n;
//    FUpdateUnit := UnitToks.Posn;
//    FUnitLine := UnitToks.LineNo;
////        Result := 0;
//  end;
end;

procedure TForm1.Folder1Click(Sender: TObject);
var
  Dir: String;
begin
  SelectDirectory('Select a directory', '', Dir);
  VCLDir := Dir;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  UnitsTree.Invalidate;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if Starting then
  begin
    Starting := False;
    FindZipMaster;
    Verify1Click(Self);
  end;
end;

function TForm1.Prepare(const UnitFile: string; UnitNode: TTreeNode): Integer;
var
  CLineNo: Integer;
//  n: Integer;
//  uname: string;
begin
  UnitStrs.Clear;
  ConstsList.Clear;
  ErrorsList.Clear;
  UnitToks.LineNo := -1;
  UnitToks.Posn := -1;
  ImpLineNo := -1;
//  ErrorsLine := -1;
  AfterErrorsLine := -1;
  UnitNumber := 0;
  UnitToks.TK := #0;
  FUpdateUnit := -1;
  FUnitLine := -1;
  Result := -9;
  if not FileExists(UnitFile) then
    exit;
  try
    UnitStrs.LoadFromFile(UnitFile);
    if UnitStrs.Count > 0 then
    begin
      UnitToks.LineNo := 0;
      UnitToks.Posn := 1;
    end;
    Result := -7;
    if UnitToks.FindIdent('implementation') then
    begin
      ImpLineNo := UnitToks.LineNo;
      Result := -6;
      if UnitToks.FindIdent('const') then
      begin
        CLineNo := UnitToks.LineNo;
        if UnitToks.FindIdent(UNIT_NAME) and (UnitToks.LineNo = (CLineNo + 1))
        then
        begin
//          uname := ExtractFileName(UnitFile);
//          n := UnitList.IndexOf(uname);
          // requires '=n;'
          Result := VerifyUnitNumber;//(UnitNode, n + 1);
        end;
      end;
    end;
  except
    UnitStrs.Clear;
    UnitToks.LineNo := -1;
    UnitToks.Posn := -1;
    UnitToks.TK := #0;
    Result := -8;
  end;
end;

// TODO: ReadLineRefs
//// TODO: PrepareTree
////procedure TForm1.PrepareTree;
////begin
////// TODO -cMM: TForm1.PrepareTree default body inserted
////end;
//
//function TForm1.ReadLineRefs: Integer;
//var
//ch: Char;
//CurValue: Integer;
//ThisLine: Integer;
//ThisPosn: Integer;
//ThisTyp: TRefTypes;
//tok: string;
//begin
//Result := 0;
////  SetLength(FRefs, UnitStrs.Count); // allow 1 per line
////  FRefCount := 0;
//tok := FindMarker;
//while tok <> '' do
//begin
//  ThisLine := UnitToks.LineNo;
//  ch := UnitToks.NextChar(False);
//  if CharInSet(ch, ['1' .. '9']) then
//  begin
//    ThisPosn := UnitToks.Posn;
//    // get current 'value'
//    CurValue := UnitToks.Number(ch);
//    if CurValue = (ThisLine + 1) then
//      ThisTyp := rtGood
//    else
//      ThisTyp := rtBad;
//    if (ch <> ',') and (ch <> ')')then
//      ThisTyp := rtInvalid;
//    if ThisTyp <> rtGood then
//      Inc(Result);
//
//    FRefs[FRefCount].Clear;
//    FRefs[FRefCount].LineNo := ThisLine;
//    FRefs[FRefCount].Posn := ThisPosn;
//    FRefs[FRefCount].Current := CurValue;
//    FRefs[FRefCount].Typ := ThisTyp;
//    Inc(FRefCount);
//  end;
//  tok := FindMarker;//FindFunc;
//end;
//end;

procedure TForm1.Refresh1Click(Sender: TObject);
begin
  FindUnits(VCLDir);
end;

procedure TForm1.SetVCLDir(const Value: string);
begin
  if VCLDir <> Value then
  begin
    FVCLDir := Value;
    FindUnits(Value);
  end;
end;

procedure TForm1.UnitsList1Click(Sender: TObject);
var
  fn: string;
  Node: TTreeNode;
  S: String;
  thisNode: TTreeNode;
  UnitNode: TTreeNode;
  UnitList: TStringList;
  UNumber: string;
  UText: string;
begin
  if UnitsTree.Items.Count < 2 then
    exit;
  Node := UnitsTree.Items[0];
  if Node = nil then
    exit;
  UnitList := TStringList.Create;
  try
    Node := Node.getFirstChild;
    while Node <> nil do
    begin
      thisNode := Node;
      Node := Node.getNextSibling;
      fn := ExtractFileName(thisNode.Text);
      UNumber := '00';
      UnitNode := thisNode.getFirstChild;
      if UnitNode <> nil then
      begin
        UText := UnitNode.Text;
        S := Copy(UText, 1, 14);
        if CompareText(S, 'Unit number : ') = 0 then
        begin
          UNumber := Trim(Copy(UText, 14, 4));
          if Length(UNumber) < 2 then
            UNumber := '0' + UNumber;
        end;
      end;
      UnitList.Add(fn + '=' + UNumber);
    end;
    UnitList.Sort;
    UnitList.SaveToFile(VCLDir + '\Units.txt');
  finally
    UnitList.Free;
  end;
  WriteUnitsInc;
end;

procedure TForm1.UnitsTreeClick(Sender: TObject);
var
  P: TPoint;
begin
  GetCursorPos(P);
  P := UnitsTree.ScreenToClient(P);
  if (htOnStateIcon in UnitsTree.GetHitTestInfoAt(P.X, P.Y)) then
    ToggleTreeViewCheckBoxes(UnitsTree.Selected, cFlatUnCheck, cFlatChecked);
end;

procedure TForm1.UnitsTreeCollapsing(Sender: TObject; Node: TTreeNode;
  var AllowCollapse: Boolean);
begin
  AllowCollapse := False;
end;

procedure TForm1.UnitsTreeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Assigned(UnitsTree.Selected) then
    ToggleTreeViewCheckBoxes(UnitsTree.Selected, cFlatUnCheck, cFlatChecked);
end;

procedure TForm1.Verify1Click(Sender: TObject);
begin
  CheckUnits;
end;

function TForm1.VerifyUnitNumber: Integer;
//var
//  ch: Char;
//  n: Integer;
begin
  Result := -6;
  if (UnitToks.NextChar = '=') then
  begin
    UnitToks.Posn := UnitToks.Posn + 1;
    if CharInSet(UnitToks.NextChar, ['1' .. '9']) then
      Result := 0;
//    if CharInSet(UnitToks.NextChar, ['1' .. '9']) then
//    begin
//      n := UnitToks.Number(ch);
//      if n > 0 then
//        AddInfo(UnitNode, 'Unit number : ' + IntToStr(n));
//      if (ch = ';') and (UnitNo = n) then
//      begin
//        UnitNumber := n;
//        Result := 0;
//      end
//      else
//      begin
//        AddInfo(UnitNode, 'Needs updating : ' + UnitToks.CurrentLine, mtWarn);
//        UnitNumber := UnitNo;//n;
//        FUpdateUnit := UnitToks.Posn;
//        FUnitLine := UnitToks.LineNo;
////        Result := 0;
//      end;
//    end;
  end;
end;

procedure TForm1.WriteUnitsInc;
var
  Cnt: Integer;
  Count: Integer;
  fn: string;
  More: string;
  Node: TTreeNode;
  S: String;
  thisNode: TTreeNode;
  UnitNode: TTreeNode;
  UnitList: TStringList;
//  UNumber: string;
  UText: string;
begin
  if UnitsTree.Items.Count < 2 then
    exit;
  Node := UnitsTree.Items[0];
  if Node = nil then
    exit;
  UnitList := TStringList.Create;
  try
    Count := 0;
    Node := Node.getFirstChild;
    while Node <> nil do
    begin
//      thisNode := Node;
      Inc(Count);
      Node := Node.getNextSibling;
    end;
    UnitList.Add('const');
    UnitList.Add('  ZMUNITMAX = ' + IntToStr(Count)+ ';');
    UnitList.Add('  ZMUnitNames: array [0..ZMUNITMAX-1] of string = (');
    UText := '';
    Cnt := 0;
    Node := UnitsTree.Items[0];
    Node := Node.getFirstChild;
    while Node <> nil do
    begin
      thisNode := Node;
      Node := Node.getNextSibling;
      fn := ExtractFileName(thisNode.Text);
      fn := '''' + Copy(fn, 1, Length(fn) - 4) + '''';
      Inc(Cnt);
      if Cnt < Count then
        fn := fn + ','
      else
        fn := fn + ');';
      if UText = '' then
        UText := '    ' + fn
      else
      begin
        if (Length(UText) + Length(fn) + 1) > 78 then
        begin
          UnitList.Add(UText);
          UText := '   ' + fn;
        end
        else
          UText := UText + ' ' + fn;
      end;
    end;
    UnitList.Add(UText);
    UnitList.SaveToFile(VCLDir + '\Units.inc');
  finally
    UnitList.Free;
  end;
end;

{ TRefPosition }
    {
procedure TRefPosition.Clear;
begin
  LineNo := 0;
  Posn := 0;
  Node := nil;
  Current := 0;
  Typ := rtNone;
end;
 }
end.
