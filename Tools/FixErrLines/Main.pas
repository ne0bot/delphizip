unit Main;
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
//modified 2012-04-24

interface

uses
  Winapi.Windows, Winapi.Messages, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids,
  Vcl.ComCtrls, Vcl.Menus, Contnrs, scan, Vcl.StdCtrls, Vcl.ImgList;

const
  FIXERRORLINESBUILD: string = '1.9.1.0001';
  FIXERRORLINESDATE: string = '21/04/2012';
  FIXERRORLINESPRIV: Integer = 1910001;

const
  Max_Used = 4;

type
  TErrRefTypes = (erNone, erConst, erUsed);

  TErrorPosition = record
    LineNo: Integer;
    Posn: Integer;
    Typ: TErrRefTypes;
    procedure Clear;
  end;

  TUsedArray = array [0 .. Max_Used - 1] of TErrorPosition;

type
  TErrorEntry = class
  private
    FFuncNo: Integer;
    FIdentifier: string;
    FOld: string;
    FRefs: Integer;
    FUsed: TUsedArray;
    function GetLineNo(Index: Cardinal): Integer;
    function GetPosn(Index: Cardinal): Integer;
    function GetTyp(Index: Cardinal): TErrRefTypes;
    procedure SetLineNo(Index: Cardinal; const Value: Integer);
    procedure SetPosn(Index: Cardinal; const Value: Integer);
    procedure SetTyp(Index: Cardinal; const Value: TErrRefTypes);
  public
    function Add(lno, psn: Integer; t: TErrRefTypes): Integer;
    procedure AfterConstruction; override;
    property FuncNo: Integer read FFuncNo write FFuncNo;
    property Identifier: string read FIdentifier write FIdentifier;
    property LineNo[index: Cardinal]: Integer read GetLineNo write SetLineNo;
    property Old: string read FOld write FOld;
    property Posn[index: Cardinal]: Integer read GetPosn write SetPosn;
    property Refs: Integer read FRefs write FRefs;
    property Typ[index: Cardinal]: TErrRefTypes read GetTyp write SetTyp;
  end;

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
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Fix1Click(Sender: TObject);
    procedure Folder1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Refresh1Click(Sender: TObject);
    procedure UnitsTreeClick(Sender: TObject);
    procedure UnitsTreeCollapsing(Sender: TObject; Node: TTreeNode;
      var AllowCollapse: Boolean);
    procedure UnitsTreeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Verify1Click(Sender: TObject);
  private
    AfterErrorsLine: Integer;
    FErrorsLine: Integer;
    FImpLineNo: Integer;
    FixButton: TButton;
    FUnitLine: Integer;
    FUnitNumber: Integer;
    FUpdateUnit: Integer;
    FVCLDir: string;
    VerifyButton: TButton;
    UnitStrs: TStringList;
    UnitToks: TTokenizer;
    procedure BackupFile(const FileName: string);
    function ErrorMsg(err: Integer): string;
    procedure FindZipMaster;
    function ReadIdentConst(var Ident: string; var Value: Integer;
      Strs: TTokenizer): Boolean;
    procedure SetVCLDir(const Value: string);
  protected
    ConstsList: TStringList;
    ErrorsList: TObjectList;
    IdentList: TStringList;
    LineDelta: Integer;
    procedure AddInfo(ParentNode: TTreeNode; const Msg: string; Typ: TMsgType =
        mtNone);
    procedure AddUnit(const UnitName: string; HasExt: Boolean);
    function AdjustDupErrors: Integer;
    function CalcCodeBase(LineNo: Integer): Integer;
    function CheckHasExt(const n: string): Boolean;
    function ExtractIdent(const ErrName: string): string;
    function FindConstErr: string; overload;
    function FindDefined(const NewIdent: string): Integer;
    function FindUnits(const Spec: string): Integer;
    function FixErrorConsts(UnitNode: TTreeNode): Integer;
    function FixUnit(const FileName: string; ParentNode: TTreeNode; FixIt:
        Boolean): Integer;
    procedure CheckUnits;
    function IdentValue(const Ident: string): Integer;
    function IsErrorIdent(const tok: string): Boolean;
    function IsMsgIdNeeded(const S: string): Boolean;
    function MakeDefinition(Identifier: string; RealLineNo: Integer;
      NeedID: Boolean): string;
    function Prepare(const UnitFile: string; UnitNode: TTreeNode): Integer;
    procedure PrepareTree;
    function ReadErrorConst(var Ident: string; var Value: Integer): Boolean;
    function ReadErrorConsts: Integer;
    function ReadErrorIdents: Integer;
    function ReadIdents(const FName: string; UnitNode: TTreeNode): Integer;
    procedure ShowErrorEntries(UnitNode: TTreeNode);
    property ErrorsLine: Integer read FErrorsLine write FErrorsLine;
    property ImpLineNo: Integer read FImpLineNo write FImpLineNo;
    property UnitNumber: Integer read FUnitNumber write FUnitNumber;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property VCLDir: string read FVCLDir write SetVCLDir;
  published
  end;

var
  Form1: TForm1;

implementation

uses
  SysUtils, Vcl.FileCtrl;

{$R *.dfm}
// const
// __UNIT__ = 1;

const
  ZipMain = 'zipmstr.pas';
  MinBuild = 1910000;
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
  ERROR_PREFIX = '__ERR_';
  UNIT_NAME = '__UNIT__';
  // VCLDir       = 'D:\Code\DelZip\191\VCL\';

const
  BakExt = '.bak';

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

procedure TForm1.AddInfo(ParentNode: TTreeNode; const Msg: string; Typ:
    TMsgType = mtNone);
var
  sub: TTreeNode;
begin
  if ParentNode = nil then
    exit;
  sub := nil;
  if ParentNode.HasChildren then
    sub := ParentNode.getFirstChild;
  if sub = nil then
    sub := UnitsTree.Items.AddChild(ParentNode, Msg)
  else
    sub := UnitsTree.Items.Add(sub, Msg);
  if Typ >= mtUnknown then
  begin
    while sub <> nil do
    begin
      sub.ImageIndex := Ord(Typ);
      sub := sub.Parent;
      if (Typ < mtWarn) {and (sub = UnitsTree.Items[0])} then
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

// ERROR_FILE   = '__ERR_FILE';

function TForm1.AdjustDupErrors: Integer;
var
  dups: Integer;
  i: Integer;
  Ident: string;
  J: Integer;
  NewIdent: string;
begin
  Result := 0;
  for i := 0 to ErrorsList.Count - 2 do
  begin
    Ident := TErrorEntry(ErrorsList[i]).Identifier;
    for J := i + 1 to ErrorsList.Count - 1 do
    begin
      if SameText(Ident, TErrorEntry(ErrorsList[J]).Identifier) then
      begin
        Inc(Result);
        dups := 0;
        repeat
          Inc(dups);
          NewIdent := Ident + '_' + IntToStr(dups);
        until FindDefined(NewIdent) < 0;
        TErrorEntry(ErrorsList[J]).Old := Ident;
        TErrorEntry(ErrorsList[J]).Identifier := NewIdent;
      end;
    end;
  end;
end;

procedure TForm1.AfterConstruction;
begin
  inherited;
  UnitStrs := TStringList.Create;
  ConstsList := TStringList.Create;
  ErrorsList := TObjectList.Create;
  IdentList := TStringList.Create;
  UnitToks := TTokenizer.Create(UnitStrs);
end;

procedure TForm1.BackupFile(const FileName: string);
begin
  if FileExists(FileName) then
  begin
    if FileExists(FileName + BakExt) then
    begin
      SysUtils.DeleteFile(FileName + BakExt);
    end;

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

function TForm1.CalcCodeBase(LineNo: Integer): Integer;
begin
  Result := ((UnitNumber and $3F) shl 23) + (((LineNo + 1) and $1FFF) shl 10);
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

function TForm1.ExtractIdent(const ErrName: string): string;
var
  n: Integer;
begin
  Result := '';
  if IsErrorIdent(ErrName) then
  begin
    Result := Copy(ErrName, Length(ERROR_PREFIX) + 1, 100);
    n := Length(Result);
    if CharInSet(Result[n], ['0' .. '9']) then
    begin
      while (n > 3) and CharInSet(Result[n],
        ['0' .. '9' { , '_', '$', 'a'..'f', 'A'..'F' } ]) do
        Dec(n);
      if n > 3 then
        Result := Copy(Result, 1, n);
    end;
  end;
end;

function TForm1.FindConstErr: string;
begin
  repeat
    repeat
      Result := UnitToks.Token(False);
    until (Result = '') or (CompareText(Result, 'const') = 0);
    if Result = '' then
      break;
    if UnitToks.Posn > 10 then
      Continue;
    Result := UnitToks.Token(False);
  until (Result = '') or (IsErrorIdent(Result));
end;

function TForm1.FindDefined(const NewIdent: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to ConstsList.Count - 1 do
    if SameText(NewIdent, ConstsList.Names[i]) then
    begin
      Result := i;
      break;
    end;
end;

function TForm1.FindUnits(const Spec: string): Integer;
var
  fn: string;
  n: string;
  path: string;
  r: string;
  sr: TSearchRec;
begin
  Result := 0;
  PrepareTree; // clear it
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
        if (sr.Name <> '.') and (sr.Name <> '..') and
          (AnsiCompareFilename(sr.Name, '.svn') <> 0) and
          (AnsiCompareFilename(sr.Name, '__history') <> 0) then
        begin
          n := path + sr.Name;
          AddUnit(n, CheckHasExt(n));
          Inc(Result);
        end;
      end;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
  Application.ProcessMessages;
  UnitsTree.FullExpand;
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

// returns the number of bad consts
function TForm1.FixErrorConsts(UnitNode: TTreeNode): Integer;
var
  CRef: Integer;
  Def: string;
  entry: TErrorEntry;
  i: Integer;
  J: Integer;
  S: string;
  SubNode: TTreeNode;
  t: string;
  URef: Integer;
  WantMsgId: Boolean;
begin
  AddInfo(UnitNode, 'Verifying Error Constants');
  SubNode := UnitNode.GetLastChild;
  Result := 0;
  for i := 0 to ErrorsList.Count - 1 do
  begin
    entry := TErrorEntry(ErrorsList[i]);
    URef := -1;
    CRef := -1;
    for J := 0 to entry.Refs - 1 do
    begin
      case entry.Typ[J] of
        // erNone: T := '? ';
        erConst:
          begin
            if CRef <> -1 then
              AddInfo(SubNode, entry.Identifier + ' error: duplicate consts', mtError)
            else
              CRef := J;
          end;
        erUsed:
          begin
            if URef <> -1 then
            begin
              S := entry.Identifier + ' warning: multiple uses';
              S := S + '   lines ' + IntToStr(entry.LineNo[URef] + 1) + ' and '
                + IntToStr(entry.LineNo[J] + 1);
              AddInfo(SubNode, S);
            end
            else
              URef := J;
          end;
      end;
    end;
    if CRef < 0 then
      AddInfo(SubNode, entry.Identifier + ' error: not defined', mtError);
    if URef < 0 then
//      AddInfo(SubNode, entry.Identifier + ' defined line: ' +
//        IntToStr(entry.LineNo[CRef] + 1) + ' - not used', mtWarn);
      AddInfo(SubNode, IntToStr(entry.LineNo[CRef] + 1) + ': ' +
        entry.Identifier + ' - not used', mtWarn);
    if (CRef < 0) or (URef < 0) then
      Continue;

    S := UnitStrs[entry.LineNo[CRef]];
    WantMsgId := IsMsgIdNeeded(S);
    Def := MakeDefinition(entry.Identifier, entry.LineNo[URef] + 1, WantMsgId);
    t := Copy(S, 1, entry.Posn[CRef] - 1) + Def;
    if CompareText(S, t) <> 0 then
    begin
      UnitStrs[entry.LineNo[CRef]] := t;
      AddInfo(SubNode, IntToStr(entry.LineNo[CRef] + 1) + ': ' + t, mtWarn);
      Inc(Result);
    end;
  end;
end;

function TForm1.FixUnit(const FileName: string; ParentNode: TTreeNode; FixIt:
    Boolean): Integer;
var
  m: string;
  S: string;
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
//    AddInfo(ParentNode, ' Fixing: ' + FileName);
    Result := Prepare(FileName, ParentNode);
    if Result < 0 then
    begin
      AddInfo(ParentNode, ErrorMsg(Result));
      exit;
    end;
    ErrorsLine := UnitToks.LineNo;
    Result := ReadErrorIdents;
    AddInfo(ParentNode, 'found error idents = ' + IntToStr(Result));
    Result := FixErrorConsts(ParentNode);
    if Result < 0 then
    begin
      AddInfo(ParentNode, ErrorMsg(Result), mtCritical);
    end;
    if FixIt then
      s := 'fixed '
    else
      s := 'Need to fix ';
    if Result > 0 then
      AddInfo(ParentNode, {'  fixed '}s + IntToStr(Result) + ' error consts');
//    else
//      AddInfo(ParentNode, '  none to fix');

    if (FUpdateUnit > 5) and (FUnitLine > 5) then
    begin
      S := UnitStrs[FUnitLine];
      m := Copy(S, 1, FUpdateUnit - 1);
      UnitStrs[FUnitLine] := m + ' shl 23;';
      AddInfo(ParentNode, ' fixed: ' + UnitStrs[FUnitLine]);
      Inc(Result);
    end;

    if (Result > 0) and FixIt then
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
  FindZipMaster;
end;

function TForm1.IdentValue(const Ident: string): Integer;
var
  S: string;
begin
  S := IdentList.Values[Ident];
  Result := StrToIntDef(S, 0);
end;

function TForm1.IsErrorIdent(const tok: string): Boolean;
begin
  Result := (Length(tok) > Length(ERROR_PREFIX)) and
    (Copy(tok, 1, Length(ERROR_PREFIX)) = ERROR_PREFIX);
end;

function TForm1.IsMsgIdNeeded(const S: string): Boolean;
var
  EOS: Integer;
  LastClose: Integer;
  LastPlus: Integer;
begin
  EOS := Pos(';', S);
  LastPlus := LastPos(S, '+', EOS);
  LastClose := LastPos(S, ')', EOS);
  Result := (LastClose < LastPlus) or (LastClose < 1);
end;

function TForm1.MakeDefinition(Identifier: string; RealLineNo: Integer;
  NeedID: Boolean): string;
begin
  Result := '= __UNIT__ + (' + IntToStr(RealLineNo) + ' shl 10)';
  if NeedID then
    Result := Result + ' + ' + ExtractIdent(Identifier);
  Result := Result + ';';
end;

function TForm1.Prepare(const UnitFile: string; UnitNode: TTreeNode): Integer;
var
  ch: Char;
  CLineNo: Integer;
  n: Integer;
  S: string;
begin
  UnitStrs.Clear;
  ConstsList.Clear;
  ErrorsList.Clear;
  UnitToks.LineNo := -1;
  UnitToks.Posn := -1;
  ImpLineNo := -1;
  ErrorsLine := -1;
  AfterErrorsLine := -1;
  UnitNumber := 0;
  UnitToks.TK := #0;
  FUpdateUnit := -1;
  FUnitLine := -1;
  Result := -9;
  if not FileExists(UnitFile) then
    exit;
  // Result := -8;
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
          // requires '=n;'
          if (UnitToks.NextChar = '=') then
          begin
            UnitToks.Posn := UnitToks.Posn + 1;
            if CharInSet(UnitToks.NextChar, ['1' .. '9']) then
            begin
              n := UnitToks.Number(ch);
              if ch = ';' then
              begin
                AddInfo(UnitNode, 'Needs updating : ' + UnitToks.CurrentLine);
                UnitNumber := n;
                FUpdateUnit := UnitToks.Posn;
                FUnitLine := UnitToks.LineNo;
                Result := 0;
              end
              else
              begin
                S := UnitToks.Alpha(ch);
                if CompareText(S, 'shl') = 0 then
                begin
                  // Memo1.Lines.Add('Needs updating : ' + UnitToks.CurrentLine);
                  UnitNumber := n;
                  Result := 0;
                end;
              end;
            end;
          end;
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

procedure TForm1.PrepareTree;
begin
  // TODO -cMM: TForm1.PrepareTree default body inserted
end;

// TODO: PrepareGrid
// procedure TForm1.PrepareGrid;
// begin
// Rows := 1;
// UnitsGrid.ColWidths[0] := 18;
// UnitsGrid.ColWidths[1] := Width div 2;
// UnitsGrid.ColWidths[2] := 128;
// UnitsGrid.ColWidths[3] := 128;
/// /  UnitsGrid.RowCount := 3;
// UnitsGrid.Cells[0, 0] := '';
// UnitsGrid.Cells[1, 0] := 'Unit';
// UnitsGrid.Cells[2, 0] := '?';
// UnitsGrid.Cells[3, 0] := 'something';
// end;

function TForm1.ReadErrorConst(var Ident: string; var Value: Integer): Boolean;
var
  ch: Char;
  n: Integer;
begin
  Result := False;
  Ident := UnitToks.Token;
  if Length(Ident) <= Length(ERROR_PREFIX) then
    exit;
  if Copy(Ident, 1, Length(ERROR_PREFIX)) <> ERROR_PREFIX then
    exit;
  // requires '=n;'
  if (UnitToks.NextChar = '=') then
  begin
    UnitToks.Posn := UnitToks.Posn + 1;
    if CharInSet(UnitToks.NextChar, ['$', '1' .. '9']) then
    begin
      n := UnitToks.Number(ch);
      if UnitToks.NextChar = ';' then
      begin
        // Memo1.Lines.Add('Found File : ' + IntToStr(N));
        Value := n;
        Result := True;
      end;
    end;
  end;
end;

function TForm1.ReadErrorConsts: Integer;
var
  Ident: string;
  Value: Integer;
begin
  ConstsList.Clear;
  while ReadErrorConst(Ident, Value) do
    ConstsList.Add(Ident + '=' + IntToHex(Value, 8));
  Result := ConstsList.Count;
end;

function TForm1.ReadErrorIdents: Integer;
var
  entry: TErrorEntry;
  ErrFuncNo: Integer;
  First: Integer;
  i: Integer;
  psn: Integer;
  Refs: Integer;
  ThisLine: string;
  tok: string;
begin
  ErrorsList.Clear;
  ErrFuncNo := -1;
  tok := FindConstErr;
  while tok <> '' do
  begin
    Refs := 0;
    First := ErrorsList.Count;
    // is const __ERR_
    Inc(ErrFuncNo);
    repeat
      ThisLine := UnitToks.CurrentLine;
      psn := UnitToks.Posn;
      if Skip(psn, ThisLine) = '=' then
      begin
        Inc(Refs);
        // Memo1.Lines.Add('Defined: ' + IntToStr(UnitToks.LineNo) + ' >>' + ThisLine);
        entry := TErrorEntry.Create;
        entry.Identifier := tok;
        entry.FuncNo := ErrFuncNo;
        entry.Add(UnitToks.LineNo, psn, erConst);
        ErrorsList.Add(entry);
        UnitToks.Posn := Length(ThisLine) + 1;
        tok := UnitToks.Token;
      end;
    until (not IsErrorIdent(tok));
    // find references for this block

    tok := UnitToks.Token;
    while tok <> '' do
    begin
      if CompareText(tok, 'const') = 0 then
      begin
        tok := UnitToks.Token;
        if not IsErrorIdent(tok) then
        begin
          tok := FindConstErr;
        end;
        break;
      end;
      if IsErrorIdent(tok) then
      begin
        ThisLine := UnitToks.CurrentLine;
        // locate its entry
        for i := 0 to Refs - 1 do
        begin
          if CompareText(tok, TErrorEntry(ErrorsList[First + i]).Identifier) = 0
          then
          begin
            // found
            entry := TErrorEntry(ErrorsList[First + i]);
            if entry.Refs >= Max_Used then
            begin
              raise Exception.Create('Too many references ' +
                IntToStr(UnitToks.LineNo) + ' [' +
                IntToStr(entry.Refs + 1) + ']');
            end;
            entry.Add(UnitToks.LineNo, UnitToks.Posn, erUsed);
            break;
          end;
        end;
      end;
      tok := UnitToks.Token;
    end;
  end;
  Result := ErrorsList.Count;
end;

function TForm1.ReadIdentConst(var Ident: string; var Value: Integer;
  Strs: TTokenizer): Boolean;
var
  n: Integer;
  psn: Integer;
  S: string;
begin
  Result := False;
  Ident := Strs.Token(False);
  if Length(Ident) <= 4 then
    exit;
  if Ident[3] <> '_' then
    exit;
  // requires '=n;'
  if (Strs.NextChar = '=') then
  begin
    Strs.Posn := Strs.Posn + 1;
    if CharInSet(Strs.NextChar, ['$', '0' .. '9']) then
    begin
      S := Strs.CurrentLine;
      psn := Strs.Posn;
      n := Number(psn, S);
      Strs.Posn := psn;
      if Strs.NextChar = ';' then
      begin
        // Memo1.Lines.Add('Found File : ' + IntToStr(N));
        Value := n;
        Result := True;
      end;
    end;
  end;
end;

function TForm1.ReadIdents(const FName: string; UnitNode: TTreeNode): Integer;
var
  Ident: string;
  IdentStrs: TTokenizer;
  Value: Integer;
begin
  IdentList.Clear;
  Result := -19;
  if not FileExists(FName) then
    exit;
  IdentStrs := TTokenizer.Create(nil);
  try
    IdentStrs.LineNo := -1;
    IdentStrs.Posn := -1;
    IdentStrs.LoadFromFile(FName);
    if IdentStrs.Count > 0 then
    begin
      IdentStrs.LineNo := 0;
      IdentStrs.Posn := 1;
      IdentStrs.TK := #0;
    end;
    Result := -18;
    if IdentStrs.FindIdent('interface') then
    begin
      // Memo1.Lines.Add('Found: ');
      Result := -17;
      if IdentStrs.FindIdent('const') then
      begin
        while ReadIdentConst(Ident, Value, IdentStrs) do
        begin
          IdentList.Add(Ident + '=' + IntToStr(Value));
          AddInfo(UnitNode, 'Found: ' + Ident + ' = ' + IntToStr(Value));
        end;
        Result := IdentList.Count;
      end;
    end;
  finally
    IdentStrs.Free;
  end;
end;

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

procedure TForm1.ShowErrorEntries(UnitNode: TTreeNode);
var
  entry: TErrorEntry;
  i: Integer;
  J: Integer;
  S: string;
  t: string;
begin
  for i := 0 to ErrorsList.Count - 1 do
  begin
    entry := TErrorEntry(ErrorsList[i]);
    S := IntToStr(entry.FuncNo) + ' ';
    S := S + entry.Identifier + ' [' + entry.Old + ']  ';
    AddInfo(UnitNode, S);
    for J := 0 to entry.Refs - 1 do
    begin
      S := '  ';
      case entry.Typ[J] of
        erNone:
          t := '? ';
        erConst:
          t := 'C ';
        erUsed:
          t := 'U ';
      end;
      S := S + t + IntToStr(entry.LineNo[J]) + ', ' + IntToStr(entry.Posn[J]);
      t := UnitStrs[entry.LineNo[J]];
      S := S + ' |' + Copy(t, 1, entry.Posn[J] - 1) + '|' +
        Copy(t, entry.Posn[J], 255) + '|';
      AddInfo(UnitNode, S);
    end;
  end;
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

{ TErrorPosition }

procedure TErrorPosition.Clear;
begin
  LineNo := 0;
  Posn := 0;
  Typ := erNone;
end;

function TErrorEntry.Add(lno, psn: Integer; t: TErrRefTypes): Integer;
begin
  if Refs >= Max_Used then
    raise Exception.Create('Used index out of range');
  Result := Refs;
  FUsed[Result].LineNo := lno;
  FUsed[Result].Posn := psn;
  FUsed[Result].Typ := t;
  Refs := Result + 1;
end;

procedure TErrorEntry.AfterConstruction;
var
  i: Integer;
begin
  inherited;
  for i := 0 to Max_Used - 1 do
    FUsed[i].Clear;
end;

function TErrorEntry.GetLineNo(Index: Cardinal): Integer;
begin
  if index >= Max_Used then
    raise Exception.Create('Used index out of range');
  Result := FUsed[index].LineNo;
end;

function TErrorEntry.GetPosn(Index: Cardinal): Integer;
begin
  if index >= Max_Used then
    raise Exception.Create('Used index out of range');
  Result := FUsed[index].Posn;
end;

function TErrorEntry.GetTyp(Index: Cardinal): TErrRefTypes;
begin
  if index >= Max_Used then
    raise Exception.Create('Used index out of range');
  Result := FUsed[index].Typ;
end;

procedure TErrorEntry.SetLineNo(Index: Cardinal; const Value: Integer);
begin
  if index >= Max_Used then
    raise Exception.Create('Used index out of range');
  FUsed[index].LineNo := Value;
end;

procedure TErrorEntry.SetPosn(Index: Cardinal; const Value: Integer);
begin
  if index >= Max_Used then
    raise Exception.Create('Used index out of range');
  FUsed[index].Posn := Value;
end;

procedure TErrorEntry.SetTyp(Index: Cardinal; const Value: TErrRefTypes);
begin
  if index >= Max_Used then
    raise Exception.Create('Used index out of range');
  FUsed[index].Typ := Value;
end;

end.
