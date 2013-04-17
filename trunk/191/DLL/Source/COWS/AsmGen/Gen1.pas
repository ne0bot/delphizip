unit Gen1;
(************************************************************************
 Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht,
      Eric W. Engler and Chris Vleghert.

   This file is part of TZipMaster Version 1.9.

    TZipMaster is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    TZipMaster is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with TZipMaster.  If not, see <http://www.gnu.org/licenses/>.

    contact: problems@delphizip.org (include ZipMaster in the subject).
    updates: http://www.delphizip.org
    DelphiZip maillist subscribe at http://www.freelists.org/list/delphizip 
************************************************************************)


interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    Button2: TButton;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
//    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    funcs: TStringList;
    template: TStringList;
  public
    function LoadFuncList(const fn: string): Integer;
    function LoadTemplate(const fn: string): Integer;
    function MakeAsm(const aName, sName: string): Integer;
//    function MakeAsm(dll: Integer; const fn: String): Integer;
    function MakeFunc(dst: TStrings; const func: string; dll: Integer): integer;
//    function Process(const base: string): Integer;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}
                
//#define DLL_KERNEL32        0
//#define DLL_USER32          1
//#define DLL_SHELL32         2
const
  MAX_DLL = 2;
  DLL_Names: array [0..MAX_DLL] of string = (
    'KERNAL' , 'USER', 'SHELL');
//  ASM_Names: array [0..MAX_DLL] of string = (
//    'DZKnl.asm', 'DZUsr.asm', 'DZShl.asm');

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(funcs);
  FreeAndNil(template);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  funcs := TSTringList.Create;
  template := TStringList.Create;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  l: integer;
begin
  try
    l := LoadFuncList('..\FuncList.txt');
  except
    l := -999;
  end;
  if l < 1 then
  begin
    Memo1.Lines.Add(' could not load function list');
    exit;
  end;
  try
    l := MakeAsm('..\jmps.asm', '..\funcs.cpp');
  except
    l := -999;
  end;
  if l < 12 then
  begin
    Memo1.Lines.Add(' could not write asm');
    exit;
  end;              
end;

function TForm1.LoadFuncList(const fn: string): Integer;
var
  good: boolean;
  i: integer;
  n: integer;
  c: integer;
  s: string;
  d: string;
  f: string;
begin
  funcs.Clear;
  funcs.LoadFromFile(fn);
  i := funcs.Count -1;
  while i >= 0 do
  begin
    good := false;
    s := StringReplace(funcs[i], ' ', '', [rfReplaceAll]);
    if (Length(s) > 6) and (s[1] in ['A'..'Z'])   then
    begin
      c := Pos(',', s);
      if c >= 5 then
      begin
        d := copy(s, 1, c-1);
        for n := 0 to MAX_DLL do
          if d = Dll_Names[n] then
          begin
            // check the function
            f := copy(s, c+1, 70);
            if (length(f) >= 4) and (f[1] in ['a'..'z', 'A'..'Z', '_']) then
            begin
              good := True;
            end;
            break;
          end;
      end;
    end;
    if not good then
    begin
      Memo1.Lines.Add(' removed ' + funcs[i]);
      funcs.Delete(i);  // remove rubbish
    end;
    dec(i);
  end;
  Result := funcs.Count;
end;

function TForm1.LoadTemplate(const fn: string): Integer;
begin
  template.Clear;
  template.LoadFromFile(fn);
  Result := template.Count;
end;

function DllNo(const dname: string): Integer;
begin
  result := 0;
  while Result <= MAX_Dll do
  begin
    if dname = Dll_Names[Result] then
      exit;
    inc(Result);
  end;
  Result := -1;
end;

function TForm1.MakeAsm(const aName, sName: string): Integer;
var
//  ids: integer;
  asmtxt: TStringList;
  nametxt: TStringList;
  func: string;
  i: integer;
  c: integer;
  dll: integer;
  d: string;
  s: string;
begin
  Result := 10;
  nametxt := nil;
  asmtxt := TStringList.Create;
  try
    nametxt := TStringList.Create;
    asmtxt.Add('        .486p');
    asmtxt.Add('        model flat');
    asmtxt.Add('  ');
    for i := 0 to Funcs.Count - 1 do
    begin
      s := funcs[i];
      c := Pos(',', s);
      if c >= 5 then
      begin
        d := copy(s, 1, c-1);
        dll := DllNo(d);
        if dll < 0 then
        begin
          Memo1.Lines.Add('  failed to make: ' + s);
          exit;
        end;
        // get the function
        func := copy(s, c+1, 70);       
        asmtxt.Add('        extern z' + func + ' : proc');
        asmtxt.Add('        global ' + func + ' : proc');
      end;
    end;
    asmtxt.Add('  ');
    asmtxt.Add('        public _u_jmps');
    asmtxt.Add('  ');
    asmtxt.Add('_DATA   segment dword public use32 ''DATA''');
    asmtxt.Add('_u_jmps:');
    for i := 0 to Funcs.Count - 1 do
    begin
      s := funcs[i];
      c := Pos(',', s);
      if c >= 5 then
      begin
        // get the function
        func := copy(s, c+1, 70);
        asmtxt.Add('        dd z' + func);
      end;
    end;
    asmtxt.Add('_DATA   ends');
    asmtxt.Add('  ');
    asmtxt.Add('_TEXT   segment dword public use32 ''CODE''');  
    nametxt.Add(' ');
//    nametxt.Add('STRINGTABLE LANGUAGE 9, 1 // 0409');
    nametxt.Add('const char *func_names[] =');
    nametxt.Add('{');
//    ids := 512;
    Result := 0;
    for i := 0 to Funcs.Count - 1 do
    begin
      s := funcs[i];
      c := Pos(',', s);
      if c >= 5 then
      begin
        d := copy(s, 1, c-1);
        dll := DllNo(d);
        if dll < 0 then
        begin
          Memo1.Lines.Add('  failed to make: ' + s);
          Result := -1;
          break;
        end;
        // get the function
        func := copy(s, c+1, 70);
        asmtxt.Add(func + ' proc near');
        asmtxt.Add('        jmp dword ptr [_u_jmps + ' + IntToStr(i * 4) + ' ]');
        asmtxt.Add(func + ' endp');
        asmtxt.Add('  ');
//        nametxt.Add('    ' + IntToStr(ids) + ', "' + IntToStr(dll) + func + '"');
        nametxt.Add('    "' + IntToStr(dll) + func + '",');
        inc(Result);
//        inc(ids);
      end;
    end;
    asmtxt.Add('_TEXT   ends');
    asmtxt.Add('  ');
    asmtxt.Add('      end');   
//    nametxt.Add('}  ');
    nametxt.Add('    0');
    nametxt.Add('};');
    if Result > 0 then
    begin
      asmtxt.SaveToFile(aname);
      nametxt.SaveToFile(sName);
    end;
  finally
    asmtxt.Free;
    FreeAndNil(nametxt);
  end;
end;

function TForm1.MakeFunc(dst: TStrings; const func: string; dll: Integer):
    integer;
var
  lno: integer;
  active: boolean;
  s: string;
  dno: string;
begin
  Memo1.Lines.Add('Making: ' + DLL_Names[dll] + '.' + func);
  Result := 0;
  active := false;
  dno := IntToStr(dll);
  for lno := 0 to template.Count -1 do
  begin
    s := template[lno];     
    if (length(s) >= 3) and (copy(s, 1, 3) = ';;;') then
    begin
      if active then
        break;
      active := true;
      continue;
    end;
    if active then
    begin
      s := StringReplace(s, 'NAME', func, [rfReplaceAll]);
      s := StringReplace(s, 'DLL', dno, [rfReplaceAll]);
      dst.Add(s);
      inc(Result);
    end;
  end;
end;


end.
