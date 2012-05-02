unit CHILDWIN;
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
//modified 2012-04-09

interface

uses Windows, Classes, Graphics, Forms, Controls, StdCtrls;

type
{$IFDEF UNICODE}
  TZString = string;
  TZWString = string;
{$ELSE}
  UTF8String = type string;
  TZString = UTF8String;
  TZWString = WideString;
{$ENDIF}

type
  TMDIChild = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FIsReadOnly: Boolean;
    FMain: TForm;
    { Private declarations }
  public
    function AppendLine(const line: UTF8String): Boolean;
    function Load(const fname: string): Integer;
    property IsReadOnly: Boolean read FIsReadOnly write FIsReadOnly;
    property Main: TForm read FMain write FMain;
    { Public declarations }
  end;

implementation

uses SnifferMain2;

{$R *.dfm}

function TMDIChild.AppendLine(const line: UTF8String): Boolean;
var
  wmsg: TZWString;
begin
  Result := not IsReadOnly;
  if Result then
  begin
{$IFDEF UNICODE}
      wmsg := WideString(line);
      Memo1.Lines.Add(wmsg);
{$ELSE}
      Memo1.Lines.Add(line);
{$ENDIF}
  end;
end;

procedure TMDIChild.FormCreate(Sender: TObject);
begin
  IsReadOnly := false;
  Main := nil;
end;

procedure TMDIChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if Main is TSnifferForm then
    TSnifferForm(Main).ReleaseMe(self);
end;

function TMDIChild.Load(const fname: string): Integer;
begin
  Memo1.Lines.loadFromFile(fname);
  IsReadOnly := True;
  Result := Memo1.Lines.Count;
end;

end.
