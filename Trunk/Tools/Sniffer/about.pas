unit About;
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

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Comments: TLabel;
    OKButton: TButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation

{$R *.dfm}
   
function VerInfoFileVersion(const fname: string): integer;
var
  dwInfoSize,           // Size of VERSIONINFO structure
  dwVerSize,            // Size of Version Info Data
  dwWnd: DWORD;         // Handle for the size call.
  FI: PVSFixedFileInfo; // Delphi structure; see WINDOWS.PAS
  ptrVerBuf: Pointer;   // pointer to a version buffer
begin
  Result := 0;
  dwInfoSize :=
    getFileVersionInfoSize(PChar(fname), dwWnd);

  if (dwInfoSize = 0) then
    Result := 0
  else
  begin
    getMem(ptrVerBuf, dwInfoSize);
    try

      if getFileVersionInfo(PChar(fname), dwWnd,
        dwInfoSize, ptrVerBuf) then
        if verQueryValue(ptrVerBuf, '\', pointer(FI),
          dwVerSize) then
          if dwVerSize = SizeOf(TVSFixedFileInfo) then
            Result := (hiWord(cardinal(FI.dwFileVersionMS)) *
              1000000) +
              ((FI.dwFileVersionMS and $FF) * 10000) +
              (hiWord(cardinal(FI.dwFileVersionLS)) * 100) +
              (FI.dwFileVersionLS and $FF);
    finally
      freeMem(ptrVerBuf);
    end;
  end;
end;

function VerToStr(ver: integer; Comma: boolean = false): string;
const
   fmt: array [boolean] of string = ('%d.%d.%2.2d.%2.2d', '%d,%d,%d,%d');
begin
  Result := Format(fmt[Comma], [Ver div 1000000,
    (Ver mod 1000000) div 10000, (Ver mod 10000) div 100, Ver mod 100]);
end;

procedure TAboutBox.FormActivate(Sender: TObject);
begin
  Version.Caption := VerToStr(VerInfoFileVersion(Application.ExeName));
end;


end.
 
