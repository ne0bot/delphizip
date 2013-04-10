program ZipResMaker;

(*
  ZipResMaker.pas - project file
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.

   This file is part of TZipMaster Version 1.9.1.x

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
//modified 2012-04-03


{$R 'ZipResMaker_ver.res' 'ZipResMaker_ver.rc'}

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Idents in 'Idents.pas',
  UWriter in 'UWriter.pas',
  About in 'About.pas' {AboutBox},
  SFXMakeStr in 'SFXMakeStr.pas',
  PathParser in 'PathParser.pas',
  Template in 'Template.pas',
  ToolHelper in 'ToolHelper.pas',
  StrMaker in 'StrMaker.pas',
  LocList in 'LocList.pas',
  ZipMstr in '..\..\ZipMstr.pas',
  ZMArgSplit in '..\..\ZMArgSplit.pas',
  ZMCenDir in '..\..\ZMCenDir.pas',
  ZMCentral in '..\..\ZMCentral.pas',
  ZMCompat in '..\..\ZMCompat.pas',
  ZMCore in '..\..\ZMCore.pas',
  ZMCtx in '..\..\ZMCtx.pas',
  ZMDefMsgs in '..\..\ZMDefMsgs.pas',
  ZMDelZip in '..\..\ZMDelZip.pas',
  ZMDlg in '..\..\ZMDlg.pas',
  ZMDllLoad in '..\..\ZMDllLoad.pas',
  ZMDllOpr in '..\..\ZMDllOpr.pas',
  ZMDrv in '..\..\ZMDrv.pas',
  ZMEOC in '..\..\ZMEOC.pas',
  ZMFileOpr in '..\..\ZMFileOpr.pas',
  ZMHash in '..\..\ZMHash.pas',
  ZMInflt in '..\..\ZMInflt.pas',
  ZMIRec in '..\..\ZMIRec.pas',
  ZMLister in '..\..\ZMLister.pas',
  ZMMatch in '..\..\ZMMatch.pas',
  ZMModOpr in '..\..\ZMModOpr.pas',
  ZMMsg in '..\..\ZMMsg.pas',
  ZMMsgStr in '..\..\ZMMsgStr.pas',
  ZMSFXInt in '..\..\ZMSFXInt.pas',
  ZMStructs in '..\..\ZMStructs.pas',
  ZMUTF8 in '..\..\ZMUTF8.pas',
  ZMUtils in '..\..\ZMUtils.pas',
  ZMWFuncs in '..\..\ZMWFuncs.pas',
  ZMWorkFile in '..\..\ZMWorkFile.pas',
  ZMWZip in '..\..\ZMWZip.pas',
  ZMXcpt in '..\..\ZMXcpt.pas',
  ZMZipFile in '..\..\ZMZipFile.pas',
  ZMZippedOpr in '..\..\ZMZippedOpr.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ZipMaster Resource Maker';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
