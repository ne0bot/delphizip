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
  FastMM4 in 'FastMM4.pas',
  Forms,
  Main in 'Main.pas' {Form1},
  About in 'About.pas' {AboutBox},
  PathParser in 'PathParser.pas',
  Template in 'Template.pas',
  LocList in 'LocList.pas',
  Base in 'Base.pas',
  SFXOprs in 'SFXOprs.pas',
  Opers in 'Opers.pas',
  StrOprs in 'StrOprs.pas',
  StrIdents in 'StrIdents.pas',
  DLLOprs in 'DLLOprs.pas',
  ResOprs in 'ResOprs.pas',
  ZipMstr in '..\..\ZipMstr.pas',
  ZMArgSplit in '..\..\ZMArgSplit.pas',
  ZMBaseOpr in '..\..\ZMBaseOpr.pas',
  ZMBody in '..\..\ZMBody.pas',
  ZMCoDec in '..\..\ZMCoDec.pas',
  ZMCommand in '..\..\ZMCommand.pas',
  ZMCompat in '..\..\ZMCompat.pas',
  ZMCore in '..\..\ZMCore.pas',
  ZMCRC in '..\..\ZMCRC.pas',
  ZMCtx in '..\..\ZMCtx.pas',
  ZMDelZip in '..\..\ZMDelZip.pas',
  ZMDlg in '..\..\ZMDlg.pas',
  ZMDllLoad in '..\..\ZMDllLoad.pas',
  ZMDrv in '..\..\ZMDrv.pas',
  ZMEngine in '..\..\ZMEngine.pas',
  ZMEntryReader in '..\..\ZMEntryReader.pas',
  ZMFileOpr in '..\..\ZMFileOpr.pas',
  ZMFStream in '..\..\ZMFStream.pas',
  ZMHandler in '..\..\ZMHandler.pas',
  ZMLister in '..\..\ZMLister.pas',
  ZMMatch in '..\..\ZMMatch.pas',
  ZMMFStream in '..\..\ZMMFStream.pas',
  ZMMisc in '..\..\ZMMisc.pas',
  ZMMsg in '..\..\ZMMsg.pas',
  ZMOprCore in '..\..\ZMOprCore.pas',
  ZMOprDeflate in '..\..\ZMOprDeflate.pas',
  ZMOprDel in '..\..\ZMOprDel.pas',
  ZMOprDll in '..\..\ZMOprDll.pas',
  ZMOprFile in '..\..\ZMOprFile.pas',
  ZMOprMerge in '..\..\ZMOprMerge.pas',
  ZMOprMod in '..\..\ZMOprMod.pas',
  ZMOprMsgStr in '..\..\ZMOprMsgStr.pas',
  ZMOprUnzip in '..\..\ZMOprUnzip.pas',
  ZMReg in '..\..\ZMReg.pas',
  ZMSFXInt in '..\..\ZMSFXInt.pas',
  ZMStructs in '..\..\ZMStructs.pas',
  ZMUnzipOpr in '..\..\ZMUnzipOpr.pas',
  ZMUTF8 in '..\..\ZMUTF8.pas',
  ZMUtils in '..\..\ZMUtils.pas',
  ZMWinFuncs in '..\..\ZMWinFuncs.pas',
  ZMXcpt in '..\..\ZMXcpt.pas',
  ZMZipBase in '..\..\ZMZipBase.pas',
  ZMZipDirectory in '..\..\ZMZipDirectory.pas',
  ZMZipEOC in '..\..\ZMZipEOC.pas',
  ZMZipMulti in '..\..\ZMZipMulti.pas',
  ZMZipReader in '..\..\ZMZipReader.pas',
  ZMZipWriter in '..\..\ZMZipWriter.pas',
  ZMZLibExApi in '..\..\ZMZLibExApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ZipMaster Resource Maker';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
