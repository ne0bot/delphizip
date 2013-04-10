Program MergeDemo_7;
(* ***************************************************************************
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht,
      Eric W. Engler and Chris Vleghert.

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

uses
  Forms,
  msgunit in 'msgunit.pas' {Msgform},
  mainunit in 'mainunit.pas' {Mainform},
  SortGrid in '..\SortGrid\SortGrid.pas',
  SortGridPreview in '..\SortGrid\SortGridPreview.pas' {SortGridPreviewForm},
  Mergeunit in 'Mergeunit.pas' {MergeForm},
  ZipMstr_A in '..\..\vcl\ZipMstr_A.pas',
  ZMArgSplit_A in '..\..\VCL\ZMArgSplit_A.pas',
  ZMCenDir_A in '..\..\VCL\ZMCenDir_A.pas',
  ZMCentral_A in '..\..\VCL\ZMCentral_A.pas',
  ZMCompat_A in '..\..\VCL\ZMCompat_A.pas',
  ZMCopyOpr_A in '..\..\VCL\ZMCopyOpr_A.pas',
  ZMCore_A in '..\..\VCL\ZMCore_A.pas',
  ZMCtx_A in '..\..\VCL\ZMCtx_A.pas',
  ZMDefMsgs_A in '..\..\VCL\ZMDefMsgs_A.pas',
  ZMDelZip_A in '..\..\VCL\ZMDelZip_A.pas',
  ZMDlg_A in '..\..\VCL\ZMDlg_A.pas',
  ZMDllLoad_A in '..\..\VCL\ZMDllLoad_A.pas',
  ZMDllOpr_A in '..\..\VCL\ZMDllOpr_A.pas',
  ZMDrv_A in '..\..\VCL\ZMDrv_A.pas',
  ZMEOC_A in '..\..\VCL\ZMEOC_A.pas',
  ZMExtrLZ77_A in '..\..\VCL\ZMExtrLZ77_A.pas',
  ZMFileOpr_A in '..\..\VCL\ZMFileOpr_A.pas',
  ZMHash_A in '..\..\VCL\ZMHash_A.pas',
  ZMInflt_A in '..\..\VCL\ZMInflt_A.pas',
  ZMIRec_A in '..\..\VCL\ZMIRec_A.pas',
  ZMLister_A in '..\..\VCL\ZMLister_A.pas',
  ZMMatch_A in '..\..\VCL\ZMMatch_A.pas',
  ZMModOpr_A in '..\..\VCL\ZMModOpr_A.pas',
  ZMMsg_A in '..\..\VCL\ZMMsg_A.pas',
  ZMMsgStr_A in '..\..\VCL\ZMMsgStr_A.pas',
  ZMSFXInt_A in '..\..\VCL\ZMSFXInt_A.pas',
  ZMStructs_A in '..\..\VCL\ZMStructs_A.pas',
  ZMUTF8_A in '..\..\VCL\ZMUTF8_A.pas',
  ZMUtils_A in '..\..\VCL\ZMUtils_A.pas',
  ZMWFuncs_A in '..\..\VCL\ZMWFuncs_A.pas',
  ZMWorkFile_A in '..\..\VCL\ZMWorkFile_A.pas',
  ZMWUtils_A in '..\..\VCL\ZMWUtils_A.pas',
  ZMWZip_A in '..\..\VCL\ZMWZip_A.pas',
  ZMXcpt_A in '..\..\VCL\ZMXcpt_A.pas',
  ZMZipFile_A in '..\..\VCL\ZMZipFile_A.pas',
  FilterUnit in 'FilterUnit.pas' {FilterForm};

{$R *.RES}
{.$R '..\..\Res\ZMRes_str.res'}
{.$R '..\..\Res\ZMRes_sfx.res'}

Begin
  Application.Initialize;
  Application.Title := 'Zip Merge Demo';
  Application.CreateForm(TMainform, Mainform);
  Application.CreateForm(TMsgform, Msgform);
  Application.CreateForm(TMergeForm, MergeForm);
  Application.CreateForm(TSortGridPreviewForm, SortGridPreviewForm);
  Application.CreateForm(TFilterForm, FilterForm);
  Application.Run;
End.
