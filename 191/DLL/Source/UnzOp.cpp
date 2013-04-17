#include "stdafx.h"
#pragma hdrstop
/*

Copyright (c) 1990-2007 Info-ZIP.  All rights reserved.

See the accompanying file LICENSE, version 2007-Mar-4 or later
(the contents of which are also included in zip.h) for terms of use.
If, for some reason, all these files are missing, the Info-ZIP license
also may be found at:  ftp://ftp.info-zip.org/pub/infozip/license.html

parts Copyright (C) 1997 Mike White, Eric W. Engler
 ************************************************************************
Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht

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
 *********************************************************************** */
#include "UnzOp.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZOP_CPP

// ---------------------------------------------------------------------------
// #pragma package(smart_init)

#include "enter.h"


UnzOpr::UnzOpr(const DllCommands *C) : UnzInf(C)
{
    fdflag = 0;
    ffflag = 0;
    fjflag = 0;
    flflag = 0;
    foverwrite_all = 0;
    fvolflag = 0;
    fT_flag = 0;
    fuflag = 0;
    fzflag = 0;

    ffilespecs = 0;
    SrchSpecs = NULL;
    fprocess_all_files = 0;
    fcreate_dirs = 0;
    fextract_flag = 0;
    freal_ecrec_offset = 0;
    fexpect_ecrec_offset = 0;
    fziplen = 0;
    fhold = 0;
    fno_ecrec = 0;
#ifdef NOVELL_BUG_FAILSAFE
    fdne = 0;
#endif

    fcreated_dir = 0;
    frenamed_fullpath = 0;
    ffnlen = 0;
    fnLabelDrive = 0;
    frootlen = 0;
    fhave_dirname = 0;
    fdirnamelen = 0;
    fnotfirstcall = 0;
    fwild_dir = 0;

    ffiles_acted_on = 0;
    fEncodedAs = 0; // default Auto
    // #ifdef CRYPT
//    flpszPassword[PWLEN + 1];
    fcchPassword = 0;
    fpwork = 0;
    frcode = 0;
    // #endif
    flflag = (-1);
    fsol = true; // At start of line.
    // give the component the 'key'
    CB->SetArg1( ((unsigned)this) >> 2 );
    Set_Oper(this, UNZOPER); // add to 'active' list
    CB->UserCB(zacKey);
}

UnzOpr::~UnzOpr(void)
{
    Set_Oper(this, 0); // remove from list
    CB->SetArg1( 0 );
    CB->UserCB(zacKey);
    delete[]SrchSpecs;
    SrchSpecs = NULL;
    TakeDownFromProcessZipFile();
}

int UnzOpr::Init(void)
{
    DZStrW ExtDir = CB->UserArg(zcbExtractDir, 0, 0);
    // RP allow relative extract path
    if (!ExtDir)
        ExtDir = _T(".\\");
    TCHAR lc = ExtDir.LastChar();
    if (lc != _T('\\') && lc != _T(':'))
        ExtDir += _T('\\');
    DZStrW fExtractPath;
	int l = (int)GetFullPathName(ExtDir.c_str(), MAX_PATH, fExtractPath.GetBuffer(MAX_PATH),
        NULL);
    fExtractPath.ReleaseBuffer(l);
    if (!l)
        return -1;
    AddBase(fExtractPath, true);

    return 0;
}

DZOp *MakeUnzipper(const DllCommands *C)
{
    return new UnzOpr(C);
}
