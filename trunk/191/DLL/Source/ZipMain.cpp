#include "stdafx.h"
#pragma hdrstop
#include "zipop.h"
#include "enter.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPMAIN_CPP

/* DLLmain.c * Copyright (C) 1997 Mike White and Eric W. Engler
* Permission is granted to any individual or institution to use, copy, or
* redistribute this software so long as all of the original files are included,
* that it is not sold for profit, and that this copyright notice is retained.
* This version modified by Chris Vleghert BCB/Delphi Zip.
** distributed under LGPL license
** see license.txt for details

  Copyright (c) 1990-2007 Info-ZIP.  All rights reserved.

  See the accompanying file LICENSE, version 2007-Mar-4 or later
  (the contents of which are also included in zip.h) for terms of use.
  If, for some reason, all these files are missing, the Info-ZIP license
  also may be found at:  ftp://ftp.info-zip.org/pub/infozip/license.html

  parts Copyright (C) 1997 Mike White, Eric W. Engler
************************************************************************
 Copyright (C) 2009, 2010, 2011  by Russell J. Peters, Roger Aelbrecht

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
************************************************************************/

#include <signal.h>
#include <stdarg.h>
#include <direct.h>

long ZipOp::Exec(const DllCommands *C)
{
    try
	{
        if (Verbose < 0)
            Notify(0,  _T("trace is on, ZipExec [%d %d %d]"), FCount,
                   ZCount, UCount);

		ZipSetParam(C);

        ftempath = CB->UserArg(zcbTempPath, 0, 0);
        if (!ftempath.IsEmpty())
		{
            // Temporary path -b
            ftempath = StrIncSep(ftempath);
//            ftempdir = 1;
        }

        //  fzipfile
        // in this state, we need the name of the zip file
        DZStrW tmp = CB->UserArg(zcbFilename, 0, 0);
        if (tmp.IsEmpty() && !fSS)
        {
            // Something screwed up, we don't have a filename
            Notify(DZ_ERM_BAD_OPTIONS, _T("No zip filename received"));
			throw DZException(DZ_ERM_GENERAL);
        }
		if (Verbose < 0)
			Notify(ITRACE, _T("zip name = '%s'"), tmp.c_str());

		fzipfile = ziptyp(tmp);

		if (Verbose)
		{
//			DZStrW tmp(DZ_Banner());
//			Notify(IVERBOSE, _T("Using %s"), tmp.c_str());
			if (Verbose < 0)
			{
				GiveTime();
				GiveGlobals();
				Notify(ITRACE, _T("zip name = '%s'"), tmp.c_str());
				Notify(ITRACE, _T("root = %s"), fRootDir.c_str());
			}
			Notify(ITRACE, _T("full zip name = '%s'"), fzipfile.c_str());
			DZStrW tmp1(DZ_Banner());
			Notify(IVERBOSE, _T("Using %s"), tmp1.c_str());
		}

		if (fSS)
		{
			ZipStreamStream();
			ffiles_acted_on++;
			CB->UserCB(zacEndOfBatch); // done with in-memory compression
		}
		else
		{
			int err = ZipSelect(C);
			if (!err)//ZipSelect(C))
			{
//				if (Abort_Flag || ZipProcess() != 0)
//					ffiles_acted_on = 0;
				if (!Abort_Flag)
					err = ZipProcess();

				if (fBatchStarted)
					CB->UserCB(zacEndOfBatch); // done with a batch of files

				diag(_T("*** BACK FROM ZipProcess ***"));
				if (Abort_Flag || err != 0)
					ffiles_acted_on = 0;
			}
			if (err)
				return -DZ_ERR(err);
		}

		if (Verbose)
			Notify(0, _T("Files acted on = %d"), ffiles_acted_on);

		if (Abort_Flag)
		{
			if (Abort_Flag & (GA_EXCEPT | GA_EXCEPT2))
              return -DZ_ERR_CALLBACK;
            if (Abort_Flag & GA_ABORT)
			  return -DZ_ERR_ABORT;
            if (Abort_Flag & GA_CANCEL)
              return -DZ_ERR_CANCELLED;
        }
    }
    catch (DZException &E)
    {
		OutputDebugString(L"Exec - exception");
        DZError(E.ENo(), E.EMsg());

        if (fBatchStarted)
            CB->UserCB(zacEndOfBatch);

        return -DZ_ERR(E.ENo());
//        throw;
    }

    return ffiles_acted_on;
}

int ZipOp::ZipSetParam(const DllCommands *C)
{
    int ii;                               

    ii = C->fLevel;

    if (ii < 0)
        ii = 0;
    else
        if (ii > 9)
            ii = 9;

    flevel = ii;

    if (Verbose < 0)
        Notify(ITRACE, _T("setting compression level to %d"), flevel);

    faction = C->fOptions.OpIsDelete ? PURGE : ADD;

    if (C->fOptions.Freshen) // Freshen zip file--overwrite only -f
    {
        if (faction != ADD)
        {
            Notify(DZ_ERM_BAD_OPTIONS, _T("invalid options"));
			throw DZException(DZ_ERM_BAD_OPTIONS);
        }

        faction = FRESHEN;
    }

    if (C->fOptions.Update) // Update zip file--overwrite only if newer -u
    {
        if (faction != ADD)
        {
            Notify(DZ_ERM_BAD_OPTIONS, _T("invalid options"));
			throw DZException(DZ_ERM_BAD_OPTIONS);
        }

        faction = UPDATE;
    }

//    fzcomment = CB->UserArg(zcbComment, 0, 0);
//    if (fzcomment)
//        fzcomlen = strlen(fzcomment);
/* **** */
    fzcomment = CB->UserZCmnt();
    if (fzcomment)
		fzcomlen = (int)fzcomment.Length();
/* **** */

//    fGPassword = CB->UserArg(zcbPassword, 0, 0);

    return 0;
}


//#ifdef DZ_LOGGING
static char w(/*bool*/ int v)
{
    if (v)
        return '+';

    return '-';
}

void ZipOp::GiveGlobals(void)
{
//    if (!Verbose)
//        return;

    DZStrW tmp;

    tmp.Format(_T("g%c j%c J%c" /* k%c"*/), w(fAllowGrow), w(fpathput),
               w(fjunk_sfx));//, w(fdosify));

    Notify(IVERBOSE, _T("%s m%c o%c S%c t%c"), tmp.c_str() , w(fdispose), w(flatest),
           w(fhidden_files), w(fbefore));
}
//#endif







