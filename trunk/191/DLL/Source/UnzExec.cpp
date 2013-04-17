#include "stdafx.h"
#pragma hdrstop
#include "enter.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZEXEC_CPP
/*
unzmain.c -

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
************************************************************************/


/* Main UnZip DLL module
* This version modified by Chris Vleghert and Eric W. Engler
* for BCB/Delphi Zip.
* Based on Mike White's DLL, but is dissimilar in many ways.
* This module has the entry point for the two calls from BCB and Delphi.
*/

#include "UnzOp.h"
#include <excpt.h>

#define PWLEN 80
/* ===================================================== */

long UnzOpr::Exec(const DllCommands *C)
{
  int FilesActedOn;

  if (Verbose < 0)
	Notify(0,  _T("trace is on, UnzDllExec [%d %d %d]"), FCount, 
      ZCount, UCount);

  fzipfn = CB->UserArg(zcbFilename, 0, 0);
  if (!fzipfn)
  {
    /* Something screwed up, we don't have a filename */
	throw DZException(DZ_ERM_BAD_OPTIONS, _T("No zip filename received"));
  }

  /* ---------------------------------------------------------------- */
  if (Verbose < 0)
    Notify(ITRACE, _T("ready to setup"));
  if (FSetUpToProcessZipFile(C))
  {
    if (Verbose)
    {
        DZStrW tmp(DZ_Banner());
        Notify(IVERBOSE, _T("Using %s"), tmp.c_str());
        if (Verbose < 0)
            Notify(ITRACE, _T("root = %s"), CurBase->Base);
    }

//#ifdef USE_STRM_INPUT
//    if (fUseInStream)
//	  MemExtract();
//    else
//    {
//      if (fSS)
//      {
//        UnzStreamStream();
//        if (Verbose < 0)
//          Notify(ITRACE, _T("*** BACK FROM CALL TO unzip stream ***"));
//      }
//      else
//      {
//        process_zipfiles();   // Pass ptr to global bufs.
//        if (Verbose < 0)
//          Notify(ITRACE, _T("*** BACK FROM CALL TO process_zipfiles ***"));
//      }
//    }
//#else
    if (fSS)
    {
      UnzStreamStream(); 
      if (Verbose < 0)
        Notify(ITRACE, _T("*** BACK FROM CALL TO unzip stream ***"));
    }
    else
    {
	  process_zipfiles();   // Pass ptr to global bufs.
      if (Verbose < 0)
        Notify(ITRACE, _T("*** BACK FROM CALL TO process_zipfiles ***"));
    }
//#endif

    if (Verbose)
      Notify(IVERBOSE, _T("Files acted on = %d"), ffiles_acted_on);
  }
  FilesActedOn = ffiles_acted_on;
  inflate_free();
  TakeDownFromProcessZipFile();

  if (Verbose < 0)
	GiveTime();

  if (Abort_Flag)
  {
    if (Abort_Flag & (GA_EXCEPT | GA_EXCEPT2))
	  return -DZ_ERR_CALLBACK;
	if (Abort_Flag & GA_ABORT)
	  return -DZ_ERR_ABORT;
    if (Abort_Flag & GA_CANCEL)
	  return -DZ_ERR_CANCELLED;
  }
  return FilesActedOn;
}

/* =========================================================================== */
bool UnzOpr::FSetUpToProcessZipFile(const DllCommands *C)
{
//  char * ExtDir, *tp;
//  int len;

  fqflag = C->fOptions.Quiet; // quiet flag
  if (Verbose < 0)
    Notify(ITRACE, _T("FSetUpToProcessZipFile"));

  /* These flags all have a fixed value in this version. */
  fextract_flag = 1;
  ftflag = C->fOptions.OpIsTest;     /* if true, test zipfile */
  fT_flag = 1;     /* -T: timestamps (unzip) or dec. time fmt (zipinfo) */

  // set options from caller
  fcreate_dirs = C->fOptions.Directories; // used by main(), mapname(), checkdir()
  fdflag = C->fOptions.Directories; // "recreate dir structure"
  fjflag = !(fdflag);   // "junk pathnames"

  if (C->fHandle == 0)   // if we don't have a window handle, then
    fqflag = true; // we need to be quiet (no dialog boxes)

  fuflag = C->fOptions.Update; // "Update" - extract only newer files & brand new files
  ffflag = C->fOptions.Freshen; // "freshen" (extract only newer files)

  if (C->fOptions.Overwrite)
    foverwrite_all = true; // Don't ask, always overwrite else Don't overwrite; skip that file.

  ffilespecs = 0; // number of fspecs

//#ifdef CRYPT
  /* IMPORTANT! For ZIPDLL, the password is malloc'd, and then pointed-to
  * by the global "key",  However, in UNZDLL, this is done:
  * - "key" and "fkey" must remain NULL
  * - fpwdarg must point to the password passed-in, or else must be NULL
  * - fP_flag must be set to true if a password was passed-in */

  /* if no password, we will prompt user later (only if encrypted file found) */
  fpwdarg = CB->UserArg(zcbPassword, 0, 0).c_str();
  fP_flag = fpwdarg.empty() ? 0 : 1;// ? 1 : 0;
  fPwdReqCount = C->fPwdReqCount & 0x0F;
//#endif

  if (!C->fSS)
  {
//#ifdef USE_STRM_INPUT
//  if (!C->fUseInStream)
//  {
//	if (_stati64(fzipfn, & fstatbuf) || (fstatbuf.st_mode & S_IFMT) == S_IFDIR)
//	  STRCAT(fzipfn, ZSUFX);
//	if (_stati64(fzipfn, & fstatbuf))
//	{
//	  Notify(IERROR, _T(" can't find zipfile [%s]"), fzipfn);
//	  return false;
//	}
//	else
//	  fziplen = fstatbuf.st_size;
//  }
//#else
  if (_tstati64(fzipfn.c_str(), & fstatbuf))
  {
    Notify(IERROR, _T(" can't find zipfile [%s]"), fzipfn.c_str());
    return false;
  }
  else
    fziplen = fstatbuf.st_size;
//#endif
  }

  if (Verbose < 0)
	GiveTime();

  /*---------------------------------------------------------------------------
  *     Ok, we have everything we need to get started.
  *---------------------------------------------------------------------------*/
  foutbuf = new uch[OUTBUFSIZ + 1];
  finbuf = new uch[INBUFSIZ + 4];
  frootlen = 0;
  fhold = & finbuf[INBUFSIZ];
  /* to check for boundary-spanning signatures */

  if (fSS)
  {                                        
      fredirect_pointer = 0;
      fredirect_size = 0;
    return true;
  }
//#ifdef USE_STRM_INPUT
//  if (C->fUseInStream)
//  {
//    fUseInStream = C->fUseInStream;
//    fInStream = C->fInStream;
//    fInStreamSize = C->fInStreamSize;
//  }
//#endif
//#ifdef USE_STRM_OUTPUT
//  if (C->fUseOutStream)
//  {
//	fredirect_data = true;
//	fbuffer_size = C->fOutStreamSize;
//	fredirect_buffer = (unsigned char *) C->fOutStream;
//#ifdef USE_STRM_INPUT
//	if (fUseInStream)
//	{
//	  fredirect_pointer = foutbuf; // Circular buffer inside DLL
//	  fredirect_size = OUTBUFSIZ;
//    }
//	else
//    {
//#endif
//      fredirect_pointer = fredirect_buffer;
//	  fredirect_size = C->fOutStreamSize;
//#ifdef USE_STRM_INPUT
//	}
//#endif
//	if (!fredirect_buffer)
//	  return false;
//  }
//#endif
  return true;
  /* set up was OK */
}


/* ============================================================ */
void UnzOpr::TakeDownFromProcessZipFile(void)
{
  if (finbuf)
  {
	delete[] finbuf;
    finbuf = NULL;
  }
  if (foutbuf)
  {
	delete[] foutbuf;
    foutbuf = NULL;
  }
}



