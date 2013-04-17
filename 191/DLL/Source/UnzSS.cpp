#include "stdafx.h"
#pragma hdrstop
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZSS_CPP
/*
UnzSS.cpp -

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

#include "UnzOp.h"     
#include <stdio.h>

int UnzOpr::UnzStreamStream(void)
{
    CHdrInfo sinfo;
    fpInfo = &sinfo;
    int b, r;
    int error = 0;
    memset(&sinfo, 0, sizeof(CHdrInfo));
//    fnewzip = true;
    InProgress = true;
    fBytesWritten = 0;
    ffilename = _T("0:<INSTREAM>");
//    STRCPY(ffilename, "<INSTREAM>");
    fUnzInfile = new ZStream(this, ffilename, fSS->fSSInput);
    AutoStream inz(&fUnzInfile);
    /*---------------------------------------------------------------------------
    *    Initialize variables, buffers, etc.
    *---------------------------------------------------------------------------*/
//  fcsize = 50028;
//  fcsize = 0x40000000000i64;
    fcsize = fSS->Size;
    fbits_left = 0;
    fbitbuf = 0L;
    /* unreduce and unshrink only */
    fzipeof = 0;
    fnewfile = true;
    fdisk_full = 0;
    fcrc32val = CRCVAL_INITIAL;
    defer_leftover_input();
    // so NEXTBYTE bounds check will work
    int method = fSS->Method & 0xff;

    if ((unsigned)fSS->Method > 0xFF)
    {
        if (Verbose < 0)
            Notify(ITRACE, _T("about to call decrypt"));

        Encrypted = true;

        if ((error = decrypt(fSS->CRC >> 24, false)) != PK_COOL)
		{
//			int cbe;
//            CB->Arg1 = 0;
//            CB->Arg2 = SKIPPED_BAD_PASSWORD;
//			cbe = CB->UserCB(zacSkipped, ffilename);

            if (Verbose < 0)
				Notify(ITRACE, _T("Skipping encrypted Stream, bad password"));

			if (Skipping(ffilename, error, SKIPPED_BAD_PASSWORD))
				Fatal(DZ_ERM_SKIPPED, 2);

			if (Abort_Flag)// || cbe == CALLBACK_TRUE)
				Fatal(DZ_ERM_ABORT, 0);

            return error;
        }
    }

    fBytesWritten = 0;

    CB->SetArg1(1);
    CB->UserCB(zacCount);

    // Pass total filesize.
    CB->SetFileSize(fcsize);
    CB->UserCB(zacSize);
//    CBData.MsgP = "<UNDEFLATE>";
    CB->SetFileSize(fcsize);
    CB->UserCB(zacItem, _T("<undeflate>"));

    fUnzOutfile = new ZStream(this, _T("0:<OUTSTREAM>"), fSS->fSSOutput);
	AutoStream outz(&fUnzOutfile);
    fMax_Write = -1;//20UL * 1024 * 1024;

	switch (method)
    {

        case STORED:
            foutptr = Slide;
            foutcnt = 0L;

            while ((b = NEXTBYTE) != EOF && !fdisk_full)
            {
                * foutptr++ = (uch)b;

                if (++foutcnt == wsize)
                {
                    // EWE: wsize = 32K
                    flush(Slide, foutcnt, 0);
                    foutptr = Slide;
                    foutcnt = 0L;

                    if (Abort_Flag)
                    {
                        /* v1.6026 */
                        CloseOut();
                        undefer_input();
						Fatal(DZ_ERM_ABORT, 0);
                    }
                }
            }

            if (foutcnt) // flush final (partial) buffer
                flush(Slide, foutcnt, 0);
            ffiles_acted_on = 1;
            break;

        case DEFLATED:

        case ENHDEFLATED:
        {
            if (Verbose < 0)
                Notify(ITRACE,  _T("about to call Inflate"));

            if ((r = inflate(method == ENHDEFLATED)) != 0)
            {
                unsigned int zerr = DZ_ERR(r);
            if (zerr != DZ_ERR(PK_WARN) && 
                (zerr == DZ_ERR(PK_BADERR) || zerr == DZ_ERR(PK_NOZIP) ||
				zerr == DZ_ERR(PK_FIND) || zerr == DZ_ERR(PK_EOF)))
                {
                    return r;
                }

                Notify(0, _T("Error unzipping files"));

                error = PK_ERR;
            }
            else
            {
                ffiles_acted_on++;
                fSS->CRC = fcrc32val;
            }
        }

        break;

        default:
            /* should never get to this point */

            if (Verbose)// < 0)
                Notify(ITRACE, _T("should NEVER get here"));

            Notify(0, _T("Error stream - unknown method"));

			/* close and delete file before return? */
            return PK_ERR;
    }

    /* end switch (compression method) */
    CB->UserCB(zacEndOfBatch); // done with stream extraction

    if (fdisk_full)
    {
        /* set by flush() */
        if (fdisk_full > 1)
        {
            return PK_DISKFULL;
        }

        error = PK_WARN;
    }

    if (PK_Rank(error) > PK_Rank(PK_WARN))
    {
        return error;
    }

    if (Verbose)
        Notify(IVERBOSE,  _T("stream of size %Lu %s"), fBytesWritten,
            ftflag ? _T("Tested  ") : _T("Unzipped"));

    if (error != PK_COOL && Verbose < 0)
        Notify(ITRACE, _T("UnzStreamStream returning error: %d"), error);

    return error;
}

