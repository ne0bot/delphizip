#include "stdafx.h"
#pragma hdrstop
#include "DZRaw.h"
#include "ZipDflt.h"
#include "ZipOp.h"
//#include "Zip.h"
//#include "ZipErr.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPSS_CPP

/* Win32Zip.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
 ** distributed under LGPL license ** see license.txt for details

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

int ZipOp::ZipStreamStream(void)
{
    fwindow_size = 0L;

    if (fGEncrypt)
    {
        fkey = fGPassword;

        if (!fkey || !*(fkey))
        {
            // use global
            if (GetUserPW() != DZ_ERR_GOOD)
                return -1;  // error

            fkey = fuser_key;
        }
    }

    fZipInfile = new ZStream(this, _T("0:<INSTREAM>"), fSS->fSSInput);

    AutoStream inz(&fZipInfile);
    fimax = fSS->Size;
    fcrc = crc32(0L, NULL, 0);
    fisize = 0;
    CB->SetArg1(1);
    CB->UserCB(zacCount);

    // Pass total filesize.
    CB->SetFileSize(fimax);
    CB->UserCB(zacSize);
    ulg f_crc = 0;
    __int64 fsz = 0;
    bool haveCRC = false;

    if (fkey)
    {
        if (!fNoPrecalc)
        {
            if (Verbose < 0)
                Notify(ITRACE,  _T("about to call Precalculate CRC"));

            // +++++ get CRC before we start
			CB->UserXItem(fimax, 13, _T("*PreCalculate"));
            __int64 pos1 = 0;

            if (!fZipInfile->IsFile)
                pos1 = fZipInfile->SetPosition(0, FILE_CURRENT); // get start posn

			f_crc = crc32(0L, NULL, 0);
            unsigned long byts;

            while (true)
            {
                unsigned ToRead = sizeof(fwindow);

                if (fimax > 0 && (fsz + ToRead) > fimax)
                {
                    ToRead = (unsigned)(fimax - fsz);

                    if (!ToRead)
                        break;
                }

                if (!fZipInfile->Read(fwindow, ToRead, &byts) || !byts)
                    break;

				fsz += byts;
				f_crc = crc32(f_crc, (const uch*)fwindow, (int)byts);
				CB->UserXProgress(byts, 13);

                if (Abort_Flag)
					Fatal(DZ_ERM_ABORT, 0);
            }

            fSS->CRC = f_crc;

            haveCRC = true;
            // reposition

            if (fZipInfile->SetPosition(pos1, FILE_BEGIN) != pos1)
            {
                if (Verbose)
					Notify(IVERBOSE, _T("Could not reposition %s [%s]"),
						fZipInfile->fname.c_str(), SysMsg().c_str());

				return  DZError(DZ_ERM_ERROR_SEEK);
			}

            if (fimax > fsz)
                fimax = fsz;
        }

        // ----- get CRC before we start
        // Since we do not yet know the crc here, we pretend that the crc is the
        //   modification time:
//    if (!haveCRC)
//      fSS->CRC = z->tim << 16;
        if (Verbose < 0)
            Notify(ITRACE,  _T("using supplied CRC %lu"), fSS->CRC);
    }

    // connect to output
    fZipOutfile = new ZStream(this, _T("0:<OUTSTREAM>"), fSS->fSSOutput);

    AutoStream outz(&fZipOutfile);

    CB->UserItem(fimax, _T("<INSTREAM>"));

	if (fkey)
		crypthead(fkey, fSS->CRC);  // write

    // Write stored or deflated file to zip file
    fSS->Method &= 0xFF;

    if (fSS->Method != DEFLATE)
        fSS->Method = 0;

    if (flevel < 1)
        fSS->Method = 0;

	int mthd = (int)fSS->Method;

    if (mthd == DEFLATE)
    {
        if (Verbose < 0)
            Notify(ITRACE,  _T("about to call Deflate"));

        bi_init();
        ush att = BINARY;
        ush flg = FLAG_ENCRYPT_BIT;

        // will be changed in deflate()
        ct_init(&att, &mthd);
        lm_init(flevel, &flg);

        // PERFORM THE DEFLATE
        fSS->Size = deflate();

        if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);
    }
    else
    {
        int k;

        if (Verbose)
            Notify(IVERBOSE, _T("Storing %s "), fZipInfile->fname.c_str());

        while ((k = read_buf(fwindow, sizeof(fwindow))) > 0
                && k != EOF)
        {
            if (Abort_Flag)
				Fatal(DZ_ERM_ABORT, 0);

			if (!zfwrite(fwindow,  (extent)k))
				return DZ_ERM_TEMP_FAILED;
        }

    }
    /*  Finished Item */
    CB->UserItem(-1, _T("<INSTREAM>")); // mark end of item
    CB->UserCB(zacEndOfBatch); // done with stream compression

    if (haveCRC)
    {
        if (f_crc != fcrc)
            Notify(DZ_ERR_ERROR_READ | IWARNING, _T(" File CRC changed while zipping: %s"),
                   fZipInfile->fname.c_str());

        if (fisize != fsz)
            Notify(DZ_ERR_ERROR_READ | IWARNING, _T(" File size changed while zipping: %s"),
                   fZipInfile->fname.c_str());
    }

    fSS->Size = fisize;

    fSS->CRC = fcrc;
    fSS->Method = (DWORD)mthd | (fkey ? 0xff00 : 0);
    return DZ_ERR_GOOD;
}


