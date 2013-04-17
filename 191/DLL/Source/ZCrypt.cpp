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
************************************************************************/
#include "ZipDflt.h"
#include "crypt.h"

#include <stdlib.h>
#include <process.h>                /* For prototype of getpid()      */
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZCRYPT_CPP

#ifdef _MSC_VER
   #define ZCR_SEED2 _getpid()          /* RCV Added; see note in Crypt.c */
#else
   #define ZCR_SEED2 getpid()          /* RCV Added; see note in Crypt.c */
#endif

#define zencode(c, t, k)  (t = decrypt_byte(k), update_keys(c, k), t^(c))

// Write encryption header to file zfile using the password passwd and the
//   cyclic redundancy check crc. passwd :: Password string. crc :: Crc of file
//   being encrypted. zfile :: Where to write header.
#define Putc(c, f)  (temp[tcnt++] = (c))
//void ZipDflt::crypthead(/*HANDLE zfile,*/ const char *passwd, ulg crc)
void ZipDflt::crypthead(const char* passwd, ulg crc)
{
    int           n;      // Index in random header.
    int           t;      // Temporary.
    int           c;      // Random byte.
    int           ztemp;  // Temporary for zencoded value.
    uch           header[RAND_HEAD_LEN - 2];  // Random header.
    uch           temp[RAND_HEAD_LEN + 2];
    int           tcnt = 0;
    unsigned long k;

    // First generate RAND_HEAD_LEN - 2 random bytes. We encrypt the output of
    //   rand() to get less predictability, since rand() is often poorly
    //   implemented.

    if (++fcalls == 1)
    {
		srand((unsigned)time(NULL) ^ (unsigned)ZCR_SEED2);
    }

    init_keys(passwd, fkeys);

    for (n = 0; n < RAND_HEAD_LEN - 2; n++)
    {
        c = (rand() >> 7) & 0xFF;
        header[n] = (uch) zencode(c, t, fkeys);
    }

    // Encrypt random header (last two bytes is high word of crc)
    init_keys(passwd, fkeys);

    for (n = 0; n < RAND_HEAD_LEN - 2; n++)
    {
        ztemp = zencode(header[n], t, fkeys);
        Putc((uch) ztemp, zfile);             // V1.5 Added (uch)
    }

    ztemp = zencode((int)(crc >> 16) & 0xFF, t, fkeys);

    Putc((uch) ztemp, zfile);               // V1.5 Added (uch)
    ztemp = zencode((int)(crc >> 24) & 0xFF, t, fkeys);
    Putc((uch) ztemp, zfile);               // V1.5 Added (uch)
    fZipOutfile->Write(temp, (DWORD)tcnt, &k);
    //  Assert(k < (RAND_HEAD_LEN + 2), "temp buffer overflow");
    fBytesWritten += k;
}

#define BUF_SIZE (1024 * 16)
// If requested, encrypt the data in buf, and in any case call fwrite() with
//   the arguments to zfwrite(). Return what fwrite() returns. buf :: Data
//   buffer. item_size :: Size of each item in bytes. nb :: Number of items. f
//   :: File to write to.
unsigned __fastcall ZipDflt::zfwrite(const uch *buf, ::extent nb)
{
    unsigned long k;
    int           t;            // temporary
    unsigned      r;

    if (fkey)
    {
        // key is the global password pointer
        ulg   size;               // buffer size
        char  *p = (char *)buf; // steps through buffer
        DZStrA bf;
        char * bfr = bf.GetBuffer(BUF_SIZE);
        while (nb)
        {
            ulg tw = BUF_SIZE;// sizeof(fewetmp);
            char *d = bfr;//fewetmp;

            if (tw > nb)
                tw = (ulg)nb;

            // Encrypt data in buffer
            for (size = tw; size != 0; p++, size--)
            {
                *d++ = (char)zencode(*p, t, fkeys);
            }

            r = fZipOutfile->Write(bfr, tw, &k);
//            r = fZipOutfile->Write(fewetmp, tw, &k);

            fBytesWritten += k;
            nb -= k;

            if (!r)
            {
                diag(_T("zfwrite failed"));
                return r;
            }
        }

        return 1;
    }

    // Write the buffer out
    r = fZipOutfile->Write(buf, (DWORD)nb, &k);
    if (!r)                            
        Notify(IDIAG, _T("zfwrite failed [%s]"), SysMsg().c_str());
//        diag(_T("zfwrite failed"));

    fBytesWritten += k;
    return r;
}

/* 30/1/07 */


