#include "stdafx.h"
#pragma hdrstop
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZCRYPT_CPP
/*
* crypt.c (full version) by Info-ZIP.      Last revised:  [see crypt.h]
*
* This code is not copyrighted and is put in the public domain.  The
* encryption/decryption parts (as opposed to the non-echoing password
* parts) were originally written in Europe; the whole file can there-
* fore be freely distributed from any country except the USA.  If this
* code is imported into the USA, it cannot be re-exported from from
* there to another country.  (This restriction might seem curious, but
* this is what US law requires.) */

/* This version modified by Chris Vleghert and Eric W. Engler
* for BCB/Delphi Zip, Sep 22, 2000. */

/* This encryption code is a direct transcription of the algorithm from
* Roger Schlafly, described by Phil Katz in the file appnote.txt.  This
* file (appnote.txt) is distributed with the PKZIP program (even in the
* version without encryption capabilities). *

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

//#define ZCRYPT_INTERNAL

#include "UnzInf.h"

#include "crypt.h"
//#include "ucrypt.h"

//static int testp(UGlobals * pG, uch * h);
//static int testkey(UGlobals * pG, uch * h, char * key);

//#  define NEXTBYTE  (--fincnt >= 0 ? (int)(*finptr++) : readbyte())
/* ===========================================================================
* Get the password and set up keys for current zipfile member.  Return * PK_ class error. */
int __fastcall UnzInf::decrypt(ulg crc, bool noAsk)
{
    ush b;
    int n, n1; // , r;
    char h[RAND_HEAD_LEN];

    if (Verbose < 0)
        Notify(ITRACE, _T("in crypt.c, [incnt = %d]: "), fincnt); // stout

    /* get header once (turn off "encrypted" flag temporarily so we don't * try to decrypt the same data twice) */
    // fpInfo->encrypted = false;
    Encrypted = false;
    defer_leftover_input();
    for (n = 0; n < RAND_HEAD_LEN; n++)
    {
        b = (ush)NEXTBYTE;
        /* RCV: ush added */
        h[n] = /*(uch)*/(char)b;
        // Trace((pG, " (%02x)", h[n]));  //stdout
    }
    undefer_input();
    Encrypted = true;

//    if (Verbose < 0)
//        Notify(ITRACE, _T("EWE - in crypt.c, near fnewzip test"));

//    if (!fPwdReqCount)
//        return PK_WARN;
//    if (fnewzip)
//    {
//        fnewzip = false;
////        if (Verbose < 0)
////            Notify(ITRACE, _T("newzip was set to false"));
//
//        if (fP_flag == true)
//        {
//            /* user gave password on command line */
//#ifdef ZDEBUG
//            if (Verbose < 0)
//                Notify(ITRACE, _T("user set password=%s"), fpwdarg.c_str());
//#endif
//            fkey = fpwdarg;
//        }
//    }

    if (Verbose < 0)
        Notify(ITRACE, _T("EWE- near 'have key already' test"));
    /* if have key already, test it; else allocate memory for it */
    if (fkey)
    {
        if (!testkey(h, fkey, crc))
        {
            if (Verbose < 0)
                Notify(ITRACE, _T("existing pwd OK"));
            return PK_COOL; // existing password OK (else prompt for new)
        }
        else
            if (fnopwd || noAsk)
                return PK_WARN; // user indicated no more prompting
    }
    else
        if (fnopwd || noAsk)
            return PK_WARN; // user indicated no more prompting
    if (Verbose < 0)
        Notify(ITRACE, _T("EWE- near 'try a few keys' test"));

    if (!fPwdReqCount)
        return PK_WARN;
    /* try a few keys */
    n  = fPwdReqCount;
    n1 = 15;
    do
    {
        // Call the component with a request for a password
		CB->SetArg1(n);
		CB->SetMsg(ffilename);
        int ucb  = CB->UserCB(zacPassword);
        if (ucb == CALLBACK_TRUE) // have password
        {
            fkey = DupStr(DZStrA(CB->GetMsg()));
            if (!testkey(h, fkey, crc)) // test local
                    return PK_COOL;
        }
        if (ucb == CALLBACK_3) // gave no password
        {
            fnopwd = true;
            n1     = 1;
        }
        n = MIN((int)CB->GetArg1() & 0x0F, n1);
    }
    while (--n > 0 && --n1 > 0 && !Abort_Flag);

    return PK_WARN;
}
/* end function decrypt() */


/* ===========================================================================
*h   :: Decrypted header.
*key :: Decryption password to test. */
int UnzInf::testkey(const char *h, const char *key, ulg crc)
{
  ush b;

  int n;
  uch * p;
  uch hh[RAND_HEAD_LEN];
  /* decrypted header */

  /* set keys and save the encrypted header */
  init_keys(key, fkeys);
  memcpy(hh, h, RAND_HEAD_LEN);

  /* check password */
  for (n = 0; n < RAND_HEAD_LEN; n++)
  {
    hh[n] = (uch) zdecode(hh[n], fkeys);
  }

  b = hh[RAND_HEAD_LEN - 1];
  if (b != (ush)crc)
	return -1; // bad
//#endif
  /* password OK:  decrypt current buffer contents before leaving */
  for (n = (unsigned)fincnt > (unsigned) fcsize ? (int)fcsize : fincnt, p = finptr; n--; p++)
	*p = (uch) zdecode(*p, fkeys);
  return 0;  // OK
}
/* end function testkey() */





