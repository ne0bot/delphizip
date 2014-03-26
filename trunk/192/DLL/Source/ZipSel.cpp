#include "stdafx.h"
#pragma hdrstop
#include "zipOp.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPSEL_CPP

/*
  ZipSel.cpp - common definitions and functions

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
#if 0
// ** moved to DzOp
DZStrW ZipOp::ConvExclFilters(const DZStrW & filters)
{
    DZStrW exc;
    if (filters.IsEmpty())
        return exc;
    int n = 0;
    if (filters[0] == _T('|'))
    {
		exc = _T("|");
        n++;
    }
    int len = filters.length();
    while (n < len)
    {                        
        int nx = filters.Find(_T('|'), n+1);
        if (nx < 0)
            nx = len + 1;
        DZStrW comp = filters.Mid(n, nx - n);
        n = nx + 1;
        comp.Trim();
		DZStrW p = ex2IntForm(comp, true);

        if (!p.IsEmpty())
        {
            if (!exc.IsEmpty())
                exc += _T('|');
            exc += p;
        }
    }
    return exc;
}

DZStrW ZipOp::MakeExclFilters(void)
{
    DZStrW exc;
    int n = 0;
    while (true)
    {
        DZStrW arg = CB->UserArg(zcbFSpecArgsExcl, n, 0);

        if (arg.IsEmpty())
            break;

        if (!exc.IsEmpty())
            exc += _T('|');
        exc += arg;
        n++;
    }

	return ConvExclFilters(exc);
}
#endif

int ZipOp::MakeSpecials(void)
{
//	DZStrW
	const wchar_t* delims(_T(":;,"));

    ZFilter *spcl, *top = 0;
    DZStrW s, l, e, o;
	unsigned int len;
	int v, cnt = 0;
	unsigned int posn;
	int nxt;
    e = CB->UserArg(zcbSpecials, 0, 0);

	len = e.length();
    if (len && Verbose < 0)
        Notify(IVERBOSE, _T("Do not compress: \"%s\""), e.c_str());
    posn = 0;

    while (posn < len)
    {
		nxt = e.FindOneOf(delims, posn);

        if (nxt < 0)
            nxt = (int)len;

		unsigned int ol = (unsigned)nxt - posn;
		o = e.Mid(posn, ol).Trim();
		posn = (unsigned)nxt + 1;
        if (o.IsEmpty())
            continue;

        v = 0;      // level
        int n = o.Find(_T('|'));

        if (n > 0)
        {
			l = o.Mid((unsigned)n + 1).TrimLeft();
			o = o.Left((unsigned)n - 1).TrimRight();

            if (o.IsEmpty())
                continue;

            if (!l.IsEmpty())
                v = ((int)l[0]) & 15;
        }

        // make filter
        o.Insert(0, _T('*'));

        spcl = new ZFilter(o);

        spcl->Level = v;

        if (fSpecials == NULL)
            fSpecials = spcl;
        else
            top->Next = spcl;

        top = spcl;

        cnt++;
    }

    return cnt;
}

/* =========================================================================== */
//#ifdef CRYPT
int ZipOp::GetUserPW(void)
{
    if (fuser_key && *(fuser_key))
        return DZ_ERR_GOOD;

    // get password from user
    diag(_T("DLL was not passed a password"));

	CB->SetArg1(1);        // request cnt
    if (CB->UserCB(zacPassword) == CALLBACK_TRUE)
    {
        DZStrW pw(CB->GetMsg().c_str(), PWLEN);
        fkey = fuser_key = AddPW(pw, false);
    }
    else
		return DZ_ERM_PASSWORD_CANCEL;

    if (Verbose)
        Notify(IVERBOSE, _T("passwords match: %s"), fkey);

    return DZ_ERR_GOOD;
} 
//#endif

/* ===========================================================================
     select files to be processed
*/
int ZipOp::ZipSelect(const DllCommands *C)
{
    int r; // temporary variable
	long g_before = (long)fbefore; // 1.74 global 'before'
    int g_recurse = frecurse;
    int g_level = C->fLevel;
    g_level = g_level < 0 ? 0 : (g_level > 9 ? 9 : g_level);
	bool encrypt;
	int argno;

#ifdef DEBUG
    fToTest = 0;
    fTested = 0;
#endif
    fOldFAT = IsFileSystemOldFAT(fRootDir);
    MakeSpecials();
    DZStrW g_excludes = MakeExclFilters();   // Process arguments
    if (Verbose < 0)
        Notify(IVERBOSE, _T("Exclude: \"%s\""), g_excludes.c_str());
    diag(_T("ready to read zip file"));

    // the read will be done in file: zipfile.c
    if ((r = readzipfile()) != DZ_ERR_GOOD)
    {
        diag(_T("err returned from \"readzipfile\""));
        return DZError(r);
    }

    if (faction == UPDATE || faction == FRESHEN)
        fdoall = 1;

    r = 0;

	bool ExcChanged = true;
	DZStrW arg, spec, pwd, opt, tmp, theArg;

    for (argno = 0; !r; argno++)
    {
        spec.Empty();
        pwd.Empty();
        if (ExcChanged)
        {
            fExcludes = g_excludes;
            if (Verbose < 0)
                Notify(IVERBOSE, _T("Exclude now: \"%s\""), fExcludes.c_str());
            ExcChanged = false;
        }
        arg = CB->UserArg(zcbFSpecArgs, argno, 0);

		arg.TrimLeft();
        if (arg.IsEmpty())
            break;   // ran out of args

        // reset globals
        fkey = 0;
        frecurse = g_recurse;
        flevel = g_level;
		fbefore = (ulg)g_before;
        encrypt = fGEncrypt != 0;
        AddBase(fRootDir, true);
		AddPW(fGPassword, true);

		unsigned index = 0;
		TCHAR ch = arg[0];
		if (ch == ZForceNoRecurse || ch == ZForceRecurse)
		{
			index = 1;
			frecurse = (ch == ZForceNoRecurse)? 0 : 1;
		}

		theArg = GetArg(arg, index, true);
		int ffnerr = CleanPath(theArg, spec);//, checkA);
		if (ffnerr < 0 || spec.IsEmpty())
		{
			Notify(IWARNING, _T("skipping invalid search spec %s (-%d)"), arg.c_str(), -ffnerr);
            if ((ffnerr == Z_BAD_CHAR || ffnerr == Z_BAD_NAME) && Verbose < 0)
            {
				DZStrW tmp1, tmp2;
                for (unsigned i = 0; i < spec.Length(); i++)
                {
                    tmp2.Format(L" %x", spec[i]);
					tmp1 += tmp2;
                }
				Notify(IVERBOSE, _T("spec:%s"), tmp1.c_str());
            }
			if (Skipping(arg, 0, SKIPPED_BAD_NAME))
				Fatal(DZ_ERM_SKIPPED, 2);
			continue;
		}
		// check switches
		while (arg[index] == '/')
		{
			// process switches
			theArg = GetArg(arg, ++index, true);
			if (theArg.length() < 1)
				continue;
			ch = theArg[0]; // the switch
			if (ch > _T('z') || ch < _T('A'))
				continue;	// invalid
			if (ch >= _T('a'))
				ch &= 0xDF;	// uppercase

			if (theArg[1] == _T(':'))
			{
				switch (ch)
				{
					case _T('C'):
						if (_istdigit(theArg[2]))
						{
							// new compression level
							flevel = theArg[2] - _T('0');
						}
						break;
					case _T('F'):
						// new base
						theArg = theArg.Mid(2);
						if (theArg.IsEmpty())
						{
							// use current dir
							if (!GetFullPathName(_T(".\\"), MAX_PATH, theArg.GetBuffer(MAX_PATH), NULL))
								throw DZFatalException(DZ_ERM_MEMORY);
							theArg.ReleaseBuffer();
						}
						if (theArg.LastChar() != BSLASH)
							theArg += BSLASH;

						if (!CleanPath(theArg, tmp))//, checkA))
						{
							AddBase(tmp, true);

							if (Verbose < 0)
								Notify(IWARNING, _T("Root dir now %s"), tmp.c_str());
						}
						break;
					case _T('E'):
						tmp = ConvExclFilters(theArg.Mid(2));
						if (tmp.IsEmpty())
							fNoExtChk = 1;
						else
						if (tmp[0] == _T('|'))
							tmp = g_excludes + tmp; // append the new ones
						fExcludes = tmp;
						ExcChanged = true;

						if (Verbose < 0)
							Notify(IVERBOSE, _T("Exclude now: \"%s\""), fExcludes.c_str());
						break;
				default:
					if (Verbose < 0)
						Notify(IWARNING, _T("Ignoring invalid switch %s"), theArg.c_str());
				}
				continue;
			}
			if (ch == _T('S'))
			{
				if (theArg.length() == 1)
				{
					frecurse = 1;
					frecurse = 1;
				}
				if (theArg.length() == 2 && theArg[1] == _T('-'))
				{
					frecurse = 0;
				}
				continue;
			}
			if (Verbose < 0)
				Notify(IWARNING, _T("Ignoring invalid switch %s"), theArg.c_str());
		}
		if (arg[index] == ZPasswordFollows)
		{
			pwd = arg.Mid(++index);
			encrypt = !pwd.IsEmpty();
		}

        fpcount = fExcludes.IsEmpty() ? 0 : 1;

//#ifdef CRYPT
		if (encrypt)
		{
			// use global unless there is override
			if (pwd.IsEmpty())
				fkey = AddPW(fGPassword, true);
			else
				fkey = AddPW(pwd, true);

			if (!fkey || !*(fkey))
			{
				// use global
				if ((r = GetUserPW()) != DZ_ERR_GOOD)
					break;

				fkey = fuser_key;
			}
		}
//#endif
		fOldFAT = IsFileSystemOldFAT(spec);

        fdoall = 0; // do selected
        if (Is_Drv(spec) < 0)
        {
            if (Verbose < 0)
                Notify(IVERBOSE, _T("Collecting stream %s"), spec.c_str());

            r = procname(spec, false);
        }
        else
        {
            if ((faction == ADD) || (faction == UPDATE))
            {
                if (Verbose < 0)
                    Notify(IVERBOSE, _T("Collecting %s %s"), spec.c_str(),
                         (const wchar_t*)(frecurse ? _T("recurse") : _T(" ")));

                r = Wild(spec);
            }
            else    // Freshen or Delete - must be internal file
            {
                if (Verbose < 0)
                    Notify(IVERBOSE, _T("collecting %s %s"), spec.c_str(),
                           (frecurse ? _T("recurse") : _T(" ")));

                r = procname(spec, frecurse != 0);
            }
        }

		if (r != DZ_ERR_GOOD &&
			(DZ_ERR(r) == DZ_ERR_MISS || DZ_ERR(r) == DZ_ERR_INVAL_NAME))
		{
			/* this occurs if new file wasn't found */
			Notify((unsigned)r, _T("File specification \"%s\" skipped"), spec.c_str());
//			r = DZ_ERR_GOOD;
			if (Skipping(arg, 0,
				(DZ_ERR(r) == DZ_ERR_MISS) ? SKIPPED_NO_FILES : SKIPPED_BAD_NAME))
				r = DZ_ERM_SKIPPED;
			else
				r = DZ_ERR_GOOD;
//            }
		}

        if (r)
            return DZError(r);
        if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);
    }
//#ifdef DEBUG
//    if (Verbose)
//        Notify(IDIAG, "Filter tests - did %Lu of %Lu", fTested, fToTest);
//#endif
    return DZ_ERR_GOOD;
}





