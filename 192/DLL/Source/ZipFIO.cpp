#include "stdafx.h"
#pragma hdrstop
#include <stdlib.h>
#include "ZipOp.h"
#include "ZipFnc.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPFIO_CPP

/* FileIO.c
 * Copyright (C) 1990-1996 Mark Ad\ler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
 * distributed under LGPL license
 ** see license.txt for details

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


#include <time.h>
#include <errno.h>
#include <shellapi.h>
#include <io.h>

//#define PAD     0
#define ENOTSAM 17

#ifndef FOF_NORECURSION
#define FOF_NORECURSION            0x1000  // don't recurse into directories.
#endif


// Return a pointer to the start of the last path component. For a directory
//   name terminated by the character in c, the return value is an empty
//   string. p :: Sequence of path components. c :: Path components separator
//   character.
//static DZStrW last(const DZStrW &p, TCHAR c)
//{
//	int t = p.ReverseFind(c);
//    if (t < 0)
//        return p;
//	return p.Mid(t+1);
//}



int ZipFunc::check_dupExt(void)
{
	FndItem  *f;       // steps through found linked list
	if (!ffound)
		ffcount = 0;
	HashListExt fnds(ffcount);
	// Try adding each entry
	f = ffound;
    FndItem *p = NULL;
    while (f)
    {
        if (fnds.AddNode(f))
        {
            // duplicate found
//            if (Verbose)
                Notify(IWARNING,
                       _T("duplicate filename removed: %s "), f->xname);

            if (p)
              p->nxt = f->nxt;
            delete f;
            f = p->nxt;
			ffcount--;
        }
        else
        {
            p = f;
            f = f->nxt;
        }
    }
    return DZ_ERR_GOOD;
}

// Return true if the attributes are those of a symbolic link a ::
//   Attributes returned by filetime().
int issymlnk(ulg)// a)
{
	return 0;// (int)a & 0;  // Avoid warning on unused parameter.
//#pragma argused
}
                                                    
#ifndef UNICODE
int ZipFunc::replaceOrig(const DZStrW &d, const DZStrW& s)
{
    struct stati64 t;        // results of stat()
    int         copy = 0;
    int         d_exists;

	d_exists = _tstati64(d, &t) == 0;
	if (d_exists)
	{
		// respect existing soft and hard links!
		if (t.st_nlink > 1)
			copy = 1;
        else
        {
            if (_tunlink(d))
                return DZ_ERM_ERROR_CREATE;      // Can't erase zip file--give up
            Sleep(5);
        }
    }
    if (!copy)
    {
            // Just move s on top of d
        if (_trename(s, d))// !MoveFile(s, d))
        {
            if (Verbose < 0)
                Notify(IERROR, _T(" replace failed %s (%s)"), s, errno);
            copy = 1;                   // failed ?
            if (errno != ENOTSAM)
				return DZ_ERM_ERROR_CREATE;
        }
    }

    if (copy)
    {
        HANDLE  f,
        g;                    // source and destination files
        int     r;                    // temporary variable
        diag(_T("in replace - open for read"));

        if ((f = CreateFile(s, GENERIC_READ, 0, NULL, OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL)) <= 0)
        {
            diag(_T("in replace - bad open for Read"));
            Notify(0, _T(" replace: can't open %s [%s]"), s, SysMsg());

			return DZ_ERM_TEMP_FAILED;
        }

        diag(_T("in replace - fopen for write"));

        if ((g = CreateFile(d, GENERIC_WRITE, 0, NULL, CREATE_NEW,
                            FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL)) <= 0)
        {
            Close_Handle(&f);
            diag(_T("in replace - bad open for Write"));
			return DZ_ERM_ERROR_CREATE;
        }

        r = filecopy(f, g);

        Close_Handle(&f);
        if (!Close_Handle(&g) || r != DZ_ERR_GOOD)
        {
            DeleteFile(d);
            return r ? (DZ_ERR(r) == DZ_ERR_TEMP_FAILED ? DZ_ERR_ERROR_WRITE : r) : DZ_ERM_ERROR_WRITE;
        }

        DeleteFile(s);
    }

    return DZ_ERR_GOOD;
}
#endif

static DWORD CALLBACK MoveProgress(
    LARGE_INTEGER TotalFileSize,
    LARGE_INTEGER TotalBytesTransferred,
	LARGE_INTEGER /*StreamSize*/,
	LARGE_INTEGER /*StreamBytesTransferred*/,
	DWORD /*dwStreamNumber*/,
	DWORD /*dwCallbackReason*/,
	HANDLE /*hSourceFile*/,
	HANDLE /*hDestinationFile*/,
    LPVOID lpData
)
{
    long cnt = 0;
    ZipFunc *pG = (ZipFunc *)lpData;
    if (!pG)
        return PROGRESS_CONTINUE;
    if (TotalFileSize.QuadPart && TotalBytesTransferred.QuadPart)
    {
        cnt = (long)((100 * TotalBytesTransferred.QuadPart) / TotalFileSize.QuadPart);
    }
    if (pG->Verbose)
        pG->Notify(IVERBOSE, _T("replace %d%%"), cnt);
    return pG->CB->UserXProgress(cnt, 2) < CALLBACK_IGNORED ?
            PROGRESS_CANCEL : PROGRESS_CONTINUE;
//#pragma argsused
}

typedef BOOL(WINAPI *MoveWithProgress)(
    LPCTSTR lpExistingFileName,
    LPCTSTR lpNewFileName,
    LPPROGRESS_ROUTINE lpProgressRoutine,
    LPVOID lpData,
    DWORD dwFlags
);

int ZipFunc::replace(const DZStrW &d, const DZStrW &s)
{
#ifndef UNICODE
    int             r;
    HINSTANCE       hKernal;
    MoveWithProgress mover;
#endif

    if (Verbose)
        Notify(IVERBOSE, _T("replace '%s' with '%s'"), d.c_str(), s.c_str());
    else
      CB->UserCB(zacTick);  // take a little time

    if (d.IsEmpty() || s.IsEmpty())
    {
        diag(_T("in replace - missing filename"));
		return DZ_ERM_TEMP_FAILED;
    }
#ifdef UNICODE
	if (!MoveFileWithProgress(s.c_str(), d.c_str(), MoveProgress, this,
                  MOVEFILE_COPY_ALLOWED | MOVEFILE_REPLACE_EXISTING))
    {
	   int re = DZ_ERM_TEMP_FAILED;
		Notify((unsigned)re, _T(" replace: Move failed -[%s]"),  SysMsg().c_str());
        return re;
    }
    return DZ_ERR_GOOD;
#else
    hKernal = LoadLibrary("kernel32.dll");
    if (hKernal == NULL)
        return replaceOrig(d, s);
    mover = (MoveWithProgress) GetProcAddressA(hKernal, "MoveFileWithProgressA");
    if (mover == NULL)
    {
        FreeLibrary(hKernal);
        return replaceOrig(d, s);
    }
    r = DZ_ERR_GOOD;

    CB->UserXItem(100, 2, _T("Copying Temporary File"));
    if (!((mover)(s, d, MoveProgress, this,
                  MOVEFILE_COPY_ALLOWED | MOVEFILE_REPLACE_EXISTING)))
    {
        r = DZ_ERM_TEMP_FAILED;
        Notify(DZ_ERM_TEMP_FAILED, _T(" replace: Move failed [%s]"), SysMsg());
    }

    FreeLibrary(hKernal);
    return r;
#endif
}

// Return the file attributes for file f or 0 if failure. f :: File path.
// int getfileattr(char *f) { struct stat s; return SSTAT(f, &
//   s) == 0 ? s.st_mode : 0; }
// Give the file f the attributes a, return non-zero on failure. f :: file
//   path. a :: attributes returned by getfileattr().
int setfileattr(const DZStrW &f, int a)
{
	return _tchmod(f.c_str(), a);
}

// Return a temporary file name in its own malloc'ed space, using tempath.
DZStrW ZipFunc::tempname(void)
{
    int i;

    DZStrW pat = ftempath;
    pat.Trim();
    // p -> last char tempath (pat)
    DZStrW tempBase;
    if (!pat.IsEmpty() && pat[0] == WILDALL)
    {
        GetTempPath(MAX_PATH, tempBase.GetBuffer(MAX_PATH));
        tempBase.ReleaseBuffer();
        // skip leading dots in pat
        pat.TrimLeft(WILDALL);
    }
    // get path, name, ext
    int ldf = pat.ReverseFind(DOT);
    int lpf = pat.ReverseFind(BSLASH);
    if (ldf < lpf)
        ldf = -1;
    DZStrW ext;
    DZStrW nam;
    if (ldf >= 0)
    {
		ext = nam.Mid((unsigned)ldf);
    }
    if (ldf > lpf)
    {
        if (lpf > 0)
			nam = pat.Mid((unsigned)lpf + 1, (unsigned)(ldf - lpf) -1);
        else
			nam = pat.Mid((unsigned)ldf + 1);
    }
    // add path to base (if any of either)
    if (lpf >= 0)
    {
		tempBase += pat.Left((unsigned)lpf);
    }
    if (ext.IsEmpty())
        ext = _T(".zip");
    if (nam.IsEmpty())
        nam = _T("temp");

    if (!tempBase.IsEmpty() && tempBase.LastChar() != BSLASH)
        tempBase += BSLASH;

    // allow 256 attempts
    for (i = 0; i < 256; i++)
    {
        WIN32_FIND_DATA fdata;
        HANDLE          fh;
        DZStrW name = tempBase + nam;
        name.AppendFormat(_T("%04.4x"), rand());
		name += ext;

        fh = FindFirstFile(name.c_str(), &fdata);
        if (fh == INVALID_HANDLE_VALUE)
            return name;

        FindClose(fh);
    }


    // could not do it so try old way
    DZStrW name = tempBase + _T("ZipTmpXXXXXX");
#ifdef _MSC_VER
    if (_tmktemp_s(name.GetBuffer(MAX_PATH+2), MAX_PATH + 1) == NULL)
#else
    if (_tmktemp(name.GetBuffer(MAX_PATH+2)) == NULL)
#endif
    {
        name.ReleaseBuffer();
        name.Empty();
    }
    else
        name.ReleaseBuffer();

    return name;
}


// new 1,72 Copy from file *f to file *g, until EOF Return an error code in
//   the ZEN_ class. b :: malloc'ed buffer for copying. k :: Result of fread().
//   m :: Bytes copied so far.
int ZipFunc::filecopy(HANDLE f, HANDLE g)
{
    unsigned long cnt;            // bytes done
    ZInt64           m;              // bytes copied so far
    ZInt64          n;              // file size
    BY_HANDLE_FILE_INFORMATION info;
    if (Verbose)
        Notify(IVERBOSE, _T("Copying File"));

    if (!GetFileInformationByHandle(f, &info))
        return DZ_ERM_ERROR_READ;
    n = info.nFileSizeLow + ((ZInt64)info.nFileSizeHigh << 32);
    m = 0;
    CB->UserXItem(n, 2, _T("Copying Temporary File"));
	while (m < n)
    {
        if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);
        cnt = (unsigned long)ZWSIZE;
        if ((m + cnt) > n)
            cnt = (unsigned long)(n - m);


        if (!ReadFile(f, fwindow, cnt, &cnt, NULL))
			return DZ_ERM_ERROR_READ;

        if (!cnt)
            break;  // none read - finished
        if (!WriteFile(g, fwindow, cnt, &cnt, NULL))
        {
            Notify(0, _T(" filecopy: write error"));
			return DZ_ERM_TEMP_FAILED;
		}
        fBytesWritten += cnt;

        m += cnt;
        CB->UserXProgress(cnt, 2);
    }

    return DZ_ERR_GOOD;
}

// changed 1.71 - copies n bytes only (not to eof) Copy n bytes from file *f
//   to file *g. Return an error code in the ZEN_ class. b :: malloc'ed buffer
//   for copying. k :: Result of fread(). m :: Bytes copied so far.
int ZipFunc::fcopy(ZInt64 n)
{
    char  *b;   // buffer for copying
    ulg   k;
    ZInt64   m;    // bytes copied so far

    b = (char *) fwindow;
    m = 0;
    while (m < n)
    {
        k = ZWSIZE;
        if ((m + k) > n)
            k = (ulg)(n - m);

        if (!ReadFile(fhInz, b, k, &k, NULL))
			return DZ_ERM_ERROR_READ;

        if (!k)
            break;  // none read - finished
        if (!fZipOutfile->Write(b, k, &k))
        {
            Notify(0, _T(" fcopy: write error"));
			return DZ_ERM_TEMP_FAILED;
		}
        fBytesWritten += k;

		m += k;
		CB->UserProgress(k); // Added for progress bar support.
    }

    return DZ_ERR_GOOD;
}




