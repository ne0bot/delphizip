#include "stdafx.h"
#pragma hdrstop
#include "ZipOp.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPWIN32_CPP

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

#include <direct.h>                   // for rmdir()
#include <time.h>
#include <io.h>
#include <ctype.h>
#include <sys\stat.h>
#ifndef __BORLANDC__
#define stati64 _stati64
#endif

const char *GetLongPathEA(void);

#define PATH_END      BSLASH //'\\'            // SLASH
#define HIDD_SYS_BITS (FILE_ATTRIBUTE_HIDDEN | FILE_ATTRIBUTE_SYSTEM)

class ZDir
{
		WIN32_FIND_DATA fdata;
        bool    ffirst;
//        bool    fdone;
        bool    hidden;
        bool    archiveOnly;
        ZipOp *Owner;
        HANDLE  d_hFindFile;
        ZDir();
        ZDir(const ZDir&);
        ZDir& operator=(const ZDir&);
public:
	bool Done;
        DWORD __fastcall GetAttrs() const;
        DZStrW __fastcall GetFilename() const;
        __int64 __fastcall GetSize() const;

    protected:
        bool __fastcall next(void);

    public:
        ZDir(ZipOp* theOwner, const DZStrW& path);
        ~ZDir(void);
        bool __fastcall Read();
//        __property DWORD Attrs = {read = GetAttrs};
//        __property DZStrW Filename = {read = GetFilename};
//        __property bool Done = {read = fdone};
//        __property __int64 size = {read = GetSize};
};

ZDir::ZDir(ZipOp* theOwner, const DZStrW& path)
{
    Owner = theOwner;
	Done = true;
    hidden = Owner->fhidden_files != 0;
    archiveOnly = Owner->fArchiveFiles != 0;

    DZStrW tmp;
    if (!Is_DrvEx(path))
        tmp = Owner->CurBase->Base;

    tmp += path;
    if (tmp.LastChar() != BSLASH)
        tmp += BSLASH;

    tmp += _T("*.*");   // "*.*" MS-DOS match-all spec
	d_hFindFile = FindFirstFile(tmp.c_str(), &fdata);
	Done = d_hFindFile == INVALID_HANDLE_VALUE;
}

ZDir::~ZDir(void)
{
    if (d_hFindFile != INVALID_HANDLE_VALUE)
        FindClose(d_hFindFile);
}

bool __fastcall ZDir::next(void)
{
    if (Done)
        return false;

    if (ffirst)
    {
        ffirst = false;
        return true;
    }

    if (!FindNextFile(d_hFindFile, &fdata))
		Done = true;

    return !Done;
}

DWORD __fastcall ZDir::GetAttrs() const
{
    if (Done)
        return 0;

    return fdata.dwFileAttributes & 0xFF; // ignore rest
}

DZStrW __fastcall ZDir::GetFilename() const
{
    if (Done)
        return DZStrW(_T(""));

    return DZStrW(fdata.cFileName);
}

__int64 __fastcall ZDir::GetSize() const
{
    if (Done)
        return 0;

	__int64 x = fdata.nFileSizeHigh;
    x <<= 32;
    return x + fdata.nFileSizeLow;
}


bool __fastcall ZDir::Read(void)
{
    if (Done)
        return false;

    unsigned        cnt = 0;
    DWORD attrs;

    // Loop to read all dir entries, until next normal entry is found
    do
	{
        if (!next())
            break;

        if ((++cnt & 0x1F) == 0)        // added 1.71.00
        {
            // tickle and check cancel
            if (Owner->CB->UserCB(zacTick) < CALLBACK_IGNORED)
                return false;
		}
		attrs = GetAttrs();
    }
    // Loop to read all dir entries, until next normal entry is found   
    while ((!hidden && (attrs & HIDD_SYS_BITS))
                 || (archiveOnly && !(attrs & FILE_ATTRIBUTE_DIRECTORY)
					 && !(attrs & FILE_ATTRIBUTE_ARCHIVE)));
    return !Done;
}


// If not in exclude mode, expand the pattern based on the contents of the
//   file system. This function is used while gathering filenames to be added
//   or updated w :: Path/pattern to match. Possible return values: ZEN_MISS,
//   DZ_ERR_GOOD, ZEN_ABORT, ZEN_MEM or ZEN_PARMS.
int ZipOp::Wild(const DZStrW &w)
{
	int   WError;           // Result of Wild()
	int   r;                // Result / temp var.
    bool  StopRecurs = false; // No recursion if filespec is file.

    if (Verbose < 0)
		Notify(ITRACE, _T("in Wild of win32zip.c, pattern=%s recurse=%d"), w.c_str(),
               frecurse);

    // diag(fewemsg, pG);
    // "zip -$ foo a:" can be used to force a drive name once. // v1.6017
    if (fvolume_label == 1)
    {
		fvolume_label = 2;
        flabel = getVolumeLabel(w, &flabel_time, &flabel_mode, &flabel_utim);

		// 1.78.4.1 RP - reject empty label
        if (!flabel.IsEmpty())
            (void)newname(flabel, 0);

        if (Is_Drv(w) == 1)
            return DZ_ERR_GOOD;
    }

    // Allocate and copy pattern
    // Separate path and name into p and q
    // We have '\' or '\name' or 'path1\path2\name2' or 'C:\path\name' but NOT
    // 'C:\name'
	DZStrW path(w);
	DZStrW name;
	DZStrW cdir;
	int cpos = w.ReverseFind(BSLASH);
	if (cpos >= 0 && (cpos > 0 && w[(unsigned)cpos - 1] != _T(':')))
    {
		path = w.Left((unsigned)cpos);   // before sep
        if (path.IsEmpty())
            cdir = _T("\\.");
		name = w.Mid((unsigned)cpos + 1);
    }
    else
    {
		cpos = w.ReverseFind(_T(':'));
        if (cpos > 0)
        {
            // We have 'C:' or 'C:\' or 'C:\name'
			if ((cpos + 1) < (int) w.length() && w[(unsigned)cpos + 1] == BSLASH)
                cpos++;

			path = w.Left((unsigned)cpos + 1);
			name = w.Mid((unsigned)cpos + 1);
            path += _T('.');
			cdir = path + _T(':');
        }
		else
			if (frecurse && (!w.Compare(_T(".")) || !w.Compare(_T(".."))))
            {
                // current or parent directory
                // Get "zip -r foo ." to work. Allow the dubious "zip -r foo .." but
				//   reject "zip -r -m foo ..". "dispose" means wipe out source path.
                if (fdispose && !w.Compare(_T("..")))
                    DZException(DZ_ERM_BAD_OPTIONS);

                name = _T("*.*");
            }
            else
            {
				// no path or device
                name = w;
                cdir = _T(".");
                path = cdir;
            }

    }

	if (frecurse && name.IsEmpty())
        name = _T("*.*");

    // take out a possibly redundant dir name of "."
    DZStrW tmp = path.Right(2);

    if (!tmp.Compare(_T(":.")) || !tmp.Compare(_T("\\.")))
        path = path.Left(path.length() - 1);

	// Only filename (not the path) can have special matching characters
    if (IsWild(path))
    {
        diag(_T("path has illegal chars"));
        return DZ_ERM_INVAL_NAME;
    }

    if (!IsWild(name))
    {
		// Speed up checking if file exits in case there are no wildcards
        struct stati64 s;  // and no recursion and no archiving v1.6016

		if (!frecurse && !fArchiveFiles)
        {
			if (!ZStat(GetFullPath(w), &s))  // file exists ?
                return procname(w, false);

			return DZ_ERM_MISS;  // woops, no wildcards where is the file!
		}
    }

    // Now that we have a dir spec, along with an fspec, we'll step in the dir
    //   specified to see if there's any matches against the fspec.
    WError = DZ_ERM_MISS; //ZEN_MISS02;

    ZDir dir(this, path);

    if (path[0] != _T('.') && path.LastChar() != BSLASH)
        path += BSLASH;

    if (!dir.Done)
	{
        while (dir.Read())
        {
            if (Abort_Flag)
				Fatal(DZ_ERM_ABORT, 0);

			DZStrW n = dir.GetFilename();

            // if e is NOT '.' or '..', and is a dir or match fspec.
            if (!n.Compare(_T(".")) || !n.Compare(_T("..")))
                continue;

			if (dir.GetAttrs() & FILE_ATTRIBUTE_DIRECTORY)
            {
                // We do not save dirs or go into dirs if norecursefiles==1 and we
                // have a file without * or ? specs.
                if (!StopRecurs && (fdirnames || frecurse))
                {
                    if (!path.IsEmpty() && path[0] == _T('.'))
                        path.Empty();

                    if (!path.IsEmpty())
                    {
                        if (path.LastChar() != BSLASH)
                            path += BSLASH;

                        n = path + n;
                    }

                    if (fdirnames)
                    {
                        // Save directory names also.
                        r = procname(n, false);

                        if (ZEN_Rank(r) > ZEN_Rank(WError))
                            WError = r;

                        if (DZ_ERR(r) == DZ_ERR_INVAL_NAME)
                            continue; // ignore invalid name

						if (ZEN_Rank(r) > 0)
							break;
                    }

                    if (frecurse)
                    {
						// Recursively go into dir and check for other pattern matches.
                        if (n.LastChar() != BSLASH)
                            n += BSLASH;

                        n += name;  // Add the original pattern.

                        r = Wild(n);

						// We keep a DZ_ERR_GOOD even when DZ_ERR_MISS occurs.
                        if (ZEN_Rank(r) > ZEN_Rank(WError))
                            WError = r;

                        if (DZ_ERR(r) == DZ_ERR_INVAL_NAME)
                            continue; // ignore invalid name

                        if (ZEN_Rank(r) > 0)
                            break;                        // An error, stop processing.
                    }
                }
            }
            else   // not a directory
                if (ZMatch(name, n))
                {
                    if (!path.Compare(_T(".")))
                        r = procname(n, false);
                    else
                    {
                        n = path + n;
                        r = procname(n, false);
                    }

					if (ZEN_Rank(r) > ZEN_Rank(WError))
                        WError = r;

                    if (DZ_ERR(r) == DZ_ERR_INVAL_NAME)
                        continue; // ignore invalid name

					if (ZEN_Rank(r) > 0)
                        break;
				}
		}                                         // end while
    }
    else
		diag(_T("can't open dir"));

    return WError;
}

// Process a name or wildcard expression to operate on (or exclude). We will
//   only arrive here if we do a Freshen or Delete. Return an error code in the
//   ZEN_ class. DZ_ERR_GOOD, ZEN_ABORT, ZEN_MISS03, ZEN_MISS04, ZEN_MISS05,
//   ZEN_MEM22, ZEN_MEM23 ArgName :: Name to process.
int ZipOp::procname(const DZStrW &ArgName, bool RecurseDir)
{
	int           m;                            // matched flag
    int           pnError;                      // ProcName error

    struct stati64 StatRes;                    // result of stat()
    ZipItem  *z;                           // steps through zfiles list

    if (Verbose < 0)
        Notify(ITRACE, _T("in procname, name=%s recurse=%d"), ArgName.c_str(),
               RecurseDir);

    m = 1;                                      // set dflt for success indicator

    ///* (0=success)

    if (Abort_Flag)
		Fatal(DZ_ERM_ABORT, 0);

    if (ArgName.IsEmpty())
        return DZ_ERM_MISS;

    // LSSTAT returns 0 if it's arg is any kind of file (even a dir).
    // IsShExp returns true if a wildcard symbol is found, or NULL if none
	//   were found -- IsShExp is in util.c
    if (IsWild(ArgName) || ZStat(GetFullPath(ArgName), &StatRes))
    {
        // diag("not a file or dir - 'ArgName' must be a wildcard fspec");
        // Upon finding a wildcard fspec, we need to mark entries in
        // the "zfiles" list that are included by the wildcard.
        // convert the "external" (native) filename to an internal
		//   ZIP-archive-compatible filename.
        DZStrW p = ex2IntForm(ArgName, false);  // shouldn't affect matching chars

        if (Verbose < 0)
			Notify(ITRACE, _T("in procname, searching %s"), p.c_str());

        // does any file already in the archive match this spec?
        // Note that we need the pathname and filename together for this

        for (z = fzfiles; z; z = z->nxt)
        {
            if (z->mark)
            {
#ifdef ZDEBUG
                if (Verbose < 0)
					Notify(ITRACE, _T("%s already marked"), z->GetIName().c_str());
#endif
                continue;       // only mark once
            }

            // z->name = 'external' version of zname for unmatched names
			if (ZMatch(p, z->GetIName()))
            {
                // name is in archive - mark it for updating
				z->mark = fpcount ? !ZMatch(fExcludes, z->GetIName()) : 1;
                if (Verbose)
					Notify(0, _T("%scluding %s"), z->mark ? _T("in") : _T("ex"), z->GetIName().c_str());

                if (z->mark)
				{
                    if (Is_Drv(ArgName) > 0  && ArgName[2] == BSLASH)
                    {
                        z->SetXName(ArgName.Left(3) + z->GetIName());
                    }

                    // RA password added
                    z->Passw = fkey;
                    z->Base = CurBase->Base;
                    z->options.keepver = fversion ? 1 : 0;

                    m = 0;  // we had at least one file in the archive that we marked
                }

                // if not wild it can only match once
                if (!IsWild(ArgName))
                    break;
            }
        }

        // returns 1 if no "zfiles" entries were marked, 0 if at least one entry
        //   was marked
        if (m)
        {
            // diag("returning ZEN_MISS04 from procname");
            return DZ_ERM_MISS;
        }

        // diag("returning DZ_ERR_GOOD from procname");
        return DZ_ERR_GOOD;
    }                         // end of "if (LSSTAT..."

    // Existing and good filename or directory-- add the name if it's a file,
    //   recurse if directory
    // diag("good entry to add, or a dir to go into");
    // check the status returned by LSSTAT earlier to see if 'ArgName' is a dir
    pnError = DZ_ERR_GOOD;

    if (!(StatRes.st_mode & S_IFDIR))
    {
        // it's not a directory - add file to found list
		if (Verbose < 0)
            Notify(IVERBOSE, _T("Adding file %s to found list"), ArgName.c_str());

        // newname (fileio.c) adds to found list. If error m!=0 on return
        if ((m = newname(ArgName, StatRes.st_size)) != DZ_ERR_GOOD)
        {
            if (Verbose < 0)
                Notify(ITRACE, _T("returning %d from procname after newname call"), m);

            return m;
        }
    }
    else
    {
        // It is a directory - Add trailing / to the directory name
        if (Verbose < 0)
            Notify(ITRACE, _T("procname dir = %s"), ArgName.c_str());

		// diag("Spot 2, directory found");
        DZStrW p;

		if (ArgName.Compare(_T(".")) && ArgName.Compare(_T("\\.")))
        {
			if (Verbose < 0)
				Notify(IVERBOSE, _T("Adding folder %s to found list"), ArgName.c_str());
			p = ArgName;
            if (p.LastChar() != BSLASH)
                p += BSLASH;

            // newname (fileio.c) adds to found list. If error m != 0 on return
            if (fdirnames && (m = newname(p, 0)) != DZ_ERR_GOOD)
            {
                if (Verbose < 0)
                    Notify(ITRACE, _T("returning %d from procname after 2nd newname call"),
                        DZ_ERR(m));

                return m;
            }
        }

        // recurse into directory
        // p = empty or dirname
        // diag("spot 4: optional recurse into dir");
        if (RecurseDir)
        {
			ZDir dir(this, ArgName);
            while (dir.Read())
            {
                if (Abort_Flag)
					Fatal(DZ_ERM_ABORT, 0);
				DZStrW a(p);

                a += dir.GetFilename();

                // form the new dir's pathname followed by the fname just read
                // (we need to send in the dir and fname, or it won't be detected
				//   as a valid file by LSSTAT)

                // diag("DOING RECURSIVE CALL TO PROCNAME FROM PROCNAME");
                if ((m = procname(a, RecurseDir)) != DZ_ERR_GOOD)
                {
					// recursive call failed; return code not DZ_ERR_GOOD
					// unknown error; RCV error handling changed
					pnError = m;
					break;
                }
            }                       // end while

        }
    }                           // (StatRes.st_mode & S_IFDIR) == 0)

    // diag("returning ZEN_ class from procname");
    return pnError;
}

#if 0
// ** moved to DzOp
// Convert the external file name to an "internal" file name form,
//   returning the malloc'ed string, or NULL if not enough memory.
//   I.e. Strip the drive if present, strip the path
//if we
//   don't want one and change the name to 8.3 if needed. Not implemented, but
//   also 'put in' change the short path to a long path.
DZStrW ZipOp::ex2IntForm(const DZStrW &exname, bool ignore)
{
    bool  pathput;

    if (ignore)
	{
        pathput = true;
    }
    else
	{
        pathput = fpathput;
    }

	DZStrW XName(exname);

    if (XName.IsEmpty())
        return XName;

	DZStrW nname = StrExtSep(XName);
    int p = DriveLen(nname);

    int len = nname.length();
    // Strip leading "\" to convert an absolute path into a relative path

    while (p < len && nname[p] == BSLASH)
        p++;

    // Strip leading "./" as well as drive letter v1.6017
    while ((p + 2) < len && (nname[p] == _T('.') && nname[p + 1] == BSLASH))
        p += 2;

    // This is where the dirname gets stripped if user doesn't want it
    if (!pathput)
    {
        int t = nname.ReverseFind(BSLASH);

        if (t >= p)
            p = ++t;
    }

    if (p)
    {
        nname = nname.Mid(p);
    }

    if (nname.length() > MAX_PATH)
    {
        int t = nname.length();
        XName = nname.Left(MAX_PATH);

        if (Verbose) // < 0)
            Notify(IWARNING, _T("File name %s is too long [%d]. Truncated to %s"),
                   nname.c_str(), t, XName.c_str());

        nname = XName;
    }

    return nname;
}
#endif

// Set last updated and accessed time of file f to the DOS time d. This is
//   called by dllzip.c to set date/time of the zipfile. RCV Removed __TURBOC__
//   version. f :: name of file to change. d :: dos-style time to change it to.
typedef struct  DosDate
{
    WORD  lo;
    WORD  hi;
}dosdate;

void stamp(const DZStrW& f, ulg d)
{
    HANDLE    fHandle;
    FILETIME  LocalFT,
    FT;

	fHandle = CreateFile(f.c_str(), GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
						 FILE_ATTRIBUTE_ARCHIVE, NULL);

    if (fHandle != INVALID_HANDLE_VALUE)
    {
        if (DosDateTimeToFileTime(((struct DosDate *) &d)->hi,
                                  ((struct DosDate *) &d)->lo, &LocalFT)
                && LocalFileTimeToFileTime(&LocalFT, &FT))
            SetFileTime(fHandle, NULL, NULL, &FT);

        CloseHandle(fHandle);
    }
}

/*********************************/
/* Function NtfsFileTime2utime() */
/*********************************/

/* scale factor and offset for conversion time_t -> FILETIME */
#define NT_QUANTA_PER_UNIX 10000000L
#define UNIX_TIME_ZERO_HI  0x019DB1DEUL
#define UNIX_TIME_ZERO_LO  0xD53E8000UL

// /* nonzero if `y' is a leap year, else zero */
//#define leap(y) (((y)%4 == 0 && (y)%100 != 0) || (y)%400 == 0)
// /* number of leap years from 1970 to `y' (not including `y' itself) */
//#define nleap(y) (((y)-1969)/4 - ((y)-1901)/100 + ((y)-1601)/400)

int NtfsFileTime2utime(const FILETIME *pft, time_t *ut)
{
	unsigned __int64 NTtime;

	NTtime = ((unsigned __int64)pft->dwLowDateTime +
			  ((unsigned __int64)pft->dwHighDateTime << 32));

	NTtime -= ((unsigned __int64)UNIX_TIME_ZERO_LO +
			   ((unsigned __int64)UNIX_TIME_ZERO_HI << 32));
    *ut = (time_t)(NTtime / (unsigned long)NT_QUANTA_PER_UNIX);
    return TRUE;
} /* end function NtfsFileTime2utime() */


// If file *f does not exist, return 0. Else, return the file's last
//   modified date and time as an MSDOS date and time. The date and time is
//   returned in a long with the date most significant to allow unsigned
//   integer comparison of absolute times. Also, if a is not a NULL pointer,
//   store the file attributes there, with the high two bytes being the Unix
//   attributes, and the low byte being a mapping of that to DOS attributes.
//   If n is not NULL, store the file size there.
//     If f is "-", use standard input as the file.
//     If f is a device, return a file size of -1
//     If f is a Volume Name, return a file size of -2
//   If t is not NULL, the file's access and modification time are stored there
//    as UNIX time_t values.
//   f :: Name of file to get info on.
//   a :: Return value: file attributes.
//   n :: Return value: file size.
//   t :: Return value: access and modification time.
extern BOOL unixtofile(time_t tim, FILETIME *ftp);

//ulg ZipFunc::zfiletime(const DZStrW& f, ulg *a, __int64 *n, ztimbuf *t)
BOOL ZipFunc::zfiletime(const DZStrW &name, ulg *attr, __int64 *size,
	ulg *stamp)
{
	WIN32_FIND_DATA ff;
	HANDLE hfind; /* file handle */
	// ztimbuf  t;
	// if (f == flabel)
//	if (name.Compare(flabel.c_str()) == 0)
	if (fvolume_label == 2 && name.Compare(flabel.c_str()) == 0)
	{
		if (attr != NULL)
			*attr = flabel_mode;

		if (size != NULL)
			*size = ZFT_LABEL; // convention for a label name

		// if (t != NULL)
		// t->actime = t->modtime = flabel_utim;

		return (BOOL)flabel_time;
	}

	int drv = Is_Drv(name);

	if (drv < 0)
	{
		// stream
		ZSData.BufP = 0;
		ZSData.Number = (-drv) - 2; // stream number
		ZSData.OpCode = zsaIdentify;
		int ok = StreamCB() == CALLBACK_TRUE;

		if (ok)
		{
			if (size)
				* size = ZSData.ArgLL;

			if (attr != NULL)
			{
				DWORD dwAttr = ZSData.ArgA;
				*attr = (dwAttr & FILE_ATTRIBUTE_READONLY ? A_RONLY : 0) |
					(dwAttr & FILE_ATTRIBUTE_HIDDEN ? A_HIDDEN : 0) |
					(dwAttr & FILE_ATTRIBUTE_SYSTEM ? A_SYSTEM : 0) |
					(dwAttr & FILE_ATTRIBUTE_DIRECTORY ? A_DIR : 0) |
					(dwAttr & FILE_ATTRIBUTE_ARCHIVE ? A_ARCHIVE : 0);
			}
			if (stamp)
			{
				*stamp = ZSData.ArgD;
			}
			return true; // ZSData.ArgD;
		}

		return 0;
	}

	DZStrW fullname = drv ? name : GetFullPath(name);

	// not all systems allow stat'ing a file with / appended, so remove it
	if (fullname.LastChar() == BSLASH)
		fullname = fullname.Left(fullname.length() - 1);

	// Accept about any kind of file including directories
	if ((hfind = FindFirstFile(fullname.c_str(), &ff)) == INVALID_HANDLE_VALUE)
	{
		if (Verbose < 0)
			Notify(ITRACE, _T("Could not find %s"), fullname.c_str());

		return false; // 0;                 // error in stat!
	}

	FindClose(hfind);
	// found
	if (size != NULL)
	{
		*size = ff.nFileSizeLow | ((__int64)ff.nFileSizeHigh << 32);
	}

	if (attr != NULL)
	{
		DWORD dwAttr = ff.dwFileAttributes;
		*attr = (dwAttr & FILE_ATTRIBUTE_READONLY ? A_RONLY : 0) |
			(dwAttr & FILE_ATTRIBUTE_HIDDEN ? A_HIDDEN : 0) |
			(dwAttr & FILE_ATTRIBUTE_SYSTEM ? A_SYSTEM : 0) |
			(dwAttr & FILE_ATTRIBUTE_DIRECTORY ? A_DIR : 0) |
			(dwAttr & FILE_ATTRIBUTE_ARCHIVE ? A_ARCHIVE : 0);
	}

	// if (t != NULL)
	if (stamp != NULL)
	{
		// NtfsFileTime2utime(&ff.ftLastAccessTime, t.actime);
		// NtfsFileTime2utime(&ff.ftLastWriteTime, t.modtime);
		*stamp = 0;// dostime(1980, 1, 1, 12, 0, 0);
		_FILETIME local;
		if (FileTimeToLocalFileTime(&ff.ftLastWriteTime, &local))
		{
			_SYSTEMTIME stime;
			if (FileTimeToSystemTime(&local, &stime))
			{
				if (stime.wYear >= 1980 && stime.wYear < 2108)
				{
					*stamp = dostime(stime.wYear, stime.wMonth, stime.wDay,
						stime.wHour, stime.wMinute, stime.wSecond);
				}
				else
				if (Verbose)
				{
					Notify(IWARNING,
						_T("%s Invalid File date: %i-%i-%i:%i:%i:%i"),
						fullname.c_str(), stime.wYear, stime.wMonth, stime.wDay,
						stime.wHour, stime.wMinute, stime.wSecond);
				}
			}
		}
		if (!*stamp)
		{
			*stamp = dostime(1980, 1, 1, 12, 0, 0); // reasonable default
		}
	}

	return true; // FileTime2DosTime1(ff.ftLastWriteTime);
}

//ulg FileTime2DosTime(_FILETIME ftime)
//{
//    _FILETIME local;
//    WORD dt, tm;
//
//    if (FileTimeToLocalFileTime(&ftime, &local) &&
//            FileTimeToDosDateTime(&local, &dt, &tm))
//		return (dt << 16) | tm;
////      Notify(0, "zip diagnostic: GetFileAttributes failed");
//    return 0;
//}



//#define EAID  0x0009

// NOTE: NTFS is handled by the HPFS code. FAT / HPFS detection.
int ZipOp::IsFileSystemOldFAT(const DZStrW &dir)
{
    DWORD vfnsize;
    DWORD vfsflags;

    if (Verbose < 0)
        Notify(ITRACE, _T("IsFileSyatemOldFAT called for %s"), dir.c_str());

    // We separate FAT and HPFS+other file systems here. I consider other
    //   systems to be similar to HPFS/NTFS, i.e. support for long file names and
    //   being case sensitive to some extent.
    DZStrW fp = dir;

    if (fp.IsEmpty() || Is_DrvEx(fp) <= 0)
    {
        fp = fRootDir;   // use current root

        if (fp.IsEmpty() || Is_Drv(fp) <= 0)
            return false; // assume ntfs
    }

    int dt = Is_DrvEx(fp);

    if (dt < 0)
        return false;

    if (dt == 1)
    {
        fp = fp.Left(1);
        fp.ToUpper();
        TCHAR r = fp[0];

		if (r == fDrv[0])
            return fDrv[1] != 0;  // use cached result

        fp += _T(":\\");
    }
    else
    {
        if (dt != 2)
            return false;

        // specify \\MyServer\MyShare as \\MyServer\MyShare\.
		fp = fp.Left((unsigned)DriveLen(fp));
    }

	if (!GetVolumeInformation(fp.c_str(), NULL, 0, NULL, &vfnsize, &vfsflags, NULL, 0))
    {
        if (Verbose)
		{
			Notify(IVERBOSE, _T(" GetVolumeInformation failed for %s [%s]"),
				   fp.c_str(), SysMsg().c_str());
        }

        return (false);    // assume NTFS
    }

    if (Verbose)
        Notify(IVERBOSE, _T("Max component size for %s = %d"), fp.c_str(), vfnsize);

    if (_istascii(fp[0]))
    {
        fDrv[0] = fp[0];
		fDrv[1] = (TCHAR)(vfnsize <= 12 ? 1 : 0);
    }

    return vfnsize <= 12;
}

// access mode bits and time stamp.
int ZipOp::GetFileMode(const DZStrW& name)
{
    DWORD dwAttr;

	dwAttr = GetFileAttributes(GetFullPath(name).c_str());   // v1.6017

    if (dwAttr == 0xFFFFFFFF)
    {
        // RCV Changed: was ...= -1
        Notify(0, _T("zip diagnostic: GetFileAttributes failed"));

        // DLLprintf ("zip diagnostic: GetFileAttributes failed\n");
        return (0x20);       // the most likely, though why the error? security?
    }

    return
        (
            (dwAttr & FILE_ATTRIBUTE_READONLY ? A_RONLY : 0) |
            (dwAttr & FILE_ATTRIBUTE_HIDDEN ? A_HIDDEN : 0) |
            (dwAttr & FILE_ATTRIBUTE_SYSTEM ? A_SYSTEM : 0) |
            (dwAttr & FILE_ATTRIBUTE_DIRECTORY ? A_DIR : 0) |
            (dwAttr & FILE_ATTRIBUTE_ARCHIVE ? A_ARCHIVE : 0)
        );
}

const char *GetLongPathEA(void)
{
    return (NULL);                // volunteers ?
}

// If a volume label exists for the given drive, return its name and pretend
//   to set its time and mode. The returned name is global data. Drive = drive
//   name: 'A' .. 'Z' or '\0' for current drive
DZStrW ZipOp::getVolumeLabel(const DZStrW &drv, ulg *vtime, ulg *vmode, time_t *vutim)
{
	DZStrW Root(drv.c_str(), 2);
    ulg   fnlen,
    flags;
	UINT  OldMode = SetErrorMode(SEM_FAILCRITICALERRORS | SEM_NOGPFAULTERRORBOX);
    BOOL  Result;

	*vmode = A_ARCHIVE | A_LABEL; // this is what msdos returns
	*vtime = dostime(1980, 1, 1, 0, 0, 0);    // no true date info available
    *vutim = dos2unixtime(*vtime);

	DZStrW vol;

    if (Is_Drv(drv) != 1)
        Root.Empty();
    else
        Root += BSLASH;

    Result = GetVolumeInformation(Root.c_str(), vol.GetBuffer(MAX_PATH), MAX_PATH - 1,
								  NULL, &fnlen, &flags, NULL, 0);
    SetErrorMode(OldMode);
    vol.ReleaseBuffer();

    if (!Result)
        vol.Empty();

    return vol;//Result ? fvol : NULL;
}





