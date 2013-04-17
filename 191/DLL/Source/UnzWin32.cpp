#include "stdafx.h"
#pragma hdrstop
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZWIN32_CPP
/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.

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

/* WARNING: Don't pull any OS/2 or HPFS code without great care.  Much of
 * it is also used for NTFS, although it doesn't always have comments to
 * that effect. */

// MBCS update 17 Dec 2006
/*---------------------------------------------------------------------------
 * win32.c
 * 32-bit Windows-specific (NT/95) routines for use with Info-ZIP's UnZip 5.2
 * and later.  (Borrowed, pilfered and plundered code from OS/2 and MS-DOS
 * versions and from ZIP; modified as necessary.)
 * Contains:  GetLoadPath()
 *            Opendir()
 *            Readdir()
 *            Closedir()
 *            mapattr()
 *            getNTfiletime()
 *            close_outfile()
 *            isfloppy()
 *            IsVolumeOldFAT()   RCV Removed.
 *            IsFileNameValid()
 *            map2fat()
 *            checkdir()
 *            do_wild()
 *            mapname()
 *            version()
 *---------------------------------------------------------------------------*/
//#include "unzip.h"
#include "UnzOp.h"
//#include <windows.h>            /* must be AFTER unzip.h to avoid struct G problems */
#include <shlobj.h>
#include <direct.h>

/* ===========================================================================
 *                 Function mapattr()
 * Identical to MS-DOS, OS/2 versions.
 * However, NT has a lot of extra permission stuff, so this function should
 *  probably be extended in the future.
 */
int UnzOpr::mapattr(void)
{
    /* Set archive bit (file is not backed up): */
    fpInfo->file_attr =
        (unsigned)(fcrec.external_file_attributes | 32)  & 0xff;
    return 0;
}                               /* end function mapattr() */


/* ===========================================================================
 *             Function getNTfiletime()
 * Get the file time in a format that can be used by SetFileTime() in NT.
 */
int UnzOpr::getNTfiletime(FILETIME * ft)
{
    FILETIME lft;                 /* 64-bit value made up of two 32 bit [low & high] */
    WORD wDOSDate;                /* for converting from DOS date to Windows NT      */
	WORD wDOSTime;

    wDOSTime = (WORD)  flrec.last_mod_file_time;
    wDOSDate = (WORD)  flrec.last_mod_file_date;

    /* The DosDateTimeToFileTime() function converts a DOS date/time    */
    /* into a 64 bit Windows NT file time                               */
    if (!DosDateTimeToFileTime(wDOSDate, wDOSTime, &lft))
	{
        Notify(0,  _T("DosDateTime failed: %s"), SysMsg().c_str());
        return false;
    }

    if (!LocalFileTimeToFileTime(&lft, ft))
	{
        Notify(0,  _T("LocalFileTime failed: %s"), SysMsg().c_str());
        *ft = lft;
    }
    return true;
}


/* ===========================================================================
 *        Function close_outfile()
 */
void UnzOpr::close_outfile(void)
{
    FILETIME ft;                  /* File time type defined in NT */
    int gotTime;

	if (!fUnzOutfile)// || !fUnzOutfile->IsFile)
		return;
	bool WasStream = !fUnzOutfile->IsFile;

    /* don't set the time stamp on standard output */
    if (fUnzOutfile)
    {
        ZStreamIO *tmp = fUnzOutfile;
        fUnzOutfile = NULL;
        delete tmp;
	}

	if (WasStream)
		return;

    gotTime = getNTfiletime(&ft);

    /* Close the file and then re-open it using the Win32
     * CreateFile call, so that the file can be created
     * with GENERIC_WRITE access, otherwise the SetFileTime
     * call will fail. */
    if (gotTime || fpInfo->ntfs_data)
    {
        if (Verbose < 0)
            Notify(ITRACE, _T("setting date/time in close_outfile"));

        ZFile tmpz(this, ffilename, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
                                   FILE_ATTRIBUTE_NORMAL);
        if (!tmpz.IsOpen())
            Notify(0, _T("CreateFile error [%s] when trying set filetime"), SysMsg().c_str());
        else
        {
            XNTFSData times;
            memset(&times, 0, sizeof(XNTFSData));
            times.MTime = ft;
            if (fpInfo->ntfs_data)
                memcpy(&times, fpInfo->ntfs_data, sizeof(XNTFSData));
            if (!tmpz.SetTime(&times.CTime, &times.ATime, &times.MTime))// &ft))
                Notify(0,  _T("SetFileTime failed: %s"), SysMsg().c_str());
        }

    }

    /* HG: I think this could be done in the CreateFile call above - just  */
    /*     replace 'FILE_ATTRIBUTE_NORMAL' with 'fpInfo->file_attr & 0x7F'  */
	if (!SetFileAttributes(ffilename.c_str(), fpInfo->file_attr & 0x7F))
        Notify(IWARNING, _T("Could not set file attributes [%s]"), SysMsg().c_str());
    else                                                
#if defined(UNICODE) && defined(ALLOW_WIN98)
        if (IsNTorAbove)
            SHChangeNotify(SHCNE_ATTRIBUTES, SHCNF_PATH,
				fpInfo->spec->Base->FullPath(ffilename).c_str(), NULL);
        else
        {
            DZStrA atmp(fpInfo->spec->Base->FullPath(ffilename));
            SHChangeNotify(SHCNE_ATTRIBUTES, SHCNF_PATH, atmp, NULL);
        }
#else
        SHChangeNotify(SHCNE_ATTRIBUTES, SHCNF_PATH,
			fpInfo->spec->Base->FullPath(ffilename).c_str(), NULL);
#endif

    return;
}                               /* end function close_outfile() */


/* ===========================================================================
 *                    Function isfloppy()
 * more precisely, is it removable?
 */
static int isfloppy(int nDrive)
{
    /* 1 == A:, 2 == B:, etc. */
    DZStrW root;
    TCHAR c = (TCHAR)(_T('A') + nDrive - 1);    /* build the root path */
    root += c;
    root += _T(":\\");      
	return(GetDriveType(root.c_str())  == DRIVE_REMOVABLE);
}                               /* end function isfloppy() */

/* ===========================================================================
 *             Function mapname()
 * There are presently two possibilities in OS/2:  the output filesystem is
 * FAT, or it is HPFS.  If the former, we need to map to FAT, obviously, but
 * we *also* must map to HPFS and store that version of the name in extended
 * attributes.  Either way, we need to map to HPFS, so the main mapname
 * routine does that.  In the case that the output file system is FAT, an
 * extra filename-mapping routine is called in checkdir().  While it should
 * be possible to determine the filesystem immediately upon entry to mapname(),
 * it is conceivable that the DOS APPEND utility could be added to OS/2 some-
 * day, allowing a FAT directory to be APPENDed to an HPFS drive/path.  There-
 * fore we simply check the filesystem at each path component.
 *
 * Note that when alternative IFS's become available/popular, everything will
 * become immensely more complicated.  For example, a Minix filesystem would
 * have limited filename lengths like FAT but no extended attributes in which
 * to store the longer versions of the names.  A BSD Unix filesystem would
 * support paths of length 1024 bytes or more, but it is not clear that FAT
 * EAs would allow such long .LONGNAME fields or that OS/2 would properly
 * restore such fields when moving files from FAT to the new filesystem.
 *
 * GRR:  some or all of the following chars should be checked in either
 *       mapname (HPFS) or map2fat (FAT), depending:  ,=^+'"[]<>|\t&
 *
 * return 0 if no error, 1 if caution (filename trunc),
        renamed :: 2 if warning (skip file because dir doesn't exist),
                   3 if error (skip file), 10 if no memory (skip file),
                   IZ_VOL_LABEL if can't do vol label, IZ_CREATED_DIR
 */
int UnzOpr::mapname(int renamed)
{
	DZStrW build;
    int fposn;          // possition in ffilename

    /*---------------------------------------------------------------------------
     *    Initialize various pointers and counters and stuff.
     *---------------------------------------------------------------------------*/
    /* can create path as long as not just freshening, or if user told us */
    fcreate_dirs = (!ffflag || renamed);

	fcreated_dir = false;      /* not yet */
	bool isAbs = false;
    fposn = 0;

    if (renamed)
    {
        if (fpInfo->vollabel)
        {
			int drv = 0;
			/* use root or renamed path, but don't store */
			if (ffilename[1]  == _T(':'))
            {
                TCHAR d = (TCHAR) _totlower(ffilename[0]);
                drv = d - (_T('a') + 1); 
            }
            else
			{
                int l = (int)GetFullPathName(_T("."), MAX_PATH, build.GetBuffer(MAX_PATH), NULL);
                if (l)
                {
                    TCHAR d = (TCHAR) _totlower(ffilename[0]);
                    drv = d - (_T('a') + 1);
				}
            }
            if (fvolflag == 0 || drv < 0 || drv > 25  /* no labels/bogus? */
                    || (fvolflag == 1 && !isfloppy(drv)))
            {
                /* !fixed */
                return IZ_VOL_LABEL;    /* skipping with message */
            }
			fnLabelDrive = (unsigned)drv;
        }
        else
		{
            fposn = DriveLen(ffilename);
            if (fposn < 0)
                return 4;   // invalid renamed stream
            if (fposn > 0)
            {
//                if (ffilename[fposn] == BSLASH)
//					fposn++;
//                build = ffilename.Left(fposn);
				isAbs = true;  // is absolute path
				fposn = 0;		// want all of it
            }
            else
                if (ffilename[0] == BSLASH)
                {
					ffilename = ffilename.Mid(1);
				}
        }
    }

	/* pathcomp is ignored unless renamed is true: */
    if (!renamed)
    {
        /* cp already set if renamed */
        if (fjflag)               /* junking directories */
        {
			fposn = ffilename.ReverseFind(BSLASH);
        if (fposn < 0)
            fposn = 0;
        else
            fposn++;
		}
	}
	if (fposn > 0)
		ffilename = ffilename.Mid((unsigned)fposn);
	DZStrW ftemp;
	int cperr = CleanPath(ffilename, ftemp);
	if (cperr != 0)
	{
		Notify(IWARNING, _T("mapname: rejecting invalid filename: %s [err:%d]"), ffilename.c_str(), cperr);
		return DZ_ERM_INVAL_NAME;
	}
//	if (isAbs)
//		ffilename = build + ftemp;
////		ffilename = build = ftemp;
//	else
//		ffilename = fpInfo->spec->Base->FullPath(ftemp);
	if (isAbs)
		ffilename = ftemp;
	else
		ffilename = fpInfo->spec->Base->FullPath(ftemp);

	DZStrW dir = ExtractFilePath(ffilename);
	// no such dir - check filename
	if (!DirExists(dir))
	{
		/* path doesn't exist   // v1.6025 */
		if (!fcreate_dirs)
		{
			/* told not to create (freshening) */
			return IZ_SKIP_DIR;               // path doesn't exist:  nothing to do
		}

		if (!ForceDirectories(dir, -1))
		{
			/* create the directory   // v1.6025 */
			Notify(0, _T("can't create %s [%s], unable to process %s."),
				dir.c_str(), SysMsg().c_str(), ffilename.c_str());
			return DZ_ERM_ERROR_CREATE;     // path didn't exist, tried to create, failed
		}
		fcreated_dir = true;
	}

	 if (ffilename.LastChar() == BSLASH)
	 {
		if (fcreated_dir)
		{
//          Notify(0,  _T("   created: %s"), ffilename);
          /* HG: are we setting the date & time on a newly created   */
          /*     dir?  Not quite sure how to do this.  It does not   */
          /*     seem to be done in the MS-DOS version of mapname(). */
          return IZ_CREATED_DIR;    /* dir time already set */
        }
		return IZ_SKIP_DIR; /* dir existed already; don't look for data to extract */

     }
     return 0;
}

