#include "stdafx.h"
#pragma hdrstop

#include "ZipOp.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPPRC_CPP

/* DLLzip.c * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
 ** distributed under LGPL license

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
************************************************************************
*/

#include <process.h>
#include <signal.h>
#include <errno.h>
#include <stdlib.h>
#include <io.h>

// Local option flags
#define PURGE   0 // RCV Changed: was DELETE. (delete is also a function)
#define ADD     1
#define UPDATE  2
#define FRESHEN 3

#define MARK_PURGE      -1
#define MARK_KEEP       0
#define MARK_UPDATE     1
//#define MARK_RENAMED    2
//#define MARK_VERSION    3
#define MARK_NEW        4

#ifndef UNICODE
#define __access _access
#endif

// Process -o and -m options (if specified), free up malloc'ed stuff, and
//   exit with the code e. e :: Exit code.
void ZipFunc::finish(void)
{
    int r; // return value from trash()
    ulg t; // latest time in zip file

	ZipItem *z; // pointer into zfile list

    if (flatest && fzipfile.Compare(_T("-")))
    {
        diag(_T("changing time of zip file to time of latest file in it"));

        // find latest time in zip file
        if (fzfiles == NULL)
        {
            Notify(IWARNING,
                   _T("zip file is empty, can't make it as old as latest entry"));
        }
        else
        {
            t = 0;

			for (z = fzfiles; z != NULL; z = z->nxt) // Ignore directories in time comparisons
			if (!z->GetIsFolder() && t < z->tim)
				t = z->tim;
            // set modified time of zip file to that time
            if (t != 0)
                stamp(fzipfile, t);
            else
                Notify(IWARNING,
                       _T("zip file has only directories, can't make it as old as latest entry"));
        }
    }

    // If dispose, delete all files in the zfiles list that are marked
    if (fdispose && !Abort_Flag)
    {
        // v1.6017
        diag(_T("deleting files that were added to zip file"));

        if ((r = trash()) != DZ_ERR_GOOD)
            DZError(r);
    }
}

void ZipFunc::ZipCleanup(void)
{
    delete [] fhwbuf;
    fhwbuf = NULL;

//	if (ftempzip && (ftempzip != fzipfile))
	if (!ftempzip.IsEmpty() && (ftempzip.Compare(fzipfile.c_str())))
    {
        // using temp file
        if (fZipOutfile)
        {
            delete fZipOutfile;
            fZipOutfile = NULL;
        }

        // remove bad file - if it exists
        EraseFile(ftempzip, false);
    }

    Close_Handle(&fhInz);

    if (fZipOutfile)
    {
        bool wasopen =  fZipOutfile->IsOpen() && fZipOutfile->IsFile;
        delete fZipOutfile;
        fZipOutfile = NULL;
        // remove damaged file

        if (wasopen)
        {
            if (Verbose)
                Notify(IVERBOSE, _T("deleting damaged %s"), fzipfile.c_str());

            EraseFile(fzipfile, false);
        }
    }

    ftempzip = "";
}

static int DriveType(const DZStrW& pth)
{
    TCHAR root[3];
    root[0] = pth.IsEmpty() ? (char)0 : pth[0];
    root[1] = ':';
    root[2] = 0;

    if (root[0] == BSLASH)
        root[1] = 0;

	return (int)GetDriveType(root);
}
 /*
const char hx[] = "0123456789ABCDEF";
char hxbuf[16];
static char* toHex(unsigned val)
{
    char* p = &hxbuf[15];
    *p = 0;
    while (val)
    {
        *--p = hx[val & 0x0f];
        val >>= 4;
    }
    return p;
}
*/

int __fastcall ZipFunc::NameVer(ZipItem* z)
{
	int r;
	DZStrW name = z->GetIName();
	int i = name.ReverseFind(_T('.'));

    if (!VerDate)
    {
        FILETIME now;
        WORD d, t;
        GetSystemTimeAsFileTime(&now);

        if (FileTimeToDosDateTime(&now, &d, &t))
			VerDate = (ulg)(d << 16) | t;
        else
            VerDate = z->tim;
    }

    DZStrW nx, nn(name);
    nx = _T("}");

	if (i >= 0)
    {
		nn = name.Left((unsigned)i);
		nx += name.Mid((unsigned)i);
    }

    i = -1;

    nn.AppendFormat(_T(".{%X"), VerDate);
    name = nn;
	name += nx;
	DZStrA hn = StrIntSep(name);

	const XItem* n;
	while ((n = IntList->FindName(name)) != NULL)
	{
		// exists
		if (++i == MAXINT)
			return -2;  // give up

		name.Format(_T("%s-%X%s"), nn.c_str(), i, nx.c_str());
	}
	hn = StrIntSep(name);
	if (Verbose < 0)
		Notify(IVERBOSE, _T("%s versioned as %s"), z->Getiname(), name.c_str());
	z->SetIName(name);
	z->SetHName(hn);
	// prepare header name
	if ((r = PrepareHeaderName(z, true)) != 0)
		return r;

    if (!n)
        n = IntList->AddNode(z);

    return n ? -1 : 0;
}

void ZipFunc::DupName(bool fatal, const XItem* o, const XItem* n, const DZStrW name)
{
    // duplicate found
	Notify(IWARNING, _T("internal name in zip file repeated: %s"), name.c_str());
    Notify(IWARNING, _T("  first full name: %s"), o->xname);
	Notify(IWARNING, _T(" second full name: %s"), n->xname);

    if (fatal)
		throw DZException(DZ_ERM_DUPNAME);
	Notify(IWARNING, _T(" rejecting: %s"), n->xname);
}

int __fastcall ZipFunc::PrepareHeaderName(ZipItem* z, bool NoComment)
{
	if (!NoComment)
    {
//        // free any old data - probably obsolete
//        z->cextra.Empty();
//        z->extra.Empty();
        if (z->ntfs)
        {
            delete z->ntfs;
            z->ntfs = NULL;
        }

        // get or change the comment
		CB->SetMsg2(z->GetComment());
		if (CB->UserCB(zacComment, z->GetIName()) == CALLBACK_TRUE)
        {
            // User changed the comment
            z->GetComment().Empty();
			z->com = (extent)CB->GetArg1();

            if (z->com)
            {
				DZStrW tc(CB->GetMsg().c_str(), (int)z->com);
                z->SetComment(tc);
            }
        }
    }
	// ****** set header name and encoding
	z->options.needutf8 = 0;
	z->options.nameextd = z->GetIName().BadDOS();
	z->options.cmntextd = z->com && z->GetComment().BadDOS();
	z->options.namenew = 0;

	DZStrW iiname = StrIntSep(z->GetIName());
	bool wantOEM = true;
	int bad = 0;
	if (!z->options.nameextd && !z->options.cmntextd)
	{
		// mimic WinZip
		z->SetEnc(zeoOEM);
	}
	else
	{
        unsigned doenc = fEncodeAs;
        if (doenc == zeoAuto)
        {
			doenc = zeoUPATH;
			if (z->options.nameextd && z->options.cmntextd)
				doenc = zeoUTF8; // recommended
        }

		z->SetEnc((int)doenc);
		if (doenc == zeoUPATH)
		{
			z->SetHName(iiname.SafeNarrow(CP_OEM)); // default oem name
			z->options.dosflag = 2;  		// FAT
			wantOEM = false;
		}
		else
		if (doenc == zeoUTF8)
		{
//			z->HName = StrToUTF8(iiname);
			z->SetHName(DZStrA(iiname, CP_UTF8));
			z->options.nameutf8 = 1;   // so we set the flag
			z->options.dosflag = 2;  // assume header strings usable on MSDOS
			wantOEM = false;
		}
		else
		if (doenc == zeoNone)
		{
			z->SetHName(iiname.SafeNarrow(CP_ACP, bad));
			z->options.nameutf8 = 0;   // so we set the flag
			bool ntfs = z->GetHName().BadDOS();
			if (!ntfs && z->options.cmntextd)
			{
				DZStrA tmp = z->GetComment().SafeNarrow(CP_ACP);
				ntfs = tmp.BadDOS();
			}
			z->options.dosflag = ntfs ? 0 : 1;  // assume header strings usable on NTFS
			wantOEM = false;
		}
	}
	if (wantOEM)
	{
		z->SetHName(iiname.SafeNarrow(CP_OEM, bad)); // default oem name
		z->options.dosflag = 2;  		// FAT
	}
	if (bad)
	{
		z->options.namenew = 1; // name was made safe
		unsigned cp = wantOEM ? CP_OEM : CP_ACP;
		DZStrW xname(cp, z->Gethname(), (int)z->GetHName().length());
		if (Verbose < 0)
			Notify(IVERBOSE, _T("%s made safe as %hs"),	iiname.c_str(), xname.c_str());
	}
#ifdef ZDEBUG
	if (Verbose < 0)
	{
		Notify(ITRACE, _T("Prepare %s need %x dos %X"), z->iname, z->ver,
			   z->options.dosflag);
	}
#endif

    return 0;
}

// Add, update, freshen, or delete zip entries in a zip file. argc; /*
//   Number of tokens in command line. argv; /* Command line tokens.
int ZipFunc::ZipProcess(void)
{
    int a;                // attributes of zip file
    ZInt64 censtt;        // start of central directory

    FndItem *f;           // steps through "found" linked list
    FndItem *tempf;
    int k;                // next argument type, marked counter, comment size, entry count
    int marks;            // replaces k as marked counter
	int r;                // temporary variable
	int err;			  // temporary error variable
	ulg t = 0;            // file time, length of central directory

	ZipItem *v;           // temporary variable
    ZipItem *z;           // steps through "zfiles" linked list
    const TCHAR *Actions[4] = {_T("PURGE"), _T("ADD"), _T("UPDATE"), _T("FRESHEN")};

    int DestType;         //  destination drive type
    unsigned long TotFiles = 0;
    __int64 TotSize = 0;
	unsigned long KeptCnt = 0;  // number of 'kept' files
	__int64 KeptSize = 0;       // size of 'kept' files
    __int64 VerSize = 0;        // size of 'kept' files
	__int64 fsz;                // file size;
    int No_File;                // 1.75 try if file does not exist
    // Process arguments
    diag(_T("processing lists"));

	if (Verbose)
	{
		Notify(IVERBOSE, _T("action = %s"), Actions[faction]);

        // zcount is no. of entries in linked-list
        // zfiles is name of the linked-list of filenames for the archive
        Notify(IVERBOSE, _T("zcount=%d (no. of files in ZIP already)"), fzcount);
    }

    if (!fzipbeg)
        fjunk_sfx = 0;   // nothing to junk

    if (!fzfiles)
        fadjust = 0;     // nothing to adjust


    if ((r = check_dupExt()) != DZ_ERR_GOOD)   // remove duplicates in ffound
		return DZError(r);

    // Check option combinations
    // ?????
    if (faction == PURGE && (fdispose || frecurse || fkey))
		return DZError(DZ_ERM_BAD_OPTIONS);

    // AllowGrow is the "allow append" indicator
    if (!fzcount && ((faction == ADD) || (faction == UPDATE)))
        fAllowGrow = 55; // create new file normally

    // if zcount is 0, then zipfile doesn't exist, or is empty
    if (fzcount == 0 && ((faction != ADD && faction != UPDATE)
                         || !fAllowGrow))
    {
        // RCV150199 added UPDATE
        Notify(IWARNING, _T("%s: not found or empty"), fzipfile.c_str());
        return 0;
    }

    if (fndlist)
    {
        delete fndlist;
        fndlist = NULL;
    }

    DestType = DriveType(fzipfile);

    if (Verbose < 0)
        Notify(IVERBOSE, _T("Destination type = %d"), DestType);

    // RP - check destination type - if CD set tempath to Windows Temp
    if (ftempath.IsEmpty() && (DestType != DRIVE_FIXED &&
                               DestType != DRIVE_RAMDISK))
    {
        GetTempPath(2047, ftempath.GetBuffer(2047));
        ftempath.ReleaseBuffer();
    }

    // If -b not specified, set temporary path to zipfile path
	int pp;

    if (ftempath.IsEmpty() && ((pp = fzipfile.ReverseFind(BSLASH)) >= 0
                               || (pp = fzipfile.ReverseFind(_T(':'))) >= 0))
    {
		if (fzipfile[(unsigned)pp] == _T(':'))
            pp++;

		ftempath = fzipfile.Left((unsigned)pp);
    }

    // if first_listarg is 0, then we didn't got any fspecs on cmd line
    if (fdoall && (faction == UPDATE || faction == FRESHEN))
    {
        // if -update or -freshen with no args, do all, but, when present, apply
        //   filters
        for (z = fzfiles; z != NULL; z = z->nxt)
			z->mark = fpcount ? !ZMatch(fExcludes, z->GetIName()) : MARK_UPDATE;
    }

    // NOTE: "k" is being redefined below this point. Now, it going to
    // track the no. of marked files in the "zfiles" linked list.
    // For each marked entry in "zfiles" linked list, if not deleting, check
    //   if a corresponding "external" file exists. If updating or freshening,
    //   compare date of "external" file with entry in orig zipfile. Unmark if it
    //   external file doesn't exist or is too old, else mark it. Entries that
    //   are marked will cause that file to be rezipped.
    diag(_T("checking marked entries"));
    marks = 0; // Initialize marked count
    ZipItem **zlast;          // pointer to last link in "zfiles" list
    ZipItem **verlast = &VerFiles;   // pointer to last link
	ZipItem **fndlast = &fzfound;
	int vercount = 0;
		for (z = fzfiles; z != NULL; z = z->nxt)
		{
			if (z->mark)
			{
				ulg FileAttr;
				if (faction != PURGE)
				{
//					t = zfiletime(z->FullPath(), &FileAttr, &fsz, NULL);
					BOOL ok = zfiletime(z->FullPath(), &FileAttr, &fsz, &t);

//                    if ((t == 0 || t < fbefore
					if ((!ok || t < fbefore
                            || ((faction == UPDATE || faction == FRESHEN) &&
                                t <= z->tim)
                            || (fArchiveFiles && faction == FRESHEN &&
                                !(FileAttr &A_ARCHIVE)))
                       )
                    {
                        z->mark = MARK_KEEP;      // keep
                        z->trash = (t && t >= fbefore);
                        // delete if -um or -fm

                        if (Verbose)
                        {
                            const TCHAR *expl;

                            if (t)
                                expl = z->trash ?  _T("up to date") : _T("early");
                            else
                                expl = _T("missing");

							Notify(0, _T("%s is %s"), z->xname, expl);
                        }
                    }
                    else
                    {
						// replace
                        TotSize += fsz;
                        TotFiles++;
                        marks++;
                        z->mark = MARK_UPDATE;    // marker for replace

                        if (z->options.keepver)
                        {
							ZipItem* vz = new ZipItem(*z);
							*(verlast) = vz;
							verlast = &vz->nxt;
							vz->nxt = NULL;
							vz->Passw = fkey;
							vz->Base = CurBase->Base;
                            vercount++;
                        }
                    }
                }
                else
                {
                    // PURGE
                    marks++;    // incr. number of marked entries
					z->mark = MARK_PURGE;  // marker for Purge
				}
            }
		}
    // RP - verify file specified to 'Purge'
    if (faction == PURGE && !marks)
        return DZError(DZ_ERM_NOTHING_TO_DO);

    // Remove entries from "found" linked list if: Action is PURGE or FRESHEN
    //   or No "external" matching file is found, or if found, but is too old or
    //   The external file is equal to the ziparchive name while ziparchive name
    //   != "-" If filetime() returns any valid time, then at least we know the
    //   file was found.
    diag(_T("checking new entries"));
	FndItem *prev = NULL;
	// fileio.c built the found list
	for (f = ffound; f != NULL;)
	{
		if (faction == PURGE || faction == FRESHEN
				|| (!zfiletime(f->FullPath(), NULL, NULL, &t))
//				|| (!zfiletime(f->xname, NULL, NULL, &t))
				|| t < fbefore
				|| (ZMatch(f->FullPath(), fzipfile)))
		{
			if (Verbose && t < fbefore)
				Notify(IVERBOSE, _T("rejecting %s as too early"), f->xname);

			if (Verbose < 0)
				Notify(ITRACE, _T("expel being called for %s"), f->xname);
			tempf = f;
			f = f->nxt;
			if (prev)
				prev->nxt = f;
			else
				ffound = f;     // new first
			delete tempf;
			ffcount--;
		}
		else                 // file found, and not too old.
		{
			prev = f;
			f = f->nxt;         // save this one, link it up.
		}
	}
    if (Verbose)
    {
        if (ffound == NULL)
            Notify(IVERBOSE, _T("found list empty - a"));
        else
            Notify(IVERBOSE, _T("found list has at least one entry - a"));
    }
    // 'fix' safe and header names
	IntList = new HashListInt(ffcount + (int)fzcount + vercount);
	// add 'keep' files - no duplicate allowed but should not happen
	ZipItem  *prv = NULL;
	for (z = fzfiles; z != NULL; /* z = z->nxt */)
	{
		FndItem  *e;
		if (z->mark >= MARK_KEEP)
		{
			// add internal name to list - check for duplicate
			if ((e = (FndItem*) IntList->AddNode(z)) != 0)
			{
				DupName(Skipping(z->GetXName(), DZ_ERM_DUPNAME, SKIPPED_DUP_NAME),
						e, z, z->GetIName());
				// delete later duplicate
				ZipItem  *x = z->nxt;
				if (prv)
					prv->nxt = x;
				else
					fzfiles = x;
				delete z;
				z = x;
				fzcount--;
				continue;
			}
		}
		prv = z;
		z = z->nxt;
    }

	// now new files - no duplicates
    while ((f = ffound) != NULL)
	{
		ZipItem **prvlast = fndlast;      	// save
		ZipItem *prvlastnxt = *(fndlast);   // save
        ZipItem *zi = new ZipItem(f);       // copy as ZipItem
		*(fndlast) = zi;
        fndlast = &zi->nxt;
		zi->mark = MARK_NEW;
        ffound = f->nxt;
        delete f;

		// set comment and header name
		if ((r = PrepareHeaderName(zi, false)) != 0)
			return r;
		FndItem  *e;
		if ((e = (FndItem*) IntList->AddNode(zi)) != 0)
		{
			DupName(Skipping(zi->GetXName(), 0, SKIPPED_DUP_NAME),
					e, f, f->GetIName());
			// delete later duplicate
			fndlast = prvlast;     		// restore
			*(fndlast) = prvlastnxt;	// restore
			delete zi;
			ffcount--;
        }
	}
	// now rename versions to unique internal names
	if (vercount)
	{
		fAllowGrow = 0; // append not allowed
	}
	//
	if (fadjust <= 0)
        fadjust = 0;

    // Make sure there's something left to do
    if (marks == 0 && fzfound == NULL && !(fzfiles != NULL && (flatest ||
                                          fadjust || fjunk_sfx)))
    {
        // FOUND WAS NULL HERE, so just figure out which error message to show
        if (faction == UPDATE || faction == FRESHEN)
        {
            finish();
            Notify(IWARNING, _T("no files %s"), (faction == UPDATE) ? _T("updated")
                   : _T("freshened"));
            return 0;
        }
        else
        {
            if (fzfiles == NULL && (flatest || fadjust || fjunk_sfx))
                return DZError(DZ_ERM_EMPTY_ZIP);
            return DZError(DZ_ERM_NOTHING_TO_DO);
        }
    }

    // AllowGrow is false if writing temporary file
    fAllowGrow = (fAllowGrow && (marks == 0)
                  // is allowed and no changes to existing
                  && (fzipbeg || fzfiles != NULL) // something to append to
                 );

    // continue on to add new files
    a = 0;

    // ignore self-extracting code in front of the zip file (for -J)
    if (fjunk_sfx)
        fzipbeg = 0;

    // Calc size of version files
    for (z = VerFiles; z; z = z->nxt)
    {
        if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);

		VerSize += (z->siz + (ulg)(4 + LOCHEAD) + (ulg)z->nam + (ulg)z->GetExt());

        if (z->lflg &8)
            VerSize += 16;
    }

    // Count files and sizes which we have to Keep; RP Added
    zlast = &fzfiles;
    while ((z =  *zlast) != NULL)
    {
        if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);

        if (!z->mark)
        {
			KeptSize += (z->siz + (ulg)(4 + LOCHEAD) + (ulg)z->nam + (ulg)z->GetExt());

            if (z->lflg &8)
                KeptSize += 16;

            KeptCnt++;
        }

        zlast = &z->nxt;
    }

    //Inform(pG, 0, IDIAG, "Kept = %u %Lu Total = %Lu %Lu", KeptCnt, KeptSize, TotFiles, TotSize);
    // Count files and sizes which we have to process; RCV Added
    // First the files in the old zip file...
    // RP - already calculated with new sizes
    // And the found list...
    z = fzfound;
    while (z)
    {
		if (Abort_Flag)
            Fatal(DZ_ERM_ABORT, 0);

        TotSize += z->len;
        TotFiles++;
        z = z->nxt;
	}

    fhInz = INVALID_HANDLE_VALUE;
    fOutPosn = 0;

    if (!fAllowGrow)
    {
        // check file exists
		No_File = _taccess(fzipfile.c_str(), 0) && errno == ENOENT;

		if (No_File && (DestType == DRIVE_FIXED || DestType == DRIVE_RAMDISK))
        {
            // create file using given name
			diag(_T("Processing - ready to create new file"));
            fZipOutfile = new ZFile(this, fzipfile, GENERIC_WRITE, 0, NULL,
                                    CREATE_NEW, FILE_ATTRIBUTE_NORMAL);

            if (fZipOutfile->IsOpen())
            {
                ftempzip = fzipfile;
                fAllowGrow =  -1; // new files do grow
            }
            else
                if (Verbose < 0)
                    Notify(DZ_ERM_ERROR_CREATE, _T("CreateFile failed %s [%s]"),
                           fzipfile.c_str(), SysMsg().c_str());
        }
    }

    if (fAllowGrow > 0)
    {
        // zipfile is not stdout, and we are allowed to append
        // AllowGrow is true if we're just appending (-g)
		diag(_T("Processing - ready to open for appending"));
        fZipOutfile = new ZFile(this, fzipfile, GENERIC_READ | GENERIC_WRITE, 0,
                                NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL);

        if (!fZipOutfile->IsOpen())
        {
            if (Verbose < 0)
                Notify(DZ_ERM_ERROR_CREATE, _T("CreateFile failed 2 %s [%s]"),
                       fzipfile.c_str(), SysMsg().c_str());

            return DZError(DZ_ERM_ERROR_CREATE);
        }

        ftempzip = fzipfile;

        if (fZipOutfile->SetPosition(fcenbeg, FILE_BEGIN) == -1)
            return DZError(GetLastError() ? DZ_ERM_ERROR_SEEK : DZ_ERM_ZIP_EOF);

        fOutPosn = fcenbeg;
    }

    if (!fAllowGrow)
    {
        diag(_T("Processing - ready to open for Exclusive Read"));

        if ((fzfiles != NULL || fzipbeg) &&
			(fhInz = CreateFile(fzipfile.c_str(), GENERIC_READ, 0, NULL, OPEN_EXISTING,
				FILE_ATTRIBUTE_NORMAL |  FILE_FLAG_SEQUENTIAL_SCAN, NULL)) ==
                    INVALID_HANDLE_VALUE)
        {
            if (Verbose < 0)
                Notify(DZ_ERM_ERROR_CREATE, _T("CreateFile failed 3 %s [%s]"),
                       fzipfile.c_str(), SysMsg().c_str());

            return DZError(DZ_ERM_ERROR_CREATE);
        }

        ftempzip = tempname();

        if (Verbose)
            Notify(IVERBOSE, _T("Temp Filename = %s"), ftempzip.c_str());

        fZipOutfile = new ZFile(this, ftempzip, GENERIC_WRITE, 0,
                                NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL);

        if (!fZipOutfile->IsOpen())
        {
			if (Verbose)// < 0)
				Notify(DZ_ERR_ERROR_CREATE | ITRACE, _T("CreateFile failed for %s [%s]"),
                       ftempzip.c_str(), SysMsg().c_str());

            return DZError(DZ_ERM_TEMP_FAILED);
        }
    }

    if (!fAllowGrow)
    {
		TotFiles += KeptCnt + (unsigned long)vercount;
        TotSize += KeptSize + VerSize;
    }

    // Pass total number of files and Total Filesize.
	CB->SetArg1((long)TotFiles);
    CB->UserCB(zacCount);

	CB->SetFileSize(TotSize);
    CB->UserCB(zacSize);

    fBatchStarted = 1;

    if (!fAllowGrow && fzipbeg)
    {
        if (Verbose)
            Notify(IVERBOSE, _T("Copying SFX stub [%X]"), fzipbeg);

        // copy a compressed file from old archive to new archive
		CB->SetFileSize(fzipbeg);
		CB->SetMsg(_T("SFX"));
        CB->UserCB(zacItem);

        if ((r = fcopy(fzipbeg)) != DZ_ERR_GOOD)
            return DZError(r);

        fOutPosn = fzipbeg;
    }

    // Process zip file, copying from old archive to new archive. Rezip any
    //   marked files
    if (fzfiles != NULL)
        diag(_T("going through old zip file"));

    zlast = &fzfiles;
    while ((z =  *zlast) != NULL)
    {
        if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);

        if (z->mark == MARK_UPDATE)
        {
            // This file is marked
			// if not deleting, rezip it
			Notify(0, _T("updating: %s"), z->Getiname());
			ZipItem zr = *z;	// make copy
			// update comment and name (if required)
			if ((r = PrepareHeaderName(z, false)) != 0)
				return r;
			r = zipup(z);
            CB->UserItem(-1, ""); // mark end of item
			int dze = DZ_ERR(r);
			if (dze == DZ_ERR_NO_FILE_OPEN || dze == DZ_ERR_MISS || //)
				dze == DZ_ERR_ERROR_READ || dze ==DZ_ERR_SKIPPED)
			{
				_TCHAR *msg;

				*z = zr; // restore and try to keep
				if (dze == DZ_ERR_NO_FILE_OPEN)
					msg = _T("could not open for reading: %s");
				else if (dze == DZ_ERR_MISS)
					msg = _T("file and directory with the same name: %s");
				else
					msg = _T("skipped: %s");
				Notify(IWARNING, msg, z->xname);
				CB->UserMsg(r, z->xname);
				dze = 0; 	// handled that error
				Notify(IWARNING, _T("will just use old version: %s"), z->Getiname());
                if ((r = zipcopy(z)) != DZ_ERR_GOOD)
                {
					Notify(IERROR, _T("was copying %s"), z->Getiname());
					dze = -1;	// has error in r
				}
				else
					z->mark = 0;
			}
			if (dze != DZ_ERR_GOOD)
				return DZError(r);

            zlast = &z->nxt;
			ffiles_acted_on++;
		}
        else
        if (z->mark == MARK_PURGE)
        {
            // desired action is DELETE, this file marked
			Notify(0, _T("deleting: %s"), z->Getiname());

            v = z->nxt; // delete entry from list
            delete z;
            *zlast = v;           // link prev to next
            fzcount--;
            ffiles_acted_on++;
        }
        else   // mark != 1
        {
            // this file wasn't marked
            // copy the original entry verbatim
            if (!fAllowGrow)
            {
				Notify(0, _T("keeping: %s"), z->Getiname());

                if ((r = zipcopy(z)) != DZ_ERR_GOOD)
                {
                    Notify(IERROR, _T("was copying %s"), z->Getiname());
                    return DZError(r);
                }
            }

            zlast = &z->nxt;
        }
    } // end while

    // Process the 'Version' files
    if (vercount)
    {
        if (Verbose < 0)
            Notify(ITRACE, _T("Copying %d version entries"), vercount);

        while ((z = VerFiles) != NULL)
        {
            if (Abort_Flag)
				Fatal(DZ_ERM_ABORT, 0);

            if (Verbose)
				Notify(0, _T("Versioning: %s"), z->Getiname());

            VerFiles = VerFiles->nxt;
            z->nxt = NULL;

            *zlast = z;     // link to prev->nxt (allow cleanup)
			// make new internal and header names
			if ((r = NameVer(z)) != 0)
				return r;

            if ((r = zipVersion(z)) != DZ_ERR_GOOD)
            {
                Notify(IERROR, _T("was copying %s"), z->Getiname());
                return DZError(r);
            }

            // "zipup" of this file was good
            *zlast = z;
            zlast = &z->nxt;
            fzcount++;
            ffiles_acted_on++;
        }
    }

    // Process the "found" list, adding them to the zip file.
    // This is used to add files that weren't already in the archive.
    if (Verbose)
        Notify(IVERBOSE, _T("Zipping up %d NEW entries from found list"), ffcount);

    // For each new file to add (src names in found list), make a new entry
    //   for it in the "zfiles" linked list, zip up the new file, then remove the
    //   entry from the found list.
    // The last item in the for loop control deallocates spc for fname that
	//   was just zipped up

    bool Stopped = false;
    while ((z = fzfound) != NULL)
    {
        // add a new entry to "zfiles" list, before we zip up the file. That way
        //   we'll be ready to update the ZIP file's directory later.
        if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);

        fzfound = z->nxt;
        ffcount--;
        if (Stopped)
        {
            z->nxt = NULL;
			// give zacSkipped
			if (Skipping(z->FullPath(), DZ_ERR_SKIPPED, SKIPPED_USER))
				Fatal(DZ_ERM_SKIPPING, 2);
            delete z;
            continue;
        }
        *zlast = z;     // link to prev->nxt (allow cleanup)
        z->nxt = NULL;
        z->mark = MARK_NEW;

		// zip it up
		if (z->options.namenew)
			Notify(0, _T("  adding: %s as %hs"), z->Getiname(), z->Gethname());
		else
			Notify(0, _T("  adding: %s"), z->Getiname());

		// This is it - try to zip up new file
		r = zipup(z);
		int uret = CB->UserItem(-1, z->GetXName()); // mark end of item
		if (r == DZ_ERR_GOOD)
		{
			// "zipup" of this file was good
			*zlast = z;
			zlast = &z->nxt;
			fzcount++;
			ffiles_acted_on++;
            if (uret == CALLBACK_TRUE)
              Stopped = true;  // skip the rest
   			continue;
		}
        *zlast = NULL;  // remove from list
        DZStrW XName(z->GetXName());
        delete z;
		err = DZ_ERR(r);
		if (err != DZ_ERR_NO_FILE_OPEN && err != DZ_ERR_MISS &&//)
				err != DZ_ERR_ERROR_READ && err != DZ_ERR_SKIPPED)
			return DZError(r);

        if (err == DZ_ERR_NO_FILE_OPEN)
			Notify((unsigned)r | IWARNING, _T("could not open for reading: %s"), XName.c_str());
//             z->xname);
        else
        {
            _TCHAR *msg;
            if (err == DZ_ERR_MISS)
                msg = _T("file and directory with the same name: %s");
            else
				msg = _T("skipped: %s");
            Notify(IWARNING, msg, XName.c_str());//z->xname);
        }
//        *zlast = NULL;  // remove from list
//        delete z;
    }

    // Write central directory and end header to temporary zip
    diag(_T("writing central directory"));

    // get start of central directory
//    Assert(fOutPosn == SetFilePointer64(fhOutz, 0, 2), _T("invalid out posn dlz 983"));

    censtt = fOutPosn;
    k = 0; // keep count of new fnames for ZIPfile's end header
    __int64 usiz = 0;
    __int64 csiz = 0;
	CB->SetArg1(7);    // type
	CB->SetFileSize((long long)fzcount);

	CB->SetMsg(_T("*writing central directory"));
    CB->UserCB(zacXItem);

    for (z = fzfiles; z != NULL; z = z->nxt)
    {
        if ((r = putcentral(z)) != DZ_ERR_GOOD)
            return DZError(r);

        usiz += z->len;
        csiz += z->siz;
        k++;

        CB->UserXProgress(1, 7);
    }

    if (k == 0)
        Notify(IWARNING, _T("zip file empty"));

    if ((Verbose) && (faction == ADD) && (!fglobal_error_code)
            && (ffiles_acted_on > 0))
    {
        Notify(IVERBOSE, _T("Total Bytes=%Lu, compr bytes=%Lu -> %d%% savings"), usiz, csiz,
               percent(usiz, csiz));
    }

    diag(_T("writing end of central directory"));
//    Assert(fOutPosn == SetFilePointer64(fhOutz, 0, 1), _T("invalid out posn dlz 1055"));

	if (k && ((r = PutEnd((unsigned)k, censtt)) != DZ_ERR_GOOD))
        return DZError(r);

    Close_Handle(&fhInz);

    delete fZipOutfile;
    fZipOutfile = NULL;

    // Replace old zip file with new zip file, leaving only the new one
    if (!fAllowGrow && (k || faction != PURGE))
    {
        diag(_T("replacing old zip file with new zip file"));

        if ((r = replace(fzipfile, ftempzip)) != DZ_ERR_GOOD)
        {
            Notify(IWARNING, _T("new zip file left as: %s"), ftempzip.c_str());
            return DZError(r);
        }
    }

    // 1.78.1.2
    if (!k && faction == PURGE)
        // empty file - remove
    {
        a = flatest = 0;
		DeleteFile(fzipfile.c_str());

        if (!fAllowGrow)
			DeleteFile(ftempzip.c_str());
    }
    if (a)
		setfileattr(fzipfile.c_str(), a);

    // Reset the archive bit when needed for all successfull zipped files
    if (fResetArchiveBit && faction != PURGE)
    {
        unsigned cnt = 0; // 1.71.0.0 added extra callbacks
        diag(_T("resetting archive bits"));

        for (z = fzfiles; z != NULL; z = z->nxt)
			if (z->mark > 0) // new || updated
                cnt++;

        if (cnt)
        {
            // new file op.
            // Pass total number of files. filesize.
			CB->SetArg1(1);    // type
			CB->SetFileSize(cnt);
			CB->SetMsg(_T("*resetting archive bits"));
            CB->UserCB(zacXItem);
        }

        cnt = 0;

        for (z = fzfiles; z != NULL; z = z->nxt)
        {
            if (z->mark > 0)
            {
                if (++cnt == 30)
                {
                    CB->UserXProgress(cnt, 1);
                    cnt = 0;
                    if (Abort_Flag)
                        break;
                }

                DZStrW fullname = z->FullPath();

				if (!SetFileAttributes(fullname.c_str(),
					GetFileAttributes(fullname.c_str()) &~(ulg)FILE_ATTRIBUTE_ARCHIVE))
                    Notify(IWARNING, _T("Archive bit could not be set for: %s [%s]"),
                         fullname.c_str(), SysMsg().c_str());
            }
        }

        if (cnt)
            CB->UserXProgress(cnt, 1);
    }
    finish();

	return 0;
}





