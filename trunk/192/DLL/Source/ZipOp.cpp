#include "stdafx.h"
#pragma hdrstop

#include "ZipOp.h"
//---------------------------------------------------------------------------

#include "enter.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPOP_CPP

/* ZGlobals.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * Permission is granted to any individual or institution to use, copy, or
 * redistribute this software so long as all of the original files are included,
 * that it is not sold for profit, and that this copyright notice is retained.
 * This version modified by Chris Vleghert for BCB/Delphi Zip.
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

ZipOp::ZipOp(const DllCommands *C): ZipFunc(C)
{
    fnoisy = 1;

    fuser_notified_of_abort = 0;
    fdll_handles_errors = 1;       // By dflt, this DLL will generate error msg boxes

//    fvolume_label = 0;
	fhidden_files = 0;
    ffix = 0;

    if (C->fOptions.System) // include system and hidden files -S
        fhidden_files = 1;

//	if (C->fOptions.Volume) // Include volume label -$
//		fvolume_label = 1;

    if (C->fOptions.Quiet || !C->fHandle)
    {
        // quiet operation -q
        fnoisy = 0; // shut us up!
        fdll_handles_errors = 0; // All error msgs passed to caller via callback function
    }

    if (Verbose)
        fnoisy = 1;
    // give the component the 'key'
	CB->SetArg1( ((unsigned)this) >> 2 ); // ???????????????
    Set_Oper(this, ZIPOPER);        // add to 'active' list
    CB->UserCB(zacKey);
}

ZipOp::~ZipOp(void)
{
    Set_Oper(this, 0);  // remove from list
    CB->SetArg1(0);
    CB->UserCB(zacKey);
//  ZipCleanup();   // something may have failed
}


// Add (or exclude) the name of an existing disk file. Return an error code
//   in the ZEN_ class. Return DZ_ERR_GOOD if OK. n :: Name to add (or exclude).
//   nSize :: Size of the file or dir(0).
int ZipOp::newname(const DZStrW &nme, ZInt64 nSize)
{
    FndItem  *f;  // where in found, or new found entry
    ZipItem  *z;  // where in zfiles (if found)
    int           dosflag;    // force 8x3?
    int           ErrMsg = DZ_ERR_GOOD;
    int           levl;       // required compression
    bool          noext = false;  // don't ignore extension

    DZStrW name, undosm, tmp;
    DZStrW iname;
    DZStrW m;
//    ArgSplitter splitter;
//	if (Verbose)
//		Notify(IVERBOSE, _T("adding file %s to found list"), nme.c_str());
	int r = CleanPath(nme, name);//, fEncodeAs == zeoNone);

	if (r)
    {
        if (Verbose)
            Notify(IERROR, _T("invalid new name %s"), nme.c_str());

        return DZ_ERM_INVAL_NAME;
    }

    levl = flevel;
    do
    {
        // Just one loop, with this w'll get a better error handling. RCV: 1.605
        dosflag = (fEncodeAs == zeoOEM) ? 3 : 0;

//        if (fdosify)
//            dosflag = 1;

        m = ex2IntForm(name, false);
        // Discard directory names with zip -rj
        if (m.IsEmpty())
        {
            // If extensions needs to be swapped, we will have empty directory
            //   names instead of the original directory. For example, zipping 'c.',
            //   'c.main' should zip only 'main.c' while 'c.' will be converted to
            //   '\0' by ex2in.
            if (fpathput)
                throw DZException(DZ_ERM_LOGIC_ERROR);

//                ziperr(ZEN_LOGIC05);
            break;
        }

        undosm = m;

        if (dosflag || !fpathput)
        {
            // use default settings
            undosm = ex2IntForm(name, true);
            if (undosm.IsEmpty())
                undosm = m;
        }

        // check excluded before going further
		if (fpcount && ZMatch(fExcludes, undosm))
        {
            // Do not clear z->mark if "exclude", because, when "dosify ||
            //   !pathput" is in effect, two files with different filter options
            //   may hit the same z entry.
            if (Verbose)
                Notify(IVERBOSE, _T("excluding %s"), name.c_str());

            break;
        }

        // **** start of rename check
        // Give the user a chance to change the internal name
        if (Is_DrvEx(name))
            tmp = name;
        else
			tmp = GetFullPath(name); // uses current root

		CB->SetMsg2(tmp);// full path/name
		CB->SetMsg(undosm);// proposed name
		if (CB->UserCB(zacNewName) == CALLBACK_TRUE)  // changed
		{
			int nerr;// = 0;
			tmp = CB->GetMsg();
			unsigned index = 0;
			DZStrW fs;

			DZStrW arg = GetArg(tmp, index, false);
			nerr = CleanPath(arg, fs);
			if (!nerr)
			{
				if (arg.IsEmpty())
				{
					if (Verbose)
						Notify(IVERBOSE, _T("caller excluding %s"), tmp.c_str());

					break;
				}
				// any switches issued
				while (!nerr && tmp[index] == '/')
				{
					// process switches
					arg = GetArg(tmp, ++index, false);
					if (arg.length() < 1)
						continue;
					TCHAR ch = arg[0]; // the switch
					if (arg.length() == 3 && arg[1] == _T(':') &&
						(ch == _T('c') || ch == _T('C')) && _istdigit(arg[2]))
					{
						// new compression level
						levl = arg[2] - _T('0');
						noext = true;
						continue;
					}
					nerr = 55;
//					if (Verbose)
//						Notify(IVERBOSE, _T("invalid switch %s ignored"), arg.c_str());
				}
				if (tmp[index] == ZPasswordFollows)
				{
					nerr = 66;
				}
			}
			if (nerr)// < 0)
			{
				nerr = DZ_ERM_INVAL_NAME;
				Notify((unsigned)nerr, _T("invalid new name %s"), tmp.c_str());
				return nerr;
			}
			// prepare the new name
			m = ex2IntForm(fs, false);

			// Discard directory names with zip -rj
			if (m.IsEmpty())
			{
				// If extensions needs to be swapped, we will have empty directory
				//   names instead of the original directory. For example, zipping 'c.',
				//   'c.main' should zip only 'main.c' while 'c.' will be converted to
				//   '\0' by ex2in.
				if (fpathput)
					throw DZException(DZ_ERM_LOGIC_ERROR);
				break;
			}

			if (dosflag || !fpathput)
			{
				undosm = ex2IntForm(name, true);
				if (undosm.IsEmpty())
					undosm = m;
			}
		}

        // Search for name in zip file. If there, mark it, else add to list of
        //   new names to do (or remove from that list).
//        if ((z = zsearch(undosm)) != NULL)
        if ((z = zsearch(m)) != NULL)
        {
            z->mark = 1;
			z->SetXName(name);
            z->Passw = fkey;                 // p RP 173 current password
            z->Base = CurBase->Base;
            z->options.keepver = fversion ? 1 : 0;
#ifdef FORCE_NEWNAME
            z->IName = m;//undosm;
#endif

            // Better keep the old name. Useful when updating on MSDOS a zip
            //   file made on Unix.
            z->options.dosflag = dosflag & 3;  // want OEM
#ifdef _ZDEBUG
Notify(ITRACE, _T("newname 1: %s dosflag %i"), z->iname, z->options.dosflag);
#endif

            if (Verbose)
                Notify(IVERBOSE, _T("including %s"), name.c_str());

//			if (name == flabel)
			if (name.Compare(flabel.c_str())==0)
				flabel = z->Getiname();
        }
        else
        {
            // not in zipfile already - add to or remove from list
            // if (!pcount || filter(undosm, pG)) RP cannot get here if excluded
            // Check that we are not adding the zip file to itself. This catches
            //   cases like "zip -m foo ../dir/foo.zip". SLASH
            if (SameNameExt(fzipfile, name))  // check likely same
            {

                struct stati64 statb;

                if (fzipstate == -1)
                {
                    fzipstate = /*fzipfile.CompareExact(_T("-")) &&*/
								_tstati64(fzipfile.c_str(), &fzipstatb) == 0;
                }

                if (fzipstate == 1
                        && (statb = fzipstatb, ZStat(GetFullPath(name), &statb) == 0
                            && fzipstatb.st_mode == statb.st_mode
                            && fzipstatb.st_ino == statb.st_ino
                            && fzipstatb.st_dev == statb.st_dev
                            && fzipstatb.st_uid == statb.st_uid
                            && fzipstatb.st_gid == statb.st_gid
                            && fzipstatb.st_size == statb.st_size
                            && fzipstatb.st_mtime == statb.st_mtime
                            && fzipstatb.st_ctime == statb.st_ctime
                           ))
                {
                    // Don't compare a_time since we are reading the file
                    break;                // is same
                }
            }

            // allocate space and add to list
            f = new FndItem;
            *(ffnxt) = f;
//            f->lst = ffnxt;
            f->nxt = NULL;
            ffnxt = &f->nxt;
            ffcount++;
			f->Setxname(name.c_str());
            f->Passw = fkey;       // p 173
            f->Base = CurBase->Base;
            f->options.keepver = fversion ? 1 : 0;
            f->SetIName(m);//undosm;
            f->options.dosflag = dosflag & 3;  // want OEM
            f->options.level = levl & 15;
            f->options.noext = (noext || fNoExtChk) ? 1 : 0;
            f->len = nSize;           // RCV added.
//			if (name == flabel)
			if (name.Compare(flabel.c_str())==0)
				flabel = f->Getiname();
#ifdef _ZDEBUG
Notify(ITRACE, _T("newname 2: %s dosflag %i"), f->xname, f->options.dosflag);
#endif
        }

        break;
    }
	while (true);

    return ErrMsg;
}

#define PATHCUT _T('\\')
// If the file name *s has a dot (other than the first char), or if the -A
//   option is used (adjust self-extracting file) then return the name,
//   otherwise append .zip to the name. Allocate the space for the name in
//   either case. Return a pointer to the new name, or NULL if malloc() fails.
//   s :: File name to force to zip.
DZStrW ZipOp::ziptyp(const DZStrW &s)
{
    DZStrW tmp;

    if (s.IsEmpty())
        return tmp;

    unsigned res = GetFullPathName(s.c_str(), MAX_PATH, tmp.GetBuffer(MAX_PATH), NULL);

	tmp.ReleaseBuffer((int)res);

    if (tmp.IsEmpty() || !fadjust)
        return tmp;

    int sp = tmp.ReverseFind(BSLASH);

    if (tmp.ReverseFind(_T('.')) <= sp)
        tmp += _T(".zip");

    return tmp;
}



DZOp *MakeZipper(const DllCommands *C)
{
    return new ZipOp(C);
}

int ZEN_Rank(int err)
{
    int t = DZ_ERR(err);
    if (t == DZ_ERR_MISS || t == DZ_ERR_INVAL_NAME)
        return -1;
    return t;
}
