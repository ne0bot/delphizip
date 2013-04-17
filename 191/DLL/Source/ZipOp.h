//---------------------------------------------------------------------------

#ifndef ZipOpH
#define ZipOpH
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
************************************************************************
*/

#include "common.h"
#include "dzoper.h"
#include "DZRaw.h"
#include "ZipDflt.h"
#include "ZipFnc.h"

#define A_RONLY   0x01
#define A_HIDDEN  0x02
#define A_SYSTEM  0x04
#define A_LABEL   0x08
#define A_DIR     0x10
#define A_ARCHIVE 0x20

/* Forget FILENAME_MAX (incorrectly = 14 on some System V) */
//#define FNMAX 256

/* Lengths of headers after signatures in bytes */
//#define LOCHEAD 26
//#define CENHEAD 42
//#define ENDHEAD 18

#include "Crypt.h"

#define ADD           1

class ZipOp : public ZipFunc
{

    private:
        ZipOp(const ZipOp&);
        ZipOp& operator=(const ZipOp&);

    protected:
        long fOCDlength;
        int fOldFAT;   // **
        TCHAR fDrv[2];  // **
        int fnoisy;
//        int fvolume_label;
        int ffix;

    public:
        int fhidden_files;
        ZipOp(const DllCommands *C);
        ~ZipOp(void);
        long Exec(const DllCommands *C);
        int     readzipfile(void);

    protected:
        int ZipSelect(const DllCommands *C);
        DZStrW ziptyp(const DZStrW &name);
        int GetFileMode(const DZStrW &name);

        int   IsFileSystemOldFAT(const DZStrW &dir);

        DZStrW getVolumeLabel(const DZStrW &drv, ulg *vtime, ulg *vmode, time_t *vutim);
//        DZStrW ex2IntForm(const DZStrW &xname, bool ignore); // convert external to internal form
		int ZipStreamStream(void);
    private:
		int ZipSetParam(const DllCommands *C);
		void GiveGlobals(void);
		int GetUserPW(void);               // ZipSel
		int MakeSpecials(void); //ZipSel
//        DZStrW MakeExclFilters(void);
//		DZStrW ConvExclFilters(const DZStrW & filters);
		int Wild(const DZStrW &spec);
        int procname(const DZStrW &fname, bool);
        int newname(const DZStrW &n, ZInt64 nSize);
        int readzipfile1(HANDLE hInz);
};
         
int ZEN_Rank(int err);

//ZGlobals; /* end of struct ZGlobals */
int             issymlnk(ulg a);

#endif





