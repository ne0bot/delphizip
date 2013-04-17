#ifndef _DZ_ERRS_H_
#define _DZ_ERRS_H_
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
//#include "DllMsg.h"

#define DZ_ERR_GOOD   0  // ZEN_OK
#define DZ_ERR_CANCELLED  1
#define DZ_ERR_ABORT   2
#define DZ_ERR_CALLBACK  3
#define DZ_ERR_MEMORY   4
#define DZ_ERR_STRUCT   5
#define DZ_ERR_ERROR   6
#define DZ_ERR_PASSWORD_FAIL  7
#define DZ_ERR_PASSWORD_CANCEL 8
#define DZ_ERR_INVAL_ZIP      9  // ZEN_FORM PK_BADERR
#define DZ_ERR_NO_CENTRAL     10  // UEN_EOF01
#define DZ_ERR_ZIP_EOF        11  // ZEN_EOF 
#define DZ_ERR_ZIP_END        12  // UEN_EOF02
#define DZ_ERR_ZIP_NOOPEN     13
#define DZ_ERR_ZIP_MULTI      14
#define DZ_ERR_NOT_FOUND      15
#define DZ_ERR_LOGIC_ERROR    16  // ZEN_LOGIC
#define DZ_ERR_NOTHING_TO_DO  17  // ZEN_NONE
#define DZ_ERR_BAD_OPTIONS    18  // ZEN_PARM
#define DZ_ERR_TEMP_FAILED    19  // ZEN_TEMP
#define DZ_ERR_NO_FILE_OPEN   20  // ZEN_OPEN
#define DZ_ERR_ERROR_READ     21  // ZEN_READ
#define DZ_ERR_ERROR_CREATE   22  // ZEN_CREAT
#define DZ_ERR_ERROR_WRITE    23  // ZEN_WRITE
#define DZ_ERR_ERROR_SEEK     24
#define DZ_ERR_EMPTY_ZIP      25
#define DZ_ERR_INVAL_NAME     26
#define DZ_ERR_GENERAL        27  // PK_ERR
#define DZ_ERR_MISS           28  // ZEN_MISS UEN_MISC03
#define DZ_ERR_WARNING        29  // PK_WARN
#define DZ_ERR_ERROR_DELETE   30  // PK_NODEL
#define DZ_ERR_FATAL_IMPORT   31
#define DZ_ERR_SKIPPING       32
#define DZ_ERR_LOCKED         33
#define DZ_ERR_DENIED         34
#define DZ_ERR_DUPNAME        35
#define DZ_ERR_SKIPPED		  36

//#define DZ_RES_BASE           4000
//#define DZ_ERR_MISS     ((signed char)-1)
//#define DZ_ERR_FIX_MISS(x) ((DZ_ERR(x)==DZ_ERR_MISS)?(x&0x7FFFFF00L)|DZ_ERR_SKIPPING:x)

#ifndef _DZ_RES_ONLY

/*    Message code format
0FFF FFFF  LLLL LLLL   LLLL MTTT  EEEE EEEE  {31 .. 0}
F = file number (7 bits = 128 files)
L = line number (12 bits=4096 lines)
M = message instead of error string
T = type  (3 bits=8)
E = error/string code (8 bits = 256 errors)
*/

const unsigned int DZM_MessageBit = 0x800;    // mask for buffer bit
// t = type, e = error
#define DZ_MESSAGE(t, e) ((t&0xF00) | e)
#define DZ_MSG(x) (x & 0xff)
#define DZ_MSGTYP(x) (x & 0x700)
const unsigned int DZM_General = 0x000;
const unsigned int DZM_Error   = 0x600;	// 1 1 x (E... is identifier)
const unsigned int DZM_Warning = 0x400;	// 1 0 x
const unsigned int DZM_Trace   = 0x300;  // 0 1 1
const unsigned int DZM_Verbose = 0x100;	// 0 0 1
const unsigned int DZM_Message = 0x200;  // 0 1 0 (E... is identifier)

// t = type, e = error
#define DZ_ERROR(t, e) ((_DZ_FILE_ << 24) | (__LINE__ << 12) | DZ_MESSAGE(t, e))
//#define DZ_ERR(x) ((signed char)(x & 0xff))
#define DZ_ERR(x) (x & 0xff)

#define IMSG      (DZ_ERROR(DZM_General, 0))
#define IWARNING  (DZ_ERROR(DZM_Warning, 0))
#define IERROR    (DZ_ERROR(DZM_Error, 0))
#define IVERBOSE  (DZ_ERROR(DZM_Verbose, 0))
#define ITRACE    (DZ_ERROR(DZM_Trace, 0))
#define IDIAG     (DZ_ERROR(DZM_Trace, 0))

#define diag(m) \
    if (Verbose < 0)    \
        Notify(ITRACE, (m))
// files
#define DZ_COMMON_CPP   1
#define DZ_CRC32_CPP    2
#define DZ_CRCTAB_CPP   3
#define DZ_CRYPT_CPP    4
#define DZ_DZFRAME_CPP  5
#define DZ_DZOPER_CPP   6
#define DZ_ENTER_CPP    7
#define DZ_HELPERS_CPP  8
#define DZ_INGMTCH_CPP  9
#define DZ_UINFLATE_CPP 10
#define DZ_UNZCRYPT_CPP 11
#define DZ_UNZEXEC_CPP  12
#define DZ_UNZFIO_CPP   13
#define DZ_UNZINF_CPP   14
#define DZ_UNZOP_CPP    15
#define DZ_UNZPROC_CPP  16
#define DZ_UNZSS_CPP    17
#define DZ_UNZSUP_CPP   18
#define DZ_UNZWIN32_CPP 19
#define DZ_UNZXPLODE_CPP 20
#define DZ_UNZXTRCT_CPP 21
#define DZ_UTIL_CPP     22
#define DZ_ZBITS_CPP    23
#define DZ_ZCRYPT_CPP   24
#define DZ_ZDEFLATE_CPP 25
#define DZ_ZIPDFLT_CPP  26
#define DZ_ZIPFILE_CPP  27
#define DZ_ZIPFIO_CPP   28
#define DZ_ZIPFNC_CPP   29
#define DZ_ZIPMAIN_CPP  30
#define DZ_ZIPOP_CPP    31
#define DZ_ZIPPRC_CPP   32
#define DZ_ZIPREAD_CPP  33
#define DZ_ZIPSEL_CPP   34
#define DZ_ZIPSS_CPP    35
#define DZ_ZIPUP_CPP    36
#define DZ_ZIPWIN32_CPP 37
#define DZ_ZMATCH_CPP   38
#define DZ_ZSTRINGS_CPP 39
#define DZ_ZTREES_CPP   40
#define DZ_DZIMPORT_CPP 41
#define DZ_DZ_STRW_CPP  42
#define DZ_DZRAW_CPP    43


#define DZ_ERM_CANCELLED        DZ_ERROR(DZM_Error, DZ_ERR_CANCELLED)
#define DZ_ERM_MEMORY           DZ_ERROR(DZM_Error, DZ_ERR_MEMORY)
#define DZ_ERM_PASSWORD_FAIL    DZ_ERROR(DZM_Error, DZ_ERR_PASSWORD_FAIL)
#define DZ_ERM_PASSWORD_CANCEL  DZ_ERROR(DZM_Error, DZ_ERR_PASSWORD_CANCEL)
#define DZ_ERM_INVAL_ZIP        DZ_ERROR(DZM_Error, DZ_ERR_INVAL_ZIP)
#define DZ_ERM_NO_CENTRAL       DZ_ERROR(DZM_Error, DZ_ERR_NO_CENTRAL)
#define DZ_ERM_ZIP_EOF          DZ_ERROR(DZM_Error, DZ_ERR_ZIP_EOF)
#define DZ_ERM_ZIP_END          DZ_ERROR(DZM_Error, DZ_ERR_ZIP_END)
#define DZ_ERM_ZIP_NOOPEN       DZ_ERROR(DZM_Error, DZ_ERR_ZIP_NOOPEN)
#define DZ_ERM_ZIP_MULTI        DZ_ERROR(DZM_Error, DZ_ERR_ZIP_MULTI)
#define DZ_ERM_ABORT            DZ_ERROR(DZM_Error, DZ_ERR_ABORT)
#define DZ_ERM_NOT_FOUND        DZ_ERROR(DZM_Error, DZ_ERR_NOT_FOUND)
#define DZ_ERM_LOGIC_ERROR      DZ_ERROR(DZM_Error, DZ_ERR_LOGIC_ERROR)
#define DZ_ERM_NOTHING_TO_DO    DZ_ERROR(DZM_Error, DZ_ERR_NOTHING_TO_DO)
#define DZ_ERM_BAD_OPTIONS      DZ_ERROR(DZM_Error, DZ_ERR_BAD_OPTIONS)
#define DZ_ERM_TEMP_FAILED      DZ_ERROR(DZM_Error, DZ_ERR_TEMP_FAILED)
#define DZ_ERM_NO_FILE_OPEN     DZ_ERROR(DZM_Error, DZ_ERR_NO_FILE_OPEN)
#define DZ_ERM_ERROR_READ       DZ_ERROR(DZM_Error, DZ_ERR_ERROR_READ)
#define DZ_ERM_ERROR_CREATE     DZ_ERROR(DZM_Error, DZ_ERR_ERROR_CREATE)
#define DZ_ERM_ERROR_WRITE      DZ_ERROR(DZM_Error, DZ_ERR_ERROR_WRITE)
#define DZ_ERM_ERROR_SEEK       DZ_ERROR(DZM_Error, DZ_ERR_ERROR_SEEK)
#define DZ_ERM_EMPTY_ZIP        DZ_ERROR(DZM_Error, DZ_ERR_EMPTY_ZIP)
#define DZ_ERM_INVAL_NAME       DZ_ERROR(DZM_Error, DZ_ERR_INVAL_NAME)
#define DZ_ERM_ERROR            DZ_ERROR(DZM_Error, DZ_ERR_ERROR)
#define DZ_ERM_SKIPPING         DZ_ERROR(DZM_Error, DZ_ERR_SKIPPING)
#define DZ_ERM_GENERAL          DZ_ERROR(DZM_Error, DZ_ERR_GENERAL)
#define DZ_ERM_MISS             DZ_ERROR(DZM_Error, DZ_ERR_MISS)
#define DZ_ERM_SKIPPED          DZ_ERROR(DZM_Error, DZ_ERR_SKIPPED)

#define DZ_ERM_WARNING          DZ_ERROR(DZM_Warning, DZ_ERR_WARNING)
#define DZ_ERM_ERROR_DELETE     DZ_ERROR(DZM_Error, DZ_ERR_ERROR_DELETE)  
#define DZ_ERM_FATAL_IMPORT     DZ_ERROR(DZM_Error, DZ_ERR_FATAL_IMPORT)
#define DZ_ERM_LOCKED           DZ_ERROR(DZM_Error, DZ_ERR_LOCKED)
#define DZ_ERM_DENIED           DZ_ERROR(DZM_Error, DZ_ERR_DENIED)
#define DZ_ERM_DUPNAME          DZ_ERROR(DZM_Error, DZ_ERR_DUPNAME)

#define DZ_MSG_ERROR 37
#define DZ_MSG_SKIPPING 38
#define DZ_MSG_ADDED    39
#define DZ_MSG_EXTRACTED 40
#define DZ_MSG_TESTED  41

//#include <exception>

class DZFatalException//: public exception()
{
    protected:
        const TCHAR *fEMsg;
        int fENo;

    public:
        DZFatalException(int no = 0, const TCHAR *msg = NULL): fEMsg(msg), fENo(no)
        {
            ;
        }

		int ENo(void) const {return fENo;}
		const TCHAR* EMsg(void) const {return fEMsg;}
//		__property int ErrNo = {read = fENo};
//        __property const TCHAR *Msg = {read = fEMsg};
};

// gives message

class DZException: public DZFatalException
{
    public:
        DZException(int no, const TCHAR *msg = NULL):  DZFatalException(no, msg) {}
};

//class DZAbort: public DZException
//{
//    public:
//        DZAbort(int no): DZException(no, _T("User Abort")) {}
//};

#endif
#endif

