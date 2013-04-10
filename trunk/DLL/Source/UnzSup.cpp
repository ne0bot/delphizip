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
#include "UnzSup.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZSUP_CPP
//---------------------------------------------------------------------------
//#pragma package(smart_init)


UnzSup::UnzSup(const DllCommands *C): DZOp(C)
{
    fqflag = 0;
    ftflag = 0;

    fredirect_data = 0;
    fredirect_pointer = 0;
    fredirect_buffer = 0;
    fredirect_size = 0;
    fbuffer_size = 0;
    chdrList = NULL;
    chdrCount = 0;
    Encrypted = false;
    InProgress = false;

    fpInfo = NULL;

    fcsize = 0;                 /* used by list_files(), NEXTBYTE: must be signed   */
    fucsize = 0;                /* used by list_files(), unReduce(), explode() */
    fused_csize = 0;            /* used by extract_or_test_member(), explode() */
    fcur_zipfile_bufstart = 0;    /* extract_or_test, readbuf, ReadByte */

    fincnt_leftover = 0;           /* so improved NEXTBYTE does not waste input */
    finptr_leftover = 0;
    foutbufptr = 0;               /* extract.c static */
    foutsize = 0;                  /* extract.c static */
    freported_backslash = 0;       /* extract.c static */

    fcrc32val = 0;                 /* CRC shift reg. (was static in funzip) */

    finbuf = 0;                   /* input buffer (any size is OK) */
    finptr = 0;                   /* pointer into input buffer  */
    fincnt = 0;
    fbitbuf = 0;
    fbits_left = 0;                /* unreduce and unshrink only */
	fzipeof = 0;
    fUnzInfile = NULL;
    fUnzOutfile = NULL;
    fextra_bytes = 0;

    fmem_mode = 0;
    foutbuf = 0;
    frealbuf = 0;

    foutbuf2 = 0;                 /*  main() (never changes); else malloc'd */
    foutptr = 0;
    foutcnt = 0;                   /* number of chars stored in outbuf */
    fnewfile = 0;

    fTextLast = 0;                // last text convert buffer character
    fsol = 0;                      /* fileio static: at start of line */
    fdisk_full = 0;
}

UnzSup::~UnzSup(void)
{
    CHdrInfo *t, *p = chdrList;

    while (p)
    {
        t = p;
        p = p->next;

        delete t;
    }

    if (fUnzInfile)
        delete fUnzInfile;

    if (fUnzOutfile)
        delete fUnzOutfile;
}

                // used by extract for searches
UnzFileSpec::UnzFileSpec()
{
    fFileSpec = NULL;
    fPassword = NULL;
    Base = NULL;
    Match = 0;
}

UnzFileSpec::~UnzFileSpec()
{
    if (fFileSpec)
        delete [] fFileSpec;
}

void __fastcall UnzFileSpec::SetSpec(const DZStrW& value)
{
	if (fFileSpec)
		delete [] fFileSpec;
	fFileSpec = (TCHAR*)DupStr(value);
	Hash = 0;
//	if (!IsWild(value))
		Hash = HashFunc(value);
}                      

void __fastcall UnzFileSpec::SetPassw(const DZStrA& value)
{
    if (fPassword)
        delete [] fPassword;
    fPassword = (char*)DupStr(value);
}


CHdrInfo::CHdrInfo()
{
    offset = 0;
    compr_size = 0;
    uncomp_size = 0;
	crc = 0;
	_flags = 0;
	file_attr = 0;
	chdrseq = 0;
	spec = NULL;
    next = NULL;
    fxname = NULL;       
    fhname = NULL;
    ntfs_data = NULL;
}

CHdrInfo::~CHdrInfo()
{
    if (fxname)
        delete[] fxname;
    if (fhname)
        delete[] fhname;
    if (ntfs_data)
        delete ntfs_data;
}

void CHdrInfo::Clear(void)
{
    if (fxname)
        delete[] fxname;
    fxname = NULL;
    if (fhname)
        delete[] fhname;
    fhname = NULL;
    if (ntfs_data)
        delete ntfs_data;
    ntfs_data = NULL;
    _flags = 0;
	offset = 0;
    compr_size = 0;
    uncomp_size = 0;
    crc = 0;
	file_attr = 0;
	chdrseq = 0;
    spec = NULL;
    next = NULL;
}

DZStrW __fastcall CHdrInfo::FullPath(void) const
{
    return spec->Base->FullPath(GetXName());
}                      
     

void __fastcall CHdrInfo::Setxname(const TCHAR *value)
{
    if (fxname)
        delete[] fxname;
    fxname = NULL;
    if (value)
        fxname = zstrdup(value);
}

   
 
void __fastcall CHdrInfo::Sethname(const char* value)
{
    if (fhname)
        delete[] fhname;
    fhname = NULL;
    if (value)
        fhname = zstrdupB(value);
}
     
bool __fastcall CHdrInfo::GetIsFolder(void) const
{
    return fhname[strlen(fhname)- 1] == '/';
}

const int ErrOrder[] = {DZ_ERR_WARNING, DZ_ERR_GENERAL, DZ_ERR_INVAL_NAME, DZ_ERR_INVAL_ZIP,
    DZ_ERR_NO_FILE_OPEN, DZ_ERR_BAD_OPTIONS, DZ_ERR_MISS, DZ_ERR_ERROR_WRITE,
    DZ_ERR_ZIP_EOF, DZ_ERR_ERROR_CREATE, DZ_ERR_ERROR_DELETE, 0};
int __fastcall PK_Rank(int err)
{
    int e = DZ_ERR(err);
    int r = 2;//0;
    if (!e)
		return r;
	for (const int *p = ErrOrder; *p && *p != e; /*r++,*/ p++) r++;
    return r;
}

unsigned __fastcall HashFunc(const DZStrW& strng)
{
	if (strng.FindOneOf(_T("*?|"), 0) >= 0)
		return 0;
	DZStrW upr(StrExtSep(strng));
//	DZStrW upr(StrExtSep(strng).AsLower());
//	DZStrW upr = StrExtSep(strng).AsLower();
//	upr.ToUpper().Trim();
	upr.ToLower();
	upr.Trim();
	const wchar_t *str = upr.c_str();
	int len = (int)upr.length();
	if (Is_Drv(upr) < 0)
	{
		unsigned int dlen = (unsigned)DriveLen(upr);
		str += dlen; // get filename for stream
		len -= dlen;
	}
	return crc32(0, (uch*)str, len * (int)sizeof(TCHAR));
}

