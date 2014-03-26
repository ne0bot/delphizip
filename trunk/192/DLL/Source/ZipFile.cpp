#include "stdafx.h"
#pragma hdrstop
#include <shellapi.h>
#include <stdlib.h>
#include "DZRaw.h"
#include "ZipOp.h"
#include "ZipFnc.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPFILE_CPP

/* ZipFile.c Copyright (C) 1990-1996
   Mark Adler, Richard B. Wales, Jean-loup Gailly, Kai Uwe Rommel,
   Onno van der Linden and Igor Mandrichenko.
   This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
  * distributed under LGPL license ** see license.txt for details

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
#include "direct.h"

// -- Structure of a ZIP file --
// Signatures for zip file information headers
#define LOCSIG    0x04034b50L
#define CENSIG    0x02014b50L
#define ENDSIG    0x06054b50L
#define EXTLOCSIG 0x08074b50L

// Added R. Aelbrecht for OEM2ISO conversion
//#define cpAUTO    0
//#define cpISO     1
//#define cpOEM     2

//#define FS_FAT_   0                   // filesystem used by MS-DOS, OS/2, Win32
//#define FS_HPFS_  6                   // filesystem used by OS/2, NT
//#define FS_NTFS_  11                  // filesystem used by Windows NT


// Return a pointer to the entry in zfile with the name n, or NULL if not
//   found. n :: Name to find.
ZipItem *ZipFunc::zsearch(const DZStrW &name)
{
    if (fzcount && fndlist)
    {
        ZipItem *x = (ZipItem*)fndlist->FindName(name);
        return x;
    }

    return NULL;
}


void ZipFunc::HWriteInit(void)
{
    if (!fhwbuf)
	{
        fhwsize = 4096;
        fhwbuf = new unsigned char[fhwsize];
    }

    LocXData = 0;

    fhwcnt = 0;
}

// Write headers to a buffer
// if arg == NULL then reset
int ZipFunc::HWrite(const void *arg, int siz)
{
    if (!fhwbuf)
    {
        fhwcnt = -1;
        return -1;
    }

    // if error don't continue
    if (fhwcnt < 0)
    {
        return -1;
    }

    if (!arg || siz < 1)
    {
        return fhwcnt;
    }

    while (fhwcnt + siz >= fhwsize)
	{
        unsigned char * b2 = new unsigned char [fhwsize + 4096];

        if (!b2)
        {
            fhwcnt = -1;
            return -1;
        }

        memmove(b2, fhwbuf, (size_t)fhwcnt);

        delete[] fhwbuf;
        fhwbuf = b2;
        fhwsize += 4096;
    }

	memmove(fhwbuf + fhwcnt, arg, (size_t)siz);

    fhwcnt += siz;
    return fhwcnt;
}

#pragma pack(push, 1)
struct XLocal
{
    WORD Tag;
    WORD Siz;
	__int64 usiz;
	__int64 clen;
};
#pragma pack(pop)

#pragma pack(push, 1)
struct XNTFS
{
    WORD Tag;
    WORD Siz;
    unsigned res;
    WORD tg1;
    WORD sz1;
};
#pragma pack(pop)

// Write a local header described by *z to file *f. Return DZ_ERR_GOOD or
//   ZEN_TEMP07. z :: Zip entry to write local header for. f :: File to write
//   to.
int __fastcall ZipFunc::PutLocal(ZipItem *z)
{
    ush tag;
    int npos;
    unsigned long k;
    ZipLocalHeader zlh;
	DZRawData ox = z->extra;

    if (z->ntfs)
        ox = ox - NTFS_STAMP_TAG;

    if (Verbose < 0)
		Notify(ITRACE, _T("writing Local Header: %s"), z->Getiname());

    HWriteInit();   // reset

	unsigned short UFlag = 0;
	if (z->GetEnc() == zeoUTF8)
	{
        z->options.nameutf8 = 1;
        UFlag = FLAG_UTF8_BIT;
	}

	if (z->GetEnc() >= zeoUTF8)
		z->vem = OUR_VEM;

#ifdef _FORCE_Z64_
    z->ver = 45;
    z->vem = OS_NTFS + 45;
#endif
    if (z->siz >= MAX_UNSIGNED)
    {
        z->ver = 45;            // need Zip64
        z->vem = OS_NTFS + 45;
    }

    // may have been truncated
	z->nam = strlen(z->Gethname());

    if (z->nam > MAX_PATH)
        z->nam = MAX_PATH;  // keep sane

	zlh.HeaderSig = LOCSIG; // Should be 0x04034B50
    zlh.Flag = (WORD)(UFlag | z->lflg | ((z->options.level > 8) ? 2 : 0));
    zlh.ComprMethod = z->how;
    zlh.ModifStamp = z->tim;
    zlh.CRC32 = z->crc;

    // allow for bad 'compression'
    bool MakeX64 = (z->ver >= 45) || (z->len + 0x800) > MAX_UNSIGNED ||
        (z->siz + 0x800) > MAX_UNSIGNED;

    if ((z->ver < 45) && (z->len >= MAX_UNSIGNED))
    {
        z->flg |= FLAG_EXTEND_BIT;        // use extended data for sizes
        z->ver = 45;        // must be 64 bit
        z->vem = OS_NTFS + 45;
    }

    zlh.VersionNeed = (ush)z->ver;

    zlh.ComprSize = z->siz >= MAX_UNSIGNED ? MAX_UNSIGNED : (unsigned)z->siz;
    zlh.UnComprSize = z->len >= MAX_UNSIGNED ? MAX_UNSIGNED : (unsigned)z->len;
    zlh.FileNameLen = (ush)z->nam;
    zlh.ExtraLen = 0;   // will be updated
    HWrite(&zlh, sizeof(ZipLocalHeader));
	HWrite(z->Gethname(), (int)z->nam);
    LocXData = -1;
    npos = fhwcnt;  // size without xtra data

    // write Zip64 data
    if (MakeX64)
    {
        XLocal xl;
        xl.Tag = ZIP64_XDATA_TAG;
        xl.Siz = 16;
        xl.usiz = z->siz;
        xl.clen = 0;        // not known
        HWrite(&xl, sizeof(xl));
		LocXData = npos;
        ox -= ZIP64_XDATA_TAG;
    }

	// save utf8 name if needed
	if (z->options.enc == zeoUPATH && z->options.nameextd)
    {
//        DZStrA unam = StrToUTF8(StrIntSep(z->IName));
		DZStrA unam(StrIntSep(z->GetIName()), CP_UTF8);
        unsigned ncrc = crc32(0, (uch*)z->Gethname(), (int)z->nam);
        tag = UPATH_XDATA_TAG;   // Info-Zip Unicode Path
		int sz = 5 + (int)unam.length();
        HWrite(&tag, 2);
        HWrite(&sz, 2);
        sz = 1; // version
        HWrite(&sz, 1);
        HWrite(&ncrc, 4);
		HWrite(unam.c_str(), (int)unam.length());
        ox = UPATH_XDATA_TAG;
    }

    if (!ox.IsEmpty())
		HWrite(ox, (int)ox.Length());

    ((ZipLocalHeader *)fhwbuf)->ExtraLen = (ush)(fhwcnt - npos);

    if (fhwcnt <= 0
			|| !fZipOutfile->Write(fhwbuf, (DWORD)fhwcnt, &k)
            || k != (unsigned)fhwcnt)
		return DZ_ERM_TEMP_FAILED;

    fBytesWritten += k;
    fOutPosn += k;

//  if (Verbose < 0)
//    Notify(IDIAG, "wrote Local Header: %s [%d]", z->name, k);
//#ifdef ZDEBUG1
//Notify(ITRACE, _T("writing Local Header: %s need %x made %x"), z->iname, z->ver, z->vem);
//#endif
#ifdef ZDEBUG
    if (Verbose < 0)
    {
		const ZipLocalHeader *zc = (ZipLocalHeader *)fhwbuf;
		Notify(ITRACE, _T("local %s need %x [%x]"), z->iname,
			   zc->VersionNeed, z->ver);
	}
#endif
    return DZ_ERR_GOOD;
}

// Rewrite a local header described by *z to file *f. Return DZ_ERR_GOOD or
//   ZEN_TEMP07. z :: Zip entry to write local header for. f :: File to write
//   to.
int __fastcall ZipFunc::UpdateLocal(ZipItem *z)
{
    unsigned long k;

    if (!LocXData || fhwcnt <= 0)
    {
        // something went wrong - local not written or old data missing
		Notify(ITRACE, _T("error updating Local Header: %s"), z->Getiname());
        return DZ_ERM_LOGIC_ERROR;
    }

    if (Verbose < 0)
		Notify(ITRACE, _T("updating Local Header: %s"), z->Getiname());

    bool is64 = z->siz >= MAX_UNSIGNED || z->len >= MAX_UNSIGNED;

    XLocal *pxd = 0;

    if (LocXData > 0)
    {
        pxd = (XLocal*) & fhwbuf[LocXData];

        if (pxd->Tag != ZIP64_XDATA_TAG)
            pxd = 0;        // should not happen
    }

    ZipLocalHeader *ploc = (ZipLocalHeader *)fhwbuf;

    ploc->ComprSize = (unsigned)z->siz;
    ploc->UnComprSize = (unsigned)z->len;

    if (is64)
    {
        if (!pxd)
        {
            // cannot update without data
			Notify(IWARNING, _T("error updating Local Header: %s"), z->Getiname());
            return DZ_ERM_LOGIC_ERROR;
        }

        z->ver = 45;        // must be 64 bit

        z->vem = OS_NTFS + 45;
		pxd->usiz = z->len;
		pxd->clen = z->siz;

        if (z->siz >= MAX_UNSIGNED)
            ploc->ComprSize = MAX_UNSIGNED;

        if (z->len >= MAX_UNSIGNED)
            ploc->UnComprSize = MAX_UNSIGNED;
    }

    ploc->ComprMethod = z->how;  // may have changed

    if (z->ver < 45) //30)
        z->ver = (ush)((z->how == STORE) ? 10 : 20);

    ploc->VersionNeed = (ush)z->ver;

    ploc->Flag = (ush)((ploc->Flag & FLAG_UTF8_BIT) | z->lflg |
                    ((z->options.level > 8) ? 2 : 0));

    ploc->ModifStamp = z->tim;

    ploc->CRC32 = z->crc;

	if (!fZipOutfile->Write(fhwbuf, (DWORD)fhwcnt, &k)
            || k != (unsigned)fhwcnt)
		return DZ_ERM_TEMP_FAILED;

//  if (Verbose < 0)
//    Notify(IDIAG, "updated Local Header: %s", z->name);
    return DZ_ERR_GOOD;
}

// Write an extended local header described by *z to file *f. Return an
//   error code in the ZEN_ class. z :: Zip entry to write local header for. f
//   :: File to write to.
int __fastcall ZipFunc::putextended(ZipItem *z)
{
    unsigned long k;
    ZipDataDescriptor zdd;
    ZipDataDescriptor64 zdd64;

    if (Verbose < 0)
		Notify(ITRACE, _T("writing extended Local data: %s"), z->Getiname());

    if (z->ver >= 45)
    {
        zdd64.DataDescSig = EXTLOCSIG;
        zdd64.CRC32 = z->crc;
        zdd64.ComprSize = z->siz;
		zdd64.UnComprSize = z->len;
		if (!fZipOutfile->Write(&zdd64, sizeof(ZipDataDescriptor64), &k)
                || k != sizeof(ZipDataDescriptor64))
			return DZ_ERM_TEMP_FAILED;
    }
    else
    {
        zdd.DataDescSig = EXTLOCSIG;
        zdd.CRC32 = z->crc;
        zdd.ComprSize = (unsigned)z->siz;
		zdd.UnComprSize = (unsigned)z->len;
		if (!fZipOutfile->Write(&zdd, sizeof(ZipDataDescriptor), &k) ||
                k != sizeof(ZipDataDescriptor))
			return DZ_ERM_TEMP_FAILED;
    }

    fBytesWritten += k;

    fOutPosn += k;
    return DZ_ERR_GOOD;
}

// Write a central header described by *z to file *f. Return an error code
//   in the ZEN_ class. z :: Zip entry to write central header for. f :: File
//   to write to.
int __fastcall ZipFunc::putcentral(ZipItem *z)
{
    ush tag;
    ush siz;
#pragma pack(push, 1)
    ZInt64 dat[3];
#pragma pack(pop)
    int xargs, npos;
    unsigned long k;
    ZipCentralHeader zch;
	DZRawData ox = z->cextra;

    HWriteInit();   // reset

	tag = ZIP64_XDATA_TAG;
    xargs = 0;

    zch.UnComprSize = z->len >= MAX_UNSIGNED ? MAX_UNSIGNED : (unsigned)z->len;
    zch.ComprSize = z->siz >= MAX_UNSIGNED ? MAX_UNSIGNED : (unsigned)z->siz;
    zch.RelOffLocal = z->off >= MAX_UNSIGNED ? MAX_UNSIGNED : (unsigned)z->off;

    if (zch.UnComprSize == MAX_UNSIGNED)
        dat[xargs++] = z->len;  // uncompressed

    if (zch.ComprSize == MAX_UNSIGNED)
        dat[xargs++] = z->siz;  // compressed

    if (zch.RelOffLocal == MAX_UNSIGNED)
        dat[xargs++] = z->off;

#ifdef _FORCE_Z64_
    if (!xargs)
    {
        zch.UnComprSize = MAX_UNSIGNED;
        dat[xargs++] = z->len;  // uncompressed
    }
#endif
    siz = (ush)(xargs ? (xargs << 3) : 0);

	unsigned short UFlag = 0;
	DZStrA zcom;
//	unsigned cp = z->options.dosflag ? CP_OEM : CP_ACP;
	if (z->com)
		zcom = z->GetComment().SafeNarrow(z->options.dosflag ? CP_OEM : CP_ACP);
//		zcom = z->Comment.SafeAnsi();
	bool ExtCom = z->options.cmntextd;

	if (ExtCom && z->GetEnc() == zeoUTF8)
    {
        UFlag = FLAG_UTF8_BIT;
//        zcom = StrToUTF8(z->Comment);
		zcom = DZStrA(z->GetComment(), CP_UTF8);
		ExtCom = false;
//		z->com = (ush) zcom.length();
	}

    if (z->options.nameutf8)
        UFlag = FLAG_UTF8_BIT;

	z->com = (ush) zcom.length();

    if (Verbose < 0)
        Notify(ITRACE, _T("central [%d] %s"), xargs, z->Getiname());

    zch.HeaderSig = CENSIG;
    zch.MadeBy = z->vem;
    zch.VersionNeed = (ush)(xargs ? 45 : z->ver);
    zch.Flag = (WORD)(z->flg | UFlag | ((z->options.level > 8) ? 2 : 0));
    zch.ComprMethod = z->how;
    zch.ModifStamp = z->tim;
    zch.CRC32 = z->crc;
    zch.FileNameLen = (ush)z->nam;
    zch.ExtraLen = 0; // will update
//    zch.FileComLen = (ush)zcom.Length();
	zch.FileComLen = (ush)z->com;
    zch.DiskStart = z->dsk;
    zch.IntFileAtt = z->att;
    zch.ExtFileAtt = z->atx;

    HWrite(&zch, sizeof(ZipCentralHeader));
	HWrite(z->Gethname(), zch.FileNameLen);
    npos = fhwcnt;

    // write Zip64 extra data
    if (xargs)
    {
        HWrite(&tag, 2);
        HWrite(&siz, 2);
		HWrite(dat, siz);
        ox -= ZIP64_XDATA_TAG;
    }

    if (z->ntfs)
    {
        XNTFS xn;
        xn.Tag = NTFS_STAMP_TAG;
        xn.Siz = 32;
        xn.res = 0;
        xn.tg1 = 1;
        xn.sz1 = 24;
        HWrite(&xn, sizeof(xn));
        HWrite(z->ntfs, 24);
        ox = ox - NTFS_STAMP_TAG;
    }

	// save utf8 name if needed
    if (z->options.enc == zeoUPATH && z->options.nameextd)
	{
//		DZStrA unam = StrToUTF8(StrIntSep(z->IName));
		DZStrA unam(StrIntSep(z->GetIName()), CP_UTF8);
        unsigned ncrc = crc32(0, (uch*)z->Gethname(), (int)z->nam);
        tag = UPATH_XDATA_TAG;   // Info-Zip Unicode Path
		int sz = 5 + (int)unam.length();
        HWrite(&tag, 2);
        HWrite(&sz, 2);
        sz = 1; // version
        HWrite(&sz, 1);
        HWrite(&ncrc, 4);
		HWrite(unam.c_str(), (int)unam.length());
        ox -= UPATH_XDATA_TAG;
    }

	// UTF8 comment too
	if (ExtCom && z->options.enc == zeoUPATH && z->options.cmntextd)
    {
		unsigned ncrc = crc32(0, (uch*)zcom.c_str(), (int)zcom.length());
//		DZStrA ucom = StrToUTF8(z->Comment);
		DZStrA ucom(z->GetComment(), CP_UTF8);
		tag = UCMNT_XDATA_TAG;   // Info-Zip Unicode Comment
		int sz = 5 + (int)ucom.length();
        HWrite(&tag, 2);
        HWrite(&sz, 2);
        sz = 1; // version
        HWrite(&sz, 1);
        HWrite(&ncrc, 4);
		HWrite(ucom, (int)ucom.length());
        ox -= UCMNT_XDATA_TAG;
    }

    // write remaining 'old' extra data
    if (!ox.IsEmpty())
		HWrite(ox, (int)ox.Length());

    ((ZipCentralHeader *)fhwbuf)->ExtraLen = (ush)(fhwcnt - npos);

    if (zch.FileComLen)
		HWrite(zcom.c_str(), zch.FileComLen);

    if (fhwcnt <= 0
			|| !fZipOutfile->Write(fhwbuf, (DWORD)fhwcnt, &k)
            || k != (unsigned)fhwcnt)
        return DZ_ERM_TEMP_FAILED;

    fBytesWritten += k;
    fOutPosn += k;

#ifdef ZDEBUG
	if (Verbose < 0)
    {
        const ZipCentralHeader *zc = (ZipCentralHeader *)fhwbuf;
        Notify(ITRACE, _T("central %s-"), z->iname);
		Notify(ITRACE, _T(" made %x [%x] need %x [%x]"),
			   zc->MadeBy, z->vem, zc->VersionNeed, z->ver);
	}
#endif
    return DZ_ERR_GOOD;
}

// Write the end of central directory data to file *f. Return DZ_ERR_GOOD or
//   ZEN_TEMP09.
//    n :: Number of entries in central directory.
//    s :: Size of central directory.
//    c :: Offset of central directory.
//    f :: File to write to.
int __fastcall ZipFunc::PutEnd(unsigned CentralCnt, __int64 CentralOffs) //, HANDLE f)
{
    ZipEndOfCentral eoc;
    unsigned long k;
    __int64 cz = fOutPosn - CentralOffs;
//    Assert(fOutPosn == SetFilePointer64(fhOutz, 0, 2), _T("invalid out posn"));
    HWriteInit();   // reset
#ifndef _FORCE_Z64_

    if (CentralCnt >= MAXWORD || cz >= MAX_UNSIGNED || CentralOffs >= MAX_UNSIGNED)
#endif
    {
        // make EOC64
        ZipEOC64 e64;
        Zip64EOCLocator loc;
        e64.HeaderSig = EndCentral64Sig;
        e64.vsize = sizeof(ZipEOC64) - 12;    // data size
        e64.VersionMade = 45;
        e64.VersionNeed = 45;
        e64.ThisDiskNo = 0;
        e64.CentralDiskNo = 0;
        e64.CentralEntries = CentralCnt;
        e64.TotalEntries = CentralCnt;
        e64.CentralSize = cz;
        e64.CentralOffSet = CentralOffs;
        HWrite(&e64, sizeof(ZipEOC64));
        // make EOC64Locator
        loc.HeaderSig = EOC64LocatorSig;
        loc.EOC64DiskStt = 0;
        loc.EOC64RelOfs = fOutPosn;
        loc.NumberDisks = 1;
        HWrite(&loc, sizeof(Zip64EOCLocator));
    }
    eoc.HeaderSig = ENDSIG;
    eoc.CentralDiskNo = eoc.ThisDiskNo = 0;

    if (CentralCnt < MAXWORD)
    {
        eoc.CentralEntries = (ush)CentralCnt;
        eoc.TotalEntries = (ush)CentralCnt;
    }
    else
    {
        eoc.CentralEntries = (ush) - 1;
        eoc.TotalEntries = (ush) - 1;
    }

    if (cz < MAX_UNSIGNED)
        eoc.CentralSize = (unsigned)cz;
    else
        eoc.CentralSize = (unsigned) - 1;

    if (CentralOffs < MAX_UNSIGNED)
        eoc.CentralOffset = (unsigned)CentralOffs;
    else
        eoc.CentralOffset = (unsigned) - 1;

#ifdef _FORCE_Z64_
    eoc.CentralOffset = (unsigned) - 1;
#endif
    eoc.ZipCommentLen = (ush)fzcomlen;

    HWrite(&eoc, sizeof(ZipEndOfCentral));

    if (eoc.ZipCommentLen)
        HWrite(fzcomment, fzcomlen);

	if (fhwcnt <= 0
			|| !fZipOutfile->Write(fhwbuf, (DWORD)fhwcnt, &k)
            || k != (unsigned)fhwcnt)
		return DZ_ERM_TEMP_FAILED;

    fBytesWritten += k;
    fOutPosn += k;

    return DZ_ERR_GOOD;
}

// Note: a zip "entry" includes a local header (which includes the file
//   name), an encryption header if encrypting, the compressed data and
//   possibly an extended local header.
// Copy the zip entry described by *z from file *x to file *y. Return an
//   error code in the ZEN_ class. Also update tempzn by the number of bytes
//   copied. z :: Zip entry to copy. x, *y :: Source and destination files.
int __fastcall ZipFunc::zipcopy(ZipItem *z)
{
    ZInt64 n; // holds local header offset
    //  Trace(("zipcopy %s\n", z->zname));
    n = (ulg)(4 + LOCHEAD) + (ulg)z->nam + (ulg)z->GetExt();
	// seek to local header
    if (SetFilePointer64(fhInz, z->off, FILE_BEGIN) == -1)
		return DZ_ERM_ERROR_SEEK;

    z->off = fOutPosn;

    n += z->siz;

    // copy the compressed data and the extended local header if there is one
    if (z->lflg & FLAG_EXTEND_BIT)
        n += (z->ver >= 45 ? 20 : 16);

    fOutPosn += n;
	CB->UserItem(n, z->GetIName());

//    return fcopy(n);
	int err = fcopy(n);
	/* Finished Item */
	CB->UserItem(-1, z->GetIName());  // mark end of item
	return err;
}

//-------------------------------------------------------------------------


int ZipFunc::TrashDir(const DZStrW &dir)
{
    DZStrW tmp = StrExcSep(dir);

    if (tmp.IsEmpty())
		return -1;

    if (Verbose)
        Notify(IVERBOSE, _T("trashing directory %s (if empty)"), tmp.c_str());

	if (!RemoveDirectory(tmp.c_str()))
	{
	  if (Verbose < 0)
		Notify(IWARNING, _T("error deleting %s {%s}"), tmp.c_str(), LastSystemMsg().c_str());
	  return 1;
	}
//	int err = EraseFile(tmp, fHowToMove);
//	if (err && Verbose < 0)
//	  Notify(IWARNING, _T("error deleting %s {%x}"), tmp.c_str(), err);

    return 0;
}


// Used by qsort() to compare entries in the zfile list. Compare the
//   strings in reverse order. a, *b :: char *
static int rcmp(const void *a, const void *b)
{
	return _tcscmp(*(const TCHAR**)b, *(const TCHAR**)a);
}

// Delete the compressed files and the directories that contained the
//   deleted files, if empty. Return an error code in the ZEN_ class. Failure
//   of destroy() or rmdir() is ignored.
int ZipFunc::trash(void)
{
    int i; // counter on deleted names
	int n; // number of directories to delete
    ZipItem *z, *prv = NULL; // current zip entry
    // Delete marked names and count directories
    n = 0;

    for (z = fzfiles; z != NULL; z = z->nxt)
    {
//		if (z->mark == 1 || z->trash)
//		if (z->mark == MARK_NEW /*1*/ || z->trash)
		if (z->mark > 0 || z->trash)
        {
            z->mark = 1;
            DZStrW f = z->FullPath();
			if (!z->GetIsFolder())
            {
                // don't unlink directory SLASH
                if (Verbose)
                    Notify(IVERBOSE, _T("deleting file %s"), f.c_str());

                int err = EraseFile(f, fHowToMove);
                if (err && Verbose < 0)
					Notify(IWARNING, _T("error deleting %s {%x}"), f.c_str(), err);

                // Try to delete all paths that lead up to marked names. This is
                //   necessary only with the -D option.
                if (!fdirnames)
                {
                    diag(_T("no dirnames will be kept"));
                    DZStrW pth = z->xname;
					int ls = pth.ReverseFind(_T('\\'));
                    if (ls > 0)
                    {
						pth = pth.Left((unsigned)ls);   // only leave prev folders
						// remove a lot of duplicates
                        if (prv && ZMatch(prv->xname, pth))
                            z->mark = 0;   // ignore same path
                        else
							z->Setxname(pth.c_str());
                    }
                    else
                    {
                        z->mark = 0;   // done with it
                        z->Setxname(NULL);  // free up
                    }
                }
            }
        }

        // is it directory we want to remove?
        if (z->mark)
        {
            n++;
            prv = z;
        }
    }

    if (n == 1 && prv)
    {
        TrashDir(prv->FullPath());
        n = 0;
    }

    if (n)
    {
        // Construct the list of all marked directories. Some may be duplicated if
        //   -D was used.
        TCHAR **dlist;       // for sorting
        typedef TCHAR* PTChar;
        dlist = new PTChar[n];

        int dcnt = 0;
        for (z = fzfiles; z != NULL; z = z->nxt)
        {
			if (z->mark  && dcnt < n)
			{
                if (_tcslen(z->xname) < 1)
                    continue;

                DZStrW fullpth = z->FullPath();
                const TCHAR *fp = fullpth.c_str();
				int dlen = (int)_tcslen(fp);
                TCHAR *dn = new TCHAR[dlen + 1];

                if (!dn)
                    break;   // keep safe

                TCHAR *d = dn;

				for (int ii = 0; ii < dlen; ii++)
                    *d++ = (TCHAR)_totupper(*fp++);

                *d = 0;
                if (dcnt == 0 || !ZMatch(dn, dlist[dcnt - 1]))
                    dlist[dcnt++] = dn;
                else
                    delete dn;
            }
        }

        // Sort the files in reverse order to get subdirectories first,
        //  so x/y/z will always be removed before x/y.
		qsort((TCHAR*)dlist, (size_t)dcnt, sizeof(TCHAR*), rcmp);

        for (i = 0; i < dcnt; i++)
        {
            TCHAR *p = dlist[i];

            if (*p == '\0')
                continue;

            if (i == 0 || !ZMatch(p, dlist[i - 1]))
                TrashDir(p);
        }

        // clean up
		for (int ii = 0; ii < dcnt; ii++)
			delete[] dlist[ii];
        delete[] dlist;
    }

	return DZ_ERR_GOOD;
}




