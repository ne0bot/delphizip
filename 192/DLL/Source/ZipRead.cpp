#include "stdafx.h"
#pragma hdrstop
#include "ZStrings.h"
#include "ZipOp.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPREAD_CPP

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

// -- Structure of a ZIP file --
// Signatures for zip file information headers
#define LOCSIG    0x04034b50L
#define CENSIG    0x02014b50L
#define ENDSIG    0x06054b50L
#ifdef _WIN64
#define EXTLOCSIG 0x08074b50L
#endif

// Make first pass through zip file, reading information from local file
// headers and then verifying that information with the central file headers.
// Any deviation from the expected zip file format returns an error. At the
// end, a sorted list of file names in the zip file is made to facilitate
// searching by name. The name of the zip file is pointed to by the global
// "zipfile". The globals zfiles, zcount, zcomlen, zcomment, and zsort are
// filled in. Return an error code in the ZEN_xxx class.
// static int readzipfile1(ZGlobals *pG, HANDLE hInz);
int ZipOp::readzipfile(void)
{
	HANDLE hInz;
	ZipItem *z; /* current zip entry structure */
	int r = 0; // Initialize zip file info
	fzipbeg = 0;
	fcenbeg = 0;
	fzfiles = NULL; // Points to first header
	fzcount = 0; // number of files

	// If zip file exists, read headers and check structure
	if (fzipfile.IsEmpty())
		return DZ_ERR_GOOD;

	if (Verbose)
		Notify(IVERBOSE, _T("ready to open: %s for read only"), fzipfile.c_str()
			);

	if ((hInz = CreateFile(fzipfile.c_str(), GENERIC_READ, 0, NULL, OPEN_EXISTING,
				FILE_ATTRIBUTE_NORMAL | FILE_FLAG_SEQUENTIAL_SCAN, NULL))
		!= INVALID_HANDLE_VALUE)
	{
		diag(_T("zip archive is now open"));
		r = readzipfile1(hInz);
		Close_Handle(&hInz);

		if (r < 0)
			r = (GetLastError() ? DZ_ERM_ERROR_READ : DZ_ERM_ZIP_EOF);
	}

	if (r != 0)
		return r;

	// If one or more files, sort by name
	if (fzcount)
	{
		fndlist = new HashListInt((int)fzcount);

		for (z = fzfiles; z != NULL; z = z->nxt)
			fndlist->AddNode(z);
	}

	return DZ_ERR_GOOD;
}

// -------------------------------------------------------------------------

/* searches the cextra member of zlist for a zip64 extra field. if it finds one it */
/* updates the len, siz and off members of zlist with the corresponding values of */
/* the zip64 extra field, that is if either the len, siz or off member of zlist is */
/* set to its max value we have to use the corresponding value from the zip64 extra */
/* field. as of now the dsk member of zlist is not much of interest since we should */
/* not modify multi volume archives at all. */
static void adjust_zip_central_entry(ZipItem *z)
{
	const uch* pTemp;
	/* assume not using zip64 fields */

	/* check if we have a "large file" Zip64 extra field ... */
	pTemp = z->cextra.Find(ZIP64_XDATA_TAG);

	if (pTemp == NULL)
		return;

	/* using zip64 field */
	pTemp += sizeof(XDataHead);

	/* ... if so, update corresponding entries in struct zlist */
	if (z->len == MAX_UNSIGNED)
	{
		z->len = *(ZInt64*)pTemp;
		pTemp += 8;
	}

	if (z->siz == MAX_UNSIGNED)
	{
		z->siz = *(ZInt64*)pTemp;
		pTemp += 8;
	}

	if (z->off == MAX_UNSIGNED)
		z->off = *(ZInt64*)pTemp;
}

#ifdef _WIN64
# define ZIP64_EOCD_SIG                  0x06064b50
#endif

static bool NeedEOC64(ZipEndOfCentral *eoc)
{
	if (eoc->CentralOffset == MAX_UNSIGNED || eoc->TotalEntries == MAX_WORD ||
		eoc->CentralSize == MAX_UNSIGNED || eoc->CentralEntries == MAX_WORD ||
		eoc->ThisDiskNo == MAX_WORD || eoc->CentralDiskNo == MAX_WORD)
		return true;

	return false;
}

int ZipOp::readzipfile1(HANDLE hInz)
{
	ZipEndOfCentral* zeoc;
	Zip64EOCLocator* zloc;
	ZipEOC64* zeoc64;
	ZipCentralHeader *zcen;
	ZipLocalHeader *zlcl;
	::extent n; /* length of name */
	ZipItem **x; /* pointer last entry's link */
	ZipItem *z; /* current zip entry structure */
	char *t; /* temporary pointer */
	int found;
	char *buf; /* temp buffer for reading zipfile */
	DWORD k; // count of bytes read
	int idx;
	int cnt, CenEntries;
	ZInt64 u8; /* unsigned 8 byte variable */
	ZInt64 censiz; /* size of central directory */
	ZInt64 z64eocd; /* Zip64 End Of Central Directory record byte offset */
	ZInt64 deltaoff = 0;
	bool need64; // = false;
	DZStrA tmp;

	buf = (char*)fwindow;

	found = 0;
	t = &buf[4096];
	t[1] = '\0';
	t[2] = '\0';
	t[3] = '\0';
	fzipbeg = SetFilePointer64(hInz, -(ZInt64)sizeof(ZipEndOfCentral),
		SEEK_END);

	if (fzipbeg < 0)
	{
		Notify(IWARNING, _T("not a zip file"));
		return DZ_ERM_INVAL_ZIP;
	}

	if (ReadFile(hInz, buf, sizeof(ZipEndOfCentral), &k, NULL) && k == sizeof
		(ZipEndOfCentral) && *(ulg*)buf == ENDSIG)
		found = 2;
	else
	{
		fzipbeg += sizeof(ZipEndOfCentral); // size of file

		idx = 16; // max 16 * 4k = 64k

		while (!found && idx-- >= 0 && fzipbeg >= 0)
		{
			size_t sz;
			fzipbeg -= 4096;

			if (fzipbeg < 0)
				fzipbeg = 0;

			if (SetFilePointer64(hInz, fzipbeg, SEEK_SET) < 0)
				break;

			if (!ReadFile(hInz, buf, 4096 /* (size_t) fzipbeg */ , &k, NULL))
				break;

			sz = (size_t)k;
			buf[sz] = t[1];
			buf[sz + 1] = t[2];
			buf[sz + 2] = t[3];
			t = &buf[sz - 1];
			while (t >= buf)
			{
				/* Check for ENDSIG ("PK\5\6" in ASCII) */
				if (*(ulg*)t == ENDSIG)
				{
					found = 1;
					fzipbeg += (ZInt64)(t - buf); // at the eoc
					break;
				}

				--t;
			}
		}
	}

	if (!found)
	{
		Notify(IWARNING, _T("missing end signature--probably not a zip file"));
		return DZ_ERM_INVAL_ZIP;
	}

	// fzipbeg at eoc offset

	/* Read end header */
	zeoc = (ZipEndOfCentral*)fwindow;
	SetFilePointer64(hInz, (ZInt64)fzipbeg, SEEK_SET);

	if (!ReadFile(hInz, zeoc, sizeof(ZipEndOfCentral), &k, NULL) || k != sizeof
		(ZipEndOfCentral))
		return-1;

	if (zeoc->ThisDiskNo || zeoc->CentralDiskNo || zeoc->CentralEntries !=
		zeoc->TotalEntries)
		Notify(IWARNING, _T("multiple disk information ignored"));

	CenEntries = zeoc->CentralEntries;

	if (!fzcomlen) // ignore if have new
	{
		fzcomlen = zeoc->ZipCommentLen;

		if (fzcomlen)
		{
			if (!ReadFile(hInz, fzcomment.GetBuffer(fzcomlen), (DWORD)fzcomlen, &k,
					NULL) || k != (DWORD)fzcomlen)
			{
				fzcomment.Empty();
				return-1;
			}
		}
	}

	need64 = NeedEOC64(zeoc);
	if (need64)
	{
		/* account for Zip64 EOCD Record and Zip64 EOCD Locator */
		/* Z64 EOCDL must be just before EOCD if it exists */
		fcenbeg = fzipbeg-(int)sizeof(Zip64EOCLocator);

		/* check for empty archive */
		if (fcenbeg >= 0)
		{
			/* look for signature */
			if (SetFilePointer64(hInz, fcenbeg, SEEK_SET) == -1)
			{
				Notify(IWARNING, _T("end of file seeking Z64EOCLocator"));
				return DZ_ERM_INVAL_ZIP;
			}

			zloc = (Zip64EOCLocator*)(fwindow + 128);
			if (!ReadFile(hInz, zloc, sizeof(Zip64EOCLocator), &k, NULL)
				|| k != sizeof(Zip64EOCLocator))
			{
				throw DZException(DZ_ERM_INVAL_ZIP);
			}

			if (zloc->HeaderSig == EOC64LocatorSig)
			{
				/* found Zip64 EOCD Locator */
				/* check for disk information */
				if (zloc->NumberDisks != 1)
				{
					Notify(IWARNING, _T("multiple disk archives not supported")
						);
					throw DZException(DZ_ERM_ZIP_MULTI);
				}

				/* look for Zip64 EOCD Record */
				z64eocd = zloc->EOC64RelOfs;
				if (SetFilePointer64(hInz, z64eocd, SEEK_SET) == -1)
				{
					throw DZException(DZ_ERM_INVAL_ZIP);
				}

				zeoc64 = (ZipEOC64*)(fwindow + 256);
				if (!ReadFile(hInz, zeoc64, sizeof(ZipEOC64), &k, NULL)
					|| k != sizeof(ZipEOC64))
				{
					throw DZException(DZ_ERM_INVAL_ZIP);
				}

				if (zeoc64->HeaderSig != EndCentral64Sig)
				{
					Notify(IWARNING, _T(
							"Z64 EOCD not found but Z64 EOCD Locator exists"));
					throw DZException(DZ_ERM_INVAL_ZIP);
				}

				/* get size of CD */
				/* get start of CD */
				CenEntries = zeoc->CentralEntries;
				fcenbeg = zeoc64->CentralOffSet;
				deltaoff = 0; // Zip64 offsets must be correct or could not locate EOC64
				fadjust = 0; // no adjust needed
			}
			else
			{
				/* assume no Locator and no Zip64 EOCD Record */
				need64 = false;
				censiz = zeoc->CentralSize;
				fcenbeg = censiz ? zeoc->CentralOffset : fzipbeg;
				u8 = (fzipbeg - censiz) - fcenbeg;

				if (u8)
				{
					if (Verbose)
						Notify(IWARNING, _T("wrong central directory offset"));
				}
				else
					fadjust = 0; // no adjust needed

				deltaoff = fadjust ? u8 : 0L;
			}
		}
	}
	else
	{
		/* assume no Locator and no Zip64 EOCD Record */
		censiz = zeoc->CentralSize;
		fcenbeg = censiz ? zeoc->CentralOffset : fzipbeg;
		u8 = (fzipbeg - censiz) - fcenbeg;

		if (u8)
		{
			if (Verbose)
				Notify(IWARNING, _T("wrong central directory offset"));
		}
		else
			fadjust = 0; // no adjust needed

		deltaoff = fadjust ? u8 : 0L;
	}

	if (!CenEntries || (need64 && (unsigned long long)fzipbeg < sizeof(Zip64EOCLocator)))
	{
		/* zip file seems empty */
		return DZ_ERR_GOOD;
	}

	if (SetFilePointer64(hInz, fcenbeg + deltaoff, SEEK_SET) == -1)
	{
		Notify(IWARNING, _T("error seeking central directory"));
		return DZ_ERM_INVAL_ZIP;
	}

	CB->UserXItem(CenEntries + CenEntries, 9, _T("*Loading central"));

	cnt = 0;

	x = &fzfiles; /* first link */
	zcen = (ZipCentralHeader*)fwindow + 512;

	if (!ReadFile(hInz, zcen, 4, &k, NULL) || k != 4)
		return-1;

	while (zcen->HeaderSig == CENSIG)
	{
		/* Read central header. The portion of the central header that should
		be in common with local header is read raw, for later comparison.
		(this requires that the offset of ext in the zlist structure
		be greater than or equal to LOCHEAD) */
		if (!ReadFile(hInz, &zcen->MadeBy, CENHEAD, &k, NULL) || k != CENHEAD)
			return-1;

		z = new ZipItem;
		/* Link into list */
		z->nxt = NULL;
		*x = z; // link it now so it can't get lost
		x = &z->nxt;
		z->vem = zcen->MadeBy; // MadeByVersion;
		z->ver = zcen->VersionNeed;
		z->flg = zcen->Flag;
		z->how = zcen->ComprMethod;
		z->tim = zcen->ModifStamp;
		z->crc = zcen->CRC32;
		z->siz = zcen->ComprSize;
		z->len = zcen->UnComprSize;
		z->nam = zcen->FileNameLen;
		z->SetExt(0);
		z->com = zcen->FileComLen;
		z->dsk = zcen->DiskStart;
		z->att = zcen->IntFileAtt;
		z->atx = zcen->ExtFileAtt;
		z->off = zcen->RelOffLocal + deltaoff;
		z->options.dosflag = ((z->vem & 0xff00) == 0) ? 1 : 0;
		z->options.level = (unsigned)flevel;
		z->options.noext = 0;

		if (!z->nam || z->nam > MAX_PATH)
		{
			const TCHAR*erm = z->nam ? _T("name too long") : _T
				("zero-length name");
			Notify(IWARNING, _T("%s for entry #%lu"), erm, (ulg)fzcount + 1);
#ifndef DEBUG
			return DZ_ERM_INVAL_ZIP;
#endif
		}

		/* Read file name, extra field and comment field */
		if (!ReadFile(hInz, tmp.GetBuffer((int)z->nam), (DWORD)z->nam, &k, NULL)
			|| k != z->nam)
			return-1;

		tmp.ReleaseBuffer((int)z->nam);

		z->SetHName(tmp);

		if (zcen->ExtraLen)
		{
			DZRawData xdata;
			if (!ReadFile(hInz, xdata.GetBuffer(zcen->ExtraLen),
					zcen->ExtraLen, &k, NULL) || k != (unsigned)zcen->ExtraLen)
				return-1;

			z->SetCext(zcen->ExtraLen);
			z->cextra = xdata;
		}

		DZStrA zcom; // file comment
		if (z->com)
		{
			if (!ReadFile(hInz, zcom.GetBuffer((int)z->com), (DWORD)z->com, &k, NULL)
				|| k != z->com)
				return-1;

			zcom.ReleaseBuffer((int)z->com);
		}

		// decode header name
		z->SetEnc(IsEncoded(z->vem, z->flg & FLAG_UTF8_BIT) & 3);

		if (z->GetEnc() == zeoAuto && ValidUTF8(z->Gethname()) > 0)
			z->SetEnc(zeoUTF8);
		DZStrW wn;
		if (z->GetCext() && (fEncodedAs == zeoAuto || fEncodedAs == zeoUPATH))
		{
			const UPhead *up = (const UPhead*)z->cextra.Find(UPATH_XDATA_TAG);
			if (up && up->ver == 1 && up->hed.vsize > sizeof(UPhead))
			{
				ulg ocrc = crc32(0, (uch*)z->Gethname(), (int)z->nam);
				if (ocrc == up->crc)
				{
					const char *bf = ((char*)up)+sizeof(UPhead);
					int ulen = up->hed.vsize - 5;
					wn = DZStrW(CP_UTF8, bf, ulen);
				}
			}

		}
		if (wn.IsEmpty())
		{
			int UseCP = CP_THREAD_ACP;
			switch((Encodings)z->GetEnc())
			{
			case zeoAuto:
			case zeoNone:
				UseCP = (int)fFromPage;
				break;

			case zeoUTF8:
				UseCP = CP_UTF8;
				break;

			case zeoOEM:
				UseCP = CP_OEM;
				break;

			case zeoUPATH: break;
			}
			wn = DZStrW((UINT)UseCP, z->Gethname(), (int)z->nam);
		}

		DZStrW wx, wt = StrExtSep(wn);
		int nerr = CleanPath(wt, wx);
		if (nerr)
		{
			if (Verbose)
				Notify(IVERBOSE, _T("read file - invalid filename %s [%d]"),
				z->Gethname(), nerr);
			return DZ_ERM_INVAL_NAME;
		}

		z->SetIName(wx);
		z->SetXName(wx);
		if (!zcom.IsEmpty())
		{
			wn.Empty();
			if (z->GetEnc() == zeoAuto || z->GetEnc() == zeoUPATH)
			{
				const UPhead *up = (const UPhead*)z->cextra.Find
					(UCMNT_XDATA_TAG);

				if (up && up->ver == 1 && up->hed.vsize > sizeof(UPhead))
				{
					ulg ocrc = crc32(0, (uch*)zcom.c_str(), (int)z->com);
					if (ocrc == up->crc)
					{
						int len = up->hed.vsize-sizeof(UPhead);
						const char *bf = ((char*)up)+sizeof(UPhead);
						wn = DZStrW(CP_UTF8, bf, len);
					}
				}

			}
			int UseCP = CP_THREAD_ACP;
			switch((Encodings)z->GetEnc())
			{
			case zeoAuto:
			case zeoNone:
				UseCP = (int)fFromPage;
				break;

			case zeoUTF8:
				UseCP = CP_UTF8;
				break;

			case zeoOEM:
				UseCP = CP_OEM;
				break;

			case zeoUPATH: break;
			}
			if (wn.IsEmpty())
				wn = DZStrW((UINT)UseCP, zcom.c_str(), (int)z->com);
			z->SetComment(wn);
		}

		/* zip64 support 08/31/2003 R.Nausedat */
		/* here, we have to read the len, siz etc values from the CD */
		/* entry as we might have to adjust them regarding their */
		/* correspronding zip64 extra fields. */
		/* also, we cannot compare the values from the CD entries with */
		/* the values from the LH as they might be different. */
		/* adjust/update siz,len and off (to come: dsk) entries */
		/* PKZIP does not care of the version set in a CDH: if */
		/* there is a zip64 extra field assigned to a CDH PKZIP */
		/* uses it, we should do so, too. */
		adjust_zip_central_entry(z);
#ifdef _ZDEBUG
		Notify(ITRACE, _T("readzipfile1: %s dosflag %i"), z->iname,
			z->options.dosflag);
#endif

		/* Update zipbeg offset, prepare for next header */
		if (z->off < fzipbeg)
			fzipbeg = z->off;

		fzcount++;

		if (Verbose < 0)
			Notify(ITRACE, _T("read file [%d] %s"), fzcount, z->xname);

		/* Read next signature */
		zcen = (ZipCentralHeader*)fwindow + 512;

		if (!ReadFile(hInz, zcen, 4, &k, NULL) || k != 4)
			return-1;

		if (++cnt >= 10)
		{
			CB->UserXProgress(cnt, 9);
			cnt = 0;

			if (Abort_Flag)
				Fatal(DZ_ERM_ABORT, 0);
		}
	}

	/* Point to start of header list and read local headers */
	idx = 0;

	z = fzfiles;

	while (z != NULL)
	{
		if (Verbose < 0)
			Notify(ITRACE, _T("checking file [%d] %s"), ++idx, z->Getiname());

		/* Read next signature */
		zlcl = (ZipLocalHeader*)fwindow + 640;

		if (SetFilePointer64(hInz, z->off, SEEK_SET) < 0 || !ReadFile
			(hInz, zlcl, sizeof(ZipLocalHeader), &k, NULL) || k != sizeof(ZipLocalHeader))
			return-1;

		if (zlcl->HeaderSig != LOCSIG)
		{
			Notify(IWARNING, _T("local header not found for %s"), z->Getiname());
			return DZ_ERM_INVAL_ZIP;
		}

		z->lflg = zlcl->Flag;

		n = zlcl->FileNameLen;
		z->SetExt(zlcl->ExtraLen);
		bool IsProblematic = false;

		if ((z->atx & 0xFFFF0000u) != 0 && z->vem == FS_FAT &&
			(z->ver == 25 || z->ver == 26 || z->ver == 40))
			IsProblematic = true;

		/* Compare name and extra fields */
		if (n != z->nam)
		{
			Notify(IWARNING, _T("name lengths in local and central differ for %s"),
				z->Getiname());

			if (!IsProblematic) // unreliable in PKZIP
				return DZ_ERM_INVAL_ZIP;
		}

		if (!ReadFile(hInz, tmp.GetBuffer((int)z->nam + 1), (DWORD)z->nam, &k, NULL)
			|| k != z->nam)
		{
			return-1;
		}

		tmp.ReleaseBuffer();

		if (memcmp(tmp.c_str(), z->Gethname(), z->nam) &&
			(!IsProblematic || !ZMatch(tmp, z->Getiname())))
		{
			Notify(IWARNING, _T("names in local and central differ for %s"),
				z->Getiname());
			if (!IsProblematic) // unreliable in PKZIP
			{
				return DZ_ERM_INVAL_ZIP;
			}
		}

		if (z->GetExt())
		{
			DZRawData tmp2;

			if (!ReadFile(hInz, tmp2.GetBuffer(z->GetExt()), z->GetExt(), &k, NULL)
				|| k != (DWORD)z->GetExt())
			{
				z->extra = NULL;
				return-1;
			}
			tmp2.SetLength(z->GetExt());
			z->extra = tmp2;
		}

		/* Clear actions */
		z->mark = 0;
		z->trash = 0;
		z = z->nxt;
		if (++cnt >= 5)
		{
			CB->UserXProgress(cnt, 9);
			cnt = 0;

			if (Abort_Flag)
				break;
		}
	}

	if (cnt)
		CB->UserXProgress(cnt, 9); // last few

	return DZ_ERR_GOOD;
}
