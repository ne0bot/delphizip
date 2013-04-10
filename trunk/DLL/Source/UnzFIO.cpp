#include "stdafx.h"
#pragma hdrstop
#include <stdio.h>
#include "common.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZFIO_CPP
/*
unzfio.cpp -

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


/* This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 * RP - new GetFullPath, Jul 10, 2002.
 * RP - "fixed" dos_to_unix_time for problem with localtime Jul 10, 2002
 */

/*---------------------------------------------------------------------------
 * fileio.c
 * This file contains routines for doing direct but relatively generic input/
 * output, file-related sorts of things, plus some miscellaneous stuff.  Most
 * of the stuff has to do with opening, closing, reading and/or writing files.
 * Contains:  open_input_file()
 *            open_outfile()           (non-VMS, non-AOS/VS, non-CMS_MVS)
 *            undefer_input()
 *            defer_leftover_input()
 *            readbuf()
 *            readbyte()
 *            fillinbuf()
 *            flush()                  (non-VMS)
 *            disk_error()             (non-VMS)
 *            handler()
 *            dos_to_unix_time()       (non-VMS, non-OS/2, non-VM/CMS, non-MVS)
 *            check_for_newer()        (non-VMS, non-OS/2, non-VM/CMS, non-MVS)
 *            do_string()
 *            makeword()
 *            makelong()
 *            str2iso()
 *            str2oem()
 *            zstrnicmp()
 *            ExtASCII2Native()
 *            WriteToMemory()
 *            ShowSysMsg()
 *---------------------------------------------------------------------------
 */

#include "WinUser.h"
#include <shlobj.h>
#include <io.h>
#include "UnzOp.h"
#include "enter.h"

/* ===========================================================================
*                  Function open_outfile() *
* Return 0 on success and leave foutfile open.
* Return PK_DISK if failed. */
int UnzOpr::open_outfile(void)
{
	if (Is_Drv(ffilename) < 0)
		fUnzOutfile = new ZStream(this, ffilename);
	else
	{
		DWORD dwAttrs = GetFileAttributes(ffilename.c_str());
		if (dwAttrs != INVALID_FILE_ATTRIBUTES)
		{
			if (Verbose)
				Notify(IVERBOSE, _T(
					"open_outfile:  stat(%s) returns file exists"),
				ffilename.c_str());
			if ((dwAttrs & FILE_ATTRIBUTE_READONLY))
			{
				if (Verbose)
					Notify(IVERBOSE, _T(
						"open_outfile:  existing file %s is read-only"),
					ffilename.c_str());
				if (!SetFileAttributes(ffilename.c_str(), (unsigned)
						dwAttrs&(DWORD)~FILE_ATTRIBUTE_READONLY) && Verbose < 0)
					Notify(IVERBOSE, _T("could not reset read-only %s [%s]"),
					ffilename.c_str(), SysMsg().c_str());
			}
			if (!DeleteFile(ffilename.c_str()))
			{
				if (Verbose)
					Notify(IVERBOSE, _T("DeleteFile(%s) failed [%s]"),
					ffilename.c_str(), SysMsg().c_str());
				return PK_NODEL;
			}
#if defined(UNICODE) && defined(ALLOW_WIN98)
			if (IsNTorAbove)
				SHChangeNotify(SHCNE_DELETE, SHCNF_PATH,
				fpInfo->spec->Base->FullPath(ffilename).c_str(), NULL);
			else
			{
				DZStrA atmp(fpInfo->spec->Base->FullPath(ffilename));
				SHChangeNotify(SHCNE_DELETE, SHCNF_PATH, atmp, NULL);
			}
#else
			SHChangeNotify(SHCNE_DELETE, SHCNF_PATH,
				fpInfo->spec->Base->FullPath(ffilename), NULL);
#endif
			if (Verbose) // vflag)
				Notify(0, _T("Deleted pre-existing file %s"),
				ffilename.c_str());
		}

		if (Is_Drv(ffilename) < 0)
			fUnzOutfile = new ZStream(this, ffilename);
		else
			fUnzOutfile = new ZFile(this, ffilename, GENERIC_WRITE, 0, NULL,
			CREATE_NEW, FILE_ATTRIBUTE_NORMAL);
	}
	if (!fUnzOutfile->IsOpen())
	{
		if (Verbose)
			Notify(0, _T("Error [%s] opening file %s for write"),
			SysMsg().c_str(), ffilename.c_str());
		else
			Notify(0, _T("Error opening file %s for write"), ffilename.c_str());
		return PK_DISK;
	}
	// EWE SHChangeNotify(SHCNE_CREATE, SHCNF_PATH, GetFullPath(ffilename), NULL);
	if (Verbose < 0)
		Notify(ITRACE,
		_T("open_outfile:  CreateFile(%s) for writing succeeded"),
		ffilename.c_str());

	return PK_OK;
}
/* end function open_outfile() */


/* ===========================================================================
*                  function undefer_input()
* These functions allow NEXTBYTE to function without needing two bounds
* checks.  Call defer_leftover_input() if you ever have filled finbuf
* by some means other than readbyte(), and you then want to start using
* NEXTBYTE.  When going back to processing bytes without NEXTBYTE, call
* undefer_input().  For example, extract_or_test_member brackets its
* central section that does the decompression with these two functions.
* If you need to check the number of bytes remaining in the current
* file while using NEXTBYTE, check (fcsize + fincnt), not fcsize. */
void UnzSup::undefer_input(void)
{
	if (fincnt > 0)
		fcsize += fincnt;
	if (fincnt_leftover > 0)
	{
		/* We know that "(fcsize < MAXINT)" so we can cast fcsize to int:
		 * This condition was checked when fincnt_leftover was set > 0 in
		 * defer_leftover_input(), and it is NOT allowed to touch fcsize
		 * before calling undefer_input() when (fincnt_leftover > 0)
		 * (single exception: see read_byte()'s  "fcsize <= 0" handling) !! */
		fincnt = fincnt_leftover + (int)fcsize;
		finptr = finptr_leftover - (int)fcsize;
		fincnt_leftover = 0;
	}
	else if (fincnt < 0)
		fincnt = 0;
}

/* ===========================================================================
*              function defer_leftover_input() */
void UnzSup::defer_leftover_input(void)
{
	if (fcsize < MAXINT && (long)fincnt > (long)fcsize)
	{
		/* (fcsize < MAXINT), we can safely cast it to int !! */
		if (fcsize < 0L)
			fcsize = 0L;
		finptr_leftover = finptr + (int)fcsize;
		fincnt_leftover = fincnt - (int)fcsize;
		fincnt = (int)fcsize;
	}
	else
		fincnt_leftover = 0;
	fcsize -= fincnt;
}


/* ===========================================================================
*                      Function readbuf()
* Return number of bytes read into buf. */
unsigned __fastcall UnzSup::readbuf(char * buf, unsigned size)
{
	register unsigned count;
	unsigned n;

	n = size;
	while (size)
	{
		if (fincnt <= 0)
		{
			if ((fincnt = vclRead((char*)finbuf, INBUFSIZ)) == 0)
				return(n - size);
			else if (fincnt < 0)
			{
				Notify(0, _T("Error reading ZIP file"));
				return 0;
				/* discard some data - better than lock-up */
			}
			if (InProgress)
				CB->UserProgress(fincnt);
			/* buffer ALWAYS starts on a block boundary: */
			fcur_zipfile_bufstart += INBUFSIZ;
			finptr = finbuf;
		}
		count = MIN(size, (unsigned)fincnt);
		memcpy(buf, finptr, count);
		buf += count;
		finptr += count;
		fincnt -= count;
		size -= count;
	}
	// Trace(("returning %u from readbuf", n));
	return n;
}
/* end function readbuf() */


/* ===========================================================================
*                      Function readbyte()
* Refill inbuf and return a byte if available, else EOF. */
int UnzSup::readbyte(void)
{
	if (fcsize <= 0)
	{
		fcsize = -1;
		/* for tests done after exploding */
		fincnt = 0;
		return EOF;
	}
	if (fincnt <= 0)
	{
		if ((fincnt = vclRead((char*)finbuf, INBUFSIZ)) == 0)
		{
			fincnt = 0;
			/* do not allow negative value to affect stuff */
			return EOF;
		}
		else if (fincnt < 0)
		{
			/* "fail" (abort, retry, ...) returns this */
			Notify(0, _T("Error reading from ZIP file"));
			throw DZException(DZ_ERM_ERROR_READ);
		}
		if (InProgress)
			CB->UserProgress(fincnt);
		fcur_zipfile_bufstart += INBUFSIZ; // always starts on block bndry
		finptr = finbuf;
		defer_leftover_input(); // decrements fcsize
	}
	if (Encrypted)
	{
		uch * p;
		int n;

		/* This was previously set to decrypt one byte beyond fcsize, when
		 * incnt reached that far.  GRR said, "but it's required:  why?"  This
		 * was a bug in fillinbuf() -- was it also a bug here? */
		for (n = fincnt, p = finptr; n--; p++)
			* p = (uch)zdecode(*p, fkeys);
	}
	--fincnt;
	return * finptr++;
}
/* end function readbyte() */


/* ===========================================================================
*                     Function flush()
* cflag => always 0; 50 if write error */
int UnzSup::part_flush(uch * rawbuf, ulg size, int unshrink)
{
	register uch *p, *q;
	uch *transbuf;
	unsigned long k;

	if (size)
	{
		// restrict size blowouts
		if (size > fMax_Write && fMax_Write >= 0)
		{
			size = (ulg)fMax_Write;
			if (Verbose < 0)
				Notify(IWARNING, _T("File size overrun"));
		}
		fMax_Write -= size;
		if (!InProgress)
			CB->UserProgress(size);
		fBytesWritten += size;
	}
	/* ---------------------------------------------------------------------------
	 * Compute the CRC first; if testing or if disk is full, that's it.
	 *--------------------------------------------------------------------------- */
	fcrc32val = crc32(fcrc32val, rawbuf, (int)size);

	if (ftflag || !size) // testing or nothing to write:  all done
			return 0;
	if (fdisk_full)
		return PK_DISKFULL; // disk already full:  ignore rest of file
	if (!fpInfo->textmode)
	{
		/* write raw binary data */
		if ((!fUnzOutfile->Write(rawbuf, size, &k) || k != size))
			return disk_error();
		return 0;
	}

	/* ---------------------------------------------------------------------------
	 * Write the bytes rawbuf[0..size-1] to the output device, first converting
	 * end-of-lines and ASCII/EBCDIC as needed.  If SMALL_MEM or MED_MEM are NOT
	 * defined, outbuf is assumed to be at least as large as rawbuf and is not * necessarily checked for overflow.
	 *--------------------------------------------------------------------------- */
//	if (!fpInfo->textmode)
//	{
//		/* write raw binary data */
//		if ((!fUnzOutfile->Write(rawbuf, size, &k) || k != size))
//			return disk_error();
//	}
//	else
//	{
	if (unshrink)
	{
		/* rawbuf = outbuf */
		transbuf = foutbuf2;
	}
	else
	{
		/* rawbuf = slide */
		transbuf = foutbuf;
	}
	if (fnewfile)
	{
		fTextLast = 0;
		/* no previous buffers written */
		fnewfile = false;
	}
	p = rawbuf;
	if (*p == LF && fTextLast == CR)
		++p;

	/* -----------------------------------------------------------------------
	 * Algorithm:  CR/LF => native; lone CR => native; lone LF => native.
	 * This routine is only for non-raw-VMS, non-raw-VM/CMS files (i.e.,
	 * stream-oriented files, not record-oriented).
	 *----------------------------------------------------------------------- */
	q = transbuf;
	fTextLast = 0;
	for (; p < rawbuf + (unsigned)size; ++p)
	{
		if (*p == CR)
		{
			/* lone CR or CR/LF: EOL either way */
			*q++ = CR;
			*q++ = LF;
			if (p == rawbuf + (unsigned)size - 1) /* last char in buffer */
					fTextLast = CR;
			else if (p[1] == LF) /* get rid of accompanying LF */
					++p;
		}
		else if (*p == LF)
		{
			/* lone LF */
			*q++ = CR;
			*q++ = LF;
		}
		else
		{
			*q++ = *p;
		}
	}

	/* -----------------------------------------------------------------------
	 * Done translating:  write whatever we've got to file
	 *----------------------------------------------------------------------- */
	if (Verbose < 0)
		Notify(ITRACE,
		_T("p - rawbuf = %u   q-transbuf = %u   size = %lu"),
		(unsigned)(p - rawbuf), (unsigned)(q - transbuf), size);
	if (q > transbuf)
	{
		unsigned long kk, sz = (unsigned)(q - transbuf);
		if ((!fUnzOutfile->Write(transbuf, sz, &kk) || kk != sz))
			return disk_error();
	}
//	}
	return 0;
}
/* end function flush() */


int UnzSup::flush(uch * rawbuf, ulg size, int unshrink)
{
  int ret;
  while (size > 0x8000L)
  {
    ret = part_flush(rawbuf, 0x8000L, unshrink);
    if (DZ_ERR(ret)) //ret != PK_OK)
      return ret;
    size -= 0x8000L;
    rawbuf += (::extent)0x8000;
  }
  return part_flush(rawbuf, size, unshrink);
}
/* ===========================================================================
*        Function disk_error() */
int UnzSup::disk_error(void)
{
  DZError(DZ_ERM_ERROR_WRITE);
  return PK_DISK;
  /* disk is full */
}


/* ===========================================================================
*           Function handler()
* Upon interrupt, turn on echo and exit cleanly. */

/*void handler(int signal) { #ifdef SIGSEGV if (signal == SIGSEGV) { EXIT(PK_BADERR); } #endif
/ * probably ctrl-C EXIT(PK_ERR); } */

//#ifndef timezone
//#  define timezone _timezone
//#endif

#define YRBASE  1970

/* ===========================================================================
*        Function dos_to_unix_time()
* only used for freshening/updating */
static time_t dos_to_unix_time(unsigned ddate, unsigned dtime)
{
	time_t m_time;
	unsigned int yr, mo, dy, hh, mm, ss;
	static short yday[] =
	{
		0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
	};
	int leap;
	unsigned days;
//	struct tm * lt; // RP - add

	/* dissect date */
	yr = ((ddate >> 9) & 0x7f) + (1980 - YRBASE);
	mo = ((ddate >> 5) & 0x0f) - 1;
	dy = (ddate & 0x1f) - 1;

	/* dissect time */
	hh = (dtime >> 11) & 0x1f;
	mm = (dtime >> 5) & 0x3f;
	ss = (dtime & 0x1f) * 2;

	/* leap = # of leap yrs from YRBASE up to but not including current year */
	leap = ((yr + YRBASE - 1) / 4);
	/* leap year base factor */

	/* calculate days from BASE to this year and add expired days this year */
	days = (yr * 365) + (unsigned)(leap - 492) + (unsigned)yday[mo];

	/* if year is a leap year and month is after February, add another day */
	if ((mo > 1) && ((yr + YRBASE) % 4 == 0) && ((yr + YRBASE) != 2100))
		++days;
	/* OK through 2199 */

	/* convert date & time to seconds relative to 00:00:00, 01/01/YRBASE */
	m_time = (time_t)((long)(days + (unsigned)dy) * 86400L + (long)hh * 3600L + (long)
		(mm * 60 + ss));
	/* - 1;   MS-DOS times always rounded up to nearest even second */

	/* ---------------------------------------------------------------------------
	 *    Adjust for local standard timezone offset.
	 *------------------------------------------------------------------------- */
	{
		TIME_ZONE_INFORMATION tzinfo;
		DWORD res = GetTimeZoneInformation(&tzinfo);

		/* account for timezone differences */
		if (res != TIME_ZONE_ID_UNKNOWN)
			m_time += (60 * tzinfo.Bias);
	}

	/* ---------------------------------------------------------------------------
	 *    Adjust for local daylight savings (summer) time.
	 *------------------------------------------------------------------------- */
	if ((ddate >= (unsigned)DOSDATE_2038_01_18) && (m_time < (time_t)
			0x70000000L))
		m_time = U_TIME_T_MAX;
	/* saturate in case of (unsigned) overflow */
	if (m_time < (time_t)0L) /* a converted DOS time cannot be negative */
			m_time = S_TIME_T_MAX;
#ifdef _MSC_VER
	struct tm lt; 
    if (localtime_s(&lt, &m_time) == 0)
	{
		if (lt.tm_isdst)
		m_time -= (60L * 60L);

	if ((ddate >= (unsigned)DOSDATE_2038_01_18) && (m_time < (time_t)0x70000000L))
		m_time = U_TIME_T_MAX;
	/* saturate in case of (unsigned) overflow */
	if (m_time < (time_t)0L) /* a converted DOS time cannot be negative */
			m_time = S_TIME_T_MAX;	/* -> saturate at max signed time_t value */
	}
#else	
	struct tm * lt; // RP - add
	/* -> saturate at max signed time_t value */
	lt = localtime((time_t*) & m_time); // RP - added
	if (lt && lt->tm_isdst) // RP - changed
		m_time -= (60L * 60L);

	if ((ddate >= (unsigned)DOSDATE_2038_01_18) && (m_time < (time_t)0x70000000L))
		m_time = U_TIME_T_MAX;
	/* saturate in case of (unsigned) overflow */
	if (m_time < (time_t)0L) /* a converted DOS time cannot be negative */
			m_time = S_TIME_T_MAX;
	/* -> saturate at max signed time_t value */
#endif
	return m_time;
}
/* end function dos_to_unix_time() */


/* ===========================================================================
*       Function check_for_newer()
* Used for overwriting/freshening/updating.
* Return 1 if existing file is newer.
*
*filename :: Or equal; 0 if older; -1 if doesn't exist yet. */
int UnzOpr::check_for_newer(const DZStrW& filename)
{
	time_t existing, archive;
	DZStrW tmp(filename);
	if (tmp.LastChar() == BSLASH)
		tmp = tmp.Left(tmp.length() - 1); // remove
	if (_tstati64(tmp.c_str(), &fstatbuf))
	{
		if (Verbose < 0)
			Notify(ITRACE, _T("check_for_newer:  stat(%s) error: %s"),
			filename.c_str(), SysMsg().c_str());
		return DOES_NOT_EXIST;
	}
	// Trace(("check_for_newer:  stat(%s) returns 0:  file exists", filename));
	/* round up existing filetime to nearest 2 seconds for comparison */
	existing = fstatbuf.st_mtime & 1 ? fstatbuf.st_mtime +
		1 : fstatbuf.st_mtime;
	archive = dos_to_unix_time(flrec.last_mod_file_date,
		flrec.last_mod_file_time);

	if (Verbose < 0)
		Notify(ITRACE, _T(
			"check_for_newer:  existing %ld, archive %ld, Result %d"),
		existing, archive, existing >= archive);
	return(existing >= archive);
}
/* end function check_for_newer() */


/* ===========================================================================
*            Function do_string()
* Return PK-type error code. len    :: Without prototype, ush converted to this.
option :: SKIP, DISPLAY, DS_FN or EXTRA_FIELD */
int UnzOpr::do_string(unsigned int len, int option)
{
	int error = PK_OK;
	ush extra_len;

	/* ---------------------------------------------------------------------------
	 * This function processes arbitrary-length (well, usually) strings.  Four
	 * options are allowed:  SKIP, wherein the string is skipped (pretty logical,
	 * eh?); DISPLAY, wherein the string is printed to standard output after un-
	 * dergoing any necessary or unnecessary character conversions; DS_FN,
	 * wherein the string is put into the filename[] array after undergoing ap-
	 * propriate conversions (including case-conversion, if that is indicated:
	 * see the global variable pInfo->lcflag); and EXTRA_FIELD, wherein the
	 * `string' is assumed to be an extra field and is copied to the (freshly
	 * malloced) buffer fextra_field.  The third option should be OK since
	 * filename is dimensioned at 1025, but we check anyway.
	 *
	 * The string, by the way, is assumed to start at the current file-pointer
	 * position; its length is given by len.  So start off by checking length
	 * of string:  if zero, we're already done.
	 *--------------------------------------------------------------------------- */
	if (!len)
		return PK_COOL;

	switch(option)
	{
		/* * Second case:  read string into filename[] array.  The filename should
		 * never ever be longer than FILNAMSIZ - 1 (1024), but for now we'll check,
		 * just to be sure. */
	case DS_FN:
		extra_len = 0;
		if (len >= FILNAMSIZ)
		{
			Notify(IWARNING, _T(" filename too long; truncated"));
			error = PK_WARN;
			extra_len = (ush)(len - FILNAMSIZ + 1);
			len = FILNAMSIZ - 1;
		}
		{
			file_name.Empty();
			if (readbuf(file_name.GetBuffer((int)len), len) == 0)
				return PK_EOF;
			file_name.ReleaseBuffer((int)len);
		}

		if (fpInfo->vollabel && len > 8 && ffilename[8] == _T('.'))
		{
			ffilename.Delete(8, 1);
			/* disk label, and 8th char is dot:  remove dot */
		}

		if (!extra_len)
			break;
		/* we're done here */

		/* * We truncated the filename, so fall * through to the SKIP routine. */
		len = extra_len;

		/* FALL THROUGH... */
		/* * Third case:  skip string, adjusting readbuf's internal variables
		 * as necessary (and possibly skipping to and reading a new block of data). */
	case SKIP:
		/* cur_zipfile_bufstart already takes account of extra_bytes, so don't
		 * correct for it twice: */
		zlseek(fcur_zipfile_bufstart - fextra_bytes + (finptr - finbuf) + len);
		break;

		/* * Fourth case:  assume we're at the start of an "extra field"; malloc
		 * storage for it and read data into the allocated space. */
	case EXTRA_FIELD:
		extra_field.Empty();
		if (readbuf((char*)extra_field.GetBuffer(len), len) == 0)
			return PK_EOF;
		extra_field.SetLength(len);
		getZip64Data(extra_field);
		break;
	} /* end switch (option) */

	return error;
}
/* end function do_string() */

int UnzSup::vclRead(void * buf, unsigned LenToRead)
{
	unsigned long cnt;
	if (!fUnzInfile->Read(buf, LenToRead, &cnt))
		return 0;
	return (int)cnt;
}


int UnzSup::zlseek(ZInt64 abs_offset)
{
	ZInt64 request = (abs_offset) + fextra_bytes;
	ZInt64 inbuf_offset = request % INBUFSIZ;
	ZInt64 bufstart = request - inbuf_offset;

	if (request < abs_offset && fextra_bytes > 0)
	{
		return(PK_BADERR);
	}
	else if (bufstart != fcur_zipfile_bufstart)
	{
		fcur_zipfile_bufstart = fUnzInfile->SetPosition(bufstart, SEEK_SET);
		if ((fincnt = vclRead((char*)finbuf, INBUFSIZ)) <= 0)
			return(PK_EOF);
		finptr = finbuf + (int)inbuf_offset;
		fincnt -= (int)inbuf_offset;
	}
	else
	{
		fincnt += int(finptr - finbuf) - (int)inbuf_offset;
		finptr = finbuf + (int)inbuf_offset;
	}

	return 0;
}
