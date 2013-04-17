#include "stdafx.h"
#pragma hdrstop
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZPROC_CPP
/*
Process.c -

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
 *********************************************************************** */

/* ---------------------------------------------------------------------------
 * process.c
 * This file contains the top-level routines for processing multiple zipfiles.
 * Contains:  process_zipfiles()
 *            do_seekable()
 *            find_ecrec()
 *            uz_end_central()
 *            process_cdir_file_hdr()
 *            get_cdir_ent()
 *            process_local_file_hdr()
 *            ef_scan_for_izux()
 *--------------------------------------------------------------------------- */

#include "UnzOp.h"
#include <stdio.h>

/* ===========================================================================
 *                     Function process_zipfiles()
 * return PK-type error code
 */
void UnzOpr::process_zipfiles(void)
{
	int NumWinFiles, NumLoseFiles, NumWarnFiles;
	int NumMissDirs, NumMissFiles;
	unsigned int error; // ,      error_in_archive = 0;

	if (Verbose < 0)
		Notify(ITRACE, _T("In process_zipfiles"));

	fhold = finbuf + INBUFSIZ;
	/* to check for boundary-spanning sigs */

	if (Verbose < 0)
		Notify(ITRACE, _T("two"));
	/* ---------------------------------------------------------------------------
	 * Match (possible) wildcard zipfile specification with existing files and
	 * attempt to process each.  If no hits, try again after appending ".zip"
	 * suffix.  If still no luck, give up.
	 *--------------------------------------------------------------------------- */
	NumWinFiles = NumLoseFiles = NumWarnFiles = 0;
	NumMissDirs = NumMissFiles = 0;
	if (Verbose < 0)
		Notify(ITRACE, _T("four in process.c")); // - ready to dowild");

	if (DZ_ERR(error = do_seekable(0)) == PK_WARN)
		++NumWarnFiles;
	else if (error == IZ_DIR)
		++NumMissDirs;
	else if (DZ_ERR(error) == PK_NOZIP)
		++NumMissFiles;
	else if (error)
		++NumLoseFiles;
	else
		++NumWinFiles;

	if (Verbose < 0)
	{
		Notify(ITRACE, _T(
				"after do_seekable, warn=%d   missdir=%d  missfi=%d  losefi=%d  winfi=%d")
			, NumWarnFiles, NumMissDirs, NumMissFiles, NumLoseFiles,
			NumWinFiles);

		Notify(ITRACE, _T("do_seekable(0) returns %d"), error);
	}

	if (Verbose < 0)
		Notify(ITRACE, _T("six"));
	/* ---------------------------------------------------------------------------
	 * Print summary of all zipfiles, assuming zipfile spec was a wildcard (no
	 * need for a summary if just one zipfile).
	 *--------------------------------------------------------------------------- */
	if ((NumWinFiles > 1) || (NumWinFiles == 1 && NumMissDirs + NumMissFiles +
			NumLoseFiles + NumWarnFiles > 0))
		Notify(0, _T("files processed OK"));
	if (NumWarnFiles > 0)
		Notify(0, _T("warnings were given"));
	if (NumLoseFiles > 0 || NumMissFiles > 0)
		Notify(0, _T("file(s) not found"));
	if (NumMissDirs == 1)
		Notify(0, _T("zip file was dir"));
	else if (NumMissDirs > 0)
		Notify(0, _T("many zip file were dirs"));
	if (NumWinFiles + NumLoseFiles + NumWarnFiles == 0)
		Notify(0, _T("no files found"));

	if (Verbose < 0)
		Notify(ITRACE, _T("seven"));
	/* free allocated memory */
	inflate_free();

	if (foutbuf2)
		delete[]foutbuf2;
	return;
}
/* end function process_zipfiles() */

/* ===========================================================================
 *                     Function do_seekable()
 * return PK-type error code */
int UnzOpr::do_seekable(int lastchance)
{
	/* static int no_ecrec = FALSE;  SKM: moved to globals.h */
	ulg sig;
	struct stati64 stt64;
	int maybe_exe = false;
	int too_weird_to_continue = false;
	int error = 0, error_in_archive;

	if (Verbose < 0)
		Notify(ITRACE, _T("starting do_seekable"));

	/* ---------------------------------------------------------------------------
	 * Open the zipfile for reading in BINARY mode to prevent CR/LF translation,
	 * which would corrupt the bit streams.
	 *--------------------------------------------------------------------------- */
	if (_tstati64(fzipfn.c_str(), &stt64) || (error = S_ISDIR(stt64.st_mode)) != 0)
		return error ? IZ_DIR : PK_NOZIP;

	fziplen = stt64.st_size;

	if (stt64.st_mode & S_IEXEC)
		maybe_exe = true;
	/* might find unzip, not unzip.zip; etc. */

	if (Verbose) // < 0)
		Notify(ITRACE, _T("opening zip file; fname=%s"), fzipfn.c_str());
	fUnzInfile = NULL;
	AutoStream uzfile(&fUnzInfile);

	fUnzInfile = new ZFile(this, fzipfn,
		FILE_ATTRIBUTE_NORMAL | FILE_FLAG_RANDOM_ACCESS);
	if (!uzfile.Stream()->IsOpen())
	{
		if (Verbose < 0)
			Notify(ITRACE, _T("could not open: %s [%s]"), fzipfn.c_str(),
			SysMsg().c_str());

		throw DZException(DZ_ERM_ERROR_CREATE);
	}

	if (Verbose < 0)
		Notify(ITRACE, _T("zip file %s opened OK"), fzipfn.c_str());
	/* the stat() test above, but... */
	// if (Verbose < 0)
	// Notify(ITRACE, "do_seekable, loc 3");

	/* ---------------------------------------------------------------------------
	 * Find and process the end-of-central-directory header.  UnZip need only
	 * check last 65557 bytes of zipfile:  comment may be up to 65535, end-of-
	 * central-directory record is 18 bytes, and signature itself is 4 bytes;
	 * add some to allow for appended garbage.
	 *--------------------------------------------------------------------------- */

	fcur_zipfile_bufstart = 0;
	finptr = finbuf;

#ifdef TIMESTAMP
	if (!fqflag && !fT_flag)
#else
		if (!fqflag)
#endif

			Notify(0, _T("Archive:  %s"), fzipfn.c_str());

	if (Verbose < 0)
		Notify(ITRACE, _T("do_seekable, loc 4"));
	if ((((error_in_archive = find_ecrec(MIN(fziplen, 66000L))) != 0 ||
				((error_in_archive = uz_end_central()) != 0 && DZ_ERR
					(error_in_archive) != PK_WARN))))
	{
		delete fUnzInfile;
		fUnzInfile = NULL;

		if (maybe_exe)
			Notify(0, _T("maybe an EXE file: %s"), fzipfn.c_str());
		if (lastchance)
			return error_in_archive;
		else
		{
			fno_ecrec = true;
			/* assume we found wrong file:  e.g., */
			return PK_NOZIP;
			/* unzip instead of unzip.zip */
		}
	}

	if (Verbose < 0)
		Notify(ITRACE, _T("do_seekable, loc 5"));
	if (fzflag > 0)
	{
		delete fUnzInfile;
		fUnzInfile = NULL;
		return error_in_archive;
	}

	/* ---------------------------------------------------------------------------
	 * Test the end-of-central-directory info for incompatibilities (multi-disk
	 * archives) or inconsistencies (missing or extra bytes in zipfile).
	 *--------------------------------------------------------------------------- */
#ifdef NO_MULTIPART
	error = (fecrec.number_this_disk == 1) && (fecrec.num_disk_start_cdir == 1);
#else
	error = (fecrec.number_this_disk != 0);
#endif
#ifdef NO_MULTIPART /* concatenation of multiple parts works in some cases */
	// else
	if (!error && fecrec.number_this_disk != 0)
	{
		DZError(DZ_ERM_ZIP_MULTI);
		error_in_archive = PK_FIND;
		too_weird_to_continue = true;
	}
#endif

	if (Verbose < 0)
		Notify(ITRACE, _T("do_seekable, loc 6"));
	if (!too_weird_to_continue)
	{
		/* (relatively) normal zipfile:  go for it */
		if (error)
		{
			Notify(0, _T("maybe a pak bug in %s"), fzipfn.c_str());
			error_in_archive = PK_WARN;
		}

		if ((fextra_bytes = freal_ecrec_offset - fexpect_ecrec_offset) < 0L)
		{
			Notify(0, _T("missing bytes in zipfile"));
			error_in_archive = PK_ERR;
		}
		else if (fextra_bytes > 0)
		{
			if ((fecrec.offset_start_central_directory == 0) &&
				(fecrec.size_central_directory != 0))
			{
				/* zip 1.5 -go bug */
				Notify(0, _T("NULL central dir"));
				fecrec.offset_start_central_directory = fextra_bytes;
				fextra_bytes = 0;
				error_in_archive = PK_ERR;
			}
			else
			{
				Notify(0, _T("Warning: extra bytes at start of zipfile"));
				error_in_archive = PK_WARN;
			}
		}

		/* -----------------------------------------------------------------------
		 *       Check for empty zipfile and exit now if so.
		 *----------------------------------------------------------------------- */
		if (Verbose < 0)
			Notify(ITRACE, _T("do_seekable, loc 7"));
		if (fexpect_ecrec_offset == 0L && fecrec.size_central_directory == 0)
		{
			Notify(0, _T("Empty zipfile"));
			delete fUnzInfile;
			fUnzInfile = NULL;
			return PK_ERR_NOWARN(error_in_archive) ? error_in_archive : PK_WARN;
		}

		/* -----------------------------------------------------------------------
		 * Compensate for missing or extra bytes, and seek to where the start
		 * of central directory should be.  If header not found, uncompensate
		 * and try again (necessary for at least some Atari archives created
		 * with STZip, as well as archives created by J.H. Holm's ZIPSPLIT 1.1).
		 *----------------------------------------------------------------------- */
		zlseek(fecrec.offset_start_central_directory);
#ifdef OLD_SEEK_TEST
		if (readbuf((char*) & sig, 4) == 0)
		{
			close(fzipfd);
			fzipfd = 0;
			/* RCV added 29-1-99 */
			return PK_ERR;
			/* file may be locked, or possibly disk error(?) */
		}

		if (strncmp(fsig, fcentral_hdr_sig, 4))
#else
			if ((readbuf((char*) & sig, 4) == 0) || sig != CentralFileHeaderSig)
#endif
			{
				if (Verbose < 0)
					Notify(ITRACE, _T("central dir found"));
				fextra_bytes = 0;
				zlseek(fecrec.offset_start_central_directory);
				if ((readbuf((char*) & sig, 4) == 0)
					|| sig != CentralFileHeaderSig)
				{
					DZError(DZ_ERM_NO_CENTRAL);
					delete fUnzInfile;
					fUnzInfile = NULL;

					return PK_BADERR;
				}

				Notify(0, _T("central dir too long"));
				error_in_archive = PK_ERR;
			}

		/* -----------------------------------------------------------------------
		 * Seek to the start of the central directory one last time, since we
		 * have just read the first entry's signature bytes; then list, extract
		 * or test member files as instructed, and close the zipfile.
		 *----------------------------------------------------------------------- */
		if (Verbose < 0)
			Notify(ITRACE, _T("about to extract files (error = %d)"),
			error_in_archive);
		zlseek(fecrec.offset_start_central_directory);
		// GO DO EXTRACT OR TEST
		error = extract_or_test_files();
		/* EXTRACT OR TEST 'EM */

		if (Verbose < 0)
			Notify(ITRACE, _T("Done with extract/list files (error = %d)"),
			error);
		if (error > error_in_archive) /* don't overwrite stronger error */
				error_in_archive = error;
		/* with (for example) a warning */
	}
	/* end if (!too_weird_to_continue) */

	delete fUnzInfile;
	fUnzInfile = NULL;

	return error_in_archive;
}

/* end function do_seekable() */

/** ******************** */
/* Function rec_find() */
/** ******************** */

/* return 0 when rec found, 1 when not found, 2 in case of read error */
int UnzOpr::rec_find(ZInt64 searchlen, ulg signature, int rec_size)
{
	int i, numblks, found = FALSE;
	ZInt64 tail_len;

	/* ---------------------------------------------------------------------------
	Zipfile is longer than INBUFSIZ:  may need to loop.  Start with short
	block at end of zipfile (if not TOO short).
	--------------------------------------------------------------------------- */

	if ((tail_len = fziplen % INBUFSIZ) > rec_size)
	{
		fcur_zipfile_bufstart = fUnzInfile->SetPosition(fziplen - tail_len,
			SEEK_SET);
		if ((fincnt = vclRead((char*)finbuf, (unsigned int)tail_len)) != (int)
			tail_len)
			return 2; /* it's expedient... */

		/* 'P' must be at least (rec_size+4) bytes from end of zipfile */
		for (finptr = finbuf + (int)tail_len - (rec_size + 4);
			finptr >= finbuf; --finptr)
		{
			if (*(ulg*)finptr == signature)
			{
				// if ((*finptr == (uch)0x50) &&         /* ASCII 'P' */
				// !strncmp((char *)finptr, signature, 4)) {
				fincnt -= (int)(finptr - finbuf);
				found = TRUE;
				break;
			}
		}
		/* sig may span block boundary: */
		memcpy((char*)fhold, (char*)finbuf, 3);
	}
	else
		fcur_zipfile_bufstart = fziplen - tail_len;

	/* -----------------------------------------------------------------------
	Loop through blocks of zipfile data, starting at the end and going
	toward the beginning.  In general, need not check whole zipfile for
	signature, but may want to do so if testing.
	----------------------------------------------------------------------- */

	numblks = (int)((searchlen - tail_len + (INBUFSIZ - 1)) / INBUFSIZ);
	/* ==amount=   ==done==   ==rounding==    =blksiz= */

	for (i = 1; !found && (i <= numblks); ++i)
	{
		fcur_zipfile_bufstart -= INBUFSIZ;
		fUnzInfile->SetPosition(fcur_zipfile_bufstart, SEEK_SET);
		if ((fincnt = vclRead((char*)finbuf, INBUFSIZ)) != INBUFSIZ)
			return 2; /* read error is fatal failure */

		for (finptr = finbuf + INBUFSIZ - 1; finptr >= finbuf; --finptr)
			if (*(ulg*)finptr == signature)
			{
				// if ((*finptr == (uch)0x50) &&         /* ASCII 'P' */
				// !strncmp((char *)finptr, signature, 4)) {
				fincnt -= (int)(finptr - finbuf);
				found = TRUE;
				break;
			}
		/* sig may span block boundary: */
		memcpy((char*)fhold, (char*)finbuf, 3);
	}
	return(found ? 0 : 1);
} /* end function rec_find() */

/** ***************************** */
/* Function check_ecrec_zip64() */
/** ***************************** */

int UnzOpr::check_ecrec_zip64(void)
{
	return fecrec.offset_start_central_directory == 0xFFFFFFFFL ||
		fecrec.size_central_directory == 0xFFFFFFFFL ||
		fecrec.total_entries_central_dir == 0xFFFF ||
		fecrec.num_entries_centrl_dir_ths_disk == 0xFFFF ||
		fecrec.num_disk_start_cdir == 0xFFFF || fecrec.number_this_disk ==
		0xFFFF;
} /* end function check_ecrec_zip64() */

/** ************************ */
/* Function find_ecrec64() */
/** ************************ */
#      define NUMBER_THIS_DISK64                16
#      define NUM_DISK_WITH_START_CEN_DIR64     20
#      define NUM_ENTRIES_CEN_DIR_THS_DISK64    24
#      define TOTAL_ENTRIES_CENTRAL_DIR64       32
#      define SIZE_CENTRAL_DIRECTORY64          40
#      define OFFSET_START_CENTRAL_DIRECTORY64  48

int UnzOpr::find_ecrec64(ZInt64 searchlen) /* return PK-class error */
{
	uch byterec[ECREC64_SIZE + 4];

	switch(rec_find(searchlen, EndCentral64Sig /* fend_central64_sig */ ,
			ECREC64_SIZE))
	{
	case 1:
		return PK_OK;
	case 2:
		// if (uO.qflag || uO.zipinfo_mode)
		// Info(slide, 0x401, ((char *)slide, "[%s]\n", fzipfn));
		Notify(IERROR, _T("end-of-central-dir64 signature not found [%s]"),
			fzipfn.c_str());
		// Info(slide, 0x401, ((char *)slide,
		// LoadFarString(Cent64EndSigSearchErr)));
		return PK_ERR;
		/* else: found ECREC64, continue! */
	}

	freal_ecrec_offset = fcur_zipfile_bufstart + (finptr - finbuf);

	if (readbuf((char*)byterec, ECREC64_SIZE + 4) == 0)
		return PK_EOF;

	if (fecrec.number_this_disk == 0xffff)
		fecrec.number_this_disk = makelong(&byterec[NUMBER_THIS_DISK64]);
	if (fecrec.num_disk_start_cdir == 0xffff)
		fecrec.num_disk_start_cdir = makelong
			(&byterec[NUM_DISK_WITH_START_CEN_DIR64]);
	if (fecrec.num_entries_centrl_dir_ths_disk == 0xffff)
		fecrec.num_entries_centrl_dir_ths_disk = makeint64
			(&byterec[NUM_ENTRIES_CEN_DIR_THS_DISK64]);
	if (fecrec.total_entries_central_dir == 0xffff)
		fecrec.total_entries_central_dir = makeint64
			(&byterec[TOTAL_ENTRIES_CENTRAL_DIR64]);
	if (fecrec.size_central_directory == 0xffffffff)
		fecrec.size_central_directory = makeint64
			(&byterec[SIZE_CENTRAL_DIRECTORY64]);
	if (fecrec.offset_start_central_directory == 0xffffffff)
		fecrec.offset_start_central_directory = makeint64
			(&byterec[OFFSET_START_CENTRAL_DIRECTORY64]);

	/* We know its a big file now. The "end of the central directory" mark
	used as break condition for the central-directory scan  is the
	"end_central64" signature. */
	fecrec.is_zip64_archive = TRUE;
	return PK_COOL;
} /* end function find_ecrec64() */

/* ===========================================================================
 *                    Function find_ecrec()
 */
int UnzOpr::find_ecrec(ZInt64 searchlen)
{
	/* return PK-class error */
	int // i, numblks,
	found = false;
	int result;
	// /*long*/ZInt64 tail_len;
	ec_byte_rec byterec;

	if (Verbose < 0)
		Notify(ITRACE, _T("in find_ecrec (end of central dir)"));

	/* ---------------------------------------------------------------------------
	 *    Treat case of short zipfile separately.
	 *-------------------------------------------------------------------------- */
	if (fziplen <= INBUFSIZ)
	{
		fUnzInfile->SetPosition(0L, SEEK_SET);
		if ((fincnt = vclRead((char*)finbuf, (unsigned int)fziplen)) == (int)
			fziplen)
		/* 'P' must be at least 22 bytes from end of zipfile */
			for (finptr = finbuf + (int)fziplen - 22; finptr >= finbuf;
				--finptr)
				if (*(ulg*)finptr == EndCentralDirSig)
				// if ((native(* finptr) == 'P') && !strncmp((char *) finptr, fend_central_sig, 4))
				{
					fincnt -= (int)(finptr - finbuf);
					found = true;
					break;
				}

		/* ---------------------------------------------------------------------------
		 *  Zipfile is longer than INBUFSIZ:  may need to loop.  Start with short
		 *  block at end of zipfile (if not TOO short).
		 *--------------------------------------------------------------------------- */
	}
	else
	{
		found = rec_find(searchlen, EndCentralDirSig /* fend_central_sig */ ,
			ECREC_SIZE) == 0;
	}
	/* end if (ziplen > INBUFSIZ) */

	/* ---------------------------------------------------------------------------
	 * Searched through whole region where signature should be without finding
	 * it.  Print informational message and die a horrible death.
	 *--------------------------------------------------------------------------- */
	// fail:
	if (!found)
	{
		// UnzErr(UEN_EOF01);
		DZError(DZ_ERM_NO_CENTRAL);
		return PK_ERR;
		/* failed */
	}

	/* ---------------------------------------------------------------------------
	 * Found the signature, so get the end-central data before returning.  Do
	 * any necessary machine-type conversions (byte ordering, structure padding
	 * compensation) by reading data into character array and copying to struct.
	 *--------------------------------------------------------------------------- */
	freal_ecrec_offset = fcur_zipfile_bufstart + (finptr - finbuf);

	if (Verbose < 0)
	{
		Notify(ITRACE, _T(
				"Found end-of-central-dir signature at offset %Ld (%.8LXh)"),
			freal_ecrec_offset, freal_ecrec_offset);
		Notify(ITRACE, _T(
				"    from beginning of file; offset %d (%.4Xh) within block"),
			finptr - finbuf, finptr - finbuf);
	}

	if (readbuf((char*)byterec, ECREC_SIZE + 4) == 0)
		return PK_EOF;
	fecrec.number_this_disk = makeword(&byterec[NUMBER_THIS_DISK]);
	fecrec.num_disk_start_cdir = makeword
		(&byterec[NUM_DISK_WITH_START_CENTRAL_DIR]);
	fecrec.num_entries_centrl_dir_ths_disk = makeword
		(&byterec[NUM_ENTRIES_CENTRL_DIR_THS_DISK]);
	fecrec.total_entries_central_dir = makeword
		(&byterec[TOTAL_ENTRIES_CENTRAL_DIR]);
	fecrec.size_central_directory = makelong(&byterec[SIZE_CENTRAL_DIRECTORY]);
	fecrec.offset_start_central_directory = makelong
		(&byterec[OFFSET_START_CENTRAL_DIRECTORY]);
	fecrec.zipfile_comment_length = makeword(&byterec[ZIPFILE_COMMENT_LENGTH]);

	if (check_ecrec_zip64())
	{
		result = find_ecrec64(searchlen + 76);
		/* 76 bytes for zip64ec & zip64 locator */
		if (result != PK_COOL)
			return result;
	}

	fexpect_ecrec_offset = fecrec.offset_start_central_directory +
		fecrec.size_central_directory;
	return PK_COOL;
}
/* end function find_ecrec() */

/* ===========================================================================
 *                    Function uz_end_central()
 * Get the zipfile comment (up to 64KB long), if any, and print it out.
 * Then position the file pointer to the beginning of the central directory
 * and fill buffer.
 * Return PK-type error code.
 */
int UnzOpr::uz_end_central(void)
{
	int error = PK_COOL;
	return error;
}

/* end function uz_end_central() */

/* ===========================================================================
 *                      Function process_cdir_file_hdr()
 * Return PK-type error code.
 * cdir = central dir of zipfile.
 */
int UnzOpr::process_cdir_file_hdr(void)
{
	int error;

	if (Verbose < 0)
		Notify(ITRACE, _T("in process_cdir_file_hdr()"));

	/* ---------------------------------------------------------------------------
	 * Get central directory info, save host and method numbers, and set flag
	 * for lowercase conversion of filename, depending on the OS from which the
	 * file is coming.
	 *--------------------------------------------------------------------------- */
	if ((error = get_cdir_ent()) != 0)
	{
		if (Verbose)
			Notify(ITRACE, _T("Error returned by get_cdir_ent call"));
		return error;
	}
	fpInfo->host = fcrec.made_by;
	int hostnum = fcrec.made_by >> 8;
	if (hostnum > NUM_HOSTS)
	{
		hostnum = NUM_HOSTS;
		fpInfo->host = (fpInfo->host & 255) | (NUM_HOSTS * 256);
	}

	if (Verbose < 0)
		Notify(ITRACE, _T("Good entry; hostnum of file = %d"), hostnum);

	/* is there a volume label? */
	if (IS_VOLID(fcrec.external_file_attributes) &&
		(hostnum == FS_FAT_ || hostnum == FS_HPFS_ || hostnum == FS_NTFS_ ||
			hostnum == ATARI_))
	{
		fpInfo->vollabel = true;
	}
	else
		fpInfo->vollabel = false;
	fpInfo->UTF8flag = (fcrec.general_purpose_bit_flag) & FLAG_UTF8_BIT ? 1 : 0;
	fpInfo->UNIXflag = fcrec.external_file_attributes & 0xFFFF0000u;
	if (Verbose < 0)
		Notify(ITRACE, _T("process_cdir_file_hdr returning PK_COOL"));
	return PK_COOL;
}
/* end function process_cdir_file_hdr() */

/* ===========================================================================
 *                          Function get_cdir_ent()
 * Return PK-type error code.
 */
int UnzOpr::get_cdir_ent(void)
{
	ZipCentralHeader zch;

	if (Verbose < 0)
		Notify(ITRACE, _T("in get_cdir_ent"));

	/* ---------------------------------------------------------------------------
	 * Read the next central directory entry and do any necessary machine-type
	 * conversions (byte ordering, structure padding compensation--do so by
	 * copying the data from the array into which it was read (byterec) to the
	 * usable struct (crec)).
	 *--------------------------------------------------------------------------- */
	if (readbuf((char*) & zch.MadeBy,
			sizeof(ZipCentralHeader)-sizeof(unsigned long)) == 0)
	{
		if (Verbose)
			Notify(ITRACE, _T("Central Header not found"));
		return PK_EOF;
	}

	fcrec.made_by = zch.MadeBy;
	fcrec.version_needed_to_extract[0] = zch.VersionNeeded[0];
	fcrec.version_needed_to_extract[1] = zch.VersionNeeded[1];

	fcrec.general_purpose_bit_flag = zch.Flag;
	fcrec.compression_method = zch.ComprMethod;
	fcrec.last_mod_file_time = zch.ModifTime;
	fcrec.last_mod_file_date = zch.ModifDate;
	fcrec.crc32 = zch.CRC32;
	fcrec.csize = zch.ComprSize;
	fcrec.ucsize = zch.UnComprSize;
	fcrec.filename_length = zch.FileNameLen;
	fcrec.extra_field_length = zch.ExtraLen;
	fcrec.file_comment_length = zch.FileComLen;
	fcrec.disk_number_start = zch.DiskStart;
	fcrec.internal_file_attributes = zch.IntFileAtt;
	fcrec.external_file_attributes = zch.ExtFileAtt;
	fcrec.relative_offset_local_header = zch.RelOffLocal;

	if (Verbose < 0)
		Notify(ITRACE, _T("Found Central Directory entry, filename of len %d"),
		fcrec.filename_length);

	return PK_COOL;
}

/* end function get_cdir_ent() */

/* ===========================================================================
 *                           Function process_local_file_hdr()
 */
int UnzOpr::process_local_file_hdr(void)
{
	/* return PK-type error code */
	// local_byte_hdr byterec;
	ZipLocalHeader zlh;

	/* ---------------------------------------------------------------------------
	 * Read the next local file header and do any necessary machine-type con-
	 * versions (byte ordering, structure padding compensation--do so by copy-
	 * ing the data from the array into which it was read (byterec) to the
	 * usable struct (lrec)).
	 *--------------------------------------------------------------------------- */
	if (readbuf((char*) & zlh.VersionNeed,
			sizeof(ZipLocalHeader)-sizeof(unsigned long)) == 0)
		return PK_EOF;

	flrec.version_needed_to_extract[0] = zlh.VersionNeeded[0];
	flrec.version_needed_to_extract[1] = zlh.VersionNeeded[1];

	flrec.general_purpose_bit_flag = zlh.Flag;
	flrec.compression_method = zlh.ComprMethod;
	flrec.last_mod_file_time = zlh.ModifTime;
	flrec.last_mod_file_date = zlh.ModifDate;
	flrec.crc32 = zlh.CRC32;
	flrec.csize = zlh.ComprSize;
	flrec.ucsize = zlh.UnComprSize;
	flrec.filename_length = zlh.FileNameLen;
	flrec.extra_field_length = zlh.ExtraLen;

	fcsize = flrec.csize;
	fucsize = flrec.ucsize;

	if ((flrec.general_purpose_bit_flag & FLAG_EXTEND_BIT) != 0)
	{
		/* can't trust local header, use central directory: */
		flrec.crc32 = fpInfo->crc;
		fcsize = flrec.csize = fpInfo->compr_size;
		fucsize = flrec.ucsize = fpInfo->uncomp_size;
	}
	if (Verbose < 0)
		Notify(ITRACE, _T("found Local Header entry, filename len of %d"),
		flrec.filename_length);
	return PK_COOL;
}

/* end function process_local_file_hdr() */

/** **************************** */
/* Function getZip64Data() */
/** **************************** */

int __fastcall UnzOpr::getZip64Data(const DZRawData& extra)
// uch *ef_buf, unsigned ef_len)
{
	unsigned eb_id;
	unsigned eb_len;

	/* ---------------------------------------------------------------------------
	This function scans the extra field for zip64 information, ie 8-byte
	versions of compressed file size, uncompressed file size, relative offset
	and a 4-byte version of disk start number.
	Sets both local header and central header fields.  Not terribly clever,
	but it means that this procedure is only called in one place.
	--------------------------------------------------------------------------- */

	if (!extra)
		return PK_COOL;

	const uch* ef_buf = extra.begin();
	unsigned ef_len = extra.Length();

	// TTrace((stderr,"\ngetZip64Data: scanning extra field of length %u\n", ef_len));

	while (ef_len >= EB_HEADSIZE)
	{
		eb_id = makeword(EB_ID + ef_buf);
		eb_len = makeword(EB_LEN + ef_buf);

		if (eb_len > (ef_len - EB_HEADSIZE))
		{
			/* discovered some extra field inconsistency! */
			if (Verbose) // < 0)
				Notify(ITRACE, _T(
					"Invalid extra data: block length %u > rest of block %u"),
				eb_len, ef_len - EB_HEADSIZE);
			break;
		}
		if (eb_id == ZIP64_XDATA_TAG)
		{

			int offset = EB_HEADSIZE;

			if (fcrec.ucsize == MAX_UNSIGNED || flrec.ucsize == MAX_UNSIGNED)
			{
				flrec.ucsize = fcrec.ucsize = makeint64(offset + ef_buf);
				offset += sizeof(fcrec.ucsize);
			}
			if (fcrec.csize == MAX_UNSIGNED || flrec.csize == MAX_UNSIGNED)
			{
				fcsize = flrec.csize = fcrec.csize = makeint64(offset + ef_buf);
				offset += sizeof(fcrec.csize);
			}
			if (fcrec.relative_offset_local_header == MAX_UNSIGNED)
			{
				fcrec.relative_offset_local_header = makeint64(offset + ef_buf);
				offset += sizeof(fcrec.relative_offset_local_header);
			}
			if (fcrec.disk_number_start == MAX_WORD)
			{
				fcrec.disk_number_start = makelong(offset + ef_buf);
				// offset += sizeof(fcrec.disk_number_start);
			}
		}

		/* Skip this extra field block */
		ef_buf += (eb_len + EB_HEADSIZE);
		ef_len -= (eb_len + EB_HEADSIZE);
	}

	return PK_COOL;
} /* end function getZip64Data() */
