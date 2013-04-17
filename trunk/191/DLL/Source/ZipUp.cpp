#include "stdafx.h"
#pragma hdrstop
#include "ZipFnc.h"
#include "Helpers.h"
#include <stdlib.h>
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPUP_CPP

/* ZipUp.c Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup
     Gailly, Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko. This
     version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
     distributed under LGPL license ** see license.txt for details

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

#include <share.h>

// Return the percentage compression from n to m using only integer
//   operations. n :: Is the original size. m :: Is the new size.
//int percent(ulg n, ulg m)
int percent(__int64 n, __int64 m)
{
    while (n > 0xFFFFFFL)
    {
        // If n >= 16M
        n += 0x80;
        n >>= 8; // then divide n and m by 256
        m += 0x80;
        m >>= 8;
    }

    return n > m ? (int)(1 + (200 *(n - m) / n)) / 2 : 0;
}

// Note: a zip "entry" includes a local header (which includes the file
//   name), an encryption header if encrypting, the compressed data and
//   possibly an extended local header.
// Compress the file z->name into the zip entry described by *z and write it
//   to the file. Encrypt if requested. Return an error code in the ZEN_
//   class. Also, update tempzn by the number of bytes written. z :: Zip entry
//   to compress.
// writes to fhOutz
#define OPENREADFLAGS (FILE_ATTRIBUTE_ARCHIVE | FILE_ATTRIBUTE_HIDDEN |  \
			FILE_ATTRIBUTE_READONLY | FILE_ATTRIBUTE_SYSTEM | \
			FILE_FLAG_SEQUENTIAL_SCAN)
			 //FILE_SHARE_READ |}
int ZipFunc::zipup(ZipItem *z)
{
	ulg tim;            // Time returned by filetime()
	ulg attr = 0L;      // Attributes returned by filetime()
	int k = 0;          // Result of zread
	int mthd;           // Method for this entry
	__int64 o, p;       // Offsets in zip file
	__int64 fsize = -3; // ZFT_SIZE; // Size returned by filetime
	int r;              // Temporary variable
	__int64 csize = 0L; // Size of compressed data
	int isdir;          // Set for a directory name
	int set_type = 0;   // Set if file type (ascii/binary) unknown
	unsigned char *tempextra; // to hold pointer returned by user
	ZInt64 HChk;        // position after local header
	AutoStream inz(NULL);

	fimax = 0;
	isdir = z->GetIsFolder();

	if (Verbose < 0)
		Notify(ITRACE, _T("zipup: %s"), z->Getiname());

	// get file position
	fOutPosn = fZipOutfile->SetPosition(0, 1);
	if (fOutPosn < 0)
		DZError(DZ_ERM_ERROR_SEEK);
	z->off = fOutPosn;

//	if (name.Compare(flabel.c_str()) == 0)
//	if (fvolume_label == 2 && name.Compare(flabel.c_str()) == 0)
	if (fvolume_label == 2 && flabel.Compare(z->xname) == 0)
	{
		attr = flabel_mode;
		fsize = ZFT_LABEL; // convention for a label name
		tim = flabel_time;
	}
	else
	{
		if (!zfiletime(z->FullPath(), &attr, &fsize, &tim)) //|| tim == 0)
			return DZ_ERM_NO_FILE_OPEN;
	}

	// fsize is set to -1 if the input file is a device, -2 for a volume label
	if (fsize == ZFT_LABEL)
	{
		isdir = 1;
		fsize = 0;
	}
	else
	if (isdir != ((attr & MSDOS_DIR_ATTR) != 0))
//	if (fsize != ZFT_LABEL && isdir != ((attr & MSDOS_DIR_ATTR) != 0))
	// don't overwrite a directory with a file and vice-versa
		return DZ_ERM_MISS;

	z->att = (ush)UNKNOWN; // will be changed later
	z->atx = 0; // may be changed by set_extra_field()

	fkey = z->Passw; // 1.73 restore global password
	fwindow_size = 0L;
	// Select method based on the suffix and the global method
	mthd = DEFLATE; // default method
	flevel = z->options.level;

	if (flevel && !z->options.noext && fSpecials)
	{
		ZFilter *f = fSpecials->Find(z->xname);
		if (f)
		{
			if (f->Level < flevel)
				flevel = f->Level;
		}
	}

	if (!flevel || isdir)
		mthd = STORE;

	// CHANGE 1.73 RAEL changed RPETERS Code added to generate and handle
	// action code 14 file extra data Extra data needs to be delivered
	// formatted according to PKZIP appnote.txt
	CB->SetArg1(z->GetExt()); // size
	CB->SetData2((const char*)z->extra.begin()); // old data
	CB->SetArg2(z->options.level);
	int uret = CB->UserCB(zacData, z->FullPath());
	if (uret <= CALLBACK_CANCEL) // TODO: should not happen
		Fatal(DZ_ERM_ABORT, 0);

	// free any old data - probably obsolete
	z->cextra.Empty();
	z->extra.Empty();
	if (z->ntfs)
	{
		delete z->ntfs;
		z->ntfs = NULL;
	}

	if (uret > 0 && uret & 2)
	{
		unsigned lvl = CB->GetArg2();

		if (lvl <= 9)
		{
			z->options.level = lvl;
			z->options.noext = 1;
			mthd = lvl ? DEFLATE : STORE;
			flevel = (int)lvl;
		}
	}

	if (uret > 0 && uret & 1)
	{
		// user changed extrafield data
		tempextra = (unsigned char*)CB->GetData();

		if (!tempextra)
			CB->SetArg1(0);

		// user changed extrafield data
		if (CB->GetArg1())
		{
			z->extra.Assign(tempextra, CB->GetArg1());
			if (fNTFSStamps)
				z->extra -= NTFS_STAMP_TAG; // - most likely wrong stamp
			z->cextra = z->extra;
		}
	}
	if (!z->options.dosflag && !z->GetHName().BadDOS()) 	// mimic WinZip
		z->options.dosflag = 2; // if has no extended chars use MSDOS

	// For a FAT file system, we cheat and pretend that the file was not made
	// on OS2, but under DOS. unzip is confused otherwise.
	// Made under MSDOS by PKZIP 2.0, NTFS by PKZIP 2.2
	z->vem = (ush)(z->options.dosflag ? 20 : OS_NTFS + 22);
	if (z->GetEnc() >= zeoUTF8)
		z->vem = OUR_VEM;

	// Need PKUNZIP 2.0 to extract, unless it is stored
	z->ver = (ush)((mthd == STORE) ? 10 : 20);

	CB->UserItem(z->len, z->GetXName());
	if (isdir)
	{
		if (/*IsNTorAbove &&*/ fNTFSStamps)
		{
			// get folder stamps
			HANDLE hFolder = CreateFile(z->FullPath().c_str(), GENERIC_WRITE,
				FILE_SHARE_READ, NULL, OPEN_EXISTING,
				FILE_ATTRIBUTE_DIRECTORY | FILE_FLAG_BACKUP_SEMANTICS, NULL);
			if (hFolder == INVALID_HANDLE_VALUE)
			{
				if (Verbose < 0)
					Notify(IWARNING, _T("Could not get times for %s"),
					z->FullPath().c_str());
			}
			else
			{
				// get stamps
				XNTFSData stamps;
				if (GetFileTime(hFolder, &stamps.CTime, &stamps.ATime,
						&stamps.MTime))
				{
					z->ntfs = new XNTFSData;
					memcpy(z->ntfs, &stamps, sizeof(XNTFSData));
				}
				CloseHandle(hFolder);
			}
		}
		// directory
		mthd = STORE;
		fsize = 0;
	}
	else
	{
		// Callback: action, error code, filesize, filename
//		CB->UserItem(z->len, z->XName);

		if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);

		fZipInfile = NULL;
		if (Is_Drv(z->GetXName()) < 0)
			fZipInfile = new ZStream(this, z->GetXName());
		else
			fZipInfile = new ZFile(this, z->FullPath(), OPENREADFLAGS);

		inz.Assign(&fZipInfile); // make certain it is destroyed when finished
		if (fZipInfile == NULL)
			Notify(ITRACE, _T("no input file"));

		if (fZipInfile != NULL && !fZipInfile->IsOpen())
		{
			int le = (int)GetLastError();
			if (Verbose < 0)
				Notify(IWARNING, _T("Could not open %s [%X]"),
				fZipInfile->fname.c_str(), le);
			// give zacSkipped
			int typ = SKIPPED_NO_OPEN;
			if (le == ERROR_SHARING_VIOLATION)
				typ = SKIPPED_NO_SHARE;
			else if (le == ERROR_ACCESS_DENIED)
				typ = SKIPPED_NO_ACCESS;

			if (Skipping(z->FullPath(), DZ_ERR_NO_FILE_OPEN, typ))
				Fatal(DZ_ERM_SKIPPING, 2);

			return DZ_ERR_NO_FILE_OPEN;
		}
		if (/*IsNTorAbove &&*/ fNTFSStamps)
		{
			// get stamps
			XNTFSData stamps;
			if (fZipInfile->GetTime(&stamps.CTime, &stamps.ATime,
					&stamps.MTime))
			{
				z->ntfs = new XNTFSData;
				memcpy(z->ntfs, &stamps, sizeof(XNTFSData));
			}
			else if (Verbose < 0)
				Notify(IWARNING, _T("Could not get times for %s"),
				fZipInfile->fname.c_str());
		}
	}

	z->tim = tim;
	fFileError = 0;
	if (fsize == 0)
		mthd = STORE;

	// Do not create STORED files with extended local headers if the input
	// size is not known, because such files could not be extracted. So if the
	// zip file is not seekable and the input file is not on disk, obey the -0
	// option by forcing deflation with stored block. Note however that using
	// "zip -0" as filter is not very useful... ??? to be done.
	// Fill in header information and write local header to zip file. This
	// header will later be re-written since compressed length and crc are not
	// yet known.
	// (Assume ext, cext, com, and zname already filled in.)
	// (RCV Added (ush)(...)
	z->flg = FLAG_EXTEND_BIT; // to be updated later

	// (RCV Added below (ush)
	z->how = (ush)mthd; // may be changed later

	if (z->att == (ush)UNKNOWN)
	{
		z->att = BINARY; // set sensible value in header
		set_type = 1;
	}

	// Attributes from filetime(), flag bits from set_extra_field():
	z->atx = z->options.dosflag ? attr & 0xFF : attr | (z->atx & 0x0000FFFF);
	z->crc = 0; // to be updated later
#ifdef _ZDEBUG
	Notify(ITRACE, _T("zipup: %s dosflag %i"), z->iname, z->options.dosflag);
#endif

	// Assume first that we will need an extended local header:
	ulg f_crc = 0;
	__int64 fsz = 0;
	bool haveCRC = false;

	if (fkey && !isdir && fsize)
	{
		if (!fNoPrecalc)
		{
			// get CRC before we start
			__int64 pos1 = 0;

			if (!fZipInfile->IsFile)
				pos1 = fZipInfile->SetPosition(0, FILE_CURRENT);
			// get start posn

			f_crc = crc32(0L, (uch*)NULL, 0);
			unsigned long byts;

			while (fZipInfile->Read(fwindow, sizeof(fwindow), &byts))
			{
				if (!byts)
					break;

				fsz += byts;
				f_crc = crc32(f_crc, (uch*)fwindow, (int)byts);
			}

			// Check input size
			if (fsz != fsize)
			{
				int skip;
				int re;
				// may be file error
				if (fFileError)
				{
					if (fFileError == ERROR_LOCK_VIOLATION)
						Notify(DZ_ERR_LOCKED | IWARNING, z->xname);
					else if (fFileError == ERROR_ACCESS_DENIED)
						Notify(DZ_ERR_DENIED | IWARNING, z->xname);
					else
						Notify(DZ_ERR_NO_FILE_OPEN | IWARNING,
						_T(" File read error [%d]: %s"), fFileError, z->xname);

					re = DZ_ERM_ERROR_READ;
					Notify((unsigned)re, _T(" File error [%d] while zipping: %s"), fFileError, z->xname);
					skip = SKIPPED_READ_ERROR;
				}
				else
				{
					re = DZ_ERM_SKIPPED;
					Notify(IWARNING, _T(" file size changed while zipping: %s"),
						z->xname);

					if (Verbose < 0)
						Notify(ITRACE, _T(" was=%Lu, expected=%Lu "), fisize, fsize);
					skip = SKIPPED_SIZE_CHANGE;
				}
				if (Skipping(z->GetXName(), 0, skip))
						Fatal(DZ_ERM_SKIPPED, 2);
				return re;		// skip the file
			}
			else
			{
				z->crc = f_crc;
				haveCRC = true;
			}

			if (fZipInfile->SetPosition(pos1, FILE_BEGIN) != pos1)
			{
				if (Verbose)
					Notify(IVERBOSE, _T("Could not reposition %s [%s]"),
					z->FullPath().c_str(), SysMsg().c_str());

				if (fZipInfile->IsFile)
				{
					inz.Assign(NULL);
					fZipInfile = new ZFile(this, z->FullPath(), OPENREADFLAGS);
					inz.Assign(&fZipInfile);

					if (fZipInfile == NULL || !fZipInfile->IsOpen())
						return DZError(DZ_ERM_ERROR_READ);
				}
				else
					return DZError(DZ_ERM_ERROR_READ);
			}
		}

		z->flg |= FLAG_ENCRYPT_BIT;
		// Since we do not yet know the crc here, we pretend that the crc is the
		// modification time:
		if (!haveCRC)
			z->crc = z->tim << 16;
	}

	fFileError = 0;
	if (fsize == 0)
		mthd = STORE;
	z->how = (ush)mthd; // may be changed later
	z->lflg = z->flg;
	z->siz = fsize; // not compressed yet
	z->len = fsize; // may be changed later
	z->dsk = 0;
//	// get file position
//	fOutPosn = fZipOutfile->SetPosition(0, 1);
//	if (fOutPosn < 0)
//		DZError(DZ_ERM_ERROR_SEEK);
//	z->off = fOutPosn;
////	Assert(fOutPosn == fZipOutfile->SetPosition(0, 1),
////		_T("invalid out posn 1"));

	// now put it in the file
	if ((r = PutLocal(z)) != DZ_ERR_GOOD)
		return r;

	HChk = fOutPosn; // save position after header
	if (HChk == -1)
		DZError(DZ_ERM_ERROR_WRITE);

	if (fkey)
	{
		crypthead(fkey, z->crc);
		z->siz += RAND_HEAD_LEN; // to be updated later
		fOutPosn += RAND_HEAD_LEN;
	}

	// for error checking, ftell can fail on pipes
	o = fOutPosn;
	// Write stored or deflated file to zip file
	fisize = 0; // L;
	fcrc = crc32(0L, (uch*)NULL, 0);

	if (fsize == 0)
		mthd = STORE;
	// Need PKUNZIP 2.0 to extract, unless it is stored
	z->ver = (ush)((mthd == STORE) ? 10 : 20);
	z->how = (ush)mthd; // may be changed later

	if (mthd == DEFLATE)
	{
		bi_init();

		if (set_type)
			z->att = (ush)UNKNOWN;

		// will be changed in deflate()
		ct_init(&z->att, &mthd);
		lm_init(flevel, &z->flg);

		// PERFORM THE DEFLATE
		csize = deflate();

		if (Abort_Flag)
			Fatal(DZ_ERM_ABORT, 0);
	}
	else if (!isdir)
	{
		if (Verbose)
			Notify(IVERBOSE, _T("Storing %s "), z->FullPath().c_str());

		while ((k = read_buf(fwindow, sizeof(fwindow))) > 0 && k != EOF)
		{
			if (Abort_Flag)
				Fatal(DZ_ERM_ABORT, 0);

			if (!zfwrite(fwindow, (extent)k))
				return DZ_ERM_TEMP_FAILED;
		}

		csize = fisize;
	}

	if (!fZipInfile && k == (-1))
		Notify(IWARNING, _T("could not read input file: %s"), z->xname);

	if (fZipInfile != NULL)
		fZipInfile->Close(); // Close the input file

	fOutPosn += csize;
	p = fOutPosn; // save for future fseek()

	if (haveCRC && f_crc != fcrc)
	{
		int re = DZ_ERM_ERROR_READ;
		Notify((unsigned)re, _T(" File CRC changed while zipping: %s"), z->xname);
		return re;
	}

	// Check input size
	if (fisize != fsize)
	{
		int skip;
		unsigned int re;
		// may be file error
		if (fFileError)
		{
			if (fFileError == ERROR_LOCK_VIOLATION)
				Notify(DZ_ERR_LOCKED | IWARNING, z->xname);
			else if (fFileError == ERROR_ACCESS_DENIED)
				Notify(DZ_ERR_DENIED | IWARNING, z->xname);
			else
				Notify(DZ_ERR_NO_FILE_OPEN | IWARNING,
				_T(" File read error [%d]: %s"), fFileError, z->xname);

			/*int*/ re = DZ_ERM_ERROR_READ;
			Notify(re, _T(" File error [%d] while zipping: %s"), fFileError, z->xname);
			skip = SKIPPED_READ_ERROR;
//			return re;
		}
		else
		{
			re = DZ_ERM_SKIPPED;
			Notify(IWARNING, _T(" file size changed while zipping: %s"),
				z->xname);

			if (Verbose < 0)
				Notify(ITRACE, _T(" was=%Lu, expected=%Lu "), fisize, fsize);
			skip = SKIPPED_SIZE_CHANGE;
		}
		if (Skipping(z->GetXName(), 0, skip))
				Fatal(DZ_ERM_SKIPPED, 2);
		// reposition
		o = fZipOutfile->SetPosition(z->off, FILE_BEGIN);
//		if (fZipOutfile->SetPosition(z->off, FILE_BEGIN) == -1)
		if (o == -1)
		{
			if (Verbose)// < 0)
				Notify(ITRACE, _T(" could not reposition file "));
			DZError(DZ_ERM_ERROR_SEEK);
		}
		fZipOutfile->SetEndOfFile();
		fOutPosn = o;
		return (int)re;
	}

	// Try to rewrite the local header with correct information
	z->crc = fcrc;
	z->siz = csize; // compressed size
	z->len = fisize;

	if (fkey)
		z->siz += RAND_HEAD_LEN;

	if (fZipOutfile->SetPosition(z->off, FILE_BEGIN) == -1)
	{
		if (z->how != (ush)mthd)
			DZError(DZ_ERM_ERROR_WRITE);

		putextended(z);
		z->flg = z->lflg; // if flg modified by inflate
	}
	else
	{
		// seek ok, ftell() should work, check compressed size
		if (p - o != csize)
		{
			Notify(IWARNING, _T(" s=%Lu, actual=%Lu "), csize, p - o);
			DZError(DZ_ERM_LOGIC_ERROR);
		}

		// (RCV Added in two lines below (ush)(...)
		z->how = (ush)mthd;

		if ((z->flg & FLAG_ENCRYPT_BIT) == 0 || haveCRC)
			z->flg &= ~FLAG_EXTEND_BIT;
		// clear the extended local header flag
		z->lflg = z->flg;

		// rewrite the local header:
		if ((r = UpdateLocal(z)) != DZ_ERR_GOOD)
			return r;

		if (HChk != fZipOutfile->SetPosition(0, FILE_CURRENT))
			return DZ_ERM_LOGIC_ERROR; // size changed

		fOutPosn = fZipOutfile->SetPosition(p, FILE_BEGIN);

		if (fOutPosn < p)
			return DZ_ERM_ERROR_SEEK;

		if ((z->flg & (FLAG_EXTEND_BIT | FLAG_ENCRYPT_BIT)) != 0)
		{
			// encrypted file, extended header still required
			if ((r = putextended(z)) != DZ_ERR_GOOD)
				return r;
		}
	}

	// Free the local extra field which is no longer needed
	if (z->GetExt())
		z->SetExt(0);

	// Display statistics
	if (Verbose)
	{
		Notify(0, _T("%s  in=%Lu,  out=%Lu,  %d%%"),
			(mthd == DEFLATE) ? _T("deflated") : _T("stored"), fisize, csize,
			percent(fisize, csize));
	}
	/* TODO 1 -oRP -cenhancement : Finished Item */
	return DZ_ERR_GOOD;
}


// copy an existing entry with updated name
int ZipFunc::zipVersion(ZipItem *z)
{
	int r;        // Temporary variable

    fwindow_size = 0L;

    // Callback: action, error code, filesize, filename
	CB->UserItem(z->len, z->GetIName());
    if (Abort_Flag)
		Fatal(DZ_ERM_ABORT, 0);

    fFileError = 0;
    ZInt64 lofs = z->off;   // local header offset
    lofs += sizeof(ZipLocalHeader) + z->nam + z->GetExt();

    // seek to local data
    if (SetFilePointer64(fhInz, lofs, FILE_BEGIN) == -1)
        return DZ_ERR_ERROR_READ;

    z->off = fOutPosn;
    ZInt64 n = z->siz;

    // copy the compressed data and the extended local header if there is one
    if (z->lflg & FLAG_EXTEND_BIT)
        n += (z->ver >= 45 ? 20 : 16);

    // write the modified header
    if ((r = PutLocal(z)) != DZ_ERR_GOOD)
        return r;

    fOutPosn += n;

//    return fcopy(n);
	int err = fcopy(n);

    /* TODO 1 -oRP -cenhancement : Finished Item */
	return err;
}
