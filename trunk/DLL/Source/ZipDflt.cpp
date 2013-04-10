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
#include "DZOper.h"
#include "ZipDflt.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPDFLT_CPP

//---------------------------------------------------------------------------
int         extra_lbits[LENGTH_CODES] // extra bits for each length code
=
{
    0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  2,  2,  2,  2,
    3,  3,  3,  3,  4,  4,  4,  4,  5,  5,  5,  5,  0
};

int         extra_dbits[D_CODES]      // extra bits for each distance code
=
{
    0,  0,  0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,
    7,  7,  8,  8,  9,  9,  10,  10,  11,  11,  12,  12,  13,  13
};

int         extra_blbits[BL_CODES]    // extra bits for each bit length code
= { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 7 };

ZipDflt::ZipDflt(const DllCommands *C): DZOp(C)
{
    flevel = 9;
    fhInz = INVALID_HANDLE_VALUE;
    fZipInfile = NULL;
	fZipOutfile = NULL;
    fcalls = 0; // crypt static: ensure diff. random header each time
    fkey = NULL; // crypt static: decryption password or NULL
#ifdef DEBUG
    fbits_sent = 0;
#endif
	fin_buf = NULL;
    fwindow_size = 0;
	fimax = 0;
	fzipfile = "";
	fdispose = 0;

    memset(fdyn_ltree, 0, sizeof(fdyn_ltree));
    memset(fdyn_dtree, 0, sizeof(fdyn_dtree));
    memset(fstatic_ltree, 0, sizeof(fstatic_ltree));
    memset(fstatic_dtree, 0, sizeof(fstatic_dtree));

    memset(&fl_desc, 0, sizeof(fl_desc));
    memset(&fd_desc, 0, sizeof(fd_desc));
    memset(&fbl_desc, 0, sizeof(fbl_desc));
    memset(fbl_tree, 0, sizeof(fbl_tree));
    memset(fbase_length, 0, sizeof(fbase_length));
    memset(flength_code, 0, sizeof(flength_code));
    memset(fbase_dist, 0, sizeof(fbase_dist));
    memset(fdist_code, 0, sizeof(fdist_code));
    memset(fbl_count, 0, sizeof(fbl_count));
    fopt_len = 0;
    fstatic_len = 0;
    flast_lit = 0;
    flast_dist = 0;
    flast_flags = 0;
    fflags = 0;
    fflag_bit = 0;
    memset(fheap, 0, sizeof(fheap));
    fheap_len = 0;
    fheap_max = 0;
    memset(fdepth, 0, sizeof(fdepth));
	memset(fflag_buf, 0, sizeof(fflag_buf));
	memset(fl_buf, 0, sizeof(fl_buf));
	memset(fd_buf, 0, sizeof(fd_buf));

    fl_desc.dyn_tree = fdyn_ltree;
    fl_desc.static_tree = fstatic_ltree;
    fl_desc.extra_bits = extra_lbits;
    fl_desc.extra_base = LITERALS + 1;
    fl_desc.elems = L_CODES;
    fl_desc.max_length = MAX_BITS;

    fd_desc.dyn_tree = fdyn_dtree;
    fd_desc.static_tree = fstatic_dtree;
    fd_desc.extra_bits = extra_dbits;
    fd_desc.elems = D_CODES;
    fd_desc.max_length = MAX_BITS;

    fbl_desc.dyn_tree = fbl_tree;
    fbl_desc.extra_bits = extra_blbits;
    fbl_desc.elems = BL_CODES;
	fbl_desc.max_length = MAX_BL_BITS;
}

ZipDflt::~ZipDflt(void)
{
    if (fZipInfile)
        delete fZipInfile;

    fZipInfile = NULL;

    if (fZipOutfile)
        delete fZipOutfile;

    fZipOutfile = NULL;
}

// Read a new buffer from the current input file, perform end-of-line
//   translation, and update the crc and input file size. IN assertion: size >=
//   2 (for end-of-line translation)
int ZipDflt::read_buf(unsigned char *buf, unsigned size)
{
    DWORD len, olen;

    if (fimax > 0 && (fisize + size) > fimax)
    {
        if (fisize < fimax)
            size = (unsigned)(fimax - fisize);
        else
            return 0;
    }

    if (!fZipInfile->Read(buf, size, &len))
    {
		fFileError = (int)GetLastError();
        return EOF;
    }

	olen = len;

    if (!len)
        return 0;

    fcrc = crc32(fcrc, (uch*)buf, (int)len);

    fisize += (ulg)len;

	CB->UserProgress(olen);

    return (int)len;
}

