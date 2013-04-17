#ifndef UnzInfH
#define UnzInfH
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
 *********************************************************************** */
#include "dzoper.h"
#include "unzsup.h"
#include "crypt.h"

#define INBUFSIZ  0x8000L // larger buffers for real OSes

// #ifndef WSIZE                   /* default is 32K */
#    define UWSIZE   65536L       // 64K for Inflate64
#  define wsize UWSIZE           /* wsize is a constant */

#define OUTBUFSIZ   0x8000      // EWE: 32K (for unshrink.c)
typedef int shrint;

extern const ush mask_bits[];

/* Huffman code lookup table entry--this entry is four bytes for machines
 * that have 16-bit pointers (e.g. PC's in the small or medium model).
 * Valid extra bits are 0..13.  e == 15 is EOB (end of block), e == 16
 * means that v is a literal, 16 < e < 32 means that v is a pointer to
 * the next table, which codes e - 16 bits, and lastly e == 99 indicates
 * an unused code.  If a code with e == 99 is looked up, this implies an
 * error in the data. */

#pragma pack(push, 4)
struct huft
{
	uch e; /* number of extra bits or operation */
	uch b;
	/* number of bits in this code or subcode */
	union
	{
		ush n; /* literal, length base, or distance base */
		struct huft *t; /* pointer to next level of table */
	} v;
};
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct local_file_header
{ /* LOCAL */
	uch version_needed_to_extract[2];
	ush general_purpose_bit_flag;
	ush compression_method;
	ush last_mod_file_time;
	ush last_mod_file_date;
	ulg crc32;
	ZInt64 csize;
	ZInt64 ucsize;
	ush filename_length;
	ush extra_field_length;
}local_file_hdr;
#pragma pack(pop)

#define CRCVAL_INITIAL  0L

class UnzInf : public UnzSup
{
private:
	UnzInf(void);
	UnzInf(const UnzInf&);
	UnzInf& operator=(const UnzInf&);

protected:
	int fP_flag; /* -P: give password on command line (ARGH!) */
	int fnopwd; /* crypt static */
	const char *fkey; /* crypt static: decryption password or NULL */
	DZStrA fpwdarg;
	uch Slide[UWSIZE]; /* explode(), inflate(), unreduce() */

	unsigned fhufts; /* track memory usage */

	struct huft *ffixed_tl; /* inflate static */
	struct huft *ffixed_td; /* inflate static */
	unsigned int ffixed_bl; /* inflate static */
	unsigned int ffixed_bd; /* inflate static */
	struct huft *ffixed_tl64; /* inflate static */
	struct huft *ffixed_td64; /* inflate static */
	unsigned ffixed_bl64, ffixed_bd64; /* inflate static */
	struct huft *ffixed_tl32; /* inflate static */
	struct huft *ffixed_td32; /* inflate static */
	unsigned ffixed_bl32, ffixed_bd32; /* inflate static */
	const ush *fcplens; /* inflate static */
	const uch *fcplext; /* inflate static */
	const uch *fcpdext; /* inflate static */
	unsigned fwp; /* inflate static: current position in slide */
	ulg fbb; /* inflate static: bit buffer */
	unsigned fbk; /* inflate static: bits in bit buffer */
	local_file_hdr flrec; /* used in unzip.c, extract.c */
	int fPwdReqCount;
//	int fnewzip;

	/* used in extract.c, crypt.c, zipinfo.c */
public:
	UnzInf(const DllCommands *C);
	~UnzInf(void);
	int inflate(bool defl64);
	int inflate_free(void);
	int explode(void);

protected:
	int __fastcall decrypt(ulg crc, bool noAsk);

private:
	int inflate_codes(struct huft *tl, struct huft *td, int bl, int bd);
	int huft_build(unsigned *b, unsigned n, unsigned s, const ush * d,
		const uch * e, struct huft **t, int *m);
	int inflate_stored(void);
	int inflate_fixed(void);
	int inflate_dynamic(void);
	int inflate_block(int *e);
	int get_tree(unsigned * l, unsigned n);
	int explode_lit8(struct huft * tb, struct huft * tl, struct huft * td,
		int bb, int bl, int bd);
	int explode_lit4(struct huft * tb, struct huft * tl, struct huft * td,
		int bb, int bl, int bd);
	int explode_nolit8(struct huft * tl, struct huft * td, int bl, int bd);
	int explode_nolit4(struct huft * tl, struct huft * td, int bl, int bd);
	int testkey(const char *h, const char *key, ulg crc);
};

int huft_free(struct huft * t);
#endif
