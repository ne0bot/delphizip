//---------------------------------------------------------------------------

#ifndef ZipDfltH
#define ZipDfltH
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

#include "dzoper.h"
#include "zipasrt.h"
#include "helpers.h"

/* Set up portability
 * Anything defined in tailor.h will cause the default in this
 * header file to be ignored.
 */
#include <stdio.h>

/* MSDOS file attribute for directories */
#define MSDOS_DIR_ATTR  0x10


/* System independent replacement for "struct utimbuf",
 * which is missing in many older OS environments.
 */

typedef struct  ztimbuf
{
    time_t  actime;     /* new access time */
    time_t  modtime;    /* new modification time */
}ztimbuf;

#define MIN_MATCH 3
#define MAX_MATCH 258

/* The minimum and maximum match lengths */
// #ifndef WSIZE
#define ZWSIZE (0x8000)
// #endif

/* Maximum window size = 32K. If you are really short of memory, compile
 * with a smaller WSIZE but this reduces the compression ratio for files
 * of size > ZWSIZE. ZWSIZE must be a power of two in the current implementation.
 */
#define MIN_LOOKAHEAD (MAX_MATCH + MIN_MATCH + 1)

/* Minimum amount of lookahead, except at the end of the input file.
 * See deflate.c for comments about the MIN_MATCH + 1.
 */
#define MAX_DIST  (ZWSIZE - MIN_LOOKAHEAD)

/* In order to simplify the code, particularly on 16 bit machines, match
 * distances are limited to MAX_DIST instead of ZWSIZE.
 */
/* Forget FILENAME_MAX (incorrectly = 14 on some System V) */
#define FNMAX 256

/* Lengths of headers after signatures in bytes */
#define LOCHEAD 26
#define CENHEAD 42
#define ENDHEAD 18

/* internal file attribute */
#define UNKNOWN   (-1)
#define BINARY    0
#define ASCII     1
#define __EBCDIC  2


//#define BEST    - 1         // Use best method (deflation or store)
#define STORE   0           // Store method
#define DEFLATE 8           // Deflation method

#define HASH_BITS 15    // Number of bits used to hash strings
#define HASH_SIZE (unsigned)(1 << HASH_BITS)
#define HASH_MASK (HASH_SIZE - 1)
#define WMASK     (ZWSIZE - 1)

// HASH_SIZE and ZWSIZE must be powers of two
#define NIL 0

// Tail of hash chains
typedef ush Pos;

#pragma pack(push, 4)
struct ct_data{
    union
    {
        ush freq; // frequency count
        ush code; // bit string
    } fc;
    union
    {
        ush dad; // father node in Huffman tree
        ush len; // length of bit string
    } dl;
};
#pragma pack(pop)

#pragma pack(push, 4)
struct tree_desc
{
	ct_data *dyn_tree;    // the dynamic tree
    ct_data *static_tree; // corresponding static tree or NULL
	int *extra_bits;      // extra bits for each code or NULL
	int extra_base;       // base index for extra_bits
	int elems;            // max number of elements in the tree
	int max_length;       // max bit length for the codes
	int max_code;         // largest code with non zero frequency
};
#pragma pack(pop)

#define Freq          fc.freq
#define Code          fc.code
#define Dad           dl.dad
#define Len           dl.len

#define ADD           1
#define MAX_BITS      15  // All codes must not exceed MAX_BITS bits
#define MAX_BL_BITS   7   // Bit length codes must not exceed MAX_BL_BITS bits
#define LENGTH_CODES  29
// number of length codes, not counting the special END_BLOCK code
#define LITERALS      256 // number of literal bytes 0..255
#define END_BLOCK     256 // end of block literal code
#define L_CODES       (LITERALS + 1 + LENGTH_CODES)
// number of Literal or Length codes, including the END_BLOCK code
#define D_CODES       30                  // number of distance codes
#define BL_CODES      19
// number of codes used to transfer the bit lengths
#define HEAP_SIZE     (2 * L_CODES + 1) // maximum heap size
#define Max(a, b)   (a >= b ? a : b)
#define REP_3_6       16
#define REPZ_3_10     17
#define REPZ_11_138   18

extern int      extra_lbits[LENGTH_CODES];  // extra bits for each length code
extern int      extra_dbits[D_CODES];       // extra bits for each distance code
extern int      extra_blbits[BL_CODES];     // extra bits for each bit length code

#ifndef LIT_BUFSIZE
#define LIT_BUFSIZE 0x8000
#endif
#define DIST_BUFSIZE  LIT_BUFSIZE

/*************/
/*  ZGlobals  */
/*************/

class ZipDflt : public DZOp
{

    private:
        ZipDflt(void);
        ZipDflt(const ZipDflt&);
        ZipDflt& operator=(const ZipDflt&);

	protected:
		unsigned fcalls; 	// crypt static: ensure diff. random header each time
		ulg fkeys[3]; 		// crypt static: keys defining pseudo-random sequence
		const char* fkey; 	// crypt static: decryption password or NULL
#ifdef DEBUG
        unsigned fbits_sent;
#endif
        unsigned short fbi_buf;
		int fbi_valid;
        uch f_outbuf[4096];
		uch *fin_buf;
		unsigned fin_size;
        unsigned fin_offset;
        unsigned fout_offset;
        ulg fwindow_size;
        int flevel;
        int fsliding;
        Pos fhead[HASH_SIZE];
        Pos fprev[ZWSIZE];
        int fnice_match;
        unsigned fgood_match;
        unsigned fstrstart;
        unsigned flookahead;
        unsigned fins_h;
        unsigned fmatch_start;
        unsigned int fmax_chain_length;
        unsigned int fmax_lazy_match;
        unsigned int fprev_length;
        long fblock_start;
		int feofile;
        DZStrW fzipfile;
        int fdispose;
		HANDLE fhInz;     // handle to 'orig' zip file
        ZStreamIO *fZipInfile;
        ZStreamIO *fZipOutfile;

        tree_desc fl_desc;
        tree_desc fd_desc;
        tree_desc fbl_desc;
        ct_data fdyn_ltree[HEAP_SIZE]; // literal and length tree
        ct_data fdyn_dtree[2 *D_CODES + 1]; // distance tree
        ct_data fstatic_ltree[L_CODES + 2];
        ct_data fstatic_dtree[D_CODES];
        int *fextra_bits;
        int fextra_base;
        ct_data fbl_tree[2 *BL_CODES + 1];
        ush *ffile_type;
        int *ffile_method;
        int fbase_length[LENGTH_CODES];
        uch flength_code[MAX_MATCH - MIN_MATCH + 1];
        int fbase_dist[D_CODES];
        uch fdist_code[512];
        ush fbl_count[MAX_BITS + 1];
        ulg fopt_len;
        ulg fstatic_len;               // bit length of current block with static trees
        unsigned flast_lit;            // running index in l_buf
        unsigned flast_dist;           // running index in d_buf
        unsigned flast_flags;          // running index in flag_buf
        uch fflags;                    // current flags not yet saved in flag_buf
        uch fflag_bit;                 // current bit used in flags
        int fheap[2 *L_CODES + 1];     // heap used to build the Huffman trees
        int fheap_len;                 // number of elements in the heap
        int fheap_max;                 // element of largest frequency
        uch fdepth[2 *L_CODES + 1];
		uch fflag_buf[(LIT_BUFSIZE / 8)];
        uch fl_buf[LIT_BUFSIZE];       // buffer for literals/lengths
		ush fd_buf[DIST_BUFSIZE];      // buffer for distances
        int fFileError;                // 1.78.6.0
		ulg fcrc;
#ifdef DEBUG
        ZInt64 finput_len;
#endif
        ZInt64 fisize;
        ZInt64 fcompressed_len;
		ZInt64 fimax;
#ifdef DEBUG
        unsigned __int64 fToTest, fTested;
#endif

    public:
        uch fwindow[2L *ZWSIZE];
        ZipDflt(const DllCommands *C);
        ~ZipDflt(void);
		void    lm_init(int pack_level, ush *flags);
        ZInt64  deflate(void);
		void    crypthead(const char *, ulg);

    protected:
        unsigned __fastcall zfwrite(const uch *buf, ::extent);
        int read_buf(uch *buf, unsigned size);
		int file_read(uch *buf, unsigned size);
        /* in Trees.c */
        void              ct_init(ush *attr, int *method);
        void              bi_init(void);
		int __fastcall    ct_tally(int dist, int lc);
        ZInt64 __fastcall  FlushBlock(int eof);

        /* in Bits.c */
		void __fastcall     send_bits(int value, int length);
        void __fastcall     bi_windup(void);
		void __fastcall     copy_block(const uch *block, unsigned len, int header);
        int __fastcall      longest_match(unsigned cur_match);

    private:
        void __fastcall  flush_outbuf(unsigned w, unsigned bytes);
        void init_block(void);
        void __fastcall  pqdownheap(ct_data *tree, int k);
        void __fastcall  gen_bitlen(tree_desc *desc);
        void __fastcall  gen_codes(ct_data *tree, int max_code);
        void __fastcall  build_tree(tree_desc *desc);
        void __fastcall  scan_tree(ct_data *tree, int max_code);
        void __fastcall  send_tree(ct_data *tree, int max_code);
        int __fastcall   build_bl_tree(void);
        void             send_all_trees(int lcodes, int dcodes, int blcodes);
        void             compress_block(ct_data *ltree, ct_data *dtree);
        void             set_file_type(void);
        ZInt64          deflate_fast(void);
        void fill_window(void);
#ifdef _DEBUG
        void check_match(unsigned start, unsigned match, int length);
#endif
};

unsigned __fastcall bi_reverse(unsigned value, int length);

#endif




