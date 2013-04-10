#include "stdafx.h"
#pragma hdrstop

#include <stdio.h>
#include "UnzInf.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UINFLATE_CPP
/*
  Inflate.c -

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

/* inflate.c -- put in the public domain by Mark Adler
 * This version modified by Chris Vleghert and Eric W. Engler
 * for BCB/Delphi Zip, Jun 18, 2000.
 */

/* Inflate deflated (PKZIP's method 8 compressed) data.  The compression
 * method searches for as much of the current string of bytes (up to a
 * length of 258) in the previous 32K bytes.  If it doesn't find any
 * matches (of at least length 3), it codes the next byte.  Otherwise, it
 * codes the length of the matched string and its distance backwards from
 * the current position.  There is a single Huffman code that codes both
 * single bytes (called "literals") and match lengths.  A second Huffman
 * code codes the distance information, which follows a length code.  Each
 * length or distance code actually represents a base value and a number
 * of "extra" (sometimes zero) bits to get to add to the base value.  At
 * the end of each deflated block is a special end-of-block (EOB) literal/
 * length code.  The decoding process is basically: get a literal/length
 * code; if EOB then done; if a literal, emit the decoded byte; if a
 * length then get the distance and emit the referred-to bytes from the
 * sliding window of previously emitted data.
 *
 * There are (currently) three kinds of inflate blocks: stored, fixed, and
 * dynamic.  The compressor outputs a chunk of data at a time and decides
 * which method to use on a chunk-by-chunk basis.  A chunk might typically
 * be 32K to 64K, uncompressed.  If the chunk is uncompressible, then the
 * "stored" method is used.  In this case, the bytes are simply stored as
 * is, eight bits per byte, with none of the above coding.  The bytes are
 * preceded by a count, since there is no longer an EOB code.
 *
 * If the data are compressible, then either the fixed or dynamic methods
 * are used.  In the dynamic method, the compressed data are preceded by
 * an encoding of the literal/length and distance Huffman codes that are
 * to be used to decode this block.  The representation is itself Huffman
 * coded, and so is preceded by a description of that code.  These code
 * descriptions take up a little space, and so for small blocks, there is
 * a predefined set of codes, called the fixed codes.  The fixed method is
 * used if the block ends up smaller that way (usually for quite small
 * chunks); otherwise the dynamic method is used.  In the latter case, the
 * codes are customized to the probabilities in the current block and so
 * can code it much better than the pre-determined fixed codes can.
 *
 * The Huffman codes themselves are decoded using a multi-level table
 * lookup, in order to maximize the speed of decoding plus the speed of
 * building the decoding tables.  See the comments below that precede the
 * lbits and dbits tuning parameters.
 * GRR:  return values(?)
 *         0  OK
 *         1  incomplete table
 *         2  bad input
 *         3  not enough memory
 */

/*
 * Notes beyond the 1.93a appnote.txt:
 *  1. Distance pointers never point before the beginning of the output
 *     stream.
 *  2. Distance pointers can point back across blocks, up to 32k away.
 *  3. There is an implied maximum of 7 bits for the bit length table and
 *     15 bits for the actual data.
 *  4. If only one code exists, then it is encoded using one bit.  (Zero
 *     would be more efficient, but perhaps a little confusing.)  If two
 *     codes exist, they are coded using one bit each (0 and 1).
 *  5. There is no way of sending zero distance codes--a dummy must be
 *     sent if there are none.  (History: a pre 2.0 version of PKZIP would
 *     store blocks with no distance codes, but this was discovered to be
 *     too harsh a criterion.)  Valid only for 1.93a.  2.04c does allow
 *     zero distance codes, which is sent as one code of zero bits in
 *     length.
 *  6. There are up to 286 literal/length codes.  Code 256 represents the
 *     end-of-block.  Note however that the static length tree defines
 *     288 codes just to fill out the Huffman codes.  Codes 286 and 287
 *     cannot be used though, since there is no length base or extra bits
 *     defined for them.  Similarily, there are up to 30 distance codes.
 *     However, static trees define 32 codes (all 5 bits) to fill out the
 *     Huffman codes, but the last two had better not show up in the data.
 *  7. Unzip can check dynamic Huffman blocks for complete code sets.
 *     The exception is that a single code would not be complete (see #4).
 *  8. The five bits following the block type is really the number of
 *     literal codes sent minus 257.
 *  9. Length codes 8,16,16 are interpreted as 13 length codes of 8 bits
 *     (1+6+6).  Therefore, to output three times the length, you output
 *     three codes (1+1+1), whereas to output four times the same length,
 *     you only need two codes (1+3).  Hmm.
 * 10. In the tree reconstruction algorithm, Code = Code + Increment
 *     only if BitLength(i) is not zero.  (Pretty obvious.)
 * 11. Correction: 4 Bits: # of Bit Length codes - 4     (4 - 19)
 * 12. Note: length code 284 can represent 227-258, but length code 285
 *     really is 258.  The last length deserves its own, short code
 *     since it gets used a lot in very redundant files.  The length
 *     258 is special since 258 - 3 (the min match length) is 255.
 * 13. The literal/length and distance code bit lengths are read as a
 *     single stream of lengths.  It is possible (and advantageous) for
 *     a repeat code (16, 17, or 18) to go across the boundary between
 *     the two sets of lengths.
 * 14. The Deflate64 (PKZIP method 9) variant of the compression algorithm
 *     differs from "classic" deflate in the following 3 aspect:
 *     a) The size of the sliding history window is expanded to 64 kByte.
 *     b) The previously unused distance codes #30 and #31 code distances
 *        from 32769 to 49152 and 49153 to 65536.  Both codes take 14 bits
 *        of extra data to determine the exact position in their 16 kByte
 *        range.
 *     c) The last lit/length code #285 gets a different meaning. Instead
 *        of coding a fixed maximum match length of 258, it is used as a
 *        "generic" match length code, capable of coding any length from
 *        3 (min match length + 0) to 65538 (min match length + 65535).
 *        This means that the length code #285 takes 16 bits (!) of uncoded
 *        extra data, added to a fixed min length of 3.
 *     Changes a) and b) would have been transparent for valid deflated
 *     data, but change c) requires to switch decoder configurations between
 *     Deflate and Deflate64 modes.
 */

//#define PKZIP_BUG_WORKAROUND    /* PKZIP 1.93a problem--live with it */

/*  inflate.h must supply the uch slide[UWSIZE] array, the void typedef
 *  (void if (void *) is accepted, else char) and the NEXTBYTE,
 *  FLUSH() and memzero macros.  If the window size is not 32K, it
 *  should also define UWSIZE.  If INFMOD is defined, it can include
 *  compiled functions to support the NEXTBYTE and/or FLUSH() macros.
 *  There are defaults for NEXTBYTE and FLUSH() below for use as
 *  examples of what those functions need to do.  Normally, you would
 *  also want FLUSH() to compute a crc on the data.  inflate.h also
 *  needs to provide these typedefs:
 *      typedef unsigned char  uch;
 *      typedef unsigned short ush;
 *      typedef unsigned long  ulg;
 */


/* The inflate algorithm uses a sliding 32K byte window on the uncompressed
 * stream to find repeated byte strings.  This is implemented here as a
 * circular buffer.  The index is updated simply by incrementing and then
 * and'ing with 0x7fff (32K-1).
 * It is left to other modules to supply the 32K area.  It is assumed
 * to be usable as if it were declared "uch slide[32768];" or as just
 * "uch *slide;" and then malloc'ed in the latter case.  The definition
 * must be in unzip.h, included above.
 */
#define INVALID_CODE 99
#define IS_INVALID_CODE(c)  ((c) == INVALID_CODE)
/* Tables for deflate from PKZIP's appnote.txt. */
static const unsigned border[]  =
  {      /* Order of the bit length code lengths */
    16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15
  };
static const ush cplens32[]  =
  {   /* Copy lengths for literal codes 257..285 */
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0
  };
/* note: see note #13 above about the 258 in this list. */
static const ush cplens64[] =
  {
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 3, 0, 0
  };
/* For Deflate64, the code 285 is defined differently. */
static const uch cplext32[]  =
  {   /* Extra bits for literal codes 257..285 */
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, INVALID_CODE, INVALID_CODE
  }
  ;                              /* 99==invalid */
static const uch cplext64[] =
  {
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 16, INVALID_CODE, INVALID_CODE
  };

static const ush cpdist[]  =
  {   /* Copy offsets for distance codes 0..29 */
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
    257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
    8193, 12289, 16385, 24577, 32769, 49153
  };
static const uch cpdext32[]  =
  {   /* Extra bits for distance codes */
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
    12, 12, 13, 13, INVALID_CODE, INVALID_CODE
  };
static const uch cpdext64[] =
  {
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
    12, 12, 13, 13, 14, 14
  };
#define MAXLITLENS 288
#define MAXDISTS 32

/* Macros for inflate() bit peeking and grabbing.
 * The usage is:
 *      NEEDBITS(j)
 *      x = b & mask_bits[j];
 *      DUMPBITS(j)
 * where NEEDBITS makes sure that b has at least j bits in it, and
 * DUMPBITS removes the bits from b.  The macros use the variable k
 * for the number of bits in b.  Normally, b and k are register
 * variables for speed and are initialized at the begining of a
 * routine that uses these macros from a global bit buffer and count.
 *
 * In order to not ask for more bits than there are in the compressed
 * stream, the Huffman tables are constructed to only ask for just
 * enough bits to make up the end-of-block code (value 256).  Then no
 * bytes need to be "returned" to the buffer at the end of the last
 * block.  See the huft_build() routine.
 */
                                                   
//#  define NEXTBYTE  (--fincnt >= 0 ? (int)(*finptr++) : readbyte())
#ifndef CHECK_EOF
#  define CHECK_EOF             /* default as of 5.13/5.2 */
#endif

#ifndef CHECK_EOF
#  define NEEDBITS(n) {while(k < (n)) {b |= ((ulg)NEXTBYTE) << k;k += 8;} }
#else
//#  define NEEDBITS(n) {while(k < (n)) {int c = NEXTBYTE; if (c == EOF) return 1;
//    b |= ((ulg)c) << k; k += 8;} }
#ifdef TRACE_INFLATE
#  define NEEDBITS(n) {while((int)k < (int)(n)) {int c = NEXTBYTE; if (c == EOF){\
     if ((int)k>=0)break;retval=1; \
     if (Verbose < 0) Notify(ITRACE, "eos %s", __LINE__); goto fini;} \
    b |= ((ulg)c) << k; k += 8;} }
#else
#  define NEEDBITS(n) {while((int)k < (int)(n)) {int c = NEXTBYTE; if (c == EOF){\
     if ((int)k>=0)break;retval=1;goto fini;} \
    b |= ((ulg)c) << k; k += 8;} }
#endif
#endif /* Piet Plomp:  change "return 1" to "break" */

#define DUMPBITS(n) {b >>= (n); k -= (n);}

/* Huffman code decoding is performed using a multi-level table lookup.
 * The fastest way to decode is to simply build a lookup table whose
 * size is determined by the longest code.  However, the time it takes
 * to build this table can also be a factor if the data being decoded
 * are not very long.  The most common codes are necessarily the
 * shortest codes, so those codes dominate the decoding time, and hence
 * the speed.  The idea is you can have a shorter table that decodes the
 * shorter, more probable codes, and then point to subsidiary tables for
 * the longer codes.  The time it costs to decode the longer codes is
 * then traded against the time it takes to make longer tables.
 *
 * This results of this trade are in the variables lbits and dbits
 * below.  lbits is the number of bits the first level table for literal/
 * length codes can decode in one step, and dbits is the same thing for
 * the distance codes.  Subsequent tables are also less than or equal to
 * those sizes.  These values may be adjusted either when all of the
 * codes are shorter than that, in which case the longest code length in
 * bits is used, or when the shortest code is *longer* than the requested
 * table size, in which case the length of the shortest code in bits is
 * used.
 *
 * There are two different values for the two tables, since they code a
 * different number of possibilities each.  The literal/length table
 * codes 286 possible values, or in a flat code, a little over eight
 * bits.  The distance table codes 30 possible values, or a little less
 * than five bits, flat.  The optimum values for speed end up being
 * about one bit more than those, so lbits is 8+1 and dbits is 5+1.
 * The optimum values may differ though from machine to machine, and
 * possibly even between compilers.  Your mileage may vary.
 */

static const int lbits = 9;     /* bits in base literal/length lookup table */
static const int dbits = 6;     /* bits in base distance lookup table       */
                                                          
//#  define NEXTBYTE  (--fincnt >= 0 ? (int)(*finptr++) : readbyte())
  
int huft_free(struct huft *t);

/* ===========================================================================
 * inflate (decompress) the codes in a deflated (compressed) block.
 * Return an error code or zero if it all goes ok.
        *tl, *td :: Literal/length and distance decoder tables.
         bl, bd  :: Number of bits decoded by tl[] and td[].
 */
int UnzInf::inflate_codes(struct huft *tl, struct huft *td, int bl, int bd)
{
  register unsigned e;          /* table entry flag/number of extra bits  */
  unsigned n, d;                /* length and index for copy */
  unsigned w;                   /* current window position */
  struct huft *t;               /* pointer to table entry */
  unsigned ml, md;              /* masks for bl and bd bits */
  register ulg b;               /* bit buffer   */
  register unsigned k;          /* number of bits in bit buffer   */
  int retval = 0;

#ifdef TRACE_INFLATE
  if (Verbose < 0)
    Notify(ITRACE, _T("inflate codes"));
#endif
  /* make local copies of globals */
  b = fbb;                   /* initialize bit buffer    */
  k = fbk;
  w = fwp;                   /* initialize window position */

  /* inflate the coded data */
  ml = mask_bits[bl];           /* precompute masks for speed */
  md = mask_bits[bd];

  while (1)
  {                   /* do until end of block   */
    NEEDBITS((unsigned)  bl)
    t = tl + ((unsigned)  b & ml);
    while (1)
    {
      DUMPBITS(t->b)

      if ((e = t->e) == 32)     /* then it's a literal */
      {
        Slide[w++] = (uch)t->v.n;
//#ifdef USE_STRM_OUTPUT
//		if (fredirect_data && !(w % 0x8000))    // RCV1.6019
//		{
//		  // bump up progress bar
//		  UserProgress(0x8000);
//		}
//#endif
		if (w == wsize)
		{
//		  if ((retval = FLUSH(w)) != 0)
		  if ((retval = flush(Slide, (ulg)(w), 0)) != 0)
			goto fini;
          w = 0;
        }
        break;
      }

      if (e < 31)               /* then it's a length */
      {
        /* get length of block to copy */
        NEEDBITS(e)
        n = t->v.n + ((unsigned)b & mask_bits[e]);
        DUMPBITS(e)

        /* decode distance of block to copy */
        NEEDBITS(bd)
        t = td + ((unsigned)b & md);
        while (1)
        {
          DUMPBITS(t->b)
          if ((e = t->e) < 32)
            break;
          if (IS_INVALID_CODE(e))
            return 1;
          e &= 31;
          NEEDBITS(e)
          t = t->v.t + ((unsigned)b & mask_bits[e]);
        }
        NEEDBITS(e)
        d = (unsigned)w - t->v.n - ((unsigned)b & mask_bits[e]);
        DUMPBITS(e)

        /* do the copy */
        do
        {
//#ifdef USE_STRM_OUTPUT
//		  if (fredirect_data && !fUseInStream)
//		  {       /* &= w/ wsize unnecessary & wrong if redirect      */
//			if (d >= wsize)
//			  return 1;   // invalid compression data
//			e = (unsigned)(wsize - (d > (unsigned)w ? (ulg)d : w));
//		  }
//		  else
//#endif
            e = (unsigned)(wsize -
                           ((d &= (unsigned)(wsize-1)) > (unsigned)w ?
                            (ulg)d : w));
          if ((ulg)e > n)
            e = (unsigned)n;
          n -= e;
//#ifndef NOMEMCPY
		  if (w - d >= e)
          {       /* (this test assumes unsigned comparison)              */
            memcpy(Slide + w, Slide + d, e);
//#ifdef USE_STRM_OUTPUT
//			if (fredirect_data && ((w + e)  / 0x8000 - w / 0x8000))      // RCV1.6022
//			{
//			  // bump up progress bar
//			  UserProgress(0x8000);
//			}
//#endif
            w += e;
            d += e;
          }
          else                    /* do it slowly to avoid memcpy() overlap */
//#  endif /* !NOMEMCPY */
			do
            {
              Slide[w++]  = Slide[d++];
//# ifdef USE_STRM_OUTPUT
//			  if (fredirect_data && !(w % 0x8000))      // RCV1.6019 1.6022
//			  {
//				// bump up progress bar
//				UserProgress(0x8000);
//			  }
//#  endif
            }
            while (--e);


          if (w == wsize)
		  {
//			if ((retval = FLUSH(w)) != 0)
			if ((retval = flush(Slide, (ulg)(w), 0)) != 0)
              goto fini;
            w = 0;
          }
        }
        while (n);
        break;
      }

      if (e == 31)              /* it's the EOB signal */
      {
        /* sorry for this goto, but we have to exit two loops at once */
        goto clean1;
      }

      if (IS_INVALID_CODE(e))
        return 1;

      e &= 31;
      NEEDBITS(e)
      t = t->v.t + ((unsigned)b & mask_bits[e]);
    }
  }
clean1:
  /* restore the globals from the locals */
  fwp = w;                   /* restore global window pointer */
  fbb = b;                   /* restore global bit buffer */
  fbk = k;
fini:
#ifdef TRACE_INFLATE
  if (Verbose < 0)
    Notify(ITRACE, _T("inflate_codes returning %d"), retval);
#endif
  return retval;
}


/* ===========================================================================
 * "decompress" an inflated type 0 (stored) block.
 */
int UnzInf::inflate_stored(void)
{
  unsigned n;                   /* number of bytes in block    */
  unsigned w;                   /* current window position   */
  register ulg b;               /* bit buffer      */
  register unsigned k;          /* number of bits in bit buffer   */
  int retval = 0;

  /* make local copies of globals */
#ifdef TRACE_INFLATE
  if (Verbose < 0)
    Notify(ITRACE, _T("extracting files from stored block"));
#endif
  b = fbb;                   /* initialize bit buffer      */
  k = fbk;
  w = fwp;                   /* initialize window position */

  /* go to byte boundary */
  n = k & 7;
  DUMPBITS(n);

  /* get the length and its complement */
  NEEDBITS(16)
  n = ((unsigned)  b & 0xffff);
  DUMPBITS(16)
  NEEDBITS(16)
  if (n != (unsigned)((~b)  & 0xffff))
  {
#ifdef TRACE_INFLATE
  if (Verbose < 0)
    Notify(ITRACE, _T("error in compressed stored data"));
#endif
    return 1;                   /* error in compressed data */
  }
  DUMPBITS(16)

  /* read and output the compressed data */
  while (n--)
  {
    NEEDBITS(8)
    Slide[w++]  = (uch)  b;
//# ifdef USE_STRM_OUTPUT
//	if (fredirect_data && !(w % 0x8000))      // RCV1.6019
//	{
//	  // bump up progress bar
//	  UserProgress(0x8000);
//	}
//#  endif
    if (w == wsize)
	{
//	  if ((retval = FLUSH(w)) != 0)
	  if ((retval = flush(Slide, (ulg)(w), 0)) != 0)
        goto fini;
      w = 0;
    }
    DUMPBITS(8)
  }
  /* restore the globals from the locals */
  fwp = w;                   /* restore global window pointer */
  fbb = b;                   /* restore global bit buffer */
  fbk = k;
fini:
  return retval;
}


/* ===========================================================================
 * decompress an inflated type 1 (fixed Huffman codes) block.  We should
 * either replace this with a custom decoder, or at least precompute the
 * Huffman tables.
 */
int UnzInf::inflate_fixed(void)
{
  /* if first time, set up tables for fixed blocks */
#ifdef TRACE_INFLATE
  if (Verbose < 0)
    Notify(ITRACE, _T("literal block"));
#endif
  if (ffixed_tl == (struct huft *)  NULL)
  {
    int i;                      /* temporary variable                      */
    unsigned l[288];            /* length list for huft_build */

    /* literal table */
    for (i = 0; i < 144; i++)
      l[i]  = 8;
    for (; i < 256; i++)
      l[i]  = 9;
    for (; i < 280; i++)
      l[i]  = 7;
    for (; i < 288; i++)
      l[i]  = 8;                 /* make a complete, but wrong code set */

    ffixed_bl = 7;
    if ((i =
           huft_build(l, 288, 257, fcplens, fcplext, &ffixed_tl,
                      (int*)&ffixed_bl))  != 0)
    {
      ffixed_tl = (struct huft *)  NULL;
      return i;
    }
    /* distance table */
    for (i = 0; i < MAXDISTS; i++)
      l[i]  = 5;                 /* make an incomplete code set */

    ffixed_bd = 5;
    if ((i =
           huft_build(l, MAXDISTS, 0, cpdist, fcpdext, &ffixed_td,
                     (int*) &ffixed_bd))  > 1)
    {
      huft_free(ffixed_tl);
      ffixed_tl = (struct huft *)  NULL;
      return i;
    }
  }
  /* Decompress until an end-of-block code. */
  return inflate_codes(ffixed_tl, ffixed_td, (int)ffixed_bl,
                       (int)ffixed_bd)  != 0;
}


/* ===========================================================================
 * decompress an inflated type 2 (dynamic Huffman codes) block.
 */
int UnzInf::inflate_dynamic(void)
{
  int i;                        /* temporary variables    */
  unsigned j;
  unsigned l;                   /* last length     */
  unsigned m;                   /* mask for bit lengths table   */
  unsigned n;                   /* number of lengths to get   */
  int bl;                       /* lookup bits for tl      */
  int bd;                       /* lookup bits for td      */
  unsigned nb;                  /* number of bit length codes     */
  unsigned nl;                  /* number of literal/length codes   */
  unsigned nd;                  /* number of distance codes    */
  int retval;// =0;
  struct huft *tl = NULL;              /* literal/length code table     */
  struct huft *td = NULL;              /* distance code table         */
//#ifdef PKZIP_BUG_WORKAROUND
  unsigned ll[288 + MAXDISTS]; /* literal/length and distance code lengths */
//#else
//  unsigned ll[286 + 30];        /* literal/length and distance code lengths */
//#endif
  register ulg b;               /* bit buffer */
  register unsigned k;          /* number of bits in bit buffer */

  /* make local bit buffer */
#ifdef TRACE_INFLATE
  if (Verbose < 0)
    Notify(ITRACE, _T("in inflate_dynamic"));
#endif
  b = fbb;
  k = fbk;

  /* read in table lengths */
  NEEDBITS(5)
  nl = 257 + ((unsigned)  b & 0x1f);   /* number of literal/length codes */
  DUMPBITS(5)
  NEEDBITS(5)
  nd = 1 + ((unsigned)  b & 0x1f);     /* number of distance codes */
  DUMPBITS(5)
  NEEDBITS(4)
  nb = 4 + ((unsigned)  b & 0xf);      /* number of bit length codes */
  DUMPBITS(4)
//#ifdef PKZIP_BUG_WORKAROUND
  if (nl > MAXLITLENS || nd > MAXDISTS)
//#else
//  if (nl > 286 || nd > 30)
//#endif
    return 1;                   /* bad lengths */

  /* read in bit-length-code lengths */
  for (j = 0; j < nb; j++)
  {
    NEEDBITS(3)
    ll[border[j]]  = (unsigned)  b & 7;
    DUMPBITS(3)
  }
  for (; j < 19; j++)
    ll[border[j]]  = 0;

  /* build decoding table for trees--single level, 7 bit lookup */
  bl = 7;
  retval = huft_build(ll, 19, 19, NULL, NULL, &tl, &bl);
  if (bl == 0)
    retval = 1;
  if (retval)
  {
    if (retval == 1)
      huft_free(tl);
    return retval;                   /* incomplete code set */
  }

  /* read in literal and distance code lengths */
  n = nl + nd;
  m = mask_bits[bl];
  i = l = 0;

  while ((unsigned)  i < n)
  {
    NEEDBITS((unsigned)  bl)
    j = (td = tl + ((unsigned)  b & m)) ->b;
    DUMPBITS(j)
    j = td->v.n;

    if (j < 16)                  /* length of code in bits (0..15)       */
      ll[i++]  = l = j;          /* save last length in l         */
    else
      if (j == 16)
      {         /* repeat last length 3 to 6 times */
        NEEDBITS(2)
        j = 3 + ((unsigned)  b & 3);
        DUMPBITS(2)
        if ((unsigned)  i + j > n)
        {
          huft_free(tl);
          return 1;
        }
        while (j--)
          ll[i++]  = l;
      }
      else
        if (j == 17)
        {         /* 3 to 10 zero length codes */
          NEEDBITS(3)
          j = 3 + ((unsigned)  b & 7);
          DUMPBITS(3)
          if ((unsigned)  i + j > n)
          {
            huft_free(tl);
            return 1;
          }
          while (j--)
            ll[i++]  = 0;
          l = 0;
        }
        else
        {                      /* j == 18: 11 to 138 zero length codes */
          NEEDBITS(7)
          j = 11 + ((unsigned)  b & 0x7f);
          DUMPBITS(7)
          if ((unsigned)  i + j > n)
          {
            huft_free(tl);
            return 1;
          }
          while (j--)
            ll[i++]  = 0;
          l = 0;
        }
  }

  /* free decoding table for trees */
  huft_free(tl);

  /* restore the global bit buffer */
  fbb = b;
  fbk = k;

  /* build the decoding tables for literal/length and distance codes */
  bl = lbits;
  retval = huft_build(ll, nl, 257, fcplens, fcplext, &tl, &bl);
  if (bl == 0)
    retval = 1;
  if (retval)
  {
    if (retval == 1 && !fqflag)
    {
      Notify(0, _T("Fatal error: incomplete l - tree"));
      huft_free(tl);
    }
    return retval;                   /* incomplete code set */
  }

  bd = dbits;
  retval = huft_build(ll + nl, nd, 0, cpdist, fcpdext, &td, &bd);
  if (retval == 1)
    retval =0;
  if (bd == 0 && nl > 257)   // lengths but no distances
    retval = 1;
  if (retval)
  {
    if (retval == 1)
    {
      if (!fqflag)
        Notify(0, _T("Fatal Error: incomplete d-tree"));
//#ifdef PKZIP_BUG_WORKAROUND
//                            i = 0;  RCV not used later on why???
//#else
      huft_free(td);
//#endif
    }
//#ifndef PKZIP_BUG_WORKAROUND
    huft_free(tl);
    return retval;                   /* incomplete code set */
//#endif

  }
  /* decompress until an end-of-block code */
  retval = inflate_codes(tl, td, bl, bd);
fini:
  /* free the decoding tables, return */
  huft_free(tl);
  huft_free(td);
#ifdef TRACE_INFLATE
  if (Verbose < 0)
    Notify(ITRACE, _T("inflate_dynamic returning %d"), retval);
#endif
  return retval;
}


/* ===========================================================================
 * decompress an inflated block
        *e :: Last block flag.
 */
int UnzInf::inflate_block(int *e)
{
  unsigned t;                   /* block type */
  register ulg b;               /* bit buffer */
  register unsigned k;          /* number of bits in bit buffer */
  int retval = 2;               /* bad block type */

  /* make local bit buffer */
  b = fbb;
  k = fbk;

  /* read in last block bit */
  NEEDBITS(1)
  * e = (int)  b & 1;
  DUMPBITS(1)

  /* read in block type */
  NEEDBITS(2)
  t = (unsigned)  b & 3;
  DUMPBITS(2)

  /* restore the global bit buffer */
  fbb = b;
  fbk = k;

  /* inflate that block type */
  if (t == 2)
    return inflate_dynamic();
  if (t == 0)
    return inflate_stored();
  if (t == 1)
    return inflate_fixed();

fini:
  return retval;
}


/* ===========================================================================
 * Main entry to inflate a compressed file
 * decompress an inflated entry
 */
int UnzInf::inflate(bool defl64)
{
  int e;                        /* last block flag */
  int retval;                   /* result code     */
  unsigned h;                   /* maximum struct huft's malloc'ed */

//#ifdef USE_STRM_OUTPUT
//  if (fredirect_data)
//  {
//    wsize = fredirect_size;
//    Slide = fredirect_pointer;
//  }
//  else
//  {
//    wsize = UWSIZE;
//    Slide = Slide;
//  }
//#else
////    wsize = UWSIZE;
////    Slide = Slide;
//#endif

  if (Verbose < 0)
    Notify(ITRACE, defl64? _T("starting inflate64") : _T("starting inflate"));
  if (defl64)
  {
    fcplens = cplens64;
    fcplext = cplext64;
    fcpdext = cpdext64;
    ffixed_tl = ffixed_tl64;
    ffixed_bl = ffixed_bl64;
	ffixed_td = ffixed_td64;
	ffixed_bd = ffixed_bd64;
  }
  else
  {
    fcplens = cplens32;
    fcplext = cplext32;
    fcpdext = cpdext32;
    ffixed_tl = ffixed_tl32;
    ffixed_bl = ffixed_bl32;
	ffixed_td = ffixed_td32;
	ffixed_bd = ffixed_bd32;
  }
  /* initialize window, bit buffer */
  fwp = 0;
  fbk = 0;
  fbb = 0;

  /* decompress until the last block */
  h = 0;
  do
  {
    fhufts = 0;
    if ((retval = inflate_block(&e))  != 0)
    {
      e = 888;  // break loop
      if (Verbose < 0)
        Notify(ITRACE,
               _T("inflate_block returned poss error=%d, inflate will also return it"),  retval);
      break;
    }
    if (fhufts > h)
      h = fhufts;
    if (Abort_Flag)
    {
      retval = DZ_ERM_ABORT;//UEN_ABORT03;
      e = 999;  //break loop
      break;
    }
  }
  while (!e);

  if (defl64)
  {
    ffixed_tl64 = ffixed_tl;
    ffixed_bl64 = ffixed_bl;
	ffixed_td64 = ffixed_td;
	ffixed_bd64 = ffixed_bd;
  }
  else
  {
    ffixed_tl32 = ffixed_tl;
    ffixed_bl32 = ffixed_bl;
	ffixed_td32 = ffixed_td;
	ffixed_bd32 = ffixed_bd;
  }

  /* flush out Slide */
  if (!retval)
	retval = flush(Slide, (ulg)(fwp), 0);
//	retval = FLUSH(fwp);

  /* return success */
  if (!retval && Verbose < 0)
    Notify(ITRACE, _T("NO ERROR - %u bytes in Huffman tables (%d/entry)"),
           h * sizeof(struct huft), sizeof(struct huft));
  return retval;
}


/* ===========================================================================
 */
int UnzInf::inflate_free(void)
{
  if (ffixed_tl != (struct huft *)  NULL)
  {
    huft_free(ffixed_td);
    huft_free(ffixed_tl);
    ffixed_td = ffixed_tl = (struct huft *)  NULL;
  }
  return 0;
}


/*
 * GRR:  moved huft_build() and huft_free() down here; used by explode()
 */

/* If BMAX needs to be larger than 16, then h and x[] should be ulg. */
#define BMAX 16                 /* maximum bit length of any code (16 for explode) */
#define N_MAX 288               /* maximum number of codes in any set */

/* ===========================================================================
 * Given a list of code lengths and a maximum table size, make a set of
 * tables to decode that set of codes.  Return zero on success, one if
 * the given code set is incomplete (the tables are still built in this
 * case), two if the input is invalid (all zero length codes or an
 * oversubscribed set of lengths), and three if not enough memory.
 * The code with value 256 is special, and the tables are constructed
 * so that no bits beyond that code are fetched when that code is
 * decoded.
         *b :: Code lengths in bits (all assumed <= BMAX).
          n :: Number of codes (assumed <= N_MAX).
          s :: Number of simple-valued codes (0..s-1).
         *d :: List of base values for non-simple codes.
         *e :: List of extra bits for non-simple codes.
        **t :: Result: starting table.
         *m :: Maximum lookup bits, returns actual.
 */
int UnzInf::huft_build(unsigned *b, unsigned n, unsigned s,
		   const ush *d, const uch *e, struct huft **t, int *m)
{
  unsigned a;                   /* counter for codes of length k   */
  unsigned c[BMAX + 1];         /* bit length count table       */
  unsigned el;                  /* length of EOB code (value 256)   */
  unsigned f;                   /* i repeats in table every f entries    */
  int g;                        /* maximum code length     */
  int h;                        /* table level         */
  register unsigned i;          /* counter, current code      */
  register unsigned j;          /* counter        */
  register int k;               /* number of bits in current code    */
  int lx[BMAX + 1];             /* memory for l[-1..BMAX-1]  */
  int *l = lx + 1;              /* stack of bits per table       */
  register unsigned *p;         /* pointer into c[], b[], or v[]     */
  register struct huft *q;      /* points to current table          */
  struct huft r;                /* table entry for structure assignment   */
  struct huft *u[BMAX];         /* table stack       */
  unsigned v[N_MAX];            /* values in order of bit length       */
  register int w;               /* bits before this table == (l * h)     */
  unsigned x[BMAX + 1];         /* bit offsets, then code stack      */
  unsigned *xp;                 /* pointer into x         */
  int y;                        /* number of dummy codes added      */
  unsigned z;                   /* number of entries in current table    */

  /* Generate counts for each bit length */
  el = n > 256 ? b[256]  : BMAX; /* set length of EOB code, if any */
  ZeroMemory((char *)  c, sizeof(c));
  p = b;
  i = n;

  do
  {
    c[*p]++;
    p++;                        /* assume all entries <= BMAX */
  }
  while (--i);

  if (c[0]  == n)
  {              /* null input--all zero length codes */
	*t = (struct huft *) NULL;
    *m = 0;
    return 0;
  }

  /* Find minimum and maximum length, bound *m by those */
  for (j = 1; j <= BMAX; j++)
    if (c[j])
      break;

  k = (int)j;                        /* minimum code length */
  if ((unsigned)*m < j)
	*m = (int)j;

  for (i = BMAX; i; i--)
    if (c[i])
      break;

  g = (int)i;                        /* maximum code length */
  if ((unsigned)*m > i)
	*m = (int)i;

  /* Adjust last length count to fill out codes, if needed */
  for (y = 1 << j; j < i; j++, y <<= 1)
    if ((y -= c[j])  < 0)
      return 2;                 /* bad input: more codes than bits */

  if ((y -= c[i])  < 0)
    return 2;
  c[i]  += (unsigned)y;

  /* Generate starting offsets into the value table for each length */
  x[1]  = j = 0;
  p = c + 1;
  xp = x + 2;

  while (--i)
  {                 /* note that i == g from above */
    *xp++ = (j += *p++);
  }

  /* Make a table of values in order of bit lengths */
  ZeroMemory((char *)v, sizeof(v));
  p = b;
  i = 0;

  do
  {
    if ((j = *p++)  != 0)
      v[x[j] ++]  = i;
  }
  while (++i < n);
  n = x[g];                     /* set n to length of v */

  /* Generate the Huffman codes and for each, make the table entries */
  x[0]  = i = 0;                 /* first Huffman code is zero     */
  p = v;                        /* grab values in bit order     */
  h = -1;                       /* no tables yet--level -1  */
  w = l[-1]  = 0;                /* no bits decoded yet     */
  u[0]  = (struct huft *)  NULL;  /* just to keep compilers happy   */
  q = (struct huft *)  NULL;     /* ditto */
  z = 0;                        /* ditto */

  /* go through the bit lengths (k already is bits in shortest code) */
  for (; k <= g; k++)
  {
    a = c[k];
    while (a--)
    {
      /* here i is the Huffman code of length k bits for value *p */
      /* make tables up to required level */
      while (k > w + l[h])
      {
        w += l[h++];            // add bits already decoded

		/* compute minimum size table less than or equal to *m bits  */
		z = (z = (unsigned)(g - w))  > (unsigned)*m ? (unsigned)*m : z; // upper limit
		if ((f = 1 << (j = (unsigned)(k - w))) > a + 1)
		{   /* try a k-w bit table - too few codes for k-w bit table  */
          f -= a + 1;           /* deduct codes from patterns left  */
		  xp = c + k;
          while (++j < z)
          {     /* try smaller tables up to z bits  */
            if ((f <<= 1)  <= *++xp)
              break;            /* enough codes to use up j bits */
            f -= *xp;           /* else deduct codes from patterns */
          }
        }
        if ((unsigned)  w + j > el && (unsigned)  w < el)
		  j = el - (unsigned)w;           /* make EOB code end at table    */
        z = 1 << j;             /* table entries for j-bit table */
		l[h]  = (int)j;               /* set table size in stack       */

        /* allocate and link in new table */
//        if ((q =
//               (struct huft *)  MALLOC((z + 1)  * sizeof(struct huft)))  ==
//            (struct huft *)  NULL)
        q = new huft[z + 1];
//        if ((q = new huft[z + 1]) == NULL)
//        {
//          if (h)
//            huft_free(u[0]);
//          return 3;             /* not enough memory */
//        }
        fhufts += z + 1;     /* track memory usage               */
        *t = q + 1;             /* link to list for huft_free()     */
        *(t = &(q->v.t))  = (struct huft *)  NULL;
        u[h]  = ++q;             /* table starts after link          */

        /* connect to last table, if there is one */
        if (h)
        {
          x[h]  = i;             /* save pattern for backing up      */
          r.b = (uch)  l[h - 1]; /* bits to dump before this table   */
          r.e = (uch)(32 + j); /* bits in this table               */
          r.v.t = q;            /* pointer to this table            */
          j = (i & ((1 << w)  - 1))  >> (w - l[h - 1]);
          u[h - 1][j]  = r;      /* connect to last table            */
        }
      }

      /* set up table entry in r */
      r.b = (uch)(k - w);
      if (p >= v + n)
        r.e = INVALID_CODE;         /* out of values--invalid code     */
      else
        if (*p < s)
        {
          r.e = (uch)(*p < 256 ? 32 : 31); /* 256 is end-of-block code */
          r.v.n = (ush)  * p++;    /* simple code is just the value   */
        }
        else
        {
          if (!e)
            return 1;             /* RCV v1.6015 added               */
          r.e = (uch)  e[*p - s];  /* non-simple--look up in lists    */
          r.v.n = d[*p++ - s];
        }

      /* fill code-like entries with r */
      f = 1 << (k - w);
      for (j = i >> w; j < z; j += f)
        q[j]  = r;

      /* backwards increment the k-bit code i */
      for (j = 1 << (k - 1); i & j; j >>= 1)
        i ^= j;
      i ^= j;

      /* backup over finished tables */
      while ((i & ((1 << w)  - 1))  != x[h])
        w -= l[--h];            /* don't need to update q */
    }
  }
  /* return actual size of base table */
  *m = l[0];

  /* Return true (1) if we were given an incomplete table */
  return y != 0 && g != 1;
}


/* ===========================================================================
 * Free the malloc'ed tables built by huft_build(), which makes a linked
 * list of the tables it made, with the links in a dummy first entry of
 * each table.
        *t :: Table to free.
 */
int huft_free(struct huft *t)
{
  register struct huft *p, *q;

  /* Go through linked list, freeing from the malloced (t[-1]) address. */
  p = t;
  while (p != (struct huft *)  NULL)
  {
    q = (--p) ->v.t;
//    FREE(p);
    delete[] p;
    p = q;
  }
  return 0;
}
/* 30/1/07 */
