#include "stdafx.h"
#pragma hdrstop
//#include <stdio.h>
#include "ZipDflt.h"
//#include "Zip.h"
//#include "ZipErr.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZDEFLATE_CPP

/* Deflate.c
* Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
* Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
* This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
** distributed under LGPL license ** see license.txt for details
*/
/*  PURPOSE
*      Identify new text as repetitions of old text within a fixed-
*      length sliding window trailing behind the new text.
**  DISCUSSION
*      The "deflation" process depends on being able to identify portions
*      of the input text which are identical to earlier input (within a
*      sliding window trailing behind the input currently being processed).
*
*      The most straightforward technique turns out to be the fastest for
*      most input files: try all possible matches and select the longest.
*      The key feature of this algorithm is that insertions into the string
*      dictionary are very simple and thus fast, and deletions are avoided
*      completely. Insertions are performed at each input character, whereas
*      string matches are performed only when the previous match ends. So it
*      is preferable to spend more time in matches to allow very fast string
*      insertions and avoid deletions. The matching algorithm for small
*      strings is inspired from that of Rabin & Karp. A brute force approach
*      is used to find longer strings when a small match has been found.
*      A similar algorithm is used in comic (by Jan-Mark Wams) and freeze
*      (by Leonid Broukhis).
*         A previous version of this file used a more sophisticated algorithm
*      (by Fiala and Greene) which is guaranteed to run in linear amortized
*      time, but has a larger average cost, uses more memory and is patented.
*      However the F&G algorithm may be faster for some highly redundant
*      files if the parameter max_chain_length (described below) is too large.
 *
 *  ACKNOWLEDGEMENTS
*      The idea of lazy evaluation of matches is due to Jan-Mark Wams, and
*      I found it in 'freeze' written by Leonid Broukhis.
*      Thanks to many info-zippers for bug reports and testing.
*
*  REFERENCES
*      APPNOTE.TXT documentation file in PKZIP 1.93a distribution.
*
*      A description of the Rabin and Karp algorithm is given in the book
*         "Algorithms" by R. Sedgewick, Addison-Wesley, p252.
*
*      Fiala,E.R., and Greene,D.H.
*         Data Compression with Finite Windows, Comm.ACM, 32,4 (1989) 490-595
*
*  INTERFACE
*      void lm_init (int pack_level, ush *flags)
 *          Initialize the "longest match" routines for a new file
 *
*      ulg deflate (void)
*          Processes a new input file and return its compressed length. Sets
*          the compressed length, crc, deflate flags and internal file
 *          attributes.

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

// Configuration parameters
// Compile with HASH_BITS = 14 to reduce the memory requirements or with
//   HASH_BITS = 13 to use as little memory as possible. Otherwise if the
//   entire input file can be held in memory (not possible on 16 bit systems).
//   Warning: defining this symbol affects the compression ratio. The
//   compressed output is still correct, and might even be smaller in some
//   cases. For portability to 16 bit machines, do not use values above 15.
// #ifndef HASH_BITS
// #define HASH_BITS 15  // Number of bits used to hash strings
// #endif

//#define HASH_SIZE (unsigned)(1 << HASH_BITS)
//#define HASH_MASK (HASH_SIZE - 1)
//#define WMASK     (ZWSIZE - 1)
// HASH_SIZE and ZWSIZE must be powers of two
#define NIL 0

// Tail of hash chains
#define FAST  4
#define SLOW  2

// speed options for the general purpose bit flag
// #ifndef TOO_FAR
#define TOO_FAR 4096
// #endif

// Matches of length 3 are discarded if their distance exceeds TOO_FAR
// Local data used by the "longest match" routines.
//typedef ush Pos;
typedef unsigned IPos;

// A Pos is an index in the character window. We use short instead of int to
//   save space in the various tables. unsigned is used only for parameter passing.
#define H_SHIFT ((HASH_BITS + MIN_MATCH - 1) / MIN_MATCH)

// Number of bits by which ins_h and del_h must be shifted at each input
//   step. It must be such that after MIN_MATCH steps, the oldest byte no
//   longer takes part in the hash key, that is: H_SHIFT * MIN_MATCH >=
//   HASH_BITS
#define max_insert_length fmax_lazy_match

// Insert new strings in the hash table only if the match length is not
//   greater than this length. This saves time but degrades compression.
//   max_insert_length is used only for compression levels <= 3.
// Values for max_lazy_match, good_match and max_chain_length, depending on
//   the desired pack level (0..9). The values given below have been tuned to
//   exclude worst case performance for pathological files. Better values may
//   be found for specific files.

typedef struct config
{
    ush good_length; // reduce lazy search above this match length
    ush max_lazy; // do not perform lazy search above this match length
    ush nice_length; // quit search above this match length
    ush max_chain;
}

config;

#ifdef FULL_SEARCH
#define nice_match  MAX_MATCH
#endif
static config configuration_table[10] =
{
    // good lazy nice chain
    // 0
    { 0, 0, 0, 0 },
    // store only
    // 1
    { 4, 4, 8, 4 },
    // maximum speed, no lazy matches
    // 2
    { 4, 5, 16, 8 },
    // 3
    { 4, 6, 32, 32 },
    // 4
    { 4, 4, 16, 16 },
    // lazy matches
    // 5
    { 8, 16, 32, 32 },
    // 6
    { 8, 16, 128, 128 },
    // 7
    { 8, 32, 128, 256 },
    // 8
    { 32, 128, 258, 1024 },
    // 9
    { 32, 258, 258, 4096 }
}
; // maximum compression

// Note: the deflate() code requires max_lazy >= MIN_MATCH and max_chain >=
//   4 For deflate_fast() (levels <= 3) good is ignored and lazy has a
//   different meaning.
#define EQUAL 0

// result of memcmp for equal strings
// Prototypes for local functions.
//static void fill_window(ZGlobals * pG);
//static ZInt64 deflate_fast(ZGlobals * pG);

//extern int __fastcall longest_match(ZGlobals *pG, unsigned cur_match);

//#ifdef DEBUG
//static void check_match(ZGlobals *pG, unsigned start, unsigned match, int length);
//#endif

// Update a hash value with the given input byte IN assertion: all calls to
//   to UPDATE_HASH are made with consecutive input characters, so that a
//   running hash key can be computed from the previous key instead of complete
//   recalculation each time.
#define UPDATE_HASH(h, c) (h = (((h) << H_SHIFT) ^ (c)) & HASH_MASK)

// Insert string s in the dictionary and set match_head to the previous head
//   of the hash chain (the most recent string with same hash key). Return the
//   previous length of the hash chain. IN assertion: all calls to to
//   INSERT_STRING are made with consecutive input characters and the first
//   MIN_MATCH bytes of s are valid (except for the last MIN_MATCH-1 bytes of
//   the input file).
#define INSERT_STRING(s, match_head)      \
    (     \
          UPDATE_HASH(fins_h, fwindow[(s) + (MIN_MATCH - 1)]), fprev[          \
                  (s) & WMASK] = (ush) (match_head = fhead[fins_h]), fhead[fins_h] = \
                          (ush) (s)  \
    )

// RCV Added 2x (ush)
// Initialize the "longest match" routines for a new file IN assertion:
//   window_size is > 0 if the input file is already read or mmap'ed in the
//   window[] array, 0 otherwise. In the first case, window_size is sufficient
//   to contain the whole input file plus MIN_LOOKAHEAD bytes (to avoid
//   referencing memory beyond the end of window[] when looking for matches
//   towards the end). pack_level :: 0: store, 1: best speed, 9: best
//   compression. flags :: General purpose bit flag.
void ZipDflt::lm_init(int pack_level, ush * flags)
{
    register unsigned j;

    if (pack_level < 1 || pack_level > 9)
        throw DZException(DZ_ERM_LOGIC_ERROR);

//        FatalError(ZEN_LOGIC03);

    // Do not slide the window if the whole input is already in memory
    //   (window_size > 0)
    fsliding = 0;

    if (fwindow_size == 0L)
    {
        fsliding = 1;
        fwindow_size = (ulg)2L * ZWSIZE;
    }

    // Initialize the hash table (avoiding 64K overflow for 16 bit systems).
    //   prev[] will be initialized on the fly.
    fhead[HASH_SIZE - 1] = NIL;            
    memset((char *) fhead, NIL, (unsigned)(HASH_SIZE - 1) * sizeof(* fhead));

    // Set the default configuration parameters:
    fmax_lazy_match = configuration_table[pack_level].max_lazy;
    fgood_match = configuration_table[pack_level].good_length;

#ifndef FULL_SEARCH
    fnice_match = configuration_table[pack_level].nice_length;
#endif

    fmax_chain_length = configuration_table[pack_level].max_chain;

    if (pack_level <= 2)
    {
        * flags |= FAST;
    }
    else
        if (pack_level >= 8)
        {
            * flags |= SLOW;
        }

    // ??? reduce max_chain_length for binary files
    fstrstart = 0;

    fblock_start = 0L;

    j = ZWSIZE;

    j <<= 1; // We Can read 64K in one step.

//  flookahead = (* fread_buf)((uch *) fwindow, j);
	flookahead = (unsigned)read_buf((uch *) fwindow, j);

    if (flookahead == 0 || flookahead == (unsigned)EOF)
    {
        feofile = 1, flookahead = 0;
        return;
    }

    feofile = 0;

    // Make sure that we always have enough lookahead. This is important if
    //   input comes from a device such as a tty.

    if (flookahead < MIN_LOOKAHEAD)
        fill_window();

    fins_h = 0;

    for (j = 0; j < MIN_MATCH - 1; j++)
        UPDATE_HASH(fins_h, fwindow[j]);

    // If lookahead < MIN_MATCH, ins_h is garbage, but this is not important
    //   since only literal bytes will be emitted.
}


#ifdef _DEBUG
// Check that the match at match_start is indeed a match.
void ZipDflt::check_match(unsigned start, unsigned match, int length)
{
    // check that the match is indeed a match
	if (memcmp((char *)fwindow + match, (char *)fwindow + start, (size_t)length) != EQUAL)
        Notify(DZ_ERM_LOGIC_ERROR, _T("invalid match - start %d, match %d, length %d")
               , start, match, length);
}

#else
#define check_match(start, match, length)
#endif

// Fill the window when the lookahead becomes insufficient. Updates strstart
//   and lookahead, and sets eofile if end of input file. IN assertion:
//   lookahead < MIN_LOOKAHEAD &
//   &
//   strstart + lookahead > 0 OUT assertions: strstart <=
//   window_size-MIN_LOOKAHEAD At least one byte has been read, or eofile is
//   set; file reads are performed for at least two bytes (required for the
//   translate_eol option).
void ZipDflt::fill_window(void)
{
    register unsigned n, m;
    unsigned more; // Amount of free space at the end of the window.

    do
    {
        more = (unsigned)(fwindow_size - (ulg)flookahead - (ulg)fstrstart);

        // If the window is almost full and there is insufficient lookahead,
        //   move the upper half to the lower one to make room in the upper half.

        if (more == (unsigned)EOF)
        {
            // Very unlikely, but possible on 16 bit machine if strstart == 0 and
            //   lookahead == 1 (input done one byte at time)
            more--;

            // For MMAP or BIG_MEM, the whole input file is already in memory so
            //   we must not perform sliding. We must however call file_read in order
            //   to compute the crc, update lookahead and possibly set eofile.
        }
        else
            if (fstrstart >= ZWSIZE + MAX_DIST && fsliding)
            {
                // By the IN assertion, the window is not empty so we can't confuse
                //   more == 0 with more == 64K on a 16 bit machine.
                memcpy((char *) fwindow, (char *) fwindow + ZWSIZE, (unsigned)ZWSIZE);
                fmatch_start -= ZWSIZE;
                fstrstart -= ZWSIZE; // we now have strstart
                ///* >= MAX_DIST:
                fblock_start -= (long)ZWSIZE;

                for (n = 0; n < HASH_SIZE; n++)
                {
                    m = fhead[n];
                    fhead[n] = (Pos)(m >= ZWSIZE ? m - ZWSIZE : NIL);
                }

                for (n = 0; n < ZWSIZE; n++)
                {
                    m = fprev[n];
                    fprev[n] = (Pos)(m >= ZWSIZE ? m - ZWSIZE : NIL);

                    // If n is not on any hash chain, prev[n] is garbage but its value
                    //   will never be used.
                }

                more += ZWSIZE;

                if (Abort_Flag)
                    return; // UPDATE: the ziperr() func in dllzip.c closes file.
            }

        if (feofile)
            return;

        // If there was no sliding: strstart <= WSIZE+MAX_DIST - 1 &
        //   &
        //   lookahead <= MIN_LOOKAHEAD - 1 &
        //   &
        //   more == window_size - lookahead - strstart => more >= window_size -
        //   (MIN_LOOKAHEAD - 1 + WSIZE + MAX_DIST - 1) => more >= window_size - 2
        //   * WSIZE + 2 In the BIG_MEM or MMAP case (not yet supported in gzip),
        //   window_size == input_size + MIN_LOOKAHEAD &
        //   &
        //   strstart + lookahead <= input_size => more >= MIN_LOOKAHEAD.
        //   Otherwise, window_size == 2 * ZWSIZE so more >= 2. If there was
        //   sliding, more >= ZWSIZE. So in all cases, more >= 2.
        Assert(more >= 2, _T("more < 2"));

//    n = (* fread_buf)((uch *) fwindow + fstrstart + flookahead, more);
		n = (unsigned)read_buf((uch *)fwindow + fstrstart + flookahead, more);

        if (n == 0 || n == (unsigned)EOF)
        {
            feofile = 1;
        }
        else
        {
            flookahead += n;
        }
    }
    while (flookahead < MIN_LOOKAHEAD && !feofile);
}

/*
// Flush the current block, with given end-of-file flag. IN assertion:
//   strstart is set to the end of the current match.
#define FLUSH_BLOCK(eof)                         \
    flush_block(fblock_start >= 0L ? (uch *) &fwindow[(unsigned)fblock_start] : \
                (uch *)NULL, (ulg)fstrstart - (ulg)fblock_start, (eof))
*/

// Processes a new input file and return its compressed length. This
//   function does not perform lazy evaluationof matches and inserts new
//   strings in the dictionary only for unmatched strings or for short matches.
//   It is used only for the fast compression options.
ZInt64 ZipDflt::deflate_fast(void)
{
    unsigned hash_head; // head of the hash chain
    int flush; // set if current block must be flushed
    unsigned match_length = 0; // length of best match
    fprev_length = MIN_MATCH - 1;

    while (flookahead != 0)
    {
        // Insert the string window[strstart .. strstart+2] in the dictionary,
        //   and set hash_head to the head of the hash chain:
        INSERT_STRING(fstrstart, hash_head);

        // Find the longest match, discarding those <= prev_length. At this
        //   point we have always match_length < MIN_MATCH

        if (hash_head != NIL && fstrstart - hash_head <= MAX_DIST)
        {
            // To simplify the code, we prevent matches with the string of window
            //   index 0 (in particular we have to avoid a match of the string with
            //   itself at the start of the input file).
#ifndef HUFFMAN_ONLY
			match_length = (unsigned)longest_match(hash_head);
#endif
            // longest_match() sets match_start

            if (match_length > flookahead)
                match_length = flookahead;
        }

        if (match_length >= MIN_MATCH)
        {
#ifdef _DEBUG
			check_match(fstrstart, fmatch_start, (int)match_length);
#endif
			flush = ct_tally((int)(fstrstart - fmatch_start), (int)match_length - MIN_MATCH);

            flookahead -= match_length;

            // Insert new strings in the hash table only if the match length is
            //   not too large. This saves time but degrades compression.

            if (match_length <= max_insert_length)
            {
                match_length--; // string at strstart already in hash table

                do
                {
                    fstrstart++;
                    INSERT_STRING(fstrstart, hash_head);

                    // strstart never exceeds ZWSIZE-MAX_MATCH, so there are always
                    //   MIN_MATCH bytes ahead. If lookahead < MIN_MATCH these bytes are
                    //   garbage, but it does not matter since the next lookahead bytes
                    //   will be emitted as literals.
                }
                while (--match_length != 0);

                fstrstart++;
            }
            else
            {
                fstrstart += match_length;
                match_length = 0;
                fins_h = fwindow[fstrstart];
                UPDATE_HASH(fins_h, fwindow[fstrstart + 1]);
#if MIN_MATCH != 3
                Call UPDATE_HASH() MIN_MATCH - 3 more times
#endif
            }
        }
        else
        {
            // No match, output a literal byte
//      Tracevv(("%c"), (fwindow[fstrstart]));
            flush = ct_tally(0, fwindow[fstrstart]);
            flookahead--;
            fstrstart++;
        }

        if (flush)
        {
            FlushBlock(0);
//   FLUSH_BLOCK(0);
			fblock_start = (long)fstrstart;
        }

        // Make sure that we always have enough lookahead, except at the end of
        //   the input file. We need MAX_MATCH bytes for the next match, plus
        //   MIN_MATCH bytes to insert the string following the next match.
        if (flookahead < MIN_LOOKAHEAD)
            fill_window();

        if (Abort_Flag)
            Fatal(DZ_ERM_ABORT, 0);
//            throw DZAbort(DZ_ERM_ABORT);

//        {
        // the ziperr() func in dllzip.c closes file.
        //return ZEN_ABORT;
//        }
    }

//  return FLUSH_BLOCK(1); // eof
    return FlushBlock(1); // eof
}

// Same as above, but achieves better compression. We use a lazy evaluation
//   for matches: a match is finally adopted only if there is no better match
//   at the next window position.
ZInt64 ZipDflt::deflate(void)
{
    unsigned hash_head; // head of hash chain
    unsigned prev_match; // previous match
    int flush; // set if current block
    ///* must be flushed
    int match_available = 0; // set if previous match
    ///* exists
    register unsigned match_length = MIN_MATCH - 1; // length of best match

    if (flevel <= 3)
        return deflate_fast(); // optimized for speed

    // Process the input block.
    while (flookahead != 0)
    {        
//        if (fout_offset > fout_size || fout_size > sizeof(ffile_outbuf))
//        {
//            Notify(IWARNING, _T("Write error in flush_outbuf - out_size %d [%d]"),
//                fout_offset, fout_size);
//            throw DZException(DZ_ERM_ERROR_WRITE);
//        }
        // Insert the string window[strstart .. strstart+2] in the dictionary,
        //   and set hash_head to the head of the hash chain:
        INSERT_STRING(fstrstart, hash_head);       
//        if (fout_offset > fout_size || fout_size > sizeof(ffile_outbuf))
//        {
//            Notify(IWARNING, _T("Write error in flush_outbuf - out_size %d [%d]"),
//                fout_offset, fout_size);
//            throw DZException(DZ_ERM_ERROR_WRITE);
//        }

        // Find the longest match, discarding those <= prev_length.
        fprev_length = match_length, prev_match = fmatch_start;
        match_length = MIN_MATCH - 1;

        if (hash_head != NIL && fprev_length < fmax_lazy_match
                && fstrstart - hash_head <= MAX_DIST)
        {
            // To simplify the code, we prevent matches with the string of window
            //   index 0 (in particular we have to avoid a match of the string with
            //   itself at the start of the input file).
#ifndef HUFFMAN_ONLY
			match_length = (unsigned)longest_match(hash_head);
#endif
            // longest_match() sets match_start

            if (match_length > flookahead)
                match_length = flookahead;

            /*
            #ifdef FILTERED
                  // Ignore matches of length <= 5
                  if (match_length <= 5)
                  {
            #else   */
            // Ignore a length 3 match if it is too distant:
            if (
#ifdef FILTERED
                match_length <= 5 &&
#endif
                match_length == MIN_MATCH && fstrstart - fmatch_start > TOO_FAR)
            {
                //#endif
                // If prev_match is also MIN_MATCH, match_start is garbage but we
                //   will ignore the current match anyway.
                match_length = MIN_MATCH - 1;
            }
        }

        // If there was a match at the previous step and the current match is
        //   not better, output the previous match:
        if (fprev_length >= MIN_MATCH && match_length <= fprev_length)
        {
#ifdef _DEBUG
			check_match(fstrstart - 1, prev_match, (int)fprev_length);
#endif
			flush = ct_tally((int)((fstrstart - 1) - prev_match), (int)fprev_length - (int)MIN_MATCH);

            // Insert in hash table all strings up to the end of the match.
            //   strstart-1 and strstart are already inserted.
            flookahead -= fprev_length - 1;
            fprev_length -= 2;

            do
            {
                fstrstart++;
                INSERT_STRING(fstrstart, hash_head);

                // strstart never exceeds ZWSIZE-MAX_MATCH, so there are always
                //   MIN_MATCH bytes ahead. If lookahead < MIN_MATCH these bytes are
                //   garbage, but it does not matter since the next lookahead bytes
                //   will always be emitted as literals.
            }
            while (--fprev_length != 0);

            match_available = 0;
            match_length = MIN_MATCH - 1;
            fstrstart++;

            if (flush)
            {
//  FLUSH_BLOCK(0);
                FlushBlock(0);
				fblock_start = (long)fstrstart;
            }
//if (fout_offset > fout_size || fout_size > sizeof(ffile_outbuf))
//{
//    Notify(IWARNING, _T("Write error in flush_outbuf - out_size %d [%d]"),
//        fout_offset, fout_size);
//    throw DZException(DZ_ERM_ERROR_WRITE);
//}

        }
        else
            if (match_available)
            {             
//if (fout_offset > fout_size || fout_size > sizeof(ffile_outbuf))
//{
//    Notify(IWARNING, _T("Write error in flush_outbuf - out_size %d [%d]"),
//        fout_offset, fout_size);
//    throw DZException(DZ_ERM_ERROR_WRITE);
//}

                // If there was no match at the previous position, output a single
                //   literal. If there was a match but the current match is longer,
                //   truncate the previous match to a single literal.
//        Tracevv(("%c"), (fwindow[fstrstart - 1]));
                if (ct_tally(0, fwindow[fstrstart - 1]))
                {
//          FLUSH_BLOCK(0);
                    FlushBlock(0);
					fblock_start = (long)fstrstart;
                }
                fstrstart++;
                flookahead--;
            }
            else
            {
                // There is no previous match to compare with, wait for the next
                //   step to decide.
                match_available = 1;
                fstrstart++;
                flookahead--;
            }

        Assert(fstrstart <= fisize && flookahead <= fisize, _T("a bit too far"));

#ifdef _DEBUG
        if (!(fstrstart <= fisize && flookahead <= fisize))
            Notify(DZ_ERM_LOGIC_ERROR, _T("a bit too far"));
#endif
        // Make sure that we always have enough lookahead, except at the end
        //   of the input file. We need MAX_MATCH bytes for the next match, plus
        //   MIN_MATCH bytes to insert the string following the next match.
        if (flookahead < MIN_LOOKAHEAD)
            fill_window();

        if (Abort_Flag)
            Fatal(DZ_ERM_ABORT, 0);
//            throw DZAbort(DZ_ERM_ABORT);
//        {
        // UPDATE: the ziperr() func in dllzip.c closes file.
//            return -ZEN_ABORT;
        //}
    }

    if (match_available)
        ct_tally(0, fwindow[fstrstart - 1]);

//  return FLUSH_BLOCK(1); // eof
    return FlushBlock(1); //eof
}

/* 30/1/07 */




