#include "stdafx.h"
#pragma hdrstop
#if defined(_USE_ASM_) && (__BORLANDC__ < 0x0570)
#pragma inline
#endif
#include "ZipDflt.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_LNGMTCH_CPP

/*
  Lngmtch.cpp - zip 'longmatch' function
* part of Deflate.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
  ** distributed under LGPL license ** see license.txt for details

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

/*  PURPOSE
 *      Identify new text as repetitions of old text within a fixed-
 *      length sliding window trailing behind the new text.
 *
 *  DISCUSSION
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
 */


#define NIL 0

// Tail of hash chains
#define FAST  4
#define SLOW  2

// speed options for the general purpose bit flag
#define TOO_FAR 4096

// Matches of length 3 are discarded if their distance exceeds TOO_FAR
// Local data used by the "longest match" routines.
typedef unsigned  IPos;

// Set match_start to the longest match starting at the given string and
//   return its length. Matches shorter or equal to prev_length are discarded,
//   in which case the result is equal to prev_length and match_start is
//   garbage. IN assertions: cur_match is the head of the hash chain for the
//   current string (strstart) and its distance is <= MAX_DIST, and prev_length
//   >= 1
#ifndef _USE_ASM_
int __fastcall ZipDflt::longest_match(unsigned cur_match)
{
    unsigned      chain_length = fmax_chain_length;  // max hash chain length
    register uch  *scan = fwindow + fstrstart;    // current string
    register uch  *match;                     // matched string
    register int  len;                        // length of current match
	int           best_len = (int)fprev_length; // best match length so far
    unsigned          limit = fstrstart > (unsigned) MAX_DIST ? fstrstart -
                              (unsigned) MAX_DIST : NIL;

    // Stop when cur_match becomes <= limit. To simplify the code, we prevent
    //   matches with the string of window index 0.
    // The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of
    //   16. It is easy to get rid of this optimization if necessary.
#if HASH_BITS < 8 || MAX_MATCH != 258
error :
    Code too clever
#endif
    register uch * strend = fwindow + fstrstart + MAX_MATCH;

    register uch  scan_end1 = scan[best_len - 1];
    register uch  scan_end = scan[best_len];

    // Do not waste too much time if we already have a good match:

    if (fprev_length >= fgood_match)
        chain_length >>= 2;

    Assert(fstrstart <= fwindow_size - MIN_LOOKAHEAD, _T("insufficient lookahead"));

    do
    {
        Assert(cur_match < fstrstart, _T("no future"));
        match = fwindow + cur_match;

        // Skip to next match if the match length cannot increase or if the
        //   match length is less than 2:

        if (match[best_len] != scan_end
                || match[best_len - 1] != scan_end1
                || *match != *scan
                || *++match != scan[1])
            continue;

        // The check at best_len-1 can be removed because it will be made again
        //   later. (This heuristic is not always a win.) It is not necessary to
        //   compare scan[2] and match[2] since they are always equal when the
        //   other bytes match, given that the hash keys are equal and that
        //   HASH_BITS >= 8.
        scan += 2, match++;

        // We check for insufficient lookahead only every 8th comparison; the
        //   256th check will be made at strstart+258.
        do
            {}
        while
        (
            *++scan == * ++match
            && *++scan == * ++match
            && *++scan == * ++match
            && *++scan == * ++match
            && *++scan == * ++match
            && *++scan == * ++match
            && *++scan == * ++match
            && *++scan == * ++match
            && scan < strend
        );

        Assert(scan <= fwindow + (unsigned)(fwindow_size - 1), _T("wild scan"));

        len = MAX_MATCH - (int)(strend - scan);

        scan = strend - MAX_MATCH;

        if (len > best_len)
        {
            fmatch_start = cur_match;
            best_len = len;

            if (len >= fnice_match)
                break;

            scan_end1 = scan[best_len - 1];

            scan_end = scan[best_len];
        }
    }
    while ((cur_match = fprev[cur_match & WMASK]) > limit && --chain_length != 0);

    return best_len;
}

#else

// The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
//   It is easy to get rid of this optimization if necessary.
#if HASH_BITS < 8 || MAX_MATCH != 258

error :
Code too clever
#endif
// esp cur_match (D) +4 chain_length (D) +8 limit (D) +12 scan_end (W)
int __fastcall ZipDflt::longest_match(unsigned cur_match)
{
#pragma warn - rvl
#pragma argsused
    asm
    {
		add esp, -16
        mov dword ptr[esp], edx // cur_match
        mov edx, eax  // pG

        // unsigned chain_length = pG->max_chain_length; /* max hash chain length
        // ; EDX = pG
        mov eax, dword ptr[edx + .fmax_chain_length];
        mov dword         ptr[esp + 4], eax

        // register uch *scan = pG->window + pG->strstart; /* current string
        // register uch *match; /* matched string
        // register int len; /* length of current match
        mov eax, dword ptr[edx +.fstrstart];
        mov ecx, eax
        add ecx, edx
        add ecx, .fwindow;

        // int best_len = pG->prev_length; /* best match length so far
        // ; ECX = scan, EDX = pG, EAX = @temp11
        mov edi, dword ptr[edx +.fprev_length];

        // unsigned limit =
        // pG->strstart > (unsigned) MAX_DIST ? pG->strstart - (unsigned) MAX_DIST : NIL;
        // Stop when cur_match becomes <= limit. To simplify the code, we
        //   prevent matches with the string of window index 0.
        // ; ECX = scan, EDX = pG, EDI = best_len, EAX = @temp11
        cmp eax, MAX_DIST;
        jbe short L2
        mov  eax, dword ptr[edx +.fstrstart];
        sub  eax, MAX_DIST;
		jmp short L3
L2:
		xor eax, eax
L3:
        mov dword ptr[esp + 8], eax

        // register uch *strend = pG->window + pG->strstart + MAX_MATCH;
        // ; ECX = scan, EDX = pG, EDI = best_len
        // register uch scan_end1 = scan[best_len - 1];
        // register uch scan_end = scan[best_len];
        // ; ECX = scan, EDX = pG, EDI = best_len, EAX = strendXX
        mov bx, word ptr[ecx + edi - 1]
        mov word ptr[esp + 12], bx

        // ; ECX = scan, EDX = pG, EDI = best_len, EAX =
        // Do not waste too much time if we already have a good match:
        // if (pG->prev_length >= pG->good_match)
        // chain_length >>= 2;
        mov ebx, dword ptr[edx +.fprev_length];
        cmp ebx, dword ptr[edx +.fgood_match];
        jb short L4
		shr dword ptr[esp + 4], 2   // chain_length
L4:
L5:
        // match = pG->window + cur_match;
        mov esi, dword ptr[esp]                 // cur_match
        add esi, edx
        add esi, .fwindow;

        // Skip to next match if the match length cannot increase or if the
        //   match length is less than 2:
        // if (match[best_len] != scan_end ||
        // match[best_len - 1] != scan_end1 ||
        // *match != *scan || *++match != scan[1])
        // continue;
        // ; ECX = scan, ESI = match, EDX = pG, EDI = best_len, EAX = ??
        mov  bx, word ptr[esi + edi - 1]
        cmp bx, word ptr[esp + 12]  // scan_end
        jne L8
        mov bx, word ptr[esi]
        cmp bx, word ptr[ecx]
        jne short L8

        // The check at best_len-1 can be removed because it will be made again
        //   later. (This heuristic is not always a win.) It is not necessary to
        //   compare scan[2] and match[2] since they are always equal when the
        //   other bytes match, given that the hash keys are equal and that
        //   HASH_BITS >= 8.
        // scan += 2, match++;
        // ; ECX = scan, EDX = pG, EDI = best_len, EAX = len
        mov eax, 2  // start 3rd byte

        // We check for insufficient lookahead only every 8th comparison; the
        //   256th check will be made at strstart+258.
		// ; ECX = scan, ESI = match, EDX = pG, EDI = best_len, EAX = len
L9:
        inc eax
        mov ebx, [ecx + eax]
        xor ebx, [esi + eax]
        test ebx, 0xFF // byte 0
        jnz short L10
        inc eax
        test ebx, 0xFF00    // byte 1
        jnz short L10
        inc eax
        test ebx, 0xFF0000  // byte 2
        jnz short L10
        inc eax
        test ebx, ebx       // byte 3
        jnz short L10
        inc eax
        mov ebx, [ecx + eax]
        xor ebx, [esi + eax]
        test ebx, 0xFF  // byte 0
        jnz short L10
        inc eax
        test ebx, 0xFF00    // byte 1
        jnz short L10
        inc eax
        test ebx, 0xFF0000  // byte 2
        jnz short L10
        inc eax
        test ebx, ebx       // byte 3
		jnz short L10

        cmp eax, MAX_MATCH
        jb short L9

L10:
        // ; ECX = scan, ESI = match, EDX = pG, EDI = best_len, EAX = len
        // free EBX , ESI = match
        // if (len > best_len)
        cmp eax, edi
        jl short L11

        // pG->match_start = cur_match;
        // ; ECX = scan, EDX = pG, EAX = strend, EBX = len
        mov esi, dword ptr[esp]
        mov dword ptr[edx +.fmatch_start], esi

        // best_len = len;
        mov edi, eax

        // if (len >= pG->nice_match)
        // break;
        // ; ECX = scan, EDX = pG, EDI = best_len, EAX = len, EBX = ??
        cmp eax, dword ptr[edx +.fnice_match];
        jge short L14 // break

        // scan_end1 = scan[best_len - 1];
        // ; EDI = best_len
        mov bx, word ptr[ecx + edi - 1]
        mov word ptr[esp + 12], bx  // scan_end

L11:
		// while ((cur_match = pG->prev[cur_match & WMASK]) > limit && --chain_length != 0);
L8:
        mov ebx, dword ptr[esp]
        and ebx, WMASK;
        movzx ebx, word ptr[edx + 2 * ebx +.fprev];
        mov dword ptr[esp], ebx // cur_match
        cmp ebx, dword ptr[esp + 8]
        jbe short L14
        dec dword ptr[esp + 4]  // chain_length
		jne L5
        // ; EDI = best_len

L14:
        // return best_len;
        mov eax, edi
        add esp, 16
    };
};

#endif


