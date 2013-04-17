#include "stdafx.h"
#pragma hdrstop
#include "ZipDflt.h"
//#include "Zip.h"
//#include "ZipErr.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZBITS_CPP

/* Bits.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * Permission is granted to any individual or institution to use, copy, or
 * redistribute this software so long as all of the original files are included,
 * that it is not sold for profit, and that this copyright notice is retained.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.

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

/*
 *  bits.c by Jean-loup Gailly and Kai Uwe Rommel.
 *
 *  This is a new version of im_bits.c originally written by Richard B. Wales
 *
 *  PURPOSE
 *
 *      Output variable-length bit strings. Compression can be done
 *      to a file or to memory.
 *
 *  DISCUSSION
 *
 *      The PKZIP "deflate" file format interprets compressed file data
 *      as a sequence of bits.  Multi-bit strings in the file may cross
 *      byte boundaries without restriction.
 *
 *      The first bit of each byte is the low-order bit.
 *
 *      The routines in this file allow a variable-length bit value to
 *      be output right-to-left (useful for literal values). For
 *      left-to-right output (useful for code strings from the tree routines),
 *      the bits must have been reversed first with bi_reverse().
 *
 *      For in-memory compression, the compressed bit stream goes directly
 *      into the requested output buffer. The input data is read in blocks
 *      by the mem_read() function. The buffer is limited to 64K on 16 bit
 *      machines.
 *
 *  INTERFACE
 *
 *      void bi_init(FILE *zipfile)
 *          Initialize the bit string routines.
 *
 *      void send_bits(int value, int length)
 *          Write out a bit string, taking the source bits right to
 *          left.
 *
 *      int bi_reverse(int value, int length)
 *          Reverse the bits of a bit string, taking the source bits left to
 *          right and emitting them right to left.
 *
 *      void bi_windup(void)
 *          Write out any remaining bits in an incomplete byte.
 *
 *      void copy_block(char *buf, unsigned len, int header)
 *          Copy a stored block to the zip file, storing first the length and
 *          its one's complement if requested.
 *
 *      int seekable(void)
 *          Return true if the zip file can be seeked.
 *
 *      ulg memcompress(char *tgt, ulg tgtsize, char *src, ulg srcsize);
 *          Compress the source buffer src into the target buffer tgt.
 */

// Number of bits used within bi_buf. (bi_buf might be implemented on more
//   than 16 bits on some systems.)
#define Buf_size  (8 * 2 * sizeof(char))
                                                                       
//            fout_buf[fout_offset++] = (char)(b);
// Output a 16 bit value to the bit stream, lower (oldest) byte first
#define PUTBYTE(b)                                 \
    {                                                  \
        if (fout_offset < sizeof(f_outbuf))             \
        {                                                \
            f_outbuf[fout_offset++] = (uch)(b); \
        }                                                \
        else                                             \
        {                                                \
            flush_outbuf((b), 1);                    \
        }                                                \
    }

// Prototypes for local functions
#ifdef _USE_ASM_
extern unsigned __fastcall bi_reverse(unsigned code, int len);
#endif

// Initialize the bit string routines. zipfile :: Output zip file, NULL for
//   in-memory compression.
void ZipDflt::bi_init(void) //HANDLE zipfile)
{
//  fzfile = zipfile;
    fbi_buf = 0;
    fbi_valid = 0;
#ifdef DEBUG
    fbits_sent = 0L;
#endif
    // Set the defaults for file compression. They are set by memcompress for
    //   in-memory compression.
//  if (fzfile > 0)    //!= INVALID_HANDLE_VALUE)
//  if (fhOutz != INVALID_HANDLE_VALUE)

    if (fZipOutfile && fZipOutfile->IsOpen())
    {
//        fout_buf = ffile_outbuf;
//        fout_size = sizeof(ffile_outbuf);
        fout_offset = 0;
//    fread_buf = &ZipDflt::file_read;
//#if defined(USE_STRM_INPUT) && defined(USE_STRM_OUTPUT)
//        ReadingFile = -1;
//#endif
    }
}

// Send a value on a given number of bits. IN assertion: length <= 16 and
//   value fits in length bits. value :: Value to send. length :: Number of
//   bits.
void __fastcall ZipDflt::send_bits(int value, int length)
{
#ifdef DEBUG
    Tracevv((_T(" l %2d v %4x ")), (length, value));
    Assert(length > 0 && length <= 15, _T("invalid length"));
    fbits_sent += (ulg) length;
#endif

    // If not enough room in bi_buf, use (valid) bits from bi_buf and (16 -
    //   bi_valid) bits from value, leaving (width - (16-bi_valid)) unused bits
    //   in value.

    if (fbi_valid > (int)Buf_size - length)
    {
        fbi_buf |= (ush)(value << fbi_valid);                         // RCV Added (ush)

        //  PUTSHORT(fbi_buf);  /*

        if (fout_offset < sizeof(f_outbuf) -1) //fout_size - 1)
        {
            *(ush *)(f_outbuf + fout_offset) = (ush) fbi_buf;
//            f_outbuf[fout_offset] = (ush) fbi_buf;
            fout_offset += 2;
        }
        else
        {
            flush_outbuf((fbi_buf), 2);
        }

        fbi_buf = (ush)((ush) value >> ((int)Buf_size - (int)fbi_valid));     // RCV Added (ush)(...)
		fbi_valid += (int)(length - (int)Buf_size);
    }
    else
    {
        fbi_buf |= (ush)(value << fbi_valid);                         // RCV Added (ush)
        fbi_valid += length;
    }
}

// Reverse the first len bits of a code, using straightforward code (a
//   faster method would use a table) IN assertion: 1 <= len <= 15 code :: The
//   value to invert. len :: Its bit length.
#ifndef _USE_ASM_
unsigned __fastcall bi_reverse(unsigned code, int len)
{
    register unsigned res = 0;

    do
    {
        res |= code & 1;
        code >>= 1, res <<= 1;
    }
    while (--len > 0);

    return (res >> 1);
}
//
//#else
//unsigned __fastcall bi_reverse(unsigned code, int len)
//{
//#pragma warn - rvl
//#pragma argsused
//    asm
//    {
//        // EAX=code EDX=len
//        mov ecx, eax
//        xor eax, eax
//
//Loop1:
//        dec edx
//        jl short doneit
//        ror ecx, 1
//        rcl eax, 1
//        jmp short Loop1
//
//doneit:
//    };
//}

#endif

// Flush the current output buffer. w :: Value to flush. bytes :: Number of
//   bytes to flush (0, 1 or 2).
void __fastcall ZipDflt::flush_outbuf(unsigned w, unsigned bytes)
{
    // Encrypt and write the output buffer:
    if (fout_offset != 0)
    {
        if (!zfwrite(f_outbuf, (::extent) fout_offset))
        {
            diag(_T("Write error in flush_outbuf"));
//            FatalError(ZEN_WRITE01);
            throw DZException(DZ_ERM_ERROR_WRITE);
        }
    }

    fout_offset = 0;

    if (bytes == 2)
    {
        //    PUTSHORT(w); /*
        if (fout_offset < sizeof(f_outbuf) - 1)//fout_size - 1)
        {
            *(ush *)(f_outbuf + fout_offset) = (ush) w;
//            f_outbuf[fout_offset] = (ush) w;
            fout_offset += 2;
        }
        else
        {
            flush_outbuf((w), 2);
        }
    }
    else
        if (bytes == 1)
            f_outbuf[fout_offset++] = (uch)/*(char)*/(w & 0xFF);
}

// Write out any remaining bits in an incomplete byte.
void __fastcall ZipDflt::bi_windup(void)
{
    if (fbi_valid > 8)
    {
        if (fout_offset < sizeof(f_outbuf) - 1)//fout_size - 1)
        {
            *(ush *)(f_outbuf + fout_offset) = (ush) fbi_buf;
//            f_outbuf[fout_offset] = (ush) fbi_buf;
            fout_offset += 2;
        }
        else
        {
            flush_outbuf((fbi_buf), 2);
        }
    }
    else
        if (fbi_valid > 0)
        {
			PUTBYTE(fbi_buf);
        }

    if (fZipOutfile->IsOpen())
        flush_outbuf(0, 0);

    fbi_buf = 0;
    fbi_valid = 0;
#ifdef DEBUG
    fbits_sent = (fbits_sent + 7) & ~7;
#endif
}

// Copy a stored block to the zip file, storing first the length and its
//   one's complement if requested. block :: The input data. len :: Its length.
//   header :: True if block header must be written.
void __fastcall ZipDflt::copy_block(const uch *block, unsigned len, int header)
{
    bi_windup();        // align on byte boundary

    if (header)
    {
        if (fout_offset < sizeof(f_outbuf) -1)// fout_size - 1)
        {
            *(ush *)(f_outbuf + fout_offset) = (ush) len;
            fout_offset += 2;
        }
        else
        {
            flush_outbuf((len), 2);
        }

        //    PUTSHORT((ush)~len);
        if (fout_offset < sizeof(f_outbuf) -1)// fout_size - 1)
        {
            *(ush *)(f_outbuf + fout_offset) = (ush)~len;
            fout_offset += 2;
        }
        else
        {
            flush_outbuf((~len), 2);
        }

#ifdef DEBUG
        fbits_sent += 2 * 16;
#endif
    }

//  if (fzfile)
//  if (fhOutz != INVALID_HANDLE_VALUE)
    if (fZipOutfile)
    {
        flush_outbuf(0, 0);

        if (!zfwrite(block, len))
            throw DZException(DZ_ERM_ERROR_WRITE);

//            FatalError(ZEN_WRITE02);
    }
    else
    {
        if (fout_offset + len > sizeof(f_outbuf))//fout_size)
            throw DZException(DZ_ERM_LOGIC_ERROR);

//            FatalError(ZEN_LOGIC01);
//        else
//        {
        memcpy(f_outbuf + fout_offset, block, len);
        fout_offset += len;

    }

#ifdef DEBUG
    fbits_sent += (ulg) len << 3;
#endif
}

//// Return true if the zip file can be seeked. This is used to check if the
////   local header can be re-rewritten. This function always returns true for
////   in-memory compression. IN assertion: the local header has already been
////   written (ftell() > 0).
//int ZipDflt::seekable(void)
//{
//    return fZipOutfile->IsSeekable;
//}


//#ifdef USING_MEM_STRMS
//// In-memory compression. This version can be used only if the entire input
////   fits in one memory buffer. The compression is then done in a single call
////   of memcompress(). (An extension to allow repeated calls would be possible
////   but is not needed here.) The first two bytes of the compressed output are
////   set to a short with the method used (DEFLATE or STORE). The following four
////   bytes contain the CRC. The values are stored in little-endian order on all
////   machines. This function returns the byte size of the compressed output,
////   including the first six bytes (method and crc). tgt, *src :: Target and
////   source buffers. tgtsize, srcsize :: Target and source sizes.
//ulg ZipDflt::memcompress(char *tgt, ulg tgtsize, char *src, ulg srcsize)
//{
//    ush att = (ush) UNKNOWN;
//    ush flags = 0;
//    ulg crc;              // RCV Removed ...= 0;
//    int method = DEFLATE;
//
//    if (tgtsize <= 6L)
//        throw DZException(DZ_ERM_ERROR_LOGIC);
//
////        FatalError(ZEN_LOGIC02);
//
//    crc = crc32(0L, (uch *)NULL, 0);
//    crc = crc32(crc, (uch *)src, (extent) srcsize);
//
////  fread_buf = &ZipDflt::mem_read;
//    ReadingFile = 0;
//    fin_buf = src;
//    fin_size = (unsigned)srcsize;
//    fin_offset = 0;
//    fout_buf = tgt;
//    fout_size = (unsigned)tgtsize;
//    fout_offset = 2 + 4;
//    fwindow_size = 0L;
//    if (fZipOutfile)
//    {
//        delete fZipOutfile;
//        fZipOutfile = NULL;
//    }
//
////  if (fhOutz != INVALID_HANDLE_VALUE)
////    Close_Handle(&fhOutz);  // should not happen but...
//    bi_init(); //NULL);
//    ct_init(&att, &method);
//    lm_init((flevel != 0 ? flevel : 1), &flags);
//    deflate();
//    fwindow_size = 0L; // was updated by lm_init()
//    // For portability, force little-endian order on all machines:
//    tgt[0] = (char)(method & 0xFF);
//    tgt[1] = (char)((method >> 8) & 0xFF);
//    tgt[2] = (char)(crc & 0xFF);
//    tgt[3] = (char)((crc >> 8) & 0xFF);
//    tgt[4] = (char)((crc >> 16) & 0xFF);
//    tgt[5] = (char)((crc >> 24) & 0xFF);
//
//    return (ulg) fout_offset;
//}
//
//// In-memory read function. As opposed to file_read(), this function does
////   not perform end-of-line translation, and does not update the crc and input
////   size. Note that the size of the entire input buffer is an unsigned long,
////   but the size used in mem_read() is only an unsigned int. This makes a
////   difference on 16 bit machines. mem_read() may be called several times for
////   an in-memory compression.
//int ZipDflt::mem_read(unsigned char *b, unsigned bsize)
//{
//    if (fin_offset < fin_size)
//    {
//        ulg block_size = fin_size - fin_offset;
//
//        if (block_size > (ulg) bsize)
//            block_size = (ulg) bsize;
//
//        memcpy(b, fin_buf + fin_offset, (unsigned)block_size);
//        fin_offset += (unsigned)block_size;
//        return (int)block_size;
//    }
//
//    return 0;             // end of input
//}
//
//#endif




