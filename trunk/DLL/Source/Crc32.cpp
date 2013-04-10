#include "stdafx.h"
#pragma hdrstop
#if defined(_USE_ASM_) && (__BORLANDC__ < 0x0570)
#pragma inline
#endif
#include "common.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_CRC32_CPP
/* crc32.c -- compute the CRC-32 of a data stream
* Copyright (C) 1995 Mark Adler
* For conditions of distribution and use, see copyright notice in zlib.h
* This version modified by Chris Vleghert for BCB/Delphi Zip.
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

/* $Id: crc32.c,v 1.5 1996/01/13 14:55:12 spc Exp $ */
/*$Id This is generic text from the file Header_Func.txt
It is optionally included via the Source Format utility
It can be as long and descriptive as desired.
 */
//It is optionally included via the Source Format utility
//It can be as long and descriptive as desired.
//

#ifndef _USE_ASM_

// Run a set of bytes through the crc shift register. If buf is a NULL
//   pointer, then initialize the crc shift register contents instead. Return
//   the current crc in either case. crc :: crc shift register. buf :: Pointer
//   to bytes to pump through. len :: Number of bytes in buf[].
ulg __fastcall crc32(ulg crc, const uch *buf, int len)
{
    if (buf == NULL)
        return 0L;

    crc = crc ^ 0xFFFFFFFFL;
    if (len)
        do
        {
			crc = crc_table[((int)(crc) ^ (*buf++)) & 0xFF] ^ ((crc) >> 8);
        }
        while (--len);

    return crc ^ 0xFFFFFFFFL; // (instead of ~c for 64-bit machines)
}

#else
// ; These two (three) macros make up the loop body of the CRC32 cruncher.
// ; registers modified:
// ; eax : crc value "c"
// ; edx : pointer to next data byte (or dword) "buf++"
// ; registers read:
// ; esi : pointer to base of crc_table array
// ; scratch registers:
// ; ebx : index into crc_table array
// Run a set of bytes through the crc shift register. If buf is a NULL
//   pointer, then initialize the crc shift register contents instead. Return
//   the current crc in either case. crc :: crc shift register. buf :: Pointer
//   to bytes to pump through. len :: Number of bytes in buf[].
ulg __fastcall crc32(ulg crc, const uch *buf, int len)
{
#pragma argsused
    asm
    {
        // ; EAX = crc, EDX = buf, ESI = &
        // crc_table, ECX = len
        // if (buf == NULL) return 0L;
        test  edx, edx
        jne short L2
        xor eax, eax
        jmp Empty

L2:
        // if (!len) return crc;
        test ecx, ecx
        jz Empty
        mov esi, offset crc_table

        // ; EAX = crc, EDX = buf, ESI = &
        // crc_table, ECX = len
        // crc = crc ^ 0xFFFFFFFFL;
        not eax

N3:
        test edx, 3                 // aligned ?
        jz short Next_Eight
		xor al, byte ptr[edx];
		inc edx;
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];

        dec ecx
        jnz short N3
        // aligned

Next_Eight:
        cmp ecx, 8
        jb NoEights

        // Next_Eight:
		xor eax, dword ptr[edx];
		add edx, 4;
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];

		xor eax, dword ptr[edx];
		add edx, 4;
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];

        sub ecx, 8
        jnz Next_Eight
        jmp short Done

        // ; EAX = crc, EDX = buf, ESI = &
        // crc_table, ECX = len

NoEights:
        sub ecx, 1
        jb short Done

        // DoLast:
		xor al, byte ptr[edx];
		inc edx;
		movzx ebx, al;
		shr eax, 8;
		xor eax, dword ptr[esi + 4 * ebx];
        jmp short NoEights

Done:
        xor eax, -1

        // ; EAX = crc

Empty:
#pragma warn - rvl
    };
};

#endif // !_USE_ASM_
