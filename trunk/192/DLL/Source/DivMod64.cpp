#include "stdafx.h"
#pragma hdrstop

#if defined(__BORLANDC__) && (__BORLANDC__ < 0x0570)
#pragma inline
#endif
/*
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

//---------------------------------------------------------------------------

// arg = arg / divisor , returns remainder
unsigned  DivMod64(unsigned __int64& arg, unsigned divisor)
{
    asm
    {
        mov ecx, [ebp + 8]  // arg
        mov eax, [ebp + 12] // divisor
        or eax, eax
        jnz @@ok
		// / 0   _ return 0, 0 _ not correct but safer
        mov [ecx + 4] ,eax  // arg hi
        mov [ecx + 0] ,eax  // arg lo
        jmp @@done
    @@ok:
        xor edx, edx
        mov eax, [ecx + 4]  // arg hi
        or eax, eax         // zero?
        jz @@nohi
        div dword ptr [ebp + 12]
    @@nohi:
        mov [ecx + 4], eax  // quot hi
        mov eax, [ecx]      // _dividend[0]
        div dword ptr [ebp + 12]
        mov [ecx], eax      // quot lo
        mov eax, edx        // remainder
    @@done:
    }
#pragma warn -rvl
#pragma warn -par
}
