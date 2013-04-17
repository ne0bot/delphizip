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
#ifdef _USE_ASM_
	#if defined(__BORLANDC__) && (__BORLANDC__ < 0x0570)
	#pragma inline
	#endif

#include "ZipDflt.h"

void __fastcall ZipDflt::pqdownheap(ct_data *tree, int k)
{
#pragma warn - rvl
#pragma argsused
//  int v, j, b;
    int v = fheap[k];
    int j = k << 1;       // left son of k
    int htemp;
    asm
    {

l1:
        mov       ebx, this
        // ebx = this
        //   While (j <= fheap_len)
        mov       edx, j
        cmp       edx, dword ptr [ebx .fheap_len]
        jg        lx
        //    if (J < fheap_len
        jge       l3
        //      && smaller
        //      tree[j + 1].Freq < tree[j].Freq
        mov       ecx, tree
        mov       edx, j
        mov       edx, dword ptr [ebx+4*edx .fheap+4]
        mov       ax, word ptr [ecx+4*edx]
        mov       edx, j
        mov       edx, dword ptr [ebx+4*edx .fheap]
        cmp       ax, word ptr [ecx+4*edx]
        jb        short l2
        // || (tree[n].Freq == tree[m].Freq
        jne       short l3
        //  && fdepth[j + 1] <= fdepth[j])
        mov       eax, j
        mov       edx, this
        mov       eax, dword ptr [ebx+4*eax .fheap+4]
        mov       cl, byte ptr [ebx+eax .fdepth]
        mov       eax, j
        mov       eax, dword ptr [ebx+4*eax .fheap]
        cmp       cl, byte ptr [ebx+eax .fdepth]
        ja        short l3

l2:
        //    j++
        inc       j

l3:
        // Exit if v is smaller than both sons
        //   htemp = fheap[j];
        mov       ecx, j
        mov       edx, dword ptr [ebx+4*ecx .fheap]
        mov       htemp, edx
        //      if (smaller(tree, v, htemp)
        //      tree[v].Freq < tree[htemp].Freq
        mov       ecx, tree
        mov       eax, v
        mov       dx, word ptr [ecx+4*eax]
        mov       eax, htemp
        cmp       dx, word ptr [ecx+4*eax]
        jb        short lx
        // || (tree[v].Freq == tree[htemp].Freq
        jne       short l4
        //  && fdepth[v] <= fdepth[htemp])
        mov       eax, v
        mov       cl, byte ptr [ebx+eax .fdepth]
        mov       eax, htemp
        cmp       cl, byte ptr [ebx+eax .fdepth]
        jbe       short lx

l4:
        // Exchange v with the smallest son
        //    fheap[k] = htemp;
        mov       ecx, k
        mov       edx, htemp
        mov       dword ptr [ebx+4*ecx .fheap], edx

        //    k = j;
        mov       ecx, j
        mov       k, ecx

        // And continue down the tree, setting j to the left son of k
        //    j <<= 1;
        shl       j, 1
	#if defined(__BORLANDC__) && (__BORLANDC__ < 0x0550)
        jmp       l1
	#else
        jmp       short l1
	#endif
        // } while

lx:
        //  fheap[k] = v;
        mov       ecx, k
        mov       eax, this
        mov       edx, v
        mov       dword ptr [eax+4*ecx .fheap], edx
        // fini
    };
};



unsigned __fastcall bi_reverse(unsigned code, int len)
{
#pragma warn - rvl
#pragma argsused
    asm
    {
        // EAX=code EDX=len
        mov ecx, eax
        xor eax, eax

Loop1:
        dec edx
        jl short doneit
        ror ecx, 1
        rcl eax, 1
        jmp short Loop1

doneit:
    };
}


#endif

