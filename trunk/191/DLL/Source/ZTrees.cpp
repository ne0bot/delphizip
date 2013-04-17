#include "stdafx.h"
#pragma hdrstop
//#if defined(_USE_ASM_) && (__BORLANDC__ < 0x0570)
//#pragma inline
//#endif

#include "ZipDflt.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZTREES_CPP

/* Trees.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * This version modified by Chris Vleghert and Eric Engler for BCB/Delphi Zip.
  ** distributed under LGPL license ** see license.txt for details
 */
/*  Trees.c by Jean-loup Gailly
 *  This is a new version of im_ctree.c originally written by Richard B. Wales
 *  for the defunct implosion method.
 *
 *  PURPOSE
 *      Encode various sets of source values using variable-length
 *      binary code trees.
 *
 *  DISCUSSION
 *      The PKZIP "deflation" process uses several Huffman trees. The more
 *      common source values are represented by shorter bit sequences.
 *
 *      Each code tree is stored in the ZIP file in a compressed form
 *      which is itself a Huffman encoding of the lengths of
 *      all the code strings (in ascending order by source values).
 *      The actual code strings are reconstructed from the lengths in
 *      the UNZIP process, as described in the "application note"
 *      (APPNOTE.TXT) distributed as part of PKWARE's PKZIP program.
 *
 *  REFERENCES
 *      Lynch, Thomas J.
 *          Data Compression:  Techniques and Applications, pp. 53-55.
 *          Lifetime Learning Publications, 1985.  ISBN 0-534-03418-7.
 *
 *      Storer, James A.
 *          Data Compression:  Methods and Theory, pp. 49-50.
 *          Computer Science Press, 1988.  ISBN 0-7167-8156-5.
 *
 *      Sedgewick, R.
 *          Algorithms, p290.
 *          Addison-Wesley, 1983. ISBN 0-201-06672-6.
 *
 *  INTERFACE
 *      void ct_init(ush *attr, int *method)
 *          Allocate the match buffer, initialize the various tables and save
 *          the location of the internal file attribute (ascii/binary) and
 *          method (DEFLATE/STORE)
 *
 *      void ct_tally(int dist, int lc);
 *          Save the match info and tally the frequency counts.
 *
 *      long flush_block(char *buf, ulg stored_len, int eof)
 *          Determine the best encoding for the current block: dynamic trees,
 *          static trees or store, and output the encoded block to the zip
 *          file. Returns the total compressed length for the file so far.
 */
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
#include <ctype.h>

#define d_code(dist) \
	((dist) < 256 ? fdist_code[dist] : fdist_code[256 + ((dist) >> 7)])

#define send_code(c, tree)  send_bits(tree[c].Code, tree[c].Len)

#define STORED_BLOCK  0
#define STATIC_TREES  1
#define DYN_TREES     2

/*
// Local (static) data
int         extra_lbits[LENGTH_CODES] // extra bits for each length code
=
 {
   0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,  2,  2,  2,  2,
   3,  3,  3,  3,  4,  4,  4,  4,  5,  5,  5,  5,  0};

int         extra_dbits[D_CODES]      // extra bits for each distance code
=
 {
   0,  0,  0,  0,  1,  1,  2,  2,  3,  3,  4,  4,  5,  5,  6,  6,
   7,  7,  8,  8,  9,  9,  10,  10,  11,  11,  12,  12,  13,  13
 };

int         extra_blbits[BL_CODES]    // extra bits for each bit length code
= { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 3, 7 };
*/
static uch  bl_order[BL_CODES] =
{
    16,  17,  18,  0,  8,  7,  9,  6,  10,  5,  11,  4,  12,  3,
    13,  2,  14,  1,  15
};

// The lengths of the bit length codes are sent in order of decreasing
//   probability, to avoid transmitting the lengths for unused bit length
//   codes.

// Allocate the match buffer, initialize the various tables and save the
//   location of the internal file attribute (ascii/binary) and method
//   (DEFLATE/STORE). attr :: Pointer to internal file attribute. method ::
//   Pointer to compression method.
void ZipDflt::ct_init(ush *attr, int *method)
{
    int n;      // iterates over tree elements
    int bits;   // bit counter
    int length; // length value
    int code;   // code value
    int dist;   // distance index
    ffile_type = attr;
    ffile_method = method;
    fcompressed_len = 0;//ui64;  // v1.6018
#ifdef DEBUG
    finput_len = 0L;
#endif

    if (fstatic_dtree[0].Len != 0)
        return;                     // ct_init already called

    // Initialize the mapping length (0..255) -> length code (0..28)
    length = 0;

    for (code = 0; code < LENGTH_CODES - 1; code++)
    {
        fbase_length[code] = length;

        for (n = 0; n < (1 << extra_lbits[code]); n++)
            flength_code[length++] = (uch) code;
    }

    Assert(length == 256, _T("ct_init: length != 256"));

    // Note that the length 255 (match length 258) can be represented in two
    //   different ways: code 284 + 5 bits or code 285, so we overwrite
    //   length_code[255] to use the best encoding:
    flength_code[length - 1] = (uch) code;

    // Initialize the mapping dist (0..32K) -> dist code (0..29)
    dist = 0;

    for (code = 0; code < 16; code++)
    {
        fbase_dist[code] = dist;

        for (n = 0; n < (1 << extra_dbits[code]); n++)
            fdist_code[dist++] = (uch) code;
    }

    Assert(dist == 256, _T("ct_init: dist != 256"));

    dist >>= 7;                   // from now on, all distances are divided by 128

    for (; code < D_CODES; code++)
    {
        fbase_dist[code] = dist << 7;

        for (n = 0; n < (1 << (extra_dbits[code] - 7)); n++)
            fdist_code[256 + dist++] = (uch) code;
    }

    Assert(dist == 256, _T("ct_init: 256 + dist != 512"));

    // Construct the codes of the static literal tree

    for (bits = 0; bits <= MAX_BITS; bits++)
        fbl_count[bits] = 0;

    n = 0;

    while (n <= 143)
        fstatic_ltree[n++].Len = 8, fbl_count[8]++;

    while (n <= 255)
        fstatic_ltree[n++].Len = 9, fbl_count[9]++;

    while (n <= 279)
        fstatic_ltree[n++].Len = 7, fbl_count[7]++;

    while (n <= 287)
        fstatic_ltree[n++].Len = 8, fbl_count[8]++;

    // Codes 286 and 287 do not exist, but we must include them in the tree
    //   construction to get a canonical Huffman tree (longest code all ones)
    gen_codes((ct_data *)fstatic_ltree, L_CODES + 1);

    // The static distance tree is trivial:
    for (n = 0; n < D_CODES; n++)
    {
        fstatic_dtree[n].Len = 5;
        fstatic_dtree[n].Code = (ush)(bi_reverse((unsigned)n, 5));   // RCV Added (ush)
    }

    // Initialize the first block of the first file:
    init_block();
}

// Initialize a new block.
void ZipDflt::init_block(void)
{
    int n;                // iterates over tree elements

    // Initialize the trees.

    for (n = 0; n < L_CODES; n++)
        fdyn_ltree[n].Freq = 0;

    for (n = 0; n < D_CODES; n++)
        fdyn_dtree[n].Freq = 0;

    for (n = 0; n < BL_CODES; n++)
        fbl_tree[n].Freq = 0;

    fdyn_ltree[END_BLOCK].Freq = 1;
    fopt_len = fstatic_len = 0L;
    flast_lit = flast_dist = flast_flags = 0;
    fflags = 0;           
    fflag_bit = 1;
}

#define SMALLEST  1

// Index within the heap array of least frequent node in the Huffman tree
// Remove the smallest element from the heap and recreate the heap with one
//   less element. Updates heap and heap_len.
#define pqremove(tree, top)                      \
    {                                                \
        top = fheap[SMALLEST];                      \
        fheap[SMALLEST] = fheap[fheap_len--]; \
        pqdownheap(tree, SMALLEST);              \
    }

#ifndef _USE_ASM_
// Compares to subtrees, using the tree depth as tie breaker when the
//   subtrees have equal frequency. This minimizes the worst case length.
#define smaller(tree, n, m)                                         \
    (                                                              \
            tree[n].Freq < tree[m].Freq                                     \
            || (tree[n].Freq == tree[m].Freq && fdepth[n] <= fdepth[m]) \
    )

// Restore the heap property by moving down the tree starting at node k,
//   exchanging a node with the smallest of its two sons if necessary, stopping
//   when the heap property is re-established (each father smaller than its two
//   sons). tree :: The tree to restore. k :: Node to move down.
void __fastcall ZipDflt::pqdownheap(ct_data *tree, int k)
{
    int v = fheap[k];
    int j = k << 1;       // left son of k
    int htemp;            // required because of bug in SASC compiler

    while (j <= fheap_len)
    {
        // Set j to the smallest of the two sons:
        if (j < fheap_len && smaller(tree, fheap[j + 1], fheap[j]))
            j++;

        // Exit if v is smaller than both sons
        htemp = fheap[j];

        if (smaller(tree, v, htemp))
            break;

        // Exchange v with the smallest son
        fheap[k] = htemp;

        k = j;

        // And continue down the tree, setting j to the left son of k
        j <<= 1;
    }

    fheap[k] = v;
}
  /*
#else

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
        jmp       short l1
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
 */
#endif

// Compute the optimal bit lengths for a tree and update the total bit
//   length for the current block. IN assertion: the fields freq and dad are
//   set, heap[heap_max] and above are the tree nodes sorted by increasing
//   frequency. OUT assertions: the field len is set to the optimal bit length,
//   the array bl_count contains the frequencies for each bit length. The
//   length opt_len is updated; static_len is also updated if stree is not
//   null. desc :: The tree descriptor.
void __fastcall ZipDflt::gen_bitlen(tree_desc *desc)
{
    ct_data *tree = desc->dyn_tree;
    int     *extra = desc->extra_bits;
    int     base = desc->extra_base;
    int     max_code = desc->max_code;
    int     max_length = desc->max_length;
    ct_data *stree = desc->static_tree;
    int     h;            // heap index
    int     n,
    m;            // iterate over the tree elements
    int     bits;         // bit length
    int     xbits;        // extra bits
    ush     f;            // frequency
    int     overflow = 0; // number of elements with bit length too large

    for (bits = 0; bits <= MAX_BITS; bits++)
        fbl_count[bits] = 0;

    // In a first pass, compute the optimal bit lengths (which may overflow in
    //   the case of the bit length tree).
    tree[fheap[fheap_max]].Len = 0; // root of the heap

    for (h = fheap_max + 1; h < HEAP_SIZE; h++)
    {
        n = fheap[h];
        bits = tree[tree[n].Dad].Len + 1;

        if (bits > max_length)
            bits = max_length, overflow++;

        tree[n].Len = (ush) bits;         // RCV Added (ush)

        // We overwrite tree[n].Dad which is no longer needed
        if (n > max_code)
            continue;           // not a leaf node

        fbl_count[bits]++;
        xbits = 0;

        if (n >= base)
            xbits = extra[n - base];

        f = tree[n].Freq;
        fopt_len += (ulg) f * (ulg)(bits + xbits);

        if (stree)
			fstatic_len += (ulg) f * (ulg)(stree[n].Len + xbits);
    }

    if (overflow == 0)
        return;

//  Trace(("\nbit length overflow\n", 0));

    // This happens for example on obj2 and pic of the Calgary corpus
    // Find the first bit length which could increase:
    do
    {
        bits = max_length - 1;
        while (fbl_count[bits] == 0)
            bits--;

        fbl_count[bits]--; // move one leaf down the tree
        fbl_count[bits + 1] += (ush) 2;  // move one overflow item as its brother
        fbl_count[max_length]--;

        // The brother of the overflow item also moves one step up, but this
        //   does not affect bl_count[max_length]
        overflow -= 2;
    }
    while (overflow > 0);

    // Now recompute all bit lengths, scanning in increasing frequency. h is
    //   still equal to HEAP_SIZE. (It is simpler to reconstruct all lengths
    //   instead of fixing only the wrong ones. This idea is taken from 'ar'
    //   written by Haruhiko Okumura.)
    for (bits = max_length; bits != 0; bits--)
    {
        n = fbl_count[bits];

        while (n != 0)
        {
            m = fheap[--h];

            if (m > max_code)
                continue;

            if (tree[m].Len != (ush) bits)
            { // RCV Changed (unsigned) in (ush)
//        Trace(("code %d bits %d->%d"), m, tree[m].Len, bits);
				fopt_len += (unsigned)((ush)bits - tree[m].Len) * tree[m].Freq;
                tree[m].Len = (ush) bits; // RCV Added (ush)
            }

            n--;
        }
    }
}

// Generate the codes for a given tree and bit counts (which need not be
//   optimal). IN assertion: the array bl_count contains the bit length
//   statistics for the given tree and the field len is set for all tree
//   elements. OUT assertion: the field code is set for all tree elements of
//   non zero code length. tree :: The tree to decorate max_code :: Largest
//   code with non zero frequency.
void __fastcall ZipDflt::gen_codes(ct_data *tree, int max_code)
{
    ush next_code[MAX_BITS + 1];      // next code value for each bit length
    ush code = 0;                     // running code value
    int bits; // bit index
    int n;    // code index

    // The distribution counts are first used to generate the code values
    //   without bit reversal.

    for (bits = 1; bits <= MAX_BITS; bits++)
    {
        next_code[bits] = code = (ush)((code + fbl_count[bits - 1]) << 1);   // RCV Added (ush)
    }

    // Check that the bit counts in bl_count are consistent. The last code
    //   must be all ones.
    Assert(code + fbl_count[MAX_BITS] - 1 == (1 << MAX_BITS) - 1,
           _T("inconsistent bit counts"));

    //  Tracev(("\ngen_codes: max_code %d "), (max_code));

    for (n = 0; n <= max_code; n++)
    {
        int len = tree[n].Len;

        if (len == 0)
            continue;

        // Now reverse the bits
        tree[n].Code = (ush)(bi_reverse(next_code[len]++, len));              // RCV Added (ush)

        //    Tracec(tree != fstatic_ltree, ("\nn %3d %c l %2d c %4x (%x) "),(n,
        //              (isgraph(n) ? n : ' '), len, tree[n].Code, next_code[len] - 1));
    }
}

// Construct one Huffman tree and assigns the code bit strings and lengths.
//   Update the total bit length for the current block. IN assertion: the field
//   freq is set for all tree elements. OUT assertions: the fields len and code
//   are set to the optimal bit length and corresponding code. The length
//   opt_len is updated; static_len is also updated if stree is not null. The
//   field max_code is set. desc :: The tree descriptor.
void __fastcall ZipDflt::build_tree(tree_desc *desc)
{
    ct_data *tree = desc->dyn_tree;
    ct_data *stree = desc->static_tree;
    int     elems = desc->elems;
    int     n,
    m;                      // iterate over heap elements
    int     max_code = -1;          // largest code with non zero frequency
    int     node = elems;           // next internal node of the tree

    // Construct the initial heap, with least frequent element in
    //   heap[SMALLEST]. The sons of heap[n] are heap[2*n] and heap[2*n+1].
    //   heap[0] is not used.
    fheap_len = 0, fheap_max = HEAP_SIZE;

    for (n = 0; n < elems; n++)
    {
        if (tree[n].Freq != 0)
        {
            fheap[++fheap_len] = max_code = n;
            fdepth[n] = 0;
        }
        else
            tree[n].Len = 0;
    }

    // The pkzip format requires that at least one distance code exists, and
    //   that at least one bit should be sent even if there is only one possible
    //   code. So to avoid special checks later on we force at least two codes of
    //   non zero frequency.
    while (fheap_len < 2)
    {
        int nw = fheap[++fheap_len] = ((max_code < 2) ? ++max_code : 0);

        tree[nw].Freq = 1;
        fdepth[nw] = 0;
        fopt_len--;

        if (stree)
            fstatic_len -= stree[nw].Len;

        // new is 0 or 1 so it does not have extra bits
    }

    desc->max_code = max_code;

    // The elements heap[heap_len/2+1 .. heap_len] are leaves of the tree,
    //   establish sub-heaps of increasing lengths:

    for (n = fheap_len / 2; n >= 1; n--)
        pqdownheap(tree, n);

    // Construct the Huffman tree by repeatedly combining the least two
    //   frequent nodes.
    do
    {
        pqremove(tree, n);          // n = node of least frequency
        m = fheap[SMALLEST];       // m = node of next least frequency
        fheap[--fheap_max] = n; // keep the nodes sorted by frequency
        fheap[--fheap_max] = m;

        // Create a new node father of n and m
        tree[node].Freq = (ush)(tree[n].Freq + tree[m].Freq);   // RCV Added (ush)
        fdepth[node] = (uch)(Max(fdepth[n], fdepth[m]) + 1);
        tree[n].Dad = tree[m].Dad = (ush) node;                   // RCV Added (ush)
#ifdef DUMP_BL_TREE
        if (tree == bl_tree)
        {
            DLLprintf("\nnode %d(%d), sons %d(%d) %d(%d)", node, tree[node].Freq, n,
                      tree[n].Freq, m, tree[m].Freq);
        }
#endif

        // and insert the new node in the heap
        fheap[SMALLEST] = node++;

        pqdownheap(tree, SMALLEST);
    }
    while (fheap_len >= 2);

    fheap[--fheap_max] = fheap[SMALLEST];

    // At this point, the fields freq and dad are set. We can now generate the
    //   bit lengths.
    gen_bitlen((tree_desc *)desc);

    // The field len is now set, we can generate the bit codes
    gen_codes((ct_data *)tree, max_code);
}

// Scan a literal or distance tree to determine the frequencies of the codes
//   in the bit length tree. Updates opt_len to take into account the repeat
//   counts. (The contribution of the bit length codes will be added later
//   during the construction of bl_tree.) tree :: The tree to be scanned.
//   max_code :: And its largest code of non zero frequency.
void __fastcall ZipDflt::scan_tree(ct_data *tree, int max_code)
{
    int n;                      // iterates over all tree elements
    int prevlen = -1;           // last emitted length
    int curlen;                 // length of current code
    int nextlen = tree[0].Len;  // length of next code
    int count = 0;              // repeat count of the current code
    int max_count = 7;          // max repeat count
    int min_count = 4;          // min repeat count

    if (nextlen == 0)
        max_count = 138, min_count = 3;

    tree[max_code + 1].Len = (ush) - 1;           // guard

    for (n = 0; n <= max_code; n++)
    {
        curlen = nextlen;
        nextlen = tree[n + 1].Len;

        if (++count < max_count && curlen == nextlen)
        {
            continue;
        }
        else
            if (count < min_count)
            {
                fbl_tree[curlen].Freq += (ush) count;  // RCV Added (ush)
            }
            else
                if (curlen != 0)
                {
                    if (curlen != prevlen)
                        fbl_tree[curlen].Freq++;

                    fbl_tree[REP_3_6].Freq++;
                }
                else
                    if (count <= 10)
                    {
                        fbl_tree[REPZ_3_10].Freq++;
                    }
                    else
                    {
                        fbl_tree[REPZ_11_138].Freq++;
                    }

        count = 0;
        prevlen = curlen;
        if (nextlen == 0)
        {
            max_count = 138, min_count = 3;
        }
        else
            if (curlen == nextlen)
            {
                max_count = 6, min_count = 3;
            }
            else
            {
                max_count = 7, min_count = 4;
            }
    }
}

// Send a literal or distance tree in compressed form, using the codes in
//   bl_tree. tree :: The tree to be scanned. max_code :: And its largest code
//   of non zero frequency.
void __fastcall ZipDflt::send_tree(ct_data *tree, int max_code)
{
    int n;                      // iterates over all tree elements
    int prevlen = -1;           // last emitted length
    int curlen;                 // length of current code
    int nextlen = tree[0].Len;  // length of next code
    int count = 0;              // repeat count of the current code
    int max_count = 7;          // max repeat count
    int min_count = 4;          // min repeat count

    // tree[max_code+1].Len = -1;
    // guard already set

    if (!nextlen)
        max_count = 138, min_count = 3;

    for (n = 0; n <= max_code; n++)
    {
        curlen = nextlen;
        nextlen = tree[n + 1].Len;

        if (++count < max_count && curlen == nextlen)
        {
            continue;
        }
        else
            if (count < min_count)
            {
                do
				{
					send_code(curlen, fbl_tree);
                }
                while (--count != 0);
            }
            else
                if (curlen != 0)
                {
                    if (curlen != prevlen)
                    {
                        send_code(curlen, fbl_tree);
                        count--;
                    }

                    Assert(count >= 3 && count <= 6, _T(" 3_6?"));

                    send_code(REP_3_6, fbl_tree);
                    send_bits(count - 3, 2);
                }
                else
                    if (count <= 10)
                    {
                        send_code(REPZ_3_10, fbl_tree);
                        send_bits(count - 3, 3);
                    }
                    else
                    {
                        send_code(REPZ_11_138, fbl_tree);
                        send_bits(count - 11, 7);
                    }

        count = 0;

        prevlen = curlen;

        if (!nextlen)
        {
            max_count = 138, min_count = 3;
        }
        else
            if (curlen == nextlen)
            {
                max_count = 6, min_count = 3;
            }
            else
            {
                max_count = 7, min_count = 4;
            }
    }
}

// Construct the Huffman tree for the bit lengths and return the index in
//   bl_order of the last bit length code to send.
int __fastcall ZipDflt::build_bl_tree(void)
{
    int max_blindex;            // index of last bit length code of non zero freq

    // Determine the bit length frequencies for literal and distance trees
    scan_tree((ct_data *)fdyn_ltree, fl_desc.max_code);
    scan_tree((ct_data *)fdyn_dtree, fd_desc.max_code);

    // Build the bit length tree:
    build_tree((tree_desc *)(&fbl_desc));

    // opt_len now includes the length of the tree representations, except the
    //   lengths of the bit lengths codes and the 5+5+4 bits for the counts.
    // Determine the number of bit length codes to send. The pkzip format
    //   requires that at least 4 bit length codes be sent. (appnote.txt says 3
    //   but the actual value used is 4.)

    for (max_blindex = BL_CODES - 1; max_blindex >= 3; max_blindex--)
    {
        if (fbl_tree[bl_order[max_blindex]].Len != 0)
            break;
    }

    // Update opt_len to include the bit length tree and counts
    fopt_len += (unsigned)(3 * (max_blindex + 1) + 5 + 5 + 4);
    //  Tracev(("\ndyn trees: dyn %ld, stat %ld"), (fopt_len, fstatic_len));

    return max_blindex;
}

// Send the header for a block using dynamic Huffman trees: the counts, the
//   lengths of the bit length codes, the literal tree and the distance tree.
//   IN assertion: lcodes >= 257, dcodes >= 1, blcodes >= 4. lcodes, dcodes,
//   blcodes :: Number of codes for each tree.
void ZipDflt::send_all_trees(int lcodes, int dcodes, int blcodes)
{
    int rank;                   // index in bl_order
    /*  Assert(lcodes >= 257 && dcodes >= 1 && blcodes >= 4, "not enough codes");
      Assert(lcodes <= L_CODES && dcodes <= D_CODES && blcodes <= BL_CODES,
              "too many codes");
      Tracev(("\nbl counts: "), 0);   */
    send_bits(lcodes - 257, 5);

    // not +255 as stated in appnote.txt 1.93a or -256 in 2.04c
    send_bits(dcodes - 1, 5);
    send_bits(blcodes - 4, 4);  // not -3 as stated in appnote.txt

    for (rank = 0; rank < blcodes; rank++)
    {
        //    Tracev(("\nbl code %2d "), (bl_order[rank]));
        send_bits(fbl_tree[bl_order[rank]].Len, 3);
    }

    //  Tracev(("\nbl tree: sent %ld"), (fbits_sent));
    send_tree((ct_data *)fdyn_ltree, lcodes - 1);  // send the literal tree
    //  Tracev(("\nlit tree: sent %ld"), (fbits_sent));
    send_tree((ct_data *)fdyn_dtree, dcodes - 1);  // send the distance tree
    //  Tracev(("\ndist tree: sent %ld"), (fbits_sent));
}

// Determine the best encoding for the current block: dynamic trees, static
//   trees or store, and output the encoded block to the zip file. This
//   function returns the total compressed length for the file so far. buf ::
//   Input block, or NULL if too old. stored_len :: Length of input block. eof;
//   :: true if this is the last block for a file.
//ulg

//   strstart is set to the end of the current match.
//#define FLUSH_BLOCK(eof)
//  flush_block(fblock_start >= 0L ? (uch *) &fwindow[(unsigned)fblock_start] :
//  (uch *)NULL, (ulg)fstrstart - (ulg)fblock_start, (eof))
ZInt64 __fastcall  ZipDflt::FlushBlock(int eof)
{
    ulg opt_lenb, static_lenb;                // opt_len and static_len in bytes
    int max_blindex;                          // index of last bit length code of non zero freq
    fflag_buf[flast_flags] = fflags; // Save the flags for the last 8 items
    const uch *buf = fblock_start >= 0L ? (uch *) & fwindow[(unsigned)fblock_start] :
                     (uch *)NULL;
    ulg stored_len = (ulg)fstrstart - (ulg)fblock_start;

    // Check if the file is ascii or binary
    if (*ffile_type == (ush) UNKNOWN)
        set_file_type();

    // Construct the literal and distance trees
    build_tree((tree_desc *)(&fl_desc));
//  Tracev(("\nlit data: dyn %ld, stat %ld"), (fopt_len, fstatic_len));
    build_tree((tree_desc *)(&fd_desc));
//  Tracev(("\ndist data: dyn %ld, stat %ld"), (fopt_len, fstatic_len));

    // At this point, opt_len and static_len are the total bit lengths of the
    //   compressed block data, excluding the tree representations.
    // Build the bit length tree for the above two trees, and get the index in
    //   bl_order of the last bit length code to send.
    max_blindex = build_bl_tree();

    // Determine the best encoding. Compute first the block length in bytes
    opt_lenb = (fopt_len + 3 + 7) >> 3;
    static_lenb = (fstatic_len + 3 + 7) >> 3;

#ifdef DEBUG
    finput_len += stored_len;              // for debugging only
#endif
//  Trace(("\nopt %lu(%lu) stat %lu(%lu) stored %lu lit %u dist %u ",
//         opt_lenb, fopt_len, static_lenb, fstatic_len,
//         stored_len, flast_lit, flast_dist));

    if (static_lenb <= opt_lenb)
        opt_lenb = static_lenb;

    // If compression failed and this is the first and last block, and if the
    //   zip file can be seeked (to rewrite the local header), the whole file is
    //   transformed into a stored file:
#ifdef FORCE_METHOD
    if (flevel == 1 && eof && fcompressed_len == 0ui64)
    { // force stored file v1.6018
#else
    if (stored_len <= opt_lenb
            && eof
            && fcompressed_len == 0//ui64
            && fZipOutfile->GetIsSeekable())
//            && (fCanSeek || seekable())) // 1.75 only need to check once
    { // v1.6018
#endif

        // Since LIT_BUFSIZE <= 2 * ZWSIZE, the input data must be there:
        if (buf == NULL)
            throw DZException(DZ_ERM_LOGIC_ERROR);

//      FatalError(ZEN_LOGIC07);

        copy_block(buf, (unsigned)stored_len, 0);                   // without header
		fcompressed_len = ((__int64)stored_len) << 3;//ui64;  // v1.6018
        *ffile_method = STORE;
    }
    else
#ifdef FORCE_METHOD
        if (flevel == 2 && buf != (uch *)NULL)
        { // force stored block
#else
        if (stored_len + 4 <= opt_lenb && buf != (uch *)NULL)
        {
            // 4: two words for the lengths
#endif
            // The test buf != NULL is only necessary if LIT_BUFSIZE > ZWSIZE.
            //   Otherwise we can't have processed more than WSIZE input bytes
            //   since the last block flush, because compression would have been
            //   successful. If LIT_BUFSIZE <= ZWSIZE, it is never too late to
            //   transform a block into a stored block.
            send_bits((STORED_BLOCK << 1) + eof, 3);                // send block type
			fcompressed_len = (__int64)(((unsigned __int64)fcompressed_len + 3 + 7) & ~7ui64); // v1.6018
            fcompressed_len += ((unsigned __int64)(stored_len + 4)) << 3;  // v1.6018
            copy_block(buf, (unsigned)stored_len, 1);                     // with header
#ifdef FORCE_METHOD
        }
        else
            if (flevel == 3)
            { // force static trees
#else
        }
        else
            if (static_lenb == opt_lenb)
            {
#endif
                send_bits((STATIC_TREES << 1) + eof, 3);
                compress_block((ct_data *)fstatic_ltree, (ct_data *)fstatic_dtree);
                fcompressed_len += 3 + fstatic_len;
            }
            else
            {
                send_bits((DYN_TREES << 1) + eof, 3);
                send_all_trees(fl_desc.max_code + 1, fd_desc.max_code + 1, max_blindex + 1);
                compress_block((ct_data *)fdyn_ltree, (ct_data *)fdyn_dtree);
                fcompressed_len += 3 + fopt_len;
            }

//  Assert(fcompressed_len == (unsigned __int64)fbits_sent, "bad compressed size");
    init_block();

    if (eof)
    {
        Assert(finput_len == fisize, _T("bad input size"));
        bi_windup();
        fcompressed_len += 7;                  // align on byte boundary
    }

//  Tracev(("\ncomprlen %Lu(%Lu) "), (fcompressed_len >> 3,
//                                      fcompressed_len - 7 * eof));

    return (fcompressed_len >> 3);
}

// Save the match info and tally the frequency counts. Return true if
//   the current block must be flushed. dist :: Distance of matched string.
//   lc :: Match length-MIN_MATCH or unmatched char (if dist==0)
int __fastcall ZipDflt::ct_tally(int dist, int lc)
{
    fl_buf[flast_lit++] = (uch) lc;

    if (!dist)
    {
        // lc is the unmatched char
        fdyn_ltree[lc].Freq++;
    }
    else
    {
        // Here, lc is the match length - MIN_MATCH
        dist--;                                     // dist = match distance - 1
        Assert((ush) dist < (ush) MAX_DIST && (ush) lc <= (ush)(MAX_MATCH - MIN_MATCH)
               && (ush) d_code(dist) < (ush) D_CODES, _T("ct_tally: bad match"));

        fdyn_ltree[flength_code[lc] + LITERALS + 1].Freq++;
        fdyn_dtree[d_code(dist)].Freq++;

        fd_buf[flast_dist++] = (ush) dist;  // RCV Added (ush)
        fflags |= fflag_bit;
    }

    fflag_bit <<= 1;

    // Output the flags if they fill a byte:

    if ((flast_lit & 7) == 0)
    {
        fflag_buf[flast_flags++] = fflags;
        fflags = 0, fflag_bit = 1;
    }

    // Try to guess if it is profitable to stop the current block here
    if (flevel > 2 && (flast_lit & 0xFFF) == 0)
    {
        // Compute an upper bound for the compressed length
        ulg out_length = (ulg) flast_lit * 8L;
		ulg in_length = (ulg) fstrstart - (ulg)fblock_start;
        int dcode;

        for (dcode = 0; dcode < D_CODES; dcode++)
        {
			out_length += (ulg) fdyn_dtree[dcode].Freq * (ulg)(5L + extra_dbits[dcode]);
        }

        out_length >>= 3;

        /*    Trace(("\nlast_lit %u, last_dist %u, in %ld, out ~%ld(%ld%%) "), (flast_lit, f
                     last_dist, in_length, out_length, 100L - out_length * 100L / in_length
             ));*/

        if (flast_dist < flast_lit / 2 && out_length < in_length / 2)
            return 1;
    }

    return (flast_lit == LIT_BUFSIZE - 1 || flast_dist == DIST_BUFSIZE);

    // We avoid equality with LIT_BUFSIZE because of wraparound at 64K on
    //   16 bit machines and because stored blocks are restricted to 64K-1
    //   bytes.
}

// Send the block data compressed using the given Huffman trees ltree ::
//   Literal tree. dtree :: Distance tree.
void ZipDflt::compress_block(ct_data *ltree, ct_data *dtree)
{
    unsigned  dist;                   // distance of matched string
    int       lc;                     // match length or unmatched char (if dist == 0)
    unsigned  lx = 0;                 // running index in l_buf
    unsigned  dx = 0;                 // running index in d_buf
    unsigned  fx = 0;                 // running index in flag_buf
    uch       flag = 0;               // current flags
    unsigned  code;                   // the code to send
    int       extra;                  // number of extra bits to send

    if (flast_lit != 0)
    {
        do
        {
            if ((lx & 7) == 0)
                flag = fflag_buf[fx++];

            lc = fl_buf[lx++];

            if ((flag & 1) == 0)
            {
                send_code(lc, ltree); // send a literal byte
                //        Tracecv(isgraph(lc), (" '%c' "), (lc));
            }
            else
            {
                // Here, lc is the match length - MIN_MATCH
                code = flength_code[lc];
                send_code(code + LITERALS + 1, ltree);  // send the length code
                extra = extra_lbits[code];

                if (extra != 0)
                {
                    lc -= fbase_length[code];
                    send_bits(lc, extra);                 // send the extra length bits
                }

                dist = fd_buf[dx++];

                // Here, dist is the match distance - 1
                code = d_code(dist);
                Assert(code < D_CODES, _T("bad d_code"));

                send_code(code, dtree);                 // send the distance code
                extra = extra_dbits[code];

                if (extra != 0)
                {
					dist -= (unsigned)fbase_dist[code];
					send_bits((int)dist, extra);               // send the extra distance bits
                }
            } // literal or match pair ?

            flag >>= 1;
        }
        while (lx < flast_lit);
    }

    send_code(END_BLOCK, ltree);
}

// Set the file type to ASCII or BINARY, using a crude approximation:
//   binary if more than 20% of the bytes are <= 6 or >= 128, ascii
//   otherwise. IN assertion: the fields freq of dyn_ltree are set and the
//   total of all frequencies does not exceed 64K (to fit in an int on 16
//   bit machines).
void ZipDflt::set_file_type(void)
{
    unsigned  ascii_freq = 0;
    unsigned  bin_freq = 0;
    int       n = 0;

    while (n < 7)
        bin_freq += fdyn_ltree[n++].Freq;

    while (n < 128)
        ascii_freq += fdyn_ltree[n++].Freq;

    while (n < LITERALS)
        bin_freq += fdyn_ltree[n++].Freq;

    *ffile_type = (ush)((bin_freq > (ascii_freq >> 2)) ? BINARY : ASCII);  // RCV Added

    ///* (ush)
//  if (*ffile_type == BINARY && ftranslate_eol)
//  {
//    Notify(PF, IWARNING, "-l used on binary file");
//  }
}

/* 29/1/07 */


