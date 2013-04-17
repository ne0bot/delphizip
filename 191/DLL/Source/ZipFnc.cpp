#include "stdafx.h"
#pragma hdrstop

#include "ZipFnc.h"

#include <assert.h>
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ZIPFNC_CPP

/* ZGlobals.c
 * Copyright (C) 1990-1996 Mark Adler, Richard B. Wales, Jean-loup Gailly,
 * Kai Uwe Rommel, Onno van der Linden and Igor Mandrichenko.
 * Permission is granted to any individual or institution to use, copy, or
 * redistribute this software so long as all of the original files are included,
 * that it is not sold for profit, and that this copyright notice is retained.
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


ZipFunc::ZipFunc(const DllCommands *C): ZipDflt(C)//, BaseOpts()
{
    fndlist = NULL;       // quicklist (internal) of found files
    IntList = NULL;
//    HdrList = NULL;
    faction = ADD;                 // Must be ADD - the default action
    fzipstate = -1;
    fuser_key = NULL;
    fglobal_error_code = 0;
    ffiles_acted_on = 0;
    CurPW = NULL;
    CurBase = NULL;
    fzcomlen = 0;
    fSpecials = NULL;
    fpcount = 0;
    ffound = NULL;
    fzfiles = NULL;
    VerFiles = NULL;
    fzfound = NULL;
    fzcount = 0;
    flatest = 0;
    ffcount = 0;
    faction = 0;
	fversion = 0;
	fjunk_sfx = 0;
    frecurse = 0;
    flinkput = 0;
    fBatchStarted = 0;
    fcenbeg = 0;
    fzipbeg = 0;
    fAllowGrow = 0;

    fhwbuf = NULL;
    fpathput = 1;
    fdirnames = 1;
	fvolume_label = 0;

    // linked list for new files to be added (not yet in ZIP)
    ffnxt = &ffound;
    fadjust = 1;
	fEncodeAs = C->fEncodeAs;     // write strings
	fArchiveFiles = C->fOptions.ArchiveFilesOnly;
	fResetArchiveBit = C->fOptions.ResetArchiveBit;
	// build the conventional cmd line switches
    if (C->fOptions.NoDirEntries) // Do not add directory entries -D
        fdirnames = 0;
    if (C->fOptions.Grow) // Allow appending to a zip file -g Normally TRUE
        fAllowGrow = 255;
    if (C->fOptions.JunkDir) // Junk directory names -j
        fpathput = 0;
    if (C->fOptions.JunkSFX) // Junk sfx prefix
        fjunk_sfx = 1;
    if (C->fOptions.Move) // Delete files added or updated in zip file -m
        fdispose = 1;
    if (C->fOptions.LatestTime) // Set zip file time to time of latest file in it -o
		flatest = 1;
//	if (fNTFSStamps)
//		fdirnames = 1;//0;
	if (C->fOptions.Volume) // Include volume label -$
        fvolume_label = 1;

    fHowToMove = C->fOptions.HowToMove;
    fNoPrecalc = C->fOptions.NoPrecalc;
    fGEncrypt = C->fOptions.Encrypt;
    frecurse = C->fOptions.Recurse;
	fbefore = C->fDate;
    fversion = C->fOptions.Versioning;
    VerDate = 0;
}

ZipFunc::~ZipFunc(void)
{
    if (fSpecials) {          
        delete fSpecials;
        fSpecials = NULL;
    }
    ZipCleanup();   // something may have failed
    if (CurPW) {
        PWRec* t, *p = CurPW;
        CurPW = 0;
        while (p)
        {
            t = p;
            p = p->Next;
            delete t;
        }
    }
    if (CurBase) {
        BaseRec* t, *p = CurBase;
        CurBase = 0;
        while (p)
        {
            t = p;
            p = p->Next;
            delete t;
        }
    }

    if (IntList)
    {
        delete IntList;
        IntList = NULL;
    }

    if (fndlist)
    {
        delete fndlist;
        fndlist = NULL;
    }

    FndItem *f; // steps through found list
	ZipItem *z; // pointer to next entry in zfiles list
    while (ffound != NULL)
    {
        f = ffound;
        ffound = f->nxt;
        delete f;
    }

    while (fzfiles != NULL)
	{
        z = fzfiles;
        fzfiles = z->nxt;
        delete z;
    }

    while (VerFiles != NULL)
	{
        z = VerFiles;
        VerFiles = z->nxt;
        delete z;
    }

    while (fzfound != NULL)
    {
        z = fzfound;
        fzfound = z->nxt;
        delete z;
    }
}

int ZipFunc::Init(void) // after construction
{
    int r = ZipDflt::Init();
    if (r)
        return r;
    // do active initialisation

    DZStrW cd = CB->UserArg(zcbRootDir, 0, 0);
    if (cd.IsEmpty())    
        cd = _T(".\\");    // use current

	if (!GetFullPathName(cd.c_str(), MAX_PATH, fRootDir.GetBuffer(MAX_PATH), NULL))
    {
        throw DZFatalException(DZ_ERM_MEMORY);
	}
	fRootDir.ReleaseBuffer();
	if (fRootDir.LastChar() != BSLASH)
		fRootDir += BSLASH;
    AddBase(fRootDir, true);

    fGPassword = CB->UserArg(zcbPassword, 0, 0);
    AddPW(fGPassword, true);

    return 0;
}

XItem::XItem()
{
    finame = NULL;
	xname = NULL;
    fhname = NULL;
	options._opts = 0;
    Base = NULL;
    Passw = NULL;
    len = 0;
}

XItem::~XItem()
{
    if (fhname)
        delete[] fhname;
    if (finame)
        delete[] finame;
	if (xname)
		delete[] xname;
    // does not own Base or Passw
}

XItem::XItem(const XItem& other)
{
    fhname = NULL;
    finame = NULL;
	xname = NULL;
	options._opts = 0;
	if (other.Gethname())
		fhname = zstrdupB(other.Gethname());
	if (other.finame)
		finame = zstrdup(other.finame);
    if (other.xname)
		xname = zstrdup(other.xname);
//    how = other.how;
    Base = other.Base;
	Passw = other.Passw;
    // does not copy options or cextra
}

XItem& XItem::operator=(const XItem& other)
{
	if (&other != this) {
        if (fhname)
            delete[] fhname;
        if (finame)
            delete finame;
		if (xname)
			delete[] xname;
        fhname = NULL;
        finame = NULL;
        xname = NULL;
		if (other.Gethname())
            fhname = zstrdupB(other.Gethname());
		if (other.finame)
			finame = zstrdup(other.finame);
        if (other.xname)
			xname = zstrdup(other.xname);
        Base = other.Base;
		Passw = other.Passw;
		// does not copy options or cextra
		options._opts = other.options._opts;
    }
    return *this;
}

DZStrW __fastcall XItem::FullPath(void) const
{
    if (!Is_DrvEx(xname))
    {
        DZStrW tmp(Base);
        tmp += xname;
        return tmp;
    }
    return DZStrW(xname);
}

void __fastcall XItem::Setxname(const TCHAR *value)
{
	if (xname)
		delete[] xname;
	xname = NULL;
    if (value)
        xname = zstrdup(value);
}
                                           

void __fastcall XItem::Setiname(const TCHAR *value)
{
    if (finame)
        delete[] finame;
    finame = NULL;
    if (value)
        finame = zstrdup(value);
}
 
void __fastcall XItem::Sethname(const char* value)
{
    if (fhname)
        delete[] fhname;
    fhname = NULL;
    if (value)
        fhname = zstrdupB(value);
}

bool __fastcall XItem::GetIsFolder(void) const
{
    return finame[_tcslen(finame)- 1] == BSLASH;
}

HL_node::HL_node()
{
    nxt = NULL;
    hash = 0;
    xdata = NULL;
}

HL_block::HL_block(HL_block *prev)
{
    prv = prev;
    memset(nodes, 0, sizeof(nodes));
}

HL_block::~HL_block(void)
{
    if (prv)
    {
        HL_block *tmp = prv;
        prv = NULL;
        delete tmp;
    }
}

typedef HL_node* PHL_node;

HashList::HashList(int siz)
{
    unsigned int size = 512;  // smallest allowing MAX_PATH strings
    while (size < 16384 && size < (unsigned)siz)
        size += size;
	Mask = size - 1;
	Table = new PHL_node[size];
	memset(Table, 0, size * (int)sizeof(PHL_node));
    blocks = NULL;
    nno = 0;
}

HashList::~HashList(void)
{
    if (blocks)
        delete blocks;
    delete [] Table;
}


HL_node * HashList::NewNode(const XItem *x, ulg hash)
{
    if (!blocks || nno >= NODE_BLOCK_SIZE)
    {
        blocks = new HL_block(blocks);
        nno = 0;
    }
    HL_node *ret = &blocks->nodes[nno++];
    ret->nxt = NULL;
    ret->hash = hash;
    ret->xdata = x;
    return ret;
}

const XItem *HashList::AddANode(const XItem *xname)
{
	HL_node *lr, *lt;
    lr = Prepare();
    if (!lr)
    {
        // no entries
        Table[Index] = NewNode(xname, Hash);  // add it
        return NULL;
    }
    lt = NULL;
    while (lr)
    {
        if (Hash == lr->hash && Matches(lr))
        {
            // exists
            return lr->xdata;  // return existing
        }

        lt = lr;
        lr = lr->nxt;
    }
    // chain it to lt
    if (lt)
        lt->nxt = NewNode(xname, Hash);

    return NULL;
}

XItem *HashList::FindAName(void)
{
    HL_node* lr = Prepare();
    while (lr)  // search chain
    {
        if (Hash == lr->hash && Matches(lr))
            break; // exists @lr
        lr = lr->nxt;
    }
    if (lr)
        return (XItem*)lr->xdata;
    return NULL;
}

HashListExt::~HashListExt(void)
{
    //
}
                                         
XItem *HashListExt::FindName(const DZStrW& name)
{
    fxname = name;
    return FindAName();
}

// returns pointer to existing entry if duplicate
const XItem *HashListExt::AddNode(const XItem *xname_)
{
	fxname = xname_->GetXName();
    return AddANode(xname_);
}

bool HashListExt::Matches(const HL_node *node) const
{
	return fxname.CompareNoCase(node->xdata->GetXName().c_str()) == 0;
}

HL_node* HashListExt::Prepare(void)
{
//    fxname.ToUpper();
	fxname.ToLower();
	int len = (int)fxname.length();
	Hash = crc32(0, (uch*)fxname.c_str(), len * (int)sizeof(TCHAR));
    Index = Hash & Mask;
	Hash = (Hash & ~Mask) | ((unsigned)len & Mask);
    return Table[Index];
}

HashListInt::~HashListInt(void)
{
    //
}
                                           
XItem *HashListInt::FindName(const DZStrW& name)
{                      
    finame = name;
    return FindAName();
}

// returns pointer to existing entry if duplicate
const XItem *HashListInt::AddNode(const XItem *xname)
{                             
    finame = xname->GetIName();
    return AddANode(xname);
}

bool HashListInt::Matches(const HL_node *node) const
{
	return finame.CompareNoCase(node->xdata->GetIName().c_str()) == 0;
}

HL_node* HashListInt::Prepare(void)
{
//    finame.ToUpper();
	finame.ToLower();
	unsigned int len = finame.length();
	Hash = crc32(0, (uch*)finame.c_str(), (int)(len * (int)sizeof(TCHAR)));
	Index = Hash & Mask;
	Hash = (Hash & ~Mask) | (len & Mask);
    return Table[Index];
}

ZipItem::ZipItem() : XItem()
{
    vem = 0;
	ver = 0;
	flg = 0;
	how = 0;
	nam = 0;
	SetExt(0);
	SetCext(0);
    com = 0;
    dsk = 0;
    att = 0;
    lflg = 0;
    atx = 0;
    tim = 0;
    crc = 0;
    siz = 0;
    off = 0;
	extra = NULL;
	cextra = NULL;
    fcomment = NULL;
    ntfs = NULL;
    mark = 0;
    trash = 0;
    nxt = NULL;
}        

ZipItem::ZipItem(const ZipItem& other): XItem(other)
{
	Copy(other);
}

ZipItem& ZipItem::operator=(const ZipItem& other)
{
	if (&other != this)
	{
		*((XItem*)this) = (const XItem&)other;
		Copy(other);
		nxt = other.nxt;
	}
	return *this;
}

ZipItem::ZipItem(const FndItem* f): XItem(*f)
{
    vem = 0;
    ver = 0;
    flg = 0;
    how = 0;
    nam = 0;
	SetExt(0);
	SetCext(0);
    com = 0;
    dsk = 0;
    att = 0;
    lflg = 0;
    atx = 0;
    tim = 0;
    crc = 0;
    siz = 0;
	off = 0;
	ntfs = NULL;
    fcomment = NULL;
    mark = 0;
    trash = 0;
    nxt = NULL;
	options = f->options;
	len = f->len; // RCV added.
}

ZipItem::~ZipItem()
{
    if (fcomment)
        delete[] fcomment;
	if (ntfs)
		delete ntfs;
}

void __fastcall ZipItem::Copy(const ZipItem& other)
{
	if (&other != this)
	{
		vem = other.vem;
		ver = other.ver;
		flg = other.flg;
		how = other.how;
		nam = other.nam;
		SetExt(other.GetExt());
		SetCext(other.GetCext());
		com = other.com;
        dsk = other.dsk;
		att = other.att;
        lflg = other.lflg;
		atx = other.atx;
        tim = other.tim;
        crc = other.crc;
		siz = other.siz;
        len = other.len;
		off = other.off;
        mark = other.mark;
		trash = other.trash;
		nxt = NULL;
		extra = NULL;
		cextra = NULL;
        fcomment = NULL;
		extra = other.extra;
        cextra = other.cextra;
		SetComment(other.GetComment());

		ntfs = NULL;
		if (other.ntfs)
		{
			ntfs = new XNTFSData;
            memcpy(ntfs, other.ntfs, sizeof(XNTFSData));
		}
	}
}

void __fastcall ZipItem::SetComment(const DZStrW& value)
{
    if (fcomment)
        delete[] fcomment;
    fcomment = NULL;
	if (!value.IsEmpty())
    {
        fcomment = (wchar_t*) DupStr(value);
        com = value.length();
    }
    else
        com = 0;
}

FndItem::FndItem(): XItem()
{
    nxt = NULL;
}

FndItem::FndItem(const XItem& other) : XItem(other)
{
    nxt = NULL;
}

FndItem::~FndItem()
{
  	//
}


// ? CompNameExt compare ends of strings for name.ext
int __fastcall SameNameExt(const DZStrW& fname, const DZStrW& oname)
{
  if (fname.IsEmpty() || oname.IsEmpty())
    return 0;         // cannot do it
  int fs, os;
  if (fname.LastChar() != oname.LastChar())
      return 0;
  fs = fname.ReverseFind(_T('\\'));
  os = oname.ReverseFind(_T('\\'));
  DZStrW f = fs >= 0 ? fname.Mid((unsigned)fs+1) : fname;
  DZStrW o = os >= 0 ? oname.Mid((unsigned)os+1) : oname;
  return (f.length() == o.length()) ? f.CompareNoCase(o.c_str()) == 0 : 0;
}

