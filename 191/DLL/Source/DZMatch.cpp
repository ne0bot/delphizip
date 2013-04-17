#include "stdafx.h"
#pragma hdrstop
#include "zstrings.h"

//#include "dzmatch.h"

/* DZMatch.cpp * Copyright (C) 1997 Mike White, Eric W. Engler and Russell Peters
* Permission is granted to any individual or institution to use, copy, or
* redistribute this software so long as all of the original files are included,
* that it is not sold for profit, and that this copyright notice is retained.
* This version modified by Chris Vleghert BCB/Delphi Zip.
** distributed under LGPL license
** see license.txt for details

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
	updated 2010-AUG-22
************************************************************************/


struct TBounds
{
	wchar_t* Start;
	wchar_t* Finish;
};

struct TParts
{
	TBounds Main;
	TBounds Extn;
	int MainLen;
	int ExtnLen;
};

// return <0 _ match to *, 0 _ match to end, >0 _ no match
static __fastcall int Wild(TBounds& bp, TBounds& bs)
{
	wchar_t cp, cs;
	// handle matching characters before wild
	while ((bs.Start <= bs.Finish) && (bp.Start <= bp.Finish))
	{
		cp = *bp.Start;
		cs = *bs.Start;
		if (cp != cs)
		{
			if (cp == '*')
				break; // matched to *
			// would match anything except path sep
			if ((cp != '?') || (cs == '\\'))
				return 1; // no match
		}
		// they match
		++bp.Start;
		++bs.Start;
	}
	// we have * or eos
	if (bp.Start > bp.Finish && bs.Start > bs.Finish)
		return 0; // matched to end
	// handle matching characters from end back to *
	while (bs.Start <= bs.Finish)
	{
		cp = *bp.Finish;
		cs = *bs.Finish;
		if (cp != cs)
		{
			if (cp == '*')
				break;
			// must not match path sep
			if ((cp != '?') || (cs == '\\'))
				return 1;
		}
		// they match
		--bp.Finish;
		--bs.Finish;
	}
	return -1;
}

static __fastcall int WildCmp(TBounds bp, TBounds bs)
{
	// quick check for '*'
	if ((bp.Start == bp.Finish) && (*bp.Start == '*'))
		return 0; // matches any/none
	// no more spec?
	if (bs.Finish < bs.Start)
		return (bp.Finish < bp.Start) ? 0 : 1; // empty matches empty
	// handle matching characters before wild
	int Result = Wild(bp, bs);
	if (Result >= 0)
		return Result;
	wchar_t* pidx = bp.Start;
	wchar_t* sidx = bs.Start;
	if (bp.Start > bp.Finish)
		return(sidx <= bs.Finish) ? 1 : 0;
	// handle wild
	if (sidx <= bs.Finish && *pidx == '*')
	{
		// skip multiple *
		while (pidx < bp.Finish && *(pidx + 1) == '*' && *pidx == '*')
			++pidx;
		// end of pattern?
		if (pidx == bp.Finish)
			return 0;  // match
		TBounds bpt, bst;
		bpt.Start = ++pidx;
		bpt.Finish = bp.Finish;
		bst.Start = sidx;
		bst.Finish = bs.Finish;
		while (bst.Start <= bst.Finish)
		{
			// recursively compare sub patterns
			int sm = WildCmp(bpt, bst);
			if (!sm)
				return 0;  // match
			++bst.Start;
		}
		return 1; // no match
	}
	// end of spec - pattern must only have *
	while ((pidx <= bp.Finish) && (*pidx == '*'))
		++pidx;
	if (pidx > bp.Finish)
		return 0;  // match
	return -1;	// matched so far
}

// returned bit values
const int MAIN = 0x01; // not empty
const int MAIN_WILDALL = 0x02; // is *
const int MAIN_HASWILD = 0x04; 
const int HAD_DOT = 0x08; 
const int EXTN = 0x10; 
const int EXTN_WILDALL = 0x20;
const int EXTN_HASWILD = 0x40;

static __fastcall int Decompose(wchar_t* &idx, TParts &parts)
{
	int Result = 0;
	int mwildall = 0;
	int xwildall = 0;
	// parts.MainLen = 0;
	parts.ExtnLen = 0;
	wchar_t* ExtnStart = NULL;
	wchar_t* ExtnFinish = NULL;
	// start of text or spec
	wchar_t* MainStart = idx;
	wchar_t* MainFinish = NULL;
	bool trying = true;
	while (trying)
	{
		wchar_t c = *idx;
		// find last dot ignoring those in paths
		switch(c)
		{
		case '.':
			if (idx > MainStart)
			{ // we probably have extn
//				if (ExtnStart)
				mwildall += xwildall; // catch all * in main
				xwildall = 0;
				ExtnStart = idx + 1;
			}
			break;
		case '/':
			*idx = '\\'; // normalize path seps
		case ':':
		case '\\':
			if (ExtnStart)
			{ // was false start of extn
				ExtnStart = NULL; // no extn (yet)
				mwildall += xwildall;
				xwildall = 0;
			}
			break;
		case ' ':
			{
				// space can be embedded but cannot trail
				wchar_t* tmp = idx - 1;
				++idx;
				while (*idx == ' ')
					++idx;
				if (*idx < ' ' || *idx == '|')
				{
					if (!*idx)
						idx--;
					// terminate
					MainFinish = tmp;
					trying = false;
				}
				else
					--idx;
			}
			break;
		case '*':
			if (ExtnStart)
				++xwildall;
			else
				++mwildall;
			break;
		case '\0':
			MainFinish = idx - 1;
			trying = false;
			--idx;
			break;
		case '|':
		case '\1':
		case '\2':
		case '\3':
		case '\4':
		case '\5':
		case '\6':
		case '\7':
		case '\x8':
		case '\x9':
		case '\x0a':
		case '\x0b':
		case '\x0c':
		case '\x0d':
		case '\x0e':
		case '\x0f':
		case '\x10':
		case '\x11':
		case '\x12':
		case '\x13':
		case '\x14':
		case '\x15':
		case '\x16':
		case '\x17':
		case '\x18':
		case '\x19':
		case '\x1a':
		case '\x1b':
		case '\x1c':
		case '\x1d':
		case '\x1e':
		case '\x1f':
			MainFinish = idx - 1;
			trying = false;
			break;
		}
		++idx;
	}
	// was there an extension?
	if (ExtnStart)
	{
		Result |= HAD_DOT;
		if (ExtnStart <= MainFinish)
		{ // we have extn
			ExtnFinish = MainFinish;
			MainFinish = ExtnStart - 2;
			parts.ExtnLen = 1 + (int) (ExtnFinish - ExtnStart);
			Result |= EXTN;
			if (xwildall)
			{
				if (xwildall == parts.ExtnLen)
					Result |= EXTN_WILDALL;
				Result |= EXTN_HASWILD;
			}
		}
		else
		{
			// dot but no extn
			ExtnStart = NULL;
			--MainFinish; // before dot
		}
	}
	parts.MainLen = 1 + (int)(MainFinish - MainStart);
	if (parts.MainLen > 0)
	{
		Result |= MAIN;
		if (mwildall)
		{
			if (mwildall == parts.MainLen)
				Result |= MAIN_WILDALL;
			Result |= MAIN_HASWILD;
		}
	}
	// set resulting pointers
	parts.Main.Start = MainStart;
	parts.Main.Finish = MainFinish;
	parts.Extn.Start = ExtnStart;
	parts.Extn.Finish = ExtnFinish;
	return Result;
}

// only gets called to compare same length names
static int FileRCmp(TBounds& Bp, TBounds& Bs)
{
	if (Bs.Start > Bs.Finish)
		return 1;  // no match
	if ((*Bp.Start != *Bs.Start) && (*Bp.Start == '\\' || *Bp.Start != '?'))
		return 1; // cannot match
	++Bs.Start;
	++Bp.Start;
	while ((Bs.Start <= Bs.Finish))
	{
		wchar_t cp = *Bp.Finish--;
		wchar_t cs = *Bs.Finish--;
		if (cp != cs)
		{
			// must not match path sep
			if ((cp != '?') || (cs == '\\'))
				return 1; // no match
		}
	}
	return 0; // match
//	return (Bs.Start < Bs.Finish && Bp.Start < Bp.Finish) ? 0 : 1;
}

/*
class membuf
{
	wchar_t *buf;
	public:
	membuf(const wchar_t* src);
	~membuf(void){delete[]buf;}
	__property wchar_t *buffer={read=buf,write=buf};
};

membuf::membuf(const wchar_t* src)
{
	int len = 0;
	buf = NULL;
	if (src)
		len = wcslen(src);

	if (len)
	{
		buf = new wchar_t[(len | 63) + 1];
		wcsncpy(buf, src, len);
		buf[len] = 0;         // mark end - in case
		CharUpperBuffW(buf, len);
	}
}    */

bool __fastcall ZMatch(const DZStrW& thePattern, const DZStrW& theSpec)
{
	if (thePattern.IsEmpty() != theSpec.IsEmpty())
		return false;
//	membuf Pattern(thePattern.c_str());
//	membuf Spec(theSpec.c_str());
	// make unique lowercase
	DZStrW Pattern = thePattern.AsLower();
	DZStrW Spec = theSpec.AsLower();

	const int FULL_WILD = MAIN_WILDALL | EXTN_WILDALL;
	bool Result = false;
	TParts ptn, spc, spc1;
	// check the spec if has extension
	wchar_t* SpecStt = (wchar_t*)Spec.c_str();//Spec.buffer;
	wchar_t* sidx = SpecStt;
	while (*sidx <= ' ')
	{
		if (!(*sidx))
			return false;
		++sidx;
	}
	int sFlag = Decompose(sidx, spc);
	if (!spc.Main.Finish)
		return Result;
	// now start processing each pattern
	wchar_t* pidx = (wchar_t*)Pattern.c_str();//Pattern.buffer;
	do
	{
		wchar_t ch = *pidx;
		// skip garbage or separator
		while ((ch <= ' ') || (ch == '|'))
		{
			if (!ch)
				return false;
			ch = *(++pidx);
		}
		int pFlag = Decompose(pidx, ptn);
		// work out what we must test

		if (((pFlag & FULL_WILD) == FULL_WILD) ||
			((pFlag & (FULL_WILD | EXTN | HAD_DOT)) == MAIN_WILDALL))
			return true;
		if (((pFlag & (EXTN_HASWILD | EXTN)) == EXTN) &&
			(spc.ExtnLen != ptn.ExtnLen))
			continue; // cannot match
		if (!(pFlag & MAIN_HASWILD) && (spc.MainLen != ptn.MainLen))
			continue; // cannot match
		int xres = -1; // not tried to match
		// make copy of spc
		memcpy(&spc1, &spc, sizeof(TParts));
		if (pFlag & EXTN_WILDALL)
			xres = 0; // ignore extn as matched
		else
		{
			// if pattern has extn, we must 'split' spec
			if (pFlag & HAD_DOT)
			{
				// check special cases
				// pattern ends with dot?
				if (!(pFlag & EXTN))
				{
					// pattern ended in dot - spec must not have extn
					if (sFlag & EXTN)
						continue; // spec has extn -  cannot match
					xres = 0; // no extn to check
				}
				else
				{
					// spec must have extn
					if (!(sFlag & EXTN))
						continue; // no spec extn - cannot match
				}
			}
			else
			{
				// no Pattern dot _ test fill spec
				if (sFlag & EXTN)
					spc1.Main.Finish = spc.Extn.Finish; // full psec
				xres = 0; // no extn to match
			}
			// test extn first (if required)
			if (xres < 0)
				xres = WildCmp(ptn.Extn, spc1.Extn);
		}
		// if extn matched test main part
		if (!xres)
		{
			if (!(pFlag & MAIN_WILDALL))
			{
				if (pFlag & MAIN_HASWILD)
					xres = WildCmp(ptn.Main, spc1.Main);
				else
					xres = FileRCmp(ptn.Main, spc1.Main);
			}
		}
		// equate
		Result = !xres;
		// at next pattern
	}
	while (!Result);
	return Result;
}
