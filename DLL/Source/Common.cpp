#include "stdafx.h"
#pragma hdrstop

/*
  Common.cpp - common definitions and functions
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

#include "common.h"
#include "cpyrght.h"
#include "dz_errs.h"
#include <time.h>

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_COMMON_CPP
                                      

int __fastcall Close_Handle(HANDLE *h)
{
    HANDLE ht = *h;

    if (ht != INVALID_HANDLE_VALUE)
    {
        *h = INVALID_HANDLE_VALUE;
        return CloseHandle(ht);
	}
    return 0;
}

__int64 SetFilePointer64(HANDLE hf, __int64 ofs, int from)
{
    typedef union
    {
        __int64 i64;

        struct
        {
            unsigned lo;
            int hi;
        };
    }_I64;

    _I64 o;
    o.i64 = ofs;
	o.lo = SetFilePointer(hf, (long)o.lo, (LONG*) & o.hi, (DWORD)from);

    if (o.lo == INVALID_SET_FILE_POINTER  && GetLastError())
        return -1;

    return o.i64;
}

void Cleanup_Process(void)
{
// nothing to do
}

DWORD __fastcall GetFileAttrs(const TCHAR *p)
{
    WIN32_FIND_DATA fdata;
    HANDLE          fh;
	DWORD           ret = (DWORD)-1; // no_file indicator

    fh = FindFirstFile(p, &fdata);

    if (fh != INVALID_HANDLE_VALUE)
    {
        ret = fdata.dwFileAttributes;
        FindClose(fh);
    }

    return ret;
}

// Return the Unix time_t value (GMT/UTC time) for the DOS format (local)
//   time dostime, where dostime is a four byte value (date in most significant
//   word, time in least significant word), see dostime() function. dostime ::
//   DOS time to convert.
time_t dos2unixtime(ulg dostime)
{
    const time_t  clock = time(NULL);
#ifdef _MSC_VER
    struct tm  t;             // argument for mktime()
    if (localtime_s(&t, &clock) == 0)
	{
    // Convert DOS time to UNIX time_t format
		t.tm_sec = (((int)dostime) << 1) & 0x3E;
		t.tm_min = (((int)dostime) >> 5) & 0x3F;
		t.tm_hour = (((int)dostime) >> 11) & 0x1F;
		t.tm_mday = (int)(dostime >> 16) & 0x1F;
		t.tm_mon = ((int)(dostime >> 21) & 0x0F) - 1;
		t.tm_year = ((int)(dostime >> 25) & 0x7F) + 80;

    return mktime(&t);
	}
	return 0;
#else	
    struct tm     *t;             // argument for mktime()

    t = localtime(&clock);

    // Convert DOS time to UNIX time_t format
    t->tm_sec = (((int)dostime) << 1) & 0x3E;
    t->tm_min = (((int)dostime) >> 5) & 0x3F;
    t->tm_hour = (((int)dostime) >> 11) & 0x1F;
    t->tm_mday = (int)(dostime >> 16) & 0x1F;
    t->tm_mon = ((int)(dostime >> 21) & 0x0F) - 1;
    t->tm_year = ((int)(dostime >> 25) & 0x7F) + 80;
    return mktime(t);
#endif
}

// Convert the date y/n/d and time h:m:s to a four byte DOS date and time
//   (date in high two bytes, time in low two bytes allowing magnitude
//   comparison). y :: Year. n :: Month. d :: Day. h :: Hour. m :: Minute. s ::
//   Second.
ulg dostime(int y, int n, int d, int h, int m, int s)
{
    return y < 1980 ? dostime(1980, 1, 1, 0, 0, 0) : (((ulg) y - 1980) << 25) |
           ((ulg) n << 21) | ((ulg) d << 16) | ((ulg) h << 11) | ((ulg) m << 5) |
           ((ulg) s >> 1);
}

// Return the Unix time t in DOS format, rounded up to the next two second
//   boundary. t :: Unix time to convert.
ulg unix2dostime(time_t *t)
{
    time_t    t_even;
#ifdef _MSC_VER
    struct tm s;//*s;                 // result of localtime()
    t_even = (*t + 1) & (~1); // Round up to even seconds.
  //  s = localtime(&t_even);     // Use local time since MSDOS does.

	if  (localtime_s(&s, &t_even) == 0)     // Use local time since MSDOS does.
    {
        return dostime(s.tm_year + 1900, s.tm_mon + 1, s.tm_mday, s.tm_hour, s.tm_min, s.tm_sec);
    }
#else
    struct tm *s;                 // result of localtime()
    t_even = (*t + 1) & (~1); // Round up to even seconds.
    s = localtime(&t_even);     // Use local time since MSDOS does.

    if (s)
    {
        // Russell Peters s can be null
        return dostime(s->tm_year + 1900, s->tm_mon + 1, s->tm_mday, s->tm_hour, s->tm_min,
                       s->tm_sec);
    }
#endif
    return dostime(1980, 1, 1, 0, 0, 1);
}

char * __fastcall zstrdupB(const char* from)
{
    int len = (int)strlen(from);
    if (!len)
        return NULL;
    char *tmp = new char[len+1];
#ifdef _WIN64
	strcpy_s(tmp, (size_t)len +1, from);
#else	
    strcpy(tmp, from);
#endif	
    tmp[len] = 0;
    return tmp;
}                                      

TCHAR * __fastcall zstrdup(const TCHAR* from)
{
    int len = (int)_tcslen(from);
    if (!len)
        return NULL;
    TCHAR *tmp = new TCHAR[len+1];
#ifdef _WIN64
	wcscpy_s(tmp, (size_t)(len + 1), from);
#else	
    _tcscpy(tmp, from);
#endif	
    tmp[len] = 0;
    return tmp;
}


const unsigned char* __fastcall FindTag(WORD tag, const unsigned char *p, unsigned &siz)
{
#pragma pack(push, 1)
union XTAG
{
	struct
	{
		unsigned short tg;
		unsigned short sz;
	};
	unsigned xtag;
};//xtg;
#pragma pack(pop)

	if (!p || siz < sizeof(XTAG))
		return NULL;
	const unsigned char *e = p + siz - 1;
    while (p + 3 < e)
	{
//		xtg = *((const XTAG*)p)++;
		const XTAG* xtg = (const XTAG*)p;
		p += sizeof(XTAG);
		if (xtg->tg == tag)
		{
			siz = xtg->sz;
			return p;  // beyond the tag
		}
		p += xtg->sz;
    }
    return NULL;
}

