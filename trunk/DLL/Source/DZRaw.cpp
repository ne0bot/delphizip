#include "stdafx.h"
#pragma hdrstop

#include "DZRaw.h"

#include <assert.h>
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_DZRAW_CPP

/* DZRaw.cpp * Copyright (C)  2009 Russell Peters
* Permission is granted to any individual or institution to use, copy, or
* redistribute this software so long as all of the original files are included,
* that it is not sold for profit, and that this copyright notice is retained.
** distributed under LGPL license
** see license.txt for details

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

dzraw_imp* __fastcall DZRawData::NewImp(unsigned siz)
{
    unsigned rawsize = sizeof(dzraw_imp) + siz - (8 * sizeof(char));
    if (rawsize & 63)
        rawsize = (rawsize | 63) + 1;
    if (rawsize > 0xFFF0)
        rawsize = 0xFFF0;

    dzraw_imp* _imp = (dzraw_imp*) (new char[rawsize]);
    if (! _imp)
        throw DZException(DZ_ERM_MEMORY);

    _imp->refs      = 1;
	_imp->capacity  = (WORD)((rawsize - sizeof(dzraw_imp)) + (8 * sizeof(char)));
	_imp->len       = 0;
    return _imp;
}

dzraw_imp* DZRawData::NewImp(const unsigned char* src, int Len, int Space)
{
    if (!src)
        Len = 0;
    if (Space >= 0 && Len > Space)
        Len = Space;

    int siz = (Space >= 0) ? Space : Len;

    if (!siz)
        return NULL;    // empty

    dzraw_imp* nimp = NewImp((unsigned)siz); // make new
    if (src && Len)
    {
		memcpy(nimp->data, src, (size_t)Len);
        nimp->len = (WORD)Len;
    }
    return nimp;
}

void __fastcall DZRawData::Release(void)
{
    if (imp)
    {
        if (!DecRefs())
        {
			/*void*/char *_imp = (char*)imp;
            imp = NULL;
//            free(_imp);
			delete[] _imp;
        }
        imp = NULL;
    }
}

int __fastcall DZRawData::IncRefs(void)
{
    if (imp)
        return InterlockedIncrement(&(imp->refs));
    return 0;
}

int __fastcall DZRawData::DecRefs(void)
{
    if (imp && imp->refs)
        return InterlockedDecrement(&(imp->refs));
    return -1;
}

void __fastcall DZRawData::Append(const unsigned char* src, int Len)
{
    if (!src)
        Len = 0;

    // check something to append
    if (Len)
    {
        if (!imp)
            imp = NewImp(src, Len);
        else
        {
			unsigned nlen = imp->len + (unsigned)Len;
            if (nlen > 0xFFF0)
                return;

            // do we need a new one
            if (imp->refs > 1 || (WORD)nlen > imp->capacity)
            {
                // need new imp - make copy with enough space
				dzraw_imp* nimp = NewImp(imp->data, imp->len, (int)nlen);
                Release();      // out with the old
                imp = nimp;     // in with the new
            }
            // append data
            unsigned char *bf = &imp->data[imp->len];
			memcpy(bf, src, (size_t)Len);
            imp->len = (WORD)nlen;
        }
    }
}

void __fastcall DZRawData::Assign(const unsigned char* src, int Len)
{
    Release();
    imp = NewImp(src, Len);
}

unsigned char * __fastcall DZRawData::GetBuffer(unsigned size)
{
    Release();
    imp = NewImp(size);
	imp->len = (WORD)size;
    return imp->data;
}

void __fastcall DZRawData::SetLength(unsigned Len)
{
    if (!Len)
        Release();
	else
    {
        dzraw_imp* nimp;
        // do we need a new one
        if (!imp || imp->refs > 1 || (WORD)Len > imp->capacity)
        {
            // need new imp - make copy with enough space
            if (!imp)
                nimp = NewImp(Len);
            else
				nimp = NewImp(imp->data, imp->len, (int)Len);
            Release();      // out with the old
            imp = nimp;     // in with the new
        }
        imp->len = (WORD)Len;
    }
}

__fastcall DZRawData::DZRawData(const DZRawData& other)
{
    imp = NULL;
    if (!other.IsEmpty())
    {
        imp = other.imp;
        IncRefs();
    }
}

__fastcall DZRawData::DZRawData(unsigned size)
{
    imp = NULL;
    if (size)
        imp = NewImp(size);
}

__fastcall DZRawData::DZRawData(const unsigned char* str, unsigned len)
{
    imp = NULL;
    if (str && len)
    {
		imp = NewImp(str, (int)len);
    }
}

unsigned __fastcall DZRawData::Capacity(void) const
{
    if (imp)
        return imp->capacity;
    return NULL;
}

unsigned __fastcall DZRawData::Length(void) const
{
    if (imp)
        return imp->len;
    return 0;
}

const unsigned char* DZRawData::begin(void) const
{
    if (imp)
        return imp->data;
    return NULL;
}

const unsigned char* DZRawData::end(void) const
{
    if (imp)
        return imp->data + imp->len;
    return NULL;
}

const unsigned char* DZRawData::Find(WORD tag) const
{
    if (Length() < sizeof(XWord))
        return NULL;
    const unsigned char *p = begin();
    const unsigned char *q;
    const unsigned char *e = end();
    XWord tg, sz;
    while (p + 3 < e)
    {
        q = p;
        tg.b[0] = *p++;
        tg.b[1] = *p++;
        if (tg.w == tag)
            return q;   // found
        sz.b[0] = *p++;
        sz.b[1] = *p++;
        p += sz.w;
    }
    return NULL;
}

DZRawData& __fastcall DZRawData::operator=(const DZRawData& other)
{
    if (this != &other)
    {
        Release();
        if (!other.IsEmpty())
        {
            imp = other.imp;
            IncRefs();
        }
    }
    return *this;
}

DZRawData __fastcall DZRawData::operator+(const DZRawData& other)
{
    DZRawData res(*this);
	res.Append(other.begin(), (int)other.Length());
    return res;
}

DZRawData& __fastcall DZRawData::operator+=(const DZRawData& other)
{
	Append(other.begin(), (int)other.Length());
    return *this;
}

DZRawData& __fastcall DZRawData::operator+=(unsigned char ch)
{
    Append(&ch, 1);
    return *this;
}

DZRawData& __fastcall DZRawData::operator+=(WORD w)
{
    Append((const unsigned char*)&w, sizeof(WORD));
    return *this;
}

//unsigned char __fastcall DZRawData::operator [](unsigned idx) const
//{
//    if (!imp || idx >= Length())
//        return 0;
//    return *(imp + idx);
//}


WORD  __fastcall DZRawData::operator[](unsigned idx) const
{
    unsigned widx = idx * sizeof(WORD);
    if (!imp || widx >= Length() - (sizeof(WORD)-1))
        return 0;
    return ((WORD*)idx)[idx];
}

DZRawData __fastcall DZRawData::operator-(WORD tag)
{
    DZRawData res(Capacity());
    if (imp)
    {
        const unsigned char *p = begin();
        const unsigned char *e = end();
        XWord tg, sz;
        while (p + 3 < e)
        {
            tg.b[0] = *p++;
            tg.b[1] = *p++;
            sz.b[0] = *p++;
            sz.b[1] = *p++;

            if (tg.w != tag)
            {
                res += tg.w;
                res += sz.w;

                while (p < e && sz.w-- > 0)
                    res += *p++;
            }
            else
                p += sz.w;
        }

        while (p < e)
            res += *p++;
    }
    return res;
}


DZRawData& __fastcall DZRawData::operator-=(WORD tag)
{
    if (Find(tag))
    {
        DZRawData tmp = (*this) - tag;
        (*this) = tmp;
    }
    return *this;
}

