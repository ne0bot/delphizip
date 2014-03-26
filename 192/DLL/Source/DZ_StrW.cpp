#include "stdafx.h"
#pragma hdrstop

#include "DZ_StrW.h"
#include <stdio.h>
#ifdef _WIN64
#include <stdarg.h>
#endif
#include <tchar.h>
//#include "common.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_DZ_STRW_CPP

/* DZ_StrW.cpp
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
#ifndef WC_NO_BEST_FIT_CHARS
#  define WC_NO_BEST_FIT_CHARS 0x00000400
#endif


#ifdef DEBUG_COUNT
int AliveCountW = 0;
#endif

#define RAW_SIZE_MASK 0xff

int DZ_MIN(int arg1, int arg2)
{
	return arg2 < arg1 ? arg2 : arg1;
}

int DZ_MAX(int arg1, int arg2)
{
	return arg2 > arg1 ? arg2 : arg1;
}

#ifdef _WIN64
  #define __fastcall
#endif
unsigned int DZ_UMIN(unsigned int arg1, unsigned int arg2)
{
	return arg2 < arg1 ? arg2 : arg1;
}
unsigned int DZ_UMAX(unsigned int arg1, unsigned int arg2)
{
	return arg2 > arg1 ? arg2 : arg1;
}

// min capacity = 25 chars, grow n * 32
dzstrw_imp* __fastcall DZStrW::NewImp(/*unsigned*/int siz)
{
	if (siz < 0)
		siz = 0;

	unsigned int datasize = (unsigned int)(siz * (int)sizeof(wchar_t));
	unsigned int rawsize = sizeof(dzstrw_imp) + datasize;
	if (rawsize & RAW_SIZE_MASK)//63)
        rawsize = (rawsize | RAW_SIZE_MASK/*63*/) + 1;

	dzstrw_imp* _imp = (dzstrw_imp*) (new char[rawsize]);
	if (! _imp)
		throw DZException(DZ_ERM_MEMORY);

    _imp->refs      = 1;
    _imp->capacity  = (rawsize - sizeof(dzstrw_imp)) / sizeof(wchar_t);
    _imp->len       = 0;
    _imp->data[0]   = 0;
#ifdef DEBUG_COUNT
    AliveCountW++;
#endif
    return _imp;
}

dzstrw_imp* __fastcall DZStrW::NewImp(const wchar_t* src, int maxLen)
{
    int len = 0;
    if (src)
        len = (int)wcslen(src);

    if (maxLen >= 0 && len > maxLen)
        len = maxLen;

    if (!len)
        return NULL;    // empty

    dzstrw_imp* nimp = NewImp(len >= maxLen ? len : maxLen); // make new
#ifdef _WIN64
    wcsncpy_s(nimp->data, nimp->capacity, src, (size_t)len);
#else	
	wcsncpy(nimp->data, src, (size_t)len);
#endif	
    nimp->data[len] = (wchar_t)0;         // mark end - in case
    nimp->len = (unsigned int)wcslen(nimp->data);
    return nimp;
}

dzstrw_imp* __fastcall DZStrW::NewImp(const char* src, int maxLen)
{
    int len = 0;
    if (src)
        len = (int)strlen(src);

    if (maxLen >= 0 && len > maxLen)
        len = maxLen;

    if (!len)
        return NULL;    // empty
    int space = len + (len / 2);    // generous

    dzstrw_imp* nimp = NewImp(space >= maxLen ? space : maxLen); // make new
	int wcnt = MultiByteToWideChar(0, 0, src, len, nimp->data, (int)nimp->capacity);
	nimp->data[wcnt] = (wchar_t)0;
    nimp->len = (unsigned int)wcslen(nimp->data);
    return nimp;
}

unsigned __fastcall DZStrW::_Capacity(dzstrw_imp* _imp) const
{
    if (_imp)
        return _imp->capacity;
    return 0;
}

unsigned __fastcall DZStrW::Capacity(void) const
{
    return _Capacity(_IMP_Ptr(imp));
}

unsigned __fastcall DZStrW::_Length(dzstrw_imp* _imp) const
{
    if (_imp)
        return _imp->len;
    return 0;
}

unsigned __fastcall DZStrW::Length(void) const
{
    return _Length(_IMP_Ptr(imp));
}

long __fastcall DZStrW::_IncImpRefs(dzstrw_imp* _imp)
{
    if (_imp)
        return InterlockedIncrement(& (_imp->refs));
    return 0;
}

long __fastcall DZStrW::_DecImpRefs(dzstrw_imp* _imp)
{
	if (_imp->refs)
        return InterlockedDecrement(& (_imp->refs));
    return 0;
}

void __fastcall DZStrW::_ReleaseImp(dzstrw_imp* _imp)
{
    if (_imp && !_DecImpRefs(_imp))
    {
        delete [] _imp;
#ifdef DEBUG_COUNT
        AliveCountW--;
#endif
    }
}

void __fastcall DZStrW::Release(void)
{
    if (imp)
    {
        dzstrw_imp* _imp = _IMP_Ptr(imp);
        imp = NULL;
        _ReleaseImp(_imp);
    }
}

int __fastcall DZStrW::IncRefs(void)
{
    if (imp)
        return (int)_IncImpRefs(_IMP_Ptr(imp));
    return 0;
}

int __fastcall DZStrW::DecRefs(void)
{
    if (imp)
        return (int)_DecImpRefs(_IMP_Ptr(imp));
    return -1;
}

// maxSpace is max characters
void __fastcall DZStrW::_Assign(const wchar_t* src, int maxLen)
{
    Release();
    imp = _IMP_Data(NewImp(src, maxLen));
}

void __fastcall DZStrW::_Assign(const char* src, int maxLen)
{
    Release();
    imp = _IMP_Data(NewImp(src, maxLen));
}

void __fastcall DZStrW::_Append(const wchar_t* src, int maxLen)
{
    dzstrw_imp* _imp = _IMP_Ptr(imp);

    unsigned len = 0;
    if (src)
        len = (unsigned int)wcslen((const wchar_t*)src);
    if (maxLen >= 0 && len > (unsigned)maxLen)
        len = (unsigned)maxLen;
    // check something to append
    if (len)
    {
        if (!imp)
            imp = _IMP_Data(NewImp(src, (int)len));
        else
        {
            unsigned oldlen = _imp->len;
            unsigned nlen = oldlen + len;

            // do we need a new one
            if (_imp->refs > 1 || nlen >= _imp->capacity)
            {
                // need new imp - make copy with enough space
                dzstrw_imp* nimp = NewImp(_imp->data, (int)nlen);
                Release();              // out with the old
                imp = _IMP_Data(nimp);  // in with the new
                _imp = nimp;
            }
            // append data
            wchar_t *bf = &_imp->data[_imp->len];
#ifdef _WIN64			
			wcsncpy_s(bf, _imp->capacity - _imp->len, src, len);
#else			
			wcsncpy(bf, src, len);
#endif	
            _imp->data[nlen] = (wchar_t)0;
            _imp->len = (unsigned)wcslen(_imp->data);
        }
    }
}

void __fastcall DZStrW::_Append(const char* src, int maxSpace)
{
    DZStrW Asrc(src, maxSpace);
    _Append(Asrc.imp, maxSpace);
}

void __fastcall DZStrW::_Append(wchar_t ch)
{
    wchar_t chr[2];
    chr[0] = ch;
    chr[1] = 0;
    _Append(chr, 1);
}

__fastcall DZStrW::DZStrW(const DZStrW& other)
{
    if (other.IsEmpty())
        imp = NULL;
    else
    {
        imp = other.imp;
        IncRefs();
    }
}

__fastcall DZStrW::DZStrW(UINT cp, const DZStrA& other)
{
	DZStrW(cp, other.c_str(), (int)other.Length());
}

__fastcall DZStrW::DZStrW(const DZStrA& other)
{
	if (other.IsEmpty())
		imp = NULL;
	else
        imp = _IMP_Data(NewImp(other.c_str(), (int)other.Length()));
}

__fastcall DZStrW::DZStrW(const wchar_t* str, int len)
{
	imp = _IMP_Data(NewImp(str, len));
}

__fastcall DZStrW::DZStrW(const char* str, int len)
{
    imp = _IMP_Data(NewImp(str, len));
}

__fastcall DZStrW::DZStrW(UINT cp, const char* str, int len)
{
	imp = NULL;
	if (str && *str && len && len < (32 * 1024))
	{
		if (len < 0)
			len = (int)strlen(str);
		if (cp == CP_UTF8)
		{
			// convert to Unicode from UTF8
			int wcnt = UTF8_To_Wide((const UChar*)str, len, NULL, 0);
			if (wcnt > 0)
			{
				dzstrw_imp* nimp = NewImp(wcnt);
				imp = _IMP_Data(nimp);
				wcnt = UTF8_To_Wide((const UChar*)str, len, imp, wcnt);
				imp[wcnt] = 0;
				nimp->len = (unsigned)wcnt;
			}
		}
		else
		{
			// convert to Unicode
			int wcnt = MultiByteToWideChar(cp, 0, str, len, NULL, 0);
			if (wcnt > 0)
			{
				dzstrw_imp* nimp = NewImp(wcnt);
				imp = _IMP_Data(nimp);
				wcnt = MultiByteToWideChar(cp, 0, str, len, imp, wcnt);
				imp[wcnt] = 0;
				nimp->len = (unsigned)wcnt;
			}
		}
	}
}

__fastcall DZStrW::DZStrW(wchar_t ch, unsigned cnt)
{
    if (cnt) {
        dzstrw_imp* nimp = NewImp((int)cnt);
        imp = _IMP_Data(nimp);
        wchar_t* bf = imp;
        for (unsigned i = cnt; i < cnt; i++)
            *bf++ = ch;
        *bf = 0;
        nimp->len = cnt;
    }
    else
        imp = NULL;
}

DZStrW& __fastcall DZStrW::operator=(const DZStrW& other)
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

DZStrW& __fastcall DZStrW::operator=(const wchar_t* str)
{
    Release();
    imp = _IMP_Data(NewImp(str, -1));
    return *this;
}

DZStrW& __fastcall DZStrW::operator=(const char* str)
{
    Release();
    imp = _IMP_Data(NewImp(str, -1));
    return *this;
}

DZStrW __fastcall DZStrW::operator+(const DZStrW& other)const
{
    DZStrW res(*this);
    res._Append(other.c_str(), -1);
    return res;
}

DZStrW __fastcall DZStrW::operator+(const wchar_t* str)const
{
	DZStrW res(*this);
	res._Append(str, -1);
	return res;
}

DZStrW __fastcall DZStrW::operator+(const wchar_t wch)const
{
	DZStrW res(*this);
	wchar_t buf[2];
	buf[0] = wch;
	buf[1] = 0;
	res._Append(buf, 1);
	return res;
}

DZStrW& __fastcall DZStrW::operator+=(const DZStrW& other)
{
    _Append(other.c_str(), -1);
    return *this;
}

DZStrW& __fastcall DZStrW::operator+=(const wchar_t* str)
{
    _Append(str, -1);
    return *this;
}


__fastcall DZStrW::~DZStrW(void)
{
    Release();
}

DZStrW& __fastcall DZStrW::operator+=(wchar_t ch)
{
    wchar_t buf[2];
    buf[0] = ch;
    buf[1] = 0;
    _Append(buf, 1);
    return *this;
}

wchar_t __fastcall DZStrW::operator[](unsigned idx) const
{
    if (!imp || idx >= Length())
        return 0;
    return *(imp + idx);
}

wchar_t __fastcall DZStrW::operator[](int idx) const
{
    if (!imp || (unsigned int)idx >= Length())
        return 0;
    return *(imp + idx);
}

wchar_t* __fastcall DZStrW::GetBuffer(int minsize)
{
    dzstrw_imp* _imp = _IMP_Ptr(imp);

    int nlen = (int)(_imp ? _imp->len : 0);
    if (minsize >= 0)
        nlen = minsize;
    // need new imp - make copy with enough space
    dzstrw_imp* nimp;
    if (!_imp)
        nimp = NewImp(nlen);
    else
    {
        nimp = NewImp(_imp->data, nlen);
        Release();              // out with the old
    }
	imp = _IMP_Data(nimp);  // in with the new
    return imp;
}

void __fastcall DZStrW::ReleaseBuffer(int newlen)
{
    if (imp)
    {
        dzstrw_imp* _imp = _IMP_Ptr(imp);
        _imp->data[_imp->capacity] = 0; //ensure end marked
        int len = (int)wcslen(imp);
        if (newlen >=0 && newlen < len) {
            len = newlen;
            _imp->data[len] = 0; // truncate
        }
        _imp->len = (unsigned)len;
    }
}

dzstrw_imp* DZStrW::Unique(void)
{
    dzstrw_imp* _imp = _IMP_Ptr(imp);
    if (imp && _imp->refs > 1)
    {
        dzstrw_imp* nimp = NewImp(imp, (int)Capacity());
        Release();
        imp = _IMP_Data(nimp);
        return nimp;
    }
    return _imp;
}

void __cdecl  DZStrW::AppendFormat(const wchar_t *fmt, ...)
{
    if (fmt)
    {
        DZStrW tmp;
        va_list argList;
        va_start(argList, fmt);
        tmp.FormatV(fmt, argList);
        va_end(argList);
        if (!tmp.IsEmpty())
            _Append(tmp.c_str());
    }
}

static int WINAPI WideCharToMultiByteOptionalFlags(UINT CodePage, DWORD dwFlags,
	LPCWSTR lpWideCharStr, int cchWideChar, LPSTR lpMultiByteStr,
	int cbMultiByte, LPCSTR lpDefaultChar, LPBOOL lpUsedDefaultChar)
{
	int ret = WideCharToMultiByte(CodePage, dwFlags, lpWideCharStr, cchWideChar,
		lpMultiByteStr, cbMultiByte, lpDefaultChar, lpUsedDefaultChar);

	// For older OS compatibility:
	// Drop the flags if at least one of them is unknown to the OS
	if (GetLastError() == ERROR_INVALID_FLAGS /* 1004 */)
	{
		ret = WideCharToMultiByte(CodePage, 0, lpWideCharStr, cchWideChar,
			lpMultiByteStr, cbMultiByte, lpDefaultChar, lpUsedDefaultChar);
	}

	return ret;
}

bool __fastcall DZStrW::BadDOS(void) const
{
	if (IsEmpty())
		return false;

//    int Bad;
//	WideCharToMultiByteOptionalFlags(CP_ACP, 0, c_str(), Length(), NULL, 0, 0, &Bad);
//	return Bad;
	wchar_t tmp;
	for (const wchar_t* p = imp; (tmp = *p) != 0; p++)
		if (tmp < 32 || tmp > 126)
			return true;
	return false;
}

int __fastcall DZStrW::Compare(const wchar_t *other) const
{
    if (!imp)
        return (other && *other)? -1 : 0;
    if (!other || !*other)
        return -1;
	return wcscmp(imp, other);
}

int __fastcall DZStrW::CompareNoCase(const wchar_t *other) const
{
    if (!imp)
        return (other && *other)? -1 : 0;
    if (!other || !*other)
        return -1;

//	DZStrW me(*this);
	DZStrW me = AsLower();
//	me.ToUpper();
	DZStrW theOther(other);
//    theOther.ToUpper();
	theOther.ToLower();
	return wcscmp(me.imp, theOther.imp);
}

int __fastcall DZStrW::Delete(unsigned pos, unsigned cnt)
{
    if (!imp || pos >= Length())
        return 0;
    dzstrw_imp* _imp = Unique();
    wchar_t* p = _imp->data + pos;
    if (pos + cnt < _imp->len)
    {
        for (wchar_t* q = p + cnt; *q; q++)
            *p++ = *q;
    }
    *p = 0;
    _imp->len = (unsigned)wcslen(_imp->data);
    return (int)_imp->len;
}

int __fastcall DZStrW::Find(const wchar_t *sub, unsigned start) const
{
    if (!imp || start > Length() || !sub || !*sub)
        return -1;
    unsigned int slen = (unsigned int)wcslen(sub);
    if (start + slen > Length())
        return -1;
    const wchar_t* si = wcsstr(imp + start, sub);
    if (!si)
        return -1;
    return (int)(si - imp);
}

int __fastcall DZStrW::Find(wchar_t ch, unsigned start) const
{
    if (!imp || start > Length())
        return -1;

    unsigned int i = start;
    for (const wchar_t* p = &imp[start]; *p && *p != ch; p++)
        i++;
    return (unsigned)i >= Length() ? -1 : (int)i;
}

int __fastcall DZStrW::FindOneOf(const wchar_t *chars, unsigned start) const
{
    if (!imp || start > Length())
        return -1;

    unsigned int i = start;
    for (const wchar_t* p = &imp[start]; *p; p++)
    {
        bool found = false;
        wchar_t ch = *p;
        for (const wchar_t* q = chars; *q; q++)
        {
            if (*q == ch)
            {
                found = true;
                break;
            }
        }
        if (found)
            break;
        i++;
    }
    return (unsigned)i >= Length() ? -1 : (int)i;
}

int __cdecl  DZStrW::Format(const wchar_t *fmt, ...)
{
    Release();
    int r = 0;
    if (fmt)
    {
        va_list argList;
        va_start(argList, fmt);
        r = FormatV(fmt, argList);
        va_end(argList);
    }
    return r;
}

#define MAXDIGITS  64

//static const wchar_t digits[] = L"0123456789abcdefghijklmnopqrstuvwxyz";
static const wchar_t zeros[] = L"00000000000000000000000000000000";
static const wchar_t spaces[] = L"                                ";

static
int convert(unsigned __int64 value, int radix, wchar_t *buffer, bool upper)
{
    wchar_t buf[MAXDIGITS + 2];
	int i;
	wchar_t aA = upper ? L'A' : L'a';

    i = 0;

    if (radix == -10) {
        if ((int)value < 0) {
            buffer[i++] = '-';
            value = 0-value;
        }
        radix = 10;
    }

	if (radix != 10 && radix != 16 && radix != 8 && radix != 2 && radix != 36)
		radix = 16;

    wchar_t* p = &buf[MAXDIGITS + 1];
    *p = 0;
    while (value)
	{
		//int cv = (int)(((unsigned int)value) & 0x7fff) % radix;
		int cv = (int)(value % radix);
		value = value / (unsigned long long)radix;

		wchar_t c;// = digits[DivMod64(value, radix) & 15];
		if (cv <= 9)
			c = (wchar_t)(L'0' + cv);
		else
			c = (wchar_t)(aA + (cv - 10));
        *--p = c;

		if (++i > MAXDIGITS)
            break;
    }
    if (!i)
    {
        *--p = L'0';
        i++;
    }
#ifdef _WIN64
	wcsncpy_s(buffer, 120, p, (size_t)i);
#else	
    wcsncpy(buffer, p, (size_t)i);
#endif	
	buffer[i] = 0;
    return i;
}

#define FORMAT_BUFFER_SIZE 2048
int __cdecl DZStrW::FormatV(const wchar_t *fmt, va_list argList)
{
    int i;
    int j;
    __int64 n = 0;
    //int arg;
    wchar_t c;
    int zero;
    bool right;
    wchar_t flag;
    int width;
    int prec;
    const wchar_t *p;
    const char *CPtr;
    const wchar_t *WPtr;
    wchar_t temp[MAXDIGITS+1+55];

    Release();
    if (!fmt)
        return 0;
    dzstrw_imp* _imp = NewImp(FORMAT_BUFFER_SIZE + 1);
    imp = _IMP_Data(_imp);

    i = 0;
    for (p = fmt; *p; p++)
    {
		c = *p;
        if (c != L'%')
        {
            _Append(c);
            i++;
            continue;
		}
		zero = 0;
		CPtr = NULL;
		WPtr = NULL;
		width = 0;
		right = false;
		flag = 0;
		c = *++p;
		if (c == L'%')
		{
			_Append(c);
			i++;
			continue;
		}

		if (c == L'-')
		{
			right = true;
			c = *++p;
		}

		if (c == L'0')
		{
			zero = 1;
			c = *++p;
		}
		if (iswdigit(c))
		{
			width = c - L'0';
			c = *++p;
			if (iswdigit(c))
			{
				width = (width*10) + (c - L'0');
				c = *++p;
			}
		}

		prec = 0x7fffffff;
		if (c == L'.')
		{
			prec = 0;
			c = *++p;
			while (iswdigit(c))
			{
				prec = (prec*10) + (c - L'0');
				c = *++p;
			}
		}

		if (c == L'h' || c == L'l' || c == L'L')
		{
			flag = c;
			c = *++p;
		}

		if (flag == L'L' && (c == L'd' || c == L'u' || c == L'x' || c == L'X'))
		{
			n = va_arg(argList, __int64);
		}
		else
		if (c == 's')
		{
			if (flag == L'h')
				CPtr = va_arg(argList, const char*);
			else
				WPtr = va_arg(argList, const wchar_t*);
		}
		else
		{
			n = va_arg(argList, int);
		}
		bool upperXZ = false;
		switch (c)
		{
			case L'b':
				j = convert((unsigned __int64)n, 2, temp, false);
				break;
			case L'c':
				temp[0] = (wchar_t)n;
				temp[1] = L'\0';
				j = 1;
				break;
			case L'i':
			case L'd':
				j = convert((unsigned __int64)n, -10, temp, false);
				break;
			case L'u':
				j = convert((unsigned __int64)n, 10, temp, false);
				break;
			case L'o':
				j = convert((unsigned __int64)n, 8, temp, false);
				break;
			case L's':
				j = 0;
				if (flag == L'h')
				{
					if (CPtr)
						j = DZ_MIN((int)strlen(CPtr), prec);
					}
					else
					{
						if (WPtr)
							j = DZ_MIN((int)wcslen(WPtr), prec);
					}
				break;
			case L'X':
				upperXZ = true;
			case L'x':
				j = convert((unsigned __int64)n, 16, temp, upperXZ);
				break;
			case L'Z':
				upperXZ = true;
			case L'z':
				j = convert((unsigned __int64)n, 36, temp, upperXZ);
				break;
			default:
//OutputDebugString(L"FormatV - error");
#ifdef _WIN64
				wcscpy_s(temp, MAXDIGITS + 55, L"<<error>>");
#else				
				wcscpy(temp, L"<<error>>");
#endif				
				j = (int)wcslen(temp);
		}

		if (!right && j < width)
		{
			if (zero)
			{
				_Append(zeros, width-j);
			}
			else
			{
				_Append(spaces, width-j);
			}
			i += width-j;
		}

		if (c == L's')
		{
			if ((flag == L'h') && (CPtr))
				_Append(CPtr, j);
			if ((flag != L'h') && (WPtr))
				_Append(WPtr, j);
		}
		else
		{
			_Append(temp, j);
		}
		if (right)
			_Append(spaces, width-j);
		i += j;
    }
    return i;
}

int __fastcall DZStrW::Insert(unsigned pos, wchar_t ch)
{
    if (!imp)
        imp = _IMP_Data(NewImp(1));

    pos = DZ_UMIN(pos, Length());  // limit to appending

    wchar_t* bf = GetBuffer((int)Length() + 1);
    wchar_t* stop = bf + pos;
    wchar_t* p = bf + Length() + 2;
    while (--p > stop)
        *p = *(p - 1);
    *p = ch;
    dzstrw_imp* _imp = _IMP_Ptr(imp);
    _imp->len = (unsigned)wcslen(_imp->data);
    return (int)_imp->len;
}

wchar_t DZStrW::LastChar(void) const
{
    if (!imp)
        return 0;
    return imp[Length()-1];
}

DZStrW __fastcall DZStrW::Left(unsigned len) const
{
    if (!imp)
        return DZStrW();
    return DZStrW(imp, DZ_MIN((int)len, (int)Length()));
}

DZStrW __fastcall DZStrW::Mid(unsigned pos, unsigned len) const
{
    if (!imp || pos > Length())
        return DZStrW();
    return DZStrW(imp + pos, DZ_MIN((int)len, (int)Length() - pos));
}


int __fastcall DZStrW::ReverseFind(wchar_t ch, unsigned int pos) const
{
    if (!imp)
        return -1;
    int i = (int)Length();
    if (pos < (unsigned)i)
        i = (int)pos;
    const wchar_t* p = &imp[i];
    while (i >= 0 && *p != ch)
    {
        i--;
        p--;
    }
    return i;
}

DZStrA __fastcall DZStrW::SafeAnsi(void) const
{
	int tmp;
	return SafeAnsi(tmp);
}

DZStrA __fastcall DZStrW::SafeAnsi(int &bad) const
{
	DZStrA tmp;
    if (IsEmpty())
        return tmp;

	bad = 0;
	int subst = 0x1b; // substitute char - escape
	int cnt = WideCharToMultiByteOptionalFlags(CP_ACP, WC_NO_BEST_FIT_CHARS, c_str(), (int)Length(), NULL, 0,
	(char*)&subst, &bad);
    if (cnt > 0)
    {
        cnt = WideCharToMultiByteOptionalFlags(CP_ACP, WC_NO_BEST_FIT_CHARS, c_str(), (int)Length(),
			tmp.GetBuffer(cnt), cnt, (char*)&subst, &bad);
        tmp.ReleaseBuffer(cnt);
	}
	if (!bad || !tmp)
        return tmp;

    DZStrA ret;
    const char *pa = tmp.c_str();
    const char *good = 0;
    cnt = 0;
    int i = 0;
    wchar_t wc = 0;
	char c;
    int wlen = (int)Length();
    while ((c= *pa) != 0)
    {
        if (i < wlen)
            wc = (*this)[(unsigned)(i++)];
		if (((unsigned char)c) < 0x20)
        {
            if (cnt)
            {
                ret._Append(good, cnt);
                cnt = 0;
            }
            ret += "#$";
			ret += DZStrA(wc, 4);
        }
        else
        {
            if (!cnt)
                good = pa;
            cnt++;
		}
        pa = CharNextA(pa);
    }
    if (cnt)
    {
		ret._Append(good, cnt);
    }
    return ret;
}

DZStrA __fastcall DZStrW::SafeNarrow(unsigned cp) const
{
	int tmp;
	return SafeNarrow(cp, tmp);
}

DZStrA __fastcall DZStrW::SafeNarrow(unsigned cp, int &bad) const
{
	DZStrA tmp;
    if (IsEmpty())
        return tmp;

	bad = 0;
	int subst = 0x1b; // substitute char - escape
	int cnt = WideCharToMultiByteOptionalFlags(cp, WC_NO_BEST_FIT_CHARS,
			c_str(), (int)Length(), NULL, 0,	(char*)&subst, &bad);
	if (cnt > 0)
	{
		cnt = WideCharToMultiByteOptionalFlags(cp, WC_NO_BEST_FIT_CHARS,
			c_str(), (int)Length(), tmp.GetBuffer(cnt), cnt, (char*)&subst, &bad);
        tmp.ReleaseBuffer(cnt);
	}
	if (!bad || !tmp)
        return tmp;

    DZStrA ret;
    const char *pa = tmp.c_str();
    const char *good = 0;
    cnt = 0;
    int i = 0;
    wchar_t wc = 0;
	char c;
    int wlen = (int)Length();
    while ((c= *pa) != 0)
    {
        if (i < wlen)
            wc = (*this)[(unsigned)(i++)];
		if (((unsigned char)c) < 0x20)
        {
            if (cnt)
            {
                ret._Append(good, cnt);
                cnt = 0;
            }
            ret += "#$";
			ret += DZStrA(wc, 4);
        }
        else
        {
            if (!cnt)
                good = pa;
            cnt++;
		}
        pa = CharNextA(pa);
    }
    if (cnt)
    {
		ret._Append(good, cnt);
    }
	return ret;
}

DZStrW __fastcall DZStrW::AsLower(void)const
{
	DZStrW ret;
	if (imp)
	{
		register wchar_t *bf = ret.GetBuffer((int)Length());
		register const wchar_t *src = c_str();
		for (;*src; src++, bf++)
			*bf = (wchar_t)Unicode_To_Lower((UCS4)*src);
		*bf = 0;
		ret.ReleaseBuffer();
	}
	return ret;
}

DZStrW& __fastcall DZStrW::ToLower(void)
{
	if (imp)
	{
		wchar_t *bf = GetBuffer();
		Lower_BuffW(bf, (int)Length());
		ReleaseBuffer();
	}
	return *this;
}

DZStrW& __fastcall DZStrW::ToUpper(void)
{
    if (imp)
    {
		wchar_t * bf = GetBuffer();
		CharUpperBuffW(bf, Length());
        ReleaseBuffer();
    }
    return *this;
}

DZStrW& __fastcall DZStrW::Trim(void)
{
    return TrimLeft().TrimRight();
}

DZStrW& __fastcall DZStrW::TrimLeft(void)
{
    return TrimLeft(L' ');
}

DZStrW& __fastcall DZStrW::TrimLeft(wchar_t ch)
{
    if (imp && ch && *imp == ch)
    {
        // something to trim
        wchar_t* bf = GetBuffer(); // will make unique
        const wchar_t* p = bf;
        while (*p == ch)
            p++;
        if (!*p)
        {
            // we empty it
            Release();
        }
        else
        {
            // move rest
            wchar_t tc;
            do
            {
                tc = *bf++ = *p++;
            }
            while (tc);
            // get new length
            dzstrw_imp* _imp = _IMP_Ptr(imp);
            _imp->len = (unsigned)wcslen(_imp->data);
        }
    }
    return *this;
}

DZStrW& __fastcall DZStrW::TrimRight(void)
{
    if (imp && imp[Length()-1] == ' ')
    {
        // something to trim
        wchar_t* bf = GetBuffer(); // will make unique
        wchar_t* p = bf + Length() - 1;
        while (p >= bf && *p == ' ')
            --p;
        // truncate it
        *++p = 0;
        if (p == bf)
        {
            // we empty it
            Release();
        }
        else
        {
            // set new length
            dzstrw_imp* _imp = _IMP_Ptr(imp);
            _imp->len = (unsigned)wcslen(_imp->data);
        }
    }
    return *this;
}

#ifdef DEBUG_COUNT
int AliveCountA = 0;
#endif

// min capacity = 25 chars, grow n * 32
dzstra_imp* __fastcall DZStrA::NewImp(unsigned siz)
{
    unsigned datasize = (1 + siz) * sizeof(char);
    unsigned rawsize = sizeof(dzstra_imp) + datasize;
    if (rawsize & RAW_SIZE_MASK)
        rawsize = (rawsize | RAW_SIZE_MASK) + 1;

    dzstra_imp* _imp = (dzstra_imp*) (new char[rawsize]);
    if (! _imp)
        throw DZException(DZ_ERM_MEMORY);

    _imp->refs      = 1;
    _imp->capacity  = (rawsize - sizeof(dzstra_imp)) / sizeof(char);
    _imp->len       = 0;
    _imp->data[0]   = 0;
#ifdef DEBUG_COUNT
    AliveCountA++;
#endif
    return _imp;
}

dzstra_imp* __fastcall DZStrA::NewImp(const char* src, int maxLen)
{
    int len = 0;
    if (src)
        len = (int)strlen(src);

    if (maxLen >= 0 && len > maxLen)
        len = maxLen;

    if (!len)
        return NULL;    // empty

    dzstra_imp* nimp = NewImp((unsigned)(len >= maxLen ? len : maxLen)); // make new
#ifdef _WIN64
	strncpy_s(nimp->data, nimp->capacity, src, (size_t)len);
#else	
	strncpy(nimp->data, src, (size_t)len);
#endif
    nimp->data[len] = 0;         // mark end - in case
    nimp->len = (unsigned int)strlen(nimp->data);
    return nimp;
}

unsigned __fastcall DZStrA::_Capacity(dzstra_imp* _imp) const
{
    if (_imp)
        return _imp->capacity;
    return 0;
}

unsigned __fastcall DZStrA::Capacity(void) const
{
    return _Capacity(_IMP_Ptr(imp));
}

unsigned __fastcall DZStrA::_Length(dzstra_imp* _imp) const
{
    if (_imp)
        return _imp->len;
    return 0;
}

unsigned __fastcall DZStrA::Length(void) const
{
    return _Length(_IMP_Ptr(imp));
}

int __fastcall DZStrA::_IncImpRefs(dzstra_imp* _imp)
{
    if (_imp)
        return InterlockedIncrement(& (_imp->refs));
    return 0;
}

int __fastcall DZStrA::_DecImpRefs(dzstra_imp* _imp)
{
    if (_imp && _imp->refs)
        return InterlockedDecrement(& (_imp->refs));
    return 0;
}

void __fastcall DZStrA::_ReleaseImp(dzstra_imp* _imp)
{
    if (!_DecImpRefs(_imp))
	{
        delete[] _imp;
#ifdef DEBUG_COUNT
    AliveCountA--;
#endif
    }
}

void __fastcall DZStrA::Release(void)
{
    if (imp)
    {
        dzstra_imp* _imp = _IMP_Ptr(imp);
        imp = NULL;
        _ReleaseImp(_imp);
    }
}

int __fastcall DZStrA::IncRefs(void)
{
    if (imp)
        return (int)_IncImpRefs(_IMP_Ptr(imp));
    return 0;
}

int __fastcall DZStrA::DecRefs(void)
{
    if (imp)
        return (int)_DecImpRefs(_IMP_Ptr(imp));
    return -1;
}

// maxSpace is max characters
void __fastcall DZStrA::_Assign(const char* src, int maxLen)
{
    Release();
    imp = _IMP_Data(NewImp(src, maxLen));
}

void __fastcall DZStrA::_Append(const char* src, int maxLen)
{
    dzstra_imp* _imp = _IMP_Ptr(imp);

    unsigned len = 0;
    if (src)
        len = (unsigned int)strlen(src);
    if (maxLen >= 0 && len > (unsigned)maxLen)
        len = (unsigned)maxLen;
    // check something to append
    if (len)
    {
        if (!imp)
            imp = _IMP_Data(NewImp(src, (int)len));
        else
        {
            unsigned oldlen = _imp->len;
            unsigned nlen = oldlen + len;

            // do we need a new one
            if (_imp->refs > 1 || nlen >= _imp->capacity)
            {
                // need new imp - make copy with enough space
                dzstra_imp* nimp = NewImp(_imp->data, (int)nlen);
                Release();              // out with the old
                imp = _IMP_Data(nimp);  // in with the new
                _imp = nimp;
            }
            // append data
            char *bf = &_imp->data[_imp->len];
#ifdef _WIN64
            strncpy_s(bf, _imp->capacity - _imp->len, src, len);
#else			
            strncpy(bf, src, len);
#endif		
            bf[len] = 0;                // mark end
            _imp->len = (unsigned int)strlen(_imp->data);
        }
    }
}

__fastcall DZStrA::DZStrA(const DZStrA& other)
{
    imp = other.imp;
    IncRefs();
}

__fastcall DZStrA::DZStrA(const DZStrW& other, UINT cp)
{
    imp = 0;
    int len;// = other.Length();
//    if (other && ((len = other.length()) >0))
	if (/*other &&*/ ((len = (int)other.length()) >0))
	{
		if (cp != CP_UTF8)
		{
			int alen = len * 3;// + (len / 2); // just in case
			// make new imp with enough space
			dzstra_imp* _imp = NewImp((unsigned)alen);
			imp = _IMP_Data(_imp);  // new
			int cnt = WideCharToMultiByteOptionalFlags(cp, WC_NO_BEST_FIT_CHARS,
						other.c_str(),
				len, imp, alen, NULL, NULL);
			_imp->data[cnt] = 0;
			_imp->len = (unsigned int)strlen(_imp->data);
		}
		else
		{
			int alen = Wide_To_UTF8(other.c_str(), len, 0, 0);
			// make new imp with enough space
			dzstra_imp* _imp = NewImp((unsigned)alen);
			imp = _IMP_Data(_imp);  // new
			int cnt = Wide_To_UTF8(other.c_str(), len, (UChar*)imp, alen);
			_imp->data[cnt] = 0;
			_imp->len = (unsigned int)strlen(_imp->data);
        }
    }
}

__fastcall DZStrA::DZStrA(const char* str, int len)
{
    imp = _IMP_Data(NewImp(str, len));
}

__fastcall DZStrA::DZStrA(const wchar_t* wstr)
{
	int len;
	imp = 0;
    if (wstr && ((len = (int)wcslen(wstr)) > 0))
    {
        int alen = len + (len / 2); // just in case
        // make new imp with enough space
        dzstra_imp* nimp = NewImp((unsigned)alen);
        imp = _IMP_Data(nimp);  // new
		int cnt = WideCharToMultiByteOptionalFlags(0, WC_NO_BEST_FIT_CHARS, wstr, len, imp,
			alen, NULL, NULL);
        nimp->data[cnt] = 0;
        nimp->len = (unsigned int)strlen(nimp->data);
    }
}
#ifdef _WIN64
const char hx[17] = "0123456789ABCDEF";
#else
const char hx[16] = "0123456789ABCDEF";
#endif
__fastcall DZStrA::DZStrA(unsigned val, unsigned cnt)
{
    imp = _IMP_Data(NewImp(cnt));

    char* bp = imp + cnt;
    int x = (int)cnt;
    *bp = 0;
    while (val && x-- > 0)
    {
        *--bp = hx[val & 0x0f];
        val >>= 4;
    }
    while (bp > imp)
        *--bp = '0';
}

DZStrA& __fastcall DZStrA::operator=(const DZStrA& other)
{
    if (this != &other)
    {
        Release();
        imp = other.imp;
        IncRefs();
    }
    return *this;
}

DZStrA& __fastcall DZStrA::operator=(const char* str)
{
    Release();
    imp = _IMP_Data(NewImp(str, -1));
    return *this;
}

DZStrA __fastcall DZStrA::operator+(const DZStrA& other)const
{
    DZStrA res(*this);
    res._Append(other.c_str(), -1);
    return res;
}

DZStrA __fastcall DZStrA::operator+(const char* str)const
{
    DZStrA res(*this);
    res._Append(str, -1);
    return res;
}

DZStrA& __fastcall DZStrA::operator+=(const DZStrA& other)
{
    _Append(other.c_str(), -1);
    return *this;
}

DZStrA& __fastcall DZStrA::operator+=(const char* str)
{
    _Append(str, -1);
    return *this;
}

DZStrA& __fastcall DZStrA::operator+=(char ch)
{
    char buf[2];
    buf[0] = ch;
    buf[1] = 0;
    _Append(buf, 1);
    return *this;
}

char __fastcall DZStrA::operator[](unsigned idx) const
{
    if (!imp || idx >= Length())
        return 0;
    return *(imp + idx);
}

__fastcall DZStrA::~DZStrA(void)
{
    Release();
}

char * __fastcall DZStrA::GetBuffer(int minsize)
{
    dzstra_imp* _imp = _IMP_Ptr(imp);

    int nlen = _imp ? (int)_imp->len : 0;
    if (minsize >= 0)
        nlen = minsize;
    // need new imp - make copy with enough space
    dzstra_imp* nimp;
    if (!_imp)
        nimp = NewImp((unsigned)nlen);
    else
    {
        nimp = NewImp(_imp->data, nlen);
        Release();              // out with the old
    }
    imp = _IMP_Data(nimp);  // in with the new
    return imp;
}

void __fastcall DZStrA::ReleaseBuffer(int newlen)
{
    if (imp)
    {
        dzstra_imp* _imp = _IMP_Ptr(imp);
        _imp->data[_imp->capacity] = 0; //ensure end marked
        int len = (int)strlen(imp);
        if (newlen >=0 && newlen < len) {
            len = newlen;
            _imp->data[len] = 0; // truncate
        }
        _imp->len = (unsigned)len;
    }
}

bool __fastcall DZStrA::BadDOS(void) const
{
	if (IsEmpty())
		return false;

	char tmp;
	for (const char* p = imp; (tmp = *p) != 0; p++)
		if (tmp < 32 || tmp > 126)
			return true;
	return false;
}

int __fastcall DZStrA::Compare(const char* other) const
{
    if (!imp)
        return (other && *other)? -1 : 0;
    if (!other || !*other)
        return -1;

    return strcmp(imp, other);
}



int __fastcall DZStrA::CompareNoCase(const char* other) const
{
    if (!imp)
        return (other && *other)? -1 : 0;
    if (!other || !*other)
        return -1;

    DZStrA me(*this);
//    me.ToUpper();
	me.ToLower();
	DZStrA theOther(other);
//    theOther.ToUpper();
	theOther.ToLower();
    return strcmp(me.imp, theOther.imp);
}


int __fastcall DZStrA::Find(char ch) const
{
    if (!imp)
        return -1;

    int i = 0;
    for (const char* p = imp; *p && *p != ch; p++)
        i++;
    return (unsigned)i >= Length() ? -1 : i;
}

int __fastcall DZStrA::FindOneOf(const char *chars, unsigned start) const
{
    if (!imp || start > Length())
        return -1;

    int i = (int)start;
    for (const char* p = &imp[start]; *p; p++)
    {
        bool found = false;
        char ch = *p;
        for (const char* q = chars; *q; q++)
        {
            if (*q == ch)
            {
                found = true;
                break;
            }
        }
        if (found)
            break;
        i++;
    }
    return (unsigned)i >= Length() ? -1 : i;
}


DZStrA __fastcall DZStrA::Left(unsigned len) const
{
    if (!imp)
        return DZStrA();
    return DZStrA(imp, DZ_MIN((int)len, (int)Length()));
}

DZStrA __fastcall DZStrA::Mid(unsigned pos, unsigned len) const
{
    if (!imp || pos > Length())
        return DZStrA();
    return DZStrA(imp + pos, (int)DZ_UMIN(len, Length() - pos));
}

int __fastcall DZStrA::ReverseFind(char ch, unsigned int pos) const
{
    if (!imp)
        return -1;
    int i = (int)Length();
    if (pos < (unsigned)i)
        i = (int)pos;
    const char* p = &imp[i];
    while (i >= 0 && *p != ch)
    {
        i--;
        p--;
    }
    return i;
}

DZStrA& __fastcall DZStrA::ToLower(void)
{
	if (imp)
	{
		char * bf = GetBuffer();    // make unique
		CharLowerBuffA(bf, Length());
		ReleaseBuffer();
	}
	return *this;
}

DZStrA& __fastcall DZStrA::ToUpper(void)
{
	if (imp)
	{
		char * bf = GetBuffer();    // make unique
		CharUpperBuffA(bf, Length());
		ReleaseBuffer();
	}
	return *this;
}

// --------------------- Convert Unicode to lower ----------------------
// start of C tables
DWORD Xors[89] = {
  0x0020, 0x0001, 0x0159, 0x0003, 0x0007, 0x007F, 0x000F, 0x0187, 0x03D2, 0x03DF,
   0x03DD, 0x0053, 0x03D6, 0x03CB, 0x03F3, 0x03F7, 0x03FF, 0x03EF, 0x03EA,
   0x0326, 0x032A, 0x001F, 0x033B, 0x0339, 0x0325, 0x0002, 0x000E, 0x0006,
   0x0063, 0x0048, 0x03BE, 0x2E5F, 0x03A7, 0x2E58, 0x03C3, 0x00CD, 0x00C9,
   0x002A, 0x0025, 0x0027, 0x0040, 0x0043, 0x0041, 0x0060, 0x0018, 0x004C,
   0x000B, 0x0086, 0x0082, 0x0050, 0x0030, 0x00D0, 0x3DA0, 0x3DE0, 0x1E41,
   0x0008, 0x00CA, 0x00BA, 0x00BE, 0x00AC, 0x0090, 0x0009, 0x0080, 0x22EF,
   0x2141, 0x21CE, 0x007C, 0x0010, 0x0066, 0x006A, 0x006E, 0x001A, 0x001E,
   0x0026, 0x002E, 0x0070, 0x2E09, 0x311E, 0x2E19, 0x2E3C, 0x2E1F, 0x2E3F,
   0x2E22, 0x2E41, 0xBA04, 0x0028, 0x0038, 0x0058, 0x0068
   };

UChar Lows[43][64] = {
  /* 0*/ {0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 1*/ {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 2*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 3,
   0, 2, 0, 2, 0, 2, 0, 0, 4, 0, 5, 0, 4, 0, 6},
  /* 3*/ {0, 4, 0, 5, 0, 4, 0, 7, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 8, 4, 0, 5, 0, 4, 0, 0},
  /* 4*/ {0, 9, 2, 0, 2, 0, 9, 7, 0, 10, 11, 5, 0, 0, 12, 13, 14, 4, 0, 15, 16,
   0, 17, 17, 2, 0, 0, 0, 15, 18, 0, 19, 2, 0, 2, 0, 2, 0, 20, 7, 0, 21, 0, 0,
   2, 0, 20, 22, 0, 23, 24, 5, 0, 4, 0, 25, 2, 0, 0, 0, 2, 0, 0, 0},
  /* 5*/ {0, 0, 0, 0, 26, 4, 0, 27, 2, 0, 28, 5, 0, 4, 0, 22, 0, 4, 0, 5, 0, 4,
   0, 7, 0, 4, 0, 5, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0,
   0, 26, 2, 0, 2, 0, 29, 30, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 6*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 31, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0,
   2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 32, 5, 0, 33, 34, 0},
  /* 7*/ {0, 4, 0, 35, 36, 37, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 8*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2,
   0, 2, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 9*/ {0, 0, 0, 0, 0, 0, 38, 0, 39, 40, 39, 0, 41, 0, 42, 43, 0, 1, 1, 1, 1,
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 44, 44, 0, 44, 44, 44, 44, 44, 44, 44, 44,
   44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 10*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 45, 0, 0, 0, 0, 0, 0, 0,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 0,
   0, 0, 0, 46, 0, 0, 7, 0, 47, 2, 0, 0, 48, 49, 49},
  /* 11*/ {50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 1, 1,
   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 44, 44, 44, 44, 44, 44, 44, 44, 44,
   44, 44, 44, 44, 44, 44, 44, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 12*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 13*/ {2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 14*/ {7, 4, 0, 5, 0, 4, 0, 7, 0, 4, 0, 5, 0, 4, 0, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 15*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50},
  /* 16*/ {51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 52,
   52, 52, 52, 52, 52, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 17*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53,
   53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53},
  /* 18*/ {54, 54, 54, 54, 54, 54, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 19*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 20*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 21*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 55, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0,
   2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 22*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 23*/ {0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 56, 56, 56, 56, 56, 56, 0, 0, 0, 0,
   0, 0, 0, 0, 56, 56, 56, 56, 56, 56, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 56,
   56, 56, 56, 56, 56, 0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 56, 56, 56, 56, 56, 56},
  /* 24*/ {0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 56, 56, 56, 56, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 56, 0, 56, 0, 56, 0, 56, 0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 56, 56,
   56, 56, 56, 56, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 25*/ {0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 56, 56, 56, 56, 56, 56, 0, 0, 0, 0,
   0, 0, 0, 0, 56, 56, 56, 56, 56, 56, 56, 56, 0, 0, 0, 0, 0, 0, 0, 0, 56, 56,
   56, 56, 56, 56, 56, 56, 0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 57, 57, 7, 0, 0, 0},
  /* 26*/ {0, 0, 0, 0, 0, 0, 0, 0, 58, 58, 59, 59, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 56, 56, 60, 60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 56, 56, 61, 61, 62,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 63, 63, 48, 48, 7, 0, 0, 0},
  /* 27*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 65, 66, 0, 0, 0, 0,
   0, 0, 67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 28*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68,
   68, 68, 68, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 29*/ {0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 30*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 69, 69, 70, 70, 71, 71, 70, 70, 69, 69},
  /* 31*/ {72, 72, 73, 73, 72, 72, 74, 74, 38, 38, 75, 75, 38, 38, 74, 74, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 32*/ {51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 50,
   50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 76, 76, 76, 76,
   76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0},
  /* 33*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 77, 78, 79, 0, 0, 7, 0, 4, 0, 5, 0, 80, 81,
   82, 83, 0, 2, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 84, 82},
  /* 34*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 35*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 4, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 36*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 37*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 38*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 0,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0},
  /* 39*/ {2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2,
   0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 5, 0, 85, 2, 0},
  /* 40*/ {2, 0, 2, 0, 2, 0, 2, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 41*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44,
   44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 0, 0, 0, 0, 0},
  /* 42*/ {86, 86, 86, 86, 86, 86, 86, 86, 87, 87, 87, 87, 87, 87, 87, 87, 86,
   86, 86, 86, 86, 86, 86, 86, 88, 88, 88, 88, 88, 88, 88, 88, 89, 89, 89, 89,
   89, 89, 89, 89, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0}
   };
UChar Mids[10][16] = {
  /* 0*/ {0, 1, 0, 2, 3, 4, 5, 6, 7, 8, 0, 0, 0, 9, 10, 11},
  /* 1*/ {12, 13, 14, 15, 16, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 2*/ {0, 0, 18, 19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 3*/ {0, 0, 0, 0, 0, 0, 0, 0, 20, 21, 22, 23, 24, 25, 26, 27},
  /* 4*/ {0, 0, 0, 0, 28, 29, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 5*/ {0, 0, 31, 32, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 6*/ {33, 34, 35, 36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  /* 7*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 37, 38, 0, 39, 40, 41, 0},
  /* 8*/ {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 42, 0, 0, 0},
  /* 9*/ {43, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
   };
UChar Highs[66] = {
  1, 2, 0, 0, 3, 0, 0, 4, 5, 6, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 9, 0, 10
   };
const UCS4 Highest_Upper = 0x010427;
// end of C tables

UCS4 __fastcall Unicode_To_Lower(UCS4 Code)
{
	if (Code < 0x80)
	{
		if (Code >= L'A' && Code <= L'Z')
			return Code + 0x20;
		return Code;
	}

	if (Code <= Highest_Upper)
	{
		UCS4 Index_low = Code & 63;
		UCS4 Index_mid = Code >> 6;
		UCS4 Index_high = Index_mid >> 4;
		UCS4 n = Highs[Index_high];
		if (n)
		{
			n = Mids[n - 1][Index_mid & 0xF];
			if (n)
			{
				n = Lows[n - 1][Index_low];
				if (n)
					Code ^= Xors[n - 1];
			}
		}
	}
	return Code;
}

void __fastcall Lower_BuffW(wchar_t *ws, int len)
{
	if (ws && len > 0)
	{
		for (;*ws && len > 0; ws++, len --)
			*ws = (wchar_t)Unicode_To_Lower((UCS4)*ws);
		*ws = 0;
	}
}

//----------------- Conversion routines to/from UTF8  ----------------------

const int halfShift = 10;

const UCS4 halfBase = 0x0010000;
const UCS4 halfMask = 0x3FF;

const UCS4 offsetsFromUTF8[6] =
	 {0x00000000, 0x00003080, 0x000E2080, 0x03C82080, 0xFA082080, 0x82082080};

const UChar bytesFromUTF8[256] = {
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
	2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5};

const UChar firstByteMark[7] = {0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC};
const UCS4 ReplacementCharacter = 0x0000FFFD;
const UCS4 MaximumUCS2 = 0x0000FFFF;
const UCS4 MaximumUTF16 = 0x0010FFFF;
const UCS4 MaximumUCS4 = 0x7FFFFFFF;

const UCS4 SurrogateHighStart = 0xD800;
const UCS4 SurrogateHighEnd = 0xDBFF;
const UCS4 SurrogateLowStart = 0xDC00;
const UCS4 SurrogateLowEnd = 0xDFFF;

//--------------------------------------------------------------------------

// ud == 0 _ count chars required
int Wide_To_UTF8(const wchar_t *ws, int wslen, UChar *ud, int udlen)
{
	if (!ws || !*ws || udlen < 0)//!ud)
		return 0;

	if (wslen < 0)
	{
		wslen = 0;
		for (const wchar_t*p = ws; *p; p++)
			wslen++;
	}
	if (!wslen)
		return 0;

	int bytesWritten = 0;
	for (int J = 0; J < wslen; J++)
	{
		int bytesToWrite;
		UCS4 byteMask = 0xBF;
		UCS4 byteMark = 0x80;

		UCS4 ch = UCS4(ws[J]);

		if (ch < 0x80)
			bytesToWrite = 1;
		else
			if (ch < 0x800)
				bytesToWrite = 2;
			else
			{
				if (ch >= SurrogateHighStart && ch <= SurrogateHighEnd)
				{
					// could be Surrogate pair
					UCS4 cl;
					if ((J + 1) < wslen &&
					 ((cl = (UCS4)ws[J+1]) >= SurrogateLowStart) &&
						cl <= SurrogateLowEnd)
					{
						// have pair
						J++;
						cl -= SurrogateLowStart;
						ch -= SurrogateHighStart;
						ch = (ch * 0x400) + cl + 0x10000;
					}
				}
				if (ch < 0x10000)
					bytesToWrite = 3;
				else
					if (ch < 0x200000)
						bytesToWrite = 4;
					else
						if (ch < 0x4000000)
							bytesToWrite = 5;
						else
							if (ch <= MaximumUCS4)
								bytesToWrite = 6;
							else
							{
								bytesToWrite = 2;
								ch = ReplacementCharacter;
							}
			}
		if (!ud)//len < 1)
			bytesWritten += bytesToWrite; // just count
		else
		{
			if (bytesWritten + bytesToWrite <= udlen)
			{
				for (int L = bytesToWrite; L >= 2; L--)
				{
					ud[bytesWritten + L - 1] =
						(UChar)((ch | byteMark) & byteMask);
					ch = ch >> 6;
				}
				ud[bytesWritten] = (UChar)(ch | firstByteMark[bytesToWrite]);
				bytesWritten += bytesToWrite;
			}
			else
				break;
		}
	}
	return bytesWritten;
}
//--------------------------------------------------------------------------

// wd == 0 _ count chars required
int UTF8_To_Wide(const UChar* us, int ulen, wchar_t* wd, int wdlen)
{
	if (!us || !*us || wdlen < 0)//!wd)
		return 0;

	int L = 0; // read
	int T = 0; // written
	if (ulen < 0)
	{
		ulen = 0;
		for (const UChar *p = us; *p; p++)
			ulen++;
	}
	if (!ulen)
		return 0;
	while (L < /*=*/ ulen)
	{
		UCS4 ch = 0;
		int extraBytesToWrite = bytesFromUTF8[us[L]];
		if (extraBytesToWrite + L - 1 > ulen)
			return T;
		for (int J = extraBytesToWrite; J > 0; J--)
		{
			ch = ch + us[L++];
			ch = ch << 6;
		}
		ch = ch + us[L++];
		ch = ch - offsetsFromUTF8[extraBytesToWrite];

		if (ch <= MaximumUCS2)
		{
			if (!wd) //wdlen < 1)
				T++; // just count
			else
			{
				if (T >= wdlen)
					return T;
				wd[T++] = (wchar_t)ch;
			}
		}
		else
			if (ch > MaximumUCS4)
			{
				if (!wd)//wdlen < 1)
					T++; // just count
				else
				{
					if (T >= wdlen)
						return T;
					wd[T++] = (wchar_t)ReplacementCharacter;
				}
			}
			else
			{
				if (!wd)//wdlen < 1)
					T += 2; // just count
				else
				{
					if (T + 1 >= wdlen)
						return T;
					ch = ch - halfBase;
					wd[T++] = (wchar_t)((ch >> halfShift) + SurrogateHighStart);
					wd[T++] = (wchar_t)((ch & halfMask) + SurrogateLowStart);
				}
			}
	}
	return T;
}

