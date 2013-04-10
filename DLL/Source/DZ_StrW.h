//---------------------------------------------------------------------------

#ifndef DZ_StrWH
#define DZ_StrWH

#include <windows.h>
#include <malloc.h>
#include <string.h>
#include <stdarg.h>

//#define BAD_MEM_EXCEPT -999

/* DZ_StrW.H *

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

int DZ_MIN(int arg1, int arg2);
int DZ_MAX(int arg1, int arg2);
unsigned int DZ_UMIN(unsigned int arg1, unsigned int arg2);
unsigned int DZ_UMAX(unsigned int arg1, unsigned int arg2);

class DZStrA;

#pragma pack(push, 4)
    struct dzstrw_imp
    {
        long refs;          // reference counter
        unsigned long capacity; // characters
        unsigned long len;      // string length
        wchar_t data[2];    // the actual data + end 0
    };
#pragma pack(pop)

class DZStrW
{
    private:
        wchar_t* imp; // -->dzstrw_imp.data

    protected:
inline   wchar_t* __fastcall _IMP_Data(dzstrw_imp* _imp) const
        {
            return  _imp ? (wchar_t*)((wchar_t*)_imp + 6) : NULL;
        }
inline    dzstrw_imp* __fastcall _IMP_Ptr(wchar_t* p) const
        {
            return  p ? (dzstrw_imp*)(p - 6) : NULL;
        }

		dzstrw_imp* __fastcall NewImp(/*unsigned*/ int siz);
        dzstrw_imp* __fastcall NewImp(const wchar_t* src, int maxLen = -1);
        dzstrw_imp* __fastcall NewImp(const char* src, int maxLen = -1);
        unsigned __fastcall _Capacity(dzstrw_imp* _imp) const;
        unsigned __fastcall _Length(dzstrw_imp* _imp) const;
		/*unsigned*/ long __fastcall _IncImpRefs(dzstrw_imp* _imp);
		/*unsigned*/ long __fastcall _DecImpRefs(dzstrw_imp* _imp);
        void __fastcall _ReleaseImp(dzstrw_imp* _imp);
        void __fastcall Release(void);
        int __fastcall IncRefs(void);
        int __fastcall DecRefs(void);
        // maxSpace is max characters
        void __fastcall _Assign(const wchar_t* src, int maxLen = -1);
        void __fastcall _Assign(const char* src, int maxLen = -1);
        void __fastcall _Append(const wchar_t* src, int maxSpace = -1);
        void __fastcall _Append(const char* src, int maxSpace = -1);
        void __fastcall _Append(wchar_t ch);
		dzstrw_imp* Unique(void);
	public:
		__fastcall DZStrW(void): imp(0){}
		__fastcall DZStrW(const DZStrW& other);
		__fastcall DZStrW(const DZStrA& other);
		__fastcall DZStrW(UINT cp, const DZStrA& other);
		__fastcall DZStrW(const wchar_t* str, int len = -1);
		__fastcall DZStrW(const char* str, int len = -1);
		__fastcall DZStrW(UINT cp, const char* str, int len = -1);
        __fastcall DZStrW(wchar_t ch, unsigned cnt);
		__fastcall ~DZStrW(void);
 inline void __fastcall Append(const wchar_t* src, int maxSpace = -1) {_Append(src, maxSpace);}
		unsigned __fastcall Capacity(void) const;
		unsigned __fastcall Length(void) const;
		unsigned __fastcall length(void) const {return Length();}
 inline bool IsEmpty(void) const {return !imp || !*imp;}
        void Empty(void) {Release();}
 inline bool empty(void) {return IsEmpty();}
		wchar_t * __fastcall GetBuffer(int minsize = -1);
		void __fastcall ReleaseBuffer(int newlen = -1);
        void __cdecl  AppendFormat(const wchar_t *fmt, ...);
		bool __fastcall BadDOS(void) const;
		int __fastcall Compare(const wchar_t *other) const;
		int __fastcall CompareNoCase(const wchar_t *other) const;
		int __fastcall Delete(unsigned pos, unsigned cnt = 1);
		int __fastcall Find(const wchar_t *sub, unsigned start = 0) const;
		int __fastcall Find(wchar_t ch, unsigned start = 0) const;
		int __fastcall FindOneOf(const wchar_t *chars, unsigned start = 0) const;
        int __cdecl Format(const wchar_t *fmt, ...);
        int __cdecl FormatV(const wchar_t *fmt, va_list arglist);
		int __fastcall Insert(unsigned pos, wchar_t ch);
        wchar_t LastChar(void) const;
		DZStrW __fastcall Left(unsigned len) const;
 		DZStrW __fastcall Mid(unsigned pos, unsigned len = MAXINT) const;
		int __fastcall ReverseFind(wchar_t ch, unsigned int pos = MAXINT) const;
		DZStrW __fastcall Right(unsigned cnt)const {return Mid(Length() - cnt, cnt);}
		DZStrA __fastcall SafeAnsi(void) const;
		DZStrA __fastcall SafeAnsi(int &bad) const;
		DZStrA __fastcall SafeNarrow(unsigned cp) const;
		DZStrA __fastcall SafeNarrow(unsigned cp, int &bad) const;
		DZStrW __fastcall AsLower(void)const;
		DZStrW& __fastcall ToLower(void);
		DZStrW& __fastcall ToUpper(void);
		DZStrW& __fastcall Trim(void);
        DZStrW& __fastcall TrimLeft(void);
        DZStrW& __fastcall TrimLeft(wchar_t ch);
        DZStrW& __fastcall TrimRight(void);
        const wchar_t* c_str(void)const {return imp;}
        operator const wchar_t*()const {return imp;}
		DZStrW& __fastcall operator=(const DZStrW& other);
		DZStrW& __fastcall operator=(const wchar_t* str);
		DZStrW& __fastcall operator=(const char* str);
		DZStrW __fastcall operator+(const DZStrW& other)const;
		DZStrW __fastcall operator+(const wchar_t* str)const;
		DZStrW __fastcall operator+(const wchar_t wch)const;
		DZStrW& __fastcall operator+=(const DZStrW& other);
		DZStrW& __fastcall operator+=(const wchar_t* str);
		DZStrW& __fastcall operator+=(wchar_t ch);
		wchar_t __fastcall operator[](unsigned idx) const;
		bool operator!()const {return !imp;}
		bool operator==(const DZStrW& other)const{return other.Compare(c_str())==0;}
		bool operator!=(const DZStrW& other)const{return other.Compare(c_str())!=0;}
};

#pragma pack(push, 4)
    struct dzstra_imp
    {
        long refs;          // reference counter
        unsigned  capacity; // characters
        unsigned  len;      // string length
        char data[1];    // the actual data + end 0
    };
#pragma pack(pop)

class DZStrA
{
    private:
        char* imp; // -->dzstra_imp.data

    protected:
inline   char* __fastcall _IMP_Data(dzstra_imp* _imp) const
        {
            return  _imp ? (char*)((char*)_imp + 12) : NULL;
        }
inline    dzstra_imp* __fastcall _IMP_Ptr(char* p) const
        {
            return  p ? (dzstra_imp*)(p - 12) : NULL;
        }

		dzstra_imp* __fastcall NewImp(unsigned siz);
        dzstra_imp* __fastcall NewImp(const char* src, int maxLen = -1);
        unsigned __fastcall _Capacity(dzstra_imp* _imp) const;
        unsigned __fastcall _Length(dzstra_imp* _imp) const;
		/*unsigned*/int __fastcall _IncImpRefs(dzstra_imp* _imp);
        /*unsigned*/int __fastcall _DecImpRefs(dzstra_imp* _imp);
        void __fastcall _ReleaseImp(dzstra_imp* _imp);
        void __fastcall Release(void);
        int __fastcall IncRefs(void);
        int __fastcall DecRefs(void);
        // maxSpace is max characters
		void __fastcall _Assign(const char* src, int maxLen = -1);
	public:
        void __fastcall _Append(const char* src, int maxSpace = -1);
		__fastcall DZStrA(void): imp(0){}
		__fastcall DZStrA(const DZStrA& other);
		__fastcall DZStrA(const DZStrW& other, UINT cp = 0);
		__fastcall DZStrA(const char* str, int len = -1);
        __fastcall DZStrA(const wchar_t* wstr);
		__fastcall DZStrA(unsigned val, unsigned cnt);
		__fastcall ~DZStrA(void);
		unsigned __fastcall Capacity(void) const;
		unsigned __fastcall Length(void) const;
        unsigned __fastcall length(void) const {return Length();}
 inline bool IsEmpty(void) const {return !imp || !*imp;}
		void Empty(void) {Release();}
 inline bool empty(void) const {return IsEmpty();}
		char * __fastcall GetBuffer(int minsize = -1);
		bool __fastcall BadDOS(void) const;
		int __fastcall Compare(const char* other) const;
		int __fastcall CompareNoCase(const char* other) const;
		DZStrA __fastcall Left(unsigned len) const;
		DZStrA __fastcall Mid(unsigned pos, unsigned len = MAXINT) const;
		void __fastcall ReleaseBuffer(int newlen = -1);
		int __fastcall Find(char ch) const;
		int __fastcall FindOneOf(const char *chars, unsigned start = 0) const;
		int __fastcall ReverseFind(char ch, unsigned int pos = MAXINT) const;
		DZStrA& __fastcall ToLower(void);
		DZStrA& __fastcall ToUpper(void);
        const char* c_str(void)const {return imp;}
        operator const char*()const {return imp;}
		DZStrA& __fastcall operator=(const DZStrA& other);
		DZStrA& __fastcall operator=(const char* str);
		DZStrA __fastcall operator+(const DZStrA& other)const;
		DZStrA __fastcall operator+(const char* str)const;
		DZStrA& __fastcall operator+=(const DZStrA& other);
		DZStrA& __fastcall operator+=(const char* str);
		DZStrA& __fastcall operator+=(char ch);
		char __fastcall operator[](unsigned idx) const;
		bool operator!()const {return !imp;}
};

typedef unsigned long UCS4;
typedef unsigned char UChar;

void __fastcall Lower_BuffW(wchar_t *ws, int len);
// wdlen <= 0 _ count chars required
int UTF8_To_Wide(const UChar* us, int ulen, wchar_t* wd, int wdlen);
// udlen <= 0 _ count chars required
int Wide_To_UTF8(const wchar_t* ws, int wslen, UChar* ud, int udlen);

UCS4 __fastcall Unicode_To_Lower(UCS4 Code);

#endif
