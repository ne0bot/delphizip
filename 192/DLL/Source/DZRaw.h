
#ifndef DZRawH
#define DZRawH
//---------------------------------------------------------------------------

/* DZRaw.H * Copyright (C)  2009 Russell Peters
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

#pragma pack(push, 1)
union XWord
{
    unsigned char b[2];
    unsigned short w;
};
#pragma pack(pop)

#pragma pack(push, 2)
struct dzraw_imp
{
    long refs;      // reference counter
    WORD  capacity; // characters
    WORD  len;      // string length
    unsigned char data[8];    // the actual data
};
#pragma pack(pop)

class DZRawData
{
    private:
        dzraw_imp* imp;
    protected:
        dzraw_imp* __fastcall NewImp(unsigned siz);
        dzraw_imp* NewImp(const unsigned char* src, int Len, int Space = -1);
        void __fastcall Release(void);
        int __fastcall IncRefs(void);
        int __fastcall DecRefs(void);
    public:
        __fastcall DZRawData(void): imp(0){};
        __fastcall DZRawData(unsigned size);
        __fastcall DZRawData(const DZRawData& other);
        __fastcall DZRawData(const unsigned char* str, unsigned len);
        __fastcall ~DZRawData(void){Release();}
        unsigned __fastcall Capacity(void) const;
        unsigned __fastcall Length(void) const;
 inline bool IsEmpty(void) const {return !imp || !Length();}
 inline void Empty(void) {Release();}
        void __fastcall Append(const unsigned char* src, int Len);
        void __fastcall Assign(const unsigned char* src, int Len);
        const unsigned char* begin(void) const;
 inline const unsigned char* data(void) const {return begin();}
        const unsigned char* end(void) const;
        const unsigned char* Find(WORD tag) const;
		unsigned char * __fastcall GetBuffer(unsigned size);
		void __fastcall SetLength(unsigned Len = 0);
		operator const unsigned char*()const {return begin();}
		DZRawData& __fastcall operator=(const DZRawData& other);
		DZRawData __fastcall operator+(const DZRawData& other);
		DZRawData& __fastcall operator+=(const DZRawData& other);
		DZRawData& __fastcall operator+=(unsigned char ch);
		DZRawData& __fastcall operator+=(WORD w);
		DZRawData __fastcall operator-(WORD tag);
		DZRawData& __fastcall operator-=(WORD tag);
		WORD __fastcall operator[](unsigned idx) const;
 inline bool operator!()const {return IsEmpty();}
};


#endif
