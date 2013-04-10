//---------------------------------------------------------------------------

#ifndef ZStringsH
#define ZStringsH
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
************************************************************************
*/
//---------------------------------------------------------------------------
#ifndef UNICODE
#error UNICODE required
#endif
#ifndef _UNICODE
#define _UNICODE
#endif
#include "DZ_StrW.h"

#ifndef CP_OEM
#define CP_OEM 1
#endif

#define SLASH _T('/')
#define BSLASH _T('\\')
#define DOT _T('.')
#define COLON _T(':')
#define WILDALL _T('*')
#define WILDANY _T('?')

int __fastcall ValidUTF8(const DZStrA &str);

#define HOWRAW 0
#define HOWUTF8 2
#define HOWOEM 3
DZStrW __fastcall StrIncSep(const DZStrW &p);
DZStrW __fastcall StrExcSep(const DZStrW &p);
DZStrW __fastcall StrIntSep(const DZStrW &p);
DZStrA __fastcall StrIntSep(const DZStrA &p);
DZStrW __fastcall StrExtSep(const DZStrW &p);
DZStrA __fastcall StrExtSep(const DZStrA &p);

//DZStrA __fastcall StrToUTF8(const DZStrW &str);
//DZStrA __fastcall StrToOEM(const DZStrW &str);
//DZStrA __fastcall StrToOEM(const DZStrA &str);
//DZStrW __fastcall OEMToStr(const char *str);

DZStrA __fastcall toHex(unsigned val, unsigned cnt);
int __fastcall Is_Drv(const DZStrW &spec);
int __fastcall Is_DrvEx(const DZStrW &spec);
bool __fastcall Is_AbsPath(const DZStrW& pth);

const int Z_BAD_DRIVE = -1;
const int Z_BAD_SEP = -2;
const int Z_BAD_SPACE = -3;     // lead/trail space
const int Z_BAD_CLEN = -4;      // component too long
const int Z_BAD_CHAR = -5;      // invalid char
const int Z_BAD_NAME = -6;      // has reserved name
const int Z_BAD_PARENT = -7;    // attempt to back below root
const int Z_IS_THIS = -8;
const int Z_IS_PARENT = -9;
const int Z_WILD = 1;
int __fastcall CleanPath(const DZStrW& pathin, DZStrW& pathout);//, bool CheckA);
int __fastcall CheckComponent(const DZStrW& c);
int __fastcall DriveLen(const DZStrW &fspec);

bool __fastcall ZMatch(const DZStrW &thePattern, const DZStrW &theSpec);
DWORD __fastcall GetFileAttrs(const DZStrW& p);
bool __fastcall IsWild(const DZStrW& p);
                                   
const TCHAR* __fastcall Is_In(const TCHAR *p, TCHAR c);  
const char* __fastcall Is_InA(const char *p, char c);

TCHAR __fastcall LastChar(const TCHAR *p);

const char* DupStr(const DZStrA& from);
const wchar_t* DupStr(const DZStrW& from);

DZStrW __fastcall GetArg(const DZStrW &CmdLine, unsigned &idx, bool AllowPW);

bool __fastcall DirExists(const DZStrW& path);
DZStrW __fastcall ExtractFilePath(const DZStrW path);
bool __fastcall ForceDirectories(const DZStrW Dir, int minlen);
//unsigned long __fastcall Unicode_To_Lower(unsigned long Code);

#endif
