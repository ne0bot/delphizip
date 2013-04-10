#ifndef _STDAFX_H_
#define _STDAFX_H_
// stdafx.h : include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//
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
************************************************************************/

#pragma once

#ifndef UNICODE
  #define UNICODE
#endif
#ifndef _UNICODE
  #define _UNICODE
#endif

#ifdef _WIN64
  #define __fastcall
  #define __cdecl
  #ifdef _USE_ASM_
    #undef _USE_ASM_
  #endif
#else
// Modify the following defines if you have to target a platform prior to the ones specified below.
#ifdef ALLOW_WIN98
    #ifndef _WIN32_WINDOWS  // Allow use of features specific to Windows 98 or later.
        #define _WIN32_WINDOWS 0x0410 // Change this to the appropriate value to target Windows Me or later.
    #endif          
    #ifndef WINVER    // Allow use of features specific to Windows 98 or later.
        #define WINVER 0x0400
    #endif
#else
// Refer to MSDN for the latest info on corresponding values for different platforms.
    #ifndef WINVER    // Allow use of features specific to Windows XP or later.
        //#define WINVER 0x0400
        #define WINVER 0x0501  // Change this to the appropriate value to target other versions of Windows.
    #endif
//
    #ifndef _WIN32_WINNT  // Allow use of features specific to Windows XP or later.
        #define _WIN32_WINNT 0x0500 // Change this to the appropriate value to target other versions of Windows.
    #endif
    #ifndef _WIN32_WINDOWS  // Allow use of features specific to Windows 98 or later.
        #define _WIN32_WINDOWS 0x0500 // Change this to the appropriate value to target Windows Me or later.
    #endif
//
//
//#ifndef _WIN32_IE   // Allow use of features specific to IE 6.0 or later.
//#define _WIN32_IE 0x0600 // Change this to the appropriate value to target other versions of IE.
//#endif
//
#endif
#endif

#define WIN32_LEAN_AND_MEAN  // Exclude rarely-used stuff from Windows headers
// Windows Header Files:
#include <windows.h>



#include <stdio.h>
#include <stdlib.h>
#include <tchar.h>
#include <time.h>
#include <sys\types.h>
#include <errno.h>
#ifndef MAXINT
#include <values.h>
#endif
#if defined(__BORLANDC__) && (__BORLANDC__ < 0x0570)
//    #include <values.h>
    #define FOF_NORECURSION 0x1000
    #define INVALID_SET_FILE_POINTER  ((DWORD)-1)
    using namespace std;
#endif

#define  MAX_UNSIGNED 0xFFFFFFFFul
#define  MAX_WORD 0xFFFFu
#define  MAX_BYTE = 0xFFu
#ifndef MAXINT
    #define MAXINT      0x7fffffff
#endif
#ifndef MAXLONG
    #define MAXLONG     0x7fffffffl
#endif

#ifndef MAXINT
  #define MAXINT INT_MAX
#endif

#endif





