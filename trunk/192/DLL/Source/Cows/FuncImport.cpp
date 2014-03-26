#include "stdafx.h"
#pragma hdrstop
/*
 * Copyright (c) 2001-2004 Vaclav Slavik
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE. 
 * 
 * $Id: unicows_import.c,v 1.14 2005/01/02 13:30:37 vaclavslavik Exp $
 * 
 */
/************************************************************************
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
#if defined(UNICODE) && defined(ALLOW_WIN98)

//#include <windows.h>
 /*
char *func_names[] =
{
    "0CreateDirectoryW",
"0CreateFileW",
"0DeleteFileW",
"0FindFirstFileW",
"0FindNextFileW",
"0FindResourceW",
"0GetCurrentDirectoryW",
"0GetDriveTypeW",
"0GetFileAttributesW",
"0GetFullPathNameW",
"0GetModuleHandleW",
"0GetShortPathNameW",
"0GetStringTypeW",
"0GetTempPathW",
"0GetVolumeInformationW",
"0LCMapStringW",
"0MoveFileWithProgressW",
"0RemoveDirectoryW",
"0SetFileAttributesW",
//"2SHChangeNotify",
"2SHFileOperationW",
"1CharToOemW",
"1LoadStringW",
"1MessageBoxW",
"1OemToCharW",
"0lstrlenW",
"0OutputDebugStringW",
0
}; */
              
extern const char *func_names[];

#define DLL_KERNEL32        0
#define DLL_USER32          1
#define DLL_SHELL32         2

#define DLLS_COUNT         3

static HMODULE dllHandles[DLLS_COUNT] = {0};

static const char *dllNames[DLLS_COUNT] =
    {
      "kernel32.dll", "user32.dll", "shell32.dll"
    };
                          
//typedef void WINAPI (SHChangeNotifyPtr*)(LONG wEventId, UINT uFlags, LPCVOID dwItem1, LPCVOID dwItem2);

//SHChangeNotifyPtr OrigSHChangeNotify;

extern void *u_jmps[];

int Init_Imports(void)
{
    const char **fn;
    const char *n;
    int dll;//, i;
    void *pf;
	void **uj = u_jmps;
    for (fn = func_names; *fn; fn++)
    {
        n = *fn;
        dll = *n++ - '0';
        if (dll < 0 || dll >= DLLS_COUNT)
        {
            return -1;
        }
        if (!dllHandles[dll])
        {
            dllHandles[dll] = LoadLibraryA(dllNames[dll]);
            if (!dllHandles[dll])
                return -2;
    //            UnicowsImportError(dllNames[dll], NULL);
        }                                            
        pf = GetProcAddress(dllHandles[dll], n);
        if (pf)
            *uj++ = pf;
    }
    return 0;
}

void FreeDLLs(void)
{
    size_t i;

	for (i = 0; i < DLLS_COUNT; i++)
	{
		if (dllHandles[i])
			FreeLibrary(dllHandles[i]);
		dllHandles[i] = 0;
	}
}
#endif

