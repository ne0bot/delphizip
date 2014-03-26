#include "stdafx.h"
#pragma hdrstop
#if defined(UNICODE) && defined(ALLOW_WIN98)
/* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1/GPL 2.0/LGPL 2.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is for Open Layer for Unicode (opencow).
 *
 * The Initial Developer of the Original Code is Brodie Thiesfield.
 * Portions created by the Initial Developer are Copyright (C) 2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Alternatively, the contents of this file may be used under the terms of
 * either the GNU General Public License Version 2 or later (the "GPL"), or
 * the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
 * in which case the provisions of the GPL or the LGPL are applicable instead
 * of those above. If you wish to allow use of your version of this file only
 * under the terms of either the GPL or the LGPL, and not to allow others to
 * use your version of this file under the terms of the MPL, indicate your
 * decision by deleting the provisions above and replace them with the notice
 * and other provisions required by the GPL or the LGPL. If you do not delete
 * the provisions above, a recipient may use your version of this file under
 * the terms of any one of the MPL, the GPL or the LGPL.
 *
 * ***** END LICENSE BLOCK ***** */
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

// define these symbols so that we don't get dllimport linkage
// from the system headers
//#define _SHELL32_

//#include <windows.h>
#include "MbcsBuffer.h"
#include <shellapi.h>
#include <shlobj.h>
//#include "WinUser.h"


// ----------------------------------------------------------------------------
// API

// DragQueryFileW
// ExtractIconExW
// ExtractIconW
// FindExecutableW
#if 0
EXTERN_C LPITEMIDLIST STDAPICALLTYPE
SHBrowseForFolderW(
    LPBROWSEINFOW lpbi
    )
{
    char mbcsDisplayName[MAX_PATH];

    BROWSEINFOA biA;
    ::ZeroMemory(&biA, sizeof(biA));

    biA.hwndOwner       = lpbi->hwndOwner;
    biA.pidlRoot        = lpbi->pidlRoot;
    biA.ulFlags         = lpbi->ulFlags;
    biA.lpfn            = lpbi->lpfn;
    biA.lParam          = lpbi->lParam;
    biA.iImage          = lpbi->iImage;
    biA.pszDisplayName  = mbcsDisplayName;

    CMbcsBuffer mbcsTitle;
    if (!mbcsTitle.FromUnicode(lpbi->lpszTitle))
        return NULL;
    biA.lpszTitle = mbcsTitle;

    LPITEMIDLIST result = ::SHBrowseForFolderA(&biA);
    if (!result)
        return NULL;

    ::MultiByteToWideChar(CP_ACP, 0, mbcsDisplayName, -1, lpbi->pszDisplayName, MAX_PATH);
    return result;
}
#endif
// SHChangeNotify
#if 0
extern "C" void WINAPI zSHChangeNotify(LONG wEventId, UINT uFlags, LPCVOID dwItem1, LPCVOID dwItem2)
{
    if (!(uFlags & SHCNF_PATH))
    {
       ::SHChangeNotify(wEventId, uFlags, dwItem1, dwItem2);
       return;
    }   
    CMbcsBuffer mbcsItem1;
    if (!mbcsItem1.FromUnicode((LPCTSTR)dwItem1))
        return ;         
    char *item2 = 0;
    if (dwItem2)
    {
    CMbcsBuffer mbcsItem2;
        if (!mbcsItem2.FromUnicode((LPCTSTR)dwItem2))
            return ;          
        item2 = mbcsItem2;
    }
    ::SHChangeNotify(wEventId, uFlags, mbcsItem1, item2);
}
#endif
// SHFileOperationW
int zzlen(LPCTSTR str)
{
    if (!str || !*str)
        return 0;

    int r = 0;
    do
    {
        for (str; *str; str++)
            r++;
        r++;
    }
    while (*++str);
    return r;
}

extern "C" int WINAPI zSHFileOperationW(LPSHFILEOPSTRUCT lpFileOp)
{
    CMbcsBuffer mbcsFrom;
    CMbcsBuffer mbcsTo;
    CMbcsBuffer mbcsTitle;
    if (lpFileOp->pFrom)
    {
        int FromLen = zzlen(lpFileOp->pFrom);
        if (!mbcsFrom.FromUnicode(lpFileOp->pFrom, FromLen))
            return 0x81;
        lpFileOp->pFrom = (TCHAR*)mbcsFrom.get();
    }
    if (lpFileOp->pTo)
    {
        int ToLen = zzlen(lpFileOp->pTo);
        if (!mbcsTo.FromUnicode(lpFileOp->pTo, ToLen))
            return 0x81;
        lpFileOp->pTo = (TCHAR*)mbcsTo.get();
    }
    if (lpFileOp->lpszProgressTitle)
    {
        int TitleLen = zzlen(lpFileOp->lpszProgressTitle);
        if (!mbcsTitle.FromUnicode(lpFileOp->lpszProgressTitle, TitleLen))
            return 0x81;
        lpFileOp->lpszProgressTitle = (TCHAR*)mbcsTitle.get();
    }
    return ::SHFileOperationA((_SHFILEOPSTRUCTA *)lpFileOp);
}
// SHGetFileInfoW
// SHGetNewLinkInfoW
#if 0
EXTERN_C BOOL STDAPICALLTYPE
SHGetPathFromIDListW(
    LPCITEMIDLIST pidl, 
    LPWSTR pszPath
    )
{
    char mbcsPath[MAX_PATH];
    BOOL success = ::SHGetPathFromIDListA(pidl, mbcsPath);
    if (!success)
        return FALSE;

    int nResult = ::MultiByteToWideChar(CP_ACP, 0, mbcsPath, -1, pszPath, MAX_PATH);
    if (nResult < 1)
        return FALSE;

    return TRUE;
}

EXTERN_C INT STDAPICALLTYPE
ShellAboutW(
    HWND hWnd, 
    LPCWSTR szApp, 
    LPCWSTR szOtherStuff, 
    HICON hIcon
    )
{
    CMbcsBuffer mbcsApp;
    if (!mbcsApp.FromUnicode(szApp))
        return 0;

    CMbcsBuffer mbcsOtherStuff;
    if (!mbcsOtherStuff.FromUnicode(szOtherStuff))
        return 0;

    return ::ShellAboutA(hWnd, mbcsApp, mbcsOtherStuff, hIcon);
}

// ShellExecuteExW

EXTERN_C HINSTANCE STDAPICALLTYPE
ShellExecuteW(
    HWND hwnd, 
    LPCWSTR lpOperation, 
    LPCWSTR lpFile, 
    LPCWSTR lpParameters, 
    LPCWSTR lpDirectory, 
    INT nShowCmd
    )
{
    CMbcsBuffer mbcsOperation;
    if (!mbcsOperation.FromUnicode(lpOperation))
        return 0;

    CMbcsBuffer mbcsFile;
    if (!mbcsFile.FromUnicode(lpFile))
        return 0;

    CMbcsBuffer mbcsParameters;
    if (!mbcsParameters.FromUnicode(lpParameters))
        return 0;

    CMbcsBuffer mbcsDirectory;
    if (!mbcsDirectory.FromUnicode(lpDirectory))
        return 0;

    return ::ShellExecuteA(hwnd, mbcsOperation, mbcsFile, mbcsParameters, mbcsDirectory, nShowCmd);
}
#endif
// Shell_NotifyIconW
#endif
