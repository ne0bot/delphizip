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

//#include <windows.h>
#include <stdlib.h>

#include "MbcsBuffer.h"

// ----------------------------------------------------------------------------
// CMbcsBuffer

bool 
CMbcsBuffer::SetCapacity(
    int aMinCapacity
    )
{
    if (aMinCapacity > mBufferSize)
    {
        if (mBuffer != mStackBuffer)
            ::free(mBuffer);

        mLength = 0;
        mBufferSize = 0;

        mBuffer = (char *) ::malloc(aMinCapacity);
        if (!mBuffer) {
            SetLastError(ERROR_NOT_ENOUGH_MEMORY);
            errno = ENOMEM;
            return false;
        }
        *mBuffer = '\0';

        mBufferSize = aMinCapacity;
    }

    return true;
}

bool 
CMbcsBuffer::FromUnicode(
    LPCWSTR aString, 
    int     aStringLen, 
    int     aMinCapacity)
{
    if (!aString) {
        SetNull();
        return true;
    }

    if (aStringLen == -1)
        aStringLen = ::lstrlenW(aString) + 1;

    int aRequiredLen = ::WideCharToMultiByte(CP_ACP, 0, 
        aString, aStringLen, NULL, 0, NULL, NULL);
    if (aRequiredLen < 1) {
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        errno = ENOMEM;
        return false;
    }

    if (aRequiredLen > aMinCapacity)
        aMinCapacity = aRequiredLen;

    mLength = aRequiredLen - 1; // don't include the null byte

    if (!SetCapacity(aMinCapacity)) {
        SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        errno = ENOMEM;
        return false;
    }

    ::WideCharToMultiByte(CP_ACP, 0, aString, aStringLen, 
        mBuffer, mBufferSize, NULL, NULL);

    return true;
}

//int CMbcsBuffer::ToUnicode(LPWSTR lpszDst, int MaxLen)
//{
//    return ::MultiByteToWideChar(CP_ACP, 0, mbcsDst, -1, lpszDst, MaxLen) > 0;
//}
#endif

