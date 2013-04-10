#include "stdafx.h"
#pragma hdrstop

#include "common.h"
#include "DZOper.h"
#include "Helpers.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_HELPERS_CPP
/*
  Helpers.cpp - internal 'stream' like helpers
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

ZStreamIO::ZStreamIO(DZOp *theOwner, const DZStrW& filename)
{
    Owner = theOwner;
	IsFile = false;
    if (Is_Drv(filename) < 0)
		fname = filename.Mid((unsigned)DriveLen(filename)); // only filename
    else
		fname = filename;
    IsTemp = false;
	canseek = -1; // untested
}

ZStreamIO::~ZStreamIO()
{
//
}

bool ZStreamIO::operator!() const
{
    return !IsOpen();
}


bool __fastcall ZStreamIO::GetIsSeekable(void)
{
    if (!IsOpen())
        return false;

    if (canseek >= 0)
        return canseek > 0;

    __int64 curpos = SetPosition(0, FILE_CURRENT);
    if (curpos < 1)
        return DefSeekable();   // cannot test at beginning

    canseek = 0;

    if (SetPosition(-1, FILE_CURRENT) == -1)
    {
        if (Owner->Verbose < 0)
            Owner->Notify((unsigned)-555, _T("Seek- failed for %s"), fname.c_str());

        return false;
	}

    if (SetPosition(curpos, FILE_BEGIN) == -1)
    {
        if (Owner->Verbose < 0)
			Owner->Notify((unsigned)-555, _T("Seek+ failed for %s"), fname.c_str());

        return false;
    }

    canseek = 1;      // 1.75 can seek

    return true;
}

bool ZStreamIO::SetEndOfFile(void)
{
    return false;// set length = position
}

ZFile::ZFile(DZOp *theOwner, const DZStrW& lpFileName, DWORD dwDesiredAccess,
				DWORD dwShareMode,  LPSECURITY_ATTRIBUTES lpSecurityAttributes,
             DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes) : ZStreamIO(theOwner, lpFileName)
{
	fHandle = CreateFile(lpFileName.c_str(), dwDesiredAccess, dwShareMode,
                         lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
                         NULL);
    IsFile = true;
}
             
ZFile::ZFile(DZOp *theOwner, const DZStrW& lpFileName, DWORD Flags) :
						ZStreamIO(theOwner, lpFileName)
{     
  fHandle = CreateFile(lpFileName.c_str(), GENERIC_READ, 0, NULL,
				OPEN_EXISTING, Flags, NULL);
  if (fHandle == INVALID_HANDLE_VALUE && GetLastError() == ERROR_SHARING_VIOLATION )
  {
	  fHandle = CreateFile(lpFileName.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL,
				OPEN_EXISTING, Flags, NULL);
	  if (fHandle == INVALID_HANDLE_VALUE && GetLastError() == ERROR_SHARING_VIOLATION )
	  {
		fHandle = CreateFile(lpFileName.c_str(), GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE,
			NULL, OPEN_EXISTING, Flags, NULL);
	  }
  }             
  if (fHandle == INVALID_HANDLE_VALUE && Owner->Verbose < 0)
	Owner->Notify(ITRACE, _T("open read failed; filename=%s [%d]"), lpFileName.c_str(),
			GetLastError());
	IsFile = true;

}

ZFile::~ZFile()
{
    Close();

    if (IsTemp && !fname.IsEmpty())
    {
        // remove temporary
        Owner->EraseFile(fname, true);

        if (Owner->Verbose < 0)
            Owner->Notify(ITRACE, _T("Erased %s"), fname.c_str());
    }
}

bool __fastcall ZFile::DefSeekable(void)
{
    return false;
}

bool ZFile::Read(LPVOID lpBuffer, DWORD nNumberOfBytesToRead,
                 LPDWORD lpNumberOfBytesRead)
{
    return ReadFile(fHandle, lpBuffer, nNumberOfBytesToRead, lpNumberOfBytesRead, NULL) != 0;
}

bool ZFile::Write(LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite,
                  LPDWORD lpNumberOfBytesWritten)
{
    return WriteFile(fHandle, lpBuffer, nNumberOfBytesToWrite, lpNumberOfBytesWritten, NULL) != 0;
}

__int64 ZFile::SetPosition(__int64 ofs, int from)
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
    o.lo = ::SetFilePointer(fHandle, (long)o.lo, (LONG*)& o.hi, (DWORD)from);

    if (o.lo == INVALID_SET_FILE_POINTER  && GetLastError())
        return -1;

    return o.i64;
}

bool ZFile::SetEndOfFile(void)
{
    return ::SetEndOfFile(fHandle) != 0;
}

bool ZFile::Close()
{
    bool was = IsOpen();

    if (was)
    {
        HANDLE tmp = fHandle;
        fHandle = INVALID_HANDLE_VALUE;
        CloseHandle(tmp);
    }

    return was;
}

bool ZFile::IsOpen() const
{
    return fHandle != INVALID_HANDLE_VALUE;
}

bool ZFile::SetTime(const FILETIME* lpCreationTime,
                    const FILETIME* lpLastAccessTime, const FILETIME* lpLastWriteTime)
{
    return SetFileTime(fHandle, lpCreationTime, lpLastAccessTime, lpLastWriteTime) != 0;
}

bool ZFile::GetTime(FILETIME* lpCreationTime,
                    FILETIME* lpLastAccessTime,
                    FILETIME* lpLastWriteTime)
{
    return GetFileTime(fHandle, lpCreationTime, lpLastAccessTime, lpLastWriteTime) != 0;
}



//ALL interface structures BYTE ALIGNED
/* stream operation arg usage
   zacStIdentify,
//      IN BufP = name
      IN Number = number
     OUT ArgLL = size, ArgD = Date, ArgA = Attrs
   zacStCreate,
//      IN BufP = name
      IN Number = number
     OUT StrmP = stream
   zacStClose,
      IN Number = number
      IN StrmP = stream
     OUT StrmP = stream (= NULL)
   zacStPosition,
      IN Number = number
      IN StrmP = stream, ArgLL = offset, ArgI = from
     OUT ArgLL = position
   zacStRead,
      IN Number = number
      IN StrmP = stream, BufP = buf, ArgI = count
     OUT ArgI = bytes read
   zacStWrite
      IN Number = number
      IN StrmP = stream, BufP = buf, ArgI = count
     OUT ArgI = bytes written
*/
ZStream::ZStream(DZOp *theOwner, const DZStrW& filename) :
        ZStreamIO(theOwner, filename)
{
    SetLastError(0);
    CanClose = true;
    zHandle = NULL;
	IsFile = false;
    Number = (-Is_Drv(filename)) -2;
    // cannot use for path
    if (Number < 0 || filename.LastChar() == '\\' || filename.LastChar() == '/')
    {
        SetLastError(DZ_STREAM_NO_OPEN);
        return;
    }
    ZS_Rec *cb = &Owner->ZSData;
    cb->StrmP = NULL;
	cb->OpCode = zsaCreate;
	cb->Number = Number;
    int r = Owner->StreamCB();

    if (r == CALLBACK_TRUE)
        zHandle = cb->StrmP;
    else
        SetLastError(DZ_STREAM_NO_OPEN);
}

ZStream::ZStream(DZOp *theOwner, const DZStrW& filename, void* astream):
        ZStreamIO(theOwner, filename)
{
    Number = 0;
    zHandle = astream;
    IsFile = false;
    CanClose = false;
}

ZStream::~ZStream()
{
    if (CanClose)
        Close();
}

bool __fastcall ZStream::DefSeekable(void)
{
    return true;
}

//const int CHUNK = 0x400000;    // limit
// problem TStream uses integer count
bool ZStream::Read(LPVOID lpBuffer, DWORD nNumberOfBytesToRead,
                   LPDWORD lpNumberOfBytesRead)
{
    SetLastError(0);

    if ((int)nNumberOfBytesToRead < 0)
    {
        // failed
        SetLastError(DZ_STREAM_NO_READ);
        return false;
	}

    ZS_Rec *cb = &Owner->ZSData;
    cb->StrmP = zHandle;
    cb->Number = Number;
	cb->ArgI = (int)nNumberOfBytesToRead;
    cb->BufP = (unsigned char*)lpBuffer;
    cb->OpCode = zsaRead;
    int r = Owner->StreamCB();

    if (r != CALLBACK_TRUE)
    {
        // failed
        SetLastError(DZ_STREAM_NO_READ);
        return false;
    }

	DWORD res = (DWORD)cb->ArgI;  // bytes read

    if (lpNumberOfBytesRead)
        *lpNumberOfBytesRead = res;

    return true;
}

bool ZStream::Write(LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite,
                    LPDWORD lpNumberOfBytesWritten)
{
    SetLastError(0);

    if ((int)nNumberOfBytesToWrite < 0)
    {
        // failed
        SetLastError(DZ_STREAM_NO_WRITE);
        return false;
	}

    ZS_Rec *cb = &Owner->ZSData;
    cb->StrmP = zHandle;
    cb->Number = Number;
	cb->ArgI = (int)nNumberOfBytesToWrite;
    cb->BufP = (unsigned char*)lpBuffer;
    cb->OpCode = zsaWrite;
    int r = Owner->StreamCB();

    if (r != CALLBACK_TRUE)
    {
        // failed
        SetLastError(DZ_STREAM_NO_WRITE);
        return false;
    }

	DWORD res = (DWORD)cb->ArgI;  // bytes written

    if (lpNumberOfBytesWritten)
        *lpNumberOfBytesWritten = res;

    if (res == nNumberOfBytesToWrite)
        return true;

    return false;
}

__int64 ZStream::SetPosition(__int64 ofs, int from)
{
    SetLastError(0);
    ZS_Rec *cb = &Owner->ZSData;
    cb->StrmP = zHandle;
    cb->Number = Number;
    cb->ArgI = from;
    cb->ArgLL = ofs;
    cb->OpCode = zsaPosition;
    int r = Owner->StreamCB();

    if (r == CALLBACK_TRUE)
        return cb->ArgLL;

    SetLastError(DZ_STREAM_NO_SEEK);

    return -1;
}

bool ZStream::Close()
{
    bool was = IsOpen();
    SetLastError(0);

    if (CanClose && was)
    {
        ZS_Rec *cb = &Owner->ZSData;
        cb->StrmP = zHandle;
        cb->Number = Number;
        cb->OpCode = zsaClose;
        int r = Owner->StreamCB();

        if (r == CALLBACK_TRUE)
            zHandle = 0;//cb->StrmP;
        else
            SetLastError(DZ_STREAM_NOT_OPEN);
    }

    return was;
}

bool ZStream::IsOpen() const
{
    return zHandle != 0;// && Owner != NULL;
}

bool ZStream::SetTime(const FILETIME* /*lpCreationTime*/,
					  const FILETIME* /*lpLastAccessTime*/, const FILETIME* /*lpLastWriteTime*/)
{
//#pragma argsused
    return true; // ignore
}

bool ZStream::GetTime(FILETIME* /*lpCreationTime*/,
                      FILETIME* /*lpLastAccessTime*/, FILETIME* /*lpLastWriteTime*/)
{
//#pragma argsused
    return true; // ignore
}

AutoStream::AutoStream(ZStreamIO **zs)
{
    theStream = zs;
}

AutoStream::~AutoStream()
{
    if (theStream)
    {
        ZStreamIO *tmp = *theStream;
        *theStream = NULL;
        theStream = NULL;

        if (tmp)
            delete tmp;
    }
}

void AutoStream::Assign(ZStreamIO **zs)
{
    if (theStream)
    {
        ZStreamIO *tmp = *theStream;
        *theStream = NULL;
        theStream = NULL;

        if (tmp)
            delete tmp;
    }

    theStream = zs;
}


