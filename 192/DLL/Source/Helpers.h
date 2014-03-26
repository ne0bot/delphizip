#ifndef HelpersH
#define HelpersH

/*
  Helpers.h - internal 'stream' like helpers

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

  last modified 2009-07-05
---------------------------------------------------------------------------*/

// return >0 has drive, 0 no drive, -1 stream, -2 autostream
bool __fastcall ZStreamStat(const char *fn, struct stati64 *res);

class ZStreamIO
{
private:
    ZStreamIO(const ZStreamIO& src);
    ZStreamIO& operator=(const ZStreamIO &src);
    int canseek;
//    bool __fastcall GetIsSeekable(void);
protected:
//	DZStrW ffname;
//	bool fFile;
//    bool fIsTemp;
    DZOp *Owner;
    virtual bool __fastcall DefSeekable(void) = 0;
public:
    ZStreamIO(DZOp *theOwner, const DZStrW& filename);
    virtual ~ZStreamIO();
    bool operator!() const;
    virtual bool Read(LPVOID lpBuffer, DWORD nNumberOfBytesToRead,
                      LPDWORD lpNumberOfBytesRead) = 0;
    virtual bool Write(LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite,
                       LPDWORD lpNumberOfBytesWritten) = 0;
    virtual __int64 SetPosition(__int64 ofs, int from) = 0;
    virtual bool SetTime(const FILETIME* lpCreationTime,
                         const FILETIME* lpLastAccessTime,
                         const FILETIME* lpLastWriteTime) = 0;
    virtual bool GetTime(FILETIME* lpCreationTime,
                         FILETIME* lpLastAccessTime,
						 FILETIME* lpLastWriteTime) = 0;
	virtual bool SetEndOfFile(void);
    virtual bool Close() = 0;
    virtual bool IsOpen() const = 0;
//	__property bool IsFile = {read = fFile};
//	__property DZStrW fname = {read = ffname};
//    __property bool IsTemp = {read = fIsTemp, write = fIsTemp};
	bool IsFile;
	DZStrW fname;
	bool IsTemp;
//	__property bool IsSeekable = {read = GetIsSeekable};
	bool __fastcall GetIsSeekable(void);
};

class ZFile: public ZStreamIO
{
private:
    ZFile(const ZFile& src);
    ZFile& operator=(const ZFile &src);
    HANDLE fHandle;
protected:
    virtual bool __fastcall DefSeekable(void);
public:
    ZFile(DZOp *theOwner, const DZStrW& lpFileName, DWORD dwDesiredAccess, DWORD dwShareMode,
          LPSECURITY_ATTRIBUTES lpSecurityAttributes,
          DWORD dwCreationDisposition, DWORD dwFlagsAndAttributes);
    ZFile(DZOp *theOwner, const DZStrW& lpFileName, DWORD Flags);
    virtual ~ZFile();
    bool Read(LPVOID lpBuffer, DWORD nNumberOfBytesToRead,
              LPDWORD lpNumberOfBytesRead);
    bool Write(LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite,
               LPDWORD lpNumberOfBytesWritten);
    __int64 SetPosition(__int64 ofs, int from);
    bool SetTime(const FILETIME* lpCreationTime,
                 const FILETIME* lpLastAccessTime,
                 const FILETIME* lpLastWriteTime);
    bool GetTime(FILETIME* lpCreationTime,
                 FILETIME* lpLastAccessTime,
				 FILETIME* lpLastWriteTime);
	bool SetEndOfFile(void);
    bool Close();
	bool IsOpen() const;
};

class ZStream: public ZStreamIO
{
private:
    ZStream(const ZStream& src);
    ZStream& operator=(const ZStream &src);
    bool CanClose;
    void* zHandle;
    int Number;
protected:
    virtual bool __fastcall DefSeekable(void);
public:
    ZStream(DZOp *theOwner, const DZStrW& filename, void* astream);
    ZStream(DZOp *theOwner, const DZStrW& filename);
    virtual ~ZStream();
    bool Read(LPVOID lpBuffer, DWORD nNumberOfBytesToRead,
              LPDWORD lpNumberOfBytesRead);
    bool Write(LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite,
               LPDWORD lpNumberOfBytesWritten);
    __int64 SetPosition(__int64 ofs, int from);
    bool SetTime(const FILETIME* lpCreationTime,
                 const FILETIME* lpLastAccessTime,
                 const FILETIME* lpLastWriteTime);
    bool GetTime(FILETIME* lpCreationTime,
                 FILETIME* lpLastAccessTime,
				 FILETIME* lpLastWriteTime);
    bool Close();
	bool IsOpen() const;
};

class AutoStream
{
private:
    AutoStream(const AutoStream& src);
    AutoStream& operator=(const AutoStream &src);
    ZStreamIO **theStream;
public:
    AutoStream(ZStreamIO **zs);
    ~AutoStream();
    operator ZStreamIO*()const
    {
        return *theStream;
    };

    void Assign(ZStreamIO **zs);
    ZStreamIO *Stream()
    {
        return *theStream;
    };
};

#endif



