//---------------------------------------------------------------------------
#ifndef CommonH
#define CommonH
//---------------------------------------------------------------------------

/*
  Common.h - common definitions and functions

  Copyright (c) 1990-2007 Info-ZIP.  All rights reserved.

  See the accompanying file LICENSE, version 2007-Mar-4 or later
  (the contents of which are also included in zip.h) for terms of use.
  If, for some reason, all these files are missing, the Info-ZIP license
  also may be found at:  ftp://ftp.info-zip.org/pub/infozip/license.html

  parts Copyright (C) 1997 Mike White, Eric W. Engler
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
#ifndef _WIN64
#ifdef __MT__
    #define MULTITHREAD
#else
#define __fastcall
#define __cdecl
#undef MULTITHREAD
    #error no Multithread
#endif

#endif

#include "DelZip.h"
#include "dz_msgs.h"

//#define __fastcall
//#define __cdecl

__int64 SetFilePointer64(HANDLE hf, __int64 ofs, int from);

typedef unsigned char   uch;          /* unsigned 8-bit value */
typedef unsigned short  ush;          /* unsigned 16-bit value */
typedef unsigned long   ulg;          /* unsigned 32-bit value */
typedef size_t  extent;

typedef char zchar;

//#ifdef _WIN64
//  extern HANDLE ModuleInst;
//#else
  extern HINSTANCE ModuleInst;
//#endif

typedef unsigned long OperKeys;

#ifndef _FORCE_Z64_
#define Z64THRESHOLD 0xFFED0000u
#else
#define Z64THRESHOLD 0x400u
#endif

//                       these are stored in reverse order
#define  CentralFileHeaderSig   0x02014B50   // 'PK'12
#define  LocalFileHeaderSig     0x04034B50   // 'PK'34  (in file: 504b0304)
#define  CentralDigSig          0x05054B50   // 'PK'55
#define  EndCentralDirSig       0x06054B50   // 'PK'56
#define  ExtLocalSig            0x08074B50   // 'PK'78
#define  EndCentral64Sig        0x06064B50   // 'PK'66
#define  EOC64LocatorSig        0x07064B50   // 'PK'67

typedef __int64 ZInt64;
/*  The zipfile format is defined as below:
 *
 *   [Extra Extended Signature]
 *   NumberOfFiles *{ local header + Filename + [extra data] + [file data] + [data_descriptor] }
 *   NumberOfFiles *{ central directory + Filename + [extra data] + [filecomment] }
 *   End Of Central directory + [ZipFile comment]
 */
#pragma pack(push, 1)
typedef struct _ZipLocalHeader
{
    unsigned long   HeaderSig;        // Should be 0x04034B50
    union
    {
        unsigned char VersionNeeded[2];
        unsigned short VersionNeed;
    };
    unsigned short  Flag;
    unsigned short  ComprMethod;
    union
    {

        struct
        {
            unsigned short  ModifTime;
            unsigned short  ModifDate;
        };

        unsigned long ModifStamp;
    };
    unsigned long   CRC32;
    unsigned long   ComprSize;
    unsigned long   UnComprSize;
    unsigned short  FileNameLen;
    unsigned short  ExtraLen;

    // ... and the filename itself,
    // ... and the extra data.
}ZipLocalHeader;
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct _ZipDataDescriptor
{
    unsigned long DataDescSig;        // Should be 0x08074B50
    unsigned long CRC32;
    unsigned long ComprSize;
    unsigned long UnComprSize;
} ZipDataDescriptor;
#pragma pack(pop)

#pragma pack(push, 1)
struct ZipCentralHeader
{
    unsigned long HeaderSig;          // Should be 0x02014B50
    union
    {
        struct
        {
            unsigned char MadeByVersion;  // Made by version number
            unsigned char MadeByHost;     // Made by host number
        };
        unsigned short  MadeBy;
    };
    union
    {
        unsigned char VersionNeeded[2];
        unsigned short VersionNeed;
    };
//  unsigned short  VersionNeed;
    unsigned short  Flag;
    unsigned short  ComprMethod;
    union
    {
        struct
        {
            unsigned short  ModifTime;
            unsigned short  ModifDate;
        };
        unsigned long ModifStamp;
    };
    unsigned long   CRC32;
    unsigned long   ComprSize;
    unsigned long   UnComprSize;
    unsigned short  FileNameLen;
    unsigned short  ExtraLen;
    unsigned short  FileComLen;
    unsigned short  DiskStart;        // The number of the disk on which this file begins.
    unsigned short  IntFileAtt;
    unsigned long   ExtFileAtt;
    unsigned long   RelOffLocal;      // This is the offset from the start of the first disk on

    // which this file appears, to where the local header
    // should be found.
    // ... and the filename itself,
    // ... and the extra data,
    // ... and the file comment.
};
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct _ZipEndOfCentral
{
    unsigned long   HeaderSig;        // Should be 0x06054B50
    unsigned short  ThisDiskNo;
    unsigned short  CentralDiskNo;
    unsigned short  CentralEntries;   // Total number of entries in the central dir on this disk.
    unsigned short  TotalEntries;     // Total number of entries in the central dir.
    unsigned long   CentralSize;
    unsigned long   CentralOffset;    // Offset of start of central directory with respect to the starting disk number.
    unsigned short  ZipCommentLen;
    // And the comment itself.
} ZipEndOfCentral;
#pragma pack(pop)

#pragma pack(push, 1)
// must be same disk as EOC
typedef struct _Zip64EOCLocator
{
    unsigned long   HeaderSig;        // (4) Should be 0x07064B50
    unsigned long   EOC64DiskStt;     // (4)
    ZInt64          EOC64RelOfs;      // (8) relative to start of it's disk
    unsigned long   NumberDisks;      // (4) total disks
}Zip64EOCLocator;
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct _ZipEOC64
{
    unsigned long   HeaderSig;            // (4) should be 0x06064b50
    ZInt64          vsize;                // (8)    size of variable part
    // variable part   - fields as needed? (old field = 0XFFFF or 0XFF)
    unsigned short  VersionMade;          // (2)
    unsigned short  VersionNeed;          // (2)
    unsigned long   ThisDiskNo;           // (4)
    unsigned long   CentralDiskNo;        // (4)
    ZInt64          CentralEntries;       // (8) Number of central dir entries on this disk
    ZInt64          TotalEntries;         // (8) Number of entries in central dir
    ZInt64          CentralSize;          // (8) Size of central directory
    ZInt64          CentralOffSet;        // (8) offsett of central dir on 1st disk
//  zip64 extensible data sector    (variable size)
}ZipEOC64;
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct _ZipDataDescriptor64
{
    unsigned long DataDescSig;        // Should be 0x08074B50
    unsigned long CRC32;
    ZInt64 ComprSize;
    ZInt64 UnComprSize;
}ZipDataDescriptor64;
#pragma pack(pop)


#pragma pack(push, 1)
typedef struct _ZipCentralDigitSignature
{
    unsigned long   HeaderSig;            // (4) should be 0x05054b50
    unsigned short  vsize;                // (2)
//    data[vsize]
}ZipCentralDigitSignature;
#pragma pack(pop)

#pragma pack(push, 1)
union XDataHead //Zip64_xdata
{
    struct
    {
        unsigned short Tag;                   // (2) should be 0x0001
        unsigned short vsize;                 // (2)
    };
    unsigned long _head;
    // data[vsize]
};
#pragma pack(pop)

#pragma pack(push, 1)
struct UPhead
{
    XDataHead hed;
    unsigned char ver;         // 1
    unsigned long crc;          // crc of orig field
//        char u;
};
#pragma pack(pop)

#pragma pack(push, 1)
struct XNTFSData
{
    _FILETIME MTime;
    _FILETIME ATime;
    _FILETIME CTime;
};
#pragma pack(pop)

#pragma pack(push, 1)
struct XNTFSHead
{
	XDataHead hed;
	unsigned  rsrvd;
	unsigned short Tg1;       // (2) should be 0x0001
	unsigned short Sz1;       // (2) should be 24
	XNTFSData times;
};
#pragma pack(pop)

#pragma pack(push, 1)
struct X_NTFSHeader
{
	XDataHead hed;
	unsigned  rsrvd;
//	unsigned short Tg1;       // (2) should be 0x0001
//	unsigned short Sz1;       // (2) should be 24
//	XNTFSData times;
};
#pragma pack(pop)


#define  ZIP64_XDATA_TAG 0x0001     // Zip64 extra field tag
#define  UPATH_XDATA_TAG 0x7075
#define  UCMNT_XDATA_TAG 0x6375
#define  NTFS_STAMP_TAG 0x000A      // NTFS time stamps

const unsigned char* __fastcall FindTag(WORD tag, const unsigned char *p, unsigned &siz);

#define FS_FAT 0
#define FS_HPFS 6
#define FS_NTFS 11
#define OS_NTFS 0x0B00
#define FLAG_ENCRYPT_BIT 0x0001
#define FLAG_EXTEND_BIT 0x0008
#define FLAG_UTF8_BIT 0x0800

#ifndef MAX
#  define MAX(a, b)   ((a) > (b) ? (a) : (b))
#endif
#ifndef MIN
#  define MIN(a, b)  ((a) < (b) ? (a) : (b))
#endif


extern const ulg crc_table[];

ulg __fastcall crc32(ulg crc, const uch *buf, int len);

int __fastcall Close_Handle(HANDLE *h);

time_t          dos2unixtime(ulg dostime);
ulg             dostime(int, int, int, int, int, int);
ulg             unix2dostime(time_t *);

char * __fastcall zstrdupB(const char* from);
TCHAR * __fastcall zstrdup(const TCHAR* from);
#ifndef _WIN64
void Cleanup_Process(void);
DWORD __fastcall GetFileAttrs(const TCHAR *p);
#endif

#endif







