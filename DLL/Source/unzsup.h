// ---------------------------------------------------------------------------

#ifndef UnzSupH
#define UnzSupH
/*

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
 *********************************************************************** */
#include "dzoper.h"
#include "crypt.h"
#include "helpers.h"

#define SAFE_MAX_PATH ((MAX_PATH * 3) / 2)
#define SAFE_FILNAMSIZ (FILNAMSIZ + (FILNAMSIZ / 4))

/* GRR 960218:  testing a new feature...definitely *not* ready for primetime */
#ifdef TIMESTAMP
#  undef TIMESTAMP
#endif

/* Predefined, Machine-specific Macros */

#if (!defined(MSDOS))
#  define MSDOS
#endif

// By defining COPYRIGHT_CLEAN, we lose support for these 2 compression
// types: tokenizing (which was never popular)
// and  reducing   (which was only used in pre-v1 beta releases of PKZIP
// #ifndef COPYRIGHT_CLEAN
#  define COPYRIGHT_CLEAN
// #endif

#  define NO_MULTIPART

#if (defined ASM_INFLATECODES)
# undef ASM_INFLATECODES
#endif

// EWE note: All platforms need this VMS version:
#define VMS_UNZIP_VERSION 42
/* ---------------------------------------------------------------------------
Win32 section:
--------------------------------------------------------------------------- */

/** ********** */

/* Defines */

/** ********** */
#define UNZIP_VERSION     45  /* 20    / * compatible with PKUNZIP 2.0 */

/* clean up with a few defaults */
#  define DATE_FORMAT   DF_MDY  /* defaults to US convention */

/* Max. value of signed (>=32 bit) time_t */
# define S_TIME_T_MAX ((time_t)(ulg)0x7FFFFFFFL)

/* Max. value of unsigned (>=32 bit) time_t */
# define U_TIME_T_MAX ((time_t)(ulg)0xFFFFFFFFL)

#ifdef DOSDATE_2038_01_18
# undef DOSDATE_2038_01_18
#endif
#define DOSDATE_2038_01_18 ((ush)0x7432)

// defines for extraction
#  define NEXTBYTE  (--fincnt >= 0 ? (int)(*finptr++) : (int)readbyte())

#if (defined(SFX) && !defined(NO_ZIPINFO))
#  define NO_ZIPINFO
#endif

/* If <limits.h> exists on most systems, should include that, since it may
 * define some or all of the following:  NAME_MAX, PATH_MAX, _POSIX_NAME_MAX,
 * _POSIX_PATH_MAX.
 */
#include <limits.h>

#ifndef PATH_MAX
#        define PATH_MAX  1024
#endif /* !PATH_MAX */

#if defined(__BORLANDC__) && __BORLANDC__ < 0x0560
#define INVALID_FILE_ATTRIBUTES ((DWORD) -1)
#endif

#define FILNAMSIZ  PATH_MAX

#define ZSUFX           ".zip"

/* 9 and 10 not yet implemented */

#define PK_OK             0     /* no error */
#define PK_COOL           0     /* no error */
#define PK_GNARLY         0     /* no error */
#define PK_WARN           DZ_ERR_WARNING //1     /* warning error */

#define PK_ISWARN(e)   (DZ_ERR(e) == DZ_ERR_WARNING)
#define PK_ERR_NOWARN(e) (DZ_ERR(e) && DZ_ERR(e) != DZ_ERR_WARNING)

int __fastcall PK_Rank(int err);
#define PK_ERR            DZ_ERM_GENERAL  //2     /* error in zipfile */
#define PK_BADERR         DZ_ERM_INVAL_ZIP //3     /* severe error in zipfile */
// #define PK_MEM            4     /* insufficient memory */
// #define PK_MEM2           5     /* insufficient memory */
// #define PK_MEM3           6     /* insufficient memory */
// #define PK_MEM4           7     /* insufficient memory */
// #define PK_MEM5           8     /* insufficient memory */
#define PK_NOZIP         DZ_ERM_INVAL_ZIP // 9     /* zipfile not found */
// #define PK_PARAM         DZ_ERM_BAD_OPTIONS // 10    /* bad or illegal parameters specified */
#define PK_FIND         DZ_ERM_MISS //  11    /* no files found */
#define PK_DISKFULL      DZ_ERM_ERROR_WRITE // 50    /* disk full */
#define PK_EOF           DZ_ERM_ZIP_EOF // 51    /* unexpected EOF */
#define PK_DISK          DZ_ERM_ERROR_CREATE // 52    // disk error
#define PK_NODEL         DZ_ERM_ERROR_DELETE // 54    // could no delete old file

#define IZ_DIR            76    /* potential zipfile is a directory */
// #define IZ_CREATED_DIR    77    /* directory created: set time and permissions */
#define IZ_VOL_LABEL      78    /* volume label, but can't set on hard disk */
// #define IZ_EF_TRUNC       79    /* local extra field truncated (PKZIP'd) */
// #define IZ_SKIPPED        80    // 1.75 skipped file
#define IZ_CREATED_DIR    -2    /* directory created: set time and permissions */
#define IZ_SKIP_DIR      -3    // directory exists: nothing to do

/* return codes of password fetches (negative = user abort; positive = error) */
#define IZ_PW_ENTERED      0    /* got some password string; use/try it */
#define IZ_PW_CANCEL      -1    /* no password available (for this entry) */
#define IZ_PW_CANCELALL   -2    /* no password, skip any further pwd. request */
// #define IZ_PW_ERROR        5    /* = PK_MEM2 : failure (no mem, no tty, ...) */

#define DF_MDY            0     /* date format 10/26/91 (USA only) */
#define DF_DMY            1     /* date format 26/10/91 (most of the world) */
#define DF_YMD            2     /* date format 91/10/26 (a few countries) */

class UnzFileSpec
{
	TCHAR *fFileSpec;
	DZStrW fExcludes;
	const char *fPassword; // pointer to list
public:
	const BaseRec *Base;  // pointer to list
	unsigned Hash;
	inline const TCHAR* GetFileSpec(void)const
	{
		return(const TCHAR*)fFileSpec;
	}

	// void __fastcall SetFileSpec(const TCHAR *value);
	inline DZStrW GetSpec(void)const
	{
		return DZStrW(fFileSpec);
	}
	void __fastcall SetSpec(const DZStrW& value);

	inline const char* GetPassword(void)const
	{
		return(const char*)fPassword;
	}

	inline DZStrA GetPassw(void)const
	{
		return DZStrA(fPassword);
	}
	void __fastcall SetPassw(const DZStrA& value);

	inline DZStrW GetExtBase(void)const
	{
		return DZStrW(Base->Base);
	}

	inline DZStrW& GetExcludes(void)
	{
		return fExcludes;
	}
	inline void SetExcludes(const DZStrW& value)
	{
		fExcludes = value;
    }
	inline void SetPassword(const char* value)
	{
		fPassword = value;
	}

public:
	long Match;
	// used by extract for searches
	UnzFileSpec();
	~UnzFileSpec();
//	__property const TCHAR *FileSpec =
//	{
//		read = GetFileSpec
//	}; // ,write=SetFileSpec};
//	__property DZStrW Spec =
//	{
//		read = GetSpec, write = SetSpec
//	};
//	__property const char *Password =
//	{
//		read = GetPassword, write = fPassword
//	};
//	__property DZStrA Passw =
//	{
//		read = GetPassw, write = SetPassw
//	};
//	__property const BaseRec* Base =
//	{
//		read = fBase, write = fBase
//	};
//	__property DZStrW ExtBase =
//	{
//		read = GetExtBase
//	};
//	__property unsigned Hash =
//	{
//		read = fHash
//	};
//	__property DZStrW Excludes =
//	{
//		read = GetExcludes, write = SetExcludes
//	};
};

class CHdrInfo
{
private:
	char* fhname; // header name field
	const TCHAR* fxname;
public:
	void __fastcall Setxname(const TCHAR *value);

	inline DZStrW GetXName(void)const
	{
		return DZStrW(fxname);
	}

	inline void __fastcall SetXName(const DZStrW& value)
	{
		Setxname(value.c_str());
	}

	inline const char* Gethname(void)const
	{
		return(const char*)fhname;
	}
	void __fastcall Sethname(const char* value);

	inline DZStrA GetHName(void)const
	{
		return DZStrA(fhname);
	}

	inline void __fastcall SetHName(const DZStrA& value)
	{
		Sethname(value.c_str());
	}
	bool __fastcall GetIsFolder(void)const;

public:
	ZInt64 offset;
	ZInt64 compr_size; // compressed size (needed if extended header)
	ZInt64 uncomp_size; // new - limit output size
	ulg crc; // crc (needed if extended header)
	unsigned file_attr;
	// local flavor, as used by creat(), chmod()...
	union
	{
		struct
		{
			// unsigned file_attr;           // local flavor, as used by creat(), chmod()...
			unsigned host : 16;
			// unsigned hostnum: 8;
			// unsigned hostver: 8;
			unsigned encrypted : 1; // file encrypted: decrypt before uncompressing
			unsigned ExtLocHdr : 1; // use time instead of CRC for decrypt check
			unsigned textfile : 1; // file is text (according to zip)
			unsigned textmode : 1; // file is to be extracted as text
			unsigned vollabel : 1; // "file" is an MS-DOS volume (disk) label
			unsigned UTF8flag : 1; // set if flag utf8 flag set
			// unsigned PKZflag: 1;          // possible made by PKZIP
			unsigned UNIXflag : 1; // possible made on unix
			// unsigned nouse: 11; //10;
		};
		unsigned long _flags;
	};

	unsigned chdrseq;
	// unsigned hash;                // hash of uppercase filename
	const XNTFSData *ntfs_data;
	UnzFileSpec *spec; // the spec line
	CHdrInfo *next;

	CHdrInfo();
	~CHdrInfo();
	void Clear(void);
	DZStrW __fastcall FullPath(void)const; // full path of name
//	__property const TCHAR *xname =
//	{
//		read = fxname, write = Setxname
//	};
//	__property DZStrW XName =
//	{
//		read = GetXName, write = SetXName
//	};
//	__property const char *hname =
//	{
//		read = Gethname, write = Sethname
//	};
//	__property DZStrA HName =
//	{
//		read = GetHName, write = SetHName
//	};
//	__property bool IsFolder =
//	{
//		read = GetIsFolder
//	};
};

class UnzSup : public DZOp
{
private:
	UnzSup(void);
	UnzSup(const UnzSup&);
	UnzSup& operator=(const UnzSup&);

protected:
	bool fredirect_data; // redirect data to memory buffer
	ZInt64 fMax_Write;		// maximum bytes to write
	unsigned f_wsize;
	uch *fredirect_pointer;
	uch *fredirect_buffer;
	unsigned fredirect_size;
	unsigned fbuffer_size;

	CHdrInfo *chdrList;
	CHdrInfo *fpInfo;
	int chdrCount;
	bool Encrypted; // current file setting

	ZInt64 fcsize; /* used by list_files(), NEXTBYTE: must be signed */
	ZInt64 fucsize; /* used by list_files(), unReduce(), explode() */
	ZInt64 fused_csize; /* used by extract_or_test_member(), explode() */
	ZInt64 fcur_zipfile_bufstart; /* extract_or_test, readbuf, ReadByte */

	int fincnt_leftover; /* so improved NEXTBYTE does not waste input */
	uch *finptr_leftover;
	uch *foutbufptr; /* extract.c static */
	ulg foutsize; /* extract.c static */
	int freported_backslash; /* extract.c static */

	ulg fcrc32val; /* CRC shift reg. (was static in funzip) */

	uch *finbuf; /* input buffer (any size is OK) */
	uch *finptr; /* pointer into input buffer */
	int fincnt;
	ulg fbitbuf;
	int fbits_left; /* unreduce and unshrink only */
	int fzipeof;
	DZStrW fzipfn;
	ZStreamIO *fUnzInfile; // zipfile
	ZStreamIO *fUnzOutfile; // stream being written
	ZInt64 fextra_bytes;
	Keys fkeys; /* crypt static: keys defining pseudo-random sequence */

	int fmem_mode;
	uch *foutbuf;
	uch *frealbuf;

	uch *foutbuf2; /* main() (never changes); else malloc'd */
	uch *foutptr;
	ulg foutcnt; /* number of chars stored in outbuf */
	int fnewfile;
	bool InProgress;

	char fTextLast; // last text convert buffer character
	int fsol; /* fileio static: at start of line */
	int fdisk_full;
	DZStrW ffilename;

	int readbyte(void);
	int part_flush(uch * rawbuf, ulg size, int unshrink);
	int flush(uch * rawbuf, ulg size, int unshrink);
	int vclRead(void * buf, unsigned LenToRead);
	int zlseek(ZInt64 abs_offset);
	void undefer_input(void);
	void defer_leftover_input(void);
	unsigned __fastcall readbuf(char * buf, unsigned size);

public:
	int fqflag; /* -q: produce a lot less output */
	int ftflag;
	/* -t: test (unzip) or totals line (zipinfo) */
	UnzSup(const DllCommands *C);
	~UnzSup(void);
	DZStrW __fastcall FullPath(const CHdrInfo* info)const;

private:
	int disk_error(void);
};

unsigned __fastcall HashFunc(const DZStrW & strng);

#endif
