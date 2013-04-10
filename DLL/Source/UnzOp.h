//---------------------------------------------------------------------------

#ifndef UnzOpH
#define UnzOpH
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
************************************************************************/

// #include "unzerr.h"
#include "dzraw.h"
#include "dzoper.h"
#include "unzsup.h"
#include "unzinf.h"

#define SKIP              0     /* choice of activities for do_string() */
#define DISPLAY           1
#define DISPL_8           5     /* Display file comment (ext. ASCII)    */
#define DS_FN             2
#define EXTRA_FIELD       3
#define DS_EF             3

#define DOES_NOT_EXIST    -1    /* return values for check_for_newer() */
#define EXISTS_AND_OLDER  0
#define EXISTS_AND_NEWER  1

#define ROOT              0     /* checkdir() extract-to path:  called once */
#define INIT              1     /* allocate buildpath:  called once per member */
#define APPEND_DIR        2     /* append a dir comp.:  many times per member */
#define APPEND_NAME       3     /* append actual filename:  once per member */
#define GETPATH           4     /* retrieve the complete path and free it */
#define END               5     /* free root path prior to exiting program */

//----------------------------------------------------------

/* version_made_by codes (central dir):  make sure these */

/*  are not defined on their respective systems!! */
//----------------------------------------------------------
#define FS_FAT_           0     /* filesystem used by MS-DOS, OS/2, Win32 */
#define AMIGA_            1
#define VMS_              2
#define UNIX_             3
#define VM_CMS_           4
#define ATARI_            5     /* what if it's a minix filesystem? [cjh]     */
#define FS_HPFS_          6     /* filesystem used by OS/2, NT                */
#define MAC_              7
#define Z_SYSTEM_         8
#define CPM_              9
#define TOPS20_           10
#define FS_NTFS_          11    /* filesystem used by Windows NT              */
#define QDOS_MAYBE_       12    /* a bit premature, but somebody once started */
#define ACORN_            13    /* Archimedes Acorn RISC OS                   */
#define FS_VFAT_          14    /* filesystem used by Windows 95, NT          */
#define MVS_              15
#define BEBOX_            16
#define TANDEM_           17    /* Tandem/NSK                                 */
#define NUM_HOSTS         18    /* index of last system + 1                   */

#define STORED            0     /* compression methods                        */
#define SHRUNK            1
#define REDUCED1          2
#define REDUCED2          3
#define REDUCED3          4
#define REDUCED4          5
#define IMPLODED          6
#define TOKENIZED         7
#define DEFLATED          8
#define ENHDEFLATED       9
#define DCLIMPLODED      10
//#define NUM_METHODS       9     /* index of last method + 1 */
//#define PKRESMOD11       11
//#define BZIPPED          12
//#define NUM_METHODS      13    /* index of last method + 1 */

/* don't forget to update //list_files() appropriately if NUM_METHODS changes */

/* 9 and 10 not yet implemented */

/* extra-field ID values, little-endian: */
#define EF_AV        0x0007     /* PKWARE's authenticity verification */
#define EF_OS2       0x0009     /* OS/2 extended attributes */
#define EF_PKVMS     0x000c     /* PKWARE's VMS */
#define EF_PKUNIX    0x000d     /* PKWARE's Unix */
#define EF_IZVMS     0x4d49     /* Info-ZIP's VMS ("IM") */
#define EF_IZUNIX    0x5855     /* Info-ZIP's old Unix[1] ("UX") */
#define EF_IZUNIX2   0x7855     /* Info-ZIP's new Unix[2] ("Ux") */
#define EF_TIME      0x5455     /* universal timestamp ("UT") */
#define EF_JLMAC     0x07c8     /* Johnny Lee's old Macintosh (= 1992) */
#define EF_ZIPIT     0x2605     /* Thomas Brown's Macintosh (ZipIt) */
#define EF_VMCMS     0x4704     /* Info-ZIP's VM/CMS ("\004G") */
#define EF_MVS       0x470f     /* Info-ZIP's MVS ("\017G") */
#define EF_ACL       0x4c41     /* (OS/2) access control list ("AL") */
#define EF_NTSD      0x4453     /* NT security descriptor ("SD") */
#define EF_BEOS      0x6542     /* BeOS ("Be") */
#define EF_QDOS      0xfb4a     /* SMS/QDOS ("J\373") */
#define EF_AOSVS     0x5356     /* AOS/VS ("VS") */
#define EF_SPARK     0x4341     /* David Pilling's Acorn/SparkFS ("AC") */
#define EF_MD5       0x4b46     /* Fred Kantor's MD5 ("FK") */
#define EF_ASIUNIX   0x756e     /* ASi's U */

#define EB_HEADSIZE       4     /* length of extra field block header */
#define EB_ID             0     /* offset of block ID in header */
#define EB_LEN            2     /* offset of data length field in header */
#define EB_UCSIZE_P       0     /* offset of ucsize field in compr. data */
#define EB_CMPRHEADLEN    6     /* lenght of compression header */

#define EB_UX_MINLEN      8     /* minimal "UX" field contains atime, mtime */
#define EB_UX_FULLSIZE    12    /* full "UX" field (atime, mtime, uid, gid) */
#define EB_UX_ATIME       0     /* offset of atime in "UX" extra field data */
#define EB_UX_MTIME       4     /* offset of mtime in "UX" extra field data */
#define EB_UX_UID         8     /* byte offset of UID in "UX" field data */
#define EB_UX_GID         10    /* byte offset of GID in "UX" field data */
                                               
/*---------------------------------------------------------------------------
    True sizes of the various headers, as defined by PKWARE--so it is not
    likely that these will ever change.  But if they do, make sure both these
    defines AND the typedefs below get updated accordingly.
  ---------------------------------------------------------------------------*/
#define LREC_SIZE   26          /* lengths of local file headers, central */
#define CREC_SIZE   42          /*  directory headers, and the end-of-    */
#define ECREC_SIZE  18          /*  central-dir record, respectively      */
#define ECREC64_SIZE 56

// #define MAX_BITS    13          /* used in unshrink() */
// #define HSIZE       (1 << MAX_BITS)     /* size of global work area */

#define LF     10               /* '\n' on ASCII machines; must be 10 due to EBCDIC */
#define CR     13               /* '\r' on ASCII machines; must be 13 due to EBCDIC */
#define CTRLZ  26               /* DOS & OS/2 EOF marker (used in fileio.c, vms.c) */

#define ENV_UNZIP     "UNZIP"   /* the standard names */
#define ENV_ZIPINFO   "ZIPINFO"

#ifndef S_ISDIR
#ifdef CMS_MVS
#    define S_ISDIR(m)  (false)
#else
#    define S_ISDIR(m)  (((m) & S_IFMT) == S_IFDIR)
#endif
#endif

#ifndef IS_VOLID
#  define IS_VOLID(m)  ((m) & 0x08)
#endif

#define makeword(p) ((ush)*(ush *)(p))
#define makelong(p) ((ulg)*(ulg*)(p))
#define makeint64(p) ((ZInt64)*(ZInt64*)(p))
/**************/

/*  Typedefs  */

/**************/

typedef uch ec_byte_rec[ECREC_SIZE + 4];

/*     define SIGNATURE                         0   space-holder only */
#      define NUMBER_THIS_DISK                  4
#      define NUM_DISK_WITH_START_CENTRAL_DIR   6
#      define NUM_ENTRIES_CENTRL_DIR_THS_DISK   8
#      define TOTAL_ENTRIES_CENTRAL_DIR         10
#      define SIZE_CENTRAL_DIRECTORY            12
#      define OFFSET_START_CENTRAL_DIRECTORY    16
#      define ZIPFILE_COMMENT_LENGTH            20

#pragma pack(push, 4)
typedef struct end_central_dir_record
{
    /* END CENTRAL */
	ulg number_this_disk;
	ulg num_disk_start_cdir;
	ZInt64 num_entries_centrl_dir_ths_disk;
	ZInt64 total_entries_central_dir;
	ZInt64 size_central_directory;
    ZInt64 offset_start_central_directory;
    int is_zip64_archive;
    ush zipfile_comment_length;
}ecdir_rec;
#pragma pack(pop)

#pragma pack(push, 1)
typedef struct central_directory_file_header
{
	ush made_by;
    uch version_needed_to_extract[2];
    ush general_purpose_bit_flag;
    ush compression_method;
    ush last_mod_file_time;
    ush last_mod_file_date;
	ulg crc32;
    ush filename_length;
    ush extra_field_length;
    ush file_comment_length;
    ulg disk_number_start;
    ush internal_file_attributes;
	ulg external_file_attributes;
    ZInt64 csize;
    ZInt64 ucsize;
    ZInt64 relative_offset_local_header;
}
cdir_file_hdr;
#pragma pack(pop)

/*************/

class UnzOpr : public UnzInf
{
    protected:
        int fdflag;                    /* -d: all args are files/dirs to be extracted */
        int ffflag;                    /* -f: "freshen" (extract only newer files) */
        int fjflag;                    /* -j: junk pathnames (unzip) */
        int flflag;                    /* -12slmv: listing format (zipinfo)*/
        int foverwrite_all;            /* -o combined with -n: Ok to overwrite used as default */
        int fvolflag;                  /* -$: extract volume labels */
        int fT_flag;                   /* -T: timestamps (unzip) or dec. time fmt (zipinfo) */
        int fuflag;                    /* -u: "update" (extract only newer/brand-new files) */
        int fzflag;                    /* -z: display the zipfile comment (only, for unzip) */

        int ffilespecs;                /* number of real file specifications to be matched */
        UnzFileSpec *SrchSpecs;        // files to extract
        int fprocess_all_files;
        int fcreate_dirs;              /* used by main(), mapname(), checkdir() */
        int fextract_flag;
        ZInt64 freal_ecrec_offset;
        ZInt64 fexpect_ecrec_offset;
		ZInt64 fziplen;
        DZRawData extra_field;
        DZStrA file_name;
        uch *fhold;
        cdir_file_hdr fcrec;           /* used in unzip.c, extract.c, misc.c */
        ecdir_rec fecrec;              /* used in unzip.c, extract.c */

        struct stati64 fstatbuf;          /* used by main, mapname, check_for_newer */
        int fno_ecrec;                 /* process static */
#ifdef NOVELL_BUG_FAILSAFE
        int fdne;                      /* true if stat() says file doesn't exist */
#endif
        int fcreated_dir;
        int frenamed_fullpath;
        int ffnlen;
        unsigned fnLabelDrive;    
        DZStrW fendHPFS;
        DZStrW fendFAT;
        DZStrW fdirname;
        DZStrW fwildname;
        DZStrW fmatchname;
        int frootlen;
        int fhave_dirname;
        int fdirnamelen;
        int fnotfirstcall;
        void *fwild_dir;

        int ffiles_acted_on;
        DZStrW frootpath;
        DZStrW fTempPath;
//#ifdef CRYPT                    /* If no crypt don't use */
		char flpszPassword[PWLEN + 2]; /* Used in PassMsg.c                                                                                             */
		WORD fcchPassword;
		bool fpwork;
		int frcode;
//#endif

    public:
        UnzOpr(const DllCommands *C);
        ~UnzOpr(void);
        long Exec(const DllCommands *C);
        int FatalError(int c);
        int Init(void);

    protected:
        DZStrW UnzMsg;
        DZStrW PrevPath;      // avoid checking path again
        int mapattr(void);
        void close_outfile(void);
        DZStrW GetFullPath(const DZStrW& p);
        int mapname(int renamed);
        int open_input_file(void);
        int open_outfile(void);
        int check_for_newer(const DZStrW& filename);
        int AskOverwrite(const DZStrW& filename, int cond); // true = overwrite
        int do_string(unsigned int len, int option);
		void UnzErr(int c); // probably FatalError
		int __fastcall getZip64Data(const DZRawData& extra);//const uch *ef_buf, unsigned ef_len);
        void process_zipfiles(void);
        int extract_or_test_files(void);
        int process_cdir_file_hdr(void);
        int get_cdir_ent(void);
        int process_local_file_hdr(void);
        int uz_end_central(void);
		int SSExtract(void);
        bool FSetUpToProcessZipFile(const DllCommands *C);
        void TakeDownFromProcessZipFile(void);
        int UnzStreamStream(void);

    private:
        int getNTfiletime(FILETIME * ft);
        int do_seekable(int lastchance);
        int find_ecrec(ZInt64 searchlen);
        int rec_find(ZInt64, ulg, int);
        int check_ecrec_zip64(void);
        int find_ecrec64(ZInt64 searchlen);
        int StoreInfo(void);
        int extract_or_test_member(void);
        int SelectMembers(void);
		void CloseOut(void);
        int CD_Init(const DZStrW& pathcomp, bool renamed_fullpath, DZStrW build);
        int CD_AppendDir(const DZStrW& comp, DZStrW build);
        int CD_AppendName(const DZStrW& comp, DZStrW build);

        void GiveGlobals(void);
};

#endif


