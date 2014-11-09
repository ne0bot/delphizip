//---------------------------------------------------------------------------

#ifndef ZipFncH
#define ZipFncH
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

#include "common.h"
#include "DZRaw.h"
#include "dzoper.h"
#include "ZipDflt.h"

/* Lengths of headers after signatures in bytes */
#define LOCHEAD 26
#define CENHEAD 42
#define ENDHEAD 18

#define OS_NTFS 0x0B00

#include "Crypt.h"

// Local option flags
#define PURGE   0 // RCV Changed: was DELETE. (delete is also a function)
#define ADD     1
#define UPDATE  2
#define FRESHEN 3

//#define ZFT_DEVICE (-1)
#define ZFT_LABEL (-2)

#pragma pack(push, 4)
union FOpts
{
    struct
    {
        unsigned dosflag: 2;  // nz forces dos
        unsigned level:   4;
        unsigned noext:   1;
        unsigned keepver: 1;    // version was active
        unsigned enc:       3;  // do encoded
        unsigned copyold:   1;  // copy from old zip
		unsigned needver:   1;  // need unique version name
        unsigned needutf8:  1;  // need utf8 name
        unsigned nameutf8:  1;  // name is utf8
		unsigned nameextd:  1;  // has extended chars
		unsigned cmntextd:  1;  // has extended chars
		unsigned namenew:   1;  // name has been changed
		unsigned nameuser:  1;  // user changed name
    };
    unsigned long _opts;      // make copying/clearing easier
};// FOpts;

// hname = zname = name in header (8 bit char)
// iname = internal name
// xname = xname = external name of found file
// common fields
class XItem //: public ZIName
{
protected:
    char* fhname;        // header name field
//	const TCHAR* fxname;
    const TCHAR* finame;
public:
	const TCHAR* xname;
    bool __fastcall GetIsFolder(void) const;
    inline int  __fastcall GetEnc(void) const {return (int)options.enc;}
    void __fastcall SetEnc(int value) {options.enc = (unsigned)value & 7;}
    void __fastcall Setiname(const TCHAR *value);
    void __fastcall Setxname(const TCHAR *value);
	inline DZStrW GetXName(void) const {return DZStrW(xname);}
	inline const TCHAR* Getiname(void) const {return finame;}
    inline void __fastcall SetXName(const DZStrW& value) {Setxname(value.c_str());}
    inline DZStrW GetIName(void) const {return DZStrW(finame);}
    inline void __fastcall SetIName(const DZStrW& value) {Setiname(value.c_str());}
    inline const char* Gethname(void) const {return (const char*)fhname;}  
    void __fastcall Sethname(const char* value);  
    inline DZStrA GetHName(void) const {return DZStrA(fhname);}
    inline void __fastcall SetHName(const DZStrA& value) {Sethname(value.c_str());}
public:
    FOpts       options;
    const TCHAR *Base;
    const char  *Passw;     //* Password if set on a per file base -P switch added R.Aelbrecht */
    ZInt64        len;           // uncompressed Size of the file. RCV Added
    XItem();
    XItem(const XItem& other);
    virtual ~XItem();
    XItem& operator=(const XItem& other);
    DZStrW __fastcall FullPath(void) const;  // full path of name
//    __property const TCHAR *xname = {read=fxname, write=Setxname};
//    __property DZStrW XName = {read=GetXName, write=SetXName};
//	__property const TCHAR *iname = {read=finame, write=Setiname};
//    __property DZStrW IName = {read=GetIName, write=SetIName};
//    __property const char *hname = {read=Gethname, write=Sethname};
//    __property DZStrA HName = {read=GetHName, write=SetHName};
//    __property int Enc = {read=GetEnc, write=SetEnc};
//    __property bool IsFolder = {read=GetIsFolder};
};

// found files
class FndItem : public XItem
{
public:
    FndItem  *nxt;       //* Link to next name
    FndItem();
    FndItem(const XItem& other);
	~FndItem();
};

// as read or for processing
class ZipItem : public XItem
{
private:
	char* fhname;        // header name field
	wchar_t* fcomment;
public:
	XNTFSData* ntfs;
	DZRawData extra;
	DZRawData cextra;
    inline WORD GetExt(void) const {return (WORD)extra.Length();}
    inline WORD GetCext(void) const {return (WORD)cextra.Length();}
    inline void SetExt(WORD value) {extra.SetLength(value);}
    inline void SetCext(WORD value) {cextra.SetLength(value);}
    inline DZStrW GetComment(void) const {return DZStrW(fcomment);}
	void __fastcall SetComment(const DZStrW& value);
protected:
	void __fastcall Copy(const ZipItem& other);
public:
//#pragma pack(push, 2)
	ush           vem,      // version made
    ver,                    // version required
    flg,                    // general bit flag
    how;                    // method
	::extent nam,           // length zname
//    ext,                  // length local header extra data
//    cext,                 // length central header extra data
    com;                    // length comment
    ush           dsk,      // disk number of local header
    att,
    lflg;                   // local header flag
//#pragma pack(pop)
    ulg           atx;
    ulg           tim,        // file time (dos)
    crc;                    // crc
    ZInt64        siz;        // compressed
    ZInt64        off;        // offset local header
    int           mark;       // Marker for files to operate on
	int           trash;      // Marker for files to delete
//	__property DZRawData extra = {read=fextra, write=fextra};
//	__property DZRawData cextra = {read=fcextra, write=fcextra};
//	__property XNTFSData* ntfs = {read=fntfs, write=fntfs};
//	__property DZStrW Comment = {read=GetComment, write=SetComment};
//	__property WORD ext = {read =GetExt, write=SetExt};
//    __property WORD cext = {read =GetCext, write=SetCext};
    ZipItem  *nxt;       // Pointer to next header in list
    ZipItem();
    ZipItem(const FndItem* f);
    ZipItem(const ZipItem& other);
	ZipItem& operator=(const ZipItem& other);
    ~ZipItem();
};
#pragma pack(pop)

// node used for duplicate checks
class HL_node
{
public:
    /*struct*/ HL_node *nxt;
    ulg hash;
    const XItem *xdata;
    HL_node();//:nxt(NULL),hash(0),xdata(NULL){};
};

#define NODE_BLOCK_SIZE 5000
class HL_block
{
private:
    HL_block(void);
    HL_block(const HL_block&);
    HL_block& operator=(const HL_block&);
public:
    HL_node nodes[NODE_BLOCK_SIZE];
    HL_block *prv;   // link for removal
    HL_block(HL_block *prev);
    ~HL_block(void);
};


class HashList
{
private:
    HashList(void);
    HashList(const HashList&);
    HashList& operator=(const HashList&);
public:
    HashList(int siz);
    virtual ~HashList(void);
protected:
    HL_node **Table;
    HL_block *blocks;
    int nno;
    unsigned Mask;
    unsigned Hash;
    unsigned Index;
    HL_node * NewNode(const XItem *x, ulg hash);
    const XItem *AddANode(const XItem *xname);
    XItem *FindAName(void);
    virtual bool Matches(const HL_node *node) const = 0;
    virtual HL_node* Prepare(void) = 0;
};

class HashListExt : public HashList
{
private:
    DZStrW fxname;
    HashListExt(void);
    HashListExt(const HashListExt&);
    HashListExt& operator=(const HashList&);
public:
    HashListExt(int siz): HashList(siz){;}
    ~HashListExt(void);                    
    XItem *FindName(const DZStrW& name);
    const XItem *AddNode(const XItem *xname);
protected:
    bool Matches(const HL_node *node) const;
    HL_node* Prepare(void);
};

class HashListInt : public HashList
{
private:
    HashListInt(void);
    HashListInt(const HashList&);
    HashListInt& operator=(const HashList&);
protected:
    DZStrW finame;
    bool Matches(const HL_node *node) const;
    HL_node* Prepare(void);
public:
    HashListInt(int siz): HashList(siz){;}
    ~HashListInt(void);
    XItem *FindName(const DZStrW& name);
    const XItem *AddNode(const XItem *xname);
};

/*************/
/*  ZGlobals  */
/*************/

class ZipFunc : public ZipDflt
{
private:
    ZipFunc(void);
    ZipFunc(const ZipFunc&);
    ZipFunc& operator=(const ZipFunc&);
protected:
    int fglobal_error_code; // in UnzErr() in dllmain
    int ffiles_acted_on;
    unsigned fEncodeAs;
    char ffnamebuf[MAX_PATH + 1];
    DZStrW fRootDir;
    DZStrA fzcomment;
    DZStrW ftempath;
    DZStrW ftempzip;
    DZStrA fGPassword;
    DZStrW fStartCDir;
    const char* fuser_key; // user entered key or NULL RP 1.73
    int fzcomlen;
    int fResetArchiveBit;
    int fvolume_label;
    bool fHowToMove;
    DWORD fStrFileAttr;
    DWORD fStrFileDate;   
    DZStrW fExcludes;
    ZFilter *fSpecials;
    int fpcount;
    FndItem *ffound;
    FndItem **ffnxt;
    ZipItem *fzfiles;
    ZipItem *fzfound;   // list of prepared found
    ::extent fzcount;
    ZipItem* VerFiles;
    int flatest;
    int fNoExtChk;// method;
	int fadjust;
    int faction;
    int fversion;
    int fdirnames;
//    int fpathput;
	int fjunk_sfx;
    int frecurse;
    ulg fbefore;
    int flinkput;
    int ffcount;
    int fNoPrecalc;
    int fGEncrypt;
    ulg VerDate;

    DZStrW flabel;
    int fzipstate;
    struct stati64 fzipstatb;
    ulg flabel_time;
    ulg flabel_mode;
    time_t flabel_utim;

    ZInt64 fOutPosn; //tempzn;
    ZInt64 fcenbeg;
    ZInt64 fzipbeg;
    int fAllowGrow;                // new 1.73
    int fCallback_Exception;       // new 1.73.2.6
    int fBatchStarted;             // new 1.73.4.2
    DZStrW fFullPath;               // 1,74
    int fdoall;                    // new 178.5.0
    uch *fhwbuf;                  // allocated buffer
	int  fhwsize;                  // size of buffer
    int  fhwcnt;                   // current byte count
    HashListInt *fndlist;           // existing files
	HashListInt *IntList;           // internal names
public:
    int fArchiveFiles;
    ZipFunc(const DllCommands *C);
    ~ZipFunc(void);
    void ZipCleanup(void);        //ZipSel            
    int Init(void); // after construction
protected:
    int     ZipProcess(void);
    DZStrW __fastcall     CleanedPath(const DZStrW &pth);
	ZipItem    *zsearch(const DZStrW &name);
	int     trash(void);
	DZStrW tempname(void);
private:
    int  LocXData;              // offset to local header extra data (for redo)
	void    finish(void);                      //ZipSel
    int     replace(const DZStrW &oldname, const DZStrW &newname);
    int     replaceOrig(const DZStrW &d, const DZStrW &s);
	int     TrashDir(const DZStrW &dir);
    int   check_dupExt(void);
    int zipup(ZipItem *);
    int zipVersion(ZipItem *);
    int __fastcall SetVers(ZipItem* z, int mthd);

    int __fastcall  PutLocal(ZipItem *);
    int __fastcall  UpdateLocal(ZipItem *);
    int __fastcall  putextended(ZipItem *);
    int __fastcall  putcentral(ZipItem *);
    int __fastcall  zipcopy(ZipItem *);
    int __fastcall  PutEnd(unsigned CentralCnt, __int64 CentralOffs);
	int     fcopy(ZInt64);
//	ulg     zfiletime(const DZStrW &name, ulg *, __int64 *, ztimbuf *);
	BOOL     zfiletime(const DZStrW &name, ulg *attr, __int64 *size, ulg *stamp);
	int     filecopy(HANDLE f, HANDLE g);
    void    HWriteInit(void);
    int     HWrite(const void *arg, int siz);
	char *  ChangeBWSlash(char *fn);
    int     __fastcall NameVer(ZipItem* z);
    void DupName(bool fatal, const XItem* o, const XItem* n, const DZStrW name);
    int __fastcall PrepareHeaderName(ZipItem* z, bool NoComment);
}; /* end of struct ZGlobals */

void  stamp(const DZStrW &name, ulg);
//ulg     FileTime2DosTime(_FILETIME ftime);
int     NtfsFileTime2utime(const FILETIME *pft, time_t *ut);
int     percent(__int64 n, __int64 m);
int     setfileattr(const DZStrW&, int);

int __fastcall SameNameExt(const DZStrW &fname, const DZStrW &oname);

DZStrW last(const DZStrW &s, int);
int __fastcall IsShExp(const DZStrW &p);

#endif



