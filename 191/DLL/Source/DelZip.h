#ifndef DelZip_H
#define DelZip_H
/*
 delzip.h -

************************************************************************
 Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht,
      Eric W. Engler and Chris Vleghert.

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

// revised order
enum ActionCodes
{
    zacTick, zacItem, zacProgress, zacEndOfBatch,
    zacCount, zacSize, zacXItem, zacXProgress, zacMessage,
    zacNewName, zacPassword, zacCRCError, zacOverwrite, zacSkipped,
    zacComment, zacData, zacExtName, zacNone,
    zacKey, zacArg, zacWinErr
};
            
enum WinOps
{   zwoCreate, zwoOpen, zwoRead, zwoWrite, zwoSeek, zwoClose,
    zwoFind, zwoRename, zwoDelete, zwoGetAttrs, zwoSetAttrs, zwoGetDate,
    zwoSetDate, zwoMakeDir, zwoRemoveDir, zwoChangeDir
};    

enum StreamActions
{
    zsaIdentify, zsaCreate, zsaClose, zsaPosition, zsaRead, zsaWrite
};


enum  CBArgs
{
    zcbFilename, zcbPassword, zcbRootDir, zcbExtractDir, zcbComment,
    zcbFSpecArgs, zcbFSpecArgsExcl, zcbSpecials, zcbTempPath
};

#pragma pack(push, 1)
struct ZSStats
{
    __int64 Size;
    DWORD   Date;
    DWORD   Attrs;
};
#pragma pack(pop)

/*
the EncodeAs values (writing) -
 seoUPATH - convert to 'safe' but have UTF8 proper name in data
                                         [FAT 30 need 20 | NTFS 30 need 30]
 zeoUTF  - convert to UTF8 [FAT 20 need 20 | FAT 30? need 30?]
 zeoOEM  - convert to 'safe' (substitute '_' for undesirables) [FAT 20 need 20]
 zeoNone - store 'as is' (Ansi on Windows) [FAT 20 need 20 | NTFS 20 need 20]
 zeoAuto - zeoUTF

[without extended | with extended] version made, version need

'Safe' is 0x20 to 0x7e without 'reserved'. 
Encoded (reading)
 zeoUPATH - assume has UTF8 name in data
 zeoUTF  - assume name is UTF8 - convert to Ansi/Unicode
 zeoOEM  - assume name is OEM - convert to Ansi/Unicode
 zeoNone - assume name is Ansi - convert to Ansi/Unicode
 zeoAuto - unless flags/versions say otherwise, or it has UTF8 name in data,
             treat it as OEM (FAT) / Ansi (NTFS)
*/
enum Encodings
{
    zeoAuto, zeoNone, zeoOEM, zeoUTF8, zeoUPATH
};

// yy::Extract=01, Add=02, either=03  zz::skip reason  yyzz
#define SKIPPED_ON_FRESHEN                              0x0101
#define SKIPPED_NO_OVERWRITE                            0x0102
#define SKIPPED_FILE_EXISTS                             0x0103
#define SKIPPED_BAD_PASSWORD                            0x0104
//#define SKIPPED_NO_ENCRYPTION                           0x0105
#define SKIPPED_BAD_NAME                                0x0105
#define SKIPPED_COMPRESSION_UNKNOWN                     0x0106
#define SKIPPED_UNKNOWN_ZIPHOST                         0x0107
#define SKIPPED_FILEFORMAT_WRONG                        0x0108
#define SKIPPED_EXTRACT_ERROR                           0x0109
#define SKIPPED_USER                                    0x010A
#define SKIPPED_CANNOTDO                                0x010B
#define SKIPPED_NO_FILES                                0x030C
#define SKIPPED_NO_SHARE                                0x020D
#define SKIPPED_NO_ACCESS                               0x020E
#define SKIPPED_NO_OPEN                                 0x020F
#define SKIPPED_DUP_NAME								0x0210
#define SKIPPED_READ_ERROR								0x0211
#define SKIPPED_SIZE_CHANGE								0x0212

//Callbackstructs-------------------------------------------------------------

/* All the items in the CallBackStruct are passed to the BCB
 * program from the DLL.  Note that the "Caller" value returned
 * here is the same one specified earlier in ZipParms by the
 * BCB program. */

#define CALLBACK_CHECK  0x0707
#define STREAM_CHECK 0x070B
//ALL interface structures BYTE ALIGNED
/* stream operation arg usage
   zacStIdentify,
      IN MsgP = name
     OUT FileSize = size, Arg1 = Date, Arg2 = Attrs
   zacStCreate,
      IN MsgP = name
     OUT StrmP = stream
   zacStClose,
      IN StrmP = stream
     OUT StrmP = stream (= NULL)
   zacStPosition,
      IN StrmP = stream, FileSize = offset, Arg3 = from
     OUT FileSize = position
   zacStRead,
      IN StrmP = stream, MsgP = buf, Arg3 = count
     OUT Arg3 = bytes read
   zacStWrite
      IN StrmP = stream, MsgP = buf, Arg3 = count
     OUT Arg3 = bytes written
*/
#pragma pack(push, 1)
struct ZCallBackStruct
{
    void  *Caller;          // 'this' reference of the ZipBuilder class
    long   Version;         // version no. of DLL.
    BOOL   IsOperationZip;  // true=zip, false=unzip
    int    ActionCode;      // returns <0 if result valid
    int    HaveWide;        // wide string passed
    const void *MsgP;       // pointer to text/data
    const void *MsgP2;      // additional text
    __int64 FileSize;       // file size
    __int64 Written;        // number of bytes written
    long   Arg1;            // ErrorCode;
    unsigned Arg2;          // additional arg
    int Arg3;               // 'older'
    unsigned check;
};
#pragma pack(pop)

typedef struct ZCallBackStruct CB_Rec;

typedef struct ZCallBackStruct *PZCallBackStruct;
/* Declare a function pointer type for the BCB/Delphi callback function, to
 * be called by the DLL to pass updated status info back to BCB/Delphi.*/

typedef int(__stdcall *ZFunctionPtrType)(PZCallBackStruct ZCallbackRec);

#pragma pack(push, 1)
typedef struct
{
    DWORD  Method;   // low word = method, hi word nz=encrypt
    DWORD CRC;       // IN init encrypt crc OUT crc
    __int64 Size;
    void  *fSSInput;
    void  *fSSOutput;
}ZSSArgs;      // used stream-stream
#pragma pack(pop)

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
#pragma pack(push, 1)
struct ZStreamRec
{
    void *Caller;           // "self" reference of the Delphi form }
    int Version;            // version no. of DLL }
    void *StrmP;            // pointer to 'tstream'
    int  Number;            // stream number
    int  OpCode;            // TZStreamActions
    unsigned char *BufP;    // pointer to stream src/dst
    __int64  ArgLL;         // file size or stream position offset
    int  ArgI;              // stream cnt or from
    unsigned ArgD;          // date
    unsigned ArgA;          // attribs
    unsigned Check;         // ZStream_Check;
};
#pragma pack(pop)

typedef struct ZStreamRec ZS_Rec;

typedef struct ZStreamRec *PZStreamRec;
/* Declare a function pointer type for the BCB/Delphi callback function, to
 * be called by the DLL to pass updated status info back to BCB/Delphi.*/

typedef int(__stdcall *ZStreamFuncPtrType)(PZStreamRec StreamRec);

// callback error return values
//#define  CALLBACK_FALSE      0
#define  CALLBACK_UNHANDLED  0
#define  CALLBACK_TRUE       1
#define  CALLBACK_2    2
#define  CALLBACK_3    3
#define  CALLBACK_4    4
#define  CALLBACK_IGNORED   -1  // unhandled ActionCode
#define  CALLBACK_CANCEL    -2  // user cancel
#define  CALLBACK_ABORT     -3
#define  CALLBACK_EXCEPTION -4  // handled exception
#define  CALLBACK_ERROR     -5  // unknown error

#define  ZPasswordFollows _T('<')
#define  ZSwitchFollows _T('|')
#define  ZForceNoRecurse _T('|')
#define  ZForceRecurse _T('>')

const unsigned int DLL_OPT_OpIsZip           = 0x0000001;
const unsigned int DLL_OPT_OpIsDelete        = 0x0000002; // delete - not used?
const unsigned int DLL_OPT_OpIsUnz           = 0x0000004;
const unsigned int DLL_OPT_OpIsTest          = 0x0000008;
/*
const DLL_OPT_CanWide           = 0x0000010;
const DLL_OPT_Quiet             = 0x0000020;
const DLL_OPT_NoSkip            = 0x0000040;
const DLL_OPT_Update            = 0x0000080;
const DLL_OPT_Freshen           = 0x0000100;
const DLL_OPT_Directories       = 0x0000200; // extract directories
const DLL_OPT_Overwrite         = 0x0000400; // overwrite all
const DLL_OPT_NoDirEntries      = 0x0000800;
const DLL_OPT_JunkDir           = 0x0001000;
const DLL_OPT_Recurse           = 0x0002000;
const DLL_OPT_Grow              = 0x0004000;
const DLL_OPT_Force             = 0x0008000; // Force to DOS 8.3 *** not used
const DLL_OPT_Move              = 0x0010000;
const DLL_OPT_System            = 0x0020000;
const DLL_OPT_JunkSFX           = 0x0040000; // remove sfx stub
const DLL_OPT_LatestTime        = 0x0080000; // set zip to latest file
const DLL_OPT_ArchiveFilesOnly  = 0x0100000; // zip when archive bit set
const DLL_OPT_ResetArchiveBit   = 0x0200000; // reset the archive bit after successfull zip
const DLL_OPT_Versioning        = 0x0400000; // rename old version instead of replace
const DLL_OPT_HowToMove         = 0x0800000;
const DLL_OPT_NoPrecalc         = 0x1000000; // don't precalc crc when encrypt
const DLL_OPT_Encrypt           = 0x2000000; // General encrypt, if not superseded
const DLL_OPT_Volume            = 0x4000000;
const DLL_OPT_NTFSStamps        = 0x8000000; // Generate or use NTFS time stamps
//const DLL_OPT_NoSkip            = 0x10000000; // Generate or use NTFS time stamps
*/
struct DllOptions
{
  union
  {
      unsigned opts;
      struct
      {
        unsigned OpIsZip          :1;
        unsigned OpIsDelete       :1; // delete - not used?
        unsigned OpIsUnz          :1;
        unsigned OpIsTest         :1;
        unsigned CanWide          :1;
        unsigned Quiet            :1;
        unsigned NotUsedNoSkip    :1;
        unsigned Update           :1;
        unsigned Freshen          :1;
        unsigned Directories      :1; // extract directories
        unsigned Overwrite        :1; // overwrite all
        unsigned NoDirEntries     :1;
        unsigned JunkDir          :1;
        unsigned Recurse          :1;
        unsigned Grow             :1;
        unsigned Forcexx            :1; // Force to DOS 8.3
        unsigned Move             :1;
        unsigned System           :1;
        unsigned JunkSFX          :1; // remove sfx stub
        unsigned LatestTime       :1; // set zip to latest file
        unsigned ArchiveFilesOnly :1; // zip when archive bit set
        unsigned ResetArchiveBit  :1; // reset the archive bit after successfull zip
        unsigned Versioning       :1; // rename old version instead of replace
        unsigned HowToMove        :1;
        unsigned NoPrecalc        :1; // don't precalc crc when encrypt
        unsigned Encrypt          :1; // General encrypt, if not superseded
        unsigned Volume           :1;
        unsigned NTFSStamps       :1; // generate or use NTFS time stamps
      };
  };
};
#pragma pack(push, 1)
typedef struct
{
    HWND                        fHandle;        // *
    void                        *fCaller;       // *
    long                        fVersion;       // *
    ZFunctionPtrType            ZCallbackFunc;  // *
    ZStreamFuncPtrType          ZStreamFunc;    // *
    int                         fVerbosity;     // ?
    unsigned                    fEncodedAs;     // Assume names encoded as
    ZSSArgs                     *fSS;           // used stream-stream
    unsigned long               fFromPage;      // country to use
    DllOptions   			    fOptions;   // DLL_OPT_?
    // PasswordRequestCount, How many times a password will be asked per file
    unsigned long               fPwdReqCount;
    unsigned long               fEncodeAs;    // encode names as
    int                         fLevel;
    // General Date, if not superseded by FileData.fDate
    unsigned long               fDate;
    long                        fNotUsed[4];
    long                        fCheck;
}DllCommands;
#pragma pack(pop)           
#define DLLCOMMANDCHECK 0x03070505

#define OUR_VEM 30
#define Def_VER 20

#ifdef __cplusplus
extern "C"
{
#endif
    extern BOOL WINAPI  DllEntryPoint(HINSTANCE hinstDll, DWORD fdwRreason,
                                          LPVOID plvReserved);
    long WINAPI DZ_Exec(const DllCommands * C);
    long WINAPI DZ_Abort(void * C);
    long WINAPI DZ_Version(void);        
    long WINAPI DZ_PrivVersion(void);
    long WINAPI DZ_Name(void* buf, int bufsiz, int wide);
    const char* WINAPI DZ_Path(void);
    const char* WINAPI DZ_Banner(void);
#ifdef __cplusplus
}

#endif

#endif





