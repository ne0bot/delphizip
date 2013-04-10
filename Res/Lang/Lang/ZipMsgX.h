/* ***************************************************************************
TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009, 2010, 2011 Russell Peters and Roger Aelbrecht

All rights reserved.
For the purposes of Copyright and this license "DelphiZip" is the current
 authors, maintainers and developers of its code:
  Russell Peters and Roger Aelbrecht.

Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
* DelphiZip reserves the names "DelphiZip", "ZipMaster", "ZipBuilder",
   "DelZip" and derivatives of those names for the use in or about this
   code and neither those names nor the names of its authors or
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL DELPHIZIP, IT'S AUTHORS OR CONTRIBUTERS BE
 LIABLE FOR ANYDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

contact: problems AT delphizip DOT org
updates: http://www.delphizip.org
 *************************************************************************** */
//Modified 2011-10-23
// note special comments {//! shared, //* sfx,} //- discard, {//= moved} //Alpha group
#ifndef ZipMsgH
#define ZipMsgH

  #define DT_Language             10096 //K
  #define DT_Author               10097 //K
  #define DT_Desc                 10098  //K
  #define GE_FatalZip             10101 //I
  #define GE_NoZipSpecified       10102 //G
  #define GE_NoMem                10103 //I
  #define GE_WrongPassword        10104 //G
  #define GE_CopyFile             10105 //-
  #define GE_Except               10106 //I
  #define GE_Reentered            10107 //-
  #define GE_Busy                 10108 //-
  #define GE_Inactive             10109 //I
  #define GE_RangeError           10110 //I
  #define GE_TempZip              10111 //B
  #define GE_WasBusy              10112 //I
  #define GE_EventEx              10113 //C
  #define GE_DLLAbort             10118 //-
  #define GE_DLLBusy              10119 //-
  #define GE_DLLCancel            10120 //-
  #define GE_DLLMem               10121 //-
  #define GE_DLLStruct            10122 //-
  #define GE_DLLEvent             10123 //-
  #define GE_DLLCritical          10124 //I
  #define GE_Unknown              10125 //I
  #define GE_Skipped              10126 //B
  #define GE_Copying              10127 //B
  #define GE_Abort                10128 //B
  #define GE_NotPermitted         10129 //-
  #define GE_ExceptErr            10130 //I
  #define GE_NoSkipping           10131 //C
  #define GE_FileChanged          10132 //I 
  #define GE_InvalidParameter     10133 //G 
  #define GE_InvalidArguments     10134 //G 

  #define RN_ZipSFXData           10140.//-
  #define RN_NoRenOnSpan          10141 //-
  #define RN_ProcessFile          10142 //-
  #define RN_RenameTo             10143 //-
  #define RN_InvalidDateTime      10144 //G

  #define PW_UnatAddPWMiss        10150 //C
  #define PW_UnatExtPWMiss        10151 //C
  #define PW_Ok                   10152 //- moved ZB_
  #define PW_Cancel               10153 //- moved ZB_
  #define PW_Caption              10154 //E
  #define PW_MessageEnter         10155 //E
  #define PW_MessageConfirm       10156 //E
  #define PW_CancelAll            10157 //- moved ZB_
  #define PW_Abort                10158 //- moved ZB_
  #define PW_ForFile              10159.//-

  #define CF_SourceIsDest         10180 //G
  #define CF_OverwriteYN          10181 //E
  #define CF_CopyFailed           10182 //I
  #define CF_SourceNotFound       10183 //-
  #define CF_SFXCopyError         10184 //I
  #define CF_DestFileNoOpen       10185 //-
  #define CF_NoCopyOnSpan         10186 //-
  #define CF_NoDest               10187 //G
  #define CF_Merge                10188 //E
  #define CF_FileConflict         10189 //E

  #define LI_ReadZipError         10201 //I
  #define LI_ErrorUnknown         10202 //I
  #define LI_WrongZipStruct       10203 //C
  #define LI_GarbageAtEOF         10204 //-
  #define LI_FileTooBig           10205 //-
  #define LI_MethodUnknown        10206 //-

  #define ZB_Yes                  10220 //F
  #define ZB_No                   10221 //F
  #define ZB_OK                   10222 //F
  #define ZB_Cancel               10223 //F
  #define ZB_Abort                10224 //F
  #define ZB_Retry                10225 //F
  #define ZB_Ignore               10226 //F
  #define ZB_CancelAll            10227 //F
  #define ZB_NoToAll              10228 //F
  #define ZB_YesToAll             10229 //F
  #define ZB_Help                 10230 //-

  #define AD_NothingToZip         10301 //C
  #define AD_UnattPassword        10302 //G
  #define AD_NoFreshenUpdate      10303 //-
  #define AD_AutoSFXWrong         10304 //I
  #define AD_NoStreamDLL          10305 //-
  #define AD_InIsOutStream        10306 //G
  #define AD_InvalidName          10307 //G
  #define AD_NoDestDir            10308 //G
  #define AD_BadFileName          10309 //G
  #define AD_InvalidEncode        10310 //G
  #define AD_InvalidZip           10311 //G
  #define AD_DuplFileName         10312 //G
  
  #define AZ_NothingToDo          10320 //C
  #define AZ_SameAsSource         10321 //G
  #define AZ_InternalError        10322 //I

  #define DL_NothingToDel         10401 //C
  #define DL_NoDelOnSpan          10402 //-

  #define EX_FatalUnZip           10501 //-
  #define EX_UnAttPassword        10502 //G
  #define EX_NoStreamDLL          10503 //-
  #define EX_NoExtrDir            10504 //G

  #define LZ_ZipDllLoaded         10601 //-
  #define LZ_NoZipDllExec         10602 //-
  #define LZ_NoZipDllVers         10603 //-
  #define LZ_NoZipDll             10604 //-
  #define LZ_OldZipDll            10605 //-
  #define LZ_ZipDllUnloaded       10606 //-

  #define LD_NoDll                10650 //C
  #define LD_BadDll               10651 //C
  #define LD_DllLoaded            10652 //B
  #define LD_DllUnloaded          10653 //B
  #define LD_LoadErr              10654 //B

  #define LU_UnzDllLoaded         10701 //-
  #define LU_NoUnzDllExec         10702 //-
  #define LU_NoUnzDllVers         10703 //-
  #define LU_NoUnzDll             10704 //-
  #define LU_OldUnzDll            10705 //-
  #define LU_UnzDllUnloaded       10706 //-

  #define SF_StringToLong         10801 //-! alternate spelling
  #define SF_StringTooLong        10801 //G
  #define SF_NoZipSFXBin          10802 //G
  #define SF_InputIsNoZip         10803 //-
  #define SF_NoSFXSupport         10804 //-
  #define SF_MsgTooLong           10805 //-
  #define SF_DefPathTooLong       10806 //-
  #define SF_DlgTitleTooLong      10807 //-
  #define SF_CmdLineTooLong       10808 //-
  #define SF_FBkPathTooLong       10809 //-
  #define SF_DetachedHeaderTooBig 10810 //G
  #define SF_DetachedErr          10811 //-
  #define SF_NameNeeded           10812 //-

  #define CZ_NoExeSpecified       10901 //-
  #define CZ_InputNotExe          10902 //G
  #define CZ_SFXTypeUnknown       10903 //-
  #define CZ_WrongConvert         10904 //-
  #define CZ_UnknownHeader        10905 //-
  #define CZ_BrowseError          10906 //I
  #define CZ_NoExeResource        10907 //I
  #define CZ_ExeSections          10908 //I
  #define CZ_NoExeIcon            10909 //I
  #define CZ_NoIcon               10910 //I
  #define CZ_NoCopyIcon           10911 //I
  #define CZ_NoIconFound          10912 //I
  #define CZ_NoComp               10913 //-

  #define DS_NoInFile             11001 //G
  #define DS_FileOpen             11002 //I
  #define DS_NotaDrive            11003 //G
  #define DS_DriveNoMount         11004 //G
  #define DS_NoVolume             11005 //I
  #define DS_NoMem                11006 //I
  #define DS_Canceled             11007 //B
  #define DS_FailedSeek           11008 //I
  #define DS_NoOutFile            11009 //I
  #define DS_NoWrite              11010 //I
  #define DS_EOCBadRead           11011 //I
  #define DS_LOHBadRead           11012 //I
  #define DS_CEHBadRead           11013 //I
  #define DS_LOHWrongSig          11014 //-
  #define DS_CEHWrongSig          11015 //G
  #define DS_LONameLen            11016 //-
  #define DS_CENameLen            11017 //I
  #define DS_LOExtraLen           11018 //-
  #define DS_CEExtraLen           11019 //-
  #define DS_DataDesc             11020 //I
  #define DS_ZipData              11021 //-
  #define DS_CECommentLen         11022 //I
  #define DS_EOArchComLen         11023 //-
  #define DS_ErrorUnknown         11024 //I
  #define DS_NoUnattSpan          11025 //G
  #define DS_EntryLost            11026 //-
  #define DS_NoTempFile           11027 //I
  #define DS_LOHBadWrite          11028 //I
  #define DS_CEHBadWrite          11029 //I
  #define DS_EOCBadWrite          11030 //I
  #define DS_ExtWrongSig          11031 //-
  #define DS_NoDiskSpace          11032 //G
  #define DS_InsertDisk           11033 //E
  #define DS_InsertVolume         11034 //E
  #define DS_InDrive              11035 //E
  #define DS_NoValidZip           11036 //G
  #define DS_FirstInSet           11037 //-
  #define DS_NotLastInSet         11038 //-
  #define DS_AskDeleteFile        11039 //E
  #define DS_AskPrevFile          11040 //E
  #define DS_NoSFXSpan            11041 //-
  #define DS_CEHBadCopy           11042 //-
  #define DS_EOCBadSeek           11043 //-
  #define DS_EOCBadCopy           11044 //-
  #define DS_FirstFileOnHD        11045 //-
  #define DS_InsertAVolume        11046 //E
  #define DS_CopyCentral          11047 //B
  #define DS_NoDiskSpan           11048 //C
  #define DS_UnknownError         11049 //C
  #define DS_NoRenamePart         11050 //C
  #define DS_NotChangeable        11051 //G
  #define DS_Zip64FieldError      11052 //I
  #define DS_Unsupported          11053 //G
  #define DS_ReadError            11054 //I
  #define DS_WriteError           11055 //I
  #define DS_FileError            11056 //I
  #define DS_FileChanged          11057 //I
  #define DS_SFXBadRead           11058 //I
  #define DS_BadDrive             11059 //G
  #define DS_LOHWrongName         11060 //I
  #define DS_BadCRC               11061 //I
  #define DS_NoEncrypt            11062 //G
  #define DS_NoInStream           11063 //I
  #define DS_NoOutStream          11064 //I
  #define DS_SeekError            11065 //I
  #define DS_DataCopy             11066 //I
  #define DS_CopyError            11067 //I
  #define DS_TooManyParts         11068 //I
  #define DS_AnotherDisk          11069 //E
  #define DS_NoAppend             11070 //I
  
  #define FM_Erase                11101 //E
  #define FM_Confirm              11102 //E

  #define ED_SizeToLarge          11201 //-    //- alternate spelling
  #define ED_SizeTooLarge         11201 //-
  
  #define SS_StreamError          11250 //-

  #define CD_NoCDOnSpan           11301 //-
  #define CD_NoEventHndlr         11302 //-
  #define CD_LOExtraLen           11303 //-
  #define CD_CEExtraLen           11304 //-
  #define CD_CEComLen             11305 //-
  #define CD_FileName             11306 //-
  #define CD_CEHDataSize          11307 //G
  #define CD_Changing             11308 //-
  #define CD_DuplFileName         11309 //-
  #define CD_NoProtected          11310 //G
  #define CD_InvalidFileName      11311 //-
  #define CD_NoChangeDir          11312 //G
  #define CD_FileSpecSkip         11313 //-

  #define DU_InvalidName          11350 //-
  #define DU_WrongMethod          11351 //-

  #define PR_Progress             11400 //D
  #define PR_Archive              11401 //D
  #define PR_CopyZipFile          11402 //D
  #define PR_SFX                  11403 //D
  #define PR_Header               11404 //D
  #define PR_Finish               11405 //D
  #define PR_Copying              11406 //D
  #define PR_CentrlDir            11407 //D
  #define PR_Checking             11408 //D
  #define PR_Loading              11409 //D
  #define PR_Joining              11410 //D
  #define PR_Splitting            11411 //D
  #define PR_Writing              11412 //D
  #define PR_PreCalc              11413 //D
  #define PR_Processing           11414 //D
  #define PR_Merging              11415 //D

  #define DZ_Skipped              11450 //-
  #define DZ_InUse                11451 //-
  #define DZ_Refused              11452 //-
  #define DZ_NoOpen               11453 //-
  #define DZ_NoFiles              11454 //-
  #define DZ_SizeChanged          11455 //-
  #define DZ_InvalidName          11456 //-
  #define DZ_TooBig               11457 //-

  #define WZ_DropDirOnly          11500  //-
  #define WZ_NothingToWrite       11501  //-

  #define TM_Erasing              11600 //A
  #define TM_Deleting             11601 //A
  #define TM_GetNewDisk           11602 //A
  #define TM_SystemError          11603 //A
  #define TM_Trace                11604 //A
  #define TM_Verbose              11605 //A
  #define TM_Opened               11606 //-
  #define TM_Closed               11607 //-
  #define TM_Created              11608 //-

  #define DZ_RES_GOOD             11648 //J  // ZEN_OK
  #define DZ_RES_CANCELLED        11649 //J
  #define DZ_RES_ABORT            11650 //J
  #define DZ_RES_CALLBACK         11651 //J
  #define DZ_RES_MEMORY           11652 //J
  #define DZ_RES_STRUCT           11653 //J
  #define DZ_RES_ERROR            11654 //J
  #define DZ_RES_PASSWORD_FAIL    11655 //J
  #define DZ_RES_PASSWORD_CANCEL  11656 //J
  #define DZ_RES_INVAL_ZIP        11657 //J  // ZEN_FORM
  #define DZ_RES_NO_CENTRAL       11658 //J  // UEN_EOF01
  #define DZ_RES_ZIP_EOF          11659 //J  // ZEN_EOF 
  #define DZ_RES_ZIP_END          11660 //J  // UEN_EOF02
  #define DZ_RES_ZIP_NOOPEN       11661 //J
  #define DZ_RES_ZIP_MULTI        11662 //J
  #define DZ_RES_NOT_FOUND        11663 //J
  #define DZ_RES_LOGIC_ERROR      11664 //J  // ZEN_LOGIC
  #define DZ_RES_NOTHING_TO_DO    11665 //J  // ZEN_NONE
  #define DZ_RES_BAD_OPTIONS      11666 //J // ZEN_PARM
  #define DZ_RES_TEMP_FAILED      11667 //J // ZEN_TEMP
  #define DZ_RES_NO_FILE_OPEN     11668 //J  // ZEN_OPEN
  #define DZ_RES_ERROR_READ       11669 //J  // ZEN_READ
  #define DZ_RES_ERROR_CREATE     11670 //J  // ZEN_CREAT
  #define DZ_RES_ERROR_WRITE      11671 //J  // ZEN_WRITE
  #define DZ_RES_ERROR_SEEK       11672 //J
  #define DZ_RES_EMPTY_ZIP        11673 //J
  #define DZ_RES_INVAL_NAME       11674 //J
  #define DZ_RES_GENERAL          11675 //J
  #define DZ_RES_MISS             11676 //J  // ZEN_MISS UEN_MISC03
  #define DZ_RES_WARNING          11677 //J  // PK_WARN
  #define DZ_ERR_ERROR_DELETE     11678 //J  // PK_NODEL
  #define DZ_ERR_FATAL_IMPORT     11679 //J
  #define DZ_ERR_SKIPPING         11680 //J
  #define DZ_ERR_LOCKED           11681 //J
  #define DZ_ERR_DENIED           11682 //J
  #define DZ_ERR_DUPNAME          11683 //J
  
  #define DT_Last                 11699 //-

#endif

