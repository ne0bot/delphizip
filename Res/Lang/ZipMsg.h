#ifndef ZipMsgH
  #define ZipMsgH
/* **************************************************
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
************************************************** */
//Generated 2011-11-14
 
// note special comments //! shared, //* sfx, //- discard, {//= moved}
 
  #define DS_UnknownError          1 //!
  #define DS_Canceled              2
  #define DS_CopyCentral           3
  #define GE_Abort                 4
  #define GE_Copying               5
  #define GE_Skipped               6
  #define GE_TempZip               7
  #define LD_DllLoaded             8
  #define LD_DllUnloaded           9
  #define LD_LoadErr               10
  #define AD_NothingToZip          11
  #define AZ_NothingToDo           12
  #define DL_NothingToDel          13
  #define DS_NoDiskSpan            14
  #define DS_NoRenamePart          15
  #define GE_EventEx               16
  #define GE_NoSkipping            17
  #define LD_BadDll                18
  #define LD_NoDll                 19
  #define LI_WrongZipStruct        20
  #define PW_UnatAddPWMiss         21
  #define PW_UnatExtPWMiss         22
  #define PR_Archive               33
  #define PR_CopyZipFile           34
  #define PR_SFX                   35
  #define PR_Header                36
  #define PR_Finish                37
  #define PR_Copying               38
  #define PR_CentrlDir             39
  #define PR_Checking              40
  #define PR_Loading               41
  #define PR_Joining               42
  #define PR_Splitting             43
  #define PR_Writing               44 // new
  #define PR_PreCalc               45
  #define PR_Processing            46 // new
  #define PR_Merging               47 // new
  #define CF_FileConflict          58 // * new
  #define CF_Merge                 59 // * new
  #define CF_OverwriteYN           60
  #define DS_AnotherDisk           61 // new+++
  #define DS_AskDeleteFile         62
  #define DS_AskPrevFile           63
  #define DS_InDrive               64
  #define DS_InsertAVolume         65
  #define DS_InsertDisk            66
  #define DS_InsertVolume          67
  #define FM_Confirm               68
  #define FM_Erase                 69
  #define PW_Caption               70
  #define PW_MessageConfirm        71
  #define PW_MessageEnter          72
  #define ZB_Yes                   83
  #define ZB_No                    84
  #define ZB_OK                    85 // =PW_Ok
  #define ZB_Cancel                86 // =PW_Cancel
  #define ZB_Abort                 87 // =PW_Abort
  #define ZB_Retry                 88
  #define ZB_Ignore                89
  #define ZB_CancelAll             90 // =PW_CancelAll
  #define ZB_NoToAll               91
  #define ZB_YesToAll              92
  #define AD_BadFileName           103
  #define AD_DuplFileName          104 // new
  #define AD_InIsOutStream         105
  #define AD_InvalidEncode         106
  #define AD_InvalidName           107
  #define AD_InvalidZip            108
  #define AD_NoDestDir             109
  #define AD_UnattPassword         110
  #define AZ_SameAsSource          111
  #define CD_CEHDataSize           112
  #define CD_NoChangeDir           113
  #define CD_NoProtected           114
  #define CF_NoDest                115
  #define CF_SourceIsDest          116
  #define CZ_InputNotExe           117 //!
  #define DS_BadDrive              118 // new
  #define DS_CEHWrongSig           119
  #define DS_DriveNoMount          120
  #define DS_NoDiskSpace           121
  #define DS_NoEncrypt             122 // new
  #define DS_NoInFile              123
  #define DS_NotaDrive             124
  #define DS_NotChangeable         125
  #define DS_NoUnattSpan           126
  #define DS_NoValidZip            127 //!
  #define DS_Unsupported           128 // new
  #define EX_NoExtrDir             129
  #define EX_UnAttPassword         130
  #define GE_InvalidArguments      131 // * new 1.9.1
  #define GE_InvalidParameter      132 // * new 1.9.1
  #define GE_NoZipSpecified        133 //!
  #define GE_WrongPassword         134
  #define RN_InvalidDateTime       135
  #define SF_DetachedHeaderTooBig  136 //  new 1.80
  #define SF_NoZipSFXBin           137 //!
  #define SF_StringTooLong         138
  #define AD_AutoSFXWrong          139
  #define AZ_InternalError         140
  #define CF_CopyFailed            141
  #define CF_SFXCopyError          142
  #define CZ_BrowseError           143 // new 1.79
  #define CZ_ExeSections           144 // new 1.79
  #define CZ_NoCopyIcon            145 // new 1.79
  #define CZ_NoExeIcon             146 // new 1.79
  #define CZ_NoExeResource         147 // new 1.79
  #define CZ_NoIcon                148 // new 1.79
  #define CZ_NoIconFound           149 // new 1.79
  #define DS_BadCRC                150 // new
  #define DS_CECommentLen          151 //!
  #define DS_CEHBadRead            152 //!
  #define DS_CEHBadWrite           153 //!
  #define DS_CENameLen             154
  #define DS_CopyError             155 // new+
  #define DS_DataCopy              156 // new+
  #define DS_DataDesc              157
  #define DS_EOCBadRead            158 //!
  #define DS_EOCBadWrite           159 //!
  #define DS_ErrorUnknown          160
  #define DS_FailedSeek            161
  #define DS_FileChanged           162 // new
  #define DS_FileError             163 // new
  #define DS_FileOpen              164
  #define DS_LOHBadRead            165 //!
  #define DS_LOHBadWrite           166 //!
  #define DS_LOHWrongName          167 // new
  #define DS_NoAppend              168 // * new 1.9.1
  #define DS_NoInStream            169 // new
  #define DS_NoMem                 170
  #define DS_NoOutFile             171
  #define DS_NoOutStream           172 // new
  #define DS_NoTempFile            173
  #define DS_NoVolume              174
  #define DS_NoWrite               175
  #define DS_ReadError             176 // new
  #define DS_SeekError             177 // new+
  #define DS_SFXBadRead            178 // new
  #define DS_TooManyParts          179 // new++
  #define DS_WriteError            180 // new
  #define DS_Zip64FieldError       181 // new
  #define GE_DLLCritical           192
  #define GE_Except                193
  #define GE_ExceptErr             194
  #define GE_FatalZip              195
  #define GE_FileChanged           196 // * new 1.90
  #define GE_Inactive              197
  #define GE_NoMem                 198
  #define GE_RangeError            199
  #define GE_Unknown               200
  #define GE_WasBusy               201
  #define GE_NoProcess             202
  #define LI_ErrorUnknown          203
  #define LI_ReadZipError          204
  #define DS_SetDateError		   205
  #define GE_GeneralError		   206
  #define GE_SystemError		   207
  #define GE_SysErr				   208
  #define TM_Deleting              215
  #define TM_Erasing               216
  #define TM_GetNewDisk            217
  #define TM_SystemError           218
  #define TM_Trace                 219 // new
  #define TM_Verbose               220 // new
  #define DZ_RES_GOOD              231 // ZEN_OK
  #define DZ_RES_CANCELLED         232
  #define DZ_RES_ABORT             233
  #define DZ_RES_CALLBACK          234
  #define DZ_RES_MEMORY            235
  #define DZ_RES_STRUCT            236
  #define DZ_RES_ERROR             237
  #define DZ_RES_PASSWORD_FAIL     238
  #define DZ_RES_PASSWORD_CANCEL   239
  #define DZ_RES_INVAL_ZIP         240 // ZEN_FORM
  #define DZ_RES_NO_CENTRAL        241 // UEN_EOF01
  #define DZ_RES_ZIP_EOF           242 // ZEN_EOF
  #define DZ_RES_ZIP_END           243 // UEN_EOF02
  #define DZ_RES_ZIP_NOOPEN        244
  #define DZ_RES_ZIP_MULTI         245
  #define DZ_RES_NOT_FOUND         246
  #define DZ_RES_LOGIC_ERROR       247 // ZEN_LOGIC
  #define DZ_RES_NOTHING_TO_DO     248 // ZEN_NONE
  #define DZ_RES_BAD_OPTIONS       249 // ZEN_PARM
  #define DZ_RES_TEMP_FAILED       250 // ZEN_TEMP
  #define DZ_RES_NO_FILE_OPEN      251 // ZEN_OPEN
  #define DZ_RES_ERROR_READ        252 // ZEN_READ
  #define DZ_RES_ERROR_CREATE      253 // ZEN_CREAT
  #define DZ_RES_ERROR_WRITE       254 // ZEN_WRITE
  #define DZ_RES_ERROR_SEEK        255
  #define DZ_RES_EMPTY_ZIP         256
  #define DZ_RES_INVAL_NAME        257
  #define DZ_RES_GENERAL           258
  #define DZ_RES_MISS              259 // ZEN_MISS UEN_MISC03
  #define DZ_RES_WARNING           260 // PK_WARN
  #define DZ_ERR_ERROR_DELETE      261 // PK_NODEL
  #define DZ_ERR_FATAL_IMPORT      262
  #define DZ_ERR_SKIPPING          263
  #define DZ_ERR_LOCKED            264
  #define DZ_ERR_DENIED            265
  #define DZ_ERR_DUPNAME           266
  #define DZ_ERR_SKIPPED           267
  #define DT_Author                277
  #define DT_Desc                  278
  #define DT_Language              279
 
  #define DT_Last                  279
 
#endif
