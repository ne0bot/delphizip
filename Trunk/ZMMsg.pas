Unit ZMMsg;
 
// Built by ZipHelper
//   DO NOT MODIFY
//  ZMMsg.pas - Message Identifiers
(* **************************************************
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
************************************************** *)
//Generated 2012-04-30
 
Interface
 
Const
  DS_UnknownError = 1;
  DS_Canceled = 2;
  DS_CopyCentral = 3;
  GE_Abort = 4;
  GE_Copying = 5;
  GE_Skipped = 6;
  GE_TempZip = 7;
  LD_DllLoaded = 8;
  LD_DllUnloaded = 9;
  LD_LoadErr = 10;
  AD_NothingToZip = 11;
  AZ_NothingToDo = 12;
  DL_NothingToDel = 13;
  DS_NoDiskSpan = 14;
  DS_NoRenamePart = 15;
  GE_EventEx = 16;
  GE_NoSkipping = 17;
  LD_BadDll = 18;
  LD_NoDll = 19;
  LI_WrongZipStruct = 20;
  PW_UnatAddPWMiss = 21;
  PW_UnatExtPWMiss = 22;
  PR_Archive = 33;
  PR_CopyZipFile = 34;
  PR_SFX = 35;
  PR_Header = 36;
  PR_Finish = 37;
  PR_Copying = 38;
  PR_CentrlDir = 39;
  PR_Checking = 40;
  PR_Loading = 41;
  PR_Joining = 42;
  PR_Splitting = 43;
  PR_Writing = 44;
  PR_PreCalc = 45;
  PR_Processing = 46;
  PR_Merging = 47;
  CF_FileConflict = 58;
  CF_Merge = 59;
  CF_OverwriteYN = 60;
  DS_AnotherDisk = 61;
  DS_AskDeleteFile = 62;
  DS_AskPrevFile = 63;
  DS_InDrive = 64;
  DS_InsertAVolume = 65;
  DS_InsertDisk = 66;
  DS_InsertVolume = 67;
  FM_Confirm = 68;
  FM_Erase = 69;
  PW_Caption = 70;
  PW_MessageConfirm = 71;
  PW_MessageEnter = 72;
  ZB_Yes = 83;
  ZB_No = 84;
  ZB_OK = 85;
  ZB_Cancel = 86;
  ZB_Abort = 87;
  ZB_Retry = 88;
  ZB_Ignore = 89;
  ZB_CancelAll = 90;
  ZB_NoToAll = 91;
  ZB_YesToAll = 92;
  AD_BadFileName = 103;
  AD_DuplFileName = 104;
  AD_InIsOutStream = 105;
  AD_InvalidEncode = 106;
  AD_InvalidName = 107;
  AD_InvalidZip = 108;
  AD_NoDestDir = 109;
  AD_UnattPassword = 110;
  AZ_SameAsSource = 111;
  CD_CEHDataSize = 112;
  CD_NoChangeDir = 113;
  CD_NoProtected = 114;
  CF_NoDest = 115;
  CF_SourceIsDest = 116;
  CZ_InputNotExe = 117;
  DS_BadDrive = 118;
  DS_CEHWrongSig = 119;
  DS_DriveNoMount = 120;
  DS_NoDiskSpace = 121;
  DS_NoEncrypt = 122;
  DS_NoInFile = 123;
  DS_NotaDrive = 124;
  DS_NotChangeable = 125;
  DS_NoUnattSpan = 126;
  DS_NoValidZip = 127;
  DS_Unsupported = 128;
  EX_NoExtrDir = 129;
  EX_UnAttPassword = 130;
  GE_InvalidArguments = 131;
  GE_InvalidParameter = 132;
  GE_NoZipSpecified = 133;
  GE_WrongPassword = 134;
  RN_InvalidDateTime = 135;
  SF_DetachedHeaderTooBig = 136;
  SF_NoZipSFXBin = 137;
  SF_StringTooLong = 138;
  AD_AutoSFXWrong = 139;
  AZ_InternalError = 140;
  CF_CopyFailed = 141;
  CF_SFXCopyError = 142;
  CZ_BrowseError = 143;
  CZ_ExeSections = 144;
  CZ_NoCopyIcon = 145;
  CZ_NoExeIcon = 146;
  CZ_NoExeResource = 147;
  CZ_NoIcon = 148;
  CZ_NoIconFound = 149;
  DS_BadCRC = 150;
  DS_CECommentLen = 151;
  DS_CEHBadRead = 152;
  DS_CEHBadWrite = 153;
  DS_CENameLen = 154;
  DS_CopyError = 155;
  DS_DataCopy = 156;
  DS_DataDesc = 157;
  DS_EOCBadRead = 158;
  DS_EOCBadWrite = 159;
  DS_ErrorUnknown = 160;
  DS_FailedSeek = 161;
  DS_FileChanged = 162;
  DS_FileError = 163;
  DS_FileOpen = 164;
  DS_LOHBadRead = 165;
  DS_LOHBadWrite = 166;
  DS_LOHWrongName = 167;
  DS_NoAppend = 168;
  DS_NoInStream = 169;
  DS_NoMem = 170;
  DS_NoOutFile = 171;
  DS_NoOutStream = 172;
  DS_NoTempFile = 173;
  DS_NoVolume = 174;
  DS_NoWrite = 175;
  DS_ReadError = 176;
  DS_SeekError = 177;
  DS_SFXBadRead = 178;
  DS_TooManyParts = 179;
  DS_WriteError = 180;
  DS_Zip64FieldError = 181;
  GE_DLLCritical = 192;
  GE_Except = 193;
  GE_ExceptErr = 194;
  GE_FatalZip = 195;
  GE_FileChanged = 196;
  GE_Inactive = 197;
  GE_NoMem = 198;
  GE_RangeError = 199;
  GE_Unknown = 200;
  GE_WasBusy = 201;
  GE_NoProcess = 202;
  LI_ErrorUnknown = 203;
  LI_ReadZipError = 204;
  DS_SetDateError = 205;
  GE_GeneralError = 206;
  GE_SystemError = 207;
  GE_SysErr = 208;
  TM_Deleting = 215;
  TM_Erasing = 216;
  TM_GetNewDisk = 217;
  TM_SystemError = 218;
  TM_Trace = 219;
  TM_Verbose = 220;
  DZ_RES_GOOD = 231;
  DZ_RES_CANCELLED = 232;
  DZ_RES_ABORT = 233;
  DZ_RES_CALLBACK = 234;
  DZ_RES_MEMORY = 235;
  DZ_RES_STRUCT = 236;
  DZ_RES_ERROR = 237;
  DZ_RES_PASSWORD_FAIL = 238;
  DZ_RES_PASSWORD_CANCEL = 239;
  DZ_RES_INVAL_ZIP = 240;
  DZ_RES_NO_CENTRAL = 241;
  DZ_RES_ZIP_EOF = 242;
  DZ_RES_ZIP_END = 243;
  DZ_RES_ZIP_NOOPEN = 244;
  DZ_RES_ZIP_MULTI = 245;
  DZ_RES_NOT_FOUND = 246;
  DZ_RES_LOGIC_ERROR = 247;
  DZ_RES_NOTHING_TO_DO = 248;
  DZ_RES_BAD_OPTIONS = 249;
  DZ_RES_TEMP_FAILED = 250;
  DZ_RES_NO_FILE_OPEN = 251;
  DZ_RES_ERROR_READ = 252;
  DZ_RES_ERROR_CREATE = 253;
  DZ_RES_ERROR_WRITE = 254;
  DZ_RES_ERROR_SEEK = 255;
  DZ_RES_EMPTY_ZIP = 256;
  DZ_RES_INVAL_NAME = 257;
  DZ_RES_GENERAL = 258;
  DZ_RES_MISS = 259;
  DZ_RES_WARNING = 260;
  DZ_ERR_ERROR_DELETE = 261;
  DZ_ERR_FATAL_IMPORT = 262;
  DZ_ERR_SKIPPING = 263;
  DZ_ERR_LOCKED = 264;
  DZ_ERR_DENIED = 265;
  DZ_ERR_DUPNAME = 266;
  DZ_ERR_SKIPPED = 267;
  DT_Author = 277;
  DT_Desc = 278;
  DT_Language = 279;
 
Const
 MSG_ID_MASK = $1FF;
 
// name of compressed resource data
const 
  DZRES_Str = 'DZResStr';  // compressed language strings
  DZRES_SFX = 'DZResSFX';  // stored UPX Dll version as string
  DZRES_Dll = 'DZResDll';  // stored UPX Dll
 
implementation
 
end.
