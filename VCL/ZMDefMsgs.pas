Unit ZMDefMsgs;
 
// Built by ZipHelper
//   DO NOT MODIFY
//  ZMDefMsgs.pas - default messages and compressed tables
 
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
 
Uses
  ZMMsg;
 
{$I  '.\ZMConfig191.inc'}
 
{$IFNDEF USE_COMPRESSED_STRINGS}
 
type
  TZipResRec = packed record
    i: Word;
    s: pResStringRec;
  end;
 
ResourceString
  _DS_UnknownError = 'Unknown Error';
  _DS_Canceled = 'User canceled operation';
  _DS_CopyCentral = 'Central directory';
  _GE_Abort = 'User Abort';
  _GE_Copying = 'Copying: %s';
  _GE_Skipped = 'Skipped %s %d';
  _GE_TempZip = 'Temporary zipfile: %s';
  _LD_DllLoaded = 'Loaded %s';
  _LD_DllUnloaded = 'Unloaded %s';
  _LD_LoadErr = 'Error [%d %s] loading %s';
  _AD_NothingToZip = 'Error - no files to zip!';
  _AZ_NothingToDo = 'Nothing to do';
  _DL_NothingToDel = 'Error - no files selected for deletion';
  _DS_NoDiskSpan = 'DiskSpanning not supported';
  _DS_NoRenamePart = 'Last part left as : %s';
  _GE_EventEx = 'Exception in Event ';
  _GE_NoSkipping = 'Skipping not allowed';
  _LD_BadDll = 'Unable to load %s - It is old or corrupt';
  _LD_NoDll = 'Failed to load %s';
  _LI_WrongZipStruct = 'Warning - Error in zip structure!';
  _PW_UnatAddPWMiss = 'Error - no add password given';
  _PW_UnatExtPWMiss = 'Error - no extract password given';
  _PR_Archive = '*Resetting Archive bit';
  _PR_CopyZipFile = '*Copying Zip File';
  _PR_SFX = '*SFX';
  _PR_Header = '*??';
  _PR_Finish = '*Finalising';
  _PR_Copying = '*Copying';
  _PR_CentrlDir = '*Writing Central Directory';
  _PR_Checking = '*Checking';
  _PR_Loading = '*Loading Directory';
  _PR_Joining = '*Joining split zip file';
  _PR_Splitting = '*Splitting zip file';
  _PR_Writing = '*Writing zip file';
  _PR_PreCalc = '*Precalculating CRC';
  _PR_Processing = '*Processing';
  _PR_Merging = '*Merging';
  _CF_FileConflict = 'File conflict!';
  _CF_Merge = '''%s'''#10'will overwrite'#10' ''%s'''#10'Rename to ''%s''?';
  _CF_OverwriteYN = 'Overwrite file ''%s'' in ''%s'' ?';
  _DS_AnotherDisk = 'This disk is part of a backup set,'#10'please insert another disk';
  _DS_AskDeleteFile = 'There is already a file %s'#10'Do you want to overwrite this file';
  _DS_AskPrevFile = 'ATTENTION: This is previous disk no %d!!!'#10'Are you sure you want to overwrite the contents';
  _DS_InDrive = ''#10'in drive: %s';
  _DS_InsertAVolume = 'Please insert disk volume %.1d';
  _DS_InsertDisk = 'Please insert last disk';
  _DS_InsertVolume = 'Please insert disk volume %.1d of %.1d';
  _FM_Confirm = 'Confirm';
  _FM_Erase = 'Erase %s';
  _PW_Caption = 'Password';
  _PW_MessageConfirm = 'Confirm Password ';
  _PW_MessageEnter = 'Enter Password ';
  _ZB_Yes = '&Yes';
  _ZB_No = '&No';
  _ZB_OK = '&OK';
  _ZB_Cancel = '&Cancel';
  _ZB_Abort = '&Abort';
  _ZB_Retry = '&Retry';
  _ZB_Ignore = '&Ignore';
  _ZB_CancelAll = 'CancelAll';
  _ZB_NoToAll = 'NoToAll';
  _ZB_YesToAll = 'YesToAll';
  _AD_BadFileName = 'Invalid Filename';
  _AD_DuplFileName = 'Duplicate Filename';
  _AD_InIsOutStream = 'Input stream may not be set to the output stream';
  _AD_InvalidEncode = 'Invalid encoding options';
  _AD_InvalidName = 'Wildcards are not allowed in Filename or file specification';
  _AD_InvalidZip = 'Invalid zip file';
  _AD_NoDestDir = 'Destination directory ''%s'' must exist!';
  _AD_UnattPassword = 'Unattended action not possible without a password';
  _AZ_SameAsSource = 'source and destination on same removable drive';
  _CD_CEHDataSize = 'The combined length of CEH + FileName + FileComment + ExtraData exceeds 65535';
  _CD_NoChangeDir = 'Cannot change path';
  _CD_NoProtected = 'Cannot change details of Encrypted file';
  _CF_NoDest = 'No destination specified';
  _CF_SourceIsDest = 'Source archive is the same as the destination archive!';
  _CZ_InputNotExe = 'Error: input file is not an .EXE file';
  _DS_BadDrive = 'cannot use drive';
  _DS_CEHWrongSig = 'A central header signature is wrong';
  _DS_DriveNoMount = 'Drive %s is NOT defined';
  _DS_NoDiskSpace = 'This disk has not enough free space available';
  _DS_NoEncrypt = 'encryption not supported';
  _DS_NoInFile = 'Input file does not exist';
  _DS_NotaDrive = 'Not a valid drive: %s';
  _DS_NotChangeable = 'Cannot write to %s';
  _DS_NoUnattSpan = 'Unattended disk spanning not implemented';
  _DS_NoValidZip = 'This archive is not a valid Zip archive';
  _DS_Unsupported = 'Unsupported zip version';
  _EX_NoExtrDir = 'Extract directory ''%s'' must exist';
  _EX_UnAttPassword = 'Warning - Unattended Extract: possible not all files extracted';
  _GE_InvalidArguments = 'Invalid Arguments';
  _GE_InvalidParameter = 'Invalid Parameter! ''%s''';
  _GE_NoZipSpecified = 'Error - no zip file specified!';
  _GE_WrongPassword = 'Error - passwords do NOT match'#10'Password ignored';
  _RN_InvalidDateTime = 'Invalid date/time argument for file: ';
  _SF_DetachedHeaderTooBig = 'Detached SFX Header too large';
  _SF_NoZipSFXBin = 'Error: SFX stub ''%s'' not found!';
  _SF_StringTooLong = 'Error: Combined SFX strings unreasonably long!';
  _AD_AutoSFXWrong = 'Error %.1d occurred during Auto SFX creation.';
  _AZ_InternalError = 'Internal error';
  _CF_CopyFailed = 'Copying a file from ''%s'' to ''%s'' failed';
  _CF_SFXCopyError = 'Error while copying the SFX data';
  _CZ_BrowseError = 'Error while browsing resources.';
  _CZ_ExeSections = 'Error while reading executable sections.';
  _CZ_NoCopyIcon = 'Cannot copy icon.';
  _CZ_NoExeIcon = 'No icon resources found in executable.';
  _CZ_NoExeResource = 'No resources found in executable.';
  _CZ_NoIcon = 'No icon found.';
  _CZ_NoIconFound = 'No matching icon found.';
  _DS_BadCRC = 'CRC error';
  _DS_CECommentLen = 'Error while reading a file comment';
  _DS_CEHBadRead = 'Error while reading a central header';
  _DS_CEHBadWrite = 'Error while writing a central header';
  _DS_CENameLen = 'Error while reading a central file name';
  _DS_CopyError = 'File copy error';
  _DS_DataCopy = 'Error copying compressed data';
  _DS_DataDesc = 'Error while reading/writing a data descriptor area';
  _DS_EOCBadRead = 'Error while reading the End Of Central Directory';
  _DS_EOCBadWrite = 'Error while writing the End Of Central Directory';
  _DS_ErrorUnknown = 'UnKnown error in function ReadSpan(), WriteSpan(), ChangeFileDetails() or CopyZippedFiles()'#10;
  _DS_FailedSeek = 'Seek error in input file';
  _DS_FileChanged = 'File changed';
  _DS_FileError = 'File Error';
  _DS_FileOpen = 'Zip file could not be opened';
  _DS_LOHBadRead = 'Error while reading a local header';
  _DS_LOHBadWrite = 'Error while writing a local header';
  _DS_LOHWrongName = 'Local and Central names different : %s';
  _DS_NoAppend = 'Append failed';
  _DS_NoInStream = 'No input stream';
  _DS_NoMem = 'Not enough memory to display MsgBox';
  _DS_NoOutFile = 'Creation of output file failed';
  _DS_NoOutStream = 'No output stream';
  _DS_NoTempFile = 'Temporary file could not be created';
  _DS_NoVolume = 'Volume label could not be set';
  _DS_NoWrite = 'Write error in output file';
  _DS_ReadError = 'Error reading file';
  _DS_SeekError = 'File seek error';
  _DS_SFXBadRead = 'Error reading SFX';
  _DS_TooManyParts = 'More than 999 parts in multi volume archive';
  _DS_WriteError = 'Error writing file';
  _DS_Zip64FieldError = 'Error reading Zip64 field';
  _GE_DLLCritical = 'critical DLL Error %d';
  _GE_Except = 'Exception in Event handler ';
  _GE_ExceptErr = 'Error Exception: ';
  _GE_FatalZip = 'Fatal Error in DLL: abort exception';
  _GE_FileChanged = 'Zip file was changed!';
  _GE_Inactive = 'not Active';
  _GE_NoMem = 'Requested memory not available';
  _GE_RangeError = 'Index (%d) outside range 0..%d';
  _GE_Unknown = ' Unknown error %d';
  _GE_WasBusy = 'Busy + %s';
  _GE_NoProcess = 'Cannot process invalid zip';
  _LI_ErrorUnknown = 'Unknown error in List() function';
  _LI_ReadZipError = 'Seek error reading Zip archive!';
  _DS_SetDateError = 'Error setting file date';
  _GE_GeneralError = 'Unspecified error ';
  _GE_SystemError = 'System error ';
  _GE_SysErr = ' [$%x] %s';
  _TM_Deleting = 'EraseFloppy - Deleting %s';
  _TM_Erasing = 'EraseFloppy - Removing %s';
  _TM_GetNewDisk = 'Trace : GetNewDisk Opening: %s';
  _TM_SystemError = 'System error: %d';
  _TM_Trace = 'Trace: ';
  _TM_Verbose = 'info: ';
  _DZ_RES_GOOD = 'Good';
  _DZ_RES_CANCELLED = 'Cancelled';
  _DZ_RES_ABORT = 'Aborted by User!';
  _DZ_RES_CALLBACK = 'Callback exception';
  _DZ_RES_MEMORY = 'No memory';
  _DZ_RES_STRUCT = 'Invalid structure';
  _DZ_RES_ERROR = 'Fatal error';
  _DZ_RES_PASSWORD_FAIL = 'Password failed!';
  _DZ_RES_PASSWORD_CANCEL = 'Password cancelled!';
  _DZ_RES_INVAL_ZIP = 'Invalid zip structure!';
  _DZ_RES_NO_CENTRAL = 'No Central directory!';
  _DZ_RES_ZIP_EOF = 'Unexpected end of Zip file!';
  _DZ_RES_ZIP_END = 'Premature end of file!';
  _DZ_RES_ZIP_NOOPEN = 'Error opening Zip file!';
  _DZ_RES_ZIP_MULTI = 'Multi-part Zips not supported!';
  _DZ_RES_NOT_FOUND = 'File not found!';
  _DZ_RES_LOGIC_ERROR = 'Internal logic error!';
  _DZ_RES_NOTHING_TO_DO = 'Nothing to do!';
  _DZ_RES_BAD_OPTIONS = 'Bad Options specified!';
  _DZ_RES_TEMP_FAILED = 'Temporary file failure!';
  _DZ_RES_NO_FILE_OPEN = 'File not found or no permission!';
  _DZ_RES_ERROR_READ = 'Error reading file!';
  _DZ_RES_ERROR_CREATE = 'Error creating file!';
  _DZ_RES_ERROR_WRITE = 'Error writing file!';
  _DZ_RES_ERROR_SEEK = 'Error seeking in file!';
  _DZ_RES_EMPTY_ZIP = 'Missing or empty zip file!';
  _DZ_RES_INVAL_NAME = 'Invalid characters in filename!';
  _DZ_RES_GENERAL = 'Error ';
  _DZ_RES_MISS = 'Nothing found';
  _DZ_RES_WARNING = 'Warning: ';
  _DZ_ERR_ERROR_DELETE = 'Delete failed';
  _DZ_ERR_FATAL_IMPORT = 'Fatal Error - could not import symbol!';
  _DZ_ERR_SKIPPING = 'Skipping: ';
  _DZ_ERR_LOCKED = 'File locked';
  _DZ_ERR_DENIED = 'Access denied';
  _DZ_ERR_DUPNAME = 'Duplicate internal name';
  _DZ_ERR_SKIPPED = 'Skipped files';
  _DT_Author = 'R.Peters';
  _DT_Desc = 'Language Neutral';
  _DT_Language = 'US: default';
 
const
ResTable: array [0..203] of TZipResRec = (
    (i: DS_UnknownError; s: @_DS_UnknownError),
    (i: DS_Canceled; s: @_DS_Canceled),
    (i: DS_CopyCentral; s: @_DS_CopyCentral),
    (i: GE_Abort; s: @_GE_Abort),
    (i: GE_Copying; s: @_GE_Copying),
    (i: GE_Skipped; s: @_GE_Skipped),
    (i: GE_TempZip; s: @_GE_TempZip),
    (i: LD_DllLoaded; s: @_LD_DllLoaded),
    (i: LD_DllUnloaded; s: @_LD_DllUnloaded),
    (i: LD_LoadErr; s: @_LD_LoadErr),
    (i: AD_NothingToZip; s: @_AD_NothingToZip),
    (i: AZ_NothingToDo; s: @_AZ_NothingToDo),
    (i: DL_NothingToDel; s: @_DL_NothingToDel),
    (i: DS_NoDiskSpan; s: @_DS_NoDiskSpan),
    (i: DS_NoRenamePart; s: @_DS_NoRenamePart),
    (i: GE_EventEx; s: @_GE_EventEx),
    (i: GE_NoSkipping; s: @_GE_NoSkipping),
    (i: LD_BadDll; s: @_LD_BadDll),
    (i: LD_NoDll; s: @_LD_NoDll),
    (i: LI_WrongZipStruct; s: @_LI_WrongZipStruct),
    (i: PW_UnatAddPWMiss; s: @_PW_UnatAddPWMiss),
    (i: PW_UnatExtPWMiss; s: @_PW_UnatExtPWMiss),
    (i: PR_Archive; s: @_PR_Archive),
    (i: PR_CopyZipFile; s: @_PR_CopyZipFile),
    (i: PR_SFX; s: @_PR_SFX),
    (i: PR_Header; s: @_PR_Header),
    (i: PR_Finish; s: @_PR_Finish),
    (i: PR_Copying; s: @_PR_Copying),
    (i: PR_CentrlDir; s: @_PR_CentrlDir),
    (i: PR_Checking; s: @_PR_Checking),
    (i: PR_Loading; s: @_PR_Loading),
    (i: PR_Joining; s: @_PR_Joining),
    (i: PR_Splitting; s: @_PR_Splitting),
    (i: PR_Writing; s: @_PR_Writing),
    (i: PR_PreCalc; s: @_PR_PreCalc),
    (i: PR_Processing; s: @_PR_Processing),
    (i: PR_Merging; s: @_PR_Merging),
    (i: CF_FileConflict; s: @_CF_FileConflict),
    (i: CF_Merge; s: @_CF_Merge),
    (i: CF_OverwriteYN; s: @_CF_OverwriteYN),
    (i: DS_AnotherDisk; s: @_DS_AnotherDisk),
    (i: DS_AskDeleteFile; s: @_DS_AskDeleteFile),
    (i: DS_AskPrevFile; s: @_DS_AskPrevFile),
    (i: DS_InDrive; s: @_DS_InDrive),
    (i: DS_InsertAVolume; s: @_DS_InsertAVolume),
    (i: DS_InsertDisk; s: @_DS_InsertDisk),
    (i: DS_InsertVolume; s: @_DS_InsertVolume),
    (i: FM_Confirm; s: @_FM_Confirm),
    (i: FM_Erase; s: @_FM_Erase),
    (i: PW_Caption; s: @_PW_Caption),
    (i: PW_MessageConfirm; s: @_PW_MessageConfirm),
    (i: PW_MessageEnter; s: @_PW_MessageEnter),
    (i: ZB_Yes; s: @_ZB_Yes),
    (i: ZB_No; s: @_ZB_No),
    (i: ZB_OK; s: @_ZB_OK),
    (i: ZB_Cancel; s: @_ZB_Cancel),
    (i: ZB_Abort; s: @_ZB_Abort),
    (i: ZB_Retry; s: @_ZB_Retry),
    (i: ZB_Ignore; s: @_ZB_Ignore),
    (i: ZB_CancelAll; s: @_ZB_CancelAll),
    (i: ZB_NoToAll; s: @_ZB_NoToAll),
    (i: ZB_YesToAll; s: @_ZB_YesToAll),
    (i: AD_BadFileName; s: @_AD_BadFileName),
    (i: AD_DuplFileName; s: @_AD_DuplFileName),
    (i: AD_InIsOutStream; s: @_AD_InIsOutStream),
    (i: AD_InvalidEncode; s: @_AD_InvalidEncode),
    (i: AD_InvalidName; s: @_AD_InvalidName),
    (i: AD_InvalidZip; s: @_AD_InvalidZip),
    (i: AD_NoDestDir; s: @_AD_NoDestDir),
    (i: AD_UnattPassword; s: @_AD_UnattPassword),
    (i: AZ_SameAsSource; s: @_AZ_SameAsSource),
    (i: CD_CEHDataSize; s: @_CD_CEHDataSize),
    (i: CD_NoChangeDir; s: @_CD_NoChangeDir),
    (i: CD_NoProtected; s: @_CD_NoProtected),
    (i: CF_NoDest; s: @_CF_NoDest),
    (i: CF_SourceIsDest; s: @_CF_SourceIsDest),
    (i: CZ_InputNotExe; s: @_CZ_InputNotExe),
    (i: DS_BadDrive; s: @_DS_BadDrive),
    (i: DS_CEHWrongSig; s: @_DS_CEHWrongSig),
    (i: DS_DriveNoMount; s: @_DS_DriveNoMount),
    (i: DS_NoDiskSpace; s: @_DS_NoDiskSpace),
    (i: DS_NoEncrypt; s: @_DS_NoEncrypt),
    (i: DS_NoInFile; s: @_DS_NoInFile),
    (i: DS_NotaDrive; s: @_DS_NotaDrive),
    (i: DS_NotChangeable; s: @_DS_NotChangeable),
    (i: DS_NoUnattSpan; s: @_DS_NoUnattSpan),
    (i: DS_NoValidZip; s: @_DS_NoValidZip),
    (i: DS_Unsupported; s: @_DS_Unsupported),
    (i: EX_NoExtrDir; s: @_EX_NoExtrDir),
    (i: EX_UnAttPassword; s: @_EX_UnAttPassword),
    (i: GE_InvalidArguments; s: @_GE_InvalidArguments),
    (i: GE_InvalidParameter; s: @_GE_InvalidParameter),
    (i: GE_NoZipSpecified; s: @_GE_NoZipSpecified),
    (i: GE_WrongPassword; s: @_GE_WrongPassword),
    (i: RN_InvalidDateTime; s: @_RN_InvalidDateTime),
    (i: SF_DetachedHeaderTooBig; s: @_SF_DetachedHeaderTooBig),
    (i: SF_NoZipSFXBin; s: @_SF_NoZipSFXBin),
    (i: SF_StringTooLong; s: @_SF_StringTooLong),
    (i: AD_AutoSFXWrong; s: @_AD_AutoSFXWrong),
    (i: AZ_InternalError; s: @_AZ_InternalError),
    (i: CF_CopyFailed; s: @_CF_CopyFailed),
    (i: CF_SFXCopyError; s: @_CF_SFXCopyError),
    (i: CZ_BrowseError; s: @_CZ_BrowseError),
    (i: CZ_ExeSections; s: @_CZ_ExeSections),
    (i: CZ_NoCopyIcon; s: @_CZ_NoCopyIcon),
    (i: CZ_NoExeIcon; s: @_CZ_NoExeIcon),
    (i: CZ_NoExeResource; s: @_CZ_NoExeResource),
    (i: CZ_NoIcon; s: @_CZ_NoIcon),
    (i: CZ_NoIconFound; s: @_CZ_NoIconFound),
    (i: DS_BadCRC; s: @_DS_BadCRC),
    (i: DS_CECommentLen; s: @_DS_CECommentLen),
    (i: DS_CEHBadRead; s: @_DS_CEHBadRead),
    (i: DS_CEHBadWrite; s: @_DS_CEHBadWrite),
    (i: DS_CENameLen; s: @_DS_CENameLen),
    (i: DS_CopyError; s: @_DS_CopyError),
    (i: DS_DataCopy; s: @_DS_DataCopy),
    (i: DS_DataDesc; s: @_DS_DataDesc),
    (i: DS_EOCBadRead; s: @_DS_EOCBadRead),
    (i: DS_EOCBadWrite; s: @_DS_EOCBadWrite),
    (i: DS_ErrorUnknown; s: @_DS_ErrorUnknown),
    (i: DS_FailedSeek; s: @_DS_FailedSeek),
    (i: DS_FileChanged; s: @_DS_FileChanged),
    (i: DS_FileError; s: @_DS_FileError),
    (i: DS_FileOpen; s: @_DS_FileOpen),
    (i: DS_LOHBadRead; s: @_DS_LOHBadRead),
    (i: DS_LOHBadWrite; s: @_DS_LOHBadWrite),
    (i: DS_LOHWrongName; s: @_DS_LOHWrongName),
    (i: DS_NoAppend; s: @_DS_NoAppend),
    (i: DS_NoInStream; s: @_DS_NoInStream),
    (i: DS_NoMem; s: @_DS_NoMem),
    (i: DS_NoOutFile; s: @_DS_NoOutFile),
    (i: DS_NoOutStream; s: @_DS_NoOutStream),
    (i: DS_NoTempFile; s: @_DS_NoTempFile),
    (i: DS_NoVolume; s: @_DS_NoVolume),
    (i: DS_NoWrite; s: @_DS_NoWrite),
    (i: DS_ReadError; s: @_DS_ReadError),
    (i: DS_SeekError; s: @_DS_SeekError),
    (i: DS_SFXBadRead; s: @_DS_SFXBadRead),
    (i: DS_TooManyParts; s: @_DS_TooManyParts),
    (i: DS_WriteError; s: @_DS_WriteError),
    (i: DS_Zip64FieldError; s: @_DS_Zip64FieldError),
    (i: GE_DLLCritical; s: @_GE_DLLCritical),
    (i: GE_Except; s: @_GE_Except),
    (i: GE_ExceptErr; s: @_GE_ExceptErr),
    (i: GE_FatalZip; s: @_GE_FatalZip),
    (i: GE_FileChanged; s: @_GE_FileChanged),
    (i: GE_Inactive; s: @_GE_Inactive),
    (i: GE_NoMem; s: @_GE_NoMem),
    (i: GE_RangeError; s: @_GE_RangeError),
    (i: GE_Unknown; s: @_GE_Unknown),
    (i: GE_WasBusy; s: @_GE_WasBusy),
    (i: GE_NoProcess; s: @_GE_NoProcess),
    (i: LI_ErrorUnknown; s: @_LI_ErrorUnknown),
    (i: LI_ReadZipError; s: @_LI_ReadZipError),
    (i: DS_SetDateError; s: @_DS_SetDateError),
    (i: GE_GeneralError; s: @_GE_GeneralError),
    (i: GE_SystemError; s: @_GE_SystemError),
    (i: GE_SysErr; s: @_GE_SysErr),
    (i: TM_Deleting; s: @_TM_Deleting),
    (i: TM_Erasing; s: @_TM_Erasing),
    (i: TM_GetNewDisk; s: @_TM_GetNewDisk),
    (i: TM_SystemError; s: @_TM_SystemError),
    (i: TM_Trace; s: @_TM_Trace),
    (i: TM_Verbose; s: @_TM_Verbose),
    (i: DZ_RES_GOOD; s: @_DZ_RES_GOOD),
    (i: DZ_RES_CANCELLED; s: @_DZ_RES_CANCELLED),
    (i: DZ_RES_ABORT; s: @_DZ_RES_ABORT),
    (i: DZ_RES_CALLBACK; s: @_DZ_RES_CALLBACK),
    (i: DZ_RES_MEMORY; s: @_DZ_RES_MEMORY),
    (i: DZ_RES_STRUCT; s: @_DZ_RES_STRUCT),
    (i: DZ_RES_ERROR; s: @_DZ_RES_ERROR),
    (i: DZ_RES_PASSWORD_FAIL; s: @_DZ_RES_PASSWORD_FAIL),
    (i: DZ_RES_PASSWORD_CANCEL; s: @_DZ_RES_PASSWORD_CANCEL),
    (i: DZ_RES_INVAL_ZIP; s: @_DZ_RES_INVAL_ZIP),
    (i: DZ_RES_NO_CENTRAL; s: @_DZ_RES_NO_CENTRAL),
    (i: DZ_RES_ZIP_EOF; s: @_DZ_RES_ZIP_EOF),
    (i: DZ_RES_ZIP_END; s: @_DZ_RES_ZIP_END),
    (i: DZ_RES_ZIP_NOOPEN; s: @_DZ_RES_ZIP_NOOPEN),
    (i: DZ_RES_ZIP_MULTI; s: @_DZ_RES_ZIP_MULTI),
    (i: DZ_RES_NOT_FOUND; s: @_DZ_RES_NOT_FOUND),
    (i: DZ_RES_LOGIC_ERROR; s: @_DZ_RES_LOGIC_ERROR),
    (i: DZ_RES_NOTHING_TO_DO; s: @_DZ_RES_NOTHING_TO_DO),
    (i: DZ_RES_BAD_OPTIONS; s: @_DZ_RES_BAD_OPTIONS),
    (i: DZ_RES_TEMP_FAILED; s: @_DZ_RES_TEMP_FAILED),
    (i: DZ_RES_NO_FILE_OPEN; s: @_DZ_RES_NO_FILE_OPEN),
    (i: DZ_RES_ERROR_READ; s: @_DZ_RES_ERROR_READ),
    (i: DZ_RES_ERROR_CREATE; s: @_DZ_RES_ERROR_CREATE),
    (i: DZ_RES_ERROR_WRITE; s: @_DZ_RES_ERROR_WRITE),
    (i: DZ_RES_ERROR_SEEK; s: @_DZ_RES_ERROR_SEEK),
    (i: DZ_RES_EMPTY_ZIP; s: @_DZ_RES_EMPTY_ZIP),
    (i: DZ_RES_INVAL_NAME; s: @_DZ_RES_INVAL_NAME),
    (i: DZ_RES_GENERAL; s: @_DZ_RES_GENERAL),
    (i: DZ_RES_MISS; s: @_DZ_RES_MISS),
    (i: DZ_RES_WARNING; s: @_DZ_RES_WARNING),
    (i: DZ_ERR_ERROR_DELETE; s: @_DZ_ERR_ERROR_DELETE),
    (i: DZ_ERR_FATAL_IMPORT; s: @_DZ_ERR_FATAL_IMPORT),
    (i: DZ_ERR_SKIPPING; s: @_DZ_ERR_SKIPPING),
    (i: DZ_ERR_LOCKED; s: @_DZ_ERR_LOCKED),
    (i: DZ_ERR_DENIED; s: @_DZ_ERR_DENIED),
    (i: DZ_ERR_DUPNAME; s: @_DZ_ERR_DUPNAME),
    (i: DZ_ERR_SKIPPED; s: @_DZ_ERR_SKIPPED),
    (i: DT_Author; s: @_DT_Author),
    (i: DT_Desc; s: @_DT_Desc),
    (i: DT_Language; s: @_DT_Language));
 
{$ELSE}
 
const
 CompBlok: array [0..690] of Cardinal = (
  $0AC30409, $00005355, $E0E2D668, $6C6B5885, $1E15475C, $EF6B37AF, 
  $AEC7EBDA, $2AA895B7, $24EB0B66, $2050EC6E, $181FE351, $6E375EC7, 
  $D1F8271C, $BD090934, $F77677BE, $9677BBE2, $DEBD7B99, $05154548, 
  $8A8FC54A, $44215482, $52A04878, $9687F114, $1F9012A8, $14FE5B55, 
  $5152D0FE, $42D02509, $41021E25, $72D12A91, $AD7DCCCE, $2592A6E3, 
  $399CCCEF, $339DF3EF, $96A79097, $90927D2E, $A10F485B, $932920E4, 
  $9918E4C3, $69F25526, $C8D64AB2, $2E4EA4D3, $AB90DF90, $8D7932E4, 
  $417E42BC, $4CFE7A06, $F6B7239E, $24DE7A94, $BBFBBEEF, $DE7A379E, 
  $DD4919E9, $3A9BDA96, $5A9E3A94, $8BA9194F, $BEA7AFA9, $F5317A97, 
  $A9DBD4C7, $F50FDEA1, $E85EF52E, $52F7A87D, $57DED5EF, $FDECDF7A, 
  $7ABFEF7B, $4BEDE9DF, $903E91DF, $4E9F499E, $4C1FA66F, $EDFD2D7F, 
  $4FD30FF4, $C9A7AFA7, $9F3D999E, $9EEAF3DF, $CBE9ECBF, $90B3307C, 
  $CE9CCE59, $9911903C, $C91E642F, $58F3357C, $A5CC89E6, $97320FCC, 
  $FCCB3F33, $E662F332, $335FCCD5, $EE1DF7B7, $F70F7D13, $5AF7D53D, 
  $99FF40DF, $1BFFA77E, $FFFB97FD, $DECB9FD4, $9EC99D96, $7B297ECE, 
  $FB257B29, $365AF652, $EE68FB97, $FB939CDE, $B9E7EE71, $9DFEE6DF, 
  $1F1C0CAB, $1D1E07B0, $C6F032B8, $6E067FC0, $E0C4FC1F, $0647C185, 
  $757839BF, $3FE0FAF0, $62F9B307, $5CFE70FE, $93FCA1FE, $97F92BF9, 
  $E9A1D9F3, $5FA1CDA1, $FA1F5D0D, $43DBD0C7, $0F4F0EB7, $E2FE1F9F, 
  $DFC3E3F0, $F87E7E1F, $86B7E1F5, $F772303F, $6D48EAC8, $2BE46B64, 
  $791D3F23, $C8DAE461, $A38A477F, $D1C5D195, $767E8CCF, $27D1D1F4, 
  $7D1D9F47, $A30FF46E, $770B40A4, $CE178E17, $83C2CC14, $CF0A2785, 
  $FC285E14, $E172F0BA, $0BD785F7, $C5B2156F, $BC52B162, $78B478AB, 
  $7C59B15F, $E29DF8B8, $8A97C533, $AFFE2B7F, $63BB1C58, $A7D8C8EC, 
  $97D8C4C6, $F635BEC7, $C7E7B193, $94F58DAE, $94B74BFA, $B4AA6968, 
  $E4A07A5E, $1E962E94, $F4B77D2F, $A5B3D2A3, $E5D37775, $8556976D, 
  $4BAC82E0, $B869A826, $45987326, $C3098B79, $E64BB9B3, $C309EB98, 
  $982D96A1, $D1D171E9, $5C1BB394, $B78E6478, $9F5BB63A, $564915A6, 
  $0B56ED37, $AD2456AE, $358D6458, $A2185C5B, $ADDA2F43, $4530ED9A, 
  $4B0DC4B3, $D7759091, $052B7E89, $2E0AB9E8, $07DC53CE, $24C1F19E, 
  $16453975, $D91C7A92, $F732C995, $6B03481A, $2648DF8B, $03D306C1, 
  $B7D835E6, $992CA160, $6AE6E5B7, $BC5D70CB, $4A8F72E9, $023A05BF, 
  $A432591D, $F0865B47, $1E6AC3A8, $EA552435, $D64DB756, $06D4BBC2, 
  $27816D37, $E43336A8, $6F0E3860, $D775FDC3, $AA1870D8, $0BA3DA83, 
  $47A2E926, $03B9496D, $72C704CE, $4796FC21, $DD1B0C16, $D392131B, 
  $4D0A5086, $24A038EA, $4F4A9130, $0BE7A6F8, $679A4E56, $29A81658, 
  $8B0B9B65, $28D06DD6, $21B6CA79, $93B7A652, $49856262, $642879E6, 
  $D80D9856, $47B61BA3, $D1E08226, $016E20B3, $AB1322D4, $8993F70B, 
  $B1321999, $6386BB60, $C88D384B, $6169C4C8, $4661262B, $311947CC, 
  $E660D731, $0A5A22A6, $BC3897A2, $42CADB97, $4F6C72D9, $D2D35719, 
  $C638AD70, $788CA17B, $31F014E7, $0C77D31C, $0E657C2D, $A4C9B8F7, 
  $1304EF56, $1681FC75, $9ADD9B80, $995E9B63, $BDC8ADEC, $76DB5CF9, 
  $98B7CA1C, $965F0368, $BDBD51CF, $A9A35CC2, $19EB85C2, $50A43272, 
  $C626D532, $219FFD41, $A3880D6B, $F183C905, $6BC24954, $861BA0D4, 
  $A043E9B9, $9F207798, $90CC396B, $6D412E0C, $6E405A40, $F048A034, 
  $82675C12, $11C355E1, $703AB0CC, $C8A94955, $D39E6E7C, $DB69F70E, 
  $6D02CC80, $87A94D22, $66039532, $CBAAD6D7, $97278B6B, $0452A9A7, 
  $CB6C1075, $14A07EE6, $9562BE04, $6E7CE5CB, $3B20E416, $763F07E9, 
  $E49CABE3, $C9250841, $4B48C0E7, $6CA84240, $8365D53F, $1DC5BDE2, 
  $99551C1F, $71DACBBA, $28C560EC, $0D7E6EFB, $CFAEA57D, $D16C6841, 
  $6A457C84, $69641714, $0DC34278, $A0A0AA4A, $CFC7AC48, $97C64930, 
  $793F1939, $29CF8C9C, $1AE32290, $9857C682, $BE365907, $60B97758, 
  $3ACE1F44, $1AC4600E, $5770FFC7, $B77458FF, $952DA920, $327F18EB, 
  $9A0643EF, $DA25B806, $BE5B745A, $8CC7C587, $1D1A6D26, $0C1BE555, 
  $448E8CA3, $8BDF71F7, $CCA62269, $7C4AB935, $EA0300AE, $D32C76DA, 
  $1DC41610, $0B0091A2, $1E50ACCC, $064A8342, $5DA662D9, $A9106543, 
  $1551A642, $D90933CC, $20630EAF, $F4D9F9D6, $DB6C1021, $00F49096, 
  $16841E43, $00701561, $8B6E5189, $3C20A943, $01AF6DB5, $860243DA, 
  $F7244108, $6120C985, $58C9C02D, $5449FC06, $DF26B04F, $D2A6D052, 
  $D2197304, $BB61B9A7, $A84C0220, $F00C0D7B, $BD18F55C, $8CB51953, 
  $39CFF4B7, $62226CDE, $88AB49DE, $6786F34B, $63269E80, $87C392E0, 
  $10EC7D0E, $33F50AC6, $675B861B, $D8D793A0, $0F3165B1, $88A252F0, 
  $4E8A6BAA, $3A3A614B, $E9779964, $900FB81B, $28303578, $16A83A00, 
  $C632998C, $F793BFD0, $6300B202, $183421A7, $B811155A, $D2E682A9, 
  $55FDEAA9, $DAD4D52D, $C7F432F9, $3003352C, $4369406C, $0EBB4ACE, 
  $F05D7DAC, $9801C16D, $5B1291E6, $9F2CEC02, $35F9035C, $56025F74, 
  $C596861A, $1BD7EE5C, $261826B4, $6C668184, $7FE8FD81, $61FDB4C2, 
  $B5D6E34C, $716CAB18, $74C23016, $A10926C0, $571499D6, $00E1E079, 
  $C5EEB80E, $7D28DA51, $DA49B264, $0C36C04D, $C272A864, $02426EB5, 
  $08E09176, $A9FA4618, $91F8061C, $A0D5D798, $0A67BD41, $4261BA27, 
  $74DAE0A9, $3941C99C, $E83EED05, $5A19CA7B, $F7515999, $C2E30A9B, 
  $8086539D, $2958001C, $B6C9992B, $9C595619, $0DCF7145, $C522050B, 
  $869AF455, $B9F23667, $056C9B08, $0B64B13B, $67A0E4E0, $3A05BA63, 
  $4F4421A8, $9EC75040, $24C03661, $98F4B4E8, $C38F194E, $2CC40030, 
  $933C424C, $9DA1BF9E, $B8D6A683, $92515AEF, $A80585CE, $4BF80509, 
  $3805DF52, $90E71C92, $6E16CC0E, $8341B43D, $42FA69B0, $A17D7C60, 
  $221F00E6, $B84DD78F, $05354985, $A0738006, $C6A653EB, $083870C1, 
  $F04D559A, $6827D6A6, $26A6B49A, $BB484099, $BEAFBBA1, $CC865583, 
  $174617C1, $6DE086C1, $2B80DAEC, $54E4781A, $47F639D7, $CCDB663C, 
  $3B853DF4, $03202992, $401A10C2, $544DB508, $7F004019, $A3B4DCC4, 
  $0E636010, $29FDEC8A, $A8752314, $A8CC2A96, $661F724A, $BB3BC08F, 
  $CA781D29, $1EF7B0D4, $D170DD24, $460ED445, $DD129B9F, $550949C8, 
  $8C216BBD, $61929741, $982F4050, $D664A41B, $B809718D, $40558C1E, 
  $534A4412, $54282DD8, $5EBB61EC, $82AEB151, $76D64F47, $33770B99, 
  $D617A6EF, $49EAE3DD, $B851C2C2, $91BABBE6, $1C3C80AD, $3FEDF7DE, 
  $64487140, $B54E62E1, $5E7DB40A, $FB7D8B77, $FE62ABB1, $120F559E, 
  $7CCD823C, $B632AC8E, $B18EB319, $629CF03E, $7E85E964, $910D9D13, 
  $CC3EE4C1, $002CC1F1, $4777236F, $7161C7E6, $E17EF9B8, $525922ED, 
  $A1BFEC0B, $D8963037, $98356A18, $143E011B, $2582CF6A, $1D141340, 
  $50626BE6, $341F4851, $6C74268F, $187C55C0, $C37030D9, $D6427304, 
  $326DF28F, $363A5417, $D18460D3, $ECB11D95, $07E26C1E, $501E8DDC, 
  $EE9701A8, $741E73D3, $5374E628, $5444BCC0, $277F6298, $098E0684, 
  $40C716BD, $41DA1146, $13932F82, $3E90B400, $1C8F91D0, $7106A239, 
  $BE9B7C89, $C8E1D9E3, $C0A07619, $77CF81F3, $3DA10991, $603871FC, 
  $244C458E, $2FCC30C3, $30AF052D, $F2EEC178, $96D903F8, $F01D9003, 
  $D900288A, $9C60A017, $9C0709F8, $0381A9A6, $14799AAA, $6D4E9C55, 
  $BA41F868, $6C7A0995, $B41C9216, $F39F615E, $12813A61, $FAA8C20D, 
  $2C03345C, $A6DC3742, $7ED62AFB, $2DB4A674, $27353C48, $0A6A647D, 
  $83E1A614, $1C26859E, $E60765F5, $B7EBB8AF, $3409C3F4, $D24C068C, 
  $5F740B7D, $450C4682, $B25E9614, $DBDE126C, $F0D13B31, $1127A989, 
  $029C0F48, $5C1A86C2, $B01DAB27, $E8AE19AE, $B6570FB9, $3E87EBCF, 
  $1C160A3C, $9AF002DE, $C5EAF3A4, $48027E1E, $3CE0AC9E, $09AC9C1A, 
  $E9A6909C, $665BCC3D, $D3DC7C6D, $D1245093, $A88488E7, $0245F569, 
  $B65221E9, $EFF0E35B, $E0ADCE1E, $AFF980B5, $E8A0131E, $F7E28746, 
  $391C328D, $12F13E0E, $A0BEC3F1, $FB7512E2, $C41E95E8, $346EA75F, 
  $2ADAE81C, $28661BC7, $E335DCAF, $20FB1E27, $2E2F86E3, $EDB3D838, 
  $81A4FE96, $1C250030, $873012E6, $9C8F5D21, $D6DE91C1, $23DAE151, 
  $5813F209, $19EA932E, $7BB29B0B, $32EB2C9A, $D11CCC4C, $BAF0E058, 
  $77036A6D, $93297F7C, $B9E006A3, $8724F77E, $82101DA8, $763650DE, 
  $86B440CB, $44C5AC29, $263896D3, $87817796, $2BCF4F70, $6593363B, 
  $DB23A71E, $0DDBFD54, $07D5B6ED, $6A069403, $EC689D79, $2501438B, 
  $2148E6A9, $C5886BC3, $68C8C721, $A93875A5, $D20EA1AA, $24D20235, 
  $40264C10, $805F46D1, $0DCD3BAB, $CFA394EE, $E81D0848, $9B0A680E, 
  $AAA68698, $06A20B4F, $6A81F8AB, $9D506187, $A6D4FC3E, $32B25FC6, 
  $5B070A75, $0C009649, $60F0037C, $4198F999, $1A757D64, $84061B9F, 
  $001FFC9C );
{$ENDIF}
 
implementation
 
end.
