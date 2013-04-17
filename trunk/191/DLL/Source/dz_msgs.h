// **** DelZip190 error messages
/*************************************************************************
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
//LanguageNames=(English=1:MSG00001)
//
//  Values are 32 bit values layed out as follows:
//
//   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
//   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
//  +---+-+-+-----------------------+-------------------------------+
//  |Sev|C|R|     Facility          |               Code            |
//  +---+-+-+-----------------------+-------------------------------+
//
//  where
//
//      Sev - is the severity code
//
//          00 - Success
//          01 - Informational
//          10 - Warning
//          11 - Error
//
//      C - is the Customer code flag
//
//      R - is a reserved bit
//
//      Facility - is the facility code
//
//      Code - is the facility's status code
//
//
// Define the facility codes
//
#define FACILITY_ZIP                     0x5
#define FACILITY_SYSTEM                  0x0
#define FACILITY_STREAM                  0x2
#define FACILITY_IO                      0x3
#define FACILTY_GLOBAL                   0x4
#define FACILITY_CALLBACK                0x1


//
// Define the severity codes
//
#define STATUS_SEVERITY_WARNING          0x2
#define STATUS_SEVERITY_SUCCESS          0x0
#define STATUS_SEVERITY_INFORMATIONAL    0x1
#define STATUS_SEVERITY_ERROR            0x3


//
// MessageId: DZ_STREAM_NO_OPEN
//
// MessageText:
//
//  Stream Open failed!
//
#define DZ_STREAM_NO_OPEN                ((DWORD)0xC0020041L)

//
// MessageId: DZ_STREAM_NO_SEEK
//
// MessageText:
//
//  Stream Seek failed!
//
#define DZ_STREAM_NO_SEEK                ((DWORD)0xC0020042L)

//
// MessageId: DZ_STREAM_NO_READ
//
// MessageText:
//
//  Stream Read failed!
//
#define DZ_STREAM_NO_READ                ((DWORD)0xC0020043L)

//
// MessageId: DZ_STREAM_NO_WRITE
//
// MessageText:
//
//  Stream Write failed!
//
#define DZ_STREAM_NO_WRITE               ((DWORD)0xC0020044L)

//
// MessageId: DZ_STREAM_NO_IDENTIFY
//
// MessageText:
//
//  Identify Stream failed!
//
#define DZ_STREAM_NO_IDENTIFY            ((DWORD)0xC0020045L)

//
// MessageId: DZ_STREAM_NOT_OPEN
//
// MessageText:
//
//  Stream not Open!
//
#define DZ_STREAM_NOT_OPEN               ((DWORD)0xC0020046L)

//
// MessageId: DZ_CALLBACK_EXCEPTION
//
// MessageText:
//
//  Exception in Callback!
//
#define DZ_CALLBACK_EXCEPTION            ((DWORD)0xC0010050L)

//
// MessageId: DZ__GOOD
//
// MessageText:
//
//  Success!
//
#define DZ__GOOD                         ((DWORD)0xC0040000L)

//
// MessageId: DZ__CANCELLED
//
// MessageText:
//
//  Cancelled!
//
#define DZ__CANCELLED                    ((DWORD)0xC0040001L)

//
// MessageId: DZ__ABORT
//
// MessageText:
//
//  Aborted by User!
//
#define DZ__ABORT                        ((DWORD)0xC0040002L)

//
// MessageId: DZ__CALLBACK_EXCEPTION
//
// MessageText:
//
//  Exception in Callback!
//
#define DZ__CALLBACK_EXCEPTION           ((DWORD)0xC0040003L)

//
// MessageId: DZ__MEMORY
//
// MessageText:
//
//  No Available Memory!
//
#define DZ__MEMORY                       ((DWORD)0xC0040004L)

//
// MessageId: DZ__INVALID_STRUCTURE
//
// MessageText:
//
//  Invalid Structure supplied!
//
#define DZ__INVALID_STRUCTURE            ((DWORD)0xC0040005L)

//
// MessageId: DZ__FATAL
//
// MessageText:
//
//  Fatal Error!
//
#define DZ__FATAL                        ((DWORD)0xC0040006L)

//
// MessageId: DZ__PASSWORD_FAIL
//
// MessageText:
//
//  Password failed!
//
#define DZ__PASSWORD_FAIL                ((DWORD)0xC0040007L)

//
// MessageId: DZ__PASSWORD_CANCEL
//
// MessageText:
//
//  Password cancelled!
//
#define DZ__PASSWORD_CANCEL              ((DWORD)0xC0040008L)

//
// MessageId: DZ__INVAL_ZIP
//
// MessageText:
//
//  Invalid zip structure!
//
#define DZ__INVAL_ZIP                    ((DWORD)0xC0040009L)

//
// MessageId: DZ__NO_CENTRAL
//
// MessageText:
//
//  No Central directory!
//
#define DZ__NO_CENTRAL                   ((DWORD)0xC004000AL)

//
// MessageId: DZ__ZIP_EOF
//
// MessageText:
//
//  Unexpected end of Zip file!
//
#define DZ__ZIP_EOF                      ((DWORD)0xC004000BL)

//
// MessageId: DZ__DATA_END
//
// MessageText:
//
//  Premature end of file!
//
#define DZ__DATA_END                     ((DWORD)0xC004000CL)

//
// MessageId: DZ__ZIP_NOOPEN
//
// MessageText:
//
//  Error opening Zip file!
//
#define DZ__ZIP_NOOPEN                   ((DWORD)0xC004000DL)

//
// MessageId: DZ__ZIP_MULTI
//
// MessageText:
//
//  Multi-part Zips not supported!
//
#define DZ__ZIP_MULTI                    ((DWORD)0xC004000EL)

//
// MessageId: DZ__NOT_FOUND
//
// MessageText:
//
//  File not found!
//
#define DZ__NOT_FOUND                    ((DWORD)0xC004000FL)

//
// MessageId: DZ__LOGIC_ERROR
//
// MessageText:
//
//  Internal logic error!
//
#define DZ__LOGIC_ERROR                  ((DWORD)0xC0040010L)

//
// MessageId: DZ__NOTHING_TO_DO
//
// MessageText:
//
//  Nothing to do!
//
#define DZ__NOTHING_TO_DO                ((DWORD)0xC0040011L)

//
// MessageId: DZ__BAD_OPTIONS
//
// MessageText:
//
//  Bad Options specified!
//
#define DZ__BAD_OPTIONS                  ((DWORD)0xC0040012L)

//
// MessageId: DZ__TEMP_FAILED
//
// MessageText:
//
//  Temporary file failure!
//
#define DZ__TEMP_FAILED                  ((DWORD)0xC0040013L)

//
// MessageId: DZ__NO_FILE_PERMISSION
//
// MessageText:
//
//  File not found or no permission!
//
#define DZ__NO_FILE_PERMISSION           ((DWORD)0xC0040014L)

//
// MessageId: DZ__ERROR_READ
//
// MessageText:
//
//  Error reading file!
//
#define DZ__ERROR_READ                   ((DWORD)0xC0040015L)

//
// MessageId: DZ__ERROR_CREATE
//
// MessageText:
//
//  Error creating file!
//
#define DZ__ERROR_CREATE                 ((DWORD)0xC0040016L)

//
// MessageId: DZ__ERROR_WRITE
//
// MessageText:
//
//  Error writing file!
//
#define DZ__ERROR_WRITE                  ((DWORD)0xC0040017L)

//
// MessageId: DZ__ERROR_SEEK
//
// MessageText:
//
//  Error seeking in file!
//
#define DZ__ERROR_SEEK                   ((DWORD)0xC0040018L)

//
// MessageId: DZ__EMPTY_ZIP
//
// MessageText:
//
//  Missing or empty zip file!
//
#define DZ__EMPTY_ZIP                    ((DWORD)0xC0040019L)

//
// MessageId: DZ__INVAL_NAME
//
// MessageText:
//
//  Invalid characters in filename!
//
#define DZ__INVAL_NAME                   ((DWORD)0xC004001AL)

//
// MessageId: DZ__GENERAL
//
// MessageText:
//
//  Error:
//
#define DZ__GENERAL                      ((DWORD)0xC004001BL)

//
// MessageId: DZ__MISS
//
// MessageText:
//
//  Nothing Found
//
#define DZ__MISS                         ((DWORD)0xC004001CL)

//
// MessageId: DZ__WARNING
//
// MessageText:
//
//  Warning:
//
#define DZ__WARNING                      ((DWORD)0xC004001DL)

//
// MessageId: DZ__ERROR_DELETE
//
// MessageText:
//
//  Error Deleting file!
//
#define DZ__ERROR_DELETE                 ((DWORD)0xC004001EL)

//
// MessageId: DZ__FATAL_IMPORT
//
// MessageText:
//
//  Fatal Error - could not import symbol!
//
#define DZ__FATAL_IMPORT                 ((DWORD)0xC004001FL)

//
// MessageId: DZ__SKIPPING
//
// MessageText:
//
//  Skipping:
//
#define DZ__SKIPPING                     ((DWORD)0xC0040020L)

//
// MessageId: DZ__LOCKED
//
// MessageText:
//
//  File Locked!
//
#define DZ__LOCKED                       ((DWORD)0xC0040021L)

//
// MessageId: DZ__DENIED
//
// MessageText:
//
//  Access Denied!
//
#define DZ__DENIED                       ((DWORD)0xC0040022L)

//
// MessageId: DZ__DUPNAME
//
// MessageText:
//
//  Duplicated Name!
//
#define DZ__DUPNAME                      ((DWORD)0xC0050023L)

