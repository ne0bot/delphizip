;// **** DelZip190 error messages

; Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht

;   This file is part of TZipMaster Version 1.9.

;    TZipMaster is free software: you can redistribute it and/or modify
;    it under the terms of the GNU Lesser General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.

;    TZipMaster is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU Lesser General Public License for more details.

;    You should have received a copy of the GNU Lesser General Public License
;    along with TZipMaster.  If not, see <http://www.gnu.org/licenses/>.

;    contact: problems@delphizip.org (include ZipMaster in the subject).
;    updates: http://www.delphizip.org
;    DelphiZip maillist subscribe at http://www.freelists.org/list/delphizip 

MessageIdTypedef=DWORD

SeverityNames=(Success=0x0:STATUS_SEVERITY_SUCCESS
  Informational=0x1:STATUS_SEVERITY_INFORMATIONAL
  Warning=0x2:STATUS_SEVERITY_WARNING
  Error=0x3:STATUS_SEVERITY_ERROR
  )
  
FacilityNames=(System=0x0:FACILITY_SYSTEM
  Callback=0x1:FACILITY_CALLBACK
  Stream=0x2:FACILITY_STREAM
  Io=0x3:FACILITY_IO
  Global=0x4:FACILTY_GLOBAL
  Zip=0x5:FACILITY_ZIP
  )
  
;//LanguageNames=(English=1:MSG00001)
LanguageNames=(English=0x409:MSG0409)

MessageId=0x41
Severity=Error
Facility=Stream
SymbolicName=DZ_STREAM_NO_OPEN
Language=English
Stream Open failed!
.

MessageId=0x42
Severity=Error
Facility=Stream
SymbolicName=DZ_STREAM_NO_SEEK
Language=English
Stream Seek failed!
.

MessageId=0x43
Severity=Error
Facility=Stream
SymbolicName=DZ_STREAM_NO_READ
Language=English
Stream Read failed!
.

MessageId=0x44
Severity=Error
Facility=Stream
SymbolicName=DZ_STREAM_NO_WRITE
Language=English
Stream Write failed!
.

MessageId=0x45
Severity=Error
Facility=Stream
SymbolicName=DZ_STREAM_NO_IDENTIFY
Language=English
Identify Stream failed!
.

MessageId=0x46
Severity=Error
Facility=Stream
SymbolicName=DZ_STREAM_NOT_OPEN
Language=English
Stream not Open!
.

MessageId=0x50
Severity=Error
Facility=Callback
SymbolicName=DZ_CALLBACK_EXCEPTION
Language=English
Exception in Callback!
.

MessageId=0x00
Severity=Error
Facility=Global
SymbolicName=DZ__GOOD
Language=English
Success!
.

MessageId=0x01
Severity=Error
Facility=Global
SymbolicName=DZ__CANCELLED
Language=English
Cancelled!
.

MessageId=0x02
Severity=Error
Facility=Global
SymbolicName=DZ__ABORT
Language=English
Aborted by User!
.

MessageId=0x03
Severity=Error
Facility=Global
SymbolicName=DZ__CALLBACK_EXCEPTION
Language=English
Exception in Callback!
.

MessageId=0x04
Severity=Error
Facility=Global
SymbolicName=DZ__MEMORY
Language=English
No Available Memory!
.

MessageId=0x05
Severity=Error
Facility=Global
SymbolicName=DZ__INVALID_STRUCTURE
Language=English
Invalid Structure supplied!
.

MessageId=0x06
Severity=Error
Facility=Global
SymbolicName=DZ__FATAL
Language=English
Fatal Error!
.

MessageId=0x07
Severity=Error
Facility=Global
SymbolicName=DZ__PASSWORD_FAIL
Language=English
Password failed!
.

MessageId=0x08
Severity=Error
Facility=Global
SymbolicName=DZ__PASSWORD_CANCEL
Language=English
Password cancelled!
.

MessageId=0x09
Severity=Error
Facility=Global
SymbolicName=DZ__INVAL_ZIP
Language=English
Invalid zip structure!
.

MessageId=0x0A
Severity=Error
Facility=Global
SymbolicName=DZ__NO_CENTRAL
Language=English
No Central directory!
.

MessageId=0x0B
Severity=Error
Facility=Global
SymbolicName=DZ__ZIP_EOF
Language=English
Unexpected end of Zip file!
.

MessageId=0x0C
Severity=Error
Facility=Global
SymbolicName=DZ__DATA_END
Language=English
Premature end of file!
.

MessageId=0x0D
Severity=Error
Facility=Global
SymbolicName=DZ__ZIP_NOOPEN
Language=English
Error opening Zip file!
.

MessageId=0x0E
Severity=Error
Facility=Global
SymbolicName=DZ__ZIP_MULTI
Language=English
Multi-part Zips not supported!
.

MessageId=0x0F
Severity=Error
Facility=Global
SymbolicName=DZ__NOT_FOUND
Language=English
File not found!
.

MessageId=0x10
Severity=Error
Facility=Global
SymbolicName=DZ__LOGIC_ERROR
Language=English
Internal logic error!
.

MessageId=0x11
Severity=Error
Facility=Global
SymbolicName=DZ__NOTHING_TO_DO
Language=English
Nothing to do!
.

MessageId=0x12
Severity=Error
Facility=Global
SymbolicName=DZ__BAD_OPTIONS
Language=English
Bad Options specified!
.

MessageId=0x13
Severity=Error
Facility=Global
SymbolicName=DZ__TEMP_FAILED
Language=English
Temporary file failure!
.

MessageId=0x14
Severity=Error
Facility=Global
SymbolicName=DZ__NO_FILE_PERMISSION
Language=English
File not found or no permission!
.

MessageId=0x15
Severity=Error
Facility=Global
SymbolicName=DZ__ERROR_READ
Language=English
Error reading file!
.

MessageId=0x16
Severity=Error
Facility=Global
SymbolicName=DZ__ERROR_CREATE
Language=English
Error creating file!
.

MessageId=0x17
Severity=Error
Facility=Global
SymbolicName=DZ__ERROR_WRITE
Language=English
Error writing file!
.

MessageId=0x18
Severity=Error
Facility=Global
SymbolicName=DZ__ERROR_SEEK
Language=English
Error seeking in file!
.

MessageId=0x19
Severity=Error
Facility=Global
SymbolicName=DZ__EMPTY_ZIP
Language=English
Missing or empty zip file!
.

MessageId=0x1A
Severity=Error
Facility=Global
SymbolicName=DZ__INVAL_NAME
Language=English
Invalid characters in filename!
.

MessageId=0x1B
Severity=Error
Facility=Global
SymbolicName=DZ__GENERAL
Language=English
Error:
.

MessageId=0x1C
Severity=Error
Facility=Global
SymbolicName=DZ__MISS
Language=English
Nothing Found
.

MessageId=0x1D
Severity=Error
Facility=Global
SymbolicName=DZ__WARNING
Language=English
Warning:
.

MessageId=0x1E
Severity=Error
Facility=Global
SymbolicName=DZ__ERROR_DELETE
Language=English
Error Deleting file!
.

MessageId=0x1F
Severity=Error
Facility=Global
SymbolicName=DZ__FATAL_IMPORT
Language=English
Fatal Error - could not import symbol!
.

MessageId=0x20
Severity=Error
Facility=Global
SymbolicName=DZ__SKIPPING
Language=English
Skipping:
.

MessageId=0x21
Severity=Error
Facility=Global
SymbolicName=DZ__LOCKED
Language=English
File Locked!
.

MessageId=0x22
Severity=Error
Facility=Global
SymbolicName=DZ__DENIED
Language=English
Access Denied!
.

MessageId=0x23
Severity=Error
Facility=Zip
SymbolicName=DZ__DUPNAME
Language=English
Duplicated Name!
.
