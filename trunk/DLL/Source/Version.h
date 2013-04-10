/* Version.h
* 

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
************************************************************************
*/
#ifndef _VERSION_H
#define _VERSION_H

#define DZ_VER_MAJOR  1
#define DZ_VER_MINOR  9
#define DZ_VER_REVISION  0
#define DZ_VER_BUILD  116

#ifdef _WIN64
   #ifdef _DEBUG
      #define DLLNAME "DelZip64D"
   #else
      #define DLLNAME "DelZip64"
   #endif
   #define _DZ_VER_REVISION  (DZ_VER_REVISION + 1)
#else
   #ifdef _DEBUG
      #define DLLNAME "DelZip190D"
   #else
      #define DLLNAME "DelZip190"
   #endif
   #define _DZ_VER_REVISION  DZ_VER_REVISION
#endif

#define DZ_VER_PRIVATE ((DZ_VER_MAJOR * 1000000) + (DZ_VER_MINOR * 100000) + (_DZ_VER_REVISION * 10000) + DZ_VER_BUILD)
#define DZ_VER_VERSION (DZ_VER_PRIVATE / 10000)

//const char *mdate = __DATE__;

#endif /* _VERSION_H */




