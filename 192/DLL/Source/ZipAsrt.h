
#ifndef _ZIP_ASSERT_H_  
#define _ZIP_ASSERT_H_
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
/* Diagnostic functions */
#ifdef DEBUG
#ifdef MSDOS
#undef stderr
#define stderr  stdout
#endif
#define Assert(cond, msg) \
  {                             \
    if (!(cond))              \
      throw DZException(DZM_Error, msg);         \
  }

//            Notify(IERROR, _T("invalid new name %s"), nme);
#define Trace(x)      \
      Notify(DZM_Trace, (x));

#define Tracev(x, y) \
  {                     \
    if (Verbose)  Notify(DZM_Trace, (x), (y)); \
  }

#define Tracevv(x, y)    \
  {                                         \
    if (Verbose > 1) Notify(DZM_Trace, (x), (y)); \
  }

#define Tracec(c, x, y)    \
  {                           \
    if (Verbose && (c))   Notify(DZM_Trace, (x), (y)); \
  }

#define Tracecv(c, x, y)       \
  {                               \
    if (Verbose > 1 && (c)) Notify(DZM_Trace, (x), (y)); \
  }

#else
#define Assert(cond, msg)
#define Trace(x)
#define Tracev(x, y)
#define Tracevv(x, y)
#define Tracec(c, x, y)
#define Tracecv(c, x, y)
#endif

#endif
