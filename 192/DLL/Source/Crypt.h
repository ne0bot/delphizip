
/* Crypt.h (full version) by Info-ZIP.   Last revised:  [see CR_VERSION_DATE]
*
* This header file is not copyrighted, and non-beta versions may be
* distributed without restriction.
* This version modified by Chris Vleghert for BCB/Delphi Zip.
** distributed under LGPL license ** see license.txt for details

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

#ifndef __crypt_h /* don't include more than once */
#define __crypt_h

#define CR_MAJORVER     2
#define CR_MINORVER     7
#define CR_BETA_VER     ""
#define CR_VERSION_DATE "22 April 1997" /* last public release date */
#define CR_RELEASE

#define PWLEN         80                /* Input buffer size for reading encryption key. */
#define RAND_HEAD_LEN 12                /* Length of encryption random header.           */

typedef unsigned long Keys[3];

int _fastcall decrypt_byte(Keys keys);
int  _fastcall update_keys(int c, Keys keys);
void _fastcall init_keys(const char *passwd, Keys keys);
int _fastcall zencode(int c, Keys keys);
int _fastcall zdecode(int c, Keys keys);
#endif /* !__crypt_h */




