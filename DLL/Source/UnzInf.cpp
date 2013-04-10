#include "stdafx.h"
#pragma hdrstop
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
#include "UnzInf.h"
//---------------------------------------------------------------------------
//#pragma package(smart_init)

#include "UnzSup.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_UNZINF_CPP
//---------------------------------------------------------------------------
//#pragma package(smart_init)

 
/* And'ing with mask_bits[n] masks the lower n bits */
ush const mask_bits[]  =
  {
    0x0000,
    0x0001, 0x0003, 0x0007, 0x000f, 0x001f, 0x003f, 0x007f, 0x00ff,
    0x01ff, 0x03ff, 0x07ff, 0x0fff, 0x1fff, 0x3fff, 0x7fff, 0xffff
  };

UnzInf::UnzInf(const DllCommands *C): UnzSup(C)
{
//#ifdef CRYPT
  fP_flag = 0;
//#endif
  fnopwd = 0;
  fkey = 0;
//  fpwdarg = 0;
  fhufts = 0;

  ffixed_tl = 0;
  ffixed_td = 0;
  ffixed_bl = 0;
  ffixed_bd = 0;
  ffixed_tl64 = 0;
  ffixed_td64 = 0;
  ffixed_bl64 = 0;
  ffixed_bd64 = 0;
  ffixed_tl32 = 0;
  ffixed_td32 = 0;
  ffixed_bl32 = 0;
  ffixed_bd32 = 0;
  fcplens = 0;
  fcplext = 0;
  fcpdext = 0;
  fwp = 0;
  fbb = 0;
  fbk = 0;
//  flrec;
  fPwdReqCount = 3;
//  fnewzip = 0;
}

UnzInf::~UnzInf(void)
{
//  if (fkey && fkey != fpwdarg)
//    StrFree(fkey);
//  StrFree(fpwdarg);
  inflate_free();
}

