#include "stdafx.h"
#pragma hdrstop

#include "common.h"
#include "zstrings.h"
#include "DZFrame.h"
#include "enter.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_DZ_FRAME_CPP

/* DZFrame.cpp *

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

DZFrame::DZFrame(OperKeys C, int typ)
{
    OpTyp = typ;
	ME = 0;         // only set when 'attached'
    Abort_Flag = 0;
    OpKey = (OperKeys)C;
}

DZFrame::~DZFrame(void)
{
//
}

bool DZFrame::IsME(const DZFrame *p)
{
    return ME == p && (OpTyp == ZIPOPER || OpTyp == UNZOPER);
}

bool DZFrame::SetME(const DZFrame *Op, int typ)
{
    if (Op == this && (typ == ZIPOPER || typ == UNZOPER))
    {
        ME = Op;
        OpTyp = typ;
        return true;
    }

	ME = NULL;
    OpTyp = 0;

	return false;
}

const TCHAR* DZFrame::TypStr(void) const
{
    return OpTyp == ZIPOPER ? _T("Zip") : _T("Unzip");
}



