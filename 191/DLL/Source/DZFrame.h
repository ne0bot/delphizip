//---------------------------------------------------------------------------

#ifndef DZFrameH
#define DZFrameH
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
#include "ZStrings.h"

#define ZIPOPER 0x01010101
#define UNZOPER 0x02020202

// abort flag values
#define GA_NONE 0               // no error
#define GA_ERROR 1              // processing error
#define GA_CANCEL 2             // callback signalled cancel
#define GA_ABORT 4              // Abort requested
#define GA_EXCEPT 0x10          // callback caught exception
#define GA_EXCEPT2 0x20         // callback caused exception

class DZFrame
{
    protected:
        OperKeys OpKey;
        int OpTyp;
		const DZFrame* ME;  // validity check
    public:
        unsigned Abort_Flag;
        int GVerbose;
        DZFrame(OperKeys C, int typ);
		virtual ~DZFrame(void);
        bool IsME(const DZFrame* p);
        bool SetME(const DZFrame *Op, int typ);
	protected:
        const TCHAR* TypStr(void) const;
    private:
        DZFrame(void);
        DZFrame(const DZFrame&);        // copy not allowed
        DZFrame& operator=(const DZFrame&);
};

int Set_Oper_Abort(OperKeys key);
void Set_Oper(DZFrame *Op, int typ);

extern void Cleanup_Process(void);
extern int Clear_Oper(DZFrame* Op);
extern int Clear_Oper1(DZFrame* Op);

#endif

