#ifndef __DLLENTER_H
#define __DLLENTER_H
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
typedef unsigned long OperKeys;
#define NOT_ASSIGNED_OPER ((OperKeys)(unsigned)-1)

struct OpFrame
{
  int Abort_Flag;
  OperKeys OpKey;
  struct OpFrame* Prev;
  struct OpFrame* ME;  // validity check
};

// TLS data at index is head of list of OpFrame
//extern DWORD TgbIndex; // RCV added, our only non-constant global.

int Add_Oper(struct OpFrame* Op);
int Remove_Oper(struct OpFrame* Op);
int Set_Oper_Abort(OperKeys key);

struct OpFrame* Get_Current_Oper(void);

extern int Init_Process(void);
extern int Clear_Oper(struct OpFrame* Op);

#endif


