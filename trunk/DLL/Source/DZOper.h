//---------------------------------------------------------------------------

#ifndef DZOperH
#define DZOperH
/************************************************************************
 Copyright (C) 2009, 2010  by Russell J. Peters, Roger Aelbrecht,
      Eric W. Engler and Chris Vleghert.

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
#include <sys\stat.h>
#ifndef __BORLANDC__
#define stati64 _stati64
#endif

#include "common.h"
#include "enter.h"
#include "DZFrame.h"
#include <stdarg.h>

/*    Message code format
0FFF FFFF  LLLL LLLL   LLLL MTTT  EEEE EEEE  {31 .. 0}
F = file number (7 bits = 128 files)
L = line number (12 bits=4096 lines)
M = message instead of error string
T = type  (3 bits=8)
E = error/string code (8 bits = 256 errors)
* /

const DZM_MessageBit = 0x800;    // mask for buffer bit
// t = type, e = error
#define DZ_MESSAGE(t, e) ((t&0xF00) | e)
#define DZ_MSG(x) (x & 0xff)
#define DZ_MSGTYP(x) (x & 0x700)
const DZM_General = 0x000;
const DZM_Error   = 0x600;	// 1 1 x (E... is identifier)
const DZM_Warning = 0x400;	// 1 0 x
const DZM_Trace   = 0x300;  // 0 1 1
const DZM_Verbose = 0x100;	// 0 0 1
const DZM_Message = 0x200;  // 0 1 0 (E... is identifier)
*/
class ZFilter
{
    ZFilter();
    ZFilter(const ZFilter&);
    ZFilter operator=(const ZFilter&);
//	ZFilter* fnext;
//    int flevel;       //  only for suffixes
    DZStrW fspec;
public:
    ZFilter(DZStrW &spec);
    ~ZFilter();
    bool __fastcall ISEmpty(void) const;
    ZFilter * __fastcall Find(const DZStrW &spec);

	ZFilter* Next;
	int Level;       //  only for suffixes
//    __property int Level = {read = flevel, write = flevel};
//    __property ZFilter* Next = {read = fnext, write = fnext};
};

class PWRec
{
//    const char* fpw;
//    PWRec *fnext;
    PWRec();
    PWRec(const PWRec &);
    PWRec& operator=(const PWRec&);
    public:
    PWRec(const DZStrA &pw);
    ~PWRec();
	 const char* Passw;
	 PWRec* Next;
//	__property const char* Passw = {read=fpw};
//	__property PWRec* Next = {read=fnext, write=fnext};
};

class BaseRec
{
//    const TCHAR* fbase;
//    BaseRec *fnext;
    BaseRec();
    BaseRec(const BaseRec &);
    BaseRec& operator=(const BaseRec&);
    public:
    BaseRec(const DZStrW &base);
    ~BaseRec();
    DZStrW __fastcall FullPath(const DZStrW& filename) const;
	 const TCHAR* Base;
	 BaseRec* Next;
	inline DZStrW __fastcall GetBaseDir(void) const {return DZStrW(Base);}
//    __property const TCHAR* Base = {read=fbase};
//	__property BaseRec* Next = {read=fnext, write=fnext};
//	__property DZStrW BaseDir = {read=GetBaseDir};
};

class UserCallback;
class DZOp: public DZFrame
{
protected:
    friend class UserCallback;
    HWND global_handle;
    void *global_caller;
    const DllCommands *Command;
    int CallerVersion;
    ZFunctionPtrType callb;  // Function address for callback purposes.
    ZStreamFuncPtrType ZStreamFunc;
    ZInt64 fBytesWritten;
    ZSSArgs *fSS;         // used stream-stream
    unsigned fEncodedAs;
    bool fQuiet;        
    int fdll_handles_errors;
    int fuser_notified_of_abort;
    int fglobal_error_code;
	DZStrW DZErrMsg;
    int fpathput;
public:
	int Verbose;
	bool fNTFSStamps;
	unsigned long fFromPage;      // country to use
	PWRec *CurPW;
	BaseRec *CurBase;
//	bool NoSkipping;   //<<

    UserCallback *CB;
    ZS_Rec ZSData;               // stream op data
    DZOp(const DllCommands *C);
    ~DZOp(void);                                     
    virtual long Exec(const DllCommands *C) = 0;
    virtual int Init(void); // after construction
    void ShowSysMsg(DWORD Error);
	void SendInfo(unsigned err, const DZStrW& info);
	void __cdecl  Notify(unsigned err, const TCHAR* szFmt, ...);
	void GiveTime(void);
	int EraseFile(const DZStrW &Fname, bool safe);
	int __fastcall StreamCB(void);
	const char* AddPW(const DZStrA& pw, bool toFront);
    const BaseRec* AddBase(const DZStrW& base, bool toFront);
	DZStrW __fastcall GetFullPath(const DZStrW &Filename) const;
	DZStrW __fastcall FullPath(const DZStrW &Filename, const BaseRec* base) const;
	void MsgBox(const DZStrW& msg, bool CanCancel);
	int __fastcall Fatal(int err, unsigned flag = 0, bool raise = true);
	int __fastcall DZError(int err, const TCHAR* msg = NULL);
	unsigned __fastcall IsEncoded(ush made,unsigned utf) const;//, unsigned unix);
	bool Skipping(const DZStrW& fn, int err, int typ);
        DZStrW MakeExclFilters(void);
		DZStrW ConvExclFilters(const DZStrW & filters);
DZStrW ex2IntForm(const DZStrW &exname, bool ignore);
protected:
    bool ZStat(const DZStrW& fn, struct stati64 *res);
private:
    DZOp(void);
    DZOp(const DZOp&);        // copy not allowed
    DZOp& operator=(const DZOp&);
    DZStrW lastStatName;

    struct stati64 lastStat;
};

class UserCallback
{
    friend class DZOp;
  private:
    DZOp *Owner;
    ZFunctionPtrType callb;
    DZStrW hold, hold2;
    struct ZCallBackStruct CBData;
    int InItem;
    UserCallback(void);
    UserCallback(const UserCallback&);
    UserCallback& operator=(const UserCallback&);
public:
    inline const char *GetData(void) const {return (const char*)CBData.MsgP;}
    inline void SetData(const char *value) {CBData.MsgP = (const void*)value;}
    inline const char *GetData2(void) const {return (const char*)CBData.MsgP2;}
    inline void SetData2(const char *value) {CBData.MsgP2 = (const void*)value;}
    inline __int64 GetFileSize(void) const {return CBData.FileSize;}
    inline void SetFileSize(const __int64 value) {CBData.FileSize = value;}
    inline __int64 GetWritten(void) const {return CBData.Written;}
    inline void SetWritten(const __int64 value) {CBData.Written = value;}
    inline long GetArg1(void) const {return CBData.Arg1;}
    inline void SetArg1(const long value) {CBData.Arg1 = value;}
    inline unsigned GetArg2(void) const {return CBData.Arg2;}
    inline void SetArg2(const unsigned value) {CBData.Arg2 = value;}
    inline int GetArg3(void) const {return CBData.Arg3;}
    inline void SetArg3(const int value) {CBData.Arg3 = value;}
    DZStrW __fastcall RetMsg(const void *_msg) const;
    DZStrW __fastcall GetMsg(void) const;
    void __fastcall SetMsg(const DZStrW& value);
    DZStrW __fastcall GetMsg2(void) const;
    void __fastcall SetMsg2(const DZStrW& value);
    inline int GetAbort(void) const {return (int)Owner->Abort_Flag;}
	inline void SetAbort(const int value) {Owner->Abort_Flag = (unsigned)value;}
  protected:
    DZStrA __fastcall GetZCmnt(void) const;
//    __property int Abort_Flag = {read=GetAbort, write=SetAbort};
  public:
    UserCallback(DZOp *theOwner, bool OpIsZip);
    ~UserCallback(void);
    int __fastcall UserCB(unsigned Action);
    int __fastcall UserCB(unsigned Action, const DZStrW& msg);
    int UserCB(unsigned Action, const DZStrW& msg, const DZStrW& msg2);
    int __fastcall UserMsg(int err, const DZStrW& msg);
    int __fastcall UserItem(__int64 cnt, const DZStrW& msg);
    int __fastcall UserProgress(__int64 adv);
    int UserXProgress(__int64 adv, int typ);
    int UserXItem(__int64 cnt, int typ, const DZStrW& msg);
    DZStrW UserArg(int arg, int idx, int *cnt);
    DZStrA UserZCmnt(void);
//	__property DZStrW Msg = {read=GetMsg, write=SetMsg};
//	__property DZStrW Msg2 = {read=GetMsg2, write=SetMsg2};
//	__property const char *Data = {read=GetData, write=SetData};
//	__property const char *Data2 = {read=GetData2, write=SetData2};
//	__property __int64 FileSize = {read=GetFileSize, write=SetFileSize};
//	__property __int64 Written = {read=GetWritten, write=SetWritten};
//	__property long Arg1 = {read=GetArg1, write=SetArg1};
//	__property unsigned Arg2 = {read=GetArg2, write=SetArg2};
//    __property int Arg3 = {read=GetArg3, write=SetArg3};
};

                         
DZStrW LastSystemMsg(void);
DZStrW SystemMsg(DWORD Error, const TCHAR* arg1 = NULL);

class SysMsg : public DZStrW
{
    public:
    SysMsg() : DZStrW(LastSystemMsg()){}
    SysMsg(DWORD error): DZStrW(SystemMsg(error, NULL)){}
    SysMsg(DWORD error, const TCHAR* arg1): DZStrW(SystemMsg(error, arg1)){}
private:
    SysMsg& operator=(const SysMsg&) { return *this; }
};


DZOp *MakeZipper(const DllCommands *C);
DZOp *MakeUnzipper(const DllCommands *C);

#endif






