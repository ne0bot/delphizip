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
#include <stdio.h>
#include <shellapi.h>
#include <sys\stat.h>
//#ifndef __BORLANDC__
//#define stati64 _stati64
//#endif
#include "common.h"
#include "ZStrings.h"
#include "dzframe.h"
#include "DZOper.h"
#include "version.h"
#include "helpers.h"
#include "dz_errs.h"

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_DZOPER_CPP
//---------------------------------------------------------------------------

DZStrW SystemMsg(DWORD Error, const TCHAR* arg1)
{
    LPVOID lpMsgBuf;

    DWORD flags = FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM;
    void *mdl = 0;

    if (Error & (DWORD)0x40000000L)
	{
        flags = FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_HMODULE;
        mdl = ModuleInst;
    }                           
#if defined(UNICODE) && defined(ALLOW_WIN98)
    FormatMessageA(flags, mdl, Error, 0, (LPSTR) &lpMsgBuf, 1000, NULL);  
    DZStrW t((const char *)lpMsgBuf);
#else
    FormatMessage(flags, mdl, Error, 0, (LPTSTR) &lpMsgBuf, 1000, NULL); 
    DZStrW t((const TCHAR *)lpMsgBuf);
#endif
    if (t[t.Length() -2] == '\r')
		t.Delete(t.Length()-2, 2);

    // Free the buffer.
    LocalFree(lpMsgBuf);
    DZStrW tmp;
	tmp.Format(t.c_str(), arg1);

	return tmp;
}

DZStrW LastSystemMsg(void)
{
    DWORD err = GetLastError();
    return SystemMsg(err);
}

void DZOp::SendInfo(unsigned err, const DZStrW& info)
{
	unsigned int     typ = 0;
	DZStrW buf;

    if (err > 0)
    {
        typ = DZ_MSGTYP(err);

        if (typ == DZM_Trace)
            buf = _T("Trace: ");
        else
            if (typ == DZM_Verbose)
                buf = _T("Info: ");

        buf += info;
    }
    else
        buf = info;

//#ifdef DZ_LOGGING
//    if (LogName && *(LogName))
//        LogString(err, buf);
//#endif
    CB->UserMsg((int)err, buf);

    if ((IERROR & typ) && Verbose < -10)
    {
        Verbose++;
        MsgBox(buf, true);
    }
}

DZOp::DZOp(const DllCommands *C): DZFrame((OperKeys)C,
                C->fOptions.OpIsZip || C->fOptions.OpIsZip), Command(C)
{
    Verbose = C->fVerbosity;

    if (Verbose > 1)
        Verbose = -1;

    fEncodedAs = C->fEncodedAs;
    fQuiet = C->fOptions.Quiet;
    fglobal_error_code = 0;
    fdll_handles_errors = 0;
    fuser_notified_of_abort = 0;
	CallerVersion = C->fVersion;
    fNTFSStamps = C->fOptions.NTFSStamps && IsNTorAbove;

    fFromPage = C->fFromPage;
    fSS = C->fSS;         // used stream-stream
	Abort_Flag = 0;
//	NoSkipping = C->fOptions.NoSkip;   //<<

    // Copy the window handle and context of caller to our global vars
    global_handle = C->fHandle;
    global_caller = C->fCaller;

    // point to the C++Builder/Delphi callback function
    callb = C->ZCallbackFunc;
    ZStreamFunc = C->ZStreamFunc;
    ZSData.Check = STREAM_CHECK;
    ZSData.Caller = global_caller; // object instance pointer of caller
    CB = new UserCallback(this, C->fOptions.OpIsZip || C->fOptions.OpIsZip);

    CurPW = NULL;
    CurBase = NULL;
    fBytesWritten = 0;
}

DZOp::~DZOp(void)
{

    PWRec *tp, *tmpp = CurPW;
    CurPW = NULL;

    while (tmpp)
    {
        tp = tmpp;
        tmpp = tmpp->Next;
        delete tp;
    }

    BaseRec *tb, *tmpb = CurBase; 
    CurBase = NULL;

    while (tmpb)
    {
        tb = tmpb;
        tmpb = tmpb->Next;
        delete tb;
    }

    delete CB;
}

int DZOp::Init(void) // after construction
{
    // do active initialisation    
    if (Command->fVersion != DZ_VER_VERSION)
    {
        // see version.h
        DZStrW tmp;
		tmp.Format(_T("Warning: %s version 1.9.%d.%d is %s than required (%i.%i.%i) - Please use correct version!"),
			_T(DLLNAME), (DZ_VER_PRIVATE/10000)%10, DZ_VER_PRIVATE%1000,
			(Command->fVersion < DZ_VER_VERSION) ? _T("newer") : _T("older"),
			Command->fVersion/100, (Command->fVersion/10)%10, Command->fVersion%10);
//		tmp.Format(_T("Warning: %s version %s is %s than required (%i.%i.%i) - Please use correct version!"),
//			_T(DLLNAME), _T(DZ_VER), (Command->fVersion < DZ_VER_VERSION) ? _T("newer") : _T("older"),
//            Command->fVersion / 100, (Command->fVersion / 10) % 10, Command->fVersion % 10);

        // This message won't appear if user did pass us a zero Window handle.
        if (!Command->fOptions.Quiet)
            MsgBox(tmp, true);

        // Also report this fact via the C++Builder/Delphi callback.
		Notify(IERROR, tmp.c_str());
        return DZ_ERM_BAD_OPTIONS;
    }
    return 0;
}

void DZOp::ShowSysMsg(DWORD Error)
{
    if (Verbose)
    {
        Notify(0, _T("System Error (%lX) %s"), Error, SystemMsg(Error).c_str());
    }
}

// If we do not have a full path then FOF_ALLOWUNDO does not work!
int DZOp::EraseFile(const DZStrW &Fname, bool safe)
{                             
    SHFILEOPSTRUCT  fop;
    TCHAR           *sb;
    int             ls,
    r;
    if (Fname.IsEmpty())  
        return ENOENT;

    if (Verbose < 0)
        Notify(IVERBOSE, _T("destroying '%s'"), Fname.c_str());
    else
      CB->UserCB(zacTick);  // take a little time
    if (GetFileAttrs(Fname) == (DWORD) - 1)
    {
        if (Verbose < 0)
            Notify(IVERBOSE, _T("did not exist '%s'"), Fname.c_str());
        return DZ_ERR_GOOD;
    }
    fop.hwnd = global_handle;
    fop.wFunc = FO_DELETE;
    fop.pTo = NULL;
    fop.fFlags = FOF_NOCONFIRMATION | FOF_NOCONFIRMMKDIR | FOF_NORECURSION;
    if (safe)
        fop.fFlags |= FOF_ALLOWUNDO;
    fop.fAnyOperationsAborted = 0;
    fop.hNameMappings = NULL;
    fop.lpszProgressTitle = NULL;
    DZStrW tmp(Fname);
	ls = (int)tmp.length();
    sb = tmp.GetBuffer(ls + 2);
    sb[ls + 1] = 0;
    fop.pFrom = sb;
    r = SHFileOperation(&fop);
    if (r)
    {
        Notify(DZ_ERM_TEMP_FAILED, _T(" DestroyFile: Delete failed [%X]"), GetLastError());
        return -1;
    }

    return DZ_ERR_GOOD;
}

// cached call to 'stat' - return true if not found
bool DZOp::ZStat(const DZStrW &fn, struct stati64 *res)
{
	if (!lastStatName.IsEmpty() && ZMatch(lastStatName, fn))
//	if (!fn.Compare(lastStatName.c_str()))
	{
        memcpy(res, &lastStat, sizeof(struct stati64));
		return false;
    }

    lastStatName.empty();

    bool ok;

	int drv = Is_Drv(fn);
    if (drv < 0)
    {
		memset(res, 0, sizeof(struct stati64));
        ZSData.Number = (-drv) - 2;   // stream number
        ZSData.OpCode = zsaIdentify;
        int r = StreamCB();
        ok = (r == CALLBACK_TRUE);

        if (ok)
        {
            res->st_size = ZSData.ArgLL;
            res->st_atime = res->st_mtime = res->st_ctime = dos2unixtime(ZSData.ArgD);
			long attr = (long)ZSData.ArgA;
            res->st_mode = S_IREAD;

            if (attr & FILE_ATTRIBUTE_DIRECTORY)
                res->st_mode |= (S_IFDIR | S_IWRITE | S_IEXEC) ;
            else
			{
#ifdef _WIN64
				res->st_mode |= (unsigned short)S_IFREG;
#else
				res->st_mode |= (mode_t)S_IFREG;
#endif
                if ((attr & FILE_ATTRIBUTE_READONLY) == 0)
                    res->st_mode |= S_IWRITE;
            }

            res->st_nlink = 1;
        }
    }
    else
		ok = !_tstati64(fn.c_str(), res);

    if (ok)
    {
        lastStatName = fn;
        memcpy(&lastStat, res, sizeof(struct stati64));
    }

    return ok ? false : true;
}

void DZOp::GiveTime(void)
{
    SYSTEMTIME SystemT;

    if (!Verbose)
        return;

    GetSystemTime(& SystemT);

    Notify(IVERBOSE, _T("Time Hour %d, min %d, sec %d msec %d"),
           SystemT.wHour, SystemT.wMinute, SystemT.wSecond, SystemT.wMilliseconds);
}


const char* DZOp::AddPW(const DZStrA& pw, bool toFront)
{
    PWRec *prv, *t, *root = CurPW;
    prv = NULL;
    t = root;
	// find it
    while (t)
    {
        if (!t->Passw && pw.IsEmpty())
            break;   // same

        if (t->Passw && !pw.Compare(t->Passw))
            break;   // found

        prv = t;
        t = t->Next;
    }

    if (!t)
    {
        // new entry
        t = new PWRec(pw);

        if (toFront || !prv)
        {
            t->Next = root;
			CurPW = t;
        }
        else
            prv->Next = t;  // append it
    }
    else
        if (toFront && prv)
        {
            // move it
            prv->Next = t->Next;
            t->Next = root;
			CurPW = t;
        }

    return t->Passw;
}

const BaseRec* DZOp::AddBase(const DZStrW& base, bool toFront)
{
    DZStrW tmp = base;

    if (tmp.LastChar() != BSLASH)
        tmp += BSLASH;

    BaseRec *prv, *t, *root = CurBase;
    prv = NULL;
    t = root;
    // find it
    while (t)
    {
        if (!tmp.CompareNoCase(t->Base))
            break;   // found

        prv = t; 
        t = t->Next;
    }

    if (!t)
    {
        // new entry
        t = new BaseRec(tmp);

        if (toFront || !prv)
        {
            t->Next = root;
            CurBase = t;
//            root = t;
        }
        else
            prv->Next = t;  // append it
    }
    else
        if (toFront && prv)
        {
            // move it
            prv->Next = t->Next;
            t->Next = root;
			CurBase = t;
        }

	return (const BaseRec*)t;
}

DZStrW __fastcall DZOp::FullPath(const DZStrW &Filename, const BaseRec* base) const
{
    return base->FullPath(Filename);
}

DZStrW __fastcall DZOp::GetFullPath(const DZStrW &Filename) const
{
    return CurBase->FullPath(Filename);
}

/* ===================================================================== */
void DZOp::MsgBox(const DZStrW& msg, bool CanCancel)
{
    DZStrW tmp;
	tmp.Format(_T("Message From DelZip dll (%s)"), TypStr());
    HWND wHandle = global_handle;

    /* Did the user pass us a good window handle? if not, we can't pop-up a box. */
    if (!wHandle)
        wHandle = GetDesktopWindow(); // v1.6021

    unsigned int flg = CanCancel ? MB_OKCANCEL | MB_ICONEXCLAMATION : MB_OK;

    /* bring up a dialog box */
    if (MessageBox(wHandle, msg.c_str(), tmp.c_str(), flg) == IDCANCEL && CanCancel)
        Abort_Flag |= GA_CANCEL;  // abort
}


int __fastcall DZOp::DZError(int err, const TCHAR *msg)
{
    DZStrW errmsg;

    if (fuser_notified_of_abort)
        return err;

    fuser_notified_of_abort = 1;
    int eno = err & 0xff;
    fglobal_error_code = eno;
    if (fdll_handles_errors)
    {
        /* I'm passing the error via the callback just to get it logged in
        * the status box - I'm sending it in with a 0 error code to avoid
        * a dialog box from the application program. */
		if ((ulg)err & DZM_MessageBit)
        {
            if (msg)
                DZErrMsg = msg;

            errmsg = DZErrMsg;
        }
        else    // load from resources
			errmsg = SystemMsg((DZ__GOOD + (ulg)eno), msg);

		Notify(0, errmsg.c_str());
        errmsg.AppendFormat(_T("  code=%x"), err);
		MsgBox(errmsg, false);
    }
	else
    {
		if ((ulg)err & (int)DZM_MessageBit)
            errmsg = DZErrMsg;

		Notify((unsigned)err, errmsg.c_str());
    }

    /* Only application program handles errors. */
    return err;
}

//#define DZ_ERR_CANCELLED  1
//#define DZ_ERR_ABORT   2
//#define DZ_ERR_CALLBACK  3
//// abort flag values
//#define GA_NONE 0               // no error
//#define GA_ERROR 1              // processing error
//#define GA_CANCEL 2             // callback signalled cancel
//#define GA_ABORT 4              // Abort requested
//#define GA_EXCEPT 0x10          // callback caught exception
//#define GA_EXCEPT2 0x20         // callback caused exception
int __fastcall DZOp::Fatal(int err, unsigned flag, bool raise)
{
    const TCHAR* errs[3] = {_T("User cancelled"), _T("User Abort"), _T("Callback exception")};
    const TCHAR* erm = 0;
    int e = DZ_ERR(err);
    if (e && e <= DZ_ERR_CALLBACK)
    {
        if (e <= DZ_ERR_CALLBACK)
        {
            if (!flag)
                flag = Abort_Flag;
            e = DZ_ERR_CANCELLED;
            if (flag & 4)
                e = DZ_ERR_ABORT;
            if (flag > 4)
                e = DZ_ERR_CALLBACK;
            err &= ~3;
            err |= e;
            erm = errs[--e];
        }
    }
    DZError(err, erm);
    if (raise)
        throw DZException(err, erm);
    return err;
}

/* ===========================================================================
 * This calls the application program and passes status info.
 */
int __fastcall DZOp::StreamCB(void)
{
	int ret = 0;

    if (ZStreamFunc)
    {
        ZSData.Check = STREAM_CHECK;
        ZSData.Caller = global_caller; // object instance pointer of caller
        try
        {
            ret = ZStreamFunc(&ZSData);  // call user's program
        }
		catch(...)
        {
            Abort_Flag = GA_EXCEPT2;
        }
    }

	return ret;
}

bool DZOp::Skipping(const DZStrW& fn, int err, int typ)
{

	CB->SetArg1(err);
	CB->SetArg2((unsigned)typ);
	return CB->UserCB(zacSkipped, fn) == CALLBACK_TRUE;
//	int usr = CB->UserCB(zacSkipped, fn);
//	return /*NoSkipping ? true :*/ usr == CALLBACK_TRUE;   //<<
}

/*
  Encoded as OEM for
	DOS (default)                       FS_FAT
	OS/2                                FS_HPFS
	Win95/NT with Nico Mak's WinZip     FS_NTFS && host = 5.0
  UTF8 is flag is set
  except (someone always has to be different)
	PKZIP (Win) 2.5, 2.6, 4.0 - mark as FS_FAT but local is Windows ANSI (1252)
	PKZIP (Unix) 2.51 - mark as FS_FAT but are current code page
*/
unsigned __fastcall DZOp::IsEncoded(ush made, unsigned utf) const
{
const int WZIP = (FS_NTFS * 256) + 50;
	if (fEncodedAs == zeoAuto)
	{
		if (utf)
			return zeoUTF8;

		if (!(made & 0xff00) || (made & 0xff00) == 0x0600 || made == WZIP)
			return zeoOEM; // FAT OS/2 or WinZip 5.0

		return zeoNone;
	}

	return fEncodedAs;
}

ZFilter::ZFilter(DZStrW &spec)
{
    fspec = spec;
	Level = 0;
	Next = NULL;
}

ZFilter::~ZFilter()
{
	if (Next)
        delete Next;
}

bool __fastcall ZFilter::ISEmpty(void) const
{
    return fspec.IsEmpty();
}

ZFilter * __fastcall ZFilter::Find(const DZStrW &spec)
{
    ZFilter *l = this;
    if (!l || spec.IsEmpty())
        return NULL;
    while (l)
    {
        if (ZMatch(l->fspec, spec))
            return l;
        l = l->Next;
    }
    return NULL;
}


PWRec::PWRec(const DZStrA &pw)
{
	Passw = DupStr(pw);
	Next = NULL;
}

PWRec::~PWRec()
{
    delete[] Passw;//fpw;
}

BaseRec::BaseRec(const DZStrW &base)
{
    DZStrW tmp(base);
    TCHAR lc = tmp.LastChar();

    if (lc != _T(':') && lc != BSLASH)
        tmp += BSLASH;

	Base = DupStr(tmp);

	Next = NULL;
}

BaseRec::~BaseRec()
{
    delete[] Base;
}

DZStrW __fastcall BaseRec::FullPath(const DZStrW& filename) const
{
    if (!Is_DrvEx(filename))
    {
        DZStrW tmp(Base);
        tmp += filename;
        return tmp;
    }

    return filename;
}

UserCallback::UserCallback(DZOp *theOwner, bool OpIsZip) : Owner(theOwner)
{
    callb = Owner->callb;
    CBData.HaveWide = 0;
    CBData.Caller = Owner->global_caller; // object instance pointer of caller
    CBData.Version = DZ_VER_VERSION;
    CBData.IsOperationZip = OpIsZip;
    InItem = 0;
}

UserCallback::~UserCallback(void)
{

}

DZStrA __fastcall UserCallback::GetZCmnt(void) const
{
    DZStrA tmp;
    if (CBData.HaveWide == 0)
    {
        const char* p = (const char*)CBData.MsgP;
        if (p && *p)
		{
            char *buf = tmp.GetBuffer(CBData.Arg1);
			memmove(buf, p, (size_t)CBData.Arg1);
            tmp.ReleaseBuffer(CBData.Arg1);
        }
    }
    return tmp;
}

DZStrW __fastcall UserCallback::RetMsg(const void *_msg) const
{
    DZStrW tmp;
    if (CBData.HaveWide == 1)
    {
        const wchar_t* wp = (const wchar_t*)_msg;
        if (wp && *wp)
            tmp = wp;
        return tmp;
    }
    const char* p = (const char*)_msg;
    if (p && *p)
    {
        if (CBData.HaveWide == 2)
			tmp = DZStrW(CP_UTF8, p, -1);
//            tmp = UTF8ToStr(p, -1);
        else
            tmp = p;
    }
    return tmp;
}

DZStrW __fastcall UserCallback::GetMsg(void) const
{
    return RetMsg(CBData.MsgP);
}

void __fastcall UserCallback::SetMsg(const DZStrW& value)
{
    hold = value;
    CBData.MsgP = hold.c_str();
#ifdef UNICODE
    CBData.HaveWide = 1;
#else
    CBData.HaveWide = 0;
#endif
}

DZStrW __fastcall UserCallback::GetMsg2(void) const
{
    return RetMsg(CBData.MsgP2);
}

void __fastcall UserCallback::SetMsg2(const DZStrW& value)
{
    hold2 = value;
    CBData.MsgP2 = hold2.c_str();
#ifdef UNICODE
    CBData.HaveWide = 1;
#else
    CBData.HaveWide = 0;
#endif
}


/* This provides the calling program with updated info on what the DLL
* is doing.  Regardless of the type of call being made, the user's
* function must have a spin of the Windows message loop.  In fact, even
* if user isn't using a progress bar, he should still spin the msg
* loop upon getting these callbacks (but he doesn't need to do anything
* else).  In Delphi, "Application.ProcessMessages;" or
*         in BCPPB   "Application->ProcessMessages(); spins the loop.
* Here are the types of calls:
*
*
*   ActionCode = 0, zacTick, just roll the loop
*
*   ActionCode = 1, zacItem,
*      we're starting a zip operation on a new file
*   (O) FileSize(u64) = filesize of file we're going to operate on
*   (O) MsgP = pathname of file
*   (O) Written (u64) = [Zip] Bytes written
*   IMPORTANT: The user's function must do the math for the progress
*   bar upon getting this call.  See the Delphi sample application.
*
*   ActionCode = 2, zacProgress,
*      increment the progress bar
*      These calls will occur after every 32K of input file has been
*      processed. One additional call is made at the end of each file,
*      just to make sure the progress bar is max'ed out - this is also
*      critical for files less than 32K in size (this last one will be
*      their only one).
*   (O) FileSize(u64) = filesize of file we're going to operate on
*   (O) Written (u64) = [Zip] Bytes written
*
*   ActionCode = 3, zacEndOfBatch,
*      we're done with a batch of files
*          - program flow will quickly return to the user's program.
*   NOTE: the end of a every file will always be followed by an
*         action of 1 or 3, so a separate call for end of one file
*         isn't needed.
*   (O) Written (u64) = [Zip] Bytes written
*
*   ActionCode = 4, zacComment, a routine message is being passed
*   (O) Arg1 = error_code - code corresponding to message (not widely used yet)
*   (O) MsgP = text of message
*
*   ActionCode = 5, zacCount,
*          the total number of files is being passed.
*   (O) Arg1 = The total number of files.
*
*   ActionCode = 6, zacSize,
*      the total file size is being passed.
*   (O) FileSize (u64) = The total file size
*
*   ActionCode = 7, zacNewName,
*      the internal filename is being passed.
*   (O) MsgP2 = pointer to full filespec
*   (O) MsgP = the internal filename as the dll thinks it should be.
*   (I) ActionCode = -1 if changed
*   (I) MsgP => new internal fileanme
*   (I) Msg = the new internal filename
*
*   ActionCode = 8, zacPassword,
*      get Password
*   (O) Arg1 = request count
*   (O) MsgP = filename
*   (I) ActionCode = -1 if has password
*       (I) MsgP => password
*       (I) Msg = password
*       (I) Arg1 = request count
*   (I) ActionCode = -2 if responded 'cancel'
*       (I) Arg1 = request count
*   (I) ActionCode = -3 if responded 'cancel all'
*       (I) Arg1 = request count
*
*   Actioncode = 9, zacCRCError,
*      CRC32 error during Extract
*   (O) Arg1 = Found CRC
*   (O) Arg2 = Stored/Expected CRC
*   (O) MsgP = File for which the CRC check went wrong.
*   (I) ActionCode -1 = extract quietly
*                  -2 = extract with warning [default]
*                  -3 = delete
*
*   Actioncode = 10, zacOverwrite,
*      Extract(UnZip) Overwrite ask.
*   (O) Arg3 = 'older'
*   (O) Arg2 = Index
*   (O) Arg1 = Overwrite_All
*   (O) MsgP = filename
*   (I) ActionCode -1 = overwrite
*          -2 = don't overwrite
*
*   Actioncode = 11, zacSkipped,
*      Skipped during Extract
*   (O) Arg1 = error code
*   (O) Arg2 = type
*   (O) MsgP = filename
*
*   Actioncode = 12, zacComment,
*      FileComment while adding (ZipDll only)
*   (O) MsgP = filename
*   (O) MsgP2 = old comment
*   (I) ActionCode = -1 if comment changed
*       (I) Arg1 = length of new comment
*       (I) MsgP = new comment
*       (I) Msg = new comment
*
*   Actioncode = 13, zacStream,
*      Adjust unzip stream size
*   (O) FileSize (u64) = required size
*   (I) ActionCode = -1 if ok
*       (I) MsgP = Memory
*       (I) Msg2P = Memory
*
*   Actioncode = 14, zacData,
*      Set Extra Data  or Compression level
*   (O) Arg1 = size of data
*   (O) Arg2 = Compression level
*   (O) MsgP = filename
*   (O) MsgP2 = pointer to data
*   (I) ActionCode & 0xff0f = 0xff0f if data changed
*       (I) Arg1 = new size of data
*       (I) MsgP => new data (data must remain until next callback)
*       (I) Msg = new data (< 512 bytes)
*       (I) MsgP2 = new data (> 512 bytes) (data must remain until next callback)
*   (I) ActionCode &0xff0e = 0xff0e if level changed
*       (I) (ActionCode & 0xf0) >> 4 = new level (0..9)
*
*   ActionCode = 15, zacXItem, we're starting a zip operation on a new file
*   (O) Arg1 = type - 1 = archive bit 2 = move file
*   (O) Filesize = filesize of file we're going to operate on
*   (O) MsgP = pathname of file
*
*   ActionCode = 16, zacXProgress, increment the progress bar
*      These calls will occur after every 32K of input file has been
*      processed. One additional call is made at the end of each file,
*      just to make sure the progress bar is max'ed out - this is also
*      critical for files less than 32K in size (this last one will be
*      their only one).
*   (O) Arg1 = type
*   (O) FileSize (u64) = size
*
*   ActionCode = 17, zacExtName,
*      change extract name
*   (O) MsgP2 = ? null
*   (O) MsgP = filename
*   (I) ActionCode = -1 if changed
*      (I) MsgP => new name
*      (I) Msg = new name
*
*   ActionCode = 18, zacNone
*
*   ActionCode = 19, zacKey
*      set or clear operation key
*   (O) Arg1 = key
*
*   ActionCode = 20, zacArg
*      Get string argument
*
*   ActionCode = 21, zacWinErr
*      report windows error
*   (O) Arg1 = Operation
*   (O) Arg2 = Error code
*   (O) MsgP = target filespec
*   (I) return = 0, give error then abort (default)
*              = 1, abort without error
*              = 2, retry - presume error fixed
*
*/

/* ===========================================================================
 * This calls the application program and passes status info.
 */
int __fastcall UserCallback::UserCB(unsigned Action)
{
	int ret = 0;
//    OutputDebugString(L"UserCB - IN");

	if (callb && GetAbort() <= GA_ABORT)
    {
        CBData.Written = Owner->fBytesWritten;
#ifdef UNICODE
        CBData.HaveWide = 1;
#else
        CBData.HaveWide = 0;
#endif
		CBData.ActionCode = (int)Action;
        CBData.check = CALLBACK_CHECK;
        try
        {
            ret = callb(&CBData);  // call user's program
        }
        //__except(1)
		catch(...)
        {
			SetAbort(GA_EXCEPT2);
            ret = CALLBACK_EXCEPTION;
    OutputDebugString(L"UserCB - exception");
        }

        if (ret < CALLBACK_IGNORED)
        {
            switch (ret)
            {

                case CALLBACK_CANCEL:
					SetAbort(GetAbort() | GA_CANCEL);
                    break;

                case CALLBACK_ABORT:
                    SetAbort(GetAbort() | GA_CANCEL);
                    break;

                case CALLBACK_EXCEPTION:
                    SetAbort(GetAbort() | GA_EXCEPT);
            }
        }
    }

	hold.Empty();
    hold2.Empty();
//    OutputDebugString(L"UserCB - OUT");
	return ret;
}

int __fastcall UserCallback::UserCB(unsigned Action, const DZStrW& msg)
{
	SetMsg(msg);
    return UserCB(Action);
}

int UserCallback::UserCB(unsigned Action, const DZStrW& msg, const DZStrW& msg2)
{
	SetMsg2(msg2);
	SetMsg(msg);
    return UserCB(Action);
}

int __fastcall UserCallback::UserMsg(int err, const DZStrW& msg)
{
    CBData.Arg1 = err;    // error
    return UserCB(zacMessage, msg);
}

int __fastcall UserCallback::UserProgress(__int64 adv)
{
    CBData.FileSize = adv;
    return UserCB(zacProgress);
}

int __fastcall UserCallback::UserItem(__int64 cnt, const DZStrW& msg)
{
    if (cnt >= 0)
    {
        InItem++;
        CBData.FileSize = cnt;
        return UserCB(zacItem, msg);
    }
    if (InItem && cnt == -1)
    {
        InItem = 0;
        CBData.FileSize = -1;
        return UserCB(zacItem, msg); // mark end of item
    }
    InItem = 0; // just clear flag
    return 0;
}

int UserCallback::UserXProgress(__int64 adv, int typ)
{
    CBData.Arg1 = typ;    // type
    CBData.FileSize = adv;
    return UserCB(zacXProgress);
}

int UserCallback::UserXItem(__int64 cnt, int typ, const DZStrW& msg)
{
    InItem = 0;
    CBData.Arg1 = typ;
    CBData.FileSize = cnt;
    return UserCB(zacXItem, msg);
}

DZStrW UserCallback::UserArg(int arg, int idx, int *cnt)
{
    DZStrW tmp;
    bool raw = arg > 0x1f;

    if (raw)
        arg &= 0x1f;

    CBData.Arg1 = arg;    // type
    CBData.Arg3 = idx;
    int r = UserCB(zacArg);
    if (r >= CALLBACK_UNHANDLED)
    {
        if (cnt && (arg == zcbFSpecArgs || arg == zcbFSpecArgsExcl))
            *cnt = CBData.Arg3;

        if (r == CALLBACK_TRUE)
        {
            tmp = GetMsg();
            if (!raw)
            {
                tmp.TrimLeft();  // trim trailing unless password

                if (arg != zcbPassword && !tmp.Find(ZPasswordFollows))
                    tmp.TrimRight();
            }
        }
    }

    return tmp;
}

DZStrA UserCallback::UserZCmnt(void)
{
    DZStrA tmp;
    CBData.Arg1 = zcbComment;    // type
    CBData.Arg3 = 0;
    int r = UserCB(zacArg);
    if (r == CALLBACK_TRUE)
    {
        tmp = GetZCmnt();
    }

    return tmp;
}

void __cdecl  DZOp::Notify(unsigned err, const TCHAR* szFmt, ...)
{
    DZStrW ret;
    va_list argList;
    va_start(argList, szFmt);
    ret.FormatV(szFmt, argList);
    va_end(argList);
    if (!ret.IsEmpty())
        SendInfo(err, ret);
}

DZStrW DZOp::ConvExclFilters(const DZStrW & filters)
{
    DZStrW exc;
    if (filters.IsEmpty())
        return exc;
	unsigned int n = 0;
    if (filters[0] == _T('|'))
    {
		exc = _T("|");
        n++;
    }
	unsigned int len = filters.length();
    while (n < len)
    {
		int nx = filters.Find(_T('|'), n+1);
        if (nx < 0)
			nx = (int)len + 1;
		DZStrW comp = filters.Mid(n, (unsigned)nx - n);
		n = (unsigned)nx + 1;
        comp.Trim();
		DZStrW p = ex2IntForm(comp, true);

        if (!p.IsEmpty())
        {
            if (!exc.IsEmpty())
                exc += _T('|');
            exc += p;
        }
    }
    return exc;
}

DZStrW DZOp::MakeExclFilters(void)
{
    DZStrW exc;
    int n = 0;
    while (true)
    {
        DZStrW arg = CB->UserArg(zcbFSpecArgsExcl, n, 0);

        if (arg.IsEmpty())
            break;

        if (!exc.IsEmpty())
            exc += _T('|');
        exc += arg;
        n++;
    }

	return ConvExclFilters(exc);
}


// Convert the external file name to an "internal" file name form,
//   returning the malloc'ed string, or NULL if not enough memory.
//   I.e. Strip the drive if present, strip the path
//if we
//   don't want one and change the name to 8.3 if needed. Not implemented, but
//   also 'put in' change the short path to a long path.
DZStrW DZOp::ex2IntForm(const DZStrW &exname, bool ignore)
{
    bool  pathput;

    if (ignore)
	{
        pathput = true;
    }
    else
	{
        pathput = fpathput != 0;
    }

	DZStrW XName(exname);

    if (XName.IsEmpty())
        return XName;

	DZStrW nname = StrExtSep(XName);
    int p = DriveLen(nname);

	int len = (int)nname.length();
    // Strip leading "\" to convert an absolute path into a relative path

	while (p < len && nname[(unsigned)p] == BSLASH)
        p++;

    // Strip leading "./" as well as drive letter v1.6017
	while ((p + 2) < len && (nname[(unsigned)p] == _T('.') && nname[(unsigned)p + 1] == BSLASH))
        p += 2;

    // This is where the dirname gets stripped if user doesn't want it
    if (!pathput)
    {
        int t = nname.ReverseFind(BSLASH);

        if (t >= p)
            p = ++t;
    }

    if (p)
    {
		nname = nname.Mid((unsigned)p);
    }

    if (nname.length() > MAX_PATH)
    {
		int t = (int)nname.length();
        XName = nname.Left(MAX_PATH);

        if (Verbose) // < 0)
            Notify(IWARNING, _T("File name %s is too long [%d]. Truncated to %s"),
                   nname.c_str(), t, XName.c_str());

        nname = XName;
    }

    return nname;
}

