#include "stdafx.h"
#pragma hdrstop

#include "common.h"
#include "DZOper.h"
#include "dz_errs.h"
#include "version.h"
//#include <alloc.h>
#include <new>
//#include <except.h>

#undef _DZ_FILE_
#define _DZ_FILE_ DZ_ENTER_CPP

/* enter.cpp

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
#ifndef _WIN64
#include <alloc.H>
#ifndef MULTITHREAD
#error Multithread required
#endif
#endif
CRITICAL_SECTION csSync;
HINSTANCE ModuleInst = 0;
int UCount = 0;
int ZCount = 0;
unsigned int FCount = 0;
bool IsNTorAbove = false;
//---------------------------------------------------------------------------

/* =====================================================
* Entry point to abort operation started with C
*/
long WINAPI DZ_Abort(void * C)
{
    return Set_Oper_Abort((OperKeys)C);
}

#ifndef _WIN64
static int IsOpFrame(DZFrame * p)
{
#if defined(__BORLANDC__) && (__BORLANDC__ < 0x0550) || defined(_WIN64)
    if (!p)
        return 0;
#else
    if (!p || std::heapchecknode(p) != _USEDENTRY)
        return 0;
#endif
    return p->IsME(p);
}
#endif

#if defined(ALLOW_WIN98) && defined(UNICODE)
extern int Init_Imports(void);
extern void FreeDLLs(void);
#endif

typedef void (*pvf)();
pvf OldTerm = 0;
pvf OldUnex = 0;

#ifdef _MSC_VER
BOOL    WINAPI   DllMain (HANDLE hinstDLL, ULONG fwdreason, LPVOID lpvReserved)
#else
#pragma argsused
int WINAPI DllMain(HINSTANCE hinstDLL, DWORD fwdreason, LPVOID lpvReserved)
#endif
{
//  OutputDebugString("Entering DllEntryPoint ");
    switch (fwdreason)
    {

        case DLL_PROCESS_ATTACH:
            ModuleInst = (HINSTANCE)hinstDLL;
            InitializeCriticalSection(&csSync);
            UCount = 0;
            ZCount = 0;
			FCount = 0;
#ifdef _WIN64
			IsNTorAbove = true;
#else
//#ifndef _ONLY_A
			IsNTorAbove = GetVersion() < 0x80000000;
#endif
#if defined(ALLOW_WIN98) && defined(UNICODE)
            if (IsNTorAbove)
				Init_Imports();
//			else
//  				OutputDebugString(L"NOT WinNT!");
#endif
            // fall through

        case DLL_THREAD_ATTACH:
            break;

        case DLL_THREAD_DETACH:
            break;

        case DLL_PROCESS_DETACH:
            Cleanup_Process();
#if defined(ALLOW_WIN98) && defined(UNICODE)
            FreeDLLs();
#endif
            DeleteCriticalSection(&csSync);
    }

    return 1;
}

//---------------------------------------------------------------------------
void Set_Oper(DZFrame *Op, int typ)
{
    if (Op)
    {
        EnterCriticalSection(&csSync);

        if (Op->SetME(Op, typ))
        {
            if (typ == ZIPOPER)
                ZCount++;
            else
                UCount++;

            FCount++;
        }
        else
        {
            if (typ == ZIPOPER)
                ZCount--;
            else
                UCount--;
        }

        LeaveCriticalSection(&csSync);
    }
}


int Set_Oper_Abort(OperKeys key)
{
    int ret = 1;
    DZFrame *p = (DZFrame *)(key << 2);

#if defined(__BORLANDC__) && (__BORLANDC__ < 0x0550) || defined(_WIN64)
    if (!p)
        return -1;
#else
    if (!p || std::heapchecknode(p) != _USEDENTRY)
        return -1;
#endif

    EnterCriticalSection(&csSync);

    if (p->IsME(p))
    {
        if (p->Abort_Flag > GA_CANCEL)
            ret--;           // could not set it
        else
            p->Abort_Flag |= GA_ABORT;
    }

    LeaveCriticalSection(&csSync);

    return ret;
}

long WINAPI DZ_Version(void)
{
	return DZ_VER_VERSION; // see version.h
}

long WINAPI DZ_PrivVersion(void)
{
	return DZ_VER_PRIVATE;
}

char gbuf[MAX_PATH + 2];
const char* WINAPI DZ_Path(void)
{
	if (!GetModuleFileNameA((HINSTANCE)ModuleInst, gbuf, MAX_PATH))
	{
        gbuf[0] = 0;
    }

    return gbuf;
}

long WINAPI DZ_Name(void* buf, int bufsiz, int wide)
{
	if (wide)
		return (long)GetModuleFileNameW(ModuleInst, (wchar_t*)buf, (DWORD)bufsiz);
	else
		return (long)GetModuleFileNameA(ModuleInst, (char*)buf, (DWORD)bufsiz);
}

const char* WINAPI DZ_Banner(void)
{
#ifdef _WIN64
	sprintf_s(gbuf, MAX_PATH, "%s.dll version 1.9.%d.%d, %s", DLLNAME,
		(DZ_VER_PRIVATE/10000)%10, DZ_VER_PRIVATE%1000, __DATE__);
#else
	sprintf(gbuf, "%s.dll version 1.9.%d.%d, %s", DLLNAME,
		(DZ_VER_PRIVATE/10000)%10, DZ_VER_PRIVATE%1000, __DATE__);
#endif
//#ifdef _WIN64
//	sprintf_s(gbuf, MAX_PATH, "%s.dll version %s, %s", DLLNAME, DZ_VER, __DATE__);
//#else
//	sprintf(gbuf, "%s.dll version %s, %s", DLLNAME, DZ_VER, __DATE__);
//#endif
	return gbuf;
}

static void KillpG(DZOp** ppG)
{
    if (ppG)
    {
        DZOp * tmp = *ppG;
        *ppG = NULL;
        delete tmp;
        OutputDebugString(L"killed pG");
    }
}

int xxfilter(EXCEPTION_POINTERS *);

// Add, update, freshen, move, or delete zip entries in a zip file.
static long WINAPI DZ_Exec2(DZOp *pG, const DllCommands * C)
{
    long RetVal;
        try
        {
                RetVal = pG->Init();
                if (!RetVal)
                {
                    RetVal = pG->Exec(C);
                    if (pG->Verbose < 0)
                        pG->Notify(ITRACE, _T("RetVal = %d "), RetVal);
                }
            }
            catch(DZFatalException & E)
            {
                RetVal =  -DZ_ERR(E.ENo());
            }
            catch(std::bad_alloc)
            {
                RetVal =  -DZ_ERR_MEMORY;
            }
            catch(int X)
            {
                X = DZ_ERR(X);
                RetVal =  -X;
            }
            catch(...)
            {
                RetVal =  -DZ_ERR_ERROR;
            }
	return RetVal;
}

long WINAPI DZ_Exec(const DllCommands * C)
{
    DZOp *pG = 0;
    long RetVal;

//    EXCEPTION_POINTERS *xp = 0;

    OutputDebugString(L"DZ_Exec enter");
    if (!C || C->fCheck != DLLCOMMANDCHECK)
    {
        // This Seven is at another place as in ZCL!
        return -DZ_ERR_STRUCT;
    }

    unsigned int cmd = C->fOptions.opts & 0xF;

    if (cmd != DLL_OPT_OpIsZip)// && cmd != DLL_OPT_OpIsDelete && cmd !=
        //DLL_OPT_OpIsUnz && cmd != DLL_OPT_OpIsTest)
    {
        return -DZ_ERR_STRUCT;
    }

    __try
    {
//        if (cmd == DLL_OPT_OpIsZip || cmd == DLL_OPT_OpIsDelete)
            pG = MakeZipper(C);
//        else
//            pG = MakeUnzipper(C);

        if (!pG)
            throw DZFatalException(DZ_ERR_MEMORY);
        __try
        {
			RetVal = DZ_Exec2(pG, C);
        }
        __finally
        {
            KillpG(&pG);
        }
    }
    __except (xxfilter(/*xp =*/ GetExceptionInformation()))
    {
         RetVal = -DZ_ERR_ERROR;
    }

    OutputDebugString(L"DZ_Exec done");
    return RetVal;
}

int xxfilter(EXCEPTION_POINTERS * xp)
{
    int rc = EXCEPTION_CONTINUE_SEARCH;  // default - give up

    EXCEPTION_RECORD *xr = xp->ExceptionRecord;
//    CONTEXT *xc = xp->ContextRecord;
    switch (xr->ExceptionCode) {
        case STATUS_INTEGER_DIVIDE_BY_ZERO:
        case EXCEPTION_ACCESS_VIOLATION:
            rc = EXCEPTION_EXECUTE_HANDLER;
//            break;
    };
    return rc;
}
