/---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
//---------------------------------------------------------------------------
//   Important note about DLL memory management when your DLL uses the
//   static version of the RunTime Library:
//
//   If your DLL exports any functions that pass String objects (or structs/
//   classes containing nested Strings) as parameter or function results,
//   you will need to add the library MEMMGR.LIB to both the DLL project and
//   any other projects that use the DLL.  You will also need to use MEMMGR.LIB
//   if any other projects which use the DLL will be perfomring new or delete
//   operations on any non-TObject-derived classes which are exported from the
//   DLL. Adding MEMMGR.LIB to your project will change the DLL and its calling
//   EXE's to use the BORLNDMM.DLL as their memory manager.  In these cases,
//   the file BORLNDMM.DLL should be deployed along with your DLL.
//
//   To avoid using BORLNDMM.DLL, pass string information using "char *" or
//   ShortString parameters.
//
//   If your DLL uses the dynamic version of the RTL, you do not need to
//   explicitly add MEMMGR.LIB as this will be done implicitly for you
//---------------------------------------------------------------------------
USEUNIT("..\..\Source\ZTrees.cpp");
USEUNIT("..\..\Source\Crc32.cpp");
USEUNIT("..\..\Source\CrcTab.cpp");
USEUNIT("..\..\Source\Crypt.cpp");
USEUNIT("..\..\Source\DZ_StrW.cpp");
USEUNIT("..\..\Source\DZFrame.cpp");
USEUNIT("..\..\Source\DZMatch.cpp");
USEUNIT("..\..\Source\DZOper.cpp");
USEUNIT("..\..\Source\DZRaw.cpp");
USEUNIT("..\..\Source\Helpers.cpp");
USEUNIT("..\..\Source\lngmtch.cpp");
USEUNIT("..\..\Source\UInflate.cpp");
USEUNIT("..\..\Source\UnzCrypt.cpp");
USEUNIT("..\..\Source\UnzExec.cpp");
USEUNIT("..\..\Source\UnzFIO.cpp");
USEUNIT("..\..\Source\UnzInf.cpp");
USEUNIT("..\..\Source\UnzOp.cpp");
USEUNIT("..\..\Source\UnzProc.cpp");
USEUNIT("..\..\Source\UnzSS.cpp");
USEUNIT("..\..\Source\UnzSup.cpp");
USEUNIT("..\..\Source\UnzWin32.cpp");
USEUNIT("..\..\Source\UnzXplode.cpp");
USEUNIT("..\..\Source\UnzXtrct.cpp");
USEUNIT("..\..\Source\ZBits.cpp");
USEUNIT("..\..\Source\ZCrypt.cpp");
USEUNIT("..\..\Source\ZDeflate.cpp");
USEUNIT("..\..\Source\ZipDflt.cpp");
USEUNIT("..\..\Source\ZipFile.cpp");
USEUNIT("..\..\Source\ZipFIO.cpp");
USEUNIT("..\..\Source\ZipFnc.cpp");
USEUNIT("..\..\Source\ZipMain.cpp");
USEUNIT("..\..\Source\ZipOp.cpp");
USEUNIT("..\..\Source\ZipPrc.cpp");
USEUNIT("..\..\Source\ZipRead.cpp");
USEUNIT("..\..\Source\ZipSel.cpp");
USEUNIT("..\..\Source\ZipSS.cpp");
USEUNIT("..\..\Source\ZipUp.cpp");
USEUNIT("..\..\Source\ZipWin32.cpp");
USEUNIT("..\..\Source\ZStrings.cpp");
USEUNIT("..\..\Source\ZTreeAsm.cpp");
USEUNIT("..\..\Source\Common.cpp");
USERC("..\..\Source\dz190.rc");
USERC("..\..\Source\dz_msgs.rc");
USEDEF("DelZipExp4.def");
USEUNIT("..\..\Source\enter.cpp");
USEUNIT("..\..\Source\DivMod64.cpp");
//---------------------------------------------------------------------------
extern int WINAPI DllMain(HINSTANCE hinstDLL, DWORD fwdreason, LPVOID lpvReserved);

int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)
{
        return DllMain(hinst, reason, NULL);
}
//---------------------------------------------------------------------------

