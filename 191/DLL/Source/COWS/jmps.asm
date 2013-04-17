        .486p
        model flat
  
        extern zCompareStringW : proc
        global CompareStringW : proc
        extern zCreateDirectoryW : proc
        global CreateDirectoryW : proc
        extern zCreateFileW : proc
        global CreateFileW : proc
        extern zDeleteFileW : proc
        global DeleteFileW : proc
        extern zFindFirstFileW : proc
        global FindFirstFileW : proc
        extern zFindNextFileW : proc
        global FindNextFileW : proc
        extern zFindResourceW : proc
        global FindResourceW : proc
        extern zFreeEnvironmentStringsW : proc
        global FreeEnvironmentStringsW : proc
        extern zGetCurrentDirectoryW : proc
        global GetCurrentDirectoryW : proc
        extern zGetDriveTypeW : proc
        global GetDriveTypeW : proc
        extern zGetFileAttributesW : proc
        global GetFileAttributesW : proc
        extern zGetFullPathNameW : proc
        global GetFullPathNameW : proc
        extern zGetModuleFileNameW : proc
        global GetModuleFileNameW : proc
        extern zGetModuleHandleW : proc
        global GetModuleHandleW : proc
        extern zGetShortPathNameW : proc
        global GetShortPathNameW : proc
        extern zGetStringTypeW : proc
        global GetStringTypeW : proc
        extern zGetTempPathW : proc
        global GetTempPathW : proc
        extern zGetVolumeInformationW : proc
        global GetVolumeInformationW : proc
        extern zIsDebuggerPresent : proc
        global IsDebuggerPresent : proc
        extern zLCMapStringW : proc
        global LCMapStringW : proc
        extern zMoveFileWithProgressW : proc
        global MoveFileWithProgressW : proc
        extern zRemoveDirectoryW : proc
        global RemoveDirectoryW : proc
        extern zSetFileAttributesW : proc
        global SetFileAttributesW : proc
        extern zlstrlenW : proc
        global lstrlenW : proc
        extern zOutputDebugStringW : proc
        global OutputDebugStringW : proc
        extern zSHFileOperationW : proc
        global SHFileOperationW : proc
        extern zCharToOemW : proc
        global CharToOemW : proc
        extern zLoadStringW : proc
        global LoadStringW : proc
        extern zMessageBoxW : proc
        global MessageBoxW : proc
        extern zOemToCharW : proc
        global OemToCharW : proc
        extern zCharUpperW : proc
        global CharUpperW : proc
        extern zCharLowerW : proc
        global CharLowerW : proc
        extern zCharUpperBuffW : proc
        global CharUpperBuffW : proc
        extern zCharNextW : proc
        global CharNextW : proc
  
        public _u_jmps
  
_DATA   segment dword public use32 'DATA'
_u_jmps:
        dd zCompareStringW
        dd zCreateDirectoryW
        dd zCreateFileW
        dd zDeleteFileW
        dd zFindFirstFileW
        dd zFindNextFileW
        dd zFindResourceW
        dd zFreeEnvironmentStringsW
        dd zGetCurrentDirectoryW
        dd zGetDriveTypeW
        dd zGetFileAttributesW
        dd zGetFullPathNameW
        dd zGetModuleFileNameW
        dd zGetModuleHandleW
        dd zGetShortPathNameW
        dd zGetStringTypeW
        dd zGetTempPathW
        dd zGetVolumeInformationW
        dd zIsDebuggerPresent
        dd zLCMapStringW
        dd zMoveFileWithProgressW
        dd zRemoveDirectoryW
        dd zSetFileAttributesW
        dd zlstrlenW
        dd zOutputDebugStringW
        dd zSHFileOperationW
        dd zCharToOemW
        dd zLoadStringW
        dd zMessageBoxW
        dd zOemToCharW
        dd zCharUpperW
        dd zCharLowerW
        dd zCharUpperBuffW
        dd zCharNextW
_DATA   ends
  
_TEXT   segment dword public use32 'CODE'
CompareStringW proc near
        jmp dword ptr [_u_jmps + 0 ]
CompareStringW endp
  
CreateDirectoryW proc near
        jmp dword ptr [_u_jmps + 4 ]
CreateDirectoryW endp
  
CreateFileW proc near
        jmp dword ptr [_u_jmps + 8 ]
CreateFileW endp
  
DeleteFileW proc near
        jmp dword ptr [_u_jmps + 12 ]
DeleteFileW endp
  
FindFirstFileW proc near
        jmp dword ptr [_u_jmps + 16 ]
FindFirstFileW endp
  
FindNextFileW proc near
        jmp dword ptr [_u_jmps + 20 ]
FindNextFileW endp
  
FindResourceW proc near
        jmp dword ptr [_u_jmps + 24 ]
FindResourceW endp
  
FreeEnvironmentStringsW proc near
        jmp dword ptr [_u_jmps + 28 ]
FreeEnvironmentStringsW endp
  
GetCurrentDirectoryW proc near
        jmp dword ptr [_u_jmps + 32 ]
GetCurrentDirectoryW endp
  
GetDriveTypeW proc near
        jmp dword ptr [_u_jmps + 36 ]
GetDriveTypeW endp
  
GetFileAttributesW proc near
        jmp dword ptr [_u_jmps + 40 ]
GetFileAttributesW endp
  
GetFullPathNameW proc near
        jmp dword ptr [_u_jmps + 44 ]
GetFullPathNameW endp
  
GetModuleFileNameW proc near
        jmp dword ptr [_u_jmps + 48 ]
GetModuleFileNameW endp
  
GetModuleHandleW proc near
        jmp dword ptr [_u_jmps + 52 ]
GetModuleHandleW endp
  
GetShortPathNameW proc near
        jmp dword ptr [_u_jmps + 56 ]
GetShortPathNameW endp
  
GetStringTypeW proc near
        jmp dword ptr [_u_jmps + 60 ]
GetStringTypeW endp
  
GetTempPathW proc near
        jmp dword ptr [_u_jmps + 64 ]
GetTempPathW endp
  
GetVolumeInformationW proc near
        jmp dword ptr [_u_jmps + 68 ]
GetVolumeInformationW endp
  
IsDebuggerPresent proc near
        jmp dword ptr [_u_jmps + 72 ]
IsDebuggerPresent endp
  
LCMapStringW proc near
        jmp dword ptr [_u_jmps + 76 ]
LCMapStringW endp
  
MoveFileWithProgressW proc near
        jmp dword ptr [_u_jmps + 80 ]
MoveFileWithProgressW endp
  
RemoveDirectoryW proc near
        jmp dword ptr [_u_jmps + 84 ]
RemoveDirectoryW endp
  
SetFileAttributesW proc near
        jmp dword ptr [_u_jmps + 88 ]
SetFileAttributesW endp
  
lstrlenW proc near
        jmp dword ptr [_u_jmps + 92 ]
lstrlenW endp
  
OutputDebugStringW proc near
        jmp dword ptr [_u_jmps + 96 ]
OutputDebugStringW endp
  
SHFileOperationW proc near
        jmp dword ptr [_u_jmps + 100 ]
SHFileOperationW endp
  
CharToOemW proc near
        jmp dword ptr [_u_jmps + 104 ]
CharToOemW endp
  
LoadStringW proc near
        jmp dword ptr [_u_jmps + 108 ]
LoadStringW endp
  
MessageBoxW proc near
        jmp dword ptr [_u_jmps + 112 ]
MessageBoxW endp
  
OemToCharW proc near
        jmp dword ptr [_u_jmps + 116 ]
OemToCharW endp
  
CharUpperW proc near
        jmp dword ptr [_u_jmps + 120 ]
CharUpperW endp
  
CharLowerW proc near
        jmp dword ptr [_u_jmps + 124 ]
CharLowerW endp
  
CharUpperBuffW proc near
        jmp dword ptr [_u_jmps + 128 ]
CharUpperBuffW endp
  
CharNextW proc near
        jmp dword ptr [_u_jmps + 132 ]
CharNextW endp
  
_TEXT   ends
  
      end
