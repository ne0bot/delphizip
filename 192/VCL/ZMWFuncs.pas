unit ZMWFuncs;

//  ZMWFuncs.pas - Functions supporting UTF8/16 file names

(* ***************************************************************************
TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009, 2010, 2011 Russell Peters and Roger Aelbrecht

All rights reserved.
For the purposes of Copyright and this license "DelphiZip" is the current
 authors, maintainers and developers of its code:
  Russell Peters and Roger Aelbrecht.

Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
* DelphiZip reserves the names "DelphiZip", "ZipMaster", "ZipBuilder",
   "DelZip" and derivatives of those names for the use in or about this
   code and neither those names nor the names of its authors or
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL DELPHIZIP, IT'S AUTHORS OR CONTRIBUTERS BE
 LIABLE FOR ANYDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.

contact: problems AT delphizip DOT org
updates: http://www.delphizip.org
 *************************************************************************** *)
//Modified 2012-12-08

{$INCLUDE   '.\ZipVers.inc'}
{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
{$IFDEF UNICODE}
{$DEFINE _NO_UTF8_}
{$ELSE}
{$IFDEF SUPPORT_OLD_WINDOWS}
{$DEFINE _NO_UTF8_}
{$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF VERDXE2up}
    WinApi.Windows, System.SysUtils,
  {$ELSE}
    Windows, SysUtils,
  {$ENDIF}
    ZipMstr;

//type
//{$IFDEF _NO_UTF8_}
//  TZMMakeDirFunction = function(const Fname: TZMString): Boolean;
//{$ELSE}
//  TZMMakeDirFunction = function(const Fname: WideString): Boolean;
//{$ENDIF}

type
{$IFDEF _NO_UTF8_}
  _Z_TSearchRec = TSearchRec;
{$ELSE}
  _Z_TSearchRec = record
    Time: Integer;
    Size: Int64;
    Attr: Integer;
    name: TZMString;
    ExcludeAttr: Integer;
    FindHandle: THandle {$IFNDEF VERpre6} platform{$ENDIF};
    FindData: TWin32FindDataW {$IFNDEF VERpre6} platform{$ENDIF};
  end;
{$ENDIF}

function _Z_CreateFile(FileName: TZMString; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle;
{$IFDEF UNICODE}inline; {$ENDIF}
function _Z_DeleteFile(const FileName: TZMString): Boolean;
{$IFDEF UNICODE}inline; {$ENDIF}
function _Z_DirExists(const Fname: TZMString): Boolean;

// returns <0 _ file does not exist, 0 _ success, >0 _ error
function _Z_EraseFile(const Fname: TZMString; permanent: Boolean): Integer;
function _Z_FileCreate(const Fname: TZMString): cardinal;
{$IFDEF UNICODE}inline; {$ENDIF}
function _Z_FileExists(const Fname: TZMString): Boolean;
{$IFDEF UNICODE}inline; {$ENDIF}
function _Z_FileOpen(const Fname: TZMString; Mode: LongWord): Integer;
{$IFDEF UNICODE}inline; {$ENDIF}
function _Z_FindFirst(const Path: TZMString; Attr: Integer;
  var F: _Z_TSearchRec): Integer; {$IFDEF UNICODE}inline; {$ENDIF}
function _Z_FindNext(var F: _Z_TSearchRec): Integer;
{$IFDEF UNICODE}inline; {$ENDIF}
procedure _Z_FindClose(var F: _Z_TSearchRec);
{$IFDEF UNICODE}inline; {$ENDIF}
function _Z_ForceDirectory(const Dir: TZMString): Boolean;
//  MKDir: TZMMakeDirFunction = nil): Boolean;
function _Z_GetExeVersion(const FName: String; var MS, LS: DWORD): Boolean;
function _Z_RemoveDir(const Fname: TZMString): Boolean;
{$IFDEF UNICODE}inline; {$ENDIF}
function _Z_CreateDir(const Fname: TZMString): Boolean;
{$IFDEF UNICODE}inline; {$ENDIF}
function _Z_RenameFile(const OldName, NewName: TZMString): Boolean;
{$IFDEF UNICODE}inline; {$ENDIF}

implementation

uses
  ShellAPI, Forms, ZMStructs, ZMUtils{$IFNDEF UNICODE}, ZMUTF8,
  ZMMsgStr{$ENDIF};

const
  __UNIT__ = 25;// shl 23;

{$IFDEF _NO_UTF8_}

// use 'native' functions
function _Z_CreateFile(FileName: TZMString; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD; hTemplateFile: THandle): THandle;
begin
  Result := CreateFile(PChar(FileName), dwDesiredAccess, dwShareMode,
  lpSecurityAttributes,
  dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile);
end;

function _Z_DeleteFile(const FileName: TZMString): Boolean;
begin
  Result := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.DeleteFile(FileName);
end;

function _Z_DirExists(const Fname: TZMString): Boolean;
var
  Code: DWORD;
  Dir: string;
begin
  Result := True; // current directory exists
  Dir := DelimitPath(Fname, False);
  if Fname <> '' then
  begin
    Code := GetFileAttributes(PChar(Dir));
    Result := (Code <> MAX_UNSIGNED) and
      ((FILE_ATTRIBUTE_DIRECTORY and Code) <> 0);
  end;
end;

// returns <0 _ file does not exist, 0 _ success, >0 _ error
function _Z_EraseFile(const Fname: TZMString; permanent: Boolean): Integer;
var
  DelFileName: string;
  SHF: TSHFileOpStruct;
begin
  // If we do not have a full path then FOF_ALLOWUNDO does not work!?
  DelFileName := Fname;
  if ExtractFilePath(Fname) = '' then
    DelFileName := GetCurrentDir() + PathDelim + Fname;

  Result := -1;
  // We need to be able to 'Delete' without getting an error
  // if the file does not exists as in ReadSpan() can occur.
  if not _Z_FileExists(DelFileName) then
    Exit;
  SHF.Wnd := Application.Handle;
  SHF.wFunc := FO_DELETE;
  SHF.pFrom := PChar(DelFileName + #0);
  SHF.pTo := nil;
  SHF.fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
  if not permanent then
    SHF.fFlags := SHF.fFlags or FOF_ALLOWUNDO;

  Result := SHFileOperation(SHF);
end;

function _Z_FileCreate(const Fname: TZMString): cardinal;
begin
  Result := FileCreate(Fname);
end;

function _Z_FileExists(const Fname: TZMString): Boolean;
begin
  Result := FileExists(Fname);
end;

function _Z_FileOpen(const Fname: TZMString; Mode: LongWord): Integer;
begin
  Result := FileOpen(Fname, Mode);
end;

function _Z_FindFirst(const Path: TZMString; Attr: Integer;
  var F: _Z_TSearchRec): Integer;
begin
  Result := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FindFirst(Path, Attr, F);
end;

function _Z_FindNext(var F: _Z_TSearchRec): Integer;
begin
  Result := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FindNext(F);
end;

procedure _Z_FindClose(var F: _Z_TSearchRec);
begin
  {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.FindClose(F);
end;

function _Z_ForceDirectory(const Dir: TZMString): Boolean;
//  MKDir: TZMMakeDirFunction = nil): Boolean;
var
  sDir: string;
begin
  Result := True;
  if Dir <> '' then
  begin
    sDir := DelimitPath(Dir, False);
    if _Z_DirExists(sDir) or (ExtractFilePath(sDir) = sDir) then
      Exit; // avoid 'c:\xyz:\' problem.

    if _Z_ForceDirectory(ExtractFilePath(sDir){, MKDir}) then
    begin
//      if @MKDir <> nil then
//        Result := MKDir(sDir)
//      else
{$IFDEF UNICODE}
        Result := CreateDirectoryW(PWideChar(sDir), nil);
{$ELSE}
        Result := CreateDirectory(PAnsiChar(sDir), nil);
{$ENDIF}
    end
    else
      Result := False;
  end;
end;

function _Z_GetExeVersion(const FName: String; var MS, LS: DWORD): Boolean;
var
  Dummy: DWORD;
  Path: string;
  VerInfo: Pointer;
  VerInfoSize: DWORD;
  VerValue: PVSFixedFileInfo;
begin
  Result := False;
  Path := '\';
  if _Z_FileExists(FName) then
  begin
    VerInfoSize := GetFileVersionInfoSize(PChar(FName), Dummy);
    if VerInfoSize > 0 then
    begin
      GetMem(VerInfo, VerInfoSize);
      try
        if GetFileVersionInfo(PChar(FName), 0, VerInfoSize, VerInfo) then
        begin
          if VerQueryValue(VerInfo, PChar(Path), Pointer(VerValue), Dummy) then
          begin
            MS := VerValue^.dwFileVersionMS;
            LS := VerValue^.dwFileVersionLS;
            Result := True;
          end;
        end;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
    end;
  end;
end;

function _Z_RemoveDir(const Fname: TZMString): Boolean;
begin
  Result := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.RemoveDir(Fname);
end;

function _Z_CreateDir(const Fname: TZMString): Boolean;
begin
  Result := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.CreateDir(FName);
end;

function _Z_RenameFile(const OldName, NewName: TZMString): Boolean;
begin
  Result := {$IFDEF VERDXE2up}System.{$ENDIF}SysUtils.RenameFile(OldName, NewName);
end;

{$ELSE}
 // not UNICODE

function DelimitPathW(const Path: WideString; Sep: Boolean): WideString;
begin
  Result := Path;
  if Length(Path) = 0 then
  begin
    if Sep then
      Result := PathDelim{'\'};
    exit;
  end;
  if (Path[Length(Path)] = PathDelim) <> Sep then
  begin
    if Sep then
      Result := Path + PathDelim
    else
      Result := Copy(Path, 1, pred(Length(Path)));
  end;
end;

  // NT up can link 'W' functions
function _Z_CreateFile(FileName: TZMString; dwDesiredAccess, dwShareMode: DWORD;
  lpSecurityAttributes: PSecurityAttributes;
  dwCreationDisposition, dwFlagsAndAttributes: DWORD;
  hTemplateFile: THandle): THandle;
var
  WName: WideString;
begin
  if not UsingUTF8 then
    Result := CreateFile(PChar(FileName), dwDesiredAccess, dwShareMode,
      lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
      hTemplateFile)
  else
  begin
    WName := UTF8ToWide(FileName, -1);
    Result := CreateFileW(PWideChar(WName), dwDesiredAccess, dwShareMode,
      lpSecurityAttributes, dwCreationDisposition, dwFlagsAndAttributes,
      hTemplateFile);
  end;
end;

function _Z_DeleteFile(const FileName: TZMString): Boolean;
var
  WName: WideString;
begin
  if not UsingUTF8 then
    Result := DeleteFile(FileName)
  else
  begin
    WName := UTF8ToWide(FileName, -1);
    Result := DeleteFileW(PWideChar(WName));
  end;
end;

function _Z_DirExists(const Fname: TZMString): Boolean;
var
  Code: DWORD;
  Dir: TZMString;
  WDir: WideString;
begin
  Result := True; // current directory exists
  if Fname <> '' then
  begin
    if not UsingUTF8 then
    begin
      Dir := DelimitPath(Fname, False);
      Code := GetFileAttributes(PChar(Dir))
    end
    else
    begin
      WDir := UTF8ToWide(Fname, -1);
      WDir := DelimitPathW(WDir, False);
      Code := GetFileAttributesW(PWideChar(WDir))
    end;
    Result := (Code <> MAX_UNSIGNED) and
      ((FILE_ATTRIBUTE_DIRECTORY and Code) <> 0);
  end;
end;

// returns <0 _ file does not exist, 0 _ success, >0 _ error
function _Z_EraseFile(const Fname: TZMString; permanent: Boolean): Integer;
var
  DelFileName: TZMString;
  SHF: TSHFileOpStructW;
  WName: WideString;
begin
  // If we do not have a full path then FOF_ALLOWUNDO does not work!?
  DelFileName := Fname;
  if ExtractFilePath(Fname) = '' then
    DelFileName := GetCurrentDir() + PathDelim + Fname;

  Result := -1;
  // We need to be able to 'Delete' without getting an error
  // if the file does not exists as in ReadSpan() can occur.
  if not _Z_FileExists(DelFileName) then
    Exit;

  if not UsingUTF8 then
    WName := DelFileName // convert to wide
  else
    WName := UTF8ToWide(DelFileName, -1);

  SHF.Wnd := Application.Handle;
  SHF.wFunc := FO_DELETE;
  SHF.pFrom := PWideChar(WName + #0);
  SHF.pTo := nil;
  SHF.fFlags := FOF_SILENT or FOF_NOCONFIRMATION;
  if not permanent then
    SHF.fFlags := SHF.fFlags or FOF_ALLOWUNDO;

  Result := SHFileOperationW(SHF);
end;

function _Z_FileCreate(const Fname: TZMString): cardinal;
var
  WName: WideString;
begin
  if not UsingUTF8 then
    Result := FileCreate(Fname)
  else
  begin
    WName := UTF8ToWide(Fname, -1);
    Result := CreateFileW(PWideChar(WName), GENERIC_READ or GENERIC_WRITE, 0,
      nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  end;
end;

function ExistsLockedOrSharedW(const FileName: WideString): Boolean;
var
  FindData: TWin32FindDataW;
  LHandle: THandle;
begin
  { Either the file is locked/share_exclusive or we got an access denied }
  LHandle := FindFirstFileW(PWideChar(FileName), FindData);
  if LHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(LHandle);
    Result := FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0;
  end
  else
    Result := False;
end;

function _Z_FileExists(const Fname: TZMString): Boolean;
var
  Code: Integer;
  LastError: cardinal;
  WName: WideString;
begin
  if not UsingUTF8 then
    Result := FileExists(Fname)
  else
  begin
    WName := UTF8ToWide(Fname, -1);
    Code := GetFileAttributesW(PWideChar(WName));
    if Code <> -1 then
      Result := (FILE_ATTRIBUTE_DIRECTORY and Code = 0)
    else
    begin
      LastError := GetLastError;
      Result := (LastError <> ERROR_FILE_NOT_FOUND) and
        (LastError <> ERROR_PATH_NOT_FOUND) and
        (LastError <> ERROR_INVALID_NAME) and ExistsLockedOrSharedW(WName);
    end;
  end;
end;

function _Z_FileOpen(const Fname: TZMString; Mode: LongWord): Integer;
const
  AccessMode: array [0 .. 2] of LongWord = (GENERIC_READ, GENERIC_WRITE,
    GENERIC_READ or GENERIC_WRITE);
  ShareMode: array [0 .. 4] of LongWord = (0, 0, FILE_SHARE_READ,
    FILE_SHARE_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE);
var
  WName: WideString;
begin
  if not UsingUTF8 then
    Result := FileOpen(Fname, Mode)
  else
  begin
    Result := -1;
    WName := UTF8ToWide(Fname, -1);
    if ((Mode and 3) <= fmOpenReadWrite) and
      ((Mode and $F0) <= fmShareDenyNone) then
      Result := Integer(CreateFileW(PWideChar(WName), AccessMode[Mode and 3],
        ShareMode[(Mode and $F0) shr 4], nil, OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL, 0));
  end;
end;

function _FindMatchingFileW(var F: _Z_TSearchRec): Integer;
var
  LocalFileTime: TFileTime;
begin
  with F do
  begin
    while FindData.dwFileAttributes and ExcludeAttr <> 0 do
      if not FindNextFileW(FindHandle, FindData) then
      begin
        Result := GetLastError;
        Exit;
      end;
    FileTimeToLocalFileTime(FindData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LongRec(Time).Hi, LongRec(Time).Lo);
    Size := FindData.nFileSizeLow or Int64(FindData.nFileSizeHigh) shl 32;
    Attr := FindData.dwFileAttributes;
    name := FindData.cFileName;
  end;
  Result := 0;
end;

function _Z_FindFirst(const Path: TZMString; Attr: Integer;
  var F: _Z_TSearchRec): Integer;
const
  faSpecial = faHidden or faSysFile or faDirectory;
var
  WPath: WideString;
begin
  if not UsingUTF8 then
    WPath := Path // convert to wide
  else
    WPath := UTF8ToWide(Path, -1);
  F.ExcludeAttr := not Attr and faSpecial;
  F.FindHandle := FindFirstFileW(PWideChar(WPath), F.FindData);
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Result := _FindMatchingFileW(F);
    if Result <> 0 then
      _Z_FindClose(F);
  end
  else
    Result := GetLastError;
end;

function _Z_FindNext(var F: _Z_TSearchRec): Integer;
begin
  if FindNextFileW(F.FindHandle, F.FindData) then
    Result := _FindMatchingFileW(F)
  else
    Result := GetLastError;
end;

procedure _Z_FindClose(var F: _Z_TSearchRec);
begin
  if F.FindHandle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(F.FindHandle);
    F.FindHandle := INVALID_HANDLE_VALUE;
  end;
end;

function DirExistsW(const Fname: WideString): Boolean;
var
  Code: DWORD;
begin
  Result := True; // current directory exists

  if Fname <> '' then
  begin
    Code := GetFileAttributesW(PWideChar(Fname));
    Result := (Code <> MAX_UNSIGNED) and
      ((FILE_ATTRIBUTE_DIRECTORY and Code) <> 0);
  end;
end;

function FindLastW(const ws: WideString; const wc: widechar): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Length(ws) - 1 downto 1 do
    if ws[i] = wc then
    begin
      Result := i;
    end;
end;

function FindFirstW(const ws: WideString; const wc: widechar): Integer;
begin
  for Result := 1 to Length(ws) - 1 do
    if ws[Result] = wc then
      Exit;
  Result := -1;
end;

function ExtractFilePathW(const Path: WideString): WideString;
var
  d, c: Integer;
begin
  Result := '';
  c := FindFirstW(Path, ':');
  d := FindLastW(Path, PathDelim);
  if (d > c) and (d >= 1) then
    Result := Copy(Path, 1, pred(d));
end;

function ForceDirectoryW(const Dir: WideString): Boolean;
//  MKDir: TZMMakeDirFunction = nil): Boolean;
var
  sDir: WideString;
begin
  Result := True;
  if Dir <> '' then
  begin
    sDir := DelimitPathW(Dir, False);
    if DirExistsW(sDir) or (ExtractFilePath(sDir) = sDir) then
      Exit; // avoid 'c:\xyz:\' problem.

    if ForceDirectoryW(ExtractFilePath(sDir){, MKDir}) then
    begin
//      if @MKDir <> nil then
//        Result := MKDir(sDir)
//      else
        Result := CreateDirectoryW(PWideChar(sDir), nil);
    end
    else
      Result := False;
  end;
end;

function _Z_ForceDirectory(const Dir: TZMString): Boolean;
//  MKDir: TZMMakeDirFunction = nil): Boolean;
var
  WDir: WideString;
begin
  Result := True;
  if Dir <> '' then
  begin
    if not UsingUTF8 then
      WDir := Dir // convert to wide
    else
      WDir := UTF8ToWide(Dir, -1);
    Result := ForceDirectoryW(WDir{, MKDir});
  end;
end;

function _Z_GetExeVersionA(const FName: TZMString; var MS, LS: DWORD): Boolean;
var
  Dummy: DWORD;
  Path: TZMString;
  VerInfo: Pointer;
  VerInfoSize: DWORD;
  VerValue: PVSFixedFileInfo;
begin
  Result := False;
  Path := '\';
  VerInfoSize := GetFileVersionInfoSize(PChar(FName), Dummy);
  if VerInfoSize > 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(FName), 0, VerInfoSize, VerInfo) then
      begin
        if VerQueryValue(VerInfo, PChar(Path), Pointer(VerValue), Dummy) then
        begin
          MS := VerValue^.dwFileVersionMS;
          LS := VerValue^.dwFileVersionLS;
          Result := True;
        end;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

function _Z_GetExeVersionW(const WName: WideString; var MS, LS: DWORD): Boolean;
var
  Dummy: DWORD;
  Path: WideString;
  VerInfo: Pointer;
  VerInfoSize: DWORD;
  VerValue: PVSFixedFileInfo;
begin
  Result := False;
  Path := '\';
  VerInfoSize := GetFileVersionInfoSizeW(PWideChar(WName), Dummy);
  if VerInfoSize > 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfoW(PWideChar(WName), 0, VerInfoSize, VerInfo) then
      begin
        if VerQueryValueW(VerInfo, PWideChar(Path), Pointer(VerValue), Dummy) then
        begin
          MS := VerValue^.dwFileVersionMS;
          LS := VerValue^.dwFileVersionLS;
          Result := True;
        end;
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

function _Z_GetExeVersion(const FName: TZMString; var MS, LS: DWORD): Boolean;
var
  WName: WideString;
begin
  Result := False;
  if _Z_FileExists(FName) then
  begin
    if not UsingUTF8 then
      Result := _Z_GetExeVersionA(FName, MS, LS)
    else
    begin
      WName := UTF8ToWide(Fname, -1);
      Result := _Z_GetExeVersionW(WName, MS, LS);
    end;
  end;
end;

function _Z_RemoveDir(const Fname: TZMString): Boolean;
var
  WName: WideString;
begin
  if not UsingUTF8 then
    Result := RemoveDir(PChar(Fname))
  else
  begin
    WName := UTF8ToWide(Fname, -1);
    Result := RemoveDirectoryW(PWideChar(WName));
  end;
end;

function _Z_CreateDir(const Fname: TZMString): Boolean;
var
  WDir: WideString;
begin
  if not UsingUTF8 then
    Result := SysUtils.CreateDir(FName)
  else
  begin
    WDir := UTF8ToWide(FName, -1);
    Result := CreateDirectoryW(PWideChar(WDir), nil);
  end;
end;

function _Z_RenameFile(const OldName, NewName: TZMString): Boolean;
var
  WNew: WideString;
  WOld: WideString;
begin
  if not UsingUTF8 then
    Result := RenameFile(OldName, NewName)
  else
  begin
    WNew := UTF8ToWide(NewName, -1);
    WOld := UTF8ToWide(OldName, -1);
    Result := MoveFileW(PWideChar(WOld), PWideChar(WNew));
  end;
end;

{$ENDIF}

end.
