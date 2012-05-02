unit ToolHelper;

(* ***************************************************************************
  ToolHelper.pas - utility functions TZipResMaker for ZipMaster
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.

	This file is part of TZipMaster Version 1.9.1.x

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
//modified 2012-04-19
{$I '..\..\ZipVers.inc'}
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Template;

type
  TVersions   = (vcVCL, vcDLL);

type
  TToolHelper = class(TObject)
  private
  protected
    function GetVersion(Index: TVersions): Integer; virtual; abstract;
    procedure TemplaterMsg(Sender: TTemplater; const msg: string);
  public
    constructor Create;
    procedure BackupFile(const where: String);
    function CanWrite(const fspec: String): Boolean;
    procedure CheckCanWrite(const fn: String);
    function fCopy(const ffrom, fto: String): Integer;
    procedure Licence(strs: TStrings; Stamp: string = ''; CStyle: Boolean = False);
    function LIdOfRes(const fn: String): Word;
    function Path(const fn: String): String; virtual; abstract;
    procedure RestoreFile(const where: String);
    function ResVers(const ResName: string; Product: boolean = False): Integer;
    function RunBRCC(const rc, res, opts: String): Boolean;
    function RunProcess(const ExecCmd: String): Cardinal;
    procedure Show(const msg: String; Err: Boolean = False); virtual; abstract;
    function Res_Version(const ResName: string; ProdVers: boolean; var MS, LS:
        cardinal): Boolean;
    function VerToInt(const str: String): Integer;
    function VerToStr(ver: Integer; Comma: Boolean = False): String;
    property Version[Index: TVersions]: Integer Read GetVersion;
  end;


implementation

uses
  ZMUtils, ZMMsg, ZipMstr, ZMStructs;

{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

const
  BakExt = '.bak';


function IsSpecial(const uname: String): Boolean;
begin
  Result := False;
end;


constructor TToolHelper.Create;
begin
  inherited;
end;

procedure TToolHelper.BackupFile(const where: String);
var
  fn: String;
begin
  fn := Path(Where);
  if FileExists(fn) then
  begin
    if FileExists(fn + BakExt) then
    begin
      SysUtils.DeleteFile(fn + BakExt);
    end;

    SysUtils.RenameFile(fn, fn + BakExt);
  end;
end;

function TToolHelper.CanWrite(const fspec: String): Boolean;
var
  hFile: Integer;
  sr: TSearchRec;
  fs: String;
begin
  Result := False;
  fs := Path(fspec);
  if SysUtils.FindFirst(fs, faAnyFile, sr) = 0 then
  begin
    Result := (sr.Attr and faReadOnly) = 0;
    SysUtils.FindClose(sr);
    if Result then
    begin
      // exists and is not read-only - test locked
      hFile  := SysUtils.FileOpen(fs, fmOpenWrite);
      Result := hFile > -1;
      if Result then
      begin
        SysUtils.FileClose(hFile);
      end;
    end;
    exit;
  end;
  // file did not exist - try to create it
  hFile := FileCreate(fs);
  if hFile > -1 then
  begin
    FileClose(hFile);
    SysUtils.DeleteFile(fs);
    Result := True;
  end;
end;

procedure TToolHelper.CheckCanWrite(const fn: String);
begin
  if not CanWrite(fn) then
  begin
    raise Exception(' Cannot write: ' + fn);
  end;
end;

function TToolHelper.fCopy(const ffrom, fto: String): Integer;
var
  fs, fo: TFileStream;
begin
{$Hints off}
  Result := -1;
  fo := nil;
  fs := nil;
  try
    fs := TFileStream.Create(ffrom, fmOpenRead);
    fo := TFileStream.Create(fto, fmCreate);
    if fo.CopyFrom(fs, fs.Size) = fs.Size then
    begin
      Result := 0;
    end;
    Show('copied ' + ffrom + ' to ' + fto);
  finally
    FreeAndNil(fs);
    FreeAndNil(fo);
  end;
end;

procedure TToolHelper.Licence(strs: TStrings; Stamp: string = ''; CStyle: Boolean = False);
const
  DividerLength = 50;
var
  S: string;
begin
  if CStyle then
    S := '/* '
  else
    S := '(* ';
  strs.Add(S + StringOfChar('*', DividerLength));
  strs.Add('TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.');
  strs.Add('  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.');
  strs.Add('Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler');
  strs.Add('Copyright (C) 1992-2008 Eric W. Engler');
  strs.Add('Copyright (C) 2009, 2010, 2011, 2012 Russell Peters and Roger Aelbrecht');
  strs.Add(' ');
  strs.Add('All rights reserved.');
  strs.Add('For the purposes of Copyright and this license "DelphiZip" is the current');
  strs.Add(' authors, maintainers and developers of its code:');
  strs.Add('  Russell Peters and Roger Aelbrecht.');
  strs.Add(' ');
  strs.Add('Redistribution and use in source and binary forms, with or without');
  strs.Add(' modification, are permitted provided that the following conditions are met:');
  strs.Add('* Redistributions of source code must retain the above copyright');
  strs.Add('   notice, this list of conditions and the following disclaimer.');
  strs.Add('* Redistributions in binary form must reproduce the above copyright');
  strs.Add('   notice, this list of conditions and the following disclaimer in the');
  strs.Add('   documentation and/or other materials provided with the distribution.');
  strs.Add('* DelphiZip reserves the names "DelphiZip", "ZipMaster", "ZipBuilder",');
  strs.Add('   "DelZip" and derivatives of those names for the use in or about this');
  strs.Add('   code and neither those names nor the names of its authors or');
  strs.Add('   contributors may be used to endorse or promote products derived from');
  strs.Add('   this software without specific prior written permission.');
  strs.Add(' ');
  strs.Add('THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"');
  strs.Add(' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE');
  strs.Add(' IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE');
  strs.Add(' ARE DISCLAIMED. IN NO EVENT SHALL DELPHIZIP, IT''S AUTHORS OR CONTRIBUTERS BE');
  strs.Add(' LIABLE FOR ANYDIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR');
  strs.Add(' CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF');
  strs.Add(' SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS');
  strs.Add(' INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN');
  strs.Add(' CONTRACT, STRICT LIABILITY, OR TORT(INCLUDING NEGLIGENCE OR OTHERWISE)');
  strs.Add(' ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE');
  strs.Add(' POSSIBILITY OF SUCH DAMAGE.');
  strs.Add(' ');
  strs.Add('contact: problems AT delphizip DOT org');
  strs.Add('updates: http://www.delphizip.org');
  if CStyle then
    S := ' */'
  else
    S := ' *)';
  strs.Add(StringOfChar('*', DividerLength) + S);
  if Stamp <> '' then
    strs.Add('//' + Stamp + ' ' + FormatDateTime('yyyy-mm-dd', Now));
  strs.Add(' ');
end;

function TToolHelper.LIdOfRes(const fn: String): Word;
var
  fs:  TFileStream;
  blk: array of Word;
  rid: Integer;
  DatSiz, HedSiz: Cardinal;
  sz:  Cardinal;
  bp, hp: Integer;
begin
  Result := $FFFF;
  if not FileExists(fn) then
  begin
    exit;
  end;
  try
    fs := TFileStream.Create(fn, fmOpenRead);
    if fs.Size > 40000 then
    begin
      exit;
    end;
    SetLength(blk, Cardinal(fs.Size));
    fs.ReadBuffer(blk[0], fs.Size);
    bp := 0;
    while (bp + 8) < HIGH(Blk) do
    begin
      bp := (bp + 1) and $7fffE;  // dword align
      DatSiz := (blk[bp + 1] shl 16) + blk[bp];
      HedSiz := (blk[bp + 3] shl 16) + blk[bp + 2];
      sz := HedSiz + DatSiz - 8;
      hp := bp + 4;
      Inc(bp, 4 + (sz div 2));
      if blk[hp] <> $ffff then
      begin
        continue;
      end;  // bad res type
      if blk[hp + 1] <> 6 then
      begin
        continue;
      end;      // not string table
      if blk[hp + 2] <> $ffff then
      begin
        continue;
      end;      // not numbered
      rid := pred(blk[hp + 3]);
      if (rid < (GE_FatalZip div 16)) or (rid > (TM_SystemError div 16)) then
      begin
        continue;
      end;
      Result := blk[hp + 7];  // language id
      break;
    end;
  finally
    FreeAndNil(fs);
    blk := nil;
  end;
end;


procedure TToolHelper.RestoreFile(const where: String);
var
  fn: String;
begin
  fn := Path(Where);
  if FileExists(fn + BakExt) then
  begin
    if FileExists(fn) then
    begin
      SysUtils.DeleteFile(fn);
    end;
    SysUtils.RenameFile(fn + BakExt, fn);
  end;
end;

function TToolHelper.ResVers(const ResName: string; Product: boolean = False):
    Integer;
var
  LS: DWORD;
  MS: DWORD;
begin
  Result := -1;
  if Res_Version(ResName, Product, MS, LS) then
  begin
    Result := (Integer(MS) shr 16) * 1000000;
    Result := Result + (Integer(MS and $FFFF) * 100000);
    Result := Result + ((Integer(LS) shr 16) * 10000);
    Result := Result + Integer(LS and $FFFF);
  end;
end;

function TToolHelper.RunBRCC(const rc, res, opts: String): Boolean;
var
  cmd, fres: String;
begin
  fres := Path(res);
  Result := False;
  if not CanWrite(fres) then
  begin
    Show('BRCC32.exe fails - cannot write: ' + fres, True);
    exit;
  end;

  cmd := Path('"$(BCB)\bin\BRCC32.exe" -32  ') + opts + ' -fo "' +
    fres + '" "' + Path(rc) + '"';
  Result := RunProcess(cmd) = 0;
  if not Result then
  begin
    Show('BRCC32.exe failed', True);
  end;
end;

function TToolHelper.RunProcess(const ExecCmd: String): Cardinal;
const
  CR = #$0D;
  LF = #$0A;
  TerminationWaitTime = 5000;
var
  si: TStartupInfo;
  pi: TProcessInformation;
  sa: TSecurityAttributes;
  sd: TSecurityDescriptor;
  lpsa: PSecurityAttributes;

  TempHandle, WriteHandle, ReadHandle: THandle;
  ReadBuf: array[0..$100] of AnsiChar;
  BytesRead: Cardinal;
  LineBuf: array[0..$100] of Char;
  LineBufPtr: Integer;
  Newline: Boolean;
  i: Integer;

  procedure OutputLine;
  begin
    LineBuf[LineBufPtr] := #0;
    Show('|  ' + LineBuf);
    Newline := False;
    LineBufPtr := 0;
  end;

begin
  Show('Running process ' + ExecCmd);
  lpsa := nil;
  FillChar(si, SizeOf(TStartupInfo), 0);
  FillChar(ReadBuf, SizeOf(ReadBuf), 0);
  FillChar(sa, SizeOf(TSecurityAttributes), 0);
  LineBufPtr := 0;
  Newline := True;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    InitializeSecurityDescriptor(@sd, SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(@sd, True, nil, False);
    sa.nLength := sizeof(TSecurityAttributes);
    sa.bInheritHandle := True;
    sa.lpSecurityDescriptor := @sd;
    lpsa := @sa;
  end;
  CreatePipe(ReadHandle, WriteHandle, lpsa, 0);
  {create a pipe to act as StdOut for the child. The write end will need
   to be inherited by the child process}

  try
    {Read end should not be inherited by child process}
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      if not SetHandleInformation(ReadHandle, HANDLE_FLAG_INHERIT, 0) then
      begin
        RaiseLastOSError;
        exit;
      end;
    end
    else
    begin
      {SetHandleInformation does not work under Window95, so we
      have to make a copy then close the original}
      if not DuplicateHandle(GetCurrentProcess, ReadHandle, GetCurrentProcess,
        @TempHandle, 0, True, DUPLICATE_SAME_ACCESS) then
      begin
        RaiseLastOSError;
        exit;
      end;
      CloseHandle(ReadHandle);
      ReadHandle := TempHandle;
    end;

    with si do
    begin
      cb := SizeOf(TStartupInfo);
      dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
      wShowWindow := SW_HIDE;
      hStdOutput := WriteHandle;
    end;

    if not CreateProcess(nil, PChar(ExecCmd), nil, nil, True,
      {inherit kernel object handles from parent}
      NORMAL_PRIORITY_CLASS or CREATE_NO_WINDOW,
               {DETACHED_PROCESS relevant for Console parent only
               No need to create an output window - it would be
               blank anyway}
      nil, nil, si, pi) then
    begin
      RaiseLastOSError;
      exit;
    end;

    CloseHandle(pi.hThread);
    {not interested in threadhandle - close it}

    CloseHandle(WriteHandle);
    {close our copy of Write handle - Child has its own copy now.}

    try
      while ReadFile(ReadHandle, ReadBuf, SizeOf(ReadBuf), BytesRead, nil) do
      begin
        for  i := 0 to BytesRead - 1 do
        begin
          if (ReadBuf[i] = LF) then
          begin
            Newline := True;
          end
          else
          if (ReadBuf[i] = CR) then
          begin
            OutputLine;
          end
          else
          begin
            LineBuf[LineBufPtr] := Char(ReadBuf[i]);
            Inc(LineBufPtr);
            if LineBufPtr >= (SizeOf(LineBuf) - 1) then
            begin   {line too long - force a break}
              Newline := True;
              OutputLine;
            end;
          end;
        end;
      end;
          {There are much more efficient ways of doing this: we don't really
        need two buffers, but we do need to scan for CR & LF &&&}
      WaitForSingleObject(pi.hProcess, TerminationWaitTime);
      GetExitCodeProcess(pi.hProcess, Result);
      OutputLine; {flush the line buffer}
      if Result <> 0 then
      begin
        Show('RunProcess returned ' + IntToStr(Result), True);
      end;
    finally
      CloseHandle(pi.hProcess)
    end
  finally
    CloseHandle(ReadHandle);
  end;
end;

procedure TToolHelper.TemplaterMsg(Sender: TTemplater; const msg: string);
begin
  Show('** ' + msg);
end;

// 'read' the File/Product version of Version_info 1
function TToolHelper.Res_Version(const ResName: string; ProdVers: boolean; var
    MS, LS: cardinal): Boolean;
var
  fs:  TFileStream;
  blk: array of Word;
  rid: Integer;
  DatSiz, HedSiz: Cardinal;
  sz:  Cardinal;
  Block, hp, Data: Cardinal;
  K: Cardinal;
begin
  Result := false;
  if not FileExists(ResName) then
  begin
    exit;
  end;
  try
    fs := TFileStream.Create(ResName, fmOpenRead);
    if fs.Size < 500 then
    begin
      exit;
    end;
    SetLength(blk, Cardinal(fs.Size));
    fs.ReadBuffer(blk[0], fs.Size);
    Block := 0;
    while Integer(Block + 8) < HIGH(Blk) do
    begin
      Block := (Block + 1) and $7fffE;  // dword align
      DatSiz := (blk[Block + 1] shl 16) + blk[Block];
      HedSiz := (blk[Block + 3] shl 16) + blk[Block + 2];
      Data := Block + (HedSiz div Sizeof(WORD));
      sz := HedSiz + DatSiz - 8;
      hp := Block + 4;
      Inc(Block, 4 + (sz div 2));  // next block
      if blk[hp] <> $ffff then
        continue; // bad res type
      if blk[hp + 1] <> 16 then
        continue;  // not verinfo
      if blk[hp + 2] <> $ffff then
        continue;  // not numbered
      if blk[hp + 3] <> 1 then
        continue;  // not '1'
      if blk[Data + 2] <> 0 then
        continue;  // not binary
      if blk[Data + 3] <> $56 then
        continue;  // not 'V'S_VERSION_INFO
      if blk[Data + $14] <> $04BD then
        continue;  // not signature
      if blk[Data + $15] <> $FEEF then
        continue;  // not signature
      if blk[Data + $16] <> 0 then
        continue;  // wrong struct version
      if blk[Data + $17] <> 1 then
        continue;  // wrong struct version
      // we found it
      if ProdVers then
        hp := Data + $1c
      else
        hp := Data + $18;
      K := (blk[hp + 1] shl 16) + blk[hp];
      MS := K;
      K := (blk[hp + 3] shl 16) + blk[hp + 2];
      LS := K;
      Result := True;
      break;
    end;
  finally
    FreeAndNil(fs);
    blk := nil;
  end;
end;


function TToolHelper.VerToInt(const str: String): Integer;
const
  Muls: array [0 .. 3] of Integer = (1, 10, 100, 1000);
var
  i, v, ver, fld: Integer;
begin
  Result := -1;
  i := 1;
  ver := 0;
  fld := 0;
  v := 0;
  while i <= length(str) do
  begin
    if CharInSet(str[i], ['0'..'9']) then
    begin
      v := (v * 10) + (Ord(str[i]) and 15);
    end
    else
    if str[i] = '.' then
    begin
      Ver := (Ver * Muls[fld]) + v;
      v := 0;
      Inc(fld);
      if fld > 3 then
        exit;  // invalid
    end
    else
      exit; // invalid
    Inc(i);
  end;
  Ver := (Ver * Muls[fld]) + v;
  if (Ver >= Version[vcVCL]) and (Ver < (Version[vcVCL] + 9999)) then
  begin
    Result := Ver;
  end;
end;

function TToolHelper.VerToStr(ver: Integer; Comma: Boolean = False): String;
begin
  Result := VersStr(ver, Comma);
end;


end.
