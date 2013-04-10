unit ZMUtils;

//  ZMUtils.pas - Some utility functions

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
//modified 2011-11-20

{$INCLUDE   '.\ZipVers.inc'}

{$IFDEF VERD6up}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF WIN64}
  {$DEFINE NO_ASM}
{$ENDIF}

interface

uses
  SysUtils, Windows, Classes, ZipMstr, Graphics;

type
  TPathSlashDirection = (psdExternal, psdInternal);
//  DeleteOpts = (htdFinal, htdAllowUndo);
(*
type
{$IFDEF UNICODE}
  TZMRawBytes = RawByteString;
{$ELSE}
  TZMRawBytes =  AnsiString;
{$ENDIF}
*)
const                      // QueryZip return bit values and errors
  zqbStartEXE     = 1;     // is EXE file may be SFX
  zqbStartLocal   = 2;     // normal zip file start
  zqbStartSpan    = 4;     // first part of span
  zqbStartCentral = 8;     // continuing Central Header
  zqbHasComment   = 16;
  //  zqbGoodComment = 16;  // comment length good (no junk at end)
  zqbHasLocal     = 32;    // first Central entry points to local header
  zqbHasCentral   = 64;    // Central entry where it should be
  zqbHasEOC       = 128;   // End of Central entry
  zqbHasLoc64     = 256;   // EOC64 locator entry
  zqbHasEOC64     = 512;   // Zip64 EOC
  zqbJunkAtEnd    = 1024;  // junk at end of zip
  zqbIsDiskZero   = 2048;  // is disk 0

  zqFieldError   = -5;     // bad field value
  zqFileError    = -7;     // file handling error
  zqGeneralError = -9;     // unspecified failure


function AbsErr(err: Integer): Integer;
function AttribStr(attrs: Cardinal): String;
function DelimitPath(const Path: String; Sep: Boolean): String;
function WinPathDelimiters(const path: string): string;

function DirExists(const FName: String): Boolean;

function DiskAvailable(const path: String): Boolean;

function ExtractNameOfFile(const FileName: String): String;

function ExeVersion(const FName: TZMString; var MS, LS: DWORD): Boolean;
function ExeVers(const FName: String): Integer;
function VersStr(vers: Integer; Comma: Boolean = False): String;

// stable replacement for depreciated FileAge()
function File_Age(const FName: String): Cardinal;

procedure File_Close(var fh: Integer);

procedure File_Delete(const FName: String);

function File_Size(const FSpec: TFilename): Int64;

function ForceDirectory(const DirName: String): Boolean;

function GetVolumeLabel(const drive: String): String;

function Hi64(i: Int64): Cardinal;

function IsSameFile(const FName1, FName2: String): Boolean;

function IsWild(const FSpec: String): Boolean;
//  returns position of first wild character or 0
function HasWild(const FSpec: String): Integer;
function HasWildW(const FSpec: WideString): Integer;

//  true we're running under XP or later.
function IsWinXP: Boolean;
function WinVersion: Integer;

function IsZipSFX(const SFXExeName: String): Integer;

function Lo64(i: Int64): Cardinal;

function PathConcat(const path, extra: String): String;

function QueryZip(const FName: String): Integer;

function SetSlash(const path: String; dir: TPathSlashDirection): String;
function SetSlashW(const path: WideString; dir: TPathSlashDirection): WideString;

function StrToOEM(const astr: String): String;
function OEMToStr(const astr: Ansistring): String;
//1 return True if contains chars (<#31 ?) >#126
function StrHasExt(const astr: String): Boolean; overload;
{$IFDEF UNICODE}
function StrHasExt(const astr: AnsiString): Boolean; overload;
function StrHasExt(const astr: TZMRawBytes): Boolean; overload;
{$ENDIF}
function LastPos(const s: String; ch: Char; before: Integer = MAXINT): Integer;
function LastPosW(const s: WideString; wch: Widechar; before: Integer = MAXINT): Integer;

function CalcCRC32(const mem; len: Integer; init: DWORD): DWORD;
function Update_CRC32(init: DWORD; p: PByte; len: Integer): DWORD;
function Get_CRC32_Table: PDWORD;

function OpenResStream(const ResName: String; const rtype: PChar): TResourceStream;

function LastChar(const Name: String): Char;
{$IFDEF UNICODE}
overload;
function LastChar(const Name: TZMRawBytes): AnsiChar;  overload;
{$ENDIF}

function IsFolder(const Name: String): Boolean;
{$IFDEF UNICODE}
overload;
function IsFolder(const name: TZMRawBytes): boolean; overload;
{$ENDIF}

function CanHash(const FSpec: String): Boolean;

// return true if filename is obviously invalid
function NameIsBad(const astr: String; AllowWild: Boolean): Boolean;

 // return exe size (if < 4G)
 //    0 _ not exe
function ExeSize(const Name: String): Cardinal; overload;
function ExeSize(fileHandle: Integer): Cardinal; overload;


 // check for SFX header or detached header
 // return <0 error
const
  cstNone = 0;        // not found
  cstExe  = 1;        // might be stub of unknown type
  cstSFX17 = 17;      // found 1.7 SFX headers
  cstSFX19 = 19;      // found 1.9 SFX headers
  cstDetached = 2048; // is detached - if name specified ZipName will modified for it

function CheckSFXType(const fileHandle: Integer; var ZipName: String;
  var size: Integer): Integer; overload;
function CheckSFXType(const Name: String; var ZipName: String;
  var size: Integer): Integer; overload;

function FileDateToLocalDateTime(stamp: Integer): TDateTime;
function FileTimeToLocalDOSTime(const ft: TFileTime): Cardinal;

function _XData(const xdat: PByte; len: Word; Tag: Word; var idx, size: Integer):
    Boolean;
function XData(const x: TZMRawBytes; Tag: Word; var idx, size: Integer):
    Boolean;
function XDataAppend(var x: TZMRawBytes; const src1; siz1: Integer; const src2;
    siz2: Integer): Integer;
function XDataKeep(const x: TZMRawBytes; const tags: array of Integer):
    TZMRawBytes;
function XDataRemove(const x: TZMRawBytes; const tags: array of Integer):
    TZMRawBytes;

function HashFunc(const str: String): Cardinal;
function HashFuncNoCase(const str : String) : Cardinal;

procedure SplitArgs(const args: string; var main: string; var filearg: string;
   var switches: string; var password: string);

function QualifiedName(const ZipName: string; const FileName: string): string;
procedure SplitQualifiedName(const QName: string; var ZipName: string;
    var FileName: string);
function ZSplitString(const Delim, Raw: string; var Rest: string): String;
// split string at last delim
function ZSplitStringLast(const Delim, Raw: string; var Rest: string): String;

// find last SubStr in a string
function RPos(const SubStr, AString: String): Integer; overload;
function RPos(const SubStr, AString: String; StartPos: Integer): Integer; overload;


function GetFirstIcon(str: TMemoryStream): TIcon;
// replaces an icon in an executable file (stream)
procedure ReplaceIcon(str: TMemoryStream; oIcon: TIcon);
function WriteIconToStream(Stream: Classes.TStream; Icon: HICON;
  Width, Height, Depth: Integer): Integer; forward;
// -------------------------- ------------ -------------------------
implementation

uses ZMStructs, ShellApi, Forms, ZMUTF8, ZMSFXInt, ZMWFuncs, ZMXcpt,
  ZMMsg;

const
  __UNIT__ = 28 shl 23;

type
  TInt64Rec = packed record
    case Integer of
      0: (I: Int64);
      1: (Lo, Hi: Cardinal);
  end;

const
  CRC32Table: array[0..255] of DWORD = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD,
    $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D,
    $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
    $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC,
    $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB,
    $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA,
    $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE,
    $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409,
    $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
    $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739,
    $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0,
    $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8,
    $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703,
    $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7,
    $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE,
    $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D,
    $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5,
    $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

function Get_CRC32_Table: PDWORD;
begin
  Result := PDWORD(@CRC32Table);
end;

//--------------------------------------------------------
function Lo64(i: Int64): Cardinal;
var
  r: TInt64Rec;
begin
  r.I := i;
  Result := r.Lo;
end;

function Hi64(i: Int64): Cardinal;
var
  r: TInt64Rec;
begin
  r.I := i;
  Result := r.Hi;
end;

//--------------------------------------------------------
function AbsErr(err: Integer): Integer;
begin
  if err < 0 then
    Result := -err
  else
    Result := err;
end;

function AttribStr(attrs: Cardinal): String;
type
  attrval = record
    v: Cardinal;
    d: array[Boolean] of Char;
  end;
const
  AttrVals: array[0..6] of attrval = ((v:1;d:('r','R')),(v:2;d:('h','H')), (v:4;d:('s','S')),
    (v:$20;d:('a','A')), (v:$100;d:('t','T')), (v:$1000;d:('o','O')), (v:$4000;d:('e','E')));
var
  I: Integer;
begin
  SetLength(Result, High(AttrVals) + 1);
  for I := Low(AttrVals) to High(AttrVals) do
    Result[I + 1] := AttrVals[I].d[(attrs and AttrVals[I].v) <> 0];
end;

function DelimitPath(const Path: String; Sep: Boolean): String;
begin
  Result := Path;
  if Length(Path) = 0 then
  begin
    if Sep then
      Result := PathDelim{'\'};
    exit;
  end;
  if (AnsiLastChar(Path)^ = PathDelim) <> Sep then
  begin
    if Sep then
      Result := Path + PathDelim
    else
      Result := Copy(Path, 1, pred(Length(Path)));
  end;
end;

function WinPathDelimiters(const path: string): string;
var
  I: Integer;
begin
  Result := path;
  for I := 1 to Length(Result) do
    if Result[I] = '/' then
      Result[I] := '\';
end;


function DirExists(const FName: String): Boolean;
begin
  Result := _Z_DirExists(FName);
end;

function DiskAvailable(const path: String): Boolean;
var
  drv: Integer;
  em:  Cardinal;
  pth: String;
begin
  Result := False;
  pth := ExpandUNCFileName(path);
  if (length(pth) > 1) and (pth[2] = DriveDelim) then
  begin
    drv := Ord(Uppercase(pth)[1]) - $40;
    em  := SetErrorMode(SEM_FAILCRITICALERRORS);
    Result := DiskSize(drv) <> -1;
    SetErrorMode(em);
  end;
end;

function ExeVersion(const FName: TZMString; var MS, LS: DWORD): Boolean;
begin
  Result := _Z_GetExeVersion(FName, MS, LS);
end;


 // format M.N.RR.BBB
 // return Version as used by DelphiZip
function ExeVers(const FName: String): Integer;
var
  LS: DWORD;
  MS: DWORD;
begin
  Result := -1;
  if ExeVersion(FName, MS, LS) then
  begin
    Result := (Integer(MS) shr 16) * 1000000;
    Result := Result + (Integer(MS and $FFFF) * 100000);
    Result := Result + ((Integer(LS) shr 16) * 10000);
    Result := Result + Integer(LS and $FFFF) mod 1000;
  end;
end;

function ExtractNameOfFile(const FileName: String): String;
var
  I: Integer;
  J: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  J := LastDelimiter('.', FileName);
  if (J <= I) then
  begin
    J := MaxInt;
  end;    // no ext
  Result := Copy(FileName, I + 1, J - (I + 1));
end;

function VersStr(vers: Integer; Comma: Boolean = False): String;
const
  fmt: array [Boolean] of String =
    ('%d.%d.%d.%4.4d', '%d,%d,%d,%d');
begin
  Result := Format(fmt[Comma], [vers div 1000000, (vers mod 1000000) div
    100000, (vers mod 100000) div 10000, vers mod 1000]);
end;

function OpenResStream(const ResName: String; const rtype: PChar): TResourceStream;
var
  hFindRes: Cardinal;
  idNo: Integer;
  inst: Integer;
  rsn:  PChar;
begin
  Result := nil;
  try
    rsn  := PChar(ResName);
    inst := HInstance;
    if (Length(ResName) > 1) and (ResName[1] = '#') then
    begin
      idNo := StrToInt(copy(ResName, 2, 25));
      rsn  := PChar(idNo);
    end;
    hFindRes := FindResource(inst, rsn, rtype);
    if (hFindRes = 0) and ModuleIsLib then
    begin
      inst := MainInstance;
      hFindRes := FindResource(inst, rsn, rtype);
    end;
    if hFindRes <> 0 then
      Result := TResourceStream.Create(inst, ResName, rtype);
  except
    Result := nil;
  end;
end;

function File_Age(const FName: String): Cardinal;
var
  LocalFileTime: TFileTime;
  r: Integer;
  SRec: _Z_TSearchRec;
begin
  r := _Z_FindFirst(FName, faAnyFile, SRec);
  if r = {<>} 0 then
  begin
    FileTimeToLocalFileTime(SRec.FindData.ftLastWriteTime, LocalFileTime);
    _Z_FindClose(SRec);
    if FileTimeToDosDateTime(LocalFileTime, LongRec(Result).Hi,
        LongRec(Result).Lo) then
      Exit;
  end;
  Result := Cardinal(-1);
end;

procedure File_Close(var fh: Integer);
var
  h: Integer;
begin
  if fh <> Invalid_Handle then
  begin
    h  := fh;
    fh := Invalid_Handle;
    FileClose(h);
  end;
end;

procedure File_Delete(const FName: String);
begin
  if _Z_FileExists(FName) then
    _Z_DeleteFile(FName);
end;

function File_Size(const FSpec: TFilename): Int64;
var
  sr: _Z_TSearchRec;
begin
  Result := 0;
  if _Z_FindFirst(FSpec, faAnyFile, sr) = 0 then
  begin
    Result := sr.Size;
    _Z_FindClose(sr);
  end;
end;

function ForceDirectory(const DirName: String): Boolean;
begin
  Result :=_Z_ForceDirectory(DirName);
end;

(*? HasWild
  returns position of first wild character or 0
*)
function HasWild(const FSpec: String): Integer;
var
  c: Char;
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(FSpec) do
  begin
    c := FSpec[i];
    if (c = WILD_MULTI) or (c = WILD_CHAR) then
    begin
      Result := i;
      break;
    end;
  end;
end;


(*? HasWildW
  returns position of first wild character or 0
*)
function HasWildW(const FSpec: WideString): Integer;
var
  c: Widechar;
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(FSpec) do
  begin
    c := FSpec[i];
    if (c = WILD_MULTI) or (c = WILD_CHAR) then
    begin
      Result := i;
      break;
    end;
  end;
end;

(*? IsWild
1.73.4
 returns true if filespec contains wildcard(s)
*)
function IsWild(const FSpec: String): Boolean;
var
  c: Char;
  i: Integer;
  len: Integer;
begin
  Result := True;
  len := Length(FSpec);
  i := 1;
  while i <= len do
  begin
    c := FSpec[i];
    if (c = WILD_MULTI) or (c = WILD_CHAR) then
      exit;
    Inc(i);
  end;
  Result := False;
end;

(* ? IsZipSFX
Return value:
0 = The specified file is not a SFX
>0 = It is one
-7  = Open, read or seek error
-8  = memory error
-9  = exception error
-10 = all other exceptions
*)
function IsZipSFX(const SFXExeName: String): Integer;
const
  SFXsig = zqbStartEXE or zqbHasCentral or zqbHasEOC;
var
  n: string;
  r: Integer;
  sz: Integer;
begin
  r := QueryZip(SFXExeName);
  // SFX = 1 + 128 + 64
  Result := 0;
  if (r and SFXsig) = SFXsig then
    Result := CheckSFXType(SFXExeName, n, sz);
end;

function CanHash(const FSpec: String): Boolean;
var
  c: Char;
  i: Integer;
  len: Integer;
begin
  Result := False;
  len := Length(FSpec);
  i := 1;
  while i <= len do
  begin
    c := FSpec[i];
    if (c = WILD_MULTI) or (c = WILD_CHAR) or (c = SPEC_SEP) then
      exit;
    Inc(i);
  end;
  Result := True;
end;

//  Returns a boolean indicating whether or not we're running under XP or later.
function IsWinXP: Boolean;
var
  osv: TOSVERSIONINFO;
begin
  osv.dwOSVersionInfoSize := sizeOf(OSVERSIONINFO);
  GetVersionEx(osv);
  //   result := ( osv.dwPlatformId = VER_PLATFORM_WIN32_NT );
  Result := (osv.dwMajorVersion > 5) or ((osv.dwMajorVersion = 5) and
    (osv.dwMinorVersion >= 1));
end;

//  Returns a boolean indicating whether or not we're running under XP or later.
function WinVersion: Integer;
var
  osv: TOSVERSIONINFO;
begin
  osv.dwOSVersionInfoSize := sizeOf(OSVERSIONINFO);
  GetVersionEx(osv);
  Result := (osv.dwMajorVersion * 100) + osv.dwMinorVersion;
end;

(*? SetSlash
1.76 use enum  TPathSlashDirection = (psdExternal, psdInternal)
1.73
forwardSlash = false = Windows normal backslash '\'
forwardSlash = true = forward slash '/'
*)
function SetSlash(const path: String; dir: TPathSlashDirection): String;
{$IFDEF Delphi7up}
begin
  if dir = psdInternal then
    Result := AnsiReplaceStr(path, PathDelim, PathDelimAlt)
  else
    Result := AnsiReplaceStr(path, PathDelimAlt, PathDelim);
end;
{$ELSE}
var
  c, f, r: Char;
  i, len:  Integer;
begin
  Result := path;
  len := Length(path);
  if dir = psdInternal then
  begin
    f := PathDelim{'\'};
    r := PathDelimAlt;//'/';
  end
  else
  begin
    f := PathDelimAlt;//'/';
    r := PathDelim{'\'};
  end;
  i := 1;
  while i <= len do
  begin
    c := path[i];
{$ifndef UNICODE}
    if c in LeadBytes then
    begin
      Inc(i, 2);
      continue;
    end;
{$endif}
    if c = f then
      Result[i] := r;
    Inc(i);
  end;
end;

{$ENDIF}

function SetSlashW(const path: WideString; dir: TPathSlashDirection): WideString;
var
  c: Widechar;
  f: Widechar;
  i: Integer;
  len: Integer;
  r: Widechar;
begin
  Result := path;
  len := Length(path);
  if dir = psdInternal then
  begin
    f := PathDelim;
    r := PathDelimAlt;
  end
  else
  begin
    f := PathDelimAlt;
    r := PathDelim;
  end;
  i := 1;
  while i <= len do
  begin
    c := path[i];
    if c = f then
      Result[i] := r;
    Inc(i);
  end;
end;

 //---------------------------------------------------------------------------
 // concat path
function PathConcat(const path, extra: String): String;
var
  pathLen: Integer;
  pathLst: Char;
begin
  pathLen := Length(path);
  Result  := path;
  if pathLen > 0 then
  begin
    //    pathLst := path[pathLen];
    pathLst := AnsiLastChar(path)^;
    if (pathLst <> DriveDelim) and (Length(extra) > 0) then
      if (extra[1] = PathDelim) = (pathLst = PathDelim) then
        if pathLst = PathDelim then
          Result := Copy(path, 1, pathLen - 1) // remove trailing
        else
          Result := path + PathDelim;
  end;
  Result := Result + extra;
end;


 //const           // QueryZip return bit values and errors
 //  zqbStartEXE    = 1;     // is EXE file may be SFX
 //  zqbStartLocal  = 2;     // normal zip file start
 //  zqbStartSpan   = 4;     // first part of span
 //  zqbStartCentral = 8;    // continuing Central Header
 //  zqbHasComment  = 16;
 ////  zqbGoodComment = 16;  // comment length good (no junk at end)
 //  zqbHasLocal    = 32;    // first Central entry points to local header
 //  zqbHasCentral  = 64;    // Central entry where it should be
 //  zqbHasEOC      = 128;   // End of Central entry
 //  zqbHasLoc64    = 256;   // EOC64 locator entry
 //  zqbHasEOC64    = 512;   // Zip64 EOC
 //  zqbJunkAtEnd   = 1024;  // junk at end of zip
 //  zqbIsDiskZero  = 2048;  // is disk 0

 //  zqFieldError   = -5;    // bad field value
 //  zqFileError    = -7;     // file handling error
 //  zqGeneralError = -9;  // unspecified failure

function QueryZip(const FName: String): Integer;
const
  FileMask = (zqbStartEXE or zqbStartLocal or zqbStartSpan or
    zqbStartCentral or zqbHasComment or zqbJunkAtEnd);
var
  Buf: array of Byte;
  BufPos: Integer;
  CenDisk: Cardinal;
  CenOfs: Int64;
  DoCenDir: Boolean;
  EOC: TZipEndOfCentral;
  EOCLoc: TZip64EOCLocator;
  EOCPossible: Boolean;
  FileHandle: Integer;
  File_Sze: Int64;
  fn:  String;
  fs:  Int64;
  Need64: Boolean;
  pEOC: PZipEndOfCentral;
  pEOCLoc: PZip64EOCLocator;
  Pos0: Integer;
  ReadPos: Cardinal;
  res: Integer;
  Sig: Cardinal;
  Size: Integer;
  ThisDisk: Cardinal;

  function NeedLoc64(const QEOC: TZipEndOfCentral): Boolean;
  begin
    Result := (QEOC.ThisDiskNo = MAX_WORD) or (QEOC.CentralDiskNo = MAX_WORD) or
      (QEOC.CentralEntries = MAX_WORD) or (QEOC.TotalEntries = MAX_WORD) or
      (QEOC.CentralSize = MAX_UNSIGNED) or (QEOC.CentralOffset = MAX_UNSIGNED);
  end;
  // check central entry and, if same disk, its local header signal
  function CheckCen(fh: Integer; This_Disk: Cardinal; CenOf: Int64): Integer;
  type
    TXData_tag = packed record
      tag: Word;
      siz: Word;
    end;
    PXData_tag = ^TXData_tag;

  var
    ret: Integer;
    CentralHead: TZipCentralHeader;
    Sgn: Cardinal;
    Ofs: Int64;
    xbuf: array of Byte;
    xlen, ver: Integer;
    wtg, wsz: Word;
    has64: Boolean;
    p: PByte;
  begin  // verify start of central
    ret := 0;
    Result := zqFieldError;
    if (FileSeek(fh, CenOf, soFromBeginning) <> -1) and
      (FileRead(fh, CentralHead, sizeof(CentralHead)) = sizeof(CentralHead)) and
      (CentralHead.HeaderSig = CentralFileHeaderSig) then
    begin
      ret := zqbHasCentral;               // has linked Central
      if (CentralHead.DiskStart = This_Disk) then
      begin
        ver := CentralHead.VersionNeeded;
        if (ver and VerMask) > ZIP64_VER then
          exit;
        Ofs := CentralHead.RelOffLocal;
        if (Ofs = MAX_UNSIGNED) and ((ver and VerMask) >= ZIP64_VER) then
        begin
          if ver > 45 then
            exit;     // bad version
          // have to read extra data
          xlen := CentralHead.FileNameLen + CentralHead.ExtraLen;
          SetLength(xbuf, xlen);  // easier to read filename + extra
          if FileRead(fh, xbuf, xlen) <> xlen then
            exit;                  // error
          // find Zip64 extra data
          has64 := False;
          xlen := CentralHead.ExtraLen;
          p := @xbuf[CentralHead.FileNameLen];
          wsz := 0;   // keep compiler happy
          while xlen > sizeof(TXData_tag) do
          begin
            wtg := PXData_tag(p)^.tag;
            wsz := PXData_tag(p)^.siz;
            if wtg = Zip64_data_tag then
            begin
              has64 := xlen >= (wsz + sizeof(TXData_tag));
              break;
            end;
            Inc(p, wsz + sizeof(TXData_tag));
          end;
          if (not has64) or (wsz > (xlen - sizeof(TXData_tag))) then
            exit;              // no data so rel ofs is bad
          Inc(p, sizeof(TXData_tag));  // past header
          // locate offset  - values only exist if needed
          if CentralHead.UncomprSize = MAX_UNSIGNED then
          begin
            if wsz < sizeof(Int64) then
              exit;           // bad
            Inc(p, sizeof(Int64));
            Dec(wsz, sizeof(Int64));
          end;
          if CentralHead.ComprSize = MAX_UNSIGNED then
          begin
            if wsz < sizeof(Int64) then
              exit;           // bad
            Inc(p, sizeof(Int64));
            Dec(wsz, sizeof(Int64));
          end;
          if wsz < sizeof(Int64) then
            exit;             // bad
          Ofs := PInt64(p)^;
        end;
        if (FileSeek(fh, Ofs, 0) <> -1) and
          (FileRead(fh, Sgn, sizeof(Sgn)) = sizeof(Sgn)) and
          (Sgn = LocalFileHeaderSig) then
          ret := zqbHasCentral or zqbHasLocal;     // linked local
      end;
    end;
    Result := ret;
  end;

begin
  EOCPossible := False;
  Result := zqFileError;
  DoCenDir := True;   // test central too
  if (FName <> '') and (FName[1] = '|') then
  begin
    DoCenDir := False;
    fn := copy(FName, 2, length(FName) - 1);
  end
  else
    fn := FName;
  fn := Trim(fn);
  if fn = '' then
    exit;
  FileHandle := Invalid_Handle;
  res := 0;
  try
    try
      // Open the input archive, presumably the last disk.
      FileHandle := _Z_FileOpen(fn, fmShareDenyWrite or fmOpenRead);
      if FileHandle = Invalid_Handle then
        exit;
      Result := 0;                          // rest errors normally file too small

      // first we check if the start of the file has an IMAGE_DOS_SIGNATURE
      if (FileRead(FileHandle, Sig, sizeof(Cardinal)) <> sizeof(Cardinal)) then
        exit;
      if LongRec(Sig).Lo = IMAGE_DOS_SIGNATURE then
        res := zqbStartEXE
      else
      if Sig = LocalFileHeaderSig then
        res := zqbStartLocal
      else
      if Sig = CentralFileHeaderSig then
        res := zqbStartCentral
      // part of split Central Directory
      else
      if Sig = ExtLocalSig then
        res := zqbStartSpan;            // first part of span

      // A test for a zip archive without a ZipComment.
      fs := FileSeek(FileHandle, -Int64(sizeof(EOC)), soFromEnd);
      if fs = -1 then
        exit;                           // not zip - too small
      File_Sze := fs;
      // try no comment
      if (FileRead(FileHandle, EOC, sizeof(EOC)) = sizeof(EOC)) and
        (EOC.HeaderSig = EndCentralDirSig) and (EOC.ZipCommentLen = 0) then
      begin
        EOCPossible := True;
        res := res or zqbHasEOC;
        CenDisk := EOC.CentralDiskNo;
        ThisDisk := EOC.ThisDiskNo;
        CenOfs := EOC.CentralOffset;
        Need64 := NeedLoc64(EOC);
        if (CenDisk = 0) and (ThisDisk = 0) then
          res := res or zqbIsDiskZero;
        // check Zip64 EOC
        if Need64 and (fs > sizeof(TZip64EOCLocator)) then
        begin   // check for locator
          if (FileSeek(FileHandle, fs - sizeof(TZip64EOCLocator), soFromBeginning) <>
            -1) and (FileRead(FileHandle, EOCLoc, sizeof(TZip64EOCLocator)) =
            sizeof(TZip64EOCLocator)) and (EOCLoc.LocSig = EOC64LocatorSig) then
          begin  // found possible locator
            res := res or zqbHasLoc64;
            CenDisk := 0;
            ThisDisk := 1;
            CenOfs := -1;
          end;
        end;
        if DoCenDir and (CenDisk = ThisDisk) then
        begin
          res := res or CheckCen(FileHandle, ThisDisk, CenOfs);
          exit;
        end;
        res := res and FileMask;                // remove rest
      end;
      // try to locate EOC
      Inc(File_Sze, sizeof(EOC));
      Size := MAX_WORD + sizeof(EOC) + sizeof(TZip64EOCLocator);
      if Size > File_Sze then
        Size := File_Sze;
      SetLength(Buf, Size);
      Pos0 := Size - (MAX_WORD + sizeof(TZipEndOfCentral));
      if Pos0 < 0 then
        Pos0 := 0;    // lowest buf position for eoc
      ReadPos := File_Sze - Size;
      if (FileSeek(FileHandle, Int64(ReadPos), soFromBeginning) <> -1) and
        (FileRead(FileHandle, Buf[0], Size) = Size) then
      begin
        // Finally try to find the EOC record within the last 65K...
        BufPos := Size - (sizeof(EOC));
        pEOC := PZipEndOfCentral(@Buf[Size - sizeof(EOC)]);
        // reverse search
        while BufPos > Pos0 do         // reverse search
        begin
          Dec(BufPos);
          Dec(PAnsiChar(pEOC));
          if pEOC^.HeaderSig = EndCentralDirSig then
          begin                             // possible EOC found
            res := res or zqbHasEOC;        // EOC
            // check correct length comment
            if (BufPos + sizeof(EOC) + pEOC^.ZipCommentLen) <= Size then
              res := res or zqbHasComment;        // good comment length
            if (BufPos + sizeof(EOC) + pEOC^.ZipCommentLen) <> Size then
              res := res or zqbJunkAtEnd;        // has junk
            CenDisk := pEOC^.CentralDiskNo;
            ThisDisk := pEOC^.ThisDiskNo;
            if (CenDisk = 0) and (ThisDisk = 0) then
              res := res or zqbIsDiskZero;
            CenOfs := pEOC^.CentralOffset;
            Need64 := NeedLoc64(pEOC^);
            // check Zip64 EOC
            if Need64 and ((BufPos - sizeof(TZip64EOCLocator)) >= 0) then
            begin   // check for locator
              pEOCLoc := PZip64EOCLocator(@Buf[BufPos - sizeof(TZip64EOCLocator)]);
              if pEOCLoc^.LocSig = EOC64LocatorSig then
              begin  // found possible locator
                res := res or zqbHasLoc64;
                CenDisk := 0;
                ThisDisk := 1;
                CenOfs := -1;
              end;
            end;
            if DoCenDir and (CenDisk = ThisDisk) then
            begin                           // verify start of central
              res := res or CheckCen(FileHandle, ThisDisk, CenOfs);
              break;
            end;
            res := res and FileMask;            // remove rest
            break;
          end;
        end;                                // while
      end;
      if EOCPossible then
        res := res or zqbHasEOC;
    except
      Result := zqGeneralError;
    end;
  finally
    File_Close(FileHandle);
    if Result = 0 then
      Result := res;
  end;
end;
//? QueryZip

function GetVolumeLabel(const drive: String): String;
var
  Bits: set of 0..25;
  DriveLetter: Char;
  drv:  String;
  NamLen: Cardinal;
  Num:  Integer;
  OldErrMode: DWord;
  SysFlags: DWord;
  SysLen: DWord;
  VolNameAry: array[0..MAX_BYTE] of Char;
begin
  Result := '';
  NamLen := MAX_BYTE;
  SysLen := MAX_BYTE;;
  VolNameAry[0] := #0;
  drv := UpperCase(ExpandFileName(drive));
  DriveLetter := drv[1];
  if DriveLetter <> PathDelim{'\'} then      // Only for local drives
  begin
    if (DriveLetter < 'A') or (DriveLetter > 'Z') then
      exit;
    Integer(Bits) := GetLogicalDrives();
    Num := Ord(DriveLetter) - Ord('A');
    if not (Num in Bits) then
      exit;
  end;
  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  // Turn off critical errors:
  if GetVolumeInformation(PChar(drv), VolNameAry, NamLen, nil, SysLen,
    SysFlags, nil, 0) then
    Result := VolNameAry;
  SetErrorMode(OldErrMode);   // Restore critical errors:
end;

function IsSameFile(const FName1, FName2: String): Boolean;
var
  ff1: Boolean;
  ff2: Boolean;
  sr1: _Z_TSearchRec;
  sr2: _Z_TSearchRec;
begin
  if CompareText(ExpandFileName(FName1), ExpandFileName(FName2)) = 0 then
  begin
    Result := True;
    exit;
  end;
  Result := False;
  // in windows no alias so names must match
  if CompareText(ExtractFileName(FName1), ExtractFileName(FName2)) = 0 then
  begin
    ff1 := _Z_FindFirst(FName1, faAnyFile, sr1) = 0;
    ff2 := _Z_FindFirst(FName2, faAnyFile, sr2) = 0;
    if (ff1 = ff2) and not ff1 then
      exit;// neither found assume different
    { $ WARN SYMBOL_PLATFORM OFF}
    if ff1 = ff2 then
      Result := CompareMem(@sr1.FindData, @sr2.FindData, 2 + (4 * 4));// both exist
    if ff1 then
      _Z_FindClose(sr1);
    if ff2 then
      _Z_FindClose(sr2);
  end;
end;

function OEMToStr(const astr: Ansistring): String;
var
  buf: String;
begin
  SetLength(buf, Length(astr) + 3); // allow worst case
  OemToChar(PAnsiChar(astr), PChar(buf));
  Result := PChar(buf);
end;

function StrToOEM(const astr: String): String;
var
  buf: Ansistring;
begin
  SetLength(buf, Length(astr) + 3); // allow worst case
  CharToOem(PChar(astr), PAnsiChar(buf));
  buf := PAnsiChar(buf); // remove trailing nul
  Result := String(buf);
end;

{
  return true if contains chars (<#31 ?) >#126
}
function StrHasExt(const astr: String): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(astr) do
    if (astr[i] > #126) or (astr[i] < #31) then
    begin
      Result := True;
      break;
    end;
end;

{$IFDEF UNICODE}
function StrHasExt(const astr: AnsiString): Boolean;
var
  i: integer;
begin
  Result := false;
  for i := 1 to Length(astr) do
    if (astr[i] > #126) or (astr[i] < #31) then
    begin
      Result := True;
      break;
    end;
end;

function StrHasExt(const astr: TZMRawBytes): Boolean;
var
  i: integer;
begin
  Result := false;
  for i := 1 to Length(astr) do
    if (astr[i] > #126) or (astr[i] < #31) then
    begin
      Result := True;
      break;
    end;
end;
{$ENDIF}

function Update_CRC32(init: DWORD; p: PByte; len: Integer): DWORD;
{$IFNDEF NO_ASM}
asm
  // eax = init, edx = p, ecx = len
  cmp ecx, 0
  jle @fini
  push ebx
  mov ebx, eax
  @loop:
  mov eax, ebx
  xor al, [edx]
  and eax, $ff
  mov eax, dword ptr CRC32Table[eax * 4]
  shr ebx, 8
  xor ebx, eax
  inc edx
  dec ecx
  jnz @loop
  mov eax, ebx
  pop ebx
  @fini:
end;
{$ELSE}
begin
  Result := init;
  while len > 0 do
  begin
    Result := (Result shr 8) xor CRC32Table[(p^ xor Byte(Result))];
    Inc(p);
    Dec(len);
  end;
end;
{$ENDIF}

function CalcCRC32(const mem; len: Integer; init: DWORD): DWORD;
var
  p: pByte;
begin
  p := PByte(@mem);
  if p <> nil then
    Result := Update_CRC32(init xor $FFFFFFFF, p, len) xor $FFFFFFFF
  else
    Result := init;
end;

function LastPos(const s: String; ch: Char; before: Integer = MAXINT): Integer;
var
  i: Integer;
begin
  Result := 0;  // not found
  for i := 1 to Length(s) do
  begin
    if i >= before then
      break;
    if s[i] = ch then
      Result := i;
  end;
end;

function LastPosW(const s: WideString; wch: Widechar; before: Integer = MAXINT): Integer;
var
  i: Integer;
begin
  Result := 0;  // not found
  for i := 1 to Length(s) do
  begin
    if i >= before then
      break;
    if s[i] = wch then
      Result := i;
  end;
end;

function RPos(const SubStr, AString: String): Integer; overload;
begin
  Result := RPos(SubStr, AString, Length(AString) - Length(SubStr) + 1);
end;

function RPos(const SubStr, AString: String; StartPos: Integer): Integer; overload;
var
  FirstCh: Char;
  PStr: PChar;
  PSub: PChar;
begin
  Result := 0;
  if (SubStr = '') or (Length(SubStr) > Length(AString)) or (StartPos < 1) then
    Exit;

  if StartPos >= Length(AString) then
    StartPos := Length(AString) - Length(SubStr) + 1;
  FirstCh := SubStr[1];

  PSub := PChar(SubStr);
  Inc(PSub); // already tested
  PStr := @(AString[StartPos]);
  while StartPos > 0 do
  begin
    if FirstCh = PStr^ then
    begin
      if CompareMem(PSub, (PStr + 1), (Length(SubStr) - 1) * SizeOf(Char)) then
      begin
        result := StartPos;
        EXIT;
      end;
    end;
    Dec(PStr);
    Dec(StartPos);
  end;
end;

function IsFolder(const Name: String): Boolean;
var
  ch: Char;
begin
  Result := False;
  if Name <> '' then
  begin
    ch := Name[Length(Name)];
    Result := (ch = PathDelim) or (ch = PathDelimAlt);
  end;
end;


{$IFDEF UNICODE}
function IsFolder(const name: TZMRawBytes): boolean;
var
  ch: AnsiChar;
begin
  Result := False;
  if name <> ''  then
  begin
    ch := name[Length(name)];
    Result := (ch = PathDelim) or (ch = PathDelimAlt);
  end;
end;
{$ENDIF}

function LastChar(const Name: String): Char;
begin
  Result := #0;
  if Name <> '' then
    Result := Name[Length(Name)];
end;


{$IFDEF UNICODE}
function LastChar(const name: TZMRawBytes): AnsiChar;
begin
  Result := #0;
  if Name <> '' then
    Result := Name[Length(Name)];
end;
{$ENDIF}

// return true if filename is obviously invalid
function NameIsBad(const astr: String; AllowWild: Boolean): Boolean;
var
  I: Integer;
  c: Char;
begin
  Result := (astr = '') or (astr[1] = ' ') or (astr[1] = '\') or
    (Length(astr) > MAX_PATH);
  if not Result then
    for I := 1 to Length(astr) do
    begin
      c := astr[I];
{$IFDEF UNICODE}
      if CharInSet(c, [#0 .. #31, ':', '<', '>', '|']) or
        ((not AllowWild) and CharInSet(c, ['*', '?'])) then
{$ELSE}
      if (c in [#0 .. #31, ':', '<', '>', '|']) or
        ((not AllowWild) and (c in ['*', '?'])) then
{$ENDIF}
      begin
        Result := True;
        break;
      end;
    end;
  if not Result then
    Result := (AnsiPos('..', astr) > 0) or (AnsiPos('\ ', astr) > 0) or
      (AnsiPos(' \', astr) > 0);
end;

 // return exe size (if < 4G)
 //    0 _ not exe
function ExeSize(fileHandle: Integer): Cardinal;
var
  bad: Boolean;
  did: Integer;
  sig: DWORD;
  dosHeader: TImageDOSHeader;
  fileHeader: TImageFileHeader;
  sectionHeader: TImageSectionHeader;
  i, NumSections: Integer;
  sectionEnd: Cardinal;
const
  IMAGE_PE_SIGNATURE  = $00004550;
  IMAGE_DOS_SIGNATURE = $5A4D;
  IMAGE_FILE_MACHINE_I386 = $14C;
begin
  Result := 0;
  bad := True;
  if fileHandle <> -1 then
  begin
    try
      FileSeek(fileHandle, 0, soFromBeginning);
      while True do
      begin
        did := FileRead(fileHandle, dosHeader, sizeof(TImageDOSHeader));
        if (did <> sizeof(TImageDOSHeader)) or
          (dosHeader.e_magic <> IMAGE_DOS_SIGNATURE) then
          break;
        if FileSeek(fileHandle, dosHeader._lfanew, 0) < 0 then
          break;
        did := FileRead(fileHandle, sig, sizeof(DWORD));
        if (did <> sizeof(DWORD)) or (sig <> IMAGE_PE_SIGNATURE) then
          break;
        did := FileRead(fileHandle, fileHeader, sizeof(TImageFileHeader));
        if (did <> sizeof(TImageFileHeader)) or
          (fileHeader.Machine <> IMAGE_FILE_MACHINE_I386) then
          break;
        NumSections := fileHeader.NumberOfSections;
        if FileSeek(fileHandle, sizeof(TImageOptionalHeader), 1) < 0 then
          break;
        bad := False;
        for i := 1 to NumSections do
        begin
          did := FileRead(fileHandle, sectionHeader, sizeof(TImageSectionHeader));
          if (did <> sizeof(TImageSectionHeader)) then
          begin
            bad := True;
            break;
          end;
          sectionEnd := sectionHeader.PointerToRawData + sectionHeader.SizeOfRawData;
          if sectionEnd > Result then
            Result := sectionEnd;
        end;
      end;
    except
      bad := True;
    end;
  end;
  if bad then
    Result := 0;
end;

function ExeSize(const Name: String): Cardinal;
var
  fh: Integer;
begin
  Result := 0;
  fh := _Z_FileOpen(Name, fmOpenRead);
  if fh <> -1 then
  begin
    Result := ExeSize(fh);
    File_Close(fh);
  end;
end;

// return <0 error
//const
//  cstNone = 0;      // not found
//  cstExe  = 1;      // might be stub of unknown type
//  cstSFX17 = 2;     // found 1.7 SFX headers
//  cstSFX19 = 4;     // found 2.0 SFX headers
//  cstDetached = 64; // is detached
// -7  = Open, read or seek error
// -8  = memory error
// -9  = exception error
// -10 = all other exceptions

// check for SFX header or detached header
function CheckSFXType(const fileHandle: Integer; var ZipName: String;
  var size: Integer): Integer;
type
  T_header = packed record
    Sig: DWORD;
    Size: Word;
    X: Word;
  end;
var
  nsize: Integer;
  hed: T_header;
  SFXHeader_end: TSFXFileEndOfHeader_17;
  Detached: TSFXDetachedHeader_17;
  tmp: Ansistring;
begin
  Result := 0; // default none
  try
    size := ExeSize(fileHandle);
    if size > 0 then
    begin
      ZipName := ExtractNameOfFile(ZipName) + '.zip'; // use default
      while Result {>}= 0 do
      begin
        Result := -7; // error - maybe read error?
        if FileSeek(fileHandle, size, soFromBeginning) <> size then
          Break;
        // at end of stub - read file header
        if FileRead(fileHandle, hed, sizeof(T_header)) <> sizeof(T_header) then
          break;
        // valid?
        case hed.Sig of
          SFX_HEADER_SIG:
          begin
            // it is new header
            size  := size + sizeof(T_header);
            // skip file header
            nsize := Hed.Size - SizeOf(T_header);
            if FileSeek(fileHandle, nsize, soFromCurrent) < 0 then
              break;   // error
            // at end of stub - read file header
            if FileRead(fileHandle, hed, sizeof(T_header)) <> sizeof(T_header) then
              break;     // invalid
            size := size + nsize;
            if hed.Sig = CentralFileHeaderSig then
              Result := cstSFX19 or cstDetached  // found new detached
            else
            if hed.Sig = LocalFileHeaderSig then
              Result := cstSFX19;  // found new
          end;
          SFX_HEADER_SIG_17:
          begin
            // is old header
            size  := size + sizeof(T_header);
            // skip file header
            nsize := Hed.Size - SizeOf(T_header);
            if FileSeek(fileHandle, nsize, soFromCurrent) < 0 then
              break;   // error
            if FileRead(fileHandle, SFXHeader_end, sizeof(SFXHeader_end)) <>
              sizeof(SFXHeader_end) then
              break;     // invalid
            if (SFXHeader_end.Signature <> SFX_HEADER_END_SIG_17) then
              break;  // invalid
            // ignore header size check
            size := size + nsize + sizeof(SFXHeader_end);
            // at end of file header - check for detached header
            if FileRead(fileHandle, detached, sizeof(TSFXDetachedHeader_17)) <>
              sizeof(TSFXDetachedHeader_17) then
              break;     // not detached
            if detached.Signature = SFX_DETACHED_HEADER_SIG_17 then
            begin
              size := size + sizeof(TSFXDetachedHeader_17);
              if Detached.NameLen > 0 then
              begin
                SetLength(tmp, Detached.NameLen);
                if FileRead(fileHandle, PAnsiChar(tmp)^, Detached.NameLen) <>
                  Integer(Detached.NameLen) then
                  break;     // invalid
                ZipName := String(tmp) + ExtractFileExt(ZipName);
                size := size + Integer(Detached.NameLen);
              end;
              if Detached.ExtLen > 0 then
              begin
                SetLength(tmp, Detached.ExtLen);
                if FileRead(fileHandle, PAnsiChar(tmp)^, Detached.ExtLen) <>
                  Integer(Detached.ExtLen) then
                  break;     // invalid
                size := size + Integer(Detached.ExtLen);
                ZipName := ExtractNameOfFile(ZipName) + '.' + string(tmp);
              end;
              // at end of file header - check for detached header end
              if (FileRead(fileHandle, detached, sizeof(TSFXDetachedHeader_17)) <>
                sizeof(TSFXDetachedHeader_17)) or
                (detached.Signature <> SFX_DETACHED_HEADER_END_SIG_17) then
                break;     // invalid
              size := size + sizeof(TSFXDetachedHeader_17);
              if FileRead(fileHandle, hed, sizeof(DWORD)) <> sizeof(DWORD) then
                break;     // invalid
              if hed.Sig = CentralFileHeaderSig then
                Result := cstSFX17 or cstDetached;  // found old detached
            end;
            if detached.Signature = LocalFileHeaderSig then
              Result := cstSFX17;  // found old
          end;
          else
          begin
            Result := cstExe; // possibly stub of different loader
          end;
        end;
      end;
    end;
  except
    Result := -10;
  end;
end;

function CheckSFXType(const Name: String; var ZipName: String;
  var size: Integer): Integer;
var
  fh: Integer;
begin
  Result := 0;
  if AnsiCompareText(ExtractFileExt(Name), '.exe') = 0 then
  begin
    fh := _Z_FileOpen(Name, fmOpenRead);
    if fh <> -1 then
    begin
      ZipName := Name;
      Result := CheckSFXType(fh, ZipName, size);
      File_Close(fh);
    end;
  end;
end;

function FileDateToLocalDateTime(stamp: Integer): TDateTime;
var
  LocTime, FTime: TFileTime;
  SysTime: TSystemTime;
begin
  Result := 0;
  if DosDateTimeToFileTime(LongRec(stamp).Hi, LongRec(stamp).Lo, LocTime) and
    LocalFileTimeToFileTime(LocTime, FTime) and
    FileTimeToSystemTime(FTime, SysTime) then
    Result := SystemTimeToDateTime(SysTime);
end;

function FileTimeToLocalDOSTime(const ft: TFileTime): Cardinal;
var
  lf: TFileTime;
  wd: Word;
  wt: Word;
begin
  Result := 0;
  if FileTimeToLocalFileTime(ft, lf) and FileTimeToDosDateTime(lf, wd, wt) then
    Result := (wd shl 16) or wt;
end;

function _XData(const xdat: PByte; len: Word; Tag: Word; var idx, size: Integer):
    Boolean;
var
  i: Integer;
  p: PAnsiChar;
  wsz: Word;
  wtg: Word;
begin
  Result := False;
  idx := 0;
  size := 0;
  i := 1;
  p := PAnsiChar(xdat);
  while i < len - (2 * SizeOf(Word)) do
  begin
    wtg := pWord(p)^;
    wsz := pWord(p[2])^;
    if wtg = Tag then
    begin
      Result := (i + wsz + (2 * SizeOf(Word))) <= len + 1;
      if Result then
      begin
        idx  := i;
        size := wsz + (2 * SizeOf(Word));
      end;
      break;
    end;
    i := i + wsz + (2 * SizeOf(Word));
    Inc(p, wsz + (2 * SizeOf(Word)));
  end;
end;

// Return true if found
// if found return idx --> tag, size = tag + data
function XData(const x: TZMRawBytes; Tag: Word; var idx, size: Integer):
    Boolean;
begin
  Result := _XData(PByte(@x[1]), Length(x), Tag, idx, size);
end;

function XData_HasTag(tag: Integer; const tags: array of Integer): Boolean;
var
  ii: Integer;
begin
  Result := False;
  for ii := 0 to HIGH(tags) do
    if tags[ii] = tag then
    begin
      Result := True;
      break;
    end;
end;

function XDataAppend(var x: TZMRawBytes; const src1; siz1: Integer; const src2;
    siz2: Integer): Integer;
var
  newlen: Integer;
begin
  Result := Length(x);
  if (siz1 < 0) or (siz2 < 0) then
    exit;
  newlen := Result + siz1 + siz2;
  SetLength(x, newlen);
  Move(src1, x[Result + 1], siz1);
  Result := Result + siz1;
  if siz2 > 0 then
  begin
    Move(src2, x[Result + 1], siz2);
    Result := Result + siz2;
  end;
end;

function XDataKeep(const x: TZMRawBytes; const tags: array of Integer):
    TZMRawBytes;
var
  di: Integer;
  i: Integer;
  l: Integer;
  siz: Integer;
  wsz: Word;
  wtg: Word;
begin
  Result := '';
  siz := 0;
  l := Length(x);
  if l < 4 then
    exit;  // invalid
  i := 1;
  while i <= l - 4 do
  begin
    wtg := pWord(@x[i])^;
    wsz := pWord(@x[i + 2])^;
    if (XData_HasTag(wtg, tags)) and ((i + wsz + 4) <= l + 1) then
    begin
      Inc(siz, wsz + 4);
    end;
    i := i + wsz + 4;
  end;
  SetLength(Result, siz);
  di := 1;
  i  := 1;
  while i <= l - 4 do
  begin
    wtg := pWord(@x[i])^;
    wsz := pWord(@x[i + 2])^;
    if (XData_HasTag(wtg, tags)) and ((i + wsz + 4) <= l + 1) then
    begin
      wsz := wsz + 4;
      while wsz > 0 do
      begin
        Result[di] := x[i];
        Inc(di);
        Inc(i);
        Dec(wsz);
      end;
    end
    else
      i := i + wsz + 4;
  end;
end;

function XDataPrepend(var x: TZMRawBytes; const src; siz: Integer): Integer;
var
  newx: TZMRawBytes;
begin
  Result := Length(x);
  if siz < 0 then
    exit;
  SetLength(newx, siz);
  Move(src, newx[1], siz);
  x := newx + x;
  Result := Length(x);
end;

function XDataRemove(const x: TZMRawBytes; const tags: array of Integer):
    TZMRawBytes;
var
  di: Integer;
  i: Integer;
  l: Integer;
  siz: Integer;
  wsz: Word;
  wtg: Word;
begin
  Result := '';
  siz := 0;
  l := Length(x);
  if l < 4 then
    exit;  // invalid
  i := 1;
  while i <= l - 4 do
  begin
    wtg := pWord(@x[i])^;
    wsz := pWord(@x[i + 2])^;
    if (not XData_HasTag(wtg, tags)) and ((i + wsz + 4) <= l + 1) then
    begin
      Inc(siz, wsz + 4);
    end;
    i := i + wsz + 4;
  end;
  SetLength(Result, siz);
  di := 1;
  i  := 1;
  while i <= l - 4 do
  begin
    wtg := pWord(@x[i])^;
    wsz := pWord(@x[i + 2])^;
    if (not XData_HasTag(wtg, tags)) and ((i + wsz + 4) <= l + 1) then
    begin
      wsz := wsz + 4;
      while wsz > 0 do
      begin
        Result[di] := x[i];
        Inc(di);
        Inc(i);
        Dec(wsz);
      end;
    end
    else
      i := i + wsz + 4;
  end;
end;

// P. J. Weinberger Hash function
function HashFunc(const str : String) : Cardinal;
var
  i : Cardinal;
  x : Cardinal;
begin
  Result := 0;
  for i := 1 to Length(str) do
  begin
    Result := (Result shl 4) + Ord(str[i]);
    x := Result and $F0000000;
    if (x <> 0) then
      Result := (Result xor (x shr 24)) and $0FFFFFFF;
  end;
end;

function HashFuncNoCase(const str : String) : Cardinal;
var
  NCStr: string;
begin
  NCStr := UpperCase(str);
  Result := HashFunc(NCStr);
end;

procedure SplitArgs(const args: string; var main: string; var filearg: string;
   var switches: string; var password: string);
var
  epos: Integer;
  fpos: Integer;
  ppos: Integer;
  spos: Integer;
begin
  fpos := Pos(ZFILE_SEPARATOR, args);
  spos := Pos(' /', args);
  ppos := Pos(ZPasswordArg, args);
  epos := Length(args);
  if ppos > 0 then
  begin
    password := Copy(args, ppos, 2048);
    epos := ppos - 1;
    if spos > ppos then
      spos := ppos;
  end
  else
    password := '';
  if spos > 0 then
  begin
    switches := Trim(Copy(args, spos + 1, (epos - spos) -2));
    epos := spos -1;
    if fpos > spos then
      fpos := spos;
  end
  else
    switches := '';
  if fpos > 0 then
  begin
    filearg := Trim(Copy(args, fpos + 2, (epos - fpos) -1));
    epos := fpos -1;
  end
  else
    filearg := '';
  main := Trim(Copy(args, 1, epos));
end;

function QualifiedName(const ZipName: string; const FileName: string): string;
begin
  Result := ZipName + ZFILE_SEPARATOR + FileName;
end;

function ZSplitString(const Delim, Raw: string; var Rest: string): String;
var
  EPos: Integer;
  FPos: Integer;
begin
  FPos := Pos(Delim, Raw);
  EPos := Length(Raw);
  if FPos > 0 then
  begin
    Rest := Copy(raw, FPos + Length(Delim), EPos);
    EPos := fpos -1;
  end
  else
    Rest := '';
  Result := Copy(Raw, 1, EPos);
end;

function ZSplitStringLast(const Delim, Raw: string; var Rest: string): String;
var
  EPos: Integer;
  FPos: Integer;
begin
  FPos := RPos(Delim, Raw);
  EPos := Length(Raw);
  if FPos > 0 then
  begin
    Rest := Copy(raw, FPos + 2, (EPos - FPos) -1);
    EPos := fpos -1;
  end
  else
    Rest := '';
  Result := Copy(Raw, 1, EPos);
end;

procedure SplitQualifiedName(const QName: string; var ZipName: string; var FileName: string);
begin
  ZipName := Trim(ZSplitStringLast(ZFILE_SEPARATOR, QName, FileName));
  FileName := Trim(FileName);
end;

function FindFirstIcon(var rec: TImageResourceDataEntry; const iLevel: Integer;
  const PointerToRawData: Cardinal; str: TStream): boolean;
const
  __ERR_CZ_BrowseError = __UNIT__ + (1904 shl 10) + CZ_BrowseError;
  __ERR_CZ_BrowseError1 = __UNIT__ + (1909 shl 10) + CZ_BrowseError;
  __ERR_CZ_BrowseError2 = __UNIT__ + (1933 shl 10) + CZ_BrowseError;
var
  i: Integer;
  iPos: Integer;
  RecDir: TImageResourceDirectory;
  RecEnt: TImageResourceDirectoryEntry;
begin
  // position must be correct
  Result := false;
  if (str.Read(RecDir, sizeof(RecDir)) <> sizeof(RecDir)) then
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_BrowseError, true);

  for i := 0 to Pred(RecDir.NumberOfNamedEntries + RecDir.NumberOfIdEntries) do
  begin
    if (str.Read(RecEnt, sizeof(RecEnt)) <> sizeof(RecEnt)) then
      raise EZipMaster.CreateMsgDisp(__ERR_CZ_BrowseError1, true);

    // check if a directory or a resource
    iPos := str.Position;
    try
      if (RecEnt.un2.DataIsDirectory and IMAGE_RESOURCE_DATA_IS_DIRECTORY)
        = IMAGE_RESOURCE_DATA_IS_DIRECTORY then
      begin
        if ((iLevel = 0) and (MakeIntResource(RecEnt.un1.Name) <> RT_ICON)) or
          ((iLevel = 1) and (RecEnt.un1.Id <> 1)) then
          Continue; // not an icon of id 1

        str.Seek(RecEnt.un2.OffsetToDirectory and
          (not IMAGE_RESOURCE_DATA_IS_DIRECTORY) + PointerToRawData,
          soFromBeginning);
        Result := FindFirstIcon(rec, iLevel + 1, PointerToRawData, str);
        if Result then
          Break;
      end
      else
      begin
        // is resource bin data
        str.Seek(RecEnt.un2.OffsetToData + PointerToRawData, soFromBeginning);
        if str.Read(rec, sizeof(rec)) <> sizeof(rec) then
          raise EZipMaster.CreateMsgDisp(__ERR_CZ_BrowseError2, true);
        Result := true;
        Break;
      end;
    finally
      str.Position := iPos;
    end;
  end;
end;

procedure LocateFirstIconHeader(str: TStream;
  var hdrSection: TImageSectionHeader; var recIcon: TImageResourceDataEntry);
const
  __ERR_CZ_InputNotExe = __UNIT__ + (1965 shl 10) + CZ_InputNotExe;
  __ERR_CZ_InputNotExe1 = __UNIT__ + (1970 shl 10) + CZ_InputNotExe;
  __ERR_CZ_NoExeResource = __UNIT__ + (1976 shl 10) + CZ_NoExeResource;
  __ERR_CZ_ExeSections = __UNIT__ + (1984 shl 10) + CZ_ExeSections;
  __ERR_CZ_NoExeResource1 = __UNIT__ + (1995 shl 10) + CZ_NoExeResource;
  __ERR_CZ_NoExeIcon1 = __UNIT__ + (2002 shl 10) + CZ_NoExeIcon;
var
  bFound: boolean;
  cAddress: Cardinal;
  dataDir: PImageDataDirectory;
  hdrDos: TImageDosHeader;
  hdrNT: TImageNTHeaders;
  i: Integer;
begin
  bFound := false;
  // check if we have an executable
  str.Seek(0, soFromBeginning);
  if (str.Read(hdrDos, sizeof(hdrDos)) <> sizeof(hdrDos)) or
    (hdrDos.e_magic <> IMAGE_DOS_SIGNATURE) then
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_InputNotExe, true);

  str.Seek(hdrDos._lfanew, soFromBeginning);
  if (str.Read(hdrNT, sizeof(hdrNT)) <> sizeof(hdrNT)) or
    (hdrNT.Signature <> IMAGE_NT_SIGNATURE) then
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_InputNotExe1, true);

  // check if we have a resource section
  dataDir := @(hdrNT.OptionalHeader.DataDirectory
    [IMAGE_DIRECTORY_ENTRY_RESOURCE]);
  if (dataDir^.VirtualAddress = 0) or (dataDir^.Size = 0) then
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoExeResource, true)
  else
    cAddress := dataDir^.VirtualAddress; // store address

  // iterate over sections
  for i := 0 to Pred(hdrNT.FileHeader.NumberOfSections) do
  begin
    if (str.Read(hdrSection, sizeof(hdrSection)) <> sizeof(hdrSection)) then
      raise EZipMaster.CreateMsgDisp(__ERR_CZ_ExeSections, true);

    // with hdrSection do
    if hdrSection.VirtualAddress = cAddress then
    begin
      bFound := true;
      Break;
    end;
  end;

  if not bFound then
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoExeResource1, true);

  // go to resource data
  str.Seek(hdrSection.PointerToRawData, soFromBeginning);

  // recourse through the resource dirs to find an icon
  if not FindFirstIcon(recIcon, 0, hdrSection.PointerToRawData, str) then
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoExeIcon1, true);
end;

// replaces an icon in an executable file (stream)
function GetFirstIcon(str: TMemoryStream): TIcon;
const
  __ERR_CZ_NoIconFound = __UNIT__ + (2035 shl 10) + CZ_NoIconFound;
var
  bad: boolean;
  delta: Cardinal;
  handle: HIcon;
  hdrSection: TImageSectionHeader;
  icoData: PByte;
  icoSize: Cardinal;
  recIcon: TImageResourceDataEntry;
begin
  bad := true;
  Result := nil;
  LocateFirstIconHeader(str, hdrSection, recIcon);
  delta := Integer(hdrSection.PointerToRawData) -
    Integer(hdrSection.VirtualAddress) + Integer(recIcon.OffsetToData);
  icoData := PByte(str.Memory);
  Inc(icoData, delta);
  icoSize := hdrSection.SizeOfRawData;
  handle := CreateIconFromResource(icoData, icoSize, true, $30000);
  if handle <> 0 then
  begin
    Result := TIcon.Create;
    Result.handle := handle;
    bad := false;
  end;
  if bad then
    // no icon copied, so none of matching size found
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoIconFound, true);
end;

// returns size or 0 on error or wrong dimensions
function WriteIconToStream(Stream: Classes.TStream; Icon: HICON;
  Width, Height, Depth: Integer): Integer;
type
  PIconRec = ^TIconRec;

  TIconRec = packed record
    IDir: TIconDir;
    IEntry: TIconDirEntry;
  end;
const
  RC3_ICON = 1;
var
  BI: PBITMAPINFO;
  BIsize: Integer;
  CBits: PByte;
  cbm: Bitmap;
  cofs: Integer;
  colors: Integer;
  dc: HDC;
  Ico: TIconRec;
  IconInfo: TIconInfo;
  MBI: BitMapInfo;
  MBits: PByte;
  mofs: Integer;
begin
  Result := 0;

  if (Depth <= 4) then
    Depth := 4
  else
    if (Depth <= 8) then
      Depth := 8
    else
      if (Depth <= 16) then
        Depth := 16
      else
        if (Depth <= 24) then
          Depth := 24
        else
          exit;
  colors := 1 shl Depth;

  BI := nil;
  dc := 0;
  if GetIconInfo(Icon, IconInfo) then
  begin
    try
      ZeroMemory(@Ico, sizeof(TIconRec));
      if GetObject(IconInfo.hbmColor, sizeof(Bitmap), @cbm) = 0 then
        exit;
      if (Width <> cbm.bmWidth) or (Height <> cbm.bmHeight) then
        exit;

      // ok should be acceptable
      BIsize := sizeof(BitmapInfoHeader);
      if (Depth <> 24) then
        Inc(BIsize, colors * sizeof(RGBQUAD)); // pallet

      cofs := BIsize; // offset to colorbits
      Inc(BIsize, (Width * Height * Depth) div 8); // bits
      mofs := BIsize; // offset to maskbits
      Inc(BIsize, (Width * Height) div 8);

      // allocate memory for it
      GetMem(BI, BIsize);

      ZeroMemory(BI, BIsize);
      // set required attributes for colour bitmap
      BI^.bmiHeader.BIsize := sizeof(BitmapInfoHeader);
      BI^.bmiHeader.biWidth := Width;
      BI^.bmiHeader.biHeight := Height;
      BI^.bmiHeader.biPlanes := 1;
      BI^.bmiHeader.biBitCount := Depth;
      BI^.bmiHeader.biCompression := BI_RGB;

      CBits := PByte(BI);
      Inc(CBits, cofs);

      // prepare for mono mask bits
      ZeroMemory(@MBI, sizeof(BitMapInfo));
      MBI.bmiHeader.BIsize := sizeof(BitmapInfoHeader);
      MBI.bmiHeader.biWidth := Width;
      MBI.bmiHeader.biHeight := Height;
      MBI.bmiHeader.biPlanes := 1;
      MBI.bmiHeader.biBitCount := 1;

      MBits := PByte(BI);
      Inc(MBits, mofs);

      dc := CreateCompatibleDC(0);
      if dc <> 0 then
      begin
        if GetDIBits(dc, IconInfo.hbmColor, 0, Height, CBits, BI^,
          DIB_RGB_COLORS) > 0 then
        begin
          // ok get mask bits
          if GetDIBits(dc, IconInfo.hbmMask, 0, Height, MBits, MBI,
            DIB_RGB_COLORS) > 0 then
          begin
            // good we have both
            DeleteDC(dc); // release it quick before anything can go wrong
            dc := 0;
            Ico.IDir.ResType := RC3_ICON;
            Ico.IDir.ResCount := 1;
            Ico.IEntry.bWidth := Width;
            Ico.IEntry.bHeight := Height;
            Ico.IEntry.bColorCount := Depth;
            Ico.IEntry.dwBytesInRes := BIsize;
            Ico.IEntry.dwImageOffset := sizeof(TIconRec);
            BI^.bmiHeader.biHeight := Height * 2;
            // color height includes mask bits
            Inc(BI^.bmiHeader.biSizeImage, MBI.bmiHeader.biSizeImage);
            if (Stream <> nil) then
            begin
              Stream.Write(Ico, sizeof(TIconRec));
              Stream.Write(BI^, BIsize);
            end;
            Result := BIsize + sizeof(TIconRec);
          end;
        end;
      end;
    finally
      if dc <> 0 then
        DeleteDC(dc);
      DeleteObject(IconInfo.hbmColor);
      DeleteObject(IconInfo.hbmMask);
      if BI <> nil then
        FreeMem(BI);
    end;
  end
  else
{$ifndef VERD6up}
    RaiseLastWin32Error;
{$else}
    RaiseLastOSError;
{$endif}
end;

// replaces an icon in an executable file (stream)
procedure ReplaceIcon(str: TMemoryStream; oIcon: TIcon);
const
  __ERR_CZ_NoCopyIcon = __UNIT__ + (2201 shl 10) + CZ_NoCopyIcon;
  __ERR_CZ_NoIcon = __UNIT__ + (2208 shl 10) + CZ_NoIcon;
  __ERR_CZ_NoIcon1 = __UNIT__ + (2214 shl 10) + CZ_NoIcon;
  __ERR_CZ_NoCopyIcon1 = __UNIT__ + (2228 shl 10) + CZ_NoCopyIcon;
  __ERR_CZ_NoIconFound = __UNIT__ + (2240 shl 10) + CZ_NoIconFound;
var
  bad: Boolean;
  hdrSection: TImageSectionHeader;
  i: Integer;
  oriInfo: BitmapInfoHeader;
  pIDE: PIconDirEntry;
  recIcon: TImageResourceDataEntry;
  strIco: TMemoryStream;
begin
  bad := true;
  LocateFirstIconHeader(str, hdrSection, recIcon);
  str.Seek(Integer(hdrSection.PointerToRawData) -
    Integer(hdrSection.VirtualAddress) + Integer(recIcon.OffsetToData),
    soFromBeginning);
  if (str.Read(oriInfo, sizeof(BitmapInfoHeader)) <>
    sizeof(BitmapInfoHeader)) then
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoCopyIcon, true);

  // now check the icon
  strIco := TMemoryStream.Create;
  try
    if WriteIconToStream(strIco, oIcon.handle, oriInfo.biWidth,
      oriInfo.biHeight div 2, oriInfo.biBitCount) <= 0 then
      raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoIcon, true);

    // now search for matching icon
    with PIconDir(strIco.Memory)^ do
    begin
      if (ResType <> RES_ICON) or (ResCount < 1) or (Reserved <> 0) then
        raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoIcon1, true);

      for i := 0 to Pred(ResCount) do
      begin
        pIDE := PIconDirEntry(PAnsiChar(strIco.Memory) + sizeof(TIconDir) +
          (i * sizeof(TIconDirEntry)));
        if (pIDE^.dwBytesInRes = recIcon.Size) and (pIDE^.bReserved = 0) then
        begin
          // matching icon found, replace
          strIco.Seek(pIDE^.dwImageOffset, soFromBeginning);
          str.Seek(Integer(hdrSection.PointerToRawData) -
            Integer(hdrSection.VirtualAddress) + Integer(recIcon.OffsetToData),
            soFromBeginning);
          if str.CopyFrom(strIco, recIcon.Size) <> Integer(recIcon.Size) then
            raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoCopyIcon1, true);

          // ok and out
          bad := false;
        end;
      end;
    end;
  finally
    strIco.Free;
  end;
  if bad then
    // no icon copied, so none of matching size found
    raise EZipMaster.CreateMsgDisp(__ERR_CZ_NoIconFound, true);
end;

end.

