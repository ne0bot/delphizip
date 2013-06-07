unit PathParser;

(* ***************************************************************************
  PathParse.pas - handles path specifications for TZipResMaker for ZipMaster
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
//modified 2012-04-03
{$I '..\..\ZipVers.inc'}

interface

uses
  Classes;

type
  TLocalFolderEvent = procedure(var folder: String; var handled: Boolean) of object;

type
  TPathParserOpts = (ppDefault, ppDrives, ppBorland, ppMustExist);
  TPathParserOptions = set of TPathParserOpts;

const
  BAD_FOLDER: string = '<INVALID>';

type
  TSpecialFolder = (sfDesktop, sfAppData, sfTemplates, sfPrograms,
    sfPersonal, sfFavorites, sfStartup, sfRecent,
    sfSendTo, sfStartMenu,
    sfFonts, sfHistory, sfCookies, sfInternetCache,
    sfCommonFavorites,
    sfCommonDesktop, sfCommonStartup, sfCommonPrograms,
    sfCommonStartMenu,
    sfProgramFiles, sfProgramFilesX86, sfTemporary, sfWindows, sfSystem);

  TSpecialFolderSet = set of TSpecialFolder;

  TPathParser = class(TStringList)
  private
    FMustExist:     Boolean;
    FOnLocalFolder: TLocalFolderEvent;
  protected
    Level: Integer;
    function DriveName(var name: string; const drive: string): Boolean;
    procedure GetBorlandFolders;
    procedure GetDrives;
  public
    constructor Create(const UseDefaultMap: Boolean = True; GetLocalFunc:
        TLocalFolderEvent = nil); overload;
    constructor Create(Opts: TPathParserOptions = [ppDefault]); overload;
    class function GetSpecialFolder(const Name: TSpecialFolder): String;
    procedure InitMap; virtual;
    function Parse(const Path: String): String;
    function ParsePath(var Path: string): Integer;
    property MustExist: Boolean Read FMustExist Write FMustExist;
    property OnLocalFolder: TLocalFolderEvent Read FOnLocalFolder Write FOnLocalFolder;
  end;


implementation

{ TPathParser }

uses
  SysUtils, TypInfo, ShlObj, Registry, Windows;

{ $WARN SYMBOL_PLATFORM OFF}

const
  DelphiRegKey: array [4..18] of String = (
    'Software\Borland\Delphi\4.0',
    'Software\Borland\Delphi\5.0',
    'Software\Borland\Delphi\6.0',
    'Software\Borland\Delphi\7.0',
    'Software\Borland\Delphi\8.0',
    'Software\Borland\BDS\3.0',       //2005
    'Software\Borland\BDS\4.0',       //2006
    'Software\Borland\BDS\5.0',       //2007
    'Software\CodeGear\BDS\6.0',      //2009
    'Software\CodeGear\BDS\7.0',      //2010
    'Software\Embarcadero\BDS\8.0',   //XE
    'Software\Embarcadero\BDS\9.0',   //XE2
    'Software\Embarcadero\BDS\10.0',  //XE3
    'Software\Embarcadero\BDS\11.0',  //XE4
    'Software\Embarcadero\BDS\12.0'); //XE5
  CBuilderRegKey: array [4..6] of String = (
    'Software\Borland\C++Builder\4.0',
    'Software\Borland\C++Builder\5.0',
    'Software\Borland\C++Builder\6.0');


function RegKeyExists(const RootKey: HKEY; const Key: String): Boolean;
var
  Handle: HKEY;
begin
  if RegOpenKeyEx(RootKey, PChar(Key), 0, KEY_READ, Handle) = ERROR_SUCCESS then
  begin
    Result := True;
    RegCloseKey(Handle);
  end
  else
  begin
    Result := False;
  end;
end;
        
function GetRegistryValue(Root: HKEY; const KeyName, Name: String): String;
var
  Registry: TRegistry;
begin
{$IFDEF VERD6up}
  Registry := TRegistry.Create(KEY_READ);
{$else}
  Registry := TRegistry.Create;//(KEY_READ);
{$endif}
  try
    Registry.RootKey := Root;//HKEY_CURRENT_USER
    // False because we do not want to create it if it doesn't exist
    Registry.OpenKey(KeyName, False);
    Result := Registry.ReadString(Name);
  finally
    Registry.Free;
  end;
end;

constructor TPathParser.Create(const UseDefaultMap: Boolean = True;
    GetLocalFunc: TLocalFolderEvent = nil);
begin
//  CaseSensitive := False;
  fMustExist := True;
  if UseDefaultMap then
  begin
    InitMap;
    GetBorlandFolders;
  end;
  fOnLocalFolder := GetLocalFunc;
  Level := 0;
end;

function GetEnvVariable(const vname: string): string;
var
   len: DWORD;
begin
   SetLength(Result, 510);
   len := GetEnvironmentVariable(pChar(vname), Pchar(Result), 500);
   SetLength(Result, len);
end;
                                  
{$IFNDEF VERD2005up}
function ExcludeTrailingBackSlash(path: string): string;
begin
  Result := path;
  if Length(path) = 0 then
  begin
    exit;
  end;
  if (AnsiLastChar(path)^ = '\') <> false then
      Result := Copy(path, 1, pred(Length(Path)));
end;

function IncludeTrailingBackSlash(path: string): string;
begin
  Result := path;
  if Length(path) = 0 then
  begin
    Result := '\';
    exit;
  end;
  if (AnsiLastChar(path)^ = '\') <> true then
  begin
      Result := Path + '\';
  end;
end;
{$endif}

procedure TPathParser.GetBorlandFolders;
var
  I: Integer;
  ExePath, proj: String;
  Installed: Boolean;

begin
  ExePath := '';
  Proj := '';
  //    MsgBox('Checking path',mbInformation, MB_OK);
  // Go through all supported Delphi compilers.
  for I := HIGH(DelphiRegKey) downto LOW(DelphiRegKey) do
  begin
    Installed := False;
    ExePath := '';
    //MsgBox('Checking: '+DelphiRegKey(I),mbInformation, MB_OK);
    // See if the Software\Borland\Delphi\{Delphi Version} key exists then use it to see
    // if the EXE actually exists. Older version of compilers don't have the 'App' key they
    // have a 'Delphi 4' type key.
    if RegKeyExists(HKEY_LOCAL_MACHINE, DelphiRegKey[I]) then
    begin
      ExePath := GetRegistryValue(HKEY_LOCAL_MACHINE, DelphiRegKey[I], 'App');
      if ExePath <> '' then
        Installed := FileExists(ExePath)
      else
      begin
        ExePath := GetRegistryValue(HKEY_LOCAL_MACHINE, DelphiRegKey[I],
          'Delphi ' + IntToStr(I));
        if ExePath <> '' then
        begin
          Installed := FileExists(ExePath);
        end;
      end;
    end;
    if Installed then
    begin
      if I >= 9 then
      begin
        Proj := GetEnvVariable('BDSPROJECTSDIR');
        if Proj = '' then
        begin
          Proj := GetRegistryValue(HKEY_CURRENT_USER, DelphiRegKey[I],
            'Globals\DefaultProjectsDirectory');
          if Proj = '' then
          begin
            Proj := '$(Personal)';
            if ParsePath(Proj) >= 0 then
            begin
              Proj := IncludeTrailingBackslash(Proj);
              if I > 10 then
                Proj := Proj + 'RAD Studio\Projects'
              else
                Proj := Proj + 'Borland Studio Projects';
            end
            else
              Proj := '';
          end;
        end;
      end
      else
      begin
        // The root path to the current compiler. This path never contains a macro as it is the base
        // for macro expansion/collapsing.
        Proj := GetRegistryValue(HKEY_LOCAL_MACHINE, DelphiRegKey[I], 'RootDir');
        if Proj = '' then
          Proj := GetRegistryValue(HKEY_CURRENT_USER, DelphiRegKey[I], 'RootDir');
        if Proj <> '' then
          Proj := IncludeTrailingBackslash(Proj) + 'Projects';
      end;
      break;
    end;
  end;

  if not Installed then
  begin
    // Go through all supported BCB compilers.
    for I := HIGH(CBuilderRegKey) downto LOW(CBuilderRegKey) do
    begin
      Installed := False;
      ExePath := '';
      if RegKeyExists(HKEY_LOCAL_MACHINE, CBuilderRegKey[I]) then
      begin
        ExePath := GetRegistryValue(HKEY_LOCAL_MACHINE,
          CBuilderRegKey[I], 'App');
        if ExePath <> '' then
        begin
          Installed := FileExists(ExePath);
        end
        else
        begin
          ExePath := GetRegistryValue(HKEY_LOCAL_MACHINE, CBuilderRegKey[I],
            'CBuilder ' + IntToStr(I));
        end;
        if ExePath <> '' then
        begin
          Installed := FileExists(ExePath);
        end;
        if Installed then
        begin
          // The root path to the current compiler. This path never contains a macro as it is the base
          // for macro expansion/collapsing.   
          Proj := GetRegistryValue(HKEY_LOCAL_MACHINE, CBuilderRegKey[I], 'RootDir');
          if Proj = '' then
            Proj := GetRegistryValue(HKEY_CURRENT_USER, CBuilderRegKey[I], 'RootDir');
          if Proj <> '' then
            Proj := IncludeTrailingBackslash(Proj) + 'Projects';
          break;
        end;
      end;
    end;
  end;
  if Installed then
  begin
    // return the 'root'
    ExePath := ExtractFilePath(ExcludeTrailingBackSlash(ExtractFilePath(ExePath)));
    Add(ExcludeTrailingBackslash(Format('BCB=%s', [ExePath])));
    Add(ExcludeTrailingBackslash(Format('BDS=%s', [ExePath])));
    Add(ExcludeTrailingBackslash(Format('BDSPROJECTSDIR=%s', [Proj])));
  end;
end;
      
function WinVersion: integer;
type
    _VerRec = packed record
      Maj: byte;
      Min: byte;
      Bld: WORD;
    end;
    VerRec = packed record
      case boolean of
        true:
           (DW: DWORD);
        false:
           (v: _VerRec);
    end;
var
  ver: VerRec;
begin
  ver.DW := GetVersion;
  Result := (ver.v.maj *100) + ver.v.min;
end;

constructor TPathParser.Create(Opts: TPathParserOptions = [ppDefault]);
begin
  inherited Create;
//  CaseSensitive := False;
  fMustExist := ppMustExist in Opts;
  if ppDefault in Opts then
    InitMap;
  if ppDrives in Opts then
    GetDrives;
  if ppBorland in Opts then
    GetBorlandFolders;

  fOnLocalFolder := nil;
  Level := 0;
end;

function TPathParser.DriveName(var name: string; const drive: string): Boolean;
var
  Bits: set of 0..25;
  DriveStr: string;
  d: char;
  NamLen: Cardinal;
  Num: Integer;
  OldErrMode: DWord;
  SysFlags: DWord;
  SysLen: DWord;
  VolNameAry: array[0..255] of Char;
begin
  NamLen := 255;
//  SysLen := 255;
  VolNameAry[0] := #0;
  Result := False;
  name := '';
  if Length(Drive) < 2 then
    exit;
  DriveStr := UpperCase(drive);
  d := DriveStr[1];
  if drive <> '\' then                // Only for local drives
  begin
    if (d < 'A') or (d > 'Z') then
      exit;

    Integer(Bits) := GetLogicalDrives();
    Num := Ord(d) - Ord('A');
    if not (Num in Bits) then
      exit;
    DriveStr := d + ':\';
  end;

  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  // Turn off critical errors:
  Result :=  GetVolumeInformation(Pchar(DriveStr), VolNameAry,
    NamLen, nil, SysLen, SysFlags, Nil, 0);
  SetErrorMode(OldErrMode);   // Restore critical errors:
  if Result then
    Name := VolNameAry;
end;

procedure TPathParser.GetDrives;
var
  c: char;
  name: string;
begin
  for c := 'C' to 'Z' do
    if DriveName(name, c + ':') then
    begin
      Add(name + ':=' + c + ':');
    end;
end;

class function TPathParser.GetSpecialFolder(const Name: TSpecialFolder): String;
const
  FoldersMap: array[TSpecialFolder] of Cardinal = (CSIDL_DESKTOP,
    CSIDL_APPDATA, CSIDL_TEMPLATES, CSIDL_PROGRAMS, CSIDL_PERSONAL,
    CSIDL_FAVORITES, CSIDL_STARTUP, CSIDL_RECENT,
    CSIDL_SENDTO, CSIDL_STARTMENU,
    CSIDL_FONTS, CSIDL_HISTORY, CSIDL_COOKIES, CSIDL_INTERNET_CACHE,
    CSIDL_COMMON_FAVORITES, CSIDL_COMMON_DESKTOPDIRECTORY,
    CSIDL_COMMON_STARTUP,
    CSIDL_COMMON_PROGRAMS, {CSIDL_PROGRAM_FILESX86,} CSIDL_COMMON_STARTMENU, 0, 0, 0, 0, 0);
var
  Res:  Bool;
  Path: array[0..MAX_PATH - 1] of Char;
  Reg:  TRegistry;
  OSErr: DWORD;
  sf: TSpecialFolder;
begin
  Result := '';
  sf := Name;
  if WinVersion < 500 then
  begin
    case Name of
    sfCommonFavorites: sf := sfFavorites;
    sfCommonDesktop: sf := sfDesktop;
    sfCommonStartup: sf := sfStartup;
    sfCommonPrograms: sf := sfPrograms;
    sfCommonStartMenu: sf := sfStartMenu;
    end;
  end;
  if sf = sfProgramFilesX86 then
  begin
    begin
{$IFDEF VERD6up}
      Reg := TRegistry.Create(KEY_READ);
{$else}
      Reg := TRegistry.Create;//(KEY_READ);
{$endif}
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion',
          False);
        Result := Reg.ReadString('ProgramFilesDir (x86)');
      finally
        Reg.Free;
      end;
        if Result = '' then
          sf := sfProgramFiles
        else
        begin
          Result := IncludeTrailingBackslash(Result);
          Exit;
        end;
    end;
  end;
  case sf{Name} of
    sfWindows:
    begin
      GetWindowsDirectory(Path, MAX_PATH);
    end;
    sfTemporary:
    begin
      GetTempPath(MAX_PATH, Path);
    end;
    sfSystem:
    begin
      GetSystemDirectory(Path, MAX_PATH);
    end;
    sfProgramFiles:
    begin
{$IFDEF VERD6up}
  Reg := TRegistry.Create(KEY_READ);
{$else}
  Reg := TRegistry.Create;//(KEY_READ);
{$endif}
      try
        Reg.RootKey := HKEY_LOCAL_MACHINE;
        Reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion',
          False);
        Result := IncludeTrailingBackslash(Reg.ReadString('ProgramFilesDir'));
      finally
        Reg.Free;
      end;
      Exit;
    end;
  else
    begin
      Res := ShGetSpecialFolderPath(0, Path, FoldersMap[sf{Name}], False);
    end;
    if not Res then
    begin
      OSErr := GetLastError;
      raise Exception.Create(ClassName +
        '.GetSpecialFolder: Error on ShGetSpecialFolderPath [' +
        IntToStr(Ord(Name)) + ']: ' + IntToHex(OSErr, 8));
    end;
  end;
  Result := IncludeTrailingBackslash(Path);
end;

procedure TPathParser.InitMap;
var
  I: TSpecialFolder;
begin

  for I := Low(TSpecialFolder) to High(TSpecialFolder) do
  begin
    Add(ExcludeTrailingBackslash(
      LowerCase(Copy(GetEnumName(TypeInfo(TSpecialFolder), Ord(I)), 3, MAX_PATH)) +
      '=' + GetSpecialFolder(I)));
  end;
  Add(ExcludeTrailingBackslash(Format('windowsvolume=%s',
    [GetSpecialFolder(sfWindows)[1]])));
//  GetBorlandFolders;
end;
  (*
function TPathParser.LocalFolders(const Ident: String; var Handled: Boolean): String;
begin
  Result := '';
end; *)
 
function TPathParser.Parse(const Path: String): String;
var
  ret: integer;
begin
  Result := Path;
  ret := 55;
  while (ret > 0) and (Pos('$(', Result) > 0) do
    ret := ParsePath(Result);
//  repeat
//    ret := ParsePath(Result);
//  until ( ret <= 0);
  if (ret < 0) and MustExist then
    Result := BAD_FOLDER;
//  if not ParsePath(Result) and MustExist then
//    Result := BAD_FOLDER;
end;


// return >0 substitution made, <0 error
function TPathParser.ParsePath(var Path: string): Integer;
var
  S, P, rep: String;
  I, I2, Pos: Integer;
  handled: Boolean;
begin
  Result := 0; // nothing done
  P := Path;    // need copy
  I := 1;
  while I <= Length(P) - 3 do
  begin
    if (P[I] = '$') and (P[I + 1] = '(') then
    begin
      I2 := I + 2;
      while (I2 <= Length(P)) and (P[I2] <> ')') do
        Inc(I2);
      if I2 > Length(P) then
        Break;
      S := Copy(P, I + 2, I2 - (I + 2));
      Rep := '';
      Handled := False;
      Pos := IndexOfName(S);
      if Pos > -1 then
      begin
        Rep := Values[S];
        //Rep := ValueFromIndex[Pos];
        Handled := True;
      end;
      if (not handled) and (Level < 2) and assigned(OnLocalFolder) then
      begin
        try
          Inc(Level);
//          Rep := LocalFolders(S, Handled);
//          if (not Handled) and assigned(GetLocal) then
//          begin
          Rep := S;
          OnLocalFolder(Rep, Handled);
//          end;
        finally
          Dec(Level);
        end;
      end;
      if not Handled then
      begin
        // error
        Level := 0;  // reset
        Result := -1;
        exit;
      end//;
      else
        Inc(Result);
      System.Delete(P, I, I2 - I + 1);
      System.Insert(Rep, P, I);
      Inc(I, Length(Rep));
    end
    else
      Inc(I);
  end;
  Path := P;
//  Result := True;  // good
end;

end.

