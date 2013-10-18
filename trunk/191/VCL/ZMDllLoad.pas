unit ZMDllLoad;

//  ZMDllLoad.pas - Dynamically load the DLL

(* ***************************************************************************
TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009, 2010, 2011, 2012, 2013 Russell Peters and Roger Aelbrecht

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
//modified 2012-04-08
                           
{$I   '.\ZipVers.inc'}
{$I   '.\ZMConfig191.inc'}

interface

uses
  Classes, Windows, ZipMstr, ZMDelZip;

procedure _DLL_Abort(Master: TCustomZipMaster; key: Cardinal);
function _DLL_Banner: String;
function _DLL_Build: Integer;
function _DLL_Exec(worker: TZMWorkBase; const Rec: pDLLCommands; var key:
    Cardinal): Integer;
function _DLL_Load(worker: TZMWorkBase): Integer;
function _DLL_Loaded(Master: TCustomZipMaster): Boolean;
function _DLL_Path: String;
procedure _DLL_Remove(Master: TCustomZipMaster);
procedure _DLL_Unload(worker: TZMWorkBase);

implementation

uses
  SysUtils, ZMCompat, ZMXcpt,
{$IFNDEF STATIC_LOAD_DELZIP_DLL}
  ZMCore, ZMInflt, ZMUtils, ComObj, ActiveX,
{$IFNDEF VERpre6}
  SyncObjs,
{$ENDIF}
{$ENDIF}
  ZMMsg, ZMWFuncs;

const
  __UNIT__ = 11 shl 23;

procedure CheckExec(RetVal: Integer);
const
  __ERR_GE_DLLCritical = __UNIT__ + (93 shl 10) + GE_DLLCritical;
  __ERR_DS_Canceled = __UNIT__ + (95 shl 10) + DS_Canceled;
  __ERR_DZ_ERR = __UNIT__ + (97 shl 10);
var
  x: Integer;
begin
  if RetVal < 0 then
  begin
    x := -RetVal;
    if x > _DZ_ERR_MAX then
      raise EZipMaster.CreateMsgInt(__ERR_GE_DLLCritical, x);
    if (x = _DZ_ERR_CANCELLED) or (x = _DZ_ERR_ABORT) then
      x := __ERR_DS_Canceled
    else
      x := x + DZ_RES_GOOD + __ERR_DZ_ERR;
    raise EZipMaster.CreateMsgDisp(-x, True);
  end;
end;
{$IFDEF STATIC_LOAD_DELZIP_DLL}
// 'static' loaded dll functions
function DZ_Abort(C: Cardinal): Integer; STDCALL; EXTERNAL DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Path: pChar; STDCALL; EXTERNAL DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_PrivVersion: Integer; STDCALL; EXTERNAL DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Exec(C: pDLLCommands): Integer; STDCALL; EXTERNAL DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Version: Integer; STDCALL; EXTERNAL DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Banner: pChar; STDCALL; EXTERNAL DelZipDLL_Name
{$IFDEF VERD2010up} Delayed {$ENDIF};
function DZ_Name(var buf; bufsiz: Integer; wide: Boolean): Integer; STDCALL;
external DelZipDLL_Name {$IFDEF VERD2010up} Delayed {$ENDIF};
{$ELSE}

type
  TZMCount = record
    master: TCustomZipMaster;
    Count: Integer;
  end;

type
  TZMDLLLoader = class(TObject)
  private
    AbortFunc: TAbortOperationFunc;
    BannerFunc: TDLLBannerFunc;
    Counts: array of TZMCount;
    ExecFunc: TDLLExecFunc;
    fBanner: String;
    fHasResDLL: Integer;
    fKillTemp: Boolean;
    fLoadErr: Integer;
    fLoading: Integer;
    fLoadPath: String;
    fPath: String;
    fVer: Integer;
    // guard data for access by several threads
{$IFDEF VERpre6}
    CSection: TRTLCriticalSection;
{$ELSE}
    Guard: TCriticalSection;
{$ENDIF}
    hndl: HWND;
    NameFunc: TDLLNameFunc;
    PathFunc: TDLLPathFunc;
    Priv: Integer;
    PrivFunc: TDLLPrivVersionFunc;
    TmpFileName: String;
    VersFunc: TDLLVersionFunc;
    function GetIsLoaded: Boolean;
    function LoadLib(worker: TZMWorkBase; FullPath: String; MustExist: Boolean):
        Integer;
    procedure ReleaseLib;
{$IFNDEF Win64}
    procedure RemoveTempDLL;
{$ENDIF}
  protected
    function Counts_Dec(worker: TZMWorkBase; aMaster: TCustomZipMaster): Integer;
    function Counts_Find(Master: TCustomZipMaster): Integer;
    function Counts_Inc(worker: TZMWorkBase): Integer;
    procedure Empty;
{$IFNDEF Win64}
    function ExtractResDLL(worker: TZMWorkBase; OnlyVersion: Boolean): Integer;
{$ENDIF}
    function LoadDLL(worker: TZMWorkBase): Integer;
    function UnloadDLL: Integer;
    property IsLoaded: Boolean Read GetIsLoaded;
  public
    procedure Abort(Master: TCustomZipMaster; key: Cardinal);
    procedure AfterConstruction; override;
    function Banner: String;
    procedure BeforeDestruction; override;
    function Build: Integer;
    function Exec(worker: TZMWorkBase; const Rec: pDLLCommands; var key: Cardinal):
        Integer;
    function Load(worker: TZMWorkBase): Integer;
    function Loaded(Master: TCustomZipMaster): Boolean;
    function Path: String;
    procedure Remove(Master: TCustomZipMaster);
    procedure Unload(worker: TZMWorkBase);
    property Ver: Integer Read fVer;
  end;

const
  RESVER_UNTRIED = -99; // have not looked for resdll yet
  RESVER_NONE = -1; // not available
  RESVER_BAD = 0; // was bad copy/version
  MIN_RESDLL_SIZE = 50000;
  MAX_RESDLL_SIZE = 600000;

var
  G_LoadedDLL: TZMDLLLoader = nil;

procedure TZMDLLLoader.Abort(Master: TCustomZipMaster; key: Cardinal);
begin
  if Loaded(Master) and (hndl <> 0) then
    AbortFunc(key);
end;

procedure TZMDLLLoader.AfterConstruction;
begin
  inherited;
{$IFDEF VERpre6}
  InitializeCriticalSection(CSection);
{$ELSE}
  Guard := TCriticalSection.Create;
{$ENDIF}
  fKillTemp := False;
  Empty;
  fPath := DelZipDLL_Name;
  TmpFileName := '';
  fLoading := 0;
  fBanner := '';
  fHasResDLL := RESVER_UNTRIED; // unknown
end;

function TZMDLLLoader.Banner: String;
var
  tmp: AnsiString;
begin
  Result := '';
  if IsLoaded then
  begin
    tmp := BannerFunc;
    Result := String(tmp);
  end;
end;

procedure TZMDLLLoader.BeforeDestruction;
begin
  if hndl <> 0 then
    FreeLibrary(hndl);
  Counts := nil;
{$IFDEF VERpre6}
  DeleteCriticalSection(CSection);
{$ELSE}
  FreeAndNil(Guard);
{$ENDIF}
  hndl := 0;
{$IFNDEF Win64}
  RemoveTempDLL;
{$ENDIF}
  inherited;
end;

function TZMDLLLoader.Build: Integer;
begin
  Result := 0;
  if IsLoaded then
    Result := Priv;
end;

{ TZMDLLLoader }

//function TZMDLLLoader.Counts_Dec(worker: TZMWorkBase): Integer;
function TZMDLLLoader.Counts_Dec(worker: TZMWorkBase; aMaster:
    TCustomZipMaster): Integer;
const
  __ERR_LD_DLLUnloaded = __UNIT__ + (304 shl 10) + LD_DLLUnloaded;
var
  i: Integer;
  keepLoaded: Boolean;
  p: string;
  zm: TCustomZipMaster;
begin
  Result := -1;
{$IFDEF VERpre6}
  EnterCriticalSection(CSection);
{$ELSE}
  Guard.Enter;
{$ENDIF}
  try
    if worker <> nil then
      zm := worker.Master
    else
      zm := aMaster;
    // find master
    i := Counts_Find(zm);
    if i >= 0 then
    begin
      // found
      Dec(Counts[i].Count);
      Result := Counts[i].Count;
      if Result < 1 then
      begin
        // not wanted - remove from list
        Counts[i].master := nil;
        Counts[i].Count := 0;
      end;
    end;
    // ignore unload if loading
    if fLoading = 0 then
    begin
      keepLoaded := False;
      for i := 0 to HIGH(Counts) do
        if (Counts[i].master <> nil) and (Counts[i].Count > 0) then
        begin
          keepLoaded := True;
          break;
        end;

      if not keepLoaded then
      begin
        p := fPath;
        UnloadDLL;
        if (worker <> nil) and (worker.Verbosity >= zvVerbose) then
          worker.ReportMsg(__ERR_LD_DLLUnloaded, [p]);
      end;
    end;
  finally
{$IFDEF VERpre6}
  LeaveCriticalSection(CSection);
{$ELSE}
    Guard.Leave;
{$ENDIF}
  end;
end;

function TZMDLLLoader.Counts_Find(Master: TCustomZipMaster): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Counts <> nil then
  begin
    for i := 0 to HIGH(Counts) do
    begin
      if Counts[i].Master = Master then
      begin
        // found
        Result := i;
        break;
      end;
    end;
  end;
end;

function TZMDLLLoader.Counts_Inc(worker: TZMWorkBase): Integer;
const
  __ERR_LD_LoadErr = __UNIT__ + (419 shl 10) + LD_LoadErr;
  __ERR_LD_DLLLoaded = __UNIT__ + (426 shl 10) + LD_DLLLoaded;
var
  i: Integer;
  len: Integer;
  loadVer: Integer;
  zm: TCustomZipMaster;
begin
{$IFDEF VERpre6}
  EnterCriticalSection(CSection);
{$ELSE}
  Guard.Enter;
{$ENDIF}
  try
    zm := worker.Master;
    // find master
    i := Counts_Find(zm);
    if i >= 0 then
    begin
      // found
      Inc(Counts[i].Count);
      Result := Counts[i].Count;
    end
    else
    begin
      // need new one - any empty
      i := Counts_Find(nil);
      if i >= 0 then
      begin
        // have empty position - use it
        Counts[i].master := zm;
        Counts[i].Count := 1;
        Result := 1;
      end
      else
      begin
        // need to extend
        len := HIGH(Counts);
        if len > 0 then
          Inc(len)
        else
          len := 0;
        SetLength(Counts, len + 4);
        // clear the rest
        for i := len + 3 downto len + 1 do
        begin
          Counts[i].master := nil;
          Counts[i].Count := 0;
        end;
        i := len;
        Counts[i].master := zm;
        Counts[i].Count := 1;
        Result := 1;
      end;
    end;
    if not IsLoaded then
    begin
      // avoid re-entry
      Inc(fLoading);
      try
        if fLoading = 1 then
        begin
          try
            loadVer := LoadDLL(worker);
          except
            on Ers: EZipMaster do
            begin
              loadVer := -1;
              worker.ShowExceptionError(Ers);
            end;
            on E: Exception do
            begin
              loadVer := -1;
              worker.ShowExceptionError(E);
            end;
          end;
          if loadVer < DELZIPVERSION then
          begin // could not load it - empty it (i is index for this worker)
            Counts[i].master := nil;
            Counts[i].Count := 0;

            if worker.Verbosity >= zvVerbose then
              worker.ReportMsg(__ERR_LD_LoadErr, [fLoadErr, SysErrorMessage(fLoadErr)
                  , fLoadPath]);
            Result := -1;
          end
          else
          begin
            if worker.Verbosity >= zvVerbose then
              worker.ReportMsg(__ERR_LD_DLLLoaded, [fPath]);
          end;
        end;
      finally
        Dec(fLoading);
      end;
    end;
  finally
{$IFDEF VERpre6}
  LeaveCriticalSection(CSection);
{$ELSE}
    Guard.Leave;
{$ENDIF}
  end;
end;

procedure TZMDLLLoader.Empty;
begin
  hndl := 0;
  ExecFunc := nil;
  VersFunc := nil;
  PrivFunc := nil;
  AbortFunc := nil;
  NameFunc := nil;
  PathFunc := nil;
  BannerFunc := nil;
  fVer := 0;
  Priv := 0;
  fBanner := '';
end;

function TZMDLLLoader.Exec(worker: TZMWorkBase; const Rec: pDLLCommands; var
    key: Cardinal): Integer;
begin
  Result := -1; // what error
  if Counts_Inc(worker) > 0 then
  begin
    try
      Result := ExecFunc(Rec);
    finally
      Counts_Dec(worker, nil);
      key := 0;
    end;
  end;
end;

{$IFNDEF Win64}
function TZMDLLLoader.ExtractResDLL(worker: TZMWorkBase; OnlyVersion: Boolean):
    Integer;
const
  __ERR_DS_NoTempFile = __UNIT__ + (513 shl 10) + DS_NoTempFile;
  __ERR_DS_NoTempFile1 = __UNIT__ + (523 shl 10) + DS_NoTempFile;
var
  done: Boolean;
  uid: TGUID;
  fs: TFileStream;
  len: Integer;
  rs: TResourceStream;
  temppath: String;
  w: Word;
begin
  done := False;
  Result := -1;
  fs := nil;
  rs := nil;
  try
    // only check if unknown or know exists
    if (fHasResDLL = RESVER_UNTRIED) or (fHasResDLL >= MIN_DLL_BUILD) then
      rs := OpenResStream(DZRES_DLL, RT_RCDATA);
    if fHasResDLL = RESVER_UNTRIED then
      fHasResDLL := RESVER_NONE; // in case of exception
    // read the dll version if it exists
    if (rs <> nil) and (rs.Size >= MIN_RESDLL_SIZE) and
      (rs.Size < MAX_RESDLL_SIZE) then
    begin
      rs.Position := 0;
      rs.ReadBuffer(Result, sizeof(Integer));
      fHasResDLL := Result;   // the dll version
      if (Result >= MIN_DLL_BUILD) and not OnlyVersion then
      begin
        rs.ReadBuffer(w, sizeof(Word));
        rs.Position := sizeof(Integer);
        temppath := worker.Master.TempDir;
        if Length(temppath) = 0 then // Get the system temp dir
        begin
          SetLength(temppath, MAX_PATH + 2);
          len := GetTempPath(MAX_PATH, PChar(temppath));
          if (len > MAX_PATH) or (len = 0) then
            raise EZipMaster.CreateMsgDisp(__ERR_DS_NoTempFile, True);
          temppath := PChar(temppath);
        end
        else // Use Temp dir provided by ZipMaster
          temppath := DelimitPath(worker.Master.TempDir, True);
        if CoCreateGuid(uid) = S_OK then
          TmpFileName := temppath + GUIDToString(uid) + '.dll'
        else
          TmpFileName := worker.Master.MakeTempFileName('DZ_', '.dll');
        if TmpFileName = '' then
            raise EZipMaster.CreateMsgDisp(__ERR_DS_NoTempFile1, True);
        fs := TFileStream.Create(TmpFileName, fmCreate);
        done := ExtractZStream(fs, rs, rs.Size - sizeof(Integer)) = 0;
//        if w = IMAGE_DOS_SIGNATURE then
//          done := fs.CopyFrom(rs, rs.Size - sizeof(Integer)) =
//            (rs.Size - sizeof(Integer))
//        else
//          done := LZ77Extract(fs, rs, rs.Size - sizeof(Integer)) = 0;

        if not done then
          fHasResDLL := RESVER_BAD; // could not extract
      end;
    end;
  finally
    FreeAndNil(fs);
    FreeAndNil(rs);
    if not OnlyVersion then
    begin
      if (not done) and _Z_FileExists(TmpFileName) then
        _Z_DeleteFile(TmpFileName);
      if not _Z_FileExists(TmpFileName) then
        TmpFileName := '';
    end;
  end;
end;
{$ENDIF}

function TZMDLLLoader.GetIsLoaded: Boolean;
begin
  Result := hndl <> 0;
end;

function TZMDLLLoader.Load(worker: TZMWorkBase): Integer;
begin
  Result := 0;
  if Counts_Inc(worker) > 0 then
    Result := G_LoadedDLL.Ver;
end;

function TZMDLLLoader.LoadDLL(worker: TZMWorkBase): Integer;
const
  __ERR_LD_NoDLL = __UNIT__ + (582 shl 10) + LD_NoDLL;
var
  AllowResDLL: Boolean;
  FullPath: String;
  DBuild: Integer;
  DLLDirectory: String;
  dpth: string;
begin
  if hndl = 0 then
  begin
    fVer := 0;
    FullPath := '';
    DLLDirectory := DelimitPath(worker.Master.DLLDirectory, False);
    if DLLDirectory = '><' then
    begin
{$IFNDEF Win64}
      // use res dll (or else)
      if (TmpFileName <> '') or (ExtractResDLL(worker, False) >= MIN_DLL_BUILD) then
        LoadLib(worker, TmpFileName, True);
      if fver <= 0 then
{$ENDIF}
        raise EZipMaster.CreateMsgStr(__ERR_LD_NoDLL, DelZipDLL_Name);
      Result := fVer;
      exit;
    end;
    if DLLDirectory <> '' then
    begin
      // check relative?
      if DLLDirectory[1] = '.' then
        FullPath := PathConcat(ExtractFilePath(ParamStr(0)), DLLDirectory)
      else
        FullPath := DLLDirectory;
      if (ExtractNameOfFile(DLLDirectory) <> '') and
        (CompareText(ExtractFileExt(DLLDirectory), '.DLL') = 0) then
      begin
        // must load the named dll
        LoadLib(worker, FullPath, True);
        Result := fVer;
        exit;
      end;
      dpth := ExtractFilePath(FullPath);
      if (dpth <> '') and not _Z_DirExists(dpth) then
        FullPath := '';
    end;
{$IFDEF Win64}
    AllowResDLL := False;
{$ELSE}
    AllowResDLL := DLLDirectory = ''; // only if no path specified
    if AllowResDLL then
    begin
      // check for res dll once only
      if fHasResDLL = RESVER_UNTRIED then
        ExtractResDLL(worker, True);  // read the res dll version if it exists
      if fHasResDLL < MIN_DLL_BUILD then
        AllowResDLL := False;  // none or bad version
    end;
{$ENDIF}
    DBuild := LoadLib(worker, PathConcat(FullPath, DelZipDLL_Name), not AllowResDLL);
{$IFNDEF Win64}
    // if not loaded we only get here if allowResDLL is true;
    if DBuild < MIN_DLL_BUILD then
    begin
      // use resdll if no other available
      if (TmpFileName <> '') or (ExtractResDLL(worker, False) > 0) then
      begin
        if LoadLib(worker, TmpFileName, False) < MIN_DLL_BUILD then
        begin
          // could not load the res dll
          fHasResDLL := RESVER_BAD; // is bad version
        end;
      end;
    end;
{$ENDIF}
  end;
  Result := fVer;
end;

function TZMDLLLoader.Loaded(Master: TCustomZipMaster): Boolean;
var
  i: Integer;
begin
  Result := False;
  i := Counts_Find(Master);
  if (i >= 0) and (Counts[i].Count > 0) then
    Result := True;
end;

// returns build
function TZMDLLLoader.LoadLib(worker: TZMWorkBase; FullPath: String; MustExist:
    Boolean): Integer;
const
  __ERR_LD_LoadErr = __UNIT__ + (689 shl 10) + LD_LoadErr;
  __ERR_LD_NoDll = __UNIT__ + (691 shl 10) + LD_NoDll;
  __ERR_LD_BadDll = __UNIT__ + (718 shl 10) + LD_BadDll;
  __ERR_LD_BadDll1 = __UNIT__ + (719 shl 10) + LD_BadDll;
var
  oldMode: Cardinal;
  tmp: AnsiString;
begin
  if hndl > 0 then
    FreeLibrary(hndl);
  Empty;
  fLoadErr := 0;
  fLoadPath := FullPath;
  oldMode := SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOGPFAULTERRORBOX);
  try
    hndl := LoadLibrary(pChar(FullPath));
    if hndl > HInstance_Error then
    begin
      @ExecFunc := GetProcAddress(hndl, DelZipDLL_Execfunc);
      if (@ExecFunc <> nil) then
        @VersFunc := GetProcAddress(hndl, DelZipDLL_Versfunc);
      if (@VersFunc <> nil) then
        @PrivFunc := GetProcAddress(hndl, DelZipDLL_Privfunc);
      if (@PrivFunc <> nil) then
        @AbortFunc := GetProcAddress(hndl, DelZipDLL_Abortfunc);
      if (@AbortFunc <> nil) then
        @NameFunc := GetProcAddress(hndl, DelZipDLL_Namefunc);
      if (@NameFunc <> nil) then
        @BannerFunc := GetProcAddress(hndl, DelZipDLL_Bannerfunc);
      if (@BannerFunc <> nil) then
        @PathFunc := GetProcAddress(hndl, DelZipDLL_Pathfunc);
    end
    else
      fLoadErr := GetLastError;
  finally
    SetErrorMode(oldMode);
  end;
  if hndl <= HInstance_Error then
  begin
    Empty;
    if MustExist then
    begin
      if worker.Verbosity >= zvVerbose then
        worker.ReportMsg(__ERR_LD_LoadErr, [fLoadErr, SysErrorMessage(fLoadErr),
          fLoadPath]);
      raise EZipMaster.CreateMsgStr(__ERR_LD_NoDLL, FullPath + #13#10 + SysErrorMessage(fLoadErr));
//      raise EZipMaster.CreateMsgStr(__ERR_LD_NoDLL, FullPath);
    end;
    Result := 0;
    exit;
  end;
  if (@BannerFunc <> nil) then
  begin
    Priv := PrivFunc;
    fVer := VersFunc;
    SetLength(fPath, MAX_PATH + 1);
{$IFDEF UNICODE}
    NameFunc(fPath[1], MAX_PATH, True);
{$ELSE}
    NameFunc(fPath[1], MAX_PATH, False);
{$ENDIF}
    fPath := String(pChar(fPath));
    tmp := BannerFunc;
    fBanner := String(tmp);
  end;
  if (fVer <> DELZIPVERSION) or (Priv < MIN_DLL_BUILD) then
  begin
    FullPath := fPath;
    FreeLibrary(hndl);
    Empty;
    if MustExist then
    begin
      if worker.Verbosity >= zvVerbose then
        worker.ReportMsg(__ERR_LD_BadDll, [fLoadPath]);
      raise EZipMaster.CreateMsgStr(__ERR_LD_BadDll1, FullPath);
    end;
  end;
  Result := Priv;
end;

function TZMDLLLoader.Path: String;
begin
  Result := '';
  if IsLoaded then
    Result := fPath;
end;

procedure TZMDLLLoader.ReleaseLib;
begin
  if hndl <> 0 then
  begin
    FreeLibrary(hndl);
    hndl := 0;
  end;
  if hndl = 0 then
  begin
    Empty;
    fPath := '';
{$IFNDEF Win64}
    if fKillTemp then
      RemoveTempDLL;
{$ENDIF}
  end;
end;

procedure TZMDLLLoader.Remove(Master: TCustomZipMaster);
var
  i: Integer;
begin
{$IFDEF VERpre6}
  EnterCriticalSection(CSection);
{$ELSE}
  Guard.Enter;
{$ENDIF}
  try
    i := Counts_Find(Master);
    if i >= 0 then
    begin
      // found - remove it
      Counts[i].Master := nil;
      Counts[i].Count := 0;
    end;
  finally
{$IFDEF VERpre6}
  LeaveCriticalSection(CSection);
{$ELSE}
    Guard.Leave;
{$ENDIF}
  end;
end;

{$IFNDEF Win64}
procedure TZMDLLLoader.RemoveTempDLL;
var
  t: String;
begin
  t := TmpFileName;
  TmpFileName := '';
  fKillTemp := False;
  if (t <> '') and _Z_FileExists(t) then
    _Z_DeleteFile(t);
end;
{$ENDIF}

procedure TZMDLLLoader.Unload(worker: TZMWorkBase);
begin
  Counts_Dec(worker, nil);
end;

function TZMDLLLoader.UnloadDLL: Integer;
begin
  ReleaseLib;
  Result := fVer;
end;
{$ENDIF}
{ public functions }

procedure _DLL_Abort(Master: TCustomZipMaster; key: Cardinal);
begin
  if key <> 0 then
{$IFDEF STATIC_LOAD_DELZIP_DLL}
    DZ_Abort(key);
{$ELSE}
  G_LoadedDLL.Abort(Master, key);
{$ENDIF}
end;

function _DLL_Banner: String;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := DZ_Banner;
{$ELSE}
  Result := G_LoadedDLL.Banner;
{$ENDIF}
end;

function _DLL_Build: Integer;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := DZ_PrivVersion;
{$ELSE}
  Result := G_LoadedDLL.Build;
{$ENDIF}
end;

function _DLL_Exec(worker: TZMWorkBase; const Rec: pDLLCommands; var key:
    Cardinal): Integer;
begin
  try
{$IFDEF STATIC_LOAD_DELZIP_DLL}
    Result := DZ_Exec(Rec);
{$ELSE}
    Result := G_LoadedDLL.Exec(worker, Rec, key);
{$ENDIF}
    key := 0;
  except
    Result := -6;
    key := 0;
  end;
  CheckExec(Result);
end;

function _DLL_Load(worker: TZMWorkBase): Integer;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := DZ_Version;
{$ELSE}
  Result := G_LoadedDLL.Load(worker);
{$ENDIF}
end;

function _DLL_Loaded(Master: TCustomZipMaster): Boolean;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := True;
{$ELSE}
  Result := G_LoadedDLL.Loaded(Master);
{$ENDIF}
end;

function _DLL_Path: String;
begin
{$IFDEF STATIC_LOAD_DELZIP_DLL}
  Result := DZ_Path;
{$ELSE}
  Result := G_LoadedDLL.Path;
{$ENDIF}
end;

// remove from list
procedure _DLL_Remove(Master: TCustomZipMaster);
begin
{$IFNDEF STATIC_LOAD_DELZIP_DLL}
  if G_LoadedDLL <> nil then
  begin
  G_LoadedDLL.Counts_Dec(nil, Master); // unload if only loaded by Master
  G_LoadedDLL.Remove(Master); // remove from list
  end;
{$ENDIF}
end;

procedure _DLL_Unload(worker: TZMWorkBase);
begin
{$IFNDEF STATIC_LOAD_DELZIP_DLL}
  G_LoadedDLL.Unload(worker);
{$ENDIF}
end;

{$IFNDEF STATIC_LOAD_DELZIP_DLL}
initialization
G_LoadedDLL := TZMDLLLoader.Create;

finalization
FreeAndNil(G_LoadedDLL);
{$ENDIF}

end.
