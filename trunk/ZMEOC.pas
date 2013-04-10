unit ZMEOC;

//  ZMEOC.pas - EOC handling

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
//modified 2010-11-20

{$INCLUDE   '.\ZipVers.inc'}
{$IFDEF VER180}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

interface

uses
  Classes, ZipMstr, ZMStructs, ZMWorkFile, ZMWZip, ZMCompat, ZMCore;

type
  TZMEOC = class(TZMWorkZip)
  private
    fCentralDiskNo: Integer;
    fCentralEntries: Cardinal;
    fCentralOffset: Int64;
    fCentralSize: Int64;
    fEOCOffset: Int64;
    fMultiDisk:     Boolean;
    fOffsetDelta: Int64;
    fTotalEntries:  Cardinal;
    fVersionMadeBy: Word;
    fVersionNeeded: Word;
    fZ64:           Boolean;
    fZ64VSize: Int64;
    FZipComment: AnsiString;
    function GetEOC64(Ret: Integer): Integer;
    function GetZipCommentLen: Integer;
    procedure SetZipComment(const Value: AnsiString);
    procedure SetZipCommentLen(const Value: Integer);
  protected
    function OpenEOC1: Integer;
  public
    procedure AfterConstruction; override;
    procedure AssignFrom(Src: TZMWorkFile); override;
    function OpenEOC(EOConly: boolean): Integer;
    function OpenLast(EOConly: boolean; OpenRes: Integer): integer;
    function WriteEOC: Integer;
    property CentralDiskNo: Integer read fCentralDiskNo write fCentralDiskNo;
    property CentralEntries: Cardinal read fCentralEntries write fCentralEntries;
    property CentralOffset: Int64 read fCentralOffset write fCentralOffset;
    property CentralSize: Int64 read fCentralSize write fCentralSize;
    property EOCOffset: Int64 read fEOCOffset write fEOCOffset;
    property MultiDisk: Boolean read fMultiDisk write fMultiDisk;
    property OffsetDelta: Int64 read fOffsetDelta write fOffsetDelta;
    property TotalEntries: Cardinal read fTotalEntries write fTotalEntries;
    property VersionMadeBy: Word read fVersionMadeBy write fVersionMadeBy;
    property VersionNeeded: Word read fVersionNeeded write fVersionNeeded;
    property Z64: Boolean read fZ64 write fZ64;
    property Z64VSize: Int64 read fZ64VSize write fZ64VSize;
    property ZipComment: AnsiString read FZipComment write SetZipComment;
    property ZipCommentLen: Integer read GetZipCommentLen write SetZipCommentLen;
  end;

const
  zfi_EOC: cardinal = $800;         // valid EOC found

const
  EOCBadStruct = 2;
  EOCBadComment = 1;
  EOCWant64 = 64;


implementation

uses Windows, SysUtils, ZMXcpt, ZMMsg, ZMUtils, ZMWFuncs;

const
  __UNIT__ = 14 shl 23;

function NameOfPart(const fn: String; Compat: Boolean): String;
var
  r, n: Integer;
  SRec: _Z_TSearchRec;
  fs: String;
begin
  Result := '';
  if Compat then
    fs := fn + '.z??*'
  else
    fs := fn + '???.zip';
  r := _Z_FindFirst(fs, faAnyFile, SRec);
  while r = 0 do
  begin
    if Compat then
    begin
      fs := UpperCase(Copy(ExtractFileExt(SRec.Name), 3, 20));
      if fs = 'IP' then
        n := 99999
      else
        n := StrToIntDef(fs, 0);
    end
    else
      n := StrToIntDef(Copy(SRec.Name, Length(SRec.Name) - 6, 3), 0);
    if n > 0 then
    begin
      Result := SRec.Name; // possible name
      break;
    end;
    r := _Z_FindNext(SRec);
  end;
  _Z_FindClose(SRec);
end;

procedure TZMEOC.AfterConstruction;
begin
  inherited;
  fMultiDisk := false;
  fEOCOffset := 0;
  fZipComment := '';
end;

procedure TZMEOC.AssignFrom(Src: TZMWorkFile);
var
  theSrc: TZMEOC;
begin
  inherited;
  if (Src is TZMEOC) and (Src <> Self) then
  begin
    theSrc := TZMEOC(Src);
    fCentralDiskNo := theSrc.fCentralDiskNo;
    fCentralEntries := theSrc.fCentralEntries;
    fCentralOffset := theSrc.fCentralOffset;
    fCentralSize := theSrc.fCentralSize;
    fEOCOffset := theSrc.fEOCOffset;
    fMultiDisk := theSrc.fMultiDisk;
    fOffsetDelta := theSrc.fOffsetDelta;
    fTotalEntries := theSrc.fTotalEntries;
    fVersionMadeBy := theSrc.fVersionMadeBy;
    fVersionNeeded := theSrc.fVersionNeeded;
    fZ64 := theSrc.fZ64;
    fZ64VSize := theSrc.fZ64VSize;
    FZipComment := theSrc.FZipComment;
  end;
end;

function TZMEOC.GetEOC64(Ret: Integer): Integer;
const
  __ERR_DS_FailedSeek = __UNIT__ + (215 shl 10) + DS_FailedSeek;
  __ERR_DS_EOCBadRead = __UNIT__ + (220 shl 10) + DS_EOCBadRead;
  __ERR_DS_EOCBadRead1 = __UNIT__ + (231 shl 10) + DS_EOCBadRead;
  __ERR_DS_FailedSeek1 = __UNIT__ + (236 shl 10) + DS_FailedSeek;
  __ERR_DS_EOCBadRead2 = __UNIT__ + (241 shl 10) + DS_EOCBadRead;
  __ERR_DS_Unsupported = __UNIT__ + (251 shl 10) + DS_Unsupported;
var
  posn: Int64;
  Loc: TZip64EOCLocator;
  eoc64: TZipEOC64;
  CEnd: Int64;  // end of central directory
  function IsLocator(Locp: PZip64EOCLocator): boolean;
  begin
    Result := false;
    if (Locp^.LocSig <> EOC64LocatorSig) then
      exit;
    if (DiskNr = MAX_WORD) and (Locp^.NumberDisks < MAX_WORD) then
      exit;
    Result := true;
  end;
begin
  Result := Ret;
  if (Result <= 0) or ((Result and EOCWant64) = 0) then
    exit;
  CEnd := EOCOffset;
  posn := EOCOffset - sizeof(TZip64EOCLocator);
  if posn >= 0 then
  begin
    if Seek(posn, 0) < 0 then
    begin
      result := -__ERR_DS_FailedSeek;
      exit;
    end;
    if Read(Loc, sizeof(TZip64EOCLocator)) <> sizeof(TZip64EOCLocator) then
    begin
      Result := -__ERR_DS_EOCBadRead;
      exit;
    end;
    if (IsLocator(@Loc)) then
    begin
      // locator found
      fZ64 := true;  // in theory anyway - if it has locator it must be Z64
      TotalDisks := Loc.NumberDisks;
      DiskNr := Loc.NumberDisks - 1; // is last disk
      if Integer(Loc.EOC64DiskStt) <> DiskNr then
      begin
        Result := -__ERR_DS_EOCBadRead1;
        exit;
      end;
      if Seek(Loc.EOC64RelOfs, 0) < 0 then
      begin
        Result := -__ERR_DS_FailedSeek1;
        exit;
      end;
      if Read(eoc64, sizeof(TZipEOC64)) <> sizeof(TZipEOC64) then
      begin
        Result := -__ERR_DS_EOCBadRead2;
        exit;
      end;
      if (eoc64.EOC64Sig = EndCentral64Sig) then
      begin
        // read EOC64
        fVersionNeeded := eoc64.VersionNeed;
        if ((VersionNeeded and VerMask) > ZIP64_VER) or
              (eoc64.vsize < (sizeof(TZipEOC64) - 12)) then
        begin
          Result := -__ERR_DS_Unsupported;
          exit;
        end;
        CEnd := Loc.EOC64RelOfs;
        fVersionMadeBy := eoc64.VersionMade;
        fZ64VSize := eoc64.vsize + 12;
        if CentralDiskNo = MAX_WORD then
        begin
          CentralDiskNo := eoc64.CentralDiskNo;
          fZ64 := true;
        end;
        if TotalEntries = MAX_WORD then
        begin
          TotalEntries := Cardinal(eoc64.TotalEntries);
          fZ64 := true;
        end;
        if CentralEntries = MAX_WORD then
        begin
          CentralEntries := Cardinal(eoc64.CentralEntries);
          fZ64 := true;
        end;
        if CentralSize = MAX_UNSIGNED then
        begin
          CentralSize := eoc64.CentralSize;
          fZ64 := true;
        end;
        if CentralOffset = MAX_UNSIGNED then
        begin
          CentralOffset := eoc64.CentralOffset;
          fZ64 := true;
        end;
      end;
    end;
    // check structure
    OffsetDelta := CEnd - CentralSize - CentralOffset;
    if OffsetDelta <> 0 then
      Result := Result or EOCBadStruct;
  end;
end;

function TZMEOC.GetZipCommentLen: Integer;
begin
  Result := Length(ZipComment);
end;

(*? TZMEOC.OpenEOC
// Function to find the EOC record at the end of the archive (on the last disk.)
// We can get a return value or an exception if not found.
 1.73 28 June 2003 RP change handling split files
 return
    <0 - -reason for not finding
    >=0 - found
    Warning values (ored)
     1 - bad comment
     2 - bad structure (Central offset wrong)
*)
function TZMEOC.OpenEOC(EOConly: boolean): Integer;
begin
  try
    Result := OpenEOC1;
    if (Result >= EOCWant64) and not EOConly then
      Result := GetEOC64(Result);
    if Result > 0 then
      Result := Result and (EOCBadComment or EOCBadStruct);
  except
    on E: EZipMaster do
    begin
      File_Close;
      Result := -E.ResId;
    end;
    else
    begin
      File_Close;
      raise;
    end;
  end;
end;

function TZMEOC.OpenEOC1: Integer;
const
  __ERR_DS_FailedSeek = __UNIT__ + (368 shl 10) + DS_FailedSeek;
  __ERR_DS_FileOpen = __UNIT__ + (372 shl 10) + DS_FileOpen;
  __ERR_DS_NoInFile = __UNIT__ + (374 shl 10) + DS_NoInFile;
  __ERR_DS_NoValidZip = __UNIT__ + (384 shl 10) + DS_NoValidZip;
  __ERR_DS_NoValidZip1 = __UNIT__ + (403 shl 10) + DS_NoValidZip;
  __ERR_DS_FailedSeek1 = __UNIT__ + (433 shl 10) + DS_FailedSeek;
  __ERR_DS_EOCBadRead = __UNIT__ + (411 shl 10) + DS_EOCBadRead;
  __ERR_DS_EOCBadRead1 = __UNIT__ + (436 shl 10) + DS_EOCBadRead;
var
  fEOC: TZipEndOfCentral;
  pEOC: PZipEndOfCentral;
  Size, i, j: Integer;
  Sg: Cardinal;
  ZipBuf: array of AnsiChar;
  AfterEOC, clen: integer;
begin
  fZipComment := '';
  MultiDisk := false;
  DiskNr := 0;
  TotalDisks := 0;
  TotalEntries := 0;
  CentralEntries := 0;
  CentralDiskNo := 0;
  CentralOffset := 0;
  CentralSize := 0;
  fEOCOffset := 0;
  fVersionMadeBy := 0;
  fVersionNeeded := 0;
  OffsetDelta := 0;
  pEOC := nil;

  Result := 0;

  // Open the input archive, presumably the last disk.
  if not IsOpen then
    File_Open(fmOpenRead + fmShareDenyWrite)//;
  else
    CheckSeek(0, 0, -__ERR_DS_FailedSeek);
  if not IsOpen then
  begin
    if FileExists(FileName) then
      Result := -__ERR_DS_FileOpen
    else
      Result := -__ERR_DS_NoInFile;
    Exit;
  end;

  // First a check for the first disk of a spanned archive,
  // could also be the last so we don't issue a warning yet.
  Sig := zfsNone;
  try
    if Read(Sg, 4) <> 4 then
    begin
      Result := -__ERR_DS_NoValidZip;
      exit;
    end;
    if (Sg and $FFFF) = IMAGE_DOS_SIGNATURE then
      Sig := zfsDOS
    else
    if (Sg = LocalFileHeaderSig) then
      Sig := zfsLocal
    else
    if (Sg = ExtLocalSig) and (Read(Sg, 4) = 4) and (Sg = LocalFileHeaderSig) then
    begin
      Sig := zfsMulti;
      MultiDisk := True; // will never be true on 'valid' multi-part zip with eoc
    end;

    // Next we do a check at the end of the file to speed things up if
    // there isn't a Zip archive ZipComment.
    File_Size := Seek(-SizeOf(TZipEndOfCentral), soFromEnd);
    if File_Size < 0 then
      Result := -__ERR_DS_NoValidZip1
    else
    begin
      File_Size := File_Size + SizeOf(TZipEndOfCentral);
      // Save the archive size as a side effect.
      RealFileSize := File_Size;
      // There could follow a correction on FFileSize.
      if Read(fEOC, SizeOf(TZipEndOfCentral)) <> sizeof(TZipEndOfCentral) then
        Result := -__ERR_DS_EOCBadRead
      else
      if (fEOC.HeaderSig = EndCentralDirSig) then
      begin
        fEOCOffset := File_Size - SizeOf(TZipEndOfCentral);
        Result := 8;    // something found
        if fEOC.ZipCommentLen <> 0 then
        begin
          fEOC.ZipCommentLen := 0;    // ??? make safe
          Result := EOCBadComment;//1;        // return bad comment
        end;
        pEOC := @fEOC;
      end;
    end;

    if Result = 0 then  // did not find it - must have ZipComment
    begin
      Size := 65535 + SizeOf(TZipEndOfCentral);
      if File_Size < Size then
        Size := Integer(File_Size);
      SetLength(ZipBuf, Size);
      if Seek(-Size, soFromEnd) < 0 then
        Result := -__ERR_DS_FailedSeek1
      else
        if Read(PByte(ZipBuf)^, Size) <> Size then
          Result := -__ERR_DS_EOCBadRead1;
      //  end;
      if Result = 0 then
      begin
        for i := Size - SizeOf(TZipEndOfCentral) - 1 downto 0 do
          if PZipEndOfCentral(PAnsiChar(ZipBuf) + i)^.HeaderSig = EndCentralDirSig then
          begin
            fEOCOffset := File_Size - (Size - i);
            pEOC := PZipEndOfCentral(@ZipBuf[i]);
            Result := 8;  // something found
            // If we have ZipComment: Save it
            AfterEOC := Size - (i + SizeOf(TZipEndOfCentral));
            clen := pEOC^.ZipCommentLen;
            if AfterEOC < clen then
              clen := AfterEOC;
            if clen > 0 then
            begin
              SetLength(fZipComment, clen);
              for j := 1 to clen do
                fZipComment[j] := ZipBuf[i + Sizeof(TZipEndOfCentral) + j - 1];
            end;
            // Check if we really are at the end of the file, if not correct the File_Size
            // and give a warning. (It should be an error but we are nice.)
            if i + SizeOf(TZipEndOfCentral) + clen <> Size then
            begin
              File_Size := File_Size + ((i + SizeOf(TZipEndOfCentral) + clen) - Size);
              // Now we need a check for WinZip Self Extractor which makes SFX files which
              // almost always have garbage at the end (Zero filled at 512 byte boundary!)
              // In this special case 'we' don't give a warning.
              // Unfortunately later versions use a different boundary so the test is invalid
              Result := EOCBadComment;  // has 'garbage'
            end;
            break;
          end;  // for
      end;
    end;
    if Result > 0 then
    begin
      MultiDisk := pEOC^.ThisDiskNo > 0; // may not have had proper sig
      DiskNr := pEOC^.ThisDiskNo;
      TotalDisks := pEOC^.ThisDiskNo + 1; //check
      TotalEntries := pEOC^.TotalEntries;
      CentralEntries := pEOC^.CentralEntries;
      CentralDiskNo := pEOC^.CentralDiskNo;
      CentralOffset := pEOC^.CentralOffset;
      CentralSize := pEOC^.CentralSize;
      if (pEOC^.TotalEntries = MAX_WORD) or (pEOC^.CentralOffset = MAX_UNSIGNED)
      or (pEOC^.CentralEntries = MAX_WORD) or (pEOC^.CentralSize = MAX_UNSIGNED)
      or (pEOC^.ThisDiskNo = MAX_WORD) or (pEOC^.CentralDiskNo = MAX_WORD) then
      begin
        Result := Result or EOCWant64;
      end;
    end;
    if Result = 0 then
      Result := -DS_NoValidZip;
    if Result > 0 then
    begin
      Result := Result and (EOCBadComment or EOCBadStruct or EOCWant64); // remove 'found' flag
    end;
  finally
    ZipBuf := nil;
    if Result < 0 then
      File_Close;
  end;
end;

// GetLastVolume
function TZMEOC.OpenLast(EOConly: boolean; OpenRes: Integer): integer;
const
  __ERR_DS_FileOpen = __UNIT__ + (531 shl 10) + DS_FileOpen;
  __ERR_DS_NoInFile1 = __UNIT__ + (568 shl 10) + DS_NoInFile;
  __ERR_DS_FileOpen1 = __UNIT__ + (583 shl 10) + DS_FileOpen;
  __ERR_DS_NoInFile2 = __UNIT__ + (592 shl 10) + DS_NoInFile;
  __ERR_DS_NoInFile3 = __UNIT__ + (615 shl 10) + DS_NoInFile;
  __ERR_DS_NoValidZip = __UNIT__ + (633 shl 10) + DS_NoValidZip;
  __ERR_DS_NoInFile4 = __UNIT__ + (652 shl 10) + DS_NoInFile;
  __ERR_DS_FileOpen2 = __UNIT__ + (663 shl 10) + DS_FileOpen;
  __ERR_DS_NoValidZip1 = __UNIT__ + (669 shl 10) + DS_NoValidZip;
var
  ext: String;
  Finding: Boolean;
  FMVolume: Boolean;
  Fname: String;
  OrigName: String;
  PartNbr: Integer;
  Path: String;
  s: String;
  sName: String;
  Stamp: Integer;
  StampTmp: Integer;
  tmpNumbering: TZipNumberScheme;
  WasNoFile: Boolean;
begin
  WasNoFile := ((-OpenRes) and MSG_ID_MASK) = DS_NoInFile;
  PartNbr := -1;
  Result := -__ERR_DS_FileOpen; // default failure
  FMVolume := False;
  OrigName := FileName; // save it
  WorkDrive.DriveStr := FileName;
  Path := ExtractFilePath(FileName);
  Numbering := znsNone; // unknown as yet
  tmpNumbering := znsNone;
  try
    WorkDrive.HasMedia(False); // check valid drive
    if WasNoFile then
    begin
      // file did not exist but no EOC, maybe it is a multi volume
      FMVolume := True;
      // get the 'base' name for numbered names
      ext := UpperCase(ExtractFileExt(FileName));
      // remove extension
      Fname := Copy(FileName, 1, Length(FileName) - Length(ext));
      // if no file exists on harddisk then only Multi volume parts are possible
      if WorkDrive.DriveIsFixed then
      begin
        // filename is of type ArchiveXXX.zip
        // MV files are series with consecutive partnbrs in filename,
        // highest number has EOC
        if ext = EXT_ZIP then
        begin
          Finding := True;
          Stamp := -1;
          while Finding and (PartNbr < 1000) do
          begin
            if KeepAlive then
              exit; // cancelled
            // add part number and extension to base name
            s := Fname + Copy(IntToStr(1002 + PartNbr), 2, 3) + EXT_ZIPL;
            StampTmp := Integer(File_Age(s));
            if (StampTmp = -1) or ((Stamp <> -1) and (StampTmp <> Stamp)) then
            begin
              // not found or stamp does not match
              Result := -__ERR_DS_NoInFile1;
              exit;
            end;
            if (PartNbr = -1) and not (spAnyTime in SpanOptions) then
              Stamp := StampTmp;
            Inc(PartNbr);
            FileName := s;
            Result := OpenEOC(EOConly);
            if Result >= 0 then
            begin // found possible last part
              Finding := False;
              if (TotalDisks - 1) <> PartNbr then
              begin
                // was not last disk
                File_Close; // should happen in 'finally'
                Result := -__ERR_DS_FileOpen1;
                exit;
              end;
              Numbering := znsName;
            end;
          end; // while
        end; // if Ext = '.zip'
        if not IsOpen then
        begin
          Result := -__ERR_DS_NoInFile2;
          exit; // not found
        end;
        // should be the same as s
        FileName := Fname + Copy(IntToStr(1001 + PartNbr), 2, 3) + EXT_ZIPL;
        // check if filename.z01 exists then it is part of MV with compat names
        // and cannot be used
        if (FileExists(ChangeFileExt(FileName, '.z01'))) then
        begin
          // ambiguous - cannot be used
          File_Close; // should happen in 'finally'
          exit; // will return DS_FileOpen
        end;
      end // if WorkDrive.Fixed
      else
      begin
        // do we have an MV archive copied to a removable disk
        // accept any MV filename on disk - then we ask for last part
        sName := NameOfPart(Fname, False);
        if sName = '' then
          sName := NameOfPart(Fname, True);
        if sName = '' then // none
        begin
          Result := -__ERR_DS_NoInFile3;   // no file with likely name
          exit;
        end;
        FileName := Path + sName;
      end;
    end; // if not exists
    // zip file exists or we got an acceptable part in multivolume or split
    // archive
    // use class variable for other functions
    while not IsOpen do    // only open if found last part on hd
    begin
      // does this part contains the central dir
      Result := OpenEOC(EOConly); // don't load on success
      if Result >= 0 then
        break; // found a 'last' disk
      if WorkDrive.DriveIsFixed then
      begin
        if (not FMVolume) and (((-Result) and MSG_ID_MASK) <> DS_FileOpen) then
          Result := -__ERR_DS_NoValidZip;
        break; // file with EOC is not on fixed disk
      end;
      // it is not the disk with central dir so ask for the last disk
      NewDisk := True; // new last disk
      DiskNr := -1; // read operation
      CheckForDisk(False, False); // does the request for new disk
      if FMVolume then
      begin // we have removable disks with multi volume archives
        // get the file name on this disk
        tmpNumbering := znsName;   // only if part and last part inserted
        sName := NameOfPart(Fname, False);
        if sName = '' then
        begin
          sName := NameOfPart(Fname, True);
          tmpNumbering := znsExt;  // only if last part inserted
        end;
        if sName = '' then // none
        begin
          Result := -__ERR_DS_NoInFile4;   // no file with likely name
          FMVolume := False;
          break;
        end;
        FileName := Path + sName;
      end;
    end; // while
    if FMVolume then
    // got a multi volume part so we need more checks
    begin // is this first file of a multi-part
      if (Sig <> zfsMulti) and ((TotalDisks = 1) and (PartNbr >= 0)) then
        Result := -__ERR_DS_FileOpen2   // check
      else
      // part and EOC equal?
        if WorkDrive.DriveIsFixed and (TotalDisks <> (PartNbr + 1)) then
      begin
        File_Close; // should happen in 'finally'
        Result := -__ERR_DS_NoValidZip1;
      end;
    end;
  finally
    if Result < 0 then
    begin
      File_Close; // close filehandle if OpenLast
      FileName := ''; // don't use the file
    end //;
    else
      if (Numbering <> znsVolume) and (tmpNumbering <> znsNone) then
        Numbering := tmpNumbering;
  end;
end;


procedure TZMEOC.SetZipComment(const Value: AnsiString);
begin
  fZipComment := Value;
end;

procedure TZMEOC.SetZipCommentLen(const Value: Integer);
var
  c: AnsiString;
begin
  if (Value <> ZipCommentLen) and (Value < Length(ZipComment))then
  begin
    c := ZipComment;
    SetLength(c, Value);
    ZipComment := c;
  end;
end;

// returns >0 ok = bytes written, <0 -ErrNo
function TZMEOC.WriteEOC: Integer;
const
  __ERR_DS_EOCBadWrite = __UNIT__ + (803 shl 10) + DS_EOCBadWrite;
  __ERR_DS_EOCBadWrite1 = __UNIT__ + (807 shl 10) + DS_EOCBadWrite;
  __ERR_DS_EOCBadWrite2 = __UNIT__ + (847 shl 10) + DS_EOCBadWrite;
  __ERR_DS_EOCBadWrite3 = __UNIT__ + (850 shl 10) + DS_EOCBadWrite;
type
  TEOCrecs = packed record
    loc: TZip64EOCLocator;
    eoc: TZipEndOfCentral;
  end;
  pEOCrecs = ^TEOCrecs;
var
  t: Integer;
  er: array of byte;
  erz: Integer;
  peoc: pEOCrecs;
  eoc64: TZipEOC64;
  Need64: Boolean;
  clen: Integer;
begin
  Result := -1;//-__ERR_DS_EOCBadWrite;  // keeps compiler happy
  TotalDisks := DiskNr + 1; //check
  Need64 := false;
  ZeroMemory(@eoc64, sizeof(eoc64));
  clen := Length(ZipComment);
  ASSERT(clen = ZipCommentLen, ' ZipComment length error');
  erz := sizeof(TEOCrecs) + clen;
  SetLength(er, erz + 1);
  peoc := @er[0];
  peoc^.eoc.HeaderSig := EndCentralDirSig;
  peoc^.loc.LocSig := EOC64LocatorSig;
  if clen > 0 then
    Move(ZipComment[1], er[sizeof(TEOCrecs)], clen);
  peoc^.eoc.ZipCommentLen := clen;
  // check Zip64 needed
  if TotalDisks > MAX_WORD then
  begin
    peoc^.eoc.ThisDiskNo := Word(-1);
    Need64 := true;
  end
  else
    peoc^.eoc.ThisDiskNo := Word(TotalDisks -1); //check

  if CentralDiskNo >= MAX_WORD then
  begin
    peoc^.eoc.CentralDiskNo := Word(-1);
    Need64 := true;
  end
  else
    peoc^.eoc.CentralDiskNo := CentralDiskNo;

  if TotalEntries >= MAX_WORD then
  begin
    peoc^.eoc.TotalEntries := Word(-1);
    Need64 := true;
  end
  else
    peoc^.eoc.TotalEntries := Word(TotalEntries);

  if CentralEntries >= MAX_WORD then
  begin
    peoc^.eoc.CentralEntries := Word(-1);
    Need64 := true;
  end
  else
    peoc^.eoc.CentralEntries := Word(CentralEntries);

  if CentralSize >= MAX_UNSIGNED then
  begin
    peoc^.eoc.CentralSize := Cardinal(-1);
    Need64 := true;
  end
  else
    peoc^.eoc.CentralSize := CentralSize;

  if (CentralOffset >= MAX_UNSIGNED) then
  begin
    peoc^.eoc.CentralOffset := Cardinal(-1);
    Need64 := true;
  end
  else
    peoc^.eoc.CentralOffset := CentralOffset;

  if not Need64 then
  begin
    // write 'normal' EOC
    erz := erz - sizeof(TZip64EOCLocator); // must not split
    Result := Write(er[sizeof(TZip64EOCLocator)],-(erz or MustFitFlag));
    if Result <> erz then
    begin
      if Result = MustFitError then
      begin
        if DiskNr >= MAX_WORD then
          Need64 := true
        else
        begin
          peoc^.eoc.ThisDiskNo := Word(DiskNr);
          Result := Write(er[sizeof(TZip64EOCLocator)],-(erz or MustFitFlag));
          if Result <> erz then
            Result := -__ERR_DS_EOCBadWrite;
        end;
      end
      else
        Result := -__ERR_DS_EOCBadWrite1;
    end;
  end;

  if Need64 then
  begin
    Z64 := true;
    eoc64.EOC64Sig := EndCentral64Sig;
    eoc64.vsize := sizeof(eoc64) - 12;
    eoc64.VersionMade := ZIP64_VER;
    eoc64.VersionNeed := ZIP64_VER;
    eoc64.ThisDiskNo := DiskNr;
    eoc64.CentralDiskNo := CentralDiskNo;
    eoc64.TotalEntries := TotalEntries;
    eoc64.CentralEntries := CentralEntries;
    eoc64.CentralSize := CentralSize;
    eoc64.CentralOffset := CentralOffset;
    peoc^.loc.EOC64RelOfs := Position;
    peoc^.loc.EOC64DiskStt := DiskNr;
    Result := Write(eoc64, -sizeof(TZipEOC64));
    if Result = sizeof(TZipEOC64) then
    begin
      peoc^.loc.NumberDisks := DiskNr + 1;   // may be new disk
      if DiskNr >= MAX_WORD then
        peoc^.eoc.ThisDiskNo := MAX_WORD
      else
        peoc^.eoc.ThisDiskNo := Word(DiskNr);// + 1);
      Result := sizeof(TZipEndOfCentral) + peoc^.eoc.ZipCommentLen; // if it works
      t := Write(er[0],-(erz or MustFitFlag));
      if t <> erz then
      begin
        if t = MustFitError then
        begin
          peoc^.loc.NumberDisks := DiskNr + 1;
          if DiskNr >= MAX_WORD then
            peoc^.eoc.ThisDiskNo := MAX_WORD
          else
            peoc^.eoc.ThisDiskNo := Word(DiskNr);// + 1);
          t := Write(er[0], -erz);
          if t <> erz then
            Result := -__ERR_DS_EOCBadWrite2;
        end
        else
          Result := -__ERR_DS_EOCBadWrite3;
      end;
    end;
  end;
end;

end.

