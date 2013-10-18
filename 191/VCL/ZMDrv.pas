unit ZMDrv;
 
//  ZMDrv.pas - drive details

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
//modified 2013-10-18
{$INCLUDE   '.\ZipVers.inc'}

interface

uses
  Classes, Windows;

type
   //1 Provides details of drive
   TZMWorkDrive = class(TObject)
   private
    fDiskName: string;
    fDiskReadOnly: Boolean;
    fDiskSerial: DWORD;//cardinal;
    FDriveIsFloppy: Boolean;
    fDriveLetter: Char;
    fDriveStr: string;
    fDriveType: Integer;
    fLastDrive: string;
    fVolumeFreeClusters: DWORD;
    fVolumeSecSize: Cardinal;
    fVolumeSectorsPerCluster: DWORD;
    fVolumeSize: Int64;
    fVolumeSpace: Int64;
    fVolumeTotalClusters: DWORD;
    function GetDriveIsFixed: Boolean;
    procedure SetDrive(const path: string);
    procedure SetDriveStr(const Value: string);
    procedure SetExSizes(fields: Integer);
   public
    constructor Create;
    procedure AfterConstruction; override;
    procedure AssignFrom(const src: TZMWorkDrive);
    procedure Clear;
    function HasMedia(UnformOk: boolean): Boolean;
    function RenameDisk(const NewName: string): Boolean;
    procedure VolumeRefresh;
    property DiskName: string read fDiskName;
    property DiskReadOnly: Boolean read fDiskReadOnly;
    property DiskSerial: cardinal read fDiskSerial;
    property DriveIsFixed: Boolean read GetDriveIsFixed;
    property DriveIsFloppy: Boolean read FDriveIsFloppy;
    property DriveLetter: Char read fDriveLetter;
    property DriveStr: string read fDriveStr write SetDriveStr;
    property DriveType: Integer read fDriveType;
    property VolumeFreeClusters: DWORD read fVolumeFreeClusters;
    property VolumeSecSize: Cardinal read fVolumeSecSize;
    property VolumeSectorsPerCluster: DWORD read fVolumeSectorsPerCluster;
    property VolumeSize: Int64 read fVolumeSize;
    property VolumeSpace: Int64 read fVolumeSpace;
    property VolumeTotalClusters: DWORD read fVolumeTotalClusters;
   end;

implementation

uses
  SysUtils, ZMXcpt, ZMMsg;

const
  __UNIT__ = 13 shl 23;

Const
  MAX_REMOVABLE = 10 * 1024 * 1024;

constructor TZMWorkDrive.Create;
begin
  inherited;
end;

procedure TZMWorkDrive.AfterConstruction;
begin
  inherited;
  Clear;
end;

procedure TZMWorkDrive.AssignFrom(const src: TZMWorkDrive);
begin
  if (self <> src) then
  begin
    fDiskName := src.DiskName;
    fDiskReadOnly := src.DiskReadOnly;
    fDiskSerial := src.DiskSerial;
    fDriveIsFloppy := src.DriveIsFloppy;
    fDriveLetter := src.DriveLetter;
    fDriveStr := src.DriveStr;
    fDriveType := src.DriveType;
    fVolumeFreeClusters := src.VolumeFreeClusters;
    fVolumeSecSize := src.VolumeSecSize;
    fVolumeSectorsPerCluster := src.VolumeSectorsPerCluster;
    fVolumeSize := src.VolumeSize;
    fVolumeSpace := src.VolumeSpace;
    fVolumeTotalClusters := src.VolumeTotalClusters;
  end;
end;

procedure TZMWorkDrive.Clear;
begin
  fDiskName := '';
  fLastDrive := '';
  fDiskReadOnly := false;
  fDiskSerial := 0;
  fDriveIsFloppy := False;
  fDriveLetter := #0;
  fDriveStr := '';
  fDriveType := 0;
  fVolumeSecSize := 512; 
  fVolumeSectorsPerCluster := 4;
  fVolumeSize := 0;
  fVolumeSpace := 0;    
  fVolumeTotalClusters := 0;
end;

function TZMWorkDrive.GetDriveIsFixed: Boolean;
begin
  Result := not DriveIsFloppy;
end;

function TZMWorkDrive.HasMedia(UnformOk: boolean): Boolean;
const
  __ERR_DS_NotaDrive = __UNIT__ + (190 shl 10) + DS_NotaDrive;
  __ERR_DS_DriveNoMount = __UNIT__ + (195 shl 10) + DS_DriveNoMount;
//const
//  _FILE_READ_ONLY_VOLUME = $00080000;
var
  Bits: set of 0..25;
  err: cardinal;
  NamLen: Cardinal;
  Num: Integer;
  OldErrMode: DWord;
  SysFlags: DWord;
  SysLen: DWord;
  VolNameAry: array[0..255] of Char;
begin
  NamLen := 255;
  SysLen := 255;
  fVolumeSize := 0;
  fVolumeSpace := 0;
  fDiskName := '';
  fDiskSerial := 0;
  VolNameAry[0] := #0;
  Result := False;

  if DriveLetter <> #0 then                // Only for local drives
  begin
    if (DriveLetter < 'A') or (DriveLetter > 'Z') then
      raise EZipMaster.CreateMsgStr(__ERR_DS_NotaDrive, DriveStr);

    Integer(Bits) := GetLogicalDrives();
    Num := Ord(DriveLetter) - Ord('A');
    if not (Num in Bits) then
      raise EZipMaster.CreateMsgStr(__ERR_DS_DriveNoMount, DriveStr);
  end;

  OldErrMode := SetErrorMode(SEM_FAILCRITICALERRORS); // Turn off critical errors:

  // Since v1.52c no exception will be raised here; moved to List() itself.
  // 1.72 only get Volume label for removable drives
  if (not GetVolumeInformation(Pchar(DriveStr), VolNameAry,
    NamLen, PDWORD(@fDiskSerial), SysLen, SysFlags, Nil, 0)) then
  begin
    // W'll get this if there is a disk but it is not or wrong formatted
    // so this disk can only be used when we also want formatting.
    err := GetLastError();
    if (err = 31) and (UnformOk) then
      Result := True;
  end//;
  else
  begin
    fDiskName := VolNameAry;
    fDiskReadOnly := false;
    { get free disk space and size. }
    SetExSizes(7);      // RCV150199
  end;

  SetErrorMode(OldErrMode);   // Restore critical errors:

  // -1 is not very likely to happen since GetVolumeInformation catches errors.
  // But on W95(+OSR1) and a UNC filename w'll get also -1, this would prevent
  // opening the file. !!!Potential error while using spanning with a UNC filename!!!
  if (DriveLetter = #0) or ((DriveLetter <> #0) and
    (VolumeSize <> -1)) then
    Result := True;
end;

function TZMWorkDrive.RenameDisk(const NewName: string): Boolean;
begin
  Result := false;
  if DriveIsFloppy and HasMedia(false) and not DiskReadOnly and
    SetVolumeLabel(PChar(DriveStr), PChar(NewName)) then
  begin
    HasMedia(false);  // get new name
    Result := True;
  end;
end;

procedure TZMWorkDrive.SetDrive(const path: string);
var
  s: string;
begin
  s := Uppercase(ExtractFileDrive(ExpandUNCFileName(path)) + '\');
  if s <> fLastDrive then
  begin
    Clear;
    if (length(s) = 3) and (s[2] = ':') then
    begin
      // a local drive
      fDriveLetter := s[1];
      fDriveType := GetDriveType(Pchar(s));
      if DriveType = DRIVE_REMOVABLE then
      begin
        if (DriveLetter = 'A') or (DriveLetter = 'B') then
          fDriveIsFloppy := True;
      end;
    end
    else
      Clear;
    fLastDrive := s;
    fDriveStr := s;
  end;
end;

procedure TZMWorkDrive.SetDriveStr(const Value: string);
begin
  if Value <> fDriveStr then
    SetDrive(Value);
end;

procedure TZMWorkDrive.SetExSizes(fields: Integer);
var
  BytesPSector: DWORD;
  LDiskFree: Int64;
  LSizeOfDisk: Int64;
  SSize: cardinal;
begin
  LDiskFree := -1;
  LSizeOfDisk := -1;
  SSize := 0;
  if GetDiskFreeSpace(Pchar(DriveStr), fVolumeSectorsPerCluster, BytesPSector,
    fVolumeFreeClusters, fVolumeTotalClusters) then
  begin
    SSize := BytesPSector;
  end;
  if not GetDiskFreeSpaceEx(Pchar(DriveStr), LDiskFree, LSizeOfDisk, Nil) then
  begin
    LDiskFree := -1;
    LSizeOfDisk := -1;
    if SSize <> 0 then
    begin
      LDiskFree := Int64(BytesPSector) * VolumeSectorsPerCluster *
        VolumeFreeClusters;
      LSizeOfDisk := Int64(BytesPSector) * VolumeSectorsPerCluster *
        VolumeTotalClusters;
    end;
  end;
  if (fields and 1) <> 0 then
    fVolumeSpace := LDiskFree;
  if (fields and 2) <> 0 then
    fVolumeSize := LSizeOfDisk;
  if (fields and 4) <> 0 then
    fVolumeSecSize := SSize;
end;

procedure TZMWorkDrive.VolumeRefresh;
begin
  SetExSizes(7);
end;

end.

