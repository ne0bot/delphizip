unit ZMHash;

//  ZMHash.pas - Hash list for entries

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
//modified 2011-11-19
{$I   '.\ZipVers.inc'}

interface

uses
  ZipMstr, ZMCentral;

const
  HDEBlockEntries = 511; // number of entries per block

type
  PHashedDirEntry = ^THashedDirEntry;
  THashedDirEntry = record
    Next: PHashedDirEntry;
    ZIndex: Integer;
  end;

  // for speed and efficiency allocate blocks of entries
  PHDEBlock = ^THDEBlock;
  THDEBlock = packed record
    Entries: array [0..(HDEBlockEntries -1)] of THashedDirEntry;
    Next: PHDEBlock;
  end;

  TZMDirHashList = class(TObject)
  private
    FLastBlock: PHDEBlock;
    FMyOwner: TZMCentral;
    FNextEntry: Cardinal;
    function GetEmpty: boolean;
    function GetSize: Cardinal;
    procedure SetEmpty(const Value: boolean);
    procedure SetSize(const Value: Cardinal);
  protected
    FChains: array of PHashedDirEntry;
    FBlocks: Integer;
    //1 chain of removed nodes
    FEmpties: PHashedDirEntry;
    procedure DisposeBlocks;
    function GetEntry: PHashedDirEntry;
    function Same(Entry: PHashedDirEntry; Hash: Cardinal; const Str: String):
        Boolean;
    property MyOwner: TZMCentral read FMyOwner;
  public
    constructor Create(aList: TZMCentral);
    function Add(ZIdx: Integer; AllowDuplicate: Boolean): TZMZRec;
    procedure AfterConstruction; override;
    procedure AutoSize(Req: Cardinal);
    procedure BeforeDestruction; override;
    procedure Clear;
    function Find(const FileName: String; Discarded: boolean): Integer;
    function FindNextDupName(Index: Integer): Integer;
//  //1 return true if removed
//  function Remove(Index: Integer): boolean;
    property Empty: boolean read GetEmpty write SetEmpty;
    property Size: Cardinal read GetSize write SetSize;
  end;

implementation

uses
  SysUtils, Windows, ZMMatch, ZMUtils, ZMIRec;

const
  __UNIT__ = 17 shl 23;

const
  ChainsMax = 65537;
  ChainsMin = 61;
  CapacityMin = 64;

constructor TZMDirHashList.Create(aList: TZMCentral);
begin
  inherited Create;
  FMyOwner := aList;
end;

function TZMDirHashList.Add(ZIdx: Integer; AllowDuplicate: Boolean): TZMZRec;
var
  Entry: PHashedDirEntry;
  Hash: Cardinal;
  Idx: Integer;
  Parent: PHashedDirEntry;
  Rec: TZMZRec;
  S: String;
begin
  Rec := MyOwner[ZIdx];
  Assert(Rec <> nil, 'nil ZipDirEntry');
  if FChains = nil then
    Size := 1283;
  Result := nil;
  S := Rec.FileName;
  Hash := Rec.Hash;
  Idx := Hash mod Cardinal(Length(FChains));
  Entry := FChains[Idx];
  if Entry = nil then
  begin
    Entry := GetEntry;
    Entry.ZIndex := ZIdx + 1;
    Entry.Next := nil;
    FChains[Idx] := Entry;
  end
  else
  begin
    repeat
      if Same(Entry, Hash, S) then
      begin
        Result := MyOwner[Entry.ZIndex - 1];   // duplicate name
        if not AllowDuplicate then
          exit;
      end;
      Parent := Entry;
      Entry := Entry.Next;
      if Entry = nil then
      begin
        Entry := GetEntry;
        Entry.ZIndex := 0; // empty
        Parent.Next := Entry;
      end;
    until (Entry.ZIndex <= 0);
    // we have an entry so fill in the details
    Entry.ZIndex := ZIdx + 1;
    Entry.Next := nil;
  end;
end;

procedure TZMDirHashList.AfterConstruction;
begin
  inherited;
  FBlocks := 0;
  FLastBlock := nil;
  FEmpties := nil;
  FNextEntry := HIGH(Cardinal);
end;

// set size to a reasonable prime number
procedure TZMDirHashList.AutoSize(Req: Cardinal);
const
  PrimeSizes: array[0..29] of Cardinal =
  (61, 131, 257, 389, 521, 641, 769, 1031, 1283, 1543, 2053, 2579, 3593,
   4099, 5147, 6151, 7177, 8209, 10243, 12289, 14341, 16411, 18433, 20483,
   22521, 24593, 28687, 32771, 40961, 65537);
var
  i: Integer;
begin
  if Req < 12000 then
  begin
    // use next higher size
    for i := 0 to HIGH(PrimeSizes) do
      if PrimeSizes[i] >= Req then
      begin
        Req := PrimeSizes[i];
        break;
      end;
  end
  else
  begin
    // use highest smaller size
    for i := HIGH(PrimeSizes) downto 0 do
      if PrimeSizes[i] < Req then
      begin
        Req := PrimeSizes[i];
        break;
      end;
  end;
  SetSize(Req);
end;

procedure TZMDirHashList.BeforeDestruction;
begin
  Clear;
  inherited;
end;

procedure TZMDirHashList.Clear;
begin
  DisposeBlocks;
  FChains := nil;  // empty it
end;

procedure TZMDirHashList.DisposeBlocks;
var
  TmpBlock: PHDEBlock;
begin
  while FLastBlock <> nil do
  begin
    TmpBlock := FLastBlock;
    FLastBlock := TmpBlock^.Next;
    Dispose(TmpBlock);
  end;
  FBlocks := 0;
  FLastBlock := nil;
  FEmpties := nil;
  FNextEntry := HIGH(Cardinal);
end;

function TZMDirHashList.Find(const FileName: String; Discarded: boolean):
    Integer;
var
  Entry: PHashedDirEntry;
  Hash: Cardinal;
  idx:  Cardinal;
begin
  Result := -1;//nil;
  if FChains = nil then
    exit;
  Hash := HashFuncNoCase(FileName);
  idx  := Hash mod Cardinal(Length(FChains));
  Entry := FChains[idx];
  // check entries in this chain
  while Entry <> nil do
  begin
    if Same(Entry, Hash, FileName) and
     (Discarded or (MyOwner[Entry^.ZIndex - 1].StatusBit[zsbDiscard] = 0)) then
    begin
      Result := Entry.ZIndex - 1;
      break;
    end;
//    else
      Entry := Entry.Next;
  end;
end;

function TZMDirHashList.FindNextDupName(Index: Integer): Integer;
var
  Entry: PHashedDirEntry;
  FileName: String;
  Hash: Cardinal;
  idx:  Cardinal;
begin
  Result := -1;
  if Empty or (Index >= MyOwner.Count) then
    Exit;
  Hash := MyOwner[Index].Hash;
  FileName := MyOwner[Index].FileName;
  idx  := Hash mod Cardinal(Length(FChains));
  Entry := FChains[idx];
  Inc(Index);  // adjust to internal
  // find entry in this chain
  while Entry <> nil do
  begin
    if Entry.ZIndex = Index then
      Break; // found this entry
    Entry := Entry.Next;
  end;
  if Entry = nil then
    Exit;  // not found (How?)
  Entry := Entry.Next;
  if Entry = nil then
    Exit;  // no duplicate
  // find next higher in this chain
  while Entry <> nil do
  begin
    if (Entry.ZIndex > Index) and Same(Entry, Hash, FileName) then
      break;
    Entry := Entry.Next;
  end;
  if Entry <> nil then
    Result := Entry.ZIndex - 1;
end;

function TZMDirHashList.GetEmpty: boolean;
begin
  Result := FChains = nil;
end;

// return address in allocated block
function TZMDirHashList.GetEntry: PHashedDirEntry;
var
  TmpBlock: PHDEBlock;
begin
  if FEmpties <> nil then
  begin
    Result := FEmpties;         // last emptied
    FEmpties := FEmpties.Next;
  end
  else
  begin
    if (FBlocks < 1) or (FNextEntry >= HDEBlockEntries) then
    begin
      // we need a new block
      New(TmpBlock);
      ZeroMemory(TmpBlock, sizeof(THDEBlock));
      TmpBlock^.Next := FLastBlock;
      FLastBlock := TmpBlock;
      Inc(FBlocks);
      FNextEntry := 0;
    end;
    Result := @FLastBlock^.Entries[FNextEntry];
    Inc(FNextEntry);
  end;
end;

function TZMDirHashList.GetSize: Cardinal;
begin
  Result := Length(FChains);
end;

//function TZMDirHashList.Remove(Index: Integer): boolean;
//var
//Entry: PHashedDirEntry;
////  FileName: String;
//Hash: Cardinal;
//Idx: Cardinal;
//Prev: PHashedDirEntry;
//Rec: TZMZRec;
//begin
//Result := false;
//if (Index < 0) or (FChains = nil) then
//  exit;
//Rec := MyOwner[Index];
//Hash := Rec.Hash;
//Inc(Index); // allow for offset
//Idx := Hash mod Cardinal(Length(FChains));
//Entry := FChains[Idx];
//Prev := nil;
//while Entry <> nil do
//begin
//  if Entry.ZIndex = Index then
//  begin
//    // we found it so unlink it
//    if Prev = nil then
//    begin
//      // first in chain
//      FChains[Idx] := Entry.Next; // link to next
//    end
//    else
//      Prev.Next := Entry.Next; // link to next
//    Entry.Next := FEmpties; // link to removed
//    FEmpties := Entry;
//    Entry.ZIndex := 0; // empty
//    Result := True;
//    break;
//  end
//  else
//  begin
//    Prev := Entry;
//    Entry := Entry.Next;
//  end;
//end;
//end;

function TZMDirHashList.Same(Entry: PHashedDirEntry; Hash: Cardinal; const Str:
    String): Boolean;
var
  IRec: TZMZRec;
begin
  IRec := MyOwner[Entry^.ZIndex - 1];
  Result := (Hash = IRec.Hash) and
    (FileNameComp(Str, IRec.FileName) = 0);
end;

procedure TZMDirHashList.SetEmpty(const Value: boolean);
begin
  if Value then
    Clear;
end;

procedure TZMDirHashList.SetSize(const Value: Cardinal);
var
  TableSize: Integer;
begin
  Clear;
  if Value > 0 then
  begin
    TableSize := Value;
    // keep within reasonable limits
    if TableSize < ChainsMin then
      TableSize := ChainsMin
    else
    if TableSize > ChainsMax then
      TableSize := ChainsMax;
    SetLength(FChains, TableSize);
    ZeroMemory(FChains, Size * sizeof(PHashedDirEntry));
  end;
end;

end.
