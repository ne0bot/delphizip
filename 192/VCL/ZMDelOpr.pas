unit ZMDelOpr;

//  ZMDelOpr.pas -  Delete operations

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
//modified 2013-04-17

{$I   '.\ZipVers.inc'}

interface

uses
  {$IFDEF VERDXE2up}
    System.Classes, System.SysUtils, WinApi.Windows, VCL.Graphics,
  {$ELSE}
    SysUtils, Windows, Classes, Graphics,
  {$ENDIF}

  ZipMstr, ZMCompat, ZMLister, ZMBody, ZMModOpr, ZMMisc,
   ZMArgSplit, ZMZipBase, ZMZipReader, ZMZipWriter, ZMEntryReader;
// ------------------------------------------------------------------------

type
  TZMDelOpr = class(TZMBaseOpr)
  private
    FSkippedFiles: TStringList;
    FSplitter: TZMArgSplitter;
    FZipList: TZMZipList;
    function DeleteSelectedFiles(Zip: TZMZipReader): Integer;
    function ProcessDelete(SrcZip: TZMZipReader; const Arg, Excludes: string):
        Integer;
    function ProcessDeleteList(var SelectCount: Integer): Integer;
    function Delete1: Integer;
    function DeleteByList: Integer;
    function FlattenExcludes(Excludes: TStrings): string;
  protected
    procedure CreateInterimZip; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Delete: Integer;
  end;

implementation

uses
  {$IFDEF VERDXE2up}
    VCL.Dialogs, VCL.Controls,
  {$ELSE}
    Dialogs, Controls,
  {$ENDIF}
  ZMStructs, ZMDelZip, ZMXcpt, ZMUtils, ZMDlg, ZMCtx,
  ZMMsg, ZMDrv, ZMMatch, ZMWFuncs;

const
  __UNIT__ = 6;

const
  DeleteIncludeListThreshold = 20;
  MergeIncludeListThreshold  = 10;
  PrepareAppendThreshold     = 10;

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

procedure TZMDelOpr.AfterConstruction;
begin
  inherited;
  FSkippedFiles := nil;
  FZipList := TZMZipList.Create;
  FSkippedFiles := TStringList.Create;
  FSplitter := TZMArgSplitter.Create;
end;

procedure TZMDelOpr.BeforeDestruction;
begin
  FSkippedFiles.Free;
  FZipList.Free;
  FSplitter.Free;
  inherited;
end;

procedure TZMDelOpr.CreateInterimZip;
begin
  InterimZip := TZMZipCopier.Create(Lister);
  if not InterimZip.File_CreateTemp(PRE_INTER, '') then
    raise EZipMaster.CreateMsgDisp(ZM_Error(128, ZS_NoOutFile), true);
end;

function TZMDelOpr.Delete: Integer;
begin
  if Reporter.Verbosity > zvTrace then
    Reporter.Logger.LogSpecs('');
  if ZipFileName <> '' then
  begin
    // do it the old way
    if (Current.Count < 1) or (IncludeSpecs.Count = 0) then
      Result := -ZM_Error(139, ZS_NothingToDel)
    else
      Result := Delete1;
    // Update the Zip Directory by calling List method
    // for spanned exe avoid swapping to last disk
//    if ((-Result) and MSG_ID_MASK) <> ZS_NothingToDel then
    if AbsErr(Result) <> ZS_NothingToDel then
      Reload := zlrReload;
  end
  else
    Result := DeleteByList;
  if Result < 0 then
    Reporter.ShowZipMessage(-Result, '')
  else
    SuccessCnt := Result;
  if Reporter.Verbosity > zvTrace then
    Reporter.Logger.LogSpecs('');
end;

(* ? TZMModOpr.Delete1
  Deletes files specified in FSpecArgs from current Zip
  exit: FSpecArgs = files deleted,
  FSpecArgsExcl = files skipped
  Result = >=0 number of files deleted, <0 error
*)
function TZMDelOpr.Delete1: Integer;
var
  CurZip: TZMZipReader;
  DelCnt: Integer;
  DropCount: Integer;
  I: Integer;
  Rec: TZMEntryBase;
  SkippedFiles: TStringList;
begin
  CurZip := CurrentZip(True, false); // prepare the Current zip
  PrepareZip(CurZip);
  Result := 0;
  SkippedFiles := TStringList.Create;
  try
    DelCnt := CurZip.SelectFiles(IncludeSpecs, ExcludeSpecs, SkippedFiles);
    Body.ClearIncludeSpecs; // will contain files deleted
    Body.ClearExcludeSpecs; // will contain files skipped
    for I := 0 to SkippedFiles.Count - 1 do
    begin
      if Reporter.ReportSkippingEx(SkippedFiles[I], stNotFound, 0, ExcludeSpecs) then
        Result := -ZM_Error(183, ZS_NoSkipping);
    end;
  finally
    SkippedFiles.Free;
  end;
  if (Result = 0) and (DelCnt <= 0) then
    Result := -ZM_Error(189, ZS_NothingToDel);
  if Result = 0 then
  begin
    ASSERT(DelCnt = CurZip.SelCount, 'selcount wrong 1');
    if (CurZip.Count - DelCnt) < 1 then
    begin
      // no files left
      CurZip.File_Close;
      File_Delete(CurZip.ArchiveName);
      Result := DelCnt; // number of files deleted
    end
    else
    begin
      PrepareInterimZip;
      DropCount := 0;
      rec := CurZip.FirstRec;
      while rec <> nil do
      begin
        // Copy nonselected entries to IntermedZip
        if Rec.StatusBit[zsbSelected] <> zsbSelected then
        begin
          if TZMZipCopier(InterimZip).AffixZippedFile(Rec) = nil then
          begin
            Result := -ZM_Error(212, ZS_DuplFileName);
            break;
          end;
        end
        else
        begin
          IncludeSpecs.Add(Rec.FileName); // 15/04/2013 1:18:34 PM
          Inc(DropCount);
        end;
        rec := Rec.Next;
        CheckCancel;
      end;
      if (Result = 0) and (DropCount > 0) then
      begin
        // write results
        FinalizeInterimZip(CurZip);
        Result := DropCount;
      end;
    end;
  end;
  CurZip.Invalidate;
  Current := nil; // force reload
end;

function TZMDelOpr.DeleteByList: Integer;
var
  I: Integer;
  SelectCount: Integer;
  ShowXProgress: Boolean;
  Zip: TZMZipReader;
begin
  ShowProgress := zspFull;
  if IncludeSpecs.Count < 1 then
    raise EZipMaster.CreateMsgDisp(ZM_Error(242, ZS_NothingToDo), true);
  FZipList.Clear;
  FSkippedFiles.Clear;
  FZipList.ProtectZero := false;
  // add zips to list and select their files
  Result := ProcessDeleteList(SelectCount);
  if Result < 0 then
    raise EZipMaster.CreateMsgDisp(-Result, true);
  if SelectCount < 1 then
  begin
    Reporter.Inform('nothing selected');
    Result := -ZM_Error(253, ZS_NothingToDo);
    Reporter.ShowZipMessage(Result, '');
    exit;
  end;
  Body.ClearIncludeSpecs; // will contain files deleted
  Body.ClearExcludeSpecs; // will contain files skipped
  ShowXProgress := FZipList.Count > MergeIncludeListThreshold;
  if ShowXProgress then
    Progress.NewXtraItem(zxProcessing, FZipList.Count);
  for I := 0 to FZipList.Count - 1 do
  begin
    if ShowXProgress then
      Progress.AdvanceXtra(1);
    CheckCancel;
    Zip := FZipList[I];
    Result := DeleteSelectedFiles(Zip);
  end;
end;

function TZMDelOpr.DeleteSelectedFiles(Zip: TZMZipReader): Integer;
var
  BeforeCnt: Integer;
  DelCnt: Integer;
  Rec: TZMEntryBase;
begin
  Result := PrepareZip(Zip); // prepare the Current zip
  DelCnt := Zip.SelCount;
  if (Result = 0) and (DelCnt <= 0) then
    Result := -ZM_Error(281, ZS_NothingToDel);
  if Result = 0 then
  begin
    if (Zip.Count - Zip.SelCount) < 1 then
    begin
      // no files left
      Zip.File_Close;
      File_Delete(Zip.ArchiveName);
      Result := DelCnt; // number of files deleted
    end
    else
    begin
      Rec := Zip.FirstSelected; // from beginning
      while Rec <> nil do
      begin
        IncludeSpecs.Add(Rec.FileName); // we are deleting it
        Rec := Zip.NextSelected(Rec);
      end;
      BeforeCnt := Zip.Count;
      Zip.Select('*', zzsToggle); // write entries not selected for deletion
      ASSERT((Zip.Count - DelCnt) = Zip.SelCount, 'selcount wrong 2');
      Result := Remake(Zip, Zip.Count - DelCnt, false); // write the result
      if Result >= 0 then
        Result := BeforeCnt - Result; // if no error
    end;
  end;
end;

function TZMDelOpr.FlattenExcludes(Excludes: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  // flatten the list
  for I := 0 to Excludes.Count - 1 do
  begin
    if Excludes[I] = '' then
      Continue;
    if Result <> '' then
      Result := Result + SPEC_SEP;;
    Result := Result + Excludes[I];
  end;
end;

function TZMDelOpr.ProcessDelete(SrcZip: TZMZipReader; const Arg, Excludes:
    string): Integer;
begin
  if Reporter.Verbosity > zvVerbose then
    Reporter.Trace('Selecting: ' + Arg + ' in: ' + SrcZip.ArchiveName, ZM_Error(329, 0));
  Result := SrcZip.SelectFile(Arg, Excludes, zzsSet);
  if Result < 1 then
  begin
    // none found
    if Reporter.ReportSkippingEx(QualifiedName(SrcZip.ArchiveName, Arg), stNotFound, 0,
      FSkippedFiles) then
      Result := -ZM_Error(336, ZS_NoSkipping);
  end;
end;

// returns <0 _ error
(*
  switches
  >>spec  before any source zip is specified sets the default select spec
  >>spec  select files in current zip according to spec
  /E:[|][spec[|spec]...]   set excludes, if starts with | it appends to globals otherwise use spec
  /E  same as /E:|    (use globals)
  changes to excludes occur at current line and continue until changed - does not change already selected files.
  when used on same line as '>>' it only applies to that line and modifies the 'current' excludes.
*)
function TZMDelOpr.ProcessDeleteList(var SelectCount: Integer): Integer;
var
  DefaultArg: TZMString;
  EffectiveExcludes: TZMString;
  GlobalExcludes: TZMString;
  I: Integer;
  LocalArg: string;
  LocalExcludes: string;
  ShowXProgress: Boolean;
  Skip: TZMSkipTypes;
  SourceName: string;
  xc: string;
  Zip: TZMZipReader;
  ZipsIndex: Integer;
begin
  Result := 0;
  SelectCount := 0;
  GlobalExcludes := FlattenExcludes(ExcludeSpecs);
  EffectiveExcludes := GlobalExcludes;
  FSplitter.Allow := '>E';
  FSplitter.Options := [zaoWildSpec];
  DefaultArg := '';
  // locate source zips and their files
  SourceName := '';
  ShowXProgress := IncludeSpecs.Count > DeleteIncludeListThreshold;
  if ShowXProgress then
    Progress.NewXtraItem(zxProcessing, IncludeSpecs.Count);
  ZipsIndex := -ZM_Error(377, ZS_NoInFile); // no current yet
  for I := 0 to IncludeSpecs.Count - 1 do
  begin
    if ShowXProgress then
      Progress.AdvanceXtra(1);
    CheckCancel;
    if Result < 0 then
      break;
    Result := 0;
    FSplitter.Raw := IncludeSpecs[I];
    if FSplitter.Error <> zasNone then
      raise EZipMaster.CreateMsgStr(ZM_Error(388, ZS_InvalidParameter), FSplitter.Raw);
    if FSplitter.Main <> '' then
    begin
//      SourceName := DriveFolders.MakeFullPath(FSplitter.Main);
      Result := DriveFolders.ExpandPath(SourceName, FSplitter.Main);
      if Result < 0 then
        break;
      ZipsIndex := FZipList.Find(SourceName);
      if ZipsIndex < 0 then
      begin
        // have not opened file yet
        if FileExists(SourceName) then
        begin
          Zip := TZMZipReader.Create(Lister);
          Zip.ArchiveName := SourceName;
          ZipsIndex := Zip.Open(false, false);
          if ZipsIndex >= 0 then
            ZipsIndex := FZipList.Add(Zip) // add to list and return Index
          else
            Zip.Free; // dispose of it (will happen anyway)
        end;
        if ZipsIndex < 0 then
        begin
          // file does not exist or could not be opened
          Reporter.Inform(Format('Skipped missing or bad zip [%d] %s',
              [-ZipsIndex, SourceName]),ZM_Error(410, 0));
          case ((-ZipsIndex) and MSG_ID_MASK) of
            ZS_NoInFile: Skip := stNotFound;
            ZS_FileOpen: Skip := stNoOpen;
            else
              Skip := stReadError;
          end;
          if Reporter.ReportSkippingEx(SourceName, Skip, 0, FSkippedFiles) then
          begin
            Result := -ZM_Error(419, ZS_NoSkipping);
            break;
          end;
          Result := 0; // we can ignore it
          Continue;
        end;
      end;
    end
    else // no zip specified
    begin
      // ignore empty lines
      if FSplitter.Found = '' then
        Continue;
      // any zips specified yet
      if ZipsIndex < 0 then
      begin
        // none yet, set defaults
        if FSplitter.Has('E') then
        begin
          xc := FSplitter.Arg('E');
          if (xc <> '') and (xc[1] = SPEC_SEP) then
            EffectiveExcludes := GlobalExcludes + xc
          else
            EffectiveExcludes := xc;
          GlobalExcludes := EffectiveExcludes;
        end;
        if FSplitter.Has(ZSPECARG) then
        begin
          DefaultArg := FSplitter.Arg(ZSPECARG);
        end;
        Continue;
      end;
    end;
    if (Result < 0) or (ZipsIndex < 0) then
      break; // must have open file
    if FSplitter.Has(ZSPECARG) or (FSplitter.Main <> '') then
    begin
      // include a spec in the current zip
      Zip := FZipList[ZipsIndex];
      LocalExcludes := EffectiveExcludes;
      // check local overrides
      if FSplitter.Has('E') then
      begin
        xc := FSplitter.Arg('E');
        if (xc <> '') and (xc[1] = SPEC_SEP) then
          LocalExcludes := EffectiveExcludes + xc
        else
          LocalExcludes := xc;
      end;
      if FSplitter.Has(ZSPECARG) then
      begin
        // include a spec in the current zip
        LocalArg := FSplitter.Arg(ZSPECARG);
        Result := ProcessDelete(Zip, LocalArg, LocalExcludes);
        if Result > 0 then
          SelectCount := SelectCount + Result;
      end;
      Continue;
    end;
    // Set effective
    if FSplitter.Has('E') then
    begin
      xc := FSplitter.Arg('E');
      if (xc <> '') and (xc[1] = SPEC_SEP) then
        EffectiveExcludes := GlobalExcludes + xc
      else
        EffectiveExcludes := xc;
    end;
  end;
end;

end.
