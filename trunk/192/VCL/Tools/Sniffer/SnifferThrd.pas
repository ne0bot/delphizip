unit SnifferThrd;
(* ***************************************************************************
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.
 Copyright (C) 2013, 2014  by Russell J. Peters, Roger Aelbrecht.

   This file is part of TZipMaster Version 1.9.

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
//modified 2013-03-02
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

interface

uses
  Classes, Contnrs, GridWin3, SnifferMain2, SnifferUtil;

const
  DZUNITMAX = 43;
  ZMUNITMAX = 31;
  DZUnitFlag = $100;
  UnitMask = $FF;

type
  TThrdUpdater = class(TThread)
  private
    Fields: TFields;
    GridData: TGData;
    TheData: PZMCryData;
    TheForm: TZipSniffer;
    TheQueue: TQueue;
    procedure ProcessData;
    procedure SendMsg; overload;
  protected
    procedure Execute; override;
    procedure FetchData;
  public
    constructor Create(MyForm: TZipSniffer; Que: TQueue);
    procedure BeforeDestruction; override;
  end;

implementation

uses
  Windows, SysUtils, ZMMsg;

constructor TThrdUpdater.Create(MyForm: TZipSniffer; Que: TQueue);
begin
  inherited Create{$ifndef UNICODE}(False){$endif};
  TheForm := MyForm;
  TheQueue := Que;
end;

procedure TThrdUpdater.BeforeDestruction;
begin
  inherited;
end;

procedure TThrdUpdater.Execute;
var
  I: Integer;
begin
  I := 1000;
  Synchronize(FetchData);
  while TheData <> nil do
  begin
    ProcessData;
    FreeMem(TheData);
    Dec(I);
    if I < 1 then
      break;
    Synchronize(FetchData);
  end;
end;

procedure TThrdUpdater.FetchData;
begin
  TheData := nil;
  if TheQueue.Count > 0 then
    TheData := PZMCryData(TheQueue.Pop);
end;

procedure TThrdUpdater.ProcessData;
var
  DatSize: DWord;
  Err: Cardinal;
  fno: Integer;
  lno: Integer;
  Msg: string;
  MWidth: Integer;
  s: string;
  SL: TStringList;
  I: Integer;
  P: PAnsiChar;
  RawLen: Integer;
  UNo: Integer;
begin
    Err := TheData.Err;
    MWidth := TheData.Flags and 3;
    DatSize := TheData.Size;
    if DatSize >= SizeOf(TZMCryData) then
    begin
      RawLen := DatSize - SizeOf(TZMCryData);
      if RawLen > 0 then
      begin
        P := PAnsiChar(@TheData^.MBuf);
        if MWidth > 1 then
          Msg := string(PChar(P))
        else
        begin
          if MWidth = 1 then
            Msg := UTF8ToStr(P, RawLen)
          else
            Msg := String(P);
        end;
      end;
    end//;
    else
      exit;
    lno := 0;
    UNo := 0;
  //  fno := 0;
    s := '';
    if Integer(err) < 0 then
      err := 0 - err;
    if Err > $1FF then
    begin
      if (err and $40000000) <> 0 then
      begin
        // dll error
        fno := (err shr 24) and $3F;
        lno := (err shr 12) and $FFF;
        err := Err and $3F;
        if (fno > 0) and (fno <= DZUNITMAX) then
          UNo := fno or DZUnitFlag;
      end
      else
      begin
        // component error
        fno := ((err shr ZERR_UNIT_SHIFTS) and ZERR_UNIT_MASK_SHIFTED);
        lno := (err shr ZERR_LINE_SHIFTS) and ZERR_LINE_MASK_SHIFTED;
        err := Err and ZERR_ERROR_MASK;
        if (fno > 0) and (fno <= ZMUNITMAX) then
          UNo := fno;
      end;
      if fno <> 0 then
        UNo := fno;
    end;
    GridData.Tick := TheData.Ticks;
    GridData.SniffNo := TheData.SniffNo;
    GridData.UnitNo := UNo;
    GridData.LineNo := lno;
    GridData.ErrNo := err;
    Fields[0] := IntToStr(TheData.Ticks);
    Fields[1] := IntToHex(TheData.SniffNo, 4);
    Fields[2] := s;
    Fields[3] := IntToStr(lno);
    Fields[4] := IntToStr(Err);
    SL := TStringList.Create;
    try
      SL.Text := Msg;
      for I := 0 to SL.Count - 1 do
      begin
        GridData.Msg := SL[I];
        Fields[5] := SL[I];
        SendMsg;
      end;
    finally
      SL.Free;
    end;
end;

procedure TThrdUpdater.SendMsg;
var
  MyForm: TZipSniffer;
begin
  MyForm := TheForm as TZipSniffer;
  if MyForm.RecordState then
    MyForm.SendToBuffer(Fields);
  if MyForm.PlayState then
  begin
    MyForm.AddGridData(GridData);
  end;
end;

end.
