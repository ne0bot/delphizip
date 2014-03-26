unit GridWin3;
 (* ***************************************************************************
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.
 Copyright (C) 2013, 2014  by Russell J. Peters, Roger Aelbrecht.

   This file is part of TZipMaster Version 1.9.

TZipMaster VCL originally by Chris Vleghert, Eric W. Engler.
  Present Maintainers and Authors Roger Aelbrecht and Russell Peters.
Copyright (C) 1997-2002 Chris Vleghert and Eric W. Engler
Copyright (C) 1992-2008 Eric W. Engler
Copyright (C) 2009-2014 Russell Peters and Roger Aelbrecht

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
//modified 2014-03-17
{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}

interface

uses
  Windows, Classes, Controls, Messages, SysUtils, Variants,
  Graphics, Forms, Dialogs, Grids;

const
  DZUNITMAX = 43;
  DZUnitFlag = $100;
  UnitMask = $FF;

type
  TGData = record
    Tick: Cardinal;
    SniffNo: Cardinal;
    UnitNo: Cardinal;
    LineNo: Cardinal;
    ErrNo: Cardinal;
    Msg: String;
  end;

type
  TMDIGridWin = class(TForm)
    procedure FormResize(Sender: TObject);
  private
    FAutoScroll: boolean;
    FCount: Integer;
    FDataList: array of TGData;
    FDataSize: Integer;
    FIsReadOnly: Boolean;
    FMain: TForm;
    FRows: Integer;
    OldCount: Integer;
    procedure InitGrid;
    procedure LoadLine(const Value: string);
    procedure SetAutoScroll(const Value: boolean);
    procedure SetColumnWidths;
    procedure SetIsReadOnly(const Value: Boolean);
    procedure WriteText(ACanvas: TCanvas; const ARect: TRect; const Text: string;
        Align: TAlignment);
  protected
    procedure AppendDataRow(Data: TGData);
    procedure ClearList;
    function DataText(Index, ACol: Integer): String;
    function StrToUnitNo(const Field: string): Cardinal;
    function UnitText(UnitNo: Integer): string;
  public
    procedure AfterConstruction; override;
    function AppendData(Data: TGData): Integer;
    procedure BeforeDestruction; override;
    procedure CleanWindow;
    function Load(const fname: string): Integer;
    procedure UpdateGrid;
    property AutoScroll: boolean read FAutoScroll write SetAutoScroll;
    property Count: Integer read FCount;
    property IsReadOnly: Boolean read FIsReadOnly write SetIsReadOnly;
    property Main: TForm read FMain write FMain;
  published
    Grid: TDrawGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect;
        State: TGridDrawState);
  end;

implementation

{$R *.dfm}
uses
  SnifferMain2;


const
  CRLF = #13#10;

const
  DZUnitNames: array [0..DZUNITMAX-1] of string = (
    'COMMON', 'CRC32', 'CRCTAB', 'CRYPT', 'DZFRAME', 'DZOPER', 'ENTER',
    'HELPERS', 'INGMTCH', '', '', '', '', '',
    '', '', '', '', '', '', '',
    'UTIL', 'ZBITS', 'ZCRYPT', 'ZDEFLATE', 'ZIPDFLT', 'ZIPFILE', 'ZIPFIO',
    'ZIPFNC', 'ZIPMAIN', 'ZIPOP', 'ZIPPRC', 'ZIPREAD', 'ZIPSEL', 'ZIPSS',
    'ZIPUP', 'ZIPWIN32', 'ZMATCH', 'ZSTRINGS', 'ZTREES', 'DZImport', 'DZ_Strw',
    'DZRaw');

{$I units.inc}

const
  Titles: array [0..6] of string = ('', 'Tick', 'OpNo', 'Unit', 'Line', 'ErrNo', 'Message');

procedure TMDIGridWin.AfterConstruction;
begin
  inherited;
  IsReadOnly := false;
  Main := nil;
  SetLength(FDataList, 50000);
  FCount := 0;
  FDataSize := 50000;
end;

function TMDIGridWin.AppendData(Data: TGData): Integer;
begin
  Result := -1;
  if not IsReadOnly then
  begin
    AppendDataRow(Data);
    Result := Count;
  end;
end;

procedure TMDIGridWin.AppendDataRow(Data: TGData);
begin
  if Count >= FDataSize then
  begin
    FDataSize := FDataSize + 5000;
    SetLength(FDataList, FDataSize);
  end;
  FDataList[Count].Tick := Data.Tick;
  FDataList[Count].SniffNo := Data.SniffNo;
  FDataList[Count].UnitNo := Data.UnitNo;
  FDataList[Count].LineNo := Data.LineNo;
  FDataList[Count].ErrNo := Data.ErrNo;
  FDataList[Count].Msg := Data.Msg;
  Inc(FCount);
end;

procedure TMDIGridWin.BeforeDestruction;
begin
  ClearList;
  inherited;
end;

procedure TMDIGridWin.CleanWindow;
begin
  InitGrid;
end;

procedure TMDIGridWin.ClearList;
var
  I: Integer;
begin
  FRows := 0;
  for I := Count downto 0 do
    FDataList[I].Msg := '';
  FCount := 0;
end;

function TMDIGridWin.DataText(Index, ACol: Integer): String;
begin
  Result := '-';
  if (Index >= 0) and (Index < Count) then
  begin
    case ACol of
      0:
        Result := IntToStr(FDataList[Index].Tick);
      1:
        Result := IntToHex(FDataList[Index].SniffNo, 4);
      2:
        Result := UnitText(FDataList[Index].UnitNo);
      3:
        Result := IntToStr(FDataList[Index].LineNo);
      4:
        Result := IntToStr(FDataList[Index].ErrNo);
      5:
        Result := FDataList[Index].Msg;
    end;
  end;
end;

procedure TMDIGridWin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  if Main is TZipSniffer then
    TZipSniffer(Main).ReleaseMe(self);
end;

procedure TMDIGridWin.FormCreate(Sender: TObject) ;
begin
  InitGrid;
end;

procedure TMDIGridWin.FormResize(Sender: TObject);
begin
  SetColumnWidths;
end;

procedure TMDIGridWin.GridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect:
    TRect; State: TGridDrawState);
var
  Canvas: TCanvas;
  Align: TAlignment;
  R: Integer;
  s: String;
begin
  Canvas := Grid.Canvas;
  if (ARow = 0) or (ACol = 0) then
    Canvas.Brush.Color := ClSilver
  Else
  begin
    if IsReadOnly then
      Canvas.Brush.Color := clInfoBk
    else
      Canvas.Brush.Color := ClWhite;
  end;
  // Center the first row
  if ARow = 0 then
  begin
    s := Titles[ACol];
    WriteText(Canvas, Rect, s, taCenter);
  end
  else
  begin
    Align := taLeftJustify;
    case ACol of
      0: WriteText(Canvas, Rect, IntToStr(ARow), taRightJustify);
      1, 2: Align := taCenter;
      3..5: Align := taRightJustify;
    end;
    if ACol > 0 then
    begin
      R := ARow - 1;
      if R < Count then
      begin
        s := DataText(R, ACol-1);
        WriteText(Canvas, Rect, s, Align);
      end;
    end;
  end;
end;

procedure TMDIGridWin.InitGrid;
var
  Rows: Integer;
begin
  Grid.RowCount := 1;
  Grid.ColCount := 7;
  SetColumnWidths;
  Rows := (Grid.Height - 12) div Grid.DefaultRowHeight;
  if Rows < 3 then
    Rows := 3;
  Grid.RowCount := Rows;
end;

function TMDIGridWin.Load(const fname: string): Integer;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.loadFromFile(fname);
    IsReadOnly := True;
    for I := 0 to SL.Count - 1 do
    begin
      LoadLine(SL[I]);
      if (I and 15) = 15 then
      begin
        UpdateGrid;
        Application.ProcessMessages;
      end;
    end;
    Result := SL.Count;
  finally
    SL.Free;
  end;
  UpdateGrid;
      Grid.Refresh;
end;

procedure TMDIGridWin.LoadLine(const Value: string);
var
  Data: TGData;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
{$ifndef UNICODE}
    SL.Text := StringReplace(Value, #9, #10#13, [rfReplaceAll]);
{$else}
    SL.LineBreak := #9;
    SL.Text := Value;
{$endif}
    if SL.Count >= 1 then
      Data.Tick := StrToIntDef(SL[0], 0);
    if SL.Count >= 2 then
      Data.SniffNo := StrToIntDef('$' + SL[1], 0);
    if SL.Count >= 3 then
      Data.UnitNo := StrToUnitNo(SL[2]);
    if SL.Count >= 4 then
      Data.LineNo := StrToIntDef(SL[3], 0);
    if SL.Count >= 5 then
      Data.ErrNo := StrToIntDef(SL[4], 0);
    if SL.Count > 5 then
      Data.Msg := SL[5];
    AppendDataRow(Data);
  finally
    SL.Free;
  end;
end;

procedure TMDIGridWin.SetAutoScroll(const Value: boolean);
begin
  FAutoScroll := Value;
end;

procedure TMDIGridWin.SetColumnWidths;
var
  C: Integer;
  ColWid: Integer;
  Wid: Integer;
  LC: Integer;
begin
  Grid.ColCount := 7;
  ColWid := Grid.DefaultColWidth;
  C := ColWid div 2;        // number
  Grid.ColWidths[0] := C;
  Wid := C;
  C := ColWid;
  Grid.ColWidths[1] := C;  // tick
  Wid := Wid + C;
  C := (ColWid * 2) div 3;
  Grid.ColWidths[2] := C; // sniffno
  Wid := Wid + C;
  C := ColWid * 2;
  Grid.ColWidths[3] := C; // unit
  Wid := Wid + C;
  C := (ColWid * 2) div 3;
  Grid.ColWidths[4] := C; // line
  Wid := Wid + C;
  C := (ColWid * 2) div 3;
  Grid.ColWidths[5] := C;  // error
  Wid := Wid + C;
  LC := (Grid.Width - 12) - Wid;
  if LC < 120 then
    LC := 120;
  Grid.ColWidths[6] := LC;  // text
end;

procedure TMDIGridWin.SetIsReadOnly(const Value: Boolean);
begin
  if FIsReadOnly <> Value then
  begin
    FIsReadOnly := Value;
    if Value then
      Grid.Color := clInfoBk
    else
      Grid.Color := clWindow;
  end;
end;

function TMDIGridWin.StrToUnitNo(const Field: string): Cardinal;
var
  ext: string;
  UName: string;
  I: Integer;
begin
  Result := 0;
  if Length(Field) < 3 then
    Exit;
  if Field[1] = '<' then
  begin
    //
    Exit;
  end;
  ext := ExtractFileExt(Field);
  if Length(Ext) < 4 then
    Exit;
  UName := Copy(Field, 1, Length(Field) - Length(ext));
  if SameText(Ext, '.pas') then
  begin
    for I := 0 to ZMUNITMAX - 1 do
      if SameText(UName, ZMUnitNames[I]) then
      begin
        Result := I + 1;
        Exit;
      end;
  end
  else
  begin
    for I := 0 to DZUNITMAX - 1 do
      if SameText(UName, DZUnitNames[I]) then
      begin
        Result := (I + 1) or DZUnitFlag;
        Exit;
      end;
  end;
end;

function TMDIGridWin.UnitText(UnitNo: Integer): string;
var
  UNo: Integer;
begin
  Result := '';
  UNo := UnitNo and UnitMask;
  if UNo > 0 then
  begin
    if ((UnitNo and DZUnitFlag) <> 0) and (UNo <= DZUNITMAX) then
      Result := DZUnitNames[UNo - 1] + '.cpp'
    else
    if UNo <= ZMUNITMAX then
      Result := ZMUnitNames[UNo - 1] + '.pas';
    if Result = '' then
      Result := '<' + IntToStr(UNo) + '>';
  end;
end;

procedure TMDIGridWin.UpdateGrid;
begin
  if Count >= Grid.RowCount then
  begin
    Grid.RowCount := Count + 1;
    if AutoScroll then
      Grid.Row := Grid.RowCount - 1;
  end
  else
  if Count <> OldCount then
  begin
    OldCount := Count;
    Grid.Refresh;
  end;
end;

procedure TMDIGridWin.WriteText(ACanvas: TCanvas; const ARect: TRect;
  const Text: string; Align: TAlignment);
const
  DX = 2;
  DY = 4;
var
  XE: Integer;
begin
  case Align of
    taRightJustify:
      XE := ARect.Right - ACanvas.TextWidth(Text) - 3;

    taCenter:
      XE := ARect.Left + (ARect.Right - ARect.Left -
        ACanvas.TextWidth(Text)) div 2;
  else
    XE := ARect.Left + DX;
  end;
  ACanvas.TextRect(ARect, XE, ARect.Top+2, Text);
end;

end.
