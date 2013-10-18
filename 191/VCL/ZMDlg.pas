unit ZMDlg;

//  ZMDlg.pas - DialogBox with buttons from language strings

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
//modified 2011-11-10
{$INCLUDE   '.\ZipVers.inc'}

interface

uses
  Classes, Windows, Forms, Dialogs, StdCtrls;

// High word = $10 or TMsgDlgType, low word = context
const
  zmtWarning = $100000;
  zmtError = $110000;
  zmtInformation = $120000;
  zmtConfirmation = $130000;
  zmtPassword = $140000;

type
  TZipDialogBox = class(TForm)
  private
    AvDlgUnits: TPoint;
    BeepId: integer;
    ctx: integer;
    IconID: pChar;
    PwdEdit: TEdit;
    function GetDlgType: integer;
    function GetPWrd: string;
    procedure SetPwrd(const Value: string);
  public
    constructor CreateNew2(Owner: TComponent; context: integer); virtual;
    procedure Build(const Title, Msg: String; Btns: TMsgDlgButtons);
    function ShowModal: integer; override;
    property DlgType: integer read GetDlgType;
    property PWrd: string read GetPWrd write SetPwrd;
  end;

implementation

uses SysUtils, Graphics, ExtCtrls, Controls,
{$IFNDEF UNICODE}
  ZMUTF8,
{$ENDIF}
  ZMMsg, ZMMsgStr;
{$INCLUDE   '.\ZipVers.inc'}

const
  __UNIT__ = 10 shl 23;

const
  SZmdText = 'zmdText';
  SImage = 'Image';
  SZmdEdit = 'zmdEdit';
  SZMDlg19 = 'ZMDlg_A%d';
//  SZMDlg19 = 'ZMDlg19%d';
  { Maximum no. of characters in a password; Do not change! }
  PWLEN = 80;

  { TMsgDlgBtn = (
    mbYes,
    mbNo,
    mbOK,
    mbCancel,
    mbAbort,
    mbRetry,
    mbIgnore,
    mbAll,
    mbNoToAll,
    mbYesToAll,
    mbHelp,
    mbClose
    ); }
type
{$IFDEF UNICODE}
  TZWideLabel = TLabel;
{$ELSE}

  TZWideLabel = class(TLabel)
  private
    WideText: WideString;
    procedure SetCaption(Value: WideString);
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
  public
    // published
    property Caption: WideString read WideText write SetCaption;
  end;

procedure TZWideLabel.DoDrawText(var Rect: TRect; Flags: Longint);
begin
  Canvas.Font := Font;

  DrawTextW(Canvas.Handle, pWideChar(WideText), Length(WideText), Rect, Flags);
end;

procedure TZWideLabel.SetCaption(Value: WideString);
begin
  WideText := Value;
  Invalidate; // repaint
end;
{$ENDIF}

procedure TZipDialogBox.Build(const Title, Msg: String; Btns: TMsgDlgButtons);
const
  kHMargin = 8;
  kVMargin = 8;
  kHSpacing = 10;
  kVSpacing = 10;
  kBWidth = 50;
  kBHeight = 14;
  kBSpacing = 4;
  ModalResults: array [TMsgDlgBtn] of integer =
    (mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
    mrYesToAll, 0 {$IFDEF UNICODE}, 0 {$ENDIF});
  MsgDlgBtnIds: array [TMsgDlgBtn] of Integer = (ZB_Yes, ZB_No, ZB_OK, ZB_Cancel,
    ZB_Abort, ZB_Retry, ZB_Ignore, ZB_CancelAll, ZB_NoToAll, ZB_YesToAll, 0
    {$IFDEF UNICODE}, 0 {$ENDIF});
var
  ALeft: integer;
  B: TMsgDlgBtn;
  BHeight: integer;
  BSpacing: integer;
  ButtonCount: integer;
  ButtonGroupWidth: integer;
  BWidth: integer;
  CancelButton: TMsgDlgBtn;
  CHeight: integer;
  CWidth: integer;
  DefaultButton: TMsgDlgBtn;
  DxText: TZWideLabel;
  HMargin: integer;
  HSpacing: integer;
  i: integer;
  IconTextHeight: integer;
  IconTextWidth: integer;
  N: TButton;
  tabOrdr: integer;
  TextRect: TRect;
  tx: integer;
  VMargin: integer;
  VSpacing: integer;
  wdth: integer;
{$IFDEF UNICODE}
  wmsg: String;
{$ELSE}
  wmsg: WideString;
{$ENDIF}
  X: integer;
  Y: integer;
begin
  BiDiMode := Application.BiDiMode;
  BorderStyle := bsDialog;
  Canvas.Font := Font;
  if Title = '' then
    Caption := Application.Title
  else
    Caption := Title;
{$IFNDEF UNICODE}
  if UsingUTF8 then
    wmsg := UTF8ToWide(Msg, -1)
  else
{$ENDIF}
    wmsg := Msg;
  HMargin := MulDiv(kHMargin, AvDlgUnits.X, 4);
  VMargin := MulDiv(kVMargin, AvDlgUnits.Y, 8);
  HSpacing := MulDiv(kHSpacing, AvDlgUnits.X, 4);
  VSpacing := MulDiv(kVSpacing, AvDlgUnits.Y, 8);
  BWidth := MulDiv(kBWidth, AvDlgUnits.X, 4);
  if mbOK in Btns then
    DefaultButton := mbOK
  else if mbYes in Btns then
    DefaultButton := mbYes
  else
    DefaultButton := mbRetry;
  if mbCancel in Btns then
    CancelButton := mbCancel
  else if mbNo in Btns then
    CancelButton := mbNo
  else
    CancelButton := mbOK;
  ButtonCount := 0;
  tabOrdr := 1;
  if DlgType = zmtPassword then
    tabOrdr := 2;
  for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
    if (B <{>} mbHelp) and (B in Btns) then
    begin
      Inc(ButtonCount);
      N := TButton.Create(Self);
      // with N do
      begin
        N.Name := Format(SZMDlg19, [ButtonCount]);
        N.Parent := Self;
        N.Caption := LoadZipStr(MsgDlgBtnIds[B]);
        N.ModalResult := ModalResults[B];
        if B = DefaultButton then
          N.Default := True;
        if B = CancelButton then
          N.Cancel := True;
        N.TabStop := True;
        N.TabOrder := tabOrdr;
        Inc(tabOrdr);
      end;
      TextRect := Rect(0, 0, 0, 0);
      Windows.DrawText(Canvas.Handle, pChar(N.Caption), -1, TextRect,
        DT_CALCRECT or DT_LEFT or DT_SINGLELINE or
          DrawTextBiDiModeFlagsReadingOnly);
      wdth := TextRect.Right - TextRect.Left + 8;
      if wdth > BWidth then
        BWidth := wdth;
    end;
  BHeight := MulDiv(kBHeight, AvDlgUnits.Y, 8);
  BSpacing := MulDiv(kBSpacing, AvDlgUnits.X, 4);
  SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
  DrawTextW(Canvas.Handle, pWideChar(wmsg), Length(wmsg) + 1, TextRect,
    DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or
      DrawTextBiDiModeFlagsReadingOnly);
  IconTextWidth := TextRect.Right;
  IconTextHeight := TextRect.Bottom;
  if IconID <> NIL then
  begin
    Inc(IconTextWidth, 32 + HSpacing);
    if IconTextHeight < 32 then
      IconTextHeight := 32;
  end;
  ButtonGroupWidth := 0;
  if ButtonCount <> 0 then
    ButtonGroupWidth := BWidth * ButtonCount + BSpacing * (ButtonCount - 1);
  if IconTextWidth > ButtonGroupWidth then
    CWidth := IconTextWidth
  else
    CWidth := ButtonGroupWidth;
  CHeight := IconTextHeight + BHeight;
  if DlgType = zmtPassword then
  begin
    if CWidth < (PWLEN * AvDlgUnits.X) then
      CWidth := PWLEN * AvDlgUnits.X;
    PwdEdit := TEdit.Create(Self);
    with PwdEdit do
    begin
      Name := SZmdEdit;
      Text := '';
      Parent := Self;
      PasswordChar := '*';
      MaxLength := PWLEN;
      TabOrder := 1;
      TabStop := True;
      BiDiMode := Self.BiDiMode;
      ALeft := IconTextWidth - TextRect.Right + HMargin;
      if UseRightToLeftAlignment then
        ALeft := CWidth - ALeft - Width;
      tx := PWLEN * AvDlgUnits.X;
      if tx < TextRect.Right then
        tx := TextRect.Right;
      SetBounds(ALeft, IconTextHeight + VMargin + VSpacing, tx, 15);
    end;
    ActiveControl := PwdEdit;
    CHeight := CHeight + PwdEdit.Height + VMargin;
  end;
  ClientWidth := CWidth + (HMargin * 2);
  ClientHeight := CHeight + VSpacing + VMargin * 2;
  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
  if IconID <> NIL then
    with TImage.Create(Self) do
    begin
      Name := SImage;
      Parent := Self;
      Picture.Icon.Handle := LoadIcon(0, IconID);
      SetBounds(HMargin, VMargin, 32, 32);
    end;
  DxText := TZWideLabel.Create(Self);
  with DxText do
  begin
    Name := SZmdText;
    Parent := Self;
    WordWrap := True;
    Caption := wmsg;
    BoundsRect := TextRect;
    BiDiMode := Self.BiDiMode;
    ALeft := IconTextWidth - TextRect.Right + HMargin;
    if UseRightToLeftAlignment then
      ALeft := Self.ClientWidth - ALeft - Width;
    SetBounds(ALeft, VMargin, TextRect.Right, TextRect.Bottom);
  end;
  X := (ClientWidth - ButtonGroupWidth) div 2;
  Y := IconTextHeight + VMargin + VSpacing;
  if DlgType = zmtPassword then
    Inc(Y, PwdEdit.Height + VSpacing);
  for i := 0 to pred(ComponentCount) do
    if Components[i] is TButton then
      with Components[i] as TButton do
      begin
        SetBounds(X, Y, BWidth, BHeight);
        Inc(X, BWidth + BSpacing);
      end;
end;

constructor TZipDialogBox.CreateNew2(Owner: TComponent; context: integer);
const
  IconIDs: array [0 .. 4] of pChar = (IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK,
    IDI_QUESTION, NIL);
  BeepIDs: array [0 .. 4] of integer = (MB_ICONEXCLAMATION, MB_ICONHAND,
    MB_ICONASTERISK, MB_ICONQUESTION, 0);
var
  buf: array [0 .. 65] of Char;
  i: integer;
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(Owner, 0);
  NonClientMetrics.cbSize := sizeof(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
  ctx := context;
  if DlgType = 0 then
    ctx := ctx or zmtWarning;
  for i := 0 to 25 do
  begin
    buf[i] := Char(ord('A') + i);
    buf[i + 27] := Char(ord('a') + i);
  end;
  buf[26] := ' ';
  buf[52] := ' ';
  for i := 53 to 63 do
    buf[i] := Char(ord('0') + i - 53);
  buf[64] := #0;
  GetTextExtentPoint(Canvas.Handle, buf, 64, TSize(AvDlgUnits));
  AvDlgUnits.X := AvDlgUnits.X div 64;
  i := (DlgType shr 16) and 7;
  if i > 4 then
    i := 4;
  IconID := IconIDs[i];
  BeepId := BeepIDs[i];
end;

function TZipDialogBox.GetDlgType: integer;
begin
  Result := ctx and $1F0000;
end;

function TZipDialogBox.GetPWrd: string;
begin
  if assigned(PwdEdit) then
    Result := PwdEdit.Text
  else
    Result := '';
end;

procedure TZipDialogBox.SetPwrd(const Value: string);
begin
  if assigned(PwdEdit) and (Value <> PwdEdit.Text) then
    PwdEdit.Text := Value;
end;

function TZipDialogBox.ShowModal: integer;
begin
  if BeepId <> 0 then
    MessageBeep(BeepId);
  Result := inherited ShowModal;
end;

end.
