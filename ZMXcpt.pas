unit ZMXcpt;

//  ZMXcpt.pas - Exception class for ZipMaster

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
//modified 2012-05-01

{$INCLUDE   '.\ZipVers.inc'}

interface

uses
  SysUtils;
       
{$IFNDEF UNICODE}
type
  TZMXArgs = (zxaNoStr, zxaStr, zxa2Str);
{$ENDIF}

type
  EZMException = class(Exception)
{$IFNDEF UNICODE}
  private
    fArgs: TZMXArgs;
    fStr1: String;
    fStr2: string;
{$ENDIF}
    FExtErr: Integer;
    function GetTheMessage: string;
    procedure SetTheMessage(const Value: string);
  protected
    // We do not always want to see a message after an exception.
    fDisplayMsg: Boolean;
    // We also save the Resource ID in case the resource is not linked in the application.
    fResIdent: Integer;
    function FixIdent(Ident: Integer): boolean;
  public
    constructor CreateDisp(const aMessage: String; const Display: Boolean);

    constructor CreateMsgDisp(Ident: Integer; const Display: Boolean);
    constructor CreateMsgInt(Ident, anInt: Integer);
    constructor CreateMsgStr(Ident: Integer; const Str1: String);
    constructor CreateMsg2Str(Ident: Integer; const Str1, Str2: String); 
    constructor CreateMsgFmt(Ident: Integer; const Args: array of const);
    property ResId: Integer read fResIdent write fResIdent;
    property DisplayMsg: boolean Read fDisplayMsg;
    property ExtErr: Integer read FExtErr write FExtErr;
    property TheMessage: string read GetTheMessage write SetTheMessage;
  end;

type
  EZipMaster = class(EZMException)

  end;


implementation

uses
  ZMMsg, ZMMsgStr {$IFNDEF UNICODE}, ZMUTF8{$ENDIF};

const
  __UNIT__ = 32 shl 23;

const
  ERRORMSG: String = 'TZipMaster exception';  // should never be seen

constructor EZMException.CreateDisp(const aMessage: String; const Display:
    Boolean);
begin
  inherited Create(aMessage);
  fDisplayMsg := Display;
  fResIdent   := DS_UnknownError;
{$IFNDEF UNICODE}
  fArgs := zxaNoStr;
{$ENDIF}
end;

constructor EZMException.CreateMsgFmt(Ident: Integer; const Args: array of
    const);
begin
  inherited Create(ERRORMSG);
  if FixIdent(Ident) then
    TheMessage := Format(TheMessage, Args);
  fDisplayMsg := True;
{$IFNDEF UNICODE}
  fArgs := zxaNoStr;
{$ENDIF}
end;

constructor EZMException.CreateMsgDisp(Ident: Integer; const Display: Boolean);
begin
  inherited Create(ERRORMSG);
  FixIdent(Ident);
  fDisplayMsg := Display;
{$IFNDEF UNICODE}
  fArgs := zxaNoStr;
{$ENDIF}
end;

constructor EZMException.CreateMsgInt(Ident, anInt: Integer);
begin
  CreateMsgFmt(Ident, [anInt]);
end;

constructor EZMException.CreateMsgStr(Ident: Integer; const Str1: String);
begin
  CreateMsgFmt(Ident, [Str1]); 
{$IFNDEF UNICODE}
  fArgs := zxaStr;
  fStr1 := Str1;
{$ENDIF}
end;

constructor EZMException.CreateMsg2Str(Ident: Integer; const Str1, Str2: String);
begin
  CreateMsgFmt(Ident, [Str1, Str2]);  
{$IFNDEF UNICODE}
  fArgs := zxa2Str;
  fStr1 := Str1;
  fStr2 := Str2;
{$ENDIF}
end;

function EZMException.FixIdent(Ident: Integer): boolean;
begin
  if Ident < 0 then
    ExtErr := -Ident
  else
    ExtErr := Ident;
  fResIdent := ExtErr and MSG_ID_MASK;
  TheMessage := LoadZipStr(ResId);
  Result := TheMessage <> '';
  if not Result then
  begin
    TheMessage := Format('Unknown error (%d)', [ResId]);
    fResIdent   := DS_UnknownError;  // substitute
  end;
end;

function EZMException.GetTheMessage: string;
begin
{$IFDEF UNICODE}
  Result := Message;
{$ELSE}
  if not UsingUTF8 then
    Result := Message
  else
  begin
    if fArgs <= zxaNoStr then
      Result := StrToUTF8(Message)
    else
    begin
      Result := LoadZipStr(fResIdent);
      if Result <> '' then
      begin
        // we need the format string as UTF8
        Result := StrToUTF8(Result);
        case fArgs of
          zxaStr:Result := Format(Result, [fStr1]);
          zxa2Str:Result := Format(Result, [fStr1, fStr2]);
        end;
      end;
    end;
  end;
{$ENDIF}
end;

procedure EZMException.SetTheMessage(const Value: string);
begin
  Message := Value;
end;

end.
