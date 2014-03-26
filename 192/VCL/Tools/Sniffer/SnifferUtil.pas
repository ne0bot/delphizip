unit SnifferUtil;
(* ***************************************************************************
 Copyright (C) 2009, 2010, 2011, 2012  by Russell J. Peters, Roger Aelbrecht.
 Copyright (C) 2013, 2014  by Russell J. Peters, Roger Aelbrecht.

   This file is part of TZipMaster Version 1.9.2

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
//modified 2014-02-07

interface

type
//  ByteArray = Array of byte;
{$IFDEF UNICODE}
  TZString = string;
  TZWString = string;
{$ELSE}
  UTF8String = type string;
  TZString = UTF8String;
  TZWString = WideString;
{$ENDIF}

function To_UTF8(const wstr: TZWString): UTF8String;
function UTF8ToStr(const raw: pAnsiChar; len: integer): TZString;
function UTF8ToWideStr(const raw: pAnsiChar; len: integer): TZWString;
function UTF8ToWide(const astr: UTF8String; len: integer): TZWString;

implementation

Uses
  Windows, SysUtils;

function To_UTF8(const wstr: TZWString): UTF8String;
var
  cnt: integer;
  wcnt: integer;
begin
  wcnt := Length(wstr);
  cnt := WideCharToMultiByte(CP_UTF8, 0, pWideChar(wstr), wcnt, nil, 0, nil, nil);
  if cnt > 0 then
  begin
    SetLength(Result, cnt);
    {cnt :=} WideCharToMultiByte(CP_UTF8, 0, pWideChar(wstr), wcnt,
      PAnsiChar(@Result[1]), cnt, nil, nil);
  end;
end;

{ TFormF }

function UTF8ToStr(const raw: pAnsiChar; len: integer): TZString;
var
  wcnt: integer;
  wtemp: TZWString;
begin
  Result := '';
  wcnt := MultiByteToWideChar(CP_UTF8, 0, raw, len, nil, 0);
  if wcnt > 0 then
  begin
    SetLength(wtemp, wcnt);
    {wcnt :=} MultiByteToWideChar(CP_UTF8, 0, raw, len,
      pWideChar(wtemp), wcnt);
  end
  else
    exit;  // invalid
{$IFDEF UNICODE}
  Result := PWideChar(@wtemp[1]);
{$ELSE}
  Result := To_UTF8(wtemp);
{$ENDIF}
end;

function UTF8ToWideStr(const raw: pAnsiChar; len: integer): TZWString;
var
  wcnt: integer;
  wtemp: TZWString;
begin
  Result := '';
  wcnt := MultiByteToWideChar(CP_UTF8, 0, raw, len, nil, 0);
  if wcnt > 0 then
  begin
    SetLength(wtemp, wcnt);
    {wcnt :=} MultiByteToWideChar(CP_UTF8, 0, raw, len,
      pWideChar(wtemp), wcnt);
    Result := pWideChar(@wtemp[1]);
  end;
end;

function UTF8ToWide(const astr: UTF8String; len: integer): TZWString;
begin
  Result := '';
  if len < 0 then
    len := Length(astr);
  Result := UTF8ToWideStr(PAnsiChar(@astr[1]), len);
end;

end.
