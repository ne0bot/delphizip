unit ZMDeflateOpr;

// ZMZipOpr.pas - Zip operations (limited only this version)

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
// modified 2013-04-19

{$I   '.\ZipVers.inc'}

interface

uses
  {$IFDEF VERDXE2up}
    System.Classes, System.SysUtils, WinApi.Windows,
  {$ELSE}
    SysUtils, Windows, Classes,
  {$ENDIF}
  ZipMstr, ZMModOpr;

type
  TZMDeflateOpr = class(TZMBaseOpr)
  private
    function AskPassword(var PassPhrase: AnsiString; const EntryName: string):
        Integer;
    function DoDeflate(OutStream, InStream: TStream; Length: Int64; var Method:
        TZMDeflates; var CRC: Cardinal): Integer;
    function ProgressFunc(Sender: TStream; Count: Cardinal): boolean;
    function WriteProgressFunc(Sender: TStream; Count: Cardinal): boolean;
  public
    function AddStreamToStream(InStream: TMemoryStream): Integer;
    function Deflate(OutStream, InStream: TStream; Length: Int64; var Method:
        TZMDeflates; var CRC: Cardinal): Integer;
    function PreCalcCRC(InStream: TStream; count: Int64; var CRC: DWORD): Integer;
  end;


implementation

uses  
  {$IFDEF VERDXE2up}
    Vcl.Dialogs,
  {$ELSE}
    Dialogs,
  {$ENDIF}
  ZMZStream, ZMZLibEx, ZMMsg, ZMStructs, ZMBody;

const
  __UNIT__ = 33;

const
  ZLibDeflates: array [0..9] of TZCompressionLevel = (zcNone, zcLevel1, zcLevel2,
    zcLevel3, zcLevel4, zcLevel5, zcLevel6, zcLevel7, zcLevel8, zcLevel9);

function ZM_Error(line, error: Integer): Integer;
begin
  result := (__UNIT__ shl 23) + (line shl 10) or error;
end;

function TZMDeflateOpr.AddStreamToStream(InStream: TMemoryStream): Integer;
var
  Header: TZM_StreamHeader;
  Method: TZMDeflates;
  ZipStream: TStream;
begin
  if InStream = Body.ZipStream then
    Result := -ZM_Error(100, ZS_InIsOutStream)
  else
  if assigned(InStream) and (InStream.Size > 0) then
  begin
    if AddEncrypt in Body.AddOptions then
    begin
      Result := -ZM_Error(107, ZS_NoEncrypt);
      exit;
    end;
    ZipStream := Body.ZipStream;
    ZipStream.Size := 0;
    Method := zmDeflate;
    Header.Method := METHOD_DEFLATED;
    Header.CRC := 0;
    ZipStream.WriteBuffer(Header, SizeOf(Header));
    Result := Deflate(ZipStream, InStream, -1, Method, Header.CRC);
    if SuccessCnt = 1 then
    begin
      ZipStream.Position := 0;
      if Method <> zmDeflate then
        Header.Method := METHOD_STORED; // was stored
      ZipStream.WriteBuffer(Header, SizeOf(Header));
    end
    else
      ZipStream.Size := 0;
  end
  else
    Result := -ZM_Error(128, ZS_NothingToZip);
end;

function TZMDeflateOpr.AskPassword(var PassPhrase: AnsiString; const EntryName:
    string): Integer;
var
  pwd: String;
  Response: TmsgDlgBtn;
  RptCount: Longword;
  tmpPasswordError: TZMPasswordErrorEvent;
begin
  if Body.Password <> '' then
  begin
    PassPhrase := AnsiString(Body.Password); // return global
    Result := 0;
    Exit;
  end;
  pwd := '';
  RptCount := 1;
  Response := mbOK;
  tmpPasswordError := Master.OnPasswordError;
  if assigned(tmpPasswordError) then
    tmpPasswordError(Master, True, pwd, EntryName, RptCount, Response)
  else
    pwd := Master.GetAddPassword(Response);

  if Response = mbAbort then // Abort
    raise EZMAbort.Create;

  if (Response = mbOK) and (pwd <> '') then
  begin
    PassPhrase := AnsiString(pwd);
    Result := 0;
  end
  else
    Result := -ZM_Error(170, ZD_RES_PASSWORD_CANCEL);  // no password or cancelled
end;

function TZMDeflateOpr.Deflate(OutStream, InStream: TStream; Length: Int64; var
    Method: TZMDeflates; var CRC: Cardinal): Integer;
begin
  Result := 0;
  if not assigned(InStream) then
    Result := -ZM_Error(206, ZS_NoInStream)
  else
  if not assigned(OutStream) then
    Result := -ZM_Error(211, ZS_NoOutStream)
  else
  if InStream = OutStream then
    Result := -ZM_Error(216, ZS_InIsOutStream);
  if Result = 0 then
    Result := DoDeflate(OutStream, InStream, Length, Method, CRC);
end;

function TZMDeflateOpr.DoDeflate(OutStream, InStream: TStream; Length: Int64;
    var Method: TZMDeflates; var CRC: Cardinal): Integer;
var
  CompLevel: TZCompressionLevel;
  CRCStream: TZMCRCStream;
  DestStream: TStream;
  EncCRC: DWORD;
  Encryptor: TZMZipCryptor;
  EncStream: TZMEncryptStream;
  ncrypt: boolean;
  Phrase: AnsiString;
begin
  result := 0;
  ncrypt := (Method = zmStoreEncrypt) or (Method = zmDeflateEncrypt);
  // We can not do an Unattended Add if we don't have a password.
  if Reporter.Unattended and ncrypt and (Lister.Password = '') then
  begin
    result := -ZM_Error(223, ZS_UnattPassword);
    exit;
  end;
  if (Method = zmDeflate) or (Method = zmDeflateEncrypt) then
    CompLevel := ZLibDeflates[Master.AddCompLevel]
  else
    CompLevel := zcNone;
  if Length < 0 then
    Length := InStream.Size;
  if Length = 0 then
  begin
    Length := InStream.Size;
    InStream.Position := 0;
  end;
  Progress.TotalCount := 1;
  Progress.TotalSize := Length;
  if ncrypt then
  begin
    Reporter.Trace('Precalculate CRC');
    result := PreCalcCRC(InStream, Length, EncCRC);
    if result < 0 then
      exit;
  end;
  DestStream := nil;
  CRCStream := TZMCRCStream.Create(TZMProgressStream.Create(InStream,
    ProgressFunc, 16 * 1024), CRC, True);
  try
    Progress.NewItem('<stream>', Length);
    DestStream := TZMProgressStream.Create(OutStream,
          WriteProgressFunc, 4096);
    if ncrypt then
    begin
      Result := AskPassword(Phrase, '<stream>');
      if Result < 0 then
      begin
        ncrypt := False; // no password - don't encrypt
        Result := 0;
      end;
    end;
    if ncrypt then
    begin
      EncStream := TZMEncryptStream.Create(DestStream, TZMZipCryptor.Create, True);
      DestStream := EncStream;
      Encryptor := TZMZipCryptor(EncStream.cryptor);
      result := Encryptor.EncodeInit(Phrase, EncCRC);
      if result >= 0 then
        result := Encryptor.WriteHeader(OutStream);
    end;
    if result >= 0 then
    begin
      if CompLevel <> zcNone then
        DestStream := TZMCompressionStream.Create(DestStream, CompLevel, True);
      result := CopyStream(DestStream, CRCStream, Length);
      if ncrypt and (EncCRC <> CRC) then
      begin
        result := -ZM_Error(291, ZS_CryptError);
        Reporter.Inform('File crc changed during compression', result);
      end;
    end;
  finally
    CRCStream.Free;
    DestStream.Free;
    Progress.EndItem;
  end;
  if result >= 0 then
  begin // success
    result := 0;
    SuccessCnt := 1;
    if CompLevel <> zcNone then
      Method := zmDeflate
    else
      Method := zmStore;
    if ncrypt then
      Method := Succ(Method);
  end;
end;

function TZMDeflateOpr.PreCalcCRC(InStream: TStream; count: Int64;
  var CRC: DWORD): Integer;
const
  BufSizeMax = $F000;
var
  Buf: PByte;
  BufSize: Integer;
  CRCStream: TZMCRCStream;
  Err: Integer;
  InPosition: Int64;
begin
  InPosition := InStream.Position;
  BufSize := BufSizeMax;
  if count < BufSize then
    BufSize := count;
  GetMem(Buf, BufSize);
  try
    Progress.NewXtraItem('<stream>', count);
    CRCStream := TZMCRCStream.Create(InStream, CRC);
    try
      repeat
        result := CRCStream.Read(Buf^, BufSize);
        if result < BufSize then
          break; // return error
        Progress.AdvanceXtra(Result);
        Dec(count, result);
        if count < BufSize then
          BufSize := count;
      until count <= 0;
    finally
      CRCStream.Free;
    end;
  finally
    FreeMem(Buf);
    Err := InStream.Seek(InPosition, soBeginning);
    if Err < 0 then
      Result := Err;
  end;
end;

function TZMDeflateOpr.ProgressFunc(Sender: TStream; Count: Cardinal): boolean;
begin
  Progress.Advance(Count);
  Result := Body.Cancel <> 0;
end;

function TZMDeflateOpr.WriteProgressFunc(Sender: TStream; Count: Cardinal):
    boolean;
begin
  Progress.MoreWritten(Count);
  Result := Body.Cancel <> 0;
end;

end.
