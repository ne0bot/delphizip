unit LocList;

interface

Uses
  Classes;

type
  TLocArgs = (laCode, laCountry, laID, laPage);

type
  TLocEntry = class(TObject)
  private
    FCountry: string;
    fId: Integer;
    FPage: Cardinal;
  public
    constructor Create;
    destructor Destroy; OVERRIDE;
    procedure AssignFrom(const Src: TLocEntry);
    property Country: string read FCountry write FCountry;
    property Id: Integer READ fId WRITE fId;
    property Page: Cardinal read FPage write FPage;
  end;

type
  TLocInfo = class
  private
    FChanged: boolean;
    FEntries: TStrings;
    procedure CheckIndex(Index: Integer);
    function CodeEntry(const Code: string; Force: Boolean): TLocEntry;
    function GetCode(Index: Integer): string;
    function GetCountry(Index: Integer): string;
    function GetCountryAtCode(const Code: string): string;
    function GetID(Index: Integer): Integer;
    function GetIDAtCode(const Code: string): Integer;
    function GetLangStr(Index: Integer): string;
    function GetPage(Index: Integer): Cardinal;
    function GetPageAtCode(const Code: string): Cardinal;
    procedure SetCode(Index: Integer; const Value: string);
    procedure SetCountry(Index: Integer; const Value: string);
    procedure SetCountryAtCode(const Code: string; const Value: string);
    procedure SetEntries(const Value: TStrings);
    procedure SetID(Index: Integer; const Value: Integer);
    procedure SetIDAtCode(const Code: string; const Value: Integer);
    procedure SetPage(Index: Integer; const Value: Cardinal);
    procedure SetPageAtCode(const Code: string; const Value: Cardinal);
    function _AsNum(const Arg: string): Integer;
  protected
    property Entries: TStrings read FEntries write SetEntries;
  public
    function Add(const Code, Desc: string; IDAtCode, DefCP: Integer): integer;
    procedure AfterConstruction; override;
    procedure AssignFrom(const Src: TLocInfo);
    procedure BeforeDestruction; override;
    procedure Clear;
    function Count: integer;
    function IndexedEntry(Index: Integer; Force: Boolean): TLocEntry;
    function IndexOf(const Code: string): integer;
    function IndexOfID(ID: integer): integer;
    function LoadFromFile(const FileName: string): integer;
    function WriteToFile(const FileName: string): Integer;
    property Changed: boolean read FChanged write FChanged;
    property Code[Index: Integer]: string read GetCode write SetCode;
    property Country[Index: Integer]: string read GetCountry write SetCountry;
    property CountryAtCode[const Code: string]: string read GetCountryAtCode write
        SetCountryAtCode;
    property ID[Index: Integer]: Integer read GetID write SetID;
    property IDAtCode[const Code: string]: Integer read GetIDAtCode write
        SetIDAtCode;
    property LangStr[Index: Integer]: string read GetLangStr;
    property Page[Index: Integer]: Cardinal read GetPage write SetPage;
    property PageAtCode[const Code: string]: Cardinal read GetPageAtCode write
        SetPageAtCode;
  end;

implementation

uses
  SysUtils, ZMUtils, ToolHelper, Main;

{ TLocInfo }

function TLocInfo.Add(const Code, Desc: string; IDAtCode, DefCP: Integer): integer;
var
  Entry: TLocEntry;
begin
  Result := Entries.Add(Code);
  if Result >= 0 then
  begin
    Entries.Objects[Result] := TLocEntry.Create;  // make new entry
    Entry := Entries.Objects[Result] as TLocEntry;
    Entry.Country := Desc;
    Entry.ID := IDAtCode;
    Entry.Page := DefCP;
  end;
end;

procedure TLocInfo.AfterConstruction;
begin
  inherited;
  FEntries := TStringList.Create;
end;

procedure TLocInfo.AssignFrom(const Src: TLocInfo);
var
  I: Integer;
begin
  Clear;
  if Assigned(Src) and (Src <> Self) then
  begin
    for I := 0 to Src.Count - 1 do
      Add(Src.Code[I], Src.Country[I], Src.ID[I], Src.Page[I]);
    Changed := Src.Changed;
  end;
end;

procedure TLocInfo.BeforeDestruction;
begin
  Clear;
  FEntries.Free;
  inherited;
end;

procedure TLocInfo.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= FEntries.Count) then
    raise Exception.Create('Index (' + IntToStr(index) + ') out of range 0..' +
      IntToStr(FEntries.Count - 1));
end;

procedure TLocInfo.Clear;
var
  I: Integer;
  obj: pointer;
begin
  for I := 0 to FEntries.Count -1 do
  begin
    obj := FEntries.Objects[I];
    FEntries.Objects[I] := nil;
    if obj <> nil then
      TObject(obj).Free;
  end;
  FEntries.Clear;
end;

function TLocInfo.CodeEntry(const Code: string; Force: Boolean): TLocEntry;
var
  Idx: Integer;
begin
  Result := nil;
  Idx := IndexOf(Code);
  if (Idx >= 0) then
  begin
    // Code was found
    if (Entries.Objects[Idx] = nil) then
    begin
      if not Force then
        Exit;
      Entries.Objects[Idx] := TLocEntry.Create;  // make new entry
    end;
    Result := Entries.Objects[Idx] as TLocEntry;
  end;
end;

function TLocInfo.Count: integer;
begin
  Result := FEntries.Count;
end;

function TLocInfo.GetCode(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Entries[Index];
end;

function TLocInfo.GetCountry(Index: Integer): string;
var
  Entry: TLocEntry;
begin
  Entry := IndexedEntry(Index, false);
  if Entry <> nil then
    Result := Entry.Country
  else
    Result := '';
end;

function TLocInfo.GetCountryAtCode(const Code: string): string;
var
  Entry: TLocEntry;
begin
  Entry := CodeEntry(Code, False);
  if Entry <> nil then
    Result := Entry.Country
  else
    Result := '';
end;

function TLocInfo.GetID(Index: Integer): Integer;
var
  Entry: TLocEntry;
begin
  Entry := IndexedEntry(Index, false);
  if Entry <> nil then
    Result := Entry.ID
  else
    Result := 0;
end;

function TLocInfo.GetIDAtCode(const Code: string): Integer;
var
  Entry: TLocEntry;
begin
  Entry := CodeEntry(Code, False);
  if Entry <> nil then
    Result := Entry.ID
  else
    Result := 0;
end;

function TLocInfo.GetLangStr(Index: Integer): string;
begin
//  CheckIndex(Index);
  Result := Code[Index] + ':' + Country[Index];
end;

function TLocInfo.GetPage(Index: Integer): Cardinal;
var
  Entry: TLocEntry;
begin
  Entry := IndexedEntry(Index, false);
  if Entry <> nil then
    Result := Entry.Page
  else
    Result := 0;
end;

function TLocInfo.GetPageAtCode(const Code: string): Cardinal;
var
  Entry: TLocEntry;
begin
  Entry := CodeEntry(Code, False);
  if Entry <> nil then
    Result := Entry.Page
  else
    Result := 0;
end;

function TLocInfo.IndexedEntry(Index: Integer; Force: Boolean): TLocEntry;
begin
  Result := nil;
  CheckIndex(Index);
  if (Entries.Objects[Index] = nil) then
  begin
    if not Force then
      Exit;
    Entries.Objects[Index] := TLocEntry.Create;  // make new entry
  end;
  Result := Entries.Objects[Index] as TLocEntry;
end;

function TLocInfo.IndexOf(const Code: string): integer;
begin
  Result := FEntries.IndexOf(Code);
end;

function TLocInfo.IndexOfID(ID: integer): integer;
var
  Entry: TLocEntry;
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count -1 do
  begin
    if Entries.Objects[I] = nil then
      Continue;
    Entry := Entries.Objects[I] as TLocEntry;
    if Entry.ID = ID then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TLocInfo.LoadFromFile(const FileName: string): integer;
const
//  SEP = ' , ';
  SEP = ';';
var
//  AnsiArg: string;
  Arg: string;
  CntryList: TStringList;
  TheCode: string;
  TheCountry: string;
  I: Integer;
//  IDArg: string;
  Line: string;
  TheCP: Integer;
  TheID: Integer;
begin
  Result := -1;
  Clear;
  if not FileExists(FileName) then
    Exit;
  CntryList := TStringList.Create;
  try
    CntryList.LoadFromFile(FileName);
    for I := 0 to CntryList.Count - 1 do
    begin
      Line := Trim(CntryList[I]);
      if (Length(Line) < 1) or (Line[1] = ';') then
        Continue;   // ignore
      // must be code
      TheCode := Trim(ZSplitString(SEP, Line, Line));
      TheCountry := Trim(ZSplitString(SEP, Line, Line));
      Arg := ZSplitString(SEP, Line, Line);
      TheID := _AsNum(Arg);
      Arg := ZSplitString(SEP, Line, Line);
      TheCP := _AsNum(Arg);
      Add(TheCode, TheCountry, TheID, TheCP);
    end;
    if Entries.Count > 0 then
    begin
      Result := Entries.Count;
      Changed := False;
    end;
  finally
    CntryList.Free;
  end;
end;

procedure TLocInfo.SetCode(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  if Entries[Index] <> Value then
  begin
    Entries[Index] := Value;
    FChanged := True;
  end;
end;

procedure TLocInfo.SetCountry(Index: Integer; const Value: string);
var
  Entry: TLocEntry;
begin
  CheckIndex(Index);
  Entry := IndexedEntry(Index, True);
  if Entry <> nil then
  begin
    // Code was found
    if Entry.Country <> Value then
    begin
      Entry.Country := Value;
      FChanged := True;
    end;
  end;
end;

procedure TLocInfo.SetCountryAtCode(const Code: string; const Value: string);
var
  Entry: TLocEntry;
begin
  Entry := CodeEntry(Code, True);
  if Entry <> nil then
  begin
    // Code was found
    if Entry.Country <> Value then
    begin
      Entry.Country := Value;
      FChanged := True;
    end;
  end;
end;

procedure TLocInfo.SetEntries(const Value: TStrings);
begin
//  FEntries := Value;
end;

procedure TLocInfo.SetID(Index: Integer; const Value: Integer);
var
  Entry: TLocEntry;
begin
  Entry := IndexedEntry(Index, True);
  if Entry <> nil then
  begin
    // Code was found
    if Entry.ID <> Value then
    begin
      Entry.ID := Value;
      FChanged := True;
    end;
  end;
end;

procedure TLocInfo.SetIDAtCode(const Code: string; const Value: Integer);
var
  Entry: TLocEntry;
begin
  Entry := CodeEntry(Code, True);
  if Entry <> nil then
  begin
    // Code was found
    if Entry.ID <> Value then
    begin
      Entry.ID := Value;
      FChanged := True;
    end;
  end;
end;

procedure TLocInfo.SetPage(Index: Integer; const Value: Cardinal);
var
  Entry: TLocEntry;
begin
  Entry := IndexedEntry(Index, True);
  if Entry <> nil then
  begin
    // Code was found
    if Entry.Page <> Value then
    begin
      Entry.Page := Value;
      FChanged := True;
    end;
  end;
end;

procedure TLocInfo.SetPageAtCode(const Code: string; const Value: Cardinal);
var
  Entry: TLocEntry;
begin
  Entry := CodeEntry(Code, True);
  if Entry <> nil then
  begin
    // Code was found
    if Entry.Page <> Value then
    begin
      Entry.Page := Value;
      FChanged := True;
    end;
  end;
end;

function TLocInfo.WriteToFile(const FileName: string): Integer;
var
  CntryList: TStringList;
  Entry: TLocEntry;
  I: Integer;
  Line: string;
begin
//  Result := -1;
  CntryList := TStringList.Create;
  try
    for I := 0 to Count-1 do
    begin
      if Entries.Objects[I] = nil then
        Continue;
      Entry := Entries.Objects[I] as TLocEntry;
      Line := Code[I] + '; ';
      Line := Line + Entry.Country + '; $' + IntToHex(Entry.ID, 4) + '; ' +
        IntToStr(Entry.Page);
      CntryList.Add(Line);
    end;
    ToolSupp.BackupFile(FileName);
    CntryList.SaveToFile(FileName{$if CompilerVersion >= 18.0}, TEncoding.Default{$ifend});
    Result := CntryList.Count;
    Changed := False;
  finally
    CntryList.Free;
  end;
end;

function TLocInfo._AsNum(const Arg: string): Integer;
var
  ch: Char;
  IsHex: Boolean;
  s: string;
  I: Integer;
  n: integer;
begin
  Result := 0;
  s := Trim(Arg);
  if Length(s) < 1 then
    Exit;
  IsHex := False;
  if s[1] = '$' then
  begin
    if Length(s) < 2 then
      Exit;
    IsHex := True;
    s := Copy(s, 2, 8);
  end
  else
  if (Length(s) > 2) and (s[1] = '0') and ((s[2] = 'x') or (s[2] = 'X')) then
  begin
    IsHex := True;
    s := Copy(s, 3, 8);
  end;
  if not IsHex then
    Result := StrToIntDef(s, 0)
  else
  begin
    for I := 1 to Length(s) do
    begin
      ch := s[I];
      case ch of
        '0'..'9': n := Ord(ch) - Ord('0');
        'a'..'f': n := 10 + Ord(ch) - Ord('a');
        'A'..'F': n := 10 + Ord(ch) - Ord('A');
      else
        n := -1;
      end;
      if n < 0 then
        Break;
      Result := (Result * 16) + n;
    end;
  end;
end;

{ TLocEntry }

constructor TLocEntry.Create;
begin
//
end;

destructor TLocEntry.Destroy;
begin
  inherited;
end;

procedure TLocEntry.AssignFrom(const Src: TLocEntry);
begin
  if Assigned(Src) and (Src <> Self) then
  begin
    FCountry := Src.Country;
    fId := Src.Id;
    FPage := Src.Page;
  end;
end;

end.
