program FixErrorLines;

{$R 'FixErrorLines_ver.res' 'FixErrorLines_ver.rc'}

uses
  Forms,
  Main in 'Main.pas' {Form1},
  scan in 'scan.pas';

{$R *.res}

begin
  Application.Initialize;
{$ifdef UNICODE}
  Application.MainFormOnTaskbar := True;
{$endif}  
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
