program Project1;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  InternetStatusThread in 'InternetStatusThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
