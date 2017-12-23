program CelikAPIDemo;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1},
  CelikApi in 'CelikApi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
