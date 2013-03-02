program StringSearch;

uses
  Vcl.Forms,
  SearchStringMain in 'SearchStringMain.pas' {Form1},
  SearchStringThread in 'SearchStringThread.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
