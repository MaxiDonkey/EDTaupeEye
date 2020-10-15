program TaupeEye;



{$R *.dres}

uses
  Vcl.Forms,
  Main in 'Main.pas' {MainForm},
  EyeXHost in 'EyeXHost.pas',
  EyeX in '..\EyeX.pas',
  uEyeXThread in 'uEyeXThread.pas',
  uAreaTobii in 'uAreaTobii.pas',
  uXMLDico in 'uXMLDico.pas' {XmlDico},
  Vcl.Themes,
  Vcl.Styles,
  uEliteManager in 'uEliteManager.pas',
  EliteBindingsTools in 'EliteBindingsTools.pas',
  uStatusReader in 'uStatusReader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TXmlDico, XmlDico);
  Application.Run;
end.
