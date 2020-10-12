unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Data.DB, Datasnap.DBClient,
  System.UITypes, ActiveX,
  { Spec }
  uAreaTobii, uXMLDico, EyeXHost, uEyeXThread, uEliteManager, EliteBindingsTools,
  uStatusReader, uRegistry;



{ --- EyeXHost window messages notifications }
const WM_EYEX_HOST_STATUS_CHANGED     = WM_USER + 0; //not used
      WM_REGION_GOT_ACTIVATION_FOCUS  = WM_USER + 1;
      WM_REGION_ACTIVATED             = WM_USER + 2; //not used


type
  TMainForm = class(TForm)
    Panel1: TPanel;
    DelayedStarter: TTimer;
    Panel7: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    Panel27: TPanel;
    Panel28: TPanel;
    Panel29: TPanel;
    Panel30: TPanel;
    Panel31: TPanel;
    Panel32: TPanel;
    Panel33: TPanel;
    Panel34: TPanel;
    Panel35: TPanel;
    Panel36: TPanel;
    Panel37: TPanel;
    Panel38: TPanel;
    Panel39: TPanel;
    Panel40: TPanel;
    Panel41: TPanel;
    Panel42: TPanel;
    Panel43: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure UpdateActivatableRegions;
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure DelayedStarterTimer(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
    procedure Panel3Click(Sender: TObject);
  private
    procedure OnGotActivationFocus(var Msg: TMessage); message WM_REGION_GOT_ACTIVATION_FOCUS;
    procedure OnMove(var Msg: TWMMove); message WM_MOVE;
    procedure AppUpdate(Sender: TObject);
    procedure ContextObserverStart;
  public
    EyeXHost  : TEyeXHost;
    AreaTobii : TAreaTobii;
    procedure UpdateAreas(Sender: TObject);
  end;

var
  MainForm: TMainForm;


implementation

uses
  EyeX;

{$R *.dfm}

procedure TMainForm.AppUpdate(Sender: TObject);
begin
  Update;
end;

procedure TMainForm.ContextObserverStart;
begin
  ContextObserver := TContextObserver.Create(AreaTobii);
//  ContextObserver.Start
end;

procedure TMainForm.DelayedStarterTimer(Sender: TObject);
begin
  DelayedStarter.Enabled := False;
  UpdateActivatableRegions;
  AreaTobii.SetEliteStatus(EliteStatus);
  { --- Instancie end start context observer }
  ContextObserverStart
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS );
  SetForegroundWindow(Handle);
  TLauncher.AppInitialize;
  AreaTobii := TAreaTobii.Create(Self);
  with AreaTobii do begin
    OnUpdateAreas := UpdateAreas;
    Areas.OnUpdate := AppUpdate;
    Context.AreasDefine;
    Context.Context_MainMenu;
    Repaint
  end;
  EyeXHost := TEyeXHost.create;
  TKeyMessageSender.Initialize;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  TLauncher.AppFinalize;
  TKeyMessageSender.Finalize;
  EyeXHost.Free
end;

procedure TMainForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if key=' ' then eyeXHost.TriggerActivation
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  try
    TKeyInventory.Initialize;
    TEliteManager.KeyInventoryAssign(KeyInventory);
    KeyWrite(AppKey, 'EliteBindingsError', 0)
  except
    on E:Exception do begin
      MessageDlg(E.Message, mtWarning, [mbOK], 0);
      Application.Terminate;
     end
  end;

  { --- l'application est plein écran }
  Application.MainForm.WindowState := wsMaximized;
  EyeXHost.Init(WindowHandle, WM_EYEX_HOST_STATUS_CHANGED, WM_REGION_GOT_ACTIVATION_FOCUS, WM_REGION_ACTIVATED);
  UpdateActivatableRegions;
  { --- démarrage du thread de capture de focus pour les panels }
  CallSurveyor.Start;
  { --- on peut rendre l'application transparente }
  TransparentColor := True;
  DelayedStarter.Enabled := True;
end;


procedure TMainForm.UpdateActivatableRegions;
var
  Areas : TActivatableRegions;
  i     : Integer;
begin
  with AreaPanels do begin
    { --- Invoquer ActiveCount permet de reconstruire le tableau ActivPanels }
    SetLength(Areas, ActiveCount);
    for i := Low(ActivPanels) to High(ActivPanels) do begin
      Areas[i].id     := ActivPanels[i].Tag;
      Areas[i].bounds := GetScreenBounds(ActivPanels[i]);
    end;
  end;
  eyeXHost.SetActivatableRegions( Areas )
end;

procedure TMainForm.UpdateAreas(Sender: TObject);
begin
  UpdateActivatableRegions
end;

procedure TMainForm.OnMove(var Msg: TWMMove);
begin
  if EyeXHost <> nil then UpdateActivatableRegions
end;


procedure TMainForm.Panel1Click(Sender: TObject);
begin
  with AreaPanels do
    SelectMode := TSelectMode( (Integer(SelectMode) + 1) mod 4)
end;

procedure TMainForm.Panel3Click(Sender: TObject);
begin
  with AreaPanels do NullEnable := not NullEnable
end;

procedure TMainForm.OnGotActivationFocus(var Msg: TMessage);
var id: UINT_PTR;
begin
  id := msg.WParam;
  with AreaPanels do case BlinkType of
    bt_transitory: CallFifo.Poke( id - 1 );
    bt_persistent: CallFifo.Poke( id     );
  end;
  update
end;


initialization
  CoUninitialize
end.

