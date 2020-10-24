{*******************************************************}
{                                                       }
{             08/2020  MaxiDonkey  Library              }
{                                                       }
{*******************************************************}

unit uEyeXThread;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, EyeXHost, Vcl.ExtCtrls, System.SyncObjs,
  System.Math, System.Types, EliteBindingsTools;

const
  NULL_STACK = -1;
  NULL_PANEL = True;

type
  TAreaPanels = class;

  TCallFifo = class(TStringList)
  private
    FMutex: TMutex;
  public
    function Poke(const IntValue: Integer): Integer; overload;
    function Poke(const StrValue: string): Integer; overload;
    function Peek:Integer;

    constructor Create;
    destructor Destroy; override;

    class procedure Initialize;
    class procedure Finalize;
  end;

  TCallSurveyor = class(TThread)
  private
    ThPile  : TCallFifo;
    ThAreas : TAreaPanels;
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  protected
    procedure Execute; override;
  public
    constructor Create(const APile: TCallFifo; const AAreas : TAreaPanels);

    class procedure Initialize;
    class procedure Finnalize;
  end;

  TSelectMode  = ( sm_directnull,    sm_directnulltimed,
                   sm_directnotnull, sm_blinknotbacktimed , sm_blinknotback, sm_blinkback,
                   sm_none);
  TBlinkType   = ( bt_transitory,    bt_persistent );
  TModeTrigger = ( mt_direct,        mt_timer,              mt_zoneup );

  TTabPanels   = array of TPanel;
  TTabModes    = array of TSelectMode;
  TTabNuls     = array of Boolean;
  TBoolSelect  = array of Boolean;
  TIntTag      = array of Integer;

  TTrigNotify  = procedure (Sender: TObject; KeyUp: Boolean; ATag: Integer) of object;
  TIdxNotify   = procedure (Sender: TObject; OldValue, NewValue: Integer) of object;
  TTagNotify   = procedure (Sender: TObject; ATag: Integer) of object;
  TPnlNotify   = procedure (const Panel: TPanel) of object;

  TAreaPanels  = class
  private
    FPanels       : TTabPanels;                  { --- Liste des zones enregistrées }
    FPanelsModes  : TTabModes;                   { --- Liste des états pour chaque zone }
    FActivPanels  : TTabPanels;                  { --- Liste des zones enregistrées visibles }
    FNullPanels   : TTabNuls;                    { --- Liste des zones nulles }
    FSelected     : TBoolSelect;                 { --- Liste des zones sélectionnées; Mode == sm_blinknotback }
    FTags         : TIntTag;                     { --- Liste des Tag == n° fonction par zone }
    FVisiblePanel : TBoolSelect;                 { --- Liste des états de visibilité }

    FItemIndex    : Integer;                     { --- Index de la zone courante peut être -1 }
    FOldIndex     : Integer;                     { --- Index de la précédente zone courante }
    FBlinkTick    : Cardinal;                    { --- Tick lors de la sélection pour les changements d'états }

    FSelectMode   : TSelectMode;                 { --- Tampon d'indicateurs pour la zone courante sélectionnée }
    FValidTimer   : TTimer;                      { --- Timer pour validation transitory }
    FTimeIndex    : Integer;                     { --- Index de la zone pour le timer ValidTimer }
    FSelectTimer  : TTimer;                      { --- Timer pour déselection d'une zone sélectionnée }
    FSelectIndex  : Integer;                     { --- Index de la zone pour le timer SelectTimer }

    FKeyMessageSender : TKeyMessageSender;

    { --- Indicateurs d'états pour l'activation des zones en mode persistent}
    FBlinkType    : TBlinkType;
    FCanBlink     : Boolean;
    FBlinkBack    : Boolean;
    { --- Indicateurs d'états pour l'activation des zones en mode transitory}
    FNullEnable   : Boolean;
    FNullTimed    : Boolean;
    { --- Events }
    FUpdate       : TNotifyEvent;                { --- Methode pour valider une région de l'écran à l'affichage vs processmessage}
    FTrigNotify   : TTrigNotify;
    FBeforeSelect : TIdxNotify;
    FNullSelect   : TTagNotify;
    FOnUnSelect   : TTagNotify;

    procedure SetItemIndex(const Value: Integer);
    procedure SetSelectMode(const Value: TSelectMode);

  private
    { --- Gestion des zones et des états de chaque zone - Coloration des états }
    procedure AddPanel(const Panel: TPanel);
    procedure AddMode(const AMode: TSelectMode);
    procedure AddNull(const Value: Boolean);
    procedure AddTag(const Value: Integer);
    procedure AddVisible(const Value: Boolean);
    procedure UnColorize;
    procedure Colorize(const AIndex: Integer; const Valid: Boolean = False);
    procedure Update;
    procedure RetrieveActivPanels;

  private
    { --- Gestion du timer pour validation en mode transitory }
    procedure TimerProcess(Sender: TObject);
    procedure StartTimer;
    procedure StopTimer;

    { --- Gestion du timer pour la désélection d'une zone }
    procedure SelectProcess(Sender: TObject);
    procedure StartTimerSelected;
    procedure StopTimerSelected;

    { --- Gestion des déclancheurs }
    procedure Trigger(const From: TModeTrigger);

  private
    { --- Outils pour la mise en forme des zones }
    procedure ProcessWithPanels(const Method: TPnlNotify);
    procedure PanelChangeBorder(const Panel: TPanel);
    procedure PanelChangeVisible(const Panel: TPanel);

  private
    { --- Ascesseurs / mutateurs }
    function  GetWaitClickDelay: Integer;
    procedure SetWaitClickDelay(const Value: Integer);
    function  GetViewClickDelay: Cardinal;
    procedure SetViewClickDelay(const Value: Cardinal);
    function  GetWaitLagDelay: Integer;
    procedure SetWaitLagDelay(const Value: Integer);
    function  GetTransitoryMultiClick: Boolean;
    procedure SetTransitoryMultiClick(const Value: Boolean);
    function  GetZoneUpDelay: Integer;
    procedure SetZoneUpDelay(const Value: Integer);
    function  GetBackColor: TColor;
    procedure SetBackColor(const Value: TColor);
    function  GetSeleColor: TColor;
    procedure SetSeleColor(const Value: TColor);
    function  GetValiColor: TColor;
    procedure SetValiColor(const Value: TColor);
    function  GetBorder: Boolean;
    procedure SetBorder(const Value: Boolean);
    function  GetVisible(index: Integer): Boolean;
    procedure SetVisible(index: Integer; const Value: Boolean);

  public
    { --- Areas Managment }
    procedure Reset;
    procedure Repaint;

    procedure Add(const Panel: TPanel; const ATag: Integer; const AMode: TSelectMode; const ANull: Boolean = False);

    procedure BoxUpdate(const Raw, Col, ATag: Integer; ACaption: string; AVisible: Boolean; AMode: TSelectMode; ANull: Boolean);

    procedure CaptionUpdate(const Raw: Integer; const Values: array of string);
    procedure VisibleUpdate(const Raw: Integer; const Values: array of Boolean; Update: Boolean = False);
    procedure SelModeUpdate(const Raw: Integer; const Values: array of TSelectMode);
    procedure TagsUpdate(const Raw: Integer; const Values: array of Integer);
    procedure NullUpdate(const Raw: Integer; const Values: array of Boolean);

    procedure Unselect;

    function  IndexOfTag(const Value: Integer):Integer;
    function  IndexOfPanel(const Value: TPanel):Integer;
    function  Selected(const index: Integer):  Boolean;


    { --- Visibilité d'une zone }
    property Visible[index: Integer]: Boolean read GetVisible write SetVisible;

    { --- Construit la liste des zones actives }
    function  ActiveCount:Integer;

    { --- Liste de toutes les zones gérées }
    property Panels: TTabPanels read FPanels;

    { --- Liste des zones visibles }
    property ActivPanels: TTabPanels read FActivPanels;

    { --- Index du composant "zone" acttif }
    property ItemIndex: Integer read FItemIndex write SetItemIndex;

    { --- Mode d'activation pour la zone courante }
    property SelectMode: TSelectMode read FSelectMode write SetSelectMode;

    { --- Type de zone : transitory ou persistent }
    property BlinkType: TBlinkType read FBlinkType write FBlinkType;

    { --- Mode transitory activé/désactivé pour toutes les zones :
          SelectMode inactif si NullEnable actif }
    property NullEnable: Boolean read FNullEnable write FNullEnable;

    { --- Activation d'une zone en mode transitory avec un regard long (timé)
          sinon active directement avec la sélection au regard }
    property NullTimed: Boolean read FNullTimed write FNullTimed;

  public
    { *** Paramètres *** }

    { --- Delai en ms pour la validation du Eye Wait click  }
    property WaitClickDelay: Integer read GetWaitClickDelay write SetWaitClickDelay;

    { --- Delai en ms pour la validation du Eye View click  }
    property ViewClickDelay: Cardinal read GetViewClickDelay write SetViewClickDelay;

    { --- Delai d'affichage en ms du changement d'état pour le Eye Wait click }
    property WaitLagDelay: Integer read GetWaitLagDelay write SetWaitLagDelay;

    { --- Multi-click en mode Transitory si NullTimed }
    property TransitoryMultiClick: Boolean read GetTransitoryMultiClick write SetTransitoryMultiClick;

    { --- Delai en ms du relâchement du click si mode persistent }
    property ZoneUpDelay: Integer read GetZoneUpDelay write SetZoneUpDelay;

    { --- Couleur de fond d'une zone }
    property BackColor: TColor read GetBackColor write SetBackColor;

    { --- Couleur de sélection (remplissage) d'une zone}
    property SeleColor: TColor read GetSeleColor write SetSeleColor;

    { --- Couleur de Validation d'une zone }
    property ValiColor: TColor read GetValiColor write SetValiColor;

    { --- Bordure pour toutes les zones }
    property Border: Boolean read GetBorder write SetBorder;

    constructor Create;
    destructor Destroy; override;


    property OnUpdate: TNotifyEvent read FUpdate write FUpdate;
    property TrigNotify: TTrigNotify read FTrigNotify write FTrigNotify;
    property OnBeforeSelect: TIdxNotify read FBeforeSelect  write FBeforeSelect;
    property OnNullSelect: TTagNotify read FNullSelect write FNullSelect;
    property OnUnSelect: TTagNotify read FOnUnSelect write FOnUnSelect;

  end;

  TLauncher = class
  private
    procedure Initialize;
    procedure Finalize;
  public
    class procedure AppInitialize;
    class procedure AppFinalize;
  end;

function GetScreenBounds(const Panel: TPanel): TRect;

var
  CallFifo     : TCallFifo = nil;
  CallSurveyor : TCallSurveyor;
  AreaPanels   : TAreaPanels;


implementation

uses
  uRegistry;

function GetScreenBounds(const Panel: TPanel): TRect;
var originpoint: TPoint;
begin
  originpoint := Panel.ClientToScreen(point(0,0));
  result := TRect.Create(originpoint,Panel.Width,Panel.Height)
end;

procedure FormatPanel(const Panel: TPanel; const Border: Boolean = False);
begin
  with Panel do begin
    Anchors                := [];
    if Border then BevelOuter := bvRaised else BevelOuter := bvNone;
    Color                  := $00232323;
    Ctl3D                  := False;
    DoubleBuffered         := True;
    Enabled                := True;
    FullRepaint            := False;
    Locked                 := False;
    ParentBackground       := False;
    ParentBiDiMode         := False;
    ParentColor            := False;
    ParentCustomHint       := False;
    ParentDoubleBuffered   := False;
    ParentFont             := False;
    ParentShowHint         := False;
    ShowCaption            := True;
    TabStop                := False;
    UseDockManager         := False
  end
end;

{ TCallFifo }

constructor TCallFifo.Create;
begin
  inherited Create;
  FMutex := TMutex.Create(False)
end;

destructor TCallFifo.Destroy;
begin
  FMutex.Free;
  inherited
end;

class procedure TCallFifo.Finalize;
begin
  if Assigned(CallFifo) then FreeAndNil(CallFifo)
end;

class procedure TCallFifo.Initialize;
begin
  if not Assigned(CallFifo) then CallFifo := TCallFifo.Create
end;

function TCallFifo.Peek: Integer;
begin
  FMutex.Acquire;
  try
    Result := NULL_STACK;
    if Self.Count > 0 then begin
      try Result := StrToInt( Self.Strings[0] ) except Result := NULL_STACK end;
      Self.Delete(0);
      Sleep(60)
    end
  finally
    FMutex.Release
  end
end;

function TCallFifo.Poke(const StrValue: string): Integer;
begin
  FMutex.Acquire;
  try
    Result := Self.Add( StrValue )
  finally
    FMutex.Release
  end
end;

function TCallFifo.Poke(const IntValue: Integer): Integer;
begin
  FMutex.Acquire;
  try
    Result := Self.Add( Format('%d', [IntValue]))
  finally
    FMutex.Release
  end
end;

{ TCallSurveyor }

constructor TCallSurveyor.Create(const APile: TCallFifo; const AAreas : TAreaPanels);
begin
  Inherited Create( True );
  {Must be started}
  ThPile          := APile;
  ThAreas         := AAreas;
  FreeOnTerminate := True
end;

procedure TCallSurveyor.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 1 )
  end
end;

class procedure TCallSurveyor.Finnalize;
begin
  if Assigned(CallSurveyor) then CallSurveyor.Terminate
end;

class procedure TCallSurveyor.Initialize;
begin
 if not Assigned(CallSurveyor) then
   CallSurveyor := TCallSurveyor.Create(CallFifo, AreaPanels)
end;

procedure TCallSurveyor.Process;
var
  X: Integer;
begin
  X := ThPile.Peek;
  if (X <> NULL_STACK) then with ThAreas do begin
    if not NullEnable then SelectMode := FPanelsModes[X - 1]
      else SelectMode := FPanelsModes[X];
    case BlinkType of
      bt_transitory : ItemIndex := X;
      bt_persistent : ItemIndex := X - 1;
    end
  end
end;

procedure TCallSurveyor.ThDelay(ms: Cardinal);
var
  S : Cardinal;
begin
  S := GetTickCount + ms;
  with Application do repeat
    Sleep( 10 );
  until Self.Terminated or Terminated or (GetTickCount > S)
end;

{ TLauncher }

class procedure TLauncher.AppFinalize;
begin
   with TLauncher.Create do
  try
    Finalize
  finally
    Free
  end
end;

class procedure TLauncher.AppInitialize;
begin
  with TLauncher.Create do
  try
    Initialize
  finally
    Free
  end
end;

procedure TLauncher.Finalize;
begin
  TCallSurveyor.Finnalize;
  TCallFifo.Finalize
end;

procedure TLauncher.Initialize;
begin
  TCallFifo.Initialize;
  TCallSurveyor.Initialize
end;

{ TAreaPanels }

function TAreaPanels.ActiveCount: Integer;
begin
  RetrieveActivPanels;
  Result := Length(FActivPanels)
end;

procedure TAreaPanels.RetrieveActivPanels;
var
  i     : Integer;
  index : Integer;
begin
  SetLength(FActivPanels, 0);
  for i := Low(Panels) to High(Panels) do if Panels[i].Visible then begin
    index := Length(FActivPanels);
    SetLength(FActivPanels, index + 1);
    FActivPanels[index] := Panels[i]
  end
end;

procedure TAreaPanels.AddPanel(const Panel: TPanel);
var
  Count : Integer;
  index : Integer;
begin
  FormatPanel( Panel, Border );
  Count := Length(FPanels) + 1;
  index := Count - 1;
  with Panel do Tag := Count;
  { --- Enregister la zone }
  SetLength(FPanels, Count);
  FPanels[index] := Panel;
  { --- La zone n'est pas sélectionnée par défaut }
  SetLength(FSelected, Count);
  FSelected[index] := False
end;

procedure TAreaPanels.AddTag(const Value: Integer);
begin
  SetLength(FTags, Length(FTags) + 1);
  FTags[Length(FTags) - 1] := Value
end;

procedure TAreaPanels.AddVisible(const Value: Boolean);
begin
  SetLength(FVisiblePanel, Length(FVisiblePanel) + 1);
  FVisiblePanel[Length(FVisiblePanel) - 1] := Value
end;

procedure TAreaPanels.BoxUpdate(const Raw, Col, ATag: Integer; ACaption: string;
  AVisible: Boolean; AMode: TSelectMode; ANull: Boolean);
var
  index: Integer;
begin
  index := (Raw - 1) * 7 + Col - 1;
  try
    FTags         [index] := ATag;
    FVisiblePanel [index] := AVisible;
    FPanelsModes  [index] := AMode;
    FNullPanels   [index] := ANull;
    FSelected     [index] := False;
    with FPanels[index] do begin
      Caption := ACaption;
      Visible := AVisible
    end
  except
  end
end;

procedure TAreaPanels.Add(const Panel: TPanel; const ATag: Integer;
  const AMode: TSelectMode; const ANull: Boolean);
begin
  AddPanel   ( Panel );
  AddMode    ( AMode );
  AddNull    ( ANull );
  AddTag     ( ATag  );
  AddVisible ( False )
end;

procedure TAreaPanels.AddMode(const AMode: TSelectMode);
begin
  SetLength(FPanelsModes, Length(FPanelsModes) + 1);
  FPanelsModes[Length(FPanelsModes) - 1] := AMode
end;

procedure TAreaPanels.AddNull(const Value: Boolean);
begin
  SetLength(FNullPanels, Length(FNullPanels) + 1);
  FNullPanels[Length(FNullPanels) - 1] := Value
end;

procedure TAreaPanels.CaptionUpdate(const Raw: Integer;
  const Values: array of string);
var
  i, Max : Integer;
begin
  Max := Min(High(Values), 6);
  for i := 0 to Max do FPanels[(Raw - 1) * 7 + i ].Caption := Values[i]
end;

procedure TAreaPanels.Colorize(const AIndex: Integer; const Valid: Boolean);
var
  AColor : TColor;

  function IsNullPanel: Boolean; begin
    Result := FNullPanels[AIndex] and
            ( Integer(FSelectMode) > Integer(sm_directnulltimed) );
    { --- La zone est nulle alors réinitialiser le sélecteur de zones }
    if Result then begin
      FOldIndex := -1;
      if Assigned(FNullSelect) then FNullSelect(Self, FTags[AIndex])
    end
  end;

  function CanProcess: Boolean; begin
    Result := (AIndex > -1) and (AIndex < Length(FPanels)) and not IsNullPanel;
    if Valid then AColor := ValiColor else AColor := SeleColor
  end;

  procedure DoColorize(const SColor: TColor); begin
    with FPanels[AIndex] do Color := SColor;
    Update
  end;

  procedure ZoneUp; begin
    { --- Relâchemment visuel du contrôle }
    Sleep      ( ZoneUpDelay );
    DoColorize ( SeleColor   );
    FOldIndex := FItemIndex;
    { --- Envoi du signal }
    Trigger( mt_zoneup )
  end;

  procedure CheckSelected; begin
    if (SelectMode = sm_blinknotbacktimed) and (FPanels[AIndex].Color = ValiColor) then begin
       { --- Si la zone est sélectionnée alors lancer le timer de désélection }
       FSelected[AIndex] := True;
       FSelectIndex      := AIndex;
       StartTimerSelected
    end
  end;

  procedure Process; begin
    { --- Valide en appliquant la couleur selon le mode d'activation }
    if FSelected[AIndex] then AColor := ValiColor;
    DoColorize( AColor );
    case Valid of
      True : if FBlinkBack then ZoneUp else FOldIndex  := -1;
      else FOldIndex := FItemIndex
    end;
    CheckSelected
  end;

begin
  if CanProcess then Process
end; {Colorize}

constructor TAreaPanels.Create;
begin
  inherited Create;
  Reset;
  FBlinkType    := bt_transitory;
  FCanBlink     := False;
  FBlinkBack    := False;
  FNullEnable   := False;
  FValidTimer   := TTimer.Create(Application.MainForm);
  with FValidTimer do begin
    Enabled  := False;
    Interval := WaitClickDelay;
    OnTimer  := TimerProcess
  end;

  FSelectTimer  := TTimer.Create(Application.MainForm);
  with FSelectTimer do begin
    Enabled  := False;
    Interval := WaitClickDelay;
    Ontimer  := SelectProcess
  end;

  FKeyMessageSender := KeyMessageSender;
end;

destructor TAreaPanels.Destroy;
begin

  inherited;
end;

function TAreaPanels.GetBackColor: TColor;
begin
  Result := TColor( KeyReadInt(ParamKey, 'ColorBack', Integer($00232323)) )
end;

function TAreaPanels.GetBorder: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'AreaBorder', True)
end;

function TAreaPanels.GetSeleColor: TColor;
begin
  Result := TColor( KeyReadInt(ParamKey, 'ColorSele', Integer($000080FF)) )
end;

function TAreaPanels.GetTransitoryMultiClick: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'TransitoryMultiClick', False)
end;

function TAreaPanels.GetValiColor: TColor;
begin
  Result := TColor( KeyReadInt(ParamKey, 'ColorVali', Integer($00FF8000)) )
end;

function TAreaPanels.GetViewClickDelay: Cardinal;
begin
  Result := KeyReadInt(ParamKey, 'EyeViewClickDelai', 350)
end;

function TAreaPanels.GetVisible(index: Integer): Boolean;
begin
  try
    Result := FVisiblePanel[index]
  except
    Result := False
  end
end;

function TAreaPanels.GetWaitClickDelay: Integer;
begin
  Result := KeyReadInt(ParamKey, 'EyeWaitClickDelai', 1200)
end;

function TAreaPanels.GetWaitLagDelay: Integer;
begin
  Result := KeyReadInt(ParamKey, 'EyeWaitLagDelai', 400)
end;

function TAreaPanels.GetZoneUpDelay: Integer;
begin
  Result := KeyReadInt(ParamKey, 'ZoneUpDelai', 200)
end;

function TAreaPanels.IndexOfPanel(const Value: TPanel): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := Low(FPanels) to High(FPanels) do if FPanels[i] = Value then begin
    Result := i;
    Break
  end
end;


function TAreaPanels.IndexOfTag(const Value: Integer): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := Low(FTags) to High(FTags) do if FTags[i] = Value then begin
    Result := i;
    Break
  end
 end;

procedure TAreaPanels.NullUpdate(const Raw: Integer;
  const Values: array of Boolean);
var
  i, Max : Integer;
begin
  Max := Min(High(Values), 6);
  for i := 0 to Max do FNullPanels[(Raw - 1) * 7 + i ] := Values[i]
end;

procedure TAreaPanels.PanelChangeBorder(const Panel: TPanel);
var
  index : Integer;
begin
  index := IndexOfPanel(Panel);
  if FNullPanels[index] then begin
    with Panel do BevelOuter := bvNone
  end else with Panel do
    if Border then BevelOuter := bvRaised else BevelOuter := bvNone
end;

procedure TAreaPanels.PanelChangeVisible(const Panel: TPanel);
begin
  with Panel do
  try
    Visible := FVisiblePanel[ IndexOfPanel(Panel) ]
  except
  end
end;

procedure TAreaPanels.ProcessWithPanels(const Method: TPnlNotify);
var
  i : Integer;
begin
  for I := Low(FPanels) to High(Panels) do
    if Assigned(Method) then Method( FPanels[i] )
end;

procedure TAreaPanels.Repaint;
begin
  ProcessWithPanels( PanelChangeVisible )
end;

procedure TAreaPanels.Reset;
begin
  SetLength(FPanels,       0);
  SetLength(FPanelsModes,  0);
  SetLength(FNullPanels,   0);
  SetLength(FSelected,     0);
  SetLength(FTags,         0);
  SetLength(FVisiblePanel, 0)
end;

function TAreaPanels.Selected(const index: Integer): Boolean;
begin
  try
    Result := FSelected[index]
  except
    Result := False
  end
end;

procedure TAreaPanels.SelectProcess(Sender: TObject);

  procedure DeSelect; begin
    if FSelectIndex <> -1 then
    try
      FSelected[FSelectIndex]     := False;
      FPanels[FSelectIndex].Color := SeleColor;
      FOldIndex                   := FSelectIndex;
      FBlinkTick                  := ViewClickDelay;
      if Assigned(FOnUnSelect) then FOnUnSelect(Self, FTags[FSelectIndex])
    except
    end
  end;

begin
  StopTimerSelected;
  try
    DeSelect
  finally
    FSelectIndex := -1
  end;
end;

procedure TAreaPanels.SelModeUpdate(const Raw: Integer;
  const Values: array of TSelectMode);
var
  i, Max : Integer;
begin
  Max := Min(High(Values), 6);
  for i := 0 to Max do FPanelsModes[(Raw - 1) * 7 + i ] := Values[i]
end;

{SelectProcess}

procedure TAreaPanels.SetBackColor(const Value: TColor);
begin
  KeyReadInt(ParamKey, 'ColorBack', Integer(Value))
end;

procedure TAreaPanels.SetBorder(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'AreaBorder', Value);
  ProcessWithPanels( PanelChangeBorder )
end;

procedure TAreaPanels.SetItemIndex(const Value: Integer);

  function ClickValidate: Boolean; begin
    Result := FCanBlink and (FItemIndex = FOldIndex) and
            ( GetTickCount > ViewClickDelay + FBlinkTick )
//            ( GetTickCount - FBlinkTick > ViewClickDelay )
  end;

begin
  { --- Event to prepare index change }
  if Assigned(FBeforeSelect) then FBeforeSelect(Self, FTags[FItemIndex], FTags[Value]);
  { --- Assign new value }
  FItemIndex := Value;
  { --- Stopper le Timer si nécessaaire }
  StopTimer;
  { --- Stopper le Timer de désélection }
  StopTimerSelected;
  { --- Reset de la coloration des zones }
  UnColorize;
  { --- Traitement de la coloration }
  Colorize(FItemIndex, ClickValidate);
  { --- Enregistrer le tick processeur }
  FBlinkTick := GetTickCount;
  { --- Ne démarre que si NullEnable actif }
  StartTimer;
  { --- Routage du signal de validation }
  Trigger( mt_direct )
end; {SetItemIndex}

procedure TAreaPanels.SetSeleColor(const Value: TColor);
begin
  KeyReadInt(ParamKey, 'ColorSele', Integer(Value))
end;

procedure TAreaPanels.SetSelectMode(const Value: TSelectMode);

  procedure Activate(const indic1, indic2, indic3, indic4: Boolean); begin
    case indic1 of
      True : FBlinkType := bt_persistent;
       else  FBlinkType := bt_transitory
    end;
    FNullTimed := indic2;
    FCanBlink  := indic3;
    FBlinkBack := indic4
  end;

begin
  FSelectMode := Value;
  case Value of
    sm_none               : Activate(True,  False, True,  True);
    sm_directnull         : Activate(False, False, False, False);
    sm_directnulltimed    : Activate(False, True,  False, False);
    sm_directnotnull      : Activate(True,  False, False, False);
    sm_blinknotbacktimed  : Activate(True,  False, True,  False);
    sm_blinknotback       : Activate(True,  False, True,  False);
    sm_blinkback          : Activate(True,  False, True,  True);
  end
end; {SetSelectMode}

procedure TAreaPanels.SetTransitoryMultiClick(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'TransitoryMultiClick', Value)
end;

procedure TAreaPanels.SetValiColor(const Value: TColor);
begin
  KeyReadInt(ParamKey, 'ColorVali', Integer(Value))
end;

procedure TAreaPanels.SetViewClickDelay(const Value: Cardinal);
begin
  KeyWrite(ParamKey, 'EyeViewClickDelai', Value)
end;

procedure TAreaPanels.SetVisible(index: Integer; const Value: Boolean);
begin
  try
    FVisiblePanel[index] := Value;
    ProcessWithPanels( PanelChangeVisible )
  except
  end
end;

procedure TAreaPanels.SetWaitClickDelay(const Value: Integer);
begin
  KeyWrite(ParamKey, 'EyeWaitClickDelai', Value)
end;

procedure TAreaPanels.SetWaitLagDelay(const Value: Integer);
begin
  KeyWrite(ParamKey, 'EyeWaitLagDelai', Value)
end;

procedure TAreaPanels.SetZoneUpDelay(const Value: Integer);
begin
  KeyWrite(ParamKey, 'ZoneUpDelai', Value)
end;

procedure TAreaPanels.StartTimer;
begin
  if FNullEnable and NullTimed then with FValidTimer do begin
    FTimeIndex := ItemIndex;
    Enabled    := False;
    Sleep(10);
    Interval   := WaitClickDelay;
    Enabled    := True
  end
end;

procedure TAreaPanels.StartTimerSelected;
begin
  with FSelectTimer do begin
    Enabled    := False;
    Sleep(10);
    Interval   := WaitClickDelay;
    Enabled    := True
  end
end;

procedure TAreaPanels.StopTimer;
begin
  with FValidTimer do Enabled := False
end;

procedure TAreaPanels.StopTimerSelected;
begin
  with FSelectTimer do Enabled := False
end;

procedure TAreaPanels.TagsUpdate(const Raw: Integer;
  const Values: array of Integer);
var
  i, Max : Integer;
begin
  Max := Min(High(Values), 6);
  for i := 0 to Max do FTags[(Raw - 1) * 7 + i ] := Values[i]
end;

procedure TAreaPanels.TimerProcess(Sender: TObject);

  function WaitBlinkValidate: Boolean; begin
    Result := FNullEnable and (ItemIndex = FTimeIndex) and
            ( FPanels[ItemIndex].Color = SeleColor )
  end;

  procedure DoColorize(const AColor: TColor); begin
    FPanels[ItemIndex].Color := AColor;
    Update
  end;

  procedure DoValidation; begin
    DoColorize ( ValiColor    );
    Sleep      ( WaitLagDelay );
    DoColorize ( SeleColor    );
    FOldIndex := -1
  end;

  function Validation(const CheckValid: Boolean):Boolean; begin
    Result := CheckValid;
    if Result then try DoValidation except Result := False end
  end;

  procedure TimerRestart(const Check: Boolean); begin
    if Check then StartTimer
  end;

begin
  StopTimer;
  try
    if Validation( WaitBlinkValidate ) then Trigger( mt_timer )
  finally
    TimerRestart( TransitoryMultiClick )
  end
end; {TimerProcess}

procedure TAreaPanels.Trigger(const From: TModeTrigger);

  function IsDirect:Boolean; begin
    Result := From = mt_direct
  end;

  function IsTimered: Boolean; begin
    Result := From = mt_timer
  end;

  function IsZoneup: Boolean; begin
    Result := From = mt_zoneup
  end;

  function IsValidate: Boolean; begin
    Result := FPanels[ItemIndex].Color = ValiColor
  end;

  function IsEnable: Boolean; begin
    try
      Result := IsDirect and IsValidate
    except
      Result := False
    end
  end;

begin
  KeyMessageSender.KeyUp;
  if (ItemIndex > NULL_STACK) and Assigned(FTrigNotify) then
    case SelectMode of
      { --- mode Transitory }
      sm_directnull        : if IsDirect  then FTrigNotify(Self, False, FTags[ItemIndex]);
      sm_directnulltimed   : if IsTimered then FTrigNotify(Self, True,  FTags[ItemIndex]);
      { --- mode Persistent }
      sm_directnotnull     : if IsDirect  then FTrigNotify(Self, False, FTags[ItemIndex]);
      sm_blinknotbacktimed : if IsEnable  then FTrigNotify(Self, True,  FTags[ItemIndex]);
      sm_blinknotback      : if IsEnable  then FTrigNotify(Self, False, FTags[ItemIndex]);
      sm_blinkback         : if IsZoneup  then FTrigNotify(Self, True,  FTags[ItemIndex]);
    end
end; {Trigger}

{SetSelectMode}

procedure TAreaPanels.UnColorize;
var
  i : Integer;
begin
  for i := 0 to Length(FPanels)-1 do with FPanels[i] do
    if FSelected[i] then Color := ValiColor else Color := BackColor
end;

procedure TAreaPanels.Unselect;
begin
  FOldIndex := -1;
  UnColorize
end;

procedure TAreaPanels.Update;
begin
  if Assigned(FUpdate) then FUpdate(Self)
end;

procedure TAreaPanels.VisibleUpdate(const Raw: Integer;
  const Values: array of Boolean; Update: Boolean);
var
  i, Max : Integer;
begin
  Max := Min(High(Values), 6);
  for i := 0 to Max do FVisiblePanel[(Raw - 1) * 7 + i ] := Values[i];
  if Update then ProcessWithPanels( PanelChangeVisible )
end;

initialization
  AreaPanels := TAreaPanels.Create
finalization
  AreaPanels.Free
end.
