unit uAreaTobii;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, EyeXHost, Vcl.ExtCtrls, uEyeXThread,
  Winapi.mmSystem, Winapi.shellapi, SendKey32, KeysDef, EliteBindingsTools, Clipbrd,
  uXMLDico, Datasnap.DBClient, uEliteManager, uStatusReader;

type
  TKeyboardContext = class;
  TEliteContext = class;

  TKeyboardKind    = (kk_alpha, kk_num, kk_special, kk_navigation, kk_critik);
  TContextNotify   = procedure of object;
  TGearTypeNotify  = procedure (const Mode: TLandingGearType) of object;
  TCockpitNotify   = procedure (const Mode : TCockpitModeType) of object;
  TIntegerNotify   = procedure (Value : Byte; Tms : Integer = 0) of object;
  TIntNotify       = procedure (Value : Byte) of object;
  TAreaTobii = class(TComponent)
  private
    FForm             : TForm;
    FContext          : TKeyboardContext;
    FElite            : TEliteContext;
    FAreasUpdate      : TNotifyEvent;
    FDico             : TXmlAlternative;
    FEliteManager     : TEliteManager;
    FEliteStatus      : TEliteStatus;
    FKeyMessageSender : TKeyMessageSender;

    function  FormRetrieve: TForm;
    procedure Initialize;
    procedure DoOnTriggerNotify(Sender: TObject; KeyUp: Boolean; ATag: Integer);
    procedure DoOnNullSelect(Sender: TObject; ATag: Integer);
    procedure DoOnBeforeSelect(Sender: TObject; OldValue, NewValue: Integer);
    procedure DoOnUnSelect(Sender: TObject; ATag: Integer);

    procedure DoLaunchElite;

  private
    FBorder: Boolean;
    { --- Ascesseurs / mutateurs }
    function  GetPanel(index: Integer): TPanel;
    function  GetCaption(index: Integer): string;
    procedure SetCaption(index: Integer; const Value: string);
    function  GetSelected(index: Integer): Boolean;
    function  GetAreas: TAreaPanels;
    procedure SetBorder(const Value: Boolean);

  public
    { --- Création des zones }
    procedure Add(const Panel: TPanel; const ATag: Integer; const AMode: TSelectMode; const ANull: Boolean = False);

    { --- Paramétrage des zones }
    procedure CaptionUpdate(const Raw: Integer; const Values: array of string);
    procedure VisibleUpdate(const Raw: Integer; const Values: array of Boolean; Update: Boolean = False);
    procedure SelModeUpdate(const Raw: Integer; const Values: array of TSelectMode);
    procedure TagsUpdate(const Raw: Integer; const Values: array of Integer);
    procedure NullUpdate(const Raw: Integer; const Values: array of Boolean);
    { --- Paramétrage d'une zone dans la matrice écran }
    procedure BoxUpdate(const Raw, Col, ATag: Integer; ACaption: string; AVisible: Boolean; AMode: TSelectMode; ANull: Boolean);

    procedure SetEliteStatus(const Value: TEliteStatus);

    procedure Repaint;
    procedure UpdateZone(const AreasContext: TContextNotify);
    { --- Reset context of areas }
    procedure ResetAreas;

    { --- Actions on external components or application }
    procedure SendSignal(const k1: string; const k2 : string = ''; const k3: string = '');

    { --- Accès en lecture seule à l'instance du TAreaPanels }
    property Areas: TAreaPanels read GetAreas;
    { --- Accès en lecture seule aux contextes }
    property Context: TKeyboardContext read FContext;
    { --- Accès en lecture seule aux contextes Elite}
    property Elite: TEliteContext read FElite;

    property Panel[index: Integer]: TPanel read GetPanel;
    { --- Lit ou modifie le caption du panel à l'indice index }
    property Caption[index: Integer]: string read GetCaption write SetCaption;
    { --- True si la zone index est sélectionnée }
    property Selected[index: Integer]: Boolean read GetSelected;
    { --- Bordure pour toutes les zones }
    property Border: Boolean read FBorder write SetBorder;
    { --- External update write redraw areas to tobii }
    property OnUpdateAreas: TNotifyEvent read FAreasUpdate write FAreasUpdate;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TEliteContext = class
  private
    FOwner    : TAreaTobii;
    { --- Local Tools }
    procedure StepSelector(const ATag: Integer);
    procedure DoClicUnique(const Method: TContextNotify); overload;
    procedure DoClicUnique(const Method: TGearTypeNotify); overload;
    procedure DoClicUnique(const Method: TCockpitNotify); overload;
    procedure DoClic(const Method: TContextNotify); overload;
    procedure DoClic(const Method: TIntNotify; Value: Integer); overload;
    procedure DoClic(const Method: TIntegerNotify; Value: Integer); overload;
    { --- Elite context }
    procedure Context_Elite;
    procedure Context_Pause;
    procedure Context_EliteMenu;
    procedure Context_StepSelect;
    procedure Context_EliteDrive;
    procedure Context_Func;
    procedure Context_Func0;
    procedure Context_Func1;
    procedure Context_Func2;
    procedure Context_Func3;
    procedure Context_Func4;
    procedure Context_Func5;
    {TODO}
    procedure Context_GalaxyMap;
    procedure Context_System_Map;
    procedure Context_ACS;

    procedure Step_display;
    procedure Pause_display;
    procedure Menu_display;
    procedure Drive_display;
    procedure Func_display;
    procedure GalaxyMap_display;

    procedure Switch;
    procedure AssignFunc(const Index: Integer);
  private
    { --- Ascesseurs / Mutateurs }
    function  GetElm: TEliteManager;
    function  GetStep: Integer;
    procedure SetStep(const Value: Integer);
    function  GetCurrContextTag: Integer;
    procedure SetCurrContextTag(const Value: Integer);
    function  GetIndexFunc: Integer;
    procedure SetIndexFunc(const Value: Integer);
    function  GetIndexMode: Integer;
    procedure SetIndexMode(const Value: Integer);
    function  GetSubIndex: Integer;
    procedure SetSubIndex(const Value: Integer);
    function  GetIndexGMMode: Integer;
    procedure SetIndexGMMode(const Value: Integer);
    function  GetSubGMIndex: Integer;
    procedure SetSubGMIndex(const Value: Integer);
    function  GetGMAllSlide: Boolean;
    procedure SetGMAllSlide(const Value: Boolean);
  public
    { --- Elite notifier }
    procedure EliteNotify(const ATag: Integer);
    procedure DoMapGalaxyShow;

    procedure PauseBack;
    procedure PauseEnable;
    procedure DoTarget12h;
    procedure DoAutoBreak;
    procedure DoVRSTurret;
    procedure DoShipDismissRecall;
    procedure DoOrderPanel;
    procedure DoStartFighter;
    procedure DoBack;
    procedure DoNavModeChange;
    procedure DoNavSlide;
    procedure DoNavNord;
    procedure DoNavSud;
    procedure DoNavEst;
    procedure DoNavOuest;
    procedure DoNavPere;
    procedure DoNavFils;
    { --- Galasy map notifier actions }
    procedure DoGMSlide;
    procedure DoGMSteps;
    procedure DoGMMenu;
    procedure DoGMGoHome;
    procedure DoGMBack;
    procedure DoGMAllSlide;
    procedure DoGMReward;
    procedure DoGMUp;
    procedure DoGMUpRotate;
    procedure DoGMLeft;
    procedure DoGMRight;
    procedure DoGMDown;
    procedure DoGMDownRotate;
    procedure DoGMZoomIn;
    procedure DoGMLeftRotate;
    procedure DoGMForward;
    procedure DoGMRightRotate;
    procedure DoGMZoomOut;

    property Elm: TEliteManager read GetElm;
    property Step: Integer read GetStep write SetStep;
    property CurrContextTag: Integer read GetCurrContextTag write SetCurrContextTag;
    property IndexFunc: Integer read GetIndexFunc write SetIndexFunc;
    { --- Indexes for Ship drive }
    property IndexMode: Integer read GetIndexMode write SetIndexMode;
    property SubIndex: Integer read GetSubIndex write SetSubIndex;
    { --- Indexes for Galaxy map }
    property IndexGMMode: Integer read GetIndexGMMode write SetIndexGMMode;
    property SubGMIndex: Integer read GetSubGMIndex write SetSubGMIndex;
    property GMAllSlide: Boolean read GetGMAllSlide write SetGMAllSlide;

    constructor Create(const AOwner: TAreaTobii);
  end;

  TKeyboardContext = class
  private
    FOwner    : TAreaTobii;
    FTarget   : THandle;
    FBuffer   : string;
    FCall     : Boolean;
    FLastWord : string;
    function  GetCar(const Car: Char):Char;
  private
    { --- Send message text to external component or application }
    procedure SendTextTo(const ASt: string);
    { --- Send keyboard combinaison to external component or application }
    procedure SendSignal(const k1: string; const k2 : string = ''; const k3: string = '');
    { --- Send keyboard combinaison for internal component of keyboard }
    procedure SendMsgTo(const k1: string; const k2 : string = ''; const k3: string = '');
    procedure SendMsgToEx(const k1: string; const k2 : string = ''; const k3: string = '');
    procedure SendMsgToA(const k1: string; const k2 : string = ''; const k3: string = '');
    { --- Select, copy, cut paste text selection }
    procedure SelectAll;
    procedure Copy_;
    procedure Cut_;
    procedure Paste_;
  private
    { --- Keyboard methods }
    procedure DoUp;
    procedure DoDown;
    procedure DoHome;
    procedure DoEnd;
    procedure DoDocHome;
    procedure DoDocEnd;
    procedure DoPrevWord;
    procedure DoNextWord;
    procedure DoPageUp;
    procedure DoPageDown;
    procedure DoSelLeft;
    procedure DoSelRight;
    procedure DoSelHome;
    procedure DoSelEnd;
    procedure DoSelPageUp;
    procedure DoSelPageDown;
    procedure DoSelWordLeft;
    procedure DoSelWordRight;
    procedure DoSelTop;
    procedure DoSelBottom;
    procedure DoSelAll;
    procedure DoMenuUp;
    procedure DoMenuDown;
    procedure DoMenuLeft;
    procedure DoMenuRight;
    procedure DoMenuEnter;
    procedure DoMenuEchap;
    procedure DoAlt;
    procedure DoTab;
    procedure DataDico; //Temporaire
    procedure AltInsert(const Alt: Integer);
    procedure DoOnShift;
    procedure DoWithCar(const ATag: Integer);
    procedure AltDisplay;
    procedure DoBackSpace;
    procedure DoPrev;
    procedure DoNext;
    procedure DoAlphaRight;
    procedure DoAlphaLeft;
    { --- State indicator managment }
    procedure DisplaySpecial(AIndex: Integer);
    procedure DisplayNumpad;
    procedure DisplayAlpha;
    procedure DisplayNav(AIndex: Integer);
    { --- Sound indicators }
    procedure KeySound;
    procedure FunctionSound;
    { --- Buffer clean }
    procedure CleanBuffer;
    { --- Reset context of areas }
    procedure ResetAreas;
    { --- Refresh context for Alternative's datas }
    procedure RepaintKeyboardAlpha;
    { --- all Context of areas for application }
    procedure Context_Parameters;
    { --- Keyboard context }
    procedure Context_Keyboard;
    procedure Context_KeyboardMain;
    procedure Context_KeyboardLeft;
    procedure Context_KeyboardRight;
    procedure Context_KeyboardNumeric;
    procedure Context_KeyboardSpecials;
    procedure Context_KeyboardSpecials_1;
    procedure Context_KeyboardSpecials_2;
    procedure Context_KeyboardNavigation_1;
    procedure Context_KeyboardNavigation_2;
    procedure Context_KeyboardCritik;
    procedure Context_ResetAlternative;
    procedure Make_Alternative(ASt: string);

    procedure MainMenu_display;
  private
    { --- Ascesseurs / Mutateurs }
    function  GetCapsLock: Boolean;
    procedure SetCapsLock(const Value: Boolean);
    function  GetLeftKeyboard: Boolean;
    procedure SetLeftKeyboard(const Value: Boolean);
    function  GetKeyboardKind: TKeyboardKind;
    procedure SetKeyboardKind(const Value: TKeyboardKind);
    function  GetSpecialIndex: Integer;
    procedure SetSpecialIndex(const Value: Integer);
    function  GetNavIndex: Integer;
    procedure SetNavIndex(const Value: Integer);

  public
    { --- Areas definition call by mainform }
    procedure AreasDefine;
    { --- Display main context call by mainform }
    procedure Context_MainMenu;
    { --- Keyboard notifier }
    procedure KeyboardNotify(const ATag: Integer);

    { --- Capslock false <-> Upcase, true <-> LowCase }
    property CapsLock: Boolean read GetCapsLock write SetCapsLock;
    { --- Left keyboard visible if True else Right keyboard }
    property LeftKeyboard: Boolean read GetLeftKeyboard write SetLeftKeyboard;
    { --- Alpha or Numpad or Special or Cursor or Func : mode enabled }
    property KeyboardKind: TKeyboardKind read GetKeyboardKind write SetKeyboardKind;
    { --- 0,1,2 mode of specials }
    property SpecialIndex: Integer read GetSpecialIndex write SetSpecialIndex;
    { --- 0,1 Index of Func }
    property NavIndex: Integer read GetNavIndex write SetNavIndex;

    constructor Create(const AOwner: TAreaTobii);
  end;

implementation

uses
  Main, uRegistry, StrUtils;

type
  TAreaindexedChar = 91010..91092;
  TKeyCharArray = array[TAreaindexedChar] of char;

var
  KeyCharArray : TKeyCharArray = (
   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
   'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
   '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '.', '+', '-', '*', '/',
   '.', '@', '_', '(', ')',  ',', ';', ':', '[', ']', '\', '%', '$', '{', '}',
   '²', '&', '"', '''', '°', '=', '%', '!', '?', '#', '|', '>', '<', '£', 'µ',
   '§', 'É', 'È', 'Ê',  'Ù', 'Î', 'Ï', 'À', 'Â', 'Ô', 'Ç', ' ');

function Maj(const Car: char):char;
begin
  Result := #0;
  case IndexStr(Car, ['É', 'È', 'Ê',  'Ù', 'Î', 'Ï', 'À', 'Â', 'Ô', 'Ç']) of
    0 : Result := 'é';
    1 : Result := 'è';
    2 : Result := 'ê';
    3 : Result := 'ù';
    4 : Result := 'î';
    5 : Result := 'ï';
    6 : Result := 'à';
    7 : Result := 'â';
    8 : Result := 'ô';
    9 : Result := 'ç';
  end;
end;

function IsEliteRunning: Boolean;
begin
  Result :=  FindWindow( PWideChar(ELITE_CLASS), nil ) <> 0;
end;

procedure EliteForeGround;
begin
  try
    SetForegroundWindow( FindWindow( PWideChar(ELITE_CLASS), nil ) )
  except
  end;
end;

{ TAreaTobii }

procedure TAreaTobii.Add(const Panel: TPanel; const ATag: Integer;
  const AMode: TSelectMode; const ANull: Boolean);
begin
  Areas.Add(Panel, ATag, AMode, ANull)
end;

procedure TAreaTobii.BoxUpdate(const Raw, Col, ATag: Integer; ACaption: string;
  AVisible: Boolean; AMode: TSelectMode; ANull: Boolean);
begin
  Areas.BoxUpdate(Raw, Col, ATag, ACaption, AVisible, AMode, ANull)
end;

procedure TAreaTobii.CaptionUpdate(const Raw: Integer;
  const Values: array of string);
begin
  Areas.CaptionUpdate(Raw, Values)
end;

constructor TAreaTobii.Create(AOwner: TComponent);
begin
  inherited;
  FForm := FormRetrieve;
  Initialize
end;

destructor TAreaTobii.Destroy;
begin
  FContext.Free;
  FDico.Free;
  inherited;
end;

procedure TAreaTobii.DoLaunchElite;
begin
  with Context, Elite do UpdateZone( Context_Elite );
  EliteForeGround;
  with FEliteManager, FEliteStatus, Elite do
    if TGuiType(GuiFocus) = gt_galaxymap then begin
      MapGalaxy;
      Menu_display;
    end;

  with Context do FunctionSound;
end;

procedure TAreaTobii.DoOnBeforeSelect(Sender: TObject; OldValue,
  NewValue: Integer);
begin

end;

procedure TAreaTobii.DoOnNullSelect(Sender: TObject; ATag: Integer);
begin

end;

procedure TAreaTobii.DoOnTriggerNotify(Sender: TObject; KeyUp: Boolean;
  ATag: Integer);
begin
   with Context, Elite do case Atag of
     { --- Main actions }
     91000 : Application.MainForm.Close;
     91001 : UpdateZone( Context_Keyboard );
     91003 : DataDico;
     91004 : DoLaunchElite;
     91006 : UpdateZone( Context_Parameters );
     91007 : MainMenu_display;
     { --- Keyboard actions }
     91008..91156 : KeyboardNotify(ATag);
     { --- Elite actions }
     91200..91600 : EliteNotify(ATag);
   end
end;

procedure TAreaTobii.DoOnUnSelect(Sender: TObject; ATag: Integer);
begin

end;

function TAreaTobii.FormRetrieve: TForm;
var
  X : TComponent;
begin
  Result := nil;
  X      := Owner;
  while Assigned(X) and not (X is TForm) do X := X.Owner;
  if Assigned(X) and (X is TForm) then Result := TForm(X);
end;

function TAreaTobii.GetAreas: TAreaPanels;
begin
  Result := AreaPanels
end;

function TAreaTobii.GetCaption(index: Integer): string;
begin
  try
    Result := Panel[index].Caption
  except
    Result := EmptyStr
  end;
end;

function TAreaTobii.GetPanel(index: Integer): TPanel;
begin
  try
    Result := AreaPanels.Panels[index]
  except
    Result := nil
  end
end;

function TAreaTobii.GetSelected(index: Integer): Boolean;
begin
  Result := AreaPanels.Selected(index)
end;

procedure TAreaTobii.Initialize;
begin
  FKeyMessageSender := KeyMessageSender;
  FEliteManager     := EliteManager;
  FContext          := TKeyboardContext.Create(Self);
  FBorder           := AreaPanels.Border;
  with AreaPanels do begin
    OnBeforeSelect  := DoOnBeforeSelect;
    OnNullSelect    := DoOnNullSelect;
    TrigNotify      := DoOnTriggerNotify;
    OnUnSelect      := DoOnUnSelect;
  end;
  FDico := TXmlAlternative.Create;
  FElite := TEliteContext.Create(Self);
end;

procedure TAreaTobii.NullUpdate(const Raw: Integer;
  const Values: array of Boolean);
begin
  Areas.NullUpdate(Raw, Values)
end;

procedure TAreaTobii.Repaint;
begin
  Areas.Repaint;
end;

procedure TAreaTobii.ResetAreas;
begin
  with Context do ResetAreas
end;

procedure TAreaTobii.SelModeUpdate(const Raw: Integer;
  const Values: array of TSelectMode);
begin
  Areas.SelModeUpdate(Raw, Values)
end;

procedure TAreaTobii.SendSignal(const k1, k2, k3: string);
begin
  with FContext do SendSignal(k1, k2, k3);
end;

procedure TAreaTobii.SetBorder(const Value: Boolean);
begin
  FBorder      := Value;
  Areas.Border := Value
end;

procedure TAreaTobii.SetCaption(index: Integer; const Value: string);
begin
  try Panel[index].Caption := Value except end
end;

procedure TAreaTobii.SetEliteStatus(const Value: TEliteStatus);
begin
  FEliteStatus := Value
end;

procedure TAreaTobii.TagsUpdate(const Raw: Integer;
  const Values: array of Integer);
begin
  Areas.TagsUpdate(Raw, Values)
end;

procedure TAreaTobii.UpdateZone(const AreasContext: TContextNotify);
begin
  if Assigned(AreasContext) then begin
    AreasContext;
    Repaint;
    if Assigned(FAreasUpdate) then FAreasUpdate(Self)
  end;
  AreaPanels.Unselect
end;

procedure TAreaTobii.VisibleUpdate(const Raw: Integer;
  const Values: array of Boolean; Update: Boolean);
begin
  Areas.VisibleUpdate(Raw, Values, Update)
end;

{ TKeyboardContext }

procedure TKeyboardContext.AltDisplay;
var
  Alt: string;
begin
  with FOwner.FDico do begin
    if FBuffer <> EmptyStr then begin
      Alt := FindAltFrom(FBuffer, CapsLock);
      if Alt = EmptyStr then Alt := FindAltOrtho(FBuffer, CapsLock)
    end else begin
      Alt := FindAltPost(FLastWord, CapsLock);
    end
  end;
  Make_Alternative( Alt );
end;

procedure TKeyboardContext.AltInsert(const Alt: Integer);
var
  index : Integer;
  ASt   : string;
begin
  index     := Alt - 91150;
  ASt       := FOwner.Caption[index];
  FLastWord := ASt;
  ASt       := Copy(ASt, Length(FBuffer) + 1, Length(ASt) )+ ' ';
  FBuffer   := EmptyStr;
  SendTextTo(ASt);
  KeySound;
  AreaPanels.Unselect;
  RepaintKeyboardAlpha
end;

procedure TKeyboardContext.AreasDefine;
begin
  with FOwner, MainForm do begin
    Areas.NullEnable := False;
    Border           := False;

    Add(Panel1,   1, sm_none);  Add(Panel2,   2, sm_none);
    Add(Panel3,   3, sm_none);  Add(Panel4,   4, sm_none);
    Add(Panel5,   5, sm_none);  Add(Panel6,   6, sm_none);
    Add(Panel7,   7, sm_none);

    Add(Panel8,   8, sm_none);  Add(Panel9,   9, sm_none);
    Add(Panel10, 10, sm_none);  Add(Panel11, 11, sm_none);
    Add(Panel12, 12, sm_none);  Add(Panel13, 13, sm_none);
    Add(Panel14, 14, sm_none);

    Add(Panel15, 15, sm_none);  Add(Panel16, 16, sm_none);
    Add(Panel17, 17, sm_none);  Add(Panel18, 18, sm_none);
    Add(Panel19, 19, sm_none);  Add(Panel20, 20, sm_none);
    Add(Panel21, 21, sm_none);

    Add(Panel22, 22, sm_none);  Add(Panel23, 23, sm_none);
    Add(Panel24, 24, sm_none);  Add(Panel25, 25, sm_none);
    Add(Panel26, 26, sm_none);  Add(Panel27, 27, sm_none);
    Add(Panel28, 28, sm_none);

    Add(Panel29, 29, sm_none);  Add(Panel30, 30, sm_none);
    Add(Panel31, 31, sm_none);  Add(Panel32, 32, sm_none);
    Add(Panel33, 33, sm_none);  Add(Panel34, 34, sm_none);
    Add(Panel35, 35, sm_none);

    Add(Panel36, 36, sm_none);  Add(Panel37, 37, sm_none);
    Add(Panel38, 38, sm_none);  Add(Panel39, 39, sm_none);
    Add(Panel40, 40, sm_none);  Add(Panel41, 41, sm_none);
    Add(Panel42, 42, sm_none);  Add(Panel43, 0,  sm_none);
    //Panel43 ne sert que de biais
  end;
end;

procedure TKeyboardContext.CleanBuffer;
begin
  FLastWord := EmptyStr;
  FBuffer   := EmptyStr;
end;

procedure TKeyboardContext.Context_Keyboard;
begin
  FTarget := GetForegroundWindow;
  case LeftKeyboard of
    True : Context_KeyboardLeft;
      else Context_KeyboardRight
  end
end;

procedure TKeyboardContext.Context_KeyboardCritik;
begin
  Context_KeyboardMain;
  KeyboardKind := kk_critik;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91132, 'COPY',          True, sm_blinkback, False);
    BoxUpdate(3, 3, 91133, 'CUT',           True, sm_blinkback, False);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91138, '↑',             True, sm_blinkback, False);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);


    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91134, 'PASTE',         True, sm_blinkback, False);
    BoxUpdate(4, 4, 91139, '←',             True, sm_blinkback, False);
    BoxUpdate(4, 3, 91135, 'CANCEL',        True, sm_blinkback, False);
    BoxUpdate(4, 5, 91143, 'ESCAPE',        True, sm_blinkback, False);
    BoxUpdate(4, 6, 91141, '→',             True, sm_blinkback, False);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91144, 'ALT',           True, sm_blinkback, False);
    BoxUpdate(5, 3, 91145, 'TAB',           True, sm_blinkback, False);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91140, '↓',             True, sm_blinkback, False);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    Border := True;
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardLeft;
begin
  Context_KeyboardMain;
  LeftKeyboard := True;
  KeyboardKind := kk_alpha;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91010, GetCar('A'),             True, sm_blinkback, False);
    BoxUpdate(3, 3, 91035, GetCar('Z'),             True, sm_blinkback, False);
    BoxUpdate(3, 4, 91014, GetCar('E'),             True, sm_blinkback, False);
    BoxUpdate(3, 5, 91027, GetCar('R'),             True, sm_blinkback, False);
    BoxUpdate(3, 6, 91029, GetCar('T'),             True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91026, GetCar('Q'),             True, sm_blinkback, False);
    BoxUpdate(4, 3, 91028, GetCar('S'),             True, sm_blinkback, False);
    BoxUpdate(4, 4, 91013, GetCar('D'),             True, sm_blinkback, False);
    BoxUpdate(4, 5, 91015, GetCar('F'),             True, sm_blinkback, False);
    BoxUpdate(4, 6, 91016, GetCar('G'),             True, sm_blinkback, False);

    BoxUpdate(5, 2, 91032, GetCar('W'),             True, sm_blinkback, False);
    BoxUpdate(5, 3, 91033, GetCar('X'),             True, sm_blinkback, False);
    BoxUpdate(5, 4, 91012, GetCar('C'),             True, sm_blinkback, False);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    Border := True;
    AltDisplay
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardMain;
begin
  ResetAreas;
  with FOwner do begin

    BoxUpdate(2, 1, 91100, 'SHIFT',         True, sm_blinkback, False);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91104, 'LEFT',          True, sm_blinkback, False);
    BoxUpdate(2, 4, 91105, 'RIGHT',         True, sm_blinkback, False);
    BoxUpdate(2, 6, 91106, 'BACKSPACE',     True, sm_blinkback, False);
    BoxUpdate(2, 7, 91007, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91142, 'ENTER',         True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 7, 91108, 'CURSOR',        True, sm_blinkback, False);

    BoxUpdate(5, 1, 91136, 'FUNC',          True, sm_blinkback, False);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91102, 'SPECIALS',      True, sm_blinkback, False);

    BoxUpdate(6, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 2, 91009, 'LESS',          True, sm_blinkback, False);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91092, 'SPACE',         True, sm_blinkback, False);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91008, 'MORE',          True, sm_blinkback, False);
    BoxUpdate(6, 7, 91101, 'NUMPAD',        True, sm_blinkback, False);
  end
end;

procedure TKeyboardContext.Context_KeyboardNavigation_1;
begin
  Context_KeyboardMain;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91109, 'UP',            True, sm_blinkback, False);
    BoxUpdate(3, 3, 91110, 'HOME',          True, sm_blinkback, False);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91111, 'PAGE UP',       True, sm_blinkback, False);
    BoxUpdate(3, 6, 91113, 'DOC HOME',      True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91114, 'DOWN',          True, sm_blinkback, False);
    BoxUpdate(4, 3, 91115, 'END',           True, sm_blinkback, False);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91116, 'PAGE DOWN',     True, sm_blinkback, False);
    BoxUpdate(4, 6, 91118, 'DOC END',       True, sm_blinkback, False);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91119, 'PREV WORD',     True, sm_blinkback, False);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91120, 'NEXT WORD',     True, sm_blinkback, False);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    Border := True;
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardNavigation_2;
begin
  Context_KeyboardMain;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91121, 'SEL LEFT',      True, sm_blinkback, False);
    BoxUpdate(3, 3, 91122, 'SEL HOME',      True, sm_blinkback, False);
    BoxUpdate(3, 4, 91123, 'SEL PAGE UP',   True, sm_blinkback, False);
    BoxUpdate(3, 5, 91124, 'SEL WORD LEFT', True, sm_blinkback, False);
    BoxUpdate(3, 6, 91125, 'SEL TOP',       True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91126, 'SEL RIGHT',     True, sm_blinkback, False);
    BoxUpdate(4, 3, 91127, 'SEL END',       True, sm_blinkback, False);
    BoxUpdate(4, 4, 91128, 'SEL PAGE DOWN', True, sm_blinkback, False);
    BoxUpdate(4, 5, 91129, 'SEL WORD RIGHT',True, sm_blinkback, False);
    BoxUpdate(4, 6, 91130, 'SEL BOTTOM',    True, sm_blinkback, False);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 2, 91131, 'SELECT ALL',    True, sm_blinkback, False);
    BoxUpdate(5, 3, 91132, 'COPY',          True, sm_blinkback, False);
    BoxUpdate(5, 4, 91133, 'CUT',           True, sm_blinkback, False);
    BoxUpdate(5, 5, 91134, 'PASTE',         True, sm_blinkback, False);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    Border := True;
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardNumeric;
begin
  Context_KeyboardMain;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91049, '*',             True, sm_blinkback, False);
    BoxUpdate(3, 3, 91042, '7',             True, sm_blinkback, False);
    BoxUpdate(3, 4, 91043, '8',             True, sm_blinkback, False);
    BoxUpdate(3, 5, 91044, '9',             True, sm_blinkback, False);
    BoxUpdate(3, 6, 91047, '+',             True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91050, '/',             True, sm_blinkback, False);
    BoxUpdate(4, 3, 91039, '4',             True, sm_blinkback, False);
    BoxUpdate(4, 4, 91040, '5',             True, sm_blinkback, False);
    BoxUpdate(4, 5, 91041, '6',             True, sm_blinkback, False);
    BoxUpdate(4, 6, 91048, '-',             True, sm_blinkback, False);

    BoxUpdate(5, 2, 91045, '0',             True, sm_blinkback, False);
    BoxUpdate(5, 3, 91036, '1',             True, sm_blinkback, False);
    BoxUpdate(5, 4, 91037, '2',             True, sm_blinkback, False);
    BoxUpdate(5, 5, 91038, '3',             True, sm_blinkback, False);
    BoxUpdate(5, 6, 91046, '.',             True, sm_blinkback, False);

    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True);
    Border := True;
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardRight;
begin
  Context_KeyboardMain;
  LeftKeyboard := False;
  KeyboardKind := kk_alpha;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91034, GetCar('Y'),             True, sm_blinkback, False);
    BoxUpdate(3, 3, 91030, GetCar('U'),             True, sm_blinkback, False);
    BoxUpdate(3, 4, 91018, GetCar('I'),             True, sm_blinkback, False);
    BoxUpdate(3, 5, 91024, GetCar('O'),             True, sm_blinkback, False);
    BoxUpdate(3, 6, 91025, GetCar('P'),             True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91017, GetCar('H'),             True, sm_blinkback, False);
    BoxUpdate(4, 3, 91019, GetCar('J'),             True, sm_blinkback, False);
    BoxUpdate(4, 4, 91020, GetCar('K'),             True, sm_blinkback, False);
    BoxUpdate(4, 5, 91021, GetCar('L'),             True, sm_blinkback, False);
    BoxUpdate(4, 6, 91022, GetCar('M'),             True, sm_blinkback, False);

    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91031, GetCar('V'),             True, sm_blinkback, False);
    BoxUpdate(5, 5, 91011, GetCar('B'),             True, sm_blinkback, False);
    BoxUpdate(5, 6, 91023, GetCar('N'),             True, sm_blinkback, False);
    Border := True;
    AltDisplay;
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardSpecials;
begin
  Context_KeyboardMain;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91051, '.',             True, sm_blinkback, False);
    BoxUpdate(3, 3, 91052, '@',             True, sm_blinkback, False);
    BoxUpdate(3, 4, 91053, '_',             True, sm_blinkback, False);
    BoxUpdate(3, 5, 91054, '(',             True, sm_blinkback, False);
    BoxUpdate(3, 6, 91055, ')',             True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91056, ',',             True, sm_blinkback, False);
    BoxUpdate(4, 3, 91057, ';',             True, sm_blinkback, False);
    BoxUpdate(4, 4, 91058, ':',             True, sm_blinkback, False);
    BoxUpdate(4, 5, 91059, '[',             True, sm_blinkback, False);
    BoxUpdate(4, 6, 91060, ']',             True, sm_blinkback, False);

    BoxUpdate(5, 2, 91061, '\',             True, sm_blinkback, False);
    BoxUpdate(5, 3, 91062, '%',             True, sm_blinkback, False);
    BoxUpdate(5, 4, 91063, '$',             True, sm_blinkback, False);
    BoxUpdate(5, 5, 91064, '{',             True, sm_blinkback, False);
    BoxUpdate(5, 6, 91065, '}',             True, sm_blinkback, False);

    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);
    Border := True;
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardSpecials_1;
begin
  Context_KeyboardMain;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91066, '²',             True, sm_blinkback, False);
    BoxUpdate(3, 3, 91067, '&&',            True, sm_blinkback, False);
    BoxUpdate(3, 4, 91068, '"',             True, sm_blinkback, False);
    BoxUpdate(3, 5, 91069, '''',            True, sm_blinkback, False);
    BoxUpdate(3, 6, 91070, '°',             True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91071, '=',             True, sm_blinkback, False);
    BoxUpdate(4, 3, 91072, '%',             True, sm_blinkback, False);
    BoxUpdate(4, 4, 91073, '!',             True, sm_blinkback, False);
    BoxUpdate(4, 5, 91074, '?',             True, sm_blinkback, False);
    BoxUpdate(4, 6, 91075, '#',             True, sm_blinkback, False);

    BoxUpdate(5, 2, 91076, '|',             True, sm_blinkback, False);
    BoxUpdate(5, 3, 91077, '>',             True, sm_blinkback, False);
    BoxUpdate(5, 4, 91078, '<',             True, sm_blinkback, False);
    BoxUpdate(5, 5, 91079, '£',             True, sm_blinkback, False);
    BoxUpdate(5, 6, 91080, 'µ',             True, sm_blinkback, False);

    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);
    Border := True;
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardSpecials_2;
begin
  Context_KeyboardMain;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91081, '§',             True, sm_blinkback, False);
    BoxUpdate(3, 3, 91082, 'é',             True, sm_blinkback, False);
    BoxUpdate(3, 4, 91083, 'è',             True, sm_blinkback, False);
    BoxUpdate(3, 5, 91084, 'ê',             True, sm_blinkback, False);
    BoxUpdate(3, 6, 91085, 'ù',             True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91086, 'î',             True, sm_blinkback, False);
    BoxUpdate(4, 3, 91087, 'ï',             True, sm_blinkback, False);
    BoxUpdate(4, 4, 91088, 'à',             True, sm_blinkback, False);
    BoxUpdate(4, 5, 91089, 'â',             True, sm_blinkback, False);
    BoxUpdate(4, 6, 91090, 'ô',             True, sm_blinkback, False);

    BoxUpdate(5, 2, 91091, 'ç',             True, sm_blinkback, False);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);
    Border := True;
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_MainMenu;
begin
  ResetAreas;
  with FOwner do begin
    Border := False;
    BoxUpdate(1, 7, 91001, 'KEYBOARD',        True, sm_blinkback, False);
    BoxUpdate(2, 7, 91002, 'STOP',            True, sm_blinkback, False);
    BoxUpdate(3, 7, 91003, 'NAVIGATION',      True, sm_blinkback, False);
    BoxUpdate(4, 7, 91004, 'ELITE DANGEROUS', True, sm_blinkback, False);
    BoxUpdate(5, 7, 91005, 'RAOUL FUMIER',    True, sm_blinkback, False);
    BoxUpdate(6, 7, 91006, 'PARAMETERS',      True, sm_blinkback, False);
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_Parameters;
begin
  ResetAreas;
  with FOwner do begin
    Border := False;
    BoxUpdate(1, 2, 91000, 'QUIT',   True, sm_blinkback, False);
    BoxUpdate(1, 7, 91007, 'BACK',   True, sm_blinkback, False);
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_ResetAlternative;
begin
  with FOwner do begin
    BoxUpdate(1, 1, 91150, '',      True, sm_blinkback, True);
    BoxUpdate(1, 2, 91151, '',      True, sm_blinkback, True);
    BoxUpdate(1, 3, 91152, '',      True, sm_blinkback, True);
    BoxUpdate(1, 4, 91153, '',      True, sm_blinkback, True);
    BoxUpdate(1, 5, 91154, '',      True, sm_blinkback, True);
    BoxUpdate(1, 6, 91155, '',      True, sm_blinkback, True);
    BoxUpdate(1, 7, 91156, '',      True, sm_blinkback, True);
  end
end;

procedure TKeyboardContext.Copy_;
begin
  SendMsgToEx('Key_C', 'Key_LeftControl');
  AreaPanels.Unselect;
  FunctionSound
end;

constructor TKeyboardContext.Create(const AOwner: TAreaTobii);
begin
  inherited Create;
  FOwner  := AOwner;
  FBuffer := EmptyStr;
  FCall   := False;
end;

procedure TKeyboardContext.Cut_;
begin
  SendMsgToEx('Key_X', 'Key_LeftControl');
  AreaPanels.Unselect;
  FunctionSound
end;

procedure TKeyboardContext.DataDico;
begin
  ResetAreas;
  with FOwner do Border := True;
  XmlDico.Show
end;

procedure TKeyboardContext.DisplayAlpha;
begin
  CapsLock := not CapsLock;
  with FOwner do UpdateZone( Context.Context_Keyboard )
end;

procedure TKeyboardContext.DisplayNav(AIndex: Integer);
begin
  KeyboardKind := kk_navigation;
  NavIndex := AIndex;
  with FOwner do case NavIndex of
    0 : UpdateZone( Context_KeyboardNavigation_1 );
    1 : UpdateZone( Context_KeyboardNavigation_2 );
  end;
  AreaPanels.Unselect
end;

procedure TKeyboardContext.DisplayNumpad;
begin
  KeyboardKind := kk_num;
  with FOwner do UpdateZone( Context_KeyboardNumeric );
  AreaPanels.Unselect
end;

procedure TKeyboardContext.DisplaySpecial(AIndex: Integer);
begin
  KeyboardKind := kk_special;
  SpecialIndex := AIndex;
  with FOwner do case SpecialIndex of
    0 : UpdateZone( Context_KeyboardSpecials   );
    1 : UpdateZone( Context_KeyboardSpecials_1 );
    2 : UpdateZone( Context_KeyboardSpecials_2 );
  end;
  AreaPanels.Unselect
end;

procedure TKeyboardContext.DoAlphaLeft;
begin
  with FOwner do
    if KeyboardKind <> kk_alpha then begin
      KeyboardKind := kk_alpha;
      UpdateZone( Context_Keyboard )
    end else
      UpdateZone( Context_KeyboardLeft )
end;

procedure TKeyboardContext.DoAlphaRight;
begin
  with FOwner do
    if KeyboardKind <> kk_alpha then begin
      KeyboardKind := kk_alpha;
      UpdateZone( Context_Keyboard )
    end else
      UpdateZone( Context_KeyboardRight )
end;

procedure TKeyboardContext.DoAlt;
begin
  CleanBuffer;
  Application.ProcessMessages;
  SendSignal('Key_LeftAlt');
  FunctionSound
end;

procedure TKeyboardContext.DoBackSpace;
begin
  SendMsgToEx('Key_Backspace');
  if Length(FBuffer) >= 1 then begin
    FBuffer := Copy(FBuffer, 1, Length(FBuffer) - 1);
    AltDisplay;
    RepaintKeyboardAlpha;
  end;
  KeySound;
  AreaPanels.Unselect;
end; {DoBackSpace}

procedure TKeyboardContext.DoDocEnd;
begin
  CleanBuffer;
  SendMsgToA('Key_End', 'Key_LeftControl')
end;

procedure TKeyboardContext.DoDocHome;
begin
  CleanBuffer;
  SendMsgToA('Key_Home', 'Key_LeftControl')
end;

procedure TKeyboardContext.DoDown;
begin
  CleanBuffer;
  SendMsgToA('Key_DownArrow')
end;

procedure TKeyboardContext.DoEnd;
begin
  CleanBuffer;
  SendMsgToA('Key_End')
end;

procedure TKeyboardContext.DoHome;
begin
  CleanBuffer;
  SendMsgTo('Key_Home')
end;

procedure TKeyboardContext.DoMenuDown;
begin
  CleanBuffer;
  SendSignal('Key_DownArrow');
  FunctionSound
end;

procedure TKeyboardContext.DoMenuEchap;
begin
  CleanBuffer;
  SendSignal('Key_Echap');
  FunctionSound
end;

procedure TKeyboardContext.DoMenuEnter;
begin
  CleanBuffer;
  SendSignal('Key_Enter');
  FunctionSound
end;

procedure TKeyboardContext.DoMenuLeft;
begin
  CleanBuffer;
  SendSignal('Key_LeftArrow');
  FunctionSound
end;

procedure TKeyboardContext.DoMenuRight;
begin
  CleanBuffer;
  SendSignal('Key_RightArrow');
  FunctionSound
end;

procedure TKeyboardContext.DoMenuUp;
begin
  CleanBuffer;
  SendSignal('Key_UpArrow');
  FunctionSound
end;

procedure TKeyboardContext.DoNext;
begin
  CleanBuffer;
  SendMsgTo('Key_RightArrow')
end;

procedure TKeyboardContext.DoNextWord;
begin
  CleanBuffer;
  SendMsgTo('Key_RightArrow', 'Key_LeftControl')
end;

procedure TKeyboardContext.DoOnShift;
begin
  case KeyboardKind of
    kk_alpha      : DisplayAlpha;
    kk_special    : DisplaySpecial( (SpecialIndex + 1) mod 3 );
    kk_navigation : DisplayNav( (NavIndex + 1) mod 2 );
    kk_critik     : DoAlphaLeft;
    kk_num        : DoAlphaLeft;
  end;
  AreaPanels.Unselect
end;

procedure TKeyboardContext.DoPageDown;
begin
  CleanBuffer;
  SendMsgToA('Key_PageDown');
end;

procedure TKeyboardContext.DoPageUp;
begin
  CleanBuffer;
  SendMsgToA('Key_PageUp');
end;

procedure TKeyboardContext.DoPrev;
begin
  CleanBuffer;
  SendMsgTo('Key_LeftArrow')
end;

procedure TKeyboardContext.DoPrevWord;
begin
  CleanBuffer;
  SendMsgTo('Key_LeftArrow', 'Key_LeftControl')
end;

procedure TKeyboardContext.DoSelAll;
begin
  CleanBuffer;
  SelectAll
end;

procedure TKeyboardContext.DoSelBottom;
begin
  CleanBuffer;
  SendMsgToA('Key_End', 'Key_LeftShift', 'Key_RightControl');
end;

procedure TKeyboardContext.DoSelEnd;
begin
  CleanBuffer;
  SendMsgToA('Key_End', 'Key_LeftShift');
end;

procedure TKeyboardContext.DoSelHome;
begin
  CleanBuffer;
  SendMsgToA('Key_Home', 'Key_LeftShift');
end;

procedure TKeyboardContext.DoSelLeft;
begin
  CleanBuffer;
  SendMsgTo('Key_LeftArrow', 'Key_LeftShift');
end;

procedure TKeyboardContext.DoSelPageDown;
begin
  CleanBuffer;
  SendMsgToA('Key_PageDown', 'Key_LeftShift');
end;

procedure TKeyboardContext.DoSelPageUp;
begin
  CleanBuffer;
  SendMsgToA('Key_PageUp', 'Key_LeftShift');
end;

procedure TKeyboardContext.DoSelRight;
begin
  CleanBuffer;
  SendMsgTo('Key_RightArrow', 'Key_LeftShift');
end;

procedure TKeyboardContext.DoSelTop;
begin
  CleanBuffer;
  SendMsgToA('Key_Home', 'Key_LeftShift', 'Key_RightControl');
end;

procedure TKeyboardContext.DoSelWordLeft;
begin
  CleanBuffer;
  SendMsgTo('Key_LeftArrow', 'Key_LeftShift', 'Key_RightControl');
end;

procedure TKeyboardContext.DoSelWordRight;
begin
  CleanBuffer;
  SendMsgTo('Key_RightArrow', 'Key_LeftShift', 'Key_RightControl');
end;

procedure TKeyboardContext.DoTab;
begin
  CleanBuffer;
  SendSignal('Key_Tab');
  FunctionSound
end;

procedure TKeyboardContext.DoUp;
begin
  CleanBuffer;
  SendMsgTo('Key_UpArrow')
end;

procedure TKeyboardContext.DoWithCar(const ATag: Integer);
var
  Car : string;
begin
  if FLastWord <> EmptyStr then CleanBuffer;

  if (ATag >= 91010) and (ATag <= 91035) and CapsLock
    then Car := LowerCase( KeyCharArray[ATag] )
    else
  if (ATag >= 91082) and (ATag <= 91091) and CapsLock
    then Car := Maj( KeyCharArray[ATag] )
    else Car := KeyCharArray[ATag];

  FBuffer := FBuffer + Car;
  AltDisplay;
  SendTextTo(Car);
  KeySound;
  AreaPanels.Unselect;
  RepaintKeyboardAlpha
end; {DoWithCar}

procedure TKeyboardContext.FunctionSound;
begin
  if FCall then Exit;
  if KeyReadBoolean(ParamKey, 'KeySound', True) then  begin
    Application.ProcessMessages;
    PlaySound('volume_change.wav', 0, SND_FILENAME)
  end
end;

function TKeyboardContext.GetCapsLock: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'CapsLock', False)
end;

function TKeyboardContext.GetCar(const Car: Char): Char;
begin
  Result := Chr( Ord(Car) + 32 * Integer( CapsLock ) );
end;

function TKeyboardContext.GetKeyboardKind: TKeyboardKind;
begin
  Result := TKeyboardKind( KeyReadInt(ParamKey, 'KeyboardKind', 0) )
end;

function TKeyboardContext.GetLeftKeyboard: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'LeftKeyboard', True)
end;

function TKeyboardContext.GetNavIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'NavIndex', 0)
end;

function TKeyboardContext.GetSpecialIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SpecialIndex', 0)
end;

procedure TKeyboardContext.KeyboardNotify(const ATag: Integer);
begin
  case ATag of
  { --- Keyboard actions }

     91103 : {NULL};
     91104 : DoPrev;
     91105 : DoNext;
     91008 : DoAlphaRight;
     91009 : DoAlphaLeft;
     { --- Add char to external component }
     91010..91092 : DoWithCar( ATag );
     91100 : DoOnShift;
     91101 : DisplayNumpad;
     91102 : DisplaySpecial( SpecialIndex );
     91106 : DoBackSpace;
     91107 : {NULL};
     91108 : DisplayNav( NavIndex );
     91109 : DoUp;
     91110 : DoHome;
     91111 : DoPageUp;
     91112 : {NULL};
     91113 : DoDocHome;
     91114 : DoDown;
     91115 : DoEnd;
     91116 : DoPageDown;
     91117 : {NULL};
     91118 : DoDocEnd;
     91119 : DoPrevWord;
     91120 : DoNextWord;
     91121 : DoSelLeft;
     91122 : DoSelHome;
     91123 : DoSelPageUp;
     91124 : DoSelWordLeft;
     91125 : DoSelTop;
     91126 : DoSelRight;
     91127 : DoSelEnd;
     91128 : DoSelPageDown;
     91129 : DoSelWordRight;
     91130 : DoSelBottom;
     91131 : DoSelAll;
     91132 : Copy_;
     91133 : Cut_;
     91134 : Paste_;
     91135 : SendMsgToA('Key_Z', 'Key_LeftControl');
     91136 : FOwner.UpdateZone( Context_KeyboardCritik );
     91137 : {NULL};
     91138 : DoMenuUp;
     91139 : DoMenuLeft;
     91140 : DoMenuDown;
     91141 : DoMenuRight;
     91142 : DoMenuEnter;
     91143 : DoMenuEchap;
     91144 : DoAlt;
     91145 : DoTab;
     { --- Insert alternative to external component }
     91150..91156 : AltInsert(Atag);
  end
end;

procedure TKeyboardContext.KeySound;
begin
  if KeyReadBoolean(ParamKey, 'KeySound', True) then begin
    Application.ProcessMessages;
    PlaySound('dash_button_press.wav', 0, SND_FILENAME)
  end
end;

procedure TKeyboardContext.MainMenu_display;
begin
  with FOwner do begin
    FElite.CurrContextTag := 91007;
    UpdateZone( Context_MainMenu )
  end
end;

procedure TKeyboardContext.Make_Alternative(ASt: string);
var
  i : Integer;
begin
  Context_ResetAlternative;
  if ASt <> EmptyStr then with TStringList.Create do
  try
    Text := ASt;
    for i := 0 to Pred(Count) do with FOwner do
      BoxUpdate(1, 1 + i, 91150 + i, Strings[i], True, sm_blinkback, False)
  finally
    Free
  end;
end;

procedure TKeyboardContext.Paste_;
begin
  CleanBuffer;
  SendSignal( 'Key_V', 'Key_LeftControl' );
  AreaPanels.Unselect;
  KeySound
end;

procedure TKeyboardContext.RepaintKeyboardAlpha;
begin
  FCall := True;
  try
    FOwner.UpdateZone( Context_Keyboard )
  finally
    FCall := False
  end
end;

procedure TKeyboardContext.ResetAreas;
begin
  with FOwner do begin
    VisibleUpdate(1, [True, True, True, True, True, True, True]);
    VisibleUpdate(2, [True, True, True, True, True, True, True]);
    VisibleUpdate(3, [True, True, True, True, True, True, True]);
    VisibleUpdate(4, [True, True, True, True, True, True, True]);
    VisibleUpdate(5, [True, True, True, True, True, True, True]);
    VisibleUpdate(6, [True, True, True, True, True, True, True]);

    CaptionUpdate(1, ['', '', '', '', '', '', '']);
    CaptionUpdate(2, ['', '', '', '', '', '', '']);
    CaptionUpdate(3, ['', '', '', '', '', '', '']);
    CaptionUpdate(4, ['', '', '', '', '', '', '']);
    CaptionUpdate(5, ['', '', '', '', '', '', '']);
    CaptionUpdate(6, ['', '', '', '', '', '', '']);

    NullUpdate(1, [True, True, True, True, True, True, True]);
    NullUpdate(2, [True, True, True, True, True, True, True]);
    NullUpdate(3, [True, True, True, True, True, True, True]);
    NullUpdate(4, [True, True, True, True, True, True, True]);
    NullUpdate(5, [True, True, True, True, True, True, True]);
    NullUpdate(6, [True, True, True, True, True, True, True]);
  end
end;

procedure TKeyboardContext.SelectAll;
begin
  SendSignal('Key_End', 'Key_LeftControl');
  SendSignal('Key_Home', 'Key_LeftShift', 'Key_RightControl');

  AreaPanels.Unselect;
  FunctionSound
end;

procedure TKeyboardContext.SendMsgTo(const k1, k2, k3: string);
begin
  SendSignal( k1, k2, k3 );
  FunctionSound
end;

procedure TKeyboardContext.SendMsgToA(const k1, k2, k3: string);
begin
  SendMsgToEx(k1, k2, k3);
  AreaPanels.Unselect;
  FunctionSound
end;

procedure TKeyboardContext.SendMsgToEx(const k1, k2, k3: string);
begin
  SendSignal( k1, k2, k3 )
end;

procedure TKeyboardContext.SendSignal(const k1, k2, k3: string);
begin
  TKeyMessageSender.Signal( EncodeKey( k1, k2, k3 ), 10 )
end;

procedure TKeyboardContext.SendTextTo(const ASt: string);
var
  P : PWideChar;
begin
  if Length(ASt) = 1 then P := PWideChar(ASt+#0) else P := PWideChar(ASt);
  SendKeys(P, True)
end;

procedure TKeyboardContext.SetCapsLock(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'CapsLock', Value)
end;

procedure TKeyboardContext.SetKeyboardKind(const Value: TKeyboardKind);
begin
  KeyWrite(ParamKey, 'KeyboardKind', Integer(Value) )
end;

procedure TKeyboardContext.SetLeftKeyboard(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'LeftKeyboard', Value)
end;

procedure TKeyboardContext.SetNavIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'NavIndex', Value)
end;

procedure TKeyboardContext.SetSpecialIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SpecialIndex', Value)
end;

{ TEliteContext }

procedure TEliteContext.AssignFunc(const Index: Integer);
begin
  IndexFunc := Index;
  Func_display;
end;

procedure TEliteContext.Context_ACS;
begin
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True);
  end
end;

procedure TEliteContext.Context_Elite;
begin
  EliteForeGround;
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91007, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);


    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91581, 'MENU',          True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 7, 91591, 'DRIVE',         True, sm_blinkback, False);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True);
  end
end;

procedure TEliteContext.Context_EliteDrive;
var
  ASt     : string;
  BtnMode : TSelectMode;
begin
  BtnMode := sm_blinkback;
  if IndexMode = 0 then begin
    BtnMode := sm_directnotnull;
    ASt     := 'NO STEP'
  end else ASt := Format('STEP %d', [SubIndex]);

  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91596, 'SLIDE',         True, sm_blinkback, False);
    BoxUpdate(1, 2, 91238, Ast,             True, sm_blinkback, False);
    BoxUpdate(1, 3, 91293, 'SUPERCRUISE',   True, sm_blinkback, False);
    BoxUpdate(1, 4, 91239, 'FUNC',          True, sm_blinkback, False);
    BoxUpdate(1, 5, 91294, 'FSD',           True, sm_blinkback, False);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91004, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91282, 'SPEED LESS',    True, sm_blinkback, False);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91230, 'PITCH UP',      True, BtnMode, False);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 7, 91283, 'SPEED MORE',    True, sm_blinkback, False);

    BoxUpdate(3, 1, 91284, 'SPEED 25%',     True, sm_blinkback, False);
    BoxUpdate(3, 2, 91234, 'ROLL LEFT',     True, BtnMode, False);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91235, 'ROLL RIGHT',    True, BtnMode, False);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91285, 'SPEED 0%',      True, sm_blinkback, False);

    BoxUpdate(4, 1, 91286, 'SPEED 50%',     True, sm_blinkback, False);
    BoxUpdate(4, 2, 91232, 'YAW LEFT',      True, BtnMode, False);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91233, 'YAW RIGHT',     True, BtnMode, False);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 7, 91287, 'SPEED 100%',    True, sm_blinkback, False);

    BoxUpdate(5, 1, 91288, 'SPEED 75%',     True, sm_blinkback, False);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91237, 'SHOOT 2',       True, sm_blinkback, False);
    BoxUpdate(5, 4, 91231, 'PITCH DOWN',    True, BtnMode, False);
    BoxUpdate(5, 5, 91236, 'SHOOT 1',       True, sm_blinkback, False);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91289, 'INV SPEED',     True, sm_blinkback, False);

    BoxUpdate(6, 1, 91570, 'SWITCH',        True, sm_blinkback, False);
    BoxUpdate(6, 2, 91290, 'BOOST',         True, sm_blinkback, False);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91291, 'FLIGHT ASSIST', True, sm_blinkback, False);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_EliteMenu;
begin
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91209, 'STEEP',         True, sm_blinkback, False);
    BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 4, 91239, 'FUNC',          True, sm_blinkback, False);
    BoxUpdate(1, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91004, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91205, 'PREV',          True, sm_blinkback, False);
    BoxUpdate(2, 4, 91201, 'TOP',           True, sm_blinkback, False);
    BoxUpdate(2, 5, 91206, 'NEXT',          True, sm_blinkback, False);
    BoxUpdate(2, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91225, 'STEP 1',        True, sm_blinkback, False);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91227, 'STEP 5',       True, sm_blinkback, False);

    BoxUpdate(4, 1, 91226, 'STEP 3',        True, sm_blinkback, False);
    BoxUpdate(4, 2, 91203, 'LEFT',          True, sm_blinkback, False);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91204, 'RIGHT',         True, sm_blinkback, False);
    BoxUpdate(4, 7, 91228, 'STEP 10',       True, sm_blinkback, False);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91570, 'SWITCH',        True, sm_blinkback, False);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91207, 'ESC',           True, sm_blinkback, False);
    BoxUpdate(6, 4, 91202, 'BOTTOM',        True, sm_blinkback, False);
    BoxUpdate(6, 5, 91208, 'ENTER',         True, sm_blinkback, False);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_Func;
begin
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91240, 'PANELS',        True, sm_blinkback, False);
    BoxUpdate(1, 2, 91250, 'DIV',           True, sm_blinkback, False);
    BoxUpdate(1, 3, 91260, 'WEAPONS',       True, sm_blinkback, False);
    BoxUpdate(1, 4, 91300, 'FIGHTER',       True, sm_blinkback, False);
    BoxUpdate(1, 5, 91350, 'CAMERA',        True, sm_blinkback, False);
    BoxUpdate(1, 6, 91400, 'SRV',           True, sm_blinkback, False);
    BoxUpdate(1, 7, 91571, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True);
  end
end;

procedure TEliteContext.Context_Func0;
begin
  with FOwner do begin
    BoxUpdate(2, 1, 91248, 'SYSTEM MAP',    True, sm_blinkback, False);
    BoxUpdate(2, 4, 91241, 'COM PANEL',     True, sm_blinkback, False);
    BoxUpdate(2, 7, 91249, 'GALAXY MAP',    True, sm_blinkback, False);

    BoxUpdate(3, 2, 91580, 'MENU',          True, sm_blinkback, False);

    BoxUpdate(4, 1, 91242, 'LEFT PANEL',    True, sm_blinkback, False);
    BoxUpdate(4, 4, 91245, 'PANEL BACK',    True, sm_blinkback, False);
    BoxUpdate(4, 7, 91243, 'RIGHT PANEL',   True, sm_blinkback, False);

    BoxUpdate(6, 1, 91246, 'FRIENDS MENU',  True, sm_blinkback, False);
    BoxUpdate(6, 4, 91244, 'BOTTOM PANEL',  True, sm_blinkback, False);
    BoxUpdate(6, 7, 91247, 'BACK TO MENU',  True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_Func1;
begin
  with FOwner do begin
    BoxUpdate(2, 2, 91251, 'LANDING GEAR',  True, sm_blinkback, False);
    BoxUpdate(2, 3, 91252, 'CARGO SCOOP',   True, sm_blinkback, False);
    BoxUpdate(2, 4, 91253, 'NIGHT VISION',  True, sm_blinkback, False);
    BoxUpdate(2, 5, 91254, 'SPOTLIGHT',     True, sm_blinkback, False);
    BoxUpdate(2, 6, 91255, 'COCKPIT MODE',  True, sm_blinkback, False);
    BoxUpdate(2, 7, 91277, 'SAA',           True, sm_blinkback, False);

    BoxUpdate(3, 1, 91281, 'CHARGE ECM',    True, sm_blinkback, False);

    BoxUpdate(4, 1, 91279, 'SHIELD CELL',   True, sm_blinkback, False);
    BoxUpdate(4, 4, 91256, 'PIPE ENGINES',  True, sm_blinkback, False);

    BoxUpdate(5, 1, 91280, 'HEAT SINK',     True, sm_blinkback, False);
    BoxUpdate(5, 3, 91257, 'PIPE SYSTEMS',  True, sm_blinkback, False);
    BoxUpdate(5, 4, 91259, 'DEFAULT',       True, sm_blinkback, False);
    BoxUpdate(5, 5, 91258, 'PIPE WEAPONS',  True, sm_blinkback, False);

    BoxUpdate(6, 1, 91278, 'CHAFF LAUNCHER',True, sm_blinkback, False);
    BoxUpdate(6, 3, 91273, 'FULL SYSTEMS',  True, sm_blinkback, False);
    BoxUpdate(6, 4, 91274, 'FULL ENGINES',  True, sm_blinkback, False);
    BoxUpdate(6, 5, 91275, 'FULL WEAPONS',  True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_Func2;
begin
  with FOwner do begin
    BoxUpdate(2, 2, 91261, 'HARD POINTS',      True, sm_blinkback, False);
    BoxUpdate(2, 3, 91262, 'PREV GROUP',       True, sm_blinkback, False);
    BoxUpdate(2, 4, 91263, 'NEXT GROUP',       True, sm_blinkback, False);
    BoxUpdate(2, 6, 91264, 'PREV SUBSYSTEM',   True, sm_blinkback, False);
    BoxUpdate(2, 7, 91265, 'NEXT SUBSYSTEM',   True, sm_blinkback, False);

    BoxUpdate(3, 1, 91276, 'NEXT ROUTE',       True, sm_blinkback, False);
    BoxUpdate(3, 7, 91266, 'PREV HOSTILE',     True, sm_blinkback, False);

    BoxUpdate(4, 1, 91270, 'TARGET WINGMAN 1', True, sm_blinkback, False);
    BoxUpdate(4, 4, 91256, 'PIPE ENGINES',     True, sm_blinkback, False);
    BoxUpdate(4, 7, 91267, 'NEXT HOSTILE',     True, sm_blinkback, False);

    BoxUpdate(5, 1, 91271, 'TARGET WINGMAN 2', True, sm_blinkback, False);
    BoxUpdate(5, 3, 91257, 'PIPE SYSTEMS',     True, sm_blinkback, False);
    BoxUpdate(5, 4, 91259, 'DEFAULT',          True, sm_blinkback, False);
    BoxUpdate(5, 5, 91258, 'PIPE WEAPONS',     True, sm_blinkback, False);
    BoxUpdate(5, 7, 91268, 'HIGEST THREAT',    True, sm_blinkback, False);

    BoxUpdate(6, 1, 91272, 'TARGET WINGMAN 3', True, sm_blinkback, False);
    BoxUpdate(6, 3, 91273, 'FULL SYSTEMS',     True, sm_blinkback, False);
    BoxUpdate(6, 4, 91274, 'FULL ENGINES',     True, sm_blinkback, False);
    BoxUpdate(6, 5, 91275, 'FULL WEAPONS',     True, sm_blinkback, False);
    BoxUpdate(6, 7, 91269, '12H TARGET',       True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_Func3;
begin
  with FOwner do begin
    BoxUpdate(2, 1, 91309, 'BOTTOM PANEL',     True, sm_blinkback, False);
    BoxUpdate(2, 2, 91301, 'OPEN ORDERS',      True, sm_blinkback, False);
    BoxUpdate(2, 6, 91302, 'DOCK ORDER',       True, sm_blinkback, False);
    BoxUpdate(3, 2, 91303, 'DEFENSIVE',        True, sm_blinkback, False);
    BoxUpdate(3, 6, 91304, 'AGGRESSIVE',       True, sm_blinkback, False);
    BoxUpdate(4, 2, 91305, 'HOLD FIRE',        True, sm_blinkback, False);
    BoxUpdate(4, 6, 91306, 'FOCUS TARGET',     True, sm_blinkback, False);
    BoxUpdate(5, 2, 91307, 'HOLD POSITION',    True, sm_blinkback, False);
    BoxUpdate(5, 6, 91308, 'FOLLOW ME',        True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_Func4;
begin
  with FOwner do begin
    BoxUpdate(2, 1, 91351, 'OPEN/CLOSE',       True, sm_blinkback, False);
    BoxUpdate(2, 3, 91352, 'PREV CAM',         True, sm_blinkback, False);
    BoxUpdate(2, 5, 91353, 'NEXT CAM',         True, sm_blinkback, False);


    BoxUpdate(3, 3, 91355, 'CAM 1',            True, sm_blinkback, False);
    BoxUpdate(3, 5, 91356, 'CAM 2',            True, sm_blinkback, False);

    BoxUpdate(4, 3, 91357, 'CAM 3',            True, sm_blinkback, False);
    BoxUpdate(4, 5, 91358, 'CAM 4',            True, sm_blinkback, False);

    BoxUpdate(5, 3, 91359, 'CAM 5',            True, sm_blinkback, False);
    BoxUpdate(5, 5, 91360, 'CAM 6',            True, sm_blinkback, False);

    BoxUpdate(6, 3, 91361, 'CAM 7',            True, sm_blinkback, False);
    BoxUpdate(6, 5, 91362, 'CAM 8',            True, sm_blinkback, False);

    BoxUpdate(4, 1, 91363, 'ATH',              True, sm_blinkback, False);

  end
end;

procedure TEliteContext.Context_Func5;
begin
  with FOwner do begin
    BoxUpdate(2, 2, 91401, 'AUTO BRAK',           True, sm_blinkback, False);
    BoxUpdate(2, 4, 91402, 'TURRET MODE',         True, sm_blinkback, False);
    BoxUpdate(2, 6, 91403, 'SHIP Recall/Dismiss', True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_GalaxyMap;
var
  ASt      : string;
  ASt1     : string;
  BtnMode  : TSelectMode;
  BtnMode1 : TSelectMode;
begin
  BtnMode  := sm_blinkback;
  BtnMode1 := sm_blinkback;
  if IndexGMMode = 0 then begin
    BtnMode := sm_directnotnull;
    ASt     := 'NO STEP';
    if GMAllSlide then BtnMode1 := sm_directnotnull;
    case BtnMode1 of sm_directnotnull :
      ASt1 := 'ALL SLIDE' else ASt1 := 'NO ALL SLIDE'
    end
  end else begin
    ASt  := Format('STEP %d', [SubGMIndex]);
    ASt1 := 'NO ALL SLIDE'
  end;

  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91422, 'SLIDE',         True, sm_blinkback, False);
    BoxUpdate(1, 2, 91423, ASt,             True, sm_blinkback, False);
    BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 4, 91424, 'MENU',          True, sm_blinkback, False);
    BoxUpdate(1, 5, 91425, 'GO HOME',       True, sm_blinkback, False);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91421, 'BACK 1',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91426, ASt1,            True, sm_blinkback, False);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91427, 'REWARD',        True, BtnMode, False);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91434, 'ZOOM IN',       True, BtnMode1, False);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91428, 'UP',            True, BtnMode1, False);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91429, 'UP ROTATE',     True, BtnMode1, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91430, 'LEFT',          True, BtnMode, False);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91431, 'RIGHT',         True, BtnMode, False);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91432, 'DOWN',          True, BtnMode1, False);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91433, 'DOWN ROTATE',   True, BtnMode1, False);

    BoxUpdate(6, 1, 91570, 'SWITCH',        True, sm_blinkback , False);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91435, 'LEFT ROTATE',   True, BtnMode1, False);
    BoxUpdate(6, 4, 91436, 'FORWARD',       True, BtnMode, False);
    BoxUpdate(6, 5, 91437, 'RIGHT ROTATE',  True, BtnMode1, False);
    BoxUpdate(6, 6, 91438, 'ZOOM OUT',      True, BtnMode1, False);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_Pause;
begin
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91598, 'BACK',          True, sm_blinkback, False);
  end
end;

procedure TEliteContext.Context_StepSelect;
begin
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91580, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91210, '1',             True, sm_blinkback, False);
    BoxUpdate(2, 3, 91211, '2',             True, sm_blinkback, False);
    BoxUpdate(2, 4, 91212, '3',             True, sm_blinkback, False);
    BoxUpdate(2, 5, 91213, '4',             True, sm_blinkback, False);
    BoxUpdate(2, 6, 91214, '5',             True, sm_blinkback, False);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91215, '10',            True, sm_blinkback, False);
    BoxUpdate(3, 3, 91216, '15',            True, sm_blinkback, False);
    BoxUpdate(3, 4, 91217, '20',            True, sm_blinkback, False);
    BoxUpdate(3, 5, 91218, '30',            True, sm_blinkback, False);
    BoxUpdate(3, 6, 91219, '50',            True, sm_blinkback, False);
    BoxUpdate(3, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91220, '100',           True, sm_blinkback, False);
    BoxUpdate(4, 3, 91221, '150',           True, sm_blinkback, False);
    BoxUpdate(4, 4, 91222, '200 ',          True, sm_blinkback, False);
    BoxUpdate(4, 5, 91223, '300',           True, sm_blinkback, False);
    BoxUpdate(4, 6, 91224, '500',           True, sm_blinkback, False);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True);
  end
end;

procedure TEliteContext.Context_System_Map;
begin
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True);
  end
end;

constructor TEliteContext.Create(const AOwner: TAreaTobii);
begin
  inherited Create;
  FOwner      := AOwner;
  Step        := 1;
  IndexFunc   := 0;
  IndexMode   := 0;
  IndexGMMode := 2;
  GMAllSlide  := False;
end;

procedure TEliteContext.DoAutoBreak;
begin
  with FOwner, FEliteManager, FEliteStatus do if InSrv then VRSAutoBreak
end;

procedure TEliteContext.DoBack;
begin
  with FOwner, FEliteManager, FEliteStatus do begin
    if TGuiType(GuiFocus) = gt_galaxymap then MapGalaxy else UIBack;
    FContext.FunctionSound
  end
end;

procedure TEliteContext.DoClicUnique(const Method: TGearTypeNotify);
begin
  AreaPanels.Unselect;
  FOwner.FContext.FunctionSound;
  if Assigned(Method) then Method(lgt_none)
end;

procedure TEliteContext.DoClicUnique(const Method: TContextNotify);
begin
 AreaPanels.Unselect;
 FOwner.FContext.FunctionSound;
 if Assigned(Method) then Method
end;

procedure TEliteContext.DoClic(const Method: TContextNotify);
begin
  if Assigned(Method) then Method
end;

procedure TEliteContext.DoClic(const Method: TIntegerNotify;
  Value: Integer);
begin
  if Assigned(Method) then Method(Value);
end;

procedure TEliteContext.DoClic(const Method: TIntNotify; Value: Integer);
begin
  if Assigned(Method) then Method(Value);
end;

procedure TEliteContext.DoClicUnique(const Method: TCockpitNotify);
begin
  AreaPanels.Unselect;
  FOwner.FContext.FunctionSound;
  if Assigned(Method) then Method(cmt_none)
end;


procedure TEliteContext.DoGMAllSlide;
begin
  GMAllSlide := not GMAllSlide;
  GalaxyMap_display;
end;

procedure TEliteContext.DoGMBack;
begin
  with FOwner, FEliteStatus, FEliteManager do begin
    MapGalaxy;
    Menu_display;

//    CurrContextTag := 91580;
//    case TGuiType(GuiFocus) of
//      gt_nofocus       : if InSrv or InFighter or InMainShip then Drive_display
//                           else Menu_display;
//      gt_internalpanel,
//      gt_externalpanel,
//      gt_commspanel,
//      gt_rolepanel,
//      gt_stationservice,
//      gt_codex,
//      gt_orrery  : Menu_display;
//
//      gt_fssmode,
//      gt_saamode : Drive_display;
//    end
  end
end;

procedure TEliteContext.DoGMDown;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : Sud;
      1 : Sud(1, 90);
      2 : Sud(1, 180);
      3 : Sud(1, 360);
    end
  end
end;

procedure TEliteContext.DoGMDownRotate;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : CamPitchDown;
      1 : CamPitchDown(90);
      2 : CamPitchDown(180);
      3 : CamPitchDown(360);
    end
  end
end;

procedure TEliteContext.DoGMForward;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : Fils;
      1 : Fils(1, 90);
      2 : Fils(1, 180);
      3 : Fils(1, 360);
    end
  end
end;

procedure TEliteContext.DoGMGoHome;
begin
  with FOwner, FEliteManager do GalaxyMapHome
end;

procedure TEliteContext.DoGMLeft;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : Ouest;
      1 : Ouest(1, 90);
      2 : Ouest(1, 180);
      3 : Ouest(1, 360);
    end
  end
end;

procedure TEliteContext.DoGMLeftRotate;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : CamYawLeft;
      1 : CamYawLeft(90);
      2 : CamYawLeft(180);
      3 : CamYawLeft(360);
    end
  end
end;

procedure TEliteContext.DoGMMenu;
begin
  Menu_display;
end;

procedure TEliteContext.DoGMReward;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : Pere;
      1 : Pere(1, 90);
      2 : Pere(1, 180);
      3 : Pere(1, 360);
    end
  end
end;

procedure TEliteContext.DoGMRight;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : Est;
      1 : Est(1, 90);
      2 : Est(1, 180);
      3 : Est(1, 360);
    end
  end
end;

procedure TEliteContext.DoGMRightRotate;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : CamYawRight;
      1 : CamYawRight(90);
      2 : CamYawRight(180);
      3 : CamYawRight(360);
    end
  end
end;

procedure TEliteContext.DoGMSlide;
begin
  IndexGMMode := 0;
  GalaxyMap_display;
end;

procedure TEliteContext.DoGMSteps;
begin
  if IndexGMMode = 0 then begin
    IndexGMMode := SubGMIndex
  end else begin
    IndexGMMode := (IndexGMMode + 1) mod 4;
    if IndexGMMode = 0 then IndexGMMode := 1;
    SubGMIndex := IndexGMMode;
  end;
  FOwner.FContext.FunctionSound;
  GalaxyMap_display
end;

procedure TEliteContext.DoGMUp;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : Nord;
      1 : Nord(1, 90);
      2 : Nord(1, 180);
      3 : Nord(1, 360);
    end
  end
end;

procedure TEliteContext.DoGMUpRotate;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : CamPitchUp;
      1 : CamPitchUp(90);
      2 : CamPitchUp(180);
      3 : CamPitchUp(360);
    end
  end
end;

procedure TEliteContext.DoGMZoomIn;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : CamZoomIn;
      1 : CamZoomIn(90);
      2 : CamZoomIn(180);
      3 : CamZoomIn(360);
    end
  end
end;

procedure TEliteContext.DoGMZoomOut;
begin
  with FOwner, FEliteManager do begin
    case IndexGMMode of
      0 : CamZoomOut;
      1 : CamZoomOut(90);
      2 : CamZoomOut(180);
      3 : CamZoomOut(360);
    end
  end
end;

procedure TEliteContext.DoMapGalaxyShow;
begin
  with FOwner, EliteManager do begin
    DoClicUnique(MapGalaxy);
    Menu_display;
  end
end;

procedure TEliteContext.DoNavEst;
begin
  with FOwner, EliteManager do case IndexMode of
    0 : Est;
    1 : Est(1,120);
    2 : Est(1,200);
    3 : Est(1,300);
  end
end;

procedure TEliteContext.DoNavFils;
begin
  with FOwner, EliteManager do case IndexMode of
    0 : Fils;
    1 : Fils(1,120);
    2 : Fils(1,200);
    3 : Fils(1,300);
  end
end;

procedure TEliteContext.DoNavModeChange;
begin
  if IndexMode = 0 then begin
    IndexMode := SubIndex
  end else begin
    IndexMode := (IndexMode + 1) mod 4;
    if IndexMode = 0 then IndexMode := 1;
    SubIndex := IndexMode;
  end;
  FOwner.FContext.FunctionSound;
  Drive_display;
end;

procedure TEliteContext.DoNavNord;
begin
  with FOwner, EliteManager do case IndexMode of
    0 : Nord;
    1 : Nord(1,120);
    2 : Nord(1,200);
    3 : Nord(1,300);
  end
end;

procedure TEliteContext.DoNavOuest;
begin
  with FOwner, EliteManager do case IndexMode of
    0 : Ouest;
    1 : Ouest(1,120);
    2 : Ouest(1,200);
    3 : Ouest(1,300);
  end
end;

procedure TEliteContext.DoNavPere;
begin
  with FOwner, EliteManager do case IndexMode of
    0 : Pere;
    1 : Pere(1,120);
    2 : Pere(1,200);
    3 : Pere(1,300);
  end
end;

procedure TEliteContext.DoNavSlide;
begin
  IndexMode := 0;
  FOwner.FContext.FunctionSound;
  Drive_display;
end;

procedure TEliteContext.DoNavSud;
begin
  with FOwner, EliteManager do case IndexMode of
    0 : Sud;
    1 : Sud(1,120);
    2 : Sud(1,200);
    3 : Sud(1,300);
  end
end;

procedure TEliteContext.DoOrderPanel;
begin
  with FOwner, FEliteManager, FEliteStatus do if TGuiType(GuiFocus) <> gt_rolepanel
   then OpenOrders
   else RadarPanel
end;

procedure TEliteContext.DoShipDismissRecall;
begin
  with FOwner, FEliteManager, FEliteStatus do if InSrv then ShipDismissRecall
end;

procedure TEliteContext.DoStartFighter;
begin
  with FOwner, FEliteManager, FEliteStatus do
    case TGuiType(GuiFocus) of
      gt_rolepanel : RadarPanel;
      else begin
        RadarPanel;
        Menu_display;
      end
    end
end;

procedure TEliteContext.DoTarget12h;
begin
  with FOwner, FEliteManager do case FEliteStatus.InSrv of
    True : VRSTarget12h;
    else Target12h;
  end;
end;

procedure TEliteContext.DoVRSTurret;
begin
  with FOwner, FEliteManager, FEliteStatus do if InSrv then VRSTurret
end;

procedure TEliteContext.Drive_display;
begin
  CurrContextTag := 91590;
  FOwner.UpdateZone( Context_EliteDrive )
end;

procedure TEliteContext.EliteNotify(const ATag: Integer);
begin
  EliteForeGround;
  with FOwner, Elm do
  try
    case ATag of
      { --- Areas for Elite 91200....91600 }
      91200 : {NULL};
      { --- Menu actions }
      91201 : DoClic(Down, Step);
      91202 : DoClic(Up, Step);
      91203 : DoClic(Left, Step);
      91204 : DoClic(Right, Step);
      91205 : DoClic(PriorSheet, Step);
      91206 : DoClic(NextSheet, Step);
      91207 : DoClicUnique(UIBack);
      91208 : DoClicUnique(UISelect);
      91209 : DoClicUnique(Step_display);
      { --- Steps selector }
      91210..91228 : StepSelector(ATag);
      { --- Drive actions }
      91230 : DoNavNord;  //Pitch up
      91231 : DoNavSud;   //Pitch down
      91232 : DoNavOuest; //Yaw left
      91233 : DoNavEst;   //Yaw right
      91234 : DoNavPere;  //Roll left
      91235 : DoNavFils;  //Roll right
      91236 : PrimaryFire(10);   //Primary fire
      91237 : SecondaryFire(10); //Secondary fire
      91238 : DoClic(DoNavModeChange); //Mode
      91239 : DoClic(Func_display);
      91282 : DoClic(Deceleration);    //Less speed
      91283 : DoClic(Acceleration);    //More speed
      91285 : DoClicUnique(SpeedNull); //0%
      91284 : DoClicUnique(Speed25);   //25%
      91286 : DoClicUnique(Speed50);   //50%
      91288 : DoClicUnique(Speed75);   //75%
      91287 : DoClicUnique(Speed100);  //100%
      91289 : DoClicUnique(InverserPropulsion); //Inv speed
      91290 : DoClicUnique(Boost);      //Boost
      91291 : AssistanceDeVol(ft_none); //Flight assist
      91293 : DoClicUnique(SuperNavigation); //Supercruise
      91294 : DoClicUnique(FSD); //FSD

      { --- Functions actions }
      91240 : AssignFunc(0);
      91241 : DoClicUnique(CommsPanel);  //Top Panel
      91242 : DoClicUnique(LeftPanel);   //Left Panel
      91243 : DoClicUnique(RightPanel);  //Right Panel
      91244 : DoClicUnique(RadarPanel);  //Bottom Panel
      91245 : DoBack; //Back Panel = Back UI
      91246 : DoClicUnique(FriendBoard); //Friends menu
      91247 : DoClicUnique(Pause);       //Back to menu
      91248 : DoClicUnique(MapSystem);   //System map
      91249 : DoMapGalaxyShow;           //Galaxy map

      91250 : AssignFunc(1);
      91251 : DoClicUnique(LandingGear); //Landing gear
      91252 : DoClicUnique(CargoScoop);  //Cargo scoop
      91253 : DoClicUnique(NightVision); //Night vision
      91254 : DoClicUnique(Searchlight); //Spotlight
      91255 : DoClicUnique(CockpitMode); //Cockpit mode
      91256 : PipeMoteur; //Pipe engines
      91257 : PipeSystem; //Pipe systems
      91258 : PipeArmes;  //Pipe weapons
      91259 : DoClicUnique(PipeReset);     //Default
      91277 : DoClicUnique(ModeACS);       //ACS
      91278 : DoClicUnique(ChaffLauncher); //Chaff launcher
      91279 : DoClicUnique(ShieldCell);    //Shield cell
      91280 : DoClicUnique(HeatSink);      //Heat sink
      91281 : DoClicUnique(ChargeECM);     //Charge ECM

      91260 : AssignFunc(2);
      91261 : DoClicUnique(HardPoint);  //Hard point
      91262 : DoClic(PriorArmGroup);    //Fire group previous
      91263 : DoClic(NextArmGroup);     //Fire group next
      91264 : DoClic(PriorSubSystem);   //Previous subsystem
      91265 : DoClic(NextSubSystem);    //Next subsystem
      91266 : DoClic(MenacePrecedente); //Previous hostile
      91267 : DoClic(MenaceSuivante);   //Next hostile
      91268 : DoClic(MenacePrincipale); //Highest Threat
      91269 : DoClic(DoTarget12h);      //12h Target
      91270 : CibleOfAilier(1); //Target wingman 1
      91271 : CibleOfAilier(2); //Target wingman 2
      91272 : CibleOfAilier(3); //Target wingman 3
      91273 : DoClicUnique(FullSystem);    //Full systems
      91274 : DoClicUnique(FullMoteur);    //Full engines
      91275 : DoClicUnique(FullArme);      //Full weapons
      91276 : DoClicUnique(NextSubSystem); //Next route system

      91300 : AssignFunc(3);
      91301 : DoClicUnique(DoOrderPanel);        //Open orders
      91302 : DoClicUnique(RequestDock);         //Dock order
      91303 : DoClicUnique(DefensiveBehaviour);  //Defensive
      91304 : DoClicUnique(AggressiveBehaviour); //Aggressive
      91305 : DoClicUnique(HoldFire);            //Hold fire
      91306 : DoClicUnique(FocusTarget);         //Focus target
      91307 : DoClicUnique(HoldPosition);        //Hold position
      91308 : DoClicUnique(Follow);              //Follow order
      91309 : DoClicUnique(DoStartFighter);      //Open/close Bottom panel

      91350 : AssignFunc(4);
      91351 : DoClicUnique(Camera_ShipShow); //OPEN 91/92
      91352 : DoClic(Camera_Prior); //Prev camera
      91353 : DoClic(Camera_Next);  //Next camera
      91355 : DoClicUnique(Camera_Two);    //Cam 2
      91356 : DoClicUnique(Camera_Three);  //Cam 3
      91357 : DoClicUnique(Camera_Four);   //Cam 4
      91358 : DoClicUnique(Camera_Five);   //Cam 5
      91359 : DoClicUnique(Camera_Six);    //Cam 6
      91360 : DoClicUnique(Camera_Seven);  //Cam 7
      91361 : DoClicUnique(Camera_Eight);  //Cam 8
      91362 : DoClicUnique(Camera_Nine);   //Cam 9
      91363 : DoClicUnique(FreeCamera_Close); //ATH

      91400 : AssignFunc(5);
      91401 : DoClicUnique(DoAutoBreak); //Auto break
      91402 : DoClicUnique(DoVRSTurret); //Turret mode
      91403 : DoClicUnique(DoShipDismissRecall); //Ship call/dismiss

      { --- Galaxy map }
      91420 : GalaxyMap_display;            //Display context galaxy map
      91421 : DoClicUnique(DoGMBack);       //Back
      91422 : DoClicUnique(DoGMSlide);      //Slide
      91423 : DoClic(DoGMSteps);            //Steps
      91424 : DoClicUnique(DoGMMenu);       //Menu
      91425 : DoClicUnique(DoGMGoHome);     //Go home
      91426 : DoClicUnique(DoGMAllSlide);   //All slide
      91427 : DoClic(DoGMReward);           //Reward
      91428 : DoClic(DoGMUp);               //Up
      91429 : DoClic(DoGMUpRotate);         //Up rotate
      91430 : DoClic(DoGMLeft);             //Left
      91431 : DoClic(DoGMRight);            //Right
      91432 : DoClic(DoGMDown);             //Down
      91433 : DoClic(DoGMDownRotate);       //Down rotate
      91434 : DoClic(DoGMZoomIn);           //Zoom in
      91435 : DoClic(DoGMLeftRotate);       //Left rotate
      91436 : DoClic(DoGMForward);          //Forward
      91437 : DoClic(DoGMRightRotate);      //Right rotate
      91438 : DoClic(DoGMZoomOut);          //Zoom out


      { --- Sub contexts }
      91570 : DoClicUnique(Switch);
      91571 : DoClic(PauseBack);
      91580 : Menu_display;
      91581 : DoClicUnique(Menu_display);
      91590 : Drive_display;
      91591 : DoClicUnique(Drive_display);
      91596 : DoClic(DoNavSlide);
      91597 : DoClicUnique(FOwner.FContext.MainMenu_display);
      91598 : PauseBack;
      91599 : PauseEnable;
    end
  except
  end;
end;

procedure TEliteContext.Func_display;
begin
  with FOwner do begin
    UpdateZone( Context_Func );
    case IndexFunc of
      0 : UpdateZone( Context_Func0 );
      1 : UpdateZone( Context_Func1 );
      2 : UpdateZone( Context_Func2 );
      3 : UpdateZone( Context_Func3 );
      4 : UpdateZone( Context_Func4 );
      5 : UpdateZone( Context_Func5 );
    end;
  end
end;

procedure TEliteContext.GalaxyMap_display;
begin
  CurrContextTag := 91420;
  FOwner.UpdateZone( Context_GalaxyMap )
end;

function TEliteContext.GetCurrContextTag: Integer;
begin
  Result := KeyReadInt(ParamKey, 'CurrContextTag', 91007)
end;

function TEliteContext.GetElm: TEliteManager;
begin
  Result := FOwner.FEliteManager
end;

function TEliteContext.GetGMAllSlide: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'GMAllSlide', False)
end;

function TEliteContext.GetIndexFunc: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexFunc', 0)
end;

function TEliteContext.GetIndexGMMode: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexGMMode', 0)
end;

function TEliteContext.GetIndexMode: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexMode', 0)
end;

function TEliteContext.GetStep: Integer;
begin
  Result := KeyReadInt(ParamKey, 'Step', 1)
end;

function TEliteContext.GetSubGMIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubGMIndex', 2)
end;

function TEliteContext.GetSubIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubIndex', 2)
end;

procedure TEliteContext.Menu_display;
begin
  CurrContextTag := 91580;
  Step           := 1;
  FOwner.UpdateZone( Context_EliteMenu )
end;

procedure TEliteContext.PauseBack;
begin
  with FOwner, Context do case CurrContextTag of
    91007 : MainMenu_display;
    91420 : GalaxyMap_display;
    91580 : Menu_display;
    91590 : Drive_display;
  end
end;

procedure TEliteContext.PauseEnable;
begin
  Pause_display
end;

procedure TEliteContext.Pause_display;
begin
  FOwner.UpdateZone( Context_Pause )
end;

procedure TEliteContext.SetCurrContextTag(const Value: Integer);
begin
  KeyWrite(ParamKey, 'CurrContextTag', Value)
end;

procedure TEliteContext.SetGMAllSlide(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'GMAllSlide', Value)
end;

procedure TEliteContext.SetIndexFunc(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexFunc', Value)
end;

procedure TEliteContext.SetIndexGMMode(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexGMMode', Value)
end;

procedure TEliteContext.SetIndexMode(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexMode', Value)
end;

procedure TEliteContext.SetStep(const Value: Integer);
begin
  KeyWrite(ParamKey, 'Step', Value)
end;

procedure TEliteContext.SetSubGMIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubGMIndex', Value)
end;

procedure TEliteContext.SetSubIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubIndex', Value)
end;

procedure TEliteContext.StepSelector(const ATag: Integer);
begin
  case ATag of
    91210..91214 : Step := ATag - 91210 + 1;
    91215        : Step := 10;
    91216        : Step := 15;
    91217        : Step := 20;
    91218        : Step := 30;
    91219        : Step := 50;
    91220        : Step := 100;
    91221        : Step := 150;
    91222        : Step := 200;
    91223        : Step := 300;
    91224        : Step := 500;
    {exceptions}
    91225        : Step := 1;
    91226        : Step := 3;
    91227        : Step := 5;
    91228        : Step := 10;
  end;
  with FOwner do begin
    UpdateZone( Context_EliteMenu );
    FContext.FunctionSound;
  end
end;

procedure TEliteContext.Step_display;
begin
  FOwner.UpdateZone( Context_StepSelect )
end;

procedure TEliteContext.Switch;
begin
  with FOwner, FEliteStatus do case CurrContextTag of
    91580   : case TGuiType(GuiFocus) of
                gt_galaxymap : GalaxyMap_display;
                else if not Landed and not Docked then Drive_display;
              end;
    91590,
    91420   : Menu_display;
  end;
end;

initialization
  KeyMessageSender := TKeyMessageSender.Create
finalization
  KeyMessageSender.Free
end.


