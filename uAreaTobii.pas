{*******************************************************}
{                                                       }
{             08/2020  MaxiDonkey  Library              }
{                                                       }
{*******************************************************}

unit uAreaTobii;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Winapi.mmSystem, Winapi.shellapi,
  Datasnap.DBClient, Clipbrd,
  { Spec }
  SendKey32, KeysDef, EliteBindingsTools, uXMLDico, uEliteManager, uStatusReader, EyeXHost,
  uEyeXThread;

type
  TKindDisplay = (kd_none, kd_menu, kd_drive, kd_fss, kd_saa, kd_galaxymap, kd_systemmap,
                  kd_function, kd_pause, kd_mainmenu, kd_elitemenu, kd_keyboard, kd_params);

  TKeyboardContext = class;
  TEliteContext = class;

  TKeyboardKind    = (kk_alpha, kk_num, kk_special, kk_navigation, kk_critik);
  TKindMapCall     = (kmc_none, kmc_galaxy, kmc_system);
  TKindPanels      = (kp_none, kp_left, kp_right, kp_bottom);
  TKindHud         = (kh_menu, kh_drive);
  TContextNotify   = procedure of object;
  TGearTypeNotify  = procedure (const Mode: TLandingGearType) of object;
  TCockpitNotify   = procedure (const Mode : TCockpitModeType) of object;
  TIntegerNotify   = procedure (Value : Byte; Tms : Integer = 0) of object;
  TIntNotify       = procedure (Value : Byte) of object;
  TIntANotify      = procedure (Value : Integer) of object;
  TBoolNotify      = procedure (Value : Boolean) of object;
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
    { --- Assessors / mutators }
    function  GetPanel(index: Integer): TPanel;
    function  GetCaption(index: Integer): string;
    procedure SetCaption(index: Integer; const Value: string);
    function  GetSelected(index: Integer): Boolean;
    function  GetAreas: TAreaPanels;
    procedure SetBorder(const Value: Boolean);

  public
    { --- Creation of zones }
    procedure Add(const Panel: TPanel; const ATag: Integer; const AMode: TSelectMode; const ANull: Boolean = False);

    { --- Zone settings }
    procedure CaptionUpdate(const Raw: Integer; const Values: array of string);
    procedure VisibleUpdate(const Raw: Integer; const Values: array of Boolean; Update: Boolean = False);
    procedure SelModeUpdate(const Raw: Integer; const Values: array of TSelectMode);
    procedure TagsUpdate(const Raw: Integer; const Values: array of Integer);
    procedure NullUpdate(const Raw: Integer; const Values: array of Boolean);
    { --- Setting of an area in the screen matrix }
    procedure BoxUpdate(const Raw, Col, ATag: Integer; ACaption: string; AVisible: Boolean; AMode: TSelectMode; ANull: Boolean);

    procedure SetEliteStatus(const Value: TEliteStatus);

    procedure Repaint;
    procedure UpdateZone(const AreasContext: TContextNotify);
    { --- Reset context of areas }
    procedure ResetAreas;

    { --- Actions on external components or application }
    procedure SendSignal(const k1: string; const k2 : string = ''; const k3: string = '');

    { --- Read-only access to the TAreaPanels instance }
    property Areas: TAreaPanels read GetAreas;
    { --- Read-only access to contexts }
    property Context: TKeyboardContext read FContext;
    { --- Read-only access to Elite contexts }
    property Elite: TEliteContext read FElite;

    property Panel[index: Integer]: TPanel read GetPanel;
    { --- Read or modify the panel caption at the index index }
    property Caption[index: Integer]: string read GetCaption write SetCaption;
    { --- True if the index zone is selected }
    property Selected[index: Integer]: Boolean read GetSelected;
    { --- Border for all areas }
    property Border: Boolean read FBorder write SetBorder;
    { --- External update write redraw areas to tobii }
    property OnUpdateAreas: TNotifyEvent read FAreasUpdate write FAreasUpdate;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TEliteContext = class
  private
    FOwner        : TAreaTobii;
    FSwitchUpdate : Boolean;              //Update Switch display after ENTER
    { --- Local Tools }
    procedure StepSelector(const ATag: Integer);
    procedure DoClicUnique(const Method: TContextNotify); overload;
    procedure DoClicUnique(const Method: TGearTypeNotify); overload;
    procedure DoClicUnique(const Method: TCockpitNotify); overload;
    procedure DoClic(const Method: TContextNotify); overload;
    procedure DoClic(const Method: TIntNotify; Value: Integer); overload;
    procedure DoClic(const Method: TIntegerNotify; Value: Integer); overload;
    procedure NavClic(const SensA: TContextNotify; const SensB: TIntegerNotify); overload;
    procedure NavClic(const SensA: TContextNotify; const SensB: TIntANotify); overload;
    procedure DoUnicClic(const Method: TIntegerNotify; Value: Integer); overload;
    procedure DoUnicClic(const Method: TIntNotify; Value: Integer); overload;
    { --- Elite context }
    procedure Context_Elite;
    procedure Context_Pause;
    procedure Context_EliteMenu;
    procedure Context_StepSelect;
    procedure Context_EliteDrive;
    procedure Context_EliteDriveCombat;
    procedure Context_EliteDriveLanding;
    procedure Context_EliteDriveVRS;
    procedure Context_Func;                        // Function Header
    procedure Context_Func0;                       //0: Panels area
    procedure Context_Func1;                       //1: DIV area
    procedure Context_Func2;                       //2: Weapons area
    procedure Context_Func3;                       //3: Fighter area
    procedure Context_Func4;                       //4: Camera area

    {TODO}
    procedure Context_GalaxyMap;
    procedure Context_System_Map;
    procedure Context_ACS;
    procedure Context_DSD;

    { --- UnicStep managment }
    procedure DoUnicStep(Value: Integer);

    procedure Switch;
    procedure AssignFunc(const Index: Integer);

    { --- Retrieve context on app launch : Menu EliteDangerous }
    function EliteContextRetrieveOnLaunch(Sender: TKindHud):Boolean;

  private
    { --- Ascesseurs / Mutateurs }
    function  GetStep: Integer;
    procedure SetStep(const Value: Integer);
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
    function  GetSlideOffms: Integer;
    procedure SetSlideOffms(const Value: Integer);
    function  GetIndexSMMode: Integer;
    procedure SetIndexSMMode(const Value: Integer);
    function  GetSubSMIndex: Integer;
    procedure SetSubSMIndex(const Value: Integer);
    function  GetIndexACSMode: Integer;
    procedure SetIndexACSMode(const Value: Integer);
    function  GetSubACSIndex: Integer;
    procedure SetSubACSIndex(const Value: Integer);
    function  GetSlideAssist: Boolean;
    procedure SetSlideAssist(const Value: Boolean);
    function  GetIndexDSDMode: Integer;
    procedure SetIndexDSDMode(const Value: Integer);
    function  GetSubDSDIndex: Integer;
    procedure SetSubDSDIndex(const Value: Integer);
    function  GetStateBeforePause: TKindDisplay;
    procedure SetStateBeforePause(const Value: TKindDisplay);
    function  GetCurrentDisplay: TKindDisplay;
    procedure SetCurrentDisplay(const Value: TKindDisplay);
    function  GetMapCalled: TKindMapCall;
    procedure SetMapCalled(const Value: TKindMapCall);
    function  GetDisplayBeforeKeyboard: TKindDisplay;
    procedure SetDisplayBeforeKeyboard(const Value: TKindDisplay);
    function  GetComPanelOpened: Boolean;
    procedure SetComPanelOpened(const Value: Boolean);
    function  GetPanelCalled: TKindPanels;
    procedure SetPanelCalled(const Value: TKindPanels);
    function  GetIsDSDOpened: Boolean;
    procedure SetIsDSDOpened(const Value: Boolean);
    function  GetUnicStep: Boolean;
    procedure SetUnicStep(const Value: Boolean);
    function  GetCurrentHud: TKindHud;
    procedure SetCurrentHud(const Value: TKindHud);
    function  GetIndexLandingDrive: Integer;
    procedure SetIndexLandingDrive(const Value: Integer);
    function  GetSubLandingDriveIndex: Integer;
    procedure SetSubLandingDriveIndex(const Value: Integer);
    function  GetIndexOfPitchLanding: Integer;
    procedure SetIndexOfPitchLanding(const Value: Integer);
    function  GetIndexVRSMode: Integer;
    procedure SetIndexVRSMode(const Value: Integer);
    function  GetSubVRSIndex: Integer;
    procedure SetSubVRSIndex(const Value: Integer);
  public
    { --- Lauch and retieve Elite state }
    procedure TryLaunchOnMenuDisplay;
    procedure TryLaunchOnDriveDisplay;

    procedure Step_display;
    procedure Pause_display;
    procedure Menu_display;
    procedure Drive_display;
    procedure Func_display;
    procedure GalaxyMap_display;
    procedure SystemMap_display;
    procedure ACS_display;
    procedure DSD_display(forced: Boolean);
    procedure DoMenuEchap;
    procedure DoMenuEnter;

    { --- Elite notifier }
    procedure EliteNotify(const ATag: Integer);

    { --- Panels notify actions }
    procedure DoMapGalaxyShow(DisplayMap: boolean);
    procedure DoMapSystemShow(DisplayMap: boolean);
    procedure DoACSShow(DisplayPanel: boolean);
    procedure DoLeftPanelShow(DisplayPanel: boolean);
    procedure DoRightPanelShow(DisplayPanel: boolean);
    procedure DoComPanelShow(DisplayPanel: boolean);
    procedure DoRolePanelShow(DisplayPanel: boolean);
    procedure DoBackMenuShow;
    procedure DoFriendsMenuShow;
    procedure DoKeyBoardShow;
    procedure DoNavBack;

    { --- Elite main function notify actions }
    procedure PauseBack;
    procedure PauseFunctionBack;
    procedure PauseEnable;
    procedure DoTarget12h;
    procedure DoOrderPanel;
    procedure DoStartFighter;

    { --- VRS drive notify actions }
    procedure DoAutoBreak;
    procedure DoVRSTurret;
    procedure DoShipDismissRecall;
    procedure DoVRSSlide;                         //VRS Slide
    procedure DoVRSStep;                          //VRS Step
    procedure DoVRSUpView;                        //VRS Turret up view
    procedure DoVRSDownView;                      //VRS Turret down view
    procedure DoVRSTurnLeft;                      //VRS Turn left
    procedure DoVRSTurnRight;                     //VRS Turn right
    procedure DoVRSVerticalThrust;                //VRS Vertical thrust
    procedure DoVRSReversePropulsion;             //VRS Reverse propulsion
    procedure DoDriveAssist;

    { --- Navigation notify actions }
    procedure NavLauncher(const MethA: TContextNotify; const MethB: TIntegerNotify;
      Index: Integer); overload;
    procedure NavLauncherEx(const MethA: TContextNotify; const MethB: TIntegerNotify;
      Index: Integer);
    procedure DoNavModeChange;
    procedure DoNavSlide;
    procedure DoNavNord;
    procedure DoNavSud;
    procedure DoNavEst;
    procedure DoNavOuest;
    procedure DoNavPere;
    procedure DoNavFils;
    procedure DoSuperNavigation;
    procedure DoFSD;
    procedure DoPrevShip;
    procedure DoNextShip;
    procedure DoNavNordOuest;
    procedure DoNavNordEst;
    procedure DoNavSudOuest;
    procedure DoNavSudEst;

    { --- Galaxy map notify actions }
    procedure NavLauncher(const MethA: TContextNotify; const MethB: TIntANotify;
      Index: Integer); overload;
    procedure NavLauncherThrust(const MethA: TContextNotify; const MethB: TIntANotify;
      Index: Integer);
    procedure DoGMSlide;
    procedure DoGMSteps;
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

    { --- System map notify actions }
    procedure DoSMSlide;
    procedure DoSMSteps;
    procedure DoSMZoomIn;
    procedure DoSMZoomOut;
    procedure DoSMBack;
    procedure DoSMGoUp;
    procedure DoSMGoDown;
    procedure DoSMGoLeft;
    procedure DoSMGoRight;

    { --- Exploration FSS notify actions }
    procedure DoACSSlide;
    procedure DoACSSteps;
    procedure DoACSZoomIn;
    procedure DoACSAnalyse;
    procedure DoACSZoomOut;
    procedure DoACSHelp;
    procedure DoACSBack;
    procedure DoACSRadioDec;
    procedure DoACSPitchInc;
    procedure DoACSRadioInc;
    procedure DoACSYawDec;
    procedure DoACSYawInc;
    procedure DoACSGetTarget;
    procedure DoACSMiniZoomIn;
    procedure DoACSPitchDec;
    procedure DoACSMiniZoomOut;

    { --- Surface Analyse Area notify actions }
    procedure DoDSDSlide;
    procedure DoDSDSteps;
    procedure DoDSDZoomIn;
    procedure DoDSDZoomOut;
    procedure DoDSDBack;
    procedure DoDSDPitchUp;
    procedure DoDSDYawLeft;
    procedure DoDSDYawRight;
    procedure DoDSDViewChange;
    procedure DoDSDPitchDown;
    procedure DoDSDFire;

    { --- Drive landing or docking }
    procedure DoDriveLandingSlide;
    procedure DoDriveLandingStep;
    procedure DoDriveLandingPitch;

    procedure DoDriveLandingPitchUp;
    procedure DoLandingPitchDown;
    procedure DoDriveLandingRollLeft;
    procedure DoDriveLandingRollRight;
    procedure DoDriveLandingYawLeft;
    procedure DoDriveLandingYawRight;

    procedure DoDriveLandingUpThrust(NotLanding: Boolean = False);
    procedure DoDriveLandingDownThrust(NotLanding: Boolean = False);
    procedure DoDriveLandingLeftThrust(NotLanding: Boolean = False);
    procedure DoDriveLandingRightThrust(NotLanding: Boolean = False);
    procedure DoDriveLandingForwardThrust(NotLanding: Boolean = False);
    procedure DoDriveLandingBackThrust(NotLanding: Boolean = False);

    { --- Check in vehicle }
    function IsFlying:Boolean;
    { --- Check Docked }
    function IsDocked:Boolean;
    { --- Check Landed }
    function IsLanded:Boolean;
    { --- Check if map opened }
    function IsMapOpened:Boolean;

    property Step: Integer read GetStep write SetStep;
    property UnicStep: Boolean read GetUnicStep write SetUnicStep;
    property IndexFunc: Integer read GetIndexFunc write SetIndexFunc;
    { --- Ship drive indexes}
    property IndexMode: Integer read GetIndexMode write SetIndexMode;
    property SubIndex: Integer read GetSubIndex write SetSubIndex;
    { --- Galaxy map indexes}
    property IndexGMMode: Integer read GetIndexGMMode write SetIndexGMMode;
    property SubGMIndex: Integer read GetSubGMIndex write SetSubGMIndex;
    property GMAllSlide: Boolean read GetGMAllSlide write SetGMAllSlide;
    { --- System map indexes}
    property IndexSMMode: Integer read GetIndexSMMode write SetIndexSMMode;
    property SubSMIndex: Integer read GetSubSMIndex write SetSubSMIndex;
    { --- Exploration FSS indexes }
    property IndexACSMode: Integer read GetIndexACSMode write SetIndexACSMode;
    property SubACSIndex: Integer read GetSubACSIndex write SetSubACSIndex;
    { --- SAA indexes}
    property IndexDSDMode: Integer read GetIndexDSDMode write SetIndexDSDMode;
    property SubDSDIndex: Integer read GetSubDSDIndex write SetSubDSDIndex;
    { --- VRS indexes}
    property IndexVRSMode: Integer read GetIndexVRSMode write SetIndexVRSMode;
    property SubVRSIndex: Integer read GetSubVRSIndex write SetSubVRSIndex;
    { --- Landing or dicking drive indexes }
    property IndexLandingDrive: Integer read GetIndexLandingDrive write SetIndexLandingDrive;
    property SubLandingDriveIndex: Integer read GetSubLandingDriveIndex write SetSubLandingDriveIndex;
    property IndexOfPitchLanding: Integer read GetIndexOfPitchLanding write SetIndexOfPitchLanding;
    { --- Time slide off }
    property SlideOffms: Integer read GetSlideOffms write SetSlideOffms;
    { --- Params }
    property SlideAssist: Boolean read GetSlideAssist write SetSlideAssist;
    { --- Com panel opened }
    property ComPanelOpened: Boolean read GetComPanelOpened write SetComPanelOpened;
    { --- Kind of display before action }
    property DisplayBefore: TKindDisplay read GetStateBeforePause write SetStateBeforePause;
    property DisplayBeforeKeyboard: TKindDisplay read GetDisplayBeforeKeyboard write SetDisplayBeforeKeyboard;
    { --- Current kind od display }
    property CurrentDisplay: TKindDisplay read GetCurrentDisplay write SetCurrentDisplay;
    { --- Map is call }
    property MapCalled: TKindMapCall read GetMapCalled write SetMapCalled;
    { --- Panel called }
    property PanelCalled: TKindPanels read GetPanelCalled write SetPanelCalled;
    { --- Indic Opened DSD }
    property IsDSDOpened: Boolean read GetIsDSDOpened write SetIsDSDOpened;
    { --- Hud selected }
    property CurrentHud: TKindHud read GetCurrentHud write SetCurrentHud;
    { --- Indic for Process Thread to update menu SWITH area }
    property SwitchUpdate : Boolean read FSwitchUpdate write FSwitchUpdate;

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
    procedure DataDico; //Temporary
    procedure AltInsert(const Alt: Integer);
    procedure DoOnShift;
    procedure DoWithCar(const ATag: Integer);
    procedure AltDisplay;
    procedure DoBackSpace;
    procedure DoPrev;
    procedure DoNext;
    procedure DoAlphaRight;
    procedure DoAlphaLeft;
    procedure DoKeyBoardBack;
    procedure DoMainKeyBoardLaunch;
    procedure DoParametersLaunch;
    { --- State indicator management }
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
    { --- Assessors / Mutators }
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

  TContextObserver = class(TTHread)
  private
    ThAreaTobii     : TAreaTobii;
    Old_Docked      : Boolean;
    Old_HardPoints  : Boolean;
    Old_LandingGear : Boolean;
    Old_InSRV       : Boolean;
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create(const AAreaTobii: TAreaTobii);
  end;

var
  ContextObserver : TContextObserver;



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
  end
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

procedure Delay(ms: Cardinal);
var
  S : Cardinal;
begin
  S := GetTickCount + ms;
  with Application do repeat
    Sleep( 10 );
  until Terminated or (GetTickCount > S)
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
  inherited
end;

procedure TAreaTobii.DoLaunchElite;
begin
  with Context, FEliteManager, FEliteStatus, Elite do begin
    CurrentDisplay := kd_elitemenu;
    UpdateZone( Context_Elite );
    FunctionSound;
    EliteForeGround
  end;
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
     91001 : DoMainKeyBoardLaunch;
     91002 : ;
     91003 : ; //DataDico;
     91004 : DoLaunchElite;
     91005 : ;
     91006 : DoParametersLaunch;
     91007 : DoKeyBoardBack;
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
  if Assigned(X) and (X is TForm) then Result := TForm(X)
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
    OnUnSelect      := DoOnUnSelect
  end;
  FDico := TXmlAlternative.Create;
  FElite := TEliteContext.Create(Self)
end;

procedure TAreaTobii.NullUpdate(const Raw: Integer;
  const Values: array of Boolean);
begin
  Areas.NullUpdate(Raw, Values)
end;

procedure TAreaTobii.Repaint;
begin
  Areas.Repaint
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
  with FContext do SendSignal(k1, k2, k3)
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
    end else
      Alt := FindAltPost(FLastWord, CapsLock);
  end;
  Make_Alternative( Alt )
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
    Add(Panel42, 42, sm_none);  Add(Panel43, 0,  sm_none)
    //Panel43 only serves as a bias
  end;
end;

procedure TKeyboardContext.CleanBuffer;
begin
  FLastWord := EmptyStr;
  FBuffer   := EmptyStr
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
    Border := True
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardLeft;
begin
  AreaPanels.Unselect;
  Context_KeyboardMain;
  LeftKeyboard := True;
  KeyboardKind := kk_alpha;
  with FOwner do begin
    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91010, GetCar('A'),     True, sm_blinkback, False);
    BoxUpdate(3, 3, 91035, GetCar('Z'),     True, sm_blinkback, False);
    BoxUpdate(3, 4, 91014, GetCar('E'),     True, sm_blinkback, False);
    BoxUpdate(3, 5, 91027, GetCar('R'),     True, sm_blinkback, False);
    BoxUpdate(3, 6, 91029, GetCar('T'),     True, sm_blinkback, False);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91026, GetCar('Q'),     True, sm_blinkback, False);
    BoxUpdate(4, 3, 91028, GetCar('S'),     True, sm_blinkback, False);
    BoxUpdate(4, 4, 91013, GetCar('D'),     True, sm_blinkback, False);
    BoxUpdate(4, 5, 91015, GetCar('F'),     True, sm_blinkback, False);
    BoxUpdate(4, 6, 91016, GetCar('G'),     True, sm_blinkback, False);

    BoxUpdate(5, 2, 91032, GetCar('W'),     True, sm_blinkback, False);
    BoxUpdate(5, 3, 91033, GetCar('X'),     True, sm_blinkback, False);
    BoxUpdate(5, 4, 91012, GetCar('C'),     True, sm_blinkback, False);
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
    BoxUpdate(6, 7, 91101, 'NUMPAD',        True, sm_blinkback, False)
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
    Border := True
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
    Border := True
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
    Border := True
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_KeyboardRight;
begin
  AreaPanels.Unselect;
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
    AltDisplay
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
    Border := True
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
    Border := True
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
    Border := True
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_MainMenu;
begin
  ResetAreas;
  AreaPanels.Unselect;
  with FOwner do begin
    Border := False;
    BoxUpdate(1, 7, 91200, '',                True, sm_blinkback, True);    //91002
    BoxUpdate(2, 7, 91001, 'KEYBOARD',        True, sm_blinkback, False);
    BoxUpdate(3, 7, 91200, '',                True, sm_blinkback, True);    //91003
    BoxUpdate(4, 7, 91004, 'ELITE DANGEROUS', True, sm_blinkback, False);
    BoxUpdate(5, 7, 91200, '',                True, sm_blinkback, True);    //91005
    BoxUpdate(6, 7, 91006, 'PARAMETERS',      True, sm_blinkback, False)
  end;
  FunctionSound
end;

procedure TKeyboardContext.Context_Parameters;
begin
  ResetAreas;
  with FOwner do begin
    Border := False;
    BoxUpdate(1, 2, 91000, 'QUIT',   True, sm_blinkback, False);
    BoxUpdate(1, 7, 91007, 'BACK',   True, sm_blinkback, False)
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
    BoxUpdate(1, 7, 91156, '',      True, sm_blinkback, True)
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
  FCall   := False
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
  AreaPanels.Unselect
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

procedure TKeyboardContext.DoKeyBoardBack;
begin
  with FOwner, FElite, FEliteManager, FContext do begin
    Border := False;
    case DisplayBeforeKeyboard of
      kd_menu     : Menu_display;
      else MainMenu_display;
    end;
    DisplayBeforeKeyboard := kd_none
  end
end;

procedure TKeyboardContext.DoMainKeyBoardLaunch;
begin
  with FOwner, FContext, FElite do begin
    CurrentDisplay := kd_keyboard;
    UpdateZone( Context_Keyboard );
  end
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
  SendMsgToA('Key_PageDown')
end;

procedure TKeyboardContext.DoPageUp;
begin
  CleanBuffer;
  SendMsgToA('Key_PageUp')
end;

procedure TKeyboardContext.DoParametersLaunch;
begin
  with FOwner, FContext, FElite do begin
    CurrentDisplay := kd_params;
    UpdateZone( Context_Parameters )
  end
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
  SendMsgToA('Key_End', 'Key_LeftShift', 'Key_RightControl')
end;

procedure TKeyboardContext.DoSelEnd;
begin
  CleanBuffer;
  SendMsgToA('Key_End', 'Key_LeftShift')
end;

procedure TKeyboardContext.DoSelHome;
begin
  CleanBuffer;
  SendMsgToA('Key_Home', 'Key_LeftShift')
end;

procedure TKeyboardContext.DoSelLeft;
begin
  CleanBuffer;
  SendMsgTo('Key_LeftArrow', 'Key_LeftShift')
end;

procedure TKeyboardContext.DoSelPageDown;
begin
  CleanBuffer;
  SendMsgToA('Key_PageDown', 'Key_LeftShift')
end;

procedure TKeyboardContext.DoSelPageUp;
begin
  CleanBuffer;
  SendMsgToA('Key_PageUp', 'Key_LeftShift')
end;

procedure TKeyboardContext.DoSelRight;
begin
  CleanBuffer;
  SendMsgTo('Key_RightArrow', 'Key_LeftShift')
end;

procedure TKeyboardContext.DoSelTop;
begin
  CleanBuffer;
  SendMsgToA('Key_Home', 'Key_LeftShift', 'Key_RightControl')
end;

procedure TKeyboardContext.DoSelWordLeft;
begin
  CleanBuffer;
  SendMsgTo('Key_LeftArrow', 'Key_LeftShift', 'Key_RightControl')
end;

procedure TKeyboardContext.DoSelWordRight;
begin
  CleanBuffer;
  SendMsgTo('Key_RightArrow', 'Key_LeftShift', 'Key_RightControl')
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
//  RepaintKeyboardAlpha  { --- WARNING only display one character }
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
  Result := KeyReadBoolean(ParamKey, 'CapsLock')
end;

function TKeyboardContext.GetCar(const Car: Char): Char;
begin
  Result := Chr( Ord(Car) + 32 * Integer( CapsLock ) )
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
     { --- Add char to the external component }
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
     { --- Insert alternative to the external component }
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
    FElite.CurrentDisplay := kd_mainmenu;
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
    NullUpdate(6, [True, True, True, True, True, True, True])
  end
end;

procedure TKeyboardContext.SelectAll;
begin
  SendSignal('Key_End',  'Key_LeftControl');
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
  TKeyMessageSender.Signal( EncodeKey( k1, k2, k3 ), 10, WITH_KEYUP )
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

procedure TEliteContext.ACS_display;
begin
  CurrentDisplay := kd_fss;
  FOwner.UpdateZone( Context_ACS )
end;

procedure TEliteContext.AssignFunc(const Index: Integer);
begin
  IndexFunc := Index;
  Func_display
end;

procedure TEliteContext.Context_ACS;
var
  ASt      : string;
  BtnMode  : TSelectMode;
begin
  BtnMode  := sm_blinkback;
  if IndexACSMode = 0 then begin
    BtnMode := sm_directnotnull;
    ASt     := 'NO STEP'
  end else
    ASt  := Format('STEP %d', [SubACSIndex]);

  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91471, 'SLIDE',         True, sm_blinkback, False);
    BoxUpdate(1, 2, 91472, ASt,             True, sm_blinkback, False);
    BoxUpdate(1, 3, 91473, 'ZOOM IN',       True, BtnMode, False);
    BoxUpdate(1, 4, 91474, 'ANALYSE',       True, sm_blinkback, False);
    BoxUpdate(1, 5, 91475, 'ZOOM OUT',      True, BtnMode, False);
    BoxUpdate(1, 6, 91476, 'HELP',          True, sm_blinkback, False);
    BoxUpdate(1, 7, 91477, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91478, 'RADIO DEC',     True, BtnMode, False);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91479, 'PITCH INC',     True, BtnMode, False);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91480, 'RADIO INC',     True, BtnMode, False);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91481, 'YAW DEC',       True, BtnMode, False);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91482, 'YAW INC',       True, BtnMode, False);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91483, 'GET TARGET',    True, sm_blinkback, False);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91484, 'MINI ZOOM IN',  True, sm_blinkback, False);
    BoxUpdate(6, 4, 91485, 'PITCH DEC',     True, BtnMode, False);
    BoxUpdate(6, 5, 91486, 'MINI ZOOM OUT', True, sm_blinkback, False);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_DSD;
var
  ASt      : string;
  BtnMode  : TSelectMode;
begin
  BtnMode  := sm_blinkback;
  if IndexDSDMode = 0 then begin
    BtnMode := sm_directnotnull;
    ASt     := 'NO STEP'
  end else
    ASt  := Format('STEP %d', [SubDSDIndex]);

  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91491, 'SLIDE',         True, sm_blinkback, False);
    BoxUpdate(1, 2, 91492, ASt,             True, sm_blinkback, False);
    BoxUpdate(1, 3, 91493, 'ZOOM IN',       True, BtnMode, False);
    BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 5, 91494, 'ZOOM OUT',      True, BtnMode, False);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91495, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91496, 'PITCH UP',      True, BtnMode, False);
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
    BoxUpdate(4, 2, 91497, 'YAW LEFT',      True, BtnMode, False);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91498, 'YAW RIGHT',     True, BtnMode, False);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91499, 'VIEW CHANGE',   True, sm_blinkback, False);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91500, 'PITCH DOWN',    True, BtnMode, False);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91501, 'FIRE',          True, sm_blinkback, False);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_Elite;
begin
  EliteForeGround;
  AreaPanels.Unselect;
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
    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True)
  end
end;

procedure TEliteContext.Context_EliteDrive;
var
  ASt     : string;
  BtnMode : TSelectMode;
begin
  AreaPanels.Unselect;
  BtnMode := sm_blinkback;
  if IndexMode = 0 then begin
    BtnMode := sm_directnotnull;
    ASt     := 'NO STEP'
  end else ASt := Format('STEP %d', [SubIndex]);

  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91596, 'SLIDE',         True, sm_blinkback, False);
    BoxUpdate(1, 2, 91238, Ast,             True, sm_blinkback, False);
    if MapCalled in [kmc_galaxy, kmc_system] then begin
      BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
      BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
      BoxUpdate(1, 5, 91200, '',              True, sm_blinkback, True);
      BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    end else begin
      BoxUpdate(1, 3, 91293, 'SUPERCRUISE',   True, sm_blinkback, False);
      BoxUpdate(1, 4, 91239, 'FUNC',          True, sm_blinkback, False);
      BoxUpdate(1, 5, 91294, 'FSD',           True, sm_blinkback, False);
      BoxUpdate(1, 6, 91269, '12H TARGET',    True, sm_blinkback, False)
    end;
    BoxUpdate(1, 7, 91550, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91282, 'SPEED LESS',    True, sm_blinkback, False);
    BoxUpdate(2, 2, 91234, 'ROLL LEFT',     True, BtnMode, False);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91230, 'PITCH UP',      True, BtnMode, False);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91235, 'ROLL RIGHT',    True, BtnMode, False);
    BoxUpdate(2, 7, 91283, 'SPEED MORE',    True, sm_blinkback, False);

    BoxUpdate(3, 1, 91284, 'SPEED 25%',     True, sm_blinkback, False);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
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
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);
//    BoxUpdate(5, 7, 91289, 'INV SPEED',     True, sm_blinkback, False);

    BoxUpdate(6, 1, 91570, 'SWITCH',        True, sm_blinkback, False);
    BoxUpdate(6, 2, 91290, 'BOOST',         True, sm_blinkback, False);
    BoxUpdate(6, 3, 91237, 'SHOOT 2',       True, sm_blinkback, False);
    BoxUpdate(6, 4, 91231, 'PITCH DOWN',    True, BtnMode, False);
    BoxUpdate(6, 5, 91236, 'SHOOT 1',       True, sm_blinkback, False);
    BoxUpdate(6, 6, 91291, 'FLIGHT ASSIST', True, sm_blinkback, False);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_EliteDriveCombat;
begin
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91234, 'ROLL LEFT',     True, sm_directnotnull, False);
    BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 3, 91290, 'BOOST',         True, sm_blinkback, False);
    BoxUpdate(1, 4, 91239, 'FUNC',          True, sm_blinkback, False);
    BoxUpdate(1, 5, 91291, 'FLIGHT ASSIST', True, sm_blinkback, False);
    BoxUpdate(1, 6, 91269, '12H TARGET',    True, sm_blinkback, False);
    BoxUpdate(1, 7, 91235, 'ROLL RIGHT',    True, sm_directnotnull, False);

    BoxUpdate(2, 1, 91268, 'HIGEST THREAT', True, sm_blinkback, False);
    BoxUpdate(2, 2, 91511, 'NORD WEST',     True, sm_directnotnull, False);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91230, 'PITCH UP',      True, sm_directnotnull, False);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91512, 'NORD EST',      True, sm_directnotnull, False);
    BoxUpdate(2, 7, 91267, 'NEXT HOSTILE',  True, sm_blinkback, False);

    BoxUpdate(3, 1, 91284, 'SPEED 25%',     True, sm_blinkback, False);
    BoxUpdate(3, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91285, 'SPEED 0%',      True, sm_blinkback, False);

    BoxUpdate(4, 1, 91286, 'SPEED 50%',     True, sm_blinkback, False);
    BoxUpdate(4, 2, 91232, 'YAW LEFT',      True, sm_directnotnull, False);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91233, 'YAW RIGHT',     True, sm_directnotnull, False);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 7, 91287, 'SPEED 100%',    True, sm_blinkback, False);

    BoxUpdate(5, 1, 91288, 'SPEED 75%',     True, sm_blinkback, False);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);
//    BoxUpdate(5, 7, 91289, 'INV SPEED',     True, sm_blinkback, False);

    BoxUpdate(6, 1, 91570, 'SWITCH',        True, sm_blinkback, False);
    BoxUpdate(6, 2, 91513, 'SUD WEST',      True, sm_directnotnull, False);
    BoxUpdate(6, 3, 91237, 'SHOOT 2',       True, sm_blinkback, False);
    BoxUpdate(6, 4, 91231, 'PITCH DOWN',    True, sm_directnotnull, False);
    BoxUpdate(6, 5, 91236, 'SHOOT 1',       True, sm_blinkback, False);
    BoxUpdate(6, 6, 91514, 'SUD EST',       True, sm_directnotnull, False);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_EliteDriveLanding;
var
  ASt     : string;
  ASt1    : string;
  BtnMode : TSelectMode;
begin
  AreaPanels.Unselect;
  BtnMode := sm_blinkback;
  if IndexLandingDrive = 0 then begin
    BtnMode := sm_directnotnull;
    ASt     := 'NO STEP'
  end else ASt := Format('STEP %d', [SubLandingDriveIndex]);
  ASt1 := Format('PITCH %d', [IndexOfPitchLanding + 1]);
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91522, 'SLIDE',           True, sm_blinkback, False);
    BoxUpdate(1, 2, 91523, ASt,               True, sm_blinkback, False);
    BoxUpdate(1, 3, 91524, ASt1,              True, sm_blinkback, False);
    BoxUpdate(1, 4, 91239, 'FUNC',            True, sm_blinkback, False);
    BoxUpdate(1, 5, 91525, 'PITCH UP',        True, sm_blinkback, False);
    BoxUpdate(1, 6, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(1, 7, 91285, 'SPEED 0%',        True, sm_blinkback, False);

    BoxUpdate(2, 1, 91526, 'ROLL LEFT',       True, sm_blinkback, False);
    BoxUpdate(2, 2, 91527, 'BACKWARD THRUST', True, BtnMode, False);
    BoxUpdate(2, 3, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(2, 4, 91528, 'UP THRUST',       True, BtnMode, False);
    BoxUpdate(2, 5, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(2, 6, 91529, 'FORWARD THRUST',  True, BtnMode, False);
    BoxUpdate(2, 7, 91530, 'ROLL RIGHT',      True, sm_blinkback, False);

    BoxUpdate(3, 1, 91282, 'SPEED LESS',      True, sm_blinkback, False);
    BoxUpdate(3, 2, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(3, 3, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(3, 7, 91283, 'SPEED MORE',      True, sm_blinkback, False);

    BoxUpdate(4, 1, 91531, 'YAW LEFT',        True, sm_blinkback, False);
    BoxUpdate(4, 2, 91532, 'LEFT THRUST',     True, BtnMode, False);
    BoxUpdate(4, 3, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(4, 6, 91533, 'RIGHT THRUST',    True, BtnMode, False);
    BoxUpdate(4, 7, 91534, 'YAW RIGHT',       True, sm_blinkback, False);

    BoxUpdate(5, 1, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',                True, sm_blinkback, True);

    BoxUpdate(6, 1, 91570, 'SWITCH',          True, sm_blinkback, False);
    BoxUpdate(6, 2, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(6, 4, 91535, 'DOWN THRUST',     True, BtnMode, False);
    BoxUpdate(6, 5, 91536, 'PITCH DOWN',      True, sm_blinkback, False);
    BoxUpdate(6, 6, 91200, '',                True, sm_blinkback, True);
    BoxUpdate(6, 7, 91599, 'PAUSE',           True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_EliteMenu;
begin
  AreaPanels.Unselect;
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91209, 'STEP',          True, sm_blinkback, False);
    BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
    if MapCalled in [kmc_galaxy, kmc_system]
      then BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True)
      else BoxUpdate(1, 4, 91239, 'FUNC',          True, sm_blinkback, False);
    { --- TODO Show Keybord with context }
    if (MapCalled = kmc_galaxy) or ComPanelOpened
      then BoxUpdate(1, 5, 91560, 'KEYBOARD',      True, sm_blinkback, False)
      else BoxUpdate(1, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91550, 'BACK',          True, sm_blinkback, False);

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

    if not IsDocked or (MapCalled in [kmc_galaxy, kmc_system])
      then BoxUpdate(6, 1, 91570, 'SWITCH',        True, sm_blinkback, False)
      else BoxUpdate(6, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91207, 'ESC',           True, sm_blinkback, False);
    BoxUpdate(6, 4, 91202, 'BOTTOM',        True, sm_blinkback, False);
    BoxUpdate(6, 5, 91208, 'ENTER',         True, sm_blinkback, False);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_Func;
begin
  AreaPanels.Unselect;
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91240, 'PANELS',        True, sm_blinkback, False);
    if not IsDocked then begin
      BoxUpdate(1, 2, 91250, 'DIV',           True, sm_blinkback, False);
      BoxUpdate(1, 3, 91260, 'WEAPONS',       True, sm_blinkback, False);
      BoxUpdate(1, 4, 91300, 'FIGHTER',       True, sm_blinkback, False);
//      BoxUpdate(1, 6, 91400, 'SRV',           True, sm_blinkback, False)
      BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True)
    end else begin
      BoxUpdate(1, 2, 91200, '',              True, sm_blinkback, True);
      BoxUpdate(1, 3, 91200, '',              True, sm_blinkback, True);
      BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
      BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True)
    end;
    BoxUpdate(1, 5, 91350, 'CAMERA',        True, sm_blinkback, False);
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
    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True)
  end
end;

procedure TEliteContext.Context_Func0;
begin
  with FOwner do begin
    BoxUpdate(2, 1, 91248, 'SYSTEM MAP',    True, sm_blinkback, False);
    BoxUpdate(2, 4, 91241, 'COM PANEL',     True, sm_blinkback, False);
    BoxUpdate(2, 7, 91249, 'GALAXY MAP',    True, sm_blinkback, False);

    BoxUpdate(4, 1, 91242, 'LEFT PANEL',    True, sm_blinkback, False);
    BoxUpdate(4, 7, 91243, 'RIGHT PANEL',   True, sm_blinkback, False);

    BoxUpdate(6, 1, 91246, 'FRIENDS MENU',  True, sm_blinkback, False);
    BoxUpdate(6, 4, 91244, 'BOTTOM PANEL',  True, sm_blinkback, False);
    BoxUpdate(6, 7, 91247, 'BACK TO MENU',  True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_Func1;
begin
  with FOwner do begin
    BoxUpdate(2, 2, 91251, 'LANDING GEAR',     True, sm_blinkback, False);
    BoxUpdate(2, 3, 91252, 'CARGO SCOOP',      True, sm_blinkback, False);
    BoxUpdate(2, 4, 91253, 'NIGHT VISION',     True, sm_blinkback, False);
    BoxUpdate(2, 5, 91254, 'SPOTLIGHT',        True, sm_blinkback, False);
    BoxUpdate(2, 6, 91255, 'COCKPIT MODE',     True, sm_blinkback, False);
    BoxUpdate(2, 7, 91277, 'S.A.A',            True, sm_blinkback, False);

    BoxUpdate(3, 1, 91281, 'CHARGE ECM',       True, sm_blinkback, False);

    BoxUpdate(4, 1, 91279, 'SHIELD CELL',      True, sm_blinkback, False);
    BoxUpdate(4, 4, 91538, 'UP THRUST',        True, sm_directnotnull, False);
    BoxUpdate(4, 7, 91295, 'PREV SHIP',        True, sm_blinkback, False);

    BoxUpdate(5, 1, 91280, 'HEAT SINK',        True, sm_blinkback, False);
    BoxUpdate(5, 3, 91542, 'LEFT THRUST',      True, sm_directnotnull, False);
    BoxUpdate(5, 5, 91543, 'RIGHT THRUST',     True, sm_directnotnull, False);
    BoxUpdate(5, 7, 91296, 'NEXT SHIP',        True, sm_blinkback, False);

    BoxUpdate(6, 1, 91278, 'CHAFF LAUNCHER',   True, sm_blinkback, False);
    BoxUpdate(6, 3, 91537, 'BACKWARD THRUST',  True, sm_directnotnull, False);
    BoxUpdate(6, 4, 91545, 'DOWN THRUST',      True, sm_directnotnull, False);
    BoxUpdate(6, 5, 91539, 'FORWARD THRUST',   True, sm_directnotnull, False);
  end
end;

procedure TEliteContext.Context_Func2;
begin
  with FOwner do begin
    BoxUpdate(2, 1, 91276, 'NEXT ROUTE',       True, sm_blinkback, False);
    BoxUpdate(2, 2, 91261, 'HARD POINTS',      True, sm_blinkback, False);
    BoxUpdate(2, 3, 91262, 'PREV GROUP',       True, sm_blinkback, False);
    BoxUpdate(2, 4, 91263, 'NEXT GROUP',       True, sm_blinkback, False);
    BoxUpdate(2, 6, 91264, 'PREV SUBSYSTEM',   True, sm_blinkback, False);
    BoxUpdate(2, 7, 91265, 'NEXT SUBSYSTEM',   True, sm_blinkback, False);

    BoxUpdate(3, 1, 91306, 'FOCUS TARGET',     True, sm_blinkback, False);
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
    BoxUpdate(6, 7, 91269, '12H TARGET',       True, sm_blinkback, False)
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
    BoxUpdate(5, 6, 91308, 'FOLLOW ME',        True, sm_blinkback, False)
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

    BoxUpdate(4, 1, 91363, 'ATH',              True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_EliteDriveVRS;
var
  ASt     : string;
  BtnMode : TSelectMode;
begin
  AreaPanels.Unselect;
  BtnMode := sm_blinkback;
  if IndexVRSMode  = 0 then begin
    BtnMode := sm_directnotnull;
    ASt     := 'NO STEP'
  end else ASt := Format('STEP %d', [SubVRSIndex]);
  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91404, 'SLIDE',               True, sm_blinkback, False);
    BoxUpdate(1, 2, 91405, ASt,                   True, sm_blinkback, False);
    BoxUpdate(1, 3, 91402, 'TURRET MODE',         True, sm_blinkback, False);
    BoxUpdate(1, 4, 91239, 'FUNC',                True, sm_blinkback, False);
    BoxUpdate(1, 5, 91403, 'SHIP Recall/Dismiss', True, sm_blinkback, False);
    BoxUpdate(1, 6, 91269, '12H TARGET',          True, sm_blinkback, False);
    BoxUpdate(1, 7, 91550, 'BACK',                True, sm_blinkback, False);

    BoxUpdate(2, 4, 91406, 'UP VIEW',             True, BtnMode, False);

    BoxUpdate(3, 1, 91282, 'LESS SPEED',          True, sm_blinkback, False);
    BoxUpdate(3, 7, 91283, 'MORE SPEED',          True, sm_blinkback, False);

    BoxUpdate(4, 2, 91408, 'STEER LEFT',          True, BtnMode, False);
    BoxUpdate(4, 6, 91409, 'STEER RIGHT',         True, BtnMode, False);

    BoxUpdate(5, 1, 91401, 'AUTO BRAK',           True, sm_blinkback, False);
    BoxUpdate(5, 7, 91200, '',                    True, sm_blinkback, True);
//    BoxUpdate(5, 7, 91411, 'INV SPEED',           True, sm_blinkback, False);

    BoxUpdate(6, 1, 91570, 'SWITCH',              True, sm_blinkback, False);
    BoxUpdate(6, 2, 91410, 'VERTICAL THRUST',     True, sm_blinkback, False);
    BoxUpdate(6, 3, 91237, 'SHOOT 2',             True, sm_blinkback, False);
    BoxUpdate(6, 4, 91407, 'DOWN VIEW',           True, BtnMode, False);
    BoxUpdate(6, 5, 91236, 'SHOOT 1',             True, sm_blinkback, False);
    BoxUpdate(6, 6, 91412, 'DRIVE ASSIST',        True, sm_blinkback, False);
    BoxUpdate(6, 7, 91599, 'PAUSE',               True, sm_blinkback, False);
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
    BoxUpdate(1, 3, 91434, 'ZOOM IN',       True, BtnMode1, False);
    BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 5, 91438, 'ZOOM OUT',      True, BtnMode1, False);
    BoxUpdate(1, 6, 91425, 'GO HOME',       True, sm_blinkback, False);
    BoxUpdate(1, 7, 91421, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91426, ASt1,            True, sm_blinkback, False);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91427, 'REWARD',        True, BtnMode, False);
    BoxUpdate(2, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(3, 1, 91429, 'UP ROTATE',     True, BtnMode1, False);
    BoxUpdate(3, 2, 91428, 'UP',            True, BtnMode1, False);
    BoxUpdate(3, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(3, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(4, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 2, 91430, 'LEFT',          True, BtnMode, False);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91431, 'RIGHT',         True, BtnMode, False);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91433, 'DOWN ROTATE',   True, BtnMode1, False);
    BoxUpdate(5, 2, 91432, 'DOWN',          True, BtnMode1, False);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91570, 'SWITCH',        True, sm_blinkback , False);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91435, 'LEFT ROTATE',   True, BtnMode1, False);
    BoxUpdate(6, 4, 91436, 'FORWARD',       True, BtnMode, False);
    BoxUpdate(6, 5, 91437, 'RIGHT ROTATE',  True, BtnMode1, False);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_Pause;
begin
  AreaPanels.Unselect;
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
    BoxUpdate(6, 7, 91598, 'BACK',          True, sm_blinkback, False)
  end
end;

procedure TEliteContext.Context_StepSelect;
begin
  AreaPanels.Unselect;
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
    BoxUpdate(6, 7, 91200, '',              True, sm_blinkback, True)
  end
end;

procedure TEliteContext.Context_System_Map;
var
  ASt      : string;
  BtnMode  : TSelectMode;
begin
  BtnMode  := sm_blinkback;
  if IndexSMMode = 0 then begin
    BtnMode := sm_directnotnull;
    ASt     := 'NO STEP';
  end else
    ASt  := Format('STEP %d', [SubSMIndex]);

  with FOwner do begin
    ResetAreas;
    BoxUpdate(1, 1, 91462, 'SLIDE',         True, sm_blinkback, False);
    BoxUpdate(1, 2, 91451, ASt,             True, sm_blinkback, False);
    BoxUpdate(1, 3, 91460, 'ZOOM IN',       True, BtnMode, False);
    BoxUpdate(1, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 5, 91461, 'ZOOM OUT',      True, BtnMode, False);
    BoxUpdate(1, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(1, 7, 91453, 'BACK',          True, sm_blinkback, False);

    BoxUpdate(2, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(2, 4, 91456, 'GO UP',         True, BtnMode, False);
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
    BoxUpdate(4, 2, 91458, 'GO LEFT',       True, BtnMode, False);
    BoxUpdate(4, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(4, 6, 91459, 'GO RIGHT',      True, BtnMode, False);
    BoxUpdate(4, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(5, 1, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 4, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(5, 7, 91200, '',              True, sm_blinkback, True);

    BoxUpdate(6, 1, 91570, 'SWITCH',        True, sm_blinkback, False);
    BoxUpdate(6, 2, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 3, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 4, 91457, 'GO DOWN',       True, BtnMode, False);
    BoxUpdate(6, 5, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 6, 91200, '',              True, sm_blinkback, True);
    BoxUpdate(6, 7, 91599, 'PAUSE',         True, sm_blinkback, False)
  end
end;

constructor TEliteContext.Create(const AOwner: TAreaTobii);
begin
  inherited Create;
  FOwner                := AOwner;
  Step                  := 1;
  UnicStep              := False;
  IndexFunc             := 0;
  IndexMode             := 0;
  IndexGMMode           := 2;
  GMAllSlide            := False;
  SlideAssist           := False;
  CurrentDisplay        := kd_mainmenu;
  MapCalled             := kmc_none;
  ComPanelOpened        := False;
  PanelCalled           := kp_none;
  IsDSDOpened           := False;
  FSwitchUpdate         := False;
end;

procedure TEliteContext.DoACSAnalyse;
begin
  with FOwner, FEliteManager do ACSAnalyse(5000)
end;

procedure TEliteContext.DoACSBack;
begin
  with FOwner, FEliteManager do ACSClose;
  Drive_display
end;

procedure TEliteContext.DoACSGetTarget;
begin
  with FOwner, FEliteManager do ACSGetTarget
end;

procedure TEliteContext.DoACSHelp;
begin
  with FOwner, FEliteManager do ACSHelp
end;

procedure TEliteContext.DoACSMiniZoomIn;
begin
  with FOwner, FEliteManager do
    case SubACSIndex of
      1 : ACSZoomInMini(90);
      2 : ACSZoomInMini(180);
      3 : ACSZoomInMini(360);
    end
end;

procedure TEliteContext.DoACSMiniZoomOut;
begin
  with FOwner, FEliteManager do
    case SubACSIndex of
      1 : ACSZoomOutMini(90);
      2 : ACSZoomOutMini(180);
      3 : ACSZoomOutMini(360);
    end
end;

procedure TEliteContext.DoACSPitchDec;
begin
  with FOwner, FEliteManager do
    NavLauncher(ACSCameraPitchDec, ACSCameraPitchDec, IndexACSMode)
end;

procedure TEliteContext.DoACSPitchInc;
begin
  with FOwner, FEliteManager do
    NavLauncher(ACSCameraPitchInc, ACSCameraPitchInc, IndexACSMode)
end;

procedure TEliteContext.DoACSRadioDec;
begin
  with FOwner, FEliteManager do
    NavLauncher(ACSRadioDec, ACSRadioDec, IndexACSMode)
end;

procedure TEliteContext.DoACSRadioInc;
begin
  with FOwner, FEliteManager do
    NavLauncher(ACSRadioInc, ACSRadioInc, IndexACSMode)
end;

procedure TEliteContext.DoACSShow(DisplayPanel: boolean);
begin
  with FOwner, FEliteManager, FElite, FEliteStatus, FContext do
   if SuperCruise then begin
     if DisplayPanel then ModeACS;
     { --- WARNING ExplorationFSS must be opened }
     ACS_display;
     FunctionSound
  end
end;

procedure TEliteContext.DoACSSlide;
begin
  IndexACSMode := 0;
  ACS_display
end;

procedure TEliteContext.DoACSSteps;
begin
  if IndexACSMode = 0 then IndexACSMode := SubACSIndex
  else begin
    IndexACSMode := (IndexACSMode + 1) mod 4;
    if IndexACSMode = 0 then IndexACSMode := 1;
    SubACSIndex := IndexACSMode;
  end;
  FOwner.FContext.FunctionSound;
  ACS_display
end;

procedure TEliteContext.DoACSYawDec;
begin
  with FOwner, FEliteManager do
    NavLauncher(ACSCameraYawDec, ACSCameraYawDec, IndexACSMode)
end;

procedure TEliteContext.DoACSYawInc;
begin
  with FOwner, FEliteManager do
    NavLauncher(ACSCameraYawInc, ACSCameraYawInc, IndexACSMode)
end;

procedure TEliteContext.DoACSZoomIn;
begin
  with FOwner, FEliteManager do
    NavLauncher(ACSZoomIn, ACSZoomIn, IndexACSMode)
end;

procedure TEliteContext.DoACSZoomOut;
begin
  with FOwner, FEliteManager do
    NavLauncher(ACSZoomOut, ACSZoomOut, IndexACSMode)
end;

procedure TEliteContext.DoAutoBreak;
begin
  with FOwner, FEliteManager, FEliteStatus do if InSrv then VRSAutoBreak
end;

procedure TEliteContext.DoBackMenuShow;
begin
  with FOwner, FEliteManager do DoClicUnique(Pause);
  Menu_display
end;

function TEliteContext.IsDocked: Boolean;
begin
  { --- is docked }
  with FOwner, FEliteStatus do Result := Docked
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
  if Assigned(Method) then Method(Value)
end;

procedure TEliteContext.DoClic(const Method: TIntNotify; Value: Integer);
begin
  if Assigned(Method) then Method(Value)
end;

procedure TEliteContext.DoClicUnique(const Method: TCockpitNotify);
begin
  AreaPanels.Unselect;
  FOwner.FContext.FunctionSound;
  if Assigned(Method) then Method(cmt_none)
end;


procedure TEliteContext.DoComPanelShow(DisplayPanel: boolean);
begin
  with FOwner, FEliteManager do if DisplayPanel then DoClicUnique(CommsPanel);
  PanelCalled := kp_none;
  { --- Get display before FUNC }
  ComPanelOpened := True;
  Menu_display;
  with FOwner, FContext do if not DisplayPanel then FunctionSound
end;

procedure TEliteContext.DoDriveAssist;
begin
  with FOwner, FEliteManager do DriveAssist
end;

procedure TEliteContext.DoDriveLandingBackThrust(NotLanding: Boolean);
var
  index: Integer;
begin
  if NotLanding then index := 0 else index := IndexLandingDrive;
  with FOwner, FEliteManager do NavLauncherThrust(LandingBackwardThrust, LandingBackwardThrust, Index)
end;

procedure TEliteContext.DoDriveLandingForwardThrust(NotLanding: Boolean);
var
  index: Integer;
begin
  if NotLanding then index := 0 else index := IndexLandingDrive;
  with FOwner, FEliteManager do NavLauncherThrust(LandingForwardThrust, LandingForwardThrust, Index)
end;

procedure TEliteContext.DoDriveLandingLeftThrust(NotLanding: Boolean);
var
  index: Integer;
begin
  if NotLanding then index := 0 else index := IndexLandingDrive;
  with FOwner, FEliteManager do NavLauncherThrust(LandingLeftThrust, LandingLeftThrust, Index)
end;

procedure TEliteContext.DoDriveLandingPitch;
begin
  IndexOfPitchLanding := (IndexOfPitchLanding + 1) mod 3;
  FOwner.FContext.FunctionSound;
  Drive_display
end;

procedure TEliteContext.DoDriveLandingPitchUp;
begin
  with FOwner, FEliteManager do NavLauncher(nil, LandingPitchUp, IndexOfPitchLanding + 1)
end;

procedure TEliteContext.DoDriveLandingRightThrust(NotLanding: Boolean);
var
  index: Integer;
begin
  if NotLanding then index := 0 else index := IndexLandingDrive;
  with FOwner, FEliteManager do NavLauncherThrust(LandingRightThrust, LandingRightThrust, Index)
end;

procedure TEliteContext.DoDriveLandingRollLeft;
begin
  with FOwner, FEliteManager do NavLauncher(nil, LandingRollLeft, IndexOfPitchLanding + 1)
end;

procedure TEliteContext.DoDriveLandingRollRight;
begin
  with FOwner, FEliteManager do NavLauncher(nil, LandingRollRight, IndexOfPitchLanding + 1)
end;

procedure TEliteContext.DoDriveLandingSlide;
begin
  IndexLandingDrive := 0;
  FOwner.FContext.FunctionSound;
  Drive_display
end;

procedure TEliteContext.DoDriveLandingStep;
begin
  if IndexLandingDrive = 0 then IndexLandingDrive := SubLandingDriveIndex
  else begin
    IndexLandingDrive := (IndexLandingDrive + 1) mod 4;
    if IndexLandingDrive = 0 then IndexLandingDrive := 1;
    SubLandingDriveIndex := IndexLandingDrive
  end;
  FOwner.FContext.FunctionSound;
  Drive_display
end;

procedure TEliteContext.DoDriveLandingUpThrust(NotLanding: Boolean);
var
  index: Integer;
begin
  if NotLanding then index := 0 else index := IndexLandingDrive;
  with FOwner, FEliteManager do NavLauncherThrust(LandingUpThrust, LandingUpThrust, Index)
end;

procedure TEliteContext.DoDriveLandingYawLeft;
begin
  with FOwner, FEliteManager do NavLauncher(nil, LandingYawLeft, IndexOfPitchLanding + 1)
end;

procedure TEliteContext.DoDriveLandingYawRight;
begin
  with FOwner, FEliteManager do NavLauncher(nil, LandingYawRight, IndexOfPitchLanding + 1)
end;

procedure TEliteContext.DoDSDBack;
begin
  IsDSDOpened := False;
  with FOwner, FEliteManager do DSDClose;
  Drive_display
end;

procedure TEliteContext.DoDSDFire;
begin
  with FOwner, FEliteManager do PrimaryFire(90)
end;

procedure TEliteContext.DoDSDPitchDown;
begin
  with FOwner, FEliteManager do NavLauncher(DSDPitchDown, DSDPitchDown, IndexDSDMode)
end;

procedure TEliteContext.DoDSDPitchUp;
begin
  with FOwner, FEliteManager do NavLauncher(DSDPitchUp, DSDPitchUp, IndexDSDMode)
end;

procedure TEliteContext.DoDSDSlide;
begin
  IndexDSDMode := 0;
  DSD_display( IsDSDOpened );
  FOwner.FContext.FunctionSound;
end;

procedure TEliteContext.DoDSDSteps;
begin
  if IndexDSDMode = 0 then IndexDSDMode := SubDSDIndex
  else begin
    IndexDSDMode := (IndexDSDMode + 1) mod 4;
    if IndexDSDMode = 0 then IndexDSDMode := 1;
    SubDSDIndex := IndexDSDMode
  end;
  FOwner.FContext.FunctionSound;
  DSD_display( IsDSDOpened )
end;

procedure TEliteContext.DoDSDViewChange;
begin
  with FOwner, FEliteManager do DSDViewChange
end;

procedure TEliteContext.DoDSDYawLeft;
begin
  with FOwner, FEliteManager do NavLauncher(DSDYawLeft, DSDYawLeft, IndexDSDMode)
end;

procedure TEliteContext.DoDSDYawRight;
begin
  with FOwner, FEliteManager do NavLauncher(DSDYawRight, DSDYawRight, IndexDSDMode)
end;

procedure TEliteContext.DoDSDZoomIn;
begin
  with FOwner, FEliteManager do NavLauncher(DSDZoomIn, DSDZoomIn, IndexDSDMode)
end;

procedure TEliteContext.DoDSDZoomOut;
begin
  with FOwner, FEliteManager do NavLauncher(DSDZoomOut, DSDZoomOut, IndexDSDMode)
end;

procedure TEliteContext.DoFriendsMenuShow;
begin
  with FOwner, FEliteManager do DoClicUnique(FriendBoard);
  Menu_display
end;

procedure TEliteContext.DoFSD;
begin
  with FOwner, FEliteManager do FSD;
  Drive_display
end;

procedure TEliteContext.DoGMAllSlide;
begin
  GMAllSlide := not GMAllSlide;
  GalaxyMap_display
end;

procedure TEliteContext.DoGMBack;
begin
  with FOwner, FEliteStatus, FEliteManager do begin
    MapGalaxy;
    ComPanelOpened := False;
    MapCalled := kmc_none;
    if IsFlying then Drive_display else Menu_display
  end
end;

procedure TEliteContext.DoGMDown;
begin
  with FOwner, FEliteManager do
    NavLauncherEx(Sud, Sud, IndexGMMode)
end;

procedure TEliteContext.DoGMDownRotate;
begin
  with FOwner, FEliteManager do
    NavLauncher(CamPitchDown, CamPitchDown, IndexGMMode)
end;

procedure TEliteContext.DoGMForward;
begin
  with FOwner, FEliteManager do
    NavLauncherEx(Fils, Fils, IndexGMMode)
end;

procedure TEliteContext.DoGMGoHome;
begin
  with FOwner, FEliteManager do GalaxyMapHome
end;

procedure TEliteContext.DoGMLeft;
begin
  with FOwner, FEliteManager do
    NavLauncherEx(Ouest, Ouest, IndexGMMode)
end;

procedure TEliteContext.DoGMLeftRotate;
begin
  with FOwner, FEliteManager do
    NavLauncher(CamYawLeft, CamYawLeft, IndexGMMode)
end;

procedure TEliteContext.DoGMReward;
begin
  with FOwner, FEliteManager do
    NavLauncherEx(Pere, Pere, IndexGMMode)
end;

procedure TEliteContext.DoGMRight;
begin
  with FOwner, FEliteManager do
    NavLauncherEx(Est, Est, IndexGMMode)
end;

procedure TEliteContext.DoGMRightRotate;
begin
  with FOwner, FEliteManager do
    NavLauncher(CamYawRight, CamYawRight, IndexGMMode)
end;

procedure TEliteContext.DoGMSlide;
begin
  IndexGMMode := 0;
  GalaxyMap_display
end;

procedure TEliteContext.DoGMSteps;
begin
  if IndexGMMode = 0 then IndexGMMode := SubGMIndex
  else begin
    IndexGMMode := (IndexGMMode + 1) mod 4;
    if IndexGMMode = 0 then IndexGMMode := 1;
    SubGMIndex := IndexGMMode
  end;
  FOwner.FContext.FunctionSound;
  GalaxyMap_display
end;

procedure TEliteContext.DoGMUp;
begin
  with FOwner, FEliteManager do
    NavLauncherEx(Nord, Nord, IndexGMMode)
end;

procedure TEliteContext.DoGMUpRotate;
begin
  with FOwner, FEliteManager do
    NavLauncher(CamPitchUp, CamPitchUp, IndexGMMode)
end;

procedure TEliteContext.DoGMZoomIn;
begin
  with FOwner, FEliteManager do
    NavLauncher(CamZoomIn, CamZoomIn, IndexGMMode)
end;

procedure TEliteContext.DoGMZoomOut;
begin
  with FOwner, FEliteManager do
    NavLauncher(CamZoomOut, CamZoomOut, IndexGMMode)
end;

procedure TEliteContext.DoKeyBoardShow;
begin
  with FOwner, FEliteStatus do
    case TGuiType(GuiFOcus) of
      gt_galaxymap,
      gt_commspanel : begin
        DisplayBeforeKeyboard := CurrentDisplay;
        DisplayBefore         := CurrentDisplay;
        { --- Display KeyBoard }
        CurrentDisplay := kd_keyboard;
        with FOwner, FContext do UpdateZone( Context_Keyboard )
      end;
    end;
end;

procedure TEliteContext.DoDriveLandingDownThrust(NotLanding: Boolean);
var
  index: Integer;
begin
  if NotLanding then index := 0 else index := IndexLandingDrive;
  with FOwner, FEliteManager do NavLauncherThrust(LandingDownThrust, LandingDownThrust, Index)
end;

procedure TEliteContext.DoLandingPitchDown;
begin
  with FOwner, FEliteManager do NavLauncher(nil, LandingPitchDown, IndexOfPitchLanding + 1)
end;

procedure TEliteContext.DoLeftPanelShow(DisplayPanel: boolean);
begin
  with FOwner, FEliteManager do if DisplayPanel then DoClicUnique(LeftPanel);
  PanelCalled := kp_left;
  Menu_display;
  with FOwner, FContext do if not DisplayPanel then FunctionSound
end;

procedure TEliteContext.DoMapGalaxyShow(DisplayMap: boolean);
begin
  with FOwner, FEliteManager, FContext do begin
    MapCalled := kmc_galaxy;
    if DisplayMap then MapGalaxy;
    Menu_display;
    FunctionSound
  end
end;

procedure TEliteContext.DoMapSystemShow(DisplayMap: boolean);
begin
  with FOwner, FEliteManager, FContext do begin
    MapCalled := kmc_system;
    if DisplayMap then MapSystem;
    Menu_display;
    FunctionSound
  end
end;

procedure TEliteContext.DoMenuEchap;

  procedure Back_; begin
    with FOwner, FEliteManager do DoClicUnique(UIBack)
  end;

  function WithPanelChecked: Boolean; begin
    Result := PanelCalled in [kp_left, kp_right, kp_bottom];
    if Result then begin
      PanelCalled := kp_none;
      Back_;
      if IsFlying then Drive_display else Menu_display;
    end
  end;

  function WithMapChecked:Boolean; begin
    Result := MapCalled in [kmc_galaxy, kmc_system];
    if Result then begin
      ComPanelOpened := False;
      DoNavBack;
      MapCalled := kmc_none;
      with FOwner, FContext do FunctionSound
    end
  end;

  function WithComChecked: Boolean; begin
    Result := ComPanelOpened;
    if Result then begin
      ComPanelOpened := False;
      Back_;
      if IsFlying then Drive_display else Menu_display
    end
  end;

begin
  if not WithMapChecked then
    if not WithPanelChecked then
      if not WithComChecked then Back_
end; {DoMenuEchap}

procedure TEliteContext.DoMenuEnter;
begin
  with FOwner, FEliteManager, FContext do begin
    if IsDocked then SwitchUpdate := True;
    UISelect;
    FunctionSound
  end
end;

procedure TEliteContext.DoNavBack;

  function BackIfGalaxyMap:Boolean; begin
    Result := MapCalled = kmc_galaxy;
    if Result then begin
      with FOwner, FEliteManager do MapGalaxy;
      MapCalled := kmc_none;
      if IsFlying then Drive_display else Menu_display
    end
  end;

  function BackIfSystemMap:Boolean; begin
    Result := MapCalled = kmc_system;
    if Result then begin
      with FOwner, FEliteManager do MapSystem;
      MapCalled := kmc_none;
      if IsFlying then Drive_display else Menu_display
    end
  end;

  procedure Back_; begin
    with FOwner, FEliteManager do UIBack
  end;

  function BackPanel:Boolean; begin
    Result := PanelCalled in [kp_left, kp_right, kp_bottom];
    if Result then begin
      PanelCalled := kp_none;
      Back_;
      if IsFlying then Drive_display else Menu_display
    end
  end;

  function WithComChecked: Boolean; begin
    Result := ComPanelOpened;
    if Result then begin
      ComPanelOpened := False;
      Back_;
      if IsFlying then Drive_display else Menu_display
    end
  end;

  function BackIfFunction:Boolean; begin
    Result := CurrentDisplay = kd_function;
    if IsFlying then Drive_display else Menu_display
  end;

  function BackIfFloor:Boolean; begin
    Result := CurrentDisplay in [kd_drive, kd_menu];
    if Result then with FOwner, Felite do DoLaunchElite
  end;

begin
  if not BackIfGalaxyMap then
    if not BackIfSystemMap then
      if not BackPanel       then
        if not WithComChecked  then
          if not BackIfFunction  then
            if not BackIfFloor     then Menu_display
end; {DoNavBack}

procedure TEliteContext.DoNavEst;
begin
  with FOwner, FEliteManager do NavLauncher(Est, Est, IndexMode)
end;

procedure TEliteContext.DoNavFils;
begin
  with FOwner, FEliteManager do NavLauncher(Fils, Fils, IndexMode)
end;

procedure TEliteContext.DoNavModeChange;
begin
  if IndexMode = 0 then IndexMode := SubIndex
  else begin
    IndexMode := (IndexMode + 1) mod 4;
    if IndexMode = 0 then IndexMode := 1;
    SubIndex := IndexMode
  end;
  FOwner.FContext.FunctionSound;
  Drive_display
end;

procedure TEliteContext.DoNavNord;
begin
  with FOwner, FEliteManager do NavLauncher(Nord, Nord, IndexMode)
end;

procedure TEliteContext.DoNavNordEst;
begin
  with FOwner, FEliteManager do NavLauncher(NordEst, NordEst, IndexMode)
end;

procedure TEliteContext.DoNavNordOuest;
begin
  with FOwner, FEliteManager do NavLauncher(NordOuest, NordOuest, IndexMode)
end;

procedure TEliteContext.DoNavOuest;
begin
  with FOwner, FEliteManager do NavLauncher(Ouest, Ouest, IndexMode)
end;

procedure TEliteContext.DoNavPere;
begin
  with FOwner, FEliteManager do NavLauncher(Pere, Pere, IndexMode)
end;

procedure TEliteContext.DoNavSlide;
begin
  IndexMode := 0;
  FOwner.FContext.FunctionSound;
  Drive_display
end;

procedure TEliteContext.DoNavSud;
begin
  with FOwner, FEliteManager do NavLauncher(Sud, Sud, IndexMode)
end;

procedure TEliteContext.DoNavSudEst;
begin
  with FOwner, FEliteManager do NavLauncher(SudEst, SudEst, IndexMode)
end;

procedure TEliteContext.DoNavSudOuest;
begin
  with FOwner, FEliteManager do NavLauncher(SudOuest, SudOuest, IndexMode)
end;

procedure TEliteContext.DoNextShip;
begin
  with FOwner, FEliteManager do NextTarget
end;

procedure TEliteContext.DoOrderPanel;
begin
  with FOwner, FEliteManager, FEliteStatus do if TGuiType(GuiFocus) <> gt_rolepanel
   then OpenOrders
   else RadarPanel
end;

procedure TEliteContext.DoPrevShip;
begin
  with FOwner, FEliteManager do PriorTarget
end;

procedure TEliteContext.DoRightPanelShow(DisplayPanel: boolean);
begin
  with FOwner, FEliteManager do if DisplayPanel then DoClicUnique(RightPanel);
  PanelCalled := kp_right;
  Menu_display;
  with FOwner, FContext do  if not DisplayPanel then FunctionSound
end;

procedure TEliteContext.DoRolePanelShow(DisplayPanel: boolean);
begin
   with FOwner, FEliteManager do if DisplayPanel then DoClicUnique(RadarPanel);
   PanelCalled := kp_bottom;
   Menu_display;
   with FOwner, FContext do if not DisplayPanel then FunctionSound
end;

procedure TEliteContext.DoShipDismissRecall;
begin
  with FOwner, FEliteManager, FEliteStatus do if InSrv then ShipDismissRecall
end;

procedure TEliteContext.DoSMBack;
begin
  with FOwner, FEliteStatus, FEliteManager do begin
    MapSystem;
    MapCalled := kmc_none;
    if IsFlying then Drive_display else Menu_display
  end
end;

procedure TEliteContext.DoSMGoDown;
begin
  with FOwner, FEliteManager do NavLauncherEx(Fils, Fils, IndexSMMode)
end;

procedure TEliteContext.DoSMGoLeft;
begin
  with FOwner, FEliteManager do NavLauncherEx(Ouest, Ouest, IndexSMMode)
end;

procedure TEliteContext.DoSMGoRight;
begin
  with FOwner, FEliteManager do NavLauncherEx(Est, Est, IndexSMMode)
end;

procedure TEliteContext.DoSMGoUp;
begin
  with FOwner, FEliteManager do NavLauncherEx(Pere, Pere, IndexSMMode)
end;

procedure TEliteContext.DoSMSlide;
begin
  IndexSMMode := 0;
  SystemMap_display
end;

procedure TEliteContext.DoSMSteps;
begin
  if IndexSMMode = 0 then IndexSMMode := SubSMIndex
  else begin
    IndexSMMode := (IndexSMMode + 1) mod 4;
    if IndexSMMode = 0 then IndexSMMode := 1;
    SubSMIndex := IndexSMMode
  end;
  FOwner.FContext.FunctionSound;
  SystemMap_display
end;

procedure TEliteContext.DoSMZoomIn;
begin
  with FOwner, FEliteManager do NavLauncher(CamZoomIn, CamZoomIn, IndexSMMode)
end;

procedure TEliteContext.DoSMZoomOut;
begin
  with FOwner, FEliteManager do NavLauncher(CamZoomOut, CamZoomOut, IndexSMMode)
end;

procedure TEliteContext.DoStartFighter;
begin
  with FOwner, FEliteManager, FEliteStatus do
    case TGuiType(GuiFocus) of
      gt_rolepanel : RadarPanel;
      else begin
        RadarPanel;
        Menu_display
      end
    end
end;

procedure TEliteContext.DoSuperNavigation;
begin
  with FOwner, FEliteManager do SuperNavigation;
  Drive_display
end;

procedure TEliteContext.DoTarget12h;
begin
  with FOwner, FEliteManager do case FEliteStatus.InSrv of
    True : VRSTarget12h;
    else Target12h
  end
end;

procedure TEliteContext.DoUnicClic(const Method: TIntegerNotify;
  Value: Integer);
begin
  DoClic(Method, Value);
  if UnicStep then begin
    UnicStep := False;
    Step     := 1
  end
end;

procedure TEliteContext.DoUnicClic(const Method: TIntNotify; Value: Integer);
begin
  DoClic(Method, Value);
  if UnicStep then begin
    UnicStep := False;
    Step     := 1
  end
end;

procedure TEliteContext.DoUnicStep(Value: Integer);
begin
  UnicStep := True;
  Step     := Value
end;

procedure TEliteContext.DoVRSDownView;
begin
  with FOwner, FEliteManager, FEliteStatus do
    if SrvTurretView then NavLauncher(Sud, Sud, IndexVRSMode)
end;

procedure TEliteContext.DoVRSReversePropulsion;
begin
  with FOwner, FEliteManager, FEliteStatus do PropulsionReverse
//    if InSrv then PropulsionReverse
end;

procedure TEliteContext.DoVRSSlide;
begin
  IndexVRSMode := 0;
  FOwner.FContext.FunctionSound;
  Drive_display
end;

procedure TEliteContext.DoVRSStep;
begin
  if IndexVRSMode = 0 then IndexVRSMode := SubVRSIndex
  else begin
    IndexVRSMode := (IndexVRSMode + 1) mod 4;
    if IndexVRSMode = 0 then IndexVRSMode := 1;
    SubVRSIndex := IndexVRSMode
  end;
  FOwner.FContext.FunctionSound;
  Drive_display
end;

procedure TEliteContext.DoVRSTurnLeft;
begin
  with FOwner, FEliteManager, FEliteStatus do
    if SrvTurretView then NavLauncher(TurnLeft, TurnLeft, IndexVRSMode)
      else NavLauncher(Ouest, Ouest, IndexVRSMode)
end;

procedure TEliteContext.DoVRSTurnRight;
begin
  with FOwner, FEliteManager, FEliteStatus do
    if SrvTurretView then NavLauncher(TurnRight, TurnRight, IndexVRSMode)
      else NavLauncher(Est, Est, IndexVRSMode)
end;

procedure TEliteContext.DoVRSTurret;
begin
  with FOwner, FEliteManager, FEliteStatus do if InSrv then VRSTurret
end;

procedure TEliteContext.DoVRSUpView;
begin
  with FOwner, FEliteManager, FEliteStatus do
    if SrvTurretView then NavLauncher(Nord, Nord, IndexVRSMode)
end;

procedure TEliteContext.DoVRSVerticalThrust;
begin
  with FOwner, FEliteManager, FEliteStatus do
    if InSrv then VerticalThruster(1500)
    
end;

procedure TEliteContext.Drive_display;
begin
  CurrentDisplay := kd_drive;
  CurrentHud     := kh_drive;
  with FOwner, FEliteStatus do
    if InSrv then FOwner.UpdateZone( Context_EliteDriveVRS )
      else
    if HardpointDeployed then FOwner.UpdateZone( Context_EliteDriveCombat )
      else
    if LandinGearDown then FOwner.UpdateZone( Context_EliteDriveLanding )
      else
    FOwner.UpdateZone( Context_EliteDrive )
end;

procedure TEliteContext.DSD_display(forced: Boolean);

  procedure Process; begin
    IsDSDOpened    := True;
    CurrentDisplay := kd_saa;
    FOwner.UpdateZone( Context_DSD )
  end;

begin
  if forced then Process
    else
  if not IsDSDOpened then Process
end; {DSD_display}

function TEliteContext.EliteContextRetrieveOnLaunch(Sender: TKindHud):Boolean;

  procedure WithPanel(Method: TBoolNotify); begin
    Method(False)
  end;

begin
  Result := True;
  with FOwner, FEliteStatus do begin
    case GuiValue of
      gt_galaxymap      : WithPanel(DoMapGalaxyShow);
      gt_systemmap      : WithPanel(DoMapSystemShow);
      gt_stationservice : Menu_display;
      gt_internalpanel  : WithPanel(DoRightPanelShow );
      gt_externalpanel  : WithPanel(DoLeftPanelShow);
      gt_commspanel     : WithPanel(DoComPanelShow);
      gt_rolepanel      : WithPanel(DoRolePanelShow);
      gt_fssmode        : WithPanel(DoACSShow);
      gt_saamode        : { --- Enabled by Thread };
      else Result := False
    end
  end;
end; {EliteContextRetrieveOnLaunch}

procedure TEliteContext.EliteNotify(const ATag: Integer);
begin
  EliteForeGround;
  with FOwner, FEliteManager do
  try
    case ATag of
      { --- Areas for Elite 91200....91600 }
      91200 : {NULL};
      { --- Menu actions }
      91201 : DoUnicClic(Down, Step);
      91202 : DoUnicClic(Up, Step);
      91203 : DoUnicClic(Left, Step);
      91204 : DoUnicClic(Right, Step);
      91205 : DoUnicClic(PriorSheet, Step);
      91206 : DoUnicClic(NextSheet, Step);
      91207 : DoMenuEchap;
      91208 : DoMenuEnter;
      91209 : DoClicUnique(Step_display);
      { --- Steps selector }
      91210..91228 : StepSelector(ATag);
      { --- Drive actions drive & menu }
      91230 : DoNavNord;                        //Pitch up
      91231 : DoNavSud;                         //Pitch down
      91232 : DoNavOuest;                       //Yaw left
      91233 : DoNavEst;                         //Yaw right
      91234 : DoNavPere;                        //Roll left
      91235 : DoNavFils;                        //Roll right
      91236 : PrimaryFire(300);                 //Primary fire
      91237 : SecondaryFire(300);               //Secondary fire
      91238 : DoClic(DoNavModeChange);          //Mode
      91239 : DoClic(Func_display);             //Display functions
      91282 : DoClic(Deceleration);             //Less speed
      91283 : DoClic(Acceleration);             //More speed
      91285 : DoClicUnique(SpeedNull);          //0%
      91284 : DoClicUnique(Speed25);            //25%
      91286 : DoClicUnique(Speed50);            //50%
      91288 : DoClicUnique(Speed75);            //75%
      91287 : DoClicUnique(Speed100);           //100%
      91289 : DoClicUnique(InverserPropulsion); //Inv speed
      91290 : DoClicUnique(Boost);              //Boost
      91291 : AssistanceDeVol(ft_none);         //Flight assist
      91293 : DoClicUnique(DoSuperNavigation);  //Supercruise
      91294 : DoClicUnique(DoFSD);              //FSD
      91295 : DoPrevShip;                       //Previous ship
      91296 : DoNextShip;                       //Next ship
      91297 : ; //NULL

      { --- Functions actions }
      91240 : AssignFunc(0);
      91241 : DoComPanelShow(True);             //Top Panel
      91242 : DoLeftPanelShow(True);            //Left Panel
      91243 : DoRightPanelShow(True);           //Right Panel
      91244 : DoRolePanelShow(True);            //Bottom Panel
      91245 : ; //null
      91246 : DoFriendsMenuShow;                //Friends menu
      91247 : DoBackMenuShow;                   //Back to menu
      91248 : DoMapSystemShow(True);            //System map
      91249 : DoMapGalaxyShow(True);            //Galaxy map

      91250 : AssignFunc(1);
      91251 : DoClicUnique(LandingGear);        //Landing gear
      91252 : DoClicUnique(CargoScoop);         //Cargo scoop
      91253 : DoClicUnique(NightVision);        //Night vision
      91254 : DoClicUnique(Searchlight);        //Spotlight
      91255 : DoClicUnique(CockpitMode);        //Cockpit mode
      91256 : PipeMoteur;                       //Pipe engines
      91257 : PipeSystem;                       //Pipe systems
      91258 : PipeArmes;                        //Pipe weapons
      91259 : DoClicUnique(PipeReset);          //Default
      91277 : DoACSShow(True);                  //ACS
      91278 : DoClicUnique(ChaffLauncher);      //Chaff launcher
      91279 : DoClicUnique(ShieldCell);         //Shield cell
      91280 : DoClicUnique(HeatSink);           //Heat sink
      91281 : DoClicUnique(ChargeECM);          //Charge ECM

      91260 : AssignFunc(2);
      91261 : DoClicUnique(HardPoint);          //Hard point
      91262 : DoClic(PriorArmGroup);            //Fire group previous
      91263 : DoClic(NextArmGroup);             //Fire group next
      91264 : DoClic(PriorSubSystem);           //Previous subsystem
      91265 : DoClic(NextSubSystem);            //Next subsystem
      91266 : DoClic(MenacePrecedente);         //Previous hostile
      91267 : DoClic(MenaceSuivante);           //Next hostile
      91268 : DoClic(MenacePrincipale);         //Highest Threat
      91269 : DoClicUnique(DoTarget12h);        //12h Target
      91270 : CibleOfAilier(1);                 //Target wingman 1
      91271 : CibleOfAilier(2);                 //Target wingman 2
      91272 : CibleOfAilier(3);                 //Target wingman 3
      91273 : DoClicUnique(FullSystem);         //Full systems
      91274 : DoClicUnique(FullMoteur);         //Full engines
      91275 : DoClicUnique(FullArme);           //Full weapons
      91276 : DoClicUnique(NextSubSystem);      //Next route system

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
      91351 : DoClicUnique(Camera_ShipShow);      //OPEN 91/92
      91352 : DoClic(Camera_Prior);               //Prev camera
      91353 : DoClic(Camera_Next);                //Next camera
      91355 : DoClicUnique(Camera_Two);           //Cam 2
      91356 : DoClicUnique(Camera_Three);         //Cam 3
      91357 : DoClicUnique(Camera_Four);          //Cam 4
      91358 : DoClicUnique(Camera_Five);          //Cam 5
      91359 : DoClicUnique(Camera_Six);           //Cam 6
      91360 : DoClicUnique(Camera_Seven);         //Cam 7
      91361 : DoClicUnique(Camera_Eight);         //Cam 8
      91362 : DoClicUnique(Camera_Nine);          //Cam 9
      91363 : DoClicUnique(FreeCamera_Close);     //ATH

      91400 : AssignFunc(5);
      91401 : DoClicUnique(DoAutoBreak);          //Auto break
      91402 : DoClicUnique(DoVRSTurret);          //Turret mode
      91403 : DoClicUnique(DoShipDismissRecall);  //Ship call/dismiss
      91404 : DoClicUnique(DoVRSSlide);           //VRS Slide
      91405 : DoClicUnique(DoVRSStep);            //VRS Step
      91406 : DoVRSUpView;                        //VRS Turret up view
      91407 : DoVRSDownView;                      //VRS Turret down view
      91408 : DoVRSTurnLeft;                      //VRS Turn left
      91409 : DoVRSTurnRight;                     //VRS Turn right
      91410 : DoVRSVerticalThrust;                //VRS Vertical thrust
      91411 : DoVRSReversePropulsion;             //VRS Reverse propulsion
      91412 : DoClicUnique(DoDriveAssist);        //VRS Drive Assist

      { --- Galaxy map }
      91420 : GalaxyMap_display;                  //Display context galaxy map
      91421 : DoClicUnique(DoGMBack);             //Back
      91422 : DoClicUnique(DoGMSlide);            //Slide
      91423 : DoClic(DoGMSteps);                  //Steps
      91424 : ;//NULL
      91425 : DoClicUnique(DoGMGoHome);           //Go home
      91426 : DoClicUnique(DoGMAllSlide);         //All slide
      91427 : DoClic(DoGMReward);                 //Reward
      91428 : DoClic(DoGMUp);                     //Up
      91429 : DoClic(DoGMUpRotate);               //Up rotate
      91430 : DoClic(DoGMLeft);                   //Left
      91431 : DoClic(DoGMRight);                  //Right
      91432 : DoClic(DoGMDown);                   //Down
      91433 : DoClic(DoGMDownRotate);             //Down rotate
      91434 : DoClic(DoGMZoomIn);                 //Zoom in
      91435 : DoClic(DoGMLeftRotate);             //Left rotate
      91436 : DoClic(DoGMForward);                //Forward
      91437 : DoClic(DoGMRightRotate);            //Right rotate
      91438 : DoClic(DoGMZoomOut);                //Zoom out

      { --- System map }
      91450 : SystemMap_display;                  //Display context system map
      91451 : DoClic(DoSMSteps);                  //Steps
      91452 : ; //NULL
      91453 : DoClicUnique(DoSMBack);             //Back
      91454 : ; //NULL
      91455 : ; //NULL
      91456 : DoClic(DoSMGoUp);                   //GO UP
      91457 : DoClic(DoSMGoDown);                 //GO DOWN
      91458 : DoClic(DoSMGoLeft);                 //GO LEFT
      91459 : DoClic(DoSMGoRight);                //GO RIGHT
      91460 : DoClic(DoSMZoomIn);                 //ZOOM IN
      91461 : DoClic(DoSMZoomOut);                //ZOOM OUT
      91462 : DoClic(DoSMSlide);                  //Slide

      { --- Exploration FSS }
      91470 : ACS_display;
      91471 : DoClic(DoACSSlide);
      91472 : DoClic(DoACSSteps);
      91473 : DoClic(DoACSZoomIn);
      91474 : DoClicUnique(DoACSAnalyse);
      91475 : DoClic(DoACSZoomOut);
      91476 : DoClicUnique(DoACSHelp);
      91477 : DoClicUnique(DoACSBack);
      91478 : DoClic(DoACSRadioDec);
      91479 : DoClic(DoACSPitchDec);
      91480 : DoClic(DoACSRadioInc);
      91481 : DoClic(DoACSYawDec);
      91482 : DoClic(DoACSYawInc);
      91483 : DoClicUnique(DoACSGetTarget);
      91484 : DoClic(DoACSMiniZoomIn);
      91485 : DoClic(DoACSPitchInc);
      91486 : DoClic(DoACSMiniZoomOut);

      { --- Surface Analysis Area }
      91490 : DSD_display( IsDSDOpened );
      91491 : DoClic(DoDSDSlide);
      91492 : DoClic(DoDSDSteps);
      91493 : DoClic(DoDSDZoomIn);
      91494 : DoClic(DoDSDZoomOut);
      91495 : DoClicUnique(DoDSDBack);
      91496 : DoClic(DoDSDPitchUp);
      91497 : DoClic(DoDSDYawLeft);
      91498 : DoClic(DoDSDYawRight);
      91499 : DoClicUnique(DoDSDViewChange);
      91500 : DoClic(DoDSDPitchDown);
      91501 : DoClicUnique(DoDSDFire);

      { --- Diagonal way }
      91511 : DoNavNordOuest;
      91512 : DoNavNordEst;
      91513 : DoNavSudOuest;
      91514 : DoNavSudEst;

      { --- Drive landing or docking }
      91521 : ; //NULL
      91522 : DoDriveLandingSlide;
      91523 : DoDriveLandingStep;
      91524 : DoDriveLandingPitch;
      91525 : DoDriveLandingPitchUp;
      91526 : DoDriveLandingRollLeft;
      91527 : DoDriveLandingBackThrust;
      91528 : DoDriveLandingUpThrust;
      91529 : DoDriveLandingForwardThrust;
      91530 : DoDriveLandingRollRight;
      91531 : DoDriveLandingYawLeft;
      91532 : DoDriveLandingLeftThrust;
      91533 : DoDriveLandingRightThrust;
      91534 : DoDriveLandingYawRight;
      91535 : DoDriveLandingDownThrust;
      91536 : DoLandingPitchDown;

      { --- FUNC DIV : Free Thrusting }
      91537 : DoDriveLandingBackThrust(True);
      91538 : DoDriveLandingUpThrust(True);
      91539 : DoDriveLandingForwardThrust(True);
      91542 : DoDriveLandingLeftThrust(True);
      91543 : DoDriveLandingRightThrust(True);
      91545 : DoDriveLandingDownThrust(True);

      { --- Sub contexts }
      91550 : DoNavBack;
      91560 : DoClicUnique(DoKeyBoardShow);
      91570 : DoClicUnique(Switch);
      91571 : DoClic(PauseFunctionBack);
      91580 : Menu_display;
      91581 : DoClicUnique( TryLaunchOnMenuDisplay );
      91590 : Drive_display;
      91591 : DoClicUnique( TryLaunchOnDriveDisplay );
      91596 : DoClic(DoNavSlide);
      91597 : with FOwner, FContext do DoClicUnique( MainMenu_display );
      91598 : PauseBack;
      91599 : PauseEnable;
    end
  except
  end
end;

procedure TEliteContext.Func_display;
begin
  if CurrentDisplay <> kd_function then DisplayBefore := CurrentDisplay;
  CurrentDisplay := kd_function;
  with FOwner do begin
    UpdateZone( Context_Func );
    case IndexFunc of
      0 : UpdateZone( Context_Func0 ); //Display panels area
      1 : UpdateZone( Context_Func1 ); //Display div area
      2 : UpdateZone( Context_Func2 ); //Display weapons area
      3 : UpdateZone( Context_Func3 ); //Display fighter area
      4 : UpdateZone( Context_Func4 ); //Display camera area
    end
  end
end;

procedure TEliteContext.GalaxyMap_display;
begin
  CurrentDisplay := kd_galaxymap;
  FOwner.UpdateZone( Context_GalaxyMap )
end;

function TEliteContext.GetComPanelOpened: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'ComPanelOpened')
end;

function TEliteContext.GetCurrentDisplay: TKindDisplay;
begin
  Result := TKindDisplay( KeyReadInt(ParamKey, 'CurrentDisplay', 0))
end;

function TEliteContext.GetCurrentHud: TKindHud;
begin
  Result := TKindHud( KeyReadInt(ParamKey, 'CurrentHud') )
end;

function TEliteContext.GetDisplayBeforeKeyboard: TKindDisplay;
begin
  Result := TKindDisplay( KeyReadInt(ParamKey, 'DisplayBeforeKeyboard', 0) )
end;

function TEliteContext.GetGMAllSlide: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'GMAllSlide')
end;

function TEliteContext.GetIndexACSMode: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexACSMode', 0)
end;

function TEliteContext.GetIndexDSDMode: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexDSDMode', 0)
end;

function TEliteContext.GetIndexFunc: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexFunc', 0)
end;

function TEliteContext.GetIndexGMMode: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexGMMode', 0)
end;

function TEliteContext.GetIndexLandingDrive: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexLandingDrive')
end;

function TEliteContext.GetIndexMode: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexMode')
end;

function TEliteContext.GetIndexOfPitchLanding: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexOfPitchLanding')
end;

function TEliteContext.GetIndexSMMode: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexSMMode')
end;

function TEliteContext.GetIndexVRSMode: Integer;
begin
  Result := KeyReadInt(ParamKey, 'IndexVRSMode')
end;

function TEliteContext.GetIsDSDOpened: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'IsDSDOpened')
end;

function TEliteContext.GetMapCalled: TKindMapCall;
begin
  Result := TKindMapCall( KeyReadInt(ParamKey, 'MapCalled') )
end;

function TEliteContext.GetPanelCalled: TKindPanels;
begin
  Result := TKindPanels( KeyReadInt(ParamKey, 'PanelCalled') )
end;

function TEliteContext.GetSlideAssist: Boolean;
begin
  Result := KeyReadBoolean(IniKey, 'SlideAssist')
end;

function TEliteContext.GetSlideOffms: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SlideOffms', 2)
end;

function TEliteContext.GetStateBeforePause: TKindDisplay;
begin
  Result := TKindDisplay( KeyReadInt(ParamKey, 'StateBeforePause', 0))
end;

function TEliteContext.GetStep: Integer;
begin
  Result := KeyReadInt(ParamKey, 'Step', 1)
end;

function TEliteContext.GetSubACSIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubACSIndex', 2)
end;

function TEliteContext.GetSubDSDIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubDSDIndex', 2)
end;

function TEliteContext.GetSubGMIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubGMIndex', 2)
end;

function TEliteContext.GetSubIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubIndex', 2)
end;

function TEliteContext.GetSubLandingDriveIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubLandingDriveIndex', 2)
end;

function TEliteContext.GetSubSMIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubSMIndex', 2)
end;

function TEliteContext.GetSubVRSIndex: Integer;
begin
  Result := KeyReadInt(ParamKey, 'SubVRSIndex', 2)
end;

function TEliteContext.GetUnicStep: Boolean;
begin
  Result := KeyReadBoolean(ParamKey, 'UnicStep')
end;

function TEliteContext.IsFlying: Boolean;
begin
  with FOwner, FEliteStatus do begin
    { --- Not docked and not landed }
    Result := not IsDocked and not IsLanded;
  end
end;

function TEliteContext.IsLanded: Boolean;
begin
  { --- is Landed }
  with FOwner, FEliteStatus do Result := Landed
end;

function TEliteContext.IsMapOpened: Boolean;
begin
  with FOwner, FEliteStatus do
    case TGuiType(GuiFocus) of
      gt_galaxymap,
      gt_systemmap : Result := True;
      else Result := false
    end
end;

procedure TEliteContext.Menu_display;
begin
  Step           := 1;
  CurrentDisplay := kd_menu;
  CurrentHud     := kh_menu;
  FOwner.UpdateZone( Context_EliteMenu )
end;

procedure TEliteContext.NavClic(const SensA: TContextNotify;
  const SensB: TIntANotify);
begin
  with FOwner, FEliteManager do begin
    if SlideAssist then begin
      if Assigned(SensB) then SensB(120);
      Delay(SlideOffms)
    end;
    if Assigned(SensA) then SensA
  end
end;

procedure TEliteContext.NavLauncher(const MethA: TContextNotify;
  const MethB: TIntANotify; Index: Integer);
begin
  with FOwner, FEliteManager do
    case Index of
      0 : NavClic(MethA, MethB);
      1 : MethB(90);
      2 : MethB(180);
      3 : MethB(360);
    end
end;

procedure TEliteContext.NavLauncherEx(const MethA: TContextNotify;
  const MethB: TIntegerNotify; Index: Integer);
begin
  with FOwner, FEliteManager do
    case Index of
      0 : NavClic(MethA, MethB);
      1 : MethB(1, 90);
      2 : MethB(1, 180);
      3 : MethB(1, 360);
    end
end;

procedure TEliteContext.NavLauncherThrust(const MethA: TContextNotify;
  const MethB: TIntANotify; Index: Integer);
begin
  with FOwner, FEliteManager do
    case Index of
      0 : NavClic(MethA, MethB);
      1 : MethB(360);
      2 : MethB(720);
      3 : MethB(1440);
    end
end;

procedure TEliteContext.NavLauncher(const MethA: TContextNotify;
  const MethB: TIntegerNotify; Index: Integer);
begin
  with FOwner, FEliteManager do
    case Index of
      0 : NavClic(MethA, MethB);
      1 : MethB(1, 120);
      2 : MethB(1, 210);
      3 : MethB(1, 300);
    end
end;

procedure TEliteContext.NavClic(const SensA: TContextNotify;
  const SensB: TIntegerNotify);
begin
  with FOwner, FEliteManager do begin
    if SlideAssist then begin
      if Assigned(SensB) then SensB(1, 120);
      Delay(SlideOffms)
    end;
    if Assigned(SensA) then SensA
  end
end;

procedure TEliteContext.PauseBack;
{ --- For Elite }
  procedure DriveCase; begin
    case MapCalled of
      kmc_none   : Drive_display;
      kmc_galaxy : GalaxyMap_display;
      kmc_system : SystemMap_display;
    end
  end;

begin
  with FOwner, Context do case DisplayBefore of
    kd_mainmenu  : MainMenu_display;
    kd_menu      : Menu_display;
    kd_drive     : DriveCase;
    kd_fss       : ACS_display;
    kd_saa       : DSD_display( IsDSDOpened );
    kd_galaxymap : GalaxyMap_display;
    kd_systemmap : SystemMap_display;
  end
end; {PauseBack;}

procedure TEliteContext.PauseEnable;
begin
  Pause_display
end;

procedure TEliteContext.PauseFunctionBack;
{ --- Not for Elite }
begin
  if IsMapOpened then Menu_display
  else case DisplayBefore of
         kd_drive : Drive_display;
         kd_menu  : Menu_display;
       end
end;

procedure TEliteContext.Pause_display;
begin
  DisplayBefore  := CurrentDisplay;
  CurrentDisplay := kd_pause;
  FOwner.UpdateZone( Context_Pause )
end;

procedure TEliteContext.SetComPanelOpened(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'ComPanelOpened', Value)
end;

procedure TEliteContext.SetCurrentDisplay(const Value: TKindDisplay);
begin
  KeyWrite(ParamKey, 'CurrentDisplay', Integer(Value))
end;

procedure TEliteContext.SetCurrentHud(const Value: TKindHud);
begin
  KeyWrite(ParamKey, 'CurrentHud', Integer(Value) )
end;

procedure TEliteContext.SetDisplayBeforeKeyboard(const Value: TKindDisplay);
begin
  KeyWrite(ParamKey, 'DisplayBeforeKeyboard', Integer(Value) )
end;

procedure TEliteContext.SetGMAllSlide(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'GMAllSlide', Value)
end;

procedure TEliteContext.SetIndexACSMode(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexACSMode', Value)
end;

procedure TEliteContext.SetIndexDSDMode(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexDSDMode', Value)
end;

procedure TEliteContext.SetIndexFunc(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexFunc', Value)
end;

procedure TEliteContext.SetIndexGMMode(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexGMMode', Value)
end;

procedure TEliteContext.SetIndexLandingDrive(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexLandingDrive', Value)
end;

procedure TEliteContext.SetIndexMode(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexMode', Value)
end;

procedure TEliteContext.SetIndexOfPitchLanding(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexOfPitchLanding', Value)
end;

procedure TEliteContext.SetIndexSMMode(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexSMMode', Value)
end;

procedure TEliteContext.SetIndexVRSMode(const Value: Integer);
begin
  KeyWrite(ParamKey, 'IndexVRSMode', Value)
end;

procedure TEliteContext.SetIsDSDOpened(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'IsDSDOpened', Value)
end;

procedure TEliteContext.SetMapCalled(const Value: TKindMapCall);
begin
  KeyWrite(ParamKey, 'MapCalled', Integer(Value) )
end;

procedure TEliteContext.SetPanelCalled(const Value: TKindPanels);
begin
  KeyWrite(ParamKey, 'PanelCalled', Integer(Value))
end;

procedure TEliteContext.SetSlideAssist(const Value: Boolean);
begin
  KeyWrite(IniKey, 'SlideAssist', Value)
end;

procedure TEliteContext.SetSlideOffms(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SlideOffms', Value)
end;

procedure TEliteContext.SetStateBeforePause(const Value: TKindDisplay);
begin
  KeyWrite(ParamKey, 'StateBeforePause', Integer(Value))
end;

procedure TEliteContext.SetStep(const Value: Integer);
begin
  KeyWrite(ParamKey, 'Step', Value)
end;

procedure TEliteContext.SetSubACSIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubACSIndex', Value)
end;

procedure TEliteContext.SetSubDSDIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubDSDIndex', Value)
end;

procedure TEliteContext.SetSubGMIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubGMIndex', Value)
end;

procedure TEliteContext.SetSubIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubIndex', Value)
end;

procedure TEliteContext.SetSubLandingDriveIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubLandingDriveIndex', Value)
end;

procedure TEliteContext.SetSubSMIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubSMIndex', Value)
end;

procedure TEliteContext.SetSubVRSIndex(const Value: Integer);
begin
  KeyWrite(ParamKey, 'SubVRSIndex', Value)
end;

procedure TEliteContext.SetUnicStep(const Value: Boolean);
begin
  KeyWrite(ParamKey, 'UnicStep', Value)
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
    91226        : DoUnicStep(3);
    91227        : DoUnicStep(5);
    91228        : DoUnicStep(10);
  end;
  with FOwner do begin
    UpdateZone( Context_EliteMenu );
    FContext.FunctionSound
  end
end;

procedure TEliteContext.Step_display;
begin
  { --- Don't reset Step after use }
  UnicStep := False;
  FOwner.UpdateZone( Context_StepSelect )
end;

procedure TEliteContext.Switch;
begin
  if MapCalled <> kmc_none then begin
      case MapCalled of
        kmc_galaxy : if CurrentDisplay = kd_galaxymap then Menu_display else GalaxyMap_display;
        kmc_system : if CurrentDisplay = kd_systemmap then Menu_display else SystemMap_display;
      end
  end else begin
    case CurrentDisplay of
      kd_menu : drive_display;
      else Menu_display
    end
  end
end;

procedure TEliteContext.SystemMap_display;
begin
  CurrentDisplay := kd_systemmap;
  FOwner.UpdateZone( Context_System_Map )
end;

procedure TEliteContext.TryLaunchOnDriveDisplay;
begin
  if not EliteContextRetrieveOnLaunch(kh_drive) then Drive_display
end;

procedure TEliteContext.TryLaunchOnMenuDisplay;
begin
  if not EliteContextRetrieveOnLaunch(kh_menu) then Menu_display
end;

{ TContextObserver }

constructor TContextObserver.Create(const AAreaTobii: TAreaTobii);
begin
  inherited Create( False );
  {Automatic started}
  ThAreaTobii     := AAreaTobii;
  Old_Docked      := True;
  Old_HardPoints  := False;
  Old_InSRV       := False;
  FreeOnTerminate := True;
  Priority        := tpLower
end;

procedure TContextObserver.Execute;
begin
  { --- Breaked when app terminate and automatic freed instance }
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    { --- We suppose Status.json updated each second by Frontier }
    ThDelay( 1024 )
  end
end;

procedure TContextObserver.Process;

  procedure UpdateDSDView; begin
    with ThAreaTobii, FEliteStatus, FElite do
      if GuiValue = gt_saamode then begin
        if not IsDSDOpened then DSD_display( False )
      end else
        if IsDSDOpened then DoDSDBack
  end;

  procedure UpdatePanelsView; begin
    with ThAreaTobii, FEliteStatus, FElite do
      case GuiValue of
        gt_externalpanel : PanelCalled := kp_left;
        gt_internalpanel : PanelCalled := kp_right;
        gt_rolepanel     : PanelCalled := kp_bottom;
        gt_commspanel    : ComPanelOpened := True;
      end
  end;

  procedure UpdateWhenUndock; begin
    with ThAreaTobii, FEliteStatus, FElite do
      if (IsDocked <> Old_Docked) and SwitchUpdate then begin
        if IsDocked then Menu_display else Drive_display;
        SwitchUpdate := False
      end
  end;

  function UpdateDriveInSrv:Boolean; begin
    Result := False;
    with ThAreaTobii, FEliteStatus, FElite do
      if InSrv <> Old_InSRV then begin
        Old_InSRV := InSrv;
        Result    := True;
        Drive_display
      end
  end;

  procedure UpdateDriveDisplayForHardPoint; begin
    with ThAreaTobii, FEliteStatus, FElite do
      if (HardpointDeployed <> Old_HardPoints) and IsFlying then Drive_display
  end;

  procedure UpdateDriveDisplayWhenLanding; begin
    with ThAreaTobii, FEliteStatus, FElite do
      if (LandinGearDown <> Old_LandingGear) and not HardpointDeployed and
       not IsDocked then Drive_display
  end;

  procedure SaveOnIteration; begin
    with ThAreaTobii, FEliteStatus, FElite do begin
      Old_Docked      := IsDocked;
      Old_HardPoints  := HardpointDeployed;
      Old_LandingGear := LandinGearDown;
    end
  end;

begin
  { --- Update DSD view }
  UpdateDSDView;
  { --- Update Panels view if app. restart }
  UpdatePanelsView;
  { --- Update menu when undock aftter ENTER clic }
  UpdateWhenUndock;
  { --- Update Drive display in SRV }
  if not UpdateDriveInSrv then begin
    { --- Update Drive display when Hardpoint deployed and on flying }
    UpdateDriveDisplayForHardPoint;
    { --- Update Drive display when Landing gear down and on flying
          and Hardpoint not deployed }
    UpdateDriveDisplayWhenLanding
  end;
  { --- Save current values for next iteration process }
  SaveOnIteration
end; {Process}

procedure TContextObserver.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  { --- Automatic free when app terminate }
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 )
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

initialization
  KeyMessageSender := TKeyMessageSender.Create
finalization
  KeyMessageSender.Free
end.


