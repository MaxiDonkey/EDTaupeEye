{*******************************************************}
{                                                       }
{             08/2020  MaxiDonkey  Library              }
{                                                       }
{*******************************************************}

unit uEliteManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StrUtils,
  {Spec}
  StrCopyUtils, uRegistry, KeysDef,
  {Elite bindings}
  EliteBindingsTools, uStatusReader;

type
  TEliteRunningObserver = class(TThread)
  private
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create;
  end;

  TCockPitModeType = (cmt_none, cmt_combat, cmt_exploration);
  TFADOType        = (ft_none, ft_on, ft_off);
  TLandingGearType = (lgt_none, lgt_open, lgt_close);

  TCustomEliteManager = class
  private
    FTags         : string;
    FTagStack     : TStringList;
    FKeyInventory : TKeyInventory;
    LastCmd       : Integer;
    function  GetMaxRepeat: Integer;
    procedure SetMaxRepeat(const Value: Integer);
  private
    { --- Mutex }
    EliteMutex : Cardinal;
    procedure InitializeMutex;
    procedure FinalizeMutex;
    procedure ProtectedCode(Method: TNotifyEvent);
    procedure SendChar(const Car: Char); overload;
    procedure SendChar(VKCode: SmallInt); overload;
  private
    { --- Gestion des tags }
    procedure DoWithTag(Sender: TObject);
    procedure DoOnStack(Sender: TObject);
    procedure AssignTagsToStack;
    procedure CallCommande(const Ast: string; Again: Boolean = False);
    procedure IterateCallCommande(Count: Integer);
    procedure ProcessOnStack;
  public
    { --- Divers local et API }
    function  IsAgainCommand(const Cmd: Integer):Boolean;
    function  CanLongRepeat:Boolean;
    procedure PipeReset;
    procedure PipeSystem;
    procedure PipeMoteur;
    procedure PipeArmes;
    procedure PousseeHaut(const sTime: Cardinal);
    procedure TrainAtterrissage;
    procedure MainZoom;
    procedure MainDezoom;

    { --- Macros }
    procedure CibleOfAilier(index: Byte);
    procedure NavigationOnAilier(index: Byte);
    procedure FSD;
    procedure SuperNavigation;
    procedure PRL;
    procedure FullSystem;
    procedure FullMoteur;
    procedure FullArme;
    procedure CombatOffensif;
    procedure CombatDefensif;
    procedure ModeFuite;
    procedure ModeDefensif;
    {CMD : Vol rotation}
    procedure Nord(index: Byte; tms: Integer = 0); overload;
    procedure Nord; overload;
    procedure Sud(index: Byte; tms: Integer = 0); overload;
    procedure Sud; overload;
    procedure Est(index: Byte; tms: Integer = 0); overload;
    procedure Est; overload;
    procedure Ouest(index: Byte; tms: Integer = 0); overload;
    procedure Ouest; overload;
    procedure Pere(index: Byte; tms: Integer = 0); overload;
    procedure Pere; overload;
    procedure Fils(index: Byte; tms: Integer = 0); overload;
    procedure Fils; overload;
    {CMD : Vol poussée}
    procedure PousseGauche(index: Byte; tms: Integer = 0);
    procedure PousseDroite(index: Byte; tms: Integer = 0);
    procedure PousseHaut(index: Byte; tms: Integer = 0);
    procedure PousseBas(index: Byte; tms: Integer = 0);
    procedure PousseAvant(index: Byte; tms: Integer = 0);
    procedure PousseArriere(index: Byte; tms: Integer = 0);
    procedure Decollage;
    {CMD : Vol propulsion}
    procedure InverserPropulsion;
    procedure Acceleration;
    procedure Deceleration;
    procedure SpeedM100;
    procedure SpeedM75;
    procedure SpeedM50;
    procedure SpeedM25;
    procedure SpeedNull;
    procedure Speed25;
    procedure Speed50;
    procedure Speed75;
    procedure Speed100;
    {CMD : Vol divers}
    procedure AssistanceDeVol(const FAO: TFADOType);
    procedure Boost;
    procedure FixRotation;
    procedure OrbitDisplay;
    {CMD : Visée}
    procedure Target12h;
    procedure NextTarget;
    procedure PriorTarget;
    procedure MenacePrincipale;
    procedure MenaceSuivante;
    procedure MenacePrecedente;
    procedure Ailier1;
    procedure Ailier2;
    procedure Ailier3;
    procedure AilierTarget;
    procedure AilierNavLock;
    procedure NextSubSystem;
    procedure PriorSubSystem;
    procedure NextRoute;
    {CMD : Armes}
    procedure PrimaryFire(tms: Integer = 0);
    procedure SecondaryFire(tms: Integer = 0);
    procedure NextArmGroup;
    procedure PriorArmGroup;
    procedure HardPoint;
    procedure StealthyMode;
    procedure HeatSink;
    {CMD : Divers}
    procedure LandingGear(const Mode: TLandingGearType = lgt_none);
    procedure CargoEject;
    procedure CargoScoop;
    procedure RadarRangeDec;
    procedure RadarRangeInc;
    procedure Searchlight;
    procedure ShieldCell;
    procedure ChaffLauncher;
    procedure ChargeECM;
    procedure WeaponColour;
    procedure EngineColour;
    procedure NightVision;
    {CMD : Changement de mode}
    procedure LeftPanel;
    procedure CommsPanel;
    procedure QuickCommsPanel;
    procedure RadarPanel;
    procedure RightPanel;
    procedure MapGalaxy;
    procedure MapSystem;
    procedure Pause;
    procedure FriendBoard;
    procedure Codex;
    procedure CockpitMode(const Mode: TCockpitModeType = cmt_none);
    procedure ModeACS;
    {CMD : Mode tableau de bord}
    procedure Up(index: Byte; tms: Integer = 0);
    procedure Down(index: Byte; tms: Integer = 0);
    procedure Left(index: Byte; tms: Integer = 0);
    procedure Right(index: Byte; tms: Integer = 0);
    procedure UISelect;
    procedure UIBack;
    procedure CloseCarte;
    procedure NextSheet(index: Byte);
    procedure PriorSheet(index: Byte);
    procedure NextPage;
    procedure PriorPage;
    {CMD : Conduite}
    procedure DriveAssist;
    procedure TurnLeft(tms: Integer = 0);
    procedure TurnRight(tms: Integer = 0);
    procedure VerticalThruster(tms: Integer = 0);
    procedure VRSPrimaryFire(tms: Integer = 0);
    procedure VRSSecondaryFire(tms: Integer = 0);
    procedure VRSAutoBreak;
    procedure VRSSearchlight;
    procedure VRSTurret;
    procedure VRSNextArmGroup;
    procedure VRSPriorArmGroup;
    {CMD : Conduite visée}
    procedure VRSTarget12h;
    {CMD : Conduite tourelle}
    procedure TurretYawLeft(tms: Integer = 0);
    procedure TurretYawRight(tms: Integer = 0);
    procedure TurretPitchUp(tms: Integer = 0);
    procedure TurretPitchDown(tms: Integer = 0);
    {CMD : Conduite propulsion}
    procedure PropulsionReverse;
    procedure PropulsionAcceleration(tms: Integer = 0);
    procedure PropulsionDeceleration(tms: Integer = 0);
    {CMD : Conduite divers }
    procedure VRSCargoScoop;
    procedure VRSCargoEject;
    procedure ShipDismissRecall;
    {CMD : Ordres au chasseur}
    procedure RequestDock;
    procedure DefensiveBehaviour;
    procedure AggressiveBehaviour;
    procedure FocusTarget;
    procedure HoldFire;
    procedure HoldPosition;
    procedure Follow;
    procedure OpenOrders;
    {CMD : ACS}
    procedure ACSCameraPitchInc(tms: Integer); overload;
    procedure ACSCameraPitchInc; overload;
    procedure ACSCameraPitchDec(tms: Integer); overload;
    procedure ACSCameraPitchDec; overload;
    procedure ACSCameraYawInc(tms: Integer); overload;
    procedure ACSCameraYawInc; overload;
    procedure ACSCameraYawDec(tms: Integer); overload;
    procedure ACSCameraYawDec; overload;
    procedure ACSZoomIn(tms: Integer); overload;
    procedure ACSZoomIn; overload;
    procedure ACSZoomOut(tms: Integer); overload;
    procedure ACSZoomOut; overload;
    procedure ACSZoomInMini(tms: Integer); overload;
    procedure ACSZoomInMini; overload;
    procedure ACSZoomOutMini(tms: Integer); overload;
    procedure ACSZoomOutMini; overload;
    procedure ACSRadioInc(tms: Integer); overload;
    procedure ACSRadioInc; overload;
    procedure ACSRadioDec(tms: Integer); overload;
    procedure ACSRadioDec; overload;
    procedure ACSAnalyse(tms: Integer);
    procedure ACSClose;
    procedure ACSGetTarget;
    procedure ACSHelp;
    {CMD : DSD}
    procedure DSDViewChange;
    procedure DSDClose;
    procedure DSDYawLeft(tms: Integer); overload;
    procedure DSDYawLeft; overload;
    procedure DSDYawRight(tms: Integer); overload;
    procedure DSDYawRight; overload;
    procedure DSDPitchUp(tms: Integer); overload;
    procedure DSDPitchUp; overload;
    procedure DSDPitchDown(tms: Integer); overload;
    procedure DSDPitchDown; overload;
    procedure DSDZoomOut(tms: Integer); overload;
    procedure DSDZoomOut; overload;
    procedure DSDZoomIn(tms: Integer); overload;
    procedure DSDZoomIn; overload;
    {CMD : Carte de la galaxie}
    procedure CamPitchUp(tms: Integer); overload;
    procedure CamPitchUp; overload;
    procedure CamPitchDown(tms: Integer); overload;
    procedure CamPitchDown; overload;
    procedure CamYawLeft(tms: Integer); overload;
    procedure CamYawLeft; overload;
    procedure CamYawRight(tms: Integer); overload;
    procedure CamYawRight; overload;
    procedure CamZoomIn(tms: Integer); overload;
    procedure CamZoomIn; overload;
    procedure CamZoomOut(tms: Integer); overload;
    procedure CamZoomOut; overload;
    procedure GalaxyMapHome;
    {CMD : Système de cameras}
    procedure Camera_ShipShow;
    procedure Camera_VRSShow;
    procedure Camera_Next;
    procedure Camera_Prior;
    procedure Camera_One;
    procedure Camera_Two;
    procedure Camera_Three;
    procedure Camera_Four;
    procedure Camera_Five;
    procedure Camera_Six;
    procedure Camera_Seven;
    procedure Camera_Eight;
    procedure Camera_Nine;
    {CMD : Free cameras}
    procedure FreeCamera_Close;
    {CMD : Atterrissage manuel}
    procedure LandingYawLeft(tms: Integer = 0);
    procedure LandingYawRight(tms: Integer = 0);
    procedure LandingPitchUp(tms: Integer = 0);
    procedure LandingPitchDown(tms: Integer = 0);
    procedure LandingRollLeft(tms: Integer = 0);
    procedure LandingRollRight(tms: Integer = 0);
    procedure LandingLeftThrust(tms: Integer = 0);
    procedure LandingRightThrust(tms: Integer = 0);
    procedure LandingUpThrust(tms: Integer = 0);
    procedure LandingDownThrust(tms: Integer = 0);
    procedure LandingForwardThrust(tms: Integer = 0);
    procedure LandingBackwardThrust(tms: Integer = 0);
    {CMD : Equipage multiple}
    procedure Crew_SwitchMode;
    procedure Crew_PrimaryFire(tms: Integer = 0);
    procedure Crew_SecondaryFire(tms: Integer = 0);
    procedure Crew_ToolsPrimaryFire(tms: Integer = 0);
    procedure Crew_ToolsSecondaryFire(tms: Integer = 0);
    procedure Crew_YawLeft(tms: Integer = 0);
    procedure Crew_YawRight(tms: Integer = 0);
    procedure Crew_PitchUp(tms: Integer = 0);
    procedure Crew_PitchDown(tms: Integer = 0);
    procedure Crew_ZoomIn(tms: Integer = 0);
    procedure Crew_ZoomOut(tms: Integer = 0);
    procedure Crew_CokpitNext;
    procedure Crew_CokpitPrior;
    {CMD : NATO Alphabet}
    procedure AlphaKeyBoard(const Car: Char); overload;
    procedure AlphaKeyBoard(const Car: Char; Specials: TSpecials); overload;
    procedure AlphaKeyBoard(VKCode: SmallInt); overload;
    procedure Coller;

  public
    procedure SetTags(const Value: string);
    procedure SetKeyInventory(const Value: TKeyInventory);

    property MaxRepeat: Integer read GetMaxRepeat write SetMaxRepeat;

    constructor Create;
    destructor Destroy; override;
  published
  end;

  TEliteManager = class(TCustomEliteManager)
  public
    class procedure Initialize;
    class procedure TagAssign(const Value: string);
    class procedure KeyInventoryAssign(const Value: TKeyInventory);
    class procedure Finalize;
  published
  end;

var
  EliteManager         : TEliteManager = nil;
  EliteRunningObserver : TEliteRunningObserver;

implementation

uses
  { --- Pour gérer la saisie des textes
        SendKey(iKey: Smallint; Tms: Cardinal; Specials: TSpecials = []); }
  SendKey32;

{ TCustomEliteManager }

procedure TCustomEliteManager.AssignTagsToStack;
begin
  ProtectedCode( DoWithTag );
end;

procedure TCustomEliteManager.CallCommande(const Ast: string; Again: Boolean);
var
  indexCmd: Integer;
begin
  if not Assigned(FKeyInventory) or (ASt = '0') then Exit;
  try indexCmd := StrToInt( ASt ) except indexCmd := 0 end;
  EliteForeGround;
  case indexCmd of
    {*** VOL - ROTATION;  17-63}
    85    : Ouest   (1, 150);
    10850 : Ouest   (1, 90);
    10851 : Ouest   (1, 250);
    10852 : Ouest   (1, 400);
    10853 : Ouest   (1, 600);
    86    : Est     (1, 150);
    10860 : Est     (1, 90);
    10861 : Est     (1, 250);
    10862 : Est     (1, 400);
    10863 : Est     (1, 600);
    87    : Pere    (1, 150);
    10870 : Pere    (1, 90);
    10871 : Pere    (1, 250);
    10872 : Pere    (1, 400);
    10873 : Pere    (1, 600);
    88    : Fils    (1, 150);
    10880 : Fils    (1, 90);
    10881 : Fils    (1, 250);
    10882 : Fils    (1, 400);
    10883 : Fils    (1, 600);
    89    : Nord    (1, 150);
    10890 : Nord    (1, 90);
    10891 : Nord    (1, 250);
    10892 : Nord    (1, 400);
    10893 : Nord    (1, 400);
    90    : Sud     (1, 150);
    10900 : Sud     (1, 90);
    10901 : Sud     (1, 250);
    10902 : Sud     (1, 400);
    10903 : Sud     (1, 600);
    {*** VOL - POUSSEE; 64-115}
    79    : PousseGauche    (1, 90);
    80    : PousseDroite    (1, 90);
    81    : PousseHaut      (1, 90);
    82    : PousseBas       (1, 90);
    83    : PousseAvant     (1, 90);
    84    : PousseArriere   (1, 90);
    {*** VOL - PROPULSION; 146-225}
    67    : InverserPropulsion;
    68    : Acceleration;
    69    : Deceleration;
    70    : SpeedM100;
    71    : SpeedM75;
    72    : SpeedM50;
    73    : SpeedM25;
    74    : SpeedNull;
    75    : Speed25;
    76    : Speed50;
    77    : Speed75;
    78    : Speed100;
    {*** VOL - DIVERS; 305-348}
    60    : AssistanceDeVol(ft_none);
    10601 : AssistanceDeVol(ft_on);
    10602 : AssistanceDeVol(ft_off);
    61    : Boost;
    62    : FSD;
    63    : SuperNavigation;
    64    : PRL;
    65    : FixRotation;
    66    : OrbitDisplay;
    {*** VISEE; 349-432}
    46    : Target12h;
    47    : NextTarget;
    48    : PriorTarget;
    49    : MenacePrincipale;
    50    : MenaceSuivante;
    51    : MenacePrecedente;
    52    : Ailier1;
    53    : Ailier2;
    54    : Ailier3;
    55    : AilierTarget;
    10551 : CibleOfAilier(1);
    10552 : CibleOfAilier(2);
    10553 : CibleOfAilier(3);
    56    : AilierNavLock;
    10561 : NavigationOnAilier(1);
    10562 : NavigationOnAilier(2);
    10563 : NavigationOnAilier(3);
    57    : NextSubSystem;
    58    : PriorSubSystem;
    59    : NextRoute;
    {*** ARMES; 433-459}
    41    : PrimaryFire;
    10411 : PrimaryFire(1000);
    10412 : PrimaryFire(2000);
    10413 : PrimaryFire(3000);
    10414 : PrimaryFire(4000); 
    42    : SecondaryFire;
    10421 : SecondaryFire(1000);
    10422 : SecondaryFire(2000);
    10423 : SecondaryFire(3000);
    10424 : SecondaryFire(4000);
    43    : NextArmGroup;
    44    : PriorArmGroup;
    45    : HardPoint;
    {*** REFROIDISSEMENT; 460-472}
    39    : StealthyMode;
    40    : HeatSink;
    {*** DIVERS; 473-590}
    1     : LandingGear(lgt_none);
    99001 : LandingGear(lgt_open);
    99002 : LandingGear(lgt_close);
    2     : CargoEject;
    3     : CargoScoop;
    4     : PipeReset;
    5     : PipeSystem;
    6     : PipeArmes;
    7     : PipeMoteur;
    8     : RadarRangeDec;
    9     : RadarRangeInc;
    10    : Searchlight;
    11    : ShieldCell;
    12    : ChaffLauncher;
    13    : ChargeECM;
    14    : WeaponColour;
    15    : EngineColour;
    16    : NightVision;
    {*** CHANGEMENT DE MODE; 591-688}
    17    : LeftPanel;
    18    : CommsPanel;
    19    : QuickCommsPanel;
    20    : RadarPanel;
    21    : RightPanel;
    22    : MapGalaxy;
    23    : MapSystem;
    24    : Pause;
    25    : FriendBoard;
    26    : Codex;
    27    : CockpitMode( cmt_none        );
    10271 : CockpitMode( cmt_combat      );
    10272 : CockpitMode( cmt_exploration );
    28    : ModeACS;
    {*** MODE TABLEAU DE BORD; 689-732}
    29    : Down(1);
    30    : Up(1);
    31    : Left(1);
    32    : Right(1);
    33    : UISelect;
    34    : UIBack;
    10341 : CloseCarte;
    35    : NextSheet(1);
    10352 : NextSheet(2);
    10353 : NextSheet(3);
    10354 : NextSheet(4);
    10355 : NextSheet(5);
    36    : PriorSheet(1);
    10362 : PriorSheet(2);
    10363 : PriorSheet(3);
    10364 : PriorSheet(4);
    10365 : PriorSheet(5);
    37    : NextPage;
    38    : PriorPage;
    {*** CONDUITE; 862-968}
    113   : DriveAssist;
    114   : TurnLeft(500);
    115   : TurnRight(500);
    116   : FKeyInventory.KeyTrigger_( 'BuggyRollLeftButton', WITH_KEYUP);      //NA
    117   : FKeyInventory.KeyTrigger_( 'BuggyRollRightButton', WITH_KEYUP);     //NA
    118   : FKeyInventory.KeyTrigger_( 'BuggyPitchUpButton', WITH_KEYUP);       //NA
    119   : FKeyInventory.KeyTrigger_( 'BuggyPitchDownButton', WITH_KEYUP);     //NA
    120   : VerticalThruster(500);
    121   : VRSPrimaryFire;
    122   : VRSSecondaryFire;
    123   : VRSAutoBreak;
    124   : VRSSearchlight;
    125   : VRSTurret;
    126   : VRSNextArmGroup;
    127   : VRSPriorArmGroup;
    {*** CONDUITE VISEEE; 969-974}
    128   : VRSTarget12h;
    {*** CONDUITE TOURELLE; 975-1019}
    129   : TurretYawLeft(40);
    130   : TurretYawRight(40);
    131   : TurretPitchUp(40);
    132   : TurretPitchDown(40);
    {*** CONDUITE PROPULSION; 1020-1055}
    133   : PropulsionReverse;
    134   : PropulsionAcceleration(200);
    135   : PropulsionDeceleration(200);
    {*** CONDUITE DIVERS; 1056-1099}
    136   : FKeyInventory.KeyTrigger_( 'IncreaseEnginesPower_Buggy', WITH_KEYUP);    //NA
    137   : FKeyInventory.KeyTrigger_( 'IncreaseWeaponsPower_Buggy', WITH_KEYUP);    //NA
    138   : FKeyInventory.KeyTrigger_( 'IncreaseSystemsPower_Buggy', WITH_KEYUP);    //NA
    139   : FKeyInventory.KeyTrigger_( 'ResetPowerDistribution_Buggy', WITH_KEYUP);  //NA
    140   : VRSCargoScoop;
    141   : VRSCargoEject;
    142   : ShipDismissRecall;
    {*** CONDUITE MODES; 1100-1165}
          // NA
    {*** ORDRES AU CHASSEUR; 1238-1290}
    105   : RequestDock;
    106   : DefensiveBehaviour;
    107   : AggressiveBehaviour;
    108   : FocusTarget;
    109   : HoldFire;
    110   : HoldPosition;
    111   : Follow;
    112   : OpenOrders;
    {*** DETECTEUR D'ANALYSE COMPLETE DU SYSTEME; 1561-1659}
    143   : ACSCameraPitchInc(50);
    11431 : ACSCameraPitchInc(500); 
    144   : ACSCameraPitchDec(50);
    11441 : ACSCameraPitchDec(500); 
    145   : ACSCameraYawInc(50);
    11451 : ACSCameraYawInc(500);
    146   : ACSCameraYawDec(50);
    11461 : ACSCameraYawDec(500);
    147   : ACSZoomIn(100);
    148   : ACSZoomOut(100);
    149   : ACSZoomInMini(50);
    150   : ACSZoomOutMini(50);
    151   : ACSRadioInc(50);
    152   : ACSRadioDec(50);
    153   : ACSAnalyse(5000);
    154   : ACSClose;
    155   : ACSGetTarget;
    156   : ACSHelp;
    {*** DETECTEUR DE SURFACE DETAILLEE; 1660-1714}
    164   : DSDViewChange;
    165   : DSDClose;
    166   : DSDYawLeft(150);
    167   : DSDYawRight(150);
    168   : DSDPitchUp(150);
    169   : DSDPitchDown(150);
    170   : DSDZoomOut(150);
    171   : DSDZoomIn(150);
    {*** VOL ATTERRISSAGE MANUEL 226-305}
    172   : LandingYawLeft;
    173   : LandingYawRight;
    174   : LandingPitchUp;
    175   : LandingPitchDown;
    176   : LandingRollLeft;
    177   : LandingRollRight;
    178   : LandingLeftThrust;
    179   : LandingRightThrust;
    180   : LandingUpThrust;
    181   : LandingDownThrust;
    182   : LandingForwardThrust;
    183   : LandingBackwardThrust;
    {*** EQUIPAGE MULTIPLE 1186-1257}
    184   : Crew_SwitchMode;
    185   : Crew_PrimaryFire(1500);
    186   : Crew_SecondaryFire(1500);
    187   : Crew_ToolsPrimaryFire(1500);
    188   : Crew_ToolsSecondaryFire(1500);
    189   : Crew_YawLeft(150);
    190   : Crew_YawRight(150);
    191   : Crew_PitchUp(150);
    192   : Crew_PitchDown(150);
    193   : Crew_ZoomOut(150);
    194   : Crew_ZoomIn(150);
    195   : Crew_CokpitNext;
    196   : Crew_CokpitPrior;
    {*** CARTE DE LA GALAXIE 781-867}
    157   : CamPitchUp(200);
    158   : CamPitchDown(200);
    159   : CamYawLeft(200);
    160   : CamYawRight(200);
    161   : CamZoomIn(200);
    162   : CamZoomOut(200);
    163   : GalaxyMapHome;
    {*** SYSTÈME DE CAMERAS 1311-1370}
    91    : Camera_ShipShow;
    92    : Camera_VRSShow;
    93    : Camera_Next;
    94    : Camera_Prior;
    95    : Camera_One;
    96    : Camera_Two;
    97    : Camera_Three;
    98    : Camera_Four;
    99    : Camera_Five;
    100   : Camera_Six;
    101   : Camera_Seven;
    102   : Camera_Eight;
    103   : Camera_Nine;
    {*** LISTE DE LECTURE 1564-1580}
    {*** CAMERA LIBRE 1401-…}
    104   : FreeCamera_Close;

    { --- Vol }
    9950  : Decollage;
    { --- Pipe }
    9990  : FullSystem;
    9991  : FullArme;
    9992  : FullMoteur;
    9993  : CombatOffensif;
    9994  : CombatDefensif;
    9995  : ModeFuite;
    9996  : ModeDefensif; 
    { --- Key Entrée }
    9998  : TKeyMessageSender.Signal(VK_RETURN, 30, WITH_KEYUP);
    { --- Again methodes }
    9999  : IterateCallCommande(    1 );
    10000 : IterateCallCommande(    2 );
    10001 : IterateCallCommande(    3 );
    10002 : IterateCallCommande(    4 );
    10003 : IterateCallCommande(    5 );
    10004 : IterateCallCommande(   10 );
    10005 : IterateCallCommande(   20 );
    10006 : IterateCallCommande(   50 );
    10007 : IterateCallCommande(  100 );
    10008 : IterateCallCommande(  200 );
    10009 : IterateCallCommande(  500 );
    {descente remontée}
    10011 : Up(1);
    10012 : Up(2);
    10013 : Up(3);
    10014 : Up(4);
    10015 : Up(5);
    10021 : Down(1);
    10022 : Down(2);
    10023 : Down(3);
    10024 : Down(4);
    10025 : Down(5);
    10031 : Left(1);
    10032 : Left(2);
    10033 : Left(3);
    10034 : Left(4);
    10035 : Left(5);
    10041 : Right(1);
    10042 : Right(2);
    10043 : Right(3);
    10044 : Right(4);
    10045 : Right(5);
    {Zoom de-zoom}
    10046 : MainZoom;
    10047 : MainDezoom;
    {Nord/sud/est/ouest}
    20012 : Ouest(2, 150);
    20013 : Ouest(3, 150);
    20014 : Ouest(4, 150);
    20015 : Ouest(5, 150);
    20022 : Est(2, 150);
    20023 : Est(3, 150);
    20024 : Est(4, 150);
    20025 : Est(5, 150);
    20032 : Pere(2, 150);
    20033 : Pere(3, 150);
    20034 : Pere(4, 150);
    20035 : Pere(5, 150);
    20042 : Fils(2, 150);
    20043 : Fils(3, 150);
    20044 : Fils(4, 150);
    20045 : Fils(5, 150);
    20052 : Nord(2, 150);
    20053 : Nord(3, 150);
    20054 : Nord(4, 150);
    20055 : Nord(5, 150);
    20062 : Sud(2, 150);
    20063 : Sud(3, 150);
    20064 : Sud(4, 150);
    20065 : Sud(5, 150);
    {Pousses multiples}
    20112 : PousseGauche(2, 90);
    20113 : PousseGauche(3, 90);
    20114 : PousseGauche(4, 90);
    20115 : PousseGauche(5, 90);
    20122 : PousseDroite(2, 90);
    20123 : PousseDroite(3, 90);
    20124 : PousseDroite(4, 90);
    20125 : PousseDroite(5, 90);
    20132 : PousseHaut(2, 90);
    20133 : PousseHaut(3, 90);
    20134 : PousseHaut(4, 90);
    20135 : PousseHaut(5, 90);
    20142 : PousseBas(2, 90);
    20143 : PousseBas(3, 90);
    20144 : PousseBas(4, 90);
    20145 : PousseBas(5, 90);
    20152 : PousseAvant(2, 90);
    20153 : PousseAvant(3, 90);
    20154 : PousseAvant(4, 90);
    20155 : PousseAvant(5, 90);
    20162 : PousseArriere(2, 90);
    20163 : PousseArriere(3, 90);
    20164 : PousseArriere(4, 90);
    20165 : PousseArriere(5, 90);
    {NATO Alphabet}
    30001 : AlphaKeyBoard('A');
    30002 : AlphaKeyBoard('B');
    30003 : AlphaKeyBoard('C');
    30004 : AlphaKeyBoard('D');
    30005 : AlphaKeyBoard('E');
    30006 : AlphaKeyBoard('F');
    30007 : AlphaKeyBoard('G');
    30008 : AlphaKeyBoard('H');
    30009 : AlphaKeyBoard('I');
    30010 : AlphaKeyBoard('J');
    30011 : AlphaKeyBoard('K');
    30012 : AlphaKeyBoard('L');
    30013 : AlphaKeyBoard('M');
    30014 : AlphaKeyBoard('N');
    30015 : AlphaKeyBoard('O');
    30016 : AlphaKeyBoard('P');
    30017 : AlphaKeyBoard('Q');
    30018 : AlphaKeyBoard('R');
    30019 : AlphaKeyBoard('S');
    30020 : AlphaKeyBoard('T');
    30021 : AlphaKeyBoard('U');
    30022 : AlphaKeyBoard('V');
    30023 : AlphaKeyBoard('W');
    30024 : AlphaKeyBoard('X');
    30025 : AlphaKeyBoard('Y');
    30026 : AlphaKeyBoard('Z');
    30037 : AlphaKeyBoard(VK_NUMPAD0);
    30027 : AlphaKeyBoard(VK_NUMPAD1);
    30028 : AlphaKeyBoard(VK_NUMPAD2);
    30029 : AlphaKeyBoard(VK_NUMPAD3);
    30030 : AlphaKeyBoard(VK_NUMPAD4);
    30031 : AlphaKeyBoard(VK_NUMPAD5);
    30032 : AlphaKeyBoard(VK_NUMPAD6);
    30033 : AlphaKeyBoard(VK_NUMPAD7);
    30034 : AlphaKeyBoard(VK_NUMPAD8);
    30035 : AlphaKeyBoard(VK_NUMPAD9);
    30036 : AlphaKeyBoard(VK_SPACE);
    30038 : AlphaKeyBoard(VK_BACK);
    30039 : AlphaKeyBoard(VK_LEFT);
    30040 : AlphaKeyBoard(VK_RIGHT);
    30041 : Coller;
    30042 : AlphaKeyBoard(VK_ADD);
    30043 : AlphaKeyBoard(VK_SUBTRACT);
  end;
  if not Again and not IsAgainCommand(indexCmd) then LastCmd := indexCmd;
end; {CallCommande}

function TCustomEliteManager.CanLongRepeat: Boolean;
begin
  Result := IndexStr( IntToStr(LastCmd),
              ['29', '30', '31', '32', '35', '36', '37', '38'] ) > -1
end;

procedure TCustomEliteManager.CibleOfAilier(index: Byte);
begin
  case index of
    1  : FKeyInventory.KeyTrigger_( 'TargetWingman0' , WITH_KEYUP);
    2  : FKeyInventory.KeyTrigger_( 'TargetWingman1' , WITH_KEYUP);
    else FKeyInventory.KeyTrigger_( 'TargetWingman2' , WITH_KEYUP);
  end;
  FKeyInventory.KeyTrigger_( 'SelectTargetsTarget' ,   WITH_KEYUP);
end;

constructor TCustomEliteManager.Create;
begin
  inherited Create;
  FKeyInventory := nil;
  FTagStack     := TStringList.Create;
  InitializeMutex;
  LastCmd       := 0;
end;

destructor TCustomEliteManager.Destroy;
begin
  FinalizeMutex;
  FTagStack.Free;
  inherited
end;

procedure TCustomEliteManager.DoOnStack(Sender: TObject);
begin
  with FTagStack do
  try
    while Count > 0 do begin
      CallCommande( Strings[ Pred(Count) ] );
      Delete( Pred(Count) )
    end
  finally
  end
end;

procedure TCustomEliteManager.DoWithTag(Sender: TObject);
var
  Buffer : string;
  ASt    : string;
begin
  FTagStack.Clear;
  ASt := FTags;
  while Pos('.', ASt) > 0 do begin
    Buffer := GetBeforStr(ASt, '.');
    ASt    := GetAfterStr(ASt, '.');
    if Buffer <> 'undefined' then FTagStack.Insert(0, Buffer )
  end;
  if (Ast <> EmptyStr) and (Ast <> 'undefined') then FTagStack.Insert(0, Ast )
end;

procedure TCustomEliteManager.Fils;
begin
  FKeyInventory.KeyTrigger_('RollRightButton' , WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.FinalizeMutex;
begin
  CloseHandle( EliteMutex )
end;

procedure TCustomEliteManager.FSD;
begin
  if not EliteStatus.SuperCruise then FKeyInventory.KeyTrigger_( 'SetSpeed100', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'HyperSuperCombination', WITH_KEYUP);
end;

procedure TCustomEliteManager.FullSystem;
begin
  PipeReset;
  PipeSystem;
  PipeSystem;
end;

procedure TCustomEliteManager.FullArme;
begin
  PipeReset;
  PipeArmes;
  PipeArmes;
end;

procedure TCustomEliteManager.FullMoteur;
begin
  PipeReset;
  PipeMoteur;
  PipeMoteur;
end;

procedure TCustomEliteManager.InitializeMutex;
begin
  EliteMutex := CreateMutex(nil, False, 'StackElite');
  if EliteMutex = 0 then RaiseLastOSError;
end;

function TCustomEliteManager.IsAgainCommand(const Cmd: Integer): Boolean;
begin
  Result := IndexStr( IntToStr(Cmd), ['9999', '10000', '10001', '10002', '10003',
     '10004', '10005', '10006', '10007', '10008', '10009',
     {TODO traiter à part}
     '9990', '9991', '9992'] ) > -1
end;

procedure TCustomEliteManager.IterateCallCommande(Count: Integer);
var
  i : Integer;
  M : Integer;
begin
  M := MaxRepeat;
  for i := 1 to Count do begin
    Application.ProcessMessages;
    CallCommande(IntToStr( LastCmd ), True);
    if (i >= M) and not CanLongRepeat then Break
  end
end;

procedure TCustomEliteManager.NavigationOnAilier(index: Byte);
begin
  case index of
    1  : FKeyInventory.KeyTrigger_( 'TargetWingman0' , WITH_KEYUP);
    2  : FKeyInventory.KeyTrigger_( 'TargetWingman1' , WITH_KEYUP);
    else FKeyInventory.KeyTrigger_( 'TargetWingman2' , WITH_KEYUP);
  end;
  FKeyInventory.KeyTrigger_( 'WingNavLock', WITH_KEYUP);
end;

procedure TCustomEliteManager.PRL;
begin
  if not EliteStatus.SuperCruise then FKeyInventory.KeyTrigger_( 'SetSpeed100', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'Hyperspace',  WITH_KEYUP)
end;

procedure TCustomEliteManager.ProcessOnStack;
begin
  ProtectedCode( DoOnStack );
end;

procedure TCustomEliteManager.ProtectedCode(Method: TNotifyEvent);
begin
  if Assigned(Method) then
    if WaitForSingleObject(EliteMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Method(nil);
    finally
      ReleaseMutex( EliteMutex )
    end
end;

procedure TCustomEliteManager.SetKeyInventory(const Value: TKeyInventory);
begin
  FKeyInventory := Value
end;

procedure TCustomEliteManager.SetTags(const Value: string);  {WARNING PROD}
begin
  { --- Elite dangerous processus doit être en cours de fonctionnement }
  if KeyReadBoolean(AppKey, 'EliteLaunched', False) then begin
    FTags := Value;
    AssignTagsToStack;
    ProcessOnStack
  end
end;

procedure TCustomEliteManager.Sud;
begin
  FKeyInventory.KeyTrigger_('PitchDownButton' , WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.SuperNavigation;
begin
  if not EliteStatus.SuperCruise then FKeyInventory.KeyTrigger_( 'SetSpeed100', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'Supercruise', WITH_KEYUP)
end;

function TCustomEliteManager.GetMaxRepeat: Integer;
begin
  Result := KeyReadInt(ParamKey, 'MaxRepeat', 20)
end;

procedure TCustomEliteManager.SetMaxRepeat(const Value: Integer);
begin
  KeyWrite(ParamKey, 'MaxRepeat', Value)
end;

procedure TCustomEliteManager.Camera_ShipShow;
begin
  FKeyInventory.KeyTrigger_( 'PhotoCameraToggle', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'FreeCamToggleHUD',  WITH_KEYUP);
end;

procedure TCustomEliteManager.Camera_VRSShow;
begin
  FKeyInventory.KeyTrigger_( 'PhotoCameraToggle_Buggy', WITH_KEYUP);
  FKeyInventory.KeyTrigger_( 'FreeCamToggleHUD',        WITH_KEYUP);
end;

procedure TCustomEliteManager.Up(index: Byte; tms: Integer);
var
  i : Integer;
begin
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_( 'UI_Down', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'UI_Down', tms)
end;

procedure TCustomEliteManager.Down(index: Byte; tms: Integer);
var
  i : Integer;
begin
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_( 'UI_Up', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'UI_Up', tms)
end;

procedure TCustomEliteManager.Left(index: Byte; tms: Integer);
var
  i : Integer;
begin
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_( 'UI_Left', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'UI_Left', tms)
end;

procedure TCustomEliteManager.Right(index: Byte; tms: Integer);
var
  i : Integer;
begin
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_( 'UI_Right', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'UI_Right', tms)
end;

procedure TCustomEliteManager.Nord(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap : ASt := 'PitchUpButton';
    gt_systemmap : ASt := 'RollLeftButton'; { --> Père value }
    else
      if EliteStatus.LandinGearDown then ASt := 'PitchUpButton_Landing'
        else ASt := 'PitchUpButton';
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt , WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Sud(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap : ASt := 'PitchDownButton';
    gt_systemmap : ASt := 'RollRightButton'; { --> Fils value }
    else
      if EliteStatus.LandinGearDown then ASt := 'PitchDownButton_Landing'
        else ASt := 'PitchDownButton';
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt , WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Est(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'YawRightButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'YawRightButton_Landing'
        else ASt := 'YawRightButton';
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Ouest(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'YawLeftButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'YawLeftButton_Landing'
        else ASt := 'YawLeftButton';
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Pere(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'RollLeftButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'RollLeftButton_Landing'
        else ASt := 'RollLeftButton';
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.Fils(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap : ASt := 'RollRightButton';
    else
      if EliteStatus.LandinGearDown then ASt := 'RollRightButton_Landing'
        else ASt := 'RollRightButton';
  end;
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt , WITH_KEYUP)
       else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseGauche(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'LeftThrustButton_Landing'
    else ASt := 'LeftThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseDroite(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'RightThrustButton_Landing'
    else ASt := 'RightThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseHaut(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'UpThrustButton_Landing'
    else ASt := 'UpThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseBas(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'DownThrustButton_Landing'
    else ASt := 'DownThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseAvant(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'ForwardThrustButton_Landing'
    else ASt := 'ForwardThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PousseArriere(index: Byte; tms: Integer);
var
  i   : Integer;
  ASt : string;
begin
  if EliteStatus.LandinGearDown then ASt := 'BackwardThrustButton_Landing'
    else ASt := 'BackwardThrustButton';
  for i := 1 to index do
    if tms < 1 then FKeyInventory.KeyTrigger_(ASt, WITH_KEYUP)
      else FKeyInventory.KeyTrigger_(ASt, tms)
end;

procedure TCustomEliteManager.PipeMoteur;
begin
  FKeyInventory.KeyTrigger_( 'IncreaseEnginesPower' ,   WITH_KEYUP)
end;

procedure TCustomEliteManager.PipeSystem;
begin
  FKeyInventory.KeyTrigger_( 'IncreaseSystemsPower' ,   WITH_KEYUP)
end;

procedure TCustomEliteManager.PipeArmes;
begin
  FKeyInventory.KeyTrigger_( 'IncreaseWeaponsPower' ,   WITH_KEYUP)
end;

procedure TCustomEliteManager.PipeReset;
begin
  FKeyInventory.KeyTrigger_( 'ResetPowerDistribution' , WITH_KEYUP)
end;

procedure TCustomEliteManager.CombatOffensif;
begin
  PipeReset;
  PipeArmes; PipeArmes; PipeArmes;
  PipeMoteur;
  PipeArmes;
  PipeSystem
end;

procedure TCustomEliteManager.CombatDefensif;
begin
  PipeReset;
  PipeSystem;
  PipeArmes;
end;

procedure TCustomEliteManager.ModeFuite;
begin
  PipeReset;
  PipeSystem;
  PipeMoteur;
  PipeSystem;
  PipeMoteur;
end;

procedure TCustomEliteManager.ModeDefensif;
begin
  PipeReset;
  PipeSystem; PipeSystem;
  PipeMoteur;
  PipeSystem;
  PipeArmes; PipeArmes;
  PipeSystem;
end;

procedure TCustomEliteManager.PousseeHaut(const sTime: Cardinal);
begin
  FKeyInventory.KeyTrigger_( 'UpThrustButton' , sTime);
end;

procedure TCustomEliteManager.TrainAtterrissage;
begin
  FKeyInventory.KeyTrigger_( 'LandingGearToggle', WITH_KEYUP);
end;

procedure TCustomEliteManager.Decollage;
begin
  PousseeHaut(1000);
  TrainAtterrissage;
  PousseeHaut(2000);
  PousseeHaut(1000);
  PousseeHaut(500);
end;

procedure TCustomEliteManager.InverserPropulsion;
begin
  FKeyInventory.KeyTrigger_( 'ToggleReverseThrottleInput', WITH_KEYUP)
end;

procedure TCustomEliteManager.Acceleration;
begin
  case EliteStatus.InSrv of
    True : PropulsionAcceleration(200);
    else FKeyInventory.KeyTrigger_( 'ForwardKey', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.Deceleration;
begin
  case EliteStatus.InSrv of
    True : PropulsionDeceleration(200);
    else FKeyInventory.KeyTrigger_( 'BackwardKey', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.SpeedM100;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedMinus100', WITH_KEYUP)
end;

procedure TCustomEliteManager.SpeedM75;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedMinus75', WITH_KEYUP)
end;

procedure TCustomEliteManager.SpeedM50;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedMinus50', WITH_KEYUP)
end;

procedure TCustomEliteManager.SpeedM25;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedMinus25', WITH_KEYUP)
end;

procedure TCustomEliteManager.SpeedNull;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeedZero', WITH_KEYUP)
end;

procedure TCustomEliteManager.Speed25;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeed25', WITH_KEYUP)
end;

procedure TCustomEliteManager.Speed50;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeed50', WITH_KEYUP)
end;

procedure TCustomEliteManager.Speed75;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeed75', WITH_KEYUP)
end;

procedure TCustomEliteManager.Speed100;
begin
  FKeyInventory.KeyTrigger_( 'SetSpeed100', WITH_KEYUP)
end;

procedure TCustomEliteManager.AssistanceDeVol(const FAO: TFADOType);
begin
  case FAO of
     ft_on  : if not EliteStatus.FlightAssistOff then Exit;
     ft_off : if EliteStatus.FlightAssistOff then Exit;
  end;
  FKeyInventory.KeyTrigger_( 'ToggleFlightAssist', WITH_KEYUP)
end;

procedure TCustomEliteManager.Boost;
begin
  FKeyInventory.KeyTrigger_( 'UseBoostJuice', WITH_KEYUP)
end;

procedure TCustomEliteManager.FixRotation;
begin
  FKeyInventory.KeyTrigger_( 'DisableRotationCorrectToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.OrbitDisplay;
begin
  FKeyInventory.KeyTrigger_( 'OrbitLinesToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.Ouest;
begin
  FKeyInventory.KeyTrigger_('YawLeftButton' , WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.Target12h;
begin
  FKeyInventory.KeyTrigger_( 'SelectTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextTarget;
begin
  FKeyInventory.KeyTrigger_( 'CycleNextTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorTarget;
begin
  FKeyInventory.KeyTrigger_( 'CyclePreviousTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.MenacePrincipale;
begin
  FKeyInventory.KeyTrigger_( 'SelectHighestThreat', WITH_KEYUP)
end;

procedure TCustomEliteManager.MenaceSuivante;
begin
  FKeyInventory.KeyTrigger_( 'CycleNextHostileTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.MenacePrecedente;
begin
  FKeyInventory.KeyTrigger_( 'CyclePreviousHostileTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.Ailier1;
begin
  FKeyInventory.KeyTrigger_( 'TargetWingman0', WITH_KEYUP)
end;

procedure TCustomEliteManager.Ailier2;
begin
  FKeyInventory.KeyTrigger_( 'TargetWingman1', WITH_KEYUP)
end;

procedure TCustomEliteManager.Ailier3;
begin
  FKeyInventory.KeyTrigger_( 'TargetWingman2', WITH_KEYUP)
end;

procedure TCustomEliteManager.AilierTarget;
begin
  FKeyInventory.KeyTrigger_( 'SelectTargetsTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextSubSystem;
begin
  FKeyInventory.KeyTrigger_( 'CycleNextSubsystem', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorSubSystem;
begin
  FKeyInventory.KeyTrigger_( 'CyclePreviousSubsystem', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextRoute;
begin
  FKeyInventory.KeyTrigger_( 'TargetNextRouteSystem', WITH_KEYUP)
end;

procedure TCustomEliteManager.AilierNavLock;
begin
  FKeyInventory.KeyTrigger_( 'WingNavLock', WITH_KEYUP)
end;

procedure TCustomEliteManager.PrimaryFire(tms: Integer);

  procedure ShipFire; begin
    if tms < 1 then FKeyInventory.KeyTrigger_( 'PrimaryFire', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'PrimaryFire', tms)
  end;

begin
  case EliteStatus.InSrv of
    True  : VRSPrimaryFire;
    False : ShipFire;
  end
end; {PrimaryFire}

procedure TCustomEliteManager.SecondaryFire(tms: Integer);

  procedure ShipFire; begin
    if tms < 1 then FKeyInventory.KeyTrigger_( 'SecondaryFire', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'SecondaryFire', tms)
  end;

begin
  case EliteStatus.InSrv of
    True  : VRSSecondaryFire;
    False : ShipFire;
  end
end; {SecondaryFire}

procedure TCustomEliteManager.NextArmGroup;
begin
  FKeyInventory.KeyTrigger_( 'CycleFireGroupNext', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorArmGroup;
begin
  FKeyInventory.KeyTrigger_( 'CycleFireGroupPrevious', WITH_KEYUP)
end;

procedure TCustomEliteManager.HardPoint;
begin
  FKeyInventory.KeyTrigger_( 'DeployHardpointToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.StealthyMode;
begin
  FKeyInventory.KeyTrigger_( 'ToggleButtonUpInput', WITH_KEYUP)
end;

procedure TCustomEliteManager.HeatSink;
begin
  FKeyInventory.KeyTrigger_( 'DeployHeatSink', WITH_KEYUP)
end;

procedure TCustomEliteManager.LandingGear(const Mode: TLandingGearType);
begin
  case Mode of
    lgt_open  : if EliteStatus.LandinGearDown then Exit;
    lgt_close : if not EliteStatus.LandinGearDown then Exit;
  end;
  FKeyInventory.KeyTrigger_( 'LandingGearToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.CargoEject;
begin
  FKeyInventory.KeyTrigger_( 'EjectAllCargo', WITH_KEYUP)
end;

procedure TCustomEliteManager.CargoScoop;
begin
  FKeyInventory.KeyTrigger_( 'ToggleCargoScoop', WITH_KEYUP)
end;

procedure TCustomEliteManager.RadarRangeDec;
begin
  FKeyInventory.KeyTrigger_( 'RadarDecreaseRange', WITH_KEYUP)
end;

procedure TCustomEliteManager.RadarRangeInc;
begin
  FKeyInventory.KeyTrigger_( 'RadarIncreaseRange', WITH_KEYUP)
end;

procedure TCustomEliteManager.Searchlight;
begin
  FKeyInventory.KeyTrigger_( 'ShipSpotLightToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.ShieldCell;
begin
  if not EliteStatus.ShieldsUp then
    FKeyInventory.KeyTrigger_( 'UseShieldCell', WITH_KEYUP)
end;

procedure TCustomEliteManager.ChaffLauncher;
begin
  FKeyInventory.KeyTrigger_( 'FireChaffLauncher', WITH_KEYUP)
end;

procedure TCustomEliteManager.ChargeECM;
begin
  FKeyInventory.KeyTrigger_( 'ChargeECM', 5000)
end;

procedure TCustomEliteManager.WeaponColour;
begin
  FKeyInventory.KeyTrigger_( 'WeaponColourToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.EngineColour;
begin
  FKeyInventory.KeyTrigger_( 'EngineColourToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.Est;
begin
  FKeyInventory.KeyTrigger_('YawRightButton' , WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.NightVision;
begin
  FKeyInventory.KeyTrigger_( 'NightVisionToggle', WITH_KEYUP)
end;

procedure TCustomEliteManager.Nord;
begin
  FKeyInventory.KeyTrigger_('PitchUpButton' , WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.LeftPanel;
begin
  FKeyInventory.KeyTrigger_( 'FocusLeftPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.CommsPanel;
begin
  FKeyInventory.KeyTrigger_( 'FocusCommsPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.QuickCommsPanel;
begin
  FKeyInventory.KeyTrigger_( 'QuickCommsPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.RadarPanel;
begin
  FKeyInventory.KeyTrigger_( 'FocusRadarPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.RightPanel;
begin
  FKeyInventory.KeyTrigger_( 'FocusRightPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.MapGalaxy;
begin
  FKeyInventory.KeyTrigger_( 'GalaxyMapOpen', WITH_KEYUP)
end;

procedure TCustomEliteManager.MapSystem;
begin
  FKeyInventory.KeyTrigger_( 'SystemMapOpen', WITH_KEYUP)
end;

procedure TCustomEliteManager.Pause;
begin
  FKeyInventory.KeyTrigger_( 'Pause', WITH_KEYUP)
end;

procedure TCustomEliteManager.Pere;
begin
  FKeyInventory.KeyTrigger_('RollLeftButton' , WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.FriendBoard;
begin
  FKeyInventory.KeyTrigger_( 'FriendsMenu', WITH_KEYUP)
end;

procedure TCustomEliteManager.Codex;
begin
  FKeyInventory.KeyTrigger_( 'OpenCodexGoToDiscovery', WITH_KEYUP)
end;

procedure TCustomEliteManager.CockpitMode(const Mode: TCockpitModeType);
begin
  case Mode of
    cmt_combat      : if not EliteStatus.HudInanalysisMode then Exit;
    cmt_exploration : if EliteStatus.HudInanalysisMode then Exit;
  end;
  FKeyInventory.KeyTrigger_( 'PlayerHUDModeToggle', WITH_KEYUP)
end; {CockpitMode}

procedure TCustomEliteManager.ModeACS;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSEnter', WITH_KEYUP);
end;

procedure TCustomEliteManager.UISelect;
begin
  FKeyInventory.KeyTrigger_( 'UI_Select', WITH_KEYUP)
end;

procedure TCustomEliteManager.UIBack;
begin
  FKeyInventory.KeyTrigger_( 'UI_Back', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextSheet(index: Byte);
var
  i : Integer;
begin
  for i := 1 to index do FKeyInventory.KeyTrigger_('CycleNextPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorSheet(index: Byte);
var
  i : Integer;
begin
  for i := 1 to index do FKeyInventory.KeyTrigger_('CyclePreviousPanel', WITH_KEYUP)
end;

procedure TCustomEliteManager.NextPage;
begin
  FKeyInventory.KeyTrigger_( 'CycleNextPage', WITH_KEYUP)
end;

procedure TCustomEliteManager.PriorPage;
begin
  FKeyInventory.KeyTrigger_( 'CyclePreviousPage', WITH_KEYUP)
end;

procedure TCustomEliteManager.DriveAssist;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'ToggleDriveAssist', WITH_KEYUP);
  end
end;

procedure TCustomEliteManager.TurnLeft(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SteerLeftButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SteerLeftButton', tms)
    end
  end
end;

procedure TCustomEliteManager.TurnRight(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SteerRightButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SteerRightButton', tms);
    end
  end;
end;

procedure TCustomEliteManager.VerticalThruster(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'VerticalThrustersButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'VerticalThrustersButton', tms);
    end
  end
end;

procedure TCustomEliteManager.VRSPrimaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyPrimaryFireButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'BuggyPrimaryFireButton', tms)
end;

procedure TCustomEliteManager.VRSSecondaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggySecondaryFireButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'BuggySecondaryFireButton', tms)
end;

procedure TCustomEliteManager.VRSAutoBreak;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'AutoBreakBuggyButton', WITH_KEYUP);
  end
end;

procedure TCustomEliteManager.VRSSearchlight;
begin
  FKeyInventory.KeyTrigger_( 'HeadlightsBuggyButton', WITH_KEYUP)
end;

procedure TCustomEliteManager.VRSTurret;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'ToggleBuggyTurretButton', WITH_KEYUP);
  end
end;

procedure TCustomEliteManager.VRSNextArmGroup;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'BuggyCycleFireGroupNext', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.VRSPriorArmGroup;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'BuggyCycleFireGroupPrevious', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.VRSTarget12h;
begin
  FKeyInventory.KeyTrigger_( 'SelectTarget_Buggy', WITH_KEYUP)
end;

procedure TCustomEliteManager.TurretYawLeft(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyTurretYawLeftButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'BuggyTurretYawLeftButton', tms)
    end
  end
end;

procedure TCustomEliteManager.TurretYawRight(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyTurretYawRightButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'BuggyTurretYawRightButton', tms)
    end
  end
end;

procedure TCustomEliteManager.TurretPitchUp(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyTurretPitchUpButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'BuggyTurretPitchUpButton', tms)
    end
  end
end;

procedure TCustomEliteManager.TurretPitchDown(tms: Integer);
begin
  case EliteStatus.InSrv of
    True : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'BuggyTurretPitchDownButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'BuggyTurretPitchDownButton', tms)
    end
  end
end;

procedure TCustomEliteManager.PropulsionReverse;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'BuggyToggleReverseThrottleInput', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.PropulsionAcceleration(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'IncreaseSpeedButtonMax', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'IncreaseSpeedButtonMax', tms)
end;

procedure TCustomEliteManager.PropulsionDeceleration(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'DecreaseSpeedButtonMax', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'DecreaseSpeedButtonMax', tms)
end;

procedure TCustomEliteManager.VRSCargoScoop;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'ToggleCargoScoop_Buggy', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.VRSCargoEject;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'EjectAllCargo_Buggy', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.ShipDismissRecall;
begin
  case EliteStatus.InSrv of
    True : FKeyInventory.KeyTrigger_( 'RecallDismissShip', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.RequestDock;
begin
  FKeyInventory.KeyTrigger_( 'OrderRequestDock', WITH_KEYUP)
end;

procedure TCustomEliteManager.DefensiveBehaviour;
begin
  FKeyInventory.KeyTrigger_( 'OrderDefensiveBehaviour', WITH_KEYUP)
end;

procedure TCustomEliteManager.AggressiveBehaviour;
begin
  FKeyInventory.KeyTrigger_( 'OrderAggressiveBehaviour', WITH_KEYUP)
end;

procedure TCustomEliteManager.FocusTarget;
begin
  FKeyInventory.KeyTrigger_( 'OrderFocusTarget', WITH_KEYUP)
end;

procedure TCustomEliteManager.HoldFire;
begin
  FKeyInventory.KeyTrigger_( 'OrderHoldFire', WITH_KEYUP)
end;

procedure TCustomEliteManager.HoldPosition;
begin
  FKeyInventory.KeyTrigger_( 'OrderHoldPosition', WITH_KEYUP)
end;

procedure TCustomEliteManager.Follow;
begin
  FKeyInventory.KeyTrigger_( 'OrderFollow', WITH_KEYUP)
end;

procedure TCustomEliteManager.OpenOrders;
begin
  FKeyInventory.KeyTrigger_( 'OpenOrders', WITH_KEYUP)
end;

procedure TCustomEliteManager.ACSCameraPitchInc(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchIncreaseButton', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchIncreaseButton', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSCameraPitchDec(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchDecreaseButton', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchDecreaseButton', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSCameraPitchDec;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchDecreaseButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSCameraPitchInc;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraPitchIncreaseButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSCameraYawInc(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawIncreaseButton', WITH_KEYUP)
      else FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawIncreaseButton', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSCameraYawDec(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawDecreaseButton', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawDecreaseButton', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSCameraYawDec;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawDecreaseButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSCameraYawInc;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSCameraYawIncreaseButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSZoomIn(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomIn', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomIn', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSZoomOut(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomOut', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomOut', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSZoomIn;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomIn', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSZoomInMini;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomIn', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSZoomInMini(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomIn', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomIn', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSZoomOut;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSZoomOut', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSZoomOutMini;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomOut', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSZoomOutMini(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomOut', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSMiniZoomOut', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSRadioInc(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Increase', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Increase', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSRadioDec(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Decrease', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Decrease', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSRadioDec;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Decrease', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSRadioInc;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSRadioTuningX_Increase', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.ACSAnalyse(tms: Integer);
begin
  case EliteStatus.GuiValue of
   gt_fssmode : begin
     if tms < 1 then FKeyInventory.KeyTrigger_( 'ExplorationFSSDiscoveryScan', WITH_KEYUP)
       else FKeyInventory.KeyTrigger_( 'ExplorationFSSDiscoveryScan', tms)
   end
  end
end;

procedure TCustomEliteManager.ACSClose;
begin
  case EliteStatus.GuiValue of
   gt_fssmode : FKeyInventory.KeyTrigger_( 'ExplorationFSSQuit', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.ACSGetTarget;
begin
  case EliteStatus.GuiValue of
   gt_fssmode : FKeyInventory.KeyTrigger_( 'ExplorationFSSTarget', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.ACSHelp;
begin
  FKeyInventory.KeyTrigger_( 'ExplorationFSSShowHelp', WITH_KEYUP)
end;

procedure TCustomEliteManager.DSDViewChange;
begin
  case EliteStatus.GuiValue of
    gt_saamode : FKeyInventory.KeyTrigger_( 'ExplorationSAAChangeScannedAreaViewToggle', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.DSDClose;
begin
  case EliteStatus.GuiValue of
    gt_saamode : FKeyInventory.KeyTrigger_( 'ExplorationSAAExitThirdPerson', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.DSDYawLeft(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawLeftButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawLeftButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDYawLeft;
begin
  FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawLeftButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.DSDYawRight;
begin
  FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawRightButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.DSDYawRight(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawRightButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonYawRightButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDPitchUp(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchUpButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchUpButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDPitchDown(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchDownButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchDownButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDPitchDown;
begin
  FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchDownButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.DSDPitchUp;
begin
  FKeyInventory.KeyTrigger_( 'SAAThirdPersonPitchUpButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.DSDZoomOut(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovOutButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovOutButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDZoomIn(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_saamode : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovInButton', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovInButton', tms)
    end
  end
end;

procedure TCustomEliteManager.DSDZoomIn;
begin
  FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovInButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.DSDZoomOut;
begin
  FKeyInventory.KeyTrigger_( 'SAAThirdPersonFovOutButton', WITHOUT_KEYUP )
end;

procedure TCustomEliteManager.CamPitchUp(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'CamPitchUp', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'CamPitchUp', tms)
    end
  end
end;

procedure TCustomEliteManager.CamPitchDown(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'CamPitchDown', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'CamPitchDown', tms)
    end
  end
end;

procedure TCustomEliteManager.CamPitchDown;
begin
  FKeyInventory.KeyTrigger_( 'CamPitchDown', WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.CamPitchUp;
begin
  FKeyInventory.KeyTrigger_( 'CamPitchUp', WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.CamYawLeft(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'CamYawLeft', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'CamYawLeft', tms)
    end
  end
end;

procedure TCustomEliteManager.CamYawLeft;
begin
  FKeyInventory.KeyTrigger_( 'CamYawLeft', WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.CamYawRight;
begin
  FKeyInventory.KeyTrigger_( 'CamYawRight', WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.CamYawRight(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'CamYawRight', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'CamYawRight', tms)
    end
  end
end;

procedure TCustomEliteManager.CamZoomIn(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'CamZoomIn', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'CamZoomIn', tms)
    end
  end
end;

procedure TCustomEliteManager.CamZoomIn;
begin
  FKeyInventory.KeyTrigger_( 'CamZoomIn', WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.CamZoomOut;
begin
  FKeyInventory.KeyTrigger_( 'CamZoomOut', WITHOUT_KEYUP)
end;

procedure TCustomEliteManager.CamZoomOut(tms: Integer);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : begin
      if tms < 1 then FKeyInventory.KeyTrigger_( 'CamZoomOut', WITH_KEYUP)
        else FKeyInventory.KeyTrigger_( 'CamZoomOut', tms)
    end
  end
end;

procedure TCustomEliteManager.GalaxyMapHome;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : FKeyInventory.KeyTrigger_( 'GalaxyMapHome', WITH_KEYUP)
  end
end;

procedure TCustomEliteManager.Camera_Next;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraScrollLeft', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Prior;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraScrollRight', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_One;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraOne', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Two;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraTwo', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Three;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraThree', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Four;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraFour', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Five;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraFive', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Six;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraSix', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Seven;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraSeven', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Eight;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraEight', WITH_KEYUP)
end;

procedure TCustomEliteManager.Camera_Nine;
begin
  FKeyInventory.KeyTrigger_( 'VanityCameraNine', WITH_KEYUP)
end;

procedure TCustomEliteManager.FreeCamera_Close;
begin
  FKeyInventory.KeyTrigger_( 'FreeCamToggleHUD', WITH_KEYUP)
end;

procedure TCustomEliteManager.LandingYawLeft(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'YawLeftButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'YawLeftButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingYawRight(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'YawRightButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'YawRightButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingPitchUp(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'PitchUpButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'PitchUpButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingPitchDown(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'PitchDownButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'PitchDownButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingRollLeft(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'RollLeftButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'RollLeftButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingRollRight(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'RollRightButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'RollRightButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingLeftThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'LeftThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'LeftThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingRightThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'RightThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'RightThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingUpThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'UpThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'UpThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingDownThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'DownThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'DownThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingForwardThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'ForwardThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'ForwardThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.LandingBackwardThrust(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'BackwardThrustButton_Landing', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'BackwardThrustButton_Landing', tms)
end;

procedure TCustomEliteManager.Crew_CokpitNext;
begin
  FKeyInventory.KeyTrigger_( 'MultiCrewCockpitUICycleForward', WITH_KEYUP)
end;

procedure TCustomEliteManager.Crew_SecondaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewSecondaryFire', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewSecondaryFire', tms)
end;

procedure TCustomEliteManager.Crew_ToolsSecondaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewSecondaryUtilityFire', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewSecondaryUtilityFire', tms)
end;

procedure TCustomEliteManager.Crew_CokpitPrior;
begin
  FKeyInventory.KeyTrigger_( 'MultiCrewCockpitUICycleBackward', WITH_KEYUP)
end;

procedure TCustomEliteManager.Crew_PitchDown(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonPitchDownButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonPitchDownButton', tms)
end;

procedure TCustomEliteManager.Crew_YawLeft(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonYawLeftButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonYawLeftButton', tms)
end;

procedure TCustomEliteManager.Crew_PitchUp(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonPitchUpButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonPitchUpButton', tms)
end;

procedure TCustomEliteManager.Crew_YawRight(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonYawRightButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonYawRightButton', tms)
end;

procedure TCustomEliteManager.Crew_ZoomOut(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonFovOutButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonFovOutButton', tms)
end;

procedure TCustomEliteManager.Crew_PrimaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewPrimaryFire', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewPrimaryFire', tms)
end;

procedure TCustomEliteManager.Crew_ToolsPrimaryFire(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewPrimaryUtilityFire', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewPrimaryUtilityFire', tms)
end;

procedure TCustomEliteManager.Crew_SwitchMode;
begin
  FKeyInventory.KeyTrigger_( 'MultiCrewToggleMode', WITH_KEYUP)
end;

procedure TCustomEliteManager.Crew_ZoomIn(tms: Integer);
begin
  if tms < 1 then FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonFovInButton', WITH_KEYUP)
    else FKeyInventory.KeyTrigger_( 'MultiCrewThirdPersonFovInButton', tms)
end;

procedure TCustomEliteManager.MainDezoom;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap     : CamZoomOut(200);
    gt_fssmode       : ACSZoomOut(100);
    gt_saamode       : DSDZoomOut(150);
  end
end;

procedure TCustomEliteManager.MainZoom;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_systemmap     : CamZoomIn(200);
    gt_fssmode       : ACSZoomIn(100);
    gt_saamode       : DSDZoomIn(150);
  end
end;

procedure TCustomEliteManager.SendChar(const Car: Char);
begin
  SendKey(Ord(Car), 150, []);
end;

procedure TCustomEliteManager.SendChar(VKCode: SmallInt);
begin
  keybd_event(VKCode, MapVirtualKey(Ord(VKCode), 0), 0, 0);
  WaitForKey( 150 );
  keybd_event(Ord(VKCode), MapVirtualKey(Ord(VKCode), 0), KEYEVENTF_KEYUP, 0);
end;

procedure TCustomEliteManager.AlphaKeyBoard(const Car: Char);
begin
  { --- Ne peut être simulé que si l'on se trouve dans :
         * La carte de la galaxie
         * le panneaau des amis (comment le retrouver ?)
         * le pannneau de communication}
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_commspanel : SendChar(Car);
  end;
end;

procedure TCustomEliteManager.AlphaKeyBoard(VKCode: SmallInt);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_commspanel : SendChar(VKCode);
  end;
end;

procedure TCustomEliteManager.AlphaKeyBoard(const Car: Char;
  Specials: TSpecials);
begin
  case EliteStatus.GuiValue of
    gt_galaxymap,
    gt_commspanel : SendKey(Ord(Car), 150, Specials);
  end;
end;

procedure TCustomEliteManager.Coller;
begin
  AlphaKeyBoard('V', [ss_lctrl])
end;

procedure TCustomEliteManager.CloseCarte;
begin
  case EliteStatus.GuiValue of
    gt_galaxymap, gt_systemmap : UIBack
  end
end;

{ TEliteManager }

class procedure TEliteManager.Finalize;
begin
  if Assigned(EliteManager) then FreeAndNil( EliteManager )
end;

class procedure TEliteManager.Initialize;
begin
  if not Assigned(EliteManager) then EliteManager := TEliteManager.Create;
end;

class procedure TEliteManager.KeyInventoryAssign(const Value: TKeyInventory);
begin
  if Assigned(EliteManager) then EliteManager.SetKeyInventory( Value );
end;

class procedure TEliteManager.TagAssign(const Value: string);
begin
  if Assigned(EliteManager) then EliteManager.SetTags( Value )
end;

{ TEliteRunningObserver }

function LIsEliteRunning: Boolean;
begin
  Result := FindWindow( PWideChar(ELITE_CLASS), nil ) <> 0;
end;

constructor TEliteRunningObserver.Create;
begin
  inherited Create( False );
  {Launch on create}
  FreeOnTerminate := True;
  Priority        := tpLower;
end;

procedure TEliteRunningObserver.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 500 );
  end;
end;

procedure TEliteRunningObserver.Process;
begin
  KeyWrite(AppKey, 'EliteLaunched', LIsEliteRunning);
end;

procedure TEliteRunningObserver.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

initialization
  { --- Initialize le traitement pour Elite}
  TEliteManager.Initialize;
  EliteRunningObserver := TEliteRunningObserver.Create;
finalization
  EliteRunningObserver.Terminate;
end.
