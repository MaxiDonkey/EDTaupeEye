{*******************************************************}
{                                                       }
{                MaxiDonkey  Library                    }
{                                                       }
{         Elite Dangerous Functions interface           }
{         Bindings tools for Custom.3.0.Binds           }
{                                                       }
{                08/2020 - copyleft                     }
{                                                       }
{*******************************************************}

{...............................................................................

   1/ FONCTION NON IMPLEMENTEES

   MouseReset                             YawToRollButton
   UseAlternateFlightValuesToggle         HMDReset
   MicrophoneMute                         UIFocus
   ShowPGScoreSummaryInput                HeadLookToggle
   UI_Toggle                              HeadLookReset
   HeadLookPitchUp                        HeadLookPitchDown
   HeadLookYawLeft                        HeadLookYawRight
   CamTranslateZHold                      UIFocus_Buggy
   HeadLookToggle_Buggy                   FreeCamToggleHUD
   FreeCamSpeedInc                        FreeCamSpeedDec
   ToggleReverseThrottleInputFreeCam      MoveFreeCamForward
   MoveFreeCamBackwards                   MoveFreeCamRight
   MoveFreeCamLeft                        MoveFreeCamUp
   MoveFreeCamDown                        PitchCameraUp
   PitchCameraDown                        YawCameraLeft
   YawCameraRight                         RollCameraLeft
   RollCameraRight                        ToggleRotationLock
   FixCameraRelativeToggle                FixCameraWorldToggle
   QuitCamera                             ToggleAdvanceMode
   FreeCamZoomIn                          FreeCamZoomOut
   FStopDec                               FStopInc
   StoreEnableRotation                    StoreCamZoomIn
   StoreCamZoomOut                        StoreToggle
   CommanderCreator_Undo                  CommanderCreator_Redo
   CommanderCreator_Rotation_MouseToggle


   2/ Display managed functions by code
    . KeyInventory.CatalogTxt : return a string with keys combination code
                                associated to the functions.

    Example : BackwardKey=165000335 for keys combination "Key_RightAlt + Key_O"
                VkKeyScan( Key_Right ) = 165
                VkKeyScan( Key_O     ) = 335

   3/ Call function by function name
    . KeyInventory.KeyTrigger_( function_name , WITH_KEYUP/WITHOUT_KEYUP)

    Example : KeyInventory.KeyTrigger_( PrimaryFire , WITH_KEYUP)

   4/ Instantiation
    . Create -->
        TKeyMessageSender.Initialize;

    . Show -->
        try
          TKeyInventory.Initialize;
        except
          on E:Exception do begin
            MessageDlg(E.Message, mtWarning, [mbOK], 0);
            Application.Terminate;
          end
        end;

   5/ Finalization
    . Destroy -->
        TKeyInventory.Finalize;
        TKeyMessageSender.Finalize;

   6/ Operating mechanism
    . Copy ...Elite...Options\Bindings\Custom.3.0.binds
      to   ...App..\Temp.Custom.3.0.binds
      Extract from xml file functions and keys combinations

      if changes are made to Temp.Custom.3.0.binds then a copy of this file is
      send to ...Elite...Options\Bindings\Custom.3.0.binds

...............................................................................}

unit EliteBindingsTools;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Math,
  {Spec}
  uRegistry, StrCopyUtils;

const
  KEYEVENTF_KEYDOWN  = 0;
  WITH_KEYUP         = True;
  WITHOUT_KEYUP      = False;
  SHELL_ACCES        = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';  //Registry key
  SAVE_GAMES_KEY     = '{4C5C32FF-BB9D-43B0-B5B4-2D72E54EAAA4}';                            //Registry key
  BINDING_OPT_KEY    = 'Local AppData';
  LOCAL_SAVE         = 'SaveBindigs';
  TEMP_CUSTOM_FILE   = 'Temp.Custom.3.0.binds';
  CUSTOM_FILE        = 'Custom.3.0.binds';
  START_PRESET       = 'StartPreset.start';
  DISPLAY_SETTINGS   = 'DisplaySettings.xml';

var
  SavedGamesW    : string = 'Frontier Developments\Elite Dangerous';
  BindingsFolder : string = 'Frontier Developments\Elite Dangerous\Options\Bindings';
  GraphicsFolder : string = 'Frontier Developments\Elite Dangerous\Options\Graphics';

function GetFrontierSaveGames: string;
function GetEliteBindingsFolder: string;
function GetEliteGraphicsFolder: string;

function EncodeKey(const Key, Mod1, Mod2: Word): Cardinal; overload;
function EncodeKey(const Key, Mod1, Mod2: string): Cardinal; overload;

const
  {$EXTERNALSYM MOUSEEVENTF_XDOWN}
  MOUSEEVENTF_XDOWN = $0080;
  {$EXTERNALSYM MOUSEEVENTF_XUP}
  MOUSEEVENTF_XUP   = $0100;
  XBUTTON1          = $0001;
  XBUTTON2          = $0002;

var
  ELITE_CLASS : string = 'FrontierDevelopmentsAppWinClass';

function ExtractBalises(const Src: string; Full: Boolean):string;

{*** VOL - ROTATION;  17-63}
type
  TVolRotationType =
   ( vrt_yawleft,   vrt_yawright,
     vrt_rollleft,  vrt_rollright,
     vrt_pitchleft, vrt_pitchright );
  TVolRotationArea = 0..Integer(High(TVolRotationType));
  TArrayVolRotation = array[TVolRotationArea] of string;

var
  VolRotation : TArrayVolRotation = (
    'YawLeftButton',  'YawRightButton',
    'RollLeftButton', 'RollRightButton',
    'PitchUpButton',  'PitchDownButton'
   );

function GetVolRotation(const Value: string):TVolRotationType;
function IsVolRotation(const Value: string):Boolean;


{*** VOL - POUSSEE; 64-115}
type
  TVolPousseType =
   ( vpt_thrustleft,    vpt_thrustright,
     vpt_thrustup,      vpt_thrustdown,
     vpt_thrustforward, vpt_thrustbackward );
  TVolPousseArea = 0..Integer(high(TVolPousseType));
  TArrayVolPousse = array[TVolPousseArea] of string;

var
  VolPoussee : TArrayVolPousse = (
    'LeftThrustButton',    'RightThrustButton',
    'UpThrustButton',      'DownThrustButton',
    'ForwardThrustButton', 'BackwardThrustButton'
   );

function GetVolPousee(const Value: string): TVolPousseType;
function IsVolPousee(const Value: string):Boolean;


{*** VOL - PROPULSION; 146-225}
type
  TVolPropulsionType =
   ( vlt_reverse,
     vlt_forward,     vlt_backward,
     vlt_speedmin100, vlt_speedmin75, vlt_speedmin50, vlt_speedmin25,
     vlt_speedzero,
     vlt_speed25,     vlt_speed50,    vlt_speed75,    vlt_speed100 );
  TVolPropulsionArea = 0..Integer(high(TVolPropulsionType));
  TArrayVolPropulsion = array[TVolPropulsionArea] of string;

var
  VolPropulsion : TArrayVolPropulsion =
   ( 'ToggleReverseThrottleInput',
     'ForwardKey',       'BackwardKey',
     'SetSpeedMinus100', 'SetSpeedMinus75', 'SetSpeedMinus50', 'SetSpeedMinus25',
     'SetSpeedZero',
     'SetSpeed25',       'SetSpeed50',      'SetSpeed75',      'SetSpeed100'
   );

function GetVolPropulsion(const Value: string): TVolPropulsionType;
function IsVolPropulsion(const Value: string):Boolean;


{*** VOL - DIVERS; 305-348}

type
  TVolDiversType =
   ( vdt_flightassist,     vdt_boost,
     vdt_hypersuper,       vdt_supercruise,  vdt_hyperspace,
     vdt_rotationcorrect,  vdt_orbitlines );
  TVolDiversArea = 0..Integer(high(TVolDiversType));
  TArrayVolDivers = array[TVolDiversArea] of string;

var
  VolDivers : TArrayVolDivers =
   ( 'ToggleFlightAssist',           'UseBoostJuice',
     'HyperSuperCombination',        'Supercruise',        'Hyperspace',
     'DisableRotationCorrectToggle', 'OrbitLinesToggle'
   );

function GetVolDivers(const Value: string): TVolDiversType;
function IsVolDivers(const Value: string):Boolean;


{*** VISEE; 349-432}
type
  TViseeType =
   ( vt_selecttarget,  vt_nexttarget,        vt_previoustarget,
     vt_highestthreat, vt_nexthostile,       vt_previoushostile,
     vt_wingman0,      vt_wingman1,          vt_wingman2,
     vt_wingmantarget, vt_navlock,
     vt_nextsubsystem, vt_prevuoissubsystem, vt_nextroute );
  TViseeArea = 0..Integer(high(TViseeType));
  TArrayVisee = array[TViseeArea] of string;

var
  Visee : TArrayVisee =
   ( 'SelectTarget',        'CycleNextTarget',        'CyclePreviousTarget',
     'SelectHighestThreat', 'CycleNextHostileTarget', 'CyclePreviousHostileTarget',
     'TargetWingman0',      'TargetWingman1',         'TargetWingman2',
     'SelectTargetsTarget', 'WingNavLock',
     'CycleNextSubsystem',  'CyclePreviousSubsystem', 'TargetNextRouteSystem'
   );

function GetVisee(const Value: string): TViseeType;
function IsVisee(const Value: string):Boolean;


{*** ARMES; 433-459}
type
  TArmesType =
   ( at_primary, at_secondary,
     at_groupnext, at_groupprevious, at_hardpoint );
  TArmesArea = 0..Integer(high(TArmesType));
  TArrayArmes = array[TArmesArea] of string;

var
  Armes : TArrayArmes =
   ( 'PrimaryFire',        'SecondaryFire',
     'CycleFireGroupNext', 'CycleFireGroupPrevious', 'DeployHardpointToggle'
   );

function GetArmes(const Value: string): TArmesType;
function IsArmes(const Value: string):Boolean;


{*** REFROIDISSEMENT; 460-472}
type
  TFreezeType = ( ft_stealthy, ft_heatsink );
  TFreezeArea = 0..Integer(high(TFreezeType));
  TArrayFreeze = array[TFreezeArea] of string;

var
  Freeze : TArrayFreeze =
   ( 'ToggleButtonUpInput', 'DeployHeatSink' );

function GetFreeze(const Value: string): TFreezeType;
function IsFreeze(const Value: string):Boolean;


{*** DIVERS; 473-590}
type
  TDiversType =
   ( dt_spotlight,   dt_increaserange, dt_decreaserange,
     dt_powerengine, dt_powerweapon,   dt_powersystem,   dt_powerreset,
     dt_cargoscoop,  dt_ejectcargo,    dt_landinggear,
     dt_shielcell,   dt_firechaft,     dt_chargeecm,
     dt_colorweapon, dt_colorengine,   dt_nightvision
   );
  TDiversArea = 0..Integer(high(TDiversType));
  TArrayDivers = array[TDiversArea] of string;

var
  Divers : TArrayDivers =
   ( 'ShipSpotLightToggle',  'RadarIncreaseRange',   'RadarDecreaseRange',
     'IncreaseEnginesPower', 'IncreaseWeaponsPower', 'IncreaseSystemsPower', 'ResetPowerDistribution',
     'ToggleCargoScoop',     'EjectAllCargo',        'LandingGearToggle',
     'UseShieldCell',        'FireChaffLauncher',    'ChargeECM',
     'WeaponColourToggle',   'EngineColourToggle',   'NightVisionToggle'
   );

function GetDivers(const Value: string): TDiversType;
function IsDivers(const Value: string):Boolean;


{*** CHANGEMENT DE MODE; 591-688}
type
  TModeChangetype =
   ( mct_panelleft,   mct_paneltop,        mct_quickcom,
     mct_panelbottom, mct_panelright,
     mct_galaxymap,   mct_systemmap,
     mct_pause,       mct_friend,
     mct_codex,
     mct_hudmode,     mct_explorationfss );
  TModeChangeArea = 0..Integer(high(TModeChangetype));
  TArrayModeChange = array[TModeChangeArea] of string;

var
  ModeChange : TArrayModeChange =
   ( 'FocusLeftPanel',         'FocusCommsPanel',     'QuickCommsPanel',
     'FocusRadarPanel',        'FocusRightPanel',
     'GalaxyMapOpen',          'SystemMapOpen',
     'Pause',                  'FriendsMenu',
     'OpenCodexGoToDiscovery',
     'PlayerHUDModeToggle',    'ExplorationFSSEnter'
   );

function GetModeChange(const Value: string): TModeChangetype;
function IsModeChange(const Value: string):Boolean;


{*** MODE TABLEAU DE BORD; 689-732}
type
  TModeBoardType =
   ( mb_up,   mb_down,      mb_left,          mb_right,    mb_select,
     mb_back, mb_nextpanel, mb_previouspanel, mb_nextpage, mb_previouspage
   );
  TModeBoardArea = 0..Integer(high(TModeBoardType));
  TArrayModeBoard = array[TModeBoardArea] of string;

var
  ModeBoard : TArrayModeBoard =
   ( 'UI_Up',   'UI_Down',        'UI_Left',            'UI_Right',      'UI_Select',
     'UI_Back', 'CycleNextPanel', 'CyclePreviousPanel', 'CycleNextPage', 'CyclePreviousPage'
   );

function GetModeBoard(const Value: string): TModeBoardType;
function IsModeBoard(const Value: string):Boolean;


{*** CONDUITE; 862-968}
type
  TConduiteType =
   ( ct_driveassist, ct_steerleft, ct_steerright,       ct_rollleft,    ct_rollright,
     ct_pichup,      ct_pichdown,  ct_verticalthruster, ct_fireprimary, ct_firesecondary,
     ct_autobreak,   ct_vrslight,  ct_turret,           ct_groupnext,   ct_groupprevious );
  TConduiteArea = 0..Integer(high(TConduiteType));
  TArrayConduite = array[TConduiteArea] of string;

var
  Conduite : TArrayConduite =
   ( 'ToggleDriveAssist',    'SteerLeftButton',       'SteerRightButton',        'BuggyRollLeftButton',     'BuggyRollRightButton',
     'BuggyPitchUpButton',   'BuggyPitchDownButton',  'VerticalThrustersButton', 'BuggyPrimaryFireButton',  'BuggySecondaryFireButton',
     'AutoBreakBuggyButton', 'HeadlightsBuggyButton', 'ToggleBuggyTurretButton', 'BuggyCycleFireGroupNext', 'BuggyCycleFireGroupPrevious'
   );

function GetConduite(const Value: string): TConduiteType;
function IsConduite(const Value: string):Boolean;


{*** CONDUITE VISEEE; 969-974}
type
  TConduiteViseeType = ( cvt_buggyselecttarget );
  TConduiteViseeArea = 0..Integer(high(TConduiteViseeType));
  TArrayConduiteVisee = array[TConduiteViseeArea] of string;

var
  ConduiteVisee : TArrayConduiteVisee =
   ( 'SelectTarget_Buggy' );

function GetConduiteVisee(const Value: string): TConduiteViseeType;
function IsConduiteVisee(const Value: string):Boolean;


{*** CONDUITE TOURELLE; 975-1019}
type
  TConduiteTurretType =
   ( ctt_turretyawleft, ctt_turretyawright,
     ctt_turretpichup,  ctt_turretpichdown );
  TConduiteTurretArea = 0..Integer(high(TConduiteTurretType));
  TArrayConduiteTurret = array[TConduiteTurretArea] of string;

var
  ConduiteTurret : TArrayConduiteTurret =
   ( 'BuggyTurretYawLeftButton', 'BuggyTurretYawRightButton',
     'BuggyTurretPitchUpButton', 'BuggyTurretPitchDownButton'
   );

function GetConduiteTurret(const Value: string): TConduiteTurretType;
function IsConduiteTurret(const Value: string):Boolean;


{*** CONDUITE PROPULSION; 1020-1055}
type
  TConduitePropulsionType =
   ( cpt_buggyreverse, cpt_increasespeed, cpt_decreasespeed );
  TConduitePropulsionArea = 0..Integer(high(TConduitePropulsionType));
  TArrayConduitePropulsion = array[TConduitePropulsionArea] of string;

var
  ConduitePropulsion : TArrayConduitePropulsion =
   ( 'BuggyToggleReverseThrottleInput',
     'IncreaseSpeedButtonMax',
     'DecreaseSpeedButtonMax'
   );

function GetConduitePropulsion(const Value: string): TConduitePropulsionType;
function IsConduitePropulsion(const Value: string):Boolean;


{*** CONDUITE DIVERS; 1056-1099}
type
  TConduiteDiversType =
   ( cdt_buggypowerengine, cdt_buggypowerweapons,
     cdt_buggypowersystem, cdt_buggypowerreset,
     cdt_buggycargoscoop,  cdt_buggyejectcargo,
     cdt_buggyrecallship );
  TConduiteDiversArea = 0..Integer(high(TConduiteDiversType));
  TArrayConduiteDivers = array[TConduiteDiversArea] of string;

var
  ConduiteDivers : TArrayConduiteDivers =
   ( 'IncreaseEnginesPower_Buggy', 'IncreaseWeaponsPower_Buggy',
     'IncreaseSystemsPower_Buggy', 'ResetPowerDistribution_Buggy',
     'ToggleCargoScoop_Buggy',     'EjectAllCargo_Buggy',
     'RecallDismissShip'
   );

function GetConduiteDivers(const Value: string): TConduiteDiversType;
function IsConduiteDivers(const Value: string):Boolean;


{*** CONDUITE MODES; 1100-1165}
type
  TConduiteModeType =
   ( cmt_panelleft,   cmt_panelTop,
     cmt_quickcom,
     cmt_panelbottom, cmt_panelright,
     cmt_galaxymap,   cmt_systemmap,
     cmt_codex,
     cmt_hudmode );
  TConduiteModeArea = 0..Integer(high(TConduiteModeType));
  TArrayConduiteMode = array[TConduiteModeArea] of string;

var
  ConduiteMode : TArrayConduiteMode =
   ( 'FocusLeftPanel_Buggy',         'FocusCommsPanel_Buggy',
     'QuickCommsPanel_Buggy',
     'FocusRadarPanel_Buggy',        'FocusRightPanel_Buggy',
     'GalaxyMapOpen_Buggy',          'SystemMapOpen_Buggy',
     'OpenCodexGoToDiscovery_Buggy',
     'PlayerHUDModeToggle_Buggy'
   );

function GetConduiteMode(const Value: string): TConduiteModeType;
function IsConduiteMode(const Value: string):Boolean;


{*** ORDRES AU CHASSEUR; 1238-1290}
type
  TChasseurOrderType =
   ( cot_requestdock,
     cot_defensivebehaviour, cot_aggressivebehaviour,
     cot_focustarget,        cot_holdfire,
     cot_holdpositon,
     cot_follow,             cot_openorders );
  TChasseurOrderArea = 0..Integer(high(TChasseurOrderType));
  TArrayChasseurOrder = array[TChasseurOrderArea] of string;

var
  ChasseurOrder : TArrayChasseurOrder =
   ( 'OrderRequestDock',
     'OrderDefensiveBehaviour', 'OrderAggressiveBehaviour',
     'OrderFocusTarget',        'OrderHoldFire',
     'OrderHoldPosition',
     'OrderFollow',             'OpenOrders'
   );

function GetChasseurOrder(const Value: string): TChasseurOrderType;
function IsChasseurOrder(const Value: string):Boolean;


{*** DETECTEUR D'ANALYSE COMPLETE DU SYSTEME; 1561-1659}
type
  TExplorationFssType =
    ( eft_Campitchinc,   eft_Campitchdec,
      eft_Camyawinc,     eft_Camyawdec,
      eft_zoomin,        eft_zoomout,
      eft_minizoomin,    eft_minizoomout,
      eft_radioinc,      eft_radiodec,

      eft_discoveryscan, eft_quit,
      eft_targetsibling, eft_help );
  TExplorationFssArea = 0..Integer(high(TExplorationFssType));
  TArrayExplorationFss = array[TExplorationFssArea] of string;

var
  ExplorationFss : TArrayExplorationFss =
   ( 'ExplorationFSSCameraPitchIncreaseButton', 'ExplorationFSSCameraPitchDecreaseButton',
     'ExplorationFSSCameraYawIncreaseButton',   'ExplorationFSSCameraYawDecreaseButton',
     'ExplorationFSSZoomIn',                    'ExplorationFSSZoomOut',
     'ExplorationFSSMiniZoomIn',                'ExplorationFSSMiniZoomOut',
     'ExplorationFSSRadioTuningX_Increase',     'ExplorationFSSRadioTuningX_Decrease',

     'ExplorationFSSDiscoveryScan',             'ExplorationFSSQuit',
     'ExplorationFSSTarget',                    'ExplorationFSSShowHelp'
   );

function GetExplorationFss(const Value: string): TExplorationFssType;
function IsExplorationFss(const Value: string):Boolean;


{*** DETECTEUR DE SURFACE DETAILLEE; 1660-1714}
type
  TSurfaceDetailleeType =
   ( sdt_areaview,  sdt_exit,
     sdt_yawleft,   sdt_yawright,
     sdt_pitchleft, sdt_pitchright,
     sdt_fovout,    sdt_fovin );
  TSurfaceDetailleeArea = 0..Integer(high(TSurfaceDetailleeType));
  TArraySurfaceDetaillee = array[TSurfaceDetailleeArea] of string;

var
  SurfaceDetaillee : TArraySurfaceDetaillee =
   ( 'ExplorationSAAChangeScannedAreaViewToggle', 'ExplorationSAAExitThirdPerson',
     'SAAThirdPersonYawLeftButton',               'SAAThirdPersonYawRightButton',
     'SAAThirdPersonPitchUpButton',               'SAAThirdPersonPitchDownButton',
     'SAAThirdPersonFovOutButton',                'SAAThirdPersonFovInButton'
   );

function GetSurfaceDetaillee(const Value: string): TSurfaceDetailleeType;
function IsSurfaceDetaillee(const Value: string):Boolean;


{*** VOL ATTERRISSAGE MANUEL 226-305}
type
  TManualLandingType =
   ( mlt_yawleft,   mlt_yawright,     mlt_pitchup,       mlt_pitdown,
     mlt_rollleft,  mlt_rollright,    mlt_thrustleft,    mlt_thrustright,
     mlt_thrustup,  mlt_thrustdown,   mlt_thrustforward, mlt_thrustbackward );
  TManualLandingArea = 0..Integer(high(TManualLandingType));
  TArrayManualLanding = array[TManualLandingArea] of string;

var
  ManualLanding : TArrayManualLanding =
   ( 'YawLeftButton_Landing',       'YawRightButton_Landing',
     'PitchUpButton_Landing',       'PitchDownButton_Landing',
     'RollLeftButton_Landing',      'RollRightButton_Landing',
     'LeftThrustButton_Landing',    'RightThrustButton_Landing',
     'UpThrustButton_Landing',      'DownThrustButton_Landing',
     'ForwardThrustButton_Landing', 'BackwardThrustButton_Landing'
   );

function GetManualLanding(const Value: string): TManualLandingType;
function IsManualLanding(const Value: string):Boolean;


{*** EQUIPAGE MULTIPLE 1186-1257}
type
  TMultiCrewType =
   ( mcr_togglemode,               mcr_primaryfire,          mcr_secondaryfire,
     mcr_primaryutility,           mcr_secondaryutility,     mcr_tpyawleft,
     mcr_tpyawright,               mcr_tppitchup,            mcr_tppitchdown,
     mcr_tpfovout,                 mcr_tpfovin,              mcr_cockpituiforward,
     mcr_cockpituibackward );
  TMultiCrewArea = 0..Integer(high(TMultiCrewType));
  TArrayMultiCrew = array[TMultiCrewArea] of string;

var
  MultiCrew : TArrayMultiCrew =
   ( 'MultiCrewToggleMode',                 'MultiCrewPrimaryFire',
     'MultiCrewSecondaryFire',              'MultiCrewPrimaryUtilityFire',
     'MultiCrewSecondaryUtilityFire',       'MultiCrewThirdPersonYawLeftButton',
     'MultiCrewThirdPersonYawRightButton',  'MultiCrewThirdPersonPitchUpButton',
     'MultiCrewThirdPersonPitchDownButton', 'MultiCrewThirdPersonFovOutButton',
     'MultiCrewThirdPersonFovInButton',     'MultiCrewCockpitUICycleForward',
     'MultiCrewCockpitUICycleBackward'
   );

function GetMultiCrew(const Value: string): TMultiCrewType;
function IsMultiCrew(const Value: string):Boolean;


{*** CARTE DE LA GALAXIE 781-867}
type
  TGalaxyMapType =
   ( gmt_campitchup,          gmt_campitchdown,              gmt_camyawleft,
     gmt_camyawright,         gmt_translateforward,          gmt_translatebackward,
     gmt_translateleft,       gmt_translateright,            gmt_translateup,
     gmt_translatedown,       gmt_camzoomin,                 gmt_camzoomout,
     gmt_galawymaphome );
  TGalaxyMapArea = 0..Integer(high(TGalaxyMapType));
  TArrayGalaxyMap = array[TGalaxyMapArea] of string;

var
  GalaxyMap : TArrayGalaxyMap =
   ( 'CamPitchUp',          'CamPitchDown',
     'CamYawLeft',          'CamYawRight',
     'CamTranslateForward', 'CamTranslateBackward',
     'CamTranslateLeft',    'CamTranslateRight',
     'CamTranslateUp',      'CamTranslateDown',
     'CamZoomIn',           'CamZoomOut',
     'GalaxyMapHome'
   );

function GetGalaxyMap(const Value: string): TGalaxyMapType;
function IsGalaxyMap(const Value: string):Boolean;


{*** SYSTÈME DE CAMERAS 1311-1370}
type
  TCamSystemType =
   ( cst_shipview,       cst_crsview,     cst_scrollleft,     cst_scrollright,
     cst_freecam,        cst_camone,      cst_camtwo,         cst_camthree,
     cst_camfour,        cst_camfive,     cst_camsix,         cst_camseven,
     cst_cameight,       cst_camnine );
  TCamSystemArea = 0..Integer(high(TCamSystemType));
  TArrayCamSystem = array[TCamSystemArea] of string;

var
  CamSystem : TArrayCamSystem =
   ( 'PhotoCameraToggle',      'PhotoCameraToggle_Buggy',
     'VanityCameraScrollLeft', 'VanityCameraScrollRight',
     'ToggleFreeCam',          'VanityCameraOne',
     'VanityCameraTwo',        'VanityCameraThree',
     'VanityCameraFour',       'VanityCameraFive',
     'VanityCameraSix',        'VanityCameraSeven',
     'VanityCameraEight',      'VanityCameraNine'
   );

function GetCamSystem(const Value: string): TCamSystemType;
function IsCamSystem(const Value: string):Boolean;

{*** Liste de lecture 1564-1580}
type
  TGalnetReaderType =
   ( grt_playpause,  grt_skipforward,  grt_skipbackward,   grt_clearqueue );
  TGalnetReaderArea = 0..Integer(high(TGalnetReaderType));
  TArrayGalnetReader = array[TGalnetReaderArea] of string;

var
  GalnetReader : TArrayGalnetReader =
   ( 'GalnetAudio_Play_Pause',   'GalnetAudio_SkipForward',
     'GalnetAudio_SkipBackward', 'GalnetAudio_ClearQueue'
   );

function GetGalnetReader(const Value: string): TGalnetReaderType;
function IsGalnetReader(const Value: string):Boolean;

{*** CAMERA LIBRE 1401-…}
type
  TCamFreeType = (ToggleHud);
  TCamFreeArea = 0..Integer(high(TCamFreeType));
  TArrayCamFree = array[TCamFreeArea] of string;

var
  CamFree : TArrayCamFree = ('FreeCamToggleHUD');

function GetCamFree(const Value: string): TCamFreeType;
function IsCamFree(const Value: string):Boolean;

{*** Managed categories resumed }
type
  TBindCategoriesType =
    ( bct_volrotation,       bct_volpousee,          bct_volpropulsion,
      bct_voldivers,         bct_visee,              bct_armes,
      bct_freeze,            bct_divers,             bct_modechange,
      bct_modeboard,         bct_conduite,           bct_conduitevisee,
      bct_conduiteturret,    bct_conduitepropulsion, bct_conduitedivers,
      bct_conduitemode,      bct_chasseurorder,      bct_explorationfss,
      bct_surfacedetaillee,  bct_manuallanding,      bct_multicrew,
      bct_galaxymap,         bct_camsystem,          bct_galnetreader,
      bct_camfree );
  TBindCategoriesArea    = 0..Integer(high(TBindCategoriesType));
  TArrayOfBindCategories = array[TBindCategoriesArea] of string;

var
  MCR : TArrayOfBindCategories =
   ( 'Vol rotation',         'Vol poussée',          'Vol propulsion',
     'Vol divers',           'Visée',                'Armes',
     'Refroisissement',      'Divers',               'Changement de mode',
     'Tableau de bord',      'Conduite',             'Conduite visée',
     'Conduite tourelle',    'Conduite propulsion',  'Conduite divers',
     'Conduite modes',       'Ordres au chasseur',   'ACS',
     'Détecteur de surface', 'Atterrissage manuel',  'Equipage multiple',
     'Carte de la galaxie',  'Système de caméras',   'Liste de lecture',
     'Camera libre'
   );

function GetBindCategories(const Value: string): TBindCategoriesType;
function IsBindCategories(const Value: string):Boolean;


{*** Elite keys dedined }
type
  TEliteKeyType =
   ( Key_A,                Key_B,                  Key_C,                 Key_D,
     Key_E,                Key_F,                  Key_G,                 Key_H,
     Key_I,                Key_J,                  Key_K,                 Key_L,
     Key_M,                Key_N,                  Key_O,                 Key_P,
     Key_Q,                Key_R,                  Key_S,                 Key_T,
     Key_U,                Key_V,                  Key_W,                 Key_X,
     Key_Y,                Key_Z,                  Key_Space,             Key_F1,
     Key_F2,               Key_F3,                 Key_F4,                Key_F5,
     Key_F6,               Key_F7,                 Key_F8,                Key_F9,
     Key_F10,              Key_F11,                Key_F12,               Key_Enter,
     Key_RightArrow,       Key_DownArrow,          Key_LeftArrow,         Key_UpArrow,
     Key_Insert,           Key_Delete,             Key_Home,              Key_End,
     Key_PageUp,           Key_PageDown,           Key_ScrollLock,        Key_Pause,
     Key_Numpad_0,         Key_Numpad_1,           Key_Numpad_2,          Key_Numpad_3,
     Key_Numpad_4,         Key_Numpad_5,           Key_Numpad_6,          Key_Numpad_7,
     Key_Numpad_8,         Key_Numpad_9,           Key_NumLock,           Key_Numpad_Divide,
     Key_Numpad_Multiply,  Key_Numpad_Subtract,    Key_Numpad_Add,        Key_Numpad_Enter,
     Key_Numpad_Decimal,   Key_CapsLock,           Mouse_1,               Mouse_2,
     Mouse_3,              Mouse_4,                Mouse_5,               Key_Tab,
     Key_SuperscriptTwo,   Key_Ampersand,          Key_é,                 Key_DoubleQuote,
     Key_Apostrophe,       Key_LeftParenthesis,    Key_Minus,             Key_è,
     Key_Underline,        Key_ç,                  Key_à,                 Key_RightParenthesis,
     Key_Equals,           Key_Comma,              Key_SemiColon,         Key_Colon,
     Key_ExclamationPoint, Key_ù,                  Key_Asterisk,          Key_Circumflex,
     Key_Dollar,           Key_LessThan,           Key_Apps,              Key_Backspace,
     Key_Echap,            Key_LeftAlt,            Key_RightAlt,          Key_LeftControl,
     Key_RightControl,     Key_LeftShift,          Key_RightShift );

  TEliteKeysArea = 0..Integer(high(TEliteKeyType));
  TArrayEliteKey = array[TEliteKeysArea] of string;

var
  ArrayEliteKey : TArrayEliteKey =
   ('Key_A',               'Key_B',                'Key_C',               'Key_D',
    'Key_E',               'Key_F',                'Key_G',               'Key_H',
    'Key_I',               'Key_J',                'Key_K',               'Key_L',
    'Key_M',               'Key_N',                'Key_O',               'Key_P',
    'Key_Q',               'Key_R',                'Key_S',               'Key_T',
    'Key_U',               'Key_V',                'Key_W',               'Key_X',
    'Key_Y',               'Key_Z',                'Key_Space',           'Key_F1',
    'Key_F2',              'Key_F3',               'Key_F4',              'Key_F5',
    'Key_F6',              'Key_F7',               'Key_F8',              'Key_F9',
    'Key_F10',             'Key_F11',              'Key_F12',             'Key_Enter',
    'Key_RightArrow',      'Key_DownArrow',        'Key_LeftArrow',       'Key_UpArrow',
    'Key_Insert',          'Key_Delete',           'Key_Home',            'Key_End',
    'Key_PageUp',          'Key_PageDown',         'Key_ScrollLock',      'Key_Pause',
    'Key_Numpad_0',        'Key_Numpad_1',         'Key_Numpad_2',        'Key_Numpad_3',
    'Key_Numpad_4',        'Key_Numpad_5',         'Key_Numpad_6',        'Key_Numpad_7',
    'Key_Numpad_8',        'Key_Numpad_9',         'Key_NumLock',         'Key_Numpad_Divide',
    'Key_Numpad_Multiply', 'Key_Numpad_Subtract',  'Key_Numpad_Add',      'Key_Numpad_Enter',
    'Key_Numpad_Decimal',  'Key_CapsLock',         'Mouse_1',             'Mouse_2',
    'Mouse_3',             'Mouse_4',              'Mouse_5',             'Key_Tab',
    'Key_SuperscriptTwo',  'Key_Ampersand',        'Key_é',               'Key_DoubleQuote',
    'Key_Apostrophe',      'Key_LeftParenthesis',  'Key_Minus',           'Key_è',
    'Key_Underline',       'Key_ç',                'Key_à',               'Key_RightParenthesis',
    'Key_Equals',          'Key_Comma',            'Key_SemiColon',       'Key_Colon',
    'Key_ExclamationPoint','Key_ù',                'Key_Asterisk',        'Key_Circumflex',
    'Key_Dollar',          'Key_LessThan',         'Key_Apps',            'Key_Backspace',
    'Key_Echap',           'Key_LeftAlt',          'Key_RightAlt',        'Key_LeftControl',
    'Key_RightControl',    'Key_LeftShift',        'Key_RightShift'
   );

function GetEliteKey(const Value: string): TEliteKeyType;
function IsEliteKey(const Value: string):Boolean;
function EliteKeyToScanValue(const Value: string):SmallInt; overload;
function EliteKeyToScanValue(const Value: TEliteKeyType):SmallInt; overload;
function KeyScanToEliteString(const Value: SmallInt):string;
function GetKeyNext(const Value:TEliteKeyType):TEliteKeyType;
function GetKeyFuncNext(const Value:TEliteKeyType):TEliteKeyType;
function KeyTypeToStr(const Value:TEliteKeyType): string;
function IdKeyToStr(const IdKey: Cardinal):string;


{*** Duplicatas pour un même signal de combinaison de touches }
type
  TKeyDuplicataType =
   ( kdt_PrimaryFire,          kdt_SecondaryFire,               kdt_UI_Select,
     kdt_UI_Down,              kdt_UI_Left,                     kdt_UI_Back,
     kdt_UI_Right,             kdt_UI_Up,                       kdt_RollRight,
     kdt_RollLeft,             kdt_OpenCodexGoToDiscovery,      kdt_PlayerHUDModeToggle,
     kdt_IncreaseWeaponsPower, kdt_ResetPowerDistribution,      kdt_CycleFireGroupPrevious,
     kdt_IncreaseEnginesPower, kdt_IncreaseSystemsPower,        kdt_CycleFireGroupNext,
     kdt_LeftThrust,           kdt_ToggleCargoScoop,            kdt_ShipSpotLightToggle,
     kdt_EjectAllCargo,        kdt_SelectTarget,                kdt_FocusRightPanel,
     kdt_GalaxyMapOpen,        kdt_SystemMapOpen,               kdt_ToggleReverseThrottleInput,
     kdt_FocusLeftPanel,       kdt_FocusCommsPanel,             kdt_FocusRadarPanel,
     kdt_PitchUp,              kdt_PitchDown,                   kdt_YawRight,
     kdt_YawLeft );
  TKeyDuplicataArea = 0..Integer(high(TKeyDuplicataType));
  TArrayDuplicata   = array[1..9] of string;
  TDuplicata        = array[TKeyDuplicataType] of TArrayDuplicata;
  TPreferenceKeys   = array[TKeyDuplicataType] of Cardinal;

const
  DPrimaryFire : TArrayDuplicata = ('PrimaryFire', 'BuggyPrimaryFireButton', 'MultiCrewPrimaryFire', '', '', '', '', '', ''); //ExplorationFSSDiscoveryScan
  DSecondaryFire : TArrayDuplicata = ('SecondaryFire', 'BuggySecondaryFireButton', 'MultiCrewSecondaryFire', '', '', '', '', '', '');
  DUI_Select : TArrayDuplicata = ('UI_Select', 'ExplorationFSSTarget', '', '', '', '', '', '', '');
  DUI_Down : TArrayDuplicata = ('UI_Down', 'ExplorationFSSMiniZoomIn', '', '', '', '', '', '', '');
  DUI_Left : TArrayDuplicata = ('UI_Left', '', '', '', '', '', '', '', '');
  DUI_Back : TArrayDuplicata = ('UI_Back', 'ExplorationFSSQuit', '', '', '', '', '', '', '');
  DUI_Right : TArrayDuplicata = ('UI_Right', 'ExplorationFSSZoomOut', '', '', '', '', '', '', '');
  DUI_Up : TArrayDuplicata = ('UI_Up', 'ExplorationFSSZoomIn', '', '', '', '', '', '', '');
  DRollRight : TArrayDuplicata = ('RollRightButton', 'BackwardThrustButton_Landing', 'ExplorationFSSRadioTuningX_Increase',
    'SAAThirdPersonFovInButton', 'ToggleBuggyTurretButton', 'CamTranslateBackward', '', '', '');
  DRollLeft : TArrayDuplicata = ('RollLeftButton', 'AutoBreakBuggyButton', 'ExplorationFSSRadioTuningX_Decrease',
    'ForwardThrustButton_Landing', 'SAAThirdPersonFovOutButton', 'CamTranslateForward', '', '', '');
  DOpenCodexGoToDiscovery : TArrayDuplicata = ('OpenCodexGoToDiscovery', 'OpenCodexGoToDiscovery_Buggy', '', '', '', '', '', '', '');
  DPlayerHUDModeToggle : TArrayDuplicata = ('PlayerHUDModeToggle', 'PlayerHUDModeToggle_Buggy', '', '', '', '', '', '', '');
  DIncreaseWeaponsPower : TArrayDuplicata = ('IncreaseWeaponsPower', 'IncreaseWeaponsPower_Buggy', '', '', '', '', '', '', '');
  DResetPowerDistribution : TArrayDuplicata = ('ResetPowerDistribution', 'ResetPowerDistribution_Buggy', '', '', '', '', '', '', '');
  DCycleFireGroupPrevious : TArrayDuplicata = ('CycleFireGroupPrevious', 'BuggyCycleFireGroupPrevious', '', '', '', '', '', '', '');
  DIncreaseEnginesPower : TArrayDuplicata = ('IncreaseEnginesPower', 'IncreaseEnginesPower_Buggy', '', '', '', '', '', '', '');
  DIncreaseSystemsPower : TArrayDuplicata = ('IncreaseSystemsPower', 'IncreaseSystemsPower_Buggy', '', '', '', '', '', '', '');
  DCycleFireGroupNext : TArrayDuplicata = ('CycleFireGroupNext', 'BuggyCycleFireGroupNext', '', '', '', '', '', '', '');
  DLeftThrust : TArrayDuplicata = ('LeftThrustButton', 'OrderHoldPosition', '', '', '', '', '', '', '');
  DToggleCargoScoop : TArrayDuplicata = ('ToggleCargoScoop', 'ToggleCargoScoop_Buggy', '', '', '', '', '', '', '');
  DShipSpotLightToggle : TArrayDuplicata = ('ShipSpotLightToggle', 'HeadlightsBuggyButton', '', '', '', '', '', '', '');
  DEjectAllCargo : TArrayDuplicata = ('EjectAllCargo', 'EjectAllCargo_Buggy', '', '', '', '', '', '', '');
  DSelectTarget : TArrayDuplicata = ('SelectTarget', 'SelectTarget_Buggy', '', '', '', '', '', '', '');
  DFocusRightPanel : TArrayDuplicata = ('FocusRightPanel', 'FocusRightPanel_Buggy', '', '', '', '', '', '', '');
  DGalaxyMapOpen : TArrayDuplicata = ('GalaxyMapOpen', 'GalaxyMapOpen_Buggy', '', '', '', '', '', '', '');
  DSystemMapOpen : TArrayDuplicata = ('SystemMapOpen', 'SystemMapOpen_Buggy', '', '', '', '', '', '', '');
  DToggleReverseThrottleInput : TArrayDuplicata = ('ToggleReverseThrottleInput', 'BuggyToggleReverseThrottleInput', '', '', '', '', '', '', '');
  DFocusLeftPanel : TArrayDuplicata = ('FocusLeftPanel', 'FocusLeftPanel_Buggy', '', '', '', '', '', '', '');
  DFocusCommsPanel : TArrayDuplicata = ('FocusCommsPanel', 'FocusCommsPanel_Buggy', '', '', '', '', '', '', '');
  DFocusRadarPanel : TArrayDuplicata = ('FocusRadarPanel', 'FocusRadarPanel_Buggy', '', '', '', '', '', '', '');
  DPitchUp : TArrayDuplicata = ('PitchUpButton', 'BuggyTurretPitchUpButton', 'ExplorationFSSCameraPitchDecreaseButton',
    'IncreaseSpeedButtonMax', 'MultiCrewThirdPersonPitchUpButton', 'SAAThirdPersonPitchUpButton', 'UpThrustButton_Landing', 'CamTranslateUp', '');
  DPitchDown : TArrayDuplicata = ('PitchDownButton', 'BuggyTurretPitchDownButton', 'DecreaseSpeedButtonMax',
    'DownThrustButton_Landing', 'ExplorationFSSCameraPitchIncreaseButton', 'MultiCrewThirdPersonPitchDownButton', 'SAAThirdPersonPitchDownButton', 'CamTranslateDown', '');
  DYawRight : TArrayDuplicata = ('YawRightButton', 'BuggyTurretYawRightButton', 'ExplorationFSSCameraYawIncreaseButton',
    'MultiCrewThirdPersonYawRightButton', 'RightThrustButton_Landing', 'SAAThirdPersonYawRightButton', 'SteerRightButton', 'CamTranslateRight', '');
  DYawLeft : TArrayDuplicata = ('YawLeftButton', 'BuggyTurretYawLeftButton', 'ExplorationFSSCameraYawDecreaseButton',
    'LeftThrustButton_Landing', 'MultiCrewThirdPersonYawLeftButton', 'SAAThirdPersonYawLeftButton', 'SteerLeftButton', 'CamTranslateLeft', '');

var
  Duplicatas     : TDuplicata;
  PreferenceKeys : TPreferenceKeys;

procedure DuplicatasInitialize;
procedure PreferenceKeysInitialize;
function  DuplicataItemsCount(const Value: TKeyDuplicataType):Integer;
function  DuplicataItem(const Value: TKeyDuplicataType; index: Integer): string;

{*** Resumed detailled bindings }
type
  TBindsArrayTools = class
  private
    FSource                  : string;
    FVolRotation             : TArrayVolRotation;
    FVolPousse               : TArrayVolPousse;
    FVolPropulsion           : TArrayVolPropulsion;
    FVolDivers               : TArrayVolDivers;
    FVisee                   : TArrayVisee;
    FArmes                   : TArrayArmes;
    FFreeze                  : TArrayFreeze;
    FDivers                  : TArrayDivers;
    FModeChange              : TArrayModeChange;
    FModeBoard               : TArrayModeBoard;
    FConduite                : TArrayConduite;
    FConduiteVisee           : TArrayConduiteVisee;
    FConduiteTurret          : TArrayConduiteTurret;
    FConduitePropulsion      : TArrayConduitePropulsion;
    FConduiteDivers          : TArrayConduiteDivers;
    FConduiteMode            : TArrayConduiteMode;
    FChasseurOrder           : TArrayChasseurOrder;
    FExplorationFss          : TArrayExplorationFss;
    FSurfaceDetaillee        : TArraySurfaceDetaillee;
    FManualLanding           : TArrayManualLanding;
    FMultiCrew               : TArrayMultiCrew;
    FGalaxyMap               : TArrayGalaxyMap;
    FCamSystem               : TArrayCamSystem;
    FGalnetReader            : TArrayGalnetReader;
    FCamFree                 : TArrayCamFree;
    function ToStringEx(const AValues: array of string): string;
    function GetBindsXMLtext: string;
    function ExtractXMLByBind(const BindValue: string):string;
  public
    procedure SetSource(const Value: string);
    function  BindsByCategory_(const Category: TBindCategoriesType):string;

    property BindsXMLtext_: string read GetBindsXMLtext;

    constructor Create;
    class function BindsXMLtext: string;
    class function BindsByCategory(const Category: TBindCategoriesType):string;
    class function Extract(const ASource, BindValue: string):string;
    class function AllButtonBalises(const ASource: string; Full: Boolean):string;
  end;





type
  TRecordKey = packed record
    Device : string;
    Key    : string;
  end;

  TRecordKeySection = packed record
    Count     : Byte;         // --- 0..3
    MainKey   : TRecordKey;
    Modifier1 : TRecordKey;
    Modifier2 : TRecordKey;
  end;

  TRecordKeySignal = packed record
    Mouse     : Boolean;
    Key       : SmallInt;
    Func1     : SmallInt;
    Func2     : SmallInt;
  end;

  TKeyOrder = (ko_primary, ko_secondary);

  TKeyInventory     = class;
  TKeyItemInventory = class
  private
    FOwner          : TKeyInventory;
    FXmlSource      : string;
    FXMLPrimary     : string;
    FXMLSecondary   : string;
    FXMLToggleOn    : string;
    Buffer          : string;
    FBind           : string;
    FPrimary        : TRecordKeySection;
    FSecondary      : TRecordKeySection;
    FToggle         : Boolean;
    FToggleValue    : string;
    FSignal1Checked : Boolean;
    FSignal1        : TRecordKeySignal;
    FSignal2Checked : Boolean;
    FSignal2        : TRecordKeySignal;
    FId1            : Cardinal;
    FId2            : Cardinal;
    procedure SetXmlSource(const Value: string);
    function  GetText: string;
    function  GetUsable: Boolean;
    function  GetPrimaryEnable: Boolean;
    function  GetSecondaryEnable: Boolean;
    function  GetPrimaryAvailable: Boolean;
    function  GetSecondaryAvailable: Boolean;
  private
    { --- Read and Extract XML data }
    procedure GetDeviceKey(const Src: string; var Device, Key: string);
    procedure AssignToKey(ACount: Byte; var AKey: TRecordKeySection;
      var ASection: TRecordKey; Device, Key: string);
    procedure ExtractToggle;
    procedure ExtractSecondary;
    procedure ExtractPrimary;

    procedure ExtractData;
  private
    { --- Transformer les données en signal utilisable avec "key_event" ou "mouse_event" }
    function EliteKeyToKeyBoard(const KeySelection: TRecordKeySection;
      var KeyResult: TRecordKeySignal):Boolean;
    { --- Méthode de signature des signaux }
    function IdSigning(const Key, Mod1, Mod2: Word):Cardinal; overload;
    function IdSigning(const Value: TRecordKeySignal):Cardinal; overload;
    { --- Ajouter des signatures au catalogue }
    procedure AddToCatalog(const Order: TKeyOrder);
    procedure AddToErrors(const Msg: string);
    procedure AddToHashTable(const Order: TKeyOrder);
    procedure CheckCombination;
  private
    { --- Chech values }
    function IsJoyUsed(const Value: TRecordKeySection):Boolean;
    function IsCombinationJoyBusy: Boolean;
    function IsCombinatonFree: Boolean;
  private
    { --- Build availaible conbibnation}
    function FindFreeCombination(var Key, Func1, Func2: TEliteKeyType;
      var UseTwoFunc: Boolean; var NewSignal: Cardinal):Boolean;
  private
    { --- XML tools }
    function XMLIndentation:string;
  public
    function  ConcatXML:string;
    function  ReplaceSourceXML:string;

    property Owner: TKeyInventory read FOwner;
    property Bind: string read FBind write FBind;

    property PrimaryAvailable: Boolean read GetPrimaryAvailable;
    property PrimaryEnable: Boolean read GetPrimaryEnable;
    property PrimarySignal: TRecordKeySignal read FSignal1;
    property PrimarySignalChecked: Boolean read FSignal1Checked;
    property PrimaryIdKey: Cardinal read FId1;

    property SecondaryAvailable: Boolean read GetSecondaryAvailable;
    property SecondaryEnable: Boolean read GetSecondaryEnable;
    property SecondarySignal: TRecordKeySignal read FSignal2;
    property SecondarySignalChecked: Boolean read FSignal2Checked;
    property SecondaryIdKey: Cardinal read FId2;

    property Text: string read GetText;
    {Usable: Avant synchronisation une des deux possibilités doit être disponible }
    property Usable: Boolean read GetUsable;
    property XMLSource: string read FXmlSource write SetXmlSource;

    constructor Create(const AOwner: TKeyInventory);
  end;


  TKeyInventory = class(TStringList)
  private
    FSource           : string;
    FSourceAlt        : string;
    FCatalog          : TstringList;
    FIndexCatalog     : TStringList;
    FIgnored          : TstringList;
    FErrors           : TstringList;
    FDblUsed          : TstringList;
    FHashTable        : TstringList;

    function GetXML(Bind: string): string;
    function GetTextEx(Bind: string): string;
    function GetCatalogTxt: string;
    function GetErrors: string;
    function GetDoublons: string;
    function GetHashTxt: string;
    function GetItem(Bind: string): TKeyItemInventory;
    function GetIndexedCatalog: string;
  protected
    procedure BuildLocalSaveFolder;
    procedure ResetBuffers;
    function  ReadSource: string;
    { --- Initialisation de l'instance }
    procedure Initialize_;
    { --- Finalisation de l'instance }
    procedure Finalize_;
    function  GetIdFromCatalog(const ASt: string): Cardinal;
    { --- Restorer le source non modifié du Temp.Custom.3.0 }
    procedure RestoreAltSource;
    { --- Gestion des modifications à apporter et validation de ces modifications }
    procedure RetrieveModifications;
    procedure UpdateValidation;
    { --- New combinaisons maker }
    function  isFreeCombination(const IdKey: Cardinal):Boolean; overload;
    function  isFreeCombination(const Key, Func1, Func2: Word):Boolean; overload;
    function  BuildXMLForCombination(const Order: TKeyOrder; const Key, Func1, Func2: Word):string; overload;
    function  BuildXMLForCombination(const Order: TKeyOrder; const IdKey: Cardinal):string; overload;
    { --- Divers for building integrity }
    function  IsMultiUsageModified:Boolean;
    procedure SetMultiUsageModified(const Value: Boolean);
    procedure ErrorRemove(const ForBind: string);
    function  RetrieveMultiUsageByKey(const AKey: string; var Datas: string):Integer;
    procedure BuildMutiUsageKeyList;
    procedure CheckDupicataBinds(const Dup: TKeyDuplicataType; const RefBind, ToBind: string);
    function  HasReference(const Dup: TKeyDuplicataType; var FirstBindValue: string):Boolean;
    procedure MultiUsageAssign;
  private
    { --- Récupération d'une copie du fichier Custom.3.0.Binds pour le traitement local }
    function  Custom3Exists:Boolean;
    procedure Copy_locally;
    function  Try_Copy_locally:Boolean;
    function  IsCustomEnabled:Boolean;
    function  Pre_Initilialization:Boolean;
    { --- Suivi des modifications du fichier Custom.3.0.binds}
    procedure ResetCustomModification;
    procedure SetCustomModified;
    function  IsCustomModified:Boolean;
    procedure ReplaceAndSaveCustom30;
    function  IsFirstTimeUse:Boolean;
    procedure SetFirstTimeUse(const Value: Boolean);
    { --- La synchronisation à Vrai implique la recopie du Temp.Custom.3.0 sur le Custom3.0 }
    function  IsSynchronization:Boolean;
    procedure SetSynchronization(const Value: Boolean);
  public
    procedure Refresh_;
    procedure SetSource(Value: string);
    function  AddKey(const Binds: string): Integer;
    procedure AssignToFree;
    procedure KeyTrigger_(const BindStr: string; UpKey: Boolean); overload;
    procedure KeyTrigger_(const BindStr: string; pDelay: Integer); overload;

    property XML[Bind: string]: string read GetXML;
    property Text[Bind: string]: string read GetTextEx;
    property CatalogTxt: string read GetCatalogTxt;
    property ErrorsTxt: string read GetErrors;
    property DoublonTxt: string read GetDoublons;
    property HashTxt: string read GetHashTxt;
    property IndexedCatalog: string read GetIndexedCatalog;
    property IgnoredCmd: TStringList read FIgnored;
    property Item[Bind: string]:TKeyItemInventory read GetItem;
    property Source: string read FSource;


    constructor Create;
    destructor Destroy; override;

    class procedure Initialize;
    class procedure Finalize;
    class procedure Refresh;
  end;

  TMouseFactory = class;
  TKeySurveyor  = class;
  TKeyMessageSender = class
  private
    FIdKey             : Cardinal;
    FKey               : Word;
    FFunc1             : Word;
    FFunc2             : Word;
    FTimems            : Cardinal;
    FDown              : TStringList;
    FClock             : Cardinal;
    FSurveyor          : TKeySurveyor;
    FOnBeforeMouseDown : TNotifyEvent;
    FOnAfterMouseDown  : TNotifyEvent;
    FMouseFactory      : TMouseFactory;
    FKeySurveyor       : TKeySurveyor;
    function MouseSignalToEvent(const Value: Word; UpSignal: Boolean;
      var Button: Word):Word;
    procedure DoBeforeMouseSignal;
    procedure DoAfterMouseSignal;
    function  GetIndexMonitor: Integer;
    procedure SetIndexMonitor(const Value: Integer);
  protected
    procedure StartSurveyor;
  public
    procedure SetIdKey(const Value: Cardinal);
    procedure SetTimems(const Value: Cardinal);
    procedure SetOnBeforeMouseDown(const Value: TNotifyEvent);
    procedure SetOnAfterMouseDown(const Value: TNotifyEvent);
    function  TimeBeforeUpKey: Cardinal;
    procedure SendSignal(UpKey: Boolean);
    procedure DoKeyUp;

    property Clock: Cardinal read FClock write FClock;
    property IndexMonitor: Integer read GetIndexMonitor write SetIndexMonitor;
    {Events}
    property OnBeforeMouseDown: TNotifyEvent read FOnBeforeMouseDown write FOnBeforeMouseDown;
    property OnAfterMouseDown: TNotifyEvent read FOnAfterMouseDown write FOnAfterMouseDown;

    constructor Create;
    destructor Destroy; override;
    class procedure Initialize;
    class procedure Finalize;
    class procedure Signal(const KeyValue, TimeMs: Cardinal; UpKey: Boolean = WITH_KEYUP);
    class procedure BeforeMouseDown(const Value: TNotifyEvent);
    class procedure AfterMouseDown(const Value: TNotifyEvent);
    class procedure KeyUp;
  end;

  TMouseFactory = class
  private
    FMousePoint: TPoint;
    function  GetIndexMonitor: Integer;
    procedure SetIndexMonitor(const Value: Integer);
    procedure RetrieveIndexMonitor;
  public
    procedure DoBeforeMouseDown(Sender: TObject);
    procedure DoAfterMouseDown(Sender: TObject);

    property IndexMonitor: Integer read GetIndexMonitor write SetIndexMonitor;

    constructor Create;
  end;

  TKeySurveyor = class(TThread)
  private
    ThKeyMessageSender : TKeyMessageSender;
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  protected
    procedure Execute; override;
  public
    constructor Create(const AKeyMessager: TKeyMessageSender);
  end;




  TBindCategoryManager = class
  private
    FMCR : TArrayOfBindCategories;
    function GetCount: Integer;
    function GetString(index: Integer): string;
  protected
    procedure CheckIndex(const index: Integer);
  public

    property Count: Integer read GetCount;
    property CategoryText[index: Integer]:string read GetString;

    constructor Create;
  end;

function  IsEliteRunning: Boolean;
procedure EliteForeGround;

var
  ExtendedVKeys : set of byte = [
      VK_Up,       VK_Down,     VK_Left,    VK_Right,    VK_Home,
      VK_End,      VK_Prior,    VK_Next,    VK_Insert,   VK_Delete,
      VK_RCONTROL, VK_RMENU,    VK_RSHIFT
  ];

var
  KeyInventory     : TKeyInventory = nil;       { --- Singleton }
  KeyMessageSender : TKeyMessageSender = nil;   { --- Singleton }
  HElite           : THandle = 0;











implementation

uses
  StrUtils;

function IsEliteRunning: Boolean;
begin
  if HElite = 0 then HElite := FindWindow( PWideChar(ELITE_CLASS), nil );
  Result :=  HElite <> 0;
end;

procedure EliteForeGround;
begin
  if HElite = 0 then HElite := FindWindow( PWideChar(ELITE_CLASS), nil );
  SetForegroundWindow( HElite );
//  Sleep(30)   { --- Attention pas pour le tobii }
end;

function SetMessage(const Messages: array of string):string;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    for i := Low(Messages) to High(Messages) do Add( Messages[i] );
    Result := Text
  finally
    Free
  end
end;

function GetFrontierSaveGames: string;
begin
  KeyRead(SHELL_ACCES, SAVE_GAMES_KEY, Result);
  Result := Format('%s\%s', [Result, SavedGamesW])
end;

function GetEliteBindingsFolder: string;
begin
  KeyRead(SHELL_ACCES, BINDING_OPT_KEY, Result);
  Result := Format('%s\%s', [Result, BindingsFolder])
end;

function GetEliteGraphicsFolder: string;
begin
  KeyRead(SHELL_ACCES, BINDING_OPT_KEY, Result);
  Result := Format('%s\%s', [Result, GraphicsFolder])
end;

const
  PERFUNC1 = 1000000;
  PERFUNC2 = 1000;

procedure DecodeKey(const Value: Cardinal; var Key, Func1, Func2: Word);
var
  Reste: Cardinal;
begin
  Reste := Value;
  Func1 := Reste div PERFUNC1;
  Reste := Reste mod PERFUNC1;
  Func2 := Reste div PERFUNC2;
  Key   := Reste mod PERFUNC2
end;

function ListToSemicolumnStr(const Ast : String):string;
begin
  Result := EmptyStr;
  with TStringList.Create do
  try
    Text := ASt;
    with GetEnumerator do
    try
      while MoveNext do if Result = EmptyStr then Result := Current
        else Result := Format('%s; %s', [Result, Current])
    finally
      Free
    end
  finally
    Free
  end
end;

function HavePrimary(const Src, Balise: string):Boolean;
var
  Buffer: string;
begin
  Buffer := TBindsArrayTools.Extract(Src, Balise);
  Result := AnsiPos('Primary', Buffer) > 0;
end;

function OrderExchange(const ASt: string; Order: TKeyOrder):string;
begin
  Result := ASt;
  case Order of
    ko_primary   : while AnsiPos('Primary',   Result) > 0 do Result := StrUtils.ReplaceStr(Result,'Primary',   'Secondary');
    ko_secondary : while AnsiPos('Secondary', Result) > 0 do Result := StrUtils.ReplaceStr(Result,'Secondary', 'Primary');
  end;
end;

function ExtractBalises(const Src: string; Full: Boolean):string;
var
  OutStr: TStringList;
  RefStr: TStringList;
  Buffer: string;

  procedure LineExtract(const S: string); begin
    if (AnsiPos('/', S) = 0) and (AnsiPos(' ', S) = 0) then begin
      Buffer := GetAfterStr(S, '<');
      Buffer := GetBeforStr(Buffer, '>');
      if Full then begin
        { --- Toutes les commandes traitées et non traitées }
        if HavePrimary(Src, Buffer) then OutStr.Add( Buffer )
      end else begin
        { --- Que les commandes non traitées }
        if (RefStr.IndexOf(Buffer) = -1 ) and  HavePrimary(Src, Buffer) then OutStr.Add( Buffer )
      end
    end
  end;

begin
  OutStr := TStringList.Create;
  RefStr := TStringList.Create;
  RefStr.Text := TBindsArrayTools.BindsXMLtext;
  with TStringList.Create do
  try
    Text := Src;
    with GetEnumerator do
    try
      while MoveNext do LineExtract(Current)
    finally
      Free
    end;
    Result := Trim( OutStr.Text )
  finally
    RefStr.Free;
    OutStr.Free;
    Free
  end
end; {ExtractBalises}

function EncodeTagDate(const ADate: TDateTime):string;
var
  Y, M, D       : Word;
  H, Min, S, Ms : Word;
begin
  DecodeDate(ADate, Y, M, D);
  DecodeTime(ADate, H, Min, S, Ms);
  Result := Format('%d%0.2d%0.2dT%0.2d%0.2d%0.2d', [Y, M, D, H, Min, S])
end;

function EncodeKey(const Key, Mod1, Mod2: Word): Cardinal;
{ --- Value = Mod1 * 10^6 + Mod2 * 10^3 + Key
      avec Mod1 le premier Modifier rencontré }
begin
  Result := Trunc( Mod1 * Power(10, 6) + Mod2 * Power(10, 3) + Key )
end;

function EncodeKey(const Key, Mod1, Mod2: string): Cardinal;
begin
  Result := EncodeKey(
    EliteKeyToScanValue(Key),
    EliteKeyToScanValue(Mod1),
    EliteKeyToScanValue(Mod2))
end;

procedure DuplicatasInitialize;
begin
  Duplicatas[kdt_PrimaryFire] := DPrimaryFire;
  Duplicatas[kdt_SecondaryFire] := DSecondaryFire;
  Duplicatas[kdt_UI_Select] := DUI_Select;
  Duplicatas[kdt_UI_Down] := DUI_Down;
  Duplicatas[kdt_UI_Left] := DUI_Left;
  Duplicatas[kdt_UI_Back] := DUI_Back;
  Duplicatas[kdt_UI_Right] := DUI_Right;
  Duplicatas[kdt_UI_Up] := DUI_Up;
  Duplicatas[kdt_RollRight] := DRollRight;
  Duplicatas[kdt_RollLeft] := DRollLeft;
  Duplicatas[kdt_OpenCodexGoToDiscovery] := DOpenCodexGoToDiscovery;
  Duplicatas[kdt_PlayerHUDModeToggle] := DPlayerHUDModeToggle;
  Duplicatas[kdt_IncreaseWeaponsPower] := DIncreaseWeaponsPower;
  Duplicatas[kdt_ResetPowerDistribution] := DResetPowerDistribution;
  Duplicatas[kdt_CycleFireGroupPrevious] := DCycleFireGroupPrevious;
  Duplicatas[kdt_IncreaseEnginesPower] := DIncreaseEnginesPower;
  Duplicatas[kdt_IncreaseSystemsPower] := DIncreaseSystemsPower;
  Duplicatas[kdt_CycleFireGroupNext] := DCycleFireGroupNext;
  Duplicatas[kdt_LeftThrust] := DLeftThrust;
  Duplicatas[kdt_ToggleCargoScoop] := DToggleCargoScoop;
  Duplicatas[kdt_ShipSpotLightToggle] := DShipSpotLightToggle;
  Duplicatas[kdt_EjectAllCargo] := DEjectAllCargo;
  Duplicatas[kdt_SelectTarget] := DSelectTarget;
  Duplicatas[kdt_FocusRightPanel] := DFocusRightPanel;
  Duplicatas[kdt_GalaxyMapOpen] := DGalaxyMapOpen;
  Duplicatas[kdt_SystemMapOpen] := DSystemMapOpen;
  Duplicatas[kdt_ToggleReverseThrottleInput] := DToggleReverseThrottleInput;
  Duplicatas[kdt_FocusLeftPanel] := DFocusLeftPanel;
  Duplicatas[kdt_FocusCommsPanel] := DFocusCommsPanel;
  Duplicatas[kdt_FocusRadarPanel] := DFocusRadarPanel;
  Duplicatas[kdt_PitchUp] := DPitchUp;
  Duplicatas[kdt_PitchDown] := DPitchDown;
  Duplicatas[kdt_YawRight] := DYawRight;
  Duplicatas[kdt_YawLeft] := DYawLeft;
end;

procedure PreferenceKeysInitialize;
begin
  PreferenceKeys[kdt_PrimaryFire] := 1;
  PreferenceKeys[kdt_SecondaryFire] := 2;
  PreferenceKeys[kdt_UI_Select] := VK_RETURN;     //NOTE : Warning Tobii
  PreferenceKeys[kdt_UI_Down] := VK_NUMPAD2;
  PreferenceKeys[kdt_UI_Left] := VK_NUMPAD4;
  PreferenceKeys[kdt_UI_Back] := VkKeyScan('²');  //NOTE : Warning Tobii
  PreferenceKeys[kdt_UI_Right] := VK_NUMPAD6;
  PreferenceKeys[kdt_UI_Up] := VK_NUMPAD8;
  PreferenceKeys[kdt_RollRight] := VK_MULTIPLY;
  PreferenceKeys[kdt_RollLeft] := VK_SCROLL;
  PreferenceKeys[kdt_OpenCodexGoToDiscovery] := EncodeKey('Key_E', 'Key_LeftShift', 'Key_RightShift');
  PreferenceKeys[kdt_PlayerHUDModeToggle] := EncodeKey('Key_R', 'Key_LeftShift', 'Key_RightShift');
  PreferenceKeys[kdt_IncreaseWeaponsPower] := EncodeKey('Key_A', 'Key_RightShift', EmptyStr);
  PreferenceKeys[kdt_ResetPowerDistribution] := EncodeKey('Key_D', 'Key_RightShift', EmptyStr);
  PreferenceKeys[kdt_CycleFireGroupPrevious] := EncodeKey('Key_H', 'Key_RightShift', EmptyStr);
  PreferenceKeys[kdt_IncreaseEnginesPower] := EncodeKey('Key_M', 'Key_RightShift', EmptyStr);
  PreferenceKeys[kdt_IncreaseSystemsPower] := EncodeKey('Key_S', 'Key_RightShift', EmptyStr);
  PreferenceKeys[kdt_CycleFireGroupNext] := EncodeKey('Key_V', 'Key_RightShift', EmptyStr);
  PreferenceKeys[kdt_LeftThrust] := EncodeKey('Key_P', 'Key_LeftControl', 'Key_LeftAlt');
  PreferenceKeys[kdt_ToggleCargoScoop] := EncodeKey('Key_A', 'Key_RightControl', EmptyStr);
  PreferenceKeys[kdt_ShipSpotLightToggle] := EncodeKey('Key_H', 'Key_RightControl', EmptyStr);
  PreferenceKeys[kdt_EjectAllCargo] := EncodeKey('Key_I', 'Key_RightControl', EmptyStr);
  PreferenceKeys[kdt_SelectTarget] := EncodeKey('Key_J', 'Key_RightControl', EmptyStr);
  PreferenceKeys[kdt_FocusRightPanel] := EncodeKey('Key_A', 'Key_RightControl', 'Key_LeftAlt');
  PreferenceKeys[kdt_GalaxyMapOpen] := EncodeKey('Key_B', 'Key_RightControl', 'Key_LeftAlt');
  PreferenceKeys[kdt_SystemMapOpen] := EncodeKey('Key_C', 'Key_RightControl', 'Key_LeftAlt');
  PreferenceKeys[kdt_ToggleReverseThrottleInput] := EncodeKey('Key_D', 'Key_RightAlt', EmptyStr);
  PreferenceKeys[kdt_FocusLeftPanel] := EncodeKey('Key_X', 'Key_RightAlt', EmptyStr);
  PreferenceKeys[kdt_FocusCommsPanel] := EncodeKey('Key_Y', 'Key_RightAlt', EmptyStr);
  PreferenceKeys[kdt_FocusRadarPanel] := EncodeKey('Key_Z', 'Key_RightAlt', EmptyStr);
  PreferenceKeys[kdt_PitchUp] := VK_PRIOR;
  PreferenceKeys[kdt_PitchDown] := VK_NEXT;
  PreferenceKeys[kdt_YawRight] := VK_END;
  PreferenceKeys[kdt_YawLeft] := VK_HOME;
end;

function DuplicataItemsCount(const Value: TKeyDuplicataType):Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := Low(Duplicatas[Value]) to High(Duplicatas[Value]) do
    if Duplicatas[Value][i] <> EmptyStr then Inc(Result) else Break
end;

function DuplicataItem(const Value: TKeyDuplicataType; index: Integer): string;
begin
  Result := Duplicatas[Value][index]
end;

function GetBindCategories(const Value: string): TBindCategoriesType;
begin
  try
    Result := TBindCategoriesType( IndexStr(Value, MCR) )
  except
    raise
  end
end;

function IsBindCategories(const Value: string):Boolean;
begin
  Result := IndexStr(Value, MCR) > -1
end;

function GetVolRotation(const Value: string):TVolRotationType;
begin
  try
    Result := TVolRotationType( IndexStr(Value, VolRotation) )
  except
    raise
  end
end;

function IsVolRotation(const Value: string):Boolean;
begin
  Result := IndexStr(Value, VolRotation) > -1
end;

function GetVolPousee(const Value: string): TVolPousseType;
begin
  try
    Result := TVolPousseType( IndexStr(Value, VolPoussee) )
  except
    raise
  end
end;

function IsVolPousee(const Value: string):Boolean;
begin
  Result := IndexStr(Value, VolPoussee) > -1
end;

function GetVolPropulsion(const Value: string): TVolPropulsionType;
begin
  try
    Result := TVolPropulsionType( IndexStr(Value, VolPropulsion) )
  except
    raise
  end
end;

function IsVolPropulsion(const Value: string):Boolean;
begin
  Result := IndexStr(Value, VolPropulsion) > -1
end;

function GetVolDivers(const Value: string): TVolDiversType;
begin
  try
    Result := TVolDiversType( IndexStr(Value, VolDivers) )
  except
    raise
  end
end;

function IsVolDivers(const Value: string):Boolean;
begin
  Result := IndexStr(Value, VolDivers) > -1
end;

function GetVisee(const Value: string): TViseeType;
begin
  try
    Result := TViseeType( IndexStr(Value, Visee) )
  except
    raise
  end
end;

function IsVisee(const Value: string):Boolean;
begin
  Result := IndexStr(Value, Visee) > -1
end;

function GetArmes(const Value: string): TArmesType;
begin
  try
    Result := TArmesType( IndexStr(Value, Armes) )
  except
    raise
  end
end;

function IsArmes(const Value: string):Boolean;
begin
  Result := IndexStr(Value, Armes) > -1
end;

function GetFreeze(const Value: string): TFreezeType;
begin
  try
    Result := TFreezeType( IndexStr(Value, Freeze) )
  except
    raise
  end
end;

function IsFreeze(const Value: string):Boolean;
begin
  Result := IndexStr(Value, Freeze) > -1
end;

function GetDivers(const Value: string): TDiversType;
begin
  try
    Result := TDiversType( IndexStr(Value, Divers) )
  except
    raise
  end
end;

function IsDivers(const Value: string):Boolean;
begin
  Result := IndexStr(Value, Divers) > -1
end;

function GetModeChange(const Value: string): TModeChangetype;
begin
  try
    Result := TModeChangetype( IndexStr(Value, ModeChange) )
  except
    raise
  end
end;

function IsModeChange(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ModeChange) > -1
end;

function GetModeBoard(const Value: string): TModeBoardType;
begin
  try
    Result := TModeBoardType( IndexStr(Value, ModeBoard) )
  except
    raise
  end
end;

function IsModeBoard(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ModeBoard) > -1
end;

function GetConduite(const Value: string): TConduiteType;
begin
  try
    Result := TConduiteType( IndexStr(Value, Conduite) )
  except
    raise
  end
end;

function IsConduite(const Value: string):Boolean;
begin
  Result := IndexStr(Value, Conduite) > -1
end;

function GetConduiteVisee(const Value: string): TConduiteViseeType;
begin
  try
    Result := TConduiteViseeType( IndexStr(Value, ConduiteVisee) )
  except
    raise
  end
end;

function IsConduiteVisee(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ConduiteVisee) > -1
end;

function GetConduiteTurret(const Value: string): TConduiteTurretType;
begin
  try
    Result := TConduiteTurretType( IndexStr(Value, ConduiteTurret) )
  except
    raise
  end
end;

function IsConduiteTurret(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ConduiteTurret) > -1
end;

function GetConduitePropulsion(const Value: string): TConduitePropulsionType;
begin
  try
    Result := TConduitePropulsionType( IndexStr(Value, ConduitePropulsion) )
  except
    raise
  end
end;

function IsConduitePropulsion(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ConduitePropulsion) > -1
end;

function GetConduiteDivers(const Value: string): TConduiteDiversType;
begin
  try
    Result := TConduiteDiversType( IndexStr(Value, ConduiteDivers) )
  except
    raise
  end
end;

function IsConduiteDivers(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ConduiteDivers) > -1
end;

function GetConduiteMode(const Value: string): TConduiteModeType;
begin
  try
    Result := TConduiteModeType( IndexStr(Value, ConduiteMode) )
  except
    raise
  end
end;

function IsConduiteMode(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ConduiteMode) > -1
end;

function GetChasseurOrder(const Value: string): TChasseurOrderType;
begin
  try
    Result := TChasseurOrderType( IndexStr(Value, ChasseurOrder) )
  except
    raise
  end
end;

function IsChasseurOrder(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ChasseurOrder) > -1
end;

function GetExplorationFss(const Value: string): TExplorationFssType;
begin
  try
    Result := TExplorationFssType( IndexStr(Value, ExplorationFss) )
  except
    raise
  end
end;

function IsExplorationFss(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ExplorationFss) > -1
end;

function GetSurfaceDetaillee(const Value: string): TSurfaceDetailleeType;
begin
  try
    Result := TSurfaceDetailleeType( IndexStr(Value, SurfaceDetaillee) )
  except
    raise
  end
end;

function IsSurfaceDetaillee(const Value: string):Boolean;
begin
  Result := IndexStr(Value, SurfaceDetaillee) > -1
end;

function GetManualLanding(const Value: string): TManualLandingType;
begin
  try
    Result := TManualLandingType( IndexStr(Value, ManualLanding) )
  except
    raise
  end
end;

function IsManualLanding(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ManualLanding) > -1
end;

function GetMultiCrew(const Value: string): TMultiCrewType;
begin
  try
    Result := TMultiCrewType( IndexStr(Value, MultiCrew) )
  except
    raise
  end
end;

function IsMultiCrew(const Value: string):Boolean;
begin
  Result := IndexStr(Value, MultiCrew) > -1
end;

function GetGalaxyMap(const Value: string): TGalaxyMapType;
begin
  try
    Result := TGalaxyMapType( IndexStr(Value, GalaxyMap) )
  except
    raise
  end
end;

function IsGalaxyMap(const Value: string):Boolean;
begin
  Result := IndexStr(Value, GalaxyMap) > -1
end;

function GetCamSystem(const Value: string): TCamSystemType;
begin
  try
    Result := TCamSystemType( IndexStr(Value, CamSystem) )
  except
    raise
  end
end;

function IsCamSystem(const Value: string):Boolean;
begin
  Result := IndexStr(Value, CamSystem) > -1
end;

function GetGalnetReader(const Value: string): TGalnetReaderType;
begin
  try
    Result := TGalnetReaderType( IndexStr(Value, GalnetReader) )
  except
    raise
  end
end;

function IsGalnetReader(const Value: string):Boolean;
begin
  Result := IndexStr(Value, GalnetReader) > -1
end;

function GetCamFree(const Value: string): TCamFreeType;
begin
  try
    Result := TCamFreeType( IndexStr(Value, CamFree) )
  except
    raise
  end
end;

function IsCamFree(const Value: string):Boolean;
begin
  Result := IndexStr(Value, CamFree) > -1
end;

function GetEliteKey(const Value: string): TEliteKeyType;
begin
  try
    Result := TEliteKeyType( IndexStr(Value, ArrayEliteKey) )
  except
    raise
  end
end;

function IsEliteKey(const Value: string):Boolean;
begin
  Result := IndexStr(Value, ArrayEliteKey) > -1
end;

function EliteKeyToScanValue(const Value: string):SmallInt;
begin
  Result := 0;
  if IsEliteKey( Value ) then
    Result := EliteKeyToScanValue( GetEliteKey( Value ) );
end;



function EliteKeyToScanValue(const Value: TEliteKeyType):SmallInt;
begin
    case Value of
      Key_A                : Result := VkKeyScan('A');
      Key_B                : Result := VkKeyScan('B');
      Key_C                : Result := VkKeyScan('C');
      Key_D                : Result := VkKeyScan('D');
      Key_E                : Result := VkKeyScan('E');
      Key_F                : Result := VkKeyScan('F');
      Key_G                : Result := VkKeyScan('G');
      Key_H                : Result := VkKeyScan('H');
      Key_I                : Result := VkKeyScan('I');
      Key_J                : Result := VkKeyScan('J');
      Key_K                : Result := VkKeyScan('K');
      Key_L                : Result := VkKeyScan('L');
      Key_M                : Result := VkKeyScan('M');
      Key_N                : Result := VkKeyScan('N');
      Key_O                : Result := VkKeyScan('O');
      Key_P                : Result := VkKeyScan('P');
      Key_Q                : Result := VkKeyScan('Q');
      Key_R                : Result := VkKeyScan('R');
      Key_S                : Result := VkKeyScan('S');
      Key_T                : Result := VkKeyScan('T');
      Key_U                : Result := VkKeyScan('U');
      Key_V                : Result := VkKeyScan('V');
      Key_W                : Result := VkKeyScan('W');
      Key_X                : Result := VkKeyScan('X');
      Key_Y                : Result := VkKeyScan('Y');
      Key_Z                : Result := VkKeyScan('Z');
      Key_Space            : Result := VK_SPACE;
      Key_F1               : Result := VK_F1;
      Key_F2               : Result := VK_F2;
      Key_F3               : Result := VK_F3;
      Key_F4               : Result := VK_F4;
      Key_F5               : Result := VK_F5;
      Key_F6               : Result := VK_F6;
      Key_F7               : Result := VK_F7;
      Key_F8               : Result := VK_F8;
      Key_F9               : Result := VK_F9;
      Key_F10              : Result := VK_F10;
      Key_F11              : Result := VK_F11;
      Key_F12              : Result := VK_F12;
      Key_Enter            : Result := VK_RETURN;
      Key_RightArrow       : Result := VK_RIGHT;
      Key_DownArrow        : Result := VK_DOWN;
      Key_LeftArrow        : Result := VK_LEFT;
      Key_UpArrow          : Result := VK_UP;
      Key_Insert           : Result := VK_INSERT;
      Key_Delete           : Result := VK_DELETE;
      Key_Home             : Result := VK_HOME;
      Key_End              : Result := VK_END;
      Key_PageUp           : Result := VK_PRIOR;
      Key_PageDown         : Result := VK_NEXT;
      Key_ScrollLock       : Result := VK_SCROLL;
      Key_Pause            : Result := VK_PAUSE;
      Key_Numpad_0         : Result := VK_NUMPAD0;
      Key_Numpad_1         : Result := VK_NUMPAD1;
      Key_Numpad_2         : Result := VK_NUMPAD2;
      Key_Numpad_3         : Result := VK_NUMPAD3;
      Key_Numpad_4         : Result := VK_NUMPAD4;
      Key_Numpad_5         : Result := VK_NUMPAD5;
      Key_Numpad_6         : Result := VK_NUMPAD6;
      Key_Numpad_7         : Result := VK_NUMPAD7;
      Key_Numpad_8         : Result := VK_NUMPAD8;
      Key_Numpad_9         : Result := VK_NUMPAD9;
      Key_NumLock          : Result := VK_NUMLOCK;
      Key_Numpad_Divide    : Result := VK_DIVIDE;
      Key_Numpad_Multiply  : Result := VK_MULTIPLY;
      Key_Numpad_Subtract  : Result := VK_SUBTRACT;
      Key_Numpad_Add       : Result := VK_ADD;
      Key_Numpad_Enter     : Result := VK_RETURN;   //?   VK_PROCESSKEY
      Key_Numpad_Decimal   : Result := VK_DECIMAL;
      Key_CapsLock         : Result := VK_CAPITAL;
      Key_Tab              : Result := VK_TAB;
      Key_SuperscriptTwo   : Result := VkKeyScan('²');
      Key_Ampersand        : Result := VkKeyScan('&');
      Key_é                : Result := VkKeyScan('é');
      Key_DoubleQuote      : Result := VkKeyScan('"');
      Key_Apostrophe       : Result := VkKeyScan('''');
      Key_LeftParenthesis  : Result := VkKeyScan('(');
      Key_Minus            : Result := VkKeyScan('-');
      Key_è                : Result := VkKeyScan('è');
      Key_Underline        : Result := VkKeyScan('_');
      Key_ç                : Result := VkKeyScan('ç');
      Key_à                : Result := VkKeyScan('à');
      Key_RightParenthesis : Result := VkKeyScan(')');
      Key_Equals           : Result := VkKeyScan('=');
      Key_Comma            : Result := VkKeyScan(',');
      Key_SemiColon        : Result := VkKeyScan(';');
      Key_Colon            : Result := VkKeyScan(':');
      Key_ExclamationPoint : Result := VkKeyScan('!');
      Key_ù                : Result := VkKeyScan('ù');
      Key_Asterisk         : Result := VkKeyScan('*');
      Key_Circumflex       : Result := VkKeyScan('^');
      Key_Dollar           : Result := VkKeyScan('$');
      Key_LessThan         : Result := VkKeyScan('<');
      Key_Apps             : Result := VK_APPS;
      Key_Backspace        : Result := VK_BACK;
      Key_Echap            : Result := VK_ESCAPE;
      Key_LeftAlt          : Result := VK_LMENU;
      Key_RightAlt         : Result := VK_RMENU;
      Key_LeftControl      : Result := VK_LCONTROL;
      Key_RightControl     : Result := VK_RCONTROL;
      Key_LeftShift        : Result := VK_LSHIFT;
      Key_RightShift       : Result := VK_RSHIFT;
      {Mouse}
      Mouse_1              : Result := VK_LBUTTON;
      Mouse_2              : Result := VK_RBUTTON;
      Mouse_3              : Result := VK_MBUTTON;
      Mouse_4              : Result := VK_XBUTTON1;
      Mouse_5              : Result := VK_XBUTTON2;
      else Result := 0;
  end;
end;


function KeyScanToEliteString(const Value: SmallInt):string;
begin
  case Value of
    001 : Result := 'Mouse_1';
    002 : Result := 'Mouse_2';
    004 : Result := 'Mouse_3';
    005 : Result := 'Mouse_4';
    006 : Result := 'Mouse_5';
    008 : Result := 'Key_Backspace';
    009 : Result := 'Key_Tab';
    013 : Result := 'Key_Enter';
    (*013 : Result := 'Key_Numpad_Enter';*)
    019 : Result := 'Key_Pause';
    020 : Result := 'Key_CapsLock';
    027 : Result := 'Key_Echap';
    032 : Result := 'Key_Space';
    033 : Result := 'Key_PageUp';
    034 : Result := 'Key_PageDown';
    035 : Result := 'Key_End';
    036 : Result := 'Key_Home';
    037 : Result := 'Key_LeftArrow';
    038 : Result := 'Key_UpArrow';
    039 : Result := 'Key_RightArrow';
    040 : Result := 'Key_DownArrow';
    045 : Result := 'Key_Insert';
    046 : Result := 'Key_Delete';
    048 : Result := 'Key_à';
    049 : Result := 'Key_Ampersand';
    050 : Result := 'Key_é';
    051 : Result := 'Key_DoubleQuote';
    052 : Result := 'Key_Apostrophe';
    053 : Result := 'Key_LeftParenthesis';
    054 : Result := 'Key_Minus';
    055 : Result := 'Key_è';
    056 : Result := 'Key_Underline';
    057 : Result := 'Key_ç';
    093 : Result := 'Key_Apps';
    096 : Result := 'Key_Numpad_0';
    097 : Result := 'Key_Numpad_1';
    098 : Result := 'Key_Numpad_2';
    099 : Result := 'Key_Numpad_3';
    100 : Result := 'Key_Numpad_4';
    101 : Result := 'Key_Numpad_5';
    102 : Result := 'Key_Numpad_6';
    103 : Result := 'Key_Numpad_7';
    104 : Result := 'Key_Numpad_8';
    105 : Result := 'Key_Numpad_9';
    106 : Result := 'Key_Numpad_Multiply';
    107 : Result := 'Key_Numpad_Add';
    109 : Result := 'Key_Numpad_Subtract';
    110 : Result := 'Key_Numpad_Decimal';
    111 : Result := 'Key_Numpad_Divide';
    112 : Result := 'Key_F1';
    113 : Result := 'Key_F2';
    114 : Result := 'Key_F3';
    115 : Result := 'Key_F4';
    116 : Result := 'Key_F5';
    117 : Result := 'Key_F6';
    118 : Result := 'Key_F7';
    119 : Result := 'Key_F8';
    120 : Result := 'Key_F9';
    121 : Result := 'Key_F10';
    122 : Result := 'Key_F11';
    123 : Result := 'Key_F12';
    144 : Result := 'Key_NumLock';
    145 : Result := 'Key_ScrollLock';
    160 : Result := 'Key_LeftShift';
    161 : Result := 'Key_RightShift';
    162 : Result := 'Key_LeftControl';
    163 : Result := 'Key_RightControl';
    164 : Result := 'Key_LeftAlt';
    165 : Result := 'Key_RightAlt';
    186 : Result := 'Key_Dollar';
    187 : Result := 'Key_Equals';
    188 : Result := 'Key_Comma';
    190 : Result := 'Key_SemiColon';
    191 : Result := 'Key_Colon';
    192 : Result := 'Key_ù';
    219 : Result := 'Key_RightParenthesis';
    220 : Result := 'Key_Asterisk';
    221 : Result := 'Key_Circumflex';
    222 : Result := 'Key_SuperscriptTwo';
    223 : Result := 'Key_ExclamationPoint';
    226 : Result := 'Key_LessThan';
    321 : Result := 'Key_A';
    322 : Result := 'Key_B';
    323 : Result := 'Key_C';
    324 : Result := 'Key_D';
    325 : Result := 'Key_E';
    326 : Result := 'Key_F';
    327 : Result := 'Key_G';
    328 : Result := 'Key_H';
    329 : Result := 'Key_I';
    330 : Result := 'Key_J';
    331 : Result := 'Key_K';
    332 : Result := 'Key_L';
    333 : Result := 'Key_M';
    334 : Result := 'Key_N';
    335 : Result := 'Key_O';
    336 : Result := 'Key_P';
    337 : Result := 'Key_Q';
    338 : Result := 'Key_R';
    339 : Result := 'Key_S';
    340 : Result := 'Key_T';
    341 : Result := 'Key_U';
    342 : Result := 'Key_V';
    343 : Result := 'Key_W';
    344 : Result := 'Key_X';
    345 : Result := 'Key_Y';
    346 : Result := 'Key_Z';
    else  Result := EmptyStr
  end;
end;

function IdKeyToStr(const IdKey: Cardinal):string;
var
  Key, Func1, Func2 : Word;
  s2,    s3         : string;
begin
  DecodeKey(IdKey, Key, Func1, Func2);
  Result := KeyScanToEliteString(Key);
  s2     := KeyScanToEliteString(Func1);
  s3     := KeyScanToEliteString(Func2);
  if s3 <> EmptyStr then s2     := Format('%s + %s', [s2, s3]);
  if s2 <> emptyStr then Result := Format('%s + %s', [s2, Result]);
end;

function GetKeyNext(const Value:TEliteKeyType):TEliteKeyType;
{ --- Exclure les caractères accentués de la sélection automatique }

   function Next(X: TEliteKeyType):TEliteKeyType; begin
     Result := TEliteKeyType( (Integer(X) + 1) mod Succ(High(TEliteKeysArea)) );
   end;

begin
  Result := Next(Value);
  while Result in [ Key_Enter,           Key_LeftAlt,       Key_RightAlt,
                    Key_LeftControl,     Key_RightControl,  Key_LeftShift,
                    Key_RightShift,      Key_Backspace,     Key_Space,
                    Mouse_1,             Mouse_2,           Mouse_3,
                    Mouse_4,             Mouse_5,           Key_é,
                    Key_è,               Key_ç,             Key_à,
                    Key_ù,               Key_Echap ]
  do Result := Next(Result)
end; {GetKeyNext}

function GetKeyFuncNext(const Value:TEliteKeyType):TEliteKeyType;

  function Next(X: TEliteKeyType):TEliteKeyType; begin
    Result := TEliteKeyType( (Integer(X) + 1) mod Succ(High(TEliteKeysArea)) );
    if Integer(Result) = 0 then Result := Key_LeftAlt
  end;

begin
  if (Value = Key_RightShift) or (Value < Key_LeftAlt) then Result := Key_LeftAlt
   else Result := Next(Value)
end; {GetKeyFuncNext}

function KeyTypeToStr(const Value:TEliteKeyType): string;
begin
  Result := ArrayEliteKey[ Integer(Value) ]
end;

{ TBindCategoryManager }

procedure TBindCategoryManager.CheckIndex(const index: Integer);
begin
  if (index < 0) or (index > High(TBindCategoriesArea)) then
    raise Exception.CreateFmt('%d hors limites', [index]);
end;

constructor TBindCategoryManager.Create;
begin
  inherited Create;
  FMCR := MCR;
end;

function TBindCategoryManager.GetCount: Integer;
begin
  Result := High(TBindCategoriesArea) + 1
end;

function TBindCategoryManager.GetString(index: Integer): string;
begin
  CheckIndex(index);
  Result := FMCR[index];
end;

{ TBindsArrayTools }

class function TBindsArrayTools.AllButtonBalises(const ASource: string;
  Full: Boolean): string;
begin
  Result := ExtractBalises(ASource, Full)
end;

class function TBindsArrayTools.BindsByCategory(
  const Category: TBindCategoriesType): string;
begin
  with TBindsArrayTools.Create do
  try
    Result := BindsByCategory_( Category )
  finally
    Free
  end;
end;

function TBindsArrayTools.BindsByCategory_(
  const Category: TBindCategoriesType): string;
begin
  with TStringList.Create do
  try
    case Category of
      bct_volrotation         : Add( ToStringEx( FVolRotation          ) );
      bct_volpousee           : Add( ToStringEx( FVolPousse            ) );
      bct_volpropulsion       : Add( ToStringEx( FVolPropulsion        ) );
      bct_voldivers           : Add( ToStringEx( FVolDivers            ) );
      bct_visee               : Add( ToStringEx( FVisee                ) );
      bct_armes               : Add( ToStringEx( FArmes                ) );
      bct_freeze              : Add( ToStringEx( FFreeze               ) );
      bct_divers              : Add( ToStringEx( FDivers               ) );
      bct_modechange          : Add( ToStringEx( FModeChange           ) );
      bct_modeboard           : Add( ToStringEx( FModeBoard            ) );
      bct_conduite            : Add( ToStringEx( FConduite             ) );
      bct_conduitevisee       : Add( ToStringEx( FConduiteVisee        ) );
      bct_conduiteturret      : Add( ToStringEx( FConduiteTurret       ) );
      bct_conduitepropulsion  : Add( ToStringEx( FConduitePropulsion   ) );
      bct_conduitedivers      : Add( ToStringEx( FConduiteDivers       ) );
      bct_conduitemode        : Add( ToStringEx( FConduiteMode         ) );
      bct_chasseurorder       : Add( ToStringEx( FChasseurOrder        ) );
      bct_explorationfss      : Add( ToStringEx( FExplorationFss       ) );
      bct_surfacedetaillee    : Add( ToStringEx( FSurfaceDetaillee     ) );
      bct_manuallanding       : Add( ToStringEx( FManualLanding        ) );
      bct_multicrew           : Add( ToStringEx( FMultiCrew            ) );
      bct_galaxymap           : Add( ToStringEx( FGalaxyMap            ) );
      bct_camsystem           : Add( ToStringEx( FCamSystem            ) );
      bct_galnetreader        : Add( ToStringEx( FGalnetReader         ) );
      bct_camfree             : Add( ToStringEx( FCamFree              ) );
    end;
    Result := Trim( Text );
  finally
    Free
  end;
end;

class function TBindsArrayTools.BindsXMLtext: string;
begin
  with TBindsArrayTools.Create do
  try
    Result := BindsXMLtext_
  finally
    Free
  end;
end;

constructor TBindsArrayTools.Create;
begin
  inherited Create;
  FVolRotation             := VolRotation;
  FVolPousse               := VolPoussee;
  FVolPropulsion           := VolPropulsion;
  FVolDivers               := VolDivers;
  FVisee                   := Visee;
  FArmes                   := Armes;
  FFreeze                  := Freeze;
  FDivers                  := Divers;
  FModeChange              := ModeChange;
  FModeBoard               := ModeBoard;
  FConduite                := Conduite;
  FConduiteVisee           := ConduiteVisee;
  FConduiteTurret          := ConduiteTurret;
  FConduitePropulsion      := ConduitePropulsion;
  FConduiteDivers          := ConduiteDivers;
  FConduiteMode            := ConduiteMode;
  FChasseurOrder           := ChasseurOrder;
  FExplorationFss          := ExplorationFss;
  FSurfaceDetaillee        := SurfaceDetaillee;
  FManualLanding           := ManualLanding;
  FMultiCrew               := MultiCrew;
  FGalaxyMap               := GalaxyMap;
  FCamSystem               := CamSystem;
  FGalnetReader            := GalnetReader;
  FCamFree                 := CamFree;
end;

class function TBindsArrayTools.Extract(const ASource,
  BindValue: string): string;
begin
  with TBindsArrayTools.Create do
  try
    SetSource(ASource);
    Result := ExtractXMLByBind(BindValue)
  finally
    Free
  end;
end;



function TBindsArrayTools.ExtractXMLByBind(const BindValue: string): string;
begin
  Result := GetAfterStr(FSource, Format('<%s>',  [BindValue]));
  Result := GetBeforStr(Result,  Format('</%s>', [BindValue]))
end;

function TBindsArrayTools.GetBindsXMLtext: string;
begin
  with TStringList.Create do
  try
    Add( ToStringEx( FVolRotation          ) );
    Add( ToStringEx( FVolPousse            ) );
    Add( ToStringEx( FVolPropulsion        ) );
    Add( ToStringEx( FVolDivers            ) );
    Add( ToStringEx( FVisee                ) );
    Add( ToStringEx( FArmes                ) );
    Add( ToStringEx( FFreeze               ) );
    Add( ToStringEx( FDivers               ) );
    Add( ToStringEx( FModeChange           ) );
    Add( ToStringEx( FModeBoard            ) );
    Add( ToStringEx( FConduite             ) );
    Add( ToStringEx( FConduiteVisee        ) );
    Add( ToStringEx( FConduiteTurret       ) );
    Add( ToStringEx( FConduitePropulsion   ) );
    Add( ToStringEx( FConduiteDivers       ) );
    Add( ToStringEx( FConduiteMode         ) );
    Add( ToStringEx( FChasseurOrder        ) );
    Add( ToStringEx( FExplorationFss       ) );
    Add( ToStringEx( FSurfaceDetaillee     ) );
    Add( ToStringEx( FManualLanding        ) );
    Add( ToStringEx( FMultiCrew            ) );
    Add( ToStringEx( FGalaxyMap            ) );
    Add( ToStringEx( FCamSystem            ) );
    Add( ToStringEx( FGalnetReader         ) );
    Add( ToStringEx( FCamFree              ) );
    Result := Trim( Text )
  finally
    Free
  end;
end;

procedure TBindsArrayTools.SetSource(const Value: string);
begin
  FSource := Value
end;

function TBindsArrayTools.ToStringEx(const AValues: array of string): string;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    for i := Low(AValues) to High(AValues) do Add(AValues[i]);
    Result := Trim( Text )
  finally
    Free
  end;
end;

{ TKeyItemInventory }

procedure TKeyItemInventory.AddToCatalog(const Order: TKeyOrder);
var
  ASt   : string;
  Indic : Boolean;
begin
  if FOwner.IgnoredCmd.IndexOf(Bind) > -1 then Exit;

  case Order of
    ko_primary   : ASt := Format('%s=%d', [Bind, FId1]); //'%s.%s = %d', [Bind, 'primary',   FId1]
    ko_secondary : ASt := Format('%s=%d', [Bind, FId2]); //'%s.%s = %d', [Bind, 'primary',   FId2]
  end;
  case Order of
    ko_primary   : Indic := IndexStr(FPrimary.MainKey.Device,   ['Keyboard', 'Mouse']) > -1;
    else Indic := IndexStr(FSecondary.MainKey.Device, ['Keyboard', 'Mouse']) > -1;
  end;
  with FOwner do if Indic then begin
    { --- Eviter les doublons si le primary et le secondary sont compatibles avec le clavier }
    if FIndexCatalog.indexOf(Bind) = -1 then begin
      FIndexCatalog.Add(Bind);
      FCatalog.Add( ASt );
    end;
  end
end;

procedure TKeyItemInventory.AddToErrors(const Msg: string);
begin
  if FOwner.IgnoredCmd.IndexOf(Bind) > -1 then Exit;

  with FOwner, FErrors do Add( Format('%s=%s', [Bind, Msg]) )
end;

procedure TKeyItemInventory.AddToHashTable(const Order: TKeyOrder);
var
  IDs: string;
begin
  case Order of
    ko_primary    : IDs := Format('%d', [FId1]);
    ko_secondary  : IDs := Format('%d', [FId2]);
  end;
  case Order of
    ko_primary    : if IsJoyUsed(FPrimary)   then Exit;
    ko_secondary  : if IsJoyUsed(FSecondary) then Exit;
  end;
  with FOwner, FHashTable do if FHashTable.IndexOf(Ids) = -1 then Add(Ids);
end;

procedure TKeyItemInventory.AssignToKey(ACount: Byte;
  var AKey: TRecordKeySection; var ASection: TRecordKey; Device, Key: string);
begin
  if Device <> '{NoDevice}' then begin
    AKey.Count      := ACount;
    ASection.Device := Device;
    ASection.Key    := Key
  end else begin
    ASection.Device := Device;
    ASection.Key    := Key
  end
end;

procedure TKeyItemInventory.CheckCombination;
begin
  if IsCombinationJoyBusy then AddToErrors('(0002) Joystick full');
  if IsCombinatonFree     then AddToErrors('(0003) Free');
end;

procedure AddToStringList(const List: TStrings; const ASt: string);
begin
  with TStringList.Create do
  try
    Text := ASt;
    with GetEnumerator do
    try
      while MoveNext do List.Add(Current)
    finally
      Free
    end;
  finally
    Free
  end;
end;

function TKeyItemInventory.ConcatXML: string;
var
  OutStr : TStringList;
begin
  OutStr := TStringList.Create;
  try
    AddToStringList(OutStr, FXMLPrimary);
    AddToStringList(OutStr, FXMLSecondary);
    if FToggle then AddToStringList(OutStr, FXMLToggleOn);
    Result := OutStr.Text;
  finally
    OutStr.Free
  end;
end;

constructor TKeyItemInventory.Create(const AOwner: TKeyInventory);
begin
  inherited Create;
  FOwner := AOwner
end;

function TKeyItemInventory.EliteKeyToKeyBoard(
  const KeySelection: TRecordKeySection; var KeyResult: TRecordKeySignal):Boolean;

  function Process(ForMouse: Boolean): Boolean; begin
    Result := False;
    with KeySelection do begin
      {--- Ignorer si la touche n'est pas gérée }
      if not IsEliteKey(MainKey.Key) then Exit;
      Result := True;
      {--- Traduire la touche principale }
      with KeyResult do begin
        Mouse := ForMouse;
        Key   := EliteKeyToScanValue(MainKey.Key);
      end;
      {--- Traduire les touches "modifier" si nécessaire }
      if Count > 1 then with KeyResult do begin
        Func1 := EliteKeyToScanValue(Modifier1.Key);
        if Count > 2 then Func2 := EliteKeyToScanValue(Modifier2.Key);
      end
    end
  end;

  function KeyBoardProcess: Boolean; begin
    Result := Process(False);
  end;

  function MouseProcess: Boolean; begin
    Result := Process(True)
  end;

begin
  case IndexStr(KeySelection.MainKey.Device, ['Keyboard', 'Mouse', '{NoDevice}']) of
    0  : Result := KeyBoardProcess;
    1  : Result := MouseProcess;
    2  : Result := False;
    else Result := True;
  end;
end; {EliteKeyToKeyBoard}

procedure TKeyItemInventory.ExtractData;
begin
  Buffer := FXmlSource;
  ExtractToggle;
  ExtractSecondary;
  ExtractPrimary;
  FSignal1Checked := EliteKeyToKeyBoard(FPrimary,   FSignal1);
  FSignal2Checked := EliteKeyToKeyBoard(FSecondary, FSignal2);
  FId1            := IdSigning( FSignal1 );
  FId2            := IdSigning( FSignal2 );
  AddToCatalog(ko_primary);
  AddToCatalog(ko_secondary);
  if not PrimaryAvailable then AddToHashTable(ko_primary);
  if not SecondaryAvailable then AddToHashTable(ko_secondary);
  CheckCombination;
end;

procedure TKeyItemInventory.ExtractPrimary;
var
  ASt     : string;
  St1     : string;
  ADevice : string;
  AKey    : string;
begin
  FXMLPrimary     := Buffer;
  {Traitement du primary}
  St1             := GetBeforStr(Buffer, '>');
  ASt             := GetAfterStr(Buffer, '>');
  GetDeviceKey(St1, ADevice, AKey);
  AssignToKey(1, FPrimary, FPrimary.MainKey, ADevice, AKey);

  {First modifier}
  if AnsiPos('Modifier', ASt) > 0 then begin
    St1           := GetBeforStr(ASt, '>');
    ASt           := GetAfterStr(ASt, '>');
    GetDeviceKey(St1, ADevice, AKey);
    AssignToKey(2, FPrimary, FPrimary.Modifier1, ADevice, AKey);
  end;

  {Second modifier}
  if AnsiPos('Modifier', ASt) > 0 then begin
    GetDeviceKey(ASt, ADevice, AKey);
    AssignToKey(3, FPrimary, FPrimary.Modifier2, ADevice, AKey);
  end
end;

procedure TKeyItemInventory.ExtractSecondary;
var
  ASt     : string;
  St1     : string;
  ADevice : string;
  AKey    : string;
begin
  if AnsiPos('Secondary', Buffer) = 0 then Exit;

  ASt             := GetAfterStr(Buffer, '<Secondary');
  FXMLSecondary   := Format('<Secondary %s', [ASt]);
  Buffer          := GetBeforStr(Buffer, '<Secondary');
  {Traitement du secondary}
  St1             := GetBeforStr(Ast, '>');
  ASt             := GetAfterStr(Ast, '>');
  GetDeviceKey(St1, ADevice, AKey);
  AssignToKey(1, FSecondary, FSecondary.MainKey, ADevice, AKey);

  {First modifier}
  if AnsiPos('Modifier', ASt) > 0 then begin
    St1           := GetBeforStr(ASt, '>');
    ASt           := GetAfterStr(ASt, '>');
    GetDeviceKey(St1, ADevice, AKey);
    AssignToKey(2, FSecondary, FSecondary.Modifier1, ADevice, AKey);
  end;

  {Second modifier}
  if AnsiPos('Modifier', ASt) > 0 then begin
    GetDeviceKey(ASt, ADevice, AKey);
    AssignToKey(3, FSecondary, FSecondary.Modifier2, ADevice, AKey);
  end;
end;

procedure TKeyItemInventory.ExtractToggle;
var
  ASt: string;
begin
  if AnsiPos('Toggle', Buffer) = 0 then FToggle := False
   else begin
     FToggle      := True;
     ASt          := GetAfterStr(Buffer, '<Toggle');
     FXMLToggleOn := Format('<Toggle%s', [ASt]);
     Buffer       := GetBeforStr(Buffer, '<Toggle');
     {Extraire la toggle value}
     ASt          := GetAfterStr(ASt, 'Value="');
     FToggleValue := GetBeforStr(ASt, '"');
   end
end;

function TKeyItemInventory.FindFreeCombination(var Key, Func1,
  Func2: TEliteKeyType; var UseTwoFunc: Boolean; var NewSignal: Cardinal): Boolean;
var
  Again    : Boolean;

  function Trans(const Value: TEliteKeyType):SmallInt; begin
    Result := EliteKeyToScanValue( Value )
  end;

begin
  Func1      := Key_LeftAlt;
  Func2      := Key_LeftAlt;
  UseTwoFunc := False;
  Key        := Key_A;
  Again      := True;
  Result     := False;
  while Again and not Application.Terminated do begin
    Key := GetKeyNext(Key);
    if Key = Key_A then begin
      Func1 := GetKeyFuncNext(Func1);
      if Func1 = Key_LeftAlt then begin
        UseTwoFunc := True;
        Func2      := GetKeyFuncNext(Func2);
        if func2 = Key_LeftAlt then begin
          Result := False;
          Break;
        end
      end
    end;
    if UseTwoFunc
      then NewSignal := IdSigning(Trans(Key), Trans(Func1), Trans(Func2))
      else NewSignal := IdSigning(Trans(Key), Trans(Func1), 0);
    Result := FOwner.FHashTable.IndexOf(Format('%d', [NewSignal])) = -1;
    Again := not Result;
  end;
end; {FindFreeCombination}

procedure TKeyItemInventory.GetDeviceKey(const Src: string; var Device,
  Key: string);
begin
  Key    := GetAfterStr(Src,    'Key="');
  Key    := GetBeforStr(Key,    '"');
  Device := GetAfterStr(Src,    'Device="');
  Device := GetBeforStr(Device, '"');
end;

function TKeyItemInventory.GetPrimaryAvailable: Boolean;
begin
  Result := FPrimary.MainKey.Device = '{NoDevice}'
end;

function TKeyItemInventory.GetPrimaryEnable: Boolean;
begin
  Result := IndexStr(FPrimary.MainKey.Device,   ['Keyboard', 'Mouse'] ) > -1
end;

function TKeyItemInventory.GetSecondaryAvailable: Boolean;
begin
  Result := FSecondary.MainKey.Device = '{NoDevice}'
end;

function TKeyItemInventory.GetSecondaryEnable: Boolean;
begin
  Result := IndexStr(FSecondary.MainKey.Device, ['Keyboard', 'Mouse'] ) > -1
end;

function TKeyItemInventory.GetText: string;
var
  s : string;
begin
  with TStringList.Create do
  try
    if FToggle then Add( Format('ToggleOn  Value = %s', [FToggleValue]) );
    if Usable then s := '(peut-être utilisée)' else s := '(Full)';
    Add( Format('%s %s', [Bind,s] ));
    Add( 'Primary');
    if PrimaryAvailable then Add('{NoDevice}');
    if PrimarySignalChecked then begin
      with FPrimary do begin
        Add( Format('  Count  = %d', [Count]) );
        Add( Format('  Device = %s; Key = %s', [MainKey.Device, MainKey.Key]) );
        if Count > 1 then Add( Format('  Modifier Device = %s; Key = %s', [Modifier1.Device, Modifier1.Key]) );
        if Count = 3 then Add( Format('  Modifier Device = %s; Key = %s', [Modifier2.Device, Modifier2.Key]) );
      end;
      Add( '' );
      if PrimaryEnable then begin
        Add( Format('  Key  = %d', [PrimarySignal.Key]) );
        if Count > 1 then Add( Format('  Pri  = %d', [PrimarySignal.Func1]) );
        if Count > 2 then Add( Format('  Sec  = %d', [PrimarySignal.Func2]) );
        Add( Format('  Id   = %d', [PrimaryIdKey]));
        Add( IdKeyToStr( PrimaryIdKey ) );
      end
    end;

    Add( '' );
    Add( 'Secondary');
    if SecondaryAvailable then Add('{NoDevice}');
    if SecondarySignalChecked then begin
      with FSecondary do begin
        Add( Format('  Count  = %d', [Count]) );
        Add( Format('  Device = %s Key; = %s', [MainKey.Device, MainKey.Key]) );
        if Count > 1 then Add( Format('  Modifier Device = %s; Key = %s', [Modifier1.Device, Modifier1.Key]) );
        if Count = 3 then Add( Format('  Modifier Device = %s; Key = %s', [Modifier2.Device, Modifier2.Key]) );
      end;
      Add( '' );
      if SecondaryEnable then begin
        Add( Format('  Key  = %d', [SecondarySignal.Key]) );
        if Count > 1 then Add( Format('  Pri  = %d', [SecondarySignal.Func1]) );
        if Count > 2 then Add( Format('  Sec  = %d', [SecondarySignal.Func2]) );
        Add( Format('  Id   = %d', [SecondaryIdKey]));
        Add( IdKeyToStr( SecondaryIdKey ) );
      end
    end;

    Result := Text
  finally
    Free
  end;
end;

function TKeyItemInventory.GetUsable: Boolean;
begin
  Result := PrimaryAvailable or SecondaryAvailable
end;

function TKeyItemInventory.IdSigning(const Value: TRecordKeySignal): Cardinal;
begin
  with Value do Result := IdSigning(Key, Func1, Func2)
end;

function TKeyItemInventory.IsCombinationJoyBusy: Boolean;
begin
  Result := IsJoyUsed(FPrimary) and IsJoyUsed(FSecondary)
end;

function TKeyItemInventory.IsCombinatonFree: Boolean;
var
  Indic1 : Boolean;
  Indic2 : Boolean;
begin
  Result := (FPrimary.MainKey.Device   = '{NoDevice}') and
            (FSecondary.MainKey.Device = '{NoDevice}');
  Indic1 := IsJoyUsed(FPrimary) and (FSecondary.MainKey.Device = '{NoDevice}');
  Indic2 := (FSecondary.MainKey.Device = '{NoDevice}') and IsJoyUsed(FSecondary);
  Result := Result or Indic1 or Indic2;
end;

function TKeyItemInventory.IsJoyUsed(const Value: TRecordKeySection): Boolean;
begin
  Result := IndexStr(Value.MainKey.Device, ['Keyboard', 'Mouse', '{NoDevice}']) = -1
end;

function TKeyItemInventory.ReplaceSourceXML:string;
var
  BefBuffer : string;
  AftBuffer : string;
begin
  with FOwner do begin
    BefBuffer := GetBeforStr(FSource, Format('<%s>',  [Bind]));
    AftBuffer := GetAfterStr(FSource, Format('</%s>', [Bind]));
    with TStringList.Create do
    try
      Text := BefBuffer;
      Add( Format(#9'%s', [XMLIndentation]));
      Add( Format(#9'%s', [AftBuffer]));
      FSource := Text;
      Result  := Text;
      SaveToFile( TEMP_CUSTOM_FILE );
      SetCustomModified;
    finally
      Free
    end;
  end;

end; {ReplaceSourceXML}

function TKeyItemInventory.IdSigning(const Key, Mod1, Mod2: Word): Cardinal;
begin
  Result := EncodeKey(Key, Mod1, Mod2)
end;

procedure TKeyItemInventory.SetXmlSource(const Value: string);
begin
  with FOwner do ErrorRemove( Self.FBind );
  FXmlSource := Value;
  ExtractData
end;

function TKeyItemInventory.XMLIndentation: string;
var
  i : Integer;
  S : string;

  function CleanStr(const ASt: string):string; var i: Integer; begin
  { --- Retirer les caractères "tab" avant d'imposer l'indentation }
    Result := EmptyStr;
    for i := 1 to Length(ASt) do
      if ASt[i] <> #9 then Result := Result + ASt[i]
  end;

begin
  with TStringList.Create do
  try
    Text := FXmlSource;
    for i := 0 to Pred(Count) do begin
      S := Strings[i];
      if (AnsiPos('Primary', S) > 0) or (AnsiPos('Secondary', S) > 0)
        then Strings[i] := Format(#9#9'%s', [ CleanStr(S) ])
        else
      if AnsiPos('Modifier', S) > 0
        then Strings[i] := Format(#9#9#9'%s', [ CleanStr(S) ])
        else
      Strings[i] := Format(#9#9'%s', [ CleanStr(S) ])
    end;
    Insert(0, Format(#9'<%s>', [Bind]));
    Add( Format(#9'</%s>', [Bind]));
    Result := Trim(Text);
  finally
    Free
  end;
end; {XMLIndentation}

{ TKeyInventory }

function TKeyInventory.AddKey(const Binds: string): Integer;
var
  Item: TKeyItemInventory;
begin
  Item   := TKeyItemInventory.Create(Self);
  with Item do begin
    Bind      := Binds;
    XMLSource := TBindsArrayTools.Extract(FSource, Binds);
  end;
  Result := AddObject(Binds, Item);
end;

function BuildXMLKey(const BaliseName: string; Key, Func1, Func2: string;
  UseTwoFunc: Boolean; KeyOnly: Boolean):string;
var
  Device: string;
begin
  if IndexStr(Key, ['Mouse_1', 'Mouse_2', 'Mouse_3', 'Mouse_4', 'Mouse_5']) > -1
    then Device := 'Mouse'
    else Device := 'Keyboard';

  with TStringList.Create do
  try
    if KeyOnly then begin
      Add( Format('<%s Device="%s" Key="%s"/>', [BaliseName, Device, Key]) );
    end else begin
      Add( Format('<%s Device="Keyboard" Key="%s">', [BaliseName, Key]) );
        Add( Format(#9'<Modifier Device="Keyboard" Key="%s"/>', [Func1]) );
      if UseTwoFunc then
        Add( Format(#9'<Modifier Device="Keyboard" Key="%s"/>', [Func2]) );
      Add( Format('</%s>', [BaliseName]) );
    end;
    Result := Text;
  finally
    Free
  end;
end;

procedure TKeyInventory.AssignToFree;
var
  i          : Integer;
  AItem      : TKeyItemInventory;
  Key        : TEliteKeyType;
  Func1      : TEliteKeyType;
  Func2      : TEliteKeyType;
  UseTwoFunc : Boolean;
  NewSignal  : Cardinal;
begin
  { --- Parcourir et traiter les items de l'inventaire qui sont en erreur }
  with FErrors do for i := Pred(Count) downto 0 do begin
    AItem := Item[ Trim(Names[i]) ];
    if Assigned(AItem) then with AItem do begin
      { --- Déterminer une nouvelle combinaison de touches }
      if not FindFreeCombination(Key, Func1, Func2, UseTwoFunc, NewSignal)
        then begin
          FErrors.Text := '001 - No key combination available';
          raise Exception.Create(FErrors.Text);
        end;

      if SecondaryAvailable then begin
        { --- reconstruire la FXLMSecondary }
        FXMLSecondary := BuildXMLKey('Secondary', KeyTypeToStr(Key),
          KeyTypeToStr(Func1), KeyTypeToStr(Func2), UseTwoFunc, False );
        FId2 := NewSignal;
      end else

      if PrimaryAvailable then begin
        { --- reconstruire la FXLMSecondary }
        FXMLPrimary := BuildXMLKey('Primary', KeyTypeToStr(Key),
          KeyTypeToStr(Func1), KeyTypeToStr(Func2), UseTwoFunc, False );
        FId1 := NewSignal;
      end;

      { --- Réinitialise la keyItem courante }
      SetXmlSource( ConcatXML );
      { --- Modifier le fichier source }
      ReplaceSourceXML;
    end
  end
end;


procedure TKeyInventory.BuildLocalSaveFolder;
begin
  if not DirectoryExists(LOCAL_SAVE) then
    try MkDir(LOCAL_SAVE) except end;
end;

procedure TKeyInventory.BuildMutiUsageKeyList;
var
  i     : Integer;
  Datas : string;
  s     : string;
begin
  FDblUsed.Clear;
  with FHashTable do for i := 0 to Pred(Count) do
    if RetrieveMultiUsageByKey(Strings[i], Datas) > 1 then begin
      s := Strings[i];
      FDblUsed.Add( Format('%s=%s', [s, ListToSemicolumnStr(Datas)]) )
    end
end;

function TKeyInventory.BuildXMLForCombination(const Order: TKeyOrder;
  const IdKey: Cardinal): string;
var
  Key, Func1, Func2: Word;
begin
  DecodeKey(IdKey, Key, Func1, Func2);
  Result := BuildXMLForCombination(Order, Key, Func1, Func2)
end;

function TKeyInventory.BuildXMLForCombination(const Order: TKeyOrder;
  const Key, Func1, Func2: Word): string;
var
  Balise     : string;
  UseTwoFunc : Boolean;
  KeyOnly    : Boolean;
begin
  case Order of
    ko_primary   : Balise := 'Primary';
    ko_secondary : Balise := 'Secondary';
  end;
  KeyOnly    := (Func1 = 0) and (Func2 = 0);
  UseTwoFunc := (Func1 <> 0) and (Func2 <> 0);

  Result := BuildXMLKey(Balise, KeyScanToEliteString(Key),
    KeyScanToEliteString(Func1),
    KeyScanToEliteString(Func2), UseTwoFunc, KeyOnly );
end;



procedure TKeyInventory.CheckDupicataBinds(const Dup: TKeyDuplicataType; const RefBind, ToBind: string);
{on doit identifier dans le catalog le IdKey pour ItemA
    - s'il n'existe pas alors le créer puis mettre à jour ItemA
    - Retrouver dans ItemA la conbinaison Primary ou Secondary
    - mettre à jour ItemB
        * Trouver un emplacement libre Primary ou secondary (le premier Keyboard rencontré ou secondary s'il est libre)
        * Mettre à jour FXMLPrimary ou FXMLSecondary   ok
        * recalculer le XMLSource puis le réattribuer
  }
var
  ItemA, ItemB   : TKeyItemInventory;
  Order          : TKeyOrder;
  AOrder         : TKeyOrder;
  IdKey          : Cardinal;
  Key            : TEliteKeyType;
  Func1          : TEliteKeyType;
  Func2          : TEliteKeyType;
  UseTwoFunc     : Boolean;
  FirstBindValue : string;
  Modified       : Boolean;

  function FindFreeOrder(const Item: TKeyItemInventory):TKeyOrder; begin
    if Item.SecondaryEnable then Result := ko_secondary
      else
    if Item.PrimaryEnable then Result := ko_primary
      else
    if Item.SecondaryAvailable then Result := ko_secondary
      else
    if Item.PrimaryAvailable then Result := ko_primary
      else begin
        FErrors.Text := Format('002 - %s : delete one of the joystick or mouse inputs', [Item.FBind]);
        raise Exception.Create(FErrors.Text);
      end;
  end;

  procedure DoIfAvailaible; begin
    { --- Choisir la valeur de référence }
    IdKey := PreferenceKeys[Dup];
    { --- Si la valeur de référence est déjà utilisée alors en définir une nouvelle }
    if not isFreeCombination(IdKey) and not HasReference(Dup, FirstBindValue) then
      if not ItemA.FindFreeCombination(Key, Func1, Func2, UseTwoFunc, IdKey)
        then begin
          FErrors.Text := Format('003 - %s No key combination available', [ItemA.Bind]);
          raise Exception.Create(FErrors.Text);
        end;
    { --- Mettre à jour le XML selon l'order choisi }
    with ItemA do begin
      case AOrder of
        ko_primary   : FXMLPrimary   := BuildXMLForCombination(ko_primary,   IdKey);
        ko_secondary : FXMLSecondary := BuildXMLForCombination(ko_secondary, IdKey);
      end;
      { --- Réinitialise la keyItem courante }
      SetXmlSource( ConcatXML );
      { --- Modifier le fichier source }
      ReplaceSourceXML;
      SetMultiUsageModified( True )
    end;
  end;

  procedure CheckReference; begin
    Order := FindFreeOrder(ItemB);
    {TODO mettre à jour ItemA voire lui créer une combinaison }
    AOrder := FindFreeOrder(ItemA);
    with ItemA do case AOrder of
      ko_primary   : if PrimaryAvailable   then DoIfAvailaible;
      ko_secondary : if SecondaryAvailable then DoIfAvailaible;
    end;
  end;

  procedure Initialize; begin
    ItemA := Item[RefBind];
    ItemB := Item[ToBind];
    if not Assigned(ItemA) or not Assigned(ItemB) then begin
      FErrors.Text := Format('004 - %s vs %s : Incorrect instantiation of doublets', [RefBind, ToBind]);
      raise Exception.Create(FErrors.Text);
    end;
    CheckReference;
  end;

  function priVSpri: Boolean; begin
    Result := ItemB.FId1 <> ItemA.FId1;
    if Result then ItemB.FXMLPrimary := ItemA.FXMLPrimary
  end;

  function priVSsec: Boolean; begin
    Result := ItemB.FId1 <> ItemA.FId2;
    if Result then         { --- changer la balise Secondary en Primary }
      ItemB.FXMLPrimary := OrderExchange(ItemA.FXMLSecondary, ko_secondary)
  end;

  function secVSpri: Boolean; begin
    Result := ItemB.FId2 <> ItemA.FId1;
    if Result then           { --- changer la balise Primary en Secondary }
      ItemB.FXMLSecondary := OrderExchange(ItemA.FXMLPrimary, ko_primary)
  end;

  function secVSsec: Boolean; begin
    Result := ItemB.FId2 <> ItemA.FId2;
    if Result then ItemB.FXMLSecondary := ItemA.FXMLSecondary
  end;

  procedure Update; begin
    Modified := False;
    case Order of
      ko_primary   :
        case AOrder of
          ko_primary   : Modified := priVSpri;
          ko_secondary : Modified := priVSsec;
        end;
      ko_secondary :
        case AOrder of
          ko_primary   : Modified := secVSpri;
          ko_secondary : Modified := secVSsec;
        end;
    end;
    with ItemB do if Modified then begin
      { --- Réinitialise la keyItem courante }
      SetXmlSource( ConcatXML );
      { --- Modifier le fichier source }
      ReplaceSourceXML;
      SetMultiUsageModified( True )
    end
  end;

begin
  Initialize;
  Update;
end;

procedure TKeyInventory.Copy_locally;
var
  FromFile : string;
  ToFile   : string;
begin
  FromFile := Format('%s\%s', [GetEliteBindingsFolder, CUSTOM_FILE]);
  ToFile   := Format('%s\%s', [ExtractFileDir(Application.ExeName), TEMP_CUSTOM_FILE]);
  CopyFile(PChar(FromFile), PChar(ToFile), False);
end;

{CheckDupicataBinds}

constructor TKeyInventory.Create;
begin
  inherited Create;
  { --- Sorted pour le gain de rapidité lors d'une recherche par indexof }
  FCatalog       := TStringList.Create;
  with FCatalog do Sorted := True;
  FIndexCatalog  := TStringList.Create;
  with FIndexCatalog do Sorted := True;
  FIgnored       := TstringList.Create;
  FErrors        := TstringList.Create;
  FDblUsed       := TstringList.Create;
  FHashTable     := TstringList.Create;
  with FHashTable do Sorted := True;
end;

function TKeyInventory.Custom3Exists: Boolean;
begin
  Result := FileExists(Format('%s\%s', [GetEliteBindingsFolder, CUSTOM_FILE]))
end;

destructor TKeyInventory.Destroy;
begin
  FCatalog.Free;
  FIndexCatalog.Free;
  FIgnored.Free;
  FErrors.Free;
  FDblUsed.Free;
  FHashTable.Free;
  inherited;
end;

procedure TKeyInventory.ErrorRemove(const ForBind: string);
var
  index: Integer;
begin
  with FErrors do begin
    index := IndexOfName(ForBind);
    if index > -1 then Delete(index)
  end
end;

class procedure TKeyInventory.Finalize;
begin
  KeyInventory.Finalize_;
  FreeAndNil( KeyInventory );
end;

procedure TKeyInventory.Finalize_;
var
  i : Integer;
begin
  for I := Pred(Count) downto 0 do Objects[i].Free;
  Clear;
end;

function TKeyInventory.GetCatalogTxt: string;
begin
  Result := FCatalog.Text
end;

function TKeyInventory.GetDoublons: string;
begin
  Result := FDblUsed.Text
end;

function TKeyInventory.GetErrors: string;
begin
  Result := FErrors.Text
end;

function TKeyInventory.GetHashTxt: string;
begin
  Result := FHashTable.Text
end;

function TKeyInventory.GetIndexedCatalog: string;
begin
  Result := FIndexCatalog.Text
end;

function TKeyInventory.GetItem(Bind: string): TKeyItemInventory;
var
  index: Integer;
begin
  Result := nil;
  index  := Self.IndexOf(Bind);
  if index > -1 then Result := TKeyItemInventory( Objects[index] )
end;

function TKeyInventory.GetIdFromCatalog(const ASt: string): Cardinal;
var
  index: Integer;
begin
  Result := 0;
  index  := FCatalog.IndexOfName(ASt);
  if index > -1 then
    try
      Result := StrToInt( FCatalog.ValueFromIndex[index] );
    except
      Result := 0
    end
end;

function TKeyInventory.GetTextEx(Bind: string): string;
var
  index: Integer;
begin
  try
    index := IndexOf(Bind);
    if index > -1 then Result := TKeyItemInventory(Objects[index]).Text;
  except
    Result := EmptyStr
  end;
end;

function TKeyInventory.GetXML(Bind: string): string;
var
  index: Integer;
begin
  try
    index := IndexOf(Bind);
    if index > -1 then Result := TKeyItemInventory(Objects[index]).XMLSource;
  except
    Result := EmptyStr
  end;
end;

function TKeyInventory.HasReference(const Dup: TKeyDuplicataType;
  var FirstBindValue: string): Boolean;
var
  Count  : Integer;
  i      : Integer;
  Target : Cardinal;
  ItemX  : TKeyItemInventory;
  ASt    : string;
begin
  Result         := False;
  Target         := PreferenceKeys[Dup];
  Count          := DuplicataItemsCount(Dup);
  FirstBindValue := EmptyStr;
  for I := Low(TArrayDuplicata) to Count + Low(TArrayDuplicata) - 1 do begin
    ASt   := DuplicataItem(Dup, I);
    ItemX := Item[ASt];
    if not Assigned(ItemX) then begin
      FErrors.Text := Format('005 - %s : uninstantiated object', [ASt]);
      raise Exception.Create(FErrors.Text);
    end;
    if (ItemX.FId1 = Target) or (ItemX.FId2 = Target) then begin
      FirstBindValue := ASt;
      Result         := True;
      Break;
    end
  end
end;
class procedure TKeyInventory.Initialize;
begin
  if not Assigned(KeyInventory) then begin
    KeyInventory := TKeyInventory.Create;
    with KeyInventory do Initialize_
  end
end;

procedure TKeyInventory.Initialize_;
begin
  { --- Si une erreur survient alors pas d'initialisation
        On doit impérativement refermer l'applicatif }
  Pre_Initilialization;

  FSource    := ReadSource;
  FSourceAlt := FSource;
  with TStringList.Create do
  try
    try
      BuildLocalSaveFolder;
      ResetBuffers;
      Text := TBindsArrayTools.AllButtonBalises(FSource, True);
      with GetEnumerator do
      try
        while MoveNext do AddKey( Current )
      finally
        Free
      end;
      { *** BuildMutiUsageKeyList; Utile en conception
            --> permet de construire la liste des combinaisons partagées }
      RetrieveModifications
    except
    end;
    UpdateValidation
  finally
    Free
  end
end;

function TKeyInventory.IsCustomEnabled: Boolean;
var
  SrcFileName : string;
begin
  Result      := False;
  SrcFileName := Format('%s\%s', [GetEliteBindingsFolder, START_PRESET]);
  if FileExists(SrcFileName) then with TStringList.Create do
  try
    LoadFromFile( SrcFileName );
    Result := AnsiPos('Custom', Text) > 0
  finally
    Free
  end;
end;

function TKeyInventory.IsCustomModified: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'Custom3.0.binds Modified')
end;

function TKeyInventory.IsFirstTimeUse: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'FirstTimeUse', True)
end;

function TKeyInventory.isFreeCombination(const Key, Func1,
  Func2: Word): Boolean;
begin
  Result := isFreeCombination( EncodeKey(Key, Func1, Func2) )
end;

function TKeyInventory.IsMultiUsageModified: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'MultiUsageModified')
end;

function TKeyInventory.IsSynchronization: Boolean;
begin
  Result := KeyReadBoolean(AppKey, 'Synchronization')
end;

function TKeyInventory.isFreeCombination(const IdKey: Cardinal): Boolean;
begin
  Result := FHashTable.IndexOf( Format('%d', [IdKey])) = -1
end;

procedure TKeyInventory.KeyTrigger_(const BindStr: string; UpKey: Boolean);
var
  IdStr : Cardinal;
begin
  { --- Retrouver l'item correspondant dans le catalog et extraire l'idkey }
  IdStr := GetIdFromCatalog(BindStr);
  { --- Selon la commande et le contexte définir le temps d'appui de la touche }

  if IdStr > 0 then TKeyMessageSender.Signal(IdStr, 30, UpKey);
end;

procedure TKeyInventory.MultiUsageAssign;
var
  i,j     : Integer;
  X       : TKeyDuplicataType;
  Count   : Integer;
  BindRef : string;
begin
  SetMultiUsageModified(False);
  for i := Low(TKeyDuplicataArea) to High(TKeyDuplicataArea) do begin
    X     := TKeyDuplicataType(i);
    Count := DuplicataItemsCount(X);
    for j := Low(TArrayDuplicata) to Count + Low(TArrayDuplicata) - 1 do begin
      if j = Low(TArrayDuplicata) then BindRef := DuplicataItem(X, j)
        else CheckDupicataBinds(X, BindRef, DuplicataItem(X, j))
    end
  end;
end;

function TKeyInventory.Pre_Initilialization:Boolean;
var
  StMessage: string;
begin
  try
    StMessage := SetMessage([
      'Elite Dangerous commands settings : Please choose CUSTOM.',
      '',
      'Raoul will not be able to assist you with Elite Dangerous',
      'until you solve this problem.'
    ]);
    if not Try_Copy_locally then
      raise Exception.Create(StMessage)
    else
      { --- Si Custom n'est pas actif alors erreur }
      if not IsCustomEnabled then raise Exception.Create(StMessage);
    Result := True
  except
    raise
  end;
end;

function TKeyInventory.ReadSource: string;
begin
  with TStringList.Create do
  try
    LoadFromFile( TEMP_CUSTOM_FILE );
    Result := Text;
  finally
    Free
  end
end;

class procedure TKeyInventory.Refresh;
begin
  KeyInventory.Refresh_;
end;

procedure TKeyInventory.Refresh_;
begin
  Finalize_;
  Initialize_
end;

procedure TKeyInventory.ReplaceAndSaveCustom30;
var
  FromFile, ToFile: string;
begin
  { --- Enregistrer une version de l'original Custom3.0.binds tagé avec la date d'opération }
  FromFile := Format('%s\%s',    [GetEliteBindingsFolder, CUSTOM_FILE]);
  ToFile   := Format('%s\%s_%s', [LOCAL_SAVE, CUSTOM_FILE, EncodeTagDate(Now)]);
  CopyFile(PChar(FromFile), PChar(ToFile), False);
  { --- Remplacer le fichier actif par le fichier nouvellement construit }
  ToFile   := FromFile;
  FromFile := Format('%s', [ TEMP_CUSTOM_FILE ]);
  CopyFile(PChar(FromFile), PChar(ToFile), False);
end;

procedure TKeyInventory.ResetBuffers;
begin
  FCatalog.Clear;
  FIndexCatalog.Clear;
  FErrors.Clear;
  FDblUsed.Clear;
  FHashTable.Clear;
  FIgnored.Text := TBindsArrayTools.AllButtonBalises(FSource, False);
end;

procedure TKeyInventory.ResetCustomModification;
begin
  KeyWrite(AppKey, 'Custom3.0.binds Modified', False);
end;

procedure TKeyInventory.RestoreAltSource;
begin
  with TStringList.Create do
  try
    Text := FSourceAlt;
    SaveToFile( TEMP_CUSTOM_FILE );
  finally
    Free
  end
end;

procedure TKeyInventory.RetrieveModifications;
begin
  { --- Initialiser la valeur de suivi des modifications dans le registre }
  ResetCustomModification;
  MultiUsageAssign;
  AssignToFree;
end;

function TKeyInventory.RetrieveMultiUsageByKey(const AKey: string;
  var Datas: string): Integer;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    for i := 0 to Pred(FCatalog.Count) do
      if FCatalog.ValueFromIndex[i] = AKey then Add( Format('%s', [FCatalog.Names[i]]) );
    Datas  := Trim( Text );
    Result := Count;
  finally
    Free
  end
end;

procedure TKeyInventory.SetCustomModified;
begin
  KeyWrite(AppKey, 'Custom3.0.binds Modified', True)
end;

procedure TKeyInventory.SetFirstTimeUse(const Value: Boolean);
begin
  KeyWrite(AppKey, 'FirstTimeUse', Value);
end;

procedure TKeyInventory.SetMultiUsageModified(const Value: Boolean);
begin
  KeyWrite(AppKey, 'MultiUsageModified', Value)
end;

procedure TKeyInventory.SetSource(Value: string);
begin
  FSource := Value
end;

procedure TKeyInventory.SetSynchronization(const Value: Boolean);
begin
  KeyWrite(AppKey, 'Synchronization', Value)
end;

function TKeyInventory.Try_Copy_locally: Boolean;
begin
  Result := True;
  if Custom3Exists then Copy_locally else Result := False
end;

procedure TKeyInventory.UpdateValidation;
begin
  if FErrors.Count > 0 then
    raise Exception.CreateFmt('Failure during link operation'#10#13'%s', [ErrorsTxt])
  else
  { --- Si des modifications ont été apportées }
  if IsCustomModified then begin
    { --- On impose la synchronisation du Temp.Custom.3.0 avec le Custom.3.0 }
    SetSynchronization(True);
    { --- Vérifier qu'Elite n'est pas en cours d'exécution
          * si le cas s'avère alors lever une exception }
    if IsEliteRunning then begin
        RestoreAltSource;
        raise Exception.Create('Elite is running, please stop it')
    end else begin
        if IsSynchronization or IsFirstTimeUse then begin
        { --- Recopier le fichier obtenu à la place du custom existant et
              sauver une copie de l'ancien fichier en le tagant }
          ReplaceAndSaveCustom30;
          { --- Désactiver la première synchronisation }
          SetFirstTimeUse    ( False );
          { --- Désactiver jusqu'à la prochaine demande de synchronisation }
          SetSynchronization ( False );
        end
      end
  end else begin
    { --- First time et aucune modification on valide le first time }
    if IsFirstTimeUse    then SetFirstTimeUse    ( False );
    if IsSynchronization then SetSynchronization ( False );
  end
end;

procedure TKeyInventory.KeyTrigger_(const BindStr: string; pDelay: Integer);
var
  IdStr : Cardinal;
begin
  { --- Retrouver l'item correspondant dans le catalog et extraire l'idkey }
  IdStr := GetIdFromCatalog(BindStr);
  { --- Selon la commande et le contexte définir le temps d'appui de la touche }

  if IdStr > 0 then TKeyMessageSender.Signal(IdStr, pDelay, WITH_KEYUP);
end;

{ TKeyMessageSender }

class procedure TKeyMessageSender.AfterMouseDown(const Value: TNotifyEvent);
begin
  if Assigned(KeyMessageSender) then with KeyMessageSender do
    SetOnAfterMouseDown( Value )
end;

class procedure TKeyMessageSender.BeforeMouseDown(const Value: TNotifyEvent);
begin
  if Assigned(KeyMessageSender) then with KeyMessageSender do
    SetOnBeforeMouseDown( Value )
end;

constructor TKeyMessageSender.Create;
begin
  inherited Create;
  FDown         := TStringList.Create;
  FMouseFactory := TMouseFactory.Create;
  FKeySurveyor  := nil
end;

destructor TKeyMessageSender.Destroy;
begin
  { --- On s'assure que les touches sont relâchées }
  DoKeyUp;
  FDown.Free;
  FMouseFactory.Free;
  inherited;
end;

procedure TKeyMessageSender.DoAfterMouseSignal;
begin
  if Assigned(FOnAfterMouseDown) then FOnAfterMouseDown(Self);
end;

procedure TKeyMessageSender.DoBeforeMouseSignal;
begin
  if Assigned(FOnBeforeMouseDown) then FOnBeforeMouseDown(Self);
end;

procedure TKeyMessageSender.DoKeyUp;
var
  i          : Integer;
  Key        : Word;
  MapVirtual : Cardinal;
  MeUp       : Word;
  MButtonEx  : Word;

  procedure KeyReleased; begin
    MapVirtual := MapVirtualKey(Key, 0);
    if Key in ExtendedVKeys
      then keybd_event(Key, MapVirtual, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0)
      else keybd_event(Key, MapVirtual, KEYEVENTF_KEYUP, 0);
  end;

  procedure MouseReleased; begin
    MEUp := MouseSignalToEvent(FKey, True, MButtonEx);
    mouse_event(MEUp, 0, 0, MButtonEx, 0)
  end;

begin
  with FDown do if Count > 0 then begin
    for i := Pred(Count) downto 0 do begin
      { --- non contrôlé car ne peut-être qu'un word transformé en string }
      Key        := StrToInt( Strings[i] );
      case Key of
        1,2,4,5,6 : MouseReleased;
        else KeyReleased;
      end;
      { --- Supprime l'entrée une fois traité }
      Delete(i)
    end;
    FClock := GetTickCount
  end
end; {DoKeyUp}

class procedure TKeyMessageSender.Finalize;
begin
  if Assigned(KeyMessageSender) then FreeAndNil(KeyMessageSender)
end;

function TKeyMessageSender.GetIndexMonitor: Integer;
begin
  Result := KeyReadInt(AppKey, 'EliteMonitor')
end;

class procedure TKeyMessageSender.Initialize;
begin
  if not Assigned(KeyMessageSender) then
    KeyMessageSender := TKeyMessageSender.Create
end;

class procedure TKeyMessageSender.KeyUp;
begin
  if Assigned(KeyMessageSender) then with KeyMessageSender do
    DoKeyUp
end;

function TKeyMessageSender.MouseSignalToEvent(const Value: Word;
  UpSignal: Boolean; var Button: Word): Word;
begin
  Button := 0;
  case Value of
    1 : if UpSignal then Result := MOUSEEVENTF_LEFTUP   else Result := MOUSEEVENTF_LEFTDOWN;
    2 : if UpSignal then Result := MOUSEEVENTF_RIGHTUP  else Result := MOUSEEVENTF_RIGHTDOWN;
    4 : if UpSignal then Result := MOUSEEVENTF_MIDDLEUP else Result := MOUSEEVENTF_MIDDLEDOWN;
    5 : begin
          if UpSignal then Result := MOUSEEVENTF_XUP else Result := MOUSEEVENTF_XDOWN;
          Button := XBUTTON1
        end;
    6 : begin
          if UpSignal then Result := MOUSEEVENTF_XUP else Result := MOUSEEVENTF_XDOWN;
          Button := XBUTTON2
        end;
    else Result := 0;
    if Result = 0 then raise Exception.Create('MouseSignalToEvent : result = 0');
  end;
end; {MouseSignalToEvent}

procedure TKeyMessageSender.SendSignal(UpKey: Boolean);
var
  MapVirtual0  : Cardinal;
  MapVirtual1  : Cardinal;
  MapVirtual2  : Cardinal;
  MButtonEx    : Word;
  MEDown       : Word;
  MEUp         : Word;
  KeyCar       : Char;


  procedure SendSingleToKeyboard; begin
    { --- KeyDown }
    if FKey in ExtendedVKeys
      then keybd_event(Ord(KeyCar), MapVirtual0, KEYEVENTF_EXTENDEDKEY, 0)
      else keybd_event(Ord(KeyCar), MapVirtual0, KEYEVENTF_KEYDOWN,     0);
    { --- KeyUp }
    if UpKey then begin
      Sleep( TimeBeforeUpKey );
      if FKey in ExtendedVKeys
        then keybd_event(Ord(KeyCar), MapVirtual0, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0)
        else keybd_event(Ord(KeyCar), MapVirtual0, KEYEVENTF_KEYUP,                          0)
    end else FDown.Add( Format('%d', [FKey]) )
  end;

  procedure SendSingleToMouse; begin
    MEDown := MouseSignalToEvent(FKey, False,  MButtonEx);
    MEUp   := MouseSignalToEvent(FKey, True,   MButtonEx);
    { --- Prepare }
    DoBeforeMouseSignal;
    { --- MouseDown }
    mouse_event(MEDown, 0, 0, MButtonEx, 0);
    if UpKey then begin
      Sleep( TimeBeforeUpKey );
      mouse_event(MEUp, 0, 0, MButtonEx, 0);
    end else FDown.Add( Format('%d', [FKey]) );
    { --- Finalize Mouse signal }
    DoAfterMouseSignal;
  end;

  { --- Aucune touche de fonction dans la combinaison de touches }
  procedure SendSingle; begin
    case FKey of
      1,2,4,5,6 : SendSingleToMouse;
      else SendSingleToKeyboard;
    end;
    FClock := GetTickCount
  end;

  { --- Une seule touche de fonction dans la combinaison de touches }
  procedure SendBoth; begin
    if FFunc1 in ExtendedVKeys
      then keybd_event(FFunc1, MapVirtual1, KEYEVENTF_EXTENDEDKEY, 0)
      else keybd_event(FFunc1, MapVirtual1, KEYEVENTF_KEYDOWN, 0);
    SendSingle;
    if UpKey then begin
      if FFunc1 in ExtendedVKeys
        then keybd_event(FFunc1, MapVirtual1, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0)
        else keybd_event(FFunc1, MapVirtual1, KEYEVENTF_KEYUP, 0)
    end  else FDown.Add( Format('%d', [FFunc1]) )
  end;

  { --- Deux touches de fonction dans la combinaison de touches }
  procedure SendFull; begin
    if FFunc2 in ExtendedVKeys
      then keybd_event(FFunc2, MapVirtual2, KEYEVENTF_EXTENDEDKEY, 0)
      else keybd_event(FFunc2, MapVirtual2, KEYEVENTF_KEYDOWN, 0);
    SendBoth;
    if UpKey then begin
      if FFunc2 in ExtendedVKeys
      then keybd_event(FFunc2, MapVirtual2, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0)
      else keybd_event(FFunc2, MapVirtual2, KEYEVENTF_KEYUP, 0)
    end else FDown.Add( Format('%d', [FFunc2]) )
  end;

  procedure Initialize; begin
    { --- Avant une nouvelle combinaison on s'assure que les touches sont relâchées }
    DoKeyUp;
    { --- Initialisation des virtualkey map }
    KeyCar := Chr(FKey mod 256);
    if not (FKey in [1,2,4,5,6]) then begin
      MapVirtual0 := MapVirtualKey(Ord(KeyCar),   0);
      MapVirtual1 := MapVirtualKey(Ord(FFunc1),   0);
      MapVirtual2 := MapVirtualKey(Ord(FFunc2),   0);
    end
  end;

begin
  { --- Si FFunc = 0 alors on ignore volontairement FFunc2 }
  Initialize;
  case FFunc1 of
    0 : SendSingle;
    else begin
      case FFunc2 of
        0 : SendBoth;
        else SendFull
      end;
    end
  end;
  Sleep(90);
end; {SendSignal}

procedure TKeyMessageSender.SetIdKey(const Value: Cardinal);
begin
  FIdKey := Value;
  DecodeKey(FIdKey, FKey, FFunc1, FFunc2);
  with FMouseFactory do begin
    BeforeMouseDown  ( DoBeforeMouseDown );
    AfterMouseDown   ( DoAfterMouseDown  )
  end;
  StartSurveyor;
end;

procedure TKeyMessageSender.SetIndexMonitor(const Value: Integer);
begin
  KeyWrite(AppKey, 'EliteMonitor', Value)
end;

procedure TKeyMessageSender.SetOnAfterMouseDown(const Value: TNotifyEvent);
begin
  FOnAfterMouseDown := Value
end;

procedure TKeyMessageSender.SetOnBeforeMouseDown(const Value: TNotifyEvent);
begin
  FOnBeforeMouseDown := Value
end;

procedure TKeyMessageSender.SetTimems(const Value: Cardinal);
begin
  FTimems := Value
end;

class procedure TKeyMessageSender.Signal(const KeyValue, TimeMs: Cardinal;
  UpKey: Boolean);
begin
  if Assigned(KeyMessageSender) then with KeyMessageSender do begin
    SetIdKey   ( KeyValue );
    SetTimems  ( TimeMs   );
    SendSignal ( UpKey    );
  end
end;

procedure TKeyMessageSender.StartSurveyor;
begin
  FSurveyor := TKeySurveyor.Create(Self);
  FSurveyor.Start; //Resume;
end;

function TKeyMessageSender.TimeBeforeUpKey: Cardinal;
begin
  Result := FTimems
end;

{ TMouseFactory }

constructor TMouseFactory.Create;
begin
  inherited Create;
  RetrieveIndexMonitor
end;

procedure TMouseFactory.DoAfterMouseDown(Sender: TObject);
begin
  with FMousePoint do SetCursorPos(X, Y);
end;

procedure TMouseFactory.DoBeforeMouseDown(Sender: TObject);
begin
  GetCursorPos( FMousePoint );
  {TODO: gérer le moniteur où tourne Elite}
  with Screen.Monitors[IndexMonitor] do SetCursorPos(Left + 300, Top + 200);
  EliteForeGround;
end;

function TMouseFactory.GetIndexMonitor: Integer;
begin
  Result := KeyReadInt(AppKey, 'EliteMonitor')
end;

procedure TMouseFactory.RetrieveIndexMonitor;
var
  DisplaySettings : string;
  Buffer          : string;
begin
  DisplaySettings := Format('%s\%s', [GetEliteGraphicsFolder, DISPLAY_SETTINGS]);
  if FileExists(DisplaySettings) then with TStringList.Create do
  try
    LoadFromFile( DisplaySettings );
    Buffer := GetAfterStr(Text,   '<Monitor>'  );
    Buffer := GetBeforStr(Buffer, '</Monitor>' );
    try
      IndexMonitor := StrToInt( Buffer );
    except
    end;
  finally
    Free
  end
end;

procedure TMouseFactory.SetIndexMonitor(const Value: Integer);
begin
  KeyWrite(AppKey, 'EliteMonitor', Value)
end;

{ TKeySurveyor }

constructor TKeySurveyor.Create(const AKeyMessager: TKeyMessageSender);
begin
  Inherited Create( True );
  { --- Must be started }
  ThKeyMessageSender := AKeyMessager;
  FreeOnTerminate := True;
end;

procedure TKeySurveyor.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 2000 )
  end
end;

procedure TKeySurveyor.Process;
begin
  { --- 60 seconds for key down before automatic key up }
  with ThKeyMessageSender do if GetTickCount - Clock > 60000 then DoKeyUp
end;

procedure TKeySurveyor.ThDelay(ms: Cardinal);
var
  S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do repeat
      Sleep( 10 );
  until Self.Terminated or Terminated or (GetTickCount > S)
end;

initialization
  DuplicatasInitialize;
  PreferenceKeysInitialize;
end.
