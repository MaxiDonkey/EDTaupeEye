{*******************************************************}
{                                                       }
{             08/2020  MaxiDonkey  Library              }
{                                                       }
{*******************************************************}

unit uStatusReader;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, StrCopyUtils;

const
  MaxShl = 62;

type
  TGuiType =
    (gt_nofocus,      gt_internalpanel,     gt_externalpanel,     gt_commspanel,
     gt_rolepanel,    gt_stationservice,    gt_galaxymap,         gt_systemmap,
     gt_orrery,       gt_fssmode,           gt_saamode,           gt_codex);

  TDockingType = (dt_docked, dt_landed);

type
  TGuiNotify = procedure (Sender: TObject; Gui: TGuiType) of object;
  TStatusIntNotify = procedure (Sender: TObject; Value: Integer) of object;
  TStatusFloatNotify = procedure (Sender: TObject; Value: Double) of object;
  TSetterCardinalNotify = procedure (MainData, Value: Cardinal) of object;
  TBoolChangeNotify = procedure (Sender: TObject; Indic: Boolean) of object;

  TEliteStatus = class
  private
    FValue         : Cardinal;
    FPips          : string;
    FFireGroup     : Integer;
    FFuelMain      : Double;
    FFuelReservoir : Double;
    FGuiFocus      : Integer;
    FLatitude      : Double;
    FLongitude     : Double;
    FHeading       : Integer;
    FAltitude      : Integer;
    FCargo         : Double;
    FLegalState    : string;
    FMutex         : Cardinal;
    {Evvents}
    FOnGuiChange             : TGuiNotify;
    FOnDocking               : TBoolChangeNotify;
    FOnLanding               : TBoolChangeNotify;
    FOnLandingGear           : TBoolChangeNotify;
    FOnShieldsUp             : TBoolChangeNotify;
    FOnSuperCruise           : TBoolChangeNotify;
    FOnFlightAssistOff       : TBoolChangeNotify;
    FOnHardpointDeployed     : TBoolChangeNotify;
    FOnInWing                : TBoolChangeNotify;
    FOnCargoScoopDeployed    : TBoolChangeNotify;
    FOnSilentRunning         : TBoolChangeNotify;
    FOnScoopingFuel          : TBoolChangeNotify;
    FOnSrvhandBrake          : TBoolChangeNotify;
    FOnSrvTurretView         : TBoolChangeNotify;
    FOnTuretRetracted        : TBoolChangeNotify;
    FOnDriveAssist           : TBoolChangeNotify;
    FOnFsdMassLocked         : TBoolChangeNotify;
    FOnFsdCharging           : TBoolChangeNotify;
    FOnFsdCoolDown           : TBoolChangeNotify;
    FOnLowFuel               : TBoolChangeNotify;
    FOnOverHeating           : TBoolChangeNotify;
    FOnHasLatLong            : TBoolChangeNotify;
    FOnIsinDanger            : TBoolChangeNotify;
    FOnBeingInterdicted      : TBoolChangeNotify;
    FOnInMainShip            : TBoolChangeNotify;
    FOnInFighter             : TBoolChangeNotify;
    FOnInSrv                 : TBoolChangeNotify;
    FOnHudInanalysisMode     : TBoolChangeNotify;
    FOnNightVision           : TBoolChangeNotify;
    FOnFsdJump               : TBoolChangeNotify;
    procedure SetValue(const AValue: Cardinal);
    procedure SetPips(const Value: string);
    procedure SetFireGroup(const Value: Integer);
    procedure SetFuelMain(const Value: Double);
    procedure SetFuelReservoir(const Value: Double);
    procedure SetGuiFocus(const Value: Integer);
    procedure SetLatitude(const Value: Double);
    procedure SetLongitude(const Value: Double);
    procedure SetHeading(const Value: Integer);
    procedure SetAltitude(const Value: Integer);
    function  GetValue: Cardinal;
    function  GetPips: string;
    function  GetFireGroup: Integer;
    function  GetFuelMain: Double;
    function  GetFuelReservoir: Double;
    function  GetGuiFocus: Integer;
    function  GetLatitude: Double;
    function  GetLongitude: Double;
    function  GetHeading: Integer;
    function  GetAltitude: Integer;
    function  GetCargo: Double;
    procedure SetCargo(const Value: Double);
    function  GetLegalState: string;
    procedure SetLegalState(const Value: string);

  private
    { --- protected resources}
    function  IsByteOpen(const Status: Integer):Boolean;
    procedure CardinalAssignment(var Card1: Cardinal; Card2: Cardinal);
    function  ReadCardinal(Card: Cardinal):Cardinal;
    procedure IntegerAssignment(var Card1: Integer; Card2: Integer);
    function  ReadInteger(Card: Integer):Integer;
    procedure StringAssignment(var Card1: string; Card2: string);
    function  ReadString(Card: string):string;
    procedure DoubleAssignment(var Card1: Double; Card2: Double);
    function  ReadDouble(Card: Double):Double;
  public
    function Docked: Boolean;
    function Landed: Boolean;
    function LandinGearDown: Boolean;
    function ShieldsUp: Boolean;
    function SuperCruise: Boolean;
    function FlightAssistOff: Boolean;
    function HardpointDeployed: Boolean;
    function InWing: Boolean;
    function LightOn: Boolean;
    function CargoScoopDeployed: Boolean;
    function SilentRunning: Boolean;
    function ScoopingFuel: Boolean;
    function SrvhandBrake: Boolean;
    function SrvTurretView: Boolean;
    function TuretRetracted: Boolean;
    function DriveAssist: Boolean;
    function FsdMassLocked: Boolean;
    function FsdCharging: Boolean;
    function FsdCoolDown: Boolean;
    function LowFuel: Boolean;
    function OverHeating: Boolean;
    function HasLatLong: Boolean;
    function IsinDanger: Boolean;
    function BeingInterdicted: Boolean;
    function InMainShip: Boolean;
    function InFighter: Boolean;
    function InSrv: Boolean;
    function HudInanalysisMode: Boolean;
    function NightVision: Boolean;
    function AltitudeFromAverageRadius: Boolean;
    function FsdJump: Boolean;
    function SrvHighBeam: Boolean;

    function GuiValue: TGuiType;

    property Value: Cardinal read GetValue write SetValue;
    property Pips: string read GetPips write SetPips;
    property FireGroup: Integer read GetFireGroup write SetFireGroup;
    property FuelMain: Double read GetFuelMain write SetFuelMain;
    property FuelReservoir: Double read GetFuelReservoir write SetFuelReservoir;
    property GuiFocus: Integer read GetGuiFocus write SetGuiFocus;
    property Latitude: Double read GetLatitude write SetLatitude;
    property Longitude: Double read GetLongitude write SetLongitude;
    property Heading: Integer read GetHeading write SetHeading;
    property Altitude: Integer read GetAltitude write SetAltitude;
    property Cargo: Double read GetCargo write SetCargo;
    property LegalState: string read GetLegalState write SetLegalState;

    constructor Create;
    destructor Destroy; override;


    property OnGuiChange: TGuiNotify read FOnGuiChange write FOnGuiChange;
    property OnDocking: TBoolChangeNotify read FOnDocking write FOnDocking;
    property OnLanding: TBoolChangeNotify read FOnLanding write FOnLanding;
    property OnLandingGear: TBoolChangeNotify read FOnLandingGear write FOnLandingGear;
    property OnShieldsUp: TBoolChangeNotify read FOnShieldsUp write FOnShieldsUp;
    property OnSuperCruise: TBoolChangeNotify read FOnSuperCruise write FOnSuperCruise;
    property OnFlightAssistOff: TBoolChangeNotify read FOnFlightAssistOff write FOnFlightAssistOff;
    property OnHardpointDeployed: TBoolChangeNotify read FOnHardpointDeployed write FOnHardpointDeployed;
    property OnInWing: TBoolChangeNotify read FOnInWing write FOnInWing;
    property OnCargoScoopDeployed: TBoolChangeNotify read FOnCargoScoopDeployed write FOnCargoScoopDeployed;
    property OnSilentRunning: TBoolChangeNotify read FOnSilentRunning write FOnSilentRunning;
    property OnScoopingFuel: TBoolChangeNotify read FOnScoopingFuel write FOnScoopingFuel;
    property OnSrvhandBrake: TBoolChangeNotify read FOnSrvhandBrake write FOnSrvhandBrake;
    property OnSrvTurretView: TBoolChangeNotify read FOnSrvTurretView write FOnSrvTurretView;
    property OnTuretRetracted: TBoolChangeNotify read FOnTuretRetracted write FOnTuretRetracted;
    property OnDriveAssist: TBoolChangeNotify read FOnDriveAssist write FOnDriveAssist;
    property OnFsdMassLocked: TBoolChangeNotify read FOnFsdMassLocked write FOnFsdMassLocked;
    property OnFsdCharging: TBoolChangeNotify read FOnFsdCharging write FOnFsdCharging;
    property OnFsdCoolDown: TBoolChangeNotify read FOnFsdCoolDown write FOnFsdCoolDown;
    property OnLowFuel: TBoolChangeNotify read FOnLowFuel write FOnLowFuel;
    property OnOverHeating: TBoolChangeNotify read FOnOverHeating write FOnOverHeating;
    property OnHasLatLong: TBoolChangeNotify read FOnHasLatLong write FOnHasLatLong;
    property OnIsinDanger: TBoolChangeNotify read FOnIsinDanger write FOnIsinDanger;
    property OnBeingInterdicted: TBoolChangeNotify read FOnBeingInterdicted write FOnBeingInterdicted;
    property OnInMainShip: TBoolChangeNotify read FOnInMainShip write FOnInMainShip;
    property OnInFighter: TBoolChangeNotify read FOnInFighter write FOnInFighter;
    property OnInSrv: TBoolChangeNotify read FOnInSrv write FOnInSrv;
    property OnHudInanalysisMode: TBoolChangeNotify read FOnHudInanalysisMode write FOnHudInanalysisMode;
    property OnNightVision: TBoolChangeNotify read FOnNightVision write FOnNightVision;
    property OnFsdJump: TBoolChangeNotify read FOnFsdJump write FOnFsdJump;
  end;

  TBufferAddNotify  = procedure (const ASt: string) of object;
  TBufferReadNotify = function: string of object;
  TStatusStringFIFO = class(TStringList)
  private
    FMutex  : Cardinal;
    procedure ProtectedSetter(Method: TBufferAddNotify; const ASt: string);
    function  ProtectedGetter(Method: TBufferReadNotify):string;
    procedure DoEmpile(const ASt: string);
    function  DoDepile:string;
  public
    procedure Empile(const ASt: string);
    function  Depile:string;

    constructor Create;
    destructor Destroy; override;
  end;

  TJSonStatusExtractor = class;
  TStatusFileWriter = class(TThread)
  private
    ThStatusString : TStatusStringFIFO;
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
    procedure Execute; override;
    constructor Create;
  end;

  TStatusFileReader = class(TThread)
  private
    ThStatusString : TStatusStringFIFO;
    ThExtractor    : TJSonStatusExtractor;
    ThEliteStatus  : TEliteStatus;
    procedure ThDelay(ms: Cardinal);
    procedure Process;
  public
     procedure Execute; override;
    constructor Create;
  end;

  TJSonStatusExtractor = class
  private
    FSource: string;
  public
    procedure SetSource(const Value: string);

    function ExtractStatus: Cardinal;
    function ExtractPips: string;
    function ExtractFireGroup: Integer;
    function ExtractFuelMain: Double;
    function ExtractFuelReservoir: Double;
    function ExtractGuiFocus: Integer;
    function ExtractLatitude: Double;
    function ExtractLongitude: Double;
    function ExtractHeading: Integer;
    function ExtractAltitude: Integer;
    function ExtractCargo: Double;
    function ExtractLegalState: string;
  end;

var
  EliteStatus         : TEliteStatus = nil;
  StatusStringFIFO    : TStatusStringFIFO = nil;
  StatusSurveyor      : TStatusFileWriter;
  StatusAnalyser      : TStatusFileReader;
  JSonStatusExtractor : TJSonStatusExtractor;

function isB1(const Value: Cardinal; index: Integer):Boolean;
function IntToBit(const Value: Cardinal):string;
function Power2(Exponant: Integer):Int64;

function ReadStatusFile: string;

implementation

uses
  Math, uRegistry;

type
  TArrayOfByte = array[0..MaxShl] of Int64;

  TEliteStatusType =
   ( est_docked,                                est_landed,
     est_landingeardown,                        est_shieldsup,
     est_supercruise,                           est_flightassistoff,
     est_hardpointdeployed,                     est_inwing,
     est_lighton,                               est_cargoscoopdeployed,
     est_silentrunning,                         est_scoopingfuel,
     est_srvhandbrake,                          est_srvturretview,
     est_turetretracted,                        est_driveassist,
     est_fsdmasslocked,                         est_fsdcharging,
     est_fsdcooldown,                           est_lowfuel,
     est_overheating,                           est_haslatlong,
     est_isindanger,                            est_beinginterdicted,
     est_inmainship,                            est_infighter,
     est_insrv,                                 est_hudinanalysismode,
     est_nightvision,                           est_altitudefromaverageradius,
     est_fsdjump,                               est_srvhighbeam
   );
   TAreaEliteStatus = 0..Integer( high(TEliteStatusType) );

function EliteStatusToInt(const Value: TEliteStatusType):Integer;
begin
  if Integer(Value) <= high(TAreaEliteStatus) then Result := Integer( Value )
    else raise Exception.Create('EliteStatus hors limites')
end;

var
  ArrayOfByte: TArrayOfByte;

procedure ArrayOfByteInitialization;
var
  i : Integer;
begin
  for i := 0  to 30     do ArrayOfByte[i] := 1 shl i;
  for i := 31 to MaxShl do ArrayOfByte[i] := Trunc(Math.Power(2, i));
end;

function isB1(const Value: Cardinal; index: Integer):Boolean;
begin
  Result := Cardinal(Value) and ArrayOfByte[index] = ArrayOfByte[index]
end;

function Power2(Exponant: Integer):Int64;
begin
  Result := ArrayOfByte[Exponant]
end;

function IntToBit(const Value: Cardinal):string;
var
  index   : Integer;
begin
  Result := EmptyStr;
  index  := 0;
  while Value > ArrayOfByte[index] do begin
    if isB1(Value, index) then Result := Format('1%s', [Result])
      else Result := Format('0%s', [Result]);
    Inc( index );
  end
end;

const
  SHELL_ACCES        = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
  SAVE_GAMES_KEY     = '{4C5C32FF-BB9D-43B0-B5B4-2D72E54EAAA4}';

var
  SavedGamesW    : string = 'Frontier Developments\Elite Dangerous';

function GetFrontierSaveGames: string;
begin
  KeyRead(SHELL_ACCES, SAVE_GAMES_KEY, Result);
  Result := Format('%s\%s', [Result, SavedGamesW])
end;

function ReadStatusFile: string;
var
  Stream   : TFileStream;
  S        : AnsiChar;
  FileName : string;
begin
  FileName := Format('%s\%s', [GetFrontierSaveGames, 'Status.json']);
  Stream   := TFileStream.Create(FileName, fmOpenRead);
  try
    Stream.Position := 0;
    while Stream.Position < Stream.Size do begin
      Stream.ReadBuffer(S, SizeOf(S));
      if S = '}' then S := ',';
      Result := Result + S
    end
  finally
    Stream.Free
  end;
end;


{ TEliteStatus }

function TEliteStatus.AltitudeFromAverageRadius: Boolean;
begin
  Result := IsByteOpen( Integer(est_altitudefromaverageradius) )
end;

function TEliteStatus.BeingInterdicted: Boolean;
begin
  Result := IsByteOpen( Integer(est_beinginterdicted) )
end;

function TEliteStatus.CargoScoopDeployed: Boolean;
begin
  Result := IsByteOpen( Integer(est_cargoscoopdeployed) )
end;

function TEliteStatus.Docked: Boolean;
begin
  Result := IsByteOpen( Integer(est_docked) )
end;

function TEliteStatus.DriveAssist: Boolean;
begin
  Result := IsByteOpen( Integer(est_driveassist) )
end;

function TEliteStatus.FlightAssistOff: Boolean;
begin
  Result := IsByteOpen( Integer(est_flightassistoff) )
end;

function TEliteStatus.FsdCharging: Boolean;
begin
  Result := IsByteOpen( Integer(est_fsdcharging) )
end;

function TEliteStatus.FsdCoolDown: Boolean;
begin
  Result := IsByteOpen( Integer(est_fsdcooldown) )
end;

function TEliteStatus.FsdJump: Boolean;
begin
  Result := IsByteOpen( Integer(est_fsdjump) )
end;

function TEliteStatus.FsdMassLocked: Boolean;
begin
  Result := IsByteOpen( Integer(est_fsdmasslocked) )
end;

function TEliteStatus.HardpointDeployed: Boolean;
begin
  Result := IsByteOpen( Integer(est_hardpointdeployed) )
end;

function TEliteStatus.HasLatLong: Boolean;
begin
  Result := IsByteOpen( Integer(est_haslatlong) )
end;

function TEliteStatus.HudInanalysisMode: Boolean;
begin
  Result := IsByteOpen( Integer(est_hudinanalysismode) )
end;

function TEliteStatus.InFighter: Boolean;
begin
  Result := IsByteOpen( Integer(est_infighter) )
end;

function TEliteStatus.InMainShip: Boolean;
begin
  Result := IsByteOpen( Integer(est_inmainship) )
end;

function TEliteStatus.InWing: Boolean;
begin
  Result := IsByteOpen( Integer(est_inwing) )
end;

function TEliteStatus.IsByteOpen(const Status: Integer): Boolean;
begin
  Result := isB1(Value, Status)
end;

function TEliteStatus.IsinDanger: Boolean;
begin
  Result := IsByteOpen( Integer(est_isindanger) )
end;

function TEliteStatus.Landed: Boolean;
begin
  Result := IsByteOpen( Integer(est_landed) )
end;

function TEliteStatus.LandinGearDown: Boolean;
begin
  Result := IsByteOpen( Integer(est_landingeardown) )
end;

function TEliteStatus.LightOn: Boolean;
begin
  Result := IsByteOpen( Integer(est_lighton) )
end;

function TEliteStatus.LowFuel: Boolean;
begin
  Result := IsByteOpen( Integer(est_lowfuel) )
end;

function TEliteStatus.NightVision: Boolean;
begin
  Result := IsByteOpen( Integer(est_nightvision) )
end;

function TEliteStatus.OverHeating: Boolean;
begin
  Result := IsByteOpen( Integer(est_overheating) )
end;

function TEliteStatus.ScoopingFuel: Boolean;
begin
  Result := IsByteOpen( Integer(est_scoopingfuel) )
end;

procedure TEliteStatus.SetValue(const AValue: Cardinal);
begin
  if FValue <> AValue then begin
    CardinalAssignment(FValue, AValue);
    if Assigned(FOnDocking)            then FOnDocking(Self, Docked);
    if Assigned(FOnLanding)            then FOnLanding(Self, Landed);
    if Assigned(FOnLandingGear)        then FOnLandingGear(Self, LandinGearDown);
    if Assigned(FOnShieldsUp)          then FOnShieldsUp(Self, ShieldsUp);
    if Assigned(FOnSuperCruise)        then FOnSuperCruise(Self, SuperCruise);
    if Assigned(FOnFlightAssistOff)    then FOnFlightAssistOff(Self, FlightAssistOff);
    if Assigned(FOnHardpointDeployed)  then FOnHardpointDeployed(Self, HardpointDeployed);
    if Assigned(FOnInWing)             then FOnInWing(Self, InWing);
    if Assigned(FOnCargoScoopDeployed) then FOnCargoScoopDeployed(Self, CargoScoopDeployed);
    if Assigned(FOnSilentRunning)      then FOnSilentRunning(Self, SilentRunning);
    if Assigned(FOnScoopingFuel)       then FOnScoopingFuel(Self, ScoopingFuel);
    if Assigned(FOnSrvhandBrake)       then FOnSrvhandBrake(Self, SrvhandBrake);
    if Assigned(FOnSrvTurretView)      then FOnSrvTurretView(Self, SrvTurretView);
    if Assigned(FOnTuretRetracted)     then FOnTuretRetracted(Self, TuretRetracted);
    if Assigned(FOnDriveAssist)        then FOnDriveAssist(Self, DriveAssist);
    if Assigned(FOnFsdMassLocked)      then FOnFsdMassLocked(Self, FsdMassLocked);
    if Assigned(FOnFsdCharging)        then FOnFsdCharging(Self, FsdCharging);
    if Assigned(FOnFsdCoolDown)        then FOnFsdCoolDown(Self, FsdCoolDown);
    if Assigned(FOnLowFuel)            then FOnLowFuel(Self, LowFuel);
    if Assigned(FOnOverHeating)        then FOnOverHeating(Self, OverHeating);
    if Assigned(FOnHasLatLong)         then FOnHasLatLong(Self, HasLatLong);
    if Assigned(FOnIsinDanger)         then FOnIsinDanger(Self, IsinDanger);
    if Assigned(FOnBeingInterdicted)   then FOnBeingInterdicted(Self, BeingInterdicted);
    if Assigned(FOnInMainShip)         then FOnInMainShip(Self, InMainShip);
    if Assigned(FOnInFighter)          then FOnInFighter(Self, InFighter);
    if Assigned(FOnInSrv)              then FOnInSrv(Self, InSrv);
    if Assigned(FOnHudInanalysisMode)  then FOnHudInanalysisMode(Self, HudInanalysisMode);
    if Assigned(FOnNightVision)        then FOnNightVision(Self, NightVision);
    if Assigned(FOnFsdJump)            then FOnFsdJump(Self, FsdJump);
  end
end;

function TEliteStatus.ShieldsUp: Boolean;
begin
  Result := IsByteOpen( Integer(est_shieldsup) )
end;

function TEliteStatus.SilentRunning: Boolean;
begin
  Result := IsByteOpen( Integer(est_silentrunning) )
end;

function TEliteStatus.InSrv: Boolean;
begin
  Result := IsByteOpen( Integer(est_insrv) )
end;

function TEliteStatus.SrvhandBrake: Boolean;
begin
  Result := IsByteOpen( Integer(est_srvhandbrake) )
end;

function TEliteStatus.SrvHighBeam: Boolean;
begin
  Result := IsByteOpen( Integer(est_srvhighbeam) )
end;

function TEliteStatus.SrvTurretView: Boolean;
begin
  Result := IsByteOpen( Integer(est_srvturretview) )
end;

function TEliteStatus.SuperCruise: Boolean;
begin
  Result := IsByteOpen( Integer(est_supercruise) )
end;

function TEliteStatus.TuretRetracted: Boolean;
begin
  Result := IsByteOpen( Integer(est_turetretracted) )
end;

procedure TEliteStatus.SetPips(const Value: string);
begin
  if FPips <> Value then StringAssignment(FPips, Value)
end;

procedure TEliteStatus.SetFireGroup(const Value: Integer);
begin
  if FFireGroup <> Value then IntegerAssignment(FFireGroup, Value)
end;

procedure TEliteStatus.SetFuelMain(const Value: Double);
begin
  if FFuelMain <> Value then DoubleAssignment(FFuelMain, Value)
end;

procedure TEliteStatus.SetFuelReservoir(const Value: Double);
begin
  if FFuelReservoir <> Value then DoubleAssignment(FFuelReservoir, Value);
end;

procedure TEliteStatus.SetGuiFocus(const Value: Integer);
begin
  if FGuiFocus <> Value then begin
    IntegerAssignment(FGuiFocus, Value);
    if Assigned(FOnGuiChange) then FOnGuiChange( Self, TGuiType(Value) )
  end
end;

procedure TEliteStatus.SetLatitude(const Value: Double);
begin
  if FLatitude <> Value then DoubleAssignment(FLatitude, Value)
end;

procedure TEliteStatus.SetLongitude(const Value: Double);
begin
  if FLongitude <> Value then DoubleAssignment(FLongitude, Value)
end;

procedure TEliteStatus.SetHeading(const Value: Integer);
begin
  if FHeading <> Value then IntegerAssignment(FHeading, Value)
end;

procedure TEliteStatus.SetAltitude(const Value: Integer);
begin
  if FAltitude <> Value then IntegerAssignment(FAltitude, Value)
end;

constructor TEliteStatus.Create;
begin
  inherited Create;
  FMutex := CreateMutex(nil, False, 'StatusSate');
  if FMutex = 0 then RaiseLastOSError;
end;

destructor TEliteStatus.Destroy;
begin
  CloseHandle( FMutex );
  inherited;
end;

procedure TEliteStatus.CardinalAssignment(var Card1: Cardinal; Card2: Cardinal);
begin
  if WaitForSingleObject(FMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Card1 := Card2
    finally
      ReleaseMutex( FMutex )
    end
end;

function TEliteStatus.ReadCardinal(Card: Cardinal): Cardinal;
begin
  if WaitForSingleObject(FMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Result := Card
    finally
      ReleaseMutex( FMutex )
    end
end;

function TEliteStatus.GetValue: Cardinal;
begin
  Result := ReadCardinal(FValue)
end;

procedure TEliteStatus.IntegerAssignment(var Card1: Integer; Card2: Integer);
begin
  if WaitForSingleObject(FMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Card1 := Card2
    finally
      ReleaseMutex( FMutex )
    end
end;

function TEliteStatus.ReadInteger(Card: Integer): Integer;
begin
  if WaitForSingleObject(FMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Result := Card
    finally
      ReleaseMutex( FMutex )
    end
end;

procedure TEliteStatus.StringAssignment(var Card1: string; Card2: string);
begin
  if WaitForSingleObject(FMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Card1 := Card2
    finally
      ReleaseMutex( FMutex )
    end
end;

function TEliteStatus.ReadString(Card: string): string;
begin
  if WaitForSingleObject(FMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Result := Card
    finally
      ReleaseMutex( FMutex )
    end
end;

procedure TEliteStatus.DoubleAssignment(var Card1: Double; Card2: Double);
begin
  if WaitForSingleObject(FMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Card1 := Card2
    finally
      ReleaseMutex( FMutex )
    end
end;

function TEliteStatus.ReadDouble(Card: Double): Double;
begin
  if WaitForSingleObject(FMutex, INFINITE) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Result := Card
    finally
      ReleaseMutex( FMutex )
    end
end;

function TEliteStatus.GetPips: string;
begin
  Result := ReadString(FPips)
end;

function TEliteStatus.GetFireGroup: Integer;
begin
  Result := ReadInteger(FFireGroup)
end;

function TEliteStatus.GetFuelMain: Double;
begin
  Result := ReadDouble(FFuelMain)
end;

function TEliteStatus.GetFuelReservoir: Double;
begin
  Result := ReadDouble(FFuelReservoir)
end;

function TEliteStatus.GetGuiFocus: Integer;
begin
  Result := ReadInteger(FGuiFocus)
end;

function TEliteStatus.GetLatitude: Double;
begin
  Result := ReadDouble(FLatitude)
end;

function TEliteStatus.GetLongitude: Double;
begin
  Result := ReadDouble(FLongitude)
end;

function TEliteStatus.GetHeading: Integer;
begin
  Result := ReadInteger(FHeading)
end;

function TEliteStatus.GetAltitude: Integer;
begin
  Result := ReadInteger(FAltitude)
end;

function TEliteStatus.GetCargo: Double;
begin
  Result := ReadDouble(FCargo)
end;

procedure TEliteStatus.SetCargo(const Value: Double);
begin
  if FCargo <> Value then DoubleAssignment(FCargo, Value)
end;

function TEliteStatus.GetLegalState: string;
begin
  Result := ReadString(FLegalState)
end;

procedure TEliteStatus.SetLegalState(const Value: string);
begin
  if FLegalState <> Value then StringAssignment(FLegalState, Value);
end;

function TEliteStatus.GuiValue: TGuiType;
begin
  Result := TGuiType( GuiFocus )
end;

{ TStatusStringFIFO }

constructor TStatusStringFIFO.Create;
begin
  inherited Create;
  FMutex := CreateMutex(nil, False, 'StatusStringFIFO');
  if FMutex = 0 then RaiseLastOSError;
end;

function TStatusStringFIFO.Depile: string;
begin
  Result := ProtectedGetter(DoDepile)
end;

destructor TStatusStringFIFO.Destroy;
begin
  CloseHandle( FMutex );
  inherited
end;

function TStatusStringFIFO.DoDepile: string;
begin
  if Self.Count > 0 then begin
    Result := Trim( Strings[0] );
    Delete(0)
  end
end;

procedure TStatusStringFIFO.DoEmpile(const ASt: string);
begin
  if ASt <> EmptyStr then Add( ASt )
end;

procedure TStatusStringFIFO.Empile(const ASt: string);
begin
  ProtectedSetter(DoEmpile, Trim(ASt))
end;

function TStatusStringFIFO.ProtectedGetter(Method: TBufferReadNotify): string;
begin
  Result := EmptyStr;
  if Assigned( Method ) then begin
    if WaitForSingleObject( FMutex, INFINITE ) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Result := Method
    finally
      ReleaseMutex( FMutex )
    end
  end
end;

procedure TStatusStringFIFO.ProtectedSetter(Method: TBufferAddNotify; const ASt: string);
begin
  if Assigned( Method ) then begin
    if WaitForSingleObject( FMutex, INFINITE ) <> WAIT_OBJECT_0 then RaiseLastOSError;
    try
      Method( ASt )
    finally
      ReleaseMutex( FMutex )
    end
  end
end;

{ TStatusFileReader }

constructor TStatusFileReader.Create;
begin
  inherited Create( False );
  {Launch on create}
  ThStatusString  := StatusStringFIFO;
  ThEliteStatus   := EliteStatus;
  ThExtractor     := JSonStatusExtractor;
  FreeOnTerminate := True;
  Priority        := tpLower;
end;

procedure TStatusFileReader.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 150 );
  end;
end;

procedure TStatusFileReader.Process;
var
  Buffer: string;
begin
  Buffer := ThStatusString.Depile;
  if Buffer <> EmptyStr then begin
    ThExtractor.SetSource( Buffer );
    with ThEliteStatus do begin
      Value         := ThExtractor.ExtractStatus;
      Pips          := ThExtractor.ExtractPips;
      FireGroup     := ThExtractor.ExtractFireGroup;
      FuelMain      := ThExtractor.ExtractFuelMain;
      FuelReservoir := ThExtractor.ExtractFuelReservoir;
      GuiFocus      := ThExtractor.ExtractGuiFocus;
      Latitude      := ThExtractor.ExtractLatitude;
      Longitude     := ThExtractor.ExtractLongitude;
      Heading       := ThExtractor.ExtractHeading;
      Altitude      := ThExtractor.ExtractAltitude;
      Cargo         := ThExtractor.ExtractCargo;
      LegalState    := ThExtractor.ExtractLegalState;
    end
  end
end;

procedure TStatusFileReader.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

{ TStatusFileWriter }

constructor TStatusFileWriter.Create;
begin
  inherited Create( False );
  {Launch on create}
  ThStatusString  := StatusStringFIFO;
  FreeOnTerminate := True;
  Priority        := tpLower;
end;

procedure TStatusFileWriter.Execute;
begin
  while not Terminated and not Application.Terminated do begin
    Synchronize( Process );
    ThDelay( 160 );
  end;
end;

procedure TStatusFileWriter.Process;
begin
  try
    ThStatusString.Empile( ReadStatusFile );
  except
  end
end;

procedure TStatusFileWriter.ThDelay(ms: Cardinal);
var S: Cardinal;
begin
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
    until Self.Terminated or Terminated or (GetTickCount > S)
end;

{ TJSonStatusExtractor }

function TJSonStatusExtractor.ExtractFuelMain: Double;
var
  ASt: string;
begin
  FormatSettings.DecimalSeparator := '.';
  ASt := GetAfterStr(FSource, 'FuelMain":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToFloat( ASt ) except Result := 0.0 end
end;

function TJSonStatusExtractor.ExtractFireGroup: Integer;
var
  ASt : string;
begin
  ASt := GetAfterStr(FSource, 'FireGroup":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToInt( ASt ) except Result := 0 end
end;

function TJSonStatusExtractor.ExtractPips: string;
var
  s1, s2 : string;
begin
  Result := GetAfterStr(FSource, 'Pips":[');
  Result := GetBeforStr(Result, ']');
  s1     := GetBeforStr(Result, ',');
  Result := GetAfterStr(Result, ',');
  s2     := GetBeforStr(Result, ',');
  Result := GetAfterStr(Result, ',');
  Result := Format('%s; %s; %s', [s1,s2,Result])
end;

function TJSonStatusExtractor.ExtractStatus: Cardinal;
var
  ASt: string;
begin
  Result := 0;
  if FSource <> EmptyStr then begin
    ASt := GetAfterStr(FSource, 'Flags":');
    ASt := GetBeforStr(ASt, ',');
    try Result := StrToInt64(ASt) except Result := 0 end
  end
end;

procedure TJSonStatusExtractor.SetSource(const Value: string);
begin
  FSource := Trim( Value );
end;

function TJSonStatusExtractor.ExtractFuelReservoir: Double;
var
  ASt: string;
begin
  FormatSettings.DecimalSeparator := '.';
  ASt := GetAfterStr(FSource, 'FuelReservoir":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToFloat( ASt ) except Result := 0.0 end
end;

function TJSonStatusExtractor.ExtractGuiFocus: Integer;
var
  ASt: string;
begin
  ASt := GetAfterStr(FSource, 'GuiFocus":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToInt( ASt ) except Result := 0 end
end;

function TJSonStatusExtractor.ExtractLatitude: Double;
var
  ASt: string;
begin
  FormatSettings.DecimalSeparator := '.';
  ASt := GetAfterStr(FSource, 'Latitude":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToFloat( ASt ) except Result := 0.0 end
end;

function TJSonStatusExtractor.ExtractLongitude: Double;
var
  ASt: string;
begin
  FormatSettings.DecimalSeparator := '.';
  ASt := GetAfterStr(FSource, 'Longitude":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToFloat( ASt ) except Result := 0.0 end
end;


function TJSonStatusExtractor.ExtractHeading: Integer;
var
  ASt: string;
begin
  ASt := GetAfterStr(FSource, 'Heading":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToInt( ASt ) except Result := 0 end
end;

function TJSonStatusExtractor.ExtractAltitude: Integer;
var
  ASt: string;
begin
  ASt := GetAfterStr(FSource, 'Altitude":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToInt( ASt ) except Result := 0 end
end;


function TJSonStatusExtractor.ExtractCargo: Double;
var
  ASt: string;
begin
  FormatSettings.DecimalSeparator := '.';
  ASt := GetAfterStr(FSource, 'Cargo":');
  ASt := GetBeforStr(ASt, ',');
  try Result := StrToFloat( ASt ) except Result := 0.0 end
end;

function TJSonStatusExtractor.ExtractLegalState: string;
begin
  Result := GetAfterStr(FSource, 'LegalState":"');
  Result := GetBeforStr(Result, '"');
end;

initialization
  ArrayOfByteInitialization;
  StatusStringFIFO    := TStatusStringFIFO.Create;
  JSonStatusExtractor := TJSonStatusExtractor.Create;
  EliteStatus         := TEliteStatus.Create;
  StatusSurveyor      := TStatusFileWriter.Create;
  StatusAnalyser      := TStatusFileReader.Create;
finalization
  StatusAnalyser.Terminate;
  StatusSurveyor.Terminate;
  EliteStatus.Free;
  JSonStatusExtractor.Free;
  StatusStringFIFO.Free;
end.
