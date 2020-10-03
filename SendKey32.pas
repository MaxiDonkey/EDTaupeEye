{*******************************************************}
{                                                       }
{             08/2020  MaxiDonkey  Library              }
{                                                       }
{*******************************************************}

unit SendKey32;

interface

uses
  SysUtils, classes, Windows, Messages, Dialogs, Forms, KeysDef, uRegistry,
  StrUtils, System.Types;

type
  {For KeyBoardState}
  TByteSet = Set of byte;

{...............................................................................

  NOTE

  Instancier la classe TPushKeyCombine = class; si les appels sont fréquents
        afin d'éviter l'appel à la fonction de classe (sporadique)

...............................................................................}

procedure Wait(Time: Cardinal = 15);
procedure WaitForKey(Time: Cardinal = 250);


{ --- Buffer for working with PChar's : Note ** 1 caractère + #0 sinon ce n'est pas nécessaire --- }
function SendKeys(SendKeysString : PWideChar; Wait : Boolean) : Boolean; overload;

{ --- Appel que pour une combinaison de touches
      warning : iKey := VK_LEFT ou iKey := VkKeyScan('A')

      note : Specials permet de distinguer les alt, ctrl, shift gauche et droit  --- }
procedure SendKey(iKey: Smallint; Tms: Cardinal; Specials: TSpecials = []); overload;

{ --- Combinaison Win+Alpha --- }
procedure SendKeyWin(iKey: Smallint; Tms: Cardinal);

{ --- Alt + xxxx ex : ³ âr alt+0179 --- }
procedure SendKey(sCode: string); overload;

{ --- Mouse --- }
function  GetMousePosition: TPoint;
procedure SetMousePosition(const Position: TPoint); overload;
procedure SetMousePosition(x,y: Integer); overload;
procedure MouseLeftClic; overload;
procedure MouseLeftClic(Tms: Cardinal; Shift: TShiftState); overload;
procedure MouseRightClic; overload;
procedure MouseRightClic(Tms: Cardinal; Shift: TShiftState); overload;
procedure MouseDoubleClic;
procedure MouseMiddleClic; overload;
procedure MouseMiddleClic(Tms: Cardinal; Shift: TShiftState); overload;

{ --- KeyBordState --- }
function KeyBoardState: TByteSet;
function CapsLocKEnable: Boolean;

{ --- Managed extended char by codename (cf unit KEYSDEF) --- }
function CodeKeyToAltNum(const Value: string): string;

{ --- routine "ProcessOnKey" param Value = ShortCut
  sample : Value = 'Shift_Ctrl_a'
           Value = 'Alt_F6'
           Value = 'a e @ f alt_c suppr'

  note : les touches de GAUCHE pour alt, ctrl, shift sont utilisées --- }
procedure ProcessOnKey(const Value: string);

{ --- Lire/écrire la dernière commande --- }
function RetrieveLastCommand: string;
procedure LastCommandToReg(const ASt: string);

type
  TAllShortCut = TShortcuts;

var
  AllShortCut : TAllShortCut;


const
  WorkBufLen = 1024;
var
  WorkBuf : array[0..WorkBufLen] of Char;

{Simulate key combinaison sample Alt+C to keyBoard
    alt, shift, strl = left or right possible distinction
}
type
  TKeyPressed   = (kp_none, kp_shift, kp_ctrl, kp_alt, kp_alt_ctrl,
    kp_shift_ctrl, kp_shift_alt);

  TPushKeyCombine = class
  private
    FUpdated       : Boolean;
    FSHIFT_LEFT    : Boolean;
    FALT_LEFT      : Boolean;
    FCTRL_LEFT     : Boolean;
    FShift_pressed : Boolean;
    FCtrl_pressed  : Boolean;
    FAlt_pressed   : Boolean;
    FResume        : TKeyPressed;

  private
    procedure Initialize;
    procedure Alt_Ctrl_Key(iKey : Smallint; iTime : Cardinal);
    procedure Shift_Ctrl_Key(iKey: Smallint; iTime: Cardinal);
    procedure Shift_alt_Key(iKey: Smallint; iTime: Cardinal);
    procedure Alt_Key(iKey : Smallint; iTime : Cardinal);
    procedure Ctrl_Key(iKey : Smallint; iTime : Cardinal);
    procedure Shift_Key(iKey : Smallint; iTime : Cardinal);
    procedure For_Key(iKey : Smallint; iTime : Cardinal);
    procedure SetSpecials(const ASpecials: TSpecials);
  private
    function  NospecialPressed:Boolean;
    function  ShiftPressed:Boolean;
    function  CtrlPressed:Boolean;
    function  AltPressed:Boolean;
    function  Alt_CtrlPressed:Boolean;
    function  Shift_CtrlPressed:Boolean;
    function  Shift_AltPressed: Boolean;
  public
    procedure KeyPress(Key: Char; Tms: Cardinal; Specials: TSpecials = []);
    procedure KeyPressEx(iKey: Smallint; Tms: Cardinal; Specials: TSpecials = []);
    constructor Create;

    class procedure Simulate(Key: Char; Tms: Cardinal; Specials: TSpecials = []);
    class procedure SimulateEx(iKey: Smallint; Tms: Cardinal; Specials: TSpecials = []);
  end;

implementation

type
  THKeys = array[0..pred(MaxLongInt)] of byte;
var
  AllocationSize : integer;

(*
Envoi une chaîne de caractères à un composant windows disposant du focus
Example syntax: SendKeys('abc123def456ghi789', True);
*)

function SendKeys(SendKeysString : PWideChar; Wait : Boolean) : Boolean;
type
  WBytes = array[0..pred(SizeOf(Word))] of Byte;

  TSendKey = record
    Name : ShortString;
    VKey : Byte;
  end;

const
  {Array of keys that SendKeys recognizes.
  If you add to this list, you must be sure to keep it sorted alphabetically
  by Name because a binary search routine is used to scan it.}

  MaxSendKeyRecs = 41;
  SendKeyRecs : array[1..MaxSendKeyRecs] of TSendKey =
  (
   (Name: 'BACKSPACE';       VKey:VK_BACK),
   (Name: 'BKSP';            VKey:VK_BACK),
   (Name: 'BREAK';           VKey:VK_CANCEL),
   (Name: 'BS';              VKey:VK_BACK),
   (Name: 'CAPSLOCK';        VKey:VK_CAPITAL),
   (Name: 'CLEAR';           VKey:VK_CLEAR),
   (Name: 'DEL';             VKey:VK_DELETE),
   (Name: 'DELETE';          VKey:VK_DELETE),
   (Name: 'DOWN';            VKey:VK_DOWN),
   (Name: 'END';             VKey:VK_END),
   (Name: 'ENTER';           VKey:VK_RETURN),
   (Name: 'ESC';             VKey:VK_ESCAPE),
   (Name: 'ESCAPE';          VKey:VK_ESCAPE),
   (Name: 'F1';              VKey:VK_F1),
   (Name: 'F10';             VKey:VK_F10),
   (Name: 'F11';             VKey:VK_F11),
   (Name: 'F12';             VKey:VK_F12),
   (Name: 'F13';             VKey:VK_F13),
   (Name: 'F14';             VKey:VK_F14),
   (Name: 'F15';             VKey:VK_F15),
   (Name: 'F16';             VKey:VK_F16),
   (Name: 'F2';              VKey:VK_F2),
   (Name: 'F3';              VKey:VK_F3),
   (Name: 'F4';              VKey:VK_F4),
   (Name: 'F5';              VKey:VK_F5),
   (Name: 'F6';              VKey:VK_F6),
   (Name: 'F7';              VKey:VK_F7),
   (Name: 'F8';              VKey:VK_F8),
   (Name: 'F9';              VKey:VK_F9),
   (Name: 'HELP';            VKey:VK_HELP),
   (Name: 'HOME';            VKey:VK_HOME),
   (Name: 'INS';             VKey:VK_INSERT),
   (Name: 'LEFT';            VKey:VK_LEFT),
   (Name: 'NUMLOCK';         VKey:VK_NUMLOCK),
   (Name: 'PGDN';            VKey:VK_NEXT),
   (Name: 'PGUP';            VKey:VK_PRIOR),
   (Name: 'PRTSC';           VKey:VK_PRINT),
   (Name: 'RIGHT';           VKey:VK_RIGHT),
   (Name: 'SCROLLLOCK';      VKey:VK_SCROLL),
   (Name: 'TAB';             VKey:VK_TAB),
   (Name: 'UP';              VKey:VK_UP)
  );

  {Extra VK constants missing from Delphi's Windows API interface}
  VK_NULL = 0;
  VK_SemiColon = 186;
  VK_Equal = 187;
  VK_Comma = 188;
  VK_Minus = 189;
  VK_Period = 190;
  VK_Slash = 191;
  VK_BackQuote = 192;
  VK_LeftBracket = 219;
  VK_BackSlash = 220;
  VK_RightBracket = 221;
  VK_Quote = 222;
  VK_Last = VK_Quote;

  ExtendedVKeys : set of byte =
  [VK_Up,
   VK_Down,
   VK_Left,
   VK_Right,
   VK_Home,
   VK_End,
   VK_Prior,  {PgUp}
   VK_Next,   {PgDn}
   VK_Insert,
   VK_Delete];

const
  INVALIDKEY       = $FFFF {Unsigned -1};
  VKKEYSCANSHIFTON = $01;
  VKKEYSCANCTRLON  = $02;
  VKKEYSCANALTON   = $04;

var
  ShiftDown, ControlDown, AltDown : Boolean;
  I, L : Integer;
  MKey : Word;

function BitSet(BitTable, BitMask : Byte) : Boolean;
begin
  Result := ByteBool(BitTable and BitMask);
end;

procedure SetBit(var BitTable : Byte; BitMask : Byte);
begin
  BitTable := BitTable or Bitmask;
end;

procedure KeyboardEvent(VKey, ScanCode : Byte; Flags : Longint);
var
  KeyboardMsg : TMsg;

begin
  keybd_event(VKey, ScanCode, Flags,0);
  if Wait then
  while PeekMessage(KeyboardMsg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE) do begin
    TranslateMessage(KeyboardMsg);
    DispatchMessage(KeyboardMsg)
  end;
end;

procedure SendKeyDown(VKey: Byte; NumTimes : Word; GenUpMsg : Boolean);
var
  Cnt           : Word;
  ScanCode      : Byte;
  NumState      : Boolean;
  KeyBoardState : TKeyboardState;

begin
  if VKey = VK_NUMLOCK then begin
    NumState := ByteBool(GetKeyState(VK_NUMLOCK) and 1);
    GetKeyBoardState(KeyBoardState);

    if NumState then KeyBoardState[VK_NUMLOCK] := (KeyBoardState[VK_NUMLOCK] and not 1)
      else KeyBoardState[VK_NUMLOCK] := (KeyBoardState[VK_NUMLOCK] or 1);

    SetKeyBoardState(KeyBoardState);
    Exit
  end;

  ScanCode := Lo(MapVirtualKey(VKey, 0));
  for Cnt := 1 to NumTimes do
    if VKey in ExtendedVKeys then begin
      KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY);
      if GenUpMsg then
        KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP)

    end else begin
      KeyboardEvent(VKey, ScanCode, 0);
      if GenUpMsg then KeyboardEvent(VKey, ScanCode, KEYEVENTF_KEYUP);
    end
end;

procedure SendKeyUp(VKey: Byte);
var
  ScanCode : Byte;

begin
  ScanCode := Lo(MapVirtualKey(VKey, 0));
  if VKey in ExtendedVKeys then
    KeyboardEvent(VKey, ScanCode, KEYEVENTF_EXTENDEDKEY and KEYEVENTF_KEYUP)
  else
    KeyboardEvent(VKey, ScanCode, KEYEVENTF_KEYUP)
end;

procedure SendKey(MKey: Word; NumTimes : Word; GenDownMsg : Boolean);
begin
  if BitSet(Hi(MKey), VKKEYSCANSHIFTON) then SendKeyDown(VK_SHIFT, 1, False);
  if BitSet(Hi(MKey), VKKEYSCANCTRLON)  then SendKeyDown(VK_CONTROL, 1, False);
  if BitSet(Hi(MKey), VKKEYSCANALTON)   then SendKeyDown(VK_MENU, 1, False);

  SendKeyDown(Lo(MKey), NumTimes, GenDownMsg);
  if BitSet(Hi(MKey), VKKEYSCANSHIFTON) then SendKeyUp(VK_SHIFT);
  if BitSet(Hi(MKey), VKKEYSCANCTRLON)  then SendKeyUp(VK_CONTROL);
  if BitSet(Hi(MKey), VKKEYSCANALTON)   then SendKeyUp(VK_MENU)
end;

procedure AltPlusKey(const Number: string);
var
  i    : Integer;
  iKey : Integer;
begin
  SendKeyDown(VK_MENU, 1, False);
  try
    SendKeyDown(VK_NUMPAD0, 1, False);
    SendKeyUp(VK_NUMPAD0);
    for I := 1 to Length(Number) do begin
      iKey := VkKeyScan(Number[i]);
      case IndexStr(Number[i], ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) of
        0 : iKey := VK_NUMPAD0;
        1 : iKey := VK_NUMPAD1;
        2 : iKey := VK_NUMPAD2;
        3 : iKey := VK_NUMPAD3;
        4 : iKey := VK_NUMPAD4;
        5 : iKey := VK_NUMPAD5;
        6 : iKey := VK_NUMPAD6;
        7 : iKey := VK_NUMPAD7;
        8 : iKey := VK_NUMPAD8;
        9 : iKey := VK_NUMPAD9;
      end;
      SendKeyDown(iKey, 1, False);
      SendKeyUp(iKey)
    end
  finally
    SendKeyUp(VK_MENU)
  end;
end;

procedure SendKeyCirconflex(const car: Char);
begin
  SendKeyDown(VkKeyScan('^'), 1, False);
  SendKeyUp(VkKeyScan('^'));
  SendKeyDown(VkKeyScan(car), 1, False);
  SendKeyUp(VkKeyScan(car));
end;

procedure SendKeyTrema(const car: Char);
begin
  SendKeyDown(VK_SHIFT, 1, False);
  SendKeyDown(VkKeyScan('^'), 1, False);
  SendKeyUp(VkKeyScan('^'));
  SendKeyUp(VK_SHIFT);
  SendKeyDown(VkKeyScan(car), 1, False);
  SendKeyUp(VkKeyScan(car));
end;


{Implements a simple binary search to locate special key name strings}

function StringToVKey(KeyString : ShortString) : Word;
var
  Found, Collided : Boolean;
  Bottom, Top, Middle : Byte;

begin
  Result := INVALIDKEY;
  Bottom := 1;
  Top    := MaxSendKeyRecs;
  Found  := false;
  Middle := (Bottom+Top) div 2;
  repeat
    Collided := (Bottom = Middle) or (Top = Middle);
    if KeyString = SendKeyRecs[Middle].Name then begin
       Found := True;
       Result := SendKeyRecs[Middle].VKey;
    end
    else begin
       if KeyString > SendKeyRecs[Middle].Name then Bottom := Middle
         else Top := Middle;
       Middle := Succ(Bottom+Top) div 2;
    end;
  until (Found or Collided);
  if (Result = INVALIDKEY) then
    MessageBox(0, 'Invalid key name!', 'SendKeys Routine', MB_ICONWARNING or MB_OK);
end;

procedure PopUpShiftKeys;
begin
  if ShiftDown   then SendKeyUp(VK_SHIFT);
  if ControlDown then SendKeyUp(VK_CONTROL);
  if AltDown     then SendKeyUp(VK_MENU);
  ShiftDown      := false;
  ControlDown    := false;
  AltDown        := false;
end;

procedure PressKey(key: char);
begin
  MKey := vkKeyScan(Key);
  if (MKey <> INVALIDKEY) then begin
    SendKey(MKey, 1, True);
    PopUpShiftKeys;
  end else
    MessageBox(0, 'Invalid key name!', 'SendKeys Routine', MB_ICONWARNING or MB_OK);
end;

function PressFunction(index: Integer; Key: Byte):Integer;
begin
  SendKeyDown(Key, 1, False);
  Result := index + 1
end;

function PressKey_(index: Integer; Key: Char):Integer;
begin
  PressKey(Key);
//  if Key in ['^', '~'] then PressKey(' ');
  if CharInSet(Key, ['^', '~']) then PressKey(' ');

  Result := index + 1
end;

function FunctionKey(index: Integer):Integer;
begin
  case SendKeysString[index] of
    'a' : Result := PressFunction(index, VK_F1);
    'b' : Result := PressFunction(index, VK_F2);
    'c' : Result := PressFunction(index, VK_F3);
    'd' : Result := PressFunction(index, VK_F4);
    'e' : Result := PressFunction(index, VK_F5);
    'f' : Result := PressFunction(index, VK_F6);
    'g' : Result := PressFunction(index, VK_F7);
    'h' : Result := PressFunction(index, VK_F8);
    'i' : Result := PressFunction(index, VK_F9);
    'j' : Result := PressFunction(index, VK_F10);
    'k' : Result := PressFunction(index, VK_F11);
    'l' : Result := PressFunction(index, VK_F12);
    '+' : Result := PressKey_(index, '+');
    '%' : Result := PressKey_(index, '%');
//    '^' : Result := PressKey_(index, '^');
//    '~' : Result := PressKey_(index, '~');
    '"' : Result := PressKey_(index, '"');
    else Result := 0
  end;
end;

begin
  AllocationSize := MaxInt;
  Result         := false;
  ShiftDown      := false;
  ControlDown    := false;
  AltDown        := false;
  I              := 0;
  L              := StrLen(SendKeysString);

  if (L > AllocationSize) then L := AllocationSize;
  if L = 0 then Exit;

  while I < L do begin
    case SendKeysString[I] of
    '©' : begin //ALT
             AltDown := True;
             SendKeyDown(VK_MENU, 1, False);
             Inc(I);
          end;
    '®' : begin //SHIFT
             ShiftDown := True;
             SendKeyDown(VK_SHIFT, 1, False);
             Inc(I);
          end;
    'ª' : begin //CTRL
             ControlDown := True;
             SendKeyDown(VK_CONTROL, 1, False);
             Inc(I);
          end;
    '¹' : begin //RETURN
            SendKeyDown(VK_RETURN, 1, True);
            PopUpShiftKeys;
            Inc(I);
          end;
    'º' : begin
            Inc(I);
//            if (I < L) and (SendKeysString[I] in ['a'..'l','%','+','^','~','"'])
//              then I := FunctionKey(I);
            if (I < L) and charInSet(SendKeysString[I],['a'..'l','%','+','^','~','"'])
              then I := FunctionKey(I);
          end;
    else begin
        // By Huashan
        // In double-byte character set systems, non ASCII characters cannot be
        // sent by simulating keyboard stroke, we use IME messages
        if Windows.IsDBCSLeadByte(Byte(SendKeysString[I])) then begin
          try
            AttachThreadInput(GetWindowThreadProcessId(GetForegroundWindow, nil),
              GetCurrentThreadId, True);
            PostMessage(GetFocus, WM_IME_CHAR,
              MakeWord(Byte(SendKeysString[I+1]), Byte(SendKeysString[I])), 0);
            Inc(I);
          finally
            AttachThreadInput(GetWindowThreadProcessId(GetForegroundWindow, nil),
              GetCurrentThreadId, False);
          end;

        end else begin
          MKey := vkKeyScan(SendKeysString[I]);
          if (MKey <> INVALIDKEY) then begin
            SendKey(MKey, 1, True);
            PopUpShiftKeys;
          end else begin
            case IndexStr(SendKeysString[I], ['ô','î', 'â', 'ï',
                  'Â', 'À', 'É', 'È', 'Ê', 'ë', 'Ë', 'ê', 'Ô', 'Ï', 'Î', 'Ç', 'ç']) of
              0  : SendKeyCirconflex('o');
              1  : SendKeyCirconflex('i');
              2  : SendKeyCirconflex('a');
              3  : SendKeyTrema('i');
              4  : SendKeyCirconflex('A');
              5  : AltPlusKey('192');
              6  : AltPlusKey('201');
              7  : AltPlusKey('200');
              8  : AltPlusKey('202');
              9  : AltPlusKey('235');
              10 : AltPlusKey('203');
              11 : AltPlusKey('234');
              12 : AltPlusKey('212');
              13 : AltPlusKey('207');
              14 : AltPlusKey('206');
              15 : AltPlusKey('199');
              16 : AltPlusKey('231');

              -1 : MessageBox(0, 'Invalid key name!', 'SendKeys Routine', MB_ICONWARNING or MB_OK);
            end;
//            Inc(I);
          end;
        end;
        Inc(I);
      end;
    end;
  end;
  Result := true;
  PopUpShiftKeys;
end;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  SIMULATION DE L'APPUI DE TOUCHES WINDOWS + CHAR AVEC TEMPS DATTENTE
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

procedure SendKeyWin(iKey: Smallint; Tms: Cardinal);
begin
  keybd_event(VK_LWIN, MapVirtualKey(VK_LWIN, 0), KEYEVENTF_EXTENDEDKEY, 0);
  keybd_event(iKey, MapVirtualKey(iKey, 0), 0, 0);
  WaitForKey( Tms );
  keybd_event(iKey, MapVirtualKey(iKey, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_LWIN, MapVirtualKey(VK_LWIN, 0), KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0)
end;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  SIMULATION DE L'APPUI DE TOUCHES AVEC TEMPS DATTENTE ET
  LA DISTINCTION ENTRE LES TOUCHES ALT,SHIFT,CRTL - GEUCHE ET DROITE
+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

procedure SendKey(iKey: Smallint; Tms: Cardinal; Specials: TSpecials = []);
begin
  TPushKeyCombine.SimulateEx(iKey, Tms, Specials);
end;


const
  ExtendedVKeys : set of byte =
  [VK_Up,
   VK_Down,
   VK_Left,
   VK_Right,
   VK_Home,
   VK_End,
   VK_Prior,  {PgUp}
   VK_Next,   {PgDn}
   VK_Insert,
   VK_Delete
  ];

procedure Delay(ms: Cardinal);
var
  S : Cardinal;
  C : Cardinal;
begin
  C := 0;
  S := GetTickCount + ms;
  with Application do
    repeat
      Sleep( 10 );
      C := C + 10;
      if C mod 90 = 0 then Application.ProcessMessages;
    until Application.Terminated or (GetTickCount > S)
end;

procedure Wait(Time: Cardinal);
begin
  Delay( Time );
end;

procedure WaitForKey(Time: Cardinal);
begin
  Delay( Time );
end;

{ TPushKeyCombine }

function TPushKeyCombine.AltPressed: Boolean;
begin
  Result := not FShift_pressed and not FCtrl_pressed and FAlt_pressed
end;

function TPushKeyCombine.Alt_CtrlPressed: Boolean;
begin
  Result := not FShift_pressed and FCtrl_pressed and FAlt_pressed
end;

procedure TPushKeyCombine.Alt_Ctrl_Key(iKey: Smallint; iTime: Cardinal);
var
  Flag: Cardinal;
begin
  if not FUpdated then
  try
    FUpDated := True;
    Flag := 0;
    if (iKey in ExtendedVKeys) then Flag := KEYEVENTF_EXTENDEDKEY;
    if FCTRL_LEFT
      then keybd_event(VK_LCONTROL, MapVirtualKey(VK_LCONTROL, 0), 0, 0)
      else keybd_event(VK_RCONTROL, MapVirtualKey(VK_RCONTROL, 0), KEYEVENTF_EXTENDEDKEY, 0);
    if FALT_LEFT
      then keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU, 0), 0, 0)
      else keybd_event(VK_RMENU, MapVirtualKey(VK_RMENU, 0), KEYEVENTF_EXTENDEDKEY, 0);
    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag, 0);

    WaitForKey( iTime );

    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag or KEYEVENTF_KEYUP, 0);
    if FALT_LEFT
      then keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RMENU, MapVirtualKey(VK_RMENU, 0), KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
    if FCTRL_LEFT
      then keybd_event(VK_LCONTROL, MapVirtualKey(VK_LCONTROL, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RCONTROL, MapVirtualKey(VK_RCONTROL, 0),KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
  finally
    FUpdated := False;
  end;
end;

procedure TPushKeyCombine.Shift_Ctrl_Key(iKey: Smallint; iTime: Cardinal);
var
  Flag: Cardinal;
begin
  if not FUpdated then
  try
    FUpDated := True;
    Flag := 0;
    if (iKey in ExtendedVKeys) then Flag := KEYEVENTF_EXTENDEDKEY;
    if FCTRL_LEFT
      then keybd_event(VK_LCONTROL, MapVirtualKey(VK_LCONTROL, 0), 0, 0)
      else keybd_event(VK_RCONTROL, MapVirtualKey(VK_RCONTROL, 0), KEYEVENTF_EXTENDEDKEY, 0);
    if FALT_LEFT
      then keybd_event(VK_LSHIFT, MapVirtualKey(VK_LSHIFT, 0), 0, 0)
      else keybd_event(VK_RSHIFT, MapVirtualKey(VK_RSHIFT, 0), KEYEVENTF_EXTENDEDKEY, 0);
    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag, 0);

    WaitForKey( iTime );

    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag or KEYEVENTF_KEYUP, 0);
    if FALT_LEFT
      then keybd_event(VK_LSHIFT, MapVirtualKey(VK_LSHIFT, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RSHIFT, MapVirtualKey(VK_RSHIFT, 0), KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
    if FCTRL_LEFT
      then keybd_event(VK_LCONTROL, MapVirtualKey(VK_LCONTROL, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RCONTROL, MapVirtualKey(VK_RCONTROL, 0),KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
  finally
    FUpdated := False;
  end;
end;

procedure TPushKeyCombine.Shift_alt_Key(iKey: Smallint; iTime: Cardinal);
var
  Flag: Cardinal;
begin
  if not FUpdated then
  try
    FUpDated := True;
    Flag := 0;
    if (iKey in ExtendedVKeys) then Flag := KEYEVENTF_EXTENDEDKEY;
    if FCTRL_LEFT
      then keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU, 0), 0, 0)
      else keybd_event(VK_RMENU, MapVirtualKey(VK_RMENU, 0), KEYEVENTF_EXTENDEDKEY, 0);
    if FALT_LEFT
      then keybd_event(VK_LSHIFT, MapVirtualKey(VK_LSHIFT, 0), 0, 0)
      else keybd_event(VK_RSHIFT, MapVirtualKey(VK_RSHIFT, 0), KEYEVENTF_EXTENDEDKEY, 0);
    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag, 0);

    WaitForKey( iTime );

    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag or KEYEVENTF_KEYUP, 0);
    if FALT_LEFT
      then keybd_event(VK_LSHIFT, MapVirtualKey(VK_LSHIFT, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RSHIFT, MapVirtualKey(VK_RSHIFT, 0), KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
    if FCTRL_LEFT
      then keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RMENU, MapVirtualKey(VK_RMENU, 0),KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
  finally
    FUpdated := False;
  end;
end;

procedure TPushKeyCombine.Alt_Key(iKey: Smallint; iTime: Cardinal);
var
  Flag: Cardinal;
begin
  if not FUpdated then
  try
    FUpDated := True;
    Flag := 0;
    if (iKey in ExtendedVKeys) then Flag := KEYEVENTF_EXTENDEDKEY;
    if FALT_LEFT
      then keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU, 0), 0, 0)
      else keybd_event(VK_RMENU, MapVirtualKey(VK_RMENU, 0), KEYEVENTF_EXTENDEDKEY, 0);
    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag, 0);

    WaitForKey( iTime );

    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag or KEYEVENTF_KEYUP, 0);
    if FALT_LEFT
      then keybd_event(VK_LMENU, MapVirtualKey(VK_LMENU, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RMENU, MapVirtualKey(VK_RMENU, 0), KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
  finally
    FUpdated := False;
  end;
end;

constructor TPushKeyCombine.Create;
begin
  inherited Create;
  Initialize;
end;

function TPushKeyCombine.CtrlPressed: Boolean;
begin
  Result := not FShift_pressed and FCtrl_pressed and not FAlt_pressed
end;

procedure TPushKeyCombine.Ctrl_Key(iKey: Smallint; iTime: Cardinal);
var
  Flag: Cardinal;
begin
  if not FUpdated then
  try
    FUpDated := True;
    Flag := 0;
    if (iKey in ExtendedVKeys) then Flag := KEYEVENTF_EXTENDEDKEY;
    if FCTRL_LEFT
      then keybd_event(VK_LCONTROL, MapVirtualKey(VK_LCONTROL, 0), 0, 0)
      else keybd_event(VK_RCONTROL, MapVirtualKey(VK_RCONTROL, 0), KEYEVENTF_EXTENDEDKEY, 0);
    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag, 0);

    WaitForKey( iTime );

    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag or KEYEVENTF_KEYUP, 0);
    if FCTRL_LEFT
      then keybd_event(VK_LCONTROL, MapVirtualKey(VK_LCONTROL, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RCONTROL, MapVirtualKey(VK_RCONTROL, 0), KEYEVENTF_EXTENDEDKEY or KEYEVENTF_KEYUP, 0);
  finally
    FUpdated := False;
  end;
end;

procedure TPushKeyCombine.For_Key(iKey: Smallint; iTime: Cardinal);
var
  Flag: Cardinal;
begin
  if not FUpdated then
  try
    FUpDated := True;
    Flag := 0;
    if (iKey in ExtendedVKeys) then Flag := KEYEVENTF_EXTENDEDKEY;
    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag, 0);

    WaitForKey( iTime );

    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag or KEYEVENTF_KEYUP, 0);
  finally
    FUpdated := False;
  end;
end;

procedure TPushKeyCombine.Initialize;
begin
  FUpdated       := False;
  FSHIFT_LEFT    := True;
  FALT_LEFT      := True;
  FCTRL_LEFT     := True;
  FShift_pressed := False;
  FCtrl_pressed  := False;
  FAlt_pressed   := False;
  FResume        := kp_none;
end;

procedure TPushKeyCombine.KeyPress(Key: Char; Tms: Cardinal;
  Specials: TSpecials);
begin
  KeyPressEx( VkKeyScan(Key), Tms, Specials )
end;

procedure TPushKeyCombine.KeyPressEx(iKey: Smallint; Tms: Cardinal;
  Specials: TSpecials);
begin
  SetSpecials( Specials );
  case FResume of
    kp_none           : For_Key           ( iKey, Tms );
    kp_shift          : Shift_Key         ( iKey, Tms );
    kp_ctrl           : Ctrl_Key          ( iKey, Tms );
    kp_alt            : Alt_Key           ( iKey, Tms );
    kp_alt_ctrl       : Alt_Ctrl_Key      ( iKey, Tms );
    kp_shift_ctrl     : Shift_Ctrl_Key    ( iKey, Tms );
    kp_shift_alt      : Shift_alt_Key     ( iKey, Tms );
  end;
end;

function TPushKeyCombine.NospecialPressed: Boolean;
begin
  Result := not FShift_pressed and not FCtrl_pressed and not FAlt_pressed
end;

procedure TPushKeyCombine.SetSpecials(const ASpecials: TSpecials);
begin
  if ss_rshift in ASpecials then begin FSHIFT_LEFT := False; FShift_pressed := True end;
  if ss_lshift in ASpecials then begin FSHIFT_LEFT := True;  FShift_pressed := True end;

  if ss_rctrl  in ASpecials then begin FCTRL_LEFT  := False; FCtrl_pressed := True end;
  if ss_lctrl  in ASpecials then begin FCTRL_LEFT  := True;  FCtrl_pressed := True end;

  if ss_ralt   in ASpecials then begin FALT_LEFT   := False; FAlt_pressed := True end;
  if ss_lalt   in ASpecials then begin FALT_LEFT   := True;  FAlt_pressed := True end;

  if NospecialPressed  then FResume := kp_none;
  if ShiftPressed      then FResume := kp_shift;
  if CtrlPressed       then FResume := kp_ctrl;
  if AltPressed        then FResume := kp_alt;
  if Alt_CtrlPressed   then FResume := kp_alt_ctrl;
  if Shift_CtrlPressed then FResume := kp_shift_ctrl;
  if Shift_AltPressed  then FResume := kp_shift_alt;
end;

function TPushKeyCombine.ShiftPressed: Boolean;
begin
  Result := FShift_pressed and not FCtrl_pressed and not FAlt_pressed
end;

function TPushKeyCombine.Shift_CtrlPressed : Boolean;
begin
  Result := FShift_pressed and FCtrl_pressed and not FAlt_pressed
end;

function TPushKeyCombine.Shift_AltPressed: Boolean;
begin
  Result := FShift_pressed and not FCtrl_pressed and FAlt_pressed
end;

procedure TPushKeyCombine.Shift_Key(iKey: Smallint; iTime: Cardinal);
var
  Flag: Cardinal;
begin
  if not FUpdated then
  try
    FUpDated := True;
    Flag := 0;
    if (iKey in ExtendedVKeys) then Flag := KEYEVENTF_EXTENDEDKEY;
    if FSHIFT_LEFT
      then keybd_event(VK_LSHIFT, MapVirtualKey(VK_LSHIFT, 0), 0, 0)
      else keybd_event(VK_RSHIFT, MapVirtualKey(VK_RSHIFT, 0), 0, 0);
    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag, 0);

    WaitForKey( iTime );

    keybd_event(iKey, MapVirtualKey(iKey, 0), Flag or KEYEVENTF_KEYUP, 0);
    if FSHIFT_LEFT
      then keybd_event(VK_LSHIFT, MapVirtualKey(VK_LSHIFT, 0), KEYEVENTF_KEYUP, 0)
      else keybd_event(VK_RSHIFT, MapVirtualKey(VK_RSHIFT, 0), KEYEVENTF_KEYUP, 0);
  finally
    FUpdated := False;
  end;
end;

class procedure TPushKeyCombine.Simulate(Key: Char; Tms: Cardinal;
  Specials: TSpecials);
begin
  with TPushKeyCombine.Create do
  try
     KeyPress(Key, Tms, Specials)
  finally
    Free
  end
end;

class procedure TPushKeyCombine.SimulateEx(iKey: Smallint; Tms: Cardinal;
  Specials: TSpecials);
begin
  with TPushKeyCombine.Create do
  try
     KeyPressEx(iKey, Tms, Specials)
  finally
    Free
  end
end;

procedure SendKey(sCode: string); overload;
var
  i    : Integer;
  x    : Smallint;
begin
  try
    keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
    for i := 1 to Length(sCode) do begin
      case sCode[i] of
        '0' : x := VK_NUMPAD0;
        '1' : x := VK_NUMPAD1;
        '2' : x := VK_NUMPAD2;
        '3' : x := VK_NUMPAD3;
        '4' : x := VK_NUMPAD4;
        '5' : x := VK_NUMPAD5;
        '6' : x := VK_NUMPAD6;
        '7' : x := VK_NUMPAD7;
        '8' : x := VK_NUMPAD8;
        '9' : x := VK_NUMPAD9;
        else x := VkKeyScan(sCode[i])
      end;
      keybd_event(x, MapVirtualKey(x, 0), 0, 0);
      keybd_event(x, MapVirtualKey(x, 0), KEYEVENTF_KEYUP, 0);
    end;
  finally
    keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0)
  end
end;


{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

                          FOR MOUSE CLICK

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

procedure MouseLeftClic;
begin
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0)
end;

procedure MouseRightClic;
begin
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0)
end;

procedure MouseMiddleClic;
begin
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0)
end;

procedure MouseDoubleClic;
begin
  MouseLeftClic;
  MouseLeftClic;
end;

procedure MouseLeft_(Tms: Cardinal);
begin
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0)
end;

procedure MouseLeft_Shift_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseLeft_Ctrl_(Tms: Cardinal);
begin
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseLeft_Alt_(Tms: Cardinal);
begin
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseLeft_Shift_Ctrl_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseLeft_Shift_Alt_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseLeft_Alt_Crtl_(Tms: Cardinal);
begin
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseLeft_Shift_Alt_Crtl_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_LEFTUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

type
  TShiftKind = (sk_none, sk_shift, sk_alt, sk_ctrl, sk_shift_alt, sk_shift_ctrl,
                sk_alt_ctrl, sk_shift_alt_ctrl);


function ShiftStateToKind(const Shift: TShiftState):TShiftKind;
begin
  Result := sk_none;
  if ssShift in Shift then begin
    Result := sk_shift;
  end;
  if ssAlt in Shift then begin
    case Result of
      sk_none      : Result := sk_alt;
      sk_shift     : Result := sk_shift_alt;
    end
  end;
  if ssCtrl in Shift then begin
    case Result of
      sk_none      : Result := sk_ctrl;
      sk_shift     : Result := sk_shift_ctrl;
      sk_alt       : Result := sk_alt_ctrl;
      sk_shift_alt : Result := sk_shift_alt_ctrl;
    end
  end;
end;

procedure MouseLeftClic(Tms: Cardinal; Shift: TShiftState);
begin
  case ShiftStateToKind(Shift) of
    sk_none                : MouseLeft_(Tms);
    sk_shift               : MouseLeft_Shift_(Tms);
    sk_alt                 : MouseLeft_Alt_(Tms);
    sk_ctrl                : MouseLeft_Ctrl_(Tms);
    sk_shift_alt           : MouseLeft_Shift_Alt_(Tms);
    sk_shift_ctrl          : MouseLeft_Shift_Ctrl_(Tms);
    sk_alt_ctrl            : MouseLeft_Alt_Crtl_(Tms);
    sk_shift_alt_ctrl      : MouseLeft_Shift_Alt_Crtl_(Tms);
  end;
end;

procedure MouseRight_(Tms: Cardinal);
begin
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0)
end;

procedure MouseRight_Shift_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseRight_Alt_(Tms: Cardinal);
begin
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseRight_Ctrl_(Tms: Cardinal);
begin
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseRight_Shift_Alt_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseRight_Shift_Ctrl_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseRight_Alt_Crtl_(Tms: Cardinal);
begin
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseRight_Shift_Alt_Crtl_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_RIGHTDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_RIGHTUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseRightClic(Tms: Cardinal; Shift: TShiftState);
begin
  case ShiftStateToKind(Shift) of
    sk_none                : MouseRight_(Tms);
    sk_shift               : MouseRight_Shift_(Tms);
    sk_alt                 : MouseRight_Alt_(Tms);
    sk_ctrl                : MouseRight_Ctrl_(Tms);
    sk_shift_alt           : MouseRight_Shift_Alt_(Tms);
    sk_shift_ctrl          : MouseRight_Shift_Ctrl_(Tms);
    sk_alt_ctrl            : MouseRight_Alt_Crtl_(Tms);
    sk_shift_alt_ctrl      : MouseRight_Shift_Alt_Crtl_(Tms);
  end
end;

procedure MouseMiddle_(Tms: Cardinal);
begin
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0)
end;

procedure MouseMiddle_Shift_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseMiddle_Alt_(Tms: Cardinal);
begin
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseMiddle_Ctrl_(Tms: Cardinal);
begin
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseMiddle_Shift_Alt_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseMiddle_Shift_Ctrl_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseMiddle_Alt_Crtl_(Tms: Cardinal);
begin
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseMiddle_Shift_Alt_Crtl_(Tms: Cardinal);
begin
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), 0, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), 0, 0);
  mouse_event(MOUSEEVENTF_MIDDLEDOWN, 0, 0, 0, 0);
  WaitForKey( Tms );
  mouse_event(MOUSEEVENTF_MIDDLEUP,   0, 0, 0, 0);
  keybd_event(VK_CONTROL, MapVirtualKey(VK_CONTROL, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_MENU, MapVirtualKey(VK_MENU, 0), KEYEVENTF_KEYUP, 0);
  keybd_event(VK_SHIFT, MapVirtualKey(VK_SHIFT, 0), KEYEVENTF_KEYUP, 0)
end;

procedure MouseMiddleClic(Tms: Cardinal; Shift: TShiftState);
begin
  case ShiftStateToKind(Shift) of
    sk_none                : MouseMiddle_(Tms);
    sk_shift               : MouseMiddle_Shift_(Tms);
    sk_alt                 : MouseMiddle_Alt_(Tms);
    sk_ctrl                : MouseMiddle_Ctrl_(Tms);
    sk_shift_alt           : MouseMiddle_Shift_Alt_(Tms);
    sk_shift_ctrl          : MouseMiddle_Shift_Ctrl_(Tms);
    sk_alt_ctrl            : MouseMiddle_Alt_Crtl_(Tms);
    sk_shift_alt_ctrl      : MouseMiddle_Shift_Alt_Crtl_(Tms);
  end
end;

function GetMousePosition: TPoint;
begin
  GetCursorPos( Result )
end;

procedure SetMousePosition(const Position: TPoint);
begin
  SetCursorPos(Position.X, Position.Y)
end;

procedure SetMousePosition(x,y: Integer);
begin
  SetMousePosition( Point(x,y) )
end;




{...............................................................................
                               KeyboardState
...............................................................................1}

function KeyBoardState:TByteSet;
begin
  Result := [];
  if GetKeyState( VK_CAPITAL ) = 1 then Result := Result + [VK_CAPITAL];
  if GetKeyState( VK_NUMLOCK ) = 1 then Result := Result + [VK_NUMLOCK];
  if GetKeyState( VK_PAUSE   ) = 1 then Result := Result + [VK_PAUSE];
  if GetKeyState( VK_SCROLL  ) = 1 then Result := Result + [VK_SCROLL];
end;

function CapsLocKEnable: Boolean;
begin
  Result := VK_CAPITAL in KeyBoardState
end;



{...............................................................................
               Managed extended char by codename (cf unit KEYSDEF)
...............................................................................}

function CodeKeyToAltNum(const Value: string): string;
begin
  Result := StrKeyToCode(Value)
end;

function CanWrite:Boolean;
var
  Default: Boolean;
begin
  Default := KeyReadBoolean(IniKey, 'pendefault', False);
  Result  := KeyReadBoolean(AppKey, 'pen',        Default);
end;

procedure ProcessOnCodeTouche(const Value: string); begin
  if CanWrite then SendKey( CodeKeyToAltNum(Value) )
end;

procedure ProcessOnTouche(const Value: string); begin
  if CanWrite then SendKeys(PChar(Value), True)
end;

procedure ProcessOnKey(const Value: string);
var
  Entry: string;
begin
  Entry := Value;
  if Value <> 'null' then begin
    case KeyIdentifier(Value) of
      kk_touche                    : ProcessOnTouche           ( Value );
      kk_codetouche                : ProcessOnCodeTouche       ( Value );
      kk_function..high(TKeyKind)  : TShortcuts.Execute        ( Value );
    end;
    LastCommandToReg(Entry)
  end
end;


{ --- Lire/écrire la dernière commande --- }
function RetrieveLastCommand: string;
begin
  Result := ReadLastCommand
end;

function IsAgain(const ASt: string):Boolean;
begin
  Result := IndexStr(AnsiLowerCase(ASt), ['again', 'again2', 'again3', 'again5']) > -1
end;

procedure LastCommandToReg(const ASt: string);
begin
  if AllowedAgain(ASt) then if not IsAgain(ASt) then WriteLastCommand(ASt)
end;

initialization
  AllShortCut := Shortcuts;
end.
