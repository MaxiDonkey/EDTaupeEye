unit uProcessusManagment;

interface

uses
  Windows, SysUtils, Classes, AclApi, EncdDecd, Messages, TLHelp32, ShellAPI,
  Dialogs, PsAPI;

{Pour un même nom de processus on peut récupérer plusieurs PID
 autant de fois que le processus est lancé, la valeur est retournée dans le Result
 Attention il faut caster Les items de la liste des PIDs en CARDINAL pour les
 utiliser !!!
}
function PidsByProcessName(const AProcessName: string; var PIDs : string):Integer;
function ProcessNameByHandle(Handle:HWND):string;
function ProcessNameByPID( PID: Cardinal):string;
function HandleFromProcessName(const AProcessName: string; var AHandle: THandle;
  num : Integer = 0):Boolean;

function EnumProcessus:string;
function EnumProcessusName:string;
function EnumModules:string;
function EnumThreads:string;

function IsActivProcessus(const AProcessName: string):Boolean;

{Active windows : GetForegroundWindow:HWND}
procedure MinimizeActiveWindows;
procedure RestoreActiveWindows;
procedure CloseActiveWindows;

implementation

function ProcessNameByHandle(Handle:HWND):string;
var
  Pid      : DWord;
  SnapShot : HWND;
  Module   : TModuleEntry32;
begin
  Result := '';
  if not IsWindow(Handle) then exit;
  GetWindowThreadProcessId(Handle, @Pid); // récupere le pid
  Snapshot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, Pid); // creer un snapshot sur le pid
  try
    if Snapshot<>-1 then
    begin
      Module.dwSize := SizeOf(TModuleEntry32);
      if Module32First(Snapshot,Module) then result := Module.szModule; //szExePath; // recupere l'exe path
    end
  finally
    CloseHandle(Snapshot);
  end;
end;

{..............................................................................}

type
  TWinInfo = record
    PID: Cardinal;
    Handle: THandle;
    Found: Boolean;
  end;

function EnumWindowsProcForHandle(Wnd: HWND; lParam: lParam): BOOL; stdcall;
var
  AID : Cardinal;
begin
  Result := True;
  if (IsWindowVisible(Wnd) or IsIconic(wnd)) and
     ((GetWindowLong(Wnd, GWL_HWNDPARENT) = 0) or
      (GetWindowLong(Wnd, GWL_HWNDPARENT) = Integer(GetDesktopWindow))) and
     (GetWindowLong(Wnd, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0) then
  begin
    GetWindowThreadProcessId(Wnd, @AID);
    if AID = TWinInfo(Pointer(lParam)^).PID then begin
      TWinInfo(Pointer(lParam)^).Handle := Wnd;
      TWinInfo(Pointer(lParam)^).Found := True;
    end;
  end;
end;

{..............................................................................}

type
  PFindWindowsStruct = ^TFindWindowsStruct;
  TFindWindowsStruct = record 
    ProcessID: DWORD;
    HandleList: TList;
  end;

function EnumWindowsProc_PC(hwnd: HWND; lParam: LPARAM): boolean; stdcall;
var
  dwProcessId: DWORD;
begin
  Result:= false;
  if lParam <> 0 then begin
    GetWindowThreadProcessId(hwnd, dwProcessId);
    with PFindWindowsStruct(lParam)^ do if dwProcessID = ProcessID then
      HandleList.Add(Pointer(hwnd));
    Result:= true;
  end
end;

procedure FindProcessWindows(ProcessID: Integer; Handles: TList);
var
  findWindowsStruct: TFindWindowsStruct;
begin
  findWindowsStruct.ProcessID:= ProcessID;
  findWindowsStruct.HandleList:= Handles;
  EnumWindows(@EnumWindowsProc_PC, Integer(@findWindowsStruct));
end;


{..............................................................................}

function GetProcessID(const ProcessName: string; var ProcessIDs: string): Integer;
{Caster les ProcessIDs en integer pour les utiliser en dehors de la routine}
var
  C            : Boolean;
  FH           : THandle;
  FP           : TProcessEntry32;
begin
  FH := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FP.dwSize := Sizeof(FP);
  C := Process32First(FH, FP);
  with TStringList.Create do
  try
    while C do begin
      if (UpperCase(FP.szExeFile) = UpperCase(ProcessName)) then 
        Add( Format('%d', [FP.th32ProcessID]) );
      C := Process32Next(FH, FP);
    end;
    ProcessIDs := Text;
    Result := Count;
    CloseHandle(FH);
  finally
    Free
  end;
end;

function PidsByProcessName(const AProcessName: string; var PIDs: string):Integer;
begin
  Result := GetProcessID(AProcessName, PIDs);
end;

{..............................................................................}

function ProcessNameByPID( PID: Cardinal):string;
var
  SnapShot: Cardinal;
  ProcessEntry: TProcessEntry32;
begin
  Result := EmptyStr;
  SnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapShot = 0 then Exit;

  ProcessEntry.dwSize := SizeOf(ProcessEntry);
  if Process32First(SnapShot, ProcessEntry) then begin
    if ProcessEntry.th32ProcessID = PID then begin
      Result := ProcessEntry.szExeFile;
      Exit
    end;
    while Process32Next(SnapShot, ProcessEntry) do
      if ProcessEntry.th32ProcessID = PID then begin
        Result := ProcessEntry.szExeFile;
        Break
      end;
  end;
  CloseHandle(SnapShot);
end;

{..............................................................................}

function GetPathFromPID(PID: cardinal; ProcessName: string = ''): string;
var
  hProcess: THandle;
  Path: array[0..MAX_PATH - 1] of char;
begin
  hProcess := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, PID);
  Result := ProcessName;
  if hProcess <> 0 then
    try
      if GetModuleFileNameEx(hProcess, 0, Path, MAX_PATH) <> 0 then Result := Path
    finally
      CloseHandle(hProcess)
    end
end;

{..............................................................................}

function EnumModules:string;
var
  SnapShot: Cardinal;
  ModuleEntry: TModuleEntry32;
  C: Boolean;
begin
  with TStringList.Create do
  try
    SnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, 0);
    if SnapShot = 0 then Exit;

    ModuleEntry.dwSize := SizeOf(ModuleEntry);
    C := Module32First(SnapShot, ModuleEntry);
    while C do begin
      with ModuleEntry do Add(Format('%s; %d; %s', [szModule, modBaseSize, szExePath]));
      C := Module32Next(SnapShot, ModuleEntry);
    end;
    CloseHandle(SnapShot);
//    Sort;
    Result := Text
  finally
    Free
  end
end;

function EnumProcessus:string;
var
  SnapShot: Cardinal;
  ProcessEntry: TProcessEntry32;
  C: Boolean;
  H : THandle;
begin
  with TStringList.Create do
  try
    SnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if SnapShot = 0 then Exit;

    ProcessEntry.dwSize := SizeOf(ProcessEntry);
    C := Process32First(SnapShot, ProcessEntry);
    while C do begin
      with ProcessEntry do begin
        HandleFromProcessName( szExeFile, H );
        if IsWindowVisible( H ) and not IsIconic( H ) (*and (GetWindowLong(H, GWL_EXSTYLE) and WS_EX_TOOLWINDOW = 0) and

        ((GetWindowLong(H, GWL_HWNDPARENT) = 0) or (GetWindowLong(H, GWL_HWNDPARENT) = Int(GetDesktopWindow)))*)

        then
         Add(Format('%s; %d; %d; %d; %s',
          [szExeFile,
           th32ParentProcessID,
           th32ProcessID,
           cntThreads,
           GetPathFromPID(th32ProcessID, szExeFile)
          ]));
      end;
      C := Process32Next(SnapShot, ProcessEntry);
    end;
    CloseHandle(SnapShot);
//    Sort;
    Result := Text;
  finally
    Free
  end
end;

function EnumProcessusName:string;
var
  SnapShot: Cardinal;
  ProcessEntry: TProcessEntry32;
  C: Boolean;
begin
  with TStringList.Create do
  try
    SnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if SnapShot = 0 then Exit;

    ProcessEntry.dwSize := SizeOf(ProcessEntry);
    C := Process32First(SnapShot, ProcessEntry);
    while C do begin
      with ProcessEntry do Add(szExeFile);
      C := Process32Next(SnapShot, ProcessEntry);
    end;
    CloseHandle(SnapShot);
//    Sort;
    Result := Text;
  finally
    Free
  end
end;

function EnumThreads:string;
var
  SnapShot: Cardinal;
  ThreadEntry: TThreadEntry32;
  C: Boolean;
begin
  with TStringList.Create do
  try
    SnapShot := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
    if SnapShot = 0 then Exit;

    ThreadEntry.dwSize := SizeOf(ThreadEntry);
    C := Thread32First(SnapShot, ThreadEntry);
    while C do with ThreadEntry do begin
      Add( Format('%s; %d; %d',
        [ProcessNameByPID( ThreadEntry.th32OwnerProcessID ),
         th32OwnerProcessID,
         tpBasePri
        ] ));
        
      C := Thread32Next(SnapShot, ThreadEntry);
    end;
    CloseHandle(SnapShot);
//    Sort;
    Result := Text
  finally
    Free
  end
end;

function IsActivProcessus(const AProcessName: string):Boolean;
begin
  with TStringList.Create do
  try
    Text := EnumProcessusName;
    Result := IndexOf(AProcessName) > -1
  finally
    Free
  end
end;

{..............................................................................}

function HandleFromProcessName(const AProcessName: string; var AHandle: THandle;
  num : Integer):Boolean;
var
  WinInfo : TWinInfo;
  PIDs    : string;
begin
  Result  := False;
  AHandle := 0;
  if GetProcessID(AProcessName, PIDs) >  0 then with TStringList.Create do
  try
    Text := PIDs;
    if num < Count then begin
      WinInfo.Found := False;
      WinInfo.PID   := StrToInt( Strings[num] );
      EnumWindows(@EnumWindowsProcForHandle, Integer(@WinInfo));
      Result := WinInfo.Found;
      if Result then AHandle := WinInfo.Handle;
    end;
  finally
    Free
  end
end;

{...............................................................................
               Active windows : GetForegroundWindow:HWND
...............................................................................}

procedure MinimizeActiveWindows;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_MINIMIZE, 0)
end;

procedure RestoreActiveWindows;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_RESTORE, 0)
end;

procedure CloseActiveWindows;
begin
  SendMessage(GetForegroundWindow, WM_SYSCOMMAND, SC_CLOSE, 0)
end;

end.
