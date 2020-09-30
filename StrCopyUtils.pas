unit StrCopyUtils;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Dialogs, System.StrUtils, uRegistry;

function GetAfterStr(const Text, Str_Pattern: string):string;
function GetBeforStr(const Text, Str_Pattern: string):string;
function GetBetween(const Text, Start_Pattern, Finish_Pattern: string):string;
function GetBeaftStr(const Text, Str_Pattern: string; var StartStr, EndStr: string):string;
function DelSubStr(const Text, Str_Pattern: string):string;


function  NameStr(const Text: string; Sep : string = '='):string;
function  ValueStr(const Text: string; Sep : string = '='):string;
procedure GetNameValue(const Text: string; var AName, AValue: string; Sep : string = '=');
function  SetNameValue(const AName, AValue: string; Sep : string = '='):string;

{Replace a substring in a text}
function SubstrSeplaceInText(const Src, SubStr, ReplaceStr: string):string; overload;
function SubstrSeplaceInText(const Src, Prefix, SubStr, ReplaceStr, Suffix: string):string; overload;


{Replace "' ' by "'"}
function QuoteFix(const ASt: string):string;
function ColumnFix(const ASt: string):string;
function ProperQuoted(const ASt: string):string;

{Supression d'un caractère en début et fin de chaîne}
function CharDel(const Text: string; const ACh: Char):string;
function QuotedDel(Text: string):string;       // '
function DblQuotedDel(Text: string):string;    // "
function BraceDel(Text: string):string;        // { }
function BracketDel(Text: string):string;      // [ ]
function ParenthesisDel(Text: string):string;  // ( )
function SlashDel(Text: string):string;        // /
function AntiSlashDel(Text: string):string;    // \


{Atext sur une seule ligne de text}
function AddStrToLst(const AText: string; Item: string):string;
function ConcatStr(const AText: string; Item: string; Sep : string = ';'):string;


{Catalogs extract value}
function CatalogExtractStr(const List: TStrings; const AName: string):string; overload;
function CatalogExtractStr(const List: TStrings; Index: Integer):string; overload;

function CatalogExtractStr(const AText, AName: string):string; overload;
function CatalogExtractStr(const AText: string; Index: Integer):string; overload;


{Catalogs replace value}
procedure CatalogReplaceStr(const List: TStrings; const AName, AValue: string); overload;
procedure CatalogReplaceStr(const List: TStrings; Index : Integer; const AValue: string); overload;

function  CatalogReplaceStr(const AText, AName, AValue: string):string; overload;
function  CatalogReplaceStr(const AText : string; Index: Integer; const AValue: string):string; overload;


{Catalogs Initialize}
procedure CatalogInit(const List: TStrings; AFields: array of string); overload;

function  CatalogInit(AFields: array of string):string; overload;

{List Routines}
function  ListFusion(const Src, New: string):string;
function  Belongs(const Src, AItem: string):Boolean;
function  TextFileContent(const FileName: string):string;
procedure SaveTextFile(const FileName, Content: string);
function  TextToList(const AText: string; const Sep: string; AddSep: Boolean = False): string;


{Tick}
function TickCount:Cardinal;
function TickCountStr:string;
function TickStrToCardinal(const ATick: string):Cardinal;
function SetTickText(const AText: string):string;
function TickToDuration(const TStart, TEnd: Cardinal):TTime;

type
  TTickStackFifo = class
  private
    FPile: TStringList;
    function  GetPileText: string;
    procedure SetPileText(const Value: string);
    function  ExtractFirstItem:string;
    function  ReadHighItem:string;

  public
    procedure Clear;
    function  Count:Integer;
    function  HighItem(var Tick: Cardinal; var Value: string):Boolean;
    procedure Stack(const Value: string);
    function  Unstack(var Tick: Cardinal; var Value: string):Boolean;
    procedure SaveToRegistry;
    procedure LoadFromRegistry;

    property PileText: string read GetPileText write SetPileText;

    constructor Create(const APileStr: string);
    destructor Destroy; override;
  end;



implementation

function GetAfterStr(const Text, Str_Pattern: string):string;
var
  P : Integer;
begin
  Result := EmptyStr;
  P := Pos(Str_Pattern, Text);
  if P > 0 then Result := Trim( Copy(Text, P + Length(Str_Pattern), Length(Text)) );
end;

function GetBeforStr(const Text, Str_Pattern: string):string;
var
  P : Integer;
begin
  Result := EmptyStr;
  P := Pos(Str_Pattern, Text);
  if P > 0 then Result := Trim( Copy(Text, 1, P - 1 ) );
end;

function GetBeaftStr(const Text, Str_Pattern: string; var StartStr,
  EndStr: string):string;
var
  P : Integer;
begin
  StartStr := EmptyStr;
  EndStr   := EmptyStr;
  P := Pos(Str_Pattern, Text);
  if P > 0 then begin
    StartStr := Trim( Copy(Text, 1, P - 1 ) );
    EndStr   := Trim( Copy(Text, P + Length(Str_Pattern), Length(Text)) );
    Result   := StartStr + EndStr;
  end else begin
    StartStr := Text;
    Result   := Text;
  end;
end;

function DelSubStr(const Text, Str_Pattern: string):string;
var
  Find : Boolean;
begin
  Result := Text;
  Find := AnsiPos(AnsiUpperCase(Str_Pattern), AnsiUpperCase(Text)) > 0;
  if Find then Result := Trim(ReplaceText(Text, Str_Pattern, EmptyStr));
end;

function GetBetween(const Text, Start_Pattern, Finish_Pattern: string):string;
begin
  Result := GetAfterStr(Text, Start_Pattern);
  Result := GetBeforStr(Result, Finish_Pattern);
end;

function NameStr(const Text: string; Sep : string):string;
begin
  Result := GetBeforStr(Text, Sep)
end;

function ValueStr(const Text: string; Sep : string):string;
begin
  Result := GetAfterStr(Text, Sep)
end;

procedure GetNameValue(const Text: string; var AName, AValue: string;
  Sep : string);
begin
  AName  := NameStr(Text, Sep);
  AValue := ValueStr(Text, Sep);
end;

function SetNameValue(const AName, AValue: string; Sep : string):string;
begin
  Result := Format('%s%s%s', [AName, Sep, AValue])
end;

function CharDel(const Text: string; const ACh: Char):string;
var
  L : Integer;
begin
  Result := Trim(Text);
  if Length( Result ) = 0 then Exit;

  if Result[1] = ACh then Result := Copy(Result, 2, Length(Result) );
  L := Length(Result);
  if Result[L] = ACh then Result := Copy(Result, 1, L - 1);
end;


function QuotedDel(Text: string):string;
begin
  Result := CharDel(Text, '''')
end;

function DblQuotedDel(Text: string):string;
begin
  Result := CharDel(Text, '"')
end;

function BraceDel(Text: string):string;
begin
  Result := CharDel(Text, '{');
  Result := CharDel(Result, '}');
end;

function BracketDel(Text: string):string;
begin
  Result := CharDel(Text, '[');
  Result := CharDel(Result, ']');
end;

function ParenthesisDel(Text: string):string;
begin
  Result := CharDel(Text, '(');
  Result := CharDel(Result, ')');
end;

function SlashDel(Text: string):string;
begin
  Result := CharDel(Text, '/')
end;

function AntiSlashDel(Text: string):string;
begin
  Result := CharDel(Text, '\')
end;

function AddStrToLst(const AText: string; Item: string):string;
begin
  with TStringList.Create do
  try
    Text := AText;
    Add( Item );
    Result := Text;
  finally
    Free
  end
end;

function ConcatStr(const AText: string; Item: string; Sep : string):string;
begin
  Result := Trim( AText );
  if Result = EmptyStr then Result := Item
    else Result := Format('%s%s%s', [Result, Sep, Item])
end;

function CatalogExtractStr(const List: TStrings; const AName: string):string;
var
  Index : Integer;
begin
  Result := EmptyStr;
  with List do begin
    Index := IndexOfName( AName );
    if Index > -1 then Result := ValueFromIndex[ Index ]
  end
end;

function CatalogExtractStr(const List: TStrings; Index: Integer):string;
begin
  Result := EmptyStr;
  with List do
    if (Index > -1) and (Index < Count) then Result := ValueFromIndex[ Index ]
end;

function CatalogExtractStr(const AText, AName: string):string;
var
  Index : Integer;
begin
  Result := EmptyStr;
  with TStringList.Create do
  try
    Text   := AText;
    Index := IndexOfName( AName );
    if Index > -1 then Result := ValueFromIndex[ Index ]
  finally
    Free
  end
end;

function CatalogExtractStr(const AText: string; Index: Integer):string;
begin
  Result := EmptyStr;
  with TStringList.Create do
  try
    Text  := AText;
    if (Index > -1) and (Index < Count) then Result := ValueFromIndex[ Index ]
  finally
    Free
  end
end;

procedure CatalogReplaceStr(const List: TStrings; const AName, AValue: string);
{AList <- le catalog complet modifié}
var
  Index : Integer;
begin
  with List do begin
    Index := IndexOfName( AName );
    if Index > -1 then ValueFromIndex[Index] := AValue
  end;
end;

procedure CatalogReplaceStr(const List: TStrings; Index : Integer; const AValue: string);
{AList <- le catalog complet modifié}
begin
  with List do
    if (Index > -1) and (Index < Count) then ValueFromIndex[Index] := AValue
end;

function CatalogReplaceStr(const AText, AName, AValue: string):string;
{Result <- le catalog complet modifié}
var
  Index : Integer;
begin
  Result := AText;
  with TStringList.Create do
  try
    Text  := AText;
    Index := IndexOfName( AName );
    if Index > -1 then begin
      ValueFromIndex[Index] := AValue;
      Result := Text;
    end;
  finally
    Free
  end
end;

function CatalogReplaceStr(const AText : string; Index: Integer; const AValue: string):string;
{Result <- le catalog complet modifié}
begin
  Result := AText;
  with TStringList.Create do
  try
    Text  := AText;
    if (Index > -1) and (Index < Count) then begin
      ValueFromIndex[Index] := AValue;
      Result := Text;
    end;
  finally
    Free
  end
end;

procedure CatalogInit(const List: TStrings; AFields: array of string);
var
  I : Integer;
begin
  with List do begin
    Clear;
    for I := Low( AFields ) to High( AFields ) do
      Add( Format('%s=', [AFields[I]]) );
  end
end;

function CatalogInit(AFields: array of string):string;
var
  I : Integer;
begin
  with TStringList.Create do
  try
    for I := Low( AFields ) to High( AFields ) do
      Add( Format('%s=', [AFields[I]]) );
    Result := Text;
  finally
    Free
  end
end;

function TickCount:Cardinal;
begin
  Result := GetTickCount;
end;

function TickCountStr:string;
begin
  Result := Format('%d', [TickCount])
end;

function TickStrToCardinal(const ATick: string):Cardinal;
begin
  try
    Result := StrToInt( ATick )
  except
    on E: Exception do Result := 0
  end
end;

function SetTickText(const AText: string):string;
begin
  Result := SetNameValue(TickCountStr, AText)
end;

function TickToDuration(const TStart, TEnd: Cardinal):TTime;
var
  h, m, s: Word;
  d : Integer;
begin
  d := Trunc( (TEnd - TSTart) / MSecsPerSec );
  h := d div 3600;
  d := d mod 3600;
  m := d div 60;
  s := d mod 60;
  Result := EncodeTime(h,m,s,0)
end;

{ TTickStackFifo }

procedure TTickStackFifo.Clear;
begin
  with FPile do Clear;
end;

function TTickStackFifo.Count: Integer;
begin
  with FPile do Result := Count;
end;

constructor TTickStackFifo.Create(const APileStr: string);
begin
  inherited Create;
  FPile    := TStringList.Create;
  PileText := APileStr;
end;

destructor TTickStackFifo.Destroy;
begin
  FPile.Free;
  inherited;
end;

function TTickStackFifo.ExtractFirstItem: string;
begin
  with FPile do
  try
    Result := Strings[0];
    Delete(0);
  except
    Result := EmptyStr
  end;
end;

function TTickStackFifo.GetPileText: string;
begin
  with FPile do Result := Text
end;

function TTickStackFifo.HighItem(var Tick: Cardinal;
  var Value: string): Boolean;
var
  Buffer: string;
begin
  with FPile do if Count > 0 then begin
    GetNameValue(  ReadHighItem,      Buffer, Value );
    Tick        := TickStrToCardinal( Buffer );
    Result      := True;
  end else
    Result := False;
end;

procedure TTickStackFifo.LoadFromRegistry;
var
  Buffer: string;
begin
  KeyRead(BufferKey, 'TickStackFifo', Buffer);
  PileText := Buffer;
end;

function TTickStackFifo.ReadHighItem: string;
begin
  with FPile do Result := Strings[ Pred( Count ) ]
end;

procedure TTickStackFifo.SaveToRegistry;
begin
  with FPile do KeyWrite(BufferKey, 'TickStackFifo', Text);
end;

procedure TTickStackFifo.SetPileText(const Value: string);
begin
  with FPile do Text := Value
end;

procedure TTickStackFifo.Stack(const Value: string);
begin
  with FPile do Add( SetTickText( Value ) )
end;

function TTickStackFifo.Unstack(var Tick: Cardinal; var Value: string): Boolean;
var
  Buffer: string;
begin
  with FPile do if Count > 0 then begin
    GetNameValue(  ExtractFirstItem,  Buffer, Value );
    Tick        := TickStrToCardinal( Buffer );
    Result      := True;
  end else
    Result := False;
end;

function QuoteFix(const ASt: string):string;
begin
  Result := ASt;
  while Pos(''' ', Result) > 0 do Result := ReplaceText(Result, ''' ', '''');
  Result := ColumnFix( ProperQuoted(Result) );
end;

function ColumnFix(const ASt: string):string;
begin
  Result := ASt;
  while Pos(' , ', Result) > 0 do Result := ReplaceText(Result, ' , ', ', ');
  while Pos(' ,', Result) > 0 do Result := ReplaceText(Result, ' ,', '.');
end;

{Replace a substring in a text}
function SubstrSeplaceInText(const Src, SubStr, ReplaceStr: string):string;
begin
  Result := SubstrSeplaceInText(Src, EmptyStr, SubStr, ReplaceStr, EmptyStr)
end; {SubstrSeplaceInText}

function SubstrSeplaceInText(const Src, Prefix, SubStr, ReplaceStr, Suffix: string):string;
var
  LOut   : TStringList;
  Buffer : string;

  procedure Initialize; begin
    LOut := TStringList.Create;
    LOut.Sorted := True
  end;

  function Finalize:string; begin
    Result := LOut.Text;
    LOut.Free
  end;

  procedure ProcessOn(const S: string); begin
    if S = EmptyStr then Exit;
    Buffer := System.StrUtils.ReplaceStr(S, SubStr, ReplaceStr);
    Buffer := Format('%s%s%s', [Prefix, Buffer, Suffix]);
    Buffer := ProperQuoted(Buffer);
    with LOut do if IndexOf(Buffer) = -1 then Add(Buffer)
  end;

begin
  Result := Src;
  Initialize;
  with TStringList.Create do
  try
    Sorted := True;
    Text   := Src;
    with GetEnumerator do
    try
      while MoveNext do ProcessOn( Trim(Current) )
    finally
      Free
    end
  finally
    Free;
    Result := Finalize
  end
end; {SubstrSeplaceInText}

type
  TPosition = array[0..99] of Integer;

var
  Consons : array[0..19] of string =
    ( 'b', 'c', 'd', 'f', 'g', 'j', 'k', 'l', 'm',
      'n', 'p', 'q', 'r', 's', 't', 'v', 'w', 'x', 'z',
      'ç'
    ); //'h',

function IsConson(const C: Char):Boolean;
var
  S: string;
begin
  S := AnsiLowerCase(C);
  Result := IndexStr(S, Consons) > -1
end;

function ProperQuoted(const ASt: string):string;
var
  Positions  : TPosition;
  PosCount   : Integer;
  Buffer     : string;
  NextCar    : Char;
  SubStr     : string;
  ReplaceStr : string;
  Len        : Integer;

  procedure AddPosition(const Value: Integer); begin
    Positions[PosCount] := Value;
    Inc(PosCount);
  end;

  procedure PosRetrieve; var i : Integer; begin
    for i := 1 to Len do if Buffer[i] = '''' then
      if i < Len then AddPosition(i)
  end;

  procedure Initialize; begin
    PosCount := 0;
    Buffer   := AnsiLowerCase(ASt);
    Len      := Length(Buffer);
    PosRetrieve;
  end;

  procedure CharFix(const P: Integer); begin
    NextCar := Buffer[P + 1];
    if IsConson(NextCar) then begin
      SubStr     := '''' + NextCar;
      ReplaceStr := 'e ' + NextCar;
      Buffer     := System.StrUtils.ReplaceStr(Buffer, SubStr, ReplaceStr);
    end
  end;

  function Process:string; var i : Integer; begin
    for i := 0 to Pred(PosCount) do CharFix( Positions[i] );
    Result := Buffer;
  end;

begin
  Initialize;
  Result := Process;
end; {ProperQuoted}

{List Routines}
function ListFusion(const Src, New: string):string;
var
  L, N: TStringList;

  procedure Initialize; begin
    L := TstringList.Create;
    L.Sorted := True;
    L.Text   := Src;
    N := TStringList.Create;
    N.Sorted := True;
    N.Text   := New;
  end;

  procedure Finalize; begin
    Result := L.Text;
    L.Free;
    N.Free;
  end;

begin
  Initialize;
  try
    with N.GetEnumerator do
    try
      while MoveNext do if Trim(Current) <> EmptyStr then
        if L.IndexOf(Trim(Current)) = -1 then L.Add(Trim(Current));
      Result := L.Text
    finally
      Free
    end;
  finally
    Finalize;
  end
end; {ListFusion}

function Belongs(const Src, AItem: string):Boolean;
begin
  with TStringList.Create do
  try
    Text   := Src;
    Result := IndexOf(AItem) > -1
  finally
    Free
  end
end;

function TextFileContent(const FileName: string):string;
begin
  Result := EmptyStr;
  if FileExists(FileName) then with TStringList.Create do
  try
    LoadFromFile(FileName);
    Result := Text
  finally
    Free
  end
end;

procedure SaveTextFile(const FileName, Content: string);
begin
  with TStringList.Create do
  try
    Text := Trim( Content );
    SaveToFile( FileName );
  finally
    Free
  end
end;

function TextToList(const AText: string; const Sep: string;
  AddSep: Boolean): string;
var
  Buffer: string;

  function Ins(const ASt: string): string; begin
    if AddSep then Result := Format('%s%s', [ASt, Sep]) else Result := ASt
  end;

begin
  with TStringList.Create do
  try
    Buffer := AText;
    while Pos(Sep, Buffer) > 0 do begin
      Add( Ins( GetBeforStr(Buffer, Sep) ) );
      Buffer := GetAfterStr(Buffer, Sep)
    end;
    if Trim(Buffer) <> EmptyStr then Add( Ins( Buffer ) );
    Result := Text
  finally
    Free
  end
end; {StrToList}

end.
