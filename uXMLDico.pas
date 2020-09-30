unit uXMLDico;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.Grids, Vcl.DBGrids,
  Datasnap.DBClient, Vcl.StdCtrls, Vcl.ExtCtrls, Data.Win.ADODB, Vcl.ComCtrls;

type
  TXmlAlternative = class;

  TXMLDicoRequest = class;
  TXmlDico = class(TForm)
    ClientDataSet1: TClientDataSet;
    DataSource1: TDataSource;
    ClientDataSet1Vocable: TStringField;
    ClientDataSet1alt1: TStringField;
    ClientDataSet1alt2: TStringField;
    ClientDataSet1alt3: TStringField;
    ClientDataSet1alt4: TStringField;
    ClientDataSet1alt5: TStringField;
    ClientDataSet1alt6: TStringField;
    ClientDataSet1alt7: TStringField;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    Memo1: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    DBGrid1: TDBGrid;
    TabSheet2: TTabSheet;
    ClientDataSet2: TClientDataSet;
    DataSource2: TDataSource;
    DBGrid2: TDBGrid;
    ClientDataSet2OrthoVal: TStringField;
    TabSheet3: TTabSheet;
    ClientDataSet3: TClientDataSet;
    DataSource3: TDataSource;
    DBGrid3: TDBGrid;
    ClientDataSet3Vocable: TStringField;
    ClientDataSet3Alt1: TStringField;
    ClientDataSet3Alt2: TStringField;
    ClientDataSet3Alt3: TStringField;
    ClientDataSet3Alt4: TStringField;
    ClientDataSet3Alt5: TStringField;
    ClientDataSet3Alt6: TStringField;
    ClientDataSet3Alt7: TStringField;
    procedure ClientDataSet1AfterPost(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ClientDataSet2AfterPost(DataSet: TDataSet);
    procedure ClientDataSet3AfterPost(DataSet: TDataSet);
  private
    FRequest: TXMLDicoRequest;
  public

    property Request: TXMLDicoRequest read FRequest;
  end;

  TXMLDicoRequest = class
  private
    FOwner : TXmlDico;
    function GetDataSet: TClientDataSet;
  public
    function FindAltFrom(const AFilter: string):string;
    function FindAltOrtho(const AFilter: string):string;

    property DataSet: TClientDataSet read GetDataSet;

    constructor Create(const AOwner: TXmlDico);
  end;

  TXmlAlternative = class
  private
    ClientDataSet1: TClientDataSet;
    ClientDataSet2: TClientDataSet;
    ClientDataSet3: TClientDataSet;
  public
    function FindAltFrom(const AFilter: string; CapsLock: Boolean):string;
    function FindAltOrtho(const AFilter: string; CapsLock: Boolean):string;
    function FindAltPost(const AFilter: string; CapsLock: Boolean):string;

    constructor Create;
  end;

var
  XmlDico: TXmlDico;

implementation

{$R *.dfm}

procedure TXmlDico.Button1Click(Sender: TObject);
begin
with Memo1.Lines do Text := Request.FindAltOrtho( Edit1.Text );
end;

procedure TXmlDico.ClientDataSet1AfterPost(DataSet: TDataSet);
begin
  ClientDataSet1.SaveToFile('ShortDico.xml')
end;

procedure TXmlDico.ClientDataSet2AfterPost(DataSet: TDataSet);
begin
  ClientDataSet2.SaveToFile('OrthoLang.xml')
end;

procedure TXmlDico.ClientDataSet3AfterPost(DataSet: TDataSet);
begin
  with ClientDataSet2 do SaveToFile(FileName)
end;

procedure TXmlDico.FormCreate(Sender: TObject);
begin
  FRequest := TXMLDicoRequest.Create(Self);
  with ClientDataSet1 do begin
    IndexName := 'ClientDataSet1Index1';
    LoadFromFile('ShortDico.xml')
  end;
  with ClientDataSet2 do begin
    FileName := 'OrthoLang.xml';
    LoadFromFile(FileName);
    IndexFieldNames := Fields[0].FieldName
  end;
  with ClientDataSet3 do begin
    LoadFromFile(FileName);
    IndexFieldNames := Fields[0].FieldName
  end;
end;

procedure TXmlDico.FormDestroy(Sender: TObject);
begin
  FRequest.Free
end;

{ TXMLDicoRequest }

constructor TXMLDicoRequest.Create(const AOwner: TXmlDico);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TXMLDicoRequest.FindAltFrom(const AFilter: string): string;
var
  i : Integer;
begin
  with TStringList.Create do
  try
    with DataSet do begin
      Filtered := False;
      Filter   := Format('Vocable = %s', [ QuotedStr(AFilter) ]);
      Filtered := True;
      First;
      if RecordCount > 0 then for i := 1 to 7 do Add( Fields[i].AsString )
    end;
    Result := Trim( Text )
  finally
    Free
  end
end;

function TXMLDicoRequest.FindAltOrtho(const AFilter: string): string;
begin
  with TXmlAlternative.Create do
  try
    Result := FindAltOrtho(AFilter, False)
  finally
    Free
  end;
end;

function TXMLDicoRequest.GetDataSet: TClientDataSet;
begin
  Result := FOwner.ClientDataSet1
end;

{ TXmlAlternative }

constructor TXmlAlternative.Create;
begin
  inherited Create;
  ClientDataSet1 := TClientDataSet.Create(Application.MainForm);
  with ClientDataSet1 do begin
    FileName := 'ShortDico.xml';
    LoadFromFile( FileName );
    IndexFieldNames := Fields[0].FieldName;
    Open;
  end;
  ClientDataSet2 := TClientDataSet.Create(Application.MainForm);
  with ClientDataSet2 do begin
    FileName := 'OrthoLang.xml';
    LoadFromFile( FileName );
    IndexFieldNames := Fields[0].FieldName;
    Open;
  end;
  ClientDataSet3 := TClientDataSet.Create(Application.MainForm);
  with ClientDataSet3 do begin
    FileName := 'PostDico.xml';
    LoadFromFile( FileName );
    IndexFieldNames := Fields[0].FieldName;
    Open;
  end;
end;

function TXmlAlternative.FindAltFrom(const AFilter: string; CapsLock: Boolean): string;
var
  i        : Integer;
  LFilter  : string;

  function CanContinue:Boolean; begin
    Result := Trim(AFilter) <> EmptyStr;
    if Result then LFilter  := AnsiLowerCase(AFilter)
  end;

begin
  Result := EmptyStr;
  if not CanContinue then Exit;

  with TStringList.Create do
  try
    with ClientDataSet1 do begin
      Filtered := False;
      Filter   := Format('Vocable = %s', [ QuotedStr(LFilter) ]);
      Filtered := True;
      First;
      if RecordCount > 0 then for i := 1 to 7 do
        if not CapsLock then Add( AnsiUpperCase( Fields[i].AsString ) )
          else Add( Fields[i].AsString )
    end;
    Result := Trim( Text )
  finally
    Free
  end
end;

function TXmlAlternative.FindAltOrtho(const AFilter: string; CapsLock: Boolean): string;
var
  i        : Integer;
  LFilter  : string;
  Max      : Integer;

  function CanContinue:Boolean; begin
    Result := Trim(AFilter) <> EmptyStr;
    if Result then LFilter  := AnsiLowerCase(AFilter)
  end;

begin
  Result := EmptyStr;
  if not CanContinue then Exit;

  with TStringList.Create do
  try
    with ClientDataSet2 do begin
      Filtered := False;
      Filter   := Format('OrthoVal like %s', [ QuotedStr( Format('%s%s', [LFilter, '%'])) ]);
      Filtered := True;
      First;
      if RecordCount < 7 then Max := RecordCount else Max := 7;
      if Max > 0 then for i := 0 to Max - 1 do begin
        if not CapsLock then Add( AnsiUpperCase( Fields[0].AsString ) )
          else Add( Fields[0].AsString );
        Next
      end
    end;
    Result := Trim( Text )
  finally
    Free
  end
end;

function TXmlAlternative.FindAltPost(const AFilter: string;
  CapsLock: Boolean): string;
var
  i        : Integer;
  LFilter  : string;

  function CanContinue:Boolean; begin
    Result := Trim(AFilter) <> EmptyStr;
    if Result then LFilter  := AnsiLowerCase(AFilter)
  end;

begin
  Result := EmptyStr;
  if not CanContinue then Exit;

  with TStringList.Create do
  try
    with ClientDataSet3 do begin
      Filtered := False;
      Filter   := Format('Vocable = %s', [ QuotedStr(LFilter) ]);
      Filtered := True;
      First;
      if RecordCount > 0 then for i := 1 to 7 do
        if not CapsLock then Add( AnsiUpperCase( Fields[i].AsString ) )
          else Add( Fields[i].AsString )
    end;
    Result := Trim( Text )
  finally
    Free
  end
end;

{FindAltFrom}

end.
