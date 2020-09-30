object XmlDico: TXmlDico
  Left = 0
  Top = 0
  Caption = 'XmlDico'
  ClientHeight = 520
  ClientWidth = 1664
  Color = 2302755
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clGray
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 320
    Width = 1664
    Height = 200
    Align = alBottom
    TabOrder = 0
    object Edit1: TEdit
      Left = 24
      Top = 32
      Width = 153
      Height = 25
      Color = 2302755
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object Button1: TButton
      Left = 183
      Top = 32
      Width = 75
      Height = 25
      Caption = 'Finder'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Memo1: TMemo
      Left = 280
      Top = 1
      Width = 1383
      Height = 198
      Align = alRight
      Color = 2302755
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1664
    Height = 320
    ActivePage = TabSheet2
    Align = alClient
    DoubleBuffered = True
    ParentDoubleBuffered = False
    TabOrder = 1
    object TabSheet1: TTabSheet
      Caption = 'ShortDico'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 1656
        Height = 292
        Align = alClient
        Color = 2302755
        DataSource = DataSource1
        FixedColor = 2302755
        GradientEndColor = 2302755
        GradientStartColor = 2302755
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clGray
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Ortholang'
      ImageIndex = 1
      object DBGrid2: TDBGrid
        Left = 0
        Top = 0
        Width = 345
        Height = 292
        Align = alLeft
        DataSource = DataSource2
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clGray
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'PostDico'
      ImageIndex = 2
      object DBGrid3: TDBGrid
        Left = 0
        Top = 0
        Width = 1656
        Height = 292
        Align = alClient
        DataSource = DataSource3
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clGray
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
  end
  object ClientDataSet1: TClientDataSet
    PersistDataPacket.Data = {
      E50000009619E0BD010000001800000008000000000003000000E50007566F63
      61626C650100490000000100055749445448020002001A0004616C7431010049
      0000000100055749445448020002001A0004616C743201004900000001000557
      49445448020002001A0004616C74330100490000000100055749445448020002
      001A0004616C74340100490000000100055749445448020002001A0004616C74
      350100490000000100055749445448020002001A0004616C7436010049000000
      0100055749445448020002001A0004616C743701004900000001000557494454
      48020002001A000000}
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'Vocable'
        DataType = ftString
        Size = 26
      end
      item
        Name = 'alt1'
        DataType = ftString
        Size = 26
      end
      item
        Name = 'alt2'
        DataType = ftString
        Size = 26
      end
      item
        Name = 'alt3'
        DataType = ftString
        Size = 26
      end
      item
        Name = 'alt4'
        DataType = ftString
        Size = 26
      end
      item
        Name = 'alt5'
        DataType = ftString
        Size = 26
      end
      item
        Name = 'alt6'
        DataType = ftString
        Size = 26
      end
      item
        Name = 'alt7'
        DataType = ftString
        Size = 26
      end>
    IndexDefs = <
      item
        Name = 'ClientDataSet1Index1'
        Fields = 'Vocable'
      end>
    Params = <>
    StoreDefs = True
    AfterPost = ClientDataSet1AfterPost
    Left = 40
    Top = 64
    object ClientDataSet1Vocable: TStringField
      DisplayWidth = 11
      FieldName = 'Vocable'
      Size = 26
    end
    object ClientDataSet1alt1: TStringField
      DisplayWidth = 20
      FieldName = 'alt1'
      Size = 26
    end
    object ClientDataSet1alt2: TStringField
      DisplayWidth = 20
      FieldName = 'alt2'
      Size = 26
    end
    object ClientDataSet1alt3: TStringField
      DisplayWidth = 21
      FieldName = 'alt3'
      Size = 26
    end
    object ClientDataSet1alt4: TStringField
      DisplayWidth = 21
      FieldName = 'alt4'
      Size = 26
    end
    object ClientDataSet1alt5: TStringField
      DisplayWidth = 21
      FieldName = 'alt5'
      Size = 26
    end
    object ClientDataSet1alt6: TStringField
      DisplayWidth = 21
      FieldName = 'alt6'
      Size = 26
    end
    object ClientDataSet1alt7: TStringField
      DisplayWidth = 20
      FieldName = 'alt7'
      Size = 26
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 40
    Top = 112
  end
  object ClientDataSet2: TClientDataSet
    PersistDataPacket.Data = {
      370000009619E0BD0100000018000000010000000000030000003700084F7274
      686F56616C01004900000001000557494454480200020032000000}
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'OrthoVal'
        DataType = ftString
        Size = 50
      end>
    IndexDefs = <
      item
        Name = 'DEFAULT_ORDER'
      end
      item
        Name = 'CHANGEINDEX'
        Fields = 'Ortholang'
        Options = [ixDescending]
      end>
    IndexName = 'CHANGEINDEX'
    Params = <>
    StoreDefs = True
    AfterPost = ClientDataSet2AfterPost
    Left = 124
    Top = 64
    object ClientDataSet2OrthoVal: TStringField
      FieldName = 'OrthoVal'
      Size = 50
    end
  end
  object DataSource2: TDataSource
    DataSet = ClientDataSet2
    Left = 124
    Top = 112
  end
  object ClientDataSet3: TClientDataSet
    PersistDataPacket.Data = {
      E50000009619E0BD010000001800000008000000000003000000E50007566F63
      61626C650100490000000100055749445448020002005A0004416C7431010049
      000000010005574944544802000200280004416C743201004900000001000557
      4944544802000200280004416C74330100490000000100055749445448020002
      00280004416C7434010049000000010005574944544802000200280004416C74
      35010049000000010005574944544802000200280004416C7436010049000000
      010005574944544802000200280004416C743701004900000001000557494454
      480200020028000000}
    Active = True
    Aggregates = <>
    FileName = 'PostDico.xml'
    Params = <>
    AfterPost = ClientDataSet3AfterPost
    Left = 212
    Top = 64
    object ClientDataSet3Vocable: TStringField
      DisplayWidth = 33
      FieldName = 'Vocable'
      Size = 90
    end
    object ClientDataSet3Alt1: TStringField
      DisplayWidth = 30
      FieldName = 'Alt1'
      Size = 40
    end
    object ClientDataSet3Alt2: TStringField
      DisplayWidth = 29
      FieldName = 'Alt2'
      Size = 40
    end
    object ClientDataSet3Alt3: TStringField
      DisplayWidth = 28
      FieldName = 'Alt3'
      Size = 40
    end
    object ClientDataSet3Alt4: TStringField
      DisplayWidth = 29
      FieldName = 'Alt4'
      Size = 40
    end
    object ClientDataSet3Alt5: TStringField
      DisplayWidth = 28
      FieldName = 'Alt5'
      Size = 40
    end
    object ClientDataSet3Alt6: TStringField
      DisplayWidth = 25
      FieldName = 'Alt6'
      Size = 40
    end
    object ClientDataSet3Alt7: TStringField
      DisplayWidth = 29
      FieldName = 'Alt7'
      Size = 40
    end
  end
  object DataSource3: TDataSource
    DataSet = ClientDataSet3
    Left = 212
    Top = 112
  end
end
