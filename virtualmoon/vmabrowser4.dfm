object LoadCSV: TLoadCSV
  Left = 221
  Top = 110
  Width = 696
  Height = 540
  Caption = 'LoadCSV'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 688
    Height = 506
    ActivePage = TabSheet1
    Align = alClient
    TabIndex = 0
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Select File'
      object Bevel3: TBevel
        Left = 16
        Top = 288
        Width = 257
        Height = 97
        Shape = bsFrame
      end
      object Bevel1: TBevel
        Left = 16
        Top = 24
        Width = 257
        Height = 129
        Shape = bsFrame
      end
      object Bevel2: TBevel
        Left = 16
        Top = 168
        Width = 257
        Height = 97
        Shape = bsFrame
      end
      object Label1: TLabel
        Left = 32
        Top = 52
        Width = 72
        Height = 13
        Caption = 'Field separator:'
      end
      object Label2: TLabel
        Left = 32
        Top = 100
        Width = 71
        Height = 13
        Caption = 'Text separator:'
      end
      object Label4: TLabel
        Left = 32
        Top = 176
        Width = 86
        Height = 13
        Caption = 'CSV file to import :'
      end
      object Label5: TLabel
        Left = 32
        Top = 236
        Width = 3
        Height = 13
      end
      object Label7: TLabel
        Left = 32
        Top = 304
        Width = 225
        Height = 25
        AutoSize = False
        Caption = 'Reload a previously saved setting'
        WordWrap = True
      end
      object Edit1: TEdit
        Left = 32
        Top = 200
        Width = 193
        Height = 21
        TabOrder = 2
        OnChange = Edit1Change
      end
      object FileSelect: TButton
        Left = 224
        Top = 200
        Width = 21
        Height = 21
        Caption = '...'
        TabOrder = 4
        OnClick = FileSelectClick
      end
      object SepBox: TComboBox
        Left = 176
        Top = 48
        Width = 65
        Height = 21
        ItemHeight = 13
        TabOrder = 0
        Text = ';'
        OnChange = Edit1Change
        Items.Strings = (
          ';'
          'TAB')
      end
      object QuoteBox: TComboBox
        Left = 176
        Top = 96
        Width = 65
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        OnChange = Edit1Change
        Items.Strings = (
          ''
          '"')
      end
      object memo2: TMemo
        Left = 280
        Top = 0
        Width = 401
        Height = 481
        TabStop = False
        Color = clBtnFace
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 5
      end
      object Button1: TButton
        Left = 40
        Top = 342
        Width = 121
        Height = 25
        Caption = 'Load Setting'
        TabOrder = 3
        OnClick = Button1Click
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Select Data Field'
      ImageIndex = 1
      object Bevel6: TBevel
        Left = 352
        Top = 0
        Width = 17
        Height = 481
        Shape = bsLeftLine
      end
      object Label3: TLabel
        Left = 384
        Top = 8
        Width = 100
        Height = 13
        Caption = 'Database Fields Map'
      end
      object Label9: TLabel
        Left = 8
        Top = 8
        Width = 43
        Height = 13
        Caption = 'Input File'
      end
      object samplenext: TButton
        Left = 184
        Top = 438
        Width = 35
        Height = 25
        Caption = '>'
        TabOrder = 5
        TabStop = False
        OnClick = samplenextClick
      end
      object sampleprev: TButton
        Left = 136
        Top = 438
        Width = 35
        Height = 25
        Caption = '<'
        TabOrder = 6
        TabStop = False
        OnClick = sampleprevClick
      end
      object StringGrid1: TStringGrid
        Left = 4
        Top = 64
        Width = 320
        Height = 365
        ColCount = 2
        DefaultColWidth = 157
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected]
        ScrollBars = ssVertical
        TabOrder = 0
      end
      object CheckListBox1: TCheckListBox
        Left = 384
        Top = 60
        Width = 289
        Height = 369
        Columns = 2
        ItemHeight = 13
        TabOrder = 1
        OnClick = CheckListBox1Click
      end
      object ConstantText: TEdit
        Left = 384
        Top = 440
        Width = 169
        Height = 21
        TabOrder = 3
      end
      object AssignConstant: TButton
        Left = 560
        Top = 438
        Width = 113
        Height = 25
        Caption = 'Constant Value'
        TabOrder = 4
        OnClick = AssignConstantClick
      end
      object Msg: TEdit
        Left = 384
        Top = 30
        Width = 289
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 7
      end
      object AssignField: TButton
        Left = 328
        Top = 208
        Width = 49
        Height = 25
        Caption = '<---->'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = AssignFieldClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Load Database'
      ImageIndex = 2
      object Bevel4: TBevel
        Left = 16
        Top = 8
        Width = 233
        Height = 105
        Shape = bsFrame
      end
      object Label6: TLabel
        Left = 32
        Top = 24
        Width = 209
        Height = 33
        AutoSize = False
        Caption = 'You are now ready to load the database'
        WordWrap = True
      end
      object Bevel5: TBevel
        Left = 272
        Top = 8
        Width = 329
        Height = 105
        Shape = bsFrame
      end
      object Label8: TLabel
        Left = 296
        Top = 24
        Width = 281
        Height = 33
        AutoSize = False
        Caption = 'Save all the definition for later retrival'
        WordWrap = True
      end
      object Button3: TButton
        Left = 32
        Top = 72
        Width = 129
        Height = 25
        Caption = 'Load Database'
        TabOrder = 0
        OnClick = Button3Click
      end
      object Memo1: TMemo
        Left = 16
        Top = 120
        Width = 585
        Height = 329
        TabStop = False
        Lines.Strings = (
          '')
        ReadOnly = True
        TabOrder = 2
      end
      object Button2: TButton
        Left = 296
        Top = 72
        Width = 129
        Height = 25
        Caption = 'Save Setting'
        TabOrder = 1
        OnClick = Button2Click
      end
    end
  end
  object dbu: TLiteDB
    Active = False
    DllLoaded = False
    UniCode = False
    CallBackOnly = False
    FetchMemoryLimit = 33554432
    ResultSet = 'default'
    ThreadSafe = True
    SQLiteVersion = svAuto
    PragmasBoolean = []
    PragmaCacheSize = 0
    PragmaDefaultCacheSize = 0
    PragmaPagesize = 0
    PragmaSynchronous = spOff
    PragmaTempStore = tpDefault
    Left = 640
    Top = 32
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.csv'
    Filter = 'CSV Files|*.csv'
    InitialDir = 'My Databases'
    Left = 640
    Top = 72
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '.vmadb'
    Filter = 'VMA DB Setting|*.vmadb'
    InitialDir = 'My Databases'
    Left = 640
    Top = 112
  end
  object OpenDialog2: TOpenDialog
    DefaultExt = '.vmadb'
    Filter = 'VMA DB Setting|*.vmadb'
    InitialDir = 'My Databases'
    Left = 640
    Top = 144
  end
end
