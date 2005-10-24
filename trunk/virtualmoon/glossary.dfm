object Gloss: TGloss
  Left = 203
  Top = 130
  Width = 515
  Height = 286
  Caption = 'Glossary'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 280
    Top = 26
    Width = 153
    Height = 21
    TabOrder = 0
    OnKeyDown = Edit1KeyDown
  end
  object Button1: TButton
    Left = 440
    Top = 24
    Width = 33
    Height = 25
    Caption = '>'
    TabOrder = 1
    OnClick = Button1Click
  end
  object TreeView1: TTreeView
    Left = 8
    Top = 64
    Width = 209
    Height = 145
    AutoExpand = True
    HideSelection = False
    Indent = 15
    ReadOnly = True
    TabOrder = 2
    OnChange = TreeView1Change
    OnChanging = TreeView1Changing
  end
  object Button2: TButton
    Left = 192
    Top = 220
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Gloss1: ThtmlLite
    Left = 232
    Top = 64
    Width = 265
    Height = 145
    OnHotSpotClick = Gloss1HotSpotClick
    ViewImages = False
    TabOrder = 4
    ShowHint = True
    DefBackground = clWindow
    BorderStyle = htFocused
    HistoryMaxCount = 0
    DefFontName = 'MS Sans Serif'
    DefPreFontName = 'Courier New'
    DefFontSize = 8
    DefFontColor = clWindowText
    DefHotSpotColor = clWindowText
    DefVisitedLinkColor = clWindowText
    NoSelect = False
    ScrollBars = ssVertical
    CharSet = DEFAULT_CHARSET
    MarginHeight = 2
    MarginWidth = 2
    htOptions = [htOverLinksActive]
  end
  object Panel1: TPanel
    Left = 24
    Top = 8
    Width = 169
    Height = 49
    TabOrder = 5
    object Label1: TLabel
      Left = 8
      Top = 8
      Width = 9
      Height = 13
      Caption = 'A'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label2: TLabel
      Left = 19
      Top = 8
      Width = 9
      Height = 13
      Caption = 'B'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label3: TLabel
      Left = 30
      Top = 8
      Width = 9
      Height = 13
      Caption = 'C'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label5: TLabel
      Left = 53
      Top = 8
      Width = 9
      Height = 13
      Caption = 'E'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label6: TLabel
      Left = 64
      Top = 8
      Width = 8
      Height = 13
      Caption = 'F'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label4: TLabel
      Left = 42
      Top = 8
      Width = 10
      Height = 13
      Caption = 'D'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label7: TLabel
      Left = 76
      Top = 8
      Width = 10
      Height = 13
      Caption = 'G'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label8: TLabel
      Left = 87
      Top = 8
      Width = 10
      Height = 13
      Caption = 'H'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label9: TLabel
      Left = 98
      Top = 8
      Width = 5
      Height = 13
      Caption = 'I'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label10: TLabel
      Left = 110
      Top = 8
      Width = 7
      Height = 13
      Caption = 'J'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label11: TLabel
      Left = 121
      Top = 8
      Width = 9
      Height = 13
      Caption = 'K'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label12: TLabel
      Left = 132
      Top = 8
      Width = 8
      Height = 13
      Caption = 'L'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label13: TLabel
      Left = 144
      Top = 8
      Width = 11
      Height = 13
      Caption = 'M'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label14: TLabel
      Left = 8
      Top = 24
      Width = 10
      Height = 13
      Caption = 'N'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label15: TLabel
      Left = 19
      Top = 24
      Width = 10
      Height = 13
      Caption = 'O'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label16: TLabel
      Left = 30
      Top = 24
      Width = 9
      Height = 13
      Caption = 'P'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label17: TLabel
      Left = 42
      Top = 24
      Width = 10
      Height = 13
      Caption = 'Q'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label18: TLabel
      Left = 53
      Top = 24
      Width = 10
      Height = 13
      Caption = 'R'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label19: TLabel
      Left = 64
      Top = 24
      Width = 9
      Height = 13
      Caption = 'S'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label20: TLabel
      Left = 76
      Top = 24
      Width = 9
      Height = 13
      Caption = 'T'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label21: TLabel
      Left = 87
      Top = 24
      Width = 10
      Height = 13
      Caption = 'U'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label22: TLabel
      Left = 98
      Top = 24
      Width = 9
      Height = 13
      Caption = 'V'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label23: TLabel
      Left = 110
      Top = 24
      Width = 13
      Height = 13
      Caption = 'W'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label24: TLabel
      Left = 121
      Top = 24
      Width = 9
      Height = 13
      Caption = 'X'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label25: TLabel
      Left = 132
      Top = 24
      Width = 9
      Height = 13
      Caption = 'Y'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
    object Label26: TLabel
      Left = 144
      Top = 24
      Width = 9
      Height = 13
      Caption = 'Z'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = Label1Click
    end
  end
  object Button3: TButton
    Left = 232
    Top = 24
    Width = 33
    Height = 25
    Caption = '<'
    TabOrder = 6
    OnClick = Button3Click
  end
end
