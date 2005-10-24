object BigImaForm: TBigImaForm
  Left = 179
  Top = 0
  Width = 615
  Height = 407
  HorzScrollBar.Smooth = True
  HorzScrollBar.Size = 12
  HorzScrollBar.Tracking = True
  VertScrollBar.Smooth = True
  VertScrollBar.Size = 12
  VertScrollBar.Tracking = True
  Caption = 'Taille r'#233'elle'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  OnMouseWheel = FormMouseWheel
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 607
    Height = 25
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 61
    Customizable = True
    ParentShowHint = False
    ShowCaptions = True
    ShowHint = True
    TabOrder = 0
    Visible = False
    Wrapable = False
    OnResize = ToolBar1Resize
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Caption = 'Close'
      ImageIndex = 0
      OnClick = ToolButton1Click
    end
    object ToolButton2: TToolButton
      Left = 61
      Top = 2
      Caption = 'Zoom +'
      ImageIndex = 1
      OnClick = ToolButton2Click
    end
    object ToolButton3: TToolButton
      Left = 122
      Top = 2
      Caption = '   Zoom -    '
      ImageIndex = 2
      OnClick = ToolButton3Click
    end
    object Label1: TLabel
      Left = 183
      Top = 2
      Width = 27
      Height = 21
      AutoSize = False
      Caption = 'Label1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 25
    Width = 607
    Height = 348
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    Align = alClient
    BevelEdges = []
    BevelInner = bvNone
    BevelOuter = bvNone
    BorderStyle = bsNone
    TabOrder = 1
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 601
      Height = 348
      AutoSize = True
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 200
    OnTimer = Timer1Timer
    Left = 40
    Top = 48
  end
end
