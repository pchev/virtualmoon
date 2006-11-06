object Selection: TSelection
  Left = 503
  Top = 130
  Width = 582
  Height = 337
  Caption = 'Selection'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 16
    Top = 0
    Width = 545
    Height = 257
    ActivePage = TabSheet1
    TabIndex = 0
    TabOrder = 0
    OnChange = PageControl1Change
    object TabSheet1: TTabSheet
      Caption = 'Predefined Selections'
      object CheckListBox1: TCheckListBox
        Left = 8
        Top = 8
        Width = 497
        Height = 173
        OnClickCheck = CheckListBox1ClickCheck
        Columns = 2
        IntegralHeight = True
        ItemHeight = 13
        TabOrder = 0
      end
      object ButtonAll: TButton
        Left = 168
        Top = 192
        Width = 81
        Height = 25
        Caption = 'All'
        TabOrder = 1
        OnClick = ButtonAllClick
      end
      object ButtonNone: TButton
        Left = 280
        Top = 192
        Width = 81
        Height = 25
        Caption = 'None'
        TabOrder = 2
        OnClick = ButtonNoneClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Column Value'
      ImageIndex = 2
      object ViewSel: TLabel
        Left = 40
        Top = 204
        Width = 3
        Height = 13
      end
      object RadioGroup2: TRadioGroup
        Left = 40
        Top = 32
        Width = 425
        Height = 161
        ItemIndex = 0
        Items.Strings = (
          '='
          '>='
          '<='
          'Between')
        TabOrder = 0
        OnClick = RadioGroup2Click
      end
      object fieldlist2: TComboBox
        Left = 40
        Top = 8
        Width = 129
        Height = 21
        ItemHeight = 13
        TabOrder = 1
        TabStop = False
        OnChange = RadioGroup2Click
      end
      object colgt: TEdit
        Left = 136
        Top = 90
        Width = 121
        Height = 21
        Color = clBtnFace
        TabOrder = 2
        OnChange = RadioGroup2Click
      end
      object collt: TEdit
        Left = 136
        Top = 124
        Width = 121
        Height = 21
        Color = clBtnFace
        Enabled = False
        TabOrder = 3
        OnChange = RadioGroup2Click
      end
      object colbetween1: TEdit
        Left = 136
        Top = 159
        Width = 121
        Height = 21
        Color = clBtnFace
        Enabled = False
        TabOrder = 4
        OnChange = RadioGroup2Click
      end
      object StaticText1: TStaticText
        Left = 272
        Top = 161
        Width = 22
        Height = 17
        Caption = 'and'
        TabOrder = 5
      end
      object colbetween2: TEdit
        Left = 304
        Top = 159
        Width = 121
        Height = 21
        Color = clBtnFace
        Enabled = False
        TabOrder = 6
        OnChange = RadioGroup2Click
      end
      object coleq: TEdit
        Left = 136
        Top = 54
        Width = 121
        Height = 21
        TabOrder = 7
        OnChange = RadioGroup2Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'SQL Request'
      ImageIndex = 1
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 240
        Height = 13
        Caption = 'Enter selection and order criteria using SQL syntax.'
      end
      object Label2: TLabel
        Left = 8
        Top = 32
        Width = 481
        Height = 13
        Caption = 
          'i.e: TYPE like "Cra%" and abs(LONGIN)<20 and LENGTHKM between 10' +
          ' and 15 order by LATIN asc'
      end
      object Button1: TButton
        Tag = 1
        Left = 8
        Top = 120
        Width = 30
        Height = 25
        Caption = '='
        TabOrder = 1
        TabStop = False
        OnClick = ButtonClick
      end
      object Button2: TButton
        Tag = 1
        Left = 44
        Top = 120
        Width = 30
        Height = 25
        Caption = '>'
        TabOrder = 2
        TabStop = False
        OnClick = ButtonClick
      end
      object Button3: TButton
        Tag = 1
        Left = 80
        Top = 120
        Width = 30
        Height = 25
        Caption = '<'
        TabOrder = 5
        TabStop = False
        OnClick = ButtonClick
      end
      object Button4: TButton
        Tag = 1
        Left = 116
        Top = 120
        Width = 30
        Height = 25
        Caption = '<>'
        TabOrder = 6
        TabStop = False
        OnClick = ButtonClick
      end
      object Button5: TButton
        Tag = 1
        Left = 152
        Top = 120
        Width = 50
        Height = 25
        Caption = 'LIKE'
        TabOrder = 7
        TabStop = False
        OnClick = ButtonClick
      end
      object Button6: TButton
        Tag = 1
        Left = 208
        Top = 120
        Width = 81
        Height = 25
        Caption = 'BETWEEN'
        TabOrder = 8
        TabStop = False
        OnClick = ButtonClick
      end
      object Button7: TButton
        Tag = 1
        Left = 8
        Top = 152
        Width = 50
        Height = 25
        Caption = 'AND'
        TabOrder = 9
        TabStop = False
        OnClick = ButtonClick
      end
      object Button8: TButton
        Tag = 1
        Left = 64
        Top = 152
        Width = 50
        Height = 25
        Caption = 'OR'
        TabOrder = 10
        TabStop = False
        OnClick = ButtonClick
      end
      object Button9: TButton
        Left = 8
        Top = 200
        Width = 69
        Height = 25
        Caption = 'Clear'
        TabOrder = 4
        OnClick = Button9Click
      end
      object Button12: TButton
        Left = 352
        Top = 120
        Width = 30
        Height = 25
        Caption = '"'
        TabOrder = 11
        TabStop = False
        OnClick = ButtonClick
      end
      object Button13: TButton
        Left = 88
        Top = 200
        Width = 69
        Height = 25
        Caption = 'Last'
        TabOrder = 3
        OnClick = Button13Click
      end
      object sel: TMemo
        Left = 8
        Top = 48
        Width = 441
        Height = 65
        Lines.Strings = (
          '')
        TabOrder = 0
      end
      object Button14: TButton
        Left = 392
        Top = 120
        Width = 30
        Height = 25
        Caption = '%'
        TabOrder = 12
        TabStop = False
        OnClick = ButtonClick
      end
      object Button15: TButton
        Tag = 1
        Left = 296
        Top = 120
        Width = 50
        Height = 25
        Caption = 'NOT'
        TabOrder = 13
        TabStop = False
        OnClick = ButtonClick
      end
      object Button16: TButton
        Tag = 1
        Left = 120
        Top = 152
        Width = 81
        Height = 25
        Caption = 'ORDER BY'
        TabOrder = 14
        TabStop = False
        OnClick = ButtonClick
      end
      object Button17: TButton
        Tag = 1
        Left = 208
        Top = 152
        Width = 50
        Height = 25
        Caption = 'ASC'
        TabOrder = 15
        TabStop = False
        OnClick = ButtonClick
      end
      object Button18: TButton
        Tag = 1
        Left = 264
        Top = 152
        Width = 50
        Height = 25
        Caption = 'DESC'
        TabOrder = 16
        TabStop = False
        OnClick = ButtonClick
      end
      object fieldlist: TComboBox
        Left = 320
        Top = 152
        Width = 129
        Height = 21
        ItemHeight = 13
        TabOrder = 17
        TabStop = False
        OnSelect = fieldlistSelect
      end
    end
  end
  object Button11: TButton
    Left = 386
    Top = 264
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = Button11Click
  end
  object Button10: TButton
    Left = 482
    Top = 264
    Width = 75
    Height = 25
    Caption = 'Ok'
    TabOrder = 1
    OnClick = Button10Click
  end
  object ExpertMode: TCheckBox
    Left = 44
    Top = 264
    Width = 141
    Height = 17
    Caption = 'Expert mode'
    TabOrder = 3
    OnClick = ExpertModeClick
  end
  object Button19: TButton
    Left = 240
    Top = 264
    Width = 123
    Height = 25
    Caption = 'Select Database'
    TabOrder = 4
    OnClick = Button19Click
  end
end
