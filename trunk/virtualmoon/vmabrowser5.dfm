object SelectDB: TSelectDB
  Left = 671
  Top = 241
  Width = 418
  Height = 301
  Caption = 'SelectDB'
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
  object CheckListBox1: TCheckListBox
    Left = 16
    Top = 16
    Width = 377
    Height = 193
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 312
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object Button2: TButton
    Left = 224
    Top = 224
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object Button3: TButton
    Left = 24
    Top = 224
    Width = 75
    Height = 25
    Caption = 'All'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 120
    Top = 224
    Width = 75
    Height = 25
    Caption = 'None'
    TabOrder = 4
    OnClick = Button4Click
  end
end
