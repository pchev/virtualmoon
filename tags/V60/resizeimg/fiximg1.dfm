object Form1: TForm1
  Left = 344
  Top = 279
  Width = 346
  Height = 343
  Caption = 'Resize Images'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 6
    Width = 32
    Height = 13
    Caption = 'Label1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 16
    Top = 150
    Width = 32
    Height = 13
    Caption = 'Label1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 15
    Top = 24
    Width = 300
    Height = 121
    TabOrder = 0
    Visible = False
    object Edit1: TEdit
      Left = 8
      Top = 4
      Width = 249
      Height = 21
      TabOrder = 0
    end
    object SelDir: TButton
      Left = 263
      Top = 4
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = SelDirClick
    end
    object btnok: TButton
      Left = 8
      Top = 87
      Width = 75
      Height = 21
      Caption = 'OK'
      TabOrder = 2
      OnClick = btnokClick
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 32
      Width = 177
      Height = 49
      Caption = 'Max size'
      Columns = 3
      ItemIndex = 0
      Items.Strings = (
        '640'
        '800'
        '1024')
      TabOrder = 3
    end
  end
  object Memo1: TMemo
    Left = 16
    Top = 176
    Width = 297
    Height = 113
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer1Timer
    Left = 272
    Top = 56
  end
  object FolderDialog1: TFolderDialog
    Top = 96
    Left = 272
    Title = 'Browse for Folder'
  end
  object XPManifest1: TXPManifest
    Left = 232
    Top = 56
  end
end
