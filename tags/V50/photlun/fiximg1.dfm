object Form1: TForm1
  Left = 199
  Top = 186
  AutoSize = True
  BorderStyle = bsNone
  Caption = 'Fix old VMA image format'
  ClientHeight = 60
  ClientWidth = 416
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
    Left = 0
    Top = 14
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
    Left = 0
    Top = 38
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
    Left = 223
    Top = 0
    Width = 193
    Height = 60
    TabOrder = 0
    Visible = False
    object Edit1: TEdit
      Left = 8
      Top = 4
      Width = 153
      Height = 21
      TabOrder = 0
    end
    object SelDir: TButton
      Left = 159
      Top = 4
      Width = 25
      Height = 21
      Caption = '...'
      TabOrder = 1
      OnClick = SelDirClick
    end
    object btnok: TButton
      Left = 8
      Top = 31
      Width = 75
      Height = 21
      Caption = 'OK'
      TabOrder = 2
      OnClick = btnokClick
    end
    object btncancel: TButton
      Left = 112
      Top = 29
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = btncancelClick
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 10
    OnTimer = Timer1Timer
    Left = 200
  end
  object FolderDialog1: TFolderDialog
    Top = 32
    Left = 200
    Title = 'Browse for Folder'
  end
  object XPManifest1: TXPManifest
    Left = 168
    Top = 32
  end
end
