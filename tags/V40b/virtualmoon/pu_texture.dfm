object Form1: TForm1
  Left = 214
  Top = 114
  BorderStyle = bsNone
  Caption = 'Form1'
  ClientHeight = 65
  ClientWidth = 281
  Color = clRed
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 265
    Height = 49
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 8
      Width = 257
      Height = 35
      AutoSize = False
      Caption = 'Label1'
      WordWrap = True
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 200
    Top = 16
  end
end
