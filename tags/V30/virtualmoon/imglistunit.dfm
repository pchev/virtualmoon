object Imglist: TImglist
  Left = 346
  Top = 127
  Width = 235
  Height = 299
  Caption = 'Imglist'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object ImgLst: TListBox
    Left = 0
    Top = 0
    Width = 227
    Height = 272
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnMouseUp = ImgLstMouseUp
  end
end
