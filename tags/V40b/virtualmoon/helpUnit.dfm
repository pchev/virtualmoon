object helpForm: ThelpForm
  Left = 256
  Top = 282
  Width = 612
  Height = 425
  Caption = 'helpForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  DesignSize = (
    587
    391)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 27
    Width = 587
    Height = 368
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object html1: ThtmlLite
      Left = 1
      Top = 1
      Width = 585
      Height = 366
      Cursor = 2
      OnHotSpotCovered = html1HotSpotCovered
      OnHotSpotClick = html1HotSpotClick
      TabOrder = 0
      Align = alClient
      DefBackground = clWindow
      BorderStyle = htSingle
      HistoryMaxCount = 20
      DefFontName = 'Times New Roman'
      DefPreFontName = 'Courier New'
      ImageCacheCount = 10
      NoSelect = False
      CharSet = DEFAULT_CHARSET
      htOptions = []
    end
  end
  object BitBtn1: TBitBtn
    Left = 0
    Top = 2
    Width = 33
    Height = 25
    TabOrder = 1
    OnClick = BitBtn1Click
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000120B0000120B00001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADAD0DADA
      DADAADADAD00ADADADADDADAD0D0DADADADAADAD0DA0ADADADADDAD0DAD00000
      000AAD0DADADADADAD0DD0DADADADADADA0A0DADADADADADAD0DD0DADADADADA
      DA0AAD0DADADADADAD0DDAD0DAD00000000AADAD0DA0ADADADADDADAD0D0DADA
      DADAADADAD00ADADADADDADADAD0DADADADAADADADADADADADAD}
  end
  object BitBtn2: TBitBtn
    Left = 40
    Top = 2
    Width = 33
    Height = 25
    TabOrder = 2
    OnClick = BitBtn2Click
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000130B0000130B00001000000000000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
      DADAADADADAD0DADADADDADADADA00DADADAADADADAD0D0DADADDADADADA0AD0
      DADAA00000000DAD0DADD0DADADADADAD0DAA0ADADADADADAD0DD0DADADADADA
      DAD0A0ADADADADADAD0DD0DADADADADAD0DAA00000000DAD0DADDADADADA0AD0
      DADAADADADAD0D0DADADDADADADA00DADADAADADADAD0DADADAD}
  end
  object Edit1: TEdit
    Left = 336
    Top = 4
    Width = 265
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 88
    Top = 4
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object BitBtn3: TBitBtn
    Left = 216
    Top = 2
    Width = 33
    Height = 25
    TabOrder = 5
    OnClick = BitBtn3Click
    Glyph.Data = {
      F6000000424DF600000000000000760000002800000010000000100000000100
      04000000000080000000120B0000120B00001000000010000000000000000000
      8000008000000080800080000000800080008080000080808000C0C0C0000000
      FF00C0C0C00000FFFF00FF000000C0C0C000FFFF0000FFFFFF00DADADADADADA
      DADAADADADADADADADAD00000ADADA00000A0F000DADAD0F000D0F000ADADA0F
      000A0000000D0000000D00F000000F00000A00F000A00F00000D00F000D00F00
      000AA0000000000000ADDA0F000A0F000ADAAD00000D00000DADDAD000DAD000
      DADAADA0F0ADA0F0ADADDAD000DAD000DADAADADADADADADADAD}
  end
  object BitBtn4: TBitBtn
    Left = 256
    Top = 2
    Width = 33
    Height = 25
    TabOrder = 6
    OnClick = BitBtn4Click
    Glyph.Data = {
      7E010000424D7E01000000000000760000002800000016000000160000000100
      04000000000008010000C40E0000C40E00001000000000000000000000001F1F
      1F00363636004E4E4E00747474007B7B7B007F7F7F008E8E8E00A1A1A100A3A3
      A300CFCFCF00D5D5D500E3E3E300FAFAFA00FFFFFF00FFFFFF00AAAAAAAAAAAA
      AAAAAAAAAA006333699997966663336666006633B6BBC797664666666A00AA66
      BBDD2228BBBB4456AA00AAAABBBD4228BBBB4244AA00AAAABBDA4428BBBB4244
      AA00AAAABBBD4445BBBB4244AA00AAAABBDA4445BBBB4224AA00AAAABBAD4445
      BBBB4122AA00AAA5BBDA4225BBBB41125A00A4216BADADADBBB291112200BB42
      46DBDBDBBB2B44444400ADB4246DBDBBB4B444424A00AADB4146BBBB4B444244
      AA00AAADB4146BB4A444242AAA00AAAADB41462A444B22AAAA00AAAAAAB412A4
      449A22AAAA00AAAAAAAB4A44449A22AAAA00AAAAAAAAB444449A22AAAA00AAAA
      AAAAB999999A22AAAA00AAAAAAAAAAAAAA9B24AAAA00AAAAAAAAAAAAAAAAAAAA
      AA00}
  end
  object BitBtn5: TBitBtn
    Left = 296
    Top = 2
    Width = 33
    Height = 25
    Cancel = True
    TabOrder = 7
    OnClick = BitBtn5Click
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      333333333333333333333333000033338833333333333333333F333333333333
      0000333911833333983333333388F333333F3333000033391118333911833333
      38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
      911118111118333338F3338F833338F3000033333911111111833333338F3338
      3333F8330000333333911111183333333338F333333F83330000333333311111
      8333333333338F3333383333000033333339111183333333333338F333833333
      00003333339111118333333333333833338F3333000033333911181118333333
      33338333338F333300003333911183911183333333383338F338F33300003333
      9118333911183333338F33838F338F33000033333913333391113333338FF833
      38F338F300003333333333333919333333388333338FFF830000333333333333
      3333333333333333333888330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
end