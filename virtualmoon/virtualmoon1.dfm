object Form1: TForm1
  Left = 278
  Top = 126
  Width = 762
  Height = 590
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000888877700000000000000000000000008878877770000000000000
    0000000008888777777700000000000000000008888878777777700000000000
    0000000888888777777777000000000000000008888877777777777000000000
    0000008888887777777777770000000000000088888877777777777700000000
    0000008888888777777777777000000000000888888888877777777F70000000
    00000088888888777777777F70000000000008888888877777777777F8000000
    00000888888887777777787777000000000008888888877777F7877777000000
    00008888888887777777877777000000000088888888877777887788F7000000
    0000888888888777778777887700000000088888888888788888778870000000
    0000888888888888888887877000000000008888888888888888877770000000
    0000888888888888888887778000000000008888888888888888778700000000
    0000888888888888887777800000000000008888888888888777777000000000
    000088888888888877777700000000000000888888888788877F700000000000
    0000088888887777777800000000000000000888888888777700000000000000
    0000008888777777000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefaultPosOnly
  Scaled = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Memo2: TMemo
    Left = 0
    Top = 0
    Width = 500
    Height = 89
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    TabOrder = 2
    Visible = False
  end
  object Panel2: TPanel
    Left = 0
    Top = 25
    Width = 494
    Height = 494
    BevelOuter = bvNone
    TabOrder = 1
    object GLSceneViewer1: TGLSceneViewer
      Left = 1
      Top = 1
      Width = 476
      Height = 476
      Camera = GLCamera1
      VSync = vsmSync
      Buffer.BackgroundColor = clBlack
      Buffer.ShadeModel = smSmooth
      PopupMenu = PopupMenu1
      OnMouseDown = GLSceneViewer1MouseDown
      OnMouseMove = GLSceneViewer1MouseMove
      OnMouseUp = GLSceneViewer1MouseUp
    end
    object ScrollBar1: TScrollBar
      Left = 0
      Top = 478
      Width = 494
      Height = 16
      Align = alBottom
      LargeChange = 80
      Max = 500
      Min = -500
      PageSize = 20
      TabOrder = 1
      OnChange = ScrollBar1Change
    end
    object ScrollBar2: TScrollBar
      Left = 478
      Top = 0
      Width = 16
      Height = 478
      Align = alRight
      Kind = sbVertical
      LargeChange = 80
      Max = 500
      Min = -500
      PageSize = 20
      TabOrder = 2
      OnChange = ScrollBar1Change
    end
  end
  object PageControl1: TPageControl
    Left = 495
    Top = 25
    Width = 254
    Height = 495
    ActivePage = Position
    HotTrack = True
    MultiLine = True
    TabIndex = 0
    TabOrder = 0
    OnChange = PageControl1Change
    OnChanging = PageControl1Changing
    object Position: TTabSheet
      Caption = 'Information'
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 246
        Height = 97
        Align = alTop
        TabOrder = 0
        object Button1: TButton
          Left = 16
          Top = 35
          Width = 75
          Height = 23
          Caption = 'Recherche'
          TabOrder = 1
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 104
          Top = 35
          Width = 75
          Height = 23
          Caption = 'Suivant'
          TabOrder = 2
          OnClick = Button2Click
        end
        object GroupBox1: TGroupBox
          Left = 1
          Top = 62
          Width = 244
          Height = 34
          Align = alBottom
          Caption = 'Profile'
          TabOrder = 3
          object Label7: TLabel
            Left = 2
            Top = 8
            Width = 3
            Height = 14
            Alignment = taCenter
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Times New Roman'
            Font.Style = []
            ParentFont = False
          end
        end
        object ComboBox1: TComboBox
          Left = 16
          Top = 8
          Width = 161
          Height = 21
          AutoComplete = False
          DropDownCount = 15
          ItemHeight = 13
          TabOrder = 0
          OnKeyDown = Edit1KeyDown
          OnSelect = ComboBox1Select
        end
      end
      object Desc1: ThtmlLite
        Left = 0
        Top = 97
        Width = 246
        Height = 352
        OnHotSpotClick = Desc1HotSpotClick
        ViewImages = False
        TabOrder = 1
        Align = alClient
        PopupMenu = PopupMenu2
        ShowHint = True
        DefBackground = clWindow
        BorderStyle = htFocused
        HistoryMaxCount = 0
        DefFontName = 'Arial'
        DefPreFontName = 'Courier New'
        DefFontSize = 8
        DefFontColor = clWindowText
        NoSelect = False
        ScrollBars = ssVertical
        CharSet = DEFAULT_CHARSET
        MarginHeight = 2
        MarginWidth = 2
        htOptions = []
        OnMouseMove = Desc1MouseMove
      end
    end
    object Notes: TTabSheet
      Caption = 'Notes'
      ImageIndex = 6
      object Memo1: TMemo
        Left = 0
        Top = 58
        Width = 246
        Height = 391
        Align = alClient
        Lines.Strings = (
          '')
        ScrollBars = ssVertical
        TabOrder = 0
        WantTabs = True
        OnChange = Memo1Change
      end
      object Panel7: TPanel
        Left = 0
        Top = 0
        Width = 246
        Height = 58
        Align = alTop
        TabOrder = 1
        object notes_name: TLabel
          Left = 8
          Top = 8
          Width = 12
          Height = 13
          Caption = '    '
        end
        object Button15: TButton
          Left = 8
          Top = 28
          Width = 89
          Height = 25
          Caption = 'Update'
          TabOrder = 0
          OnClick = UpdNotesClick
        end
      end
    end
    object dbtab: TTabSheet
      Caption = 'Database'
      ImageIndex = 5
      object StringGrid2: TStringGrid
        Left = 0
        Top = 0
        Width = 246
        Height = 416
        Align = alClient
        ColCount = 2
        DefaultColWidth = 74
        DefaultRowHeight = 20
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goEditing, goAlwaysShowEditor]
        TabOrder = 0
        OnSetEditText = StringGrid2SetEditText
        ColWidths = (
          74
          963)
      end
      object Panel6: TPanel
        Left = 0
        Top = 416
        Width = 246
        Height = 33
        Align = alBottom
        TabOrder = 1
        object Button9: TButton
          Left = 86
          Top = 4
          Width = 75
          Height = 25
          Caption = 'Enregistrer'
          TabOrder = 0
          OnClick = Button9Click
        end
        object btnEffacer: TButton
          Left = 8
          Top = 4
          Width = 75
          Height = 25
          Caption = 'Nouveau'
          TabOrder = 1
          OnClick = btnEffacerClick
        end
      end
    end
    object Ephemerides: TTabSheet
      Caption = 'Eph'#233'm'#233'rides'
      ImageIndex = 1
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 246
        Height = 121
        Align = alTop
        TabOrder = 0
        TabStop = True
        object Label6: TLabel
          Left = 2
          Top = 38
          Width = 29
          Height = 13
          Caption = 'Heure'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label9: TLabel
          Left = 2
          Top = 6
          Width = 23
          Height = 13
          Caption = 'Date'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlack
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Button3: TSpeedButton
          Left = 134
          Top = 96
          Width = 33
          Height = 22
          AllowAllUp = True
          Caption = '>>'
          Transparent = False
          OnMouseDown = Button3MouseDown
          OnMouseUp = Button3MouseUp
        end
        object Button6: TSpeedButton
          Left = 92
          Top = 96
          Width = 33
          Height = 22
          AllowAllUp = True
          Caption = '>'
          Transparent = False
          OnMouseDown = Button6MouseDown
          OnMouseUp = Button3MouseUp
        end
        object Button7: TSpeedButton
          Left = 50
          Top = 96
          Width = 33
          Height = 22
          AllowAllUp = True
          Caption = '<'
          Transparent = False
          OnMouseDown = Button7MouseDown
          OnMouseUp = Button3MouseUp
        end
        object Button8: TSpeedButton
          Left = 8
          Top = 96
          Width = 33
          Height = 22
          AllowAllUp = True
          Caption = '<<'
          Transparent = False
          OnMouseDown = Button8MouseDown
          OnMouseUp = Button3MouseUp
        end
        object NM: TImage
          Left = 172
          Top = 14
          Width = 27
          Height = 24
          AutoSize = True
          Picture.Data = {
            0A544A504547496D61676518030000FFD8FFE000104A46494600010101004800
            480000FFDB0043000302020302020303030304030304050805050404050A0707
            06080C0A0C0C0B0A0B0B0D0E12100D0E110E0B0B1016101113141515150C0F17
            1816141812141514FFDB00430103040405040509050509140D0B0D1414141414
            1414141414141414141414141414141414141414141414141414141414141414
            14141414141414141414141414FFC00011080018001B03012200021101031101
            FFC4001F0000010501010101010100000000000000000102030405060708090A
            0BFFC400B5100002010303020403050504040000017D01020300041105122131
            410613516107227114328191A1082342B1C11552D1F02433627282090A161718
            191A25262728292A3435363738393A434445464748494A535455565758595A63
            6465666768696A737475767778797A838485868788898A92939495969798999A
            A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
            D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
            01010101010101010000000000000102030405060708090A0BFFC400B5110002
            0102040403040705040400010277000102031104052131061241510761711322
            328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
            292A35363738393A434445464748494A535455565758595A636465666768696A
            737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
            A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
            E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00FCB5
            B0B29B509C4512E0FF007BFBB5D7DB782ECE2B656BB9A532B9DC360F96AA7829
            5192ECB22976202FB5768FA1CB358B4B1CD97C0C0CE71401C2F88BC1736936AB
            770BFDA2D0FDE23EF27D6B96AF58F2A41686DA7F9D2453F8715E5F25B12E483C
            50069787F564B0BC1E692B6D27CAFB3EF0FF006ABD82DB5AD21ED61306A76C55
            94643CAA847E068A280391F17F8A74E86DA6B7D3D8CF7137DE915BE44AF3476D
            CE4D145007FFD9}
          OnClick = NMClick
        end
        object LabelNM: TLabel
          Left = 204
          Top = 19
          Width = 6
          Height = 14
          Caption = 'L'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnClick = NMClick
        end
        object LabelFQ: TLabel
          Left = 204
          Top = 43
          Width = 6
          Height = 14
          Caption = 'L'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnClick = FQClick
        end
        object LabelFM: TLabel
          Left = 204
          Top = 67
          Width = 6
          Height = 14
          Caption = 'L'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnClick = FMClick
        end
        object LabelLQ: TLabel
          Left = 204
          Top = 92
          Width = 6
          Height = 14
          Caption = 'L'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
          OnClick = LQClick
        end
        object FQ: TImage
          Left = 172
          Top = 37
          Width = 27
          Height = 24
          AutoSize = True
          Picture.Data = {
            0A544A504547496D6167657D030000FFD8FFE000104A46494600010101004800
            480000FFDB0043000302020302020303030304030304050805050404050A0707
            06080C0A0C0C0B0A0B0B0D0E12100D0E110E0B0B1016101113141515150C0F17
            1816141812141514FFDB00430103040405040509050509140D0B0D1414141414
            1414141414141414141414141414141414141414141414141414141414141414
            14141414141414141414141414FFC00011080018001B03012200021101031101
            FFC4001F0000010501010101010100000000000000000102030405060708090A
            0BFFC400B5100002010303020403050504040000017D01020300041105122131
            410613516107227114328191A1082342B1C11552D1F02433627282090A161718
            191A25262728292A3435363738393A434445464748494A535455565758595A63
            6465666768696A737475767778797A838485868788898A92939495969798999A
            A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
            D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
            01010101010101010000000000000102030405060708090A0BFFC400B5110002
            0102040403040705040400010277000102031104052131061241510761711322
            328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
            292A35363738393A434445464748494A535455565758595A636465666768696A
            737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
            A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
            E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00FCB9
            D334D9351B9544CA7F799ABD9FE0F780FC37A8F8B2D2CB5EB39750B796366654
            765EDC735E75E079079575903712ABB9BB2D7D01FB3D7810F8A3C7B6F676F36C
            B89A09392320951D00F7A00D8FDA17F630B2F0C78153C71E04BC373A5AC7E65D
            E993C8649625FEF29EE318C8EA335F1F2C7BD430D801EC6BF5BB4DF0A2787BC1
            97F61A862F74FBA8256B88E763B6DDD236CFD0818E0D7E4EEA2A23D42E96DA60
            20133F978FEEEE38A007685AA0D32F95DC1780B7CE8BF2EE5AFAA3F665F8A9E0
            BF0E7C48D3755D5B5B8F4AB28207F31EE030C36DE3EE83DE8A2803BCFDAABF6D
            CF0C6B1E12D4BC25F0E6D8DD3EA44FDB75E92331A843F7D2207925B8CB1C1EB5
            F0634E0B1F99A8A2803FFFD9}
          OnClick = FQClick
        end
        object FM: TImage
          Left = 172
          Top = 60
          Width = 27
          Height = 24
          AutoSize = True
          Picture.Data = {
            0A544A504547496D616765B1030000FFD8FFE000104A46494600010101004800
            480000FFDB0043000302020302020303030304030304050805050404050A0707
            06080C0A0C0C0B0A0B0B0D0E12100D0E110E0B0B1016101113141515150C0F17
            1816141812141514FFDB00430103040405040509050509140D0B0D1414141414
            1414141414141414141414141414141414141414141414141414141414141414
            14141414141414141414141414FFC00011080018001B03012200021101031101
            FFC4001F0000010501010101010100000000000000000102030405060708090A
            0BFFC400B5100002010303020403050504040000017D01020300041105122131
            410613516107227114328191A1082342B1C11552D1F02433627282090A161718
            191A25262728292A3435363738393A434445464748494A535455565758595A63
            6465666768696A737475767778797A838485868788898A92939495969798999A
            A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
            D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
            01010101010101010000000000000102030405060708090A0BFFC400B5110002
            0102040403040705040400010277000102031104052131061241510761711322
            328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
            292A35363738393A434445464748494A535455565758595A636465666768696A
            737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
            A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
            E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00FCFA
            F82BF092EBE2C7885AD03345A7DB287BA9D067E5CFDD1EE6BEE8D37E0B7C39F0
            47872DACBFE106B0D52E0A6E9AE6E51A475E3A313D0FE55E2FFB0CEAB05B689E
            26B25904124D2A89A54C798AB83923DABF44345F84DA06B5E0DFB1346B15DDC2
            605C84DAD211CA0C74E7FF00D7401F0AFED27FB21F8466F874DE39F87F22585C
            C0A1AF3474CB4472377EEF2490E07F09273CE2BE14F29CF73F857EBA6B7E1587
            C1DA1EA2639227D38C330D4967F9C44F1A1C81E9D473EF5F929A8B3CB7F72F6C
            A3C832314C7A66803B1F843F15AFBE12F8CA0D6ED614D420C08EEB4FB862B1CF
            1F7538E9EC7B57E9BF87BFE0A5FF00042FBC2D6325C58EAFE1EBF8234492D0DB
            79E32A00C8901F98668A2803E5EFDAC3F6F91F16B41BAF09782B4C7D2741BA60
            6F353B850B73743AEC007DC52464F3935F18ACEAA00DEDFF007D5145007FFFD9}
          OnClick = FMClick
        end
        object LQ: TImage
          Left = 172
          Top = 84
          Width = 27
          Height = 24
          AutoSize = True
          Picture.Data = {
            0A544A504547496D61676564030000FFD8FFE000104A46494600010101004800
            480000FFDB0043000302020302020303030304030304050805050404050A0707
            06080C0A0C0C0B0A0B0B0D0E12100D0E110E0B0B1016101113141515150C0F17
            1816141812141514FFDB00430103040405040509050509140D0B0D1414141414
            1414141414141414141414141414141414141414141414141414141414141414
            14141414141414141414141414FFC00011080018001B03012200021101031101
            FFC4001F0000010501010101010100000000000000000102030405060708090A
            0BFFC400B5100002010303020403050504040000017D01020300041105122131
            410613516107227114328191A1082342B1C11552D1F02433627282090A161718
            191A25262728292A3435363738393A434445464748494A535455565758595A63
            6465666768696A737475767778797A838485868788898A92939495969798999A
            A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
            D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
            01010101010101010000000000000102030405060708090A0BFFC400B5110002
            0102040403040705040400010277000102031104052131061241510761711322
            328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
            292A35363738393A434445464748494A535455565758595A636465666768696A
            737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
            A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
            E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00FCE5
            F861F0CF50F89BE224D3EC898A28D5649EE0AF112F622BE84F8ADF007C0DF0E3
            E19C52A5A5E5D6BB2491FF00A4C93965D87FD9038FD6B77F6065B083C37E2BF3
            1605BC9E68E213C83718D768208AF57FDA93E1EDA5AFC1FB8B98A576BC79E157
            61212A467AE0FAD007C01E22F04C7636497965279B081BA489BEF8AE2EBD65AD
            9A181E193E78C824E7B5795CD1E656F997F3A00F52F809F178FC26F180BCBD86
            4BED12E47937D670BEC6651D194F4DC3B57DC3FB487ED11F087E21FC0EB6FF00
            8453C4C64BF6920F36C2F8325C2E0FCC4AE31C7B1A28A00F827C59E34B3BAB69
            AD74C8F73C9C497078CFAED15E7B451401FFD9}
          OnClick = LQClick
        end
        object nextM: TImage
          Left = 172
          Top = 108
          Width = 27
          Height = 14
          AutoSize = True
          Picture.Data = {
            0A544A504547496D6167650D030000FFD8FFE000104A46494600010102001C00
            1C0000FFDB0043000302020302020303030304030304050805050404050A0707
            06080C0A0C0C0B0A0B0B0D0E12100D0E110E0B0B1016101113141515150C0F17
            1816141812141514FFDB00430103040405040509050509140D0B0D1414141414
            1414141414141414141414141414141414141414141414141414141414141414
            14141414141414141414141414FFC0001108000E001B03012200021101031101
            FFC4001F0000010501010101010100000000000000000102030405060708090A
            0BFFC400B5100002010303020403050504040000017D01020300041105122131
            410613516107227114328191A1082342B1C11552D1F02433627282090A161718
            191A25262728292A3435363738393A434445464748494A535455565758595A63
            6465666768696A737475767778797A838485868788898A92939495969798999A
            A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
            D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
            01010101010101010000000000000102030405060708090A0BFFC400B5110002
            0102040403040705040400010277000102031104052131061241510761711322
            328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
            292A35363738393A434445464748494A535455565758595A636465666768696A
            737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
            A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
            E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00FCAA
            AFAFFE05FF00C1323E207ED17F0E34EF1AF82BC75F0FEFB4ABACC72C32EA17AB
            71653800BDBCE82D0EC95770C8C90415652C8CAC7E40AEABC1FF00157C5FF0FF
            0042F14E8BE1BF11EA1A2E95E28B21A7EB369673144BD8036E08E3FEFA5C8C12
            924A99D923AB0055F1FF0084E1F02F8CB56F0FC1E20D1FC549A74C6DCEAFA04B
            24D6370C00DC619248D0BA86CAEF0BB5B692A594AB1E7E8A2803FFD9}
          OnClick = nextMClick
        end
        object prevM: TImage
          Left = 172
          Top = 0
          Width = 27
          Height = 14
          AutoSize = True
          Picture.Data = {
            0A544A504547496D6167652E030000FFD8FFE000104A46494600010102001C00
            1C0000FFDB0043000302020302020303030304030304050805050404050A0707
            06080C0A0C0C0B0A0B0B0D0E12100D0E110E0B0B1016101113141515150C0F17
            1816141812141514FFDB00430103040405040509050509140D0B0D1414141414
            1414141414141414141414141414141414141414141414141414141414141414
            14141414141414141414141414FFC0001108000E001B03012200021101031101
            FFC4001F0000010501010101010100000000000000000102030405060708090A
            0BFFC400B5100002010303020403050504040000017D01020300041105122131
            410613516107227114328191A1082342B1C11552D1F02433627282090A161718
            191A25262728292A3435363738393A434445464748494A535455565758595A63
            6465666768696A737475767778797A838485868788898A92939495969798999A
            A2A3A4A5A6A7A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6
            D7D8D9DAE1E2E3E4E5E6E7E8E9EAF1F2F3F4F5F6F7F8F9FAFFC4001F01000301
            01010101010101010000000000000102030405060708090A0BFFC400B5110002
            0102040403040705040400010277000102031104052131061241510761711322
            328108144291A1B1C109233352F0156272D10A162434E125F11718191A262728
            292A35363738393A434445464748494A535455565758595A636465666768696A
            737475767778797A82838485868788898A92939495969798999AA2A3A4A5A6A7
            A8A9AAB2B3B4B5B6B7B8B9BAC2C3C4C5C6C7C8C9CAD2D3D4D5D6D7D8D9DAE2E3
            E4E5E6E7E8E9EAF2F3F4F5F6F7F8F9FAFFDA000C03010002110311003F00FCAA
            AFA8BF672FF827B7C50FDA57E187893C6FE1FB7B7D3B4EB285868A9A91F28EBD
            728E0490C0C701555448BE73613CD0B1E7FD6BC3F2ED7A7695FB4C7C60D034BB
            2D334CF8AFE37D374DB1812DEDACED3C47791430448A15234459005550000A00
            0000050079FEAFA55EE81AA5E699A9D95C69DA9594CF6D75677713453412A315
            78DD1802ACAC082A4020820D51AD9F13F8AF5BF1D6BB75ADF88F58D435FD6AE8
            2FDA351D52E9EE6E25DA81577C8E4B361555464F0140E82B1A803FFFD9}
          OnClick = prevMClick
        end
        object jour: TLongEdit
          Left = 130
          Top = 4
          Width = 22
          Height = 21
          Hint = '0..31'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          Value = 31
          MaxValue = 31
        end
        object mois: TLongEdit
          Left = 88
          Top = 4
          Width = 22
          Height = 21
          Hint = '0..12'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Value = 1
          MaxValue = 12
        end
        object annee: TLongEdit
          Left = 32
          Top = 4
          Width = 36
          Height = 21
          Hint = '-2999..2999'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Value = 2002
          MinValue = -2999
          MaxValue = 2999
        end
        object seconde: TLongEdit
          Left = 130
          Top = 36
          Width = 22
          Height = 21
          Hint = '0..59'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Value = 0
          MaxValue = 59
        end
        object minute: TLongEdit
          Left = 88
          Top = 36
          Width = 22
          Height = 21
          Hint = '0..59'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 4
          Value = 0
          MaxValue = 59
        end
        object heure: TLongEdit
          Left = 43
          Top = 36
          Width = 25
          Height = 21
          Hint = '0..23'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 3
          Value = 0
          MaxValue = 23
        end
        object Button4: TButton
          Left = 92
          Top = 64
          Width = 75
          Height = 25
          Caption = 'Compute'
          TabOrder = 7
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 8
          Top = 64
          Width = 75
          Height = 25
          Caption = 'Maintenant'
          TabOrder = 6
          OnClick = Button5Click
        end
        object UpDown1: TUpDown
          Left = 68
          Top = 4
          Width = 16
          Height = 21
          Associate = annee
          Min = -2999
          Max = 2999
          Position = 2002
          TabOrder = 8
          Thousands = False
          Wrap = False
        end
        object UpDown2: TUpDown
          Left = 110
          Top = 4
          Width = 16
          Height = 21
          Associate = mois
          Min = 1
          Max = 12
          Position = 1
          TabOrder = 9
          Wrap = True
        end
        object UpDown3: TUpDown
          Left = 152
          Top = 4
          Width = 16
          Height = 21
          Associate = jour
          Min = 1
          Max = 31
          Position = 31
          TabOrder = 10
          Wrap = True
        end
        object UpDown4: TUpDown
          Left = 68
          Top = 36
          Width = 16
          Height = 21
          Associate = heure
          Min = 0
          Max = 23
          Position = 0
          TabOrder = 11
          Wrap = True
        end
        object UpDown5: TUpDown
          Left = 110
          Top = 36
          Width = 16
          Height = 21
          Associate = minute
          Min = 0
          Max = 59
          Position = 0
          TabOrder = 12
          Wrap = True
        end
        object UpDown6: TUpDown
          Left = 152
          Top = 36
          Width = 16
          Height = 21
          Associate = seconde
          Min = 0
          Max = 59
          Position = 0
          TabOrder = 13
          Wrap = True
        end
      end
      object StringGrid1: TStringGrid
        Left = 0
        Top = 121
        Width = 246
        Height = 328
        TabStop = False
        Align = alClient
        BorderStyle = bsNone
        ColCount = 2
        DefaultColWidth = 122
        DefaultRowHeight = 18
        FixedCols = 0
        RowCount = 25
        FixedRows = 0
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect, goThumbTracking]
        ParentFont = False
        TabOrder = 1
        OnMouseMove = StringGrid1MouseMove
      end
    end
    object Terminateur: TTabSheet
      Caption = 'Terminateur'
      ImageIndex = 2
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 246
        Height = 121
        Align = alTop
        TabOrder = 0
        object Label19: TLabel
          Left = 8
          Top = 12
          Width = 33
          Height = 13
          Caption = 'Int'#233'r'#234't '
        end
        object Label20: TLabel
          Left = 8
          Top = 36
          Width = 49
          Height = 13
          Caption = 'Instrument'
        end
        object Label21: TLabel
          Left = 152
          Top = 36
          Width = 19
          Height = 13
          Caption = 'mm.'
        end
        object ComboBox2: TComboBox
          Left = 80
          Top = 8
          Width = 121
          Height = 21
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 0
          Text = '1'
          OnChange = ComboBox2Change
          Items.Strings = (
            '1'
            '2'
            '3'
            '4')
        end
        object ComboBox3: TComboBox
          Left = 80
          Top = 32
          Width = 57
          Height = 21
          ItemHeight = 13
          TabOrder = 1
          Text = '999'
          OnChange = ComboBox2Change
          Items.Strings = (
            '999'
            '250'
            '150'
            '100'
            '50'
            '30'
            '5')
        end
        object RadioGroup1: TRadioGroup
          Left = 4
          Top = 56
          Width = 238
          Height = 65
          Caption = 'Tri'
          Columns = 2
          Items.Strings = (
            'Nom'
            'Int'#233'r'#234't'
            'Instrument'
            'Latitude')
          TabOrder = 2
          OnClick = ComboBox2Change
        end
      end
      object ListBox1: TListBox
        Left = 0
        Top = 121
        Width = 246
        Height = 328
        Align = alClient
        ItemHeight = 13
        Sorted = True
        TabOrder = 1
        OnClick = ListBox1Click
        OnMouseMove = ListBox1MouseMove
      end
    end
    object Outils: TTabSheet
      Caption = 'Outils'
      ImageIndex = 4
      object Bevel4: TBevel
        Left = 8
        Top = 8
        Width = 233
        Height = 137
        Shape = bsFrame
      end
      object Bevel3: TBevel
        Left = 8
        Top = 152
        Width = 233
        Height = 145
        Shape = bsFrame
      end
      object Label22: TLabel
        Left = 16
        Top = 148
        Width = 46
        Height = 13
        Caption = ' Rotation '
      end
      object Label23: TLabel
        Left = 16
        Top = 4
        Width = 48
        Height = 13
        Caption = ' Distance '
      end
      object Label24: TLabel
        Left = 24
        Top = 28
        Width = 70
        Height = 13
        Caption = 'Distance r'#233'elle'
      end
      object Label25: TLabel
        Left = 24
        Top = 52
        Width = 93
        Height = 13
        Caption = 'Distance apparente'
      end
      object Label1: TLabel
        Left = 72
        Top = 116
        Width = 6
        Height = 13
        Caption = 'L'
      end
      object Label2: TLabel
        Left = 136
        Top = 116
        Width = 7
        Height = 13
        Caption = 'B'
      end
      object Label3: TLabel
        Left = 24
        Top = 116
        Width = 31
        Height = 13
        Caption = 'Centre'
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 300
        Width = 233
        Height = 149
        Caption = 'Satellisation'
        TabOrder = 11
        Visible = False
        object Label4: TLabel
          Left = 16
          Top = 20
          Width = 87
          Height = 13
          Caption = 'Vitesse de rotation'
        end
        object SpeedButton1: TSpeedButton
          Left = 32
          Top = 44
          Width = 49
          Height = 22
          Caption = '<'
          OnClick = SpeedButton1Click
        end
        object SpeedButton2: TSpeedButton
          Left = 92
          Top = 44
          Width = 49
          Height = 22
          Caption = '| |'
          OnClick = SpeedButton2Click
        end
        object SpeedButton3: TSpeedButton
          Left = 152
          Top = 44
          Width = 49
          Height = 22
          Caption = '>'
          OnClick = SpeedButton3Click
        end
        object SpeedButton4: TSpeedButton
          Left = 156
          Top = 72
          Width = 73
          Height = 22
          Caption = 'Vue Est'
          OnClick = SpeedButton4Click
        end
        object SpeedButton5: TSpeedButton
          Left = 4
          Top = 72
          Width = 73
          Height = 22
          Caption = 'Vue Ouest'
          OnClick = SpeedButton5Click
        end
        object SpeedButton6: TSpeedButton
          Left = 80
          Top = 72
          Width = 73
          Height = 22
          Caption = 'Centre'
          OnClick = SpeedButton6Click
        end
        object Label27: TLabel
          Left = 16
          Top = 104
          Width = 73
          Height = 13
          Caption = 'Orbit Inclination'
        end
        object SpeedButton7: TSpeedButton
          Left = 156
          Top = 100
          Width = 73
          Height = 22
          Caption = 'Reset 0'#176
          OnClick = SpeedButton7Click
        end
        object ComboBox4: TComboBox
          Left = 120
          Top = 16
          Width = 97
          Height = 21
          ItemHeight = 0
          TabOrder = 0
          Text = '5 '#176'/seconde'
          OnChange = ComboBox4Change
        end
        object TrackBar6: TTrackBar
          Left = 8
          Top = 120
          Width = 221
          Height = 22
          Max = 90
          Min = -90
          Orientation = trHorizontal
          PageSize = 15
          Frequency = 15
          Position = 0
          SelEnd = 0
          SelStart = 0
          TabOrder = 1
          ThumbLength = 12
          TickMarks = tmBottomRight
          TickStyle = tsAuto
          OnChange = TrackBar6Change
        end
      end
      object GroupBox4: TGroupBox
        Left = 8
        Top = 300
        Width = 233
        Height = 149
        Caption = 'Telescope'
        TabOrder = 12
        Visible = False
        object Label26: TLabel
          Left = 136
          Top = 126
          Width = 40
          Height = 13
          Caption = 'seconds'
          Enabled = False
        end
        object Label28: TLabel
          Left = 32
          Top = 126
          Width = 26
          Height = 13
          Caption = 'every'
          Enabled = False
        end
        object ComboBox5: TComboBox
          Left = 8
          Top = 16
          Width = 105
          Height = 21
          ItemHeight = 0
          Sorted = True
          TabOrder = 0
          Text = 'ComboBox5'
          OnChange = ComboBox5Change
        end
        object Button16: TButton
          Left = 136
          Top = 14
          Width = 75
          Height = 25
          Caption = 'Show Menu'
          TabOrder = 1
          OnClick = Button16Click
        end
        object CheckBox6: TCheckBox
          Left = 8
          Top = 80
          Width = 217
          Height = 17
          Caption = 'Track position'
          TabOrder = 2
          OnClick = CheckBox6Click
        end
        object Button17: TButton
          Left = 8
          Top = 48
          Width = 97
          Height = 25
          Caption = 'Goto selected'
          TabOrder = 3
          OnClick = Button17Click
        end
        object Button18: TButton
          Left = 128
          Top = 48
          Width = 97
          Height = 25
          Caption = 'Sync selected'
          TabOrder = 4
          OnClick = Button18Click
        end
        object CheckBox7: TCheckBox
          Left = 8
          Top = 104
          Width = 217
          Height = 17
          Caption = 'Correct'
          Enabled = False
          TabOrder = 5
        end
        object Edit5: TEdit
          Left = 80
          Top = 122
          Width = 33
          Height = 21
          Enabled = False
          TabOrder = 6
          Text = '10'
        end
        object trackdelay: TUpDown
          Left = 113
          Top = 122
          Width = 16
          Height = 21
          Associate = Edit5
          Min = 0
          Max = 60
          Position = 10
          TabOrder = 7
          Wrap = False
        end
      end
      object Button12: TButton
        Left = 16
        Top = 168
        Width = 55
        Height = 25
        Caption = 'Gauche'
        TabOrder = 0
        OnMouseUp = Button12MouseUp
      end
      object Button13: TButton
        Left = 84
        Top = 168
        Width = 55
        Height = 25
        Caption = 'Droite'
        TabOrder = 1
        OnMouseUp = Button13MouseUp
      end
      object Button11: TButton
        Left = 24
        Top = 76
        Width = 185
        Height = 25
        Caption = 'Start'
        TabOrder = 2
        OnClick = Button11Click
      end
      object Edit1: TEdit
        Left = 136
        Top = 24
        Width = 73
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 3
      end
      object Edit2: TEdit
        Left = 136
        Top = 48
        Width = 73
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 4
      end
      object CheckBox1: TCheckBox
        Left = 24
        Top = 272
        Width = 201
        Height = 17
        Caption = 'Pole c'#233'leste toujours en haut.'
        TabOrder = 5
        OnClick = CheckBox1Click
      end
      object RadioGroup2: TRadioGroup
        Left = 16
        Top = 200
        Width = 209
        Height = 41
        Caption = 'Orientation par d'#233'faut'
        Columns = 2
        ItemIndex = 0
        Items.Strings = (
          'Nord'
          'Sud')
        TabOrder = 6
        OnClick = RadioGroup2Click
      end
      object CheckBox2: TCheckBox
        Left = 24
        Top = 248
        Width = 201
        Height = 17
        Caption = 'Miroir'
        TabOrder = 7
        OnClick = CheckBox2Click
      end
      object Edit3: TEdit
        Left = 88
        Top = 112
        Width = 41
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 8
      end
      object Edit4: TEdit
        Left = 152
        Top = 112
        Width = 41
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 9
      end
      object Enregistredist: TButton
        Left = 200
        Top = 112
        Width = 35
        Height = 25
        Caption = 'Copier'
        TabOrder = 10
        OnClick = EnregistredistClick
      end
    end
    object Reglage: TTabSheet
      Caption = 'R'#233'glage'
      ImageIndex = 3
      object Bevel1: TBevel
        Left = 16
        Top = 8
        Width = 193
        Height = 209
        Style = bsRaised
      end
      object Label8: TLabel
        Left = 32
        Top = 24
        Width = 48
        Height = 13
        Caption = 'P'#233'nombre'
      end
      object Label12: TLabel
        Left = 32
        Top = 152
        Width = 28
        Height = 13
        Caption = 'Direct'
      end
      object Label14: TLabel
        Left = 32
        Top = 2
        Width = 47
        Height = 13
        Caption = 'Eclairage '
      end
      object Label11: TLabel
        Left = 32
        Top = 88
        Width = 27
        Height = 13
        Caption = 'Diffus'
      end
      object CheckBox19: TCheckBox
        Left = 29
        Top = 80
        Width = 145
        Height = 17
        Caption = 'Hachure de l'#39'ombre'
        TabOrder = 5
        Visible = False
        OnClick = CheckBox19Click
      end
      object TrackBar2: TTrackBar
        Left = 24
        Top = 40
        Width = 177
        Height = 33
        Max = 255
        Orientation = trHorizontal
        PageSize = 5
        Frequency = 43
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 0
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = TrackBar2Change
      end
      object TrackBar3: TTrackBar
        Left = 24
        Top = 104
        Width = 177
        Height = 33
        Max = 255
        Orientation = trHorizontal
        PageSize = 5
        Frequency = 43
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 1
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = TrackBar3Change
      end
      object TrackBar4: TTrackBar
        Left = 24
        Top = 168
        Width = 177
        Height = 33
        Max = 255
        Orientation = trHorizontal
        PageSize = 5
        Frequency = 43
        Position = 0
        SelEnd = 0
        SelStart = 0
        TabOrder = 2
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = TrackBar4Change
      end
      object Panel8: TPanel
        Left = 16
        Top = 224
        Width = 193
        Height = 121
        TabOrder = 3
        object Label13: TLabel
          Left = 16
          Top = 24
          Width = 50
          Height = 13
          Caption = 'R'#233'solution'
        end
        object Label15: TLabel
          Left = 16
          Top = 72
          Width = 29
          Height = 13
          Caption = '0 FPS'
        end
        object Label16: TLabel
          Left = 16
          Top = 4
          Width = 60
          Height = 13
          Caption = 'Performance'
        end
        object Label17: TLabel
          Left = 16
          Top = 88
          Width = 18
          Height = 13
          Caption = 'acc'
        end
        object Label18: TLabel
          Left = 16
          Top = 104
          Width = 14
          Height = 13
          Caption = 'tex'
        end
        object TrackBar5: TTrackBar
          Left = 8
          Top = 40
          Width = 177
          Height = 33
          Max = 3
          Min = 1
          Orientation = trHorizontal
          PageSize = 1
          Frequency = 1
          Position = 1
          SelEnd = 0
          SelStart = 0
          TabOrder = 0
          TickMarks = tmBottomRight
          TickStyle = tsAuto
          OnChange = TrackBar5Change
        end
        object Button14: TButton
          Left = 96
          Top = 4
          Width = 89
          Height = 25
          Caption = 'OpenGL Info'
          TabOrder = 1
          OnClick = Button14Click
        end
      end
      object GroupBox2: TGroupBox
        Left = 16
        Top = 352
        Width = 193
        Height = 89
        Caption = 'Options valid at next startup'
        TabOrder = 4
        object CheckBox3: TCheckBox
          Left = 8
          Top = 24
          Width = 177
          Height = 17
          Caption = 'Use Double Buffer'
          TabOrder = 0
          OnClick = CheckBox3Click
        end
        object CheckBox4: TCheckBox
          Left = 8
          Top = 40
          Width = 177
          Height = 17
          Caption = 'Use Stencil Buffer'
          TabOrder = 1
          OnClick = CheckBox4Click
        end
        object CheckBox5: TCheckBox
          Left = 8
          Top = 56
          Width = 177
          Height = 17
          Caption = 'Use texture for the libration zone'
          TabOrder = 2
          OnClick = CheckBox5Click
        end
      end
    end
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 754
    Height = 27
    Align = alTop
    AutoDrag = False
    AutoSize = True
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 4
    object ToolBar2: TToolBar
      Left = 11
      Top = 2
      Width = 147
      Height = 21
      Align = alNone
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 69
      Caption = 'ToolBar2'
      DragMode = dmAutomatic
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      ShowCaptions = True
      TabOrder = 0
      Wrapable = False
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        AutoSize = True
        Caption = 'Fichier'
        MenuItem = Fichier1
        OnClick = ToolButton1Click
      end
      object ToolButton2: TToolButton
        Left = 42
        Top = 0
        AutoSize = True
        Caption = 'Configuration'
        MenuItem = Configuration1
      end
      object ToolButton8: TToolButton
        Left = 115
        Top = 0
        AutoSize = True
        Caption = 'Aide'
        MenuItem = Aide1
        OnClick = ToolButton8Click
      end
    end
    object ToolBar1: TToolBar
      Left = 231
      Top = 2
      Width = 456
      Height = 23
      Align = alNone
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 53
      Caption = 'ToolBar'
      DragMode = dmAutomatic
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      ShowCaptions = True
      TabOrder = 1
      Wrapable = False
      object Label10: TLabel
        Left = 0
        Top = 2
        Width = 30
        Height = 21
        Caption = 'Zoom:'
        Layout = tlCenter
      end
      object TrackBar1: TTrackBar
        Left = 30
        Top = 2
        Width = 150
        Height = 21
        Max = 308
        Min = 200
        Orientation = trHorizontal
        PageSize = 10
        Frequency = 10
        Position = 200
        SelEnd = 0
        SelStart = 0
        TabOrder = 0
        ThumbLength = 12
        TickMarks = tmBottomRight
        TickStyle = tsAuto
        OnChange = TrackBar1Change
      end
      object ToolButton9: TToolButton
        Left = 180
        Top = 2
        AutoSize = True
        Caption = '1:1'
        ImageIndex = 4
        OnClick = ToolButton9Click
      end
      object ToolButton5: TToolButton
        Left = 206
        Top = 2
        AutoSize = True
        Caption = 'Centre'
        ImageIndex = 1
        OnClick = ToolButton5Click
      end
      object ToolButton7: TToolButton
        Left = 248
        Top = 2
        AutoSize = True
        Caption = 'Image'
        Enabled = False
        ImageIndex = 3
        OnClick = ToolButton7Click
      end
      object ToolButton10: TToolButton
        Left = 288
        Top = 2
        AutoSize = True
        Caption = 'Voisinage'
        ImageIndex = 4
        OnClick = ToolButton10Click
      end
      object ToolButton3: TToolButton
        Left = 345
        Top = 2
        Caption = 'Rotation'
        ImageIndex = 5
        Style = tbsCheck
        OnClick = ToolButton3Click
      end
      object DebugLabel: TLabel
        Left = 398
        Top = 2
        Width = 58
        Height = 21
        Caption = 'DebugLabel'
        Layout = tlCenter
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 529
    Width = 752
    Height = 19
    Align = alNone
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 200
      end
      item
        Width = 100
      end>
    SimplePanel = False
  end
  object Panel2D: TPanel
    Left = 16
    Top = 296
    Width = 249
    Height = 201
    BevelOuter = bvNone
    TabOrder = 5
    object HorzScrollBar: TScrollBar
      Left = 0
      Top = 185
      Width = 249
      Height = 16
      Align = alBottom
      PageSize = 0
      TabOrder = 0
      OnChange = ScrollBar2DChange
    end
    object VertScrollBar: TScrollBar
      Left = 233
      Top = 0
      Width = 16
      Height = 185
      Align = alRight
      Kind = sbVertical
      PageSize = 0
      TabOrder = 1
      OnChange = ScrollBar2DChange
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 233
      Height = 185
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object Shape2: TShape
        Left = 56
        Top = 44
        Width = 5
        Height = 5
        Brush.Color = 16639
        Pen.Color = 16639
        Pen.Width = 0
        Visible = False
      end
      object Shape1: TShape
        Left = 48
        Top = 36
        Width = 5
        Height = 5
        Brush.Color = 16639
        Pen.Color = 16639
        Pen.Width = 0
      end
      object Label5: TLabel
        Left = 56
        Top = 72
        Width = 43
        Height = 16
        BiDiMode = bdLeftToRight
        Caption = 'Label5'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentBiDiMode = False
        ParentFont = False
        PopupMenu = PopupMenu1
        Transparent = True
        Layout = tlCenter
      end
      object Image1: TImage
        Left = 0
        Top = 0
        Width = 233
        Height = 185
        AutoSize = True
        PopupMenu = PopupMenu1
        Transparent = True
        OnMouseDown = Image1MouseDown
        OnMouseMove = Image1MouseMove
        OnMouseUp = GLSceneViewer1MouseUp
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 408
    Top = 48
    object Fichier1: TMenuItem
      AutoHotkeys = maAutomatic
      Caption = 'Fichier'
      object Enregistrersous1: TMenuItem
        Caption = 'Enregistrer sous'
        object BMP1: TMenuItem
          Caption = 'BMP'
          OnClick = BMP1Click
        end
        object JPG1: TMenuItem
          Caption = 'JPG'
          OnClick = JPG1Click
        end
        object BMP15001: TMenuItem
          Caption = 'BMP1500'
          OnClick = BMP15001Click
        end
        object BMP30001: TMenuItem
          Caption = 'BMP3000'
          OnClick = BMP30001Click
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Selectiondimprimante1: TMenuItem
        Caption = 'Selection d'#39'imprimante...'
        OnClick = Selectiondimprimante1Click
      end
      object Imprimer1: TMenuItem
        Caption = 'Imprimer'
        OnClick = Imprimer1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object CartesduCiel1: TMenuItem
        Caption = 'Cartes du Ciel'
        OnClick = CartesduCiel1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Quitter1: TMenuItem
        Caption = 'Quitter'
        OnClick = Quitter1Click
      end
    end
    object Configuration1: TMenuItem
      Caption = 'Configuration'
      OnClick = Configuration1Click
    end
    object Aide1: TMenuItem
      Caption = 'Aide'
      object Aide2: TMenuItem
        Caption = 'Aide'
        OnClick = Aide2Click
      end
      object Glossaire1: TMenuItem
        Caption = 'Glossaire'
        OnClick = Glossaire1Click
      end
      object Encyclopedia1: TMenuItem
        Caption = 'Encyclopedia'
        OnClick = Encyclopedia1Click
      end
      object OverlayCaption2: TMenuItem
        Caption = 'Overlay Caption'
        Visible = False
        OnClick = OverlayCaption1Click
      end
      object Apropos1: TMenuItem
        Caption = 'A propos'
        OnClick = Apropos1Click
      end
    end
  end
  object GLScene1: TGLScene
    VisibilityCulling = vcHierarchical
    Left = 16
    Top = 400
    object DummyCube2: TDummyCube
      CubeSize = 0.100000001490116
      object Annulus1: TAnnulus
        Direction.Coordinates = {DBB1493FC8A71D3F0000000000000000}
        Position.Coordinates = {000000000000000000007A440000803F}
        Up.Coordinates = {4E9EDEADF8660E2E0000803F00000000}
        Visible = False
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        BottomRadius = 3
        Height = 0.300000011920929
        Slices = 64
        BottomInnerRadius = 0.449999988079071
        TopInnerRadius = 1
        TopRadius = 3
        Parts = [anBottom]
      end
      object ArrowLine1: TArrowLine
        Direction.Coordinates = {000080BF000000000000000000000000}
        Position.Coordinates = {00000000000000000080ED440000803F}
        Up.Coordinates = {0000000000000000000080BF00000000}
        Visible = False
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Specular.Color = {9A99993E9A99993E9A99993E0000803F}
        BottomRadius = 0.00999999977648258
        Height = 0.0799999982118607
        TopRadius = 0.00999999977648258
        HeadStackingStyle = ahssIncluded
        TopArrowHeadHeight = 0.0500000007450581
        TopArrowHeadRadius = 0.0199999995529652
        BottomArrowHeadHeight = 0.5
        BottomArrowHeadRadius = 0.200000002980232
      end
      object DummyCube1: TDummyCube
        Position.Coordinates = {00000000000000000000FA440000803F}
        TransformationMode = tmParentNoPos
        Up.Coordinates = {000000000000803F0000008000000000}
        CubeSize = 1
        object GLLightSource1: TGLLightSource
          Ambient.Color = {9A99993E9A99993E9A99993E0000803F}
          ConstAttenuation = 1
          Position.Coordinates = {00000000000000000000A0410000803F}
          LightStyle = lsParallel
          Specular.Color = {CDCC4C3FCDCC4C3FCDCC4C3F0000803F}
          SpotCutOff = 180
        end
        object HiresSphere500: TSphere
          TransformationMode = tmParentWithPos
          Visible = False
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'H500'
          Bottom = 0
          Radius = 0.5
          Slices = 30
          Stacks = 30
          Stop = 1
          Top = 1
        end
        object HiresSphere: TSphere
          TransformationMode = tmParentWithPos
          Visible = False
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'H1'
          Bottom = 0
          Radius = 0.5
          Slices = 30
          Stacks = 30
          Stop = 1
          Top = 1
        end
        object Sphere1: TSphere
          TransformationMode = tmParentWithPos
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Q1'
          Bottom = 0
          Radius = 0.5
          Slices = 90
          Stacks = 90
          Start = 90
          Stop = 180
        end
        object Sphere2: TSphere
          Direction.Coordinates = {00000000000000800000803F00000000}
          TransformationMode = tmParentWithPos
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Q2'
          Bottom = 0
          Radius = 0.5
          Slices = 90
          Stacks = 90
          Start = 180
          Stop = 270
        end
        object Sphere3: TSphere
          TransformationMode = tmParentWithPos
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Q3'
          Radius = 0.5
          Slices = 90
          Stacks = 90
          Start = 90
          Stop = 180
          Top = 0
        end
        object Sphere4: TSphere
          TransformationMode = tmParentWithPos
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Q4'
          Radius = 0.5
          Slices = 90
          Stacks = 90
          Start = 180
          Stop = 270
          Top = 0
        end
        object Sphere5: TSphere
          TransformationMode = tmParentWithPos
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Q5'
          Bottom = 0
          Radius = 0.5
          Slices = 90
          Stacks = 90
          Start = 270
        end
        object Sphere6: TSphere
          TransformationMode = tmParentWithPos
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Q6'
          Bottom = 0
          Radius = 0.5
          Slices = 90
          Stacks = 90
          Stop = 90
        end
        object Sphere7: TSphere
          TransformationMode = tmParentWithPos
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Q7'
          Radius = 0.5
          Slices = 90
          Stacks = 90
          Start = 270
          Top = 0
        end
        object Sphere8: TSphere
          TransformationMode = tmParentWithPos
          Material.MaterialLibrary = GLMaterialLibrary1
          Material.LibMaterialName = 'Q8'
          Radius = 0.5
          Slices = 90
          Stacks = 90
          Stop = 90
          Top = 0
        end
      end
      object GLLightSource2: TGLLightSource
        Ambient.Color = {0000803F0000803F0000803F0000803F}
        ConstAttenuation = 1
        LightStyle = lsParallel
        Shining = False
        Specular.Color = {0000803F0000803F0000803F0000803F}
        SpotCutOff = 180
        SpotDirection.Coordinates = {00000000000000000000803F00000000}
      end
    end
    object GLMirror1: TGLMirror
      Position.Coordinates = {00000000000000000000C8C20000803F}
      Visible = False
      Material.BackProperties.Ambient.Color = {00000000000000000000000000000000}
      Material.BackProperties.Diffuse.Color = {00000000000000000000000000000000}
      Material.BackProperties.Emission.Color = {00000000000000000000000000000000}
      Material.BackProperties.Specular.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Ambient.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Diffuse.Color = {00000000000000000000000000000000}
      Material.FrontProperties.Emission.Color = {00000000000000000000000000000000}
      Material.BlendingMode = bmTransparency
      MirrorOptions = []
      Height = 1
      Width = 1
    end
    object DummyCube3: TDummyCube
      CubeSize = 1
      object HUDSprite2: THUDSprite
        Position.Coordinates = {0000C8420000C842000000000000803F}
        Visible = False
        Material.BackProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.BackProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.BackProperties.PolygonMode = pmLines
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Emission.Color = {000000000000803FF8FEFE3E0000803F}
        Material.FrontProperties.PolygonMode = pmLines
        Material.Texture.FilteringQuality = tfAnisotropic
        Width = 30
        Height = 1
        Rotation = 70
        NoZWrite = False
      end
      object HUDSprite1: THUDSprite
        Position.Coordinates = {0000C8420000C842000000000000803F}
        Up.Coordinates = {000000000000803F0000008000000000}
        Material.FrontProperties.Ambient.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Diffuse.Color = {0000000000000000000000000000803F}
        Material.FrontProperties.Emission.Color = {0000803F0000003F000000000000803F}
        Material.Texture.FilteringQuality = tfAnisotropic
        Width = 5
        Height = 5
        NoZWrite = False
      end
      object HUDText1: THUDText
        Position.Coordinates = {0000C8420000C842000000000000803F}
        Scale.Coordinates = {CDCCCC3ECDCCCC3E0000803F00000000}
        BitmapFont = BitmapFont1
        Text = 'Az'
        Alignment = taLeftJustify
        Layout = tlCenter
      end
    end
    object DummyCube4: TDummyCube
      CubeSize = 1
    end
    object GLCamera1: TGLCamera
      DepthOfView = 100000
      FocalLength = 100
      CameraStyle = csOrthogonal
      Position.Coordinates = {6F12833A6F12833A000000000000803F}
      Direction.Coordinates = {00000000000000000000803F00000000}
      Up.Coordinates = {000000800000803F0000000000000000}
      Left = 328
      Top = 216
    end
  end
  object GLMaterialLibrary1: TGLMaterialLibrary
    Materials = <
      item
        Name = 'Q1'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureOffset.Coordinates = {52499DB952499D390000000000000000}
        Texture2Name = 'O1'
      end
      item
        Name = 'Q2'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureOffset.Coordinates = {52499D3952499D390000000000000000}
        Texture2Name = 'O2'
      end
      item
        Name = 'Q3'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureOffset.Coordinates = {52499DB952499DB90000000000000000}
        Texture2Name = 'O3'
      end
      item
        Name = 'Q4'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureOffset.Coordinates = {52499D3952499DB90000000000000000}
        Texture2Name = 'O4'
      end
      item
        Name = 'Q5'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureOffset.Coordinates = {52499DB952499D390000000000000000}
        Texture2Name = 'O5'
      end
      item
        Name = 'Q6'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureOffset.Coordinates = {52499D3952499D390000000000000000}
        Texture2Name = 'O6'
      end
      item
        Name = 'Q7'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureOffset.Coordinates = {52499DB952499DB90000000000000000}
        Texture2Name = 'O7'
      end
      item
        Name = 'Q8'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        TextureOffset.Coordinates = {52499D3952499DB90000000000000000}
        Texture2Name = 'O8'
      end
      item
        Name = 'H1'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.MaterialOptions = [moIgnoreFog]
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.MinFilter = miLinear
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        Texture2Name = 'OH'
      end
      item
        Name = 'O1'
        Material.BackProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.BackProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.BackProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'O2'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'O3'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'O4'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'O5'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'O6'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'O7'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'O8'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'OH'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end
      item
        Name = 'H500'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.ImageAlpha = tiaOpaque
        Material.Texture.TextureMode = tmModulate
        Material.Texture.FilteringQuality = tfAnisotropic
        Material.Texture.Disabled = False
        Tag = 0
        Texture2Name = 'O500'
      end
      item
        Name = 'O500'
        Material.FrontProperties.Ambient.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Diffuse.Color = {0000803F0000803F0000803F0000803F}
        Material.FrontProperties.Specular.Color = {0000803F0000803F0000803F0000803F}
        Material.Texture.TextureMode = tmModulate
        Material.Texture.TextureWrap = twNone
        Material.Texture.FilteringQuality = tfAnisotropic
        Tag = 0
      end>
    Left = 16
    Top = 432
  end
  object Timer1: TAsyncTimer
    Interval = 2000
    OnTimer = Timer1Timer
    ThreadPriority = tpTimeCritical
    Left = 64
    Top = 464
  end
  object GLCadencer1: TGLCadencer
    Scene = GLScene1
    Enabled = False
    OnProgress = GLCadencer1Progress
    Left = 16
    Top = 464
  end
  object EphTimer1: TTimer
    Enabled = False
    Interval = 250
    OnTimer = EphTimer1Timer
    Left = 424
    Top = 464
  end
  object BitmapFont1: TBitmapFont
    GlyphsIntervalX = 0
    GlyphsIntervalY = 8
    Ranges = <
      item
        StartASCII = 'A'
        StopASCII = 'Z'
        StartGlyphIdx = 0
      end
      item
        StartASCII = 'a'
        StopASCII = 'z'
        StartGlyphIdx = 26
      end
      item
        StartASCII = '0'
        StopASCII = '9'
        StartGlyphIdx = 52
      end
      item
        StartASCII = '-'
        StopASCII = '.'
        StartGlyphIdx = 62
      end>
    CharWidth = 13
    VSpace = 2
    Left = 64
    Top = 432
  end
  object DdeClientConv1: TDdeClientConv
    Left = 24
    Top = 40
  end
  object DdeClientItem1: TDdeClientItem
    DdeConv = DdeClientConv1
    OnChange = DdeClientItem1Change
    Left = 72
    Top = 40
  end
  object ChartTimer: TTimer
    Enabled = False
    OnTimer = ChartTimerTimer
    Left = 24
    Top = 72
  end
  object PopupMenu1: TPopupMenu
    Left = 424
    Top = 96
    object Position1: TMenuItem
      Caption = 'Position'
      OnClick = Position1Click
    end
    object Notes1: TMenuItem
      Caption = 'Notes'
      OnClick = Notes1Click
    end
    object Image2: TMenuItem
      Caption = 'Image'
      Enabled = False
      OnClick = ToolButton7Click
    end
    object Voisinage1: TMenuItem
      Caption = 'Voisinage'
      OnClick = ToolButton10Click
    end
    object Centre1: TMenuItem
      Caption = 'Centre'
      OnClick = ToolButton5Click
    end
    object Zoom1: TMenuItem
      Caption = 'Zoom'
      object x11: TMenuItem
        Caption = 'x1'
        OnClick = ToolButton9Click
      end
      object x21: TMenuItem
        Caption = 'x2'
        OnClick = x21Click
      end
      object x41: TMenuItem
        Caption = 'x4'
        OnClick = x41Click
      end
    end
    object Eyepiece1: TMenuItem
      Caption = 'Eyepiece'
      object e01: TMenuItem
        Caption = 'none'
        OnClick = ZoomEyepieceClick
      end
      object e11: TMenuItem
        Tag = 1
        Caption = 'e1'
        OnClick = ZoomEyepieceClick
      end
      object e21: TMenuItem
        Tag = 2
        Caption = 'e2'
        OnClick = ZoomEyepieceClick
      end
      object e31: TMenuItem
        Tag = 3
        Caption = 'e3'
        OnClick = ZoomEyepieceClick
      end
      object e41: TMenuItem
        Tag = 4
        Caption = 'e4'
        OnClick = ZoomEyepieceClick
      end
      object e51: TMenuItem
        Tag = 5
        Caption = 'e5'
        OnClick = ZoomEyepieceClick
      end
      object e61: TMenuItem
        Tag = 6
        Caption = 'e6'
        OnClick = ZoomEyepieceClick
      end
      object e71: TMenuItem
        Tag = 7
        Caption = 'e7'
        OnClick = ZoomEyepieceClick
      end
      object e81: TMenuItem
        Tag = 8
        Caption = 'e8'
        OnClick = ZoomEyepieceClick
      end
      object e91: TMenuItem
        Tag = 9
        Caption = 'e9'
        OnClick = ZoomEyepieceClick
      end
      object e101: TMenuItem
        Tag = 10
        Caption = 'e10'
        OnClick = ZoomEyepieceClick
      end
    end
    object Distance1: TMenuItem
      Caption = 'Distance'
      OnClick = Distance1Click
    end
    object LgendeGologique1: TMenuItem
      Caption = 'L'#233'gende G'#233'ologique'
      Visible = False
      OnClick = LgendeGologique1Click
    end
    object OverlayCaption1: TMenuItem
      Caption = 'Overlay Caption'
      Visible = False
      OnClick = OverlayCaption1Click
    end
    object Rotation1: TMenuItem
      Caption = 'Satellisation'
      Visible = False
      object Stop1: TMenuItem
        Caption = 'Stop'
        OnClick = Stop1Click
      end
      object EastWest1: TMenuItem
        Caption = 'Direction'
        OnClick = EastWest1Click
      end
      object N10seconde1: TMenuItem
        Caption = '10'#176'/seconde'
        OnClick = N10seconde1Click
      end
      object N5seconde1: TMenuItem
        Caption = '5'#176'/seconde'
        OnClick = N5seconde1Click
      end
      object N1seconde1: TMenuItem
        Caption = '1'#176'/seconde'
        OnClick = N1seconde1Click
      end
      object N05seconde1: TMenuItem
        Caption = '0.5'#176'/seconde'
        OnClick = N05seconde1Click
      end
      object N02seconde1: TMenuItem
        Caption = '0.2'#176'/seconde'
        OnClick = N02seconde1Click
      end
    end
  end
  object SaveDialog1: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Left = 440
    Top = 48
  end
  object PrinterSetupDialog1: TPrinterSetupDialog
    Left = 376
    Top = 40
  end
  object PopupMenu2: TPopupMenu
    Left = 440
    Top = 168
    object SelectAll1: TMenuItem
      Caption = 'Select All'
      OnClick = SelectAll1Click
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      OnClick = Copy1Click
    end
  end
  object LabelTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = LabelTimerTimer
    Left = 24
    Top = 112
  end
  object ReglageTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ReglageTimerTimer
    Left = 24
    Top = 152
  end
  object RotationCadencer: TGLCadencer
    Scene = GLScene1
    Enabled = False
    SleepLength = 5
    OnProgress = RotationCadencerProgress
    Left = 104
    Top = 464
  end
  object TelescopeTimer: TTimer
    Enabled = False
    OnTimer = TelescopeTimerTimer
    Left = 24
    Top = 184
  end
  object dbm: TLiteDB
    Active = False
    DllLoaded = False
    CallBackOnly = False
    FetchMemoryLimit = 16777216
    Left = 24
    Top = 224
  end
end
