object Form1: TForm1
  Left = 269
  Top = 163
  Width = 811
  Height = 580
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
    TabOrder = 1
    Visible = False
  end
  object PageControl1: TPageControl
    Left = 495
    Top = 25
    Width = 282
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
        Width = 274
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
          Width = 272
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
        Width = 274
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
        Width = 274
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
        Width = 274
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
        Width = 274
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
        Width = 274
        Height = 33
        Align = alBottom
        TabOrder = 1
        object Button9: TButton
          Left = 86
          Top = 4
          Width = 75
          Height = 25
          Caption = 'Remplacer'
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
        object Button19: TButton
          Left = 164
          Top = 4
          Width = 73
          Height = 25
          Caption = 'Coord.'
          TabOrder = 2
          OnClick = EnregistredistClick
        end
        object Button20: TButton
          Left = 237
          Top = 4
          Width = 65
          Height = 25
          Caption = 'Delete'
          TabOrder = 3
          OnClick = Button20Click
        end
      end
    end
    object Ephemerides: TTabSheet
      Caption = 'Eph'#233'm'#233'rides'
      ImageIndex = 1
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 274
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
          Top = 107
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
          Width = 25
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
          Width = 25
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
          Width = 25
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
          Width = 25
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
          Left = 104
          Top = 64
          Width = 63
          Height = 25
          Caption = 'Compute'
          TabOrder = 7
          OnClick = Button4Click
        end
        object Button5: TButton
          Left = 8
          Top = 64
          Width = 65
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
          Left = 113
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
          Left = 155
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
          Left = 113
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
          Left = 155
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
        object Button10: TButton
          Left = 76
          Top = 64
          Width = 25
          Height = 25
          Caption = '0h'
          TabOrder = 14
          OnClick = Button10Click
        end
      end
      object StringGrid1: TStringGrid
        Left = 0
        Top = 121
        Width = 274
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
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goRangeSelect]
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
        Width = 274
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
        Width = 274
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
        Height = 193
        Shape = bsFrame
      end
      object Bevel3: TBevel
        Left = 8
        Top = 232
        Width = 233
        Height = 153
        Shape = bsFrame
      end
      object Label22: TLabel
        Left = 16
        Top = 228
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
        Left = 22
        Top = 140
        Width = 6
        Height = 13
        Caption = 'L'
      end
      object Label2: TLabel
        Left = 120
        Top = 140
        Width = 7
        Height = 13
        Caption = 'B'
      end
      object Label3: TLabel
        Left = 40
        Top = 120
        Width = 31
        Height = 13
        Caption = 'Centre'
      end
      object Button12: TButton
        Left = 16
        Top = 248
        Width = 55
        Height = 25
        Caption = 'Gauche'
        TabOrder = 0
        OnMouseUp = Button12MouseUp
      end
      object Button13: TButton
        Left = 84
        Top = 248
        Width = 55
        Height = 25
        Caption = 'Droite'
        TabOrder = 1
        OnMouseUp = Button13MouseUp
      end
      object Button11: TButton
        Left = 24
        Top = 80
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
        Top = 352
        Width = 201
        Height = 17
        Caption = 'Pole c'#233'leste toujours en haut.'
        TabOrder = 5
        OnClick = CheckBox1Click
      end
      object RadioGroup2: TRadioGroup
        Left = 16
        Top = 280
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
        Top = 328
        Width = 201
        Height = 17
        Caption = 'Miroir'
        TabOrder = 7
        OnClick = CheckBox2Click
      end
      object Edit3: TEdit
        Left = 40
        Top = 136
        Width = 57
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 8
      end
      object Edit4: TEdit
        Left = 136
        Top = 136
        Width = 57
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 9
      end
      object Enregistredist: TButton
        Left = 80
        Top = 168
        Width = 75
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
        TabOrder = 4
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
        PageSize = 10
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
        Height = 81
        Caption = 'Options valid at next startup'
        TabOrder = 5
        object CheckBox3: TCheckBox
          Left = 16
          Top = 24
          Width = 161
          Height = 17
          Caption = 'Use Double Buffer'
          TabOrder = 0
          OnClick = CheckBox3Click
        end
        object CheckBox4: TCheckBox
          Left = 16
          Top = 40
          Width = 169
          Height = 17
          Caption = 'Use Stencil Buffer'
          TabOrder = 1
          OnClick = CheckBox4Click
        end
        object CheckBox5: TCheckBox
          Left = 16
          Top = 56
          Width = 169
          Height = 17
          Caption = 'Texture de la zone de libration'
          TabOrder = 2
          OnClick = CheckBox5Click
        end
      end
    end
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 803
    Height = 27
    Align = alTop
    AutoDrag = False
    AutoSize = True
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 3
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
      Width = 815
      Height = 23
      Align = alNone
      AutoSize = True
      ButtonHeight = 21
      ButtonWidth = 55
      Caption = 'ToolBar'
      DragMode = dmAutomatic
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      ShowCaptions = True
      TabOrder = 1
      Wrapable = False
      object SpeedButton8: TSpeedButton
        Left = 0
        Top = 2
        Width = 23
        Height = 21
        Glyph.Data = {
          9E050000424D9E05000000000000360400002800000012000000120000000100
          08000000000068010000232E0000232E00000001000000000000000000003300
          00006600000099000000CC000000FF0000000033000033330000663300009933
          0000CC330000FF33000000660000336600006666000099660000CC660000FF66
          000000990000339900006699000099990000CC990000FF99000000CC000033CC
          000066CC000099CC0000CCCC0000FFCC000000FF000033FF000066FF000099FF
          0000CCFF0000FFFF000000003300330033006600330099003300CC003300FF00
          330000333300333333006633330099333300CC333300FF333300006633003366
          33006666330099663300CC663300FF6633000099330033993300669933009999
          3300CC993300FF99330000CC330033CC330066CC330099CC3300CCCC3300FFCC
          330000FF330033FF330066FF330099FF3300CCFF3300FFFF3300000066003300
          66006600660099006600CC006600FF0066000033660033336600663366009933
          6600CC336600FF33660000666600336666006666660099666600CC666600FF66
          660000996600339966006699660099996600CC996600FF99660000CC660033CC
          660066CC660099CC6600CCCC6600FFCC660000FF660033FF660066FF660099FF
          6600CCFF6600FFFF660000009900330099006600990099009900CC009900FF00
          990000339900333399006633990099339900CC339900FF339900006699003366
          99006666990099669900CC669900FF6699000099990033999900669999009999
          9900CC999900FF99990000CC990033CC990066CC990099CC9900CCCC9900FFCC
          990000FF990033FF990066FF990099FF9900CCFF9900FFFF99000000CC003300
          CC006600CC009900CC00CC00CC00FF00CC000033CC003333CC006633CC009933
          CC00CC33CC00FF33CC000066CC003366CC006666CC009966CC00CC66CC00FF66
          CC000099CC003399CC006699CC009999CC00CC99CC00FF99CC0000CCCC0033CC
          CC0066CCCC0099CCCC00CCCCCC00FFCCCC0000FFCC0033FFCC0066FFCC0099FF
          CC00CCFFCC00FFFFCC000000FF003300FF006600FF009900FF00CC00FF00FF00
          FF000033FF003333FF006633FF009933FF00CC33FF00FF33FF000066FF003366
          FF006666FF009966FF00CC66FF00FF66FF000099FF003399FF006699FF009999
          FF00CC99FF00FF99FF0000CCFF0033CCFF0066CCFF0099CCFF00CCCCFF00FFCC
          FF0000FFFF0033FFFF0066FFFF0099FFFF00CCFFFF00FFFFFF00000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000D7D7D7D7D7D7
          D7D7D7D7D7D7D7D7D7D7D7D70000D71E1E1ED2D2D2B4B4B4B9B9B9050505D7D7
          0000D71E1E1ED2D2D2B4B4B4B9B9B9050505D7D70000D71E1E1ED2D2D2B4B4B4
          B9B9B9050505D7D70000D71E1E1ED2D2D2B4B4B4B9B9B9050505D7D70000D7D7
          D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D70000D7D700000000D7D7D7D700000000
          D7D7D7D70000D7D7D70000D7D7D7D7D7D7000081D7D7D7D70000D7D7D7810081
          D7D7D7D7810000D7D7D7D7D70000D7D7D7D7000000000000000081D7D7D7D7D7
          0000D7D7D7D7810081D7D7810000D7D7D7D7D7D70000D7D7D7D7D70000D7D700
          0081D7D7D7D7D7D70000D7D7D7D7D7810081810000D7D7D7D7D7D7D70000D7D7
          D7D7D7D70000000081D7D7D7D7D7D7D70000D7D7D7D7D7D781000000D7D7D7D7
          D7D7D7D70000D7D7D7D7D7D7D7000081D7D7D7D7D7D7D7D70000D7D7D7D7D7D7
          D78100D7D7D7D7D7D7D7D7D70000D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7D7
          0000}
        OnClick = SpeedButton8Click
      end
      object ToolButton11: TToolButton
        Left = 23
        Top = 2
        Width = 4
        Caption = 'ToolButton11'
        ImageIndex = 6
        Style = tbsSeparator
      end
      object Label10: TLabel
        Left = 27
        Top = 2
        Width = 30
        Height = 21
        Caption = 'Zoom:'
        Layout = tlCenter
      end
      object TrackBar1: TTrackBar
        Left = 57
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
        Left = 207
        Top = 2
        AutoSize = True
        Caption = '1:1'
        ImageIndex = 4
        OnClick = ToolButton9Click
      end
      object ToolButton5: TToolButton
        Left = 233
        Top = 2
        AutoSize = True
        Caption = 'Centre'
        ImageIndex = 1
        OnClick = ToolButton5Click
      end
      object ToolButton4: TToolButton
        Left = 275
        Top = 2
        AutoSize = True
        Caption = 'N-S'
        ImageIndex = 7
        OnClick = ToolButton4Click
      end
      object ToolButton6: TToolButton
        Left = 304
        Top = 2
        AutoSize = True
        Caption = 'E-W'
        ImageIndex = 6
        OnClick = ToolButton6Click
      end
      object ToolButton3: TToolButton
        Left = 336
        Top = 2
        AutoSize = True
        Caption = 'Rotation'
        ImageIndex = 5
        Style = tbsCheck
        OnClick = ToolButton3Click
      end
      object LibrationButton: TToolButton
        Left = 387
        Top = 2
        AutoSize = True
        Caption = 'Libration'
        ImageIndex = 6
        Style = tbsCheck
        OnClick = LibrationButtonClick
      end
      object PhaseButton: TToolButton
        Left = 438
        Top = 2
        AutoSize = True
        Caption = 'Phase'
        ImageIndex = 7
        Style = tbsCheck
        OnClick = PhaseButtonClick
      end
      object ToolButton10: TToolButton
        Left = 479
        Top = 2
        AutoSize = True
        Caption = 'Voisinage'
        ImageIndex = 4
        OnClick = ToolButton10Click
      end
      object ToolButton7: TToolButton
        Left = 536
        Top = 2
        AutoSize = True
        Caption = 'Image'
        Enabled = False
        ImageIndex = 3
        OnClick = ToolButton7Click
      end
      object NewWindowButton: TToolButton
        Left = 576
        Top = 2
        AutoSize = True
        Caption = 'Window 2'
        ImageIndex = 4
        OnClick = NewWindowButtonClick
      end
      object DebugLabel: TLabel
        Left = 635
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
  object Panel2: TPanel
    Left = 144
    Top = 136
    Width = 185
    Height = 145
    Caption = 'Panel2'
    TabOrder = 4
    Visible = False
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
        Width = 229
        Height = 181
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
      object Database1: TMenuItem
        Caption = 'Database'
        OnClick = Database1Click
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object SnapShot1: TMenuItem
        Caption = 'SnapShot'
        OnClick = SnapShot1Click
      end
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
        Caption = 'Glosaire'
        OnClick = Glossaire1Click
      end
      object Encyclopedia1: TMenuItem
        Caption = 'Encyclopedia'
        OnClick = Encyclopedia1Click
      end
      object Apropos1: TMenuItem
        Caption = 'A propos'
        OnClick = Apropos1Click
      end
      object OverlayCaption1: TMenuItem
        Caption = 'Overlay Caption'
        OnClick = OverlayCaption1Click
      end
    end
  end
  object EphTimer1: TTimer
    Enabled = False
    Interval = 250
    OnTimer = EphTimer1Timer
    Left = 424
    Top = 464
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
    OnPopup = PopupMenu1Popup
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
    object OverlayCaption2: TMenuItem
      Caption = 'Overlay Caption'
      OnClick = OverlayCaption1Click
    end
    object RemoveMark1: TMenuItem
      Caption = 'RemoveMark'
      OnClick = RemoveMark1Click
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
    Interval = 200
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
  object dbm: TLiteDB
    Active = False
    DllLoaded = False
    UniCode = False
    CallBackOnly = False
    FetchMemoryLimit = 16777216
    ResultSet = 'default'
    ThreadSafe = True
    SQLiteVersion = svAuto
    Left = 80
    Top = 112
  end
end
