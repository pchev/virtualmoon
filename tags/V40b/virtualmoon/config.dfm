object Form2: TForm2
  Left = 450
  Top = 108
  Caption = 'Configuration'
  ClientHeight = 461
  ClientWidth = 369
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 138
    Top = 424
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 1
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 0
    Width = 345
    Height = 417
    ActivePage = TabSheet1
    MultiLine = True
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      object Bevel8: TBevel
        Left = 16
        Top = 160
        Width = 305
        Height = 97
        Shape = bsFrame
      end
      object Bevel7: TBevel
        Left = 16
        Top = 16
        Width = 305
        Height = 129
        Shape = bsFrame
      end
      object Label4: TLabel
        Left = 24
        Top = 280
        Width = 42
        Height = 13
        Caption = 'Langue :'
      end
      object Label1: TLabel
        Left = 32
        Top = 72
        Width = 38
        Height = 13
        Caption = 'Latitude'
      end
      object Label2: TLabel
        Left = 144
        Top = 72
        Width = 47
        Height = 13
        Caption = 'Longitude'
      end
      object Label3: TLabel
        Left = 32
        Top = 220
        Width = 60
        Height = 13
        Caption = 'Zone horaire'
      end
      object Label16: TLabel
        Left = 32
        Top = 12
        Width = 60
        Height = 13
        Caption = 'Observatoire'
      end
      object Label29: TLabel
        Left = 192
        Top = 220
        Width = 64
        Height = 13
        Caption = 'Positive East '
      end
      object Label23: TLabel
        Left = 32
        Top = 156
        Width = 63
        Height = 13
        Caption = 'Date / Heure'
      end
      object ComboBox3: TComboBox
        Left = 80
        Top = 276
        Width = 233
        Height = 21
        ItemHeight = 13
        TabOrder = 6
        Text = 'ComboBox3'
        OnChange = ComboBox3Change
      end
      object CheckBox3: TCheckBox
        Left = 32
        Top = 40
        Width = 225
        Height = 17
        Caption = 'Geocentrique'
        TabOrder = 0
        OnClick = CheckBox3Click
      end
      object Edit1: TEdit
        Left = 32
        Top = 88
        Width = 49
        Height = 21
        TabOrder = 1
        Text = 'Edit1'
      end
      object Edit2: TEdit
        Left = 144
        Top = 88
        Width = 49
        Height = 21
        TabOrder = 3
        Text = 'Edit2'
      end
      object ComboBox1: TComboBox
        Left = 88
        Top = 88
        Width = 49
        Height = 21
        ItemHeight = 13
        TabOrder = 2
        Text = 'N'
        Items.Strings = (
          'N'
          'S')
      end
      object ComboBox2: TComboBox
        Left = 200
        Top = 88
        Width = 49
        Height = 21
        ItemHeight = 13
        TabOrder = 4
        Text = 'E'
        Items.Strings = (
          'E'
          'W')
      end
      object Edit3: TEdit
        Left = 136
        Top = 216
        Width = 41
        Height = 21
        TabOrder = 5
        Text = 'Edit3'
      end
      object CheckBox16: TCheckBox
        Left = 32
        Top = 184
        Width = 281
        Height = 17
        Caption = 'Use Computer Date and Time Zone'
        TabOrder = 7
        OnClick = CheckBox16Click
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Database'
      ImageIndex = 7
      object Label31: TLabel
        Left = 16
        Top = 232
        Width = 74
        Height = 13
        Caption = 'User Database '
      end
      object GroupBox1: TGroupBox
        Left = 16
        Top = 18
        Width = 305
        Height = 199
        Caption = 'Database'
        TabOrder = 0
        object CheckBox19: TCheckBox
          Left = 24
          Top = 20
          Width = 257
          Height = 17
          Caption = 'Named Formation'
          Checked = True
          State = cbChecked
          TabOrder = 0
          OnClick = CheckBox19Click
        end
        object CheckBox20: TCheckBox
          Left = 24
          Top = 49
          Width = 257
          Height = 17
          Caption = 'Satellites Formation'
          TabOrder = 1
        end
        object CheckBox21: TCheckBox
          Left = 24
          Top = 78
          Width = 257
          Height = 17
          Caption = 'Far Side Named Formation'
          TabOrder = 2
        end
        object CheckBox22: TCheckBox
          Left = 24
          Top = 107
          Width = 257
          Height = 17
          Caption = 'Far Side Satellites Formation'
          TabOrder = 3
        end
        object CheckBox23: TCheckBox
          Left = 24
          Top = 136
          Width = 257
          Height = 17
          Caption = 'Landing Site'
          TabOrder = 4
        end
        object CheckBox24: TCheckBox
          Left = 24
          Top = 166
          Width = 257
          Height = 17
          Caption = 'Pyroclastic deposits'
          TabOrder = 5
        end
      end
      object CheckListBox1: TCheckListBox
        Left = 16
        Top = 256
        Width = 305
        Height = 97
        BevelInner = bvSpace
        BevelOuter = bvNone
        BevelKind = bkFlat
        BevelWidth = 4
        ItemHeight = 13
        TabOrder = 1
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Affichage'
      ImageIndex = 3
      object Bevel9: TBevel
        Left = 16
        Top = 144
        Width = 305
        Height = 81
        Shape = bsFrame
      end
      object Bevel3: TBevel
        Left = 16
        Top = 232
        Width = 305
        Height = 132
        Shape = bsFrame
      end
      object Bevel2: TBevel
        Left = 16
        Top = 78
        Width = 305
        Height = 59
        Shape = bsFrame
      end
      object Bevel1: TBevel
        Left = 16
        Top = 16
        Width = 305
        Height = 57
        Shape = bsFrame
      end
      object Shape1: TShape
        Left = 24
        Top = 108
        Width = 20
        Height = 20
        OnMouseUp = Shape1MouseUp
      end
      object Label6: TLabel
        Left = 51
        Top = 112
        Width = 60
        Height = 13
        Caption = 'Identification'
      end
      object Shape2: TShape
        Left = 128
        Top = 108
        Width = 20
        Height = 20
        OnMouseUp = Shape1MouseUp
      end
      object Label7: TLabel
        Left = 155
        Top = 112
        Width = 36
        Height = 13
        Caption = 'Marque'
      end
      object Label5: TLabel
        Left = 32
        Top = 80
        Width = 36
        Height = 13
        Caption = 'Couleur'
      end
      object Shape3: TShape
        Left = 224
        Top = 108
        Width = 20
        Height = 20
        OnMouseUp = Shape1MouseUp
      end
      object Label17: TLabel
        Left = 251
        Top = 112
        Width = 26
        Height = 13
        Caption = 'Label'
      end
      object Label18: TLabel
        Left = 24
        Top = 338
        Width = 81
        Height = 13
        Caption = 'Densit'#233' de labels'
      end
      object Label19: TLabel
        Left = 24
        Top = 314
        Width = 75
        Height = 13
        Caption = 'Taille des labels'
      end
      object Label33: TLabel
        Left = 24
        Top = 200
        Width = 88
        Height = 13
        Caption = 'Taille des marques'
      end
      object CheckBox4: TCheckBox
        Left = 24
        Top = 8
        Width = 289
        Height = 17
        Caption = 'Affichage OpenGL'
        Color = clBtnFace
        ParentColor = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = CheckBox4Click
      end
      object CheckBox2: TCheckBox
        Left = 168
        Top = 40
        Width = 145
        Height = 17
        Caption = 'Affiche la libration'
        TabOrder = 2
      end
      object CheckBox1: TCheckBox
        Left = 24
        Top = 40
        Width = 137
        Height = 17
        Caption = 'Afficher la phase'
        TabOrder = 1
      end
      object CheckBox6: TCheckBox
        Left = 24
        Top = 173
        Width = 129
        Height = 17
        Caption = 'Affiche la marque'
        TabOrder = 5
      end
      object CheckBox5: TCheckBox
        Left = 24
        Top = 245
        Width = 129
        Height = 17
        Caption = 'Affiche les labels'
        TabOrder = 4
      end
      object CheckBox14: TCheckBox
        Left = 24
        Top = 152
        Width = 289
        Height = 17
        Caption = 'Marque la libration maximum'
        TabOrder = 3
      end
      object TrackBar2: TTrackBar
        Left = 160
        Top = 336
        Width = 150
        Height = 17
        Min = -1000
        PageSize = 100
        Frequency = 100
        Position = -100
        TabOrder = 7
        ThumbLength = 12
      end
      object TrackBar3: TTrackBar
        Left = 160
        Top = 312
        Width = 150
        Height = 17
        LineSize = 5
        Max = 200
        Min = 50
        PageSize = 25
        Frequency = 25
        Position = 100
        TabOrder = 6
        ThumbLength = 12
      end
      object CheckBox17: TCheckBox
        Left = 24
        Top = 266
        Width = 249
        Height = 17
        Caption = 'Label centr'#233' sur la formation'
        TabOrder = 8
      end
      object CheckBox18: TCheckBox
        Left = 24
        Top = 288
        Width = 209
        Height = 17
        Caption = 'Label minimum'
        TabOrder = 9
      end
      object TrackBar4: TTrackBar
        Left = 160
        Top = 198
        Width = 150
        Height = 17
        LineSize = 5
        Max = 8
        Min = 1
        PageSize = 25
        Position = 5
        TabOrder = 10
        ThumbLength = 12
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Textures'
      ImageIndex = 4
      object RadioGroup1: TRadioGroup
        Left = 16
        Top = 48
        Width = 305
        Height = 57
        Caption = 'R'#233'solution des textures'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Entiere'
          'Demi'
          'Quart')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
      end
      object RadioGroup2: TRadioGroup
        Left = 16
        Top = 192
        Width = 305
        Height = 105
        Caption = 'Choix de la texture'
        ItemIndex = 0
        Items.Strings = (
          'A'#233'rographe'
          'G'#233'ologique'
          'Lopam')
        TabOrder = 1
        OnClick = RadioGroup2Click
      end
      object CheckBox10: TCheckBox
        Left = 16
        Top = 16
        Width = 297
        Height = 17
        Caption = 'Interpolation des textures'
        TabOrder = 2
      end
      object RadioGroup3: TRadioGroup
        Left = 16
        Top = 120
        Width = 305
        Height = 57
        Caption = 'R'#233'solution des textures face cach'#233'e'
        Columns = 3
        ItemIndex = 0
        Items.Strings = (
          'Entiere'
          'Demi'
          'Quart')
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'Overlay'
      ImageIndex = 6
      object nooverlay: TLabel
        Left = 40
        Top = 24
        Width = 257
        Height = 105
        AutoSize = False
        Caption = 
          'Your graphic card do not support to display more than one textur' +
          'e for the same object. Thus it is not possible to use this funct' +
          'ion on this computer. Be sure that hardware acceleration is acti' +
          'vated or change the graphic card. The require function is GL_ARB' +
          '_MULTITEXTURE '
        WordWrap = True
      end
      object Label34: TLabel
        Left = 40
        Top = 152
        Width = 211
        Height = 13
        Cursor = crHandPoint
        Caption = 'Look here for a list of supported graphic card'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsUnderline]
        ParentFont = False
        OnClick = Label34Click
      end
      object OverlayPanel: TPanel
        Left = 8
        Top = 8
        Width = 313
        Height = 337
        BevelOuter = bvNone
        TabOrder = 0
        object Image1: TImage
          Left = 33
          Top = 93
          Width = 280
          Height = 140
          Stretch = True
        end
        object Label30: TLabel
          Left = 32
          Top = 44
          Width = 42
          Height = 13
          Caption = 'Overlay :'
        end
        object Label32: TLabel
          Left = 32
          Top = 268
          Width = 68
          Height = 13
          Caption = 'Transparency:'
        end
        object CheckBox11: TCheckBox
          Left = 32
          Top = 8
          Width = 273
          Height = 17
          Caption = 'Show overlay'
          TabOrder = 0
        end
        object ComboBox5: TComboBox
          Left = 32
          Top = 64
          Width = 145
          Height = 21
          ItemHeight = 13
          Sorted = True
          TabOrder = 1
          OnChange = ComboBox5Change
        end
        object TrackBar5: TTrackBar
          Left = 112
          Top = 264
          Width = 161
          Height = 33
          LineSize = 5
          Max = 200
          PageSize = 10
          Frequency = 20
          TabOrder = 2
          OnChange = TrackBar5Change
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Images'
      ImageIndex = 1
      object Label8: TLabel
        Left = 8
        Top = 24
        Width = 141
        Height = 13
        Caption = 'Nombre de fen'#234'tre d'#39'images : '
      end
      object Label9: TLabel
        Left = 8
        Top = 72
        Width = 104
        Height = 13
        Caption = 'R'#233'pertoires d'#39'images :'
      end
      object Label20: TLabel
        Left = 48
        Top = 280
        Width = 32
        Height = 13
        Caption = 'Pr'#233'fixe'
      end
      object Label21: TLabel
        Left = 184
        Top = 280
        Width = 26
        Height = 13
        Caption = 'Suffix'
      end
      object Label22: TLabel
        Left = 8
        Top = 300
        Width = 22
        Height = 13
        Caption = 'R'#252'kl'
      end
      object UpDown1: TUpDown
        Left = 185
        Top = 20
        Width = 15
        Height = 21
        Associate = numwin
        Min = 1
        Max = 10
        Position = 2
        TabOrder = 4
        Wrap = True
      end
      object numwin: TLongEdit
        Left = 160
        Top = 20
        Width = 25
        Height = 21
        Hint = '0..10'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        Value = 2
        MaxValue = 10
      end
      object StringGrid1: TStringGrid
        Left = 8
        Top = 88
        Width = 315
        Height = 193
        ColCount = 2
        DefaultRowHeight = 18
        FixedCols = 0
        RowCount = 11
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
        ScrollBars = ssVertical
        TabOrder = 5
        OnSelectCell = StringGrid1SelectCell
        ColWidths = (
          64
          224)
      end
      object CheckBox12: TCheckBox
        Left = 8
        Top = 0
        Width = 145
        Height = 17
        Caption = 'Afficheur externe'
        TabOrder = 0
        OnClick = CheckBox12Click
      end
      object Edit4: TEdit
        Left = 160
        Top = -2
        Width = 121
        Height = 21
        Enabled = False
        TabOrder = 1
        Text = 'mspaint.exe'
      end
      object Button3: TButton
        Left = 280
        Top = -2
        Width = 21
        Height = 21
        Caption = '...'
        Enabled = False
        TabOrder = 2
        OnClick = Button3Click
      end
      object CheckBox15: TCheckBox
        Left = 8
        Top = 48
        Width = 297
        Height = 17
        Caption = 'Lien direct sur l'#39'image pour l'#39'atlas Lunar Orbiter'
        TabOrder = 6
      end
      object ruklprefix: TEdit
        Left = 48
        Top = 296
        Width = 121
        Height = 21
        TabOrder = 7
        Text = 'D:\rukl\mo'
      end
      object ruklsuffix: TEdit
        Left = 184
        Top = 296
        Width = 121
        Height = 21
        TabOrder = 8
        Text = '.jpg'
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Eyepieces'
      ImageIndex = 5
      object Label24: TLabel
        Left = 8
        Top = 8
        Width = 108
        Height = 13
        Caption = 'Telescope focal length'
      end
      object Label25: TLabel
        Left = 8
        Top = 34
        Width = 102
        Height = 13
        Caption = 'Eyepiece focal length'
      end
      object Label26: TLabel
        Left = 8
        Top = 84
        Width = 153
        Height = 13
        Caption = 'Eyepiece apparent field of vision'
      end
      object Label27: TLabel
        Left = 220
        Top = 84
        Width = 12
        Height = 13
        Caption = '=>'
      end
      object Label28: TLabel
        Left = 8
        Top = 58
        Width = 30
        Height = 13
        Caption = 'Power'
      end
      object StringGrid2: TStringGrid
        Left = 8
        Top = 112
        Width = 315
        Height = 217
        ColCount = 4
        DefaultColWidth = 30
        DefaultRowHeight = 18
        FixedCols = 0
        RowCount = 11
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goEditing, goAlwaysShowEditor]
        ScrollBars = ssVertical
        TabOrder = 0
        OnDrawCell = StringGrid2DrawCell
        OnMouseUp = StringGrid2MouseUp
        OnSelectCell = StringGrid2SelectCell
        ColWidths = (
          179
          64
          30
          30)
      end
      object Edit6: TEdit
        Left = 168
        Top = 4
        Width = 41
        Height = 21
        TabOrder = 1
        Text = '2000'
      end
      object Edit7: TEdit
        Left = 168
        Top = 30
        Width = 41
        Height = 21
        TabOrder = 2
        Text = '10'
      end
      object Edit8: TEdit
        Left = 168
        Top = 80
        Width = 41
        Height = 21
        TabOrder = 3
        Text = '50'
      end
      object Edit9: TEdit
        Left = 248
        Top = 80
        Width = 49
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 4
      end
      object Button6: TButton
        Left = 240
        Top = 22
        Width = 75
        Height = 25
        Caption = 'Compute'
        TabOrder = 5
        OnClick = Button6Click
      end
      object Edit10: TEdit
        Left = 168
        Top = 54
        Width = 41
        Height = 21
        Color = clBtnFace
        ReadOnly = True
        TabOrder = 6
      end
    end
    object Impression: TTabSheet
      Caption = 'Impression'
      ImageIndex = 2
      object Bevel6: TBevel
        Left = 8
        Top = 48
        Width = 313
        Height = 81
        Shape = bsFrame
      end
      object Bevel4: TBevel
        Left = 8
        Top = 232
        Width = 313
        Height = 65
        Shape = bsFrame
      end
      object Label12: TLabel
        Left = 16
        Top = 64
        Width = 33
        Height = 13
        Caption = 'Marge '
      end
      object Label13: TLabel
        Left = 248
        Top = 64
        Width = 47
        Height = 13
        Caption = 'millim'#232'tres'
      end
      object Label14: TLabel
        Left = 16
        Top = 100
        Width = 146
        Height = 13
        Caption = 'Largeur du texte de description'
      end
      object Bevel5: TBevel
        Left = 8
        Top = 140
        Width = 313
        Height = 81
        Shape = bsFrame
      end
      object Label15: TLabel
        Left = 16
        Top = 22
        Width = 51
        Height = 13
        Caption = 'Imprimante'
      end
      object Label10: TLabel
        Left = 24
        Top = 284
        Width = 141
        Height = 13
        Caption = 'Taille des images enregistr'#233'es'
        Visible = False
      end
      object Label11: TLabel
        Left = 16
        Top = 228
        Width = 125
        Height = 13
        Caption = 'Enregistrement des cartes '
      end
      object LongEdit1: TLongEdit
        Left = 192
        Top = 59
        Width = 49
        Height = 22
        Hint = '0..50'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Value = 0
        MaxValue = 50
      end
      object CheckBox8: TCheckBox
        Left = 24
        Top = 174
        Width = 233
        Height = 17
        Caption = 'Imprime les '#233'ph'#233'm'#233'rides'
        TabOrder = 4
      end
      object CheckBox9: TCheckBox
        Left = 24
        Top = 198
        Width = 233
        Height = 17
        Caption = 'Imprime la description '
        TabOrder = 5
      end
      object TrackBar1: TTrackBar
        Left = 184
        Top = 95
        Width = 127
        Height = 30
        Max = 1200
        Min = 200
        Frequency = 125
        Position = 700
        TabOrder = 2
      end
      object Button2: TButton
        Left = 112
        Top = 16
        Width = 90
        Height = 25
        Caption = 'Configuration'
        TabOrder = 0
        OnClick = Button2Click
      end
      object CheckBox13: TCheckBox
        Left = 24
        Top = 150
        Width = 233
        Height = 17
        Caption = 'Imprime la carte'
        TabOrder = 3
      end
      object CheckBox7: TCheckBox
        Left = 24
        Top = 256
        Width = 145
        Height = 17
        Caption = 'Fond blanc'
        TabOrder = 6
      end
      object ComboBox4: TComboBox
        Left = 192
        Top = 280
        Width = 113
        Height = 21
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 7
        Text = 'Comme '#224' l'#39#233'cran'
        Visible = False
        Items.Strings = (
          'Comme '#224' l'#39#233'cran'
          '256'
          '512'
          '1024'
          '2048')
      end
    end
  end
  object Button4: TButton
    Left = 266
    Top = 424
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ColorDialog1: TColorDialog
    Left = 16
    Top = 424
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '.exe'
    Filter = 'Image software|*.exe'
    Left = 56
    Top = 424
  end
end
