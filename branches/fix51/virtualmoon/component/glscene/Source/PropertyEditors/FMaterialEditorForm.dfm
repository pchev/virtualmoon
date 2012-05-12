object MaterialEditorForm: TMaterialEditorForm
  Left = 143
  Top = 100
  BorderStyle = bsDialog
  Caption = 'Material Editor'
  ClientHeight = 289
  ClientWidth = 560
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 261
    Width = 71
    Height = 13
    Caption = 'Blending Mode'
  end
  object Label2: TLabel
    Left = 8
    Top = 231
    Width = 68
    Height = 13
    Caption = 'Polygon Mode'
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 8
    Width = 313
    Height = 217
    ActivePage = TSTexture
    Style = tsButtons
    TabOrder = 0
    object TSFront: TTabSheet
      Caption = 'Front'
      inline FEFront: TRFaceEditor
        Left = 0
        Top = 0
        Width = 305
        Height = 186
        AutoSize = True
        TabOrder = 0
        inherited ImageList: TImageList
          Top = 0
        end
      end
    end
    object TSBack: TTabSheet
      Caption = 'Back'
      ImageIndex = 1
      inline FEBack: TRFaceEditor
        Left = 0
        Top = 0
        Width = 305
        Height = 186
        AutoSize = True
        TabOrder = 0
      end
    end
    object TSTexture: TTabSheet
      Caption = 'Texture'
      ImageIndex = 2
      inline RTextureEdit: TRTextureEdit
        Left = 0
        Top = 0
        Width = 305
        Height = 186
        Align = alClient
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        DesignSize = (
          305
          186)
        inherited SBEditImage: TSpeedButton
          Left = 287
        end
        inherited CBImageClass: TComboBox
          Width = 213
        end
      end
    end
  end
  object GroupBox1: TGroupBox
    Left = 320
    Top = 8
    Width = 233
    Height = 241
    Caption = 'Material Preview'
    TabOrder = 1
    inline MPPreview: TRMaterialPreview
      Left = 16
      Top = 22
      Width = 202
      Height = 203
      AutoSize = True
      TabOrder = 0
      inherited GLScene1: TGLScene
        inherited World: TGLDummyCube
          inherited Cube: TGLCube
            Direction.Coordinates = {FCFAF0B1D8B35D3FFEFFFF3E00000000}
            Up.Coordinates = {D7B35DBFFFFF7F3ED7B3DDBE00000000}
          end
          inherited Teapot: TGLTeapot
            Scale.Coordinates = {00000040000000400000004000000000}
          end
        end
        inherited Light: TGLDummyCube
          Position.Coordinates = {0000000000004040000020410000803F}
          inherited LightSource: TGLLightSource
            Position.Coordinates = {0000000000004040000020410000803F}
            Specular.Color = {0000803F0000803F0000803F0000803F}
          end
          inherited FireSphere: TGLSphere
            Material.FrontProperties.Ambient.Color = {A3A2223FCDCC4C3ECDCC4C3E0000803F}
            Material.FrontProperties.Emission.Color = {D3D2523FA1A0203F000000000000803F}
          end
        end
        inherited Camera: TGLCamera
          Position.Coordinates = {0000000000000000000020410000803F}
        end
      end
    end
  end
  object BBOk: TBitBtn
    Left = 376
    Top = 256
    Width = 83
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object BBCancel: TBitBtn
    Left = 472
    Top = 256
    Width = 83
    Height = 25
    TabOrder = 3
    Kind = bkCancel
  end
  object CBBlending: TComboBox
    Left = 88
    Top = 258
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 4
    OnChange = OnMaterialChanged
  end
  object CBPolygonMode: TComboBox
    Left = 88
    Top = 227
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 5
    OnChange = OnMaterialChanged
  end
end
