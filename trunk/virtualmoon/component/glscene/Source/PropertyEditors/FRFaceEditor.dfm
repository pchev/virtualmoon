object RFaceEditor: TRFaceEditor
  Left = 0
  Top = 0
  Width = 305
  Height = 186
  AutoSize = True
  TabOrder = 0
  object Label1: TLabel
    Left = 0
    Top = 6
    Width = 45
    Height = 13
    Caption = 'Shininess'
  end
  object PageControl: TPageControl
    Left = 0
    Top = 56
    Width = 305
    Height = 130
    ActivePage = TSAmbient
    Images = ImageList
    MultiLine = True
    TabOrder = 0
    object TSAmbient: TTabSheet
      BorderWidth = 3
      Caption = 'Ambient'
      inline CEAmbiant: TRColorEditor
        Left = 0
        Top = 0
        Width = 289
        Height = 95
        AutoSize = True
        TabOrder = 0
      end
    end
    object TSDiffuse: TTabSheet
      BorderWidth = 3
      Caption = 'Diffuse'
      ImageIndex = 1
      inline CEDiffuse: TRColorEditor
        Left = 0
        Top = 0
        Width = 289
        Height = 95
        AutoSize = True
        TabOrder = 0
      end
    end
    object TSEmission: TTabSheet
      BorderWidth = 3
      Caption = 'Emission'
      ImageIndex = 2
      inline CEEmission: TRColorEditor
        Left = 0
        Top = 0
        Width = 289
        Height = 95
        AutoSize = True
        TabOrder = 0
      end
    end
    object TSSpecular: TTabSheet
      BorderWidth = 3
      Caption = 'Specular'
      ImageIndex = 3
      inline CESpecular: TRColorEditor
        Left = 0
        Top = 0
        Width = 289
        Height = 95
        AutoSize = True
        TabOrder = 0
      end
    end
  end
  inline TBEShininess: TRTrackBarEdit
    Left = 52
    Top = 0
    Width = 201
    Height = 21
    AutoSize = True
    TabOrder = 1
    inherited TrackBar: TTrackBar
      Max = 128
      Frequency = 16
      OnChange = TBEShininessTrackBarChange
    end
    inherited Edit: TEdit
      Left = 160
    end
  end
  object ImageList: TImageList
    Left = 264
    Top = 8
  end
end
