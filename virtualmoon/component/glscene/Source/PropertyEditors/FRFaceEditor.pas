{: FRFaceEditor<p>

   Editor fram for a TGLFaceProperties.<p>

   <b>Historique : </b><font size=-1><ul>
      <li>05/09/08 - DanB - Removed Kylix support   
      <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
      <li>19/12/06 - DaStr - TRFaceEditor.SetGLFaceProperties bugfixed - Shiness and
                             PoligonMode are now updated when FaceProperties are assigned
      <li>03/07/04 - LR  - Make change for Linux
      <li>06/02/00 - Egg - Creation
   </ul></font>
}
unit FRFaceEditor;

interface

{$i GLScene.inc}

uses
  Windows,
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Forms, VCL.ComCtrls, VCL.StdCtrls, VCL.ImgList, VCL.Controls,
{$ELSE}
  Forms, ComCtrls, StdCtrls, ImgList, Controls,
{$ENDIF}
  FRTrackBarEdit,  FRColorEditor,
  Classes, GLTexture, GLMaterial, GLState;

type
  TRFaceEditor = class(TFrame)
    PageControl: TPageControl;
    TSAmbient: TTabSheet;
    TSDiffuse: TTabSheet;
    TSEmission: TTabSheet;
    TSSpecular: TTabSheet;
    CEAmbiant: TRColorEditor;
    Label1: TLabel;
    TBEShininess: TRTrackBarEdit;
    ImageList: TImageList;
    CEDiffuse: TRColorEditor;
    CEEmission: TRColorEditor;
    CESpecular: TRColorEditor;
    procedure TBEShininessTrackBarChange(Sender: TObject);

  private
    { Déclarations privées }
    FOnChange : TNotifyEvent;
    updating : Boolean;
    FFaceProperties : TGLFaceProperties;
    procedure SetGLFaceProperties(const val : TGLFaceProperties);
    procedure OnColorChange(Sender : TObject);

  public
    { Déclarations publiques }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
	 property FaceProperties : TGLFaceProperties read FFaceProperties write SetGLFaceProperties;

  end;

implementation

{$R *.dfm}

uses
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Graphics;
{$ELSE}
  Graphics;
{$ENDIF}

constructor TRFaceEditor.Create(AOwner : TComponent);
begin
   inherited;
   FFaceProperties:=TGLFaceProperties.Create(nil);
   CEAmbiant.OnChange:=OnColorChange;
   CEDiffuse.OnChange:=OnColorChange;
   CEEmission.OnChange:=OnColorChange;
   CESpecular.OnChange:=OnColorChange;
   PageControl.DoubleBuffered:=True;
end;

destructor TRFaceEditor.Destroy;
begin
   FFaceProperties.Free;
   inherited;
end;

procedure TRFaceEditor.OnColorChange(Sender : TObject);
var
   bmp : TBitmap;
   bmpRect : TRect;

   procedure AddBitmapFor(ce : TRColorEditor);
   begin
      with bmp.Canvas do begin
         Brush.Color:=ce.PAPreview.Color;
         FillRect(bmpRect);
      end;
      ImageList.Add(bmp, nil);
   end;

begin
   if not updating then begin
      // Update imageList
      bmp:=TBitmap.Create;
      try
         bmp.Width:=16;
         bmp.Height:=16;
         bmpRect:=Rect(0, 0, 16, 16);
         ImageList.Clear;
         AddBitmapFor(CEAmbiant);
         FFaceProperties.Ambient.Color:=CEAmbiant.Color;
         AddBitmapFor(CEDiffuse);
         FFaceProperties.Diffuse.Color:=CEDiffuse.Color;
         AddBitmapFor(CEEmission);
         FFaceProperties.Emission.Color:=CEEmission.Color;
         AddBitmapFor(CESpecular);
         FFaceProperties.Specular.Color:=CESpecular.Color;
      finally
         bmp.Free;
      end;
      // Trigger onChange
      if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

procedure TRFaceEditor.TBEShininessTrackBarChange(Sender: TObject);
begin
   if not updating then begin
      TBEShininess.TrackBarChange(Sender);
      FFaceProperties.Shininess:=TBEShininess.Value;
      if Assigned(FOnChange) then FOnChange(Self);
   end;
end;

// SetGLFaceProperties
//
procedure TRFaceEditor.SetGLFaceProperties(const val : TGLFaceProperties);
begin
   updating:=True;
   try
      CEAmbiant.Color:=val.Ambient.Color;
      CEDiffuse.Color:=val.Diffuse.Color;
      CEEmission.Color:=val.Emission.Color;
      CESpecular.Color:=val.Specular.Color;
      TBEShininess.Value:=val.Shininess;
   finally
      updating:=False;
   end;
   OnColorChange(Self);
   TBEShininessTrackBarChange(Self);
end;

end.



