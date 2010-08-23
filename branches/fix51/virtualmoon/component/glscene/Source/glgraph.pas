//
// This unit is part of the GLScene Project, http://glscene.org
//
{: GLGraph<p>

	Graph plotting objects for GLScene<p>

	<b>History : </b><font size=-1><ul>
      <li>05/03/10 - DanB - More state added to TGLStateCache
      <li>06/06/07 - DaStr - Added GLColor to uses (BugtrackerID = 1732211)
      <li>30/03/07 - DaStr - Added $I GLScene.inc
      <li>14/03/07 - DaStr - Added explicit pointer dereferencing
                             (thanks Burkhard Carstens) (Bugtracker ID = 1678644)
      <li>12/09/03 - EG - DefaultHeightField now defines color
      <li>16/07/02 - EG - Fixed TGLHeightField backface polygon mode
      <li>29/01/02 - EG - Fixed TGLHeightField.BuildList when field is empty
      <li>10/01/02 - EG - Added OnGetHeight2
      <li>30/11/01 - EG - Color fix in TGLHeightField.BuildList (thx Marc Hull)
      <li>19/07/01 - EG - TGLHeightField no longer calls OnGetHeight in design mode
      <li>06/03/01 - EG - Fix in TGLHeightField.BuildList (thx Rene Lindsay)
      <li>25/02/01 - EG - Minor T&L improvement for TGLHeightField
      <li>21/02/01 - EG - Now XOpenGL based (multitexture)
      <li>29/01/01 - EG - Changed SamplingScale "Min" and "Max" default value
                          to workaround the float property default value bug.
      <li>05/11/00 - EG - Fixed "property ZSamplingScale" (thx Davide Prade)
      <li>15/07/00 - EG - Added TXYGrid
	   <li>06/07/00 - EG - Creation (TGLSamplingScale & TGLHeightField)
	</ul></font>
}
unit GLGraph;

interface

{$I GLScene.inc}

uses Classes, GLScene, VectorGeometry, GLMaterial, GLObjects, VectorLists,
     GLColor, BaseClasses, GLRenderContextInfo;

type

	// TGLSamplingScale
	//
	TGLSamplingScale = class (TGLUpdateAbleObject)
	   private
	      { Private Declarations }
         FMin : Single;
         FMax : Single;
         FOrigin : Single;
         FStep : Single;

	   protected
	      { Protected Declarations }
         procedure SetMin(const val : Single);
         procedure SetMax(const val : Single);
         procedure SetOrigin(const val : Single);
         procedure SetStep(const val : Single);

	   public
	      { Public Declarations }
         constructor Create(AOwner: TPersistent); override;
         destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;

         {: Returns the base value for step browsing.<p>
            ie. the lowest value (superior to Min) that verifies
            Frac((Origin-StepBase)/Step)=0.0, this value may be superior to Max. }
         function StepBase : Single;
         {: Maximum number of steps that can occur between Min and Max. }
         function MaxStepCount : Integer;

         function IsValid : Boolean;

         procedure SetBaseStepMaxToVars(var base, step, max : Single;
                                        samplingEnabled : Boolean = True);

	   published
	      { Published Declarations }
         property Min : Single read FMin write SetMin;
         property Max : Single read FMax write SetMax;
         property Origin : Single read FOrigin write SetOrigin;
         property Step : Single read FStep write SetStep;
	end;

   THeightFieldGetHeightEvent = procedure (const x, y : Single;
                                           var   z : Single;
                                           var   color : TColorVector;
                                           var   texPoint : TTexPoint) of object;
   THeightFieldGetHeight2Event = procedure (Sender : TObject;
                                            const x, y : Single;
                                            var   z : Single;
                                            var   color : TColorVector;
                                            var   texPoint : TTexPoint) of object;

   // THeightFieldOptions
   //
   THeightFieldOption = (hfoTextureCoordinates, hfoTwoSided);
   THeightFieldOptions = set of THeightFieldOption;

   // THeightFieldColorMode
   //
   THeightFieldColorMode = (hfcmNone, hfcmEmission, hfcmAmbient, hfcmDiffuse,
                            hfcmAmbientAndDiffuse);

	// TGLHeightField
	//
   {: Renders a sampled height-field.<p>
      HeightFields are used to materialize z=f(x, y) surfaces, you can use it to
      render anything from math formulas to statistics. Most important properties
      of an height field are its sampling scales (X & Y) that determine the extents
      and the resolution of the base grid.<p>

      The component will then invoke it OnGetHeight event to retrieve Z values for
      all of the grid points (values are retrieved only once for each point). Each
      point may have an additionnal color and texture coordinate. }
	TGLHeightField = class (TGLSceneObject)
	   private
	      { Private Declarations }
         FOnGetHeight : THeightFieldGetHeightEvent;
         FOnGetHeight2 : THeightFieldGetHeight2Event;
         FXSamplingScale : TGLSamplingScale;
         FYSamplingScale : TGLSamplingScale;
         FOptions : THeightFieldOptions;
         FTriangleCount : Integer;
         FColorMode : THeightFieldColorMode;

	   protected
	      { Protected Declarations }
         procedure SetXSamplingScale(const val : TGLSamplingScale);
         procedure SetYSamplingScale(const val : TGLSamplingScale);
         procedure SetOptions(const val : THeightFieldOptions);
         procedure SetOnGetHeight(const val : THeightFieldGetHeightEvent);
         procedure SetOnGetHeight2(const val : THeightFieldGetHeight2Event);
         procedure SetColorMode(const val : THeightFieldColorMode);

         procedure DefaultHeightField(const x, y : Single;
                                      var z : Single; var color : TColorVector;
                                      var texPoint : TTexPoint);
         procedure Height2Field(const x, y : Single;
                                var z : Single; var color : TColorVector;
                                var texPoint : TTexPoint);

	   public
	      { Public Declarations }
	      constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;

         procedure Assign(Source: TPersistent); override;
         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure NotifyChange(Sender : TObject); override;

         property TriangleCount : Integer read FTriangleCount;

	   published
	      { Published Declarations }
         property XSamplingScale : TGLSamplingScale read FXSamplingScale write SetXSamplingScale;
         property YSamplingScale : TGLSamplingScale read FYSamplingScale write SetYSamplingScale;
         {: Define if and how per vertex color is used. }
         property ColorMode : THeightFieldColorMode read FColorMode write SetColorMode default hfcmNone;
         property Options : THeightFieldOptions read FOptions write SetOptions default [hfoTwoSided];

         {: Primary event to return heights. }
         property OnGetHeight : THeightFieldGetHeightEvent read FOnGetHeight write SetOnGetHeight;
         {: Alternate this event to return heights.<p>
            This events passes an extra "Sender" parameter, it will be invoked
            only if OnGetHeight isn't defined. }
         property OnGetHeight2 : THeightFieldGetHeight2Event read FOnGetHeight2 write SetOnGetHeight2;
	end;

   // TXYZGridParts
   //
   TXYZGridPart  = (gpX, gpY, gpZ);
   TXYZGridParts = set of TXYZGridPart;

   // TXYZGridLinesStyle
   //
   {: Rendering Style for grid lines.<p>
      - glsLine : a single line is used for each grid line (from min to max),
         this provides the fastest rendering<br>
      - glsSegments : line segments are used between each node of the grid,
         this enhances perspective and quality, at the expense of computing
         power. }
   TXYZGridLinesStyle = (glsLine, glsSegments);

   // TGLXYZGrid
   //
   {: An XYZ Grid object.<p>
      Renders an XYZ grid using lines. }
   TGLXYZGrid = class(TGLLineBase)
      private
			{ Private Declarations }
         FXSamplingScale : TGLSamplingScale;
         FYSamplingScale : TGLSamplingScale;
         FZSamplingScale : TGLSamplingScale;
         FParts : TXYZGridParts;
         FLinesStyle : TXYZGridLinesStyle;

		protected
			{ Protected Declarations }
         procedure SetXSamplingScale(const val : TGLSamplingScale);
         procedure SetYSamplingScale(const val : TGLSamplingScale);
         procedure SetZSamplingScale(const val : TGLSamplingScale);
         procedure SetParts(const val : TXYZGridParts);
         procedure SetLinesStyle(const val : TXYZGridLinesStyle);
         procedure SetLinesSmoothing(const val : Boolean);

      public
			{ Public Declarations }
         constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure Assign(Source: TPersistent); override;

         procedure BuildList(var rci : TRenderContextInfo); override;
         procedure NotifyChange(Sender : TObject); override;

      published
			{ Published Declarations }
         property XSamplingScale : TGLSamplingScale read FXSamplingScale write SetXSamplingScale;
         property YSamplingScale : TGLSamplingScale read FYSamplingScale write SetYSamplingScale;
         property ZSamplingScale : TGLSamplingScale read FZSamplingScale write SetZSamplingScale;
         property Parts : TXYZGridParts read FParts write SetParts default [gpX, gpY];
         property LinesStyle : TXYZGridLinesStyle read FLinesStyle write SetLinesStyle default glsSegments;
         {: Adjusts lines smoothing (or antialiasing).<p>
            Obsolete, now maps to Antialiased property. }
         property LinesSmoothing : Boolean write SetLinesSmoothing stored False;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, XOpenGL, GLState;

// ------------------
// ------------------ TGLSamplingScale ------------------
// ------------------

// Create
//
constructor TGLSamplingScale.Create(AOwner: TPersistent);
begin
	inherited Create(AOwner);
   FStep:=0.1;
end;

// Destroy
//
destructor TGLSamplingScale.Destroy;
begin
	inherited Destroy;
end;

// Assign
//
procedure TGLSamplingScale.Assign(Source: TPersistent);
begin
   if Source is TGLSamplingScale then begin
      FMin:=TGLSamplingScale(Source).FMin;
      FMax:=TGLSamplingScale(Source).FMax;
      FOrigin:=TGLSamplingScale(Source).FOrigin;
      FStep:=TGLSamplingScale(Source).FStep;
      NotifyChange(Self);
   end else inherited Assign(Source);
end;

// SetMin
//
procedure TGLSamplingScale.SetMin(const val : Single);
begin
   FMin:=val;
   if FMax<FMin then FMax:=FMin;
   NotifyChange(Self);
end;

// SetMax
//
procedure TGLSamplingScale.SetMax(const val : Single);
begin
   FMax:=val;
   if FMin>FMax then FMin:=FMax;
   NotifyChange(Self);
end;

// SetOrigin
//
procedure TGLSamplingScale.SetOrigin(const val : Single);
begin
   FOrigin:=val;
   NotifyChange(Self);
end;

// SetStep
//
procedure TGLSamplingScale.SetStep(const val : Single);
begin
   if val>0 then
      FStep:=val
   else FStep:=1;
   NotifyChange(Self);
end;

// StepBase
//
function TGLSamplingScale.StepBase : Single;
begin
   if FOrigin<>FMin then begin
      Result:=(FOrigin-FMin)/FStep;
      if Result>=0 then
         Result:=Trunc(Result)
      else Result:=Trunc(Result)-1;
      Result:=FOrigin-FStep*Result;
   end else Result:=FMin;
end;

// MaxStepCount
//
function TGLSamplingScale.MaxStepCount : Integer;
begin
   Result:=Round(0.5+(Max-Min)/Step);
end;

// IsValid
//
function TGLSamplingScale.IsValid : Boolean;
begin
   Result:=(Max<>Min);
end;

// SetBaseStepMaxToVars
//
procedure TGLSamplingScale.SetBaseStepMaxToVars(var base, step, max : Single;
                                                samplingEnabled : Boolean = True);
begin
   step:=FStep;
   if samplingEnabled then begin
      base:=StepBase;
      max:=FMax+((FMax-base)/step)*1e-6; // add precision loss epsilon
   end else begin
      base:=FOrigin;
      max:=base;
   end;
end;

// ------------------
// ------------------ TGLHeightField ------------------
// ------------------

constructor TGLHeightField.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   ObjectStyle:=ObjectStyle+[osDoesTemperWithColorsOrFaceWinding];
   FXSamplingScale:=TGLSamplingScale.Create(Self);
   FYSamplingScale:=TGLSamplingScale.Create(Self);
   FOptions:=[hfoTwoSided];
end;

// Destroy
//
destructor TGLHeightField.Destroy;
begin
   FXSamplingScale.Free;
   FYSamplingScale.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLHeightField.Assign(Source: TPersistent);
begin
   if Source is TGLHeightField then begin
      XSamplingScale:=TGLHeightField(Source).XSamplingScale;
      YSamplingScale:=TGLHeightField(Source).YSamplingScale;
      FOnGetHeight:=TGLHeightField(Source).FOnGetHeight;
      FOptions:=TGLHeightField(Source).FOptions;
      FColorMode:=TGLHeightField(Source).FColorMode;
   end else inherited Assign(Source);
end;

// NotifyChange
//
procedure TGLHeightField.NotifyChange(Sender : TObject);
begin
   if Sender is TGLSamplingScale then
      StructureChanged;
   inherited NotifyChange(Sender);
end;

// BuildList
//
procedure TGLHeightField.BuildList(var rci : TRenderContextInfo);
type
   TRowData = packed record
      color : TColorVector;
      z : Single;
      texPoint : TTexPoint;
      normal : TAffineVector;
   end;
   TRowDataArray = array [0..Maxint shr 6] of TRowData;
   PRowData = ^TRowDataArray;
const
   cHFCMtoEnum : array [hfcmEmission..hfcmAmbientAndDiffuse] of TGLEnum =
      (GL_EMISSION, GL_AMBIENT, GL_DIFFUSE, GL_AMBIENT_AND_DIFFUSE);
   cPolygonMode : array [pmFill..pmPoints] of TGLEnum = (GL_FILL, GL_LINE, GL_POINT);

var
   nx, m, k : Integer;
   x, y, x1, y1, y2, xStep, yStep, xBase, dx, dy : Single;
   invXStep, invYStep : Single;
   row : packed array [0..2] of PRowData;
   rowTop, rowMid, rowBottom : PRowData;
   func : THeightFieldGetHeightEvent;

   procedure IssuePoint(var x, y : Single; const pt : TRowData);
   begin
      with pt do begin
         glNormal3fv(@normal);
         if ColorMode<>hfcmNone then
            glColor4fv(@color);
         if hfoTextureCoordinates in Options then
            xglTexCoord2fv(@texPoint);
         glVertex4f(x, y, z, 1);
      end;
   end;

   procedure RenderRow(pHighRow, pLowRow : PRowData);
   var
      k : Integer;
   begin
      glBegin(GL_TRIANGLE_STRIP);
      x:=xBase;
      IssuePoint(x, y1, pLowRow^[0]);
      for k:=0 to m-2 do begin
         x1:=x+xStep;
         IssuePoint(x, y2, pHighRow^[k]);
         IssuePoint(x1, y1, pLowRow^[k+1]);
         x:=x1;
      end;
      IssuePoint(x, y2, pHighRow^[m-1]);
      glEnd;
   end;

begin
   if not (XSamplingScale.IsValid and YSamplingScale.IsValid) then Exit;
   if Assigned(FOnGetHeight) and (not (csDesigning in ComponentState)) then
      func:=FOnGetHeight
   else if Assigned(FOnGetHeight2) and (not (csDesigning in ComponentState)) then
      func:=Height2Field
   else func:=DefaultHeightField;
   // allocate row cache
   nx:=(XSamplingScale.MaxStepCount+1)*SizeOf(TRowData);
   for k:=0 to 2 do begin
      GetMem(row[k], nx);
      FillChar(row[k][0], nx, 0);
   end;
   try
      // precompute grid values
      xBase:=XSamplingScale.StepBase;
      xStep:=XSamplingScale.Step; invXStep:=1/xStep;
      yStep:=YSamplingScale.Step; invYStep:=1/yStep;
      // get through the grid
      if (hfoTwoSided in Options) or (ColorMode<>hfcmNone) then begin
         rci.GLStates.PushAttrib([sttEnable]);
         // if we're not two-sided, we doesn't have to enable face-culling, it's
         // controled at the sceneviewer level
         if hfoTwoSided in Options then begin
            rci.GLStates.Disable(stCullFace);
            rci.GLStates.PolygonMode := Material.FrontProperties.PolygonMode;
         end;
         if ColorMode<>hfcmNone then begin
            rci.GLStates.Enable(stColorMaterial);
            glColorMaterial(GL_FRONT_AND_BACK, cHFCMtoEnum[ColorMode]);
            rci.GLStates.ResetGLMaterialColors;
         end;
      end;
      rowBottom:=nil; rowMid:=nil;
      nx:=0;
      y:=YSamplingScale.StepBase;
      y1:=y; y2:=y;
      while y<=YSamplingScale.Max do begin
         rowTop:=rowMid;
         rowMid:=rowBottom;
         rowBottom:=row[nx mod 3];
         x:=xBase;
         m:=0;
         while x<=XSamplingScale.Max do begin
            with rowBottom^[m] do begin
               with texPoint do begin
                  S:=x;
                  T:=y;
               end;
               func(x, y, z, color, texPoint);
            end;
            Inc(m);
            x:=x+xStep;
         end;
         if Assigned(rowMid) then begin
            for k:=0 to m-1 do begin
               if k>0 then dx:=(rowMid^[k-1].z-rowMid^[k].z)*invXStep else dx:=0;
               if k<m-1 then dx:=dx+(rowMid^[k].z-rowMid^[k+1].z)*invXStep;
               if Assigned(rowTop) then dy:=(rowTop^[k].z-rowMid^[k].z)*invYStep else dy:=0;
               if Assigned(rowBottom) then dy:=dy+(rowMid^[k].z-rowBottom^[k].z)*invYStep;
               rowMid^[k].normal:=VectorNormalize(AffineVectorMake(dx, dy, 1));
            end;
         end;
         if nx>1 then begin
            RenderRow(rowTop, rowMid);
         end;
         Inc(nx);
         y2:=y1;
         y1:=y;
         y:=y+yStep;
      end;
      for k:=0 to m-1 do begin
         if k>0 then dx:=(rowBottom^[k-1].z-rowBottom^[k].z)*invXStep else dx:=0;
         if k<m-1 then dx:=dx+(rowBottom^[k].z-rowBottom^[k+1].z)*invXStep;
         if Assigned(rowMid) then dy:=(rowMid^[k].z-rowBottom^[k].z)*invYStep else dy:=0;
         rowBottom^[k].normal:=VectorNormalize(AffineVectorMake(dx, dy, 1));
      end;
      if Assigned(rowMid) and Assigned(rowBottom) then
         RenderRow(rowMid, rowBottom);
      FTriangleCount:=2*(nx-1)*(m-1);
      if (hfoTwoSided in Options) or (ColorMode<>hfcmNone) then
         rci.GLStates.PopAttrib;
   finally
      FreeMem(row[0]);
      FreeMem(row[1]);
      FreeMem(row[2]);
   end;
end;

// SetXSamplingScale
//
procedure TGLHeightField.SetXSamplingScale(const val : TGLSamplingScale);
begin
   FXSamplingScale.Assign(val);
end;

// SetYSamplingScale
//
procedure TGLHeightField.SetYSamplingScale(const val : TGLSamplingScale);
begin
   FYSamplingScale.Assign(val);
end;

// SetOptions
//
procedure TGLHeightField.SetOptions(const val : THeightFieldOptions);
begin
   if FOptions<>val then begin
      FOptions:=val;
      StructureChanged;
   end;
end;

// SetOnGetHeight
//
procedure TGLHeightField.SetOnGetHeight(const val : THeightFieldGetHeightEvent);
begin
   FOnGetHeight:=val;
   StructureChanged;
end;

// SetOnGetHeight2
//
procedure TGLHeightField.SetOnGetHeight2(const val : THeightFieldGetHeight2Event);
begin
   FOnGetHeight2:=val;
   StructureChanged;
end;

// SetColorMode
//
procedure TGLHeightField.SetColorMode(const val : THeightFieldColorMode);
begin
   if val<>FColorMode then begin
      FColorMode:=val;
      StructureChanged;
   end;
end;

// DefaultHeightField
//
procedure TGLHeightField.DefaultHeightField(const x, y : Single;
            var z : Single; var color : TColorVector; var texPoint : TTexPoint);
begin
   z:=VectorNorm(x, y);
   z:=cos(z*12)/(2*(z*6.28+1));
   color:=clrGray80;
end;

// Height2Field
//
procedure TGLHeightField.Height2Field(const x, y : Single;
            var z : Single; var color : TColorVector; var texPoint : TTexPoint);
begin
   FOnGetHeight2(Self, x, y, z, color, texPoint);
end;

// ------------------
// ------------------ TGLXYZGrid ------------------
// ------------------

// Create
//
constructor TGLXYZGrid.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FXSamplingScale:=TGLSamplingScale.Create(Self);
   FYSamplingScale:=TGLSamplingScale.Create(Self);
   FZSamplingScale:=TGLSamplingScale.Create(Self);
   FParts:=[gpX, gpY];
   FLinesStyle:=glsSegments;
end;

// Destroy
//
destructor TGLXYZGrid.Destroy;
begin
   FXSamplingScale.Free;
   FYSamplingScale.Free;
   FZSamplingScale.Free;
	inherited Destroy;
end;

// Assign
//
procedure TGLXYZGrid.Assign(Source: TPersistent);
begin
   if Source is TGLXYZGrid then begin
      XSamplingScale:=TGLXYZGrid(Source).XSamplingScale;
      YSamplingScale:=TGLXYZGrid(Source).YSamplingScale;
      ZSamplingScale:=TGLXYZGrid(Source).ZSamplingScale;
      FParts:=TGLXYZGrid(Source).FParts;
      FLinesStyle:=TGLXYZGrid(Source).FLinesStyle;
   end;
   inherited Assign(Source);
end;

// SetXSamplingScale
//
procedure TGLXYZGrid.SetXSamplingScale(const val : TGLSamplingScale);
begin
   FXSamplingScale.Assign(val);
end;

// SetYSamplingScale
//
procedure TGLXYZGrid.SetYSamplingScale(const val : TGLSamplingScale);
begin
   FYSamplingScale.Assign(val);
end;

// SetZSamplingScale
//
procedure TGLXYZGrid.SetZSamplingScale(const val : TGLSamplingScale);
begin
   FZSamplingScale.Assign(val);
end;

// SetParts
//
procedure TGLXYZGrid.SetParts(const val : TXYZGridParts);
begin
   if FParts<>val then begin
      FParts:=val;
      StructureChanged;
   end;
end;

// SetLinesStyle
//
procedure TGLXYZGrid.SetLinesStyle(const val : TXYZGridLinesStyle);
begin
   if FLinesStyle<>val then begin
      FLinesStyle:=val;
      StructureChanged;
   end;
end;

// SetLinesSmoothing
//
procedure TGLXYZGrid.SetLinesSmoothing(const val : Boolean);
begin
   AntiAliased:=val;
end;

// NotifyChange
//
procedure TGLXYZGrid.NotifyChange(Sender : TObject);
begin
   if Sender is TGLSamplingScale then
      StructureChanged;
   inherited NotifyChange(Sender);
end;

// BuildList
//
procedure TGLXYZGrid.BuildList(var rci : TRenderContextInfo);
var
   xBase, x, xStep, xMax, yBase, y, yStep, yMax, zBase, z, zStep, zMax : Single;
begin
   SetupLineStyle(rci);
   // precache values
   XSamplingScale.SetBaseStepMaxToVars(xBase, xStep, xMax, (gpX in Parts));
   YSamplingScale.SetBaseStepMaxToVars(yBase, yStep, yMax, (gpY in Parts));
   ZSamplingScale.SetBaseStepMaxToVars(zBase, zStep, zMax, (gpZ in Parts));
   // render X parallel lines
   if gpX in Parts then begin
      y:=yBase;
      while y<=yMax do begin
         z:=zBase;
         while z<=zMax do begin
            glBegin(GL_LINE_STRIP);
            if LinesStyle=glsSegments then begin
               x:=xBase;
               while x<=xMax do begin
                  glVertex3f(x, y, z);
                  x:=x+xStep;
               end;
            end else begin
               glVertex3f(XSamplingScale.Min, y, z);
               glVertex3f(XSamplingScale.Max, y, z);
            end;
            glEnd;
            z:=z+zStep;
         end;
         y:=y+yStep;
      end;
   end;
   // render Y parallel lines
   if gpY in Parts then begin
      x:=xBase;
      while x<=xMax do begin
         z:=zBase;
         while z<=zMax do begin
            glBegin(GL_LINE_STRIP);
            if LinesStyle=glsSegments then begin
               y:=yBase;
               while y<=yMax do begin
                  glVertex3f(x, y, z);
                  y:=y+yStep;
               end;
            end else begin
               glVertex3f(x, YSamplingScale.Min, z);
               glVertex3f(x, YSamplingScale.Max, z);
            end;
            glEnd;
            z:=z+zStep;
         end;
         x:=x+xStep;
      end;
   end;
   // render Z parallel lines
   if gpZ in Parts then begin
      x:=xBase;
      while x<=xMax do begin
         y:=yBase;
         while y<=yMax do begin
            glBegin(GL_LINE_STRIP);
            if LinesStyle=glsSegments then begin
               z:=zBase;
               while z<=zMax do begin
                  glVertex3f(x, y, z);
                  z:=z+zStep;
               end;
            end else begin
               glVertex3f(x, y, ZSamplingScale.Min);
               glVertex3f(x, y, ZSamplingScale.Max);
            end;
            glEnd;
            y:=y+yStep;
         end;
         x:=x+xStep;
      end;
   end;
   RestoreLineStyle(rci);
end;

//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------
initialization
//-------------------------------------------------------------
//-------------------------------------------------------------
//-------------------------------------------------------------

   RegisterClasses([TGLHeightField, TGLXYZGrid]);

end.

