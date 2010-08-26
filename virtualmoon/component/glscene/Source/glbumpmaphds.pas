// GLBumpmapHDS
{: Implements a HDS that automatically generates an elevation bumpmap.<p>

   The object-space elevation bumpmap can be used for dynamic terrain lighting.<p>
   A bumpmap texture is generated for each terrain tile, and placed into a TGLMaterialLibrary.

	<b>History : </b><font size=-1><ul>
      <li>13/02/07 - LIN- Thread-safe, for use with TGLAsyncHDS
                          Also takes advantage of texture-coodrinates, calculated by HeightDataSource
      <li>02/02/07 - LIN- GLBumpmapHDS is now derived from THeightDataSourceFilter.
                          HeightDataSource replaces ElevationHDS.
                          (More efficient, since it no longer has to copy and release the entire Source HDS's THeightData object.)
      <li>01/02/07 - LIN- Added 'MaxTextures' property.
                         if the MaterialLibrary.Materials.Count > MaxTextures, then unused textures are deleted.
                         Set MaxTextures=0 to disable Auto-deletes, and manage your normal-map textures manually.

                         WARNING: If you use THeightData.MaterialName, instead of THeightData.LibMaterial,
                                  then HeightData does NOT register the texture as being used.
                                  So make sure MaxTextures=0 if you use MaterialName.

      <li>25/01/07 - LIN- Replaced 'StartPreparingData' and 'GenerateBumpmap' functions.
                          Now supports a TGLBitmap with multiple tiles.
                          Now works with HeightTileFileHDS.
                          World texture coordinates for individual textures are now calculated,
                          (TGLLibMaterial.TextureOffset and TGLLibMaterial.TextureScale)
                          Bugfix: Terrain position no longer jumps when InfiniteWrap is turned off.
      <li>15/04/04 - EG - Fixed hdsNone support (Phil Scadden)
      <li>20/03/04 - EG - Works, reasonnably seamless but still quite inefficient
      <li>20/02/04 - EG - Creation
	</ul></font>
}
unit GLBumpmapHDS;

interface

uses Classes, GLHeightData, GLGraphics, VectorGeometry, GLTexture, Dialogs, Forms,
     SyncObjs;

type
   TGLBumpmapHDS = class;

   // TNewTilePreparedEvent
   //
   TNewTilePreparedEvent = procedure (Sender : TGLBumpmapHDS; heightData : THeightData;
                                      normalMapMaterial : TGLLibMaterial) of object;

	// TGLBumpmapHDS
	//
   {: An Height Data Source that generates elevation bumpmaps automatically.<p>
      The HDS must be connected to another HDS, which will provide the elevation
      data, and to a MaterialLibrary where bumpmaps will be placed. }
	 TGLBumpmapHDS = class (THeightDataSourceFilter)
	   private
	      { Private Declarations }
         //FElevationHDS : THeightDataSource;
         FBumpmapLibrary : TGLMaterialLibrary;
         FOnNewTilePrepared : TNewTilePreparedEvent;
         FBumpScale : Single;
         FSubSampling : Integer;
         FMaxTextures : integer;
         Uno:TCriticalSection;
	   protected
	      { Protected Declarations }
         procedure SetBumpmapLibrary(const val : TGLMaterialLibrary);
         procedure SetBumpScale(const val : Single);
         function  StoreBumpScale : Boolean;
         procedure SetSubSampling(const val : Integer);
         procedure Trim(MaxTextureCount:integer);
	   public
	      { Public Declarations }
	        constructor Create(AOwner: TComponent); override;
         destructor Destroy; override;
         procedure  Release(aHeightData : THeightData); override;
         procedure  Notification(AComponent: TComponent; Operation: TOperation); override;
         procedure  GenerateNormalMap(heightData : THeightData; normalMap : TGLBitmap32; scale : Single);
         procedure  TrimTextureCache(MaxTextureCount:integer);
         //procedure  TileTextureCoordinates(heightData : THeightData; TextureScale:TTexPoint; TextureOffset:TTexPoint);
         procedure PreparingData(heightData : THeightData); override;
	   published
	      { Published Declarations }
         property BumpmapLibrary : TGLMaterialLibrary read FBumpmapLibrary write SetBumpmapLibrary;
         property OnNewTilePrepared : TNewTilePreparedEvent read FOnNewTilePrepared write FOnNewTilePrepared;
         property BumpScale : Single read FBumpScale write SetBumpScale stored StoreBumpScale;
         {: Specifies the amount of subsampling for the bump texture.<p>
            This value must be a power of 2, and is used to divide the height
            tile resolution to determine the bump texture resolution (f.i.
            a tile size of 128 with a subsampling of 4 will result in textures
            of a resolution of 32x32. SubSampling won't allow texture resolution
            to get below 16x16 (minimal bumpmap resolution). }
         property SubSampling : Integer read FSubSampling write SetSubSampling default 1;
         property MaxPoolSize;
         {: If MaxTextures>0 then the Bumpmap library is trimmed down to size whenever
            the texture count is larger than MaxTextures. The oldest, unused texture is trimmed first.
            However, if you used THeightData.MaterialName, instead of THeightData.LibMaterial,
            then the THeightData component does not register the texture as being used.
            So, if you use THeightData.MaterialName then make sure MaxTextures=0.
            If MaxTextures=0 or if treads(GLAsyncHDS) are used, then the texture cache
            is NOT trimmed automatically.
            You will have to manually trim the cache from the main thread, by
            calling 'TrimTextureCache'. (GLAsyncHDS.OnIdle is a good place.)   }
         property MaxTextures :integer read FMaxTextures write FMaxTextures;
         property OnSourceDataFetched;
	 end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses SysUtils, OpenGL1x, GLUtils;

const
   cDefaultBumpScale = 0.01;

// ------------------
// ------------------ TGLBumpmapHDS ------------------
// ------------------

// Create
//
constructor TGLBumpmapHDS.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
   FBumpScale:=cDefaultBumpScale;
   FSubSampling:=1;
   Uno:=TCriticalSection.Create;
end;

// Destroy
//
destructor TGLBumpmapHDS.Destroy;
begin
   BumpmapLibrary:=nil;
	inherited Destroy;
end;

// Notification
//
procedure TGLBumpmapHDS.Notification(AComponent: TComponent; Operation: TOperation);
begin
   if Operation=opRemove then begin
      if AComponent=FBumpmapLibrary then BumpmapLibrary:=nil;
   end;
   inherited;
end;

// Release
//
procedure TGLBumpmapHDS.Release(aHeightData : THeightData);
var libMat : TGLLibMaterial;
begin
  libMat:=aHeightData.LibMaterial;
  aHeightData.MaterialName:='';
  if (FMaxTextures>0)and(assigned(LibMat))and(libMat.IsUsed=false)
    then LibMat.free;
  inherited;
end;

// TrimTextureCache
//
// This will repeatedly delete the oldest unused texture from the TGLMaterialLibrary,
// until the texture count drops to MaxTextureCount.
// DONT use this if you used THeightData.MaterialName to link your terrain textures.
// Either use with THeightData.LibMaterial, or manually delete unused Normal-Map textures.
//
procedure TGLBumpmapHDS.TrimTextureCache(MaxTextureCount:integer);  //Thread-safe Version
begin
  if assigned(self) then begin
    uno.Acquire;
      Trim(MaxTextureCount);
    uno.Release;
  end;
end;

procedure TGLBumpmapHDS.Trim(MaxTextureCount:integer); //internal use only
var matLib: TGLMaterialLibrary;
    libMat: TGLLibMaterial;
    i:integer;
    cnt:integer;
begin
  matLib:=FBumpmapLibrary;
  if matLib<>nil then begin
    cnt:=matlib.Materials.Count;
    i:=0;
    while (i<cnt)and(cnt>=MaxTextureCount) do begin
      libMat:=matlib.Materials[i];
      if libMat.IsUsed then i:=i+1
      else libmat.Free;
      cnt:=matlib.Materials.Count;
    end;
  end;
end;

// PreparingData
//
procedure TGLBumpmapHDS.PreparingData(heightData : THeightData);
var HD    : THeightData;
    libMat: TGLLibMaterial;
    bmp32 : TGLBitmap32;
    MatName:string;
begin
  if not assigned (FBumpmapLibrary) then exit;
  //--Generate Normal Map for tile--
  HD:=HeightData;
  MatName:='BumpHDS_x'+IntToStr(HD.XLeft)+'y'+IntToStr(HD.YTop)+'.'; //name contains xy coordinates of the current tile
  Uno.Acquire;
  LibMat:=FBumpmapLibrary.Materials.GetLibMaterialByName(MatName);   //Check if Tile Texture already exists
  if LibMat=nil then begin
    if (FMaxTextures>0) then begin
      if HD.Thread=nil                      //Dont trim the cache from a sub-thread;
        then TrimTextureCache(FMaxTextures) //Trim unused textures from the material library
    end;
    //Generate new NormalMap texture for this tile
    libMat:=FBumpmapLibrary.Materials.Add;
    libMat.Name:=MatName;
    //Transfer tile texture coordinates to generated texture
    libMat.TextureScale.X :=HD.TextureCoordinatesScale.S;
    libMat.TextureScale.Y :=HD.TextureCoordinatesScale.T;
    libMat.TextureOffset.X:=HD.TextureCoordinatesOffset.S;
    libMat.TextureOffset.Y:=HD.TextureCoordinatesOffset.T;
    //------------------------------------------------------
    //--Set up new Normalmap texture for the current tile--
    libMat.Material.MaterialOptions:=[moNoLighting];
    with libMat.Material.Texture do begin
      ImageClassName:=TGLBlankImage.ClassName;
      Enabled:=True;
      MinFilter:=miNearestMipmapNearest;
      MagFilter:=maLinear;   //MagFilter:=maNearest;
      TextureMode:=tmReplace;
      TextureWrap:=twNone;
      TextureFormat:=tfRGB16;
      //TextureFormat:=tfRGBA16;
      bmp32:=(Image as TGLBlankImage).GetBitmap32(GL_TEXTURE_2D);
      GenerateNormalMap(HD , bmp32, FBumpScale);
    end;
    //----------------------------------------------------
  end;
  //HD.MaterialName:=LibMat.Name;
  HD.LibMaterial:=LibMat;  //attach texture to current tile
  if Assigned(FOnNewTilePrepared) then FOnNewTilePrepared(Self,HD,libMat);
  Uno.Release;
end;

// GenerateNormalMap
//
procedure TGLBumpmapHDS.GenerateNormalMap(heightData : THeightData;
                                          normalMap : TGLBitmap32;
                                          scale : Single);
var mapSize:integer;
    HD : THeightData;
    x,y:integer;
    scaleVec:TAffineVector;
    vec   : TAffineVector;
    nmRow : PGLPixel32Array;
    px,py:integer;
begin
  HD:=HeightData;
  MapSize:=(HD.Size-1);
  mapSize:=mapSize div SubSampling;
  normalMap.Height:=mapSize;
  normalMap.Width :=mapSize;
  SetVector(ScaleVec,1,1,FBumpScale);
  for y:=0 to mapSize-1 do begin
    nmRow:=normalMap.ScanLine[mapSize-1-y];
    for x:=0 to mapSize-1 do begin
      px:=x*subsampling;
      py:=y*subsampling;
      vec:=HD.NormalAtNode(px,py,ScaleVec);
      nmRow[x].r:=round(128+127*vec[0]);      //nmRow[x].r:=0;         //Red
      nmRow[x].g:=round(128+127*vec[1]);      //nmRow[x].g:=0;         //Green
      nmRow[x].b:=round(128+127*vec[2]);      //nmRow[x].b:=0;         //Blue
      nmRow[x].a:=255;
    end;
  end;
end;

// SetBumpmapLibrary
//
procedure TGLBumpmapHDS.SetBumpmapLibrary(const val : TGLMaterialLibrary);
begin
   if val<>FBumpmapLibrary then begin
      if Assigned(FBumpmapLibrary) then
         FBumpmapLibrary.RemoveFreeNotification(Self);
      FBumpmapLibrary:=val;
      if Assigned(FBumpmapLibrary) then
         FBumpmapLibrary.FreeNotification(Self);
      MarkDirty;
   end;
end;

// SetBumpScale
//
procedure TGLBumpmapHDS.SetBumpScale(const val : Single);
begin
   if FBumpScale<>val then begin
      FBumpScale:=val;
      MarkDirty;
   end;
end;

// StoreBumpScale
//
function TGLBumpmapHDS.StoreBumpScale : Boolean;
begin
   Result:=(FBumpScale<>cDefaultBumpScale);
end;

// SetSubSampling
//
procedure TGLBumpmapHDS.SetSubSampling(const val : Integer);
begin
   if val<>FSubSampling then begin
      FSubSampling:=RoundDownToPowerOf2(val);
      if FSubSampling<1 then
         FSubSampling:=1;
      MarkDirty;
   end;
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

	// class registrations
 RegisterClass(TGLBumpmapHDS);

end.
