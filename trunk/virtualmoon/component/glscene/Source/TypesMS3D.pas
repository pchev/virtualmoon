//
// This unit is part of the GLScene Project, http://glscene.org
//
{: TypesMS3D<p>

	Types and structures for the MS3D file format.<p>

	<b>Historique : </b><font size=-1><ul>
      <li>27/04/10 - Yar - Added more types (by TL)
      <li>24/07/09 - DaStr - TMS3DGroup.MaterialIndex is now Shortint
                              (BugtrackerID = 2353633)
      <li>16/10/08 - UweR  - Compatibility fix for Delphi 2009
      <li>06/06/07 - DaStr - Added $I GLScene.inc
                             Added GLColor to uses (BugtrackerID = 1732211)
      <li>31/08/03 - DanB  - Some code standardisation (by Philipp)
      <li>16/06/03 -  EG   - Updated headers
      <li>10/03/03 - Matt  - Creation
	</ul></font>
}
unit TypesMS3D;

interface

{$I GLScene.inc}

uses
  Classes, GLVectorTypes, GLColor;

const
  MAX_MS3D_VERTICES  = 8192;
  MAX_MS3D_TRIANGLES = 16384;
  MAX_MS3D_GROUPS    = 128;
  MAX_MS3D_MATERIALS = 128;
  MAX_MS3D_JOINTS    = 128;
  MAX_MS3D_KEYFRAMES = 5000;

type
  // typedef struct
  // {
  //     byte            flags;                              // SELECTED | HIDDEN
  //     char            name[32];                           //
  //     word            numtriangles;                       //
  //     word            triangleIndices[numtriangles];      // the groups group the triangles
  //     char            materialIndex;                      // -1 = no material
  // } ms3d_group_t;
  TMS3DGroup = class
  public
    Flags: byte;
    Name: array[0..31] of AnsiChar;
    NumTriangles: word;
    TriangleIndices: TList;
    MaterialIndex: Shortint;
    constructor Create;
    destructor Destroy; override;
  end;

  // typdef struct
  // {
  //    char    id[10];                                     // always "MS3D000000"
  //    int     version;                                    // 4
  // } ms3d_header_t;
  {$A-}

  TMS3DHeader = record
    ID: array[0..9] of AnsiChar;
    Version: integer;
  end;


  // typedef struct
  // {
  //     byte    flags;                                      // SELECTED | SELECTED2 | HIDDEN
  //     float   vertex[3];                                  //
  //     char    boneId;                                     // -1 = no bone
  //     byte    referenceCount;
  // } ms3d_vertex_t;

  TMS3DVertex = record
    Flags: byte;
    Vertex: TD3DVector;
    BoneId: AnsiChar;
    ReferenceCount: byte;
  end;

  PMS3DVertexArray = ^TMS3DVertexArray;
  TMS3DVertexArray = array[0..MAX_MS3D_VERTICES - 1] of TMS3DVertex;

  // typedef struct
  // {
  //     word    flags;                                      // SELECTED | SELECTED2 | HIDDEN
  //     word    vertexIndices[3];                           //
  //     float   vertexNormals[3][3];                        //
  //     float   s[3];                                       //
  //     float   t[3];                                       //
  //     byte    smoothingGroup;                             // 1 - 32
  //     byte    groupIndex;                                 //
  // } ms3d_triangle_t;

  TMS3DTriangle = record
    Flags: word;
    VertexIndices: array[0..2] of word;
    VertexNormals: array[0..2] of TD3DVector;
    S: array[0..2] of single;
    T: array[0..2] of single;
    SmoothingGroup: byte;  // 1 - 32
    GroupIndex: byte;
  end;

  PMS3DTriangleArray = ^TMS3DTriangleArray;
  TMS3DTriangleArray = array[0..MAX_MS3D_TRIANGLES - 1] of TMS3DTriangle;



  // typedef struct
  // {
  //     char            name[32];                           //
  //     float           ambient[4];                         //
  //     float           diffuse[4];                         //
  //     float           specular[4];                        //
  //     float           emissive[4];                        //
  //     float           shininess;                          // 0.0f - 128.0f
  //     float           transparency;                       // 0.0f - 1.0f
  //     char            mode;                               // 0, 1, 2 is unused now
  //     char            texture[128];                        // texture.bmp
  //     char            alphamap[128];                       // alpha.bmp
  // } ms3d_material_t;
  TMS3DMaterial = record
    Name: array[0..31] of AnsiChar;
    Ambient: TColorVector;
    Diffuse: TColorVector;
    Specular: TColorVector;
    Emissive: TColorVector;
    Shininess: single;
    Transparency: single;
    Mode: AnsiChar;
    Texture: array[0..127] of AnsiChar;
    Alphamap: array[0..127] of AnsiChar;
  end;


  // typedef struct
  // {
  //     float           time;                               // time in seconds
  //     float           rotation[3];                        // x, y, z angles
  // } ms3d_keyframe_rot_t;
  TMS3DKeyframeRotation = record
    Time: single;
    Rotation: TD3DVector;
  end;

  PMS3DKeyframeRotationArray = ^TMS3DKeyframeRotationArray;
  TMS3DKeyframeRotationArray = array[0..MAX_MS3D_KEYFRAMES - 1] of TMS3DKeyframeRotation;

  // typedef struct
  // {
  //     float           time;                               // time in seconds
  //     float           position[3];                        // local position
  // } ms3d_keyframe_pos_t;
  TMS3DKeyframePosition = record
    Time: single;
    Position: TD3DVector;
  end;

  PMS3DKeyframePositionArray = ^TMS3DKeyframePositionArray;
  TMS3DKeyframePositionArray = array[0..MAX_MS3D_KEYFRAMES - 1] of TMS3DKeyframePosition;

  // typedef struct
  // {
  //     byte            flags;                              // SELECTED | DIRTY
  //     char            name[32];                           //
  //     char            parentName[32];                     //
  //     float           rotation[3];                        // local reference matrix
  //     float           position[3];
  //
  //     word            numKeyFramesRot;                    //
  //     word            numKeyFramesTrans;                  //
  //
  //     ms3d_keyframe_rot_t keyFramesRot[numKeyFramesRot];      // local animation matrices
  //     ms3d_keyframe_pos_t keyFramesTrans[numKeyFramesTrans];  // local animation matrices
  // } ms3d_joint_t;

  TMS3DJointBase = record
    Flags: byte;
    Name: array[0..31] of AnsiChar;
    ParentName: array[0..31] of AnsiChar;
    Rotation: TD3DVector;
    Position: TD3DVector;
    NumKeyFramesRot: word;
    NumKeyFramesTrans: word;
  end;

  TMS3DJoint = record
    Base : TMS3DJointBase;
    KeyFramesRot: PMS3DKeyframeRotationArray;
    KeyFramesTrans: PMS3DKeyframePositionArray;
  end;

  PMS3DJointArray = ^TMS3DJointArray;
  TMS3DJointArray = array[0..MAX_MS3D_JOINTS - 1] of TMS3DJoint;



  TMS3DComment=record
      index: Integer;
      commentLength: integer;
      comment: array of AnsiChar;
  end;
  pMS3DComment=^TMS3DComment;

  TMS3DCommentList=class(TList)
  private
  protected
  public
    destructor Destroy; override;
    function NewComment: pMS3DComment;
  end;



  TMS3D_vertex_ex_t=record
    boneIds: array[0..2] of byte;
    weights: array[0..2] of byte;
    extra: cardinal;
    unknown: cardinal;
  end;
  pMS3D_vertex_ex_t=^TMS3D_vertex_ex_t;


  TVertexWeightList=class(TList)
  private
    FsubVersion: Integer;
    function GetWeight(idx: Integer): pMS3D_vertex_ex_t;
    procedure SetsubVersion(const Value: Integer);
  protected
  public
    function newWeight: pMS3D_vertex_ex_t;
    procedure Clear; override;
    destructor Destroy; override;
    property Weight[idx: Integer]: pMS3D_vertex_ex_t read GetWeight;
    property subVersion: Integer read FsubVersion write SetsubVersion;

  end;



  {$A+}

implementation

{ TMS3DGroup }

// create
//
constructor TMS3DGroup.Create;
begin
  TriangleIndices := TList.Create;
end;

// destroy
//
destructor TMS3DGroup.Destroy;
begin
  TriangleIndices.Free;
  inherited;
end;

{ TMS3DCommentList }

destructor TMS3DCommentList.destroy;
var
    i: integer;
    comment: pMS3DComment;
begin
    for i:=0 to count-1 do begin
        comment:=items[i];
        comment^.comment:=nil;
        dispose(comment);
    end;
    inherited;
end;

function TMS3DCommentList.NewComment: pMS3DComment;
begin
    new(result);
    add(result);
end;

{ TVertexWeightList }

procedure TVertexWeightList.clear;
var
    i: integer;
begin
    for i:=0 to count-1 do Dispose(Weight[i]);
    inherited;
end;

destructor TVertexWeightList.destroy;
begin
    clear;
    inherited;
end;

function TVertexWeightList.GetWeight(idx: Integer): pMS3D_vertex_ex_t;
begin
    result:=pMS3D_vertex_ex_t(items[idx]);
end;

function TVertexWeightList.newWeight: pMS3D_vertex_ex_t;
var
    p: pMS3D_vertex_ex_t;
begin
    new(p);
    add(p);
    result:=p;

end;


procedure TVertexWeightList.SetsubVersion(const Value: Integer);
begin
  FsubVersion := Value;
end;

end.
