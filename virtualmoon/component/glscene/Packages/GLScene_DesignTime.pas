{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GLScene_DesignTime;

interface

uses
  FXCollectionEditor, GLSceneFormDesign, GLSceneRegister, RegisterXCollection, 
  GLObjectManager, FLibMaterialPicker, FMaterialEditorForm, FRColorEditor, 
  FRFaceEditor, FRMaterialPreview, FRTextureEdit, FRTrackBarEdit, 
  FVectorEditor, FInfo, FGUILayoutEditor, FGLSceneEdit, FShaderUniformEditor, 
  FGUISkinEditor, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('GLSceneFormDesign', @GLSceneFormDesign.Register);
  RegisterUnit('GLSceneRegister', @GLSceneRegister.Register);
  RegisterUnit('RegisterXCollection', @RegisterXCollection.Register);
end;

initialization
  RegisterPackage('GLScene_DesignTime', @Register);
end.
