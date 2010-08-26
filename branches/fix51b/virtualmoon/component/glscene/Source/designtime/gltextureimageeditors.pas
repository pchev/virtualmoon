{: GLTextureImageEditors<p>

	Standard texture image editors for standard texture image classes.<p>

	<b>History : </b><font size=-1><ul>
      <li>03/07/04 - LR - Make change for Linux
      <li>24/07/03 - EG - Creation
   </ul></font>
}
unit GLTextureImageEditors;

interface

{$i GLScene.inc}

uses
  GLTexture; 


type

   // TGLBlankTIE
   //
   TGLBlankTIE = class(TGLTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TGLTextureImage) : Boolean; override;
   end;

   // TGLPersistentTIE
   //
   TGLPersistentTIE = class(TGLTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TGLTextureImage) : Boolean; override;
   end;

   // TGLPicFileTIE
   //
   TGLPicFileTIE = class(TGLTextureImageEditor)
		public
         { Public Properties }
			class function Edit(aTexImage : TGLTextureImage) : Boolean; override;
   end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

uses
  SysUtils, GLCrossPlatform; 


// ------------------
// ------------------ TGLBlankTIE ------------------
// ------------------

// Edit
//
class function TGLBlankTIE.Edit(aTexImage : TGLTextureImage) : Boolean;
var
   p : Integer;
   buf : String;
   texImage : TGLBlankImage;
begin
   texImage:=(aTexImage as TGLBlankImage);
	buf:=InputDlg('Blank Image', 'Enter size', Format('%d x %d', [texImage.Width, texImage.Height]));
   p:=Pos('x', buf);
   if p>0 then begin
      texImage.Width:=StrToIntDef(Trim(Copy(buf, 1, p-1)), 256);
      texImage.Height:=StrToIntDef(Trim(Copy(buf, p+1, MaxInt)), 256);
      Result:=True;
   end else begin
      InformationDlg('Invalid size');
      Result:=False;
   end;
end;

// ------------------
// ------------------ TGLPersistentTIE ------------------
// ------------------

// Edit
//
class function TGLPersistentTIE.Edit(aTexImage : TGLTextureImage) : Boolean;
var
   fName : String;
begin
   fName:='';
   Result:=OpenPictureDialog(fName);
   if Result then begin
   	aTexImage.LoadFromFile(fName);
		aTexImage.NotifyChange(aTexImage);
	end;
end;

// ------------------
// ------------------ TGLPicFileTIE ------------------
// ------------------

// Edit
//
class function TGLPicFileTIE.Edit(aTexImage : TGLTextureImage) : Boolean;
var
	newName : String;
   texImage : TGLPicFileImage;
begin
   { TODO : A better TGLPicFileImage.Edit is needed... }
   texImage:=(aTexImage as TGLPicFileImage);
	newName:=InputDlg('PicFile Image', 'Enter filename', texImage.PictureFileName);
	Result:=(texImage.PictureFileName<>newName);
	if Result then
		texImage.PictureFileName:=newName
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   RegisterGLTextureImageEditor(TGLBlankImage, TGLBlankTIE);
	RegisterGLTextureImageEditor(TGLPersistentImage, TGLPersistentTIE);
	RegisterGLTextureImageEditor(TGLPicFileImage, TGLPicFileTIE);

finalization

   UnRegisterGLTextureImageEditor(TGLBlankTIE);
	UnRegisterGLTextureImageEditor(TGLPersistentTIE);
	UnRegisterGLTextureImageEditor(TGLPicFileTIE);

end.

