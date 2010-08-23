unit glscanlinedgraphics;

{
      $Log: glscanlinedgraphics.pas,v $
      Revision 1.1  2006/01/10 20:50:45  z0m3ie
      recheckin to make shure that all is lowercase

      Revision 1.3  2006/01/09 20:45:50  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/12/04 16:53:05  z0m3ie
      renamed everything to lowercase to get better codetools support and avoid unit finding bugs

      Revision 1.1  2005/12/01 21:24:11  z0m3ie
      *** empty log message ***

      Revision 1.2  2005/08/03 00:41:39  z0m3ie
      - added automatical generated History from CVS

}

interface

uses
     graphics, glcrossplatform, classes
     {$ifdef mswindows}
     ,windows
     {$endif};

type
     //always allocates as 32 bits
     TGLScanlinedGraphic = class (TGLGraphic)
     private
       FWidth, FHeight: integer;
     protected
       FData: pointer;

       function getScanline(y: integer): pointer; virtual;
       procedure setPixel(x, y: integer; val: integer); virtual;
       function getPixel(x, y: integer): integer; virtual;

       procedure allocateScanlineBuffer; virtual;
       procedure releaseScanlineBuffer; virtual;
       
       procedure AssignTo(Dest: TPersistent); override;

       procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;
       function GetEmpty: Boolean; override;
       function GetHeight: Integer; override;
       {$IFDEF MSWINDOWS}
       function GetPalette: HPALETTE; override;
       {$ENDIF}
       function GetTransparent: Boolean; override;
       function GetWidth: Integer; override;
       procedure SetHeight(Value: Integer); override;
       procedure SetWidth(Value: Integer); override;

     public
       property Scanline[index: integer]: pointer read getScanline;
       property Pixels[x, y: integer]: integer read GetPixel write setPixel;
       {$ifndef FPC}
       procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle;
         APalette: HPALETTE); override;
       procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle;
         var APalette: HPALETTE); override;
       {$endif}
     end;

                           
implementation

uses
  glgraphics, sysutils;

{ TGLScanlinedGraphic }

procedure TGLScanlinedGraphic.allocateScanlineBuffer;
begin
     ReallocMem(FData, FHeight * FWidth * 4);
end;

procedure TGLScanlinedGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
var
x, y: integer;
begin
//  raise Exception.create('draw error'); why ??
  for y:= 0 to Height -1 do
    for x:= 0 to width -1 do begin
      aCanvas.pixels[rect.left+x, rect.top+y]:= pixels[x, y];
    end;
end;

function TGLScanlinedGraphic.GetEmpty: Boolean;
begin
     result:= FData = nil;
end;

function TGLScanlinedGraphic.GetHeight: Integer;
begin
     result:= FHeight;
end;

{$IFDEF MSWINDOWS}
function TGLScanlinedGraphic.GetPalette: HPALETTE;
begin
     result:= 0;
     assert(false, 'getPalette not supported');
end;
{$ENDIF}

function TGLScanlinedGraphic.getPixel(x, y: integer): integer;
type
     TIntArray = array of integer;
     PIntArray = ^TIntArray;
begin
  result:= PIntArray(getScanline(y))^[x];
end;

procedure TGLScanlinedGraphic.setPixel(x, y: integer; val: integer);
type
     TIntArray = array of integer;
     PIntArray = ^TIntArray;
begin
  PIntArray(getScanline(y))^[x]:= val;
end;

function TGLScanlinedGraphic.getScanline(y: integer): pointer;
begin
  result:= pointer(PtrInt(FData) + y * Width * 4);
end;

function TGLScanlinedGraphic.GetTransparent: Boolean;
begin
     result:= false;
end;

function TGLScanlinedGraphic.GetWidth: Integer;
begin
     result:= FWidth;
end;

{$ifndef FPC}
procedure TGLScanlinedGraphic.LoadFromClipboardFormat(AFormat: Word;
  AData: THandle; APalette: HPALETTE);
begin
     assert(false, 'loadFromClipboardFormat not supported');
     inherited;
end;

procedure TGLScanlinedGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  assert(false, 'saveToClipboardFormat not supported');
  inherited;
end;
{$endif}

procedure TGLScanlinedGraphic.releaseScanlineBuffer;
begin
     freeMem(FData);
     FData:= nil;
end;


procedure TGLScanlinedGraphic.SetHeight(Value: Integer);
begin
  FHeight:= Value;
  allocateScanlineBuffer
end;

procedure TGLScanlinedGraphic.SetWidth(Value: Integer);
begin
  FWidth:= Value;
  allocateScanlineBuffer
end;

procedure TGLScanlinedGraphic.AssignTo(Dest: TPersistent);
var
bp: TGLBitmap32;
i: integer;
begin
  if dest.inheritsFrom(TGLBitmap32) then begin
    bp:= TGLBitmap32(dest);
    bp.Width:= Width;
    bp.Height:= height;
    for i:= 0 to Height -1 do
      move(scanline[i]^, bp.ScanLine[i]^, width*4);
  end else inherited;
end;

end.
