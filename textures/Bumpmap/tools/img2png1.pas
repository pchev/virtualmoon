unit img2png1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  FPImage,  FPWritePNG, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    binfile: file;
    imw, imh: integer;
  end; 

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var val: smallint;
    i,j: integer;
    col: TFPColor;
    Img: TFPMemoryImage;
    Writer: TFPWriterPNG;
    ms: TMemoryStream;
    fs: TFileStream;
begin
  imw:=strtoint(edit2.text);
  imh:=strtoint(edit3.text);
  Img:=TFPMemoryImage.Create(imw,imh);
  img.UsePalette:=false;
  AssignFile(binfile,edit1.text);
  reset(binfile,1);
  for i:=0 to imh-1 do begin
    for j:=0 to imw-1 do begin
      BlockRead(binfile,val,2);
      if eof(binfile) then break;
      val:=val+32768;
      col.red:=val;
      col.green:=val;
      col.blue:=val;
      col.alpha:=alphaOpaque;
      img.Colors[j,i]:=col;
    end;
  end;
  CloseFile(binfile);
  Writer:=TFPWriterPNG.create;
  Writer.Indexed:=false;
  Writer.WordSized:=true;
  Writer.UseAlpha:=false;
  ms:=TMemoryStream.Create;
  writer.ImageWrite(ms,Img);
  ms.Position:=0;
  fs:=TFileStream.Create(ChangeFileExt(edit1.text,'.png'),fmCreate);
  fs.CopyFrom(ms,ms.Size);
  img.free;
  ms.Free;
  Writer.Free;
  fs.free;
end;

initialization
  {$I img2png1.lrs}

end.

