unit cu_print;

{$mode ObjFPC}{$H+}

interface

uses  math,
  Classes, SysUtils, Graphics, Printers;

procedure InitPage(title,font: string; margincm,linespacing:double);
procedure PrinterWriteln(theString: string; fontsize:integer; bold: boolean);
procedure ClosePage;

implementation

var
  gTopMargin, gBottomMargin, gLeftMargin, gRightMargin: double;
  gLineSpacing: double;
  H, ADPI, marginX, marginY, maxX, maxY, currentX, currentY: integer;
  PrintFont: string;


function GetPoints(x:double;dpi:integer): integer;
begin
  result:=round(x*dpi/2.54);  // cm -> points
end;

procedure PrinterWriteln(theString: string; fontsize:integer; bold: boolean);
var
  W,maxW,i,j: integer;
  buf: string;
begin
  try
  if bold then
    Printer.Canvas.Font.Style := [fsBold]
  else
    Printer.Canvas.Font.Style := [];
  if currentY>maxY then begin
    Printer.NewPage;
    currentY:=marginY;
  end;
  Printer.Canvas.Font.Size := fontsize;
  H := round(Printer.Canvas.TextHeight('X') * gLineSpacing);
  W := Printer.Canvas.TextWidth(theString);   // width of the string in points
  maxW:=maxX-currentX;                        // available width on page
  if W>maxW then begin                        // need wordwrap
    i:=max(1,round(length(theString)*maxW/W));// average number of character in one line
    repeat
      if currentY>maxY then begin             // maybe a page break is need
        Printer.NewPage;
        currentY:=marginY;
      end;
      buf:=copy(theString,1,i);               // to print on this line
      j:=LastDelimiter(' ,.;:',buf);          // search last delimiter
      if j>0 then begin
        i:=j;                                 // position of delimiter
        buf:=copy(theString,1,i);             // adjust line to delimiter
      end;
      Printer.Canvas.TextOut(currentX, currentY, buf); // print line
      Inc(currentY, H);
      delete(theString,1,i);                  // delete already printed text
    until theString='';
  end
  else begin  // can print line directly in full
    Printer.Canvas.TextOut(currentX, currentY, theString);
    Inc(currentY, H);
  end;
  except
    on E: Exception do
    begin
      Printer.Abort;
      raise;
    end;
  end;
end;

procedure InitPage(title,font: string;  margincm,linespacing:double);
begin
  try
  Printer.Title := title;
  PrintFont:=font;
  gTopMargin := margincm;   // cm
  gLeftMargin := margincm;
  gRightMargin := margincm;
  gBottomMargin := margincm;
  gLineSpacing := linespacing;
  ADPI := Printer.YDPI;
  marginY := GetPoints(gTopMargin, ADPI);
  marginX := GetPoints(gLeftMargin, ADPI);
  currentX := marginX;
  currentY := marginY;
  Printer.BeginDoc;
  maxX := Printer.PageWidth-GetPoints(gRightMargin, ADPI);
  maxY := Printer.PageHeight-GetPoints(gBottomMargin, ADPI);
  Printer.Canvas.Font.Name := PrintFont;
  Printer.Canvas.Font.Style := [];
  Printer.Canvas.Pen.Color := clBlack;
  Printer.Canvas.Pen.Width := 2;
  except
    on E: Exception do
    begin
      Printer.Abort;
      raise;
    end;
  end;
end;

procedure ClosePage;
begin
  try
  Printer.EndDoc;
  except
    on E: Exception do
    begin
      Printer.Abort;
      raise;
    end;
  end;
end;

end.

