unit cu_print;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Printers;

procedure InitPage(title,font: string; margincm,linespacing:double);
procedure PrinterWriteln(theString: string; fontsize:integer; bold: boolean);
procedure ClosePage;

implementation

var
  gTopMargin, gBottomMargin, gLeftMargin, gRightMargin: double;
  gLineSpacing: double;
  H, ADPI, marginX, marginY, maxY, currentX, currentY: integer;
  PrintFont: string;


function GetPoints(x:double;dpi:integer): integer;
begin
  result:=round(x*dpi/2.54);
end;

procedure PrinterWriteln(theString: string; fontsize:integer; bold: boolean);
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
  Printer.Canvas.TextOut(currentX, currentY, theString);
  Inc(currentY, H);
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

