unit helpUnit;
{
Copyright (C) 2003 Patrick Chevalley

http://www.astrosurf.com/avl
pch@freesurf.ch

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
}
interface

uses HTMLLite, skylib,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  ThelpForm = class(TForm)
    Panel1: TPanel;
    html1: ThtmlLite;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure html1HotSpotClick(Sender: TObject; const SRC: String;
      var Handled: Boolean);
    procedure BitBtn3Click(Sender: TObject);
    procedure html1HotSpotCovered(Sender: TObject; const SRC: String);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  procedure HelpShow;

var
  helpForm: ThelpForm;
  docpath : string ;
  docfile : string = 'index.html';

implementation

{$R *.DFM}


procedure ThelpForm.FormShow(Sender: TObject);
begin
FormPos(helpForm,mouse.cursorpos.x,mouse.cursorpos.y);
end;

procedure HelpShow;
begin
with helpform do begin
show;
docpath:=lowercase(stringreplace(appdir,'\','/',[rfReplaceAll])+'/doc/');
html1.base:=docpath;
html1.loadfromfile(docpath+docfile);
end;
end;

procedure ThelpForm.BitBtn1Click(Sender: TObject);
begin
html1.historyindex:=html1.historyindex+1;
end;

procedure ThelpForm.BitBtn2Click(Sender: TObject);
begin
html1.historyindex:=html1.historyindex-1;
end;

procedure ThelpForm.html1HotSpotClick(Sender: TObject; const SRC: String;
  var Handled: Boolean);
var p,i : integer;
    buf:string;
begin
p:=pos('http:',src);
p:=p+pos('ftp:',src);
p:=p+pos('mailto:',src);
i:=pos('..',src);
if i=1 then  buf:=slash(appdir)+copy(src,4,99)
       else buf:=src;
if pos('.txt',buf)>0 then begin
  html1.LoadTextFile(buf);
  handled:=true;
  exit;
end;
if p>0 then begin
  ExecuteFile(src, '', '', SW_SHOWNOACTIVATE);
  handled:=True;
end else Handled := False;
end;

procedure ThelpForm.BitBtn3Click(Sender: TObject);
var ok : boolean;
begin
edit1.text:='';
ok:=html1.find(edit2.text,false);
if not ok then begin
  edit1.text:='Wraped';
  html1.caretpos:=0;
  html1.find(edit2.text,false);
end;
end;

procedure ThelpForm.html1HotSpotCovered(Sender: TObject; const SRC: String);
begin
edit1.text:=src;
end;

procedure ThelpForm.BitBtn4Click(Sender: TObject);
begin
docfile:=hp+'index.html';
if not fileexists(docpath+docfile) then docfile:=language+'_indexdoc.html';
if fileexists(docpath+docfile) then HelpShow;
end;

procedure ThelpForm.BitBtn5Click(Sender: TObject);
begin
helpform.Close;
end;

end.
