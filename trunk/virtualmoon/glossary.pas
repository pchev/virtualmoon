unit glossary;

{$MODE Delphi}
{$H+}
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

uses
{$ifdef mswindows}
  Windows,
{$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, LResources, IpHtml;

type

  { TGloss }

  TGloss = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    Gloss1: TIpHtmlPanel;
    TreeView1: TTreeView;
    Button2: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Button3: TButton;
    procedure Gloss1HotClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Changing(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure Button3Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    Procedure InitGlossary;
    procedure SelectTree(nom:string);
    procedure SelectNode(id:integer);
    Function SearchGloss(w:string):boolean;
    Procedure ShowGloss(txt:string);
  end;

var
  Gloss: TGloss;
  treeviewinitializing:boolean;

implementation

uses u_constant, u_util, mlb2;


var dbgloss : TMlb2;
    lastpos : integer;
    lastnode : TTreenode;

function dcopy(s:string; f,l:integer):string;
var bf,bl: integer;
begin
{bf:=CharToByteIndex(s,f);
if bf=0 then begin result:=''; exit; end;
result:=copy(s,bf,maxint);
bl:=CharToByteLen(result,l);
result:=copy(resut,1,bl);}
result:=copy(s,f,l);
end;

procedure TGloss.SelectTree(nom:string);
var i,j,n,m:integer;
    l:string;
begin
l:=dcopy(nom,1,1);
n:=-1;  m:=-1; j:=0;
for i:=0 to Treeview1.Items.Count-1 do begin
  if Treeview1.Items[i].Text>=l then begin
     n:=i;
     break;
  end;
end;
if n>=0 then for j:=0 to Treeview1.Items[n].Count-1 do begin
  if Treeview1.Items[n].Items[j].Text=nom then begin
     m:=j;
     break;
  end;
end;
if m>=0 then begin
   Treeview1.Items[n].Expand(false);
   Treeview1.Items[n].Items[j].Selected:=true;
   Treeview1.TopItem:=Treeview1.Items[n].Items[j];
end;
end;

procedure TGloss.SelectNode(id:integer);
var i,n,c,f:integer;
begin
c:=0;
n:=-1;
for i:=0 to Treeview1.Items.Count-1 do begin
  f:=Treeview1.Items[i].Count;
  if f=0 then begin
    c:=c+1;
    if c>=id then begin
      n:=i;
      break;
    end;
  end;
end;
if n>=0 then begin
   Treeview1.Items[n].Expand(false);
   Treeview1.Items[n].Selected:=true;
   Treeview1.TopItem:=Treeview1.Items[n];
end;
end;

function isupper(c:string):boolean;
begin
result:=(c=' ')or((c>='A')and(c<='Z'));
end;

Procedure TGloss.ShowGloss(txt:string);
var i,p:integer;
    c,buf,lien,lientxt:string;
    stop:boolean;
    NewHTML: TIpHtml;
    s: TStringStream;
begin
txt:=UTF8Encode(txt);
if panel1.visible and (length(txt)>0) then begin
  txt:=txt+'  ';
  buf:='<html> <body>';
  i:=1;
  p:=length(txt);
  repeat
    c:=dcopy(txt,i,1);
    if isupper(c) and (c<>' ') then begin
       lien:='';
       lientxt:='';
       repeat
         stop:=false;
         c:=dcopy(txt,i,1);
         if isupper(c) then begin
               lien:=lien+c;
               lientxt:=lientxt+c;
            end else if (c<>',') and (length(lien)>1) and isupper(dcopy(txt,i+1,1)) and isupper(dcopy(txt,i+2,1)) and isupper(dcopy(txt,i+3,1)) then begin
               lientxt:=lientxt+c;
            end else stop:=true;
         inc(i);
       until stop or(i>p);
       if i<=p then dec(i);
       dec(i);
       if dcopy(lien,length(lien),1)=' ' then begin
          lien:=dcopy(lien,1,length(lien)-1);
          dec(i);
       end;
       if length(lien)>1 then begin
            if dcopy(lientxt,length(lientxt),1)=' ' then begin
               lientxt:=dcopy(lien,1,length(lientxt)-1);
               c:=' ';
            end else c:='';
            buf:=buf+' <A HREF="#'+lien+'">'+lientxt+'</A>'+c;
          end
          else buf:=buf+lien;
    end else buf:=buf+c;
    inc(i);
  until i>p;
  buf:=buf+'</body></html>';
end else buf:='<html> <body>'+txt+'</body></html>';
try
  s:=TStringStream.Create(buf);
  try
    NewHTML:=TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
    NewHTML.LoadFromStream(s);
  finally
    s.Free;
  end;
  Gloss1.SetHtml(NewHTML);
except
end;
end;

Function TGloss.SearchGloss(w:string):boolean;
begin
  dbgloss.Gofirst;
  result:=dbgloss.MatchData('GLO_WORD', '=', w);
  if not result then result:=dbgloss.SeekData('GLO_WORD', '=', w);
  if result then begin
     ShowGloss(dbgloss.GetData('GLO_TEXT'));
  end;
end;

procedure TGloss.Label1Click(Sender: TObject);
var i,n:integer;
    l:string;
begin
with sender as TLabel do l:=caption;
n:=-1;
for i:=0 to Treeview1.Items.Count-1 do begin
  if Treeview1.Items[i].Text>=l then begin
     n:=i;
     break;
  end;
end;
if n>=0 then begin
 Treeview1.Items[n].Expand(false);
 Treeview1.Items[n].Items[0].Selected:=true;
 Treeview1.TopItem:=Treeview1.Items[n];
 SearchGloss(Treeview1.Items[n].Items[0].Text);
end;
lastpos:=dbgloss.GetPosition;
lastnode:=Treeview1.Selected;
end;

Procedure TGloss.InitGlossary;
var ok:boolean;
    fn:string;
    a,c:string;
    node: TTreeNode;
begin
fn:=Slash(appdir)+Slash('Database')+'glossary_'+language+'.csv';
if not fileexists(fn) then fn:=Slash(appdir)+Slash('database')+'glossary_UK.csv';
dbgloss.LoadFromCSVFile(fn);
ok:=dbgloss.GoFirst;
lastpos:=dbgloss.GetPosition;
ShowGloss(dbgloss.GetData('GLO_TEXT'));
treeviewinitializing:=true;
fn:=dbgloss.GetData('GLO_WORD');
c:=copy(fn,1,1);
panel1.Visible:=isupper(c);
Treeview1.Items.BeginUpdate;
Treeview1.Items.Clear;
a:='  '; node:=nil;
while ok do begin
   fn:=dbgloss.GetData('GLO_WORD');
   c:=dcopy(fn,1,1);
   if c<>a then begin
      a:=c;
      node:= Treeview1.Items.Add(nil,a);
   end;
   Treeview1.Items.AddChild(node,fn);
   ok:=dbgloss.GoNext;
end;
Treeview1.Items.EndUpdate;
treeviewinitializing:=false;
Treeview1.Items[0].Expand(false);
Treeview1.Items[0].Items[0].Selected:=true;
Treeview1.TopItem:=Treeview1.Items[0];
lastnode:=Treeview1.Selected;
end;

procedure TGloss.FormCreate(Sender: TObject);
begin
dbgloss := TMlb2.Create;
InitGlossary;
end;

procedure TGloss.FormDestroy(Sender: TObject);
begin
dbgloss.Free;
end;

procedure TGloss.Button1Click(Sender: TObject);
var buf1,buf2:string;
    i,p,first:integer;
begin
lastpos:=dbgloss.GetPosition;
lastnode:=Treeview1.Selected;
first:=dbgloss.GetPosition;
buf1:=uppercase(edit1.Text);
repeat
if dbgloss.EndOfFile then dbgloss.GoFirst
                     else dbgloss.GoNext;
buf2:=uppercase(dbgloss.GetData('GLO_TEXT')+' '+dbgloss.GetData('GLO_WORD'));
p:=pos(buf1,buf2);
until (p>0) or (dbgloss.GetPosition=first);
i:=dbgloss.GetPosition;
SelectNode(i);
ShowGloss(dbgloss.GetData('GLO_TEXT'));
end;

procedure TGloss.Button2Click(Sender: TObject);
begin
close;
end;

procedure TGloss.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if key=13 then Button1Click(Sender);
end;

procedure TGloss.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
if treeviewinitializing then exit;
SearchGloss(node.Text);
end;

procedure TGloss.TreeView1Changing(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
if treeviewinitializing then begin
   allowchange:=true;
end else begin
 if (node<>nil)and(length(node.Text)=1) then begin
   allowchange:=false;
   node.Expand(false);
   if Node.Items[0]<>nil then begin
     Node.Items[0].Selected:=true;
     Treeview1.TopItem:=Node;
   end;
end;
end;
end;

procedure TGloss.Gloss1HotClick(Sender: TObject);
var p : integer;
    buf:string;
    ok:boolean;
begin
p:=pos('#',gloss1.HotURL);
if p=1 then begin
  lastpos:=dbgloss.GetPosition;
  lastnode:=Treeview1.Selected;
  buf:=trim(copy(gloss1.HotURL,2,999));
  ok:=SearchGloss(buf);
  if ok then begin
    SelectTree(buf);
  end;
end;
end;

procedure TGloss.Button3Click(Sender: TObject);
begin
dbgloss.Go(lastpos);
Treeview1.Selected:=lastnode;
Treeview1.TopItem:=lastnode;
ShowGloss(dbgloss.GetData('GLO_TEXT'));
end;

initialization
  {$i glossary.lrs}

end.
