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
    alphaPanel: TPanel;
    definitionPanel: TIpHtmlPanel;
    htmlPanel: TPanel;
    nextButton: TButton;
    prevButton: TButton;
    buttonPanel: TPanel;
    closeButton: TButton;
    divBevel: TBevel;
    filterEdit: TEdit;
    navPanel: TPanel;
    TreeView1: TTreeView;
    Button2:   TButton;
    procedure buttonPanelResize(Sender: TObject);
    procedure definitionHotClick(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure nextButtonClick(Sender: TObject);
    procedure closeButtonClick(Sender: TObject);
    procedure filterEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
    procedure TreeView1Changing(Sender: TObject; Node: TTreeNode;
      var AllowChange: boolean);
    procedure prevButtonClick(Sender: TObject);
  private
    { Déclarations privées }
    procedure CreateNewAlphaButton(aOwner: TComponent; aChar: Char; aClickEvent: TNotifyEvent);
    procedure InitAlphaButtons;
  public
    { Déclarations publiques }
    procedure InitGlossary;
    procedure SelectTree(nom: string);
    procedure SelectNode(id: integer);
    function SearchGloss(w: string): boolean;
    procedure ShowGloss(txt: string);
  end;

function GlossaryForm: TGloss;

var
  treeviewinitializing: boolean;

implementation

uses u_constant, u_util, mlb2;

var
  Gloss: TGloss;
  dbgloss:  TMlb2;
  lastpos:  integer;
  lastnode: TTreenode;

function GlossaryForm(): TGloss;
begin
	if not Assigned(Gloss) then
		Gloss := TGloss.Create(Application);
	Result:= Gloss;
end;

function dcopy(s: string; f, l: integer): string;
begin
  Result := copy(s, f, l);
end;

procedure TGloss.SelectTree(nom: string);
var
  i, j, n, m: integer;
  l: string;
begin
  l := dcopy(nom, 1, 1);
  n := -1;
  m := -1;
  j := 0;
  for i := 0 to Treeview1.Items.Count - 1 do
  begin
    if Treeview1.Items[i].Text >= l then
    begin
      n := i;
      break;
    end;
  end;
  if n >= 0 then
    for j := 0 to Treeview1.Items[n].Count - 1 do
    begin
      if Treeview1.Items[n].Items[j].Text = nom then
      begin
        m := j;
        break;
      end;
    end;
  if m >= 0 then
  begin
    Treeview1.Items[n].Expand(False);
    Treeview1.Items[n].Items[j].Selected := True;
    Treeview1.TopItem := Treeview1.Items[n].Items[j];
  end;
end;

procedure TGloss.SelectNode(id: integer);
var
  i, n, c, f: integer;
begin
  c := 0;
  n := -1;
  for i := 0 to Treeview1.Items.Count - 1 do
  begin
    f := Treeview1.Items[i].Count;
    if f = 0 then
    begin
      c := c + 1;
      if c >= id then
      begin
        n := i;
        break;
      end;
    end;
  end;
  if n >= 0 then
  begin
    Treeview1.Items[n].Expand(False);
    Treeview1.Items[n].Selected := True;
    Treeview1.TopItem := Treeview1.Items[n];
  end;
end;

function isupper(c: string): boolean;
begin
  Result := (c = ' ') or ((c >= 'A') and (c <= 'Z'));
end;

procedure TGloss.ShowGloss(txt: string);
var
  i, p: integer;
  c, buf, lien, lientxt: string;
  stop: boolean;
  NewHTML: TIpHtml;
  s: TStringStream;
begin
  if alphaPanel.Visible and (length(txt) > 0) then
  begin
    txt := txt + '  ';
    buf := '<html> <body>';
    i   := 1;
    p   := length(txt);
    repeat
      c := dcopy(txt, i, 1);
      if isupper(c) and (c <> ' ') then
      begin
        lien    := '';
        lientxt := '';
        repeat
          stop := False;
          c    := dcopy(txt, i, 1);
          if isupper(c) then
          begin
            lien    := lien + c;
            lientxt := lientxt + c;
          end
          else if (c <> ',') and (length(lien) > 1) and isupper(dcopy(txt, i + 1, 1)) and
            isupper(dcopy(txt, i + 2, 1)) and isupper(dcopy(txt, i + 3, 1)) then
          begin
            lientxt := lientxt + c;
          end
          else
            stop := True;
          Inc(i);
        until stop or (i > p);
        if i <= p then
          Dec(i);
        Dec(i);
        if dcopy(lien, length(lien), 1) = ' ' then
        begin
          lien := dcopy(lien, 1, length(lien) - 1);
          Dec(i);
        end;
        if length(lien) > 1 then
        begin
          if dcopy(lientxt, length(lientxt), 1) = ' ' then
          begin
            lientxt := dcopy(lien, 1, length(lientxt) - 1);
            c := ' ';
          end
          else
            c := '';
          buf := buf + ' <A HREF="#' + lien + '">' + lientxt + '</A>' + c;
        end
        else
          buf := buf + lien;
      end
      else
        buf := buf + c;
      Inc(i);
    until i > p;
    buf := buf + '</body></html>';
  end
  else
    buf := '<html> <body>' + txt + '</body></html>';
  try
    s := TStringStream.Create(buf);
    try
      NewHTML := TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
      NewHTML.LoadFromStream(s);
    finally
      s.Free;
    end;
    definitionPanel.SetHtml(NewHTML);
  except
  end;
end;

function TGloss.SearchGloss(w: string): boolean;
begin
  dbgloss.Gofirst;
  Result := dbgloss.MatchData('GLO_WORD', '=', w);
  if not Result then
    Result := dbgloss.SeekData('GLO_WORD', '=', w);
  if Result then
  begin
    ShowGloss(dbgloss.GetData('GLO_TEXT'));
  end;
end;

procedure TGloss.Label1Click(Sender: TObject);
var
  i, n: integer;
  l:    string;
  tvi: TTreeNode;
begin
	if (Sender is TButton) then
		l:= (Sender as TButton).Caption
	else
		l:= '';

  n := -1;
  for i := 0 to Treeview1.Items.Count - 1 do
  begin
  	if (AnsiCompareStr(Treeview1.Items[i].Text, l) >= 0) then
    begin
      n := i;
      break;
    end;
  end;

  if n >= 0 then
  begin
    tvi := TreeView1.Items[n];

    tvi.Expand(False);
    tvi.Items[0].Selected := True;
    Treeview1.TopItem := tvi;
    SearchGloss(tvi.Items[0].Text);
  end;
  lastpos  := dbgloss.GetPosition;
  lastnode := Treeview1.Selected;
end;

procedure TGloss.InitGlossary;
var
  ok:   boolean;
  fn:   string;
  a, c: string;
  node: TTreeNode;
begin
  fn := Slash(appdir) + Slash('Database') + 'glossary_u' + uplanguage + '.csv';
  if not fileexists(fn) then
    fn := Slash(appdir) + Slash('Database') + 'glossary_uEN.csv';
  if not fileexists(fn) then
    exit;
  dbgloss.LoadFromCSVFile(fn);
  ok      := dbgloss.GoFirst;
  lastpos := dbgloss.GetPosition;
  ShowGloss(dbgloss.GetData('GLO_TEXT'));
  treeviewinitializing := True;
  fn := dbgloss.GetData('GLO_WORD');
  c  := copy(fn, 1, 1);
  alphaPanel.Visible := isupper(c);
  Treeview1.Items.BeginUpdate;
  Treeview1.Items.Clear;
  a    := '  ';
  node := nil;
  while ok do
  begin
    fn := dbgloss.GetData('GLO_WORD');
    c  := dcopy(fn, 1, 1);
    if c <> a then
    begin
      a    := c;
      node := Treeview1.Items.Add(nil, a);
    end;
    Treeview1.Items.AddChild(node, fn);
    ok := dbgloss.GoNext;
  end;
  Treeview1.Items.EndUpdate;
  treeviewinitializing := False;
  Treeview1.Items[0].Expand(False);
  Treeview1.Items[0].Items[0].Selected := True;
  Treeview1.TopItem := Treeview1.Items[0];
  lastnode := Treeview1.Selected;
end;

procedure TGloss.FormCreate(Sender: TObject);
begin
{$ifdef mswindows}
  ScaleForm(self, Screen.PixelsPerInch / 96);
{$endif}
  dbgloss := TMlb2.Create;
  InitGlossary;
  InitAlphaButtons;
end;

procedure TGloss.FormDestroy(Sender: TObject);
begin
  dbgloss.Free;
end;

procedure TGloss.nextButtonClick(Sender: TObject);
var
  buf1, buf2:  string;
  i, p, First: integer;
begin
  lastpos  := dbgloss.GetPosition;
  lastnode := Treeview1.Selected;
  First    := dbgloss.GetPosition;
  buf1     := uppercase(filterEdit.Text);
  repeat
    if dbgloss.EndOfFile then
      dbgloss.GoFirst
    else
      dbgloss.GoNext;
    buf2 := uppercase(dbgloss.GetData('GLO_TEXT') + ' ' + dbgloss.GetData('GLO_WORD'));
    p    := pos(buf1, buf2);
  until (p > 0) or (dbgloss.GetPosition = First);
  i := dbgloss.GetPosition;
  SelectNode(i);
  ShowGloss(dbgloss.GetData('GLO_TEXT'));
end;

procedure TGloss.closeButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TGloss.filterEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 13 then
    nextButtonClick(Sender);
end;

procedure TGloss.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
  if treeviewinitializing then
    exit;
  SearchGloss(node.Text);
end;

procedure TGloss.TreeView1Changing(Sender: TObject; Node: TTreeNode;
  var AllowChange: boolean);
begin
  // this code lock the cursor on the first letter
  // not remember why it is there. maybe old delphi bug ????
{if treeviewinitializing then begin
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
end; }
end;

procedure TGloss.definitionHotClick(Sender: TObject);
var
  p:   integer;
  buf: string;
  ok:  boolean;
begin
  p := pos('#', definitionPanel.HotURL);
  if p = 1 then
  begin
    lastpos := dbgloss.GetPosition;
    lastnode := Treeview1.Selected;
    buf := trim(copy(definitionPanel.HotURL, 2, 999));
    ok := SearchGloss(buf);
    if ok then
    begin
      SelectTree(buf);
    end;
  end;
end;

procedure TGloss.buttonPanelResize(Sender: TObject);
begin
  closeButton.Left:=(buttonPanel.ClientWidth - closeButton.Width) div 2;
end;

procedure TGloss.prevButtonClick(Sender: TObject);
begin
  dbgloss.Go(lastpos);
  Treeview1.Selected := lastnode;
  Treeview1.TopItem  := lastnode;
  ShowGloss(dbgloss.GetData('GLO_TEXT'));
end;

procedure TGloss.CreateNewAlphaButton(aOwner: TComponent; aChar: Char; aClickEvent: TNotifyEvent);

	function GetEnabledStatus(aStr: string): Boolean;
	begin
		Result := dbgloss.GoFirst;
		Result := Result and dbgloss.SeekData('GLO_WORD', '>=', aStr);
		Result := Result and (AnsiCompareStr(LeftStr(dbgloss.GetData('GLO_WORD'), 1), LeftStr(aStr, 1)) = 0);
  end;

var
	newBtn: TButton;
begin
	newBtn:= TButton.Create(aOwner);
	newBtn.Name:= 'alphaButton_'+aChar;
	newBtn.Caption:=aChar;
	newBtn.Tag:=Ord(aChar);
	newBtn.OnClick:=aClickEvent;
	newBtn.Parent:= (aOwner as TWinControl);
	newBtn.Enabled:= GetEnabledStatus(aChar);
	newBtn.Show;
end;

procedure TGloss.InitAlphaButtons;
var
	i: Char;
begin
	for i:='A' to 'Z' do
	begin
		CreateNewAlphaButton(alphaPanel, i, Label1Click);
	end;
end;

initialization
  {$i glossary.lrs}
  Gloss := nil;

end.

