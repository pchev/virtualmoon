unit pu_about;
{
Copyright (C) 2007 Patrick Chevalley

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

{$mode objfpc}{$H+}

interface

uses u_translation, wince_func,
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, ComCtrls, Buttons;

type

  { Tf_about }

  Tf_about = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    Title: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
  private
    { private declarations }
    FNextWindow : Tvma_window;
  public
    { public declarations }
    procedure SetLang;
    property NextWindow : Tvma_window read FNextWindow;
  end; 

var
  f_about: Tf_about;

implementation

{ Tf_about }

procedure Tf_about.SetLang;
begin
MenuItem1.Caption:=rsMenu;
MenuItem2.Caption:=rsConfiguratio;
MenuItem3.Caption:=rsDisplay;
MenuItem5.Caption:=rsEphemeris;
MenuItem6.Caption:=rsCalendar;
MenuItem7.Caption:=rsSearch;
MenuItem8.Caption:=rsHelp;
MenuItem10.Caption:=rsMap;
listbox1.clear;
if language='fr' then begin
  listbox1.Items.Add('Version:    1.1   du  13 septembre 2010');
  listbox1.Items.Add(''                                  );
  listbox1.Items.Add('Conception:     Christian Legrand' );
  listbox1.Items.Add('Programmation:  Patrick  Chevalley');
  listbox1.Items.Add('Carte:               Patrick  Chevalley' );
  listbox1.Items.Add('Base de données:       Christian Legrand');
  listbox1.Items.Add('Bibliothèques d''images:Christian Legrand');
  listbox1.Items.Add('Documentation:          Christian Legrand');
  listbox1.Items.Add('Traduction : '+rsTranslator_N);
  listbox1.Items.Add(''                                         );
  listbox1.Items.Add('Copyright (c) 2007 Christian Legrand, Patrick ');
  listbox1.Items.Add('Chevalley,  Tout droit reservé'               );
  listbox1.Items.Add('Ce programme est un logiciel libre; vous pouvez le ');
  listbox1.Items.Add('redistribuer ou le modifier selon les termes de la ');
  listbox1.Items.Add('license GNU General Public License.'               );
  listbox1.Items.Add('Toutes les bases de données, images et documentation'  );
  listbox1.Items.Add('(c) Ch. Legrand'                           );
  listbox1.Items.Add('Les bibliothèques d''images sont sous copyright de leurs'  );
  listbox1.Items.Add('propriétaires respectifs.');
end else begin
  listbox1.Items.Add('Version:    1.1   September 13 2010');
  listbox1.Items.Add(''                                  );
  listbox1.Items.Add('Conception:     Christian Legrand' );
  listbox1.Items.Add('Programming:    Patrick  Chevalley');
  listbox1.Items.Add('Map:                 Patrick  Chevalley' );
  listbox1.Items.Add('Databases:        Christian Legrand');
  listbox1.Items.Add('Pictures library: Christian Legrand');
  listbox1.Items.Add('Documentation:          Christian Legrand');
  listbox1.Items.Add('Translation : '+rsTranslator_N);
  listbox1.Items.Add(''                                         );
  listbox1.Items.Add('Copyright (c) 2007 Christian Legrand, Patrick ');
  listbox1.Items.Add('Chevalley,  All rights reserved'               );
  listbox1.Items.Add('This program is free software; you can redistribute ');
  listbox1.Items.Add('it and/or modify it under the terms of the GNU '     );
  listbox1.Items.Add('General Public License.'                             );
  listbox1.Items.Add('All database, pictures and documentation (c) '       );
  listbox1.Items.Add('Ch. Legrand'                                         );
  listbox1.Items.Add('Pictures library are copyrighted by their respective ');
  listbox1.Items.Add('owner.' );
end;
end;

procedure Tf_about.FormShow(Sender: TObject);
begin
FNextWindow:=w_none;
end;

procedure Tf_about.FormCreate(Sender: TObject);
begin
  SetLang;
end;

// main menu

procedure Tf_about.MenuItem2Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config;
end;

procedure Tf_about.MenuItem3Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_config_display;
end;

procedure Tf_about.MenuItem4Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_tools;
end;

procedure Tf_about.MenuItem5Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_ephemeris
end;

procedure Tf_about.MenuItem6Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_calendar;
end;

procedure Tf_about.MenuItem7Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_search;
end;

procedure Tf_about.MenuItem8Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_help;
end;

{procedure Tf_about.MenuItem9Click(Sender: TObject);
begin
  modalresult:=mrCancel;
  FNextWindow:=w_about;
end;}

procedure Tf_about.MenuItem10Click(Sender: TObject);
begin
  modalresult:=mrOK;
  FNextWindow:=w_none;
end;

initialization
  {$I pu_about.lrs}

end.

