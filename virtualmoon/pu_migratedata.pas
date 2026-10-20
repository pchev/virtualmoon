unit pu_migratedata;

{$mode ObjFPC}{$H+}
{
Copyright (C) 2026 Patrick Chevalley

http://www.ap-i.net
pch@ap-i.net

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

uses u_constant,u_util, FileUtil,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_migratedata }

  Tf_migratedata = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
  private

  public
    procedure Migrate(source: string);
  end;

var
  f_migratedata: Tf_migratedata;

implementation

{$R *.lfm}

procedure Tf_migratedata.FormCreate(Sender: TObject);
begin
  label1.Caption:='';
  label2.Caption:='';
end;

procedure Tf_migratedata.Migrate(source: string);
const
  oldversion='20250615'; // version 9
  newversion='20261018'; // version 10 for unchanged files
  max_amateur=9;
  pict_amateur: array [0..max_amateur] of string = ('BestOfAmateurs','BestOfHiggins','BestOfLazzarotti','Best_Pic_du_Midi','Best_of_Peach','BestOfCathala','BestOfBrahic','BestOfViladrich','BestOfHeully','BestofPelissard');
  max_probe=9;
  pict_probe: array [0..max_probe] of string = ('Lopam','Clementine','CLA','LAC_LM','ApolloMapping','LunarPits','Smart-1','Artemis2','Bestofhistorical','BestofLROobliques');
  max_texture=14;
  textures: array [0..max_texture] of string = ('Airbrush','Historical','Overlay','WAC_Color_Shade','Airbrush_no_albedo','Clementine','USGS Geological 2020','LAC','CNSA Geological map','Clementine_Negative','Change','Lopam','LOLA_Kaguya_Shade','WAC_LOWSUN','WAC');


var dest: string;
   i: integer;
   f: textfile;
   ok: boolean;
begin
 {$ifdef darwin}
  exit;   // no previous Mac version
 {$endif}

  if MessageDlg('Data migration','Do you want to migrate the data from the previous version?',
     mtConfirmation, mbYesNo,0)=mrYes
  then begin
    Application.ProcessMessages;
    // Amateur Pictures
    dest:=slash(appdir)+'Pictures';
    ForceDirectories(dest);
    ok:=false;
    for i:=0 to max_amateur do begin
      if DirectoryExists(slash(source)+pict_amateur[i]) then begin
        label1.Caption:=slash(source)+pict_amateur[i];
        label2.Caption:=slash(dest)+pict_amateur[i];
        Application.ProcessMessages;
        ok:=CopyDirTree(slash(source)+pict_amateur[i],slash(dest)+pict_amateur[i]) or ok;
      end;
    end;
    if ok then begin
      AssignFile(f,slash(dest)+'Picture1_version.txt');
      Rewrite(f);
      WriteLn(f,oldversion);
      CloseFile(f);
    end;
    // Probes Pictures
    ok:=false;
    for i:=0 to max_probe do begin
      if DirectoryExists(slash(source)+pict_probe[i]) then begin
        label1.Caption:=slash(source)+pict_probe[i];
        label2.Caption:=slash(dest)+pict_probe[i];
        Application.ProcessMessages;
        ok:=CopyDirTree(slash(source)+pict_probe[i],slash(dest)+pict_probe[i]) or ok;
      end;
    end;
    if ok then begin
      AssignFile(f,slash(dest)+'Picture2_version.txt');
      Rewrite(f);
      WriteLn(f,oldversion);
      CloseFile(f);
    end;
    // Textures
    dest:=slash(appdir)+'Textures';
    ForceDirectories(dest);
    for i:=0 to max_texture do begin
      if DirectoryExists(slash(source)+slash('Textures')+textures[i]) then begin
        label1.Caption:=slash(source)+slash('Textures')+textures[i];
        label2.Caption:=slash(dest)+textures[i];
        Application.ProcessMessages;
        ok:=CopyDirTree(slash(source)+slash('Textures')+textures[i],slash(dest)+textures[i]) or ok;
      end;
    end;

    if FileExists(slash(dest)+slash('WAC')+slash('L4')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'Textures_version.txt');
      Rewrite(f);
      WriteLn(f,oldversion);
      CloseFile(f);
    end;

    if FileExists(slash(dest)+slash('LAC')+slash('L1')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'HiresLac_version.txt');
      Rewrite(f);
      WriteLn(f,oldversion);
      CloseFile(f);
    end;

    if FileExists(slash(dest)+slash('Lopam')+slash('L5')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'HiresLopam_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('Lopam')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhLopam_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('Change')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhChange_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;

    if FileExists(slash(dest)+slash('WAC')+slash('L5')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'HiresWac_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('WAC')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhWac_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('WAC_LOWSUN')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhWacLs_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;

    if FileExists(slash(dest)+slash('LOLA_Kaguya_Shade')+slash('L5')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'HiresLola_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;
    if FileExists(slash(dest)+slash('LOLA_Kaguya_Shade')+slash('L6')+'0.jpg') then begin
      AssignFile(f,slash(dest)+'VhLola_version.txt');
      Rewrite(f);
      WriteLn(f,newversion);
      CloseFile(f);
    end;

   end;
end;

end.

