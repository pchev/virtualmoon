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

Function Slash(nom : string) : string;
begin
result:=trim(nom);
if copy(result,length(nom),1)<>PathDelim then result:=result+PathDelim;
end;

Procedure MemoryDebug;
var mem : TMEMORYSTATUS;
    HeapStatus:THeapStatus;
begin
if form1.debuglabel.visible then begin
mem.dwLength:=sizeof(mem);
GlobalMemoryStatus(mem);
{$WARNINGS OFF}
HeapStatus:=GetHeapStatus;
{$WARNINGS ON}
form1.debuglabel.caption:=Inttostr((HeapStatus.TotalAllocated div 1024))+'kb';
end;
end;

function OsVersion: word ;
var lpversioninfo: TOSVERSIONINFO;
begin
lpversioninfo.dwOSVersionInfoSize:=sizeof(TOSVERSIONINFO);
if GetVersionEx(lpversioninfo) then begin
   result:=lpversioninfo.dwMajorVersion;
end
else
 result:=4;
end;

PROCEDURE PrintBitmap(Canvas:  TCanvas; DestRect:  TRect;  Bitmap:  TBitmap);
// Based on posting to borland.public.delphi.winapi by Rodney E Geraghty, 8/8/97.
// Used to print bitmap on any Windows printer.
// found at http://www.efg2.com/lab/library/Default.htm
  VAR
    BitmapHeader:  pBitmapInfo;
    BitmapImage :  POINTER;
    HeaderSize  :  LongWord;
    ImageSize   :  LongWord;
BEGIN
  GetDIBSizes(Bitmap.Handle, HeaderSize, ImageSize);
  GetMem(BitmapHeader, HeaderSize);
  GetMem(BitmapImage,  ImageSize);
  TRY
    GetDIB(Bitmap.Handle, Bitmap.Palette, BitmapHeader^, BitmapImage^);
    StretchDIBits(Canvas.Handle,
                  DestRect.Left, DestRect.Top,     // Destination Origin
                  DestRect.Right  - DestRect.Left, // Destination Width
                  DestRect.Bottom - DestRect.Top,  // Destination Height
                  0, 0,                            // Source Origin
                  Bitmap.Width, Bitmap.Height,     // Source Width & Height
                  BitmapImage,
                  TBitmapInfo(BitmapHeader^),
                  DIB_RGB_COLORS,
                  SRCCOPY)
  FINALLY
    FreeMem(BitmapHeader);
    FreeMem(BitmapImage)
  END
END;

procedure CrePalette(var hpal:HPALETTE);
type
  palentries   = array[0..255] of TPaletteEntry;
  TMyLogPalette = record
    palVersion    : Word;
    palNumEntries : Word;
    palEntry      : palentries;
  end;
var
  pal: TMyLogPalette;
  i: Integer;
begin
pal.palVersion    := $0300;
pal.palNumEntries := 256;
for i := 0 to 255 do begin
        pal.PalEntry[i].peRed := i;
        pal.PalEntry[i].peGreen := i;
        pal.PalEntry[i].peBlue := i;
        pal.PalEntry[i].peFlags := pc_reserved;
end;
Hpal := CreatePalette(PLogPalette(@pal)^);
end;

{$IFDEF opengl}
Procedure OpenHires(forcerebuild:boolean);
var rebuild: boolean;
    hnd: THandle;
begin
  // create files if not exists
  rebuild:=false;

  if ((not fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat'))and
     (fileexists(Slash(appdir)+Slash('textures')+hiresfile)))
      then begin
        rebuild:=true;
      end;

  if ((not fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.dat'))and
     (fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.jpg')))
      then begin
        rebuild:=true;
      end;

  if ((
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c1.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c2.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c3.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c4.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c5.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c6.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c7.jpg'))or
     (not fileexists(Slash(appdir)+Slash('textures')+'moon'+imgsuffix+'_c8.jpg')))and
     (fileexists(Slash(appdir)+Slash('textures')+hiresfile)))
      then begin
        rebuild:=true;
      end;

  if rebuild then begin
     case OsVersion of
     // win 95/98
     4 : ExecuteFile(slash(appdir)+'createtexture.exe','',appdir,SW_SHOWNORMAL);
     // win 2000/xp
     5 : begin
         MessageDlg('Some texture files are missing and must be rebuild.'+crlf+
                    'Please select a user with administrative right on the next selection box, usually it is called "administrator".',
                    mtInformation,[mbOK],0);
         ExecuteAdmin(slash(appdir)+'createtexture.exe','',appdir);
         end;
     // vista    
     6..99: begin
         MessageDlg('Some texture files are missing and must be rebuild.'+crlf+
                    'In the next screen please allow the program to access the computer and give an administator password if required.',
                    mtInformation,[mbOK],0);
         ExecuteAdmin(slash(appdir)+'createtexture.exe','',appdir);
         end;
     end;
     application.ProcessMessages;
     Sleep(2000);
     repeat
       Sleep(100);
       application.ProcessMessages;
       hnd:=findwindow(nil,pchar('createtexture'));
     until hnd=0;
  end;

  // open 948m/pixel file
  if fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat') then begin
     hires.Width:=1024;
     hires.Height:=1024;
     hires.PixelFormat:=pf8bit;
     crepalette(pal);
     hires.Palette:=pal;
     filemode:=0;
     assignfile(fh,Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat');
     reset(fh,1);
     if (hi_w*hi_w/2)=filesize(fh) then hiresok:=true
                                   else begin
                                        hiresok:=false;
                                        closefile(fh);
                                   end;
     form1.HiresSphere.visible:=false;
     form1.HiresSphere.bottom:=-90;
     form1.HiresSphere.Top:=-89;
     form1.HiresSphere.Start:=0;
     form1.HiresSphere.Stop:=1;
  end else hiresok:=false;
  // open 474m/pixel file
  if hiresok and fileexists(Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.dat') then begin
     hires500.Width:=1024;
     hires500.Height:=1024;
     hires500.PixelFormat:=pf8bit;
     crepalette(pal);
     hires500.Palette:=pal;
     filemode:=0;
     assignfile(fh500,Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'_500.dat');
     reset(fh500,1);
     if (hi_w*hi_w*2)=filesize(fh500) then hires500ok:=true
                                   else begin
                                        hires500ok:=false;
                                        closefile(fh500);
                                   end;
     form1.HiresSphere500.visible:=false;
     form1.HiresSphere500.bottom:=-90;
     form1.HiresSphere500.Top:=-89;
     form1.HiresSphere500.Start:=0;
     form1.HiresSphere500.Stop:=1;
  end else hires500ok:=false;
end;
{$ENDIF}

Procedure SetEyepieceMenu;
var i : integer;
    procedure setmenuitem(mi:Tmenuitem;i:integer);
    begin
     if (trim(eyepiecename[i])>'')and(eyepiecefield[i]>0) then begin
        mi.visible:=true;
        mi.Caption:=eyepiecename[i];
     end else mi.visible:=false;
    end;
begin
with form1 do
for i:=1 to 10 do
  case i of
  1 : setmenuitem(e11,i);
  2 : setmenuitem(e21,i);
  3 : setmenuitem(e31,i);
  4 : setmenuitem(e41,i);
  5 : setmenuitem(e51,i);
  6 : setmenuitem(e61,i);
  7 : setmenuitem(e71,i);
  8 : setmenuitem(e81,i);
  9 : setmenuitem(e91,i);
  10: setmenuitem(e101,i);
  end;
end;

Procedure SetLang1;
var section,buf : string;
    inifile : Tmeminifile;
const deftxt = '?';
      blank = '        ';
      b=' ';
begin
language:='UK';
if fileexists(ConfigFile) then
   inifile:=Tmeminifile.create(ConfigFile)
else
   inifile:=Tmeminifile.create(slash(AppDir)+'virtualmoon.ini');
with inifile do begin
section:='default';
buf:=ReadString(section,'Language',language);
end;
inifile.Free;
chdir(appdir);
if fileexists(Slash(AppDir)+slash('language')+'lang_'+buf+'.ini') then language:=buf;
inifile:=Tmeminifile.create(Slash(AppDir)+slash('language')+'lang_'+language+'.ini');
section:='default';
with inifile do begin
  with Form1 do begin
    ldeg:=ReadString(section,'degree','°');
    lmin:=ReadString(section,'minute','''');
    lsec:=ReadString(section,'second','"');
    transmsg:=ReadString(section,'translator','');
    caption:=ReadString(section,'title','Virtual Moon Atlas');
  end;
end;
inifile.free;
end;

Procedure SetLang;
var section,buf : string;
    inifile : Tmeminifile;
    i : integer;
const deftxt = '?';
      blank = '        ';
      b=' ';
begin
wordformat:=0;
inifile:=Tmeminifile.create(Slash(AppDir)+slash('language')+'lang_'+language+'.ini');
section:='default';
with inifile do begin
  with Form1 do begin
    //olddatabase:=(ReadString(section,'database','DBLMW_FR.csv'));
    wordformat:=ReadInteger(section,'format',wordformat);
    helpprefix:=ReadString(section,'help_prefix','UK');
    pofile:=ReadString(section,'lang_po_file','');
    skylib.hp:=helpprefix+'_';
    fichier1.caption:=ReadString(section,'t_1',deftxt);
    ToolButton1.Caption:='&'+fichier1.caption;
    quitter1.caption:=(ReadString(section,'t_2',deftxt));
    configuration1.caption:=(ReadString(section,'t_3',deftxt));
    ToolButton2.Caption:='&'+configuration1.caption;
    label10.caption:=(ReadString(section,'t_4',deftxt));
    zoom1.Caption:=label10.caption;
    toolbutton5.hint:=ReadString(section,'t_5',deftxt);
    //toolbutton5.Caption:=copy(toolbutton5.Caption,1,2)+'&'+copy(toolbutton5.Caption,3,99);
    centre1.Caption:=toolbutton5.hint;
    Position.caption:=(ReadString(section,'t_6',deftxt));
    Position1.Caption:=Position.caption;
    Ephemerides.caption:=(ReadString(section,'t_7',deftxt));
    Button1.caption:=(ReadString(section,'t_10',deftxt));
    Button2.caption:=(ReadString(section,'t_11',deftxt));
    Label9.caption:=(ReadString(section,'t_12',deftxt));
    Label6.caption:=(ReadString(section,'t_13',deftxt));
    Groupbox1.caption:=(ReadString(section,'t_14',deftxt));
    toolbutton8.caption:='&'+ReadString(section,'t_15',deftxt);
    aide1.Caption:=toolbutton8.caption;
    aide2.Caption:=toolbutton8.caption;
    Apropos1.caption:=(ReadString(section,'t_16',deftxt));
    Button5.caption:=(ReadString(section,'t_17',deftxt));
    Button4.caption:=(ReadString(section,'t_113',Button4.caption));
    ToolButton7.hint:=ReadString(section,'t_25',deftxt);
    Image2.Caption:=ToolButton7.hint;
    Label14.caption:=(ReadString(section,'t_32',deftxt));
    Label8.caption:=(ReadString(section,'t_33',deftxt));
    Label11.caption:=(ReadString(section,'t_34',deftxt));
    Label12.caption:=(ReadString(section,'t_35',deftxt));
    Label16.caption:=(ReadString(section,'t_36',deftxt));
    Label13.caption:=(ReadString(section,'t_37',deftxt));
    Combobox2.Items.Clear;
    Combobox2.Items.Add(ReadString(section,'t_42',deftxt));
    Combobox2.Items.Add(ReadString(section,'t_43',deftxt));
    Combobox2.Items.Add(ReadString(section,'t_44',deftxt));
    Combobox2.Items.Add(ReadString(section,'t_45',deftxt));
    Combobox2.Itemindex:=0;
    Terminateur.Caption:=ReadString(section,'t_51',deftxt);
    RadioGroup1.Caption:=ReadString(section,'t_46',deftxt);
    RadioGroup1.Items[0]:=ReadString(section,'t_47',deftxt);
    RadioGroup1.Items[1]:=ReadString(section,'t_48',deftxt);
    RadioGroup1.Items[2]:=ReadString(section,'t_49',deftxt);
    RadioGroup1.Items[3]:=ReadString(section,'t_50',deftxt);
    Reglage.Caption:=ReadString(section,'t_55',deftxt);
    RadioGroup1.ItemIndex:=0;
    Label19.caption:=RadioGroup1.Items[1];
    Label20.caption:=RadioGroup1.Items[2];
    CartesduCiel1.caption:=(ReadString(section,'t_56',deftxt));
    Outils.caption:=(ReadString(section,'t_63',deftxt));
    label22.caption:=b+(ReadString(section,'t_64',deftxt))+b;
    Button12.caption:=(ReadString(section,'t_65',deftxt));
    Button13.caption:=(ReadString(section,'t_66',deftxt));
    RadioGroup2.caption:=(ReadString(section,'t_67',deftxt));
    CheckBox1.caption:=(ReadString(section,'t_68',deftxt));
    distance1.Caption:=ReadString(section,'t_69',deftxt);
    label23.caption:=b+distance1.Caption+b;
    label24.caption:=ReadString(section,'t_70',deftxt);
    label25.caption:=ReadString(section,'t_71',deftxt);
    RadioGroup2.Items[0]:=ReadString(section,'t_72',deftxt);
    RadioGroup2.Items[1]:=ReadString(section,'t_73',deftxt);
    CheckBox2.caption:=ReadString(section,'t_74',deftxt);
    Enregistrersous1.caption:=ReadString(section,'t_78',deftxt);
    Imprimer1.caption:=ReadString(section,'t_79',deftxt);
    ToolButton10.hint:=ReadString(section,'t_81',deftxt);
    Voisinage1.Caption:=ToolButton10.hint;
    Selectiondimprimante1.Caption:=ReadString(section,'t_82',deftxt);
    LgendeGologique1.Caption:=ReadString(section,'t_83',deftxt);
    selectall1.Caption:=ReadString(section,'t_84',deftxt);
    copy1.Caption:=ReadString(section,'t_85',deftxt);
    glossaire1.Caption:=ReadString(section,'t_98',deftxt);
    button15.caption:=ReadString(section,'t_114',button15.caption);
    Notes.caption:=ReadString(section,'t_115',notes.caption);
    Notes1.caption:=Notes.caption;
    label3.caption:=ReadString(section,'t_116',label3.caption);
    Eyepiece1.caption:=ReadString(section,'t_109',Eyepiece1.caption);
    e01.caption:=ReadString(section,'t_117',e01.caption);
    groupbox2.caption:=ReadString(section,'t_125',groupbox2.caption);
    CheckBox3.caption:=ReadString(section,'t_126',CheckBox3.caption);
    CheckBox4.caption:=ReadString(section,'t_127',CheckBox4.caption);
    CheckBox5.caption:=ReadString(section,'t_128',CheckBox5.caption);
    Database1.caption:=ReadString(section,'t_129',Database1.caption);
    Button14.caption:=ReadString(section,'t_145',Button14.caption);
    Checkbox19.caption:=ReadString(section,'t_151',Checkbox19.caption);
    label1.caption:=ReadString(section,'t_153',label1.caption);
    label2.caption:=ReadString(section,'t_154',label2.caption);
    label21.caption:=ReadString(section,'t_155',label21.caption);
    Encyclopedia1.caption:=ReadString(section,'t_165',deftxt);
    Snapshot1.caption:=ReadString(section,'t_167',Snapshot1.caption);
    LibrationButton.hint:=(ReadString(section,'t_175',LibrationButton.caption));
    PhaseButton.hint:=(ReadString(section,'t_176',PhaseButton.caption));
    ToolButton4.hint:=(ReadString(section,'t_177',ToolButton4.caption));
    ToolButton6.hint:=(ReadString(section,'t_178',ToolButton6.caption));
    RemoveMark1.caption:=(ReadString(section,'t_180',RemoveMark1.caption));

    {$ifdef vmalight}
       Toolbutton3.hint:=ReadString(section,'t_130',Toolbutton3.caption);
    {$endif}
    {$ifndef vmalight}
       ButtonDatabase.hint:=Database1.caption;
       CheckBox8.caption:=ReadString(section,'t_182',CheckBox5.caption);
       Toolbutton12.hint:=ReadString(section,'t_183',deftxt);
       CheckBox6.caption:=(ReadString(section,'t_156',deftxt));
       CheckBox7.caption:=(ReadString(section,'t_157',deftxt));
       label28.caption:=(ReadString(section,'t_158',deftxt));
       label26.caption:=(ReadString(section,'t_159',deftxt));
       Groupbox4.caption:=(ReadString(section,'t_160',deftxt));
       Button16.caption:=(ReadString(section,'t_161',deftxt));
       Button17.caption:=(ReadString(section,'t_162',deftxt));
       Button18.caption:=(ReadString(section,'t_163',deftxt));
    {$endif}
    {$ifdef vmaexpert}
       Toolbutton3.hint:=ReadString(section,'t_134',Toolbutton3.caption);
       Rotation1.caption:=ReadString(section,'t_135',Rotation1.caption);
       GroupBox3.caption:=Rotation1.caption;
       Stop1.caption:=ReadString(section,'t_136',Stop1.caption);
       EastWest1.caption:=ReadString(section,'t_137',EastWest1.caption);
       buf:=ReadString(section,'t_138','d/second');
       N10seconde1.caption:='10'+buf;
       N5seconde1.caption:='5'+buf;
       N1seconde1.caption:='1'+buf;
       N05seconde1.caption:='0.5'+buf;
       N02seconde1.caption:='0.2'+buf;
       ComboBox4.items.clear;
       ComboBox4.items.add(N10seconde1.caption);
       ComboBox4.items.add(N5seconde1.caption);
       ComboBox4.items.add(N1seconde1.caption);
       ComboBox4.items.add(N05seconde1.caption);
       ComboBox4.items.add(N02seconde1.caption);
       Combobox4.itemindex:=1;
       Label4.caption:=ReadString(section,'t_139',Label4.caption);
       SpeedButton5.caption:=ReadString(section,'t_140',SpeedButton5.caption);
       SpeedButton6.caption:=ReadString(section,'t_141',SpeedButton6.caption);
       SpeedButton4.caption:=ReadString(section,'t_142',SpeedButton4.caption);
       NewWindowButton.hint:=ReadString(section,'t_166',NewWindowButton.caption);
    {$endif}
  end;
  with Form2 do begin
    caption:=form1.configuration1.caption;
    if multi_instance then caption:='Temporary change only!';
    label1.caption:=(ReadString(section,'t_19',deftxt));
    label2.caption:=(ReadString(section,'t_20',deftxt));
    label3.caption:=(ReadString(section,'t_21',deftxt));
    CheckBox1.caption:=(ReadString(section,'t_22',deftxt));
    CheckBox2.caption:=(ReadString(section,'t_23',deftxt));
    label4.caption:=(ReadString(section,'t_24',deftxt));
    Button1.caption:=ReadString(section,'t_18',deftxt);
    CheckBox3.caption:=(ReadString(section,'t_26',deftxt));
    CheckBox4.caption:=(ReadString(section,'t_27',deftxt));
    Label5.caption:=(ReadString(section,'t_28',deftxt));
    Label17.caption:=(ReadString(section,'t_29',deftxt));
    RadioGroup1.Caption:=(ReadString(section,'t_38',deftxt));
    RadioGroup1.Items[0]:=(ReadString(section,'t_39',deftxt));
    RadioGroup1.Items[1]:=(ReadString(section,'t_40',deftxt));
    RadioGroup1.Items[2]:=(ReadString(section,'t_41',deftxt));
    RadioGroup3.Caption:=ReadString(section,'t_143',deftxt);
    RadioGroup3.Items[0]:=RadioGroup1.Items[0];
    RadioGroup3.Items[1]:=RadioGroup1.Items[1];
    RadioGroup3.Items[2]:=RadioGroup1.Items[2];
    Label7.caption:=(ReadString(section,'t_52',deftxt));
    CheckBox5.caption:=(ReadString(section,'t_53',deftxt));
    CheckBox6.caption:=(ReadString(section,'t_54',deftxt));
    TabSheet1.caption:=(ReadString(section,'t_57',deftxt));
    TabSheet2.caption:=(ReadString(section,'t_58',deftxt));
    label8.caption:=(ReadString(section,'t_59',deftxt));
    label9.caption:=(ReadString(section,'t_60',deftxt));
    stringgrid1.Cells[0,0]:=(ReadString(section,'t_61',deftxt));
    stringgrid1.Cells[1,0]:=(ReadString(section,'t_62',deftxt));
    Label11.caption:=(ReadString(section,'t_75',deftxt));
    Label10.caption:=(ReadString(section,'t_76',deftxt));
    Combobox4.Items[0]:=(ReadString(section,'t_77',deftxt));
    Combobox4.text:=Combobox4.Items[0];
    CheckBox10.caption:=ReadString(section,'t_87',deftxt);
    CheckBox12.caption:=ReadString(section,'t_89',deftxt);
    CheckBox7.caption:=ReadString(section,'t_80',deftxt);
    Impression.caption:=ReadString(section,'t_90',deftxt);
    Label15.caption:=ReadString(section,'t_91',deftxt);
    Button2.caption:=form1.Reglage.Caption;
    Label12.caption:=ReadString(section,'t_92',deftxt);
    Label13.caption:=ReadString(section,'t_93',deftxt);
    Checkbox8.caption:=ReadString(section,'t_94',deftxt);
    Checkbox9.caption:=ReadString(section,'t_95',deftxt);
    Checkbox13.caption:=ReadString(section,'t_96',deftxt);
    Label14.caption:=ReadString(section,'t_97',deftxt);
    Button4.Caption:=ReadString(section,'t_99',deftxt);
    Label16.caption:=ReadString(section,'t_100',deftxt);
    Tabsheet3.caption:=ReadString(section,'t_101',deftxt);
    Checkbox14.caption:=ReadString(section,'t_102',deftxt);
    Label6.caption:=ReadString(section,'t_103',deftxt);
    Label18.caption:=ReadString(section,'t_104',deftxt);
    Label19.caption:=ReadString(section,'t_105',deftxt);
    TabSheet5.caption:=ReadString(section,'t_109',TabSheet5.caption);
    label24.caption:=ReadString(section,'t_110',label24.caption);
    label25.caption:=ReadString(section,'t_111',label25.caption);
    label26.caption:=ReadString(section,'t_112',label26.caption);
    button6.caption:=ReadString(section,'t_113',button6.caption);
    stringgrid2.Cells[0,0]:=ReadString(section,'t_118','Eyepiece Name');
    stringgrid2.Cells[1,0]:=ReadString(section,'t_119','Field in minutes');
    stringgrid2.Cells[2,0]:='<->';
    stringgrid2.Cells[3,0]:='N/S';
    Checkbox17.caption:=ReadString(section,'t_120',Checkbox17.caption);
    Checkbox18.caption:=ReadString(section,'t_121',Checkbox18.caption);
    Checkbox19.caption:=ReadString(section,'t_122',Checkbox19.caption);
    Checkbox20.caption:=ReadString(section,'t_123',Checkbox20.caption);
    Checkbox21.caption:=ReadString(section,'t_131',Checkbox21.caption);
    Checkbox22.caption:=ReadString(section,'t_168',Checkbox22.caption);
    Checkbox23.caption:=ReadString(section,'t_164',Checkbox23.caption);
    Checkbox24.caption:=ReadString(section,'t_187',Checkbox24.caption);
    label28.caption:=ReadString(section,'t_124',label28.caption);
    GroupBox1.caption:=ReadString(section,'t_129',GroupBox1.caption);
    TabSheet7.caption:=GroupBox1.caption;
    RadioGroup3.Caption:=ReadString(section,'t_143',deftxt);
    RadioGroup3.Items[0]:=RadioGroup1.Items[0];
    RadioGroup3.Items[1]:=RadioGroup1.Items[1];
    RadioGroup3.Items[2]:=RadioGroup1.Items[2];
    label29.caption:=ReadString(section,'t_146',label29.caption);
    combobox1.items[0]:=ReadString(section,'t_147',combobox1.items[0]);
    combobox1.items[1]:=ReadString(section,'t_148',combobox1.items[1]);
    combobox1.itemindex:=0;
    combobox2.items[0]:=ReadString(section,'t_149',combobox2.items[0]);
    combobox2.items[1]:=ReadString(section,'t_150',combobox2.items[1]);
    combobox2.itemindex:=0;
    TabSheet4.caption:=ReadString(section,'t_152',TabSheet4.caption);
    RadioGroup2.Caption:=ReadString(section,'t_144',deftxt);
    TabSheet6.caption:=ReadString(section,'t_169',TabSheet6.caption);
    CheckBox11.caption:=ReadString(section,'t_170',CheckBox11.caption);
    label30.caption:=ReadString(section,'t_171',label30.caption);
    label32.caption:=ReadString(section,'t_172',label32.caption);
    Label23.caption:=ReadString(section,'t_173',label23.caption);
    CheckBox16.caption:=ReadString(section,'t_174',CheckBox16.caption);
    label31.caption:=ReadString(section,'t_179',label31.caption);
    label33.caption:=ReadString(section,'t_181',label33.caption);
    nooverlay.caption:=ReadString(section,'t_184',label33.caption);
    label34.caption:=ReadString(section,'t_185',label33.caption);

    RadioGroup2.items.clear;
    {$ifdef vmalight}
     RadioGroup2.items.add(ReadString(section,'t_132','Aerograph'));
     RadioGroup2.items.add(ReadString(section,'t_88','Geological Map'));
    {$endif}
    {$ifdef vmaexpert}
     RadioGroup2.items.add(ReadString(section,'t_132','Aerograph'));
     RadioGroup2.items.add(ReadString(section,'t_133','Clementine'));
     RadioGroup2.items.add(ReadString(section,'t_186','Lopam'));
    {$endif}
  end;
  imac1:=(ReadString(section,'t_30',deftxt));
  imac2:=(ReadString(section,'t_8',deftxt));
  imac3:=(ReadString(section,'t_9',deftxt));
  for i:=1 to nummessage do begin
    m[i]:=ReadString(section,'m_'+trim(inttostr(i)),deftxt);
  end;
end;
inifile.free;
if gloss<>nil then gloss.InitGlossary;
end;

procedure InitObservatoire;
var u,p : double;
const ratio = 0.99664719;
      H0 = 6378140.0 ;
begin
   p:=degtorad(ObsLatitude);
   u:=arctan(ratio*tan(p));
   SkyLib.ObsRoSinPhi:=ratio*sin(u)+(ObsAltitude/H0)*sin(p);
   SkyLib.ObsRoCosPhi:=cos(u)+(ObsAltitude/H0)*cos(p);
   ObsRefractionCor:=1;
end;

{Function GetTimeZone : double;
var lt,st : TSystemTime;
begin
 GetLocalTime(lt);GetSystemTime(st);
 result:=round(240000*(SystemTimeToDateTime(lt)-SystemTimeToDateTime(st)))/10000;
end;}
Function GetTimeZone(sdt:Tdatetime) : double;
var lt,st : TSystemTime;
begin
if UseComputerTime then begin
 DateTimeToSystemTime(sdt,st);
 if not SystemTimeToTzSpecificLocalTime(nil,st,lt) then begin
    GetLocalTime(lt);GetSystemTime(st);
 end;
 result:=round(240000*(SystemTimeToDateTime(lt)-SystemTimeToDateTime(st)))/10000;
end
else result:=timezone; 
end;

Function SetWhiteColor(x:integer):TColor;
begin
result:=x+(x shl 8)+(x shl 16);
end;

Function GetCpy(dir,nom:string):string;
var f:textfile;
begin
result:='';
if uppercase(nom)='LOPAM' then result :='Digitial Lunar Orbiter Photographic Atlas of the Moon / Courtesy Jeff Gillis - Lunar and Planetary Institute (http://www.lpi.usra.edu/research/lunar_orbiter/) '
else if uppercase(nom)='APOLLO' then result :='Courtesy NASA / http://www.nasa.gov '
else if fileexists(slash(dir)+'copyright.txt') then begin
  filemode:=0;
  assignfile(f,slash(dir)+'copyright.txt');
  reset(f);
  readln(f,result);
  closefile(f);
end;
end;

Procedure InitDBKeys;
begin
{LONGIN:=db.GetFieldIndex('LONGIN');
LATIN:=db.GetFieldIndex('LATIN');
WIDEKM:=db.GetFieldIndex('WIDEKM');
WIDEMI:=db.GetFieldIndex('WIDEMI');
LENGHTKM:=db.GetFieldIndex('LENGHTKM');
LENGHTMI:=db.GetFieldIndex('LENGHTMI');
//LENGTHKM:=db.GetFieldIndex('LENGTHKM');
//LENGTHMI:=db.GetFieldIndex('LENGTHMI');
FNAME:=db.GetFieldIndex('NAME');
INTERESTN:=db.GetFieldIndex('INTERESTN');
DIAMINST:=db.GetFieldIndex('DIAMINST'); }
end;

Procedure AddImages(dir,nom,cpy:string);
var i:integer;
    ok:boolean;
begin
ok:=true;
for i:=0 to maximgdir-1 do if (imgdir[i,2]=nom)or(imgdir[i,0]=dir) then ok:=false;
if ok then begin
  inc(maximgdir);
  setlength(imgdir,maximgdir);
  imgdir[maximgdir-1,0]:=dir;
  imgdir[maximgdir-1,2]:=nom;
  imgdir[maximgdir-1,1]:=cpy;
end;

end;

Procedure InitImages;
begin
if maximgdir=0 then begin
   maximgdir:=3;
   setlength(imgdir,maximgdir);
   imgdir[0,0]:=slash(appdir)+'Lopam';
   imgdir[0,2]:='LOPAM';
   imgdir[0,1]:=GetCpy(imgdir[0,0],imgdir[0,2]);
   imgdir[1,0]:=slash(appdir)+'Apollo';
   imgdir[1,2]:='Apollo';
   imgdir[1,1]:=GetCpy(imgdir[1,0],imgdir[1,2]);
   imgdir[2,0]:=slash(appdir)+'My Images';
   imgdir[2,2]:='My Images';
   imgdir[2,1]:=GetCpy(imgdir[2,0],imgdir[2,2]);
end;
addimages(slash(appdir)+'Clementine','Clementine','');
addimages(slash(appdir)+'Probes','Probes','');
addimages(slash(appdir)+'LunaStars','LunaStars','');
addimages(slash(appdir)+'CLA','CLA','Consolidated Lunar Atlas Copyright 2003 Lunar and Planetary Institute / Universities Space Research Association');
addimages(slash(appdir)+'LAC_LM','LAC_LM','Lunar Chart / Lunar Map. The Defense Mapping Agency 1973, Lunar and Planetary Institute 2005');
addimages(slash(appdir)+'ApolloMapping','Apollo Mapping Camera','Courtesy NASA / http://www.nasa.gov');
addimages(slash(appdir)+'BestOfAmateurs','Best of Amateurs','');
addimages(slash(appdir)+'BestOfHiggins','Best of Higgins','');
addimages(slash(appdir)+'BestOfLazzarotti','Best of Lazzarotti','');
end;

{$IFDEF opengl}
Procedure SetMirror(onoff:boolean);
begin
with form1 do begin
if onoff then begin
     GLMirror1.Visible:=true;
     glcamera1.Direction.Z:=-1;
     glcamera1.Direction.X:=0;
     glcamera1.Direction.Y:=0;
end else begin
     GLMirror1.Visible:=false;
     glcamera1.Direction.Z:=1;
     glcamera1.Direction.X:=0;
     glcamera1.Direction.Y:=0;
end;
flipx:=sgn(GLCamera1.Direction.z);
end;
end;
{$ENDIF}

{$IFDEF opengl}
Procedure ShowSphere;
// hide non visible sphere to improve performance
var ls:double;
    m:string;
Function Shows(l:double; sphere:TGLSphere; n:boolean):boolean;
var p:double;
begin
if (n and (librb>0.5))or(not n and (librb<-0.5))or(inclination<>0) then result:=true
else begin
  with form1 do p:=angulardistance(l,0,(sphere.Start+sphere.Stop)/2,0);
  if p<135 then result:=true
           else result:=false;
end;
sphere.visible:=result;
end;

begin
with form1 do begin
  GLSceneViewer1.Buffer.BeginUpdate;
  form1.sphere1.visible:=false;
  form1.sphere2.visible:=false;
  form1.sphere3.visible:=false;
  form1.sphere4.visible:=false;
  form1.sphere5.visible:=false;
  form1.sphere6.visible:=false;
  form1.sphere7.visible:=false;
  form1.sphere8.visible:=false;
  ls:=round(rmod(180-dummycube1.TurnAngle+360,360));
  m:='';
  if Shows(ls,sphere1,true) then m:=m+' 1';
  if Shows(ls,sphere2,true) then m:=m+' 2';
  if Shows(ls,sphere3,false) then m:=m+' 3';
  if Shows(ls,sphere4,false) then m:=m+' 4';
  if Shows(ls,sphere5,true) then m:=m+' 5';
  if Shows(ls,sphere6,true) then m:=m+' 6';
  if Shows(ls,sphere7,false) then m:=m+' 7';
  if Shows(ls,sphere8,false) then m:=m+' 8';
  GLSceneViewer1.Buffer.EndUpdate;
//  showmessage(m);
end;
end;

Procedure SetCompression(onoff:boolean);
var
    cmp:TGLTextureCompression;
    i: integer;
begin
if UseOpenGL then begin
 if onoff then begin
     cmp:=tcStandard;
 end else begin
     cmp:=tcDefault;
 end;
 with form1 do begin
   for i:=0 to 19 do begin
     GLMaterialLibrary1.Materials[i].Material.Texture.Compression:=cmp;
   end;
 end;
end;
end;

{$ENDIF}

Procedure Readdefault;
var inif: TMemIniFile;
    section : string;
    i,j : integer;
{$IFDEF opengl}
    smooth : integer;
{$ENDIF}
begin
SafeMode:=false;
database[1]:='Nearside_Named_FR.csv';
usedatabase[1]:=true;
usedatabase[2]:=true;
usedatabase[3]:=true;
usedatabase[4]:=true;
usedatabase[5]:=true;
usedatabase[6]:=true;
for i:=7 to maxdbn do usedatabase[i]:=false;
timezone:=0;
Obslatitude:=46;
Obslongitude:=-6;
Obsaltitude:=0;
phaseeffect:=true;
librationeffect:=true;
geocentric:=false;
showoverlay:=false;
useOpenGL:=true;
wheelstep:=1.05;
labelcolor:=clYellow;
markcolor:=clRed;
autolabelcolor:=clLime;
labelcenter:=true;
minilabel:=true;
showlabel:=true;
showmark:=true;
currentselection:='';
showlibrationmark:=false;
lockmove:=false;
maxima := 2;
LabelDensity:=600;
LabelSize:=1;
marksize:=5;
saveimagesize:=0;
saveimagewhite:=false;
ReduceTexture:=1;
ReduceTextureFar:=1;
lastima:=0;
currentphase:=-999;
fov:=45;
CameraOrientation:=0;
PoleOrientation:=0;
FollowNorth:=false;
flipx:=1;
LeftMargin:=10;
PrintTextWidth:=700;
PrintChart:=true;
PrintEph:=true;
PrintDesc:=true;
MipMaps:=false;
doublebuf:=true;
stencilbuf:=false;
antialias:='Default';
LopamDirect:=false;
ruklprefix:='C:\rukl\';
ruklsuffix:='_large.jpg';
GeologicalMap:=false;
externalimage:=false;
externalimagepath:='mspaint.exe';
hiresfile:='hires.jpg';
imgsuffix:='';
hiresok:=false;
hires500ok:=false;
hi_w:=11520;
hi_dl:=32;
hi_wd:=16;
phaseumbra:=$00484848;
eyepiecename[1]:='SCT 8" + Plossl 10mm';
eyepiecefield[1]:=15;
rotdirection:=1;
rotstep:=5;
CloseVMAbrowser:=false;
ClosePhotlun:=false;
{$IFDEF opengl}
{$IFDEF vmapro}
  smooth:=180;
{$ELSE}
  smooth:=90;
{$ENDIF}
{$ENDIF}
if fileexists(ConfigFile) then
   inif:=Tmeminifile.create(ConfigFile)
else
   inif:=Tmeminifile.create(slash(AppDir)+'virtualmoon.ini');
with inif do begin
section:='images';
LopamDirect:=ReadBool(section,'LopamDirect',LopamDirect);
externalimage:=ReadBool(section,'ExternalSoftware',externalimage);
externalimagepath:=ReadString(section,'ExternalSoftwarePath',externalimagepath);
maximgdir:=ReadInteger(section,'NumDir',0);
maxima:=ReadInteger(section,'NumWindow',2);
saveimagesize:=ReadInteger(section,'SaveImageSize',saveimagesize);
saveimagewhite:=ReadBool(section,'SaveImageWhite',saveimagewhite);
setlength(imgdir,maximgdir);
setlength(piczoom,maxima);
setlength(pictop,maxima);
setlength(picleft,maxima);
setlength(ima,maxima);
for i:=1 to maximgdir do begin
  imgdir[i-1,0]:=ReadString(section,'dir'+inttostr(i),'');
  imgdir[i-1,2]:=ReadString(section,'name'+inttostr(i),imgdir[i-1,0]);
  imgdir[i-1,1]:=GetCpy(imgdir[i-1,0],imgdir[i-1,2]);
end;
ruklprefix:=ReadString(section,'ruklprefix',ruklprefix);
ruklsuffix:=ReadString(section,'ruklsuffix',ruklsuffix);
section:='Print';
LeftMargin:=ReadInteger(section,'LeftMargin',LeftMargin);
PrintTextWidth:=ReadInteger(section,'PrintTextWidth',PrintTextWidth);
PrintChart:=ReadBool(section,'PrintChart',PrintChart);
PrintEph:=ReadBool(section,'PrintEph',PrintEph);
PrintDesc:=ReadBool(section,'PrintDesc',PrintDesc);
section:='default';
pofile:=ReadString(section,'lang_po_file','');
UseComputerTime:=ReadBool(section,'UseComputerTime',UseComputerTime);
for i:=1 to 6 do usedatabase[i]:=ReadBool(section,'UseDatabase'+inttostr(i),usedatabase[i]);
for i:=1 to 6 do db_age[i]:=ReadInteger(section,'DB_Age'+inttostr(i),0);
doublebuf:=ReadBool(section,'DoubleBuffer',doublebuf);
stencilbuf:=ReadBool(section,'StencilBuffer',stencilbuf);
farsidetexture:=ReadBool(section,'farsidetexture',farsidetexture);
compresstexture:=ReadBool(section,'compresstexture',compresstexture);
antialias:=ReadString(section,'AntiAlias',antialias);
//appdir:=ReadString(section,'Install_Dir',appdir);
Obslatitude:=ReadFloat(section,'Obslatitude',Obslatitude);
Obslongitude:=ReadFloat(section,'Obslongitude',Obslongitude);
if UseComputerTime then
   timezone:=gettimezone(now)
else begin
   timezone:=ReadFloat(section,'TimeZone',timezone);
   CurrentJD:=ReadFloat(section,'CurrentJD',CurrentJD);
   dt_ut:=ReadFloat(section,'dt_ut',dt_ut);
end;
cameraorientation:=ReadFloat(section,'CameraOrientation',CameraOrientation);
phaseeffect:=ReadBool(section,'PhaseEffect',phaseeffect);
mipmaps:=ReadBool(section,'MipMaps',mipmaps);
GeologicalMap:=ReadBool(section,'GeologicalMap',GeologicalMap);
librationeffect:=ReadBool(section,'LibrationEffect',librationeffect);
phasehash:=ReadBool(section,'PhaseHash',PhaseHash);
phaseumbra:=ReadInteger(section,'PhaseUmbra',PhaseUmbra);
ShowLabel:=ReadBool(section,'ShowLabel',ShowLabel);
ShowMark:=ReadBool(section,'ShowMark',ShowMark);
ShowLibrationMark:=ReadBool(section,'ShowLibrationMark',ShowLibrationMark);
// useOpenGL:=ReadBool(section,'useOpenGL',useOpenGL);
LabelColor:=ReadInteger(section,'LabelColor',LabelColor);
MarkColor:=ReadInteger(section,'MarkColor',MarkColor);
AutolabelColor:=ReadInteger(section,'AutolabelColor',AutolabelColor);
LabelDensity:=ReadInteger(section,'LabelDensity',LabelDensity);
marksize:=ReadInteger(section,'MarkSize',marksize);
LabelSize:=ReadFloat(section,'LabelSize',LabelSize);
labelcenter:=ReadBool(section,'LabelCenter',labelcenter);
minilabel:=ReadBool(section,'MiniLabel',minilabel);
FollowNorth:=ReadBool(section,'FollowNorth',FollowNorth);
form1.CheckBox2.Checked:=ReadBool(section,'Mirror',false);
PoleOrientation:=ReadFloat(section,'PoleOrientation',PoleOrientation);

i:=ReadInteger(section,'Top',0);
if multi_instance then i:=i+25;
if (i>=-10)and(i<screen.Height-20) then form1.Top:=i else form1.Top:=0;

i:=ReadInteger(section,'Left',0);
if multi_instance then i:=i+25;
if (i>=-10)and(i<screen.width-20) then form1.Left:=i else form1.Left:=0;

i:=screen.height-50;
if multi_instance then i:=round(0.8*i);
i:=minintvalue([i,ReadInteger(section,'Height',i)]);
if (i>=20) then form1.Height:=i;
if multi_instance then
   form1.ClientWidth:=form1.ClientHeight-form1.controlbar1.height-form1.statusbar1.height
else begin
  i:=screen.width-5;
  i:=minintvalue([i,ReadInteger(section,'Width',i)]);
  if (i>=20) then form1.Width:=i;
end;

if (not multi_instance) and ReadBool(section,'Maximized',true) then form1.windowstate:=wsMaximized;

for j:=1 to maxima do begin
  i:=ReadInteger(section,'PicTop_'+inttostr(j),-99);if (i>=-10)and(i<screen.Height-20) then PicTop[j-1]:=i else PicTop[j-1]:=20*(j-1);
  i:=ReadInteger(section,'PicLeft_'+inttostr(j),-99);if (i>=-10)and(i<screen.width-20) then PicLeft[j-1]:=i else PicLeft[j-1]:=20*(j-1);
  PicZoom[j-1]:=ReadFloat(section,'PicZoom_'+inttostr(j),0);
end;

ReduceTexture:=ReadInteger(section,'ReduceTexture',ReduceTexture);
ReduceTextureFar:=ReadInteger(section,'ReduceTextureFar',ReduceTextureFar);
{hi_w:=ReadInteger(section,'hi_w',hi_w);
hi_dl:=ReadInteger(section,'hi_dl',hi_dl);
hi_wd:=ReadInteger(section,'hi_wd',hi_wd);}
hiresfile:=ReadString(section,'hiresfile',hiresfile);
for j:=1 to 10 do begin
  eyepiecename[j]:=ReadString(section,'eyepiecename'+inttostr(j),eyepiecename[j]);
  eyepiecefield[j]:=ReadInteger(section,'eyepiecefield'+inttostr(j),eyepiecefield[j]);
  eyepiecemirror[j]:=ReadInteger(section,'eyepiecemirror'+inttostr(j),eyepiecemirror[j]);
  eyepiecerotation[j]:=ReadInteger(section,'eyepiecerotation'+inttostr(j),eyepiecerotation[j]);
end;
{$ifdef vmapro}
 useDBN:=ReadInteger(section,'useDBN',useDBN);
 for i:=1 to useDBN do usedatabase[i]:=ReadBool(section,'UseDatabase'+inttostr(i),usedatabase[i]);
 for i:=1 to useDBN do db_age[i]:=ReadInteger(section,'DB_Age'+inttostr(i),0);
 overlayname:=ReadString(section,'overlayname','Colors natural.jpg');
 overlaylum:=ReadInteger(section,'overlaylum',0);
 showoverlay:=ReadBool(section,'showoverlay',showoverlay);
{$endif}
{$ifdef vmaexpert}
 Geocentric:=ReadBool(section,'Geocentric',Geocentric);
 if copy(hiresfile,6,1)<>'.' then begin
   i:=pos('.',hiresfile);
   if i>6 then imgsuffix:=copy(hiresfile,6,i-6);
 end else imgsuffix:='';
 farsidetexture:=true;
{$endif}
{$ifdef vmabasic}
  Geocentric:=ReadBool(section,'Geocentric',Geocentric);
  usedatabase[3]:=false;
  usedatabase[4]:=false;
  usedatabase[6]:=false;
  showoverlay:=false;
  ReduceTextureFar:=4;
  hiresfile:='hires.jpg';
  imgsuffix:='';
{$endif}
{$ifndef vmalight}
  scopeinterface:=ReadString(section,'telescope','Ascom');
{$endif}
{$ifdef vmalight}
  showoverlay:=false;
  usedatabase[2]:=false;
  usedatabase[4]:=false;
  usedatabase[6]:=false;
{$endif}
{$IFDEF opengl}
useOpenGL:=true;
if useOpenGL then begin
   form1.glscene1.BeginUpdate;
   SetCompression(compresstexture);
   if not doublebuf then form1.GLSceneViewer1.Buffer.ContextOptions:=form1.GLSceneViewer1.Buffer.ContextOptions-[roDoubleBuffer];
   if stencilbuf then form1.GLSceneViewer1.Buffer.ContextOptions:=form1.GLSceneViewer1.Buffer.ContextOptions+[roStencilBuffer];
   if antialias='2x' then form1.GLSceneViewer1.Buffer.AntiAliasing:=aa2x
   else if antialias='2xHQ' then form1.GLSceneViewer1.Buffer.AntiAliasing:=aa2xHQ
   else if antialias='4x' then form1.GLSceneViewer1.Buffer.AntiAliasing:=aa4x
   else if antialias='4xHQ' then form1.GLSceneViewer1.Buffer.AntiAliasing:=aa4xHQ
   else if antialias='None' then form1.GLSceneViewer1.Buffer.AntiAliasing:=aaNone;
   form1.GLLightSource1.Ambient.AsWinColor:=ReadInteger(section,'AmbientLight',form1.GLLightSource1.Ambient.AsWinColor);
   form1.GLLightSource1.Diffuse.AsWinColor:=ReadInteger(section,'DiffuseLight',form1.GLLightSource1.Diffuse.AsWinColor);
   form1.GLLightSource1.Specular.AsWinColor:=ReadInteger(section,'SpecularLight',form1.GLLightSource1.Specular.AsWinColor);
   smooth:=ReadInteger(section,'Smooth',smooth);
   form1.glscene1.EndUpdate;
end;
{$ELSE}
  useOpenGL:=false;
{$ENDIF}
i:=ReadInteger(section,'ListCount',0);
if i>0 then for j:=0 to i do begin
   form1.combobox1.Items.add(ReadString(section,'List_'+inttostr(j),''));
end;
end;
inif.Free;
chdir(appdir);
if not useOpenGL then begin
  librationeffect:=false;
end;
InitObservatoire;
InitImages;
Showautolabel:=showlabel and (Labeldensity<1000);
{$IFDEF opengl}
if useOpenGL then with form1 do begin
  form1.glscene1.BeginUpdate;
  if smooth>120 then smooth:=180
  else if smooth>60 then smooth:=90
  else smooth:=45;
  sphere1.Slices:=smooth;
  sphere1.Stacks:=sphere1.Slices;
  sphere2.Slices:=sphere1.Slices; sphere2.Stacks:=sphere1.Slices;
  sphere3.Slices:=sphere1.Slices; sphere3.Stacks:=sphere1.Slices;
  sphere4.Slices:=sphere1.Slices; sphere4.Stacks:=sphere1.Slices;
  sphere5.Slices:=sphere1.Slices; sphere5.Stacks:=sphere1.Slices;
  sphere6.Slices:=sphere1.Slices; sphere6.Stacks:=sphere1.Slices;
  sphere7.Slices:=sphere1.Slices; sphere7.Stacks:=sphere1.Slices;
  sphere8.Slices:=sphere1.Slices; sphere8.Stacks:=sphere1.Slices;
  case sphere1.Slices of
  45  : smooth:=16;
  90  : smooth:=32;
  180 : smooth:=64;
  end;
  hiressphere.Slices:=smooth ; hiressphere.Stacks:=smooth ;
  hiressphere500.Slices:=smooth ; hiressphere500.Stacks:=smooth ;
  form1.glscene1.EndUpdate;
end;
{$ENDIF}
end;

procedure SaveDefault;
var
    inif: TMemIniFile;
    i : integer;
    section: string;
begin
if safemode then exit;
inif:=Tmeminifile.create(ConfigFile);
try
section:='default';
with inif do begin
  if not(ValueExists(section,'Install_Dir')) then WriteString(section,'Install_Dir',appdir);
  WriteString(section,'Language',Language);
  {$IFDEF opengl}
  WriteBool(section,'DoubleBuffer',doublebuf);
  WriteBool(section,'StencilBuffer',stencilbuf);
  WriteString(section,'AntiAlias',antialias);
  WriteBool(section,'MipMaps',mipmaps);
  WriteBool(section,'LibrationEffect',librationeffect);
  WriteBool(section,'compresstexture',compresstexture);
  WriteFloat(section,'CameraOrientation',CameraOrientation);
  {$ENDIF}
{$ifdef vmapro}
  WriteInteger(section,'useDBN',useDBN);
  for i:=1 to useDBN do WriteBool(section,'UseDatabase'+inttostr(i),usedatabase[i]);
  for i:=1 to useDBN do WriteInteger(section,'DB_Age'+inttostr(i),db_age[i]);
  WriteString(section,'overlayname',overlayname);
  WriteInteger(section,'overlaylum',overlaylum);
  WriteBool(section,'showoverlay',showoverlay);
{$endif}
{$ifdef vmaexpert}
  for i:=1 to 6 do WriteBool(section,'UseDatabase'+inttostr(i),usedatabase[i]);
  for i:=1 to 6 do WriteInteger(section,'DB_Age'+inttostr(i),db_age[i]);
  WriteInteger(section,'ReduceTextureFar',ReduceTextureFar);
  WriteString(section,'hiresfile',hiresfile);
  WriteBool(section,'Geocentric',Geocentric);
  WriteString(section,'telescope',form1.Combobox5.text);
{$endif}
{$ifdef vmabasic}
  WriteBool(section,'farsidetexture',farsidetexture);
  for i:=1 to 2 do WriteBool(section,'UseDatabase'+inttostr(i),usedatabase[i]);
  for i:=1 to 2 do WriteInteger(section,'DB_Age'+inttostr(i),db_age[i]);
  WriteBool(section,'UseDatabase'+inttostr(5),usedatabase[5]);
  WriteInteger(section,'DB_Age'+inttostr(5),db_age[5]);
  WriteBool(section,'Geocentric',Geocentric);
  WriteString(section,'telescope',form1.Combobox5.text);
{$endif}
{$ifdef vmalight}
  WriteBool(section,'UseDatabase'+inttostr(1),usedatabase[1]);
  WriteBool(section,'UseDatabase'+inttostr(3),usedatabase[3]);
  WriteBool(section,'UseDatabase'+inttostr(5),usedatabase[5]);
  WriteInteger(section,'DB_Age'+inttostr(1),db_age[1]);
  WriteInteger(section,'DB_Age'+inttostr(3),db_age[3]);
  WriteInteger(section,'DB_Age'+inttostr(5),db_age[5]);
{$endif}
  WriteFloat(section,'Obslatitude',Obslatitude);
  WriteFloat(section,'Obslongitude',Obslongitude);
  WriteString(section,'lang_po_file',pofile);
  WriteBool(section,'UseComputerTime',UseComputerTime);
  WriteFloat(section,'TimeZone',timezone);
  WriteFloat(section,'CurrentJD',CurrentJD);
  WriteFloat(section,'dt_ut',dt_ut);
  WriteBool(section,'PhaseEffect',phaseeffect);
  WriteBool(section,'GeologicalMap',GeologicalMap);
  WriteBool(section,'PhaseHash',PhaseHash);
  WriteInteger(section,'PhaseUmbra',PhaseUmbra);
  WriteBool(section,'ShowLabel',ShowLabel);
  WriteBool(section,'ShowMark',ShowMark);
  WriteBool(section,'ShowLibrationMark',ShowLibrationMark);
  WriteInteger(section,'LabelColor',LabelColor);
  WriteInteger(section,'MarkColor',MarkColor);
  WriteInteger(section,'AutolabelColor',AutolabelColor);
  WriteInteger(section,'LabelDensity',LabelDensity);
  WriteFloat(section,'LabelSize',LabelSize);
  WriteInteger(section,'MarkSize',marksize);
  WriteBool(section,'LabelCenter',labelcenter);
  WriteBool(section,'MiniLabel',minilabel);
  WriteBool(section,'FollowNorth',FollowNorth);
  WriteBool(section,'Mirror',form1.CheckBox2.Checked);
  WriteFloat(section,'PoleOrientation',PoleOrientation);
  WriteInteger(section,'Top',Form1.Top);
  WriteInteger(section,'Left',Form1.Left);
  WriteInteger(section,'Height',Form1.Height);
  WriteInteger(section,'Width',Form1.Width);
  WriteBool(section,'Maximized',(form1.windowstate=wsMaximized));
  for i:=1 to maxima do
   if ima[i-1]<>nil then begin
    WriteInteger(section,'PicTop_'+inttostr(i),ima[i-1].Top);
    WriteInteger(section,'PicLeft_'+inttostr(i),ima[i-1].Left);
    WriteFloat(section,'PicZoom_'+inttostr(i),ima[i-1].zoom);
   end;
  WriteInteger(section,'ReduceTexture',ReduceTexture);
{  WriteInteger(section,'hi_w',hi_w);
  WriteInteger(section,'hi_dl',hi_dl);
  WriteInteger(section,'hi_wd',hi_wd);}
  for i:=1 to 10 do begin
    WriteString(section,'eyepiecename'+inttostr(i),eyepiecename[i]);
    WriteInteger(section,'eyepiecefield'+inttostr(i),eyepiecefield[i]);
    WriteInteger(section,'eyepiecemirror'+inttostr(i),eyepiecemirror[i]);
    WriteInteger(section,'eyepiecerotation'+inttostr(i),eyepiecerotation[i]);
  end;
  {$IFDEF opengl}
  if useOpenGL and (form1.GLLightSource1<>nil) then begin
    WriteInteger(section,'AmbientLight',form1.GLLightSource1.Ambient.AsWinColor);
    WriteInteger(section,'DiffuseLight',form1.GLLightSource1.Diffuse.AsWinColor);
    WriteInteger(section,'SpecularLight',form1.GLLightSource1.Specular.AsWinColor);
    WriteInteger(section,'Smooth',form1.sphere1.Slices);
  end;
  {$ENDIF}
  WriteInteger(section,'ListCount',form1.combobox1.Items.Count-1);
  for i:=0 to form1.combobox1.Items.Count-1 do begin
    WriteString(section,'List_'+inttostr(i),form1.combobox1.Items.Strings[i]);
  end;
  section:='images';
  WriteBool(section,'LopamDirect',LopamDirect);
  WriteBool(section,'ExternalSoftware',externalimage);
  WriteString(section,'ExternalSoftwarePath',externalimagepath);
  WriteInteger(section,'NumWindow',maxima);
  WriteInteger(section,'NumDir',maximgdir);
  WriteInteger(section,'SaveImageSize',saveimagesize);
  WriteBool(section,'SaveImageWhite',saveimagewhite);
  for i:=1 to maximgdir do begin
   WriteString(section,'dir'+inttostr(i),imgdir[i-1,0]);
   WriteString(section,'name'+inttostr(i),imgdir[i-1,2]);
  end;
  WriteString(section,'ruklprefix',ruklprefix);
  WriteString(section,'ruklsuffix',ruklsuffix);
  section:='Print';
  WriteInteger(section,'LeftMargin',LeftMargin);
  WriteInteger(section,'PrintTextWidth',PrintTextWidth);
  WriteBool(section,'PrintChart',PrintChart);
  WriteBool(section,'PrintEph',PrintEph);
  WriteBool(section,'PrintDesc',PrintDesc);
  inif.UpdateFile
end;
finally
inif.Free;
end;
end;


Function sgn(x:Double):Double ;
begin
if x<0 then
   sgn:= -1
else
   sgn:=  1 ;
end ;

function ProjMoon(l,b,lc,bc : Double ; VAR X,Y : Double ):boolean;
Var hh,s1,s2,s3,c1,c2,c3 : extended ;
BEGIN
 if inclination=0 then begin
    hh := l-lc+lrot ;
    if hh>180 then hh:=hh-360;
    if hh<-180 then hh:=hh+360;
    if (abs(hh)>95) then begin result:=false; exit; end;   // tolerance sur le limbe.
    sincos(Deg2Rad*bc,s1,c1);
    sincos(Deg2Rad*b,s2,c2);
    sincos(Deg2Rad*hh,s3,c3);
    x:= -(c2*s3);
    y:= (s2*c1-c2*s1*c3);
    x:=sgn(x)*minvalue([abs(x),10000])/2;
    y:=sgn(y)*minvalue([abs(y),10000])/2;
    result:=true;
 end else result:=false;   
END ;

function InvProjMoon (x,y,lc,bc : Double ; VAR l,b : Double ):boolean;
Var r,s1,c1,s : extended ;
Begin
    sincos(deg2rad*bc,s1,c1);
    s:=1-x*x-y*y;
    if (inclination=0)and(s>=0) then begin
      r:=sqrt(s);
      l:=lc-lrot-rad2deg*arctan2(x,(c1*r-y*s1));
      if l>180 then l:=l-360;
      if l<-180 then l:=360+l;
      b:=rad2deg*arcsin(y*c1+s1*r);
      result:=true;
    end else begin
      l:=nan;
      b:=nan;
      result:=false;
    end;
end ;

procedure Moon2RaDec(x,y: Double; var r,d: double);
var spa,cpa,s :extended;
begin
sincos(deg2rad*pa,spa,cpa);
s:=diam/3600;
r:=rad+(cpa*x+spa*y)*s/15/cos(deg2rad*ded);
d:=ded+(cpa*y-spa*x)*s;
end;

procedure RaDec2Moon(r,d: double; var x,y:double);
var spa,cpa,s :extended;
begin
sincos(-deg2rad*pa,spa,cpa);
s:=diam/3600;
r:=(r-rad)*15*cos(deg2rad*ded)/s;
d:=(d-ded)/s;
x:=cpa*r+spa*d;
y:=cpa*d-spa*r;
end;

procedure InitLabel;
{$IFDEF opengl}
var 	i : Integer;
	newlabel : TGLHUDText;
{$ENDIF}
begin
{$IFDEF opengl}
if useOpenGL then begin
form1.DummyCube4.DeleteChildren;
for i:=0 to Maxlabel do begin
    newlabel:=TGLHUDText(form1.DummyCube4.AddNewChild(TGLHUDText));
    newlabel.Name:='MoonLabel'+inttostr(i);
    newlabel.Visible:=false;
    newlabel.BitmapFont:=form1.Bitmapfont1;
    newlabel.Layout:=tlCenter;
    newlabel.Up.SetVector(0,1,0);
    newlabel.Scale.SetVector(Label3dSize*LabelSize,Label3dSize*LabelSize,1);
    newlabel.ModulateColor.AsWinColor:=autolabelcolor;
end;
end;
{$ENDIF}
end;

Procedure MoreSprite;
{$IFDEF vmapro}
var 	i : Integer;
	newSprite : TGLHUDSprite;
{$ENDIF}
begin
{$IFDEF vmapro}
if useOpenGL then begin
for i:=0 to InitialSprite-1 do begin
    newSprite:=TGLHUDSprite(form1.DummyCube5.AddNewChild(TGLHUDSprite));
    newSprite.Name:='MoonSprite'+inttostr(maxsprite+i);
    newSprite.Visible:=false;
    newSprite.Material.FrontProperties.Emission.AsWinColor:=MarkColor;
    newSprite.Material.FrontProperties.Ambient.Color:=clrBlack;
    newSprite.Material.FrontProperties.Diffuse.Color:=clrBlack;
    newSprite.Material.FrontProperties.Specular.Color:=clrBlack;
    newSprite.Up.SetVector(0,1,0);
    newSprite.Scale.SetVector(1,1,1);
    newSprite.Width:=marksize;
    newSprite.Height:=marksize;
end;
maxsprite:=maxsprite+InitialSprite;
end;
{$ENDIF}
end;

procedure InitSprite;
begin
{$IFDEF opengl}
maxsprite:=0;
if useOpenGL then begin
form1.DummyCube5.DeleteChildren;
MoreSprite;
end;
{$ENDIF}
end;

Function testfloat(s:string):double;
var i:integer;
begin
if s='' then result:=-1 else begin
  val(s,result,i);
  if i>0 then result:=0;
end;
end;

Procedure Tform1.SetLabel;
var l,b,l1,b1,deltab,deltal,x,y,w,wmin,wfact: double;
    xx,yy,j : integer;
    miniok : boolean;
    nom,let:string;
    ts: Tsize;
{$IFDEF opengl}
    curlab,cursprite,i : integer;
{$ENDIF}
begin
{$IFDEF opengl}
curlab:=0;
if useOpenGL then begin
   xx:=form1.GLSceneViewer1.Width div 2;
   yy:=form1.GLSceneViewer1.Height div 2;
end else
{$ENDIF}
begin
   xx:=form1.image1.Width div 2;
   yy:=form1.image1.Height div 2;
end;
Window2World(xx,yy,x,y);
if not InvProjMoon(2*x,2*y,librl,librb,l,b) then begin
   l:=librl;
   b:=librb;
   deltab:=90;
   deltal:=90;
end else begin
   deltab:=90/zoom;
   deltal:=deltab/cos(deg2rad*b);
end;
if abs(l)>90 then begin // face cachee
 l:=0;
 deltal:=180;
end;
if minilabel then wfact:=0.5 else wfact:=1;
w:=maxfoc/minfoc;
LabelDensity:=maxintvalue([100,LabelDensity]);
if (zoom>3)and(zoom>=w) then wmin:=-1
           else wmin:=minvalue([650,1.5*LabelDensity/(zoom*zoom)]);
if showlabel then begin
dbm.Query('select NAME,LONGIN,LATIN,WIDEKM,WIDEMI,LENGTHKM,LENGTHMI from moon'+
            ' where DBN in ('+sidelist+')'+
            ' and LONGIN > '+formatfloat(d2,l-deltal)+
            ' and LONGIN < '+formatfloat(d2,l+deltal)+
            ' and LATIN > '+formatfloat(d2,b-deltab)+
            ' and LATIN < '+formatfloat(d2,b+deltab)+
            ' and (WIDEKM=0 or WIDEKM>='+formatfloat(d2,(wmin*wfact)/2.5)+')'+
            ' ;');
for j:=0 to dbm.RowCount-1 do begin
  l1:=dbm.Results[j].Format[1].AsFloat;
  b1:=dbm.Results[j].Format[2].AsFloat;
  w:=dbm.Results[j].Format[3].AsFloat;
  if w<=0 then w:=1.67*dbm.Results[j].Format[4].AsFloat;
//  if w<=0 then w:=dbm.Results[j].Format[5].AsFloat;
//  if w<=0 then w:=1.67*dbm.Results[j].Format[6].AsFloat;
  if (w>200)and(abs(l1)>90) then w:=2.5*w; // moins de grosse formation face cachee
  if w<(wmin*wfact) then continue;
  nom:=dbm.Results[j][0];
  {$IFDEF opengl}
  if useopengl and marked and (form1.hudtext1.Text=nom) then continue;
  {$ENDIF}
  if (not useopengl) and marked and (form1.label5.Caption=nom) then continue;
  if projMoon(l1,b1,librl,librb,x,y) then begin
    nom:=trim(nom);
    if minilabel then begin
      miniok:=true;
      if copy(nom,1,6)='DOMES ' then miniok:=false;
      if copy(nom,1,5)='DOME ' then miniok:=false;
      if copy(nom,1,6)='DORSA ' then miniok:=false;
      if copy(nom,1,5)='RIMA ' then miniok:=false;
      let:=trim(copy(nom,length(nom)-1,2));
      if miniok and(length(let)=1)and(let>='A')and(let<='Z') then begin
         nom:=let;
      end else begin
        if w<(wmin) then continue;
      end;
    end;
    world2window(x,y,xx,yy);
    {$IFDEF opengl}
    if useOpenGL then begin
    if (xx>0)and(yy>0)and(xx<form1.GLSceneViewer1.Width)and(yy<form1.GLSceneViewer1.Height)
     and((currenteyepiece=0)or( sqrt(Intpower(form1.GLSceneViewer1.Width/2-xx,2)+Intpower(form1.GLSceneViewer1.Height/2-yy,2))<0.475*form1.GLSceneViewer1.Width ))then begin
      with form1.DummyCube4.Children[curlab] as TGLHUDText do begin
       Visible:=true;
       Position.Y:=yy;
       Position.X:=xx;
       if labelcenter then begin
          Text:=nom;
          Alignment:=taCenter;
       end else begin
          Text:='.'+nom;
          Alignment:=taLeftJustify;
       end;
      end;
      inc(curlab);
      if curlab>Maxlabel then break;
    end;
    end else
    {$ENDIF}
    begin
    if (xx>0)and(yy>0)and(xx<form1.image1.Width)and(yy<form1.image1.Height)
     and((currenteyepiece=0)or( sqrt(Intpower(form1.image1.Width/2-xx,2)+Intpower(form1.image1.Height/2-yy,2))<0.475*form1.image1.Width ))
     then with form1.image1.canvas do begin
      Font.Color:=AutoLabelColor;
      Font.name:='Arial';
      Font.Style:=[fsBold];
      Font.Height:=round(Label2dSize*Labelsize);
      Brush.style:=bsClear;
      ts:=TextExtent(nom);
      yy:=yy-ts.cy div 2;
      if labelcenter then begin
         xx:=xx-ts.cx div 2;
      end else begin
         xx:=xx+ts.cy div 4;
         nom:='-'+nom;
      end;
      textout(xx,yy,nom);
    end;
    end;
  end;
end;
end;
{$IFDEF opengl}
// Mark selection
cursprite:=0;
if currentselection<>'' then begin
dbm.Query('select LONGIN,LATIN from moon where '+
            currentselection+
            ' and LONGIN > '+formatfloat(d2,l-deltal)+
            ' and LONGIN < '+formatfloat(d2,l+deltal)+
            ' and LATIN > '+formatfloat(d2,b-deltab)+
            ' and LATIN < '+formatfloat(d2,b+deltab)+
            ' ORDER BY WIDEKM DESC '+
            ' ;');
for j:=0 to dbm.RowCount-1 do begin
  l1:=dbm.Results[j].Format[0].AsFloat;
  b1:=dbm.Results[j].Format[1].AsFloat;
  if projMoon(l1,b1,librl,librb,x,y) then begin
    world2window(x,y,xx,yy);
    if (xx>0)and(yy>0)and(xx<form1.GLSceneViewer1.Width)and(yy<form1.GLSceneViewer1.Height)
     and((currenteyepiece=0)or( sqrt(Intpower(form1.GLSceneViewer1.Width/2-xx,2)+Intpower(form1.GLSceneViewer1.Height/2-yy,2))<0.475*form1.GLSceneViewer1.Width ))then begin
      with form1.DummyCube5.Children[cursprite] as TGLHUDSprite do begin
       Width:=marksize;
       Height:=marksize;
       Visible:=true;
       Position.Y:=yy;
       Position.X:=xx;
      end;
      inc(cursprite);
      if cursprite>=MaxSprite then
           if cursprite<AbsoluteMaxSprite then MoreSprite
               else break;
    end;  
  end;
end;
end;
//
if useopengl then for i:=curlab to Maxlabel do
  with form1.DummyCube4.Children[i] as TGLHUDText do visible:=false;
if useopengl then for i:=cursprite to MaxSprite-1 do
  with form1.DummyCube5.Children[i] as TGLHUDSprite do visible:=false;
{$ENDIF}
end;

Procedure ClearLabel;
{$IFDEF opengl}
var i:integer;
{$ENDIF}
begin
{$IFDEF opengl}
if useopengl then begin
if (form1.DummyCube4.Count>0) and form1.DummyCube4.Children[0].visible then
   for i:=0 to Maxlabel do
      with form1.DummyCube4.Children[i] as TGLHUDText do visible:=false;
if (form1.DummyCube5.Count>0) and form1.DummyCube5.Children[0].visible then
   for i:=0 to MaxSprite-1 do
      with form1.DummyCube5.Children[i] as TGLHUDSprite do visible:=false;
end;
{$ENDIF}
end;

Procedure RefreshLabel;
begin
  ClearLabel;
  form1.LabelTimer.enabled:=false;
  form1.LabelTimer.enabled:=true;
end;

Procedure Mark(x,y:double; txt:string);
var xx,yy : integer;
begin
with form1 do begin
marked:=false;
markx:=x;
marky:=y;
{$IFDEF opengl}
if useOpenGL then begin
  shapepositionX:=x;
  shapepositionY:=y;
  hudsprite1.Width:=marksize;
  hudsprite1.Height:=marksize;
  if (x=0)and(y=0) then begin
     hudsprite1.Visible:=false;
     hudtext1.Visible:=false;
  end else begin
  form1.glscene1.BeginUpdate;
  world2window(x,y,xx,yy);
  marked:=true;
  markname:=txt;
  if showmark then begin
     hudsprite1.Visible:=true;
     hudsprite1.Material.FrontProperties.Emission.AsWinColor:=MarkColor;
     hudsprite1.Position.X:=xx;
     hudsprite1.Position.Y:=yy;
  end else hudsprite1.Visible:=false;
  if showlabel then begin
     hudtext1.Visible:=true;
     hudtext1.Position.Y:=yy;
     hudtext1.Text:=txt;
     if xx<(glsceneviewer1.Width/2) then begin
        hudtext1.Position.X:=xx+4*BitmapFont1.HSpace;
        hudtext1.Alignment:=taLeftJustify;
     end else begin
        hudtext1.Position.X:=xx-4*BitmapFont1.HSpace;
        hudtext1.Alignment:=taRightJustify;
     end;
  end else hudtext1.Visible:=false;
  form1.glscene1.EndUpdate;
  end;
end else
{$ENDIF}
  begin
  shapepositionX:=x;
  shapepositionY:=y;
  if (x=0)and(y=0) then begin
    shape1.Visible:=false;
    label5.Visible:=false;
  end else begin
  marked:=true;
  markname:=txt;
  world2window(x,y,xx,yy);
  if showmark then with image1.canvas do begin
     shape1.Visible:=true;
     shape1.BringToFront;
     Shape1.Left:=xx-shape1.width div 2;
     Shape1.Top:=yy-shape1.height div 2; 
  end else shape1.Visible:=false;
  if showlabel then begin
     label5.Visible:=true;
     label5.BringToFront;
     Label5.Caption:=txt;
     Label5.Font.Color:=LabelColor;
     Label5.Left:=xx+label5.Height;
     Label5.Top:=yy-(label5.Height div 2);
  end else label5.Visible:=false;
end;
end;
end;
end;

Procedure LibrationMark(librl,librb:double);
var a,x,y :double;
    xx,yy : integer;
begin
{$IFDEF opengl}
if useopengl then with form1.Arrowline1 do
 if ShowLibrationMark and(not form1.ToolButton3.Down) then begin
  visible:=true;
  a:=arctan2(librb,-librl);
  x:=sqrt(librb*librb+librl*librl);
  height:=x*0.004/zoom;
  position.X:=(0.5+height/1.5)*cos(a);
  position.Y:=(0.5+height/1.5)*sin(a);
  TopRadius:=height/10;
  BottomRadius:=TopRadius;
  TopArrowHeadRadius:=3*TopRadius;
  TopArrowHeadHeight:=6*TopRadius;
  Turnangle:=-rad2deg*a;
  Material.FrontProperties.Emission.AsWinColor:=MarkColor;
end else begin
  visible:=false;
end else
{$ENDIF}
with form1 do
 if ShowLibrationMark then begin
  shape2.visible:=true;
  shape2.bringtofront;
  a:=arctan2(librb,-librl);
  X:=0.51*cos(a);
  Y:=0.51*sin(a);
  world2window(x,y,xx,yy);
  Shape2.Left:=xx-shape2.width div 2;
  Shape2.Top:=yy-shape2.height div 2;
  end else begin
  Shape2.visible:=false;
end;
end;

Procedure Refresh2DImage;
var x1,y1,x2,y2,dx,dy,x0,y0: integer;
begin
if zoom=0 then exit;
with form1 do begin
dx:=round(image2d.width/zoom/2);
x0:=image2d.width-dx-dx;
dy:=round(image2d.height/zoom/2);
y0:=image2d.height-dy-dy;
x1:=round(HorzScrollBar.Position*bxpos+posmin)-dx;
if x1<0 then x1:=0;
if x1>x0 then x1:=x0;
y1:=round(VertScrollBar.Position*bxpos+posmin)-dy;
if y1<0 then y1:=0;
if y1>y0 then y1:=y0;
x2:=x1+dx+dx;
y2:=y1+dy+dy;
ax:=x1;
ay:=y1;
bx:=2*dx/Image1.width;
by:=2*dy/Image1.height;
Image1.picture.bitmap.canvas.CopyMode:=cmSrcCopy;
Image1.picture.bitmap.canvas.draw(0,0,eyepiecemask); 
Image1.picture.bitmap.canvas.CopyMode:=cmSrcAnd;
Image1.picture.bitmap.canvas.CopyRect(rect(0,0,Image1.width,Image1.height),image2d.Canvas,rect(x1,y1,x2,y2));
Image1.picture.bitmap.canvas.CopyMode:=cmSrcCopy;
Mark(shapePositionX,shapePositionY,label5.Caption);
LibrationMark(librlong,librlat);
RefreshLabel;
Image1.Refresh;
end;
MemoryDebug;
end;

{$IFDEF opengl}
Procedure SetScrollBar(xx,yy:extended);
var x1,y1,s1,c1 : extended;
begin
  sincos(deg2rad*CameraOrientation,s1,c1);
  x1:=(xx*c1-yy*s1);
  y1:=(xx*s1+yy*c1);
  form1.scrollbar1.Position:=-round(x1 * 1000);
  form1.scrollbar2.Position:=-round(y1 * 1000);
end;
{$ENDIF}

procedure MoveCamera(x,y :double);
var xx,yy : integer;
begin
curx:=x;
cury:=y;
with form1 do
{$IFDEF opengl}
if useOpenGL then begin
  glscene1.BeginUpdate;
  HUDSprite2.Visible:=false;
//  GLSceneViewer1.Cursor:=crRetic;
  GLCamera1.Position.y:=y;
  GLCamera1.Position.x:=x;
  Annulus1.Position.x:=GLCamera1.Position.x;
  Annulus1.Position.y:=GLCamera1.Position.y;
  SetScrollBar(x,y);
  Mark(shapePositionX,shapePositionY,hudtext1.Text);
  RefreshLabel;
  glscene1.EndUpdate;
  MemoryDebug;
end else
{$ENDIF}
  begin
  if (x=0)and(y=0) then begin
  lockscrollbar:=true;
  HorzScrollBar.Position:=HorzScrollBar.max div 2;
  lockscrollbar:=true;
  VertScrollBar.Position:=VertScrollBar.max div 2;
  end else begin
  world2window(x,y,xx,yy);
  xx:=ax+round(bx*xx);
  yy:=ay+round(by*yy);
  lockscrollbar:=true;
  HorzScrollBar.Position:=round((xx-posmin)/bxpos);
  lockscrollbar:=true;
  VertScrollBar.Position:=round((yy-posmin)/bxpos);
  end;
  Refresh2dImage;
end;
end;

{$IFDEF opengl}
Procedure SetMinFilter(onoff:boolean);
var mi: TGLMinFilter;
    i: integer;
begin
if UseOpenGL then begin
 if onoff then begin
     mi:=miNearestMipmapNearest;
 end else begin
     mi:=miLinear;
 end;
 with form1 do begin
   GLScene1.BeginUpdate;
   for i:=0 to 8 do begin
     GLMaterialLibrary1.Materials[i].Material.Texture.MinFilter:=mi;
   end;
   GLScene1.EndUpdate;
   GLSceneViewer1.Update;
 end;
end;
end;

{$ENDIF}

Procedure DrawEyepiece;
var d:integer;
begin
image2d.Canvas.Brush.Style:=bsSolid;
image2d.Canvas.Brush.Color:=$00202020;
image2d.Canvas.FloodFill(1,1,clBlack,fsSurface);
with eyepiecemask do begin
pixelformat:=pfdevice;
Width:=form1.image1.width;
Height:=form1.image1.height;
Canvas.Brush.Color:=clBlack;
Canvas.FillRect(rect(0,0,width,height));
Canvas.Brush.Color:=clWhite;
d:=round(Width*(1-px*EyepieceRatio)/2);
Canvas.Ellipse(d,d,width-d,height-d);
end;
end;

Procedure ClearEyepiece;
begin
image2d.Canvas.Brush.Style:=bsSolid;
image2d.Canvas.Brush.Color:=clBlack;
image2d.Canvas.FloodFill(1,1,$00202020,fsSurface);
with eyepiecemask do begin
pixelformat:=pfdevice;
Width:=form1.image1.width;
Height:=form1.image1.height;
Canvas.Brush.Color:=clWhite;
Canvas.FillRect(rect(0,0,width,height));
end;
end;

Procedure SetZoom(yy:double);
var t : double;
    dx: integer;
begin
if maxfoc>0 then with form1 do begin
{$IFDEF opengl}
if useOpenGL then begin
 form1.glscene1.BeginUpdate;
 HUDSprite2.Visible:=false;
 t:=yy;
 if yy<25 then yy:=25;
// if yy<minfoc then yy:=minfoc;
 if yy>maxfoc then yy:=maxfoc;
 t:=yy/t;
 EyepieceRatio:=t;
 curfoc:=yy;
 GLCamera1.FocalLength:=yy;
 zoom:=GLCamera1.FocalLength/minfoc;
 if currenteyepiece>0 then begin
    annulus1.BottomInnerRadius:=t*0.5/zoom;
    statusbar1.Panels[3].Text:=m[43]+' '+formatfloat(d1,t*minfoc*(diam/60)/yy)+lmin;
 end else
    statusbar1.Panels[3].Text:=m[43]+' '+formatfloat(d1,100*(diam/60)/yy)+lmin;
 locktrackbar:=true;
 trackbar1.position:=round(100*log10(GLCamera1.FocalLength));
 Mark(shapePositionX,shapePositionY,hudtext1.Text);
 LibrationMark(librlong,librlat);
 RefreshLabel;
 form1.glscene1.EndUpdate;
 MemoryDebug;
end else
{$ENDIF}
begin
 t:=yy;
// if yy<25 then yy:=25;
 if yy<=minfoc then yy:=minfoc;
 if yy>maxfoc then yy:=maxfoc;
 t:=yy/t;
 EyepieceRatio:=t;
 curfoc:=yy;
 zoom:=yy/minfoc;
 dx:=round(image2d.width/zoom/2);
 posmin:=dx;
 posmax:=HorzScrollBar.max-dx;
 bxpos:=maxintvalue([1,posmax-posmin])/HorzScrollBar.max;
 if currenteyepiece>0 then begin
    statusbar1.Panels[3].Text:=m[43]+' '+formatfloat(d1,t*minfoc*(diam/60)/yy)+lmin;
 end else
    statusbar1.Panels[3].Text:=m[43]+' '+formatfloat(d1,1.05*minfoc*(diam/60)/yy)+lmin;
 locktrackbar:=true;
 trackbar1.position:=round(100*log10(yy));
 if zoom=1 then begin
    lockscrollbar:=true;
    HorzScrollBar.Position:=HorzScrollBar.Max div 2;
    lockscrollbar:=true;
    VertScrollBar.Position:=VertScrollBar.Max div 2;
 end;   
 Refresh2DImage;
end;
end;
end;

procedure Window2World(x,y : integer; var xx,yy : double);
{$IFDEF opengl}
var k,s1,c1,x1,y1:extended;
{$ENDIF}
begin
with  form1 do begin
{$IFDEF opengl}
if useOpenGL then begin
  k:=(zoom*(minfoc/100)*GLSceneViewer1.width);
  sincos(deg2rad*CameraOrientation,s1,c1);
  x1:=flipx*(x-GLSceneViewer1.width/2);
  y1:=(y-GLSceneViewer1.height/2);
  xx:=(x1*c1+y1*s1);
  yy:=(-x1*s1+y1*c1);
  xx:=GLCamera1.Position.x-xx/k;
  yy:=GLCamera1.Position.y-yy/k;
end else
{$ENDIF}
begin
  x:=ax+round(bx*x);
  y:=ay+round(by*y);
  if poleorientation>0 then begin
     y:=image2d.Height-y;
     x:=image2d.Width-x;
  end;
  if flipx<0 then x:=image2d.Width-x;
  xx:=-((x-ox*image2d.Width/os)/(px*image2d.Width)-0.5);
  yy:=-((y-oy*image2d.height/os)/(py*image2d.height)-0.5);
end;
end;
end;

procedure World2Window(xx,yy : double; var x,y : integer);
{$IFDEF opengl}
var k,s1,c1,x1,y1:extended;
{$ENDIF}
begin
with  form1 do begin
{$IFDEF opengl}
if useOpenGL then begin
  k:=(zoom*(minfoc/100)*GLSceneViewer1.width);
  sincos(deg2rad*CameraOrientation,s1,c1);
  xx:=k*(GLCamera1.Position.x-xx);
  yy:=k*(GLCamera1.Position.y-yy);
  x1:=(xx*c1-yy*s1);
  y1:=(xx*s1+yy*c1);
  x:=round(flipx*x1+GLSceneViewer1.width/2);
  y:=round(y1+GLSceneViewer1.height/2);
end else
{$ENDIF}
begin
if (bx=0)or(by=0) then begin
  x:=0;
  y:=0;
end else begin
  x:=round(ox*image2d.Width/os+(px*image2d.Width)*(0.5-xx));
  y:=round(oy*image2d.Height/os+(py*image2d.Height)*(0.5-yy));
  if flipx<0 then x:=image2d.Width-x;
  if poleorientation>0 then begin
     y:=image2d.Height-y;
     x:=image2d.Width-x;
  end;
  x:=round((x-ax)/bx);
  y:=round((y-ay)/by);
  end;
end;
end;
end;

Procedure InitDate;
var y,m,d,h,n,s,ms : word;
begin
decodedate(now,y,m,d);
decodetime(now,h,n,s,ms);
timezone:=gettimezone(now);
form1.annee.Value:=y;
form1.mois.Value:=m;
form1.jour.Value:=d;
form1.heure.Value:=h;
form1.minute.Value:=n;
form1.seconde.Value:=s;
form1.updown1.position:=y;
form1.updown2.position:=m;
form1.updown3.position:=d;
form1.updown4.position:=h;
form1.updown5.position:=n;
form1.updown6.position:=s;
dt_ut:=dtminusut(y);
CurYear:=y;
CurrentMonth:=m;
CurrentDay:=d;
Currenttime:=h+n/60+s/3600;
CurrentJD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime-timezone+DT_UT);
end;

Procedure SetJDDate;
var y,m,d : integer; h,n,s,ms : word;
    hh : double;
begin
djd(CurrentJD-(DT_UT/24)+1e-8,y,m,d,hh);
decodetime(hh/24,h,n,s,ms);
timezone:=gettimezone(encodedatetime(y,m,d,h,n,s,ms));
djd(CurrentJD+(timezone/24)-(DT_UT/24)+1e-8,y,m,d,hh);
decodetime(hh/24,h,n,s,ms);
form1.annee.Value:=y;
form1.mois.Value:=m;
form1.jour.Value:=d;
form1.heure.Value:=h;
form1.minute.Value:=n;
form1.seconde.Value:=s;
form1.updown1.position:=y;
form1.updown2.position:=m;
form1.updown3.position:=d;
form1.updown4.position:=h;
form1.updown5.position:=n;
form1.updown6.position:=s;
CurYear:=y;
CurrentMonth:=m;
CurrentDay:=d;
Currenttime:=h+n/60+s/3600;
end;

procedure SetDate(param : string);
var buf : string;
    p,yy,mm,dd,h,m,s : integer;
begin
buf:=trim(param);
p:=pos('-',buf);
if p=0 then exit;
yy:=strtoint(trim(copy(buf,1,p-1)));
buf:=copy(buf,p+1,999);
p:=pos('-',buf);
if p=0 then exit;
mm:=strtoint(trim(copy(buf,1,p-1)));
buf:=copy(buf,p+1,999);
p:=pos('T',buf);
if p=0 then exit;
dd:=strtoint(trim(copy(buf,1,p-1)));
buf:=copy(buf,p+1,999);
p:=pos(':',buf);
if p=0 then exit;
h:=strtoint(trim(copy(buf,1,p-1)));
buf:=copy(buf,p+1,999);
p:=pos(':',buf);
if p=0 then exit;
m:=strtoint(trim(copy(buf,1,p-1)));
buf:=copy(buf,p+1,999);
s:=strtoint(trim(buf));
CurYear:=yy;
CurrentMonth:=mm;
CurrentDay:=dd;
CurrentTime:=h+m/60+s/3600;
dt_ut:=dtminusut(CurYear);
CurrentJD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime-timezone+DT_UT);
SetJDDate;
end;

Procedure SetObs(param : string);
var buf : string;
    p : integer;
    s,la,lo : double;
begin
p:=pos('LAT:',param);
if p=0 then exit;
buf:=copy(param,p+4,999);
p:=pos('d',buf);
la:=strtofloat(copy(buf,1,p-1));
s:=sgn(la);
buf:=copy(buf,p+1,999);
p:=pos('m',buf);
la:=la+s*strtofloat(copy(buf,1,p-1))/60;
buf:=copy(buf,p+1,999);
p:=pos('s',buf);
la:=la+s*strtofloat(copy(buf,1,p-1))/3600;
p:=pos('LON:',param);
if p=0 then exit;
buf:=copy(param,p+4,999);
p:=pos('d',buf);
lo:=strtofloat(copy(buf,1,p-1));
s:=sgn(lo);
buf:=copy(buf,p+1,999);
p:=pos('m',buf);
lo:=lo+s*strtofloat(copy(buf,1,p-1))/60;
buf:=copy(buf,p+1,999);
p:=pos('s',buf);
lo:=lo+s*strtofloat(copy(buf,1,p-1))/3600;
p:=pos('TZ:',param);
if p>0 then begin
  buf:=copy(param,p+3,999);
  p:=pos('h',buf);
  if p=0 then Timezone:=strtofloat(trim(buf))
         else Timezone:=strtofloat(copy(buf,1,p-1));
end;
Obslatitude:=la;
Obslongitude:=lo;
InitObservatoire;
CurrentJD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime-timezone+DT_UT);
end;

Procedure SetSafeMode(pass:string);
var OldVal : LongInt;
begin
exitpassword:=uppercase(pass);
password:='';
safemode:=true;
with form1 do begin
ToolButton1.visible:=false;
ToolButton2.visible:=false;
Panel7.Visible:=false;
Notes.TabVisible:=false;
Notes1.visible:=false;
Reglage.TabVisible:=false;
{$ifdef opengl}
DataBase1.visible:=false;
NewWindowButton.visible:=false;
GroupBox4.visible:=false;
{$endif}
windowstate:=wsNormal;
top:=0;
left:=0;
width:=screen.width;
height:=screen.height;
end;
(* SystemParametersInfo (SPI_SCREENSAVERRUNNING, Word (True), @OldVal, 0); // turn off alt-tab and ctrl+esc with win98 *)
end;

Procedure ReadParam;
var i : integer;
    buf : string;
    x,y : double;
begin
try
i:=0;
while i <= param.count-1 do begin
if param[i]='-o' then begin    // observatory
     inc(i);
     buf:=trim(StringReplace(param[i],'"','',[rfReplaceAll]));
     setobs(buf);
end
else if param[i]='-d' then begin    // date
     inc(i);
     buf:=trim(StringReplace(param[i],'"','',[rfReplaceAll]));
     setdate(buf);
end
else if param[i]='-c' then begin   // center coordinates
     inc(i);
     buf:=trim(StringReplace(param[i],'"','',[rfReplaceAll]));
     x:=strtofloat(buf);
     inc(i);
     buf:=trim(StringReplace(param[i],'"','',[rfReplaceAll]));
     y:=strtofloat(buf);
     MoveCamera(x,y);
end
else if param[i]='-f' then begin   // focal
     inc(i);
     buf:=trim(StringReplace(param[i],'"','',[rfReplaceAll]));
     x:=strtofloat(buf);
     setzoom(x);
end
else if param[i]='-n' then begin  // center object
     inc(i);
     buf:=trim(StringReplace(param[i],'"','',[rfReplaceAll]));
     Firstsearch:=true;
     SearchName(buf,true);
end
else if param[i]='-s' then begin   // mark selection
     inc(i);
     {$ifdef vmapro}
     currentselection:=trim(StringReplace(param[i],'"','',[rfReplaceAll]));
     if currentselection='*' then currentselection:='';
     {$endif}
end
else if param[i]='-3d' then begin     // full globe
        form1.ToolButton3.Down:=true;
        form1.ToolButton3Click(form1);
        lrot:=0;
        inclination:=0;
end
{$ifdef opengl}
else if param[i]='-r' then begin   // full globe at give rotation
     inc(i);
     buf:=trim(StringReplace(param[i],'"','',[rfReplaceAll]));
     x:=strtofloat(buf);
     if x<>0 then begin
        form1.ToolButton3.Down:=true;
        form1.ToolButton3Click(form1);
        lrot:=x;
        inclination:=0;
        form1.GLSceneViewer1MouseMove(nil,[ssLeft],lastx,lasty);
     end;
end
{$endif}
else if param[i]='-safe' then begin  // safe mode
     inc(i);
     if (i<=param.count-1) then buf:=trim(StringReplace(param[i],'"','',[rfReplaceAll]))
                           else buf:='';
     SetSafeMode(buf);
end
else if param[i]='--' then begin   // last parameter
     break;
end;
inc(i);
end;
except
end;
end;

procedure LoadSwitchToThisWindow;
begin
 SwitchToThisWindow:=nil;
 user32dll:=LoadLibrary(Pchar('user32.dll'));
 if user32dll<>0 then begin
    SwitchToThisWindow := TSwitchToThisWindow(GetProcAddress(user32dll, 'SwitchToThisWindow'));
 end;
end;

function NewWindowProc(WindowHandle : hWnd;
                       TheMessage   : Dword;
                       ParamW       : LongInt;
                       ParamL       : LongInt) : LongInt stdcall;
var
    buf : string;
    f : textfile;
begin
  if TheMessage = MyMsg  then begin
    try
   {Tell the application to restore, let it restore the form}
    SendMessage(Application.handle, WM_SYSCOMMAND, SC_RESTORE, 0);
    if @SwitchToThisWindow=nil then
       SetForegroundWindow(Application.Handle)
    else
       SwitchToThisWindow(Application.Handle,true);
    application.ProcessMessages;
    if (ParamW > 0)and(not multi_instance) then begin
       chdir(appdir);
       filemode:=0;
       assignfile(f,'@@param@@');
       reset(f);
       param.Clear;
       while not eof(f) do begin
          readln(f,buf);
          param.add(buf);
       end;
       Closefile(f);
       deletefile('@@param@@');
       ReadParam;
       RefreshMoonImage;
       if currentname<>'' then begin
          firstsearch:=true;
          searchname(currentname,false);
       end;
    end;
    finally
   {We handled the message - we are done}
    Result := 0;
    end;
    exit;
  end;
  if TheMessage = ExitRequest  then begin
     Form1.Close;
  end;
 {Call the original winproc}
  Result := CallWindowProc(OldWindowProc,
                           WindowHandle,
                           TheMessage,
                           ParamW,
                           ParamL);
end;

{$IFDEF opengl}
Procedure SetCameraOrientation;
var c1,s1 : extended;
begin
sincos(deg2rad*CameraOrientation,s1,c1);
form1.HUDSprite2.Visible:=false;
form1.GLCamera1.BeginUpdate;
form1.GLCamera1.Up.X:=s1;
form1.GLCamera1.Up.Y:=c1;
form1.GLCamera1.Up.X:=s1;
form1.GLCamera1.Up.Y:=c1;
form1.GLCamera1.Up.X:=s1;
form1.GLCamera1.Up.Y:=c1;
form1.GLCamera1.EndUpdate;
SetScrollBar(form1.GLCamera1.Position.x,form1.GLCamera1.Position.y);
Mark(shapePositionX,shapePositionY,form1.hudtext1.Text);
end;
{$ENDIF}

Procedure RotateBitmap(var bmp:Tbitmap);
var i,j : integer;
    p1,p2 : Pbytearray;
    b : Tbitmap;
begin
b:=tbitmap.create;
try
 b.Assign(bmp);
 for i:=0 to bmp.Height-1 do begin
    p2:=bmp.ScanLine[i];
    p1:=b.ScanLine[bmp.Height-1-i];
    for j:=0 to bmp.Width-1 do begin
      p2[3*j]  :=p1[3*(bmp.Width-1-j)];
      p2[3*j+1]:=p1[3*(bmp.Width-1-j)+1];
      p2[3*j+2]:=p1[3*(bmp.Width-1-j)+2];
    end;
 end;
 b.free;
except
 b.free;
 PoleOrientation:=0;
 showmessage('Not enough memory! Use smaller textures.');
end;
end;

Procedure MirrorBitmap(var bmp:Tbitmap);
var i,j : integer;
    p1,p2 : Pbytearray;
    b : Tbitmap;
begin
b:=tbitmap.create;
try
 b.Assign(bmp);
 for i:=0 to bmp.Height-1 do begin
    p1:=b.ScanLine[i];
    p2:=bmp.ScanLine[i];
    for j:=0 to bmp.Width-1 do begin
      p2[3*j]  :=p1[3*(bmp.Width-1-j)];
      p2[3*j+1]:=p1[3*(bmp.Width-1-j)+1];
      p2[3*j+2]:=p1[3*(bmp.Width-1-j)+2];
    end;
 end;
 b.free;
except
 b.free;
 flipx:=1;
 form1.CheckBox2.Checked:=false;
 showmessage('Not enough memory! Use smaller textures.');
end;
end;

Procedure SubstractBitmap(var bmp,mask:Tbitmap);
var i,j : integer;
    p1,p2 : Pbytearray;
begin
 for i:=0 to bmp.Height-1 do begin
    p1:=mask.ScanLine[i];
    p2:=bmp.ScanLine[i];
    for j:=0 to bmp.Width-1 do begin
      p2[3*j]  :=maxintvalue([0,p2[3*j]-p1[3*j]]);
      p2[3*j+1]:=maxintvalue([0,p2[3*j+1]-p1[3*j+1]]);
      p2[3*j+2]:=maxintvalue([0,p2[3*j+2]-p1[3*j+2]]);
    end;
 end;
end;

Procedure Load2DTexture(fn:string);
var j : Tjpegimage;
    savecursor:Tcursor;
begin
savecursor:=screen.Cursor;
screen.Cursor:=crHourGlass;
j:=TJpegImage.Create;
try
  with form1 do begin
//  if (fn='moon_a.jpg')and fileexists(Slash(appdir)+Slash('textures')+'moon_a_hi.jpg') then fn:='moon_a_hi.jpg';
  j.LoadFromFile(Slash(appdir)+Slash('textures')+fn);
  image2d.canvas.copymode:=cmSrcCopy;
  if reducetexture>1 then begin
     image2d.Width:=j.Width div reducetexture;
     image2d.Height:=j.Height div reducetexture;
     image2d.Canvas.StretchDraw(rect(0,0,image2d.Width,image2d.Height),j);
  end else begin
     image2d.Width:=j.Width;
     image2d.Height:=j.Height;
     image2d.Canvas.Draw(0,0,j);
  end;
  image2d.pixelformat:=pf24bit;
  if PoleOrientation>0 then RotateBitmap(image2d);
  if CheckBox2.Checked then MirrorBitmap(image2d);
  image1.Transparent:=false;
  image1.Stretch:=true;
  image1.Proportional:=false;
  image2d.SaveToFile(tmpmap);
  HorzScrollBar.Min:=0;
  HorzScrollBar.Max:=image2d.Width;
  HorzScrollBar.SmallChange:=HorzScrollBar.Max div 100;
  HorzScrollBar.LargeChange:=HorzScrollBar.Max div 10;
  VertScrollBar.Min:=0;
  VertScrollBar.Max:=image2d.Height;
  VertScrollBar.SmallChange:=VertScrollBar.Max div 100;
  VertScrollBar.LargeChange:=VertScrollBar.Max div 10;
  currenttexture:=fn;
  end;
  j.free;
  screen.Cursor:=savecursor;
except
  j.free;
  screen.Cursor:=savecursor;
  showmessage('Not enough memory! Use smaller textures.');
end;
end;

procedure TForm1.LoadOverlay(fn:string; lum:integer);
{$ifdef opengl}
var i,n,ws,hs,x0,y0 : integer;
    b : Tbitmap;
    j : Tjpegimage;
    p: Pbytearray;
    src,dest: Trect;
    max :double;
const slice: array[1..8] of integer = (6,1,2,5,8,3,4,7);
{$endif}
begin
{$ifdef opengl}
if showoverlay and fileexists(Slash(appdir)+Slash('textures')+Slash('overlay')+fn) then begin
   j:=TJpegImage.Create;
   b:=Tbitmap.Create;
   try
   j.LoadFromFile(Slash(appdir)+Slash('textures')+Slash('overlay')+fn);
   overlayimg.assign(j);
   if (lum<>0) then begin
    max:=(255-lum)/255;
    for i:=0 to overlayimg.height-1 do begin
     p:=overlayimg.scanline[i];
     for n:=0 to overlayimg.width-1 do begin
       case overlayimg.pixelformat of
         pf8bit  : p[n]:=lum+trunc(p[n]*max);
         pf24bit : begin
                   p[3*n]:=lum+trunc(p[3*n]*max);
                   p[3*n+1]:=lum+trunc(p[3*n+1]*max);
                   p[3*n+2]:=lum+trunc(p[3*n+2]*max);
                   end;
       end;
     end;
    end;
   end;
   ws:=overlayimg.Width div 4;
   hs:=overlayimg.Height div 2;
   b.Width:=ws;
   b.Height:=hs;
   for i:=1 to 8 do begin
     n:=slice[i]+8;
     if i<=4 then begin
        x0:=(i-1)*ws;
        y0:=0;
     end else begin
        x0:=(i-5)*ws;
        y0:=hs;
     end;
     dest:=rect(0,0,ws,hs);
     src:=rect(x0,y0,x0+ws,y0+hs);
     b.Canvas.CopyRect(dest,overlayimg.canvas,src);
     GLmaterialLibrary1.Materials[n].Material.Texture.Disabled:=false;
     GLmaterialLibrary1.Materials[n].Material.Texture.Image.Assign(b);
     GLMaterialLibrary1.Materials[n].TextureScale.X:=0.999;
     GLMaterialLibrary1.Materials[n].TextureScale.Y:=0.999;
   end;
   if fileexists(slash('Textures')+slash('overlay')+slash('caption')+overlayname) then begin
      OverlayCaption1.Caption:=remext(overlayname)+' '+'Caption';
      OverlayCaption2.Caption:=OverlayCaption1.Caption;
      OverlayCaption1.visible:=true;
      OverlayCaption2.visible:=true;
   end else begin
      OverlayCaption1.visible:=false;
      OverlayCaption2.visible:=false;
   end;
   finally
   j.free;
   b.free;
   end;
end else begin
   showoverlay:=false;
   OverlayCaption1.visible:=false;
   OverlayCaption2.visible:=false;
   for i:=1 to 8 do begin
     n:=slice[i]+8;
     GLmaterialLibrary1.Materials[n].Material.Texture.Disabled:=true;
   end;  
end;
{$endif}
end;

procedure TForm1.InitGraphic(Sender: TObject);
var j : Tjpegimage;
    b : Tbitmap;
    fn: string;
  {$IFDEF opengl}
  Procedure LoadTexture(n,r:integer; img:string);
  begin
  GLmaterialLibrary1.Materials[n].Material.Texture.Disabled:=false;
  GLMaterialLibrary1.Materials[n].TextureScale.X:=0.999;
  GLMaterialLibrary1.Materials[n].TextureScale.Y:=0.999;
  j.LoadFromFile(Slash(appdir)+Slash('textures')+img);
  if r=1 then GLmaterialLibrary1.Materials[n].Material.Texture.Image.Assign(j)
  else begin
     b.Width:=j.Width div r;
     b.Height:=j.Height div r;
     b.Canvas.StretchDraw(rect(0,0,b.Width,b.Height),j);
     GLmaterialLibrary1.Materials[n].Material.Texture.Image.Assign(b);
  end;
  end;
  Procedure GrayTexture(n:integer);
  const gray=$00707070;
  begin
   GLmaterialLibrary1.Materials[n].Material.Texture.Disabled:=true;
   GLmaterialLibrary1.Materials[n].Material.FrontProperties.Ambient.AsWinColor:=gray;
   GLmaterialLibrary1.Materials[n].Material.FrontProperties.Diffuse.AsWinColor:=gray;
   GLmaterialLibrary1.Materials[n].Material.FrontProperties.Specular.AsWinColor:=gray;
  end;
  {$ENDIF}
begin
j:=TJpegImage.Create;
b:=Tbitmap.Create;
panel2d.left:=0;
panel2d.top:=form1.ToolBar1.Height;
panel2d.Height:=form1.ClientHeight-form1.ToolBar1.Height;
panel2d.width:=panel2d.height;
panel3.Width:=panel2d.Width-vertscrollbar.width;
panel3.height:=panel2d.Height-horzscrollbar.height;
lockrot:=true;
if PoleOrientation=0 then RadioGroup2.ItemIndex:=0 else RadioGroup2.ItemIndex:=1;
lockrot:=false;
{$IFDEF opengl}
if useOpenGL then begin
  panel2.Visible:=true;
  panel2d.Visible:=false;
  image1.Picture.Bitmap.assign(nil);
  InitLabel;
  InitSprite;
  GLSceneViewer1.Cursor:=crRetic;
  try
  OpenHires(false);
  except
  end;
  LoadTexture(0,reducetexture,'moon'+imgsuffix+'_c1.jpg');
  LoadTexture(1,reducetexture,'moon'+imgsuffix+'_c2.jpg');
  LoadTexture(2,reducetexture,'moon'+imgsuffix+'_c3.jpg');
  LoadTexture(3,reducetexture,'moon'+imgsuffix+'_c4.jpg');
  if farsidetexture then LoadTexture(4,reducetexturefar,'moon'+imgsuffix+'_c5.jpg')
                    else GrayTexture(4);
  if farsidetexture then LoadTexture(5,reducetexturefar,'moon'+imgsuffix+'_c6.jpg')
                    else GrayTexture(5);
  if farsidetexture then LoadTexture(6,reducetexturefar,'moon'+imgsuffix+'_c7.jpg')
                    else GrayTexture(6);
  if farsidetexture then LoadTexture(7,reducetexturefar,'moon'+imgsuffix+'_c8.jpg')
                    else GrayTexture(7);
  minfoc:=95;
  maxfoc:=round(maxfocbase*GLmaterialLibrary1.Materials[0].Material.Texture.Image.height/1024);
  trackbar1.Min:=round(100*log10(minfoc));
  trackbar1.Max:=round(100*log10(maxfoc));
  trackbar1.position:=round(100*log10(minfoc));
//  CameraOrientation:=PoleOrientation;
  checkbox1.Checked:=FollowNorth;
  SetCameraOrientation;

  Reglage.TabVisible:=true;
  checkbox19.Visible:=false;
  label11.visible:=true;
  trackbar3.visible:=true;
  label12.visible:=true;
  trackbar4.visible:=true;
  panel8.visible:=true;
  button14.visible:=true;
  groupbox2.visible:=true;

  Outils.TabVisible:=true;
  Button12.visible:=true;
  Button13.visible:=true;
  checkbox1.visible:=true;
  Distance1.Visible:=true;
  Enregistrersous1.Visible:=true;
  bitmapfont1.Glyphs.LoadFromFile(Slash(appdir)+Slash('textures')+'font_label_bold.bmp');
  HUDText1.ModulateColor.AsWinColor:=labelcolor;
  HUDText1.Scale.SetVector(Label3dSize*LabelSize,Label3dSize*LabelSize,1);
  Trackbar2.position:=GLlightSource1.ambient.AsWinColor and $FF;
  Trackbar3.position:=GLlightSource1.diffuse.AsWinColor and $FF;
  Trackbar4.position:=GLlightSource1.specular.AsWinColor and $FF;
  if sphere1.Slices=180 then Trackbar5.position:=3
  else if sphere1.Slices=90 then Trackbar5.position:=2
  else if sphere1.Slices=45 then Trackbar5.position:=1
  else Trackbar5.position:=1;
  CheckBox3.checked:=doublebuf;
  CheckBox4.checked:=stencilbuf;
  CheckBox5.Checked:=farsidetexture;
  CheckBox8.Checked:=compresstexture;
  SetMinFilter(MipMaps);
  SetMirror(checkbox2.checked);
  LoadOverlay(overlayname,overlaylum);
end else
{$ENDIF}
begin
  InitLabel;
  image1.Cursor:=crRetic;
  Image1.picture.bitmap.Width:=panel3.Width;
  Image1.picture.bitmap.Height:=panel3.height;
  Image1.width:=panel3.Width;
  Image1.height:=panel3.height;
  panel2.Visible:=false;
  panel2d.Visible:=true;
  lastscrollx:=0;
  lastscrolly:=0;
  if GeologicalMap then begin
                        LgendeGologique1.Visible:=true;
                        fn:='moon_full_geology.jpg'
                        end
                   else fn:='moon_a.jpg';
  Load2DTexture(fn);
  HorzScrollBar.min:=0;
  HorzScrollBar.max:=image2d.Width;
  VertScrollBar.min:=0;
  VertScrollBar.max:=image2d.Height;
  minfoc:=100;
  maxfoc:=round((image2d.Height/1500)*500);
  if maxfoc>500 then maxfoc:=500;
  trackbar1.Min:=round(100*log10(minfoc));
  trackbar1.Max:=round(100*log10(maxfoc));
  trackbar1.position:=round(100*log10(minfoc));
  Trackbar2.position:=phaseumbra and $FF;
  checkbox19.checked:=phasehash;
  Reglage.TabVisible:=true;
  checkbox19.Visible:=true;
  label11.visible:=false;
  trackbar3.visible:=false;
  label12.visible:=false;
  trackbar4.visible:=false;
  panel8.visible:=false;
  button14.visible:=false;
  groupbox2.visible:=false;

  Outils.TabVisible:=true;
  Distance1.Visible:=true;
  Button12.visible:=false;
  Button13.visible:=false;
  checkbox1.visible:=false;
  BMP15001.visible:=false;
  BMP30001.visible:=false;
  label5.Font.Height:=round(Label2dSize*Labelsize);
end;
j.free;
b.Free;
end;

Procedure SetRotateFont(prtresol:integer);
begin
// prepare rotated font
rotatefontresol:=prtresol;
FillChar(lf, SizeOf(lf), Byte(0));
lf.lfHeight := round(-75*prtresol/300);
lf.lfEscapement := 10 * 90; // Nombre de degrés de rotation
lf.lfOrientation := 10 * 90;
lf.lfCharSet := DEFAULT_CHARSET;
StrCopy(lf.lfFaceName, 'Arial');
lf.lfWeight:=1000;
rotatefont := TFont.Create;
rotatefont.Handle := CreateFontIndirect(lf);
end;

procedure TForm1.SetPath;
{$ifdef mswindows}
var PIDL : PItemIDList;
    Folder : array[0..MAX_PATH] of Char;
const CSIDL_APPDATA = $001A;
{$endif}
begin
  appdir:=getcurrentdir;
{$ifdef mswindows}
  SHGetSpecialFolderLocation(0, CSIDL_APPDATA, PIDL);
  SHGetPathFromIDList(PIDL, Folder);
  privatedir:=slash(Folder)+'virtualmoon';
{$endif}
{$ifdef unix}
  privatedir:=slash(ExpandFileName('~/.virtualmoon'));
{$endif}
  if not DirectoryExists(privatedir) then ForceDirectories(privatedir);
  ConfigFile:=slash(privatedir)+'virtualmoon.ini';
  if not DirectoryExists(Slash(privatedir)+'database') then ForceDirectories(Slash(privatedir)+'database');
end;

Procedure TForm1.FormCreate(Sender: TObject);
var tmpp : pchar;
    i : integer;
begin
SetPath;
skipresize:=true;
top:=0;
left:=0;
width:=screen.width;
height:=screen.height;
application.processmessages;
skipresize:=false;
{Register a custom windows message}
MyMsg := RegisterWindowMessage(IdMsg);
ExitRequest := RegisterWindowMessage(ExitMsg);
{Set form1's windows proc to ours and remember the old window proc}
OldWindowProc := Pointer(SetWindowLong(Form1.Handle,
                                       GWL_WNDPROC,
                                       LongInt(@NewWindowProc)));

searchlist:=Tstringlist.Create;
param:=Tstringlist.Create;
param.clear;
getmem(tmpp,255);
gettemppath(255,tmpp);
tmpdir:=tmpp;
freemem(tmpp,255);
tmpmap:=tmpdir+'$tmpmap.bmp';
image2d:=Tbitmap.Create;
eyepiecemask:=Tbitmap.Create;
hires:=Tbitmap.create;
hires500:=Tbitmap.create;
overlayhi:=Tbitmap.Create;
overlayimg:=Tbitmap.Create;
// hide developpement tools or not finished function
if not fileexists('version.developpement') then begin
  debuglabel.visible:=false;  // memory used
  dbtab.TabVisible:=false;    // edit database
  Enregistredist.Visible:=false; // record new object
  BMP15001.Visible:=false;   // save bmp for light version
  BMP30001.Visible:=false;   // save bmp for light version
  {$ifndef vmalight}
   label27.visible:=false;       //not finished  globe inclination
   speedbutton7.visible:=false;  //not finished  globe inclination
   trackbar6.visible:=false;     //not finished  globe inclination
  {$endif}
end;
// hide version specific function
{$ifdef vmalight}
  LibrationButton.Visible:=false;
{$endif}
{$ifdef vmabasic}
  ToolButton3.Visible:=false;
  GroupBox4.visible:=true;
{$endif}
{$ifdef vmaexpert}
  checkbox5.Visible:=false;
  GroupBox4.visible:=true;
  NewWindowButton.visible:=true;
{$endif}
{$ifndef vmapro}
  DataBase1.visible:=false;
  ButtonDatabase.visible:=false;
  OverlayCaption1.visible:=false;
  OverlayCaption2.visible:=false;
  NewWindowButton.visible:=false;
{$endif}
SetLang1;
readdefault;
Screen.Cursors[crRetic] := LoadCursor(HInstance,'RETIC');
Screen.Cursors[crPointing] := LoadCursor(HInstance,'POINTING');
currentid:='';
librl:=0;
librb:=0;
lrot:=0;
inclination:=0;
zoom:=1;
lastx:=0;
lasty:=0;
SkipIdent:=false;
dblox := TMlb2.Create;
dbnotes := TMlb2.Create;
GetSkyChartInfo;
CartesduCiel1.Visible := cieldir>'';
SetRotateFont(300);
InitGraphic(sender);
appname:=paramstr(0);
if paramcount>0 then begin
   for i:=1 to paramcount do begin
      param.Add(paramstr(i));
      if paramstr(i)='-safe' then borderstyle:=bsNone; // canot set this later in formshow
   end;
end;
{$ifdef opengl}
if multi_instance then begin
  NewWindowButton.visible:=false;
  Button15.visible:=false;
  Caption:=caption+'<2>';
end;
AsMultiTexture:=(GLSceneViewer1.Buffer.LimitOf[limNbTextureUnits]>1);
{$endif}
LoadSwitchToThisWindow;
end;

Procedure UpdTerminateur;
var interest,diam,i : integer;
    l1,l2:double;
    buf:string;
begin
with form1 do begin
listbox1.Clear;
if tphase<180 then begin
   l1:=tphase-90+librl;
   l2:=l1+12;
end else begin
   l2:=tphase-90+librl-180;
   l1:=l2-12;
end;
currentphase:=tphase;
diam:=strtointdef(combobox3.Text,5);
interest:=combobox2.itemindex+1;
try
listbox1.Items.BeginUpdate;
dbm.Query('select NAME,LATIN,INTERESTN,DIAMINST from moon '+
          ' where longin>'+formatfloat(d2,l1)+
          ' and longin<'+formatfloat(d2,l2)+
          ' and DBN in ('+sidelist+')'+
          ' and INTERESTN >='+inttostr(interest)+
          ' and DIAMINST <='+inttostr(diam) );
for i:=0 to dbm.rowcount-1 do begin
     case RadioGroup1.ItemIndex of
     0 : listbox1.Items.Add(dbm.Results[i][0]);
     1 : listbox1.Items.Add(inttostr(5-dbm.Results[i].format[2].asinteger)+' '+dbm.Results[i][0]);
     2 : listbox1.Items.Add(formatfloat('000',dbm.Results[i].format[3].asfloat)+' '+dbm.Results[i][0]);
     3 : begin
         buf:=demtostr(dbm.Results[i].format[1].asfloat)+' '+dbm.Results[i][0];
         listbox1.Items.Add(buf);
         end;
     end;
end;
finally
listbox1.Items.EndUpdate;
if listbox1.Items.Count=0 then listbox1.Items.Add(m[27]);
end;
end;
end;

Procedure Tform1.ListObject(delta: double);
var l,b,deltal: double;
    i : integer;
begin
l:=currentl;
b:=currentb;
deltal:=delta/cos(deg2rad*b);
with imglist1 do begin
behavior:=formation;
imglist1.caption:=form1.ToolButton10.hint;
imglst.Clear;
dbm.query('select NAME from moon '+
            ' where DBN in ('+sidelist+')'+
            ' and LONGIN > '+formatfloat(d2,l-deltal)+
            ' and LONGIN < '+formatfloat(d2,l+deltal)+
            ' and LATIN > '+formatfloat(d2,b-delta)+
            ' and LATIN < '+formatfloat(d2,b+delta)+
            ' ;');
for i:=0 to dbm.RowCount-1 do begin
   imglst.items.add(dbm.Results[i][0]);
end;
//if not imglist.visible then FormPos(imglist,mouse.cursorpos.x,mouse.cursorpos.y);
if not imglist1.visible then FormPos(imglist1,PageControl1.Clienttoscreen(point(0,0)).x ,PageControl1.Clienttoscreen(point(0,0)).y);
imglist1.Show;
end;
end;

function Tform1.SearchAtPos(l,b: double):boolean;
var mindist,d,l1,b1,deltab,deltal: double;
    rec,i : integer;
begin
mindist:=9999;
result:=false;
rec:=0;
deltab:=5;
deltal:=deltab/cos(deg2rad*b);
dbm.query('select ID,LONGIN,LATIN from moon '+
            ' where DBN in ('+sidelist+')'+
            ' and LONGIN > '+formatfloat(d2,l-deltal)+
            ' and LONGIN < '+formatfloat(d2,l+deltal)+
            ' and LATIN > '+formatfloat(d2,b-deltab)+
            ' and LATIN < '+formatfloat(d2,b+deltab)+
            ' ;');
for i:=0 to dbm.RowCount-1 do begin
  l1:=dbm.Results[i].format[1].AsFloat;
  b1:=dbm.Results[i].format[2].AsFloat;
  d:=angulardistance(l,b,l1,b1);
  if d<mindist then begin
     result:=true;
     mindist:=d;
     rec:=dbm.Results[i].Format[0].AsInteger;
  end;
end;
if result then begin
   currentid:=inttostr(rec);
   dbm.query('select * from moon where ID='+currentid+';');
   result:=dbm.RowCount>0;
end;   
end;

Procedure AddToList(buf:string);
var i: integer;
begin
with form1 do begin
i:=combobox1.Items.IndexOf(buf);
if i=-1 then i:=combobox1.DropDownCount-1;
combobox1.Items.Delete(i);
combobox1.Items.Insert(0,buf);
combobox1.ItemIndex:=0;
end;
end;

Procedure GetDetail(row:TResultRow; memo:Tmemo);
const b=' ';
var nom,carte,buf,buf2,txt : string;
    ok  : boolean;
    i,j:integer;
begin
with form1 do begin
  nom:=row.ByField['NAME'].AsString;
  memo.Lines.Add(nom);
  i:=row.ByField['DBN'].asInteger;
  if i>9 then begin
    buf:=m[75]+b+inttostr(i)+b;
    for j:=0 to form2.CheckListBox1.count-1 do
       if (form2.CheckListBox1.Items.Objects[j] as TDBinfo).dbnum=i then
          buf:=buf+form2.CheckListBox1.Items[j];
    memo.Lines.Add(buf);
  end;
  memo.Lines.Add(m[56]+b+row.ByField['TYPE'].AsString);
  memo.Lines.Add(m[49]+b+row.ByField['PERIOD'].AsString);
  memo.Lines.Add(m[57]); //Taille
  memo.Lines.Add(m[17]+b+row.ByField['LENGTHKM'].AsString+'x'+row.ByField['WIDEKM'].AsString+m[18]+b+'/'+b+row.ByField['LENGTHMI'].AsString+'x'+row.ByField['WIDEMI'].AsString+m[19]);
  buf:=row.ByField['HEIGHTM'].AsString;
  buf2:=row.ByField['HEIGHTFE'].AsString;
  if buf=buf2 then txt:=m[20]+b+buf  // inconnue
  else begin
     txt:=m[20]+b;
     val(buf,dummy,i);
     if i=0 then txt:=txt+buf+m[21]+b+'/'+b;
     val(buf2,dummy,i);
     if i=0 then txt:=txt+buf2+m[22];
  end;
  memo.Lines.Add(txt);
  memo.Lines.Add(m[23]+b+row.ByField['RAPPORT'].AsString);
//  memo.Lines.Add(b);
  memo.Lines.Add(m[58]); //Description
  if row.ByField['GENERAL'].AsString>'' then memo.Lines.Add(row.ByField['GENERAL'].AsString);
  if row.ByField['SLOPES'].AsString>'' then memo.Lines.Add(row.ByField['SLOPES'].AsString);
  if row.ByField['WALLS'].AsString>'' then memo.Lines.Add(row.ByField['WALLS'].AsString);
  if row.ByField['FLOOR'].AsString>'' then memo.Lines.Add(row.ByField['FLOOR'].AsString);
//  memo.Lines.Add(b);
  memo.Lines.Add(m[59]); //Observation
  memo.Lines.Add(m[24]+b+row.ByField['INTERESTC'].AsString);
  buf:=row.ByField['MOONDAYS'].AsString;
  buf2:=row.ByField['MOONDAYM'].AsString;
  if buf=buf2 then txt:=m[25]+b+buf
              else txt:=m[25]+b+buf+b+m[26]+b+buf2;
  memo.Lines.Add(txt);
  memo.Lines.Add(m[28]+b+row.ByField['PRINSTRU'].AsString);
//  memo.Lines.Add(b);
  memo.Lines.Add(m[60]); //Position
  memo.Lines.Add(m[10]+b+row.ByField['LONGIC'].AsString);
  memo.Lines.Add(m[11]+b+row.ByField['LATIC'].AsString);
  memo.Lines.Add(m[12]+b+row.ByField['QUADRANT'].AsString);
  memo.Lines.Add(m[13]+b+row.ByField['AREA'].AsString);
//  memo.Lines.Add(b);
  memo.Lines.Add(m[61]); //Atlas
  memo.Lines.Add(m[14]+b+row.ByField['RUKL'].AsString+' '+row.ByField['RUKLC'].AsString);
  buf:=row.ByField['VISCARDY'].AsString;
  if trim(buf)>'' then memo.Lines.Add(m[15]+b+buf);
  buf:=row.ByField['HATFIELD'].AsString ;
  if trim(buf)>'' then memo.Lines.Add(m[16]+b+buf);
  buf:=row.ByField['WESTFALL'].AsString ;
  if trim(buf)>'' then memo.Lines.Add(m[66]+b+buf);
  buf:=row.ByField['WOOD'].AsString ;
  if trim(buf)>'' then memo.Lines.Add(m[72]+b+buf);
  dblox.Gofirst;
  ok:=dblox.MatchData('NAME', '=', nom);
  if not ok then ok:=dblox.SeekData('NAME',  '=', nom);
  if ok then begin
    buf:=m[65];
    while ok do begin
      carte:=dblox.GetDATA('PLATE');
      buf:=buf+b+carte;
      ok:=dblox.SeekData('NAME',  '=', nom);
    end;
    memo.Lines.Add(buf);
  end;
//  memo.Lines.Add(b);
  memo.Lines.Add(m[62]); //Origine
  memo.Lines.Add(m[63]+b+row.ByField['NAMEDETAIL'].AsString);
  if (trim(row.ByField['WORK'].AsString+row.ByField['NATIONLITY'].AsString)>'')and(trim(row.ByField['CENTURYC'].AsString+row.ByField['COUNTRY'].AsString)>'') then begin
    case wordformat of
    0 : memo.Lines.Add(row.ByField['CENTURYC'].AsString+b+row.ByField['NATIONLITY'].AsString+b+row.ByField['WORK'].AsString+b+m[2]+b+row.ByField['COUNTRY'].AsString);
    1 : memo.Lines.Add(row.ByField['WORK'].AsString+b+row.ByField['NATIONLITY'].AsString+b+m[1]+b+row.ByField['CENTURYC'].AsString+b+m[2]+b+row.ByField['COUNTRY'].AsString);
    2 : memo.Lines.Add(row.ByField['NATIONLITY'].AsString+b+row.ByField['WORK'].AsString+b+row.ByField['CENTURYC'].AsString+b+m[2]+b+row.ByField['COUNTRY'].AsString);
    end;
    memo.Lines.Add(m[3]+b+row.ByField['BIRTHPLACE'].AsString+b+m[4]+b+row.ByField['BIRTHDATE'].AsString);
    memo.Lines.Add(m[5]+b+row.ByField['DEATHPLACE'].AsString+b+m[4]+b+row.ByField['DEATHDATE'].AsString);
  end;
  if (trim(row.ByField['FACTS'].AsString)<>'??')and(trim(row.ByField['FACTS'].AsString)<>'') then
    memo.Lines.Add(m[64]+b+row.ByField['FACTS'].AsString);
  memo.Lines.Add(m[6]+b+row.ByField['NAMEORIGIN'].AsString);
  memo.Lines.Add(m[7]+b+row.ByField['LANGRENUS'].AsString);
  memo.Lines.Add(m[8]+b+row.ByField['HEVELIUS'].AsString);
  memo.Lines.Add(m[9]+b+row.ByField['RICCIOLI'].AsString);
end;                                               
end;

Procedure GetHTMLDetail(row:TResultRow; var txt:string);
const b='&nbsp;';
      t1='<center><font size=+1 color="#0000FF"><b>';  t1end='</b></font></center>';
      t2='<font size=+1><b>';  t2end='</b></font>';
      t3='<b>' ; t3end='</b>';
var nom,carte,url,img,remoteurl,buf,buf2 : string;
    ok  : boolean;
    i,j:integer;
begin
with form1 do begin
  txt:='<html> <body bgcolor="white">';
  nom:=row.ByField['NAME'].AsString;
  dblox.Gofirst;
  ok:=dblox.MatchData('NAME', '=', nom);
  if not ok then ok:=dblox.SeekData('NAME',  '=', nom);
  txt:=txt+t1+nom+t1end+'<br>';
  i:=row.ByField['DBN'].asInteger;
  if i>9 then begin
    txt:=txt+t3+'From Database:'+t3end+b+inttostr(i)+b;
    for j:=0 to form2.CheckListBox1.count-1 do
       if (form2.CheckListBox1.Items.Objects[j] as TDBinfo).dbnum=i then
          txt:=txt+form2.CheckListBox1.Items[j]+'<br>';
  end;
  txt:=txt+t3+m[56]+t3end+b+row.ByField['TYPE'].AsString+'<br>';
  txt:=txt+t3+m[49]+t3end+b+row.ByField['PERIOD'].AsString+'<br>';
  txt:=txt+b+'<br>';
  txt:=txt+t2+m[57]+t2end+'<br>'; //Taille
  txt:=txt+t3+m[17]+t3end+b+row.ByField['LENGTHKM'].AsString+'x'+row.ByField['WIDEKM'].AsString+m[18]+b+'/'+b+row.ByField['LENGTHMI'].AsString+'x'+row.ByField['WIDEMI'].AsString+m[19]+'<br>';
  buf:=row.ByField['HEIGHTM'].AsString;
  buf2:=row.ByField['HEIGHTFE'].AsString;
  if buf=buf2 then txt:=txt+t3+m[20]+t3end+b+buf+'<br>'  // inconnue
  else begin
     txt:=txt+t3+m[20]+t3end+b;
     val(buf,dummy,i);
     if i=0 then txt:=txt+buf+m[21]+b+'/'+b;
     val(buf2,dummy,i);
     if i=0 then txt:=txt+buf2+m[22];
     txt:=txt+'<br>';
  end;
  txt:=txt+t3+m[23]+t3end+b+row.ByField['RAPPORT'].AsString+'<br>';
  txt:=txt+b+'<br>';
  txt:=txt+t2+m[58]+t2end+'<br>'; //Description
  if row.ByField['GENERAL'].AsString>'' then txt:=txt+row.ByField['GENERAL'].AsString+'<br>';
  if row.ByField['SLOPES'].AsString>'' then txt:=txt+row.ByField['SLOPES'].AsString+'<br>';
  if row.ByField['WALLS'].AsString>'' then txt:=txt+row.ByField['WALLS'].AsString+'<br>';
  if row.ByField['FLOOR'].AsString>'' then txt:=txt+row.ByField['FLOOR'].AsString+'<br>';
  txt:=txt+b+'<br>';
  txt:=txt+t2+m[59]+t2end+'<br>'; //Observation
  txt:=txt+t3+m[24]+t3end+b+row.ByField['INTERESTC'].AsString+'<br>';
  buf:=row.ByField['MOONDAYS'].AsString;
  buf2:=row.ByField['MOONDAYM'].AsString;
  if buf=buf2 then txt:=txt+t3+m[25]+t3end+b+buf+'<br>'
              else txt:=txt+t3+m[25]+t3end+b+buf+b+m[26]+b+buf2+'<br>';
  txt:=txt+t3+m[28]+t3end+b+row.ByField['PRINSTRU'].AsString+'<br>';
  txt:=txt+b+'<br>';
  txt:=txt+t2+m[60]+t2end+'<br>'; //Position
  txt:=txt+t3+m[10]+t3end+b+row.ByField['LONGIC'].AsString+'<br>';
  txt:=txt+t3+m[11]+t3end+b+row.ByField['LATIC'].AsString+'<br>';
  txt:=txt+t3+m[12]+t3end+b+row.ByField['QUADRANT'].AsString+'<br>';
  txt:=txt+t3+m[13]+t3end+b+row.ByField['AREA'].AsString+'<br>';
  txt:=txt+b+'<br>';
  txt:=txt+t2+m[61]+t2end+'<br>'; //Atlas
  // RUKL link
  carte:=row.ByField['RUKL'].AsString+' '+row.ByField['RUKLC'].AsString;
  img:=padzeros(row.ByField['RUKL'].AsString,2);
  url:=ruklprefix+img+ruklsuffix;
  if fileexists(url) then url:=' <A HREF="file://'+url+'">'+carte+'</A>'
                     else url:=carte;
  txt:=txt+t3+m[14]+t3end+b+url+'<br>';
  buf:=row.ByField['VISCARDY'].AsString;
  if trim(buf)>'' then txt:=txt+t3+m[15]+t3end+b+buf+'<br>';
  buf:=row.ByField['HATFIELD'].AsString ;
  if trim(buf)>'' then txt:=txt+t3+m[16]+t3end+b+buf+'<br>';
  buf:=row.ByField['WESTFALL'].AsString ;
  if trim(buf)>'' then txt:=txt+t3+m[66]+t3end+b+buf+'<br>';
  buf:=row.ByField['WOOD'].AsString ;
  if trim(buf)>'' then txt:=txt+t3+m[72]+t3end+b+buf+'<br>';
  if ok then begin
    txt:=txt+t3+m[65]+t3end;
    while ok do begin
      carte:=dblox.GetDATA('PLATE');
      if lopamdirect then begin
        img:=dblox.GetDATA('IMAGE');
        url:=lopamlocalurl+img+lopamlocalsuffix;
        remoteurl:=lopamdirecturl+img+lopamdirectsuffix;
        // on regarde d'abord en local
        if fileexists(url) then url:='file://'+url     // nouveau nom: iv_038_h3.jpg
          else begin
            img:=stringreplace(img,'_0','_',[]);
            img:=stringreplace(img,'_0','_',[]);
            url:=lopamlocalurl+img+lopamlocalsuffix;    // ancien nom: iv_38_h3.jpg
            if fileexists(url) then url:='file://'+url
               else begin
                 img:=uppercase(img);
                 img:=stringreplace(img,'I_','1-',[]);
                 img:=stringreplace(img,'II_','2-',[]);
                 img:=stringreplace(img,'III_','3-',[]);
                 img:=stringreplace(img,'IV_','4-',[]);
                 img:=stringreplace(img,'V_','5-',[]);
                 img:=stringreplace(img,'_','',[]);
                 url:=lopamlocalurl+img+lopamlocalsuffix;    // ancien nom: 4-38h3.jpg
                 if fileexists(url) then url:='file://'+url
                    else url:=remoteurl  // sinon on va chercher sur le site
               end;
          end;
      end else begin
        url:=lopamplateurl+carte+lopamplatesuffix;
      end;
      txt:=txt+b+' <A HREF="'+url+'">'+carte+'</A>';
      ok:=dblox.SeekData('NAME',  '=', nom);
    end;
    txt:=txt+'<br>';
  end;
  txt:=txt+b+'<br>';
  txt:=txt+t2+m[62]+t2end+'<br>'; //Origine
  txt:=txt+t3+m[63]+t3end+b+row.ByField['NAMEDETAIL'].AsString+'<br>';
  if (trim(row.ByField['WORK'].AsString+row.ByField['NATIONLITY'].AsString)>'')and(trim(row.ByField['CENTURYC'].AsString+row.ByField['COUNTRY'].AsString)>'') then begin
    case wordformat of
    0 : txt:=txt+row.ByField['CENTURYC'].AsString+b+row.ByField['NATIONLITY'].AsString+b+row.ByField['WORK'].AsString+b+m[2]+b+row.ByField['COUNTRY'].AsString+'<br>';        // english
    1 : txt:=txt+row.ByField['WORK'].AsString+b+row.ByField['NATIONLITY'].AsString+b+m[1]+b+row.ByField['CENTURYC'].AsString+b+m[2]+b+row.ByField['COUNTRY'].AsString+'<br>'; // francais, italian
    2 : txt:=txt+row.ByField['NATIONLITY'].AsString+b+row.ByField['WORK'].AsString+b+row.ByField['CENTURYC'].AsString+b+m[2]+b+row.ByField['COUNTRY'].AsString+'<br>';        // russian
    end;
    txt:=txt+t3+m[3]+t3end+b+row.ByField['BIRTHPLACE'].AsString+b+m[4]+b+row.ByField['BIRTHDATE'].AsString+'<br>';
    txt:=txt+t3+m[5]+t3end+b+row.ByField['DEATHPLACE'].AsString+b+m[4]+b+row.ByField['DEATHDATE'].AsString+'<br>';
  end;
  if (trim(row.ByField['FACTS'].AsString)<>'??')and(trim(row.ByField['FACTS'].AsString)<>'') then
     txt:=txt+t3+m[64]+t3end+b+row.ByField['FACTS'].AsString+'<br>';
  txt:=txt+t3+m[6]+t3end+b+row.ByField['NAMEORIGIN'].AsString+'<br>';
  txt:=txt+t3+m[7]+t3end+b+row.ByField['LANGRENUS'].AsString+'<br>';
  txt:=txt+t3+m[8]+t3end+b+row.ByField['HEVELIUS'].AsString+'<br>';
  txt:=txt+t3+m[9]+t3end+b+row.ByField['RICCIOLI'].AsString+'<br>';
  txt:=txt+b+'<br>';
  txt:=txt+'</body></html>';
  Label7.caption:=row.ByField['PROFIL'].AsString;
  Label7.Font.Size:=8;
  Label7.Left:=8; Label7.Top:=8;
  while (Label7.Font.Size>3)and(Label7.Width>GroupBox1.ClientWidth) do Label7.Font.Size:=Label7.Font.Size-1;
  Label7.Left:=(GroupBox1.ClientWidth-Label7.Width) div 2;
  Label7.Top:=(GroupBox1.ClientHeight-Label7.Height+4) div 2;
  statusbar1.Panels[0].Text:=m[10]+row.ByField['LONGIN'].AsString;
  statusbar1.Panels[1].Text:=m[11]+row.ByField['LATIN'].AsString;
  Addtolist(nom);
end;
end;

procedure Tform1.GetDBgrid;
var dbcol:integer;
begin
if dbedited then
 if messagedlg('Attention les modifications de la base de donnée n''ont pas été enregistrée et seront perdue. Continuer quand même ?',mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
   exit;
dbedited:=false;
dbm.query('select * from moon where ID='+currentid+';');
if dbm.RowCount=0 then exit;
editrow:=dbm.Results[0].ByField['ID'].AsInteger;
stringgrid2.RowCount:=dbm.ColCount-1;    // pas l'id.
for dbcol:=1 to stringgrid2.RowCount do begin
 stringgrid2.Cells[0,dbcol-1]:=dbm.GetField(dbcol);
 stringgrid2.Cells[1,dbcol-1]:=dbm.Results[0][dbcol];
end;
end;

procedure TForm1.Button9Click(Sender: TObject);
var dbcol:integer;
    cmd,v:string;
begin
if editrow<0 then begin showmessage('Pas d''enregistrement sélectionné!');exit;end;
if (stringgrid2.Cells[1,0]='') or (stringgrid2.Cells[1,1]='') or (stringgrid2.Cells[1,19]='') or (stringgrid2.Cells[1,21]='') then begin showmessage('DBN, NAME, LONGIN et LATIN obligatoire!');exit;end;
if editrow=0 then begin  // insert new record.
  cmd:='insert into moon values(NULL,';
  for dbcol:=1 to stringgrid2.RowCount do begin
    v:=stringgrid2.Cells[1,dbcol-1];
    v:=stringreplace(v,',','.',[rfreplaceall]);
    v:=stringreplace(v,'""','''',[rfreplaceall]);
    v:=stringreplace(v,'"','',[rfreplaceall]);
    cmd:=cmd+'"'+v+'",';
  end;
  cmd:=copy(cmd,1,length(cmd)-1)+');';
  dbm.Query(cmd);
  editrow:=dbm.GetLastInsertID;
  dbjournal(extractfilename(dbm.database),'INSERT DBN='+stringgrid2.Cells[1,0]+' NAME='+stringgrid2.Cells[1,1]+' ID='+inttostr(editrow));
end
else begin   // update existing record.
  cmd:='update moon set ';
  for dbcol:=1 to stringgrid2.RowCount do begin
    v:=stringgrid2.Cells[1,dbcol-1];
    v:=stringreplace(v,',','.',[rfreplaceall]);
    v:=stringreplace(v,'""','''',[rfreplaceall]);
    v:=stringreplace(v,'"','',[rfreplaceall]);
    cmd:=cmd+stringgrid2.Cells[0,dbcol-1]+'="'+v+'",';
  end;
  cmd:=copy(cmd,1,length(cmd)-1)+' where ID='+inttostr(editrow)+';';
  dbm.Query(cmd);
  dbjournal(extractfilename(dbm.database),'UPDATE DBN='+stringgrid2.Cells[1,0]+' NAME='+stringgrid2.Cells[1,1]+' ID='+inttostr(editrow));
end;
mark(0,0,'');
RefreshLabel;
dbedited:=false;
end;

procedure TForm1.Button20Click(Sender: TObject);  // delete dans la db!
var cmd: string;
begin
if editrow<=0 then begin showmessage('Pas d''enregistrement sélectionné en base de donnée!');exit;end;
if messagedlg('Vraiment supprimer '+stringgrid2.Cells[1,1]+' de la base de donnée ?',mtConfirmation, [mbYes, mbNo], 0)=mrYes then begin
  cmd:='delete from moon where ID='+inttostr(editrow)+';';
  dbm.Query(cmd);
  dbjournal(extractfilename(dbm.database),'DELETE DBN='+stringgrid2.Cells[1,0]+' NAME='+stringgrid2.Cells[1,1]+' ID='+inttostr(editrow));
  dbedited:=false;
  editrow:=-1;
  btnEffacer.Click;
  mark(0,0,'');
  RefreshLabel;
end;
end;

procedure TForm1.btnEffacerClick(Sender: TObject);   // remise a blanc de toute les champs, on fait rien dans la db.
var dbcol:integer;
begin
if dbedited then
 if messagedlg('Attention les modifications de la base de donnée n''ont pas été enregistrée et seront perdue. Continuer quand même ?',mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
   exit;
dbedited:=false;
for dbcol:=1 to stringgrid2.RowCount do begin
 stringgrid2.Cells[1,dbcol-1]:='';
end;
editrow:=0;
end;

procedure TForm1.StringGrid2SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
 if editrow>=0 then dbedited:=true;
end;

procedure TForm1.EnregistredistClick(Sender: TObject);
var dbcol,i:integer;
    l,b,w : double;
    ls,bs,ws,ls2,bs2,quadrant,dbn,defaultname : string;
    sel: TGridRect;
begin
if trim(edit1.text)='' then exit;
ws:=words(edit1.text,'',1,1);
i:=pos(m[18],ws);
if i>0 then ws:=copy(ws,1,i-1);
ls:=edit3.text;
bs:=edit4.text;
w:=strtofloat(ws);
l:=strtofloat(ls);
b:=strtofloat(bs);
if l>=0 then ls2:=formatfloat(d1,abs(l))+'° Est'
        else ls2:=formatfloat(d1,abs(l))+'° Ouest';
if b>=0 then bs2:=formatfloat(d1,abs(b))+'° Nord'
        else bs2:=formatfloat(d1,abs(b))+'° Sud';
quadrant:=words(bs2,'',2,1)+'-'+words(ls2,'',2,1);
if abs(l)<=90 then dbn:='2' // indicé face visible
              else dbn:='4'; // indicé face cachée
defaultname:=formatfloat('"+"0000;"-"0000;"+"0000',l*10)+formatfloat('"+"000;"-"000;"+"000',b*10);
for dbcol:=1 to stringgrid2.RowCount do begin
 if stringgrid2.Cells[0,dbcol-1]='DBN' then stringgrid2.Cells[1,dbcol-1]:=dbn;
 if (stringgrid2.Cells[0,dbcol-1]='NAME')and(stringgrid2.Cells[1,dbcol-1]='') then stringgrid2.Cells[1,dbcol-1]:=defaultname;
 if stringgrid2.Cells[0,dbcol-1]='LENGTHKM' then stringgrid2.Cells[1,dbcol-1]:=ws;
 if stringgrid2.Cells[0,dbcol-1]='WIDEKM' then stringgrid2.Cells[1,dbcol-1]:=ws;
 if stringgrid2.Cells[0,dbcol-1]='LENGTHMI' then stringgrid2.Cells[1,dbcol-1]:=formatfloat(d1,0.621*w);
 if stringgrid2.Cells[0,dbcol-1]='WIDEMI' then stringgrid2.Cells[1,dbcol-1]:=formatfloat(d1,0.621*w);
 if stringgrid2.Cells[0,dbcol-1]='LONGIN' then stringgrid2.Cells[1,dbcol-1]:=ls;
 if stringgrid2.Cells[0,dbcol-1]='LATIN' then stringgrid2.Cells[1,dbcol-1]:=bs;
 if stringgrid2.Cells[0,dbcol-1]='LONGIC' then stringgrid2.Cells[1,dbcol-1]:=ls2;
 if stringgrid2.Cells[0,dbcol-1]='LATIC' then stringgrid2.Cells[1,dbcol-1]:=bs2;
 if stringgrid2.Cells[0,dbcol-1]='QUADRANT' then stringgrid2.Cells[1,dbcol-1]:=quadrant;
end;
pagecontrol1.ActivePage:=dbtab;
sel.left:=1; sel.top:=1; sel.right:=1; sel.bottom:=1;
stringgrid2.Selection:=sel;
stringgrid2.Setfocus;
end;

procedure GetNotes(n: string);
begin
if notesedited then
 if messagedlg(m[67],mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    form1.UpdNotesClick(nil);
with form1 do begin
notes_name.Caption:=n;
memo1.Clear;
notesok:=false;
notesedited:=false;
notesrow:=-1;
dbnotes.GoFirst;
notesok:=dbnotes.MatchData('NAME', '=', trim(uppercase(n)));
if not notesok then notesok:=dbnotes.SeekData('NAME', '=', trim(uppercase(n)));
if notesok then begin
   notesrow:=dbnotes.GetPosition;
   memo1.Text:=dbnotes.GetData('NOTES');
   notesedited:=false;
end;
end;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
 notesedited:=true;
end;

procedure TForm1.UpdNotesClick(Sender: TObject);
var nom:string;
begin
notesedited:=false;
if notesok then begin
  dbnotes.Go(notesrow);
  dbnotes.SetData('NOTES',memo1.Text);
end else begin
  nom:=uppercase(trim(notes_name.Caption));
  if nom='' then exit;
  dbnotes.Gofirst;
  notesok:=dbnotes.MatchData('NAME','>=',nom);
  if not notesok then notesok:=dbnotes.SeekData('NAME','>=',nom);
  if not notesok then dbnotes.golast;
  if dbnotes.GetData('NAME')<>nom then dbnotes.InsertRow(not notesok);
  dbnotes.SetData('NAME',nom);
  dbnotes.SetData('NOTES',memo1.Text);
  notesrow:=dbnotes.GetPosition;
  notesok:=true;
end;
screen.Cursor:=crHourGlass;
try
  dbnotes.SaveToCSVFile(Slash(privatedir)+Slash('database')+'notes.csv');
finally
screen.Cursor:=crDefault;
end;
end;

function ImgExists(nom:string):boolean;
var f : Tsearchrec;
    r,i : integer;
    buf,nom2,dir:string;
begin
nom:=trim(nom);
r:=length(nom)-1;
if copy(nom,r,1)=' ' then begin
  buf:=copy(nom,r+1,1);
  if ((buf>='a')and(buf<='z'))or((buf>='A')and(buf<='Z')) then nom2:=copy(nom,1,r-1);
end else nom2:='';
for i:=0 to maximgdir-1 do begin
  dir:=Slash(imgdir[i,0]);
  r:=findfirst(dir+nom+'.jpg',0,f);
  findclose(f);
  if r=0 then break;
  r:=findfirst(dir+nom+'_*.jpg',0,f);
  findclose(f);
  if r=0 then break;
  if nom2<>'' then begin
    r:=findfirst(dir+nom2+'.jpg',0,f);
    findclose(f);
    if r=0 then break;
    r:=findfirst(dir+nom2+'_*.jpg',0,f);
    findclose(f);
    if r=0 then break;
  end;
end;
result:=(r=0);
end;

Procedure ListImg(nom:string);
var desc,dir,buf,nom2 : string;
    r : integer;
    f : Tsearchrec;
Procedure SearchImages;
var i : integer;
begin
chdir(appdir);
with imglist1 do begin
imglst.clear;
for i:=0 to maximgdir-1 do begin
//  dir:=appdir+PathDelim+imgdir[i,0]+PathDelim;
  dir:=Slash(imgdir[i,0]);
  r:=findfirst(dir+nom+'.jpg',0,f);
  while r=0 do begin
    imglst.Items.Add(imgdir[i,2]+' : '+f.Name);
    r:=findnext(f);
  end;
  findclose(f);
  r:=findfirst(dir+nom+'_*.jpg',0,f);
  while r=0 do begin
    imglst.Items.Add(imgdir[i,2]+' : '+f.Name);
    r:=findnext(f);
  end;
  findclose(f);
end;
end;
end;
//-------ListImg
begin
nom:=trim(nom);
r:=length(nom)-1;
if copy(nom,r,1)=' ' then begin
  buf:=copy(nom,r+1,1);
  if ((buf>='a')and(buf<='z'))or((buf>='A')and(buf<='Z')) then nom2:=copy(nom,1,r-1);
end else nom2:='';
with imglist1 do begin
imglist1.caption:=m[54];
behavior:=image;
SearchImages;
if (imglst.Count=0)and(nom2>'') then begin
  nom:=nom2;
  SearchImages;
end;
//if not imglist.visible then FormPos(imglist,mouse.cursorpos.x,mouse.cursorpos.y);
if not imglist1.visible then FormPos(imglist1,form1.PageControl1.Clienttoscreen(point(0,0)).x ,form1.PageControl1.Clienttoscreen(point(0,0)).y);
case imglst.Count of
0 : imglist1.hide;
1 : begin
    if nom2>'' then imglist1.Show
    else begin
     imglist1.hide;
     buf:=ImgLst.Items[0];
     r:=pos(':',buf);
     desc:=trim(copy(buf,1,r-1));
     nom:=trim(copy(buf,r+1,9999));
     ShowImg(desc,nom,false);
    end;
    end;
else imglist1.Show;
end;
end;
end;

function SearchName(n: string; center: boolean):boolean;
var l,b,x,y : double;
    txt:string;
    i : integer;
begin
result:=false;
with form1 do begin
if Firstsearch then begin
   dbm.Query('select id,name from moon '+
             ' where DBN in ('+sidelist+')'+
             ' and NAME like "'+trim(uppercase(n))+'%"'+
             ' order by NAME limit 100;');
   searchlist.Clear;
   for i:=0 to dbm.RowCount-1 do begin
     searchlist.Add(dbm.Results[i].Format[0].asstring);
   end;
   searchpos:=-1;
end;
Firstsearch:=false;
inc(searchpos);
if searchpos<searchlist.Count then begin
   dbm.Query('select * from moon where id='+searchlist[searchpos]);
   if dbm.RowCount=0 then exit;
   l:=dbm.Results[0].ByField['LONGIN'].AsFloat;
   b:=dbm.Results[0].ByField['LATIN'].AsFloat;
   {$IFDEF opengl}
   if toolbutton3.down then begin
     // if full globe always center the formation
     lrot:=-l;
     inclination:=0;
     GLSceneViewer1MouseMove(nil,[ssLeft],lastx,lasty);
   end;  
   {$ENDIF}
   if (not projMoon(l,b,librl,librb,x,y)) then begin
      // not on our current side, search next object
      SearchName(n,center);
      exit;
   end;
   currentl:=l;
   currentb:=b;
   currentid:=searchlist[searchpos];
   currentname:=dbm.Results[0].ByField['NAME'].AsString;
   if center then MoveCamera(x,y);
   mark(x,y,currentname);
   GetHTMLDetail(dbm.Results[0],txt);
   Desc1.LoadFromString(txt,'');
   if dbtab.TabVisible then GetDBgrid;
   GetNotes(form1.Combobox1.text);
   if ImgExists(currentname) then begin
      ToolButton7.enabled:=true;
      Image2.enabled:=true;
   end else begin
      ToolButton7.enabled:=false;
      Image2.enabled:=false;
   end;
   if Imglist1.visible then begin
     if ImgList1.behavior=formation then ToolButton10Click(nil);
     if ImgList1.behavior=image then begin
        if ToolButton7.enabled then ToolButton7Click(nil)
                               else Imglist1.Close;
     end;
   end;
   result:=true;
   end;
end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
Firstsearch:=true;
SearchText:=Combobox1.Text;
SearchName(SearchText,true);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
SearchName(SearchText,true);
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
Firstsearch:=true;
SearchName(Combobox1.Text,true);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
try
{$IFDEF opengl}
if dummycube4<>nil then DummyCube4.DeleteChildren;
if dummycube5<>nil then DummyCube5.DeleteChildren;
GLSceneViewer1.Buffer.DestroyRC;
{$ENDIF}
dblox.free;
dbnotes.free;
rotatefont.Free;
hires.Free;
hires500.Free;
if hiresok then begin
   closefile(fh);
end;
if hires500ok then begin
   closefile(fh500);
end;
image2d.Free;
eyepiecemask.free;
overlayimg.free;
overlayhi.free;
deletefile(tmpmap);
searchlist.Free;
except
end;
end;

procedure TForm1.Quitter1Click(Sender: TObject);
begin
close;
end;


{$IFDEF opengl}
Procedure OrientLightSource(phase,sunincl: double);
begin
with form1 do begin
 form1.glscene1.BeginUpdate;
 GLLightSource1.BeginUpdate;
 GLLightSource1.Position.z:=-LightDist*cos(degtorad(phase));
 GLLightSource1.Position.x:=-LightDist*sin(degtorad(phase));
 if abs(sunincl)>89.9 then sunincl:=sgn(sunincl)*89.9;
 GLLightSource1.Position.y:=-LightDist*tan(degtorad(-sunincl));
 GLLightSource1.SpotDirection.x:=GLLightSource1.Position.x;
 GLLightSource1.SpotDirection.y:=GLLightSource1.Position.y;
 GLLightSource1.SpotDirection.z:=GLLightSource1.Position.z;
 GLLightSource1.EndUpdate;
 form1.glscene1.EndUpdate;
end;
end;
{$ENDIF}

{$IFDEF opengl}
Procedure resetorientation;
begin
with form1.DummyCube1 do begin
form1.dummycube1.RollAngle:=0;
form1.dummycube1.TurnAngle:=0;
form1.dummycube1.PitchAngle:=0;
direction.X:=0;
direction.Y:=0;
direction.Z:=1;
Up.X:=0;
Up.Y:=1;
Up.Z:=0;
end;
end;
{$ENDIF}

Procedure RefreshMoonImage;
var moonrise,moonset,moontransit,azimuthrise,azimuthset : widestring;
    jd0,st0,q,tz,cphase,colong,hh,az,ah : double;
    gpa,glibrb,gsunincl,glibrl: double;
    aa,mm,dd,i,col : integer;
    h,mn,s,ms: word;
    ds1,ds2,n,ex1,ey1,xx,yy : integer;
    th,ex,ey,ci,si,sph : double;
    rvx,rvy:boolean;
    p : array[0..23] of Tpoint;
    ph:Tbitmap;
const b=' ';
begin
  st0:=0;
  Lune(CurrentJD,ra,dec,dist,dkm,diam,phase,illum);
  MoonOrientation(CurrentJD,ra,dec,dist,gpa,glibrb,gsunincl,glibrl);
  if not geocentric then begin
     jd0:=jd(CurYear,CurrentMonth,CurrentDay,0.0);
     st0:=SidTim(jd0,CurrentTime-Timezone,ObsLongitude);
     Paralaxe(st0,dist,ra,dec,ra,dec,q);
     diam:=diam/q;
     dkm:=dkm*q;
     dist:=dist*q;
  end;
  rad:=ra; ded:=dec;
  precession(jd2000,CurrentJD,rad,ded);
  MoonOrientation(CurrentJD,ra,dec,dist,pa,librb,sunincl,librl);
  cphase:=phase+glibrl;
  tphase:=phase;
  colong:=rmod(90-tphase-glibrl+360,360);
  jd0:=jd(CurYear,1,1,0.0);
  MoonPhases(CurYear+(CurrentJD-jd0)/365.25,nmjd,fqjd,fmjd,lqjd);
  lunaison:=CurrentJD-nmjd;
  if lunaison<0 then begin
     lunaison:=CurrentJD-MoonPhase(floor(12.3685*(CurYear-2000-0.04+(CurrentJD-jd0)/365.25)));
  end;
  {$IFDEF opengl}
  if FollowNorth then begin
    CameraOrientation:=rmod(-PA+PoleOrientation+360,360);
    SetCameraOrientation;
  end;
  {$ENDIF}
  form1.StatusBar1.Panels[2].Text:=m[51]+': '+dattostr(curyear,currentmonth,currentday)+'   '+m[50]+': '+timtostr(currenttime);
//  lunaison_app:=trunc(29.5*rmod(540-phase{+librl}+6.1,360)/360);
  phaseoffset:=0;

  djd(nmjd-(DT_UT)/24,aa,mm,dd,hh);
  decodetime(hh/24,h,mn,s,ms);
  tz:=gettimezone(encodedatetime(aa,mm,dd,h,mn,s,ms));
  djd(nmjd+(tz-DT_UT)/24,aa,mm,dd,hh);
  form1.labelnm.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);

  djd(fqjd-(DT_UT)/24,aa,mm,dd,hh);
  decodetime(hh/24,h,mn,s,ms);
  tz:=gettimezone(encodedatetime(aa,mm,dd,h,mn,s,ms));
  djd(fqjd+(tz-DT_UT)/24,aa,mm,dd,hh);
  form1.labelfq.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);

  djd(fmjd-(DT_UT)/24,aa,mm,dd,hh);
  decodetime(hh/24,h,mn,s,ms);
  tz:=gettimezone(encodedatetime(aa,mm,dd,h,mn,s,ms));
  djd(fmjd+(tz-DT_UT)/24,aa,mm,dd,hh);
  form1.labelfm.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);

  djd(lqjd-(DT_UT)/24,aa,mm,dd,hh);
  decodetime(hh/24,h,mn,s,ms);
  tz:=gettimezone(encodedatetime(aa,mm,dd,h,mn,s,ms));
  djd(lqjd+(tz-DT_UT)/24,aa,mm,dd,hh);
  form1.labellq.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);

  form1.Stringgrid1.colwidths[1]:=150;
  i:=0;
  if geocentric then begin form1.Stringgrid1.Cells[0,i]:=form2.Label16.caption+':'; form1.Stringgrid1.Cells[1,i]:=form2.CheckBox3.caption; end
                else begin form1.Stringgrid1.Cells[0,i]:=form2.Label16.caption+':'; form1.Stringgrid1.Cells[1,i]:=demtostr(ObsLatitude)+' '+demtostr(ObsLongitude)+' Tz:'+armtostr(timezone);end;
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[51]+':'; form1.Stringgrid1.Cells[1,i]:=dattostr(curyear,currentmonth,currentday)+' '+timtostr(currenttime);
  djd(currentjd,aa,mm,dd,hh);
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[51]+' (DT):'; form1.Stringgrid1.Cells[1,i]:=dattostr(aa,mm,dd)+' '+timtostr(hh);
  inc(i); form1.Stringgrid1.Cells[0,i]:='(J2000) '+m[29]; form1.Stringgrid1.Cells[1,i]:=artostr(ra);
  inc(i); form1.Stringgrid1.Cells[0,i]:='(J2000) '+m[30]; form1.Stringgrid1.Cells[1,i]:=detostr(dec);
  inc(i); form1.Stringgrid1.Cells[0,i]:='('+m[51]+')'+b+m[29]; form1.Stringgrid1.Cells[1,i]:=artostr(rad);
  inc(i); form1.Stringgrid1.Cells[0,i]:='('+m[51]+')'+b+m[30]; form1.Stringgrid1.Cells[1,i]:=detostr(ded);
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[31]; form1.Stringgrid1.Cells[1,i]:=inttostr(round(dkm))+m[18];
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[36]; form1.Stringgrid1.Cells[1,i]:=formatfloat(d2,diam/60)+lmin;
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[48]; form1.Stringgrid1.Cells[1,i]:=formatfloat(d1,colong)+ldeg;
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[32]; form1.Stringgrid1.Cells[1,i]:=formatfloat(d1,phase)+ldeg;
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[46]; form1.Stringgrid1.Cells[1,i]:=formatfloat(d2,lunaison)+' '+m[47];
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[35]; form1.Stringgrid1.Cells[1,i]:=formatfloat(d1,illum*100)+'%';
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[45]; form1.Stringgrid1.Cells[1,i]:=formatfloat(d1,sunincl)+ldeg;
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[33]; form1.Stringgrid1.Cells[1,i]:=demtostr(librb);
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[34]; form1.Stringgrid1.Cells[1,i]:=demtostr(librl);
  inc(i); form1.Stringgrid1.Cells[0,i]:=m[37]; form1.Stringgrid1.Cells[1,i]:=formatfloat(d1,pa)+ldeg;
  if not geocentric then begin
    eq2hz(15*(st0-rad),ded,az,ah);
    az:=rmod(az+180,360);
    inc(i); form1.Stringgrid1.Cells[0,i]:=m[73]; form1.Stringgrid1.Cells[1,i]:=demtostr(az);
    inc(i); form1.Stringgrid1.Cells[0,i]:=m[74]; form1.Stringgrid1.Cells[1,i]:=demtostr(ah);
    TZ:=-timezone;
    CalcMoonRiseSet(ObsLatitude,ObsLongitude,Tz,CurYear,CurrentMonth,CurrentDay,moonrise,moonset,moontransit,azimuthrise,azimuthset);
    inc(i); form1.Stringgrid1.Cells[0,i]:=m[38]; form1.Stringgrid1.Cells[1,i]:=moonrise;
    inc(i); form1.Stringgrid1.Cells[0,i]:=m[39]; form1.Stringgrid1.Cells[1,i]:=moontransit;
    inc(i); form1.Stringgrid1.Cells[0,i]:=m[40]; form1.Stringgrid1.Cells[1,i]:=moonset;
    inc(i); form1.Stringgrid1.Cells[0,i]:=m[41]; form1.Stringgrid1.Cells[1,i]:=azimuthrise;
    if obslatitude>0 then begin inc(i); form1.Stringgrid1.Cells[0,i]:=m[55]; form1.Stringgrid1.Cells[1,i]:=dedtostr(90-obslatitude+dec); end
                     else begin inc(i); form1.Stringgrid1.Cells[0,i]:=m[55]; form1.Stringgrid1.Cells[1,i]:=dedtostr(90+obslatitude-dec); end;
    inc(i); form1.Stringgrid1.Cells[0,i]:=m[42]; form1.Stringgrid1.Cells[1,i]:=azimuthset;
  end else begin
    inc(i); form1.Stringgrid1.Cells[0,i]:=b; form1.Stringgrid1.Cells[1,i]:=b;
    inc(i); form1.Stringgrid1.Cells[0,i]:=b; form1.Stringgrid1.Cells[1,i]:=b;
    inc(i); form1.Stringgrid1.Cells[0,i]:=b; form1.Stringgrid1.Cells[1,i]:=b;
    inc(i); form1.Stringgrid1.Cells[0,i]:=b; form1.Stringgrid1.Cells[1,i]:=b;
    inc(i); form1.Stringgrid1.Cells[0,i]:=b; form1.Stringgrid1.Cells[1,i]:=b;
    inc(i); form1.Stringgrid1.Cells[0,i]:=b; form1.Stringgrid1.Cells[1,i]:=b;
    inc(i); form1.Stringgrid1.Cells[0,i]:=b; form1.Stringgrid1.Cells[1,i]:=b;
    inc(i); form1.Stringgrid1.Cells[0,i]:=b; form1.Stringgrid1.Cells[1,i]:=b;
  end;
if not phaseeffect then begin
  phase:=1;
  cphase:=1;
  sunincl:=0;
end;
librlong:=librl;
librlat:=librb;
if (not librationeffect) or form1.ToolButton3.Down then begin
  librl:=0;
  librb:=0;
end;
{$IFDEF opengl}
if useOpenGL then begin
form1.glscene1.BeginUpdate;
form1.dummycube1.BeginUpdate;
if not form1.ToolButton3.Down then begin
  resetorientation;
  form1.dummycube1.PitchAngle:=librb;
  form1.dummycube1.TurnAngle:=-librl;
  form1.dummycube1.up.x:=0;
end;
OrientLightSource(cphase,sunincl);
form1.GLLightSource2.Shining:=not phaseeffect;
LibrationMark(librlong,librlat);
form1.dummycube1.EndUpdate;
mark(0,0,'');
ShowSphere;
RefreshLabel;
form1.glscene1.EndUpdate;
MemoryDebug;
end
else
{$ENDIF}
begin
col:=clBlack;
image2d.LoadFromFile(tmpmap);
image2d.pixelformat:=pf24bit;
if currenteyepiece>0 then begin
   col:=$00202020;
   image2d.Canvas.Brush.Style:=bsSolid;
   image2d.Canvas.Brush.Color:=col;
   image2d.Canvas.FloodFill(1,1,clBlack,fsSurface);
end;
if phaseeffect then begin
   ds1:=round(px*image2d.Width/2);
   ds2:=round(-cos(degtorad(cphase))*ds1);
   si:=sin(degtorad(-sunincl));
   ci:=cos(degtorad(-sunincl));
   th:=pi/2;
   xx:=image2d.Width div 2;
   yy:=image2d.Height div 2;
   if (phase<180) then sph:=-1
                  else sph:=1;
   if form1.toolbutton3.down then rvx:=(phase>=180)
                             else rvx:=(phase<180);
   if rvx then begin
      p[0]:=point(0,0);
      p[23]:=point(0,image2d.Height);
   end else begin
      p[0]:=point(image2d.Width,0);
      p[23]:=point(image2d.Width,image2d.Height);
   end;
   for n:=1 to 22 do begin
     ex:=sph*ds2*cos(th);
     ey:=ds1*sin(th);
     ey1:=round(ex*si - ey*ci)+yy ;
     ex1:=round(ex*ci + ey*si)+xx ;
     p[n]:=point(ex1,ey1);
     th:=th+0.15;
   end;
   rvx:=(flipx<0);
   rvy:=false;
   if poleorientation>0 then begin
      rvy:=not rvy;
      rvx:=not rvx;
   end;
   if rvx then for n:=0 to 23 do begin
      p[n].X:=image2d.Width - p[n].X;
   end;
   if rvy then for n:=0 to 23 do begin
      p[n].Y:=image2d.Height - p[n].Y;
   end;
   if phasehash then begin
     image2d.Canvas.Pen.Color:=col;
     image2d.Canvas.Pen.width:=2;
     image2d.Canvas.brush.Color:=col;
     image2d.Canvas.brush.style:=bsBDiagonal;
     image2d.Canvas.polygon(p);
   end else begin
     ph:=tbitmap.create;
     try
     ph.pixelformat:=pf24bit;
     ph.width:=image2d.width;
     ph.height:=image2d.height;
     ph.canvas.brush.color:=clBlack;
     ph.canvas.fillrect(rect(0,0,ph.width,ph.height));
     ph.canvas.brush.color:=phaseumbra;
     ph.canvas.pen.color:=ph.canvas.brush.color;
     ph.canvas.polygon(p);
     substractbitmap(image2d,ph);
//     image2d.canvas.copymode:=cmSrcAnd;
//     image2d.canvas.draw(0,0,ph);
//     image2d.canvas.copymode:=cmSrcCopy;
     finally
     ph.free;
     end;
   end;
end;
image2d.PixelFormat:=pfdevice;
image2d.FreeImage;
refresh2dimage;
LibrationMark(librlong,librlat);
RefreshLabel;
end;
end;

{$IFDEF opengl}
procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var xx,yy : double;
begin
clickX:=X;
clickY:=Y;
if measuringdistance and (button=mbLeft) then begin
  window2world(x,y,xx,yy);
  startx:=x;
  starty:=y;
  startxx:=xx;
  startyy:=yy;
  InvProjMoon(2*xx,2*yy,librl,librb,startl,startb);
  distancestart:=true;
  mark(0,0,'');
  HUDSprite2.Visible:=true;
  HUDSprite2.Material.FrontProperties.Emission.AsWinColor:=MarkColor;
  HUDSprite2.Width:=1;
  HUDSprite2.Position.X:=startx;
  HUDSprite2.Position.Y:=starty;
  HUDSprite2.Rotation:=0;
end;
end;
{$ENDIF}

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var xx,yy : double;
begin
clickX:=X;
clickY:=Y;
if measuringdistance and (button=mbLeft) then begin
  window2world(x,y,xx,yy);
  startx:=x;
  starty:=y;
  startxx:=xx;
  startyy:=yy;
  InvProjMoon(2*xx,2*yy,librl,librb,startl,startb);
  distancestart:=true;
  mark(0,0,'');
end;
end;

Procedure ShowCoordinates(x,y:integer);
var xx,yy,l,b: double;
begin
with form1 do begin
 lastx:=x;
 lasty:=y;
 lastyzoom:=y;
 Window2World(x,y,xx,yy);
 InvProjMoon(2*xx,2*yy,librl,librb,l,b);
 form1.statusbar1.Panels[0].Text:=m[10]+stringreplace(formatfloat(d1,l),'NAN','   ',[]);
 form1.statusbar1.Panels[1].Text:=m[11]+stringreplace(formatfloat(d1,b),'NAN','   ',[]);
end;
end;

Procedure MeasureDistance(x,y:integer);
var xx,yy,l,b,d : double;
begin
with form1 do begin
    window2world(x,y,xx,yy);
    InvProjMoon(2*xx,2*yy,librl,librb,l,b);
    d:=angulardistance(l,b,startl,startb);
    edit1.Text:=formatfloat(d1,deg2rad*d*Rmoon)+m[18];
    xx:=startxx-xx;
    yy:=startyy-yy;
    d:=sqrt(xx*xx+yy*yy)*diam/3600;
    edit2.Text:=copy(Deptostr(d),5,99);
    xx:=startxx-xx/2;
    yy:=startyy-yy/2;
    InvProjMoon(2*xx,2*yy,librl,librb,l,b);
    edit3.text:=formatfloat(d1,l);
    edit4.text:=formatfloat(d1,b);
    {$IFDEF opengl}
    if useopengl then begin
      x:=x-startx;
      y:=y-starty;
      d:=sqrt(x*x+y*y);
      HUDSprite2.Width:=d;
      HUDSprite2.Position.X:=startx+x/2;
      HUDSprite2.Position.Y:=starty+y/2;
      HUDSprite2.Rotation:=-rad2deg*(arctan2(y,x));
    end else
    {$ENDIF}
    begin
      image1.Canvas.Pen.Color:=clRed;
      image1.Canvas.Pen.width:=1;
      image1.Canvas.MoveTo(startx,starty);
      image1.Canvas.LineTo(x,y);
      image1.refresh;
    end;
end;
end;

{$IFDEF opengl}
procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var xx,yy,l,b : double;
begin
//if lockmove then exit;
{$ifndef openglx}
if useopengl and (not GLSceneViewer1.Focused) then
   form1.ActiveControl:=GLSceneViewer1;
{$endif}
if (abs(clickX-X)>3)or(abs(clickY-Y)>3) then begin
try
lockmove:=true;
if shift=[ssLeft] then begin
 if measuringdistance and distancestart then begin
    // distance
    MeasureDistance(x,y);
    ShowCoordinates(x,y);
 end else if ToolButton3.down then begin
  // rotation
  SkipIdent:=true;
  window2world(x,y,xx,yy);
  window2world(lastx,lasty,l,b);
  lrot:=rmod(100*(l-xx)+lrot+360,360);
  if lrot>180 then lrot:=lrot-360;
  yy:=b-yy+GLCamera1.Position.y;
  lastx:=x;
  lasty:=y;
  form1.glscene1.BeginUpdate;
  resetorientation;
  dummycube1.PitchAngle:=librb+inclination;
  dummycube1.TurnAngle:=-librl+lrot;
  GLLightSource2.Shining:=not phaseeffect;
  if abs(yy)<0.5 then GLCamera1.Position.y:=yy;
  Annulus1.Position.x:=GLCamera1.Position.x;
  Annulus1.Position.y:=GLCamera1.Position.y;
  SetScrollBar(GLCamera1.Position.x,GLCamera1.Position.y);
  mark(0,0,'');
  ShowSphere;
  RefreshLabel;
  form1.glscene1.EndUpdate;
 end else begin
  // deplacement
  SkipIdent:=true;
  window2world(x,y,xx,yy);
  window2world(lastx,lasty,l,b);
  xx:=l-xx+GLCamera1.Position.x;
  yy:=b-yy+GLCamera1.Position.y;
  lastx:=x;
  lasty:=y;
  form1.glscene1.BeginUpdate;
  if abs(xx)<0.5 then GLCamera1.Position.x:=xx;
  if abs(yy)<0.5 then GLCamera1.Position.y:=yy;
  Annulus1.Position.x:=GLCamera1.Position.x;
  Annulus1.Position.y:=GLCamera1.Position.y;
  SetScrollBar(xx,yy);
  Mark(shapePositionX,shapePositionY,hudtext1.Text);
  RefreshLabel;
  form1.glscene1.EndUpdate;
 end;
end
else if shift=[ssmiddle] then begin
 // zoom
 yy:=GLCamera1.FocalLength*(1-(y-lastyzoom)/100);
 lastyzoom:=y;
 if yy<minfoc then yy:=minfoc;
 SetZoom(yy);
end
else begin
 ShowCoordinates(x,y);
end;
finally
lockmove:=false;
end;
end;
end;
{$ENDIF}

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var xx,yy,l,b : double;
begin
if shift=[ssLeft] then begin
 if measuringdistance and distancestart then begin
    // distance
    Refresh2DImage;
    MeasureDistance(x,y);
    ShowCoordinates(x,y);
 end else begin
  SkipIdent:=true;
  xx:=bx*(lastx-x);
  yy:=by*(lasty-y);
  lastx:=x;
  lasty:=y;
  lockscrollbar:=true;
  HorzScrollBar.Position:=HorzScrollBar.Position+round(xx/bxpos);
  lockscrollbar:=true;
  VertScrollBar.Position:=VertScrollBar.Position+round(yy/bxpos);
  Refresh2DImage;
 end;
end
else if shift=[ssmiddle] then begin
 yy:=zoom*minfoc*(1-(y-lastyzoom)/200);
 lastyzoom:=y;
 if yy<minfoc then yy:=minfoc;
 SetZoom(yy);
end
else begin
 lastx:=x;
 lasty:=y;
 lastyzoom:=y;
 Window2World(x,y,xx,yy);
 InvProjMoon(2*xx,2*yy,librl,librb,l,b);
 form1.statusbar1.Panels[0].Text:=m[10]+stringreplace(formatfloat(d1,l),'NAN','   ',[]);
 form1.statusbar1.Panels[1].Text:=m[11]+stringreplace(formatfloat(d1,b),'NAN','   ',[]);
end;
end;

{$IFDEF opengl}
procedure TForm1.ScrollBar1Change(Sender: TObject);
var xx,yy,x1,y1,s1,c1 : extended;
begin
 HUDSprite2.Visible:=false;
 sincos(deg2rad*CameraOrientation,s1,c1);
 x1:=-scrollbar1.Position/1000;
 y1:=-scrollbar2.Position/1000;
 xx:=(x1*c1+y1*s1);
 yy:=(-x1*s1+y1*c1);
 form1.glscene1.BeginUpdate;
 GLCamera1.Position.x:=xx;
 GLCamera1.Position.y:=yy;
 Annulus1.Position.x:=GLCamera1.Position.x;
 Annulus1.Position.y:=GLCamera1.Position.y;
 curx:=xx;
 cury:=yy;
 Mark(shapePositionX,shapePositionY,hudtext1.Text);
 RefreshLabel;
 form1.glscene1.EndUpdate;
end;
{$ENDIF}

Procedure IdentXY(x,y:integer);
var xx,yy,l,b : double;
    txt:string;
begin
with form1 do begin
    Window2World(x,y,xx,yy);
    if InvProjMoon(2*xx,2*yy,librl,librb,l,b) and
       SearchAtPos(l,b) then begin
     l:=dbm.Results[0].ByField['LONGIN'].AsFloat;
     b:=dbm.Results[0].ByField['LATIN'].AsFloat;
     searchl:=l;
     searchb:=b;
     currentl:=l;
     currentb:=b;
     currentname:=dbm.Results[0].ByField['NAME'].AsString;
     projMoon(l,b,librl,librb,xx,yy);
     form1.Combobox1.text:=currentname;
     if not useOpenGL then Refresh2DImage;
     mark(xx,yy,form1.Combobox1.text);
     RefreshLabel;
     GetHTMLDetail(dbm.Results[0], txt);
     Desc1.LoadFromString(txt,'');
     if dbtab.TabVisible then GetDBgrid;
     GetNotes(form1.Combobox1.text);
     if ImgExists(currentname) then begin
      ToolButton7.enabled:=true;
      Image2.enabled:=true;
     end else begin
      ToolButton7.enabled:=false;
      Image2.enabled:=false;
     end;
     if Imglist1.visible then begin
       if ImgList1.behavior=formation then ToolButton10Click(nil);
       if ImgList1.behavior=image then begin
          if ToolButton7.enabled then ToolButton7Click(nil)
                                 else Imglist1.Close;
       end;
     end;
    end else mark(0,0,'');
end;
end;

procedure TForm1.GLSceneViewer1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
if measuringdistance and distancestart then begin
   // distance
   MeasureDistance(x,y);
   ShowCoordinates(x,y);
   distancestart:=false;
end
else if button=mbLeft then begin
  if not SkipIdent then begin
    // identification
    IdentXY(x,y);
  end else SkipIdent:=false;
end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var x : double;
    hd,bg : Tpoint;
begin
{$IFDEF opengl}
if useOpenGL then begin
    hd:=glsceneviewer1.ClientToScreen(point(0,0));
    bg:=glsceneviewer1.ClientToScreen(point(glsceneviewer1.width,glsceneviewer1.height));
end else
{$ENDIF}
begin
  hd:=image1.ClientToScreen(point(0,0));
  bg:=image1.ClientToScreen(point(image1.width,image1.height));
end;
if (mousepos.X>hd.x)and(mousepos.X<bg.x)and
   (mousepos.Y>hd.y)and(mousepos.Y<bg.y) then begin
   handled:=true;
   if wheeldelta>0 then x:=wheelstep*zoom*minfoc
                   else x:=minfoc*zoom/wheelstep;
   if x<minfoc then x:=minfoc;
   Setzoom(x);
end;
end;

Procedure InitLopamIdx;
var f : textfile;
    i : integer;
begin
filemode:=0;
assignfile(f,Slash(appdir)+Slash('database')+'lopamidx.txt');
reset(f);
readln(f);
readln(f,lopamplateurl);
readln(f,lopamnameurl);
readln(f,lopamdirecturl);
readln(f,lopamlocalurl);
closefile(f);
i:=pos('URL=',lopamplateurl);
lopamplateurl:=copy(lopamplateurl,i+4,999);
i:=pos('SUFFIX=',lopamplateurl);
lopamplatesuffix:=trim(copy(lopamplateurl,i+7,999));
lopamplateurl:=trim(copy(lopamplateurl,1,i-1));

i:=pos('URL=',lopamnameurl);
lopamnameurl:=copy(lopamnameurl,i+4,999);
i:=pos('SUFFIX=',lopamnameurl);
lopamnamesuffix:=trim(copy(lopamnameurl,i+7,999));
lopamnameurl:=trim(copy(lopamnameurl,1,i-1));

i:=pos('URL=',lopamdirecturl);
lopamdirecturl:=copy(lopamdirecturl,i+4,999);
i:=pos('SUFFIX=',lopamdirecturl);
lopamdirectsuffix:=trim(copy(lopamdirecturl,i+7,999));
lopamdirecturl:=trim(copy(lopamdirecturl,1,i-1));

i:=pos('URL=',lopamlocalurl);
lopamlocalurl:=copy(lopamlocalurl,i+4,999);
i:=pos('SUFFIX=',lopamlocalurl);
lopamlocalsuffix:=trim(copy(lopamlocalurl,i+7,999));
lopamlocalurl:=trim(copy(lopamlocalurl,1,i-1));
end;



Procedure ListUserDB;
var i,j,k : integer;
begin
for i:=0 to form2.Checklistbox1.Count-1 do (form2.Checklistbox1.Items.Objects[i] as TDBinfo).Free;
form2.CheckListBox1.Clear;
form1.dbm.query('select * from user_database where DBN>9');
for i:=0 to form1.dbm.RowCount-1 do begin
  j:=form1.dbm.Results[i].format[0].asinteger;
  k:=form2.CheckListBox1.Items.Add(form1.dbm.Results[i][1]);
  form2.Checklistbox1.Items.Objects[k]:=TDBinfo.Create;
  (form2.Checklistbox1.Items.Objects[k] as TDBinfo).dbnum:=j;
  form2.CheckListBox1.Checked[k]:=usedatabase[j];
end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
Setlang;
screen.cursor:=crHourGlass;
LoadDB(dbm);
screen.cursor:=crDefault;
ListUserDB;
InitLopamIdx;
{$ifdef opengl}
InitTelescope;
LibrationButton.Down:=librationeffect;
{$endif}
PhaseButton.Down:=phaseeffect;
dblox.LoadFromFile(Slash(appdir)+Slash('database')+'lopamidx.csv');
dblox.GoFirst;
if fileexists(Slash(privatedir)+Slash('database')+'notes.csv') then
   dbnotes.LoadFromFile(Slash(privatedir)+Slash('database')+'notes.csv')
else begin
   if fileexists(Slash(appdir)+Slash('database')+'notes.csv') then
     dbnotes.LoadFromFile(Slash(appdir)+Slash('database')+'notes.csv')
   else begin
     dbnotes.AddField('NAME');
     dbnotes.AddField('NOTES');
   end;
end;
dbnotes.GoFirst;
if UseComputerTime then InitDate
                   else SetJDDate;
if screen.width<750 then begin
  form1.Top:=0;
  form1.Left:=0;
  form1.Width:=screen.Width;
  form1.Height:=screen.Height;
end;
mark(0,0,'');
MoveCamera(0,0);
setzoom(minfoc);
ReadParam;
memo2.Width:=PrintTextWidth;
toolbar1.Left:=toolbar2.width+1;
RefreshMoonImage;
if currentname<>'' then begin
  firstsearch:=true;
  searchname(currentname,false);
end;
setzoom(curfoc);
btnEffacerClick(Sender);
SetEyepieceMenu;
if (not multi_instance) and (currentid='') then begin
  // show an interesting object
  Combobox2.itemindex:=0;
  Combobox3.itemindex:=6;
  RadioGroup1.itemindex:=1;
  Updterminateur;
  Firstsearch:=true;
  SearchText:=trim(copy(ListBox1.Items[0],2,999));
  SearchName(SearchText,false);
  Combobox3.itemindex:=0;
  currentphase:=-999;
end;
{$ifdef openglx}
// show viewer to another form to avoid Wine bug 2398
glsceneviewer1.parent:=formglx;
formglx.clientwidth:=glsceneviewer1.width;
formglx.clientheight:=glsceneviewer1.height;
formglx.left:=panel2.clientorigin.x;
formglx.top:=panel2.clientorigin.y;
formglx.show;
{$endif}
end;

procedure TForm1.Configuration1Click(Sender: TObject);
var reboot,reloaddb,systemtimechange: boolean;
    oldreduc,oldreducfar,i,j,oldmaxima,oldtexture: integer;
    yy,x,y:double;
begin
{$ifdef openglx}
formglx.hide;
{$endif}
try
reboot:=false;
reloaddb:=false;
form2.Edit1.text:=formatfloat(d2,abs(ObsLatitude));
form2.Edit2.text:=formatfloat(d2,abs(ObsLongitude));
form2.Edit3.text:=formatfloat(d1,timezone);
if Obslatitude>=0 then form2.ComboBox1.ItemIndex:=0
                  else form2.ComboBox1.ItemIndex:=1;
if Obslongitude>=0 then form2.ComboBox2.ItemIndex:=1
                   else form2.ComboBox2.ItemIndex:=0;
form2.checkbox19.checked:=usedatabase[1];
form2.checkbox20.checked:=usedatabase[2];
form2.checkbox21.checked:=usedatabase[3];
form2.checkbox22.checked:=usedatabase[4];
form2.checkbox23.checked:=usedatabase[5];
form2.checkbox24.checked:=usedatabase[6];
ListUserDB;
form2.checkbox4.checked:=useOpenGL;
form2.checkbox1.checked:=phaseeffect;
form2.checkbox2.checked:=librationeffect;
form2.checkbox3.checked:=Geocentric;
form2.checkbox5.checked:=showlabel;
form2.checkbox6.checked:=showmark;
form2.checkbox14.checked:=showlibrationmark;
form2.checkbox17.checked:=labelcenter;
form2.checkbox18.checked:=minilabel;
form2.Shape1.Brush.Color:=labelcolor;
form2.Shape2.Brush.Color:=markcolor;
form2.Shape3.Brush.Color:=autolabelcolor;
form2.TrackBar2.Position:=-LabelDensity;
form2.TrackBar3.Position:=round(LabelSize*100);
form2.TrackBar4.Position:=marksize;
config.newlang:=language;
oldreduc:=reducetexture;
case reducetexture of
     1 : form2.RadioGroup1.ItemIndex:=0;
     2 : form2.RadioGroup1.ItemIndex:=1;
     4 : form2.RadioGroup1.ItemIndex:=2;
end;
oldreducfar:=reducetexturefar;
case reducetexturefar of
     1 : form2.RadioGroup3.ItemIndex:=0;
     2 : form2.RadioGroup3.ItemIndex:=1;
     4 : form2.RadioGroup3.ItemIndex:=2;
end;
form2.updown1.Position:=maxima;
oldmaxima:=maxima;
form2.StringGrid1.RowCount:=maximgdir+10;
if saveimagesize=0 then form2.ComboBox4.ItemIndex:=0
   else form2.ComboBox4.Text:=inttostr(saveimagesize);
form2.CheckBox12.Checked:=externalimage;
form2.edit4.Text:=externalimagepath;
form2.CheckBox7.Checked:=saveimagewhite;
for i:=1 to maximgdir do begin
  form2.StringGrid1.Cells[0,i]:=imgdir[i-1,2];
  form2.StringGrid1.Cells[1,i]:=imgdir[i-1,0];
end;
for i:=1 to 10 do begin
  form2.StringGrid2.Cells[0,i]:=eyepiecename[i];
  form2.StringGrid2.Cells[1,i]:=inttostr(eyepiecefield[i]);
  form2.StringGrid2.Cells[2,i]:=inttostr(eyepiecemirror[i]);
  form2.StringGrid2.Cells[3,i]:=inttostr(eyepiecerotation[i]);
end;
form2.LongEdit1.Value:=LeftMargin;
form2.TrackBar1.Position:=PrintTextWidth;
form2.CheckBox13.Checked:=PrintChart;
form2.CheckBox8.Checked:=PrintEph;
form2.CheckBox9.Checked:=PrintDesc;
form2.CheckBox10.Checked:=MipMaps;
{$ifdef vmalight}
if GeologicalMap then form2.radiogroup2.itemindex:=1
                 else form2.radiogroup2.itemindex:=0;
{$endif}
{$ifdef vmaexpert}
if hiresfile='hires.jpg' then form2.radiogroup2.itemindex:=0
 else if hiresfile='hires_clem.jpg' then form2.radiogroup2.itemindex:=1
 else if hiresfile='hires_lopam.jpg' then form2.radiogroup2.itemindex:=2
 else form2.radiogroup2.itemindex:=-1;
{$endif}
oldtexture:=form2.radiogroup2.itemindex;
form2.CheckBox15.Checked:=LopamDirect;
form2.ruklprefix.text:=ruklprefix;
form2.ruklsuffix.text:=ruklsuffix;
form2.hiresfn:=hiresfile;
form2.combobox5.text:=remext(overlayname);
form2.trackbar5.position:=overlaylum;
form2.combobox5change(sender);
form2.checkbox11.checked:=showoverlay;
form2.checkbox16.checked:=UseComputerTime;
FormPos(Form2,mouse.cursorpos.x,mouse.cursorpos.y);
Form2.showmodal;
if form2.ModalResult=mrOK then begin
    screen.cursor:=crhourglass;
   {$ifdef vmaexpert}
     if (form2.combobox5.text<>remext(overlayname)) or
        (form2.trackbar5.position<>overlaylum) or
        (form2.checkbox11.checked<>showoverlay)
        then reboot:=true;
     overlayname:=form2.combobox5.text+'.jpg';
     overlaylum:=form2.trackbar5.position;
     showoverlay:=form2.checkbox11.checked;
     if hiresfile<>form2.hiresfn then begin
        hiresfile:=form2.hiresfn;
        if copy(hiresfile,6,1)<>'.' then begin
           i:=pos('.',hiresfile);
           if i>6 then imgsuffix:=copy(hiresfile,6,i-6);
        end else imgsuffix:='';
        OpenHires(true);
        reboot:=true;
     end;
   {$endif}
   ruklprefix:=form2.ruklprefix.text;
   ruklsuffix:=form2.ruklsuffix.text;
   markcolor:=form2.Shape2.Brush.Color;
   labelcolor:=form2.Shape1.Brush.Color;
   autolabelcolor:=form2.Shape3.Brush.Color;
   LabelDensity:=abs(form2.TrackBar2.Position);
   LabelSize:=form2.TrackBar3.Position/100;
   marksize:=form2.TrackBar4.Position;
   showlabel:=form2.checkbox5.checked;
   showmark:=form2.checkbox6.checked;
   showlibrationmark:=form2.checkbox14.checked;
   labelcenter:=form2.checkbox17.checked;
   minilabel:=form2.checkbox18.checked;
   Showautolabel:=showlabel and (Labeldensity<1000);
   {$IFDEF opengl}
   if useOpenGL then begin
      HUDText1.ModulateColor.AsWinColor:=labelcolor;
      HUDText1.Scale.SetVector(Label3dSize*LabelSize,Label3dSize*LabelSize,1);
      InitLabel;
      InitSprite;
   end else
   {$ENDIF}
   begin
      Label5.Font.Height:=round(Label2dSize*Labelsize);
      Label5.Font.Color:=Labelcolor;
      InitLabel;
   end;
   Obslatitude:=strtofloat(form2.Edit1.text);
   if form2.ComboBox1.ItemIndex=1 then Obslatitude:=-Obslatitude;
   Obslongitude:=strtofloat(form2.Edit2.text);
   if form2.ComboBox2.ItemIndex=0 then Obslongitude:=-Obslongitude;
   systemtimechange:=UseComputerTime<>form2.checkbox16.checked;
   timezone:=strtofloat(form2.Edit3.text);
   UseComputerTime:=form2.checkbox16.checked;
   timezone:=gettimezone(now);
   if systemtimechange then begin
      InitDate;
      reboot:=true;
   end;
   phaseeffect:=form2.checkbox1.checked;
   librationeffect:=form2.checkbox2.checked;
   Geocentric:=form2.checkbox3.checked;
   if usedatabase[1]<>form2.checkbox19.checked then reloaddb:=true;
   if usedatabase[2]<>form2.checkbox20.checked then reloaddb:=true;
   if usedatabase[3]<>form2.checkbox21.checked then reloaddb:=true;
   if usedatabase[4]<>form2.checkbox22.checked then reloaddb:=true;
   if usedatabase[5]<>form2.checkbox23.checked then reloaddb:=true;
   if usedatabase[6]<>form2.checkbox24.checked then reloaddb:=true;
   usedatabase[1]:=form2.checkbox19.checked;
   usedatabase[2]:=form2.checkbox20.checked;
   usedatabase[3]:=form2.checkbox21.checked;
   usedatabase[4]:=form2.checkbox22.checked;
   usedatabase[5]:=form2.checkbox23.checked;
   usedatabase[6]:=form2.checkbox24.checked;
   for i:=0 to form2.Checklistbox1.Count-1 do begin
      j:=(form2.Checklistbox1.Items.Objects[i] as TDBinfo).dbnum;
      if usedatabase[j]<>form2.Checklistbox1.Checked[i] then reloaddb:=true;
      usedatabase[j]:=form2.Checklistbox1.Checked[i];
   end;
   InitObservatoire;
   CurrentJD:=jd(CurYear,CurrentMonth,CurrentDay,Currenttime-timezone+DT_UT);
   case form2.RadioGroup1.ItemIndex of
        0 : reducetexture:=1;
        1 : reducetexture:=2;
        2 : reducetexture:=4;
   end;
   case form2.RadioGroup3.ItemIndex of
        0 : reducetexturefar:=1;
        1 : reducetexturefar:=2;
        2 : reducetexturefar:=4;
   end;
   if reducetexture<>oldreduc then reboot:=true;
   if reducetexturefar<>oldreducfar then reboot:=true;
   if config.newlang<>language then begin
     language:=newlang;
     setlang;
     reloaddb:=true;
   end;
   if reloaddb then LoadDB(dbm);
   {$IFDEF opengl}
   MipMaps:=form2.CheckBox10.Checked;
   SetMinFilter(MipMaps);
   if useOpenGL<>form2.checkbox4.checked then reboot:=true;
   useOpenGL:=form2.checkbox4.checked;
   LibrationButton.Down:=librationeffect;
   {$ENDIF}
   PhaseButton.Down:=phaseeffect;
   if oldtexture<>form2.radiogroup2.itemindex then reboot:=true;
   GeologicalMap:=false;
   {$ifdef vmalight}
    GeologicalMap:=(form2.radiogroup2.itemindex=1);
   {$endif}
   externalimage:=form2.CheckBox12.Checked;
   LopamDirect:=form2.CheckBox15.Checked;
   externalimagepath:=form2.edit4.Text;
   saveimagewhite:=form2.CheckBox7.Checked;
   val(form2.ComboBox4.Text,saveimagesize,i);
   if i<>0 then saveimagesize:=0;
   maximgdir:=0;
   for i:=1 to form2.StringGrid1.RowCount-1 do if trim(form2.StringGrid1.Cells[1,i])>'' then inc(maximgdir);
   setlength(imgdir,maximgdir);
   j:=0;
   for i:=1 to form2.StringGrid1.RowCount-1 do
     if trim(form2.StringGrid1.Cells[1,i])>'' then begin
     imgdir[j,0]:=form2.StringGrid1.Cells[1,i];
     imgdir[j,2]:=form2.StringGrid1.Cells[0,i];
     imgdir[j,1]:=GetCpy(imgdir[j,0],imgdir[j,2]);
     inc(j);
   end;
   maxima:=form2.updown1.Position;
   if oldmaxima<>maxima then begin
      setlength(piczoom,maxima);
      setlength(pictop,maxima);
      setlength(picleft,maxima);
      setlength(ima,maxima);
      if maxima>oldmaxima then for i:=oldmaxima+1 to maxima do begin
         piczoom[i-1]:=0;
         pictop[i-1]:=20*i;
         picleft[i-1]:=20*i;
      end;
   end;
   for i:=1 to 10 do begin
     eyepiecename[i]:=trim(form2.StringGrid2.Cells[0,i]);
     eyepiecefield[i]:=strtointdef(form2.StringGrid2.Cells[1,i],0);
     eyepiecemirror[i]:=strtointdef(form2.StringGrid2.Cells[2,i],0);
     eyepiecerotation[i]:=strtointdef(form2.StringGrid2.Cells[3,i],0);
   end;
   SetEyepieceMenu;
   LeftMargin:=form2.LongEdit1.Value;
   PrintTextWidth:=form2.TrackBar1.Position;
   memo2.Width:=PrintTextWidth;
   PrintChart:=form2.CheckBox13.Checked;
   PrintEph:=form2.CheckBox8.Checked;
   PrintDesc:=form2.CheckBox9.Checked;
//   if (not Useopengl)and(not reboot)and(not phaseeffect) then form1.image1.Picture.Bitmap.LoadFromFile(tmpmap);
   if reboot then begin
      application.processmessages;
      currenteyepiece:=0;
      {$IFDEF opengl}
      if useOpengl then begin
         annulus1.Visible:=false;
         GLsceneViewer1.Buffer.BackgroundColor:=clBlack;
      end;
      {$ENDIF}
      yy:=zoom*minfoc;
      Window2World(lastx,lasty,x,y);
      InitGraphic(Sender);
      RefreshMoonImage;
      if yy<minfoc then yy:=minfoc;
      SetZoom(yy);
      if (not UseOpenGL)and(abs(x)<0.5)and(abs(y)<0.5) then MoveCamera(x,y);
      application.processmessages;
      Formresize(Sender);
   end
   else RefreshMoonImage;
   SaveDefault;
end;
finally
  screen.cursor:=crdefault;
  {$ifdef openglx}
  formglx.show;
  {$endif}
end;
end;

procedure TForm1.FormResize(Sender: TObject);
var dx:integer;
begin
if skipresize then exit;
if csDestroying in form1.ComponentState then exit;
if csLoading in form1.ComponentState then exit;
{$IFDEF opengl}
if useOpenGL then begin
  form1.glscene1.BeginUpdate;
  HUDSprite2.Visible:=false;
  panel2.left:=0;
  panel2.top:=form1.ControlBar1.Height;
  panel2.Height:=form1.ClientHeight-form1.ControlBar1.Height-form1.StatusBar1.Height;
  panel2.width:=panel2.height;
  glsceneviewer1.Width:=panel2.clientwidth-scrollbar2.Width;
  glsceneviewer1.Height:=panel2.clientHeight-scrollbar1.Height;
  pagecontrol1.left:=panel2.left+panel2.width;
  pagecontrol1.top:=ControlBar1.Height;
  pagecontrol1.width:=form1.ClientWidth-panel2.width;
  pagecontrol1.height:=form1.ClientHeight-ControlBar1.Height-form1.StatusBar1.Height;
  Mark(shapePositionX,shapePositionY,hudtext1.Text);
  RefreshLabel;
  form1.glscene1.EndUpdate;
  {$ifdef openglx}
  if formglx<>nil then begin
    formglx.clientwidth:=glsceneviewer1.width;
    formglx.clientheight:=glsceneviewer1.height;
    formglx.left:=panel2.clientorigin.x;
    formglx.top:=panel2.clientorigin.y;
  end;
  {$endif}

end else
{$ENDIF}
begin
  panel2d.left:=0;
  panel2d.top:=ControlBar1.Height;
  panel2d.Height:=form1.ClientHeight-ControlBar1.Height-form1.StatusBar1.Height;
  panel2d.width:=panel2d.height;
  panel3.Width:=panel2d.Width-vertscrollbar.width;
  panel3.height:=panel2d.Height-horzscrollbar.height;
  Image1.picture.bitmap.Width:=panel3.Width;
  Image1.picture.bitmap.Height:=panel3.height;
  Image1.width:=panel3.Width;
  Image1.height:=panel3.height;
  pagecontrol1.left:=panel2d.left+panel2d.width;
  pagecontrol1.top:=ControlBar1.Height;
  pagecontrol1.width:=form1.ClientWidth-panel2d.width;
  pagecontrol1.height:=form1.ClientHeight-ControlBar1.Height-form1.StatusBar1.Height;
  dx:=round(image2d.width/zoom/2);
  posmin:=dx;
  posmax:=HorzScrollBar.max-dx;
  bxpos:=maxintvalue([1,posmax-posmin])/HorzScrollBar.max;
  if currenteyepiece>0 then DrawEyepiece else ClearEyepiece;
  Refresh2DImage;
end;
statusbar1.left:=0;
statusbar1.top:=form1.ClientHeight-form1.StatusBar1.Height;
statusbar1.width:=form1.ClientWidth;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
initdate;
RefreshMoonImage;
end;

procedure TForm1.Button10Click(Sender: TObject);
begin
form1.heure.Value:=0;
form1.minute.Value:=0;
form1.seconde.Value:=0;
form1.updown4.position:=0;
form1.updown5.position:=0;
form1.updown6.position:=0;
Button4Click(Sender);
end;

procedure TForm1.Button4Click(Sender: TObject);
var y,m,d : integer;
    h,n,s : word;
begin
y:=form1.annee.Value;
m:=form1.mois.Value;
d:=form1.jour.Value;
h:=form1.heure.Value;
n:=form1.minute.Value;
s:=form1.seconde.Value;
timezone:=gettimezone(encodedatetime(y,m,d,h,n,s,0));
dt_ut:=dtminusut(y);
CurYear:=y;
CurrentMonth:=m;
CurrentDay:=d;
Currenttime:=h+n/60+s/3600;
CurrentJD:=jd(y,m,d,Currenttime-timezone+DT_UT);
RefreshMoonImage;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
var p : integer;
begin
if locktrackbar then begin
   locktrackbar:=false;
   exit;
end;
p:=round(power(10,trackbar1.position/100));
if not useOpenGL and (abs(p-zoom*minfoc)<25)then exit;
 SetZoom(p);
end;

procedure TForm1.EphTimer1Timer(Sender: TObject);
begin
 if lockrepeat then exit;
 lockrepeat:=true;
 CurrentJD:=CurrentJD+EphStep;
 SetJDDate;
 RefreshMoonImage;
 EphTimer1.interval:=50;
 lockrepeat:=false;
end;

procedure TForm1.Button3MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
EphStep:=+1;
lockrepeat:=false;
ephtimer1timer(sender);
EphTimer1.interval:=1000;
EphTimer1.Enabled:=true;
end;

procedure TForm1.Button3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
EphTimer1.Enabled:=false;
end;

procedure TForm1.Button6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
EphStep:=+1/24;
lockrepeat:=false;
ephtimer1timer(sender);
EphTimer1.interval:=1000;
EphTimer1.Enabled:=true;
end;

procedure TForm1.Button7MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
EphStep:=-1/24;
lockrepeat:=false;
ephtimer1timer(sender);
EphTimer1.interval:=1000;
EphTimer1.Enabled:=true;
end;

procedure TForm1.Button8MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
EphStep:=-1;
lockrepeat:=false;
ephtimer1timer(sender);
EphTimer1.interval:=1000;
EphTimer1.Enabled:=true;
end;

procedure TForm1.ToolButton5Click(Sender: TObject);
var xx,yy : double;
begin
if zoom=1 then Movecamera(0,0)
else if marked then MoveCamera(markx,marky)
else begin
 Window2World(lastx,lasty,xx,yy);
 if (abs(xx)<0.5)and(abs(yy)<0.5) then MoveCamera(xx,yy);
end;
end;

procedure TForm1.SetFullScreen;
begin
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i : integer;
begin
if key=27 then SetFullScreen;
if safemode then
if (key=13) then begin
  if password=exitpassword then form1.close
     else password:='';
end else begin
 password:=password+uppercase(chr(key));
 i:=length(password);
 if copy(exitpassword,1,i)<>password then password:='';
end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if SafeMode and (password<>exitpassword) then CanClose:=false
     else CanClose:=true;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var hnd:thandle;
    OldVal : LongInt;
    ok:boolean;
begin
try
{Set form1's window proc back to it's original procedure}
  SetWindowLong(Form1.Handle,
               GWL_WNDPROC,
               LongInt(OldWindowProc));

if not multi_instance then begin
  SaveDefault;
  SendMessage(HWND_BROADCAST,RegisterWindowMessage(ExitMsg),0,0);
end;
param.Free;
 if safemode then begin
   SystemParametersInfo (SPI_SCREENSAVERRUNNING, Word (False), @OldVal, 0); // restore system keys
end;
if skychartrunning and startedbyds then begin
  hnd:=findwindow(nil,Pchar(skychartcaption));
  if hnd>0 then PostMessage(hnd,WM_QUIT,0,0);   // close skychart (use WM_CLOSE for the close prompt)
end;
if CloseVMAbrowser then begin
  hnd:=findwindow(nil,Pchar(VMAbrowser));
  if hnd>0 then PostMessage(hnd,WM_CLOSE,0,0);   // close VMA Browser (use WM_CLOSE to save the config)
end;
if ClosePhotlun then ExecuteFile('photlun.exe','-quit', appdir, SW_SHOWNOACTIVATE);
if scopelibok then
  if ScopeConnected then begin
     ScopeDisconnect(ok);
  end;
(* UnloadScopeLibrary; *)
except
end;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
 ToolButton1.CheckMenuDropdown;
end;

Procedure ShowImg(desc,nom:string; forceinternal:boolean);
var buf,buf1 : string;
    i,j : integer;
    jpg : Tjpegimage;
    bmp : Tbitmap;
begin
chdir(appdir);
i:=-1;
for j:=0 to maximgdir-1 do if uppercase(trim(desc))=uppercase(trim(imgdir[j,2])) then i:=j;
if i>=0 then begin
   buf:=Slash(imgdir[i,0])+trim(nom);
   buf1:=imgdir[i,1];
end else begin
   buf:=slash(desc)+trim(nom);
   buf1:='';
end;
if fileexists(buf) then begin
 if externalimage and (not forceinternal) and (not safemode) then begin
    inc(lastima);
    if lastima >= maxima then lastima:=0;
    jpg:=Tjpegimage.Create;
    bmp:=Tbitmap.Create;
    try
    i:=pos('.',nom);
    if i>0 then nom:=copy(nom,1,i-1);
    if uppercase(extractfileext(buf))='.JPG' then begin
      jpg.LoadFromFile(buf);
      bmp.Assign(jpg);
    end
      else bmp.LoadFromFile(buf);
    bmp.Canvas.Font.Name:='Arial';
    bmp.canvas.Font.Height:=-9;
    bmp.Canvas.TextOut(0,0,nom+' '+buf1);
    buf:=tmpdir+'$temp'+inttostr(lastima)+'.bmp';
    bmp.SaveToFile(buf);
    finally
    bmp.Free;
    jpg.Free;
    end;
    ExecuteFile(externalimagepath,'"'+buf+'"', '', SW_SHOWNOACTIVATE);
 end else begin
 inc(lastima);
 if lastima >= maxima then lastima:=0;
 if ima[lastima]=nil then begin
    ima[lastima]:=Tbigimaform.Create(application);
    {$ifdef openglx}
     ima[lastima].formstyle:=fsStayonTop;
    {$endif}
    ima[lastima].toolbutton1.caption:=imac1;
    ima[lastima].toolbutton2.caption:=imac2;
    ima[lastima].toolbutton3.caption:=imac3;
 end;
 if not ima[lastima].visible then begin
    ima[lastima].zoom:=PicZoom[lastima];
    ima[lastima].top:=PicTop[lastima];
    ima[lastima].left:=PicLeft[lastima];
 end;
 if forceinternal then ima[lastima].zoom:=0;
 ima[lastima].image1.visible:=true;
 ima[lastima].toolbar1.visible:=true;
 ima[lastima].titre:=nom;
 ima[lastima].labeltext:=buf1;
 ima[lastima].Toolbar1.hint:=buf1;
 ima[lastima].LoadImage(buf);
 ima[lastima].Init;
 ima[lastima].Show;
end;
end;
end;

procedure TForm1.ToolButton8Click(Sender: TObject);
begin
   ToolButton8.CheckMenuDropdown;
end;

procedure TForm1.Apropos1Click(Sender: TObject);
begin
  ToolButton8.Down:=false;
  splash := Tsplash.create(application);
  splashunit.SplashTimer:=false;
  splash.BorderStyle:=bsToolWindow;
  splash.caption:=stringreplace(Apropos1.caption,'&','',[]);;
  splash.VersionName:=VersionName;
  splash.Splashversion:=Splashversion;
  splash.transmsg:=transmsg;
  splash.show;
  splash.refresh;
end;

procedure TForm1.ToolButton9Click(Sender: TObject);
begin
setzoom(minfoc);
end;

procedure TForm1.Edit1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if key=13 then button1.Click;  //Enter
end;

procedure TForm1.CheckBox19Click(Sender: TObject);

var changed:boolean;

begin
 changed:=(phasehash<>checkbox19.checked);
 phasehash:=checkbox19.checked;
 Trackbar2.enabled:=not phasehash;
 if changed then RefreshMoonImage;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
{$IFDEF opengl}
if useopengl then begin
GLlightSource1.Ambient.AsWinColor:=SetWhitecolor(Trackbar2.position);
end else
{$ENDIF}
if pagecontrol1.activepage=reglage then begin
  ReglageTimer.enabled:=false;
  ReglageTimer.enabled:=true;
end;
end;

procedure TForm1.ReglageTimerTimer(Sender: TObject);
var changed:boolean;
begin
 changed:=(Trackbar2.position<>(phaseumbra and $FF));
 if changed and (not phaseumbrachanging) then begin
    phaseumbrachanging:=true;
    phaseumbra:=SetWhitecolor(Trackbar2.position);
    try
      screen.cursor:=crHourGlass;
      RefreshMoonImage;
    finally
      phaseumbrachanging:=false;
      screen.cursor:=crDefault;
    end;
 end;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
{$IFDEF opengl}
GLlightSource1.diffuse.AsWinColor:=SetWhitecolor(Trackbar3.position);
{$ENDIF}
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
begin
{$IFDEF opengl}
GLlightSource1.specular.AsWinColor:=SetWhitecolor(Trackbar4.position);
{$ENDIF}
end;

procedure TForm1.TrackBar5Change(Sender: TObject);
{$IFDEF opengl}
var i : integer;
{$ENDIF}
begin
{$IFDEF opengl}
i:=Trackbar5.position;
if i=5 then i:=720
else if i=4 then i:=360
else if i=3 then i:=180
else if i=2 then i:=90
else if i=1 then i:=45
else i:=90;
form1.glscene1.BeginUpdate;
sphere1.Slices:=i; sphere1.Stacks:=i;
sphere2.Slices:=i; sphere2.Stacks:=i;
sphere3.Slices:=i; sphere3.Stacks:=i;
sphere4.Slices:=i; sphere4.Stacks:=i;
sphere5.Slices:=i; sphere5.Stacks:=i;
sphere6.Slices:=i; sphere6.Stacks:=i;
sphere7.Slices:=i; sphere7.Stacks:=i;
sphere8.Slices:=i; sphere8.Stacks:=i;
case sphere1.Slices of
45  : i:=16;
90  : i:=32;
180 : i:=64;
360 : i:=128;
720 : i:=256;
end;
hiressphere.Slices:=i ; hiressphere.Stacks:=i ;
hiressphere500.Slices:=i ; hiressphere500.Stacks:=i ;
form1.glscene1.EndUpdate;
{$ENDIF}
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
{$IFDEF opengl}
Label15.Caption:=Format(m[44]+' %.2f FPS', [GLSceneViewer1.FramesPerSecond]);
GLSceneViewer1.ResetPerformanceMonitor;
{$ENDIF}
end;

procedure TForm1.PageControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
begin
 if pagecontrol1.ActivePage=Outils then begin
    mark(0,0,'');
    {$IFDEF opengl}
    if useOpenGl then begin
      GLSceneViewer1.Cursor:=crRetic;
      HUDSprite2.Visible:=false;
    end else
    {$ENDIF}
    begin
      Image1.Cursor:=crRetic;
    end;
 end;
end;

procedure TForm1.PageControl1Change(Sender: TObject);
begin

if (pagecontrol1.ActivePage=Reglage) then begin
{$IFDEF opengl}
 if useopengl then begin
   timer1.enabled:=true;
   reglagetimer.enabled:=false;
   GLCadencer1.Enabled:=true;
   Label15.Caption:=m[44]+' 0 FPS';
   case GLsceneviewer1.Buffer.Acceleration of
   chaUnknown  :label17.Caption:=m[68];
   chaHardware :label17.Caption:=m[69];
   chaSoftware :label17.Caption:=m[70];
   end;
   label18.Caption:=m[71]+' : '+inttostr(GLmaterialLibrary1.Materials[0].Material.Texture.Image.width)+'x'+inttostr(GLmaterialLibrary1.Materials[0].Material.Texture.Image.height);
 end else
{$ENDIF}
 begin
   reglagetimer.enabled:=false;
 end;
end else begin
   reglagetimer.enabled:=false;
   {$IFDEF opengl}
   timer1.enabled:=false;
   GLCadencer1.Enabled:=false;
   {$ENDIF}
end;

if pagecontrol1.ActivePage=Terminateur then begin
   if currentphase<>tphase then UpdTerminateur;
end;
if pagecontrol1.ActivePage=Outils then begin
   if measuringdistance then begin
      {$IFDEF opengl}
      if useOpenGL then GLSceneViewer1.Cursor:=crPointing
      else
      {$ENDIF}
      begin
            Image1.Cursor:=crPointing;
      end;
      Button11.Caption:=m[53];
   end
   else begin
      {$IFDEF opengl}
      if useOpenGL then GLSceneViewer1.Cursor:=crRetic
      else
      {$ENDIF}
      begin
            Image1.Cursor:=crRetic;
      end;
      Button11.Caption:=m[52];
   end;
end else begin
   measuringdistance:=false;
   {$IFDEF opengl}
   if useopengl then begin
     GLSceneViewer1.Cursor:=crRetic;
     HUDSprite2.Visible:=false;
   end else
   {$ENDIF}
   begin
     Image1.Cursor:=crRetic;
   end;
end;
end;

{$IFDEF opengl}
procedure TForm1.GLCadencer1Progress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
  perfdeltay:=-perfdeltay;
  GLCamera1.Position.y:=GLCamera1.Position.y+perfdeltay;
//  GLCamera1.Position.y:=(round(newtime) mod 10)/10000;
end;

procedure TForm1.RotationCadencerProgress(Sender: TObject; const deltaTime,
  newTime: Double);
begin
 lrot:=lrot+rotstep*rotdirection*deltatime;
 inclination:=trackbar6.Position;
 GLSceneViewer1MouseMove(nil,[ssLeft],lastx,lasty);
end;

procedure TForm1.TrackBar6Change(Sender: TObject);
begin
 inclination:=trackbar6.Position;
 GLSceneViewer1MouseMove(nil,[ssLeft],lastx,lasty);
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
trackbar6.Position:=0;
end;

procedure TForm1.Stop1Click(Sender: TObject);
begin
RotationCadencer.enabled:=false;
end;

procedure TForm1.EastWest1Click(Sender: TObject);
begin
rotdirection:=-rotdirection;
end;

procedure TForm1.N10seconde1Click(Sender: TObject);
begin
combobox4.itemindex:=0;
rotstep:=10;
RotationCadencer.enabled:=false;
RotationCadencer.enabled:=true;
end;

procedure TForm1.N5seconde1Click(Sender: TObject);
begin
combobox4.itemindex:=1;
rotstep:=5;
RotationCadencer.enabled:=false;
RotationCadencer.enabled:=true;
end;

procedure TForm1.N1seconde1Click(Sender: TObject);
begin
combobox4.itemindex:=2;
rotstep:=1;
RotationCadencer.enabled:=false;
RotationCadencer.enabled:=true;
end;

procedure TForm1.N05seconde1Click(Sender: TObject);
begin
combobox4.itemindex:=3;
rotstep:=0.5;
RotationCadencer.enabled:=false;
RotationCadencer.enabled:=true;
end;

procedure TForm1.N02seconde1Click(Sender: TObject);
begin
combobox4.itemindex:=4;
rotstep:=0.2;
RotationCadencer.enabled:=false;
RotationCadencer.enabled:=true;
end;

procedure TForm1.ComboBox4Change(Sender: TObject);
begin
case combobox4.itemindex of
0 : rotstep:=10;
1 : rotstep:=5;
2 : rotstep:=1;
3 : rotstep:=0.5;
4 : rotstep:=0.2;
end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
RotationCadencer.enabled:=false;
sleep(50);
rotdirection:=1;
RotationCadencer.enabled:=true;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);

begin
RotationCadencer.enabled:=false;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);

begin
RotationCadencer.enabled:=false;
sleep(50);
rotdirection:=-1;
RotationCadencer.enabled:=true;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
setzoom(4*minfoc);
movecamera(0.45,0);
CameraOrientation:=90;
SetCameraOrientation;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);

begin
setzoom(4*minfoc);
movecamera(-0.45,0);
CameraOrientation:=-90;
SetCameraOrientation;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
setzoom(2*minfoc);
movecamera(0,0);
case RadioGroup2.ItemIndex of
0 : CameraOrientation:=0;
1 : CameraOrientation:=180;
end;
SetCameraOrientation;
end;


{$ENDIF}

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
Updterminateur;
end;

procedure TForm1.ListBox1Click(Sender: TObject);
var i,p : integer;
begin
i:=listbox1.ItemIndex;
if i>=0 then begin
    Firstsearch:=true;
    SearchText:=listbox1.Items[i];
    p:=pos(' ',SearchText)+1;
    case RadioGroup1.ItemIndex of
    1 : SearchText:=copy(Searchtext,p,999);
    2 : SearchText:=copy(Searchtext,p,999);
    3 : SearchText:=copy(Searchtext,p,999);
    end;
    SearchName(SearchText,true);
end;
end;

Function InitConv : boolean;
begin
result:=false;
with Form1 do if ddeclientconv1.SetLink('ciel','DdeSkyChart') then begin   // initialize conversation
 ddeclientitem1.DdeItem:='DdeData';                  // if ready initialize items
 result:=true;
 SkyChartStarting:=false;
end;
end;

Procedure InitSkyChart;
begin
skychartok := {StartedByDS and} (SkyChartRunning or SkyChartStarting);  // is app already running
if not SkyChartok then begin
       SkyChartStarting:=true;
       StartSkyChart(ddeparam);        // if not start it with parameters
       StartedByDS:=true;
       Form1.ChartTimer.Enabled:=true;  // wait app start
       end
else begin
   skychartok:=Initconv;         // otherwise start communication directely
   if not skychartok then Form1.ChartTimer.Enabled:=true;   // if not ok restart timer
end;
end;


procedure TForm1.ChartTimerTimer(Sender: TObject);
begin
ChartTimer.Enabled:=false;                        // stop timer to avoid multiple call
if SkyChartRunning then begin                     // is app running
   sleep(2000);                                   // a litle more time to initialize
   skychartok:=Initconv;                          // start communication
   if not skychartok then ChartTimer.Enabled:=true;   // if not ok restart timer
end
else ChartTimer.Enabled:=true;
end;

Function SendDDEcmd(cmd:string):boolean;
const timeout=10; //seconds
var timeo:double;
begin
DDEreceiveok:=false;
form1.ddeClientConv1.PokeData('DdeData',Pchar(cmd));
timeo:=now+timeout/3600/24;
while (not DDEreceiveok)and(timeo>now) do application.processmessages;
//if stop then break;
result:=DDEreceiveok;
end;

procedure TForm1.CartesduCiel1Click(Sender: TObject);
var cmd1,cmd2,cmd3 : string;
    h,m,s,ms : word;
begin
if skychartstarting or lockchart then exit;
try
lockchart:=true;
decodetime(currenttime/24,h,m,s,ms);
//cmd1 := 'MOVE RA:'+artostr(ra)+' DEC:'+detostr4(dec)+' FOV:'+detostr4(fov);
cmd1 := 'FIND CAT:14ID:11';
cmd2 := 'LAT:'+detostr3(obslatitude)+'LON:'+detostr3(obslongitude)+'ALT:0mTZ:'+floattostr(timezone)+'h'+'OBS:AVL';
cmd3 := inttostr(CurYear)+'-'+inttostr(currentmonth)+'-'+inttostr(currentday)+'T'+inttostr(h)+':'+inttostr(m)+':'+inttostr(s);
ddeparam :=' -c '+Slash(appdir)+Slash('database')+'virtualmoon.cdc'+
         ' -ar '+floattostr(ra)+' -de '+floattostr(dec)+' -w 45'+
         ' -o '+cmd2+' -d '+cmd3;
InitSkychart;
if skychartok then begin
  CielHnd:=findwindow(nil,Pchar(skychartcaption));
  SendMessage(CielHnd, WM_SYSCOMMAND, SC_RESTORE, 0);
  if @SwitchToThisWindow=nil then
     SetForegroundWindow(CielHnd)
  else
     SwitchToThisWindow(CielHnd,true);
  if not SendDDEcmd('DATE '+cmd3) then raise exception.create('CDC timeout');
  if not SendDDEcmd('OBSL '+cmd2) then raise exception.create('CDC timeout');
  if not SendDDEcmd('RFSH 1') then raise exception.create('CDC timeout');
  if not SendDDEcmd(cmd1) then raise exception.create('CDC timeout');
end;
finally
lockchart:=false;
end;
end;


procedure TForm1.DdeClientItem1Change(Sender: TObject);
var buf : string;
    p : integer;
begin
if ddeclientitem1.lines=nil then exit;
if ddeclientitem1.lines.count=0 then exit;
buf:=ddeclientitem1.lines[0];
if pos(' ACK',buf)>0 then DDEreceiveok:=true;
if pos(' NAK',buf)>0 then raise exception.create('CDC signal an error');
p:=pos('FOV:',buf);
if p>0 then begin
   buf:=copy(buf,p+4,999);
   p:=pos('°',buf);
   fov:=strtofloat(copy(buf,1,p-1));
   buf:=copy(buf,p+1,999);
   p:=pos('''',buf);
   fov:=fov+strtofloat(copy(buf,1,p-1))/60;
   buf:=copy(buf,p+1,999);
   p:=pos('"',buf);
   fov:=fov+strtofloat(copy(buf,1,p-1))/3600;
end;
end;

procedure TForm1.Aide2Click(Sender: TObject);
begin
internalbrowser:=true;
//showhelp('Index_Doc','_'+versionname,'doc');
showhelp('Index_Doc','','doc');
end;

procedure TForm1.Encyclopedia1Click(Sender: TObject);
begin
internalbrowser:=true;
showhelp('Encyclopedia','','encyclopedia');
end;

procedure TForm1.Position1Click(Sender: TObject);
var ok : boolean;
begin
if multi_instance and (clientwidth=ClientHeight-controlbar1.height-statusbar1.height) then clientwidth:=round(1.333*clientwidth);
PageControl1Changing(Sender,ok);
Pagecontrol1.ActivePage:=Position;
PageControl1Change(Sender);
combobox1.SetFocus;
end;

procedure TForm1.Notes1Click(Sender: TObject);
var ok : boolean;
begin
if multi_instance and (clientwidth=ClientHeight-controlbar1.height-statusbar1.height) then clientwidth:=round(1.333*clientwidth);
PageControl1Changing(Sender,ok);
Pagecontrol1.ActivePage:=Notes;
PageControl1Change(Sender);
end;

procedure TForm1.x21Click(Sender: TObject);
//var xx,yy:double;
begin
//Window2World(lastx,lasty,xx,yy);
//if (abs(xx)<0.5)and(abs(yy)<0.5) then MoveCamera(xx,yy);
setzoom(2*minfoc);
end;

procedure TForm1.x41Click(Sender: TObject);
//var xx,yy:double;
begin
//Window2World(lastx,lasty,xx,yy);
//if (abs(xx)<0.5)and(abs(yy)<0.5) then MoveCamera(xx,yy);
setzoom(4*minfoc);
end;

procedure TForm1.x81Click(Sender: TObject);
//var xx,yy:double;
begin
//Window2World(lastx,lasty,xx,yy);
//if (abs(xx)<0.5)and(abs(yy)<0.5) then MoveCamera(xx,yy);
setzoom(8*minfoc);
end;

procedure TForm1.Button12MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IFDEF opengl}
if shift=[ssShift] then CameraOrientation:=CameraOrientation-1
  else if shift=[ssAlt] then CameraOrientation:=CameraOrientation-90
  else CameraOrientation:=CameraOrientation-15;
CameraOrientation:=rmod(CameraOrientation+360,360);
SetCameraOrientation;
Mark(shapePositionX,shapePositionY,hudtext1.Text);
RefreshLabel;
{$ENDIF}
end;

procedure TForm1.Button13MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
{$IFDEF opengl}
if shift=[ssShift] then CameraOrientation:=CameraOrientation+1
  else if shift=[ssAlt] then CameraOrientation:=CameraOrientation+90
  else CameraOrientation:=CameraOrientation+15;
CameraOrientation:=rmod(CameraOrientation,360);
SetCameraOrientation;
Mark(shapePositionX,shapePositionY,hudtext1.Text);
RefreshLabel;
{$ENDIF}
end;

procedure TForm1.RadioGroup2Click(Sender: TObject);
begin
if lockrot then exit;
case RadioGroup2.ItemIndex of
0 : PoleOrientation:=0;
1 : PoleOrientation:=180;
end;
ToolButton4.Down:=(RadioGroup2.ItemIndex=1);
{$IFDEF opengl}
if useopengl then begin
  if FollowNorth then CameraOrientation:=rmod(-PA+PoleOrientation+360,360)
                 else CameraOrientation:=Poleorientation;
  SetCameraOrientation;
  Mark(shapePositionX,shapePositionY,hudtext1.Text);
  RefreshLabel;
end else
{$ENDIF}
  if currenttexture<>'' then begin
  lockscrollbar:=true;
  HorzScrollBar.Position:=HorzScrollBar.max - HorzScrollBar.Position;
  lockscrollbar:=true;
  VertScrollBar.Position:=VertScrollBar.max - VertScrollBar.Position;
  Load2DTexture(currenttexture);
  RefreshMoonImage;
  Mark(shapePositionX,shapePositionY,label5.Caption);
end;
end;

procedure TForm1.ToolButton4Click(Sender: TObject);
begin
RadioGroup2.ItemIndex:=abs(RadioGroup2.ItemIndex-1);
ToolButton4.Down:=(RadioGroup2.ItemIndex=1);
end;

procedure TForm1.ToolButton6Click(Sender: TObject);
begin
CheckBox2.Checked:=not CheckBox2.Checked;
ToolButton6.Down:=CheckBox2.Checked;
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
  MeasuringDistance:=not MeasuringDistance;
  if measuringdistance then begin
          Button11.Caption:=m[53];
          {$IFDEF opengl}
          if useopengl then begin
             GLSceneViewer1.Cursor:=crPointing;
          end else
          {$ENDIF}
          begin
             Image1.Cursor:=crPointing;
          end;
     end
     else begin
          Button11.Caption:=m[52];
          {$IFDEF opengl}
          if useopengl then begin
             GLSceneViewer1.Cursor:=crRetic;
             HUDSprite2.Visible:=false;
          end else
          {$ENDIF}
          begin
             Image1.Cursor:=crRetic;
          end;
  end;
end;

procedure TForm1.Distance1Click(Sender: TObject);
begin
if multi_instance and (clientwidth=ClientHeight-controlbar1.height-statusbar1.height) then clientwidth:=round(1.333*clientwidth);
Pagecontrol1.ActivePage:=Outils;
PageControl1Change(Sender);
Button11.Caption:=m[53];
MeasuringDistance:=false;
Button11Click(Sender);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
{$IFDEF opengl}
FollowNorth:=CheckBox1.Checked;
if FollowNorth then CameraOrientation:=rmod(-PA+PoleOrientation+360,360)
               else CameraOrientation:=Poleorientation;
SetCameraOrientation;
RefreshLabel;
{$ENDIF}
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
{$IFDEF opengl}
ToolButton6.Down:=checkbox2.checked;
if useOpenGL then begin
  SetMirror(checkbox2.checked);
  Mark(shapePositionX,shapePositionY,hudtext1.Text);
  RefreshLabel;
end else
{$ENDIF}
begin
  if checkbox2.checked then flipx:=-1
                       else flipx:=1;
  if currenttexture<>'' then begin
    lockscrollbar:=true;
    HorzScrollBar.Position:=HorzScrollBar.max - HorzScrollBar.Position;
    Load2DTexture(currenttexture);
    RefreshMoonImage;
    Mark(shapePositionX,shapePositionY,label5.Caption);
  end;  
end;
end;

procedure TForm1.ToolButton10Click(Sender: TObject);
begin
listobject(5);
end;

procedure TForm1.SnapShot(var bmp : TBitmap; white:boolean);
{$IFDEF opengl}
var
   bmp32 : TGLBitmap32;
{$ENDIF}
begin
{$IFDEF opengl}
   if useopengl then begin
   if white then begin
     GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
     GLSceneViewer1.Update;
   end;
   bmp32:=GLSceneViewer1.Buffer.CreateSnapShot;
   bmp:=bmp32.Create32BitsBitmap;
   GLSceneViewer1.Buffer.BackgroundColor:=clBlack;
   bmp32.Free;
end else
{$ENDIF}
begin
   bmp.Width:=round(panel3.clientWidth);
   bmp.Height:=bmp.Width;
   bmp.PixelFormat:=image1.Picture.Bitmap.PixelFormat;
   panel3.PaintTo(bmp.Canvas,0,0);
   if saveimagewhite then begin
      bmp.Canvas.Brush.Style:=bsSolid;
      bmp.Canvas.Brush.Color:=clWhite;
      bmp.Canvas.FloodFill(1,1,clBlack,fsSurface);
      bmp.Canvas.FloodFill(1,bmp.height-1,clBlack,fsSurface);
      bmp.Canvas.FloodFill(bmp.width-1,1,clBlack,fsSurface);
      bmp.Canvas.FloodFill(bmp.width-1,bmp.height-1,clBlack,fsSurface);
   end;
end;
end;

{$IFDEF opengl}
procedure TForm1.RenderToBitmap(var bmp : TBitmap; size : integer; white:boolean);
begin
   bmp.PixelFormat:=pf24bit;
   bmp.Width:=size;
   bmp.Height:=size;
   if white then GLSceneViewer1.Buffer.BackgroundColor:=clWhite;
   GLSceneViewer1.Buffer.RenderToBitmap(bmp,96);
   GLSceneViewer1.Buffer.BackgroundColor:=clBlack;
end;
{$ENDIF}


procedure TForm1.BMP1Click(Sender: TObject);
var b: tbitmap;
begin
savedialog1.DefaultExt:='.bmp';
savedialog1.Filter:='bmp image|*.bmp';
savedialog1.FileName:=combobox1.Text;
if savedialog1.Execute then begin
  b:=Tbitmap.Create;
  try
  SnapShot(b,saveimagewhite);
  b.SaveToFile(savedialog1.FileName);
  finally
  b.Free;
  end;
end;

end;


procedure TForm1.BMP15001Click(Sender: TObject);
// pour faire les images 2D
{$IFDEF opengl}
var b: tbitmap;
{$ENDIF}
begin
{$IFDEF opengl}
savedialog1.DefaultExt:='.bmp';
savedialog1.Filter:='bmp image|*.bmp';
savedialog1.FileName:=combobox1.Text;
if savedialog1.Execute then begin
  b:=Tbitmap.Create;
  try
  Rendertobitmap(b,1500,false);
  b.SaveToFile(savedialog1.FileName);
  finally
  b.Free;
  end;
end;
{$ENDIF}
end;

procedure TForm1.BMP30001Click(Sender: TObject);
{$IFDEF opengl}
var b: tbitmap;
{$ENDIF}
begin
{$IFDEF opengl}
savedialog1.DefaultExt:='.bmp';
savedialog1.Filter:='bmp image|*.bmp';
savedialog1.FileName:=combobox1.Text;
if savedialog1.Execute then begin
  b:=Tbitmap.Create;
  try
  Rendertobitmap(b,3000,false);
  b.SaveToFile(savedialog1.FileName);
  finally
  b.Free;
  end;
end;
{$ENDIF}
end;

procedure TForm1.JPG1Click(Sender: TObject);
var b: Tbitmap;
    j: Tjpegimage;
begin
savedialog1.DefaultExt:='.jpg';
savedialog1.Filter:='jpeg image|*.jpg';
savedialog1.FileName:=combobox1.Text;
if savedialog1.Execute then begin
  b:=Tbitmap.Create;
  j:=Tjpegimage.Create;
  try
  SnapShot(b,saveimagewhite);
  j.Assign(b);
  j.SaveToFile(savedialog1.FileName);
  finally
  b.Free;
  j.Free;
  end;
end;
end;

{procedure TForm1.Snapshot1Click(Sender: TObject);
var b: Tbitmap;
    j: Tjpegimage;
    fn: string;
begin
  fn:='snapshot.jpg';
  b:=Tbitmap.Create;
  j:=Tjpegimage.Create;
  try
  SnapShot(b,false);
  j.Assign(b);
  j.SaveToFile(slash(appdir)+fn);
  ShowImg(appdir,fn,false);
  finally
  b.Free;
  j.Free;
  end;
end;}

procedure TForm1.Snapshot1Click(Sender: TObject);
var b: Tbitmap;
    fn: string;
begin
  fn:='snapshot.bmp';
  b:=Tbitmap.Create;
  try
  SnapShot(b,false);
  b.SaveToFile(slash(appdir)+fn);
  ShowImg(appdir,fn,false);
  finally
  b.Free;
  end;
end;

procedure TForm1.Selectiondimprimante1Click(Sender: TObject);
begin
PrinterSetupDialog1.Execute;
GetPrinterResolution(PrtName,PrinterResolution);
end;

procedure TForm1.Imprimer1Click(Sender: TObject);
var i,w,ww,tl,hl,l,maxt : integer;
    s : double;
    b: tbitmap;
    buf1 : string;
//    buf:pchar;
//    txt:string;
//    c:char;
begin
GetPrinterResolution(PrtName,PrinterResolution);
s:= PrinterResolution/300;
Printer.Orientation := poPortrait;
Printer.Title:=currentname;
Printer.BeginDoc;
with Printer do begin
 xmin := Canvas.ClipRect.left+round(LeftMargin*PrinterResolution/25.4);
 xmax := Canvas.ClipRect.right;
 ymin := Canvas.ClipRect.top;
 ymax := Canvas.ClipRect.bottom;
 Canvas.Font.Name:=memo2.Font.Name;
 Canvas.Font.Color := clBlack;
 Canvas.Font.Size:=8;
 hl:=round(Canvas.TextHeight('H')*1.1);
 Canvas.Font.Style:=[fsBold];
 buf1:='© '+form1.Caption;
 Canvas.TextOut(Xmin+10,ymin,buf1);
 i:=Canvas.TextWidth(buf1);
 Canvas.Font.Style:=[];
 if language='FR' then buf1:='  Reproduction interdite / Pour usage personnel uniquement'
                  else buf1:='  Forbidden copy / For personal use only';
 w:=Canvas.TextHeight(buf1);
 maxt:=ymax-2*w;
 Canvas.TextOut(Xmin+10+i,ymin,buf1);
 Canvas.TextOut(Xmin+10+i,ymax-w,buf1);
 w:=0;
 b:=Tbitmap.Create;
 try
 if PrintChart then
   // carte
   w:=(xmax-xmin) * 2 div 3;
   snapshot(b,true);
   PrintBitMap(Canvas,rect(xmin,ymin+hl,xmin+w,ymin+w+hl),b);
 if PrintDesc then begin
    ww:=w;
    if (ww=0) and PrintEph then ww:=(xmax-xmin) * 2 div 3;
    if PrinterResolution<>rotatefontresol then begin
      rotatefont.free;
      SetRotateFont(PrinterResolution);
    end;
    Canvas.Brush.Color:=clWhite;
    Canvas.pen.Color:=clWhite;
    Canvas.Font.Handle := rotatefont.Handle;
    Canvas.Font.Color := $e7e7e7;
    Canvas.TextOut(round(s*50),maxt-round(s*50), 'Lunar formations database V2.1 © Ch. Legrand');
    Canvas.Font.Name:=memo2.Font.Name;
    Canvas.Font.Color := clBlack;
    Canvas.Font.Size:=8;
    Canvas.Font.Style:=[];
    Canvas.Brush.Style := bsClear;
    tl:=ymin+ww+round(1.5*hl);
    l:=Xmin+round(s*10);
    memo2.Clear;
    dbm.Query('select * from moon where id='+currentid);
    if dbm.RowCount>0 then GetDetail(dbm.Results[0], memo2);
    for i:=1 to memo2.Lines.Count do begin
        if tl>=maxt then begin
          Canvas.TextOut(l,tl,'Truncated ...');
          break;
        end;
        buf1:=memo2.Lines[i-1];
        if (i=1)or(copy(buf1,length(buf1),1)=':') then Canvas.Font.Style:=[fsBold]
                                                  else Canvas.Font.Style:=[];
        Canvas.TextOut(l,tl,buf1);
        tl:=tl+hl;
    end;
    memo2.Clear;
 end;
 if PrintEph then begin
    tl:=ymin+3*hl;
    l:=round(s*30)+ xmin+w;
    Canvas.Font.Style:=[fsBold];
    Canvas.TextOut(l,tl,Ephemerides.Caption);
    Canvas.Font.Style:=[];
    tl:=tl+hl;
    for i:=1 to Stringgrid1.RowCount do begin
        Canvas.TextOut(l,tl,Stringgrid1.Cells[0,i-1]+' '+Stringgrid1.Cells[1,i-1]);
        tl:=tl+hl;
    end;
 end;
finally
b.Free;
Printer.EndDoc;
end;
end;
end;

procedure TForm1.Desc1HotSpotClick(Sender: TObject; const SRC: String;
  var Handled: Boolean);
var p : integer;
begin
if safemode then begin handled:=true; exit; end;  // don't launch external program in safe mode
p:=pos('file:',src);
if externalimage and(p>0)and(copy(uppercase(src),length(src)-3,4)='.JPG') then begin
   ExecuteFile(externalimagepath,'"'+copy(src,8,999)+'"', '', SW_SHOWNOACTIVATE);
   handled:=True;
end else begin
  p:=p+pos('http:',src);
  p:=p+pos('ftp:',src);
  p:=p+pos('mailto:',src);
  if p>0 then begin
    ExecuteFile(src, '', '', SW_SHOWNOACTIVATE);
    handled:=True;
  end else Handled := False;
end;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
Desc1.CopyToClipboard;
end;

procedure TForm1.SelectAll1Click(Sender: TObject);
begin
Desc1.SelectAll;
end;

procedure TForm1.LgendeGologique1Click(Sender: TObject);
var fn : string;
begin
chdir(appdir);
fn:='LegendGeo_'+language+'.jpg';
if not fileexists(slash('Textures')+fn) then fn:='LegendGeo_UK.jpg';
showimg('Textures',fn,true);
end;

procedure TForm1.ToolButton7Click(Sender: TObject);
var param,fx,fy: string;
    i: integer;
begin
{$ifdef vmapro}
 i:=findwindow(nil,pchar('PHOTLUN'));
 if i=0 then ClosePhotlun:=true;
 if flipx<0 then fx:='1' else fx:='0';
 if poleorientation>0 then fy:='1' else fy:='0';
 param:='-fx '+fx+' -fy '+fy;
 param:=param+' -nx -n "'+currentname+'"';
 ExecuteFile('photlun.exe',param, appdir, SW_SHOWNOACTIVATE);
{$else}
 ListImg(currentname);
{$endif} 
end;

procedure TForm1.OverlayCaption1Click(Sender: TObject);
var dir : string;
begin
chdir(appdir);
dir:=slash('Textures')+slash('overlay')+slash('caption');
if fileexists(dir+overlayname) then showimg(dir,overlayname,true);
end;

{$IFDEF opengl}
function GetIntLB(x,y:integer; var l,b:integer):boolean;
var xx,yy,lc,bc: double;
    resol,wd: integer;
begin
  Window2World(x,y,xx,yy);
  if InvProjMoon(2*xx,2*yy,librl,librb,lc,bc) then begin
    resol:=1;
    case form1.sphere1.Slices of
    45  : resol:=2;
    90  : resol:=1;
    180 : resol:=1;
    end;
    wd:=hi_wd div hi_mult;
    l:=round(rmod(lc+360+180,360));
    l:=(l div resol)*resol;
    if l>(360-wd) then l:=360-wd;
    if l<wd then l:=wd;
    b:=round(bc);
    b:=(b div resol)*resol;
    if b>(90-wd) then b:=90-wd;
    if b<(-90+wd) then b:=-90+wd;
    result:=true;
  end else result:=false;
end;
{$ENDIF}

{$IFDEF opengl}
Procedure SetHires500;
var x,y,l,b,l0,b0,i,n,wd,dl,w,smooth: integer;
    ll,lb : array [1..4]of integer;
    p:Pbytearray;
    ok:boolean;
    dest,src: Trect;
begin
with form1 do begin
try
ok:=true;
hi_mult:=2;
GetIntLB(0,0,ll[1],lb[1]);
GetIntLB(GLSceneViewer1.Width,0,ll[2],lb[2]);
GetIntLB(GLSceneViewer1.Width,GLSceneViewer1.Height,ll[3],lb[3]);
GetIntLB(0,GLSceneViewer1.Height,ll[4],lb[4]);
for i:=1 to 4 do begin
  if (ll[i]>form1.HiresSphere500.Stop)or
     (ll[i]<form1.HiresSphere500.Start)or
     (lb[i]>form1.HiresSphere500.Top)or
     (lb[i]<form1.HiresSphere500.Bottom) then begin
        ok:=false;
        break;
  end;
end;
if ok then hiressphere500.visible:=true
else begin
  x:=GLSceneViewer1.Width div 2;
  y:=GLSceneViewer1.Height div 2;
  wd:=hi_wd div hi_mult;
  dl:=hi_dl * hi_mult;
  w:=hi_w * hi_mult;
  if GetIntLB(x,y,l,b) then begin
    l0:=((l-wd+360) mod 360)*dl;
    b0:=(90-(b+wd))*dl;
    for i:=0 to 1023 do begin
       p:=hires500.scanline[i];
       seek(fh500,(b0+i)*w+l0);
       blockread(fh500,p[0],1024,n);
    end;
    HiresSphere500.BeginUpdate;
    if showoverlay then begin
       l0:=round(l0*overlayimg.width/w);
       b0:=round(b0*overlayimg.width/w);
       overlayhi.width:=round(1024*overlayimg.width/w);
       overlayhi.height:=overlayhi.width;
       dest:=rect(0,0,overlayhi.width,overlayhi.height);
       src:=rect(l0,b0,l0+overlayhi.width,b0+overlayhi.height);
       overlayhi.Canvas.CopyRect(dest,overlayimg.canvas,src);
       GLmaterialLibrary1.Materials[19].Material.Texture.Image.Assign(overlayhi);
       GLmaterialLibrary1.Materials[19].Material.Texture.disabled:=false;
    end else begin
       GLmaterialLibrary1.Materials[19].Material.Texture.disabled:=true;
    end;
    case sphere1.Slices of
       45  : smooth:=16 div hi_mult;
       90  : smooth:=32 div hi_mult;
       180 : smooth:=64 div hi_mult;
       else smooth:=16;
    end;
    hiressphere500.Slices:=smooth ;
    hiressphere500.Stacks:=smooth ;
    HiresSphere500.bottom:=-90;
    HiresSphere500.Top:=b+wd;
    HiresSphere500.bottom:=b-wd;
    HiresSphere500.Start:=0;
    HiresSphere500.Stop:=l+wd;
    HiresSphere500.Start:=l-wd;
    GLmaterialLibrary1.Materials[18].Material.Texture.Image.Assign(hires500);
    hiressphere500.visible:=true;
    HiresSphere500.EndUpdate;
  end;
end;
except
  hiressphere500.visible:=false;
end;
end;
end;

Procedure SetHires;
var x,y,l,b,l0,b0,i,n,wd,dl,w,smooth: integer;
    ll,lb : array [1..4]of integer;
    p:Pbytearray;
    ok:boolean;
    dest,src: Trect;
begin
with form1 do begin
if (zoom<4)or(not HiresOK)or(RotationCadencer.enabled) then begin
   hiressphere.visible:=false;
   hiressphere500.visible:=false;
end else begin
try
ok:=true;
hi_mult:=1;
GetIntLB(0,0,ll[1],lb[1]);
GetIntLB(GLSceneViewer1.Width,0,ll[2],lb[2]);
GetIntLB(GLSceneViewer1.Width,GLSceneViewer1.Height,ll[3],lb[3]);
GetIntLB(0,GLSceneViewer1.Height,ll[4],lb[4]);
for i:=1 to 4 do begin
  if (ll[i]>form1.HiresSphere.Stop)or
     (ll[i]<form1.HiresSphere.Start)or
     (lb[i]>form1.HiresSphere.Top)or
     (lb[i]<form1.HiresSphere.Bottom) then begin
        ok:=false;
        break;
  end;
end;
//if ok then debuglabel.caption:='ok ';
if ok then hiressphere.visible:=true
else begin
//  debuglabel.caption:='ini ';
  x:=GLSceneViewer1.Width div 2;
  y:=GLSceneViewer1.Height div 2;
  wd:=hi_wd div hi_mult;
  dl:=hi_dl * hi_mult;
  w:=hi_w * hi_mult;
//  debuglabel.caption:=debuglabel.caption+'1 ';
  if GetIntLB(x,y,l,b) then begin
//    debuglabel.caption:=debuglabel.caption+'2 ';
    l0:=((l-wd+360) mod 360)*dl;
    b0:=(90-(b+wd))*dl;
    filemode:=0;
//    assignfile(fh,Slash(appdir)+Slash('textures')+'hires'+imgsuffix+'.dat');
//    reset(fh,1);
    for i:=0 to 1023 do begin
      p:=hires.scanline[i];
      seek(fh,(b0+i)*w+l0);
      blockread(fh,p[0],1024,n);
    end;
//    closefile(fh);
//    debuglabel.caption:=debuglabel.caption+'3 ';
    HiresSphere.BeginUpdate;
    if showoverlay then begin
       l0:=round(l0*overlayimg.width/w);
       b0:=round(b0*overlayimg.width/w);
       overlayhi.width:=round(1024*overlayimg.width/w);
       overlayhi.height:=overlayhi.width;
       dest:=rect(0,0,overlayhi.width,overlayhi.height);
       src:=rect(l0,b0,l0+overlayhi.width,b0+overlayhi.height);
       overlayhi.Canvas.CopyRect(dest,overlayimg.canvas,src);
       GLmaterialLibrary1.Materials[17].Material.Texture.Image.Assign(overlayhi);
       GLmaterialLibrary1.Materials[17].Material.Texture.disabled:=false;
    end else begin
       GLmaterialLibrary1.Materials[17].Material.Texture.disabled:=true;
    end;
    case sphere1.Slices of
       45  : smooth:=16 div hi_mult;
       90  : smooth:=32 div hi_mult;
       180 : smooth:=64 div hi_mult;
       else smooth:=16;
    end;
//    debuglabel.caption:=debuglabel.caption+'4 ';
    hiressphere.Slices:=smooth ;
    hiressphere.Stacks:=smooth ;
    HiresSphere.bottom:=-90;
    HiresSphere.Top:=b+wd;
    HiresSphere.bottom:=b-wd;
    HiresSphere.Start:=0;
    HiresSphere.Stop:=l+wd;
    HiresSphere.Start:=l-wd;
    GLmaterialLibrary1.Materials[8].Material.Texture.Image.Assign(hires);
    hiressphere.visible:=true;
    HiresSphere.EndUpdate;
//    debuglabel.caption:=debuglabel.caption+'end ';
  end;
end;
if hires500ok and (zoom>=8) then SetHires500
   else hiressphere500.visible:=false;
except
  hiressphere.visible:=false;
end;
end;
end;
end;
{$ENDIF}

procedure TForm1.LabelTimerTimer(Sender: TObject);
begin
LabelTimer.Enabled:=false;
{$IFDEF opengl}
if useopengl then SetHires;
if (showautoLabel or (CurrentSelection<>''))and(zoom>=1)and(not RotationCadencer.enabled)and(not TelescopeTimer.enabled) then SetLabel;
{$ELSE}
if showautoLabel and(zoom>=1) then SetLabel;
{$ENDIF}
end;

procedure TForm1.Glossaire1Click(Sender: TObject);
begin
if gloss=nil then gloss:=TGloss.Create(application);
{$ifdef openglx}
  gloss.formstyle:=fsStayonTop;
{$endif}
gloss.button2.Caption:=imac1;
gloss.Caption:=stringreplace(Glossaire1.Caption,'&','',[]);
gloss.show;
end;

Procedure TForm1.Desc1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
if not Desc1.Focused then
   form1.ActiveControl:=Desc1;
end;

procedure TForm1.StringGrid1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
if not StringGrid1.Focused then
   form1.ActiveControl:=StringGrid1;
end;

procedure TForm1.ListBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
if not ListBox1.Focused then
   form1.ActiveControl:=ListBox1;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
// rotation
begin
 if ToolButton3.Down then begin
 {$IFDEF opengl}
  if useopengl then begin      // full rotation
   librl:=0;
   librb:=0;
   GroupBox4.visible:=false;
   ToolButton2.Enabled:=false;
   checkbox2.visible:=false;
   ToolButton6.enabled:=false;
   form1.Arrowline1.Visible:=false;
   SetMirror(false);
   GroupBox4.visible:=false;
   GroupBox3.visible:=true;
   LibrationButton.Enabled:=false;
   lrot:=0;
   inclination:=0;
   trackbar6.Position:=0;
   setzoom(minfoc);
   movecamera(0,0);
   GLSceneViewer1MouseMove(sender,[ssLeft],lastx,lasty);
   Rotation1.visible:=true;
//   Pagecontrol1.activepage:=Outils;
//   PageControl1Change(Sender);
  end else
  {$ENDIF}
  begin                           // far side
   ToolButton2.Enabled:=false;
   checkbox2.checked:=false;
   checkbox2.visible:=false;
   lrot:=180;
   inclination:=0;
   mark(0,0,'');
   if GeologicalMap and fileexists(Slash(appdir)+Slash('textures')+'moon_far_geology.jpg') then
           Load2DTexture('moon_far_geology.jpg')
      else Load2DTexture('moon_b.jpg');
   RefreshMoonImage;
  end;
 end else begin
   checkbox2.visible:=true;
   ToolButton6.enabled:=true;
   ToolButton2.Enabled:=true;
   {$IFDEF opengl}
   if useopengl then begin
     RotationCadencer.enabled:=false;
     Rotation1.visible:=false;
     GroupBox3.visible:=false;
     LibrationButton.Enabled:=true;
     if not safemode then GroupBox4.visible:=true;
     lrot:=0;
     inclination:=0;
     setzoom(minfoc);
     movecamera(0,0);
     case RadioGroup2.ItemIndex of
     0 : CameraOrientation:=0;
     1 : CameraOrientation:=180;
     end;
     SetCameraOrientation;
     SetMirror(checkbox2.checked);
     RefreshMoonImage;
   end else
   {$ENDIF}
   begin
     lrot:=0;
     inclination:=0;
     mark(0,0,'');
     if GeologicalMap and fileexists(Slash(appdir)+Slash('textures')+'moon_full_geology.jpg') then
           Load2DTexture('moon_full_geology.jpg')
      else Load2DTexture('moon_a.jpg');
     RefreshMoonImage;
   end;
 end;
end;

procedure TForm1.Button14Click(Sender: TObject);
begin
{$IFDEF opengl}
Glsceneviewer1.Buffer.ShowInfo;
{$ENDIF}
end;

procedure TForm1.ZoomEyepieceClick(Sender: TObject);
begin
with sender as TMenuItem do CurrentEyepiece:=tag;
if CurrentEyepiece=0 then begin
{$IFDEF opengl}
  if useOpengl then begin
    annulus1.Visible:=false;
    GLsceneViewer1.Buffer.BackgroundColor:=clBlack;
  end else
{$ENDIF}
  begin
    ClearEyepiece;
    Refresh2Dimage;
  end;
end else begin
  SetZoom(minfoc*(diam/60)/eyepiecefield[CurrentEyepiece]);
  case eyepiecerotation[CurrentEyepiece] of
  1 : begin
      RadioGroup2.ItemIndex:=0;
      RadioGroup2click(self);
      end;
  2 : begin
      RadioGroup2.ItemIndex:=1;
      RadioGroup2click(self);
      end;
  end;
  case eyepiecemirror[CurrentEyepiece] of
  1 : begin
      checkbox2.checked:=false;
      Checkbox2click(self);
      end;
  2 : begin
      checkbox2.checked:=true;
      Checkbox2click(self);
      end;
  end;    
{$IFDEF opengl}
  if useOpengl then begin
    GLsceneViewer1.Buffer.BackgroundColor:=$00202020;
    annulus1.Visible:=true;
  end else
{$ENDIF}
  begin
    DrawEyepiece;
    Refresh2Dimage;
  end;
end;
end;

procedure TForm1.ScrollBar2DChange(Sender: TObject);
begin
if lockscrollbar then begin
   lockscrollbar:=false;
   exit;
end;
Refresh2DImage;
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
doublebuf:=CheckBox3.checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
stencilbuf:=CheckBox4.checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
farsidetexture:=CheckBox5.checked;
end;

{$ifdef opengl}
procedure TForm1.CheckBox8Click(Sender: TObject);
begin
compresstexture:=CheckBox8.checked;
end;

Procedure TForm1.InitTelescope;
var fs : TSearchRec;
    i,p : integer;
    buf : string;
begin
i:=findfirst(slash(appdir)+'*.tid',0,fs);
Combobox5.clear;
while i=0 do begin
  buf:=extractfilename(fs.name);
  p:=pos('.tid',buf);
  buf:=trim(copy(buf,1,p-1));
  Combobox5.items.Add(buf);
  i:=findnext(fs);
end;
findclose(fs);
if Combobox5.items.count>0 then GroupBox4.visible:=true;
if scopeinterface<>'' then Combobox5.text:=scopeinterface
                      else Combobox5.text:=Combobox5.items[0];
scopeinterface:=ComboBox5.text;
end;

procedure TForm1.ComboBox5Change(Sender: TObject);
begin
scopeinterface:=ComboBox5.text;
end;

// Scope menu
procedure TForm1.Button16Click(Sender: TObject);
begin
if not scopelibok then InitScopeLibrary(slash(appdir)+scopeinterface+'.tid');
if scopelibok then begin
  ScopeSetObs(ObsLatitude,ObsLongitude);
  ScopeShow;
  geocentric:=false;
  librationeffect:=true;
end;
end;

// Goto button
procedure TForm1.Button17Click(Sender: TObject);
var ok1,ok2,ok3:boolean;
    ii : integer;
    buf:shortstring;
    r,d: double;
begin
if scopelibok and
   scopeconnected and
   marked
   then begin
   ScopeGetInfo(buf,OK1,OK2,OK3,ii);
   if not ok3 then exit;
   Moon2RaDec(markx,marky,r,d);
   ScopeGoto(r,d,ok1);
end
else showmessage('Please first connect the telescope and select a formation.');   // add to translation!
end;

// Sync button
procedure TForm1.Button18Click(Sender: TObject);
var ok1,ok2,ok3:boolean;
    ii : integer;
    buf:shortstring;
    r,d: double;
begin
if scopelibok and
   scopeconnected and
   marked
   then begin
   ScopeGetInfo(buf,OK1,OK2,OK3,ii);
   if not ok2 then exit;
   Moon2RaDec(markx,marky,r,d);
   ScopeAlign('markname',r,d);
end
else showmessage('Please first connect the telescope and select a formation.');
end;

// track
procedure TForm1.CheckBox6Click(Sender: TObject);
var ok1,ok2,ok3:boolean;
    ii : integer;
    buf:shortstring;
begin
ok1:=false;
if scopelibok and scopeconnected then ScopeGetInfo(buf,OK1,OK2,OK3,ii);
if not ok1 then CheckBox6.checked:=false
   else begin
     TelescopeTimer.Interval:=max(250,ii);
     TelescopeTimer.enabled:=CheckBox6.checked;
     ToolButton3.enabled:= not TelescopeTimer.enabled;
   end;
if CheckBox6.checked then begin
   CheckBox7.enabled:=true;
   Edit5.enabled:=true;
   Label26.enabled:=true;
   Label28.enabled:=true;
end else begin
   CheckBox7.checked:=false;
   CheckBox7.enabled:=false;
   CheckBox7.checked:=false;
   Edit5.enabled:=false;
   Label26.enabled:=false;
   Label28.enabled:=false;
end;
end;

procedure TForm1.TelescopeTimerTimer(Sender: TObject);
var r,d,x,y: double;
    ok: boolean;
    buf: string;
begin
ScopeGetRaDec(r,d,ok);
if ok then begin
 initdate;
 ok:=marked;
 x:=markx;
 y:=marky;
 buf:=markname;
 RefreshMoonImage;
 if ok then mark(x,y,buf);
 RaDec2Moon(r,d,x,y);
 if (abs(x)<0.5)and(abs(y)<0.5) then MoveCamera(x,y);
 if CheckBox7.checked and marked and ((LastScopeTracking+trackdelay.position/86400)<=now) then begin
    LastScopeTracking:=now;
    Button17Click(Sender);
 end;
end else begin
 TelescopeTimer.enabled:=false;
 CheckBox6.checked:=false;
end;
end;
{$endif}

procedure TForm1.NMClick(Sender: TObject);
begin
CurrentJD:=nmjd;
SetJDDate;
RefreshMoonImage;
end;

procedure TForm1.FQClick(Sender: TObject);
begin
CurrentJD:=fqjd;
SetJDDate;
RefreshMoonImage;
end;

procedure TForm1.FMClick(Sender: TObject);
begin
CurrentJD:=fmjd;
SetJDDate;
RefreshMoonImage;
end;

procedure TForm1.LQClick(Sender: TObject);
begin
CurrentJD:=lqjd;
SetJDDate;
RefreshMoonImage;
end;

procedure RefreshPhase;
var jd0,hh:double;
    aa,mm,dd : integer;
begin
  jd0:=jd(CurYear,1,1,0.0);
  MoonPhases((phaseoffset/12.3685)+CurYear+(CurrentJD-jd0)/365.25,nmjd,fqjd,fmjd,lqjd);
  djd(nmjd+(timezone-DT_UT)/24,aa,mm,dd,hh);
  form1.labelnm.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);
  djd(fqjd+(timezone-DT_UT)/24,aa,mm,dd,hh);
  form1.labelfq.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);
  djd(fmjd+(timezone-DT_UT)/24,aa,mm,dd,hh);
  form1.labelfm.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);
  djd(lqjd+(timezone-DT_UT)/24,aa,mm,dd,hh);
  form1.labellq.caption:=dattostr(aa,mm,dd)+' '+timmtostr(hh);
end;

procedure TForm1.prevMClick(Sender: TObject);
begin
system.dec(phaseoffset);
RefreshPhase;
end;

procedure TForm1.nextMClick(Sender: TObject);
begin
inc(phaseoffset);
RefreshPhase;
end;


procedure TForm1.NewWindowButtonClick(Sender: TObject);
var param:string;
    i : integer;
begin
// limit to two window to save resources
 if multi_instance then exit;
 i:=findwindow(nil,pchar(form1.caption+'<2>'));
 if i=0 then begin
    SaveDefault;
    param:= '-multi_instance';
    param:=param+' -d '+DatToStr(curyear,currentmonth,currentday)+'T'+Timtostr(CurrentTime);
    param:=param+' -r '+formatfloat(d4,lrot);
    param:=param+' -c '+formatfloat(d4,curx)+' '+formatfloat(d4,cury);
    param:=param+' -f '+formatfloat(d4,curfoc);
    if currentselection<>'' then param:=param+' -s "'+currentselection+'"';
    param:=param+' -- ';
    ExecuteFile(appname,param, appdir, SW_SHOWNOACTIVATE);
 end
 else SendMessage(i,RegisterWindowMessage(IdMsg),0,0);
end;

procedure TForm1.LibrationButtonClick(Sender: TObject);
begin
librationeffect:=LibrationButton.Down;
RefreshMoonImage;
end;

procedure TForm1.PhaseButtonClick(Sender: TObject);
begin
phaseeffect:=PhaseButton.Down;
RefreshMoonImage;
{$IFDEF opengl}
//if ToolButton3.down then
//   GLLightSource2.Shining:=not phaseeffect
//else GLLightSource2.Shining:=false;
{$endif}
end;

procedure TForm1.DataBase1Click(Sender: TObject);
var param: string;
    i:integer;
begin
 i:=findwindow(nil,pchar(VMAbrowser));
 if i=0 then CloseVMAbrowser:=true;
 param:=' -nx ';
 ExecuteFile('datlun.exe',param, appdir, SW_SHOWNOACTIVATE);
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
form2.colordialog1.color:=autolabelcolor;
if form2.colordialog1.execute then begin
  autolabelcolor:=form2.colordialog1.color;
  InitLabel;
  RefreshLabel;
end;
end;

procedure TForm1.PopupMenu1Popup(Sender: TObject);
begin
 RemoveMark1.visible:=( CurrentSelection<>'');
end;

procedure TForm1.RemoveMark1Click(Sender: TObject);
begin
CurrentSelection:='';
RefreshLabel;
end;

//////////////////////////////////////////////////////////////////
