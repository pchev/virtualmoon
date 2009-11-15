unit pu_features;

{$mode objfpc}{$H+}

interface

uses u_translation, u_constant, u_util, Classes, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, StdCtrls;

type

  { Tf_features }

  Tf_features = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    function CheckOptionalFeature(optlist:Tstrings): boolean;

  public
    { public declarations }
    procedure SetLang;
  end; 

var
  f_features: Tf_features;

implementation

procedure Tf_features.SetLang;
begin
caption:=rsCheckForOpti;
label1.caption:=rsYouCanDownlo;
button1.Caption:=rsDownload;
button2.Caption:=rst_2;
OptionalFeatureName[1]:=rsPictureLibra;
OptionalFeatureName[2]:=rsPictureLibra2;
OptionalFeatureName[3]:=rsPictureLibra3;
OptionalFeatureName[4]:=rsPictureLibra4;
OptionalFeatureName[5]:=rsPictureLibra5;
OptionalFeatureName[6]:=rsPictureLibra6;
OptionalFeatureName[7]:=rsPictureLibra7;
OptionalFeatureName[8]:=rsPictureLibra8;
OptionalFeatureName[9]:=rsPictureLibra9;
OptionalFeatureName[10]:=rsTexturesAirb;
OptionalFeatureName[11]:=rsTexturesClem;
OptionalFeatureName[12]:=rsTexturesLOPA;
OptionalFeatureName[13]:=rsTexturesPhas;
end;

procedure Tf_features.FormShow(Sender: TObject);
begin
  memo1.Clear;
  if CheckOptionalFeature(memo1.Lines) then
     memo1.Lines.Add(rsYourInstalla);
end;

procedure Tf_features.Button1Click(Sender: TObject);
begin
  ExecuteFile(rsDownloadURL);
end;

function Tf_features.CheckOptionalFeature(optlist:Tstrings): boolean;
var i: integer;
begin
result:=true;
optlist.Clear;
for i:=1 to nOptionalFeature do begin
  if not FileExists(slash(appdir)+StringReplace(OptionalFeatureCheck[i],'/',PathDelim,[rfReplaceAll])) then begin
     optlist.Add(OptionalFeatureName[i]);
     result:=false;
  end;
end;

end;

initialization
  {$I pu_features.lrs}

end.

