unit pu_features;

{$mode objfpc}{$H+}

interface

uses u_translation, u_constant, u_util, Classes, SysUtils, FileUtil, LResources, Forms,
  Buttons, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { Tf_features }

  Tf_features = class(TForm)
    Button1: TSpeedButton;
    Button2: TSpeedButton;
    Label1: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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

{$R pu_features.lfm}

procedure Tf_features.SetLang;
begin
caption:=rsCheckForOpti;
label1.caption:=rsYouCanDownlo;
button1.Caption:=rsDownload;
button2.Caption:=rst_2;
OptionalFeatureName[1]:=rsPictureLib + ' (virtualmoon-picture)';
OptionalFeatureName[2]:=rsPicturesAndM + ' (virtualmoon-data)';
OptionalFeatureName[3]:=rsHighResoluti + ' (virtualmoon-hires)';
OptionalFeatureName[4]:=format(rsVeryHighReso, ['Chang''e']) + ' (virtualmoon-veryhires-change)';
OptionalFeatureName[5]:=format(rsVeryHighReso, ['LOPAM']) + ' (virtualmoon-veryhires-lopam)';
OptionalFeatureName[6]:=format(rsVeryHighReso, ['LRO WAC']) + '( virtualmoon-veryhires-lrowac)';
OptionalFeatureName[7]:=format(rsVeryHighReso, ['LOLA-Kaguya Shade']) + ' (virtualmoon-veryhires-lolakaguya)';
OptionalFeatureName[8]:=format(rsVeryHighReso, ['LRO WAC '+rsLowSunElevat]) + ' (virtualmoon-veryhires-waclowsun)';
OptionalFeatureName[9]:=rsHighResoluti + ' Lunar Astronautical Chart' + ' (virtualmoon-hires-lac)';
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

procedure Tf_features.Button2Click(Sender: TObject);
begin
  ModalResult:=mrClose;
end;

procedure Tf_features.FormCreate(Sender: TObject);
begin
ScaleFormForFontSize(self,96);
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

end.

