{: GLSMWaveOut<p>

	windows mmsystem based sound-manager<p>

	<b>History : </b><font size=-1><ul>
      <li>17/03/07 - CRB - Created by extracting TGLSMWaveOut from GLSound
	</ul></font>
}
unit GLSMWaveOut;


interface

{$IFDEF WINDOWS}
  {$MESSAGE WARNING 'WINDOWS ONLY. By using this unit, your project cannot be compiled on other platforms'}
{$ELSE}
  {$ERROR WINDOWS ONLY UNIT}
{$ENDIF}

uses Classes, GLSound, GLSoundFileObjects, GLScene, XCollection, VectorGeometry, GLCadencer,
     GLMisc, mmsystem;
{$i GLScene.inc}

Type
	// TGLSMWaveOut
	//
   {: Basic sound manager based on WinMM <i>waveOut</i> function.<p>
      This manager has NO 3D miximing capacity, this is merely a default manager
      that should work on any system, and help showcasing/testing basic GLSS
      core functionality.<p>
      Apart from 3D, mute, pause, priority and volume are ignored too, and only
      sampling conversions supported by the windows ACM driver are supported
      (ie. no 4bits samples playback etc.). }
	TGLSMWaveOut = class (TGLSoundManager)
	   private
  function WaveFormat(ASoundSampling: TGLSoundSampling): TWaveFormatEx;
	      { Private Declarations }

	   protected
	      { Protected Declarations }
	      function DoActivate : Boolean; override;
	      procedure DoDeActivate; override;

         procedure KillSource(aSource : TGLBaseSoundSource); override;
         procedure UpdateSource(aSource : TGLBaseSoundSource); override;

      public
	      { Public Declarations }
	      constructor Create(AOwner : TComponent); override;
	      destructor Destroy; override;

      published
	      { Published Declarations }
         property MaxChannels default 4;
	end;

procedure Register;


implementation
uses SysUtils, GLCrossPlatform;

// Register
//
procedure Register;
begin
  RegisterComponents('GLScene', [TGLSMWaveOut]);
end;


// ------------------
// ------------------ TGLSMWaveOut ------------------
// ------------------

// Create
//
constructor TGLSMWaveOut.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
   MaxChannels:=4;
end;

// Destroy
//
destructor TGLSMWaveOut.Destroy;
begin
	inherited Destroy;
end;

function TGLSMWaveOut.WaveFormat(ASoundSampling : TGLSoundSampling) : TWaveFormatEx;
begin
  with ASoundSampling do begin
   Result.nSamplesPerSec:=Frequency;
   Result.nChannels:=NbChannels;
   Result.wFormatTag:=Wave_Format_PCM;
   Result.nAvgBytesPerSec:=BytesPerSec;
   Result.wBitsPerSample:=BitsPerSample;
   Result.nBlockAlign:=1024;
   Result.cbSize:=SizeOf(TWaveFormatEx);
  end;
end;


// DoActivate
//
function TGLSMWaveOut.DoActivate : Boolean;
begin
   Result:=True;
end;

// DoDeActivate
//
procedure TGLSMWaveOut.DoDeActivate;
var
   i : Integer;
begin
   for i:=Sources.Count-1 downto 0 do
      KillSource(Sources[i]);
end;

// KillSource
//
procedure TGLSMWaveOut.KillSource(aSource : TGLBaseSoundSource);
begin
   if aSource.ManagerTag<>0 then begin
      waveOutClose(aSource.ManagerTag);
      aSource.ManagerTag:=0;
   end;
end;

procedure _waveOutCallBack(hwo : HWAVEOUT; uMsg : Cardinal;
                           dwInstance, dwParam1, dwParam2 : Integer); stdcall;
begin
   if uMsg=WOM_DONE then begin
      waveOutClose(hwo);
      TGLSoundSource(dwInstance).ManagerTag:=-1;
   end;
end;

// UpdateSource
//
procedure TGLSMWaveOut.UpdateSource(aSource : TGLBaseSoundSource);
var
   i, n : Integer;
   wfx : TWaveFormatEx;
   smp : TGLSoundSample;
   wh : wavehdr;
   mmres : MMRESULT;
   hwo : hwaveout;
begin
   // count nb of playing sources and delete done ones
   n:=0;
   for i:=Sources.Count-1 downto 0 do
      if Sources[i].ManagerTag>0 then
         Inc(n)
      else if Sources[i].ManagerTag=-1 then
(* removed delphi 4 support {$ifdef GLS_DELPHI_5_UP}*)
			Sources.Delete(i);
(* removed delphi 4 support {$else}
			Sources[i].Free;
{$endif}*)
	// start sources if some capacity remains, and forget the others
   for i:=Sources.Count-1 downto 0 do if Sources[i].ManagerTag=0 then begin
      if n<MaxChannels then begin
         smp:=Sources[i].Sample;
         if Assigned(smp) and (smp.Data<>nil) then begin
            wfx:=WaveFormat(smp.Data.Sampling);
            mmres:=waveOutOpen(@hwo, WAVE_MAPPER, @wfx,
                               Cardinal(@_waveOutCallBack), Integer(Sources[i]),
                               CALLBACK_FUNCTION);
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            wh.dwBufferLength:=smp.LengthInBytes;
            wh.lpData:=smp.Data.PCMData;
            wh.dwLoops:=Sources[i].NbLoops;
            if wh.dwLoops>1 then
               wh.dwFlags:=WHDR_BEGINLOOP+WHDR_ENDLOOP
            else wh.dwFlags:=0;
            wh.lpNext:=nil;
            mmres:=waveOutPrepareHeader(hwo, @wh, SizeOf(wavehdr));
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            mmres:=waveOutWrite(hwo, @wh, SizeOf(wavehdr));
            Assert(mmres=MMSYSERR_NOERROR, IntToStr(mmres));
            Sources[i].ManagerTag:=hwo;
            Inc(n);
			end else
(* removed delphi 4 support {$ifdef GLS_DELPHI_5_UP}*)
				Sources.Delete(i);
(* removed delphi 4 support {$else}
				Sources[i].Free;
{$endif}*)
		end else
(* removed delphi 4 support {$ifdef GLS_DELPHI_5_UP}*)
			Sources.Delete(i);
(* removed delphi 4 support {$else}
			Sources[i].Free;
{$endif}*)
	end;
end;


initialization

end.

