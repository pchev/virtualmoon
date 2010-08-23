{ History:

     DaStr - 17/03/07 -  Dropped Kylix support in favor of FPC
                         (thanks Burkhard Carstens) (BugTracekrID=1681585)
     EG    - 18/10/03 - Renamed from FModDyn
                        Merged FModTypes and FModPresets content in
                        8087 stuff commented out
}
{============================================================================================ }
{ FMOD Main header file. Copyright (c), FireLight Technologies Pty, Ltd. 1999-2003.           }
{ =========================================================================================== }
{
  NOTE: For the demos to run you must have either fmod.dll (in Windows)
  or libfmod-3.7.so (in Linux) installed.

  In Windows, copy the fmod.dll file found in the api directory to either of
  the following locations (in order of preference)
  - your application directory
  - Windows\System (95/98) or WinNT\System32 (NT/2000/XP)

  In Linux, make sure you are signed in as root and copy the libfmod-3.7.so
  file from the api directory to your /usr/lib/ directory.
  Then via a command line, navigate to the /usr/lib/ directory and create
  a symbolic link between libfmod-3.7.so and libfmod.so. This is done with
  the following command (assuming you are in /usr/lib/)...
  ln -s libfmod-3.7.so libfmod.so.
}
{ =============================================================================================== }

unit FMod;

{$IFDEF FPC}
  {$MODE DELPHI}
  {$PACKRECORDS C}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE DELPHI_5_OR_LOWER}
{$ELSE}
  {$IFDEF VER120}
    {$DEFINE DELPHI_5_OR_LOWER}
  {$ELSE}
    {$IFDEF VER130}
      {$DEFINE DELPHI_5_OR_LOWER}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{
  Disable assertions by changing the following compiler directive to OFF.
  Assertions are used to check the functions are correctly loaded when using
  dynamic loading.
}
{$ASSERTIONS ON}

interface

{$IFDEF fpc}
uses
  dynlibs;
{$else}
{$IFDEF mswindows}
uses
  windows;
{$endif}
{$ENDIF}

{
  Disable warning for unsafe types in Delphi 7
}
{$IFDEF VER150}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}

{ =============================================================================================== }
{ DEFINITIONS                                                                                     }
{ =============================================================================================== }

{
  Force four-byte enums
}
{$Z4}

{$IFDEF DELPHI_5_OR_LOWER}
type
  PSingle = ^Single;
  THandle = Cardinal;
{$ENDIF}

const
  FMOD_VERSION: Single = 3.70;

{
  FMOD defined types
}

type
  PFSoundSample = Pointer;
  PFSoundStream = Pointer;
  PFSoundDSPUnit = Pointer;
  PFMusicModule = Pointer;
  PFSyncPoint = Pointer;

  PFSoundVector = ^TFSoundVector;
  TFSoundVector = record
    x: Single;
    y: Single;
    z: Single;
  end;

  {
    Callback types
  }

  TFSoundStreamCallback   = function (Stream: PFSoundStream; Buff: Pointer; Length, Param: Integer): ByteBool; stdcall;
  TFSoundDSPCallback      = function (OriginalBuffer: Pointer; NewBuffer: Pointer; Length, Param: Integer): Pointer; stdcall;
  TFMusicCallback         = procedure (Module: PFMusicModule; Param: Byte); stdcall;

  TFSoundOpenCallback     = function (Name: PChar): Cardinal; stdcall;
  TFSoundCloseCallback    = procedure (Handle: Cardinal); stdcall;
  TFSoundReadCallback     = function (Buffer: Pointer; Size: Cardinal; Handle: Cardinal): Cardinal; stdcall;
  TFSoundSeekCallback     = procedure (Handle: Cardinal; Pos: Cardinal; Mode: Byte); stdcall;
  TFSoundTellCallback     = function (Handle: Cardinal): Cardinal; stdcall;

  TFSoundAllocCallback    = function(Size: Cardinal): Pointer; stdcall;
  TFSoundReallocCallback  = function(Ptr: Pointer; Size: Cardinal): Pointer; stdcall;
  TFSoundFreeCallback     = procedure(Ptr: Pointer); stdcall;

  TFMetaDataCallback      = function(Name: PChar; Value: PChar; userdata: Integer): ByteBool; stdcall;

{
[ENUM]
[
  [DESCRIPTION]
  On failure of commands in FMOD, use FSOUND_GetError to attain what happened.

  [SEE_ALSO]
  FSOUND_GetError
]
}

type
  TFModErrors = (
    FMOD_ERR_NONE,             // No errors
    FMOD_ERR_BUSY,             // Cannot call this command after FSOUND_Init.  Call FSOUND_Close first.
    FMOD_ERR_UNINITIALIZED,    // This command failed because FSOUND_Init was not called
    FMOD_ERR_INIT,             // Error initializing output device.
    FMOD_ERR_ALLOCATED,        // Error initializing output device, but more specifically, the output device is already in use and cannot be reused.
    FMOD_ERR_PLAY,             // Playing the sound failed.
    FMOD_ERR_OUTPUT_FORMAT,    // Soundcard does not support the features needed for this soundsystem (16bit stereo output)
    FMOD_ERR_COOPERATIVELEVEL, // Error setting cooperative level for hardware.
    FMOD_ERR_CREATEBUFFER,     // Error creating hardware sound buffer.
    FMOD_ERR_FILE_NOTFOUND,    // File not found
    FMOD_ERR_FILE_FORMAT,      // Unknown file format
    FMOD_ERR_FILE_BAD,         // Error loading file
    FMOD_ERR_MEMORY,           // Not enough memory or resources
    FMOD_ERR_VERSION,          // The version number of this file format is not supported
    FMOD_ERR_INVALID_PARAM,    // An invalid parameter was passed to this function
    FMOD_ERR_NO_EAX,           // Tried to use an EAX command on a non EAX enabled channel or output.
    FMOD_ERR_CHANNEL_ALLOC,    // Failed to allocate a new channel
    FMOD_ERR_RECORD,           // Recording is not supported on this machine
    FMOD_ERR_MEDIAPLAYER,      // Windows Media Player not installed so cannot play wma or use internet streaming. */
    FMOD_ERR_CDDEVICE          // An error occured trying to open the specified CD device
  );

{
[ENUM]
[
    [DESCRIPTION]
    These output types are used with FSOUND_SetOutput, to choose which output driver to use.

	FSOUND_OUTPUT_DSOUND will not support hardware 3d acceleration if the sound card driver
	does not support DirectX 6 Voice Manager Extensions.

    FSOUND_OUTPUT_WINMM is recommended for NT and CE.

    [SEE_ALSO]
    FSOUND_SetOutput
    FSOUND_GetOutput
]
}

type
  TFSoundOutputTypes = (
    FSOUND_OUTPUT_NOSOUND,  // NoSound driver, all calls to this succeed but do nothing.
    FSOUND_OUTPUT_WINMM,    // Windows Multimedia driver.
    FSOUND_OUTPUT_DSOUND,   // DirectSound driver.  You need this to get EAX2 or EAX3 support, or FX api support.
    FSOUND_OUTPUT_A3D,      // A3D driver.

    FSOUND_OUTPUT_OSS,      // Linux/Unix OSS (Open Sound System) driver, i.e. the kernel sound drivers.
    FSOUND_OUTPUT_ESD,      // Linux/Unix ESD (Enlightment Sound Daemon) driver.
    FSOUND_OUTPUT_ALSA,     // Linux Alsa driver.

    FSOUND_OUTPUT_ASIO,     // Low latency ASIO driver
    FSOUND_OUTPUT_XBOX,     // Xbox driver
    FSOUND_OUTPUT_PS2,      // PlayStation 2 driver
    FSOUND_OUTPUT_MAC,      // Mac SoundMager driver
    FSOUND_OUTPUT_GC,       // Gamecube driver

    FSOUND_OUTPUT_NOSOUND_NONREALTIME   // This is the same as nosound, but the sound generation is driven by FSOUND_Update
  );


{
[ENUM]
[
  [DESCRIPTION]
  These mixer types are used with FSOUND_SetMixer, to choose which mixer to use, or to act
  upon for other reasons using FSOUND_GetMixer.
  It is not necessary to set the mixer.  FMOD will autodetect the best mixer for you.

  [SEE_ALSO]
  FSOUND_SetMixer
  FSOUND_GetMixer
]
}
type
  TFSoundMixerTypes = (
    FSOUND_MIXER_AUTODETECT,        // CE/PS2/GC Only - Non interpolating/low quality mixer.
    FSOUND_MIXER_BLENDMODE,         // Removed / obsolete
    FSOUND_MIXER_MMXP5,             // Removed / obsolete
    FSOUND_MIXER_MMXP6,             // Removed / obsolete

    FSOUND_MIXER_QUALITY_AUTODETECT,// All platforms - Autodetect the fastest quality mixer based on your cpu.
    FSOUND_MIXER_QUALITY_FPU,       // Win32/Linux only - Interpolating/volume ramping FPU mixer.
    FSOUND_MIXER_QUALITY_MMXP5,     // Win32/Linux only - Interpolating/volume ramping P5 MMX mixer.
    FSOUND_MIXER_QUALITY_MMXP6,     // Win32/Linux only - Interpolating/volume ramping ppro+ MMX mixer.

    FSOUND_MIXER_MONO,              // CE/PS2/GC only - MONO non interpolating/low quality mixer. For speed
    FSOUND_MIXER_QUALITY_MONO,      // CE/PS2/GC only - MONO Interpolating mixer.  For speed

    FSOUND_MIXER_MAX
  );


{
[ENUM]
[
  [DESCRIPTION]
  These definitions describe the type of song being played.

  [SEE_ALSO]
  FMUSIC_GetType
]
}
type
  TFMusicTypes = (
    FMUSIC_TYPE_NONE,
    FMUSIC_TYPE_MOD,  // Protracker / FastTracker
    FMUSIC_TYPE_S3M,  // ScreamTracker 3
    FMUSIC_TYPE_XM,   // FastTracker 2
    FMUSIC_TYPE_IT,   // Impulse Tracker
    FMUSIC_TYPE_MIDI, // MIDI file
    FMUSIC_TYPE_FSB   // FMOD Sample Bank file
  );


{
[DEFINE_START]
[
  [NAME]
  FSOUND_DSP_PRIORITIES

  [DESCRIPTION]
  These default priorities are used by FMOD internal system DSP units.  They describe the
  position of the DSP chain, and the order of how audio processing is executed.
  You can actually through the use of FSOUND_DSP_GetxxxUnit (where xxx is the name of the DSP
  unit), disable or even change the priority of a DSP unit.

  [SEE_ALSO]
  FSOUND_DSP_Create
  FSOUND_DSP_SetPriority
  FSOUND_DSP_GetSpectrum
]
}
const
  FSOUND_DSP_DEFAULTPRIORITY_CLEARUNIT        = 0;    // DSP CLEAR unit - done first
  FSOUND_DSP_DEFAULTPRIORITY_SFXUNIT          = 100;  // DSP SFX unit - done second
  FSOUND_DSP_DEFAULTPRIORITY_MUSICUNIT        = 200;  // DSP MUSIC unit - done third
  FSOUND_DSP_DEFAULTPRIORITY_USER             = 300;  // User priority, use this as reference for your own DSP units
  FSOUND_DSP_DEFAULTPRIORITY_FFTUNIT          = 900;  // This reads data for FSOUND_DSP_GetSpectrum, so it comes after user units
  FSOUND_DSP_DEFAULTPRIORITY_CLIPANDCOPYUNIT  = 1000; // DSP CLIP AND COPY unit - last
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_CAPS

  [DESCRIPTION]
  Driver description bitfields. Use FSOUND_Driver_GetCaps to determine if a driver enumerated
  has the settings you are after. The enumerated driver depends on the output mode, see
  FSOUND_OUTPUTTYPES

  [SEE_ALSO]
  FSOUND_GetDriverCaps
  FSOUND_OUTPUTTYPES
]
}
const
  FSOUND_CAPS_HARDWARE              = $1;  // This driver supports hardware accelerated 3d sound.
  FSOUND_CAPS_EAX2                  = $2;  // This driver supports EAX 2 reverb
  FSOUND_CAPS_EAX3                  = $10; // This driver supports EAX 3 reverb
// [DEFINE_END]


{
[DEFINE_START]
[
    [NAME]
    FSOUND_MODES

    [DESCRIPTION]
    Sample description bitfields, OR them together for loading and describing samples.
    NOTE.  If the file format being loaded already has a defined format, such as WAV or MP3, then
    trying to override the pre-defined format with a new set of format flags will not work.  For
    example, an 8 bit WAV file will not load as 16bit if you specify FSOUND_16BITS.  It will just
    ignore the flag and go ahead loading it as 8bits.  For these type of formats the only flags
    you can specify that will really alter the behaviour of how it is loaded, are the following.

    FSOUND_LOOP_OFF
    FSOUND_LOOP_NORMAL
    FSOUND_LOOP_BIDI
    FSOUND_HW3D
    FSOUND_2D
    FSOUND_STREAMABLE
    FSOUND_LOADMEMORY
    FSOUND_LOADRAW
    FSOUND_MPEGACCURATE

    See flag descriptions for what these do.
]
}
const
  FSOUND_LOOP_OFF      = $00000001;  // For non looping samples.
  FSOUND_LOOP_NORMAL   = $00000002;  // For forward looping samples.
  FSOUND_LOOP_BIDI     = $00000004;  // For bidirectional looping samples.  (no effect if in hardware).
  FSOUND_8BITS         = $00000008;  // For 8 bit samples.
  FSOUND_16BITS        = $00000010;  // For 16 bit samples.
  FSOUND_MONO          = $00000020;  // For mono samples.
  FSOUND_STEREO        = $00000040;  // For stereo samples.
  FSOUND_UNSIGNED      = $00000080;  // For user created source data containing unsigned samples.
  FSOUND_SIGNED        = $00000100;  // For user created source data containing signed data.
  FSOUND_DELTA         = $00000200;  // For user created source data stored as delta values.
  FSOUND_IT214         = $00000400;  // For user created source data stored using IT214 compression.
  FSOUND_IT215         = $00000800;  // For user created source data stored using IT215 compression.
  FSOUND_HW3D          = $00001000;  // Attempts to make samples use 3d hardware acceleration. (if the card supports it)
  FSOUND_2D            = $00002000;  // Ignores any 3d processing.  Overrides FSOUND_HW3D.  Located in software.
  FSOUND_STREAMABLE    = $00004000;  // For a streamimg sound where you feed the data to it. */
  FSOUND_LOADMEMORY    = $00008000;  // "name" will be interpreted as a pointer to data for streaming and samples.
  FSOUND_LOADRAW       = $00010000;  // Will ignore file format and treat as raw pcm.
  FSOUND_MPEGACCURATE  = $00020000;  // For FSOUND_Stream_OpenFile - for accurate FSOUND_Stream_GetLengthMs/FSOUND_Stream_SetTime.  WARNING, see FSOUND_Stream_OpenFile for inital opening time performance issues.
  FSOUND_FORCEMONO     = $00040000;  // For forcing stereo streams and samples to be mono - needed if using FSOUND_HW3D and stereo data - incurs a small speed hit for streams
  FSOUND_HW2D          = $00080000;  // 2D hardware sounds.  allows hardware specific effects
  FSOUND_ENABLEFX      = $00100000;  // Allows DX8 FX to be played back on a sound.  Requires DirectX 8 - Note these sounds cannot be played more than once, be 8 bit, be less than a certain size, or have a changing frequency
  FSOUND_MPEGHALFRATE  = $00200000;  // For FMODCE only - decodes mpeg streams using a lower quality decode, but faster execution
  FSOUND_XADPCM        = $00400000;  // For XBOX only - Contents are compressed as XADPCM  */
  FSOUND_VAG           = $00800000;  // For PS2 only - Contents are compressed as Sony VAG format */
  FSOUND_NONBLOCKING   = $01000000;  // For FSOUND_Stream_OpenFile - Causes stream to open in the background and not block the foreground app - stream plays only when ready.
  FSOUND_GCADPCM       = $02000000;  // For Gamecube only - Contents are compressed as Gamecube DSP-ADPCM format
  FSOUND_MULTICHANNEL  = $04000000;  // For PS2 only - Contents are interleaved into a multi-channel (more than stereo) format
  FSOUND_USECORE0      = $08000000;  // For PS2 only - Sample/Stream is forced to use hardware voices 00-23
  FSOUND_USECORE1      = $10000000;  // For PS2 only - Sample/Stream is forced to use hardware voices 24-47
  FSOUND_LOADMEMORYIOP = $20000000;  // For PS2 only - "name" will be interpreted as a pointer to data for streaming and samples.  The address provided will be an IOP address

const
  FSOUND_NORMAL = (FSOUND_16BITS or FSOUND_SIGNED or FSOUND_MONO);
// [DEFINE_END]


{
[DEFINE_START]
[
    [NAME]
    FSOUND_CDPLAYMODES

    [DESCRIPTION]
    Playback method for a CD Audio track, using FSOUND_CD_SetPlayMode

    [SEE_ALSO]
    FSOUND_CD_SetPlayMode  
    FSOUND_CD_Play
]
}
const
  FSOUND_CD_PLAYCONTINUOUS = 0;   // Starts from the current track and plays to end of CD.
  FSOUND_CD_PLAYONCE = 1;         // Plays the specified track then stops.
  FSOUND_CD_PLAYLOOPED = 2;       // Plays the specified track looped, forever until stopped manually.
  FSOUND_CD_PLAYRANDOM = 3;       // Plays tracks in random order
// [DEFINE_END]


{
[DEFINE_START]
[
  [NAME]
  FSOUND_CHANNELSAMPLEMODE

  [DESCRIPTION]
  Miscellaneous values for FMOD functions.

  [SEE_ALSO]
  FSOUND_PlaySound
  FSOUND_PlaySoundEx
  FSOUND_Sample_Alloc
  FSOUND_Sample_Load
  FSOUND_SetPan
]
}
const
  FSOUND_FREE           = -1;     // value to play on any free channel, or to allocate a sample in a free sample slot.
  FSOUND_UNMANAGED      = -2;     // value to allocate a sample that is NOT managed by FSOUND or placed in a sample slot.
  FSOUND_ALL            = -3;     // for a channel index , this flag will affect ALL channels available! Not supported by every function.
  FSOUND_STEREOPAN      = -1;     // value for FSOUND_SetPan so that stereo sounds are not played at half volume. See FSOUND_SetPan for more on this.
  FSOUND_SYSTEM_CHANNEL = -1000;  // special 'channel' ID for all channel based functions that want to alter the global FSOUND software mixing output
  FSOUND_SYSTEMSAMPLE   = -1000;  // special 'sample' ID for all sample based functions that want to alter the global FSOUND software mixing output sample
// [DEFINE_END]


{
[STRUCT_START]
[
    [NAME]
    FSOUND_REVERB_PROPERTIES

    [DESCRIPTION]
    Structure defining a reverb environment.

    [REMARKS]
    For more indepth descriptions of the reverb properties under win32, please see the EAX2/EAX3
    documentation at http://developer.creative.com/ under the 'downloads' section.
    If they do not have the EAX3 documentation, then most information can be attained from
    the EAX2 documentation, as EAX3 only adds some more parameters and functionality on top of
    EAX2.
    Note the default reverb properties are the same as the FSOUND_PRESET_GENERIC preset.
    Note that integer values that typically range from -10,000 to 1000 are represented in 
    decibels, and are of a logarithmic scale, not linear, wheras float values are typically linear.
    PORTABILITY: Each member has the platform it supports in braces ie (win32/xbox).  
    Some reverb parameters are only supported in win32 and some only on xbox. If all parameters are set then
    the reverb should product a similar effect on either platform.
    Only WIN32 supports the reverb api.

    The numerical values listed below are the maximum, minimum and default values for each variable respectively.

    [SEE_ALSO]
    FSOUND_Reverb_SetProperties
    FSOUND_Reverb_GetProperties
    FSOUND_REVERB_PRESETS
    FSOUND_REVERB_FLAGS
]
}
type
  TFSoundReverbProperties = record          // MIN     MAX    DEFAULT DESCRIPTION
    Environment: Cardinal;                  // 0       25     0       sets all listener properties (win32 only)
    EnvSize: Single;                        // 1.0     100.0  7.5     environment size in meters (win32 only)
    EnvDiffusion: Single;                   // 0.0     1.0    1.0     environment diffusion (win32/xbox)
    Room: Integer;                          // -10000  0      -1000   room effect level (at mid frequencies) (win32/xbox)
    RoomHF: Integer;                        // -10000  0      -100    relative room effect level at high frequencies (win32/xbox)
    RoomLF: Integer;                        // -10000  0      0       relative room effect level at low frequencies (win32 only)
    DecayTime: Single;                      // 0.1     20.0   1.49    reverberation decay time at mid frequencies (win32/xbox)
    DecayHFRatio: Single;                   // 0.1     2.0    0.83    high-frequency to mid-frequency decay time ratio (win32/xbox)
    DecayLFRatio: Single;                   // 0.1     2.0    1.0     low-frequency to mid-frequency decay time ratio (win32 only)
    Reflections: Integer;                   // -10000  1000   -2602   early reflections level relative to room effect (win32/xbox)
    ReflectionsDelay: Single;               // 0.0     0.3    0.007   initial reflection delay time (win32/xbox)
    ReflectionsPan: array [0..2] of Single; //                0,0,0   early reflections panning vector (win32 only)
    Reverb: Integer;                        // -10000  2000   200     late reverberation level relative to room effect (win32/xbox)
    ReverbDelay: Single;                    // 0.0     0.1    0.011   late reverberation delay time relative to initial reflection (win32/xbox)
    ReverbPan: array [0..2] of Single;      //                0,0,0   late reverberation panning vector (win32 only)
    EchoTime: Single;                       // .075    0.25   0.25    echo time (win32 only)
    EchoDepth: Single;                      // 0.0     1.0    0.0     echo depth (win32 only)
    ModulationTime: Single;                 // 0.04    4.0    0.25    modulation time (win32 only)
    ModulationDepth: Single;                // 0.0     1.0    0.0     modulation depth (win32 only)
    AirAbsorptionHF: Single;                // -100    0.0    -5.0    change in level per meter at high frequencies (win32 only)
    HFReference: Single;                    // 1000.0  20000  5000.0  reference high frequency (hz) (win32/xbox)
    LFReference: Single;                    // 20.0    1000.0 250.0   reference low frequency (hz) (win32 only)
    RoomRolloffFactor: Single;              // 0.0     10.0   0.0     like FSOUND_3D_SetRolloffFactor but for room effect (win32/xbox)
    Diffusion: Single;                      // 0.0     100.0  100.0   Value that controls the echo density in the late reverberation decay. (xbox only)
    Density: Single;                        // 0.0     100.0  100.0   Value that controls the modal density in the late reverberation decay (xbox only)
    Flags: Cardinal;                        // FSOUND_REVERB_PROPERTYFLAGS - modifies the behavior of above properties (win32 only)
  end;
// [STRUCT_END]


{
[DEFINE_START]
[
    [NAME]
    FSOUND_REVERB_FLAGS

    [DESCRIPTION]
    Values for the Flags member of the FSOUND_REVERB_PROPERTIES structure.

    [SEE_ALSO]
    FSOUND_REVERB_PROPERTIES
]
}
const
  FSOUND_REVERBFLAGS_DECAYTIMESCALE         = $00000001;  // EnvironmentSize affects reverberation decay time
  FSOUND_REVERBFLAGS_REFLECTIONSSCALE       = $00000002;  // EnvironmentSize affects reflection level
  FSOUND_REVERBFLAGS_REFLECTIONSDELAYSCALE  = $00000004;  // EnvironmentSize affects initial reflection delay time
  FSOUND_REVERBFLAGS_REVERBSCALE            = $00000008;  // EnvironmentSize affects reflections level
  FSOUND_REVERBFLAGS_REVERBDELAYSCALE       = $00000010;  // EnvironmentSize affects late reverberation delay time
  FSOUND_REVERBFLAGS_DECAYHFLIMIT           = $00000020;  // AirAbsorptionHF affects DecayHFRatio
  FSOUND_REVERBFLAGS_ECHOTIMESCALE          = $00000040;  // EnvironmentSize affects echo time
  FSOUND_REVERBFLAGS_MODULATIONTIMESCALE    = $00000080;  // EnvironmentSize affects modulation time
  FSOUND_REVERB_FLAGS_CORE0                 = $00000100;  // PS2 Only - Reverb is applied to CORE0 (hw voices 0-23)
  FSOUND_REVERB_FLAGS_CORE1                 = $00000200;  // PS2 Only - Reverb is applied to CORE1 (hw voices 24-47)
  FSOUND_REVERBFLAGS_DEFAULT                = FSOUND_REVERBFLAGS_DECAYTIMESCALE or FSOUND_REVERBFLAGS_REFLECTIONSSCALE or
                                              FSOUND_REVERBFLAGS_REFLECTIONSDELAYSCALE or FSOUND_REVERBFLAGS_REVERBSCALE or
                                              FSOUND_REVERBFLAGS_REVERBDELAYSCALE or FSOUND_REVERBFLAGS_DECAYHFLIMIT or
                                              FSOUND_REVERB_FLAGS_CORE0 or FSOUND_REVERB_FLAGS_CORE1;
// [DEFINE_END]


{
[DEFINE_START]
[
    [NAME]
    FSOUND_REVERB_PRESETS

    [DESCRIPTION]
    A set of predefined environment PARAMETERS, created by Creative Labs
    These are used to initialize an FSOUND_REVERB_PROPERTIES structure statically.
    ie 
    FSOUND_REVERB_PROPERTIES prop = FSOUND_PRESET_GENERIC;

    [SEE_ALSO]
    FSOUND_Reverb_SetProperties
]
}
{
const
//                                 Env  Size    Diffus  Room   RoomHF  RmLF DecTm   DecHF  DecLF   Refl  RefDel  RefPan           Revb  RevDel  ReverbPan       EchoTm  EchDp  ModTm  ModDp  AirAbs  HFRef    LFRef  RRlOff Diffus  Densty  FLAGS 
  FSOUND_PRESET_OFF              = 0,	7.5f,	1.00f, -10000, -10000, 0,   1.00f,  1.00f, 1.0f,  -2602, 0.007f, 0.0f,0.0f,0.0f,   200, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,   0.0f,   0.0f, 0x3f ;
  FSOUND_PRESET_GENERIC          = 0,	7.5f,	1.00f, -1000,  -100,   0,   1.49f,  0.83f, 1.0f,  -2602, 0.007f, 0.0f,0.0f,0.0f,   200, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PADDEDCELL       = 1,	1.4f,	1.00f, -1000,  -6000,  0,   0.17f,  0.10f, 1.0f,  -1204, 0.001f, 0.0f,0.0f,0.0f,   207, 0.002f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_ROOM             = 2,	1.9f,	1.00f, -1000,  -454,   0,   0.40f,  0.83f, 1.0f,  -1646, 0.002f, 0.0f,0.0f,0.0f,    53, 0.003f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_BATHROOM         = 3,	1.4f,	1.00f, -1000,  -1200,  0,   1.49f,  0.54f, 1.0f,   -370, 0.007f, 0.0f,0.0f,0.0f,  1030, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f,  60.0f, 0x3f ;
  FSOUND_PRESET_LIVINGROOM       = 4,	2.5f,	1.00f, -1000,  -6000,  0,   0.50f,  0.10f, 1.0f,  -1376, 0.003f, 0.0f,0.0f,0.0f, -1104, 0.004f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_STONEROOM        = 5,	11.6f,	1.00f, -1000,  -300,   0,   2.31f,  0.64f, 1.0f,   -711, 0.012f, 0.0f,0.0f,0.0f,    83, 0.017f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_AUDITORIUM       = 6,	21.6f,	1.00f, -1000,  -476,   0,   4.32f,  0.59f, 1.0f,   -789, 0.020f, 0.0f,0.0f,0.0f,  -289, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CONCERTHALL      = 7,	19.6f,	1.00f, -1000,  -500,   0,   3.92f,  0.70f, 1.0f,  -1230, 0.020f, 0.0f,0.0f,0.0f,    -2, 0.029f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CAVE             = 8,	14.6f,	1.00f, -1000,  0,      0,   2.91f,  1.30f, 1.0f,   -602, 0.015f, 0.0f,0.0f,0.0f,  -302, 0.022f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_ARENA            = 9,	36.2f,	1.00f, -1000,  -698,   0,   7.24f,  0.33f, 1.0f,  -1166, 0.020f, 0.0f,0.0f,0.0f,    16, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_HANGAR           = 10,	50.3f,	1.00f, -1000,  -1000,  0,   10.05f, 0.23f, 1.0f,   -602, 0.020f, 0.0f,0.0f,0.0f,   198, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CARPETTEDHALLWAY = 11,	1.9f,	1.00f, -1000,  -4000,  0,   0.30f,  0.10f, 1.0f,  -1831, 0.002f, 0.0f,0.0f,0.0f, -1630, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_HALLWAY          = 12,	1.8f,	1.00f, -1000,  -300,   0,   1.49f,  0.59f, 1.0f,  -1219, 0.007f, 0.0f,0.0f,0.0f,   441, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_STONECORRIDOR    = 13,	13.5f,	1.00f, -1000,  -237,   0,   2.70f,  0.79f, 1.0f,  -1214, 0.013f, 0.0f,0.0f,0.0f,   395, 0.020f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_ALLEY 	         = 14,	7.5f,	0.30f, -1000,  -270,   0,   1.49f,  0.86f, 1.0f,  -1204, 0.007f, 0.0f,0.0f,0.0f,    -4, 0.011f, 0.0f,0.0f,0.0f, 0.125f, 0.95f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_FOREST 	         = 15,	38.0f,	0.30f, -1000,  -3300,  0,   1.49f,  0.54f, 1.0f,  -2560, 0.162f, 0.0f,0.0f,0.0f,  -229, 0.088f, 0.0f,0.0f,0.0f, 0.125f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  79.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_CITY             = 16,	7.5f,	0.50f, -1000,  -800,   0,   1.49f,  0.67f, 1.0f,  -2273, 0.007f, 0.0f,0.0f,0.0f, -1691, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  50.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_MOUNTAINS        = 17,	100.0f, 0.27f, -1000,  -2500,  0,   1.49f,  0.21f, 1.0f,  -2780, 0.300f, 0.0f,0.0f,0.0f, -1434, 0.100f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  27.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_QUARRY           = 18,	17.5f,	1.00f, -1000,  -1000,  0,   1.49f,  0.83f, 1.0f, -10000, 0.061f, 0.0f,0.0f,0.0f,   500, 0.025f, 0.0f,0.0f,0.0f, 0.125f, 0.70f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PLAIN            = 19,	42.5f,	0.21f, -1000,  -2000,  0,   1.49f,  0.50f, 1.0f,  -2466, 0.179f, 0.0f,0.0f,0.0f, -1926, 0.100f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  21.0f, 100.0f, 0x3f ;
  FSOUND_PRESET_PARKINGLOT       = 20,	8.3f,	1.00f, -1000,  0,      0,   1.65f,  1.50f, 1.0f,  -1363, 0.008f, 0.0f,0.0f,0.0f, -1153, 0.012f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_SEWERPIPE        = 21,	1.7f,	0.80f, -1000,  -1000,  0,   2.81f,  0.14f, 1.0f,    429, 0.014f, 0.0f,0.0f,0.0f,  1023, 0.021f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 0.000f, -5.0f, 5000.0f, 250.0f, 0.0f,  80.0f,  60.0f, 0x3f ;
  FSOUND_PRESET_UNDERWATER       = 22,	1.8f,	1.00f, -1000,  -4000,  0,   1.49f,  0.10f, 1.0f,   -449, 0.007f, 0.0f,0.0f,0.0f,  1700, 0.011f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 1.18f, 0.348f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x3f ;

// Non I3DL2 presets 

  FSOUND_PRESET_DRUGGED          = 23,	1.9f,	0.50f, -1000,  0,      0,   8.39f,  1.39f, 1.0f,  -115,  0.002f, 0.0f,0.0f,0.0f,   985, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 0.25f, 1.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_DIZZY            = 24,	1.8f,	0.60f, -1000,  -400,   0,   17.23f, 0.56f, 1.0f,  -1713, 0.020f, 0.0f,0.0f,0.0f,  -613, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 1.00f, 0.81f, 0.310f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
  FSOUND_PRESET_PSYCHOTIC        = 25,	1.0f,	0.50f, -1000,  -151,   0,   7.56f,  0.91f, 1.0f,  -626,  0.020f, 0.0f,0.0f,0.0f,   774, 0.030f, 0.0f,0.0f,0.0f, 0.250f, 0.00f, 4.00f, 1.000f, -5.0f, 5000.0f, 250.0f, 0.0f, 100.0f, 100.0f, 0x1f ;
}
// [DEFINE_END]


{
[STRUCTURE] 
[
    [NAME]
    FSOUND_REVERB_CHANNELPROPERTIES

    [DESCRIPTION]
    Structure defining the properties for a reverb source, related to a FSOUND channel.

    [REMARKS]
    For more indepth descriptions of the reverb properties under win32, please see the EAX3
    documentation at http://developer.creative.com/ under the 'downloads' section.
    If they do not have the EAX3 documentation, then most information can be attained from
    the EAX2 documentation, as EAX3 only adds some more parameters and functionality on top of
    EAX2.

    Note the default reverb properties are the same as the FSOUND_PRESET_GENERIC preset.
    Note that integer values that typically range from -10,000 to 1000 are represented in 
    decibels, and are of a logarithmic scale, not linear, wheras float values are typically linear.
    PORTABILITY: Each member has the platform it supports in braces ie (win32/xbox).  
    Some reverb parameters are only supported in win32 and some only on xbox. If all parameters are set then
    the reverb should product a similar effect on either platform.
    Linux and FMODCE do not support the reverb api.

    The numerical values listed below are the maximum, minimum and default values for each variable respectively.

    [SEE_ALSO]
    FSOUND_Reverb_SetChannelProperties
    FSOUND_Reverb_GetChannelProperties
    FSOUND_REVERB_CHANNELFLAGS
]
}
type
  TFSoundReverbChannelProperties = record   // MIN     MAX    DEFAULT
    Direct: Integer;                        // -10000  1000   0       direct path level (at low and mid frequencies) (win32/xbox)
    DirectHF: Integer;                      // -10000  0      0       relative direct path level at high frequencies (win32/xbox)
    Room: Integer;                          // -10000  1000   0       room effect level (at low and mid frequencies) (win32/xbox)
    RoomHF: Integer;                        // -10000  0      0       relative room effect level at high frequencies (win32/xbox)
    Obstruction: Integer;                   // -10000  0      0       main obstruction control (attenuation at high frequencies)  (win32/xbox)
    ObstructionLFRatio: Single;             // 0.0     1.0    0.0     obstruction low-frequency level re. main control (win32/xbox)
    Occlusion: Integer;                     // -10000  0      0       main occlusion control (attenuation at high frequencies) (win32/xbox)
    OcclusionLFRatio: Single;               // 0.0     1.0    0.25    occlusion low-frequency level re. main control (win32/xbox)
    OcclusionRoomRatio: Single;             // 0.0     10.0   1.5     relative occlusion control for room effect (win32)
    OcclusionDirectRatio: Single;           // 0.0     10.0   1.0     relative occlusion control for direct path (win32)
    Exclusion: Integer;                     // -10000  0      0       main exlusion control (attenuation at high frequencies) (win32)
    ExclusionLFRatio: Single;               // 0.0     1.0    1.0     exclusion low-frequency level re. main control (win32)
    OutsideVolumeHF: Integer;               // -10000  0      0       outside sound cone level at high frequencies (win32)
    DopplerFactor: Single;                  // 0.0     10.0   0.0     like DS3D flDopplerFactor but per source (win32)
    RolloffFactor: Single;                  // 0.0     10.0   0.0     like DS3D flRolloffFactor but per source (win32)
    RoomRolloffFactor: Single;              // 0.0     10.0   0.0     like DS3D flRolloffFactor but for room effect (win32/xbox)
    AirAbsorptionFactor: Single;            // 0.0     10.0   1.0     multiplies AirAbsorptionHF member of FSOUND_REVERB_PROPERTIES (win32)
    Flags: Integer;                         // FSOUND_REVERB_CHANNELFLAGS - modifies the behavior of properties (win32)
  end;
// [STRUCT_END]

{
[DEFINE_START] 
[
    [NAME] 
    FSOUND_REVERB_CHANNELFLAGS
    
    [DESCRIPTION]
    Values for the Flags member of the FSOUND_REVERB_CHANNELPROPERTIES structure.

    [SEE_ALSO]
    FSOUND_REVERB_CHANNELPROPERTIES
]
}
const
  FSOUND_REVERB_CHANNELFLAGS_DIRECTHFAUTO  = $01;  // Automatic setting of 'Direct'  due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_ROOMAUTO      = $02;  // Automatic setting of 'Room'  due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_ROOMHFAUTO    = $04;  // Automatic setting of 'RoomHF' due to distance from listener
  FSOUND_REVERB_CHANNELFLAGS_DEFAULT       = FSOUND_REVERB_CHANNELFLAGS_DIRECTHFAUTO or 
                                             FSOUND_REVERB_CHANNELFLAGS_ROOMAUTO or
                                             FSOUND_REVERB_CHANNELFLAGS_ROOMHFAUTO;
// [DEFINE_END]


{
[ENUM] 
[
	[DESCRIPTION]
    These values are used with FSOUND_FX_Enable to enable DirectX 8 FX for a channel.

	[SEE_ALSO]
    FSOUND_FX_Enable
    FSOUND_FX_Disable
    FSOUND_FX_SetChorus
    FSOUND_FX_SetCompressor
    FSOUND_FX_SetDistortion
    FSOUND_FX_SetEcho
    FSOUND_FX_SetFlanger
    FSOUND_FX_SetGargle
    FSOUND_FX_SetI3DL2Reverb
    FSOUND_FX_SetParamEQ
    FSOUND_FX_SetWavesReverb
]
}

type
  TFSoundFXModes = (
    FSOUND_FX_CHORUS,
    FSOUND_FX_COMPRESSOR,
    FSOUND_FX_DISTORTION,
    FSOUND_FX_ECHO,
    FSOUND_FX_FLANGER,
    FSOUND_FX_GARGLE,
    FSOUND_FX_I3DL2REVERB,
    FSOUND_FX_PARAMEQ,
    FSOUND_FX_WAVES_REVERB,
    FSOUND_FX_MAX
  );
// [DEFINE_END]


{
[ENUM]
[
	[DESCRIPTION]
    These are speaker types defined for use with the FSOUND_SetSpeakerMode command.
    Note - Only reliably works with FSOUND_OUTPUT_DSOUND or FSOUND_OUTPUT_XBOX output modes.  Other output modes will only 
    interpret FSOUND_SPEAKERMODE_MONO and set everything else to be stereo.

	[SEE_ALSO]
    FSOUND_SetSpeakerMode

    [REMARKS]
    Note - Only reliably works with FSOUND_OUTPUT_DSOUND or FSOUND_OUTPUT_XBOX output modes.  Other output modes will only 
    interpret FSOUND_SPEAKERMODE_MONO and set everything else to be stereo.

    Using either DolbyDigital or DTS will use whatever 5.1 digital mode is available if destination hardware is unsure.
]
}
type
  TFSoundSpeakerModes =
  (
    FSOUND_SPEAKERMODE_DOLBYDIGITAL,  // The audio is played through a speaker arrangement of surround speakers with a subwoofer.
    FSOUND_SPEAKERMODE_HEADPHONES,    // The speakers are headphones.
    FSOUND_SPEAKERMODE_MONO,          // The speakers are monaural.
    FSOUND_SPEAKERMODE_QUAD,          // The speakers are quadraphonic.
    FSOUND_SPEAKERMODE_STEREO,        // The speakers are stereo (default value).
    FSOUND_SPEAKERMODE_SURROUND,      // The speakers are surround sound.
    FSOUND_SPEAKERMODE_DTS            // The audio is played through a speaker arrangement of surround speakers with a subwoofer.
  );
  FSOUND_SPEAKERMODES = TFSoundSpeakerModes;


{
[DEFINE_START]
[
    [NAME] 
    FSOUND_INIT_FLAGS
    
    [DESCRIPTION]
    Initialization flags.  Use them with FSOUND_Init in the flags parameter to change various behaviour.

    FSOUND_INIT_ENABLEOUTPUTFX Is an init mode which enables the FSOUND mixer buffer to be affected by DirectX 8 effects.
    Note that due to limitations of DirectSound, FSOUND_Init may fail if this is enabled because the buffersize is too small.
    This can be fixed with FSOUND_SetBufferSize.  Increase the BufferSize until it works.
    When it is enabled you can use the FSOUND_FX api, and use FSOUND_SYSTEMCHANNEL as the channel id when setting parameters.

    [SEE_ALSO]
    FSOUND_Init
]
}
const
  FSOUND_INIT_USEDEFAULTMIDISYNTH     = $01;  // Causes MIDI playback to force software decoding.
  FSOUND_INIT_GLOBALFOCUS             = $02;  // For DirectSound output - sound is not muted when window is out of focus.
  FSOUND_INIT_ENABLEOUTPUTFX          = $04;  // For DirectSound output - Allows FSOUND_FX api to be used on global software mixer output!
  FSOUND_INIT_ACCURATEVULEVELS        = $08;  // This latency adjusts FSOUND_GetCurrentLevels, but incurs a small cpu and memory hit
  FSOUND_INIT_PS2_DISABLECORE0REVERB  = $10;  // PS2 only - Disable reverb on CORE 0 to regain SRAM
  FSOUND_INIT_PS2_DISABLECORE1REVERB  = $20;  // PS2 only - Disable reverb on CORE 1 to regain SRAM
  FSOUND_INIT_PS2_SWAPDMACORES        = $40;  // PS2 only - By default FMOD uses DMA CH0 for mixing, CH1 for uploads, this flag swaps them around
  FSOUND_INIT_DONTLATENCYADJUST       = $80;  // Callbacks are not latency adjusted, and are called at mix time.  Also information functions are immediate
  FSOUND_INIT_GC_INITLIBS             = $100; // Gamecube only - Initializes GC audio libraries
  FSOUND_INIT_STREAM_FROM_MAIN_THREAD = $200; // Turns off fmod streamer thread, and makes streaming update from FSOUND_Update called by the user

// [DEFINE_END]

(*
[ENUM]
[
    [DESCRIPTION]
    Status values for internet streams. Use FSOUND_Stream_Net_GetStatus to get the current status of an internet stream.

    [SEE_ALSO]
    FSOUND_Stream_Net_GetStatus
]
*)
type
  TFSoundStreamNetStatus =
  (
    FSOUND_STREAM_NET_NOTCONNECTED,         (* Stream hasn't connected yet *)
    FSOUND_STREAM_NET_CONNECTING,           (* Stream is connecting to remote host *)
    FSOUND_STREAM_NET_BUFFERING,            (* Stream is buffering data *)
    FSOUND_STREAM_NET_READY,                (* Stream is ready to play *)
    FSOUND_STREAM_NET_ERROR                 (* Stream has suffered a fatal error *)
  );


(*
[ENUM]
[
    [DESCRIPTION]
    Describes the type of a particular tag field.

    [SEE_ALSO]
    FSOUND_Stream_GetNumTagFields
    FSOUND_Stream_GetTagField
    FSOUND_Stream_FindTagField
]
*)
type
  TFSoundTagFieldType =
  (
    FSOUND_TAGFIELD_VORBISCOMMENT,          (* A vorbis comment *)
    FSOUND_TAGFIELD_ID3V1,                  (* Part of an ID3v1 tag *)
    FSOUND_TAGFIELD_ID3V2,                  (* An ID3v2 frame *)
    FSOUND_TAGFIELD_SHOUTCAST,              (* A SHOUTcast header line *)
    FSOUND_TAGFIELD_ICECAST,                (* An Icecast header line *)
    FSOUND_TAGFIELD_ASF                     (* An Advanced Streaming Format header line *)
  );


(*
[DEFINE_START]
[
    [NAME]
    FSOUND_STATUS_FLAGS

    [DESCRIPTION]
    These values describe the protocol and format of an internet stream. Use FSOUND_Stream_Net_GetStatus to retrieve this information for an open internet stream.

    [SEE_ALSO]
    FSOUND_Stream_Net_GetStatus
]
*)
const
  FSOUND_PROTOCOL_SHOUTCAST = $00000001;
  FSOUND_PROTOCOL_ICECAST   = $00000002;
  FSOUND_PROTOCOL_HTTP      = $00000004;
  FSOUND_FORMAT_MPEG        = $00010000;
  FSOUND_FORMAT_OGGVORBIS   = $00020000;
(* [DEFINE_END] *)

(*
[DEFINE_START]
[
    [NAME]
    FSOUND_REVERB_PRESETS

    [DESCRIPTION]
    A set of predefined environment PARAMETERS, created by Creative Labs
    These are used to initialize an FSOUND_REVERB_PROPERTIES structure statically.
    ie
    FSOUND_REVERB_PROPERTIES prop = FSOUND_PRESET_GENERIC;

    [SEE_ALSO]
    FSOUND_Reverb_SetProperties
]
*)

{$IFDEF COMPILER6_UP}{$J+}{$ENDIF}
const
  FSOUND_PRESET_OFF:              TFSoundReverbProperties = (Environment: 0;  EnvSize: 7.5;   EnvDiffusion: 1.00;   Room: -10000; RoomHF: -10000; RoomLF: 0;  DecayTime: 1.00;  DecayHFRatio: 1.00; DecayLFRatio: 1.0;  Reflections: -2602; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 200;  ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 0.0;   Density: 0.0;   Flags: $33f);
  FSOUND_PRESET_GENERIC:          TFSoundReverbProperties = (Environment: 0;  EnvSize: 7.5;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -100;   RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.83; DecayLFRatio: 1.0;  Reflections: -2602; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 200;  ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_PADDEDCELL:       TFSoundReverbProperties = (Environment: 1;  EnvSize: 1.4;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -6000;  RoomLF: 0;  DecayTime: 0.17;  DecayHFRatio: 0.10; DecayLFRatio: 1.0;  Reflections: -1204; ReflectionsDelay: 0.001;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 207;  ReverbDelay: 0.002; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_ROOM:             TFSoundReverbProperties = (Environment: 2;  EnvSize: 1.9;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -454;   RoomLF: 0;  DecayTime: 0.40;  DecayHFRatio: 0.83; DecayLFRatio: 1.0;  Reflections: -1646; ReflectionsDelay: 0.002;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 53;   ReverbDelay: 0.003; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_BATHROOM:         TFSoundReverbProperties = (Environment: 3;  EnvSize: 1.4;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -1200;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.54; DecayLFRatio: 1.0;  Reflections: -370;  ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 1030; ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 60.0;  Flags: $3f);
  FSOUND_PRESET_LIVINGROOM:       TFSoundReverbProperties = (Environment: 4;  EnvSize: 2.5;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -6000;  RoomLF: 0;  DecayTime: 0.50;  DecayHFRatio: 0.10; DecayLFRatio: 1.0;  Reflections: -1376; ReflectionsDelay: 0.003;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1104; ReverbDelay: 0.004; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_STONEROOM:        TFSoundReverbProperties = (Environment: 5;  EnvSize: 11.6;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -300;   RoomLF: 0;  DecayTime: 2.31;  DecayHFRatio: 0.64; DecayLFRatio: 1.0;  Reflections: -711;  ReflectionsDelay: 0.012;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 83;   ReverbDelay: 0.017; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_AUDITORIUM:       TFSoundReverbProperties = (Environment: 6;  EnvSize: 21.6;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -476;   RoomLF: 0;  DecayTime: 4.32;  DecayHFRatio: 0.59; DecayLFRatio: 1.0;  Reflections: -789;  ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-289;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_CONCERTHALL:      TFSoundReverbProperties = (Environment: 7;  EnvSize: 19.6;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -500;   RoomLF: 0;  DecayTime: 3.92;  DecayHFRatio: 0.70; DecayLFRatio: 1.0;  Reflections: -1230; ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-2;    ReverbDelay: 0.029; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_CAVE:             TFSoundReverbProperties = (Environment: 8;  EnvSize: 14.6;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: 0;      RoomLF: 0;  DecayTime: 2.91;  DecayHFRatio: 1.30; DecayLFRatio: 1.0;  Reflections: -602;  ReflectionsDelay: 0.015;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-302;  ReverbDelay: 0.022; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);
  FSOUND_PRESET_ARENA:            TFSoundReverbProperties = (Environment: 9;  EnvSize: 36.2;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -698;   RoomLF: 0;  DecayTime: 7.24;  DecayHFRatio: 0.33; DecayLFRatio: 1.0;  Reflections: -1166; ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 16;   ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_HANGAR:           TFSoundReverbProperties = (Environment: 10; EnvSize: 50.3;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -1000;  RoomLF: 0;  DecayTime: 10.05; DecayHFRatio: 0.23; DecayLFRatio: 1.0;  Reflections: -602;  ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 198;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_CARPETTEDHALLWAY: TFSoundReverbProperties = (Environment: 11; EnvSize: 1.9;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -4000;  RoomLF: 0;  DecayTime: 0.30;  DecayHFRatio: 0.10; DecayLFRatio: 1.0;  Reflections: -1831; ReflectionsDelay: 0.002;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1630; ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_HALLWAY:          TFSoundReverbProperties = (Environment: 12; EnvSize: 1.8;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -300;   RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.59; DecayLFRatio: 1.0;  Reflections: -1219; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 441;  ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_STONECORRIDOR:    TFSoundReverbProperties = (Environment: 13; EnvSize: 13.5;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -237;   RoomLF: 0;  DecayTime: 2.70;  DecayHFRatio: 0.79; DecayLFRatio: 1.0;  Reflections: -1214; ReflectionsDelay: 0.013;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 395;  ReverbDelay: 0.020; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_ALLEY:            TFSoundReverbProperties = (Environment: 14; EnvSize: 7.5;   EnvDiffusion: 0.30;   Room: -1000;  RoomHF: -270;   RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.86; DecayLFRatio: 1.0;  Reflections: -1204; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-4;    ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.125;  EchoDepth: 0.95;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_FOREST:           TFSoundReverbProperties = (Environment: 15; EnvSize: 38.0;  EnvDiffusion: 0.30;   Room: -1000;  RoomHF: -3300;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.54; DecayLFRatio: 1.0;  Reflections: -2560; ReflectionsDelay: 0.162;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-229;  ReverbDelay: 0.088; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.125;  EchoDepth: 1.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 79.0;  Density: 100.0; Flags: $3f);
  FSOUND_PRESET_CITY:             TFSoundReverbProperties = (Environment: 16; EnvSize: 7.5;   EnvDiffusion: 0.50;   Room: -1000;  RoomHF: -800;   RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.67; DecayLFRatio: 1.0;  Reflections: -2273; ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1691; ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 50.0;  Density: 100.0; Flags: $3f);
  FSOUND_PRESET_MOUNTAINS:        TFSoundReverbProperties = (Environment: 17; EnvSize: 100.0; EnvDiffusion: 0.27;   Room: -1000;  RoomHF: -2500;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.21; DecayLFRatio: 1.0;  Reflections: -2780; ReflectionsDelay: 0.300;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1434; ReverbDelay: 0.100; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 1.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 27.0;  Density: 100.0; Flags: $1f);
  FSOUND_PRESET_QUARRY:           TFSoundReverbProperties = (Environment: 18; EnvSize: 17.5;  EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -1000;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.83; DecayLFRatio: 1.0;  Reflections: -10000;ReflectionsDelay: 0.061;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 500;  ReverbDelay: 0.025; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.125;  EchoDepth: 0.70;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);
  FSOUND_PRESET_PLAIN:            TFSoundReverbProperties = (Environment: 19; EnvSize: 42.5;  EnvDiffusion: 0.21;   Room: -1000;  RoomHF: -2000;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.50; DecayLFRatio: 1.0;  Reflections: -2466; ReflectionsDelay: 0.179;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1926; ReverbDelay: 0.100; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 1.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 21.0;  Density: 100.0; Flags: $3f);
  FSOUND_PRESET_PARKINGLOT:       TFSoundReverbProperties = (Environment: 20; EnvSize: 8.3;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: 0;      RoomLF: 0;  DecayTime: 1.65;  DecayHFRatio: 1.50; DecayLFRatio: 1.0;  Reflections: -1363; ReflectionsDelay: 0.008;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-1153; ReverbDelay: 0.012; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);
  FSOUND_PRESET_SEWERPIPE:        TFSoundReverbProperties = (Environment: 21; EnvSize: 1.7;   EnvDiffusion: 0.80;   Room: -1000;  RoomHF: -1000;  RoomLF: 0;  DecayTime: 2.81;  DecayHFRatio: 0.14; DecayLFRatio: 1.0;  Reflections:  429;  ReflectionsDelay: 0.014;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 1023; ReverbDelay: 0.021; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 0.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 80.0;  Density: 60.0;  Flags: $3f);
  FSOUND_PRESET_UNDERWATER:       TFSoundReverbProperties = (Environment: 22; EnvSize: 1.8;   EnvDiffusion: 1.00;   Room: -1000;  RoomHF: -4000;  RoomLF: 0;  DecayTime: 1.49;  DecayHFRatio: 0.10; DecayLFRatio: 1.0;  Reflections: -449;  ReflectionsDelay: 0.007;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 1700; ReverbDelay: 0.011; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 1.18; ModulationDepth: 0.348; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $3f);

(* Non I3DL2 presets *)

  FSOUND_PRESET_DRUGGED:          TFSoundReverbProperties = (Environment: 23; EnvSize: 1.9;   EnvDiffusion: 0.50;   Room: -1000;  RoomHF: 0;      RoomLF: 0;  DecayTime: 8.39;  DecayHFRatio: 1.39; DecayLFRatio: 1.0;  Reflections: -115;  ReflectionsDelay: 0.002;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 985;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 0.25; ModulationDepth: 1.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);
  FSOUND_PRESET_DIZZY:            TFSoundReverbProperties = (Environment: 24; EnvSize: 1.8;   EnvDiffusion: 0.60;   Room: -1000;  RoomHF: -400;   RoomLF: 0;  DecayTime: 17.23; DecayHFRatio: 0.56; DecayLFRatio: 1.0;  Reflections: -1713; ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb:-613;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 1.00;  ModulationTime: 0.81; ModulationDepth: 0.310; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);
  FSOUND_PRESET_PSYCHOTIC:        TFSoundReverbProperties = (Environment: 25; EnvSize: 1.0;   EnvDiffusion: 0.50;   Room: -1000;  RoomHF: -151;   RoomLF: 0;  DecayTime: 7.56;  DecayHFRatio: 0.91; DecayLFRatio: 1.0;  Reflections: -626;  ReflectionsDelay: 0.020;  ReflectionsPan: (0.0, 0.0, 0.0);  Reverb: 774;  ReverbDelay: 0.030; ReverbPan: (0.0, 0.0, 0.0); EchoTime: 0.250;  EchoDepth: 0.00;  ModulationTime: 4.00; ModulationDepth: 1.000; AirAbsorptionHF: -5.0;  HFReference: 5000.0;  LFReference: 250.0; RoomRolloffFactor: 0.0; Diffusion: 100.0; Density: 100.0; Flags: $1f);

(* PlayStation 2 Only presets *)
(* Delphi/Kylix cannot create PlayStation 2 executables, so there is no need to
   convert the PlayStation 2 presets. *)
{$IFDEF COMPILER6_UP}{$J-}{$ENDIF}

(* [DEFINE_END] *)

//===============================================================================================
// FUNCTION PROTOTYPES
//===============================================================================================

{ ================================== }
{ Library load/unload functions.     }
{ ================================== }

{
  If no library name is passed to FMOD_Load, then the default library name
  used.
}

function FMOD_Load(LibName: PChar = nil): Boolean;
procedure FMOD_Unload;

{ ================================== }
{ Initialization / Global functions. }
{ ================================== }

{
  Pre FSOUND_Init functions. These can't be called after FSOUND_Init is
  called (they will fail). They set up FMOD system functionality.
}

var
  FSOUND_SetOutput: function (OutputType: TFSoundOutputTypes): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetDriver: function (Driver: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMixer: function (Mixer: TFSoundMixerTypes): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetBufferSize: function (LenMs: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetHWND: function (Hwnd: THandle): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMinHardwareChannels: function (Min: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMaxHardwareChannels: function (Max: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMemorySystem: function (Pool: Pointer;
        PoolLen: Integer;
        UserAlloc: TFSoundAllocCallback;
        UserRealloc: TFSoundReallocCallback;
        UserFree: TFSoundFreeCallback): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Main initialization / closedown functions
  Note : Use FSOUND_INIT_USEDEFAULTMIDISYNTH with FSOUND_Init for software override with MIDI playback.
       : Use FSOUND_INIT_GLOBALFOCUS with FSOUND_Init to make sound audible
         no matter which window is in focus. (FSOUND_OUTPUT_DSOUND only)
}

var
  FSOUND_Init: function (MixRate: Integer; MaxSoftwareChannels: Integer; Flags: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Close: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime system level functions
}

var
  FSOUND_Update: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};  // This is called to update 3d sound / non-realtime output
  FSOUND_SetSpeakerMode: procedure (SpeakerMode: Cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSFXMasterVolume: procedure (Volume: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPanSeperation: procedure (PanSep: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_File_SetCallbacks: procedure (OpenCallback: TFSoundOpenCallback;
                                       CloseCallback: TFSoundCloseCallback;
                                       ReadCallback: TFSoundReadCallback;
                                       SeekCallback: TFSoundSeekCallback;
                                       TellCallback: TFSoundTellCallback); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  System information functions
}

var
  FSOUND_GetError: function: TFModErrors; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVersion: function: Single; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutput: function: TFSoundOutputTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetOutputHandle: function: Pointer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriver: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMixer: function: TFSoundMixerTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumDrivers: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverName: function (Id: Integer): PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetDriverCaps: function (Id: Integer; var Caps: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FSOUND_GetOutputRate: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMaxChannels: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMaxSamples: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSFXMasterVolume: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetNumHardwareChannels: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetChannelsPlaying: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCPUUsage: function: Single; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMemoryStats: Procedure (var CurrentAlloced: Cardinal; var MaxAlloced: Cardinal); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================================== }
{ Sample management / load functions. }
{ =================================== }

{
  Sample creation and management functions
  Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Sample_Load to load from memory.
         Use FSOUND_LOADRAW      flag with FSOUND_Sample_Load to treat as as raw pcm data.
}

var
  FSOUND_Sample_Load: function (Index: Integer; const NameOrData: PChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Alloc: function (Index: Integer; Length: Integer; Mode: Cardinal; DefFreq: Integer; DefVol: Integer; DefPan: Integer; DefPri: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Free: procedure (Sptr: PFSoundSample); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Upload: function (Sptr: PFSoundSample; SrcData: Pointer; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Lock: function (Sptr: PFSoundSample; Offset: Integer; Length: Integer; var Ptr1: Pointer; var Ptr2: Pointer; var Len1: Cardinal; var Len2: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_Unlock: function (Sptr: PFSoundSample; Ptr1: Pointer; Ptr2: Pointer; Len1: Cardinal; Len2: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Sample control functions
}

var
  FSOUND_Sample_SetMode: function (Sptr: PFSoundSample; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetLoopPoints: function (Sptr: PFSoundSample; LoopStart, LoopEnd: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetDefaults: function (Sptr: PFSoundSample; DefFreq, DefVol, DefPan, DefPri: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMinMaxDistance: function (Sptr: PFSoundSample; Min, Max: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_SetMaxPlaybacks: function (Sptr: PFSoundSample; Max: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Sample information functions
}

var
  FSOUND_Sample_Get: function (SampNo: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetName: function (Sptr: PFSoundSample): PCHAR; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLength: function (Sptr: PFSoundSample): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetLoopPoints: function (Sptr: PFSoundSample; var LoopStart: Integer; var LoopEnd: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetDefaults: function (Sptr: PFSoundSample; var DefFreq: Integer; var DefVol: Integer; var DefPan: Integer; var DefPri: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Sample_GetMode: function (Sptr: PFSoundSample): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============================ }
{ Channel control functions.   }
{ ============================ }

{
  Playing and stopping sounds.
  Note : Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
         Use FSOUND_ALL as the 'channel' variable to control ALL channels with one function call!
}

var
  FSOUND_PlaySound: function (Channel: Integer; Sptr: PFSoundSample): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_PlaySoundEx: function (Channel: Integer; Sptr: PFSoundSample; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_StopSound: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Functions to control playback of a channel.
}

var
  FSOUND_SetFrequency: function (Channel: Integer; Freq: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolume: function (Channel: Integer; Vol: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetVolumeAbsolute: function (Channel: Integer; Vol: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPan: function (Channel: Integer; Pan: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetSurround: function (Channel: Integer; Surround: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetMute: function (Channel: Integer; Mute: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPriority: function (Channel: Integer; Priority: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetReserved: function (Channel: Integer; Reserved: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetPaused: function (Channel: Integer; Paused: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetLoopMode: function (Channel: Integer; LoopMode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_SetCurrentPosition: function (Channel: Integer; Offset: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Channel information functions
}

var
  FSOUND_IsPlaying: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetFrequency: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetVolume: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPan: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetSurround: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetMute: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPriority: function (Channel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetReserved: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetPaused: function (Channel: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetLoopMode: function (Channel: Integer): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentPosition: function (Channel: Integer): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentSample: function (Channel: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_GetCurrentLevels: function (Channel: Integer; L, R: PSingle): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ FX functions.       }
{ =================== }

{
    Functions to control DX8 only effects processing.

    - FX enabled samples can only be played once at a time, not multiple times at once.
    - Sounds have to be created with FSOUND_HW2D or FSOUND_HW3D for this to work.
    - FSOUND_INIT_ENABLEOUTPUTFX can be used to apply hardware effect processing to the
      global mixed output of FMOD's software channels.
    - FSOUND_FX_Enable returns an FX handle that you can use to alter fx parameters.
    - FSOUND_FX_Enable can be called multiple times in a row, even on the same FX type,
      it will return a unique handle for each FX.
    - FSOUND_FX_Enable cannot be called if the sound is playing or locked.
    - Stopping or starting a sound resets all FX and they must be re-enabled each time
      if this happens.
}

var
  FSOUND_FX_Enable: function (Channel: Integer; Fx: Cardinal): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};    { Set bits to enable following fx }

  FSOUND_FX_SetChorus: function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetCompressor: function (FXId: Integer; Gain, Attack, Release, Threshold, Ratio, Predelay: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetDistortion: function (FXId: Integer; Gain, Edge, PostEQCenterFrequency, PostEQBandwidth, PreLowpassCutoff: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetEcho: function (FXId: Integer; WetDryMix, Feedback, LeftDelay, RightDelay: Single; PanDelay: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetFlanger: function (FXId: Integer; WetDryMix, Depth, Feedback, Frequency: Single; Waveform: Integer; Delay: Single; Phase: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetGargle: function (FXId, RateHz, WaveShape: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetI3DL2Reverb: function (FXId, Room, RoomHF: Integer; RoomRolloffFactor, DecayTime, DecayHFRatio: Single; Reflections: Integer; ReflectionsDelay: Single; Reverb: Integer; ReverbDelay, Diffusion, Density, HFReference: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetParamEQ: function (FXId: Integer; Center, Bandwidth, Gain: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_FX_SetWavesReverb: function (FXId: Integer; InGain, ReverbMix, ReverbTime, HighFreqRTRatio: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ 3D sound functions. }
{ =================== }

{
    See also FSOUND_Sample_SetMinMaxDistance (above)
    Note! FSOUND_3D_Update is now called FSOUND_Update.
}

var
  FSOUND_3D_SetDopplerFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetDistanceFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetRolloffFactor: procedure (Scale: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_SetAttributes: function (Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_GetAttributes: function (Channel: Integer; Pos: PFSoundVector; Vel: PFSoundVector): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_3D_Listener_SetCurrent: procedure (current: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_SetAttributes: procedure (Pos: PFSoundVector;
    Vel: PFSoundVector;
    fx: Single;
    fy: Single;
    fz: Single;
    tx: Single;
    ty: Single;
    tz: Single); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_3D_Listener_GetAttributes: procedure (Pos: PFSoundVector;
    Vel: PFSoundVector;
    fx: PSingle;
    fy: PSingle;
    fz: PSingle;
    tx: PSingle;
    ty: PSingle;
    tz: PSingle); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ========================= }
{ File Streaming functions. }
{ ========================= }

{
    Note : Use FSOUND_LOADMEMORY   flag with FSOUND_Stream_Open to stream from memory.
           Use FSOUND_LOADRAW      flag with FSOUND_Stream_Open to treat stream as raw pcm data.
           Use FSOUND_MPEGACCURATE flag with FSOUND_Stream_Open to open mpegs in 'accurate mode' for settime/gettime/getlengthms.
           Use FSOUND_FREE as the 'channel' variable, to let FMOD pick a free channel for you.
}

var
  // call this before opening streams, not after
  FSOUND_Stream_SetBufferSize: function (Ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Open: function(const name_or_data: PChar; Mode: Cardinal; Offset: Integer; Length: Integer): PFSoundStream; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Create: function (Callback: TFSoundStreamCallback; Length: Integer; Mode: Cardinal; SampleRate: Integer; UserData: Integer): PFSoundStream; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Close: function(Stream: PFSoundStream): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Play: function(Channel: Integer; Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_PlayEx: function (Channel: Integer; Stream: PFSoundStream; Dsp: PFSoundDSPUnit; StartPaused: ByteBool): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Stop: function(Stream: PFSoundStream): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetPosition: function (Stream: PFSoundStream; Position: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetPosition: function (Stream: PFSoundStream): Cardinal; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetTime: function (Stream: PFSoundStream; Ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTime: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLength: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetLengthMs: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetMode: function (Stream: PFSoundStream; mode: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetMode: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetLoopPoints: function (Stream: PFSoundStream; LoopStartPCM, LoopEndPCM: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetLoopCount: function (Stream: PFSoundStream; Count: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetOpenState: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSample: function (Stream: PFSoundStream): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF}; { Every stream contains a sample to play back on }
  FSOUND_Stream_CreateDSP: function (Stream: PFSoundStream; Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetEndCallback: function (Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSyncCallback: function (Stream: PFSoundStream; Callback: TFSoundStreamCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  
  FSOUND_Stream_AddSyncPoint: function (Stream: PFSoundStream; PCMOffset: Cardinal; Name: PChar): PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_DeleteSyncPoint: function (Point: PFSyncPoint): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetNumSyncPoints: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSyncPoint: function (Stream: PFSoundStream; Index: Integer): PFSyncPoint; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetSyncPointInfo: function (Point: PFSyncPoint; var PCMOffset: Cardinal): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_SetSubStream: function (Stream: PFSoundStream; Index: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetNumSubStreams: function (Stream: PFSoundStream): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_SetSubStreamSentence: function (Stream: PFSoundStream; var sentencelist: Cardinal; numitems: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
                                                
  FSOUND_Stream_GetNumTagFields: function (Stream: PFSoundStream; var Num: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_GetTagField: function (Stream: PFSoundStream; Num: Integer; var _Type: TFSoundTagFieldType; var Name: PCHAR; var Value: Pointer; Length: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_FindTagField: function (Stream: PFSoundStream; _Type: TFSoundTagFieldType; Name: PChar; var Value: Pointer; var Length: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

  FSOUND_Stream_Net_SetProxy: function (Proxy: PChar): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetLastServerStatus: function: PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_SetBufferProperties: function (BufferSize: Integer; PreBuffer_Percent: Integer; ReBuffer_Percent:  Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetBufferProperties: function (var Buffersize: Integer; var PreBuffer_Percent: Integer;  var ReBuffer_Percent: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_SetMetadataCallback: function (Stream: PFSoundStream; Callback: TFMetaDataCallback; UserData: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Stream_Net_GetStatus: function (Stream: PFSoundStream; var Status: TFSoundStreamNetStatus; var BufferPercentUsed: Integer; var BitRate: Integer; var Flags: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ =================== }
{ CD audio functions. }
{ =================== }

{
    Note : 0 = default cdrom.  Otherwise specify the drive letter, for example. 'D'. 
}

var
  FSOUND_CD_Play: function (Drive: Byte; Track: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPlayMode: procedure (Drive: Byte; Mode: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_Stop: function (Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetPaused: function (Drive: Byte; Paused: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetVolume: function (Drive: Byte; Volume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_SetTrackTime: function (Drive: Byte; ms: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_Eject: function (Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FSOUND_CD_GetPaused: function (Drive: Byte): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrack: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetNumTracks: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetVolume: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackLength: function (Drive: Byte; Track: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_CD_GetTrackTime: function (Drive: Byte): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============== }
{ DSP functions. }
{ ============== }

{
  DSP Unit control and information functions.
}

var
  FSOUND_DSP_Create: function (Callback: TFSoundDSPCallback; Priority: Integer; Param: Integer): PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_Free: procedure (DSPUnit: PFSoundDSPUnit); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetPriority: procedure (DSPUnit: PFSoundDSPUnit; Priority: Integer); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetPriority: function (DSPUnit: PFSoundDSPUnit): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_SetActive: procedure (DSPUnit: PFSoundDSPUnit; Active: ByteBool); {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetActive: function (DSPUnit: PFSoundDSPUnit): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Functions to get hold of FSOUND 'system DSP unit' handles.
}

var
  FSOUND_DSP_GetClearUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetSFXUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetMusicUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetClipAndCopyUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetFFTUnit: function: PFSoundDSPUnit; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Miscellaneous DSP functions
  Note for the spectrum analysis function to work, you have to enable the FFT DSP unit with
  the following code FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE);
  It is off by default to save cpu usage.
}

var
  FSOUND_DSP_MixBuffers: function (DestBuffer: Pointer; SrcBuffer: Pointer; Len: Integer; Freq: Integer; Vol: Integer; Pan: Integer; Mode: Cardinal): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_ClearMixBuffer: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_DSP_GetBufferLength: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};      { Length of each DSP update }
  FSOUND_DSP_GetBufferLengthTotal: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF}; { Total buffer length due to FSOUND_SetBufferSize }
  FSOUND_DSP_GetSpectrum: function: PSingle; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};          { Array of 512 floats - call FSOUND_DSP_SetActive(FSOUND_DSP_GetFFTUnit(), TRUE)) for this to work. }

{ ========================================================================== }
{ Reverb functions. (eax2/eax3 reverb)  (NOT SUPPORTED IN LINUX/CE)               }
{ ========================================================================== }

{
  See structures above for definitions and information on the reverb parameters.
}

var
  FSOUND_Reverb_SetProperties: function (const Prop: TFSoundReverbProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetProperties: function (var Prop: TFSoundReverbProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_SetChannelProperties: function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Reverb_GetChannelProperties: function (Channel: Integer; var Prop: TFSoundReverbChannelProperties): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ================================================ }
{ Recording functions  (NOT SUPPORTED IN LINUX/MAC) }
{ ================================================ }

{
  Recording initialization functions
}

var
  FSOUND_Record_SetDriver: function (OutputType: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetNumDrivers: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriverName: function (Id: Integer): PChar; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetDriver: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Recording functionality. Only one recording session will work at a time.
}

var
  FSOUND_Record_StartSample: function (Sptr: PFSoundSample; Loop: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_Stop: function: ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FSOUND_Record_GetPosition: function: Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{ ============================================================================================= }
{ FMUSIC API (MOD,S3M,XM,IT,MIDI PLAYBACK)                                                      }
{ ============================================================================================= }

{
  Song management / playback functions.
}

var
  FMUSIC_LoadSong: function (const Name: PChar): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_LoadSongEx: function (Name_Or_Data: Pointer; Offset: Integer; Length: Integer; Mode: Cardinal; var SampleList: Integer; SampleListNum: Integer): PFMusicModule; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetOpenState: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_FreeSong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_PlaySong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopSong: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_StopAllSongs: procedure; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FMUSIC_SetZxxCallback: function (Module: PFMusicModule; Callback: TFMusicCallback): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetRowCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; RowStep: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrderCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; OrderStep: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetInstCallback: function (Module: PFMusicModule; Callback: TFMusicCallback; Instrument: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

var
  FMUSIC_SetSample: function (Module: PFMusicModule; SampNo: Integer; Sptr: PFSoundSample): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetUserData: function (Module: PFMusicModule; userdata: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_OptimizeChannels: function (Module: PFMusicModule; MaxChannels: Integer; MinVolume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime song functions.
}

var
  FMUSIC_SetReverb: function (Reverb: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetLooping: function (Module: PFMusicModule; Looping: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetOrder: function (Module: PFMusicModule; Order: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPaused: function (Module: PFMusicModule; Pause: ByteBool): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterVolume: function (Module: PFMusicModule; Volume: Integer): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetMasterSpeed: function (Module: PFMusicModule; Speed: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_SetPanSeperation: function (Module: PFMusicModule; PanSep: Single): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Static song information functions.
}

var
  FMUSIC_GetName: function (Module: PFMusicModule): PCHAR; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetType: function (Module: PFMusicModule): TFMusicTypes; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumOrders: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumPatterns: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumInstruments: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumSamples: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetNumChannels: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSample: function (Module: PFMusicModule; SampNo: Integer): PFSoundSample; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPatternLength: function (Module: PFMusicModule; OrderNo: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

{
  Runtime song information.
}

var
  FMUSIC_IsFinished: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_IsPlaying: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetMasterVolume: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetGlobalVolume: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetOrder: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPattern: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetSpeed: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetBPM: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRow: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetPaused: function (Module: PFMusicModule): ByteBool; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetTime: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetRealChannel: function (Module: PFMusicModule; ModChannel: Integer): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};
  FMUSIC_GetUserData: function (Module: PFMusicModule): Integer; {$IFDEF UNIX} cdecl {$ELSE} stdcall {$ENDIF};

implementation

{$IFDEF fpc}
{$IFDEF UNIX}
(*
uses
  //BaseUnix,
  cTypes;
*)
{$endif}
{$ENDIF}

const
{$IFDEF UNIX}
  FMOD_DLL = 'libfmod.so';
{$ELSE}
{$IFDEF WINDOWS}
  FMOD_DLL = 'fmod.dll';
{$ENDIF}
{$ENDIF}

type
  TFMODModuleHandle = TLibHandle;

const
  INVALID_MODULEHANDLE_VALUE = TFMODModuleHandle(0);

var
  FMODHandle: TFMODModuleHandle;

function GetAddress(Handle: TFMODModuleHandle; FuncName: PChar): Pointer;
begin

  {$IFDEF UNIX}
    Result := GetProcAddress(Handle, copy(FuncName,2,pos('@',FuncName)-2));
  {$ELSE}
    Result := GetProcAddress(Handle, FuncName);
  {$ENDIF}
  Assert(Result <> nil, 'Failed to find ' + FuncName + ' in ' + FMOD_DLL);
end;

function FMOD_Load(LibName: PChar {$ifndef FPC}= nil{$endif}): Boolean;
begin
  Result := False;

  { Make sure the previous library is unloaded }
  FMOD_Unload;

  { If no library name given, use the default library names }
  if LibName = nil then
    LibName := FMOD_DLL;

  { Load the library }
  FMODHandle := LoadLibrary(LibName);
  if FMODHandle = INVALID_MODULEHANDLE_VALUE then
    Exit;

  { Get all the function addresses from the library }
  FSOUND_SetOutput                      := GetAddress(FMODHandle, '_FSOUND_SetOutput@4');
  FSOUND_SetDriver                      := GetAddress(FMODHandle, '_FSOUND_SetDriver@4');
  FSOUND_SetMixer                       := GetAddress(FMODHandle, '_FSOUND_SetMixer@4');
  FSOUND_SetBufferSize                  := GetAddress(FMODHandle, '_FSOUND_SetBufferSize@4');
  FSOUND_SetHWND                        := GetAddress(FMODHandle, '_FSOUND_SetHWND@4');
  FSOUND_SetMinHardwareChannels         := GetAddress(FMODHandle, '_FSOUND_SetMinHardwareChannels@4');
  FSOUND_SetMaxHardwareChannels         := GetAddress(FMODHandle, '_FSOUND_SetMaxHardwareChannels@4');
  FSOUND_SetMemorySystem                := GetAddress(FMODHandle, '_FSOUND_SetMemorySystem@20');
  FSOUND_Init                           := GetAddress(FMODHandle, '_FSOUND_Init@12');
  FSOUND_Close                          := GetAddress(FMODHandle, '_FSOUND_Close@0');
  FSOUND_SetSFXMasterVolume             := GetAddress(FMODHandle, '_FSOUND_SetSFXMasterVolume@4');
  FSOUND_SetPanSeperation               := GetAddress(FMODHandle, '_FSOUND_SetPanSeperation@4');
  FSOUND_SetSpeakerMode                 := GetAddress(FMODHandle, '_FSOUND_SetSpeakerMode@4');
  FSOUND_GetError                       := GetAddress(FMODHandle, '_FSOUND_GetError@0');
  FSOUND_GetVersion                     := GetAddress(FMODHandle, '_FSOUND_GetVersion@0');
  FSOUND_GetOutput                      := GetAddress(FMODHandle, '_FSOUND_GetOutput@0');
  FSOUND_GetOutputHandle                := GetAddress(FMODHandle, '_FSOUND_GetOutputHandle@0');
  FSOUND_GetDriver                      := GetAddress(FMODHandle, '_FSOUND_GetDriver@0');
  FSOUND_GetMixer                       := GetAddress(FMODHandle, '_FSOUND_GetMixer@0');
  FSOUND_GetNumDrivers                  := GetAddress(FMODHandle, '_FSOUND_GetNumDrivers@0');
  FSOUND_GetDriverName                  := GetAddress(FMODHandle, '_FSOUND_GetDriverName@4');
  FSOUND_GetDriverCaps                  := GetAddress(FMODHandle, '_FSOUND_GetDriverCaps@8');
  FSOUND_GetOutputRate                  := GetAddress(FMODHandle, '_FSOUND_GetOutputRate@0');
  FSOUND_GetMaxChannels                 := GetAddress(FMODHandle, '_FSOUND_GetMaxChannels@0');
  FSOUND_GetMaxSamples                  := GetAddress(FMODHandle, '_FSOUND_GetMaxSamples@0');
  FSOUND_GetSFXMasterVolume             := GetAddress(FMODHandle, '_FSOUND_GetSFXMasterVolume@0');
  FSOUND_GetNumHardwareChannels         := GetAddress(FMODHandle, '_FSOUND_GetNumHardwareChannels@0');
  FSOUND_GetChannelsPlaying             := GetAddress(FMODHandle, '_FSOUND_GetChannelsPlaying@0');
  FSOUND_GetCPUUsage                    := GetAddress(FMODHandle, '_FSOUND_GetCPUUsage@0');
  FSOUND_GetMemoryStats                 := GetAddress(FMODHandle, '_FSOUND_GetMemoryStats@8');
  FSOUND_Sample_Load                    := GetAddress(FMODHandle, '_FSOUND_Sample_Load@20');
  FSOUND_Sample_Alloc                   := GetAddress(FMODHandle, '_FSOUND_Sample_Alloc@28');
  FSOUND_Sample_Free                    := GetAddress(FMODHandle, '_FSOUND_Sample_Free@4');
  FSOUND_Sample_Upload                  := GetAddress(FMODHandle, '_FSOUND_Sample_Upload@12');
  FSOUND_Sample_Lock                    := GetAddress(FMODHandle, '_FSOUND_Sample_Lock@28');
  FSOUND_Sample_Unlock                  := GetAddress(FMODHandle, '_FSOUND_Sample_Unlock@20');
  FSOUND_Sample_SetMode                 := GetAddress(FMODHandle, '_FSOUND_Sample_SetMode@8');
  FSOUND_Sample_SetLoopPoints           := GetAddress(FMODHandle, '_FSOUND_Sample_SetLoopPoints@12');
  FSOUND_Sample_SetDefaults             := GetAddress(FMODHandle, '_FSOUND_Sample_SetDefaults@20');
  FSOUND_Sample_SetMinMaxDistance       := GetAddress(FMODHandle, '_FSOUND_Sample_SetMinMaxDistance@12');
  FSOUND_Sample_SetMaxPlaybacks         := GetAddress(FMODHandle, '_FSOUND_Sample_SetMaxPlaybacks@8');
  FSOUND_Sample_Get                     := GetAddress(FMODHandle, '_FSOUND_Sample_Get@4');
  FSOUND_Sample_GetName                 := GetAddress(FMODHandle, '_FSOUND_Sample_GetName@4');
  FSOUND_Sample_GetLength               := GetAddress(FMODHandle, '_FSOUND_Sample_GetLength@4');
  FSOUND_Sample_GetLoopPoints           := GetAddress(FMODHandle, '_FSOUND_Sample_GetLoopPoints@12');
  FSOUND_Sample_GetDefaults             := GetAddress(FMODHandle, '_FSOUND_Sample_GetDefaults@20');
  FSOUND_Sample_GetMode                 := GetAddress(FMODHandle, '_FSOUND_Sample_GetMode@4');
  FSOUND_PlaySound                      := GetAddress(FMODHandle, '_FSOUND_PlaySound@8');
  FSOUND_PlaySoundEx                    := GetAddress(FMODHandle, '_FSOUND_PlaySoundEx@16');
  FSOUND_StopSound                      := GetAddress(FMODHandle, '_FSOUND_StopSound@4');
  FSOUND_SetFrequency                   := GetAddress(FMODHandle, '_FSOUND_SetFrequency@8');
  FSOUND_SetVolume                      := GetAddress(FMODHandle, '_FSOUND_SetVolume@8');
  FSOUND_SetVolumeAbsolute              := GetAddress(FMODHandle, '_FSOUND_SetVolumeAbsolute@8');
  FSOUND_SetPan                         := GetAddress(FMODHandle, '_FSOUND_SetPan@8');
  FSOUND_SetSurround                    := GetAddress(FMODHandle, '_FSOUND_SetSurround@8');
  FSOUND_SetMute                        := GetAddress(FMODHandle, '_FSOUND_SetMute@8');
  FSOUND_SetPriority                    := GetAddress(FMODHandle, '_FSOUND_SetPriority@8');
  FSOUND_SetReserved                    := GetAddress(FMODHandle, '_FSOUND_SetReserved@8');
  FSOUND_SetPaused                      := GetAddress(FMODHandle, '_FSOUND_SetPaused@8');
  FSOUND_SetLoopMode                    := GetAddress(FMODHandle, '_FSOUND_SetLoopMode@8');
  FSOUND_IsPlaying                      := GetAddress(FMODHandle, '_FSOUND_IsPlaying@4');
  FSOUND_GetFrequency                   := GetAddress(FMODHandle, '_FSOUND_GetFrequency@4');
  FSOUND_GetVolume                      := GetAddress(FMODHandle, '_FSOUND_GetVolume@4');
  FSOUND_GetPan                         := GetAddress(FMODHandle, '_FSOUND_GetPan@4');
  FSOUND_GetSurround                    := GetAddress(FMODHandle, '_FSOUND_GetSurround@4');
  FSOUND_GetMute                        := GetAddress(FMODHandle, '_FSOUND_GetMute@4');
  FSOUND_GetPriority                    := GetAddress(FMODHandle, '_FSOUND_GetPriority@4');
  FSOUND_GetReserved                    := GetAddress(FMODHandle, '_FSOUND_GetReserved@4');
  FSOUND_GetPaused                      := GetAddress(FMODHandle, '_FSOUND_GetPaused@4');
  FSOUND_GetLoopMode                    := GetAddress(FMODHandle, '_FSOUND_GetLoopMode@4');
  FSOUND_GetCurrentPosition             := GetAddress(FMODHandle, '_FSOUND_GetCurrentPosition@4');
  FSOUND_SetCurrentPosition             := GetAddress(FMODHandle, '_FSOUND_SetCurrentPosition@8');
  FSOUND_GetCurrentSample               := GetAddress(FMODHandle, '_FSOUND_GetCurrentSample@4');
  FSOUND_GetCurrentLevels               := GetAddress(FMODHandle, '_FSOUND_GetCurrentLevels@12');
  FSOUND_FX_Enable                      := GetAddress(FMODHandle, '_FSOUND_FX_Enable@8');
  FSOUND_FX_SetChorus                   := GetAddress(FMODHandle, '_FSOUND_FX_SetChorus@32');
  FSOUND_FX_SetCompressor               := GetAddress(FMODHandle, '_FSOUND_FX_SetCompressor@28');
  FSOUND_FX_SetDistortion               := GetAddress(FMODHandle, '_FSOUND_FX_SetDistortion@24');
  FSOUND_FX_SetEcho                     := GetAddress(FMODHandle, '_FSOUND_FX_SetEcho@24');
  FSOUND_FX_SetFlanger                  := GetAddress(FMODHandle, '_FSOUND_FX_SetFlanger@32');
  FSOUND_FX_SetGargle                   := GetAddress(FMODHandle, '_FSOUND_FX_SetGargle@12');
  FSOUND_FX_SetI3DL2Reverb              := GetAddress(FMODHandle, '_FSOUND_FX_SetI3DL2Reverb@52');
  FSOUND_FX_SetParamEQ                  := GetAddress(FMODHandle, '_FSOUND_FX_SetParamEQ@16');
  FSOUND_FX_SetWavesReverb              := GetAddress(FMODHandle, '_FSOUND_FX_SetWavesReverb@20');
  FSOUND_Update                         := GetAddress(FMODHandle, '_FSOUND_Update@0');
  FSOUND_3D_SetAttributes               := GetAddress(FMODHandle, '_FSOUND_3D_SetAttributes@12');
  FSOUND_3D_GetAttributes               := GetAddress(FMODHandle, '_FSOUND_3D_GetAttributes@12');
  FSOUND_3D_Listener_SetCurrent         := GetAddress(FMODHandle, '_FSOUND_3D_Listener_SetCurrent@8');
  FSOUND_3D_Listener_SetAttributes       := GetAddress(FMODHandle, '_FSOUND_3D_Listener_SetAttributes@32');
  FSOUND_3D_Listener_GetAttributes       := GetAddress(FMODHandle, '_FSOUND_3D_Listener_GetAttributes@32');
  FSOUND_3D_SetDopplerFactor            := GetAddress(FMODHandle, '_FSOUND_3D_SetDopplerFactor@4');
  FSOUND_3D_SetDistanceFactor           := GetAddress(FMODHandle, '_FSOUND_3D_SetDistanceFactor@4');
  FSOUND_3D_SetRolloffFactor            := GetAddress(FMODHandle, '_FSOUND_3D_SetRolloffFactor@4');
  FSOUND_Stream_Open                    := GetAddress(FMODHandle, '_FSOUND_Stream_Open@16');
  FSOUND_Stream_Create                  := GetAddress(FMODHandle, '_FSOUND_Stream_Create@20');
  FSOUND_Stream_Close                   := GetAddress(FMODHandle, '_FSOUND_Stream_Close@4');
  FSOUND_Stream_Play                    := GetAddress(FMODHandle, '_FSOUND_Stream_Play@8');
  FSOUND_Stream_PlayEx                  := GetAddress(FMODHandle, '_FSOUND_Stream_PlayEx@16');
  FSOUND_Stream_Stop                    := GetAddress(FMODHandle, '_FSOUND_Stream_Stop@4');
  FSOUND_Stream_SetEndCallback          := GetAddress(FMODHandle, '_FSOUND_Stream_SetEndCallback@12');
  FSOUND_Stream_SetSyncCallback         := GetAddress(FMODHandle, '_FSOUND_Stream_SetSyncCallback@12');
  FSOUND_Stream_GetSample               := GetAddress(FMODHandle, '_FSOUND_Stream_GetSample@4');
  FSOUND_Stream_CreateDSP               := GetAddress(FMODHandle, '_FSOUND_Stream_CreateDSP@16');
  FSOUND_Stream_SetBufferSize           := GetAddress(FMODHandle, '_FSOUND_Stream_SetBufferSize@4');
  FSOUND_Stream_SetPosition             := GetAddress(FMODHandle, '_FSOUND_Stream_SetPosition@8');
  FSOUND_Stream_GetPosition             := GetAddress(FMODHandle, '_FSOUND_Stream_GetPosition@4');
  FSOUND_Stream_SetTime                 := GetAddress(FMODHandle, '_FSOUND_Stream_SetTime@8');
  FSOUND_Stream_GetTime                 := GetAddress(FMODHandle, '_FSOUND_Stream_GetTime@4');
  FSOUND_Stream_GetLength               := GetAddress(FMODHandle, '_FSOUND_Stream_GetLength@4');
  FSOUND_Stream_GetLengthMs             := GetAddress(FMODHandle, '_FSOUND_Stream_GetLengthMs@4');
  FSOUND_Stream_SetMode                 := GetAddress(FMODHandle, '_FSOUND_Stream_SetMode@8');
  FSOUND_Stream_GetMode                 := GetAddress(FMODHandle, '_FSOUND_Stream_GetMode@4');
  FSOUND_Stream_SetLoopPoints           := GetAddress(FMODHandle, '_FSOUND_Stream_SetLoopPoints@12');
  FSOUND_Stream_SetLoopCount            := GetAddress(FMODHandle, '_FSOUND_Stream_SetLoopCount@8');
  FSOUND_Stream_GetOpenState            := GetAddress(FMODHandle, '_FSOUND_Stream_GetOpenState@4');
  FSOUND_Stream_AddSyncPoint            := GetAddress(FMODHandle, '_FSOUND_Stream_AddSyncPoint@12');
  FSOUND_Stream_DeleteSyncPoint         := GetAddress(FMODHandle, '_FSOUND_Stream_DeleteSyncPoint@4');
  FSOUND_Stream_GetNumSyncPoints        := GetAddress(FMODHandle, '_FSOUND_Stream_GetNumSyncPoints@4');
  FSOUND_Stream_GetSyncPoint            := GetAddress(FMODHandle, '_FSOUND_Stream_GetSyncPoint@8');
  FSOUND_Stream_GetSyncPointInfo        := GetAddress(FMODHandle, '_FSOUND_Stream_GetSyncPointInfo@8');
  FSOUND_Stream_SetSubStream            := GetAddress(FMODHandle, '_FSOUND_Stream_SetSubStream@8');
  FSOUND_Stream_GetNumSubStreams        := GetAddress(FMODHandle, '_FSOUND_Stream_GetNumSubStreams@4');
  FSOUND_Stream_SetSubStreamSentence    := GetAddress(FMODHandle, '_FSOUND_Stream_SetSubStreamSentence@12');
  FSOUND_Stream_GetNumTagFields         := GetAddress(FMODHandle, '_FSOUND_Stream_GetNumTagFields@8');
  FSOUND_Stream_GetTagField             := GetAddress(FMODHandle, '_FSOUND_Stream_GetTagField@24');
  FSOUND_Stream_FindTagField            := GetAddress(FMODHandle, '_FSOUND_Stream_FindTagField@20');
  FSOUND_Stream_Net_SetProxy            := GetAddress(FMODHandle, '_FSOUND_Stream_Net_SetProxy@4');
  FSOUND_Stream_Net_GetLastServerStatus := GetAddress(FMODHandle, '_FSOUND_Stream_Net_GetLastServerStatus@0');
  FSOUND_Stream_Net_SetBufferProperties := GetAddress(FMODHandle, '_FSOUND_Stream_Net_SetBufferProperties@12');
  FSOUND_Stream_Net_GetBufferProperties := GetAddress(FMODHandle, '_FSOUND_Stream_Net_GetBufferProperties@12');
  FSOUND_Stream_Net_SetMetadataCallback := GetAddress(FMODHandle, '_FSOUND_Stream_Net_SetMetadataCallback@12');
  FSOUND_Stream_Net_GetStatus           := GetAddress(FMODHandle, '_FSOUND_Stream_Net_GetStatus@20');
  FSOUND_CD_Play                        := GetAddress(FMODHandle, '_FSOUND_CD_Play@8');
  FSOUND_CD_SetPlayMode                 := GetAddress(FMODHandle, '_FSOUND_CD_SetPlayMode@8');
  FSOUND_CD_Stop                        := GetAddress(FMODHandle, '_FSOUND_CD_Stop@4');
  FSOUND_CD_SetPaused                   := GetAddress(FMODHandle, '_FSOUND_CD_SetPaused@8');
  FSOUND_CD_SetVolume                   := GetAddress(FMODHandle, '_FSOUND_CD_SetVolume@8');
  FSOUND_CD_SetTrackTime                := GetAddress(FMODHandle, '_FSOUND_CD_SetTrackTime@8');
  FSOUND_CD_Eject                       := GetAddress(FMODHandle, '_FSOUND_CD_Eject@4');
  FSOUND_CD_GetPaused                   := GetAddress(FMODHandle, '_FSOUND_CD_GetPaused@4');
  FSOUND_CD_GetTrack                    := GetAddress(FMODHandle, '_FSOUND_CD_GetTrack@4');
  FSOUND_CD_GetNumTracks                := GetAddress(FMODHandle, '_FSOUND_CD_GetNumTracks@4');
  FSOUND_CD_GetVolume                   := GetAddress(FMODHandle, '_FSOUND_CD_GetVolume@4');
  FSOUND_CD_GetTrackLength              := GetAddress(FMODHandle, '_FSOUND_CD_GetTrackLength@8');
  FSOUND_CD_GetTrackTime                := GetAddress(FMODHandle, '_FSOUND_CD_GetTrackTime@4');
  FSOUND_DSP_Create                     := GetAddress(FMODHandle, '_FSOUND_DSP_Create@12');
  FSOUND_DSP_Free                       := GetAddress(FMODHandle, '_FSOUND_DSP_Free@4');
  FSOUND_DSP_SetPriority                := GetAddress(FMODHandle, '_FSOUND_DSP_SetPriority@8');
  FSOUND_DSP_GetPriority                := GetAddress(FMODHandle, '_FSOUND_DSP_GetPriority@4');
  FSOUND_DSP_SetActive                  := GetAddress(FMODHandle, '_FSOUND_DSP_SetActive@8');
  FSOUND_DSP_GetActive                  := GetAddress(FMODHandle, '_FSOUND_DSP_GetActive@4');
  FSOUND_DSP_GetClearUnit               := GetAddress(FMODHandle, '_FSOUND_DSP_GetClearUnit@0');
  FSOUND_DSP_GetSFXUnit                 := GetAddress(FMODHandle, '_FSOUND_DSP_GetSFXUnit@0');
  FSOUND_DSP_GetMusicUnit               := GetAddress(FMODHandle, '_FSOUND_DSP_GetMusicUnit@0');
  FSOUND_DSP_GetClipAndCopyUnit         := GetAddress(FMODHandle, '_FSOUND_DSP_GetClipAndCopyUnit@0');
  FSOUND_DSP_GetFFTUnit                 := GetAddress(FMODHandle, '_FSOUND_DSP_GetFFTUnit@0');
  FSOUND_DSP_MixBuffers                 := GetAddress(FMODHandle, '_FSOUND_DSP_MixBuffers@28');
  FSOUND_DSP_ClearMixBuffer             := GetAddress(FMODHandle, '_FSOUND_DSP_ClearMixBuffer@0');
  FSOUND_DSP_GetBufferLength            := GetAddress(FMODHandle, '_FSOUND_DSP_GetBufferLength@0');
  FSOUND_DSP_GetBufferLengthTotal       := GetAddress(FMODHandle, '_FSOUND_DSP_GetBufferLengthTotal@0');
  FSOUND_DSP_GetSpectrum                := GetAddress(FMODHandle, '_FSOUND_DSP_GetSpectrum@0');
  FSOUND_Reverb_SetProperties           := GetAddress(FMODHandle, '_FSOUND_Reverb_SetProperties@4');
  FSOUND_Reverb_GetProperties           := GetAddress(FMODHandle, '_FSOUND_Reverb_GetProperties@4');
  FSOUND_Reverb_SetChannelProperties    := GetAddress(FMODHandle, '_FSOUND_Reverb_SetChannelProperties@8');
  FSOUND_Reverb_GetChannelProperties    := GetAddress(FMODHandle, '_FSOUND_Reverb_GetChannelProperties@8');
  FSOUND_Record_SetDriver               := GetAddress(FMODHandle, '_FSOUND_Record_SetDriver@4');
  FSOUND_Record_GetNumDrivers           := GetAddress(FMODHandle, '_FSOUND_Record_GetNumDrivers@0');
  FSOUND_Record_GetDriverName           := GetAddress(FMODHandle, '_FSOUND_Record_GetDriverName@4');
  FSOUND_Record_GetDriver               := GetAddress(FMODHandle, '_FSOUND_Record_GetDriver@0');
  FSOUND_Record_StartSample             := GetAddress(FMODHandle, '_FSOUND_Record_StartSample@8');
  FSOUND_Record_Stop                    := GetAddress(FMODHandle, '_FSOUND_Record_Stop@0');
  FSOUND_Record_GetPosition             := GetAddress(FMODHandle, '_FSOUND_Record_GetPosition@0');
  FSOUND_File_SetCallbacks              := GetAddress(FMODHandle, '_FSOUND_File_SetCallbacks@20');
  FMUSIC_LoadSong                       := GetAddress(FMODHandle, '_FMUSIC_LoadSong@4');
  FMUSIC_LoadSongEx                     := GetAddress(FMODHandle, '_FMUSIC_LoadSongEx@24');
  FMUSIC_GetOpenState                   := GetAddress(FMODHandle, '_FMUSIC_GetOpenState@4');
  FMUSIC_FreeSong                       := GetAddress(FMODHandle, '_FMUSIC_FreeSong@4');
  FMUSIC_PlaySong                       := GetAddress(FMODHandle, '_FMUSIC_PlaySong@4');
  FMUSIC_StopSong                       := GetAddress(FMODHandle, '_FMUSIC_StopSong@4');
  FMUSIC_StopAllSongs                   := GetAddress(FMODHandle, '_FMUSIC_StopAllSongs@0');
  FMUSIC_SetZxxCallback                 := GetAddress(FMODHandle, '_FMUSIC_SetZxxCallback@8');
  FMUSIC_SetRowCallback                 := GetAddress(FMODHandle, '_FMUSIC_SetRowCallback@12');
  FMUSIC_SetOrderCallback               := GetAddress(FMODHandle, '_FMUSIC_SetOrderCallback@12');
  FMUSIC_SetInstCallback                := GetAddress(FMODHandle, '_FMUSIC_SetInstCallback@12');
  FMUSIC_SetSample                      := GetAddress(FMODHandle, '_FMUSIC_SetSample@12');
  FMUSIC_SetUserData                    := GetAddress(FMODHandle, '_FMUSIC_SetUserData@8');
  FMUSIC_OptimizeChannels               := GetAddress(FMODHandle, '_FMUSIC_OptimizeChannels@12');
  FMUSIC_SetReverb                      := GetAddress(FMODHandle, '_FMUSIC_SetReverb@4');
  FMUSIC_SetLooping                     := GetAddress(FMODHandle, '_FMUSIC_SetLooping@8');
  FMUSIC_SetOrder                       := GetAddress(FMODHandle, '_FMUSIC_SetOrder@8');
  FMUSIC_SetPaused                      := GetAddress(FMODHandle, '_FMUSIC_SetPaused@8');
  FMUSIC_SetMasterVolume                := GetAddress(FMODHandle, '_FMUSIC_SetMasterVolume@8');
  FMUSIC_SetMasterSpeed                 := GetAddress(FMODHandle, '_FMUSIC_SetMasterSpeed@8');
  FMUSIC_SetPanSeperation               := GetAddress(FMODHandle, '_FMUSIC_SetPanSeperation@8');
  FMUSIC_GetName                        := GetAddress(FMODHandle, '_FMUSIC_GetName@4');
  FMUSIC_GetType                        := GetAddress(FMODHandle, '_FMUSIC_GetType@4');
  FMUSIC_GetNumOrders                   := GetAddress(FMODHandle, '_FMUSIC_GetNumOrders@4');
  FMUSIC_GetNumPatterns                 := GetAddress(FMODHandle, '_FMUSIC_GetNumPatterns@4');
  FMUSIC_GetNumInstruments              := GetAddress(FMODHandle, '_FMUSIC_GetNumInstruments@4');
  FMUSIC_GetNumSamples                  := GetAddress(FMODHandle, '_FMUSIC_GetNumSamples@4');
  FMUSIC_GetNumChannels                 := GetAddress(FMODHandle, '_FMUSIC_GetNumChannels@4');
  FMUSIC_GetSample                      := GetAddress(FMODHandle, '_FMUSIC_GetSample@8');
  FMUSIC_GetPatternLength               := GetAddress(FMODHandle, '_FMUSIC_GetPatternLength@8');
  FMUSIC_IsFinished                     := GetAddress(FMODHandle, '_FMUSIC_IsFinished@4');
  FMUSIC_IsPlaying                      := GetAddress(FMODHandle, '_FMUSIC_IsPlaying@4');
  FMUSIC_GetMasterVolume                := GetAddress(FMODHandle, '_FMUSIC_GetMasterVolume@4');
  FMUSIC_GetGlobalVolume                := GetAddress(FMODHandle, '_FMUSIC_GetGlobalVolume@4');
  FMUSIC_GetOrder                       := GetAddress(FMODHandle, '_FMUSIC_GetOrder@4');
  FMUSIC_GetPattern                     := GetAddress(FMODHandle, '_FMUSIC_GetPattern@4');
  FMUSIC_GetSpeed                       := GetAddress(FMODHandle, '_FMUSIC_GetSpeed@4');
  FMUSIC_GetBPM                         := GetAddress(FMODHandle, '_FMUSIC_GetBPM@4');
  FMUSIC_GetRow                         := GetAddress(FMODHandle, '_FMUSIC_GetRow@4');
  FMUSIC_GetPaused                      := GetAddress(FMODHandle, '_FMUSIC_GetPaused@4');
  FMUSIC_GetTime                        := GetAddress(FMODHandle, '_FMUSIC_GetTime@4');
  FMUSIC_GetRealChannel                 := GetAddress(FMODHandle, '_FMUSIC_GetRealChannel@8');
  FMUSIC_GetUserData                    := GetAddress(FMODHandle, '_FMUSIC_GetUserData@4');

  Result := True;
end;

procedure FMOD_Unload;
begin
  { Only free the library if it was already loaded }
  if FMODHandle <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(FMODHandle);
  FMODHandle := INVALID_MODULEHANDLE_VALUE;
end;

//{$IFDEF FPC} //FPC do not have this function in its RTL
//const
//  Default8087CW = $1332; //according to the FPC site it's the value used in the
                         //startup code.
//Get a problem here with lazarus ? k00m.
//procedure Set8087CW( value :word ); Assembler;
//asm
//   FLDCW  := value
//end;
//{$endif}

initialization
  FMODHandle := INVALID_MODULEHANDLE_VALUE;

  { Save the current FPU state and then disable FPU exceptions }
//  Saved8087CW := Default8087CW;
//  Set8087CW($133f); { Disable all fpu exceptions }

finalization
  { Make sure the library is unloaded }
  FMOD_Unload;

  { Reset the FPU to the previous state }
//  Set8087CW(Saved8087CW);
end.
