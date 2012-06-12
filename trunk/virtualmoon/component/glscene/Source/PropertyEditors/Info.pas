// Info
{ : Informations on OpenGL driver.<p>

  <b>History : </b><font size=-1><ul>
  <li>23/08/10 - Yar - Added OpenGLTokens to uses, replaced OpenGL1x functions to OpenGLAdapter
  <li>13/06/10 - DaStr - Removed compiler hints
  <li>04/05/10 - Yar - Redecoration (thanks Conferno and Predator)
  <li>20/02/10 - DanB - Now uses correct DC, rather than using
  the info form (bug due to "with" keyword)
  <li>25/10/08 - DanB - Delphi 2009 compatibility, extensions are now looked
  up from www.opengl.org/registry/
  <li>29/03/07 - DaStr - Renamed LINUX to KYLIX (BugTrackerID=1681585)
  <li>08/07/04 - LR - Suppress CommCtrl in the uses of Linux
  <li>06/07/04 - LR - Display some infos for Linux
  <li>03/07/04 - LR - Make change for Linux
  <li>21/02/04 - EG - Added extensions popup menu and hyperlink to
  Delphi3D's hardware registry
  <li>08/02/04 - NelC - Added option for modal
  <li>09/09/03 - NelC - Added Renderer info
  <li>26/06/03 - EG - Double-clicking an extension will now go to its OpenGL
  registry webpage
  <li>22/05/03 - EG - Added Texture Units info
  <li>21/07/02 - EG - No longer modal
  <li>03/02/02 - EG - InfoForm registration mechanism
  <li>24/08/01 - EG - Compatibility with new Buffer classes
  <li>17/04/00 - EG - Creation of header, minor layout changes
  </ul></font>
}
unit Info;

interface

{$I GLScene.inc}

uses
  Windows,
{$IFDEF GLS_DELPHI_XE2_UP}
  VCL.Forms, VCL.Controls, VCL.Buttons, VCL.StdCtrls, VCL.ComCtrls,
  VCL.ExtCtrls, VCL.Graphics, VCL.Menus, VCL.Imaging.JPEG,
{$ELSE}
  Forms, Controls, Buttons, StdCtrls, ComCtrls,
  ExtCtrls, Graphics, Menus, JPEG,
{$ENDIF}
  GLScene, Classes;

type

  TInfoForm = class(TForm)
    AccLabel: TLabel;
    AccumLabel: TLabel;
    AuxLabel: TLabel;
    ClipLabel: TLabel;
    ColorLabel: TLabel;
    CopyLabel: TLabel;
    DepthLabel: TLabel;
    DoubleLabel: TLabel;
    EvalLabel: TLabel;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label23: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label37: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LightLabel: TLabel;
    ListLabel: TLabel;
    Memo1: TMemo;
    Contributors: TMemo;
    ModelLabel: TLabel;
    NameLabel: TLabel;
    OverlayLabel: TLabel;
    PageControl: TPageControl;
    PixelLabel: TLabel;
    ProjLabel: TLabel;
    RendererLabel: TLabel;
    ScrollBox1: TScrollBox;
    TabSheet4: TTabSheet;
    StencilLabel: TLabel;
    StereoLabel: TLabel;
    SubLabel: TLabel;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TexSizeLabel: TLabel;
    TexStackLabel: TLabel;
    TexUnitsLabel: TLabel;
    UnderlayLabel: TLabel;
    VendorLabel: TLabel;
    VersionLabel: TLabel;
    TabSheet5: TTabSheet;
    Extensions: TListBox;
    PMWebLink: TPopupMenu;
    MIRegistryLink: TMenuItem;
    MIDelphi3D: TMenuItem;
    TabSheet1: TTabSheet;
    CloseButton: TButton;
    VersionLbl: TLabel;
    ViewLabel: TLabel;
    WebsiteLbl: TLabel;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ExtensionsDblClick(Sender: TObject);
    procedure ExtensionsClick(Sender: TObject);
    procedure ExtensionsKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure MIDelphi3DClick(Sender: TObject);
    procedure WebsiteLblClick(Sender: TObject);
  protected
    procedure LoadContributors;
    function GetSceneVersion: string;
  public
    procedure GetInfoFrom(aSceneBuffer: TGLSceneBuffer);
  end;

implementation

uses
  OpenGLTokens, OpenGLAdapter, GLContext, SysUtils, GLCrossPlatform;

{$R *.dfm}
{$R Info.res}

// ShowInfoForm
//
procedure ShowInfoForm(aSceneBuffer: TGLSceneBuffer; Modal: boolean);
var
  infoForm: TInfoForm;
begin
  infoForm := TInfoForm.Create(nil);
  try
    infoForm.GetInfoFrom(aSceneBuffer);
    with infoForm do
      if Modal then
        ShowModal
      else
        Show;
  except
    infoForm.Free;
    raise;
  end;
end;

// CloseButtonClick
//
procedure TInfoForm.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

// GetInfoFrom
//
procedure TInfoForm.GetInfoFrom(aSceneBuffer: TGLSceneBuffer);
const
  DRIVER_MASK = PFD_GENERIC_FORMAT or PFD_GENERIC_ACCELERATED;
var
  pfd: TPixelformatDescriptor;
  pixelFormat: Integer;
  dc: HDC;
  i: Integer;
  ExtStr: String;

  procedure IntLimitToLabel(const aLabel: TLabel; const aLimit: TLimitType);
  begin
    aLabel.Caption := IntToStr(aSceneBuffer.LimitOf[aLimit]);
  end;

begin
  Caption := Caption + ' (current context in ' +
    (aSceneBuffer.Owner as TComponent).Name + ')';
  aSceneBuffer.RenderingContext.Activate;
  try
    with aSceneBuffer do
    begin
      // common properties
      VendorLabel.Caption := String(GL.GetString(GL_VENDOR));
      RendererLabel.Caption := String(GL.GetString(GL_RENDERER));
      dc := wglGetCurrentDC();
      pixelFormat := GetPixelFormat(dc);
      DescribePixelFormat(dc, pixelFormat, SizeOf(pfd), pfd);
      // figure out the driver type
      if (DRIVER_MASK and pfd.dwFlags) = 0 then
        AccLabel.Caption := 'Installable Client Driver'
      else if (DRIVER_MASK and pfd.dwFlags) = DRIVER_MASK then
        AccLabel.Caption := 'Mini-Client Driver'
      else if (DRIVER_MASK and pfd.dwFlags) = PFD_GENERIC_FORMAT then
        AccLabel.Caption := 'Generic Software Driver';
      VersionLabel.Caption := String(GL.GetString(GL_VERSION));
      ExtStr := String(GL.GetString(GL_EXTENSIONS));
      Extensions.Clear;
      while Length(ExtStr) > 0 do
      begin
        i := Pos(' ', ExtStr);
        if i = 0 then
          i := 255;
        Extensions.Items.Add(Copy(ExtStr, 1, i - 1));
        Delete(ExtStr, 1, i);
      end;

      if LimitOf[limDoubleBuffer] = GL_TRUE then
        DoubleLabel.Caption := 'yes'
      else
        DoubleLabel.Caption := 'no';

      if LimitOf[limStereo] = GL_TRUE then
        StereoLabel.Caption := 'yes'
      else
        StereoLabel.Caption := 'no';

      // Include WGL extensions
      if GL.W_ARB_extensions_string then
      begin
        ExtStr := String(GL.WGetExtensionsStringARB(dc));
        while Length(ExtStr) > 0 do
        begin
          i := Pos(' ', ExtStr);
          if i = 0 then
            i := 255;
          Extensions.Items.Add(Copy(ExtStr, 1, i - 1));
          Delete(ExtStr, 1, i);
        end;
      end;

      // Some extra info about the double buffer mode
      if (pfd.dwFlags and PFD_DOUBLEBUFFER) = PFD_DOUBLEBUFFER then
      begin
        CopyLabel.Caption := '';
        if (pfd.dwFlags and PFD_SWAP_EXCHANGE) > 0 then
          CopyLabel.Caption := 'exchange';
        if (pfd.dwFlags and PFD_SWAP_COPY) > 0 then
        begin
          if Length(CopyLabel.Caption) > 0 then
            CopyLabel.Caption := CopyLabel.Caption + ', ';
          CopyLabel.Caption := CopyLabel.Caption + 'copy';
        end;
        if Length(CopyLabel.Caption) = 0 then
          CopyLabel.Caption := 'no info available';
      end
      else
      begin
        CopyLabel.Caption := 'n/a';
      end;
      // buffer and pixel depths
      ColorLabel.Caption :=
        Format('red: %d,  green: %d,  blue: %d,  alpha: %d  bits',
        [LimitOf[limRedBits], LimitOf[limGreenBits], LimitOf[limBlueBits],
        LimitOf[limAlphaBits]]);
      DepthLabel.Caption := Format('%d bits', [LimitOf[limDepthBits]]);
      StencilLabel.Caption := Format('%d bits', [LimitOf[limStencilBits]]);
      AccumLabel.Caption :=
        Format('red: %d,  green: %d,  blue: %d,  alpha: %d  bits',
        [LimitOf[limAccumRedBits], LimitOf[limAccumGreenBits],
        LimitOf[limAccumBlueBits], LimitOf[limAccumAlphaBits]]);
      IntLimitToLabel(AuxLabel, limAuxBuffers);
      IntLimitToLabel(SubLabel, limSubpixelBits);
      OverlayLabel.Caption := IntToStr(pfd.bReserved and 7);
      UnderlayLabel.Caption := IntToStr(pfd.bReserved shr 3);

      // Maximum values
      IntLimitToLabel(ClipLabel, limClipPlanes);
      IntLimitToLabel(EvalLabel, limEvalOrder);
      IntLimitToLabel(LightLabel, limLights);
      IntLimitToLabel(ListLabel, limListNesting);
      IntLimitToLabel(ModelLabel, limModelViewStack);
      IntLimitToLabel(ViewLabel, limViewportDims);

      IntLimitToLabel(NameLabel, limNameStack);
      IntLimitToLabel(PixelLabel, limPixelMapTable);
      IntLimitToLabel(ProjLabel, limProjectionStack);
      IntLimitToLabel(TexSizeLabel, limTextureSize);
      IntLimitToLabel(TexStackLabel, limTextureStack);
      IntLimitToLabel(TexUnitsLabel, limNbTextureUnits);
    end;
    VersionLbl.Caption := GetSceneVersion;
  finally
    aSceneBuffer.RenderingContext.Deactivate;
  end;
end;

// ------------------------------------------------------------------------------

procedure TInfoForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
end;

// ------------------------------------------------------------------------------

procedure TInfoForm.FormKeyPress(Sender: TObject; var Key: Char);

begin
  if Key = #27 then
    Close;
end;

procedure TInfoForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Release;
end;

procedure TInfoForm.ExtensionsDblClick(Sender: TObject);
var
  p: Integer;
  url, buf: String;
begin
  with Extensions do
  begin
    if ItemIndex < 0 then
      Exit;
    url := Items[ItemIndex];
  end;
  p := Pos('_', url);
  buf := Copy(url, 1, p - 1);
  url := Copy(url, p + 1, 255);
  if (buf <> 'GL') and (buf <> 'WGL') and (buf <> 'GLX') then
    Exit;
  p := Pos('_', url);
  buf := Copy(url, 1, p - 1);
  url := 'http://www.opengl.org/registry/specs/' + buf + '/' +
    Copy(url, p + 1, 255) + '.txt';
  ShowHTMLUrl(url);
end;

procedure TInfoForm.MIDelphi3DClick(Sender: TObject);
var
  url: String;
begin
  with Extensions do
  begin
    if ItemIndex < 0 then
      Exit;
    url := 'http://www.delphi3d.net/hardware/extsupport.php?extension=' +
      Items[ItemIndex];
  end;
  ShowHTMLUrl(url);
end;

procedure TInfoForm.ExtensionsClick(Sender: TObject);
var
  extName: String;
begin
  if Extensions.ItemIndex < 0 then
    Extensions.PopupMenu := nil
  else
  begin
    Extensions.PopupMenu := PMWebLink;
    extName := Extensions.Items[Extensions.ItemIndex];
    MIRegistryLink.Caption := 'View OpenGL Extension Registry for ' + extName;
    MIDelphi3D.Caption := 'View Delphi3D Hardware Registry for ' + extName;
  end;
end;

procedure TInfoForm.ExtensionsKeyPress(Sender: TObject; var Key: Char);
begin
  ExtensionsClick(Sender);
end;

procedure TInfoForm.FormShow(Sender: TObject);
begin
  PageControl.ActivePageIndex := 0;
end;

procedure TInfoForm.LoadContributors;
// var
// ContributorsFileName: string;
begin
  // In the future, will be loaded from a file

  { ContributorsFileName:=
    // 'GLSceneContributors.txt';

    if FileExistsUTF8(ContributorsFileName) then
    Contributors.Lines.LoadFromFile(UTF8ToSys(ContributorsFileName))
    else
    Contributors.Lines.Text:='Cannot find contributors list.';
    Contributors.Lines.Add( ContributorsFileName) }
end;

function TInfoForm.GetSceneVersion: string;
var
  FExePath, FGLSceneRevision: string;
begin
  FGLSceneRevision := Copy(GLSCENE_REVISION, 12, 4);
  FExePath := ExtractFilePath(ParamStr(0));
  if FileExists(FExePath + 'GLSceneRevision') then 
  try
    with TStringList.Create do 
    try
      LoadFromFile(FExePath + 'GLSceneRevision');
      if (Count >= 1) and (trim(Strings[0]) <> '') then
        FGLSceneRevision:= trim(Strings[0]);
    finally
      Free;
    end;
  except
  end;

  Result := Format(GLSCENE_VERSION, [FGLSceneRevision]);
end;

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
procedure TInfoForm.WebsiteLblClick(Sender: TObject);
begin
  ShowHTMLUrl(WebsiteLbl.Caption);
end;

initialization

// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------
// ------------------------------------------------------------------------------

RegisterInfoForm(ShowInfoForm);

end.
