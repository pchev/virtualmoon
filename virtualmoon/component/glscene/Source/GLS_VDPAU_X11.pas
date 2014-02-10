//
// This unit is part of the GLScene Project, http://glscene.org
//
{ : GLS_VDPAU<p>

  <b>History : </b><font size=-1><ul>
  <li>23/10/10 - Yar - Creation
  </ul></font>
}

//{
//  * This copyright notice applies to this header file:
//  *
//  * Copyright (c) 2008-2009 NVIDIA Corporation
//  *
//  * Permission is hereby granted, free of charge, to any person
//  * obtaining a copy of this software and associated documentation
//  * files (the "Software"), to deal in the Software without
//  * restriction, including without limitation the rights to use,
//  * copy, modify, merge, publish, distribute, sublicense, and/or sell
//  * copies of the Software, and to permit persons to whom the
//  * Software is furnished to do so, subject to the following
//  * conditions:
//  *
//  * The above copyright notice and this permission notice shall be
//  * included in all copies or substantial portions of the Software.
//  *
//  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
//  * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
//  * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
//  * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
//  * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
//  * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
//  * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
//  * OTHER DEALINGS IN THE SOFTWARE.
//}

unit GLS_VDPAU_X11;

{$I GLScene.inc}

interface

uses
  GLS_VDPAU_API;

type

  PDisplay = ^Display;
  PVdpDevice = ^VdpDevice;
  PVdpGetProcAddress = ^VdpGetProcAddress;
  PVdpPresentationQueueTarget = ^VdpPresentationQueueTarget;

  TVdpDeviceCreateX11 = function(Display: PDisplay; screen: longint; device: PVdpDevice; get_proc_address: PPVdpGetProcAddress): TVdpStatus; cdecl;
  TVdpPresentationQueueTargetCreateX11 = function(device: TVdpDevice; drawable: TDrawable; target: PVdpPresentationQueueTarget): TVdpStatus; cdecl;

var
  vdp_device_create_x11: TVdpDeviceCreateX11;

implementation

end.
