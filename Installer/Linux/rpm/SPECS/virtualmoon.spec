Summary: Virtual Moon Atlas
Name: virtualmoon
Version: 5
Release: 1
Group: Sciences/Astronomy
License: GPL
URL: http://virtualmoon.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: i386
Provides: virtualmoon libplan404.so
Requires: gtk2 glib2 pango libjpeg libpng libsqlite3.so.0
AutoReqProv: no

%description
This software can visualize the Moon aspect for every location, date and hour. 
It permits also to study lunar formations with unique database of more than 9000 entries 
and a more than 7000 pictures library

%files
%defattr(-,root,root)
/usr/bin/atlun
/usr/bin/datlun
/usr/bin/photlun
/usr/bin/weblun
/usr/bin/cclun
/usr/lib64/libvma404.so
/usr/share/applications/virtualmoon.desktop
/usr/share/pixmaps/virtualmoon.xpm
/usr/share/doc/virtualmoon
/usr/share/virtualmoon

