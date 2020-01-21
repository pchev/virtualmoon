Summary: Virtual Moon Atlas - Very high resolution LOPAM
Name: virtualmoon-vhres-lopam
Version: 5
Release: 1
Group: Sciences/Astronomy
License: GPL
URL: http://virtualmoon.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: noarch
Provides: virtualmoon-vhres-lopam
Requires: virtualmoon
AutoReqProv: no

%description
This software can visualize the Moon aspect for every location, date and hour. 
It permits also to study lunar formations with unique database and pictures library

%files
%defattr(-,root,root)
/usr/share/virtualmoon

