Summary: Virtual Moon Atlas - data files 1
Name: virtualmoon-data1
Version: 5
Release: 1
Group: Sciences/Astronomy
License: GPL
URL: http://virtualmoon.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: noarch
Provides: virtualmoon-data1
Requires: virtualmoon
AutoReqProv: no

%description
This software can visualize the Moon aspect for every location, date and hour. 
It permits also to study lunar formations with unique database and pictures library

%files
%defattr(-,root,root)
/usr/share/virtualmoon

