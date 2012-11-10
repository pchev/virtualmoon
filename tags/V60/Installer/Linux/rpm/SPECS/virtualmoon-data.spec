Summary: Virtual Moon Atlas - data files
Name: virtualmoon-data
Version: 5
Release: 1
Group: Sciences/Astronomy
License: GPL
URL: http://virtualmoon.sourceforge.net
Packager: Patrick Chevalley
BuildRoot: %_topdir/%{name}
BuildArch: noarch
Provides: virtualmoon-data
Requires: virtualmoon
AutoReqProv: no

%description
This software can visualize the Moon aspect for every location, date and hour. 
It permits also to study lunar formations with unique database of more than 9000 entries 
and a more than 7000 pictures library

%files
%defattr(-,root,root)
/usr/share/virtualmoon

