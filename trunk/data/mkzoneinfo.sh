# Extract part of tzdata package for use on Windows systems.

wd=`pwd`

rm -rf tzdata zoneinfo
mkdir tzdata
mkdir zoneinfo

wget --retr-symlinks 'ftp://ftp.iana.org/tz/tz*.tar.gz'

cd tzdata

tar xzf ../tzcode*.tar.gz
tar xzf ../tzdata*.tar.gz

make TOPDIR=$wd/tzdata/ install

cd etc/zoneinfo
cp zone.tab $wd/zoneinfo
cat zone.tab |grep -v \# | cut -f3 |xargs -I'{}' -n1  cp -L --parent '{}' $wd/zoneinfo
cp -rL Etc $wd/zoneinfo

cd $wd

rm -rf tzdata
rm tzcode*
rm tzdata*
