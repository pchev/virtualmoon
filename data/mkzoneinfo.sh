# Extract part of tzdata package for use on Windows systems.

wd=`pwd`

rm -rf tzdata zoneinfo
mkdir tzdata
mkdir zoneinfo

wget --retr-symlinks 'ftp://ftp.iana.org/tz/tzcode-latest.tar.gz'
if [[ $? != 0 ]]; then echo wget error;  exit 1; fi
wget --retr-symlinks 'ftp://ftp.iana.org/tz/tzdata-latest.tar.gz'
if [[ $? != 0 ]]; then echo wget error;  exit 1; fi

cd tzdata

tar xzf ../tzcode*.tar.gz
tar xzf ../tzdata*.tar.gz

make TOPDIR=$wd/tzdata/ install

cd etc/zoneinfo
cp zone.tab $wd/zoneinfo
cat zone.tab |grep -v \# | cut -f3 |xargs -I'{}' -n1  cp -L --parent '{}' $wd/zoneinfo
cp -rL Etc $wd/zoneinfo

cd usr/share/zoneinfo
if [[ $? != 0 ]]; then  exit 1; fi
cp zone.tab $wd/zoneinfo
if [[ $? != 0 ]]; then  exit 1; fi
cat zone.tab |grep -v \# | cut -f3 |xargs -I'{}' -n1  cp --parent '{}' $wd/zoneinfo
if [[ $? != 0 ]]; then  exit 1; fi
cp -a Etc $wd/zoneinfo
if [[ $? != 0 ]]; then  exit 1; fi


cd $wd

rm -rf tzdata
rm tzcode*
rm tzdata*
