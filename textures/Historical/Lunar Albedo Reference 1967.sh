#!/bin/bash

# Lunar Albedo Reference 1967 
ifile="Lunar Albedo Reference 1967.png"
odir=Lunar-Albedo-Reference-1967
# 8172x8172
xc=4086
yc=4086
# ppd= 8172*pi/2/180 = 71.31
ppd=71.31

std2isis from="$ifile" to=m1.cub mode=RGB

maptemplate map=ortho.map projection=ORTHOGRAPHIC clon=0 clat=0 targopt=user TargetName=Moon eqradius=1737.4 polradius=1737.4 londir=PositiveEast londom=180 rngopt=user  minlon=-90 maxlon=90 minlat=-90 maxlat=90 resopt=ppd resolution=$ppd 

maptemplate map=rect.map projection=Equirectangular clon=0 clat=0 targopt=user TargetName=Moon eqradius=1737.4 polradius=1737.4 londir=PositiveEast londom=180 rngopt=user  minlon=-180 maxlon=180 minlat=-90 maxlat=90

maplab from=m1.cub map=ortho.map sample=$xc line=$yc coordinates=latlon lat=0 lon=0

map2map from=m1.cub map=rect.map to=m2.cub minlon=-180 maxlon=180

reduce from=m2.cub to=m3.cub mode=total ons=20000 onl=10000

stretch from=m3.cub to=l3.cub null=180

isis2std red=l3.cub+1 green=l3.cub+2 blue=l3.cub+3 to=l3.png mode=rgb format=png stretch=manual rmin=0 rmax=255 gmin=0 gmax=255 bmin=0 bmax=255

rm m1.cub  m2.cub  m3.cub l3.cub ortho.map rect.map l3.pgw
rm print.prt

mkdir $odir

#echo make L3 slice 
#mkdir $odir/L3
#/usr/bin/convert l3.png +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L3/%d.jpg
#/usr/bin/convert l3.png +gravity -crop 20000x1000 R%d.png
#/usr/bin/convert R0.png -resize 3000x1000\!  RS0.jpg
#/usr/bin/convert R9.png -resize 3000x1000\!  RS9.jpg
#/usr/bin/convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L3/1000%d.jpg
#/usr/bin/convert RS9.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L3/2000%d.jpg
#rm R[0-9].png RS[0-9].jpg

# make L2 slice 
/usr/bin/convert l3.png -resize 10000x5000 l2.png
mkdir $odir/L2
/usr/bin/convert l2.png +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L2/%d.jpg
/usr/bin/convert l2.png +gravity -crop 10000x1000 R%d.png
/usr/bin/convert R0.png -resize 3000x1000\!  RS0.jpg
/usr/bin/convert R4.png -resize 3000x1000\!  RS4.jpg
/usr/bin/convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L2/1000%d.jpg
/usr/bin/convert RS4.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L2/2000%d.jpg
rm R[0-4].png RS[0-4].jpg

# resize to 4000x2000
/usr/bin/convert l2.png -resize 4000x2000 l1.png
# make L1 slice 
mkdir $odir/L1
/usr/bin/convert l1.png +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L1/%d.jpg

rm l1.png l2.png l3.png



