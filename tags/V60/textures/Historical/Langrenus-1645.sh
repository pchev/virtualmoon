#!/bin/bash

# Langrenus 1645 
ifile="Langrenus 1645.jpg"
odir=Langrenus-1645
# 4589x4589
xc=2295
yc=2295
# ppd= 4589*pi/2/180 = 40.05 
ppd=40.05

std2isis from="$ifile" to=m1.cub mode=GRAYSCALE

maptemplate map=ortho.map projection=ORTHOGRAPHIC clon=0 clat=0 targopt=user TargetName=Moon londir=PositiveEast londom=180 rngopt=user  minlon=-90 maxlon=90 minlat=-90 maxlat=90 resopt=ppd resolution=$ppd 

maptemplate map=rect.map projection=Equirectangular clon=0 clat=0 targopt=user TargetName=Moon londir=PositiveEast londom=180 rngopt=user  minlon=-180 maxlon=180 minlat=-90 maxlat=90

maplab from=m1.cub map=ortho.map sample=$xc line=$yc coordinates=latlon lat=0 lon=0

map2map from=m1.cub map=rect.map to=m2.cub minlon=-180 maxlon=180

iwidth=$(catlab from=m2.cub|grep " Samples" | awk '{print $3}')
if (( iwidth>10000))
  then reduce from=m2.cub to=m3.cub mode=total ons=10000 onl=5000
  else enlarge from=m2.cub to=m3.cub mode=total ons=10000 onl=5000
fi

stretch from=m3.cub to=l2.cub null=180

isis2std from=l2.cub to=l2.png mode=grayscale format=png stretch=manual minimum=0 maximum=255

rm m1.cub  m2.cub  m3.cub l2.cub ortho.map rect.map l2.pgw
rm print.prt

mkdir $odir

# make L2 slice 
mkdir $odir/L2
convert l2.png +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L2/%d.jpg
convert l2.png +gravity -crop 10000x1000 R%d.png
convert R0.png -resize 3000x1000\!  RS0.jpg
convert R4.png -resize 3000x1000\!  RS4.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L2/1000%d.jpg
convert RS4.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L2/2000%d.jpg
rm R[0-4].png RS[0-4].jpg

# resize to 4000x2000
convert l2.png -resize 4000x2000 l1.png
# make L1 slice 
mkdir $odir/L1
convert l1.png +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% $odir/L1/%d.jpg

rm l1.png l2.png



