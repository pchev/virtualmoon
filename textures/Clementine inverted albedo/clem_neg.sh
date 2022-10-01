#!/bin/bash

# clementine negative 

odir=Clementine_Negative

convert "hires_clem negative enhanced.jpg" -resize 10000x5000 l2.png

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
