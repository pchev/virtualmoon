#!/bin/bash
isis2std red=l4.cub+1 green=l4.cub+2 blue=l4.cub+3 to=m4.tif mode=rgb format=tiff stretch=manual rmin=0 rmax=255 gmin=0 gmax=255 bmin=0 bmax=255
isis2std red=l3.cub+1 green=l3.cub+2 blue=l3.cub+3 to=m3.tif mode=rgb format=tiff stretch=manual rmin=0 rmax=255 gmin=0 gmax=255 bmin=0 bmax=255

rm m4.cub  m3.cub
rm print.prt m4.tfw m3.tfw rect.map

echo L4
mkdir L4
# 40000x20000
/usr/bin/convert m4.tif -crop 40000x1000 L4R%d.png
/usr/bin/convert L4R0.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 0 L4/%d.jpg
/usr/bin/convert L4R1.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 40 L4/%d.jpg
/usr/bin/convert L4R2.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 80 L4/%d.jpg
/usr/bin/convert L4R3.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 120 L4/%d.jpg
/usr/bin/convert L4R4.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 160 L4/%d.jpg
/usr/bin/convert L4R5.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 200 L4/%d.jpg
/usr/bin/convert L4R6.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 240 L4/%d.jpg
/usr/bin/convert L4R7.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 280 L4/%d.jpg
/usr/bin/convert L4R8.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 320 L4/%d.jpg
/usr/bin/convert L4R9.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 360 L4/%d.jpg
/usr/bin/convert L4R10.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 400 L4/%d.jpg
/usr/bin/convert L4R11.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 440 L4/%d.jpg
/usr/bin/convert L4R12.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 480 L4/%d.jpg
/usr/bin/convert L4R13.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 520 L4/%d.jpg
/usr/bin/convert L4R14.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 560 L4/%d.jpg
/usr/bin/convert L4R15.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 600 L4/%d.jpg
/usr/bin/convert L4R16.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 640 L4/%d.jpg
/usr/bin/convert L4R17.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 680 L4/%d.jpg
/usr/bin/convert L4R18.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 720 L4/%d.jpg
/usr/bin/convert L4R19.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 760 L4/%d.jpg
/usr/bin/convert L4R0.png -resize 3000x1000\!  RS0.jpg
/usr/bin/convert L4R19.png -resize 3000x1000\!  RS19.jpg
/usr/bin/convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L4/1000%d.jpg
/usr/bin/convert RS19.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L4/2000%d.jpg
rm RS0.jpg RS19.jpg L4R*.png

# make L3 slice 
mkdir L3
/usr/bin/convert m3.tif +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/%d.jpg
/usr/bin/convert m3.tif +gravity -crop 20000x1000 R%d.png
/usr/bin/convert R0.png -resize 3000x1000\!  RS0.jpg
/usr/bin/convert R9.png -resize 3000x1000\!  RS9.jpg
/usr/bin/convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/1000%d.jpg
/usr/bin/convert RS9.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/2000%d.jpg
rm R[0-9].png RS[0-9].jpg

# make L2 slice 
/usr/bin/convert m3.tif -resize 10000x5000 l2.png
mkdir L2
/usr/bin/convert l2.png +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/%d.jpg
/usr/bin/convert l2.png +gravity -crop 10000x1000 R%d.png
/usr/bin/convert R0.png -resize 3000x1000\!  RS0.jpg
/usr/bin/convert R4.png -resize 3000x1000\!  RS4.jpg
/usr/bin/convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/1000%d.jpg
/usr/bin/convert RS4.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/2000%d.jpg
rm R[0-4].png RS[0-4].jpg

# resize to 4000x2000
/usr/bin/convert l2.png -resize 4000x2000 l1.png
# make L1 slice 
mkdir L1
/usr/bin/convert l1.png +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L1/%d.jpg

rm l1.png l2.png l3.png



