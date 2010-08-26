#!/bin/bash

echo L1
mkdir L1
#convert USGS_airbrushed_relief_warp.jpg -resize 4000x2000 l1.jpg
convert USGS_airbrushed_relief_warp_L1.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L1/%d.jpg

echo L2
mkdir L2
#convert USGS_airbrushed_relief_warp.jpg -resize 10000x5000 l2.jpg
convert USGS_airbrushed_relief_warp_L2.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/%d.jpg
convert USGS_airbrushed_relief_warp_L2.jpg +gravity -crop 10000x1000 R%d.png
convert R0.png -resize 3000x1000\!  RS0.jpg
convert R4.png -resize 3000x1000\!  RS4.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/1000%d.jpg
convert RS4.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/2000%d.jpg
rm R[0-4].png RS[0-4].jpg

echo L3
mkdir L3
convert USGS_airbrushed_relief_warp_L3.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/%d.jpg
convert USGS_airbrushed_relief_warp_L3.jpg +gravity -crop 20000x1000 R%d.png
convert R0.png -resize 3000x1000\!  RS0.jpg
convert R9.png -resize 3000x1000\!  RS9.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/1000%d.jpg
convert RS9.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/2000%d.jpg
rm R[0-9].png RS[0-9].jpg

