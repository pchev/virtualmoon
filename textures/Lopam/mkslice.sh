#!/bin/bash

echo L1
mkdir L1
#convert lopamL3.jpg -resize 4000x2000 l1.jpg
convert lopamL1.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L1/%d.jpg

echo L2
mkdir L2
#convert lopamL3.jpg -resize 10000x5000 l2.jpg
convert lopamL2.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/%d.jpg
convert lopamL2.jpg +gravity -crop 10000x1000 R%d.png
convert R0.png -resize 3000x1000\!  RS0.jpg
convert R4.png -resize 3000x1000\!  RS4.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/1000%d.jpg
convert RS4.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/2000%d.jpg
rm R[0-4].png RS[0-4].jpg

echo L3
mkdir L3
convert lopamL3.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/%d.jpg
convert lopamL3.jpg +gravity -crop 20000x1000 R%d.png
convert R0.png -resize 3000x1000\!  RS0.jpg
convert R9.png -resize 3000x1000\!  RS9.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/1000%d.jpg
convert RS9.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/2000%d.jpg
rm R[0-9].png RS[0-9].jpg

exit

echo L4
mkdir L4
# 40000x20000
convert lopamL4.jpg -crop 40000x1000 L4R%d.png
convert L4R0.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 0 L4/%d.jpg
convert L4R1.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 40 L4/%d.jpg
convert L4R2.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 80 L4/%d.jpg
convert L4R3.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 120 L4/%d.jpg
convert L4R4.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 160 L4/%d.jpg
convert L4R5.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 200 L4/%d.jpg
convert L4R6.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 240 L4/%d.jpg
convert L4R7.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 280 L4/%d.jpg
convert L4R8.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 320 L4/%d.jpg
convert L4R9.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 360 L4/%d.jpg
convert L4R10.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 400 L4/%d.jpg
convert L4R11.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 440 L4/%d.jpg
convert L4R12.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 480 L4/%d.jpg
convert L4R13.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 520 L4/%d.jpg
convert L4R14.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 560 L4/%d.jpg
convert L4R15.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 600 L4/%d.jpg
convert L4R16.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 640 L4/%d.jpg
convert L4R17.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 680 L4/%d.jpg
convert L4R18.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 720 L4/%d.jpg
convert L4R19.png +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene 760 L4/%d.jpg
convert L4R0.png -resize 3000x1000\!  RS0.jpg
convert L4R19.png -resize 3000x1000\!  RS19.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L4/1000%d.jpg
convert RS19.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L4/2000%d.jpg
rm RS0.jpg RS19.jpg
