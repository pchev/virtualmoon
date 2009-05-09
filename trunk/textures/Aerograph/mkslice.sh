#!/bin/bash

echo L1
mkdir L1
convert aerograph.jpg -resize 4000x2000 l1.jpg
convert l1.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L1/%d.jpg
rm l1.jpg

echo L2
mkdir L2
convert aerograph.jpg -resize 10000x5000 l2.jpg
convert l2.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/%d.jpg
convert l2.jpg +gravity -crop 10000x1000 R%d.png
convert R0.png -resize 3000x1000\!  RS0.jpg
convert R4.png -resize 3000x1000\!  RS4.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/1000%d.jpg
convert RS4.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/2000%d.jpg
rm R[0-4].png RS[0-4].jpg
rm l2.jpg

