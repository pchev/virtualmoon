
convert Map_15072022_PreProof_equi.jpg -resize 20000x10000 CNSA-Geological-l3.png

echo make L3 slice 
mkdir L3
convert CNSA-Geological-l3.png +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/%d.jpg
convert CNSA-Geological-l3.png +gravity -crop 20000x1000 R%d.png
convert R0.png -resize 3000x1000\!  RS0.jpg
convert R9.png -resize 3000x1000\!  RS9.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/1000%d.jpg
convert RS9.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/2000%d.jpg
rm R[0-9].png RS[0-9].jpg

echo make L2 slice
convert CNSA-Geological-l3.png -resize 10000x5000 l2.tif
mkdir L2
convert l2.tif +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/%d.jpg
convert l2.tif +gravity -crop 10000x1000 R%d.png
convert R0.png -resize 3000x1000\!  RS0.jpg
convert R4.png -resize 3000x1000\!  RS4.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/1000%d.jpg
convert RS4.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/2000%d.jpg
rm R[0-4].png RS[0-4].jpg

echo make L1 slice 
convert l2.tif -resize 4000x2000 l1.tif
mkdir L1
convert l1.tif +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L1/%d.jpg


