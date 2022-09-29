
isis2std from=l3.cub to=l3.tif mode=grayscale format=tiff stretch=manual minimum=0 maximum=255
echo make L3 slice 
mkdir L3
/usr/bin/convert l3.tif +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/%d.jpg
/usr/bin/convert l3.tif +gravity -crop 20000x1000 R%d.png
/usr/bin/convert R0.png -resize 3000x1000\!  RS0.jpg
/usr/bin/convert R9.png -resize 3000x1000\!  RS9.jpg
/usr/bin/convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/1000%d.jpg
/usr/bin/convert RS9.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L3/2000%d.jpg
rm R[0-9].png RS[0-9].jpg

echo make L2 slice
/usr/bin/convert l3.tif -resize 10000x5000 l2.tif
mkdir L2
/usr/bin/convert l2.tif +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/%d.jpg
/usr/bin/convert l2.tif +gravity -crop 10000x1000 R%d.png
/usr/bin/convert R0.png -resize 3000x1000\!  RS0.jpg
/usr/bin/convert R4.png -resize 3000x1000\!  RS4.jpg
/usr/bin/convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/1000%d.jpg
/usr/bin/convert RS4.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L2/2000%d.jpg
rm R[0-4].png RS[0-4].jpg

echo make L1 slice 
/usr/bin/convert l3.tif -resize 4000x2000 l1.tif
mkdir L1
/usr/bin/convert l1.tif +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L1/%d.jpg

#rm l1.tif l2.tif l3.tif

