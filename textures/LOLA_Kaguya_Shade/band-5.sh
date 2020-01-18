
# make l5 band
crop from=l5.cub to=l5r\$1.cub line=\$2 nlines=1000 -batchlist=n40.txt
isis2std from=l5r\$1.cub to=l5r\$1.tif mode=grayscale format=tiff stretch=manual minimum=0 maximum=255 -batchlist=n40.txt
rm l5r*.cub
# make l5 slices
mkdir L5
i=1; while ((i<41)); 
do ((j=80*(i-1))); 
 echo /usr/bin/convert l5r$i.tif +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene $j  L5/%d.jpg;
 /usr/bin/convert l5r$i.tif +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene $j  L5/%d.jpg;
 ((i++));  
done;
/usr/bin/convert l5r1.tif -resize 3000x1000\!  RS0.jpg
/usr/bin/convert l5r40.tif -resize 3000x1000\!  RS1.jpg
/usr/bin/convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L5/1000%d.jpg
/usr/bin/convert RS1.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L5/2000%d.jpg
rm RS0.jpg RS1.jpg
rm l5r*.tif

