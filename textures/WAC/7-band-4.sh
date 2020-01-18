
# make l4 band
crop from=l4.cub to=l4r\$1.cub line=\$2 nlines=1000 -batchlist=n20.txt
isis2std from=l4r\$1.cub to=l4r\$1.tif mode=grayscale format=tiff stretch=manual minimum=0 maximum=255 -batchlist=n20.txt
rm l4r*.cub
# make l4 slices
mkdir L4
i=1; while ((i<21)); 
do ((j=40*(i-1))); 
 echo convert l4r$i.tif +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene $j  L4/%d.jpg;
 convert l4r$i.tif +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene $j  L4/%d.jpg;
 ((i++));  
done;
convert l4r1.tif -resize 3000x1000\!  RS0.jpg
convert l4r20.tif -resize 3000x1000\!  RS1.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L4/1000%d.jpg
convert RS1.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L4/2000%d.jpg
rm RS0.jpg RS1.jpg
rm l4r*.tif

