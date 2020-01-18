
# make l6 band
crop from=l6.cub to=l6r\$1.cub line=\$2 nlines=1000 -batchlist=n80.txt
isis2std from=l6r\$1.cub to=l6r\$1.tif mode=grayscale format=tiff stretch=manual minimum=0 maximum=255 -batchlist=n80.txt
rm l6r*.cub
# make l6 slices
mkdir L6
i=1; while ((i<81)); 
do ((j=160*(i-1))); 
 echo convert l6r$i.tif +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene $j  L6/%d.jpg;
 convert l6r$i.tif +repage +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% -scene $j  L6/%d.jpg;
 ((i++));  
done;
convert l6r1.tif -resize 3000x1000\!  RS0.jpg
convert l6r80.tif -resize 3000x1000\!  RS1.jpg
convert RS0.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L6/11000%d.jpg
convert RS1.jpg +gravity -crop 1000x1000 -bordercolor white -border 12x12 -quality 65% L6/12000%d.jpg
rm RS0.jpg RS1.jpg
rm l6r*.tif

