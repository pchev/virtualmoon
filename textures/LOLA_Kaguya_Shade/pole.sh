wget -4 http://astropedia.astrogeology.usgs.gov/download/Moon/LMMP/LOLA-derived/ancillary/LRO_LOLA_Shade_NPole45_100m_v04.tif
wget -4 http://astropedia.astrogeology.usgs.gov/download/Moon/LMMP/LOLA-derived/ancillary/LRO_LOLA_Shade_SPole45_100m_v04.tif

std2isis from=LRO_LOLA_Shade_NPole45_100m_v04.tif to=LRO_LOLA_Shade_NPole45_100m_v04.cub mode=GRAYSCALE
std2isis from=LRO_LOLA_Shade_SPole45_100m_v04.tif to=LRO_LOLA_Shade_SPole45_100m_v04.cub mode=GRAYSCALE

maptemplate map=npole.map projection=PolarStereographic clon=0 clat=90 targopt=user TargetName=Moon londir=PositiveEast londom=180 rngopt=user  minlon=-180 maxlon=180 minlat=-90 maxlat=90 resopt=mpp resolution=100
maptemplate map=spole.map projection=PolarStereographic clon=0 clat=-90 targopt=user TargetName=Moon londir=PositiveEast londom=180 rngopt=user  minlon=-180 maxlon=180 minlat=-90 maxlat=90 resopt=mpp resolution=100
maptemplate map=rect.map projection=Equirectangular clon=0 clat=0 targopt=user TargetName=Moon londir=PositiveEast londom=180 rngopt=user  minlon=-180 maxlon=180 minlat=-90 maxlat=90 resopt=ppd resolution=512

maplab from=LRO_LOLA_Shade_NPole45_100m_v04.cub map=npole.map sample=14400 line=14400 coordinates=latlon lat=90 lon=0
maplab from=LRO_LOLA_Shade_SPole45_100m_v04.cub map=spole.map sample=14400 line=14400 coordinates=latlon lat=-90 lon=0

map2map from=LRO_LOLA_Shade_NPole45_100m_v04.cub map=rect.map to=npole.cub pixres=map minlon=-180 maxlon=180 minlat=60 maxlat=90
map2map from=LRO_LOLA_Shade_SPole45_100m_v04.cub map=rect.map to=spole.cub pixres=map minlon=-180 maxlon=180 minlat=-90 maxlat=-60

echo "0:0 255:253" > stretch.txt
stretch from=npole.cub to=npolestr.cub null=0 lis=0 lrs=0 his=253 hrs=253 readfile=true inputfile=stretch.txt
stretch from=spole.cub to=spolestr.cub null=0 lis=0 lrs=0 his=253 hrs=253 readfile=true inputfile=stretch.txt

reduce from=npolestr.cub to=l6n.cub mode=total ons=160000 onl=13333
reduce from=spolestr.cub to=l6s.cub mode=total ons=160000 onl=13333

handmos from=l6n.cub mosaic=l6.cub priority=ontop insample=1 inline=1 outsample=1 outline=2 create=no
handmos from=l6s.cub mosaic=l6.cub priority=ontop insample=1 inline=1 outsample=1 outline=66667 create=no
