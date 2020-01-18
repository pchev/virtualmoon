wget https://planetarymaps.usgs.gov/mosaic/LolaKaguya_Topo/Lunar_LRO_LOLAKaguya_Shade_60N60S_512ppd.tif
wget https://planetarymaps.usgs.gov/mosaic/LolaKaguya_Topo/Lunar_LRO_LOLAKaguya_Shade_60N60S_512ppd_pds3.lbl
pds2isis f=Lunar_LRO_LOLAKaguya_Shade_60N60S_512ppd_pds3.lbl t=LOLAKaguya.cub

maptemplate map=rect.map projection=Equirectangular clon=0 clat=0 targopt=user TargetName=Moon londir=PositiveEast londom=180 rngopt=user  minlon=-180 maxlon=180 minlat=-90 maxlat=90 resopt=ppd resolution=512
map2map from=LOLAKaguya.cub map=rect.map to=full.cub minlon=-180 maxlon=180 minlat=-90 maxlat=90

echo "0:0 255:253" > stretch.txt
stretch from=full.cub to=fullstr.cub null=255 readfile=true inputfile=stretch.txt

reduce from=fullstr.cub to=l6.cub mode=total ons=160000 onl=80000

./pole.sh

reduce from=l6.cub to=l5.cub mode=total ons=80000 onl=40000
reduce from=l5.cub to=l4.cub mode=total ons=40000 onl=20000
reduce from=l4.cub to=l3.cub mode=total ons=20000 onl=10000

./band1-2-3.sh
./band-4.sh
./band-5.sh
./band-6.sh
./mkzip.sh
