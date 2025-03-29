pds2isis from=WAC_CSHADE_E300N0450_128P.IMG to=WAC_CSHADE_E300N0450_128P.cub
pds2isis from=WAC_CSHADE_E300N1350_128P.IMG to=WAC_CSHADE_E300N1350_128P.cub
pds2isis from=WAC_CSHADE_E300N2250_128P.IMG to=WAC_CSHADE_E300N2250_128P.cub
pds2isis from=WAC_CSHADE_E300N3150_128P.IMG to=WAC_CSHADE_E300N3150_128P.cub
pds2isis from=WAC_CSHADE_E300S0450_128P.IMG to=WAC_CSHADE_E300S0450_128P.cub
pds2isis from=WAC_CSHADE_E300S1350_128P.IMG to=WAC_CSHADE_E300S1350_128P.cub
pds2isis from=WAC_CSHADE_E300S2250_128P.IMG to=WAC_CSHADE_E300S2250_128P.cub
pds2isis from=WAC_CSHADE_E300S3150_128P.IMG to=WAC_CSHADE_E300S3150_128P.cub
pds2isis from=WAC_CSHADE_P900N0000_128P.IMG to=WAC_CSHADE_P900N0000_128P.cub
pds2isis from=WAC_CSHADE_P900S0000_128P.IMG to=WAC_CSHADE_P900S0000_128P.cub

# polar rect map 
maptemplate map=rect.map projection=Equirectangular eqradius=1737400 polradius=1737400 clon=0 clat=0 targopt=user TargetName=Moon londir=PositiveEast londom=180 rngopt=user  minlon=-180 maxlon=180 minlat=-90 maxlat=90 resopt=ppd resolution=128

# polar map convert
map2map from=WAC_CSHADE_P900N0000_128P.cub map=rect.map to=WAC_CSHADE_E900N0000_128P.cub pixres=map minlon=-180 maxlon=180 minlat=60 maxlat=90
map2map from=WAC_CSHADE_P900S0000_128P.cub map=rect.map to=WAC_CSHADE_E900S0000_128P.cub pixres=map minlon=-180 maxlon=180 minlat=-90 maxlat=-60

# mosaic
automos fromlist=mosaic.lst mosaic=WAC_CSHADE_128P.cub matchbandbin=false grange=user minlat=-90 maxlat=90 minlon=-180 maxlon=180

