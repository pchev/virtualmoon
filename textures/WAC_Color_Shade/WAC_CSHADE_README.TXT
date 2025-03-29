WAC Color-Shaded Relief

The WAC color-shaded relief map was created from the Global Lunar Digital Terrain Model 100 m pixel scale (GLD100 Scholten et al, 2012; from 79 degrees S latitude to 79 degrees N latitude) and the LOLA 30-m gridded DTM downsampled to 100 meters/pixel (Smith et al., 2010; +/- 79 degrees latitude to the poles). Shading was created by illuminating the surface from a given Sun direction and elevation. To convey an absolute sense of height, the shaded relief grayscale pixels were colored to represent elevation (relative to the mean lunar radius of 1737400 meters). Thirty-four discrete color bins were selected to emphasize topographic landforms. Binning of the elevations for each color was based on a global elevation histogram, with middle elevations having smaller bins (320 m of elevation) and elevation extremes having larger bins (720 m). The color-shaded relief map is available at 100 meters per pixel and 128 pixels per degree in the same tiled format as the GLD100 (eight equirectangular equatorial tiles and a single tile for each pole), as well as in global files at lesser resolutions. All of the color-shaded relief products are available with and without latitude and longitude grids (10 degree increments).

Naming schemes for WAC color-shaded relief products consist of four main parts:
1) WAC_CSHADE: Identifies a product as being a WAC Colorshaded Relief product
2) A 9 Character Identifier:
      - E|P|O for the Projection (E for Equirectangular, P for Polar Stereographic, or O for Orthographic)
      - 3 digits for Center Latitude, in degrees x 10 
      - N or S indicates whether Center Latitude is northern or southern, respectively
      - 4 digits for Center Longitude, in degrees x 10
3) Pixel Scale: 3 digits followed by M for meters/pixel or P for pixels/degree.
4) (Optional) GRID: Indicates that this is a gridded product, with latitude and longitude grids in 10 degree increments.

Latitude and Longitude Ranges:
Product Name |       Projection    | Latitude Range | Longitude Range
-------------|---------------------|----------------|-----------------
E000N1800    |    Equirectangular  |  -90° to 90°   |    0° to 360°
E300N0450    |    Equirectangular  |    0° to 60°   |    0° to 90°
E300N1350    |    Equirectangular  |    0° to 60°   |   90° to 180°
E300N2250    |    Equirectangular  |    0° to 60°   |  180° to 270°     
E300N3150    |    Equirectangular  |    0° to 60°   |  270° to 360°
E300S0450    |    Equirectangular  |  -60° to 0°    |    0° to 360°
E300S1350    |    Equirectangular  |  -60° to 0°    |   90° to 180°
E300S2250    |    Equirectangular  |  -60° to 0°    |  180° to 270°
E300S3150    |    Equirectangular  |  -60° to 0°    |  270° to 360°
O000N0000    |     Orthographic    |  -90° to 90°   |  270° to 90°
O000N0600    |     Orthographic    |  -90° to 90°   |  330° to 120°
O000N1200    |     Orthographic    |  -90° to 90°   |   30° to 210° 
O000N1800    |     Orthographic    |  -90° to 90°   |   90° to 270°
O000N2400    |     Orthographic    |  -90° to 90°   |  150° to 330°
O000N3000    |     Orthographic    |  -90° to 90°   |  210° to 30°
O900N0000    |     Orthographic    |    0° to 90°   |    0° to 360°
O900S0000    |     Orthographic    |  -90° to 0°    |    0° to 360°
P900N0000    | Polar Stereographic |   60° to 90°   |    0° to 360°
P900S0000    | Polar Stereographic |  -90° to 60°   |    0° to 360°

The WAC_CSHADE_LEGEND applies to all the WAC color-shaded relief products. It is a
3-channel RGB TIFF, 8 bits/sample, 3 samples/pixel in single image plane configuration.

Low-resolution 8-bit browse products in PNG format for this product, along with 
full-resolution 8-bit GeoTIFFs, are available at the following location, with 
names matching those of the full-resolution IMG files:
http://lroc.sese.asu.edu/data/LRO-L-LROC-5-RDR-V1.0/LROLRC_2001/EXTRAS/BROWSE/WAC_CSHADE

When citing this product, use the following references:
Scholten, F., Oberst, J., Matz, K. D., Roatsch, T., Wählisch, M., Speyerer, E. J., & Robinson, M. S. (2012). GLD100: The near-global lunar 100 m raster DTM from LROC WAC stereo image data. Journal of Geophysical Research: Planets, 117(E12). https://dx.doi.org/10.1029/2011JE003926

Smith, D. E., Zuber, M. T., Neumann, G. A., Lemoine, F. G., Mazarico, E., Torrence, M. H., McGarrey, J. F., Rowlands, D. D., Head, J. W., Duxbury, T. H., Aharonson, O., Lucey, P. G., Robinson, M. S., Barnouin, O. S., Cavanaugh, J. F., Sun, X., Liiva, P., Mao, D., Smith, J.C., & Bartels, A. E. (2010). Initial observations from the lunar orbiter laser altimeter (LOLA). Geophysical Research Letters, 37(18). https://dx.doi.org/10.1029/2010GL043751

For more information on LROC Reduced Data Records (RDRs), please refer to the
Software Interface Specification (SIS):
http://lroc.sese.asu.edu/data/LRO-L-LROC-5-RDR-V1.0/LROLRC_2001/DOCUMENT/RDRSIS.PDF
