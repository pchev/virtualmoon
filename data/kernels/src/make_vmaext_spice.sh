#!/bin/bash

# script to build the Moon extended kernel for VMA with full range of DE440, 1550 to 2650
# detail of the merge operation is in file merge_vmaext.txt

# if not already installed, get the SPICE toolkit
#wget http://naif.jpl.nasa.gov/pub/naif/toolkit//C/PC_Linux_GCC_64bit/packages/cspice.tar.Z
#tar xf cspice.tar.Z
#spkmerge=cspice/exe/spkmerge
#brief=cspice/exe/brief

cd ..
mkdir -p share/virtualmoon/data/kernels
cd share/virtualmoon/data/kernels

rm *.bpc *.tf *.tpc *.tls *.bsp *.tm

wget https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/earth_200101_990825_predict.bpc
wget https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/earth_720101_230601.bpc
wget https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/earth_latest_high_prec.bpc
wget https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/moon_pa_de440_200625.bpc
wget https://naif.jpl.nasa.gov/pub/naif/generic_kernels/fk/satellites/moon_080317.tf
wget https://naif.jpl.nasa.gov/pub/naif/generic_kernels/pck/pck00010.tpc

# get the leap seconds file
wget https://naif.jpl.nasa.gov/pub/naif/generic_kernels/lsk/latest_leapseconds.tls

# get the SPICE earth moon data ~ 114 MB
wget https://naif.jpl.nasa.gov/pub/naif/generic_kernels/spk/planets/de440.bsp

# merge the vma files
rm vma.bsp
spkmerge ../../../../src/merge_vmaext.txt

# check the result
brief vma.bsp

rm de440.bsp

# make earth true equator equinox
cat <<EOF > earth_true_equator_equinox.tf
KPL/FK

   \begintext

    Definition of the Earth True Equator and Equinox of Date frame:

    The earth precession model is the 1976 IAU model.

    The earth nutation model is the 1980 IAU model.

   \begindata
                              
   FRAME_EARTH_TRUE_EQUATOR    =  1539901
   FRAME_1539901_NAME          =  'EARTH_TRUE_EQUATOR'
   FRAME_1539901_CLASS         =  5
   FRAME_1539901_CLASS_ID      =  1539901
   FRAME_1539901_CENTER        =  399
   FRAME_1539901_RELATIVE      = 'J2000'
   FRAME_1539901_DEF_STYLE     = 'PARAMETERIZED'
   FRAME_1539901_FAMILY        = 'TRUE_EQUATOR_AND_EQUINOX_OF_DATE'
   FRAME_1539901_PREC_MODEL    = 'EARTH_IAU_1976'
   FRAME_1539901_NUT_MODEL     = 'EARTH_IAU_1980'
   FRAME_1539901_ROTATION_STATE= 'INERTIAL'
   
   \begintext

   =====================================================================
   End of kernel
EOF

# meta kernel for VMA
cat <<EOF > vma.tm
\begintext 

   List of SPICE kernels required by VMA

\begindata 

PATH_VALUES  = ( '', '' )

PATH_SYMBOLS = ( 'A', 'B' )

KERNELS_TO_LOAD = ('\$A/earth_true_equator_equinox.tf',
                   '\$A/moon_080317.tf',
                   '\$A/pck00010.tpc',
                   '\$B/latest_leapseconds.tls',
                   '\$A/earth_200101_990825_predict.bpc',
                   '\$A/earth_720101_230601.bpc',      
                   '\$B/earth_latest_high_prec.bpc',
                   '\$A/moon_pa_de440_200625.bpc',
                   '\$A/vma.bsp')
EOF

cd ../../../..
tar cvzf Ext_Kernels.tgz share
