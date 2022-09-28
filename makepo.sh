#!/bin/bash
# Update the translations in all modules
cd calclun
./makepo.sh
cd -
cd cclun
./makepo.sh
cd -
cd datlun
./makepo.sh
cd -
cd notelun
./makepo.sh
cd -
cd photlun
./makepo.sh
cd -
cd virtualmoon
./makepo.sh
cd -
cd weblun
./makepo.sh
cd -
