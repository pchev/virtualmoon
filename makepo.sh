#!/bin/bash
# Update the translations in all modules
cd calclun
pwd
./makepo.sh
cd - >/dev/null
cd cclun
pwd
./makepo.sh
cd - >/dev/null
cd datlun
pwd
./makepo.sh
cd - >/dev/null
cd notelun
pwd
./makepo.sh
cd - >/dev/null
cd photlun
pwd
./makepo.sh
cd - >/dev/null
cd virtualmoon
pwd
./makepo.sh
cd - >/dev/null
cd weblun
pwd
./makepo.sh
cd - >/dev/null
