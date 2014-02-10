#!/bin/bash

curd=$(pwd)
lg=$1 

cd $lg; tar czf ../../lang_$lg.tgz  --owner=root --group=root *

cd $curd
