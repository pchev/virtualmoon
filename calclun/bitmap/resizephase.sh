#!/bin/bash

cd 500

ls m*.png | xargs -i+ -n1 convert +  -resize "22x22" ../22/+
