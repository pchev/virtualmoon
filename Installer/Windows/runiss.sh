#!/bin/bash

innosetup='C:\Program Files\Inno Setup 5\ISCC.exe'  # Install under Wine from http://www.jrsoftware.org/isinfo.php

wine "$innosetup" $1
