curd=$(pwd)
cd Pro/bin
cp /home/vma/src/virtualmoon/units/i386-linux-gtk2/atlun .
cp /home/vma/src/datlun/units/i386-linux-gtk2/datlun .
cp /home/vma/src/photlun/units/i386-linux-gtk2/photlun .
strip atlun
strip datlun
strip photlun
cd ../share/virtualmoon/language
cp /home/vma/src/virtualmoon/language/maplun.*.po .
cp /home/vma/src/datlun/language/datlun.*.po .
cp /home/vma/src/datlun/language/vmadatabase.*.po .
cp /home/vma/src/photlun/language/photlun.*.po .

cd $curd
cd CD/bin
cp /home/vma/src/virtualmoon/units/i386-linux-gtk2/atlun .
cp /home/vma/src/datlun/units/i386-linux-gtk2/datlun .
cp /home/vma/src/photlun/units/i386-linux-gtk2/photlun .
strip atlun
strip datlun
strip photlun
cd ../share/virtualmoon/language
cp /home/vma/src/virtualmoon/language/maplun.*.po .
cp /home/vma/src/datlun/language/datlun.*.po .
cp /home/vma/src/datlun/language/vmadatabase.*.po .
cp /home/vma/src/photlun/language/photlun.*.po .
