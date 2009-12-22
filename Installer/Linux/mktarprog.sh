curd=$(pwd)
cd CD; tar czf ../CD_Linux/vma5_full.tgz --owner=root --group=root * 
cd $curd
cd Pro; tar czf ../vmapro5.tgz --owner=root --group=root * 
cd $curd
