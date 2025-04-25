# Copy the translation from download to source tree

cd download

for f in $(ls -1 virtualmoon/*.po)
 do 
 fg="${f/virtualmoon\//}"
 fg="${fg/-/.}"
 echo cp $f ../../$fg
 cp $f ../../$fg
 done
