# Run this script to update all the translations after modification of a
# resource string in u_translation.pas and compilation of the program
# Update first the path to your Lazarus installation and run "make" in lazarus/tools

#rstconv -i units/i386-linux-gtk2/u_translation.rst -o language/maplun.po
rstconv -i units/x86_64-linux-gtk2/u_translation.rst -o language/maplun.po
/home/compiler/lazarus/tools/updatepofiles language/maplun.po
