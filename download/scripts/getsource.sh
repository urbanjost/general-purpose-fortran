#!/bin/bash
####################################################################################################################################
banner.sh getsource.sh
GETSOURCE(){
#@(#) expand source archive file from my target location
(cd tmp || exit
   ar xv $HOME/lib/`sr`/JUST4.a
   # rename C source  that confuses makemake(1) because there is a NAME.f90 too
   ls *.c|xargs -iXX mv XX C-XX
)
}
GETSOURCE
####################################################################################################################################
exit
####################################################################################################################################
