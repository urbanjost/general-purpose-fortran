#!/bin/bash
exec 2>&1
for NAME in *.f90
do
   ccall $NAME
   set -x
   xterm -e $HOME/bin/`sr`/$(basename $NAME .f90) &
   set +x
done|tee x.out
