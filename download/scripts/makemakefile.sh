#!/bin/bash
#@(#) run makemake(1) to make Makefile for this collection
banner makemakefile.sh
MAKEMAKEFILE(){
#  create basic makefile for plain .f90 files that compose libjust4.a
#  and create a compressed archive file for simple downloads on the
#  Internet
(
cd tmp
    SANI='fsanitize=address,null,undefined'
    SANI=
env MAKEMAKE_LDFLAGS="-fno-range-check -Wall -fbounds-check -g -I. -J. $SANI" \
    MAKEMAKE_F90FLAGS="-Wall -Wuninitialized -fbounds-check -fno-range-check -g -I. -J. $SANI" \
    MAKEMAKE_F90='gfortran' \
    MAKEMAKE_LIBS='-lncurses -lreadline' \
    makemake "`find PROGRAMS -type f|xargs`"
mv Makefile Makefile_picky
env MAKEMAKE_F90='gfortran' \
    MAKEMAKE_LIBS='-lncurses -lreadline' \
    makemake "`find PROGRAMS -type f|xargs`"
)
}
MAKEMAKEFILE
