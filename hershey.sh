#!/bin/bash
#@(#) Makes the binary Hershey font file for M_DRAW
#
FONTLIB=/usr/share/hershey
FONTLIB=./lib/hershey
BINDIR=$(dirname $0)/../PROGRAMS/MAKE_HERSHEY
PATH=$PATH:$BINDIR

# uses ccall script to compile M_DRAW executables using machine-specific options
#
if test ! -d $FONTLIB
then 
   mkdir -p $FONTLIB
   chmod a+xr $FONTLIB
fi
(
sed -e 's/#.*//' <<\EOF
  data/hersh.oc  fonts/astrol.hmp    astrology
  data/hersh.oc  fonts/scripts.hmp   cursive
  data/hersh.oc  fonts/cyrilc.hmp    cyrillic
  data/hersh.oc  fonts/romans.hmp    futura.l
  data/hersh.oc  fonts/romand.hmp    futura.m
  data/hersh.oc  fonts/gotheng.hmp   gothic.eng
  data/hersh.oc  fonts/gothger.hmp   gothic.ger
  data/hersh.oc  fonts/gothita.hmp   gothic.ita
  data/hersh.oc  fonts/greeks.hmp    greek
  data/hersh.or  fonts/japan.hmp     japanese
  data/hersh.oc  fonts/marker.hmp    markers
  data/hersh.oc  fonts/marker2.hmp   markers      #  replace  default  marker  font
  data/hersh.oc  fonts/lowmat.hmp    math.low
  data/hersh.oc  fonts/uppmat.hmp    math.upp
  data/hersh.oc  fonts/meteo.hmp     meteorology
  data/hersh.oc  fonts/music.hmp     music
  data/hersh.oc  fonts/scriptc.hmp   script
  data/hersh.oc  fonts/symbol.hmp    symbolic
  data/hersh.oc  fonts/greekc.hmp    times.g
  data/hersh.or  fonts/orall_aa.hmp  orall_aa
  data/hersh.or  fonts/orall_ac.hmp  orall_ac
  data/hersh.or  fonts/orall_ae.hmp  orall_ae
  data/hersh.or  fonts/orall_ag.hmp  orall_ag
  data/hersh.or  fonts/orall_ab.hmp  orall_ab
  data/hersh.or  fonts/orall_ad.hmp  orall_ad
  data/hersh.or  fonts/orall_af.hmp  orall_af
  data/hersh.or  fonts/orall_ah.hmp  orall_ah
  data/hersh.or  fonts/orall_ai.hmp  orall_ai
  data/hersh.oc  fonts/italicc.hmp   times.i
  data/hersh.oc  fonts/italict.hmp   times.ib
  data/hersh.oc  fonts/romanc.hmp    times.r
  data/hersh.oc  fonts/romant.hmp    times.rb
EOF
)| while read DATA INPUT OUTPUT
do
   h2v $DATA  $INPUT  $FONTLIB/$OUTPUT
   chmod a+r $FONTLIB/$OUTPUT
done 

exit
