#!/bin/sh
cd $(dirname $0)/../tmp/html
for NAME in *.html
do
   NAMESHORT=$(basename $NAME .html)
   html2txt $NAME >../md/$NAMESHORT.md
done
exit
