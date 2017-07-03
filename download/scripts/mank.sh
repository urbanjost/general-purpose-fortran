#!/bin/bash
####################################################################################################################################
#@(#) mank(1) - build table of contents of man pages as an html page.
banner.sh mank.sh
# uses footer.sh and header.sh
####################################################################################################################################
RANDOMCOLOR(){
# generate a random light background color 
# so each grouping has a color in the table
printf -v KOLOR '#%2.2x%2.2x%2.2x' $((RANDOM % 64+3*64)) $((RANDOM % 64+3*64)) $((RANDOM % 64+3*64))
}
####################################################################################################################################
MANK(){
SECTION=${1:-3}
#----------------------------------------------------------------------------------------------------------------------------------#
header.sh # create header for document including CSS style
#----------------------------------------------------------------------------------------------------------------------------------#
echo "<h1>$DOCUMENT_HEADER</h1>"
#----------------------------------------------------------------------------------------------------------------------------------#
cat <<\EOF
<table border="1">
<tr> <th>grouping</th> <th>page</th> <th>description</th> </tr>
EOF
#----------------------------------------------------------------------------------------------------------------------------------#
export OLDGROUP GROUP KOLOR
OLDGROUP='&nbsp;'
RANDOMCOLOR
#----------------------------------------------------------------------------------------------------------------------------------#
#
# sort by 'member' assuming making lines for a module member using this syntax:
#    NAME (section) - [member] description
# example:
#    M_Compare_Float_Numbers (3) - [M_Compare_Float_Numbers]perform relational comparisons on real numbers
# but still work with regular lines like
#    _pwd (1)        - list full pathname of current directory
#
DELIMITER=$(printf '\t')
mank -S $SECTION -k . |
   eval $FILTER |
   sed -e "s/\[/$DELIMITER[/" |
   sed -e "s/]/]$DELIMITER/" |
   sort -s -t "$DELIMITER" -k 2,2 -k 1,1 |
   tr -d "$DELIMITER" |
while read NAME SECT DASH OTHER
do
   GROUP=${OTHER/\]*/} GROUP=${GROUP/*\[/} GROUP=${GROUP:-'&nbsp;'}
   if [ "$OLDGROUP" != "$GROUP" ]
   then
      OLDGROUP="$GROUP"
      RANDOMCOLOR
   fi
   
   # will truncate description if description has ] not as I expect
   case "$OTHER" in
   *\]*) OTHER=${OTHER/*\]/} ;;
   *) GROUP='&nbsp;' ;;
   esac

   echo "<tr><td style=\"background:$KOLOR;\">$GROUP</td><td><a href=\"$NAME.$SECTION.html\">$NAME</a></td><td>$OTHER</td></tr>"
done
#----------------------------------------------------------------------------------------------------------------------------------#
cat <<\EOF
</table>
EOF
#----------------------------------------------------------------------------------------------------------------------------------#
footer.sh
}
####################################################################################################################################
cd $(dirname $0)
cd ..
PATH=$PATH:$(pwd)/scripts

export FILTER="fgrep -v '[INTRINSIC'"
for SUBDIR in 1 2 3 4 5 6 7 8
do
   echo "making HTML index for section $SUBDIR in tmp/html/man${SUBDIR}.html"
   DOCUMENT_HEADER="libjust4 man(${SUBDIR}) pages"
   MANK "${SUBDIR}" > tmp/html/man${SUBDIR}.html &
done
export FILTER="fgrep '[INTRINSIC'"
for SUBDIR in 3 
do
   echo "making HTML index for section $SUBDIR in tmp/html/man${SUBDIR}i.html"
   DOCUMENT_HEADER="libjust4 man(${SUBDIR}) pages"
   MANK "${SUBDIR}" > tmp/html/man${SUBDIR}i.html &
done
wait
####################################################################################################################################
exit
####################################################################################################################################
