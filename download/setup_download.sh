#!/bin/sh
####################################################################################################################################
#(@)# process doc files and convert to man(1) pages, html documents, ...
#set -x
####################################################################################################################################
(
exec 2>&1
date

DIRNAME=`dirname $0`
cd $DIRNAME
export DIRNAME=`pwd`

export PUTMAN=$DIRNAME/tmp/man
export PUTHTML=$DIRNAME/tmp/html
export MANPATH=$DIRNAME/tmp/man

PATH=$PATH:$DIRNAME/scripts

source $DIRNAME/scripts/functions.sh
####################################################################################################################################
#
#   Assuming most users would simply like the .f90 files, and not have
#   to install ufpp, what, goodbad, ccall, html2f90, html2man, and the
#   other components of the programming environment take the source archive
#   created by make.shell for the libjust4.a library and add a test program
#   and the "c" and "juown1" procedures required by the calculator module
#   and place them in a scratch directory and use makemake(1) to make a
#   simple make(1) file. Then put all that back into an archive file and
#   compress it for easy download from browsers.
####################################################################################################################################
#   Tools to help create man(1) pages
#    html2man  -- GNU commands take --help switch and then convert this to a man(1) page
#    txt2man   -- perl script converts text files following specified markdown rules into a man(1) page
#    manserver --
####################################################################################################################################
#  Unexpected
#  When changed NAME line in man(1) pages to format
#   M_modulename::topic - description
#  The mandb output looks to have been made from
#   filename (section) - description
#  As M_modulename:: does not show in a
#    man -k .
#  but
#    man -k M_modulename
#  shows the correct sections so the original data is in the database.
#  could name files M_modulename_topic but then "K" would not work in vim(1), for example.
#  apparently will have to use
#    topic - [M_modulename] description
####################################################################################################################################
#
#    XX                                      X
#   X                               X
#   X                               X
#  XXXX   XX  XX  XX XX    XXXXX   XXXX    XXX     XXXXX  XX XX    XXXXX
#   X      X   X   XX  X  X     X   X        X    X     X  XX  X  X     X
#   X      X   X   X   X  X         X        X    X     X  X   X   XXX
#   X      X   X   X   X  X         X        X    X     X  X   X      XX
#   X      X  XX   X   X  X     X   X  X     X    X     X  X   X  X     X
#  XXXX     XX XX XXX XXX  XXXXX     XX    XXXXX   XXXXX  XXX XXX  XXXXX
#
####################################################################################################################################
MAN2MAN(){

#echo 'MAN2MAN: take a *.man file and make man(1) page using txt2man(1) in tmp/man and HTML version of man(1) page in tmp/html'

SECTION=$1

# convert *.man file to roff and then to html


# convert to roff and install and convert to html and install

echo "MAN2MAN: $NAME.$SECTION.man ==> $PUTMAN/man$SECTION_NUMBER/$NAME.$SECTION_NUMBER.gz"
echo "MAN2MAN: $NAME.$SECTION.man ==> $PUTHTML/$NAME.$SECTION.html"
export AUX_FILENAME="$NAME.$SECTION"
txt2man -s $SECTION_NAME -t "$NAME" $NAME.$SECTION.man|
   tee $PUTMAN/man$SECTION_NUMBER/$NAME.$SECTION_NUMBER|
   man2html >$PUTHTML/$NAME.$SECTION_NUMBER.html

gzip --force $PUTMAN/man$SECTION_NUMBER/$NAME.$SECTION_NUMBER

chmod a=r,u+w $PUTMAN/man$SECTION_NUMBER/$NAME.$SECTION_NUMBER.gz

}
####################################################################################################################################
GETSECTION(){
# function to get section number from a document file
#echo "GETSECTION $NAME $SUFFIX" 1>&2
NAME=$1
SUFFIX=$2
   SHORTNAME=$(BASENAME $NAME $SUFFIX)
   SECTION=${SHORTNAME/*./}
   SECTION=${SECTION:-'7l'}
   SECTION_NUMBER=$(echo "$SECTION"|sed -e 's/[^0-9]*//g')
   SECTION_NUMBER=${SECTION_NUMBER:-'7'}
   SECTION_NAME=${SECTION_NUMBER}$(echo "$SECTION"|sed -e 's/^[0-9]*//')
}
####################################################################################################################################
# convert *.man files to make man(1) pages in tmp/man and HTML versions of man(1) pages in tmp/html
txt2man -h >doc/txt2man.1.man
####################################################################################################################################
MAN_TO_ROFF(){
(
for NAME in ${*:-*.[1-8]*.man}
do
   echo $NAME
   GETSECTION $NAME .man
   NAME=$(BASENAME $SHORTNAME .$SECTION)
   MAN2MAN $SECTION
done
)
}
####################################################################################################################################
# make sure directories exist for man pages and convert files to man(1) pages
MAKEMANDIR(){
# Order files will be processed
# *.htm   assumed to look like a man(1) page convert to flat txt and then run thru txt2man like a *.man file
# *.man   text files that look like a man(1) page that are to be run thru txt2man to make a *roff file.
# *.txt   if a line starts with a "." these are assumed to be *roff file files and go straight to  man page
# *.txt   if no line starts with a "." surround with .nf and .fi to make a primitive man(1) page
# *.htm   assumed to need a header and footer make into an HTML document and OVERWRITE file made from man(1) page
# *.html  copied as-is only to HTML directory, OVERWRITING anything files made by previous steps
# *.md

   pwd
   cd doc && (
      rm -rfv tmp
      mkdir -p tmp images
   )
#------------------------------------------------------------------------------#
echo 'Take *.htm pages and convert them to text *.man pages in tmp/'
mkdir -p ../tmp/html
for NAME in ${*:-*.[1-8]*.htm}
do
   NEWNAME=$(BASENAME $NAME .htm)
   echo "processing .htm file $NAME to tmp/$NEWNAME.man"
   (
   header.sh
   cat $NAME
   footer.sh
   ) | html2txt $NAME tmp/$NEWNAME.man
done
#------------------------------------------------------------------------------#
echo 'Convert *.man pages to *roff files and install as man pages'
(cd $DIRNAME/doc;MAN_TO_ROFF)
(cd $DIRNAME/doc/tmp;MAN_TO_ROFF)
#------------------------------------------------------------------------------#
echo 'Take *.[1-8]*.txt man pages and make into as-is man(1) pages and HTML pages'
for NAME in ${*:-*.[1-8]*.txt}
do
   export NAMEMAN=$(BASENAME $NAME .txt)
   export NAMEHTML=$NAMEMAN.html
   (
      # assume if a line starts with "." that already *roff
      if grep -q '^\.' $NAME
      then
              echo "copy man(1) page $NAME" 1>&2
         cat $NAME
      else
         echo 'make flat text $NAME into basic man page' 1>&2
         # assume flat text
         echo '.nf'
         cat $NAME|sed -e 's@\\@\\\\@g'
         echo '.fi'
      fi
   ) >tmp/$NAME
   echo "install $NAME as man page" 1>&2
   ccall tmp/$NAME
   rm -f tmp/$NAME
   echo "convert man page $NAME to html" 1>&2
   manserver $NAME >$PUTHTML/$NAMEHTML
done
#------------------------------------------------------------------------------#
echo 'Take *.htm pages and copy into tmp/html adding a header and footer'
echo 'done last to override what was made from man(1) pages'
mkdir -p ../tmp/html
for NAME in ${*:-*.[1-8]*.htm}
do
   NEWNAME=$(BASENAME $NAME .htm)
   echo "processing .htm file $NAME to $NEWNAME.html"
   (
   header.sh
   cat $NAME
   footer.sh
   ) > $PUTHTML/$NEWNAME.html
done
#------------------------------------------------------------------------------#
echo 'Take *.html pages and copy into $PUTHTML as-is'
echo 'Take *.html pages and make man(1) page if one has not been made of same name'
for NAME in ${*:-*.[1-8]*.html}
do
   echo "$NAME to $PUTHTML"
   cp $NAME $PUTHTML
   GETSECTION $NAME .html
   # if no man page of this name exists make a plain text one, assuming not
   # suitable for formatting (or it would be a .htm page)
   # If it does not have a NAME line at the top it will cause problems with
   # manb(1). If it does not a ".nf" line in it after that, it will be
   # formatted, which may or may not work.
   if [ ! -r "$PUTMAN/man$SECTION_NUMBER/$SHORTNAME.*" ]
   then
      html2txt $NAME >tmp/$SHORTNAME.txt
      ccall tmp/$SHORTNAME.txt
   fi
done
}
####################################################################################################################################
BLANKOUT(){
rm -rf   tmp/*             # ensure scratch directory is empty
mkdir -p tmp/PROGRAMS      # make sure scratch directories exist
mkdir -p tmp/html
mkdir -p tmp/scripts/
for NUMBER in 1 2 3 4 5 6 7 8
do
   mkdir -p tmp/man/man$NUMBER
   mkdir -p tmp/man/man${NUMBER}f
   #break
   #continue
done
}
####################################################################################################################################
#
#                                                     XX
#                                                      X
#                                                      X
#  XXXXX   XXXXX  XXX X   XXX X    XXXX   XX XX    XXXXX   XXXXX
# X     X X     X  X X X   X X X       X   XX  X  X    X  X     X
# X       X     X  X X X   X X X   XXXXX   X   X  X    X   XXX
# X       X     X  X X X   X X X  X    X   X   X  X    X      XX
# X     X X     X  X X X   X X X  X    X   X   X  X    X  X     X
#  XXXXX   XXXXX  XX X XX XX X XX  XXXX X XXX XXX  XXXXXX  XXXXX
#
####################################################################################################################################
cat <<EOF
================================================================================
DIRNAME ....... $DIRNAME
PUTMAN ........ $PUTMAN
================================================================================
EOF
####################################################################################################################################
BLANKOUT                # make empty tmp directory, removing previous output files
getsource.sh            # expand source archive file from my target location
supplemental_source.sh  # create user-supplied procedures "c" and "juown1" required for the calculator module
getprograms.sh          # get some programs to add to the procedures
makemakefile.sh         # create makefile
####################################################################################################################################
echo 'copy permanent document repository to tmp area to create tar file from'
cp -r -p doc tmp/
####################################################################################################################################
echo 'put a few scripts into the tar file that might be of interest'
for NAME in \
   txt2man    \
   man2html   \
   manserver  \
   goodbad    \
   mank       \
   makemake   \
   manvi      \
   $NULL
do
   cp `which $NAME`  tmp/scripts/
done
cp -r $(dirname $(which manserver) )/manserver_dir/ tmp/scripts/
cp /home/urbanjs/.twm/scripts_vi/vimrc tmp/
cp /home/urbanjs/.twm/scripts_vi/exrc tmp/
####################################################################################################################################
echo 'create documents in tmp/doc, tmp/man, and tmp/html'
(MAKEMANDIR)
build.apropos
mank.sh           # make html index pages of the man pages
####################################################################################################################################
# extract test programs for M_pixel module and run them to test man(1) pages
# and generate GIF images for HTML versions of the man page, which man2html(1)
# always added to the end of a man(1) page so HTML documents include documents,
# which have not been able to reliably get from utilities converting markup to
# man(1) pages, info(1) pages  and HTML.
(
   cd $DIRNAME
   cd doc/images/
   banner 'test pixel'
   env DISPLAY= test_M_pixel_manpages.sh
)
#----------------------------------------------------------------------------------------------------------------------------------#
:  copy pixmap files needed by documentation
(
cd $DIRNAME
mkdir -p tmp/html/images
cp -p doc/images/* tmp/html/images/
cp -p ../html/icons/* tmp/html/images/
)
#----------------------------------------------------------------------------------------------------------------------------------#
(
cd $DIRNAME
mkdir -p tmp/html/
cp -r -p ../html/StyleSheets tmp/html/
)
####################################################################################################################################
# combine man pages into books
book.sh M_kracken M_strings M_time M_system M_color M_pixel M_calculator M_units M_math M_process M_logic 
book.sh M_Compare_Float_Numbers M_debug M_factor M_io M_journal M_messages M_sort INTRINSIC
book.sh INDEX
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'now that all procedure descriptions are in place make main index page download.html'
make_index.sh
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'build index of source file metadata using what(1) command'
(
   cd tmp
   header.sh
   what $(find . -path './*/*' -prune -o -type f -printf '%P '|xargs -n 1|sort -t . -k 2r,2r -k 1,1|xargs) -html
   footer.sh
) >tmp/source.html
#----------------------------------------------------------------------------------------------------------------------------------#
echo 'build index of program file metadata using what(1) command'
(
   header.sh
   cd tmp/PROGRAMS
   what $(find . -path './*/*' -prune -o -type f -printf '../PROGRAMS/%P ') -html|
   sed -n -e '\%^ *<td>%,\%^ *</td>%{
s/^/ZZZZ:/
s/ZZZZ:.*DESCRIPTION://
/ZZZZ:/d
s/^/<td>/
s/$/<\/td>/
#\%^ *DESCRIPTION:.*%{!d;p;n}
}' -e p
   footer.sh
) > tmp/html/programs.html
####################################################################################################################################
echo 'create tar file for downloading'
(cd tmp;tar cvfz ../just4.tgz *)
#----------------------------------------------------------------------------------------------------------------------------------#
doxygen.sh
#----------------------------------------------------------------------------------------------------------------------------------#
date
)|tee LOG.setup_download.txt
####################################################################################################################################
exit
####################################################################################################################################
