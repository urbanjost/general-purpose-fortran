program xpand
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit, stderr=>error_unit
use M_CLI2,    only : set_args, iget, lget, files=>unnamed
use M_strings, only : dilate, upper, lower, visible, msg
use M_io,      only : getline
implicit none

! ident_1="@(#) xpand(1f) filter removes tabs trailing white space adjacent blank lines exposes non-printable characters"

character(len=:),allocatable  :: line
character(len=:),allocatable  :: out
integer                       :: ios          ! error flag from read
integer                       :: i,ii
integer                       :: iblanks
integer                       :: blanks
integer                       :: width
integer                       :: countlines
character(len=:),allocatable  :: help_text(:)
character(len=:),allocatable  :: version_text(:)
logical                       :: verbose
logical                       :: uppercase
logical                       :: lowercase
logical                       :: number
logical                       :: numbernonblank
logical                       :: isvisible
logical                       :: showend
logical                       :: expandtabs
character(len=*),parameter    :: g='(*(g0))'
character(len=*),parameter    :: g1='(*(g0,1x))'
integer                       :: icount

   call setup()
   call set_args( '&
   &  --blanks:b -1 --width:w 132 &
   &  --lowercase:l F --uppercase:U F &
   &  --number:n F --number-nonblank:N F &
   &  --show-nonprinting:c F --show-ends:e F --expand-tabs:x &
   &',help_text,version_text)
   width=iget('width')
   blanks=iget('blanks')
   verbose=lget('verbose')
   uppercase=lget('uppercase')
   lowercase=lget('lowercase')
   isvisible=lget('show-nonprinting')
   showend=lget('show-ends')
   expandtabs=lget('expand-tabs')
   number=lget('number')
   numbernonblank=lget('number-nonblank')
   icount=0

   if(size(files).eq.0)then                                ! default is to read from stdin, which the filename "-" designates
      files=['-']
   endif

   ALLFILES: do i=1,size(files)                            ! loop through all the filenames

      if(verbose)write(stderr,g)'FILE:',i,':NAME:',trim(files(i))
      if(files(i).eq.'-'.or.files(i).eq.'stdin')then       ! special filename designates stdin
         ii=stdin
      else                                                 ! open a regular file
         ii=20
         open(unit=ii,file=trim(files(i)),iostat=ios,status='old',form='formatted')
         if(ios.ne.0)then
             write(stderr,g) '*xpand* failed to open:',trim(files(i))
            cycle ALLFILES
         endif
      endif
      iblanks=0
      open(unit=ii,pad='yes')
      countlines=0
      ALLINES: do while (getline(line,ii)==0)
         icount=icount + 1
         if(line.eq.'')then
            if(numbernonblank)icount=icount-1
         elseif(uppercase)then
            line=upper(line)
         elseif(lowercase)then
            line=lower(line)
         endif
         if(expandtabs) line=dilate(line)
         if(isvisible)  line=visible(line)
         if(showend)    line=line//'$'
         if(blanks.ge.0)then
            if(line.eq.'')then
               iblanks=iblanks+1
               if(iblanks.gt.blanks)then
                  cycle ALLINES
               endif
            else
               iblanks=0
            endif
         endif
         if(len(line).gt.width)then
            write(stderr,g) trim(files(i)),':L:',icount,':C:',len(line),':',line
         endif
         if(number)then
            write(*,"(i6.0,1x,a)")icount,line
         elseif(numbernonblank.and.line.ne.'')then
            write(*,"(i6.0,1x,a)")icount,line
         else
            write(*,"(a)")line
         endif
      enddo ALLINES
      close(unit=ii,iostat=ios)
   enddo ALLFILES
contains

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   xpand(1f) - [FUNIX] expand tabs,remove trailing white space and',&
'   (optionally)  adjacent blank lines                             ',&
'                                                                  ',&
'SYNOPSIS                                                          ',&
'    xpand [ -blanks NNN][ --width ] FILENAME(S)                   ',&
'                                                                  ',&
'DESCRIPTION                                                       ',&
'   Defaults to converting tabs in each FILE to spaces, writing to ',&
'   standard output. If no filename is specified standard input is ',&
'   read. Tab stops are assumed to be every eight (8) columns. Trailing',&
'   spaces, carriage returns, and newlines are removed.                ',&
'                                                                      ',&
'OPTIONS                                                               ',&
'   FILENAMES    files to expand tab characters in.                    ',&
'   --width,-w   line width at which to produce a warning if exceeded. ',&
'                If less then or equal to 0 no warnings are produced. The',&
'                default is 0. Warning messages appear on stderr.        ',&
'   --blanks,-b  maximum number of adjacent blank lines to retain.       ',&
'                Default is -1, which is equivalent to unlimited.        ',&
'                                                                        ',&
'   --show-nonprinting,-c   use ^ and M- notation, except for linefeed   ',&
'   --show-ends,-e          display $ at end of each line                ',&
'                                                                        ',&
'   --number,-n          number all output lines                         ',&
'   --number-nonblank,-N number nonempty output lines, overrides -n      ',&
'                                                                        ',&
'   --uppercase,-U   convert all lowercase ASCII characters to uppercase ',&
'   --lowercase,-l   convert all uppercase ASCII characters to lowercase ',&
'                                                                        ',&
'STANDARD OPTIONS                                                        ',&
'   --help      display this help and exit                               ',&
'   --version   output version information and exit                      ',&
'   --usage     basic usage information including a list of arguments    ',&
'   --verbose   verbose mode                                             ',&
'                                                                        ',&
'EXAMPLES                                                                ',&
'   Sample commands:                                                     ',&
'                                                                        ',&
'     xpand < input.txt > output.txt                                     ',&
'     xpand input.txt   > output.txt                                     ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PROGRAM:      xpand(1f)',&
'DESCRIPTION:  convert tabs to spaces',&
'AUTHOR:       John S. Urban         ',&
'VERSION:      1.0.0, 20151220       ',&
'AUTHOR:       John S. Urban         ',&
'VERSION:      1.1.0, 20220626 -- allow filenames, width warning',&
'LICENSE:      MIT                                              ',&
'']
end subroutine setup

end program xpand
