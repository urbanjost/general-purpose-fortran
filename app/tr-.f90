!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program tr
!-----------------------------------------------------------------------------------------------------------------------------------
use M_kracken, only : kracken, lget, sget, iget                 ! add command-line parser module
use M_strings, only : transliterate
use M_io, only : read_line
implicit none
character(len=:),allocatable :: old
character(len=:),allocatable :: new
character(len=:),allocatable :: line
logical                      :: up
logical                      :: low
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('tr','-o -n -l .F. -u .F. -help .F. -version .F.') ! define command arguments,default values and crack command line
   call help_usage(lget('tr_help'))                             ! if -help option is present, display help text and exit
   call help_version(lget('tr_version'))                        ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
   old = trim(sget('tr_o'))                                     ! get -o STRING
   new = trim(sget('tr_n'))                                     ! get -n STRING
   low=lget('tr_l')
   up=lget('tr_u')
!-----------------------------------------------------------------------------------------------------------------------------------
   INFINITE: do while (read_line(line)==0)
      line=transliterate(line,old,new)
      if(low) line=transliterate(line,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')
      if(up)  line=transliterate(line,'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ')
      write(*,'(a)') line
   enddo INFINITE
contains
subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                                                                            ',&
'   tr-(1) - [FUNIX:M_strings] translate or delete characters                                                                    ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'       tr- [ -o SET1 [ -n SET2 ]]|-l| -u |[ --version ]|[ --help ]                                                              ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Translate, squeeze, and/or delete characters from standard input, writing to standard output.                                ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   -o SET1,-n SET2  old set of characters and new set of characters to replace the old set.                                     ',&
'                                                                                                                                ',&
'              o  Each character in the input string that matches a character                                                    ',&
'                 in the old set is replaced.                                                                                    ',&
'                                                                                                                                ',&
'              o  If the new set is the empty set the matched characters                                                         ',&
'                 are deleted.                                                                                                   ',&
'                                                                                                                                ',&
'              o  If the new set is shorter than the old set the last character                                                  ',&
'                 in the new set is used to replace the remaining                                                                ',&
'                 characters in the new set.                                                                                     ',&
'   -u         convert to uppercase                                                                                              ',&
'   -l         convert to lowercase                                                                                              ',&
'   --help     display this help and exit                                                                                        ',&
'   --version  output version information and exit                                                                               ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'  Sample usage                                                                                                                  ',&
'                                                                                                                                ',&
'   #convert input to uppercase:                                                                                                 ',&
'   tr- -u                                                                                                                       ',&
'   tr- -o ''abcdefghijklmnopqrstuvwxyz'' -n ''ABCDEFGHIJKLMNOPQRSTUVWXYZ''                                                      ',&
'                                                                                                                                ',&
'   # out of  !"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~                    ',&
'   # just leave letters                                                                                                         ',&
'   _ tr -o ''!"#$%&''"''"''()*+,-./0123456789:;<=>?@[\]^_`{|}~''                                                                ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    tr-(1) - [FUNIX:M_strings] translate or delete characters
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        tr- [ -o SET1 [ -n SET2 ]]|-l| -u |[ --version ]|[ --help ]
!!
!!##DESCRIPTION
!!    Translate, squeeze, and/or delete characters from standard input, writing to standard output.
!!
!!##OPTIONS
!!    -o SET1,-n SET2  old set of characters and new set of characters to replace the old set.
!!
!!               o  Each character in the input string that matches a character
!!                  in the old set is replaced.
!!
!!               o  If the new set is the empty set the matched characters
!!                  are deleted.
!!
!!               o  If the new set is shorter than the old set the last character
!!                  in the new set is used to replace the remaining
!!                  characters in the new set.
!!    -u         convert to uppercase
!!    -l         convert to lowercase
!!    --help     display this help and exit
!!    --version  output version information and exit
!!
!!##EXAMPLES
!!
!!   Sample usage
!!
!!    #convert input to uppercase:
!!    tr- -u
!!    tr- -o 'abcdefghijklmnopqrstuvwxyz' -n 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
!!
!!    # out of  !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
!!    # just leave letters
!!    _ tr -o '!"#$%&'"'"'()*+,-./0123456789:;<=>?@[\]^_`{|}~'
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        tr-(1)>',&
'@(#)DESCRIPTION:    translate one set of characters to another>',&
'@(#)VERSION:        1.0, 20190828>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2021-12-18 15:29:22 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program tr
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
