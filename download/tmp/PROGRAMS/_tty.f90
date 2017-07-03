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
'NAME                                                                            ',&
'       _tty - [FUNIX]print information about the file/terminal connected to standard input',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       _tty [OPTION]...                                                         ',&
'DESCRIPTION                                                                     ',&
'       Call INQUIRE(3f) and print information about the                         ',&
'      file name of the terminal connected to standard input.                    ',&
'      The results are dependent on the programming environment                  ',&
'      used, as some of the behavior is system dependent.                        ',&
'OPTIONS                                                                         ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'      The standard leaves quite a bit as system dependent. It is always         ',&
'      interesting. For example, on CygWin using gfortran V5.4                   ',&
'                                                                                ',&
'      If I just type in the command at a prompt the INPUT_UNIT                  ',&
'      of 5 is returned as unit 0. But if I call the _tty from                   ',&
'      within vim(1) it is returned as 5.                                        ',&
'                                                                                ',&
'        _tty                                                                    ',&
'        > for INPUT_UNIT=5                                                      ',&
'        >====================================================                   ',&
'        >*print_inquire* checking file:/dev/pty10                               ',&
'        >*print_inquire* file exists                                            ',&
'        >*print_inquire* using unit number  0                                   ',&
'        >*print_inquire* access type SEQUENTIAL,FORMATTED                       ',&
'                                                                                ',&
'      Even though the file is not a terminal but exists                         ',&
'      I get a name of "stdin". Other compilers return a                         ',&
'      null filename, others return the name of the redirected                   ',&
'      file. What if there was actually a file called "stdin"?                   ',&
'                                                                                ',&
'        _tty </dev/null                                                         ',&
'        > for INPUT_UNIT=5                                                      ',&
'        >====================================================                   ',&
'        >*print_inquire* checking file:stdin                                    ',&
'        >*print_inquire* file does not exist                                    ',&
'                                                                                ',&
'      Try these:                                                                ',&
'                                                                                ',&
'        _tty # a simple call from the keyboard                                  ',&
'        _tty </tmp/notthere # a file assumed to not exist                       ',&
'        _tty </dev/null   # a special file that exists                          ',&
'        _tty </etc/passwd # some non-terminal file that exists                  ',&
'        _tty < `tty`      # reading the pathname of your terminal               ',&
'        _tty < /dev/pty08 # some TTY not assigned to your ID                    ',&
'                                                                                ',&
'REPORTING BUGS                                                                  ',&
'    Report _yes bugs to <http://www.urbanjost.altervista.org/index.html>        ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'      isatty(3c),tty(1), ttyname(3)                                             ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        _tty - [FUNIX]print information about the file/terminal connected to standard input
!!
!!##SYNOPSIS
!!
!!        _tty [OPTION]...
!!##DESCRIPTION
!!        Call INQUIRE(3f) and print information about the
!!       file name of the terminal connected to standard input.
!!       The results are dependent on the programming environment
!!       used, as some of the behavior is system dependent.
!!##OPTIONS
!!        --help     display this help and exit
!!        --version  output version information and exit
!!
!!##EXAMPLE
!!
!!       The standard leaves quite a bit as system dependent. It is always
!!       interesting. For example, on CygWin using gfortran V5.4
!!
!!       If I just type in the command at a prompt the INPUT_UNIT
!!       of 5 is returned as unit 0. But if I call the _tty from
!!       within vim(1) it is returned as 5.
!!
!!         _tty
!!         > for INPUT_UNIT=5
!!         >====================================================
!!         >*print_inquire* checking file:/dev/pty10
!!         >*print_inquire* file exists
!!         >*print_inquire* using unit number  0
!!         >*print_inquire* access type SEQUENTIAL,FORMATTED
!!
!!       Even though the file is not a terminal but exists
!!       I get a name of "stdin". Other compilers return a
!!       null filename, others return the name of the redirected
!!       file. What if there was actually a file called "stdin"?
!!
!!         _tty </dev/null
!!         > for INPUT_UNIT=5
!!         >====================================================
!!         >*print_inquire* checking file:stdin
!!         >*print_inquire* file does not exist
!!
!!       Try these:
!!
!!         _tty # a simple call from the keyboard
!!         _tty </tmp/notthere # a file assumed to not exist
!!         _tty </dev/null   # a special file that exists
!!         _tty </etc/passwd # some non-terminal file that exists
!!         _tty < `tty`      # reading the pathname of your terminal
!!         _tty < /dev/pty08 # some TTY not assigned to your ID
!!
!!##REPORTING BUGS
!!     Report _yes bugs to <http://www.urbanjost.altervista.org/index.html>
!!
!!##SEE ALSO
!!       isatty(3c),tty(1), ttyname(3)
!===================================================================================================================================
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        _tty(1)>',&
'@(#)DESCRIPTION:    output a string repeatedly until killed or limit is reached>',&
'@(#)VERSION:        1.0, 20170202>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:15:51 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program tty

use ISO_FORTRAN_ENV, only : INPUT_UNIT
! The value of the default integer scalar constant INPUT_UNIT identifies the same processor-dependent external
! unit preconnected for sequential formatted input as the one identified by an asterisk in a READ statement; this
! unit is the one used for a READ statement that does not contain an input/output control list. The
! value shall not be -1.

use M_io, only : print_inquire
use M_kracken,only : kracken,lget                ! add command-line parser module
implicit none
character(len=*),parameter  :: ident="@(#)_tty(1f): print information about the file/terminal connected to standard input"
!-----------------------------------------------------------------------------------------------------------------------------------
!  define command arguments, default values and crack command line
   call kracken('tty','-help .false. -version .false.')
   call help_usage(lget('tty_help'))      ! if -help option is present, display help text and exit
   call help_version(lget('tty_version')) ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
write(*,'(a,i0)')'for INPUT_UNIT=',INPUT_UNIT
call print_inquire(INPUT_UNIT,'')

end program tty
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
