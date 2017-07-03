!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! this is a utility program. It is typically built using ccall(1).
!-----------------------------------------------------------------------------------------------------------------------------------
program yes                                                         ! combine yes(1),repeat(1),watch(1),xargs(1). Start with yes(1)
   use M_kracken, only : kracken, lget, sget, iget                  ! add command-line parser module
   implicit none
   character(len=1024) :: string
   integer             :: ios
   integer             :: repeat
   integer             :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('yes','-help .false. -version .false. -repeat -1 ') ! define command arguments,default values and crack command line
!-----------------------------------------------------------------------------------------------------------------------------------
   call help_usage(lget('yes_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('yes_version'))                           ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
   string = trim(sget('yes_oo'))                                    ! get -oo STRING
   if(string.eq.' ')then                                            ! if string is blank use default
      string='y'
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   repeat=iget('yes_repeat')
!-----------------------------------------------------------------------------------------------------------------------------------
   if(repeat.eq.-1)then
      INFINITE: do                                                  ! print string in a loop until an error occurs
         write(*,'(a)',iostat=ios) trim(string)
      enddo INFINITE
   else
      REP: do i=1,repeat
         write(*,'(a)',iostat=ios) trim(string)
      enddo REP
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
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
'@(#)PROGRAM:        _yes(1)>',&
'@(#)DESCRIPTION:    output a string repeatedly until killed or limit is reached>',&
'@(#)VERSION:        1.0, 20150508>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:15:34 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
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
'    _yes(1f) - [FUNIX]output a string repeatedly until killed or limit is reached',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    _yes [STRING[-repeat N]]|[--help|--version]                                 ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'    _yes(1) prints the command line arguments, separated by spaces and followed ',&
'    by a newline until the repeat count is reached or endlessly until it is     ',&
'    killed. If no arguments are given, it prints ''''y'''' followed by a newline',&
'    endlessly until killed. Upon a write error, _yes(1) exits with status "1".  ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    -repeat N  specify number of times to display string                        ',&
'    --help     display this help and exit                                       ',&
'    --version  output version information and exit                              ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'    Sample commands                                                             ',&
'                                                                                ',&
'       # repeat a command 20 times, pausing and clearing:                       ',&
'       _yes  date --repeat 20  |xargs -iXX  sh -c ''''XX;sleep 2;clear''''      ',&
'                                                                                ',&
'REPORTING BUGS                                                                  ',&
'    Report _yes bugs to <http://www.urbanjost.altervista.org/index.html>        ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'    yes(1), repeat(1), xargs(1)                                                 ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     _yes(1f) - [FUNIX]output a string repeatedly until killed or limit is reached
!!
!!##SYNOPSIS
!!
!!     _yes [STRING[-repeat N]]|[--help|--version]
!!
!!##DESCRIPTION
!!     _yes(1) prints the command line arguments, separated by spaces and followed
!!     by a newline until the repeat count is reached or endlessly until it is
!!     killed. If no arguments are given, it prints ''y'' followed by a newline
!!     endlessly until killed. Upon a write error, _yes(1) exits with status "1".
!!
!!##OPTIONS
!!     -repeat N  specify number of times to display string
!!     --help     display this help and exit
!!     --version  output version information and exit
!!
!!##EXAMPLES
!!
!!     Sample commands
!!
!!        # repeat a command 20 times, pausing and clearing:
!!        _yes  date --repeat 20  |xargs -iXX  sh -c ''XX;sleep 2;clear''
!!
!!##REPORTING BUGS
!!     Report _yes bugs to <http://www.urbanjost.altervista.org/index.html>
!!
!!##SEE ALSO
!!     yes(1), repeat(1), xargs(1)
!===================================================================================================================================
end program yes
