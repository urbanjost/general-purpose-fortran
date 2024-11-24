!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! this is a utility program. It is typically built using ccall(1).
!-----------------------------------------------------------------------------------------------------------------------------------
program yes                                                         ! combine yes(1),repeat(1),watch(1),xargs(1). Start with yes(1)
use M_kracken, only : kracken, lget, sget, iget                  ! add command-line parser module
!!use M_kracken, only : show
implicit none
character(len=:),allocatable :: string
logical                      :: fmt
integer                      :: ios
integer                      :: repeat
integer                      :: i
integer                      :: istart
character(len=1024)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('yes','-help .F. -version .F. -repeat -1 -fmt .F.') ! define command arguments,default values and crack command line
!-----------------------------------------------------------------------------------------------------------------------------------
   !!call show('',.false.,0)
   call help_usage(lget('yes_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('yes_version'))                           ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
   string = trim(sget('yes_oo'))                                    ! get -oo STRING
   if(string.eq.' ')then                                            ! if string is blank use default
      string='y'
   endif
   fmt = lget('yes_fmt')                                            ! get -fmt
   repeat=iget('yes_repeat')
!-----------------------------------------------------------------------------------------------------------------------------------
   istart=0
   if(repeat.eq.-1)then
      INFINITE: do                                                  ! print string in a loop until an error occurs
         call printme()
      enddo INFINITE
   else
      REP: do i=1,repeat
         call printme()
      enddo REP
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
   subroutine printme  ! print as a string or print string as a format with line count as a parameter
         istart=istart+1
         if(fmt)then
            write(*,string,iostat=ios,iomsg=message) istart
         else
            write(*,'(a)',iostat=ios) trim(string)
         endif
         if(ios.ne.0)then
            write(*,*)'*yes* error:',trim(message)
            stop
         endif
   end subroutine printme
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
'@(#)PROGRAM:        yes-(1)>',&
'@(#)DESCRIPTION:    output a string repeatedly until killed or limit is reached>',&
'@(#)VERSION:        1.0, 20150508>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2024-11-24 04:44:29 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
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
'    yes-(1f) - [FUNIX] output a string repeatedly until killed or limit is reached',&
'    (LICENSE:PD)                                                                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    yes- [STRING[ -repeat N]]|[ --help| --version]                              ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'    yes-(1) prints the command line arguments, separated by spaces and followed ',&
'    by a newline until the repeat count is reached or endlessly until it is     ',&
'    killed. If no arguments are given, it prints "y" followed by a newline      ',&
'    endlessly until killed. Upon a write error, yes-(1) exits with status "1".  ',&
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
'       yes-  date --repeat 20  |xargs -iXX  sh -c ''XX;sleep 2;clear''          ',&
'                                                                                ',&
'REPORTING BUGS                                                                  ',&
'    Report yes- bugs to <http://www.urbanjost.altervista.org/index.html>        ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'    yes(1), repeat(1), xargs(1)                                                 ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!     yes-(1f) - [FUNIX] output a string repeatedly until killed or limit is reached
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     yes- [STRING[ -repeat N]]|[ --help| --version]
!!
!!##DESCRIPTION
!!     yes-(1) prints the command line arguments, separated by spaces and followed
!!     by a newline until the repeat count is reached or endlessly until it is
!!     killed. If no arguments are given, it prints "y" followed by a newline
!!     endlessly until killed. Upon a write error, yes-(1) exits with status "1".
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
!!        yes-  date --repeat 20  |xargs -iXX  sh -c 'XX;sleep 2;clear'
!!
!!##REPORTING BUGS
!!     Report yes- bugs to <http://www.urbanjost.altervista.org/index.html>
!!
!!##SEE ALSO
!!     yes(1), repeat(1), xargs(1)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
end program yes
