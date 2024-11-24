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
'   prompt(1f) - prompt for whether to execute the command given on the command line',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   prompt "cmd"                                                                 ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Given a command prompt as to whether to execute it or not.                   ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   cmd   command to conditionally execute                                       ',&
'   --help        display this help and exit                                     ',&
'   --version     output version information and exit                            ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  Typical usage:                                                                ',&
'                                                                                ',&
'   prompt  program                                                              ',&
'SEE ALSO                                                                        ',&
'   xargs(1)                                                                     ',&
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
!!    prompt(1f) - prompt for whether to execute the command given on the command line
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    prompt "cmd"
!!
!!##DESCRIPTION
!!    Given a command prompt as to whether to execute it or not.
!!
!!##OPTIONS
!!    cmd   command to conditionally execute
!!    --help        display this help and exit
!!    --version     output version information and exit
!!
!!##EXAMPLE
!!
!!   Typical usage:
!!
!!    prompt  program
!!##SEE ALSO
!!    xargs(1)
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
'@(#)PROGRAM:        prompt(1f)>',&
'@(#)DESCRIPTION:    prompt for whether to execute command>',&
'@(#)VERSION:        1.0, 20210801>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2024-11-24 04:45:27 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program prg_prompt
use M_kracken, only: kracken, sget, lget, dget, igets, iget
implicit none
character(len=*),parameter :: ident="@(#)prompt(1):prompt whether to execute command"
character(len=:),allocatable :: cmd
character(len=1) :: cprompt
integer :: itimes
integer :: i
integer :: ios
integer :: cstat
character(len=256) :: sstat
                                                             ! parse command line
   call kracken('prompt',' -oo -repeat 0 -help .F. -version .F.')
   call help_usage(lget('prompt_help'))                        ! display help information and stop if true
   call help_version(lget('prompt_version'))                   ! display version information and stop if true
   itimes=max(1,iget('prompt_repeat'))
   cmd=trim(sget('prompt_oo'))
   do i=itimes,1,-1
      write(*,'("run '//cmd//'...")',advance="no")
      read(*,'(a)',iostat=ios) cprompt
      if(ios.ne.0)cycle
      select case(cprompt)
      case('y','Y',' ')
         if(cmd.ne.' '.and.cprompt.eq.'y')then
            call execute_command_line(cmd,cmdstat=cstat,cmdmsg=sstat)
         endif
      end select
   enddo
end program prg_prompt
