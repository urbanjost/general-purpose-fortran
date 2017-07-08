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
'  _showumask(1f) - show umask in decimal, octal, hex, and binary                ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'  _showumask [ -help|-version]                                                  ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   example program calling system_getumask(3f) to get umask value.              ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'                                                                                ',&
'   _showumask                                                                   ',&
'                                                                                ',&
'    18 O''0022'' Z"12'' B''000000010010''                                       ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
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
'@(#)PROGRAM:        _showumask(1f)>',&
'@(#)DESCRIPTION:    show umask in decimal, octal, hex, and binary>',&
'@(#)VERSION:        1.0.0>',&
'@(#)DATE:           2017-01-11>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:14:40 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_umask
use M_kracken, only : kracken, lget
use M_system, only : system_getumask, system_setumask

!  define command arguments, default values and crack command line

   call kracken('umask',' -help .false. -version .false.')
   call help_usage(lget('umask_help'))
   call help_version(lget('umask_version'))

   write(*,101)(system_getumask(),i=1,4)
   101 format(1x,i0,1x,"O'",o4.4,"'",1x,'Z"',z0,"'",1x,"B'",b12.12,"'")
end program demo_umask
