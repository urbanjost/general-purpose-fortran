program demo_system_gethostname
use M_kracken, only : kracken, lget
use M_system, only : system_gethostname
implicit none
character(len=:),allocatable :: name
integer                      :: ierr
   call kracken('hostname','-help .F. -version .F.')
   call help_usage(lget('hostname_help'))
   call help_version(lget('hostname_version'))
   call system_gethostname(name,ierr)
   write(*,'(a)')name
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
'    hostname-(1f) - [FUNIX] display hostname                                                                                    ',&
'    (LICENSE:PD)                                                                                                                ',&
'SYNTAX                                                                                                                          ',&
'    hostname- [ -help|-version]                                                                                                 ',&
'DESCRIPTION                                                                                                                     ',&
'    Calls system_gethostname(3f), which calls get_hostname(3c) to determine                                                     ',&
'    the current host name.                                                                                                      ',&
'OPTIONS                                                                                                                         ',&
'       --help     display this help and exit                                                                                    ',&
'       --version  output version information and exit                                                                           ',&
'EXAMPLE                                                                                                                         ',&
'   Sample execution:                                                                                                            ',&
'                                                                                                                                ',&
'    >hostname-                                                                                                                  ',&
'    >buzz                                                                                                                       ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!     hostname-(1f) - [FUNIX] display hostname
!!     (LICENSE:PD)
!!##SYNTAX
!!     hostname- [ -help|-version]
!!##DESCRIPTION
!!     Calls system_gethostname(3f), which calls get_hostname(3c) to determine
!!     the current host name.
!!##OPTIONS
!!        --help     display this help and exit
!!        --version  output version information and exit
!!##EXAMPLE
!!
!!    Sample execution:
!!
!!     >hostname-
!!     >buzz
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
'@(#)PROGRAM:        hostname-(1f)>',&
'@(#)DESCRIPTION:    print hostname>',&
'@(#)VERSION:        1.0, 2016-12-01>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Sun, Nov 27th, 2016 10:47:13 PM>',&
'@(#)COMPILED:       2022-01-09 23:08:27 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program demo_system_gethostname
