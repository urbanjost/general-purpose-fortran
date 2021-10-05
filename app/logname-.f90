program demo_system_getlogin
use M_system, only : system_getlogin
use M_kracken, only: kracken,lget
implicit none
character(len=:),allocatable :: name

   call kracken('logname','-help .F. -version .F.')  ! crack command line
   call help_usage(lget('logname_help'))             ! if -help present display help and exit
   call help_version(lget('logname_version'))        ! if -version present display help and exit

   name=system_getlogin()                            ! query login name
   write(*,'(a)')name                                ! display login name
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
'      logname-(1f) - [FUNIX] display login name                                                                                 ',&
'      (LICENSE:PD)                                                                                                              ',&
'SYNOPSIS                                                                                                                        ',&
'      logname- [ -help|-version]                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'      Demonstrate call to system_getlogin(3f) (which calls getlogin(3c)                                                         ',&
'OPTIONS                                                                                                                         ',&
'       --help      display command help and exit                                                                                ',&
'       --version   output version information and exit                                                                          ',&
'EXAMPLE                                                                                                                         ',&
'      Command usage:                                                                                                            ',&
'                                                                                                                                ',&
'        logname-                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
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
'@(#)PROGRAM:        logname-(1f)>',&
'@(#)DESCRIPTION:    print login name using system_getlogin(3f)/getlogin(3c)>',&
'@(#)VERSION:        1.0, 2016-12-02>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2021-09-09 17:58:03 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program demo_system_getlogin
