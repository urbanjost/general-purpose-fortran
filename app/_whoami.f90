subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'      _whoami(1f) - [FUNIX] display login name                                  ',&
'      (LICENSE:PD)                                                              ',&
'SYNOPSIS                                                                        ',&
'      _whoami [ -help|-version]                                                 ',&
'DESCRIPTION                                                                     ',&
'      Demonstrate call to system_geteuid(3f) and system_getpwuid(3f) interfaces to the corresponding C routines',&
'OPTIONS                                                                         ',&
'       --help      display command help and exit                                ',&
'       --version   output version information and exit                          ',&
'EXAMPLE                                                                         ',&
'      Command usage:                                                            ',&
'                                                                                ',&
'        _whoami                                                                 ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!       _whoami(1f) - [FUNIX] display login name
!!       (LICENSE:PD)
!!##SYNOPSIS
!!
!!       _whoami [ -help|-version]
!!##DESCRIPTION
!!       Demonstrate call to system_geteuid(3f) and system_getpwuid(3f) interfaces to the corresponding C routines
!!##OPTIONS
!!        --help      display command help and exit
!!        --version   output version information and exit
!!##EXAMPLE
!!
!!       Command usage:
!!
!!         _whoami
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
! @(#)help_version(3f): prints version information
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        _whoami(1f)>',&
'@(#)DESCRIPTION:    print effective user name>',&
'@(#)VERSION:        1.0, 2019-03-24>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Mon, May 24th, 2021 11:23:35 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program whoami
use M_system, only : system_getlogin, system_geteuid, system_getpwuid, system_getuid
use M_kracken, only: kracken,lget
implicit none
character(len=:),allocatable :: name
integer                      :: uid

   call kracken('whoami','-verbose .F. -help .F. -version .F.')   ! crack command line
   call help_usage(lget('whoami_help'))                           ! if -help present display help and exit
   call help_version(lget('whoami_version'))                      ! if -version present display help and exit
if(lget('whoami_verbose'))then
   uid=system_getuid()
   name=system_getlogin()                            ! query login name
   write(*,'("login[",a,"] from getlogin(3c) has UID ",i0)')name,uid

   uid=system_geteuid()
   name=system_getlogin()                            ! query login name
   write(*,'("effecitve username[",a,"] from geteuid(3c) has UID ",i0)')name,uid

   uid=system_getuid()
   name=system_getpwuid(uid)
   write(*,'("username[",a,"] from getuid(3c) has UID ",i0)')name,uid
else
   uid=system_geteuid()
   name=system_getlogin()                            ! query login name
   write(*,'(a)')name                                ! display login name
endif
end program whoami
