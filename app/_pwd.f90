subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'       _pwd(1f) - [FUNIX:FILESYSTEM] list full pathname of current directory    ',&
'       (LICENSE:PD)                                                             ',&
'SYNOPSIS                                                                        ',&
'       _pwd [ --version|--help]                                                 ',&
'DESCRIPTION                                                                     ',&
'       list full pathname of current directory                                  ',&
'OPTIONS                                                                         ',&
'       --help      display command help and exit                                ',&
'       --version   output version information and exit                          ',&
'EXAMPLES                                                                        ',&
' Sample command lines ...                                                       ',&
'                                                                                ',&
'        _pwd                                                                    ',&
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
!!        _pwd(1f) - [FUNIX:FILESYSTEM] list full pathname of current directory
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        _pwd [ --version|--help]
!!##DESCRIPTION
!!        list full pathname of current directory
!!##OPTIONS
!!        --help      display command help and exit
!!        --version   output version information and exit
!!##EXAMPLES
!!
!!  Sample command lines ...
!!
!!         _pwd
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
'@(#)PROGRAM:        _pwd(1f)>',&
'@(#)DESCRIPTION:    print full pathname of current directory>',&
'@(#)VERSION:        1.0, 2016-11-20>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Mon, May 24th, 2021 11:23:28 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_getcwd
use iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT, INPUT_UNIT  ! access computing environment
use M_kracken, only : kracken, lget
use M_system,  only : system_getcwd
implicit none
character(len=:),allocatable :: dirname
integer                      :: ierr
   call kracken('_pwd','-help .F. -version .F.')
   call help_usage(lget('_pwd_help'))
   call help_version(lget('_pwd_version'))
   call system_getcwd(dirname,ierr)
   if(ierr.eq.0)then
      write(OUTPUT_UNIT,'(a)')trim(dirname)
   else
      write(ERROR_UNIT,*)'*pwd* ERROR: cannot obtain current directory name'
   endif
end program demo_system_getcwd
