program demo_system_pwd
use iso_fortran_env, only : ERROR_UNIT, OUTPUT_UNIT, INPUT_UNIT  ! access computing environment
use M_kracken, only : kracken, lget
use M_system,  only : system_getcwd
implicit none
character(len=:),allocatable :: dirname
integer                      :: ierr
   call kracken('pwd','-help .F. -version .F.')
   call help_usage(lget('pwd_help'))
   call help_version(lget('pwd_version'))
   call system_getcwd(dirname,ierr)
   if(ierr.eq.0)then
      write(OUTPUT_UNIT,'(a)')trim(dirname)
   else
      write(ERROR_UNIT,*)'*pwd* ERROR: cannot obtain current directory name'
   endif
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
'NAME                                                                            ',&
'       pwd-(1f) - [FUNIX:FILESYSTEM] list full pathname of current directory    ',&
'       (LICENSE:PD)                                                             ',&
'SYNOPSIS                                                                        ',&
'       pwd- [ --version|--help]                                                 ',&
'DESCRIPTION                                                                     ',&
'       list full pathname of current directory                                  ',&
'OPTIONS                                                                         ',&
'       --help      display command help and exit                                ',&
'       --version   output version information and exit                          ',&
'EXAMPLES                                                                        ',&
' Sample command lines ...                                                       ',&
'                                                                                ',&
'        pwd-                                                                    ',&
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
!!        pwd-(1f) - [FUNIX:FILESYSTEM] list full pathname of current directory
!!        (LICENSE:PD)
!!##SYNOPSIS
!!
!!        pwd- [ --version|--help]
!!##DESCRIPTION
!!        list full pathname of current directory
!!##OPTIONS
!!        --help      display command help and exit
!!        --version   output version information and exit
!!##EXAMPLES
!!
!!  Sample command lines ...
!!
!!         pwd-
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
'@(#)PROGRAM:        pwd-(1f)>',&
'@(#)DESCRIPTION:    print full pathname of current directory>',&
'@(#)VERSION:        1.0, 2016-11-20>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2025-06-29 08:17:56 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program demo_system_pwd
