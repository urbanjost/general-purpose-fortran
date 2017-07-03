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
'       _pwd(1f) - [FUNIX]list full pathname of current directory                ',&
'SYNOPSIS                                                                        ',&
'       _pwd [--version|--help]                                                  ',&
'DESCRIPTION                                                                     ',&
'       list full pathname of current directory                                  ',&
'OPTIONS                                                                         ',&
'       --help      display command help and exit                                ',&
'       --version   output version information and exit                          ',&
'EXAMPLES                                                                        ',&
' Sample command lines ...                                                       ',&
'                                                                                ',&
'        _pwd                                                                    ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        _pwd(1f) - [FUNIX]list full pathname of current directory
!!##SYNOPSIS
!!
!!        _pwd [--version|--help]
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
'@(#)PROGRAM:        _pwd(1f)>',&
'@(#)DESCRIPTION:    print full pathname of current directory>',&
'@(#)VERSION:        1.0, 2016-11-20>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:13:35 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_getcwd
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
      write(*,'(a)')trim(dirname)
   else
      write(*,*)'*pwd* ERROR OBTAINING CURRENT DIRECTORY NAME'
   endif
end program demo_system_getcwd
