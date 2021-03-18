subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   _unlink(1f) - [FUNIX:FILESYSTEM] call the unlink(3c) function to remove the specified file',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'     _unlink file(s)                                                            ',&
'     _unlink OPTION                                                             ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       Call the unlink function to remove the specified FILE.                   ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       FILES      list of files to remove                                       ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
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
!!    _unlink(1f) - [FUNIX:FILESYSTEM] call the unlink(3c) function to remove the specified file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!      _unlink file(s)
!!      _unlink OPTION
!!
!!##DESCRIPTION
!!        Call the unlink function to remove the specified FILE.
!!
!!##OPTIONS
!!        FILES      list of files to remove
!!        --help     display this help and exit
!!        --version  output version information and exit
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
'@(#)PROGRAM:        _unlink(1f)>',&
'@(#)DESCRIPTION:    remove file by unlinking it>',&
'@(#)VERSION:        1.0, 2016-12-03>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Mon, Mar 15th, 2021 12:51:09 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_unlink
use M_kracken, only : kracken,lget,sgets,IPvalue
use M_system, only : system_unlink
use M_system, only : system_perror
implicit none
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: oldname
integer                            :: ierr, i
   call kracken('unlink','-help .F. -version .F. ')
   call help_usage(lget('unlink_help'))
   call help_version(lget('unlink_version'))
   filenames=sgets('unlink_oo')
   do i=1,size(filenames)
      oldname=trim(filenames(i))
      write(*,*)'*_unlink* '//oldname
      ierr=system_unlink(oldname)
      if(ierr.ne.0)then
         call system_perror('*_unlink*')
      endif
   enddo
end program demo_system_unlink
