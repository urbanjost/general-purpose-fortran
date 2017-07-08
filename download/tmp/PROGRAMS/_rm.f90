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
'     _rm(1f) - [FUNIX]remove file                                               ',&
'SYNOPSIS                                                                        ',&
'     _rm file(s)                                                                ',&
'DESCRIPTION                                                                     ',&
'     Remove file(s).                                                            ',&
'EXAMPLE                                                                         ',&
'     _rm *.o                                                                    ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      _rm(1f) - [FUNIX]remove file
!!##SYNOPSIS
!!
!!      _rm file(s)
!!##DESCRIPTION
!!      Remove file(s).
!!##EXAMPLE
!!
!!      _rm *.o
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
'@(#)PROGRAM:        _rm(1f)>',&
'@(#)DESCRIPTION:    remove file>',&
'@(#)VERSION:        1.0, 2016-12-03>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:13:57 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_rename
use M_kracken, only : kracken,lget,sgets,IPvalue
use M_system, only : system_remove
use M_system, only : system_perror
implicit none
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: oldname
integer                            :: ierr, i
   call kracken('rm','-help .F. -version .F. ')
   call help_usage(lget('rm_help'))
   call help_version(lget('rm_version'))
   filenames=sgets('rm_oo')
   do i=1,size(filenames)
      oldname=trim(filenames(i))
      write(*,*)'*_rm* '//oldname
      ierr=system_remove(oldname)
      if(ierr.ne.0)then
         call system_perror('*_rm*')
      endif
   enddo
end program demo_system_rename
