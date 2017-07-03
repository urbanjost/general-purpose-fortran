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
'       _ls(1f) - [FUNIX]list files in a directory                               ',&
'SYNOPSIS                                                                        ',&
'       _ls [directory|--version|--help] [-a]                                    ',&
'DESCRIPTION                                                                     ',&
'       Given a directory name list files in the directory                       ',&
'OPTIONS                                                                         ',&
'       directory   name of directory to display contents of.                    ',&
'                   Defaults to current directory.                               ',&
'       -a          show hidden files (files beginning with ".").                ',&
'       --help      display command help and exit                                ',&
'                                                                                ',&
'       --version   output version information and exit                          ',&
'EXAMPLES                                                                        ',&
' Sample command lines ...                                                       ',&
'                                                                                ',&
'        _ls                                                                     ',&
'        _ls /tmp                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!        _ls(1f) - [FUNIX]list files in a directory
!!##SYNOPSIS
!!
!!        _ls [directory|--version|--help] [-a]
!!##DESCRIPTION
!!        Given a directory name list files in the directory
!!##OPTIONS
!!        directory   name of directory to display contents of.
!!                    Defaults to current directory.
!!        -a          show hidden files (files beginning with ".").
!!        --help      display command help and exit
!!
!!        --version   output version information and exit
!!##EXAMPLES
!!
!!  Sample command lines ...
!!
!!         _ls
!!         _ls /tmp
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
'@(#)PROGRAM:        _ls(1f)>',&
'@(#)DESCRIPTION:    list files in a directory>',&
'@(#)VERSION:        1.0, 2016-11-20>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:12:08 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_readdir
use M_kracken, only  : kracken, rget, lget, sget
use M_system, only : system_opendir,system_readdir, system_closedir
use iso_c_binding, only : c_ptr
implicit none
character(len=:),allocatable :: directory
type(c_ptr)                  :: dir
character(len=:),allocatable :: filename
integer                      :: ierr
logical                      :: listall

   call kracken('ls','. -help F -version F -a .F.')
   call help_usage(lget('ls_help'))
   call help_version(lget('ls_version'))
   listall=lget('ls_a')

   directory=sget('ls_oo')
   if(directory.eq.'')then
      directory='.'
   endif
   call system_opendir(directory,dir,ierr)             ! open directory stream to read from
   if(ierr.ne.0)stop 1
   do                                                  ! read directory
      call system_readdir(dir,filename,ierr)
      if(filename.eq.' ')exit
      if(filename(1:1).eq.'.'.and..not.listall)cycle   ! do not list files starting with "." unless -a switch is present
      write(*,'(a)')filename
   enddo
   call system_closedir(dir,ierr)                      ! close directory stream
   if(ierr.ne.0)stop 3
end program demo_system_readdir
