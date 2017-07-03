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
'                                                                                ',&
'NAME                                                                            ',&
'      _rmdir(1f) - [FUNIX]remove empty directories                              ',&
'SYNOPSIS                                                                        ',&
'       _rmdir DIRECTORY... [OPTION]...                                          ',&
'DESCRIPTION                                                                     ',&
'       given the names of empty directories remove them.                        ',&
'OPTIONS                                                                         ',&
'       DIRECTORY  Remove the DIRECTORY(ies) if they are empty.                  ',&
'       --help     display this help and exit                                    ',&
'       --version  output version information and exit                           ',&
'EXAMPLES                                                                        ',&
' Sample command lines ...                                                       ',&
'                                                                                ',&
'        _rmdir a/b/c /a/b /a                                                    ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!
!!##NAME
!!       _rmdir(1f) - [FUNIX]remove empty directories
!!##SYNOPSIS
!!
!!        _rmdir DIRECTORY... [OPTION]...
!!##DESCRIPTION
!!        given the names of empty directories remove them.
!!##OPTIONS
!!        DIRECTORY  Remove the DIRECTORY(ies) if they are empty.
!!        --help     display this help and exit
!!        --version  output version information and exit
!!##EXAMPLES
!!
!!  Sample command lines ...
!!
!!         _rmdir a/b/c /a/b /a
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
'@(#)PROGRAM:        _rmdir(1f)>',&
'@(#)DESCRIPTION:    remove empty directories>',&
'@(#)VERSION:        1.0, 2016-11-21>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:14:19 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_rmdir
use M_kracken, only  : kracken, lget, sgets, IPvalue
use M_system, only   : system_rmdir, system_perror
use M_strings, only  : v2s
implicit none
character(len=*),parameter :: ident='@(#)_rmdir(1f): remove empty directories via rmdir(3c)'
character(len=IPvalue),allocatable :: directories(:) ! hold directory names, assuming length large enough
integer                            :: ierr,ierr_abs,i

   call kracken('rmdir','  -help F -version F') ! crack command line
   call help_usage(lget('rmdir_help'))          ! display help if --help option on command line
   call help_version(lget('rmdir_version'))     ! display version if --version option on command line
   directories=sgets('rmdir_oo')                ! get array of directory names

   ierr_abs=0                                   ! set this to last non-zero return value, if any
   do i=1,size(directories)                     ! try to remove names as empty directories
      ierr=system_rmdir(trim(directories(i)))
      if(ierr.ne.0)then                         ! system_rmdir(3f) supports using system_perror(3f)
         call system_perror('*_rmdir*: error '//v2s(ierr)//':failed to remove directory '''//trim(directories(i))//'''')
         ierr_abs=ierr
      endif
   enddo
   if(ierr_abs.ne.0) stop 1 ! try to return non-zero system return value
end program demo_system_rmdir
