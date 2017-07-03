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
'   _ln(1f) - [FUNIX]create hard links to a file                                 ',&
'SYNOPSIS                                                                        ',&
'  Formats:                                                                      ',&
'                                                                                ',&
'   _ln TARGET LINK_NAME     #  create a link to TARGET with the name LINK_NAME. ',&
'   _ln TARGET               #  create a link to TARGET in the current directory.',&
'   _ln TARGET... DIRECTORY  #  create links to each TARGET in DIRECTORY.        ',&
'DESCRIPTION                                                                     ',&
'   Create hard links (not symbolic links)                                       ',&
'   each destination (name of new link) should not already exist.                ',&
'   When creating hard links, each TARGET must exist.                            ',&
'   Symbolic links can hold arbitrary text; if later resolved, a relative        ',&
'   link is interpreted in relation to its parent directory.                     ',&
'OPTIONS                                                                         ',&
'      TARGET     name of existing file                                          ',&
'      LINK_NAME  if LINK_NAME follows TARGET create a link called LINK_NAME that points to TARGET',&
'      DIRECTORY  if last option is a directory previous filenames on command  are linked into DIRECTORY',&
'      --help     display this help and exit                                     ',&
'      --version  output version information and exit                            ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    _ln(1f) - [FUNIX]create hard links to a file
!!##SYNOPSIS
!!
!!   Formats:
!!
!!    _ln TARGET LINK_NAME     #  create a link to TARGET with the name LINK_NAME.
!!    _ln TARGET               #  create a link to TARGET in the current directory.
!!    _ln TARGET... DIRECTORY  #  create links to each TARGET in DIRECTORY.
!!##DESCRIPTION
!!    Create hard links (not symbolic links)
!!    each destination (name of new link) should not already exist.
!!    When creating hard links, each TARGET must exist.
!!    Symbolic links can hold arbitrary text; if later resolved, a relative
!!    link is interpreted in relation to its parent directory.
!!##OPTIONS
!!       TARGET     name of existing file
!!       LINK_NAME  if LINK_NAME follows TARGET create a link called LINK_NAME that points to TARGET
!!       DIRECTORY  if last option is a directory previous filenames on command  are linked into DIRECTORY
!!       --help     display this help and exit
!!       --version  output version information and exit
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
'@(#)PROGRAM:        _ln(1f)>',&
'@(#)DESCRIPTION:    create links to a target file>',&
'@(#)VERSION:        1.0, 2016-12-04>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Sun, Nov 27th, 2016 10:47:13 PM>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:11:29 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_link
use M_kracken, only : kracken,lget,sgets,IPvalue
use M_system, only : system_link, system_perror
implicit none
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: dirname
integer                            :: ierr, i
integer                            :: name_count
   call kracken('link','-help .F. -version .F. ')     ! define command options and crack options on command call
   call help_usage(lget('link_help'))                 ! if -help is present display help and exit
   call help_version(lget('link_version'))            ! if -version is present display version and exit
   filenames=sgets('link_oo')                         ! get array of filenames from command line
   name_count=size(filenames)
   select case(name_count)
   case(0)
      ierr=0
   case(1)
      ierr = system_link(trim(filenames(1)),'.'//trim(filenames(1)) )
      if(ierr.ne.0)then
         call system_perror('*_link*')
      endif
   case(2)
      ierr = system_link(trim(filenames(1)),trim(filenames(2)) )
      if(ierr.ne.0)then
         call system_perror('*_link*')
      endif
   case default
      do i=1,name_count-1
         dirname=trim(filenames(name_count))
         ierr = system_link(filenames(i), dirname//'/'//filenames(i))
         if(ierr.ne.0)then
            call system_perror('*_link*')
         endif
      enddo
   end select
end program demo_system_link
