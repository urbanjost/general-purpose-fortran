program demo_system_link
use M_kracken, only : kracken,lget,sgets,IPvalue
use M_system,  only : system_link
use M_system,  only : system_perror
implicit none
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: oldname
character(len=:),allocatable       :: newname
integer                            :: ierr, i
logical                            :: verbose
   call kracken('link','-help .F. -version .F. -verbose .F.')
   call help_usage(lget('link_help'))
   call help_version(lget('link_version'))
   verbose=lget('link_verbose')
   filenames=sgets('link_oo')
   i=0
   do i=2,size(filenames),2
      oldname=trim(filenames(i-1))
      newname=trim(filenames(i))
      ierr=system_link(oldname,newname)
      if(ierr.ne.0)then
         call system_perror('*_link*'//oldname//' '//newname)
      elseif(verbose)then
         write(*,'(a)')'*_link* linked '//oldname//' '//newname
      endif
   enddo
   if(i.gt.size(filenames))then
      write(*,*)'/*_link* error: odd number of files, leftover='//filenames(i-1:)
   endif
contains
subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   _link(1f) - [FUNIX:FILESYSTEM] call the link(3c) function to create the specified file link',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   _link [file newlink][file newlink][file newlink]...                          ',&
'   _link OPTION                                                                 ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Call the link(3c) function to link the specified FILE.                       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   FILES      list of file pairs to link [oldfile newfile]...                   ',&
'   --verbose  toggle on verbose mode                                            ',&
'   --help     display this help and exit                                        ',&
'   --version  output version information and exit                               ',&
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
!!    _link(1f) - [FUNIX:FILESYSTEM] call the link(3c) function to create the specified file link
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    _link [file newlink][file newlink][file newlink]...
!!    _link OPTION
!!
!!##DESCRIPTION
!!    Call the link(3c) function to link the specified FILE.
!!
!!##OPTIONS
!!    FILES      list of file pairs to link [oldfile newfile]...
!!    --verbose  toggle on verbose mode
!!    --help     display this help and exit
!!    --version  output version information and exit
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
'@(#)PROGRAM:        _link(1f)>',&
'@(#)DESCRIPTION:    link to file>',&
'@(#)VERSION:        1.0, 2020-01-19>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Mon, May 24th, 2021 11:23:23 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
end program demo_system_link
