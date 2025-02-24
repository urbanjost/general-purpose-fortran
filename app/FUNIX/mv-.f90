program demo_system_rename
use M_kracken, only : kracken,lget,sgets,IPvalue
use M_system, only : system_rename
use M_system, only : system_perror
implicit none
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: oldname
character(len=:),allocatable       :: newname
integer                            :: ierr, i
   call kracken('mv','-help .F. -version .F. ')
   call help_usage(lget('mv_help'))
   call help_version(lget('mv_version'))
   filenames=sgets('mv_oo')
   if(size(filenames).le.1)then
      write(*,*)'*mv-* error -- incorrect number of filenames'
   elseif(size(filenames).eq.2)then
      oldname=trim(filenames(1))
      newname=trim(filenames(2))
      write(*,*)'*mv-* move '//oldname//' to '//newname
      ierr=system_rename(oldname,newname)
      if(ierr.ne.0)then
         call system_perror('*mv-* error:')
      endif
   else
      newname=trim(filenames(size(filenames)))
      write(*,*)'NEWNAME=',newname
      do i=1,size(filenames)-1
         oldname=trim(filenames(i))
         write(*,*)'*mv-* move '//oldname//' to '//newname//'/'//oldname
         ierr=system_rename(oldname,newname//'/'//oldname)
         if(ierr.ne.0)then
            call system_perror('*mv-* error:')
         endif
      enddo
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
'     mv-(1f) - [FUNIX:FILESYSTEM] rename file                                   ',&
'     (LICENSE:PD)                                                               ',&
'SYNOPSIS                                                                        ',&
'     mv- SOURCE DEST                                                            ',&
'DESCRIPTION                                                                     ',&
'       Rename file SOURCE to DEST                                               ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'     mv- file.text /tmp/file.txt                                                ',&
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
!!      mv-(1f) - [FUNIX:FILESYSTEM] rename file
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!      mv- SOURCE DEST
!!##DESCRIPTION
!!        Rename file SOURCE to DEST
!!
!!##EXAMPLE
!!
!!      mv- file.text /tmp/file.txt
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
'@(#)PROGRAM:        mv-(1f)>',&
'@(#)DESCRIPTION:    rename file>',&
'@(#)VERSION:        1.0, 2016-12-03>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Sun, Nov 27th, 2016 10:47:13 PM>',&
'@(#)COMPILED:       2025-02-23 19:23:37 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version

end program demo_system_rename
