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
'     _mkfifo(1f) - [FUNIX]make a FIFO pipe by calling mkfifo(3c)                ',&
'SYNOPSIS                                                                        ',&
'     _mkfifo file(s)                                                            ',&
'DESCRIPTION                                                                     ',&
'EXAMPLE                                                                         ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!      _mkfifo(1f) - [FUNIX]make a FIFO pipe by calling mkfifo(3c)
!!##SYNOPSIS
!!
!!      _mkfifo file(s)
!!##DESCRIPTION
!!##EXAMPLE
!!
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
'@(#)PROGRAM:        _mkfifo(1f)>',&
'@(#)DESCRIPTION:    make FIFO pipe file>',&
'@(#)VERSION:        1.0, 2016-12-04>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:12:51 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program demo_system_mkfifo
use M_kracken, only : kracken,lget,sgets,IPvalue
use M_system, only : system_mkfifo, system_perror
use M_system, only : R_GRP,R_OTH,R_USR,R_WXG,R_WXO
use M_system, only : R_WXU,W_GRP,W_OTH,W_USR,X_GRP,X_OTH,X_USR
use M_system, only : DEFFILEMODE, ACCESSPERMS
use M_system, only : system_perror
implicit none
character(len=IPvalue),allocatable :: filenames(:)
character(len=:),allocatable       :: dname
integer                            :: ierr, i
   call kracken('mkfifo','-help .F. -version .F. ')
   call help_usage(lget('mkfifo_help'))
   call help_version(lget('mkfifo_version'))
   filenames=sgets('mkfifo_oo')

   do i=1,size(filenames)
      dname=trim(filenames(i))
      ierr = system_mkfifo(dname, IANY([W_USR, R_USR, X_USR]))
      if(ierr.ne.0)then
         call system_perror('*_mkfifo*')
      endif
   enddo
end program demo_system_mkfifo
