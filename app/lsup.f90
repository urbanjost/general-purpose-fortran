!===================================================================================================================================
program lsup
use M_kracken, only : kracken, sgets, lget                  ! add command-line parser module
use M_io,      only : dirname
use M_system,  only : system_getcwd, system_stat_print, system_getcwd
implicit none
character(len=:),allocatable     :: array(:)
integer                          :: i, ierr, isize
character(len=:),allocatable     :: filename
character(len=:),allocatable     :: currentdir

!-----------------------------------------------------------------------------------------------------------------------------------
!  define command arguments, default values and crack command line
   call kracken('lsof','-help .false. -version .false.')
   call help_usage(lget('lsof_help'))        ! process -help switch
   call help_version(lget('lsof_version'))   ! process -version switch
   array=sgets('lsof_oo')                    ! get -oo STRING, split on space character into array
   isize=size(array)
   if(isize.eq.0)then                        ! if no name is specified use current directory name
      call system_getcwd(filename,ierr)
      array=[trim(filename)]
      isize=1
   endif
!----------------------------------------------------------------------------------------------------------------------------------
   do i=1,isize                                 ! loop thru pathnames
      filename=trim(array(i))
      if(index(filename,'/').eq.0)then
         call system_getcwd(currentdir,ierr)
         if(ierr.eq.0)then
            filename=currentdir//'/'//filename
         endif
      endif
      do
         call system_stat_print(filename)
         filename=dirname(filename)             ! remove leaf from pathname and repeat till pathname leafs are consumed
         if(filename.eq.'')exit
         if(filename.eq.'.')exit
      enddo
      if(isize.eq.i)exit                        ! put a blank line out as a separator between pathnames
      write(*,*)
   enddo
contains
!===================================================================================================================================
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
'   lsup(1f) - [FUNIX:FILESYSTEM] list permissions of pathname and               ',&
'   directories in pathname                                                      ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   lsup NAME... |-help|-version]                                                ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Output the permissions of the specified pathnames. Then recursively          ',&
'   output each NAME with its last non-slash component and trailing              ',&
'   slashes removed, if NAME contains no /''s, output ''.'' (meaning the         ',&
'   current directory).                                                          ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   NAME      pathname. Defaults to current directory                            ',&
'   -help     display this help and exit                                         ',&
'   -version  output version information and exit                                ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
' Sample program executions:                                                     ',&
'                                                                                ',&
'  lsup                                                                          ',&
'   40755 drwxr-xr-x --- 1  JSU    Users  0   2017-10-12T21:46:51 /V6/LIB/GPF/EXE',&
'   40755 drwxr-xr-x --- 1  JSU    None   0   2017-10-08T14:50:41 /V6/LIB/GPF    ',&
'   40700 drwx------ --- 1  JSU    Users  0   2017-10-12T22:38:14 /V6/LIB        ',&
'   40700 drwx------ --- 1  JSU    None   0   2017-10-12T22:35:00 /V6            ',&
'                                                                                ',&
'  lsup /etc/hosts /usr/share/man                                                ',&
'  100750 -rwxr-x--- --- 1  SYSTEM SYSTEM 824 2017-07-14T03:58:59 /etc/hosts     ',&
'   40755 drwxr-xr-x --- 1  JSU    None   0   2017-09-11T03:29:02 /etc           ',&
'                                                                                ',&
'   40755 drwxr-xr-x --- 1  JSU    None   0   2017-09-04T19:40:32 /usr/share/man ',&
'   40755 drwxr-xr-x --- 1  JSU    None   0   2017-09-11T03:19:16 /usr/share     ',&
'   40755 drwxr-xr-x --- 1  JSU    None   0   2017-09-11T03:05:57 /usr           ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'   dirname(1), realpath(1)                                                      ',&
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
!!    lsup(1f) - [FUNIX:FILESYSTEM] list permissions of pathname and
!!    directories in pathname
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    lsup NAME... |-help|-version]
!!
!!##DESCRIPTION
!!    Output the permissions of the specified pathnames. Then recursively
!!    output each NAME with its last non-slash component and trailing
!!    slashes removed, if NAME contains no /'s, output '.' (meaning the
!!    current directory).
!!
!!##OPTIONS
!!    NAME      pathname. Defaults to current directory
!!    -help     display this help and exit
!!    -version  output version information and exit
!!
!!##EXAMPLES
!!
!!  Sample program executions:
!!
!!   lsup
!!    40755 drwxr-xr-x --- 1  JSU    Users  0   2017-10-12T21:46:51 /V6/LIB/GPF/EXE
!!    40755 drwxr-xr-x --- 1  JSU    None   0   2017-10-08T14:50:41 /V6/LIB/GPF
!!    40700 drwx------ --- 1  JSU    Users  0   2017-10-12T22:38:14 /V6/LIB
!!    40700 drwx------ --- 1  JSU    None   0   2017-10-12T22:35:00 /V6
!!
!!   lsup /etc/hosts /usr/share/man
!!   100750 -rwxr-x--- --- 1  SYSTEM SYSTEM 824 2017-07-14T03:58:59 /etc/hosts
!!    40755 drwxr-xr-x --- 1  JSU    None   0   2017-09-11T03:29:02 /etc
!!
!!    40755 drwxr-xr-x --- 1  JSU    None   0   2017-09-04T19:40:32 /usr/share/man
!!    40755 drwxr-xr-x --- 1  JSU    None   0   2017-09-11T03:19:16 /usr/share
!!    40755 drwxr-xr-x --- 1  JSU    None   0   2017-09-11T03:05:57 /usr
!!
!!##SEE ALSO
!!    dirname(1), realpath(1)
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
'@(#)PROGRAM:        lsup(1f)>',&
'@(#)DESCRIPTION:    show permits of a pathname and all directory components of the pathname>',&
'@(#)VERSION:        1.0.0>',&
'@(#)DATE:           2017-10-12>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2024-06-29 21:49:36 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program lsup
!----------------------------------------------------------------------------------------------------------------------------------
