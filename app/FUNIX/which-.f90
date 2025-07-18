program demo_which
use M_kracken, only : kracken, sgets, lget
use M_strings, only : split
use M_system, only  : system_access, R_OK, W_OK, X_OK, F_OK

implicit none
character(len=:),allocatable    :: searchpath
character(len=:),allocatable    :: directories(:)
character(len=:),allocatable    :: pathname
integer                         :: path_line_length
character(len=:),allocatable    :: names(:)
character(len=:),allocatable    :: name
integer                         :: i,j
logical                         :: which_all
   call kracken('which',' -all .F. -help .F. -version .F.')             ! get command-line arguments
   call help_usage(lget('which_help'))                                  ! process -help switch
   call help_version(lget('which_version'))                             ! process -version switch
   which_all=lget('which_all')                                          ! process -all switch

   call get_environment_variable(name="PATH", length=path_line_length)  ! get length of $PATH
   allocate(character(len=path_line_length) :: searchpath)              ! make a string variable long enough to hold $PATH
   call get_environment_variable(name="PATH", value=searchpath)         ! get value of environment variable $PATH
   call split(searchpath,directories,':')                               ! create array of directory names in $PATH
 !!write(*,'(i0,"==>",a)')(i,trim(directories(i)),i=1,size(directories))

   names=sgets('which_oo')                                              ! create array of names found on command line
   NAMESLOOP: do j=1,size(names)                                        ! try name appended to each directory name
      name=names(j)
      DIRLOOP: do i=1,size(directories)
         pathname=trim(directories(i))//'/'//trim(name)
         if(system_access(pathname,X_OK))then
            write(*,'(a)')pathname
            if(.not.which_all)exit DIRLOOP
         endif
      enddo DIRLOOP
   enddo NAMESLOOP
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
'       which-(1f) - [FUNIX:FILESYSTEM] shows the full path of (shell) commands. ',&
'       (LICENSE:PD)                                                             ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'       which- program_leafname [ --all| -a]|[ --help|--version]                 ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'       which-(1f) takes one or more pathnames. For each of its arguments        ',&
'       it prints to stdout the full path of the executables that would          ',&
'       have been executed when this argument had been entered at the            ',&
'       shell prompt. It does this by searching for an executable or             ',&
'       script in the directories listed in the environment variable PATH.       ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       --all,-a   Print all matching executables in PATH, not just the first.   ',&
'       --version  Print version information on standard output then             ',&
'                  exit successfully.                                            ',&
'       --help     Print usage information on standard output then               ',&
'                  exit successfully.                                            ',&
'                                                                                ',&
'RETURN VALUE                                                                    ',&
'       Which returns the number of failed arguments, or -1 when no              ',&
'       programname was given.                                                   ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
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
!!        which-(1f) - [FUNIX:FILESYSTEM] shows the full path of (shell) commands.
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        which- program_leafname [ --all| -a]|[ --help|--version]
!!
!!##DESCRIPTION
!!        which-(1f) takes one or more pathnames. For each of its arguments
!!        it prints to stdout the full path of the executables that would
!!        have been executed when this argument had been entered at the
!!        shell prompt. It does this by searching for an executable or
!!        script in the directories listed in the environment variable PATH.
!!
!!##OPTIONS
!!        --all,-a   Print all matching executables in PATH, not just the first.
!!        --version  Print version information on standard output then
!!                   exit successfully.
!!        --help     Print usage information on standard output then
!!                   exit successfully.
!!
!!##RETURN VALUE
!!        Which returns the number of failed arguments, or -1 when no
!!        programname was given.
!!
!!##EXAMPLE
!!
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
'@(#)PROGRAM:        which-(1f)>',&
'@(#)DESCRIPTION:    list pathnames of leaf names that are executable and can be found using the $PATH variable>',&
'@(#)VERSION:        1.0, 2017-10-15>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2025-06-29 08:18:44 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program demo_which
