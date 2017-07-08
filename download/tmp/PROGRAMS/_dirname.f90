!===================================================================================================================================
program demo_dirname
use M_kracken, only : kracken, sgets, lget                  ! add command-line parser module
use M_strings, only : split
use M_io,      only : dirname
implicit none
character(len=*),parameter       :: ident="@(#)dirname(1f):strip last component from file name(s)" ! metadata for what(1)
logical                          :: zero=.false.
character(len=4096),allocatable  :: array(:)
integer                          :: i
!-----------------------------------------------------------------------------------------------------------------------------------
!  define command arguments, default values and crack command line
   call kracken('dirname','-help .false. -version .false. -zero .false.')
   call help_usage(lget('dirname_help'))        ! process -help switch
   call help_version(lget('dirname_version'))   ! process -version switch
   zero=lget('dirname_zero')                    ! get -zero option
   array=sgets('dirname_oo')                    ! get -oo STRING, split on space character into array
!----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(array)                           ! loop thru file-or-directory names
      if(zero)then
            write(*,'(a)',advance='no')dirname(array(i))//achar(0)
      else
            write(*,'(a)')dirname(array(i))
      endif
   enddo
!----------------------------------------------------------------------------------------------------------------------------------
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
'   _dirname(1f) - [FUNIX]strip last component from file name                    ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   _dirname NAME... [-zero]|-help|-version]                                     ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Output each NAME with its last non-slash component and trailing slashes removed.',&
'   if NAME contains no /''s, output ''.'' (meaning the current directory).      ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   -zero     end each output line with NUL, not newline                         ',&
'   -help     display this help and exit                                         ',&
'   -version  output version information and exit                                ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'   Sample program executions:                                                   ',&
'                                                                                ',&
'     _dirname /usr/bin/          -> "/usr"                                      ',&
'     _dirname dir1/str dir2/str  -> "dir1" followed by "dir2"                   ',&
'     _dirname stdio.h            -> "."                                         ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'   dirname(1), basename(1), readlink(1), realpath(1)                            ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    _dirname(1f) - [FUNIX]strip last component from file name
!!
!!##SYNOPSIS
!!
!!    _dirname NAME... [-zero]|-help|-version]
!!
!!##DESCRIPTION
!!    Output each NAME with its last non-slash component and trailing slashes removed.
!!    if NAME contains no /'s, output '.' (meaning the current directory).
!!
!!##OPTIONS
!!    -zero     end each output line with NUL, not newline
!!    -help     display this help and exit
!!    -version  output version information and exit
!!
!!##EXAMPLES
!!
!!    Sample program executions:
!!
!!      _dirname /usr/bin/          -> "/usr"
!!      _dirname dir1/str dir2/str  -> "dir1" followed by "dir2"
!!      _dirname stdio.h            -> "."
!!
!!##SEE ALSO
!!    dirname(1), basename(1), readlink(1), realpath(1)
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
'@(#)PROGRAM:        _dirname(1f)>',&
'@(#)DESCRIPTION:    strip last component from file name>',&
'@(#)VERSION:        1.0.0>',&
'@(#)DATE:           2015-06-26>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:09:57 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
end program demo_dirname
!===================================================================================================================================
