program demosplitname
use M_kracken, only : kracken, sgets, lget                  ! add command-line parser module
use M_strings, only : split
use M_io,      only : splitpath
implicit none
character(len=*),parameter       :: ident="@(#)splitname(1f):strip last component from file name(s)" ! metadata for what(1)
logical                          :: zero=.false.
character(len=4096),allocatable  :: array(:)
character(len=4096)              :: dir,name,basename,ext
integer                          :: i
!-----------------------------------------------------------------------------------------------------------------------------------
!  define command arguments, default values and crack command line
   call kracken('splitname','-help .false. -version .false. -zero .false.')
   call help_usage(lget('splitname_help'))        ! process -help switch
   call help_version(lget('splitname_version'))   ! process -version switch
   zero=lget('splitname_zero')                    ! get -zero option
   array=sgets('splitname_oo')                    ! get -oo STRING, split on space character into array
!----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(array)                           ! loop thru file-or-directory names
      call splitpath(array(i),dir,name,basename,ext)
      write(*,'(4(''"'',a,''" ''))')trim(dir),trim(name),trim(basename),trim(ext)
   enddo
!----------------------------------------------------------------------------------------------------------------------------------
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
'   splitname(1f) - [FUNIX]strip pathname into components {dir,name,basename,extension}',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   splitname [NAME1 NAME2 ... |-help|-version]                                  ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Output each pathname broken into components dir, name, basename, extension   ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   NAMES     pathnames to split                                                 ',&
'   -help     display this help and exit                                         ',&
'   -version  output version information and exit                                ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
'   Sample program executions:                                                   ',&
'                                                                                ',&
'     splitname /usr/bin/          -> "/usr/bin" "" "" ""                        ',&
'     splitname /usr/bin           -> "/usr" "bin" "" ""                         ',&
'     splitname stdio.h            -> "" "studio.h" "studio" ".h"                ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'   splitname(1), basename(1), readlink(1), realpath(1)                          ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    splitname(1f) - [FUNIX]strip pathname into components {dir,name,basename,extension}
!!
!!##SYNOPSIS
!!
!!    splitname [NAME1 NAME2 ... |-help|-version]
!!
!!##DESCRIPTION
!!    Output each pathname broken into components dir, name, basename, extension
!!
!!##OPTIONS
!!    NAMES     pathnames to split
!!    -help     display this help and exit
!!    -version  output version information and exit
!!
!!##EXAMPLES
!!
!!    Sample program executions:
!!
!!      splitname /usr/bin/          -> "/usr/bin" "" "" ""
!!      splitname /usr/bin           -> "/usr" "bin" "" ""
!!      splitname stdio.h            -> "" "studio.h" "studio" ".h"
!!
!!##SEE ALSO
!!    splitname(1), basename(1), readlink(1), realpath(1)
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
'@(#)PROGRAM:        splitname(1f)>',&
'@(#)DESCRIPTION:    split file name into components (dir,name,basename,extension)>',&
'@(#)VERSION:        1.0.0>',&
'@(#)DATE:           2017-04-18>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       Mon, Jun 19th, 2017 7:32:37 PM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
end program demosplitname
!===================================================================================================================================
