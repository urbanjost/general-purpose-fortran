program demosplitname
use M_kracken, only : kracken, sgets, lget                  ! add command-line parser module
use M_strings, only : split
use M_io,      only : splitpath
implicit none
logical                          :: zero=.false.
character(len=4096),allocatable  :: array(:)
character(len=4096)              :: dir,name,basename,ext,input_name
integer                          :: i, ios
logical                          :: stdin=.false.
logical                          :: d, b, l, e
!-----------------------------------------------------------------------------------------------------------------------------------
!  define command arguments, default values and crack command line
   call kracken('splitname','-d .f. -l .f. -b .f. -e .f. -help .false. -version .false. -zero .false. -stdin .false.')
   call help_usage(lget('splitname_help'))        ! process -help switch
   call help_version(lget('splitname_version'))   ! process -version switch
   zero=lget('splitname_zero')                    ! get -zero option
   array=sgets('splitname_oo')                    ! get -oo STRING, split on space character into array
   stdin=lget('splitname_stdin')                  ! read names from stdin
   d=lget('splitname_d')                          ! get -d option
   b=lget('splitname_b')                          ! get -b option
   l=lget('splitname_l')                          ! get -l option
   e=lget('splitname_e')                          ! get -e option
!----------------------------------------------------------------------------------------------------------------------------------
   do i=1,size(array)                             ! loop thru file-or-directory names
      call splitpath(array(i),dir,name,basename,ext)
      call printit()
   enddo
   if(stdin)then
      INFINITE: do
         read(*,'(a)',iostat=ios)input_name
         if(ios.ne.0)exit INFINITE
         call splitpath(input_name,dir,name,basename,ext)
         call printit()
      enddo INFINITE
   endif
!----------------------------------------------------------------------------------------------------------------------------------
contains
subroutine printit()
logical :: printed
   printed=.false.
   if(d) then
      write(*,101,advance='no')trim(dir)
      printed=.true.
   endif
   if(l) then
      write(*,101,advance='no')trim(name)
      printed=.true.
   endif
   if(b) then
      write(*,101,advance='no')trim(basename)
      printed=.true.
   endif
   if(e) then
      write(*,101,advance='no')trim(ext)
      printed=.true.
   endif
   if(.not.printed)then
      write(*,101)trim(dir),trim(name),trim(basename),trim(ext)
   else
      write(*,*)
   endif
   101 format(*('"',a,'" ':))
end subroutine printit
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
'NAME                                                                                                                            ',&
'   splitname(1f) - [FUNIX] strip pathname into components {dir,name,basename,extension}                                         ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'   splitname [NAME1 NAME2 ... |[ -d -b -l -e]|-help|-version]                                                                   ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Output each pathname broken into components directory, leaf name, basename, extension                                        ',&
'                                                                                                                                ',&
'   Output is always in the form and order                                                                                       ',&
'                                                                                                                                ',&
'      "dir" "name" "basename" "ext"                                                                                             ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   NAMES      pathnames to split                                                                                                ',&
'   -d         print director name                                                                                               ',&
'   -l         print leaf name                                                                                                   ',&
'   -b         print base name sans any suffix                                                                                   ',&
'   -e         print suffix extension                                                                                            ',&
'   --stdin    flag to read pathnames from stdin instead of command line                                                         ',&
'   --help     display this help and exit                                                                                        ',&
'   --version  output version information and exit                                                                               ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'   Sample program executions:                                                                                                   ',&
'                                                                                                                                ',&
'     splitname /usr/bin/          -> "/usr/bin" "" "" ""                                                                        ',&
'     splitname /usr/bin           -> "/usr" "bin" "" ""                                                                         ',&
'     splitname stdio.h            -> "" "studio.h" "studio" ".h"                                                                ',&
'                                                                                                                                ',&
'SEE ALSO                                                                                                                        ',&
'   dirname(1), basename(1), readlink(1), realpath(1)                                                                            ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    splitname(1f) - [FUNIX] strip pathname into components {dir,name,basename,extension}
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    splitname [NAME1 NAME2 ... |[ -d -b -l -e]|-help|-version]
!!
!!##DESCRIPTION
!!    Output each pathname broken into components directory, leaf name, basename, extension
!!
!!    Output is always in the form and order
!!
!!       "dir" "name" "basename" "ext"
!!
!!##OPTIONS
!!    NAMES      pathnames to split
!!    -d         print director name
!!    -l         print leaf name
!!    -b         print base name sans any suffix
!!    -e         print suffix extension
!!    --stdin    flag to read pathnames from stdin instead of command line
!!    --help     display this help and exit
!!    --version  output version information and exit
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
!!    dirname(1), basename(1), readlink(1), realpath(1)
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
'@(#)PROGRAM:        splitname(1f)>',&
'@(#)DESCRIPTION:    split file name into components (dir,name,basename,extension)>',&
'@(#)VERSION:        1.0.0>',&
'@(#)DATE:           2017-04-18>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2022-12-18 00:49:55 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program demosplitname
!===================================================================================================================================
