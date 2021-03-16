!===================================================================================================================================
program demo_basename
use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
use M_CLI2   , only : set_args, lget,sget, array=>unnamed, specified
use M_strings, only : split
use M_io,      only : basename, getline
implicit none
character(len=:),allocatable     :: help_text(:), version_text(:)
character(len=:),allocatable     :: line, suffix
logical                          :: zero=.false.
   call setup()
!  define command arguments, default values and crack command line
   call set_args(' -zero:z F --suffix:s " " ',help_text,version_text)
   zero=lget('zero')
   suffix=sget('suffix')
   if(size(array).gt.0)then ! filenames on command line
         call printit()
   else  ! no filenames on command line so read from stdin
      open(unit=stdin,pad='yes')
      INFINITE: do while (getline(line)==0)
         call split(line,array)
         call printit()
      enddo INFINITE
   endif
!----------------------------------------------------------------------------------------------------------------------------------
contains
subroutine printit()
integer                          :: i
  do i=1,size(array)                           ! loop thru file-or-directory names
     if(zero)then
        if(specified('suffix'))then
           write(*,'(a)',advance='no')basename(array(i),suffix)//achar(0)
        else
           write(*,'(a)',advance='no')basename(array(i))//achar(0)
        endif
     else
        if(specified('suffix'))then
           write(*,'(a)')basename(array(i),suffix)
        else
           write(*,'(a)')basename(array(i))
        endif
     endif
  enddo
end subroutine printit
!===================================================================================================================================
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   _basename(1f) - [FUNIX:FILESYSTEM] display last component of file name(s)    ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'  syntax:                                                                       ',&
'                                                                                ',&
'   _basename NAME                                                               ',&
'     or                                                                         ',&
'   _basename [NAME...] [ --zero][--suffix SUFFIX]                               ',&
'     or                                                                         ',&
'   _basename -help|-version                                                     ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Print NAME with any leading directory components removed.                    ',&
'   If specified, also remove a trailing SUFFIX.                                 ',&
'                                                                                ',&
'   If no NAME is specified read names from stdin.                               ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'  NAME(S)              filenames                                                ',&
'  -s, --suffix=SUFFIX  remove a trailing SUFFIX.                                ',&
'  -z, --zero           end each output line with NUL, not newline               ',&
'      --help     display this help and exit                                     ',&
'      --version  output version information and exit                            ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
' Typical usage:                                                                 ',&
'                                                                                ',&
'  basename /usr/bin/sort          -> "sort"                                     ',&
'  basename -s .h include/stdio.h  -> "stdio"                                    ',&
'  basename -s '''' include/stdio.h  -> "stdio.h"                                ',&
'  basename any/str1.f90 any/str2   -> "str1" followed by "str2"                 ',&
'                                                                                ',&
'or available locally via: info ''(coreutils) basename invocation''              ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'   basename(1), basename(1), readlink(1), realpath(1)                           ',&
'']
!>
!!##NAME
!!    _basename(1f) - [FUNIX:FILESYSTEM] display last component of file name(s)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   syntax:
!!
!!    _basename NAME
!!      or
!!    _basename [NAME...] [ --zero][--suffix SUFFIX]
!!      or
!!    _basename -help|-version
!!
!!##DESCRIPTION
!!    Print NAME with any leading directory components removed.
!!    If specified, also remove a trailing SUFFIX.
!!
!!    If no NAME is specified read names from stdin.
!!
!!##OPTIONS
!!   NAME(S)              filenames
!!   -s, --suffix=SUFFIX  remove a trailing SUFFIX.
!!   -z, --zero           end each output line with NUL, not newline
!!       --help     display this help and exit
!!       --version  output version information and exit
!!
!!##EXAMPLES
!!
!!  Typical usage:
!!
!!   basename /usr/bin/sort          -> "sort"
!!   basename -s .h include/stdio.h  -> "stdio"
!!   basename -s '' include/stdio.h  -> "stdio.h"
!!   basename any/str1.f90 any/str2   -> "str1" followed by "str2"
!!
!! or available locally via: info '(coreutils) basename invocation'
!!
!!##SEE ALSO
!!    basename(1), basename(1), readlink(1), realpath(1)
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples            ',&
'PROGRAM:        _basename(1f)                                                   ',&
'DESCRIPTION:    display last component of file name                             ',&
'VERSION:        1.0.0                                                           ',&
'DATE:           2015-06-26                                                      ',&
'AUTHOR:         John S. Urban                                                   ',&
'REPORTING BUGS: http://www.urbanjost.altervista.org/                            ',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html                  ',&
'LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.',&
'                There is NO WARRANTY, to the extent permitted by law.           ',&
'']
end subroutine setup
end program demo_basename
!===================================================================================================================================
