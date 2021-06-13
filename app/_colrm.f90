!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program colrm
use M_CLI2,  only : set_args, igets
use M_io,    only : getline
use M_verify, only : stderr
implicit none
integer,allocatable          :: columns(:)
character(len=:),allocatable :: line
integer                      :: right
integer                      :: lower
integer                      :: higher
integer                      :: ilen
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
   call setup()                                             ! define help text and version text
   call set_args(' ',help_text, version_text)               ! define command arguments,default values and crack command line
   columns=igets()                                          ! get numbers from command line
   select case(size(columns))
   case(0) ; ALL: do while (getline(line)==0)               ! if no numbers just copy text
                write(*,'(a)')trim(line)
             enddo ALL
   case(1) ; right=max(1, columns(1))                       ! if one number print up to that column
             LEFT: do while (getline(line)==0)
                write(*,'(a)')line(:min(len(line), right-1) )
             enddo LEFT
   case(2) ; lower=max(1, min(columns(1), columns(2) ))     ! if two numbers cut out that range of character columns
             higher=max(1, max(columns(1), columns(2) ))
             INFINITE: do while (getline(line)==0)
                ilen=len_trim(line)
                if(lower.gt.1)then
                   write(*,'(a)',advance='no')line(:min(lower-1, ilen) )
                endif
                if(higher.gt.1)then
                   if(higher+1.le.ilen)then
                      write(*,'(a)',advance='yes')line(higher+1:)
                   else
                      write(*,'(a)')
                   endif
                endif
             enddo INFINITE
   case default
      call stderr('*colrm* incorrect number of values=', size(columns))
   endselect
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   _colrm(1f) - [FUNIX] remove columns from a file                              ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   _colrm [first [last]]                                                        ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   _colrm removes selected character columns from a file. Input is              ',&
'   taken from standard input. Output is sent to standard output.                ',&
'                                                                                ',&
'   If called with one parameter the columns of each line will be                ',&
'   removed starting with the specified first column. If called with             ',&
'   two parameters the columns from the first column to the last                 ',&
'   column will be removed.                                                      ',&
'                                                                                ',&
'   Column numbering starts with column 1. Tabs are NOT expanded.                ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   first      starting column number to remove                                  ',&
'   last       ending column number to remove                                    ',&
'   --version  Display version information and exit.                             ',&
'   --help     Display help text and exit.                                       ',&
'                                                                                ',&
'HISTORY                                                                         ',&
'   The colrm(1) command appeared in 3.0BSD.                                     ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  Samples                                                                       ',&
'                                                                                ',&
'       # trim file so no line is longer than 72 characters                      ',&
'       cat FILENAME|_colrm 73                                                   ',&
'       # remove first three characters in each line                             ',&
'       cat FILENAME|_colrm 1 3                                                  ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'                                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
!>
!!##NAME
!!    _colrm(1f) - [FUNIX] remove columns from a file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    _colrm [first [last]]
!!
!!##DESCRIPTION
!!    _colrm removes selected character columns from a file. Input is
!!    taken from standard input. Output is sent to standard output.
!!
!!    If called with one parameter the columns of each line will be
!!    removed starting with the specified first column. If called with
!!    two parameters the columns from the first column to the last
!!    column will be removed.
!!
!!    Column numbering starts with column 1. Tabs are NOT expanded.
!!
!!##OPTIONS
!!    first      starting column number to remove
!!    last       ending column number to remove
!!    --version  Display version information and exit.
!!    --help     Display help text and exit.
!!
!!##HISTORY
!!    The colrm(1) command appeared in 3.0BSD.
!!
!!##EXAMPLE
!!
!!   Samples
!!
!!        # trim file so no line is longer than 72 characters
!!        cat FILENAME|_colrm 73
!!        # remove first three characters in each line
!!        cat FILENAME|_colrm 1 3
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples            ',&
'PROGRAM:        _colrm(1)                                                       ',&
'DESCRIPTION:    remove a numeric range of characters from stdin                 ',&
'VERSION:        1.0, 20180324                                                   ',&
'AUTHOR:         John S. Urban                                                   ',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html                  ',&
'']
!>
!! PRODUCT:        GPF (General Purpose Fortran) utilities and examples
!! PROGRAM:        _colrm(1)
!! DESCRIPTION:    remove a numeric range of characters from stdin
!!##VERSION:        1.0, 20180324
!! AUTHOR:         John S. Urban
!! HOME PAGE:      http://www.urbanjost.altervista.org/index.html
end subroutine setup
end program colrm
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
