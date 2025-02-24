program roman_values
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
use M_strings, only : upper, int
use M_CLI2, only : set_args, args=>unnamed
use M_roman_numbers, only : &
   assignment(=),  &
   operator(+),    operator(-),   &
   operator(*),    operator(/),   &
   operator(==),   operator(/=),  &
   operator(<),    operator(<=),  &
   operator(>),    operator(>=),  &
   len, print_roman, int, roman_number, roman
implicit none
type(roman) :: num
integer :: i
integer :: ival
character(len=*),parameter :: g='(*(g0))'
character(len=128) :: str
character(len=:),allocatable :: arg
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
   call setup()
   call set_args('',help_text=help_text,version_text=version_text)
   do i=1,size(args)
      arg=trim(upper(args(i)))
      if(verify(arg,'IVXLCDM').eq.0)then
         num=arg
         ival=num
         write(*,g)arg,' => ',ival
      elseif(verify(arg,'0123456789').eq.0)then
         num=int(arg)
         str=num
         write(*,g)arg,' => ',trim(str)
      else
         write(stderr,g)'<ERROR> undecipherable value ',arg, ' ', verify(arg,'IVXLCDM'), ' ', verify(arg,'0123456789')
      endif
   enddo
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   roman(1f) - [FUNIX:CONVERT] display roman numbers as integers and vice versa',&
'                                                                               ',&
'DESCRIPTION                                                                    ',&
'                                                                               ',&
'                                                                               ',&
'SYNOPSIS                                                                       ',&
'    roman [values]|[ --help|--version]                                         ',&
'                                                                               ',&
'DESCRIPTION                                                                    ',&
'   display roman numbers as integers and vice versa                            ',&
'                                                                               ',&
'OPTIONS                                                                        ',&
'                                                                               ',&
'    --version,-v  Print version information on standard output then            ',&
'                  exit successfully.                                           ',&
'    --help,-h     Print usage information on standard output then              ',&
'                  exit successfully.                                           ',&
'EXAMPLE                                                                        ',&
'   Sample commands                                                             ',&
'                                                                               ',&
'    > roman MCMLVII                                                            ',&
'    > MCMLVII => 1957                                                          ',&
'    > roman 1957                                                               ',&
'    > 1957 => MCMLVII                                                          ',&
'                                                                               ',&
'SEE ALSO                                                                       ',&
'    units(1)                                                                   ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        roman(1f)                                           ',&
'DESCRIPTION:    display roman numbers as integers and vice versa    ',&
'VERSION:        1.0, 2025-02-21                                     ',&
'AUTHOR:         John S. Urban                                       ',&
'LICENSE:        MIT                                                 ',&
'']
end subroutine setup
end program roman_values
