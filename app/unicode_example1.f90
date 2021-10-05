!>
!!##NAME
!!    unicode(7f) - [FORTRAN] UNICODE example
!!
!!##DESCRIPTION
!!  Support for ISO 10646 (Unicode) is not required for Fortran 2003 any
!!  more than it is required for C, but the Standard does specify how it
!!  should be done.
!!
!!  The new intrinsic function SELECTED_CHAR_KIND(3f) can be used to request
!!  ASCII characters or ISO 10646 characters on any processor that supports
!!  them. The ISO 10646 characters that result use the UCS-4 encoding (4
!!  bytes per character); this uses more memory than, say, UCS-2 (which
!!  uses 2 bytes for most characters), but avoids the complication of
!!  surrogates. It also means that the Fortran substring operations and
!!  searching functions will operate correctly and efficiently even on
!!  characters outside the Basic Multilingual Plane.
!!
!!  Recent changes to the Standard have seen the addition of support
!!  for numeric and logical formatting to Unicode characters, conversion
!!  between ASCII, default, and Unicode characters, and support for UTF-8
!!  formatted files. The latter is indicated by the new encoding=specifier,
!!  for example:
!!
!!  open(...,encoding='UTF-8'...)
!!
!!##AUTHOR
!!   Based on an article on "ISO 10646 support", Dr. Dobbs
!!
!!   Malcolm is a principal technical consultant for the Numerical Algorithms
!!   Group, and an active member of the J3 and WG5 Fortran standardization
!!   committees. He can be contacted at malcolmnag.co.uk.
!!
!!##EXAMPLE
!!
!!
!!   HELLO WORLD
!!
!!   Example program 1
!!
!!    program character_kind
!!    use iso_fortran_env
!!    implicit none
!!    integer, parameter :: ascii = selected_char_kind ("ascii")
!!    integer, parameter :: ucs4  = selected_char_kind ('ISO_10646')
!!
!!    character(kind=ascii, len=26) :: alphabet
!!    character(kind=ucs4,  len=30) :: hello_world
!!
!!    alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
!!    hello_world = ucs4_'Hello World and Ni Hao -- ' &
!!                 // char (int (z'4F60'), ucs4)     &
!!                 // char (int (z'597D'), ucs4)
!!
!!    write (*,*) alphabet
!!
!!    open (output_unit, encoding='UTF-8')
!!    write (*,*) trim (hello_world)
!!    end program character_kind
program testit
use iso_fortran_env
implicit none
! ident_1="@(#)an example of the use of the numeric formatting facilities to create a Japanese date stamp."
character(len=*),parameter :: ident = "@(#) print date and time in japanese"
integer, parameter :: ascii = selected_char_kind ("ascii")
integer,parameter :: ucs4 = selected_char_kind("ISO_10646")

character(kind=ascii, len=26) :: alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
character(kind=ucs4,  len=30) :: hello_world
character(len=100,kind=ucs4) :: str
   hello_world = ucs4_'Hello World and Ni Hao -- ' &
      // char (int (z'4F60'), ucs4)                &
      // char (int (z'597D'), ucs4)
   write (*,*) alphabet
   open (output_unit, encoding='UTF-8')
   write (*,*) trim (hello_world)

   call create_date_string(str)
   write(*,*)str

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine create_date_string(str)
   intrinsic date_and_time,selected_char_kind
   integer,parameter :: ucs4 = selected_char_kind("ISO_10646")
   character(kind=ucs4),parameter :: nen = char(int(Z'5e74'),ucs4)
   character(kind=ucs4),parameter :: gatsu = char(int(Z'6708'),ucs4)
   character(kind=ucs4),parameter :: nichi = char(int(Z'65e5'),ucs4)
   character(len= *, kind= ucs4) str
   integer values(8)
       call date_and_time(values=values)
       write(str,10) values(1),nen,values(2),gatsu,values(3),nichi
   10  format(I0,A,I0,A,I0,A)
end subroutine
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end program testit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
