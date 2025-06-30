










!>
!!##NAME
!!    M_strings__chars(3f) - [M_strings__chars::INTRO] Fortran character module
!!
!!##DESCRIPTION
!!    Fortran allows for processing character data as strings, or as arrays
!!    of single characters or even as integer values.
!!
!!    M_strings__chars(3f) is a collection of functions that handle character variable
!!    arrays of len=1, which is more familiar for some programmers.
!!
!!##SYNOPSIS
!!
!!  public entities:
!!
!!      use M_strings__chars,only : toupper, tolower
!!
!!   CASE
!!
!!       toupper         function converts character array to uppercase
!!       tolower         function converts character array to lowercase
!!
!!##EXAMPLES
!!
!!
!!    Each of the procedures includes an example program in the corresponding
!!    man(1) page for the function.
!!
!!    Sample program:
!!
!!        program demo_M_strings__chars
!!        use M_strings__chars,   only : toupper, tolower
!!        implicit none
!!        integer,parameter  :: bytes=80
!!        character          :: string*(bytes)
!!        character          :: lets(bytes)
!!        equivalence (string,lets)
!!           string='Do unto Others'
!!           write(*,*)toupper(lets)
!!           write(*,*)tolower(lets)
!!        end program demo_M_strings__chars
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    MIT

module M_strings__chars
implicit none

! ident_1="@(#) M_strings__chars(3f) Fortran module containing routines that deal with character strings"

private

!-------------------------# CASE
public toupper            !  function converts string to uppercase
public tolower            !  function converts string to lowercase
!-------------------------#
contains

!>
!!##NAME
!!    toupper(3f) - [M_strings__chars:CASE] changes a character array to uppercase
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    pure function toupper(str) result (string)
!!
!!     character(len=1),intent(in) :: str(:)
!!     character(len=1)            :: string(size(str))  ! output string
!!
!!##DESCRIPTION
!!      toupper(string) returns a copy of the input characters with all
!!      characters converted to uppercase, assuming ASCII character sets
!!      are being used.
!!
!!##OPTIONS
!!    str    array of characters to convert to uppercase
!!
!!##RETURNS
!!    toupper  copy of the input array with all characters converted to
!!           uppercase.
!!
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_toupper
!!       use M_strings__chars, only: toupper
!!       implicit none
!!       character(len=1),allocatable :: s(:)
!!          s=transfer(' ABCDEFG abcdefg ','A',size=17)
!!          write(*,*) 'mixed-case input string is ....',s
!!          write(*,*) 'upper-case output string is ...',toupper(s)
!!       end program demo_toupper
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain

pure function toupper(str) result (string)

! ident_2="@(#) M_strings__chars toupper(3f) returns copy of input array accept lowercase letters are converted to uppercase characters"

character(len=1),intent(in) :: str(:)
character(len=1)            :: string(size(str))
   associate(a=>iachar(str))
    string=char(merge(a-32,a,a>=97.and.a<=122))
   end associate
end function toupper

!>
!!##NAME
!!    tolower(3f) - [M_strings__chars:CASE] changes a string to lowercase over
!!    specified range
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    pure function tolower(str,begin,end) result (string)
!!
!!     character(len=1),intent(in) :: str(:)
!!     character(len=1)            :: string(size(str)
!!
!!##DESCRIPTION
!!      tolower(str) returns a copy of the ASCII character array with all
!!      characters converted to miniscule (ie. "lowercase")
!!
!!##OPTIONS
!!    str    character array to convert to miniscule
!!
!!##RETURNS
!!    tolower  copy of the input array with all characters converted to
!!             lowercase.
!!
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_tolower
!!       use M_strings__chars, only: tolower
!!       implicit none
!!       character(len=1),allocatable  :: s(:)
!!          s=transfer(' ABCDEFG abcdefg ','A',size=17)
!!          write(*,*) 'mixed-case input string is ....',s
!!          write(*,*) 'lower-case output string is ...',tolower(s)
!!       end program demo_tolower
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    MIT

pure function tolower(str) result (string)

! ident_3="@(#) M_strings__chars tolower(3f) returns copy of input array accept uppercase letters are converted to lowercase characters"

character(len=1),intent(in) :: str(:)
character(len=1)            :: string(size(str))
   associate(a=>iachar(str))
    string=char(merge(a+32,a,a>=65.and.a<=90))
   end associate
end function tolower

end module M_strings__chars
