










!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_big_integer

!>
!!##NAME
!!    M_big_integer(3fm) - [M_big_integer] define new data type BIG_INTEGER and operators
!!##SYNOPSIS
!!
!!   use M_big_integer
!!##DESCRIPTION
!!
!!    The module named BIG_INTEGERS defines a new data type BIG_INTEGER.
!!    This data type represents nonnegative integers up to 10**n - 1, where n
!!    is the parameter (named constant) NR_OF_DECIMAL_DIGITS. This value may
!!    be changed, but the module must then be recompiled (it is not dynamic).
!!
!!    The following operations are implemented. "b" represents a big integer,
!!    "c" a character string, and "i" an ordinary integer.
!!
!!      big (i)
!!      big (c)
!!      int (b)
!!      int (c)
!!      char (b)
!!      char (i)
!!
!!      b = i
!!      b = c
!!      i = b
!!      i = c
!!      c = b
!!      c = i
!!
!!      b ? i, i ? b, and b ? b, where ? is
!!        +, -, *, /,
!!        <, <=, >, >=, ==, or /=
!!
!!      b ** i
!!
!!      modulo (b, i)  [result is integer]
!!      modulo (i, b)  [result is big_integer]
!!      modulo (b, b)  [result is big_integer]
!!
!!      huge (b)
!!      sqrt (b)
!!
!!      call print_big (b)
!!      call random_number (b, low, high)
!!
!!   Many operations of the form b ? i, where i < base, are implemented to
!!   be efficient as special cases.
!!
!!##AUTHOR
!!   Copyright (c) 1993-2002 Unicomp, Inc.
!!
!!   Developed at Unicomp, Inc.
!!
!!   Permission to use, copy, modify, and distribute this
!!   software is freely granted, provided that this notice
!!   is preserved.
!!
!!##EXAMPLE
!!
!!
!!   Sample program:
!!
!!    program demo_M_big_integer
!!    use M_big_integer
!!    type(big_integer) :: a, b
!!    a = "1234567890"
!!    b = a ** 34
!!    call print_big(b)
!!    end program demo_M_big_integer
!!
!!   Expected output:
!!
!!    18044389448552283571495419217299928002884571753268277623634094010253!!    66981452225691084844631888351415012575687866144963748904906312371105!!    80592001682129147574651845715171456148359301092015447205623057495772!!    65956402721330118223207623859033190068180607802717874097649095503332!!    13108684548203091283587744857909677706830329440243525585391247886790!!    67434454263656340662912379366253271751953736731074819372491000795297!!    39485301010583702548434613939308992953505848626082883004863432319153!!    74851257117575530170864942620865071745761606428864154243365627850016!!    44174485982226421972489721110767356064962137106505728778418700556641!!    02307658653980050645991117790555490389944390204219987434165392204913!!    97208851608204247050595317024494964141522065839042524403351250735123!!    55264351679192059781951740756716496372272101373104569806788535169770!!    01992757833390412200073266324230837178629444544469456556334359024793!!    85520866582032929720704074267136863063443220583328656131024989866204!!    73134625473086906778038872631750464721441869000000000000000000000000!!    00000000000000000000000000000000000000000000000000000000000000000000!!    0000000000000000000000000000000

   implicit none

   public :: big
   public :: int
   public :: char
   public :: print_big
   public :: assignment (=)
   public :: operator (+)
   public :: operator (-)
   public :: operator (*)
   public :: operator (/)
   public :: operator (**)
   public :: modulo
   public :: huge
   public :: sqrt
   public :: random_number
   public :: operator (==)
   public :: operator (/=)
   public :: operator (<=)
   public :: operator (<)
   public :: operator (>=)
   public :: operator (>)
   public test_suite_M_big_integer

   private :: big_gets_int, &
              big_int, &
              big_gets_char, &
              big_char, &
              int_gets_big, &
              int_big, &
              int_gets_char, &
              int_char, &
              char_gets_big, &
              char_big, &
              char_gets_int, &
              char_int

   private :: big_plus_int, &
              int_plus_big, &
              big_plus_big, &
              big_minus_int, &
              int_minus_big, &
              big_minus_big, &
              big_times_int, &
              int_times_big, &
              big_times_big, &
              big_div_int, &
              int_div_big, &
              big_div_big, &
              big_power_int, &
              modulo_big_int, &
              modulo_int_big, &
              modulo_big_big

   private :: big_eq_int, &
              int_eq_big, &
              big_eq_big, &
              big_ne_int, &
              int_ne_big, &
              big_ne_big, &
              big_le_int, &
              int_le_big, &
              big_le_big, &
              big_ge_int, &
              int_ge_big, &
              big_ge_big, &
              big_lt_int, &
              int_lt_big, &
              big_lt_big, &
              big_gt_int, &
              int_gt_big, &
              big_gt_big

   private :: huge_big, &
              big_base_to_power, &
              print_big_base, &
              sqrt_big, &
              msd, &
              random_number_big

   intrinsic :: char
   intrinsic :: int
   intrinsic :: modulo
   intrinsic :: huge
   intrinsic :: sqrt
   intrinsic :: random_number
   intrinsic :: radix
   intrinsic :: digits

   ! This indicates the maximum number of decimal digits
   ! that a big integer may contain.

   integer, parameter, public :: nr_of_decimal_digits = 500

   ! If the radix (returned by "radix(0)" of the integers on
   ! your system is not 2 change the following constant to
   ! the logarithm in the base 10 of the radix: log10(radix)

   real, parameter, private :: log_base_10_of_radix = 0.30103

   integer, parameter, private :: &
         d = digits (0) / 2, &
         r = radix (0), &
         base = r ** d, &
         nr_of_digits = nr_of_decimal_digits / (log_base_10_of_radix * d) + 1

   ! The base of the number system is r ** d,
   ! so that each "digit" is 0 to r**d - 1

   type, public :: big_integer
      private
      integer, dimension (0 : nr_of_digits) :: digit
   end type big_integer

   interface big
      module procedure big_char, &
                       big_int
   end interface

   interface int
      module procedure int_char, &
                       int_big
   end interface

   interface char
      module procedure char_big, &
                       char_int
   end interface

   interface assignment (=)
      module procedure big_gets_int, &
                       big_gets_char, &
                       int_gets_big, &
                       int_gets_char, &
                       char_gets_big, &
                       char_gets_int
   end interface

   interface operator (+)
      module procedure big_plus_int, &
                       big_plus_big
   end interface

   interface operator (-)
      module procedure big_minus_int, &
                       int_minus_big, &
                       big_minus_big
   end interface

   interface operator (*)
      module procedure big_times_int, &
                       int_times_big, &
                       big_times_big
   end interface

   interface operator (/)
      module procedure big_div_int, &
                       int_div_big, &
                       big_div_big
   end interface

   interface operator (**)
      module procedure big_power_int
   end interface

   interface modulo
      module procedure modulo_big_int, &
                       modulo_int_big, &
                       modulo_big_big
   end interface

   interface operator (==)
      module procedure big_eq_int, &
                       int_eq_big, &
                       big_eq_big
   end interface

   interface operator (/=)
      module procedure big_ne_int, &
                       int_ne_big, &
                       big_ne_big
   end interface

   interface operator (<=)
      module procedure big_le_int, &
                       int_le_big, &
                       big_le_big
   end interface

   interface operator (>=)
      module procedure big_ge_int, &
                       int_ge_big, &
                       big_ge_big
   end interface

   interface operator (<)
      module procedure big_lt_int, &
                       int_lt_big, &
                       big_lt_big
   end interface

   interface operator (>)
      module procedure big_gt_int, &
                       int_gt_big, &
                       big_gt_big
   end interface

   interface huge
      module procedure huge_big
   end interface

   interface sqrt
      module procedure sqrt_big
   end interface

   interface random_number
      module procedure random_number_big
   end interface

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_char (c) result (b)

   character (len=*), intent (in) :: c
   type (big_integer) :: b
   integer :: temp_digit, n

   if (len (c) > nr_of_decimal_digits) then
      b = huge (b)
      return
   end if
   b % digit = 0
   do n = 1, len (c)
      temp_digit = index ("0123456789", c (n:n)) - 1
      if (temp_digit < 0) then
         b = huge (b)
      end if
      b = b * 10 + temp_digit
   end do

end function big_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure subroutine big_gets_char (b, c)

   type (big_integer), intent (out) :: b
   character (len=*), intent (in) :: c

   b = big_char (trim (c))

end subroutine big_gets_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_int (i) result (b)

   integer, intent (in) :: i
   type (big_integer) :: b
   integer :: temp_i, n

   if (i < 0) then
      b = huge (b)
   end if

   b % digit = 0
   temp_i = i
   do n = 0, nr_of_digits - 1
      if (temp_i == 0) then
         return
      end if
      b % digit (n) = modulo (temp_i, base)
      temp_i = temp_i / base
   end do

   if (temp_i /= 0) then
      b = huge (b)
   end if

end function big_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure subroutine big_gets_int (b, i)

   type (big_integer), intent (out) :: b
   integer, intent (in) :: i

   b = big (i)

end subroutine big_gets_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_char (c) result (i)

   character (len=*), intent (in) :: c
   integer :: i

   i = int (big (trim (c)))

end function int_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure subroutine int_gets_char (i, c)

   integer, intent (out) :: i
   character (len=*), intent (in) :: c

   i = int_char (trim (c))

end subroutine int_gets_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_big (b) result (i)

   type (big_integer), intent (in) :: b
   integer :: i

   if (msd (b) > 1) then
      i = huge (i)
   else
      i = base * b % digit (1) + b % digit (0)
   end if

end function int_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure subroutine int_gets_big (i, b)

   integer, intent (out) :: i
   type (big_integer), intent (in) :: b

   i = int (b)

end subroutine int_gets_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function char_big (b) result (c)

   type (big_integer), intent (in) :: b
   character (len=nr_of_decimal_digits+9) :: c
   type (big_integer) :: temp_big
   integer :: n, remainder
   character (len = *), parameter :: digit_chars = "0123456789"

   temp_big = b
   c = repeat (" ", len(c)-1) // "0"
   do n = len (c), 1, -1
      if (temp_big == 0) then
         exit
      end if
      remainder = modulo (temp_big, 10) + 1
      temp_big = temp_big / 10
      c (n:n) = digit_chars (remainder:remainder)
   end do

   c = adjustl (c)

end function char_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure subroutine char_gets_big (c, b)

   type (big_integer), intent (in) :: b
   character (len=*), intent (out) :: c

   c = char (b)

end subroutine char_gets_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function char_int (i) result (c)

   integer, intent (in) :: i
   character (len=nr_of_decimal_digits+9) :: c

   c = big (i)

end function char_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure subroutine char_gets_int (c, i)

   integer, intent (in) :: i
   character (len=*), intent (out) :: c

   c = big (i)

end subroutine char_gets_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function msd (x) result (msd_result)

! Find most significant digit of x

   type (big_integer), intent (in) :: x
   integer :: msd_result
   integer :: n

   do n = nr_of_digits, 0, -1
      if (x % digit (n) /= 0) then
         msd_result = n
         return
      end if
   end do

   msd_result = -1

end function msd
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_plus_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   type (big_integer) :: bi
   integer :: n, summ, carry

   if (i < base) then
      carry = i
      do n = 0, nr_of_digits - 1
         summ = b % digit (n) + carry
         bi % digit (n) = modulo (summ, base)
         carry = summ / base
         if (carry == 0) then
            bi % digit (n+1:) = b % digit (n+1:)
            return
         end if
      end do
      if (n==nr_of_digits) then
         bi = huge (bi)
      end if
   else
      bi = b + big (i)
   end if

end function big_plus_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_plus_big (i, b) result (bi)

   integer, intent (in) :: i
   type (big_integer), intent (in) :: b
   type (big_integer) :: bi

   bi = b + i

end function int_plus_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_plus_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   type (big_integer) :: bb
   integer :: carry, temp_digit, n, m

   carry = 0
   m = max (msd (x), msd (y))
   do n = 0, m
      temp_digit = &
         x % digit (n) + y % digit (n) + carry
      bb % digit (n) = modulo (temp_digit, base)
      carry = temp_digit / base
   end do

   bb % digit (m+1) = carry
   bb % digit (m+2:nr_of_digits) = 0
   if (bb % digit (nr_of_digits) /= 0) then
      bb = huge (bb)
   end if

end function big_plus_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_minus_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   type (big_integer) :: bi
   integer :: n, borrow, diff, msdb

   bi % digit = 0
   msdb = msd (b)
   if (msdb<1 .and. b % digit (0) < i) then
      return
   end if

   if (i < base) then
      borrow = i
      do n = 0, nr_of_digits - 1
         diff = b % digit (n) - borrow
         bi % digit (n) = modulo (diff, base)
         borrow = (base - diff) / base
         if (borrow == 0) then
            bi % digit (n+1:msdb) = b % digit (n+1:msdb)
            return
         end if
      end do
   else
      bi = b - big (i)
   end if

end function big_minus_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_minus_big (i, b) result (ib)

   integer, intent (in) :: i
   type (big_integer), intent (in) :: b
   type (big_integer) :: ib

   ib = big (i) - b

end function int_minus_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_minus_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   type (big_integer) :: bb
   type (big_integer) :: temp_big
   integer :: n

   temp_big = x
   do n = 0, nr_of_digits - 1
      bb % digit (n) = temp_big % digit (n) - y % digit (n)
      if (bb % digit (n) < 0) then
         bb % digit (n) = bb % digit (n) + base
         temp_big % digit (n + 1) = temp_big % digit (n + 1) - 1
      end if
   end do

   if (temp_big % digit (nr_of_digits) < 0) then
      bb % digit = 0
   else
      bb % digit (nr_of_digits) = 0
   end if

end function big_minus_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_times_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   type (big_integer) :: bi
   integer :: ib, prod, carry

   if (i < base) then
      bi % digit = 0
      carry = 0
      do ib = 0, msd (b)
         prod = b % digit (ib) * i + carry
         bi % digit (ib) = modulo (prod, base)
         carry = prod / base
      end do
      if (ib==nr_of_digits .and. carry /= 0) then
         bi = huge (bi)
      else
         bi % digit (ib) = carry
      end if
   else
      bi = b * big (i)
   end if

end function big_times_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_times_big (i, b) result (bi)

   integer, intent (in) :: i
   type (big_integer), intent (in) :: b
   type (big_integer) :: bi

   bi = b * i

end function int_times_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_times_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   type (big_integer) :: bb

   integer :: ix, iy, ib, carry, prod

   bb % digit = 0

   do ix = 0, msd (x)
      carry = 0
      ib = ix
      do iy = 0, msd (y)
         prod = x % digit (ix) * y % digit (iy) + bb % digit (ib) + carry
         carry = prod / base
         bb % digit (ib) = modulo (prod, base)
         if (ib == nr_of_digits) then
            bb = huge (bb)
            return
         end if
         ib = ib + 1
      end do
      bb % digit (ib) = bb % digit (ib) + carry
   end do

end function big_times_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_base_to_power (n)  result (b)

   integer, intent (in) :: n
   type (big_integer) :: b

   if (n < 0) then
     b = 0
   else if (n >= nr_of_digits) then
      b = huge (b)
   else
      b % digit = 0
      b % digit (n) = 1
   end if

end function big_base_to_power
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_div_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   type (big_integer) :: bi
   integer :: n, temp_int, remainder

   if (i == 0) then
      bi = huge (bi)
   else if (i < base) then
      bi % digit = 0
      remainder = 0
      do n = msd(b), 0, -1
         temp_int = base * remainder + b % digit (n)
         bi % digit (n) = temp_int / i
         remainder = modulo (temp_int, i)
      end do
   else
      bi = b / big (i)
   end if

end function big_div_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_div_big (i, b) result (ib)

   integer, intent (in) :: i
   type (big_integer), intent (in) :: b
   type (big_integer) :: ib

   ib = big (i) / b

end function int_div_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_div_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   type (big_integer) :: bb

   type (big_integer) :: tx, ty

   integer :: msdx, msdy, ix, iy
   integer :: v1, v2, u0, u1, u2
   integer :: dd, bi, car, bar, prd

   if (y == 0) then
      bb = huge (bb)
      return
   end if

   msdx = msd(x)
   msdy = msd(y)

   if (msdy == 0) then
      bb = x / y % digit (0)
      return
   end if

   bb % digit = 0

   if (msdy < msdy) then
      return
   end if

   tx = x
   ty = y

   car = 0
   bar = 0
   prd = 0
   dd = base / (ty % digit (msdy) + 1)
   if (dd /= 1) then
      do ix = 0, msdx
         tx % digit (ix) = tx % digit (ix) * dd + car
         car = tx % digit (ix) / base
         tx % digit (ix) = tx % digit (ix) - base * car
      end do
      tx % digit (msdx+1) = car
      car = 0
      do iy = 0, msdy
         ty % digit (iy) = ty % digit (iy) * dd + car
         car = ty % digit (iy) / base
         ty % digit (iy) = ty % digit (iy) - base * car
      end do
   end if

   msdx = msdx + 1

   v1 = ty % digit (msdy)
   v2 = ty % digit (msdy-1)
   bb % digit = 0

   do msdx = msdx, msdy + 1, -1

      u0 = tx % digit (msdx)
      u1 = tx % digit (msdx-1)
      u2 = tx % digit (msdx-2)

      if (u0 == v1) then
         bi = base - 1
      else
         bi = (u0*base + u1) / v1
      end if

      do
         if (v2*bi <= (u0*base + u1 - bi*v1) * base + u2) then
            exit
         end if
         bi = bi - 1
      end do

      if (bi > 0) then
         car = 0
         bar = 0
         ix = msdx - msdy - 1
         do iy = 0, msdy
            prd = bi * ty % digit (iy) + car
            car = prd / base
            prd = prd - base * car
            tx % digit (ix) = tx % digit (ix) - (prd + bar)
            if (tx % digit (ix) < 0) then
               bar = 1
               tx % digit (ix) = tx % digit (ix) + base
            else
               bar = 0
            end if
            ix = ix + 1
         end do
         if (tx % digit (msdx) < car + bar) then
            car = 0
            bi = bi -1
            ix = msdx - msdy - 1
            do iy = 0, msdy
               tx % digit (ix) = tx % digit (ix) + ty % digit (iy) + car
               if (tx % digit (ix) > base) then
                  car = 1
                  tx % digit (ix) = tx % digit (ix) - base
               else
                  car = 0
               end if
               ix = ix + 1
            end do
         end if
      end if
      tx % digit (msdx) = 0
      bb % digit (1:nr_of_digits) = bb % digit (0:nr_of_digits-1)
      bb % digit (0) = bi
   end do

end function big_div_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function modulo_big_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   integer :: bi
   integer :: n

   if (i == 0) then
      bi = huge (bi)
   else if (i < base) then
      bi = 0
      do n = msd(b), 0, -1
         bi = modulo (base * bi + b % digit (n), i)
      end do
   else
      bi = modulo (b, big (i))
   end if

end function modulo_big_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function modulo_int_big (ii, b) result (ib)

   integer, intent (in) :: ii
   type (big_integer), intent (in) :: b
   type (big_integer) :: ib

   ib = modulo (big (ii), b)

end function modulo_int_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function modulo_big_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   type (big_integer) :: bb

   bb = x - x / y * y

end function modulo_big_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_eq_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = int (b) == i

end function big_eq_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_eq_big (i, b) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = int (b) == i

end function int_eq_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_eq_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   logical :: bb

   bb = all (x % digit == y % digit)

end function big_eq_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_ne_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = int (b) /= i

end function big_ne_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_ne_big (i, b) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = int (b) /= i

end function int_ne_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_ne_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   logical :: bb

   bb = any (x % digit /= y % digit)

end function big_ne_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_le_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = int (b) <= i

end function big_le_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_le_big (i, b) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = i <= int (b)

end function int_le_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_le_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   logical :: bb
   integer :: n

   bb = .true.
   do n = nr_of_digits, 0, -1
      if (x % digit (n) /= y % digit (n)) then
         bb = (x % digit (n) < y % digit (n))
         exit
      end if
   end do

end function big_le_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_gt_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = int (b) > i

end function big_gt_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_gt_big (i, b) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = i > int (b)

end function int_gt_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_gt_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   logical :: bb
   integer :: n

   bb = .true.
   do n = nr_of_digits, 0, -1
      if (x % digit (n) /= y % digit (n)) then
         bb = (x % digit (n) < y % digit (n))
         exit
      end if
   end do

   bb = .not. bb

end function big_gt_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_lt_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = int (b) < i

end function big_lt_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_lt_big (i, b) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = i < int (b)

end function int_lt_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_lt_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   logical :: bb
   integer :: n

   bb = .false.
   do n = nr_of_digits, 0, -1
      if (x % digit (n) /= y % digit (n)) then
         bb = (x % digit (n) < y % digit (n))
         exit
      end if
   end do

end function big_lt_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_ge_int (b, i) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = int (b) >= i

end function big_ge_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function int_ge_big (i, b) result (bi)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   logical :: bi

   bi = i >= int (b)

end function int_ge_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function big_ge_big (x, y) result (bb)

   type (big_integer), intent (in) :: x, y
   logical :: bb
   integer :: n

   bb = .false.
   do n = nr_of_digits, 0, -1
      if (x % digit (n) /= y % digit (n)) then
         bb = (x % digit (n) < y % digit (n))
         exit
      end if
   end do

   bb = .not. bb

end function big_ge_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function huge_big (b) result (hb)

   type (big_integer), intent (in) :: b
   type (big_integer) :: hb

   hb % digit (0) = b % digit (0) ! to avoid diagnostic
   hb % digit = base - 1
   hb % digit (nr_of_digits) = 0

end function huge_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure function sqrt_big (b) result (sb)

   type (big_integer), intent (in) :: b
   type (big_integer) :: sb
   type (big_integer) :: old_sqrt_big, new_sqrt_big
   integer :: i, n

   n = -1
   do i = nr_of_digits, 0, -1
      if (b % digit (i) /= 0) then
         n = i
         exit
      end if
   end do

   if (n == -1) then
      sb = 0
   else if (n == 0) then
      sb = int (sqrt (real (b % digit (0))))
   else
      old_sqrt_big = 0
      if (modulo (n, 2) == 0) then
         old_sqrt_big % digit (n / 2) = int (sqrt (real (b % digit (n))))
      else
         old_sqrt_big % digit ((n - 1) / 2) =  &
               int (sqrt (real (base * b % digit (n) + b % digit (n-1))))
      end if

      do
         new_sqrt_big = (old_sqrt_big + b / old_sqrt_big) / 2
         if (new_sqrt_big == old_sqrt_big .or.  &
             new_sqrt_big == old_sqrt_big + 1 .or.  &
             new_sqrt_big == 0) then
            exit
         else
            old_sqrt_big = new_sqrt_big
         end if
      end do
      sb = old_sqrt_big
   end if

end function sqrt_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
recursive function big_power_int (b, i)  &
      result (big_power_int_result)

   type (big_integer), intent (in) :: b
   integer, intent (in) :: i
   type (big_integer) :: big_power_int_result
   type (big_integer) :: temp_big

   if (i <= 0) then
      big_power_int_result = 1
   else
      temp_big = big_power_int (b, i / 2)
      if (modulo (i, 2) == 0) then
         big_power_int_result = temp_big * temp_big
      else
         big_power_int_result = temp_big * temp_big * b
      end if
   end if

end function big_power_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine print_big (b)

   type (big_integer), intent (in) :: b

   write (unit = *, fmt = "(a)", advance = "no") trim (char (b))

end subroutine print_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine print_big_base (b)

   type (big_integer), intent (in) :: b
   integer :: n

   print *, "base: ", base
   do n = nr_of_digits, 1, -1
      if (b % digit (n) /= 0) then
         exit
      end if
   end do
   print "(10i9)", b % digit (n:0:-1)

end subroutine print_big_base
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine random_number_big (r, low, high)

!  Generate by linear congruence x' = ax + c mod m
!  where m is huge (b) + 1 = base ** nr_of_digits

   type (big_integer), intent (out) :: r
   type (big_integer), intent (in) :: low, high
   integer :: n, i, carry, prod, summ
   type (big_integer), save :: x = big_integer ( (/ (1, i=0,nr_of_digits-1), 0 /) )
   type (big_integer), parameter :: h = big_integer ( (/ (base-1, i=0,nr_of_digits-1), 0 /) )
   integer, parameter :: a = 16907, c = 8191

!  Multiply by a
   carry = 0
   do n = 0, nr_of_digits - 1
      prod = x % digit (n) * a + carry
      x % digit (n) = modulo (prod, base)
      carry = prod / base
   end do

!  Add c
   carry = c
   do n = 0, nr_of_digits - 1
      summ = x % digit (n) + carry
      x % digit (n) = modulo (summ, base)
      carry = summ / base
   end do

   r = x / (h / (high -low + 1)) + low

end subroutine random_number_big
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_big_integer()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_big_char()
   call test_big_div_big()
   call test_big_div_int()
   call test_big_eq_big()
   call test_big_eq_int()
   call test_big_ge_big()
   call test_big_ge_int()
   call test_big_gets_char()
   call test_big_gets_int()
   call test_big_gt_big()
   call test_big_gt_int()
   call test_big_int()
   call test_big_le_big()
   call test_big_le_int()
   call test_big_lt_big()
   call test_big_lt_int()
   call test_big_minus_big()
   call test_big_minus_int()
   call test_big_ne_big()
   call test_big_ne_int()
   call test_big_plus_big()
   call test_big_plus_int()
   call test_big_power_int()
   call test_big_times_big()
   call test_big_times_int()
   call test_char_big()
   call test_char_gets_big()
   call test_char_gets_int()
   call test_char_int()
   call test_huge_big()
   call test_int_big()
   call test_int_char()
   call test_int_div_big()
   call test_int_eq_big()
   call test_int_ge_big()
   call test_int_gets_big()
   call test_int_gets_char()
   call test_int_gt_big()
   call test_int_le_big()
   call test_int_lt_big()
   call test_int_minus_big()
   call test_int_ne_big()
   call test_int_times_big()
   call test_modulo_big_big()
   call test_modulo_big_int()
   call test_modulo_int_big()
   call test_print_big()
   call test_random_number_big()
   call test_sqrt_big()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_char()

   call unit_check_start('big_char',msg='')
   !!call unit_check('big_char', 0.eq.0, 'checking', 100)
   call unit_check_done('big_char',msg='')
end subroutine test_big_char
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_div_big()

   call unit_check_start('big_div_big',msg='')
   !!call unit_check('big_div_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_div_big',msg='')
end subroutine test_big_div_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_div_int()

   call unit_check_start('big_div_int',msg='')
   !!call unit_check('big_div_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_div_int',msg='')
end subroutine test_big_div_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_eq_big()

   call unit_check_start('big_eq_big',msg='')
   !!call unit_check('big_eq_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_eq_big',msg='')
end subroutine test_big_eq_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_eq_int()

   call unit_check_start('big_eq_int',msg='')
   !!call unit_check('big_eq_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_eq_int',msg='')
end subroutine test_big_eq_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_ge_big()

   call unit_check_start('big_ge_big',msg='')
   !!call unit_check('big_ge_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_ge_big',msg='')
end subroutine test_big_ge_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_ge_int()

   call unit_check_start('big_ge_int',msg='')
   !!call unit_check('big_ge_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_ge_int',msg='')
end subroutine test_big_ge_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_gets_char()

   call unit_check_start('big_gets_char',msg='')
   !!call unit_check('big_gets_char', 0.eq.0, 'checking', 100)
   call unit_check_done('big_gets_char',msg='')
end subroutine test_big_gets_char
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_gets_int()

   call unit_check_start('big_gets_int',msg='')
   !!call unit_check('big_gets_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_gets_int',msg='')
end subroutine test_big_gets_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_gt_big()

   call unit_check_start('big_gt_big',msg='')
   !!call unit_check('big_gt_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_gt_big',msg='')
end subroutine test_big_gt_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_gt_int()

   call unit_check_start('big_gt_int',msg='')
   !!call unit_check('big_gt_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_gt_int',msg='')
end subroutine test_big_gt_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_int()

   call unit_check_start('big_int',msg='')
   !!call unit_check('big_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_int',msg='')
end subroutine test_big_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_le_big()

   call unit_check_start('big_le_big',msg='')
   !!call unit_check('big_le_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_le_big',msg='')
end subroutine test_big_le_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_le_int()

   call unit_check_start('big_le_int',msg='')
   !!call unit_check('big_le_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_le_int',msg='')
end subroutine test_big_le_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_lt_big()

   call unit_check_start('big_lt_big',msg='')
   !!call unit_check('big_lt_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_lt_big',msg='')
end subroutine test_big_lt_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_lt_int()

   call unit_check_start('big_lt_int',msg='')
   !!call unit_check('big_lt_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_lt_int',msg='')
end subroutine test_big_lt_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_minus_big()

   call unit_check_start('big_minus_big',msg='')
   !!call unit_check('big_minus_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_minus_big',msg='')
end subroutine test_big_minus_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_minus_int()

   call unit_check_start('big_minus_int',msg='')
   !!call unit_check('big_minus_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_minus_int',msg='')
end subroutine test_big_minus_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_ne_big()

   call unit_check_start('big_ne_big',msg='')
   !!call unit_check('big_ne_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_ne_big',msg='')
end subroutine test_big_ne_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_ne_int()

   call unit_check_start('big_ne_int',msg='')
   !!call unit_check('big_ne_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_ne_int',msg='')
end subroutine test_big_ne_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_plus_big()

   call unit_check_start('big_plus_big',msg='')
   !!call unit_check('big_plus_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_plus_big',msg='')
end subroutine test_big_plus_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_plus_int()

   call unit_check_start('big_plus_int',msg='')
   !!call unit_check('big_plus_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_plus_int',msg='')
end subroutine test_big_plus_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_power_int()

   call unit_check_start('big_power_int',msg='')
   !!call unit_check('big_power_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_power_int',msg='')
end subroutine test_big_power_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_times_big()

   call unit_check_start('big_times_big',msg='')
   !!call unit_check('big_times_big', 0.eq.0, 'checking', 100)
   call unit_check_done('big_times_big',msg='')
end subroutine test_big_times_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_big_times_int()

   call unit_check_start('big_times_int',msg='')
   !!call unit_check('big_times_int', 0.eq.0, 'checking', 100)
   call unit_check_done('big_times_int',msg='')
end subroutine test_big_times_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_char_big()

   call unit_check_start('char_big',msg='')
   !!call unit_check('char_big', 0.eq.0, 'checking', 100)
   call unit_check_done('char_big',msg='')
end subroutine test_char_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_char_gets_big()

   call unit_check_start('char_gets_big',msg='')
   !!call unit_check('char_gets_big', 0.eq.0, 'checking', 100)
   call unit_check_done('char_gets_big',msg='')
end subroutine test_char_gets_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_char_gets_int()

   call unit_check_start('char_gets_int',msg='')
   !!call unit_check('char_gets_int', 0.eq.0, 'checking', 100)
   call unit_check_done('char_gets_int',msg='')
end subroutine test_char_gets_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_char_int()

   call unit_check_start('char_int',msg='')
   !!call unit_check('char_int', 0.eq.0, 'checking', 100)
   call unit_check_done('char_int',msg='')
end subroutine test_char_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_huge_big()

   call unit_check_start('huge_big',msg='')
   !!call unit_check('huge_big', 0.eq.0, 'checking', 100)
   call unit_check_done('huge_big',msg='')
end subroutine test_huge_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_big()

   call unit_check_start('int_big',msg='')
   !!call unit_check('int_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_big',msg='')
end subroutine test_int_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_char()

   call unit_check_start('int_char',msg='')
   !!call unit_check('int_char', 0.eq.0, 'checking', 100)
   call unit_check_done('int_char',msg='')
end subroutine test_int_char
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_div_big()

   call unit_check_start('int_div_big',msg='')
   !!call unit_check('int_div_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_div_big',msg='')
end subroutine test_int_div_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_eq_big()

   call unit_check_start('int_eq_big',msg='')
   !!call unit_check('int_eq_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_eq_big',msg='')
end subroutine test_int_eq_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_ge_big()

   call unit_check_start('int_ge_big',msg='')
   !!call unit_check('int_ge_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_ge_big',msg='')
end subroutine test_int_ge_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_gets_big()

   call unit_check_start('int_gets_big',msg='')
   !!call unit_check('int_gets_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_gets_big',msg='')
end subroutine test_int_gets_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_gets_char()

   call unit_check_start('int_gets_char',msg='')
   !!call unit_check('int_gets_char', 0.eq.0, 'checking', 100)
   call unit_check_done('int_gets_char',msg='')
end subroutine test_int_gets_char
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_gt_big()

   call unit_check_start('int_gt_big',msg='')
   !!call unit_check('int_gt_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_gt_big',msg='')
end subroutine test_int_gt_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_le_big()

   call unit_check_start('int_le_big',msg='')
   !!call unit_check('int_le_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_le_big',msg='')
end subroutine test_int_le_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_lt_big()

   call unit_check_start('int_lt_big',msg='')
   !!call unit_check('int_lt_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_lt_big',msg='')
end subroutine test_int_lt_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_minus_big()

   call unit_check_start('int_minus_big',msg='')
   !!call unit_check('int_minus_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_minus_big',msg='')
end subroutine test_int_minus_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_ne_big()

   call unit_check_start('int_ne_big',msg='')
   !!call unit_check('int_ne_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_ne_big',msg='')
end subroutine test_int_ne_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_times_big()

   call unit_check_start('int_times_big',msg='')
   !!call unit_check('int_times_big', 0.eq.0, 'checking', 100)
   call unit_check_done('int_times_big',msg='')
end subroutine test_int_times_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_modulo_big_big()

   call unit_check_start('modulo_big_big',msg='')
   !!call unit_check('modulo_big_big', 0.eq.0, 'checking', 100)
   call unit_check_done('modulo_big_big',msg='')
end subroutine test_modulo_big_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_modulo_big_int()

   call unit_check_start('modulo_big_int',msg='')
   !!call unit_check('modulo_big_int', 0.eq.0, 'checking', 100)
   call unit_check_done('modulo_big_int',msg='')
end subroutine test_modulo_big_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_modulo_int_big()

   call unit_check_start('modulo_int_big',msg='')
   !!call unit_check('modulo_int_big', 0.eq.0, 'checking', 100)
   call unit_check_done('modulo_int_big',msg='')
end subroutine test_modulo_int_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_print_big()

   call unit_check_start('print_big',msg='')
   !!call unit_check('print_big', 0.eq.0, 'checking', 100)
   call unit_check_done('print_big',msg='')
end subroutine test_print_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_random_number_big()

   call unit_check_start('random_number_big',msg='')
   !!call unit_check('random_number_big', 0.eq.0, 'checking', 100)
   call unit_check_done('random_number_big',msg='')
end subroutine test_random_number_big
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqrt_big()

   call unit_check_start('sqrt_big',msg='')
   !!call unit_check('sqrt_big', 0.eq.0, 'checking', 100)
   call unit_check_done('sqrt_big',msg='')
end subroutine test_sqrt_big
!===================================================================================================================================
end subroutine test_suite_M_big_integer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_big_integer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
