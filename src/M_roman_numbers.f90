! Jeanne Martin was the convernor of WG5, the ISO Fortran working group.
! She has kindly contributed this module, a test of operator overloading.
!
! Modified by Walt Brainerd to conform to F
!
!>
!!##NAME
!!    M_roman_numbers(3fm) - [M_roman_numbers::INTRO] overloads of standard operators for Roman numbers
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!
!!    use M_roman_numbers, only : &
!!
!!       assignment(=),  &
!!       operator(+),    operator(-),   &
!!       operator(*),    operator(/),   &
!!       operator(==),   operator(/=),  &
!!       operator(<),    operator(<=),  &
!!       operator(>),    operator(>=),  &
!!       len, int,                      &
!!       print_roman, roman_number, roman
!!
!!       type(roman) :: num
!!
!!##DESCRIPTION
!!
!!    An example of overloading operators and intrinsics to allow Roman numbers to be used as
!!    a whole number type.
!!
!!    + int()  allows type(roman) values to be converted to default type integers
!!    + len()  returns the number of characters in the Roman number representation of the value
!!    + print_roman() is a subroutine for printing the Roman number representation
!!    + roman_number()
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program demo_M_roman_numbers
!!    ! Test case is courtesy of Jeanne Martin
!!    use M_roman_numbers
!!    implicit none
!!
!!    character(len=16), dimension(3999) :: table
!!    type(roman)                :: r
!!    integer                    :: i
!!
!!    type(roman)                :: year_2000, cornerstone, &
!!    & bad_place, bad_dig, bad_dig2, long_dig, BC
!!    type(roman)                :: bad_place2, too_big
!!    type(roman), dimension(10) :: errors
!!    type(roman), dimension(5)  :: arith
!!    integer                    :: centuries, ix, iy, iz, iterate_runs
!!    character(len=16)          :: long
!!    character(len=3)           :: short
!!
!!       do iterate_runs = 1,1000
!!
!!    ! test r = i
!!    ! print table of all roman numbers,
!!    ! save roman values for 2nd part of test
!!          write (unit=*, fmt="(a)") "Integer  Roman Number"
!!    ! do i = 1, 3999
!!          do i = 1900, 2000  ! shortened to reduce output
!!             r = i
!!             write (unit=*, fmt="(/, tr4, i4, tr2)", advance = "NO") i
!!             call print_roman (r)
!!             table(i) = r
!!          enddo
!!          write (unit=*, fmt="(2/)")
!!
!!    ! test r = c
!!    ! print table again converting roman to integer
!!          write (unit=*, fmt="(a,/)") "Integer  Roman Number"
!!    ! do i = 1, 3999
!!          do i = 1985, 1995  ! shortened to reduce output
!!             r = table(i)
!!             write (unit=*, fmt="(tr4, i4, tr2, 16a, /)") int(r), table(i)
!!          enddo
!!          write (unit=*, fmt="(/)")
!!
!!    ! test c = r
!!          long = r
!!          short = r
!!          write (unit=*, fmt="(a, 2a17)")  " short and long ", short, long
!!
!!    ! test i = r
!!          ix = r
!!          write (unit=*, fmt="(/, a, i4)") " ix = ", ix
!!
!!    ! test len
!!          ix = len(r)
!!          write (unit=*, fmt="(a, i4, /)") " len(r) = ", ix
!!
!!    ! test roman_number
!!          iy = roman_number(25)
!!          iz = roman_number("XXIX")
!!          write (unit=*, fmt="(a, 2i4, /)") " iy and iz ", iy, iz
!!
!!    ! test error procedures, arithmetic, and comparison
!!
!!          year_2000 = "MM"
!!          too_big = 2 * year_2000
!!          cornerstone = 1913
!!          BC = -12
!!          bad_place = "XXIC"
!!          bad_dig = "MCM XXX III"
!!          long_dig = "MCMXXXIII  "
!!          write (unit=*, fmt="(/, a)", advance = "NO") "long_dig = "
!!          call print_roman (long_dig)
!!
!!          centuries = int(cornerstone/100)
!!          if (cornerstone==1913) then
!!             write (unit=*, fmt="(/,a)") "good == test"
!!          else
!!             write (unit=*, fmt="(/,a)") "bad == test"
!!          end if
!!          if (cornerstone == "MCMXIII") then
!!             write (unit=*, fmt="(/,a)") "good == test"
!!          else
!!             write (unit=*, fmt="(/,a)") "bad == test"
!!          end if
!!          if (long_dig > 1900) then
!!             write (unit=*, fmt="(/,a)") "good > test"
!!          else
!!             write (unit=*, fmt="(/,a)") "bad > test"
!!          end if
!!
!!          bad_dig2 = "MQM"
!!          bad_place2 = "MMIVX"
!!
!!          write (unit=*, fmt="(a, i4,/)") "centuries = ", centuries
!!          write (unit=*, fmt="(a)", advance = "NO") "cornerstone = "
!!          call print_roman (cornerstone)
!!          write (unit=*, fmt="(/, a)", advance = "NO") "year_2000 = "
!!          call print_roman (year_2000)
!!          write (unit=*, fmt="(/, a)", advance = "NO") "bad_place = "
!!          call print_roman (bad_place)
!!          write (unit=*, fmt="(/, a)", advance = "NO") "bad_dig = "
!!          call print_roman (bad_dig)
!!          write (unit=*, fmt="(/)")
!!
!!          errors(1) = "MCCCCX"
!!          errors(2) = "MDDCX"
!!          errors(3) = "LXIVI"
!!          write (unit=*, fmt="(/, a, i4, /)") "LXIVI = ", int(errors(3))
!!          errors(4) = "LIXIV"
!!          errors(5) = "MCMDXX"
!!          errors(6) = "MCMXXXXI"
!!          errors(7) = "MXLX"
!!          write (unit=*, fmt="(/, a, i4, /)") "MXLX = ", int(errors(7))
!!          errors(8) = "MCMCXX"
!!          errors(9) = "MXLXI"
!!
!!          arith(1) = 2
!!          arith(2) = arith(1) * "X"
!!          arith(3) = arith(2) / "IV"
!!          arith(4) = arith(3) + cornerstone
!!          arith(5) = year_2000 - "CIII"
!!          write (unit=*, fmt="(/, a, 5i6, /)") "arith = ", &
!!          & ((int(arith(i))), i = 1, 5)
!!
!!       enddo
!!
!!    end program demo_M_roman_numbers
!!
!! Results:
!!
!!##AUTHOR
!!    + Jeanne Martin
!!    + Modified by Walt Brainerd to conform to F.
!!    + John S. Urban
!!
!!##LICENSE
!!    MIT
module M_roman_numbers
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
private

public :: assignment(=), &
        & operator(+),  operator(-),   &
        & operator(*),  operator(/),   &
        & operator(==), operator(/=),  &
        & operator(<),  operator(<=),  &
        & operator(>),  operator(>=),  &
          len, print_roman, &
          int, roman_number

type, public :: roman
  private
  integer                          :: value
  character, dimension(:), pointer :: as_seen
end type roman

interface assignment (=)
  module procedure r_ass_i, r_ass_c, c_ass_r, i_ass_r
end interface
private :: r_ass_i, r_ass_c, c_ass_r, i_ass_r

interface len
  module procedure rom_len
end interface
private :: rom_len

interface int
  module procedure rom_int
end interface
private :: rom_int

interface roman_number
  module procedure roman_number_i, roman_number_c
end interface
private :: roman_number_i, roman_number_c

interface operator (+)
  module procedure rom_a_rom, int_a_rom, rom_a_int, ch_a_rom, rom_a_ch
end interface
private :: rom_a_rom, int_a_rom, rom_a_int, ch_a_rom, rom_a_ch

interface operator (-)
   module procedure rom_s_rom, int_s_rom, rom_s_int, ch_s_rom, rom_s_ch
end interface
private :: rom_s_rom, int_s_rom, rom_s_int, ch_s_rom, rom_s_ch

interface operator (*)
   module procedure rom_m_rom, int_m_rom, rom_m_int, ch_m_rom, rom_m_ch
end interface
private :: rom_m_rom, int_m_rom, rom_m_int, ch_m_rom, rom_m_ch

interface operator (/)
   module procedure rom_d_rom, int_d_rom, rom_d_int, ch_d_rom, rom_d_ch
end interface
private :: rom_d_rom, int_d_rom, rom_d_int, ch_d_rom, rom_d_ch

interface operator (==)
   module procedure rom_eq_rom, int_eq_rom, rom_eq_int, ch_eq_rom, rom_eq_ch
end interface
private :: rom_eq_rom, int_eq_rom, rom_eq_int, ch_eq_rom, rom_eq_ch

interface operator (/=)
   module procedure rom_ne_rom, int_ne_rom, rom_ne_int, ch_ne_rom, rom_ne_ch
end interface
private :: rom_ne_rom, int_ne_rom, rom_ne_int, ch_ne_rom, rom_ne_ch

interface operator (<)
   module procedure rom_lt_rom, int_lt_rom, rom_lt_int, ch_lt_rom, rom_lt_ch
end interface
private :: rom_lt_rom, int_lt_rom, rom_lt_int, ch_lt_rom, rom_lt_ch

interface operator (<=)
   module procedure rom_le_rom, int_le_rom, rom_le_int, ch_le_rom, rom_le_ch
end interface
private :: rom_le_rom, int_le_rom, rom_le_int, ch_le_rom, rom_le_ch

interface operator (>)
   module procedure rom_gt_rom, int_gt_rom, rom_gt_int, ch_gt_rom, rom_gt_ch
end interface
private :: rom_gt_rom, int_gt_rom, rom_gt_int, ch_gt_rom, rom_gt_ch

interface operator (>=)
   module procedure rom_ge_rom, int_ge_rom, rom_ge_int, ch_ge_rom, rom_ge_ch
end interface
private :: rom_ge_rom, int_ge_rom, rom_ge_int, ch_ge_rom, rom_ge_ch

private :: error_misplaced, error_baddig, error_too_many

type(roman), private :: temp

contains

impure subroutine r_ass_i (r, i)
type(roman), intent(out)          :: r
integer, intent(in)               :: i
! local variables
integer                           :: n, j, k, u, v, l
character(len=1), dimension(13)   :: pool
character(len=1), dimension(16)   :: temp
integer                           :: space

! initializations
!  data pool / 'M','2','D','5','C','2','L','5','X','2','V','5','I' /
  pool = ["M","2","D","5","C","2","L","5","X","2","V","5","I"]
  if (i <= 0 .or. i > 3999) call error_range (i)
  n = i
  if (n <= 0 .or. n > 3999) n = 0
  v = 1000
  j = 1     ! index for pool
  l = 1     ! index for temp

! obtain roman representation
  outer: do
    inner: do
      if (n < v) exit inner
      temp(l) = pool(j)
      l = l + 1
      n = n - v
    enddo inner
    if (n <= 0) exit outer
    k = j + 2
    u = v / (ichar(pool(k-1)) - ichar("0"))
    if (pool(k-1) == "2") then
      k = k + 2
      u = u / (ichar(pool(k-1)) - ichar("0"))
    endif
    if (n + u >= v) then
      temp(l) = pool(k)
      l = l + 1
      n = n + u
    else
      j = j + 2
      v = v / (ichar(pool(j-1)) - ichar("0"))
    endif
enddo outer

! perform assignment
  r % value = i
  allocate (r % as_seen(l-1), stat = space)
  if (space /= 0) then
    r % as_seen => null()
  endif
  r % as_seen = temp(1:l-1)

end subroutine r_ass_i

impure subroutine r_ass_c (r, ch)
type(roman), intent(out)                             :: r
character(len=*), intent(in)                         :: ch

! local variables
integer, parameter                                   :: M = 1, D = 2, C = 3, L = 4, X = 5, V = 6, I = 7
integer, dimension(ichar("C"):ichar("X")), parameter :: convert = &
  [ C, D, 0, 0, 0, 0, I, 0, 0, L, M, 0, 0, 0, 0, &
                 0, 0, 0, 0, V, 0, X ]
!!!  data table /   1,  12,  21,  32,  41,  52,  61, &   ! initial
integer, dimension(7, 0:9), parameter                :: table = reshape (  &

!                   -- state transition table --

!                M    D    C    L    X    V    I
!             ________________________________________
            [    1,  12,  21,  32,  41,  52,  61, &   ! initial
                 3,   3,  21,  32,  41,  52,  61, &   ! after D
                74,  74,  21,  32,  41,  52,  61, &   ! after C
                 3,   3,   3,   3,  41,  52,  61, &   ! after L
                 3,   3,  84,  84,  41,  52,  61, &   ! after X
                 3,   3,   3,   3,   3,   3,  61, &   ! after V
                 3,   3,   3,   3,  94,  94,  61, &   ! after I
                 3,   3,   3,  32,  41,  52,  61, &   ! after CM or CD
                 3,   3,   3,   3,   3,  52,  61, &   ! after XC or XL
                 3,   3,   3,   3,   3,   3,   3   ], &   ! after IX or IV
     shape=[ 7, 10 ]  )

integer, dimension(7), parameter                     :: addend = [  1000, 500, 100, 50, 10, 5, 1  ]
integer, dimension(6), parameter                     :: addend2 = [  800, 300, 80, 30, 8, 3  ]

integer                                              :: state, new_state, letter
integer, dimension(7)                                :: count

integer                                              :: rlen            ! length of roman number
integer                                              :: ix              ! loop index
integer                                              :: space           ! check available space

! initializations
  r % value = 0
  rlen = len_trim(ch)
  if (rlen == 0)  then
    r % as_seen => null()
    return
  endif

  count = 0
  new_state = 0

! verify and translate roman digits to integer values
  do ix = 1, rlen
    if (ch(ix:ix) < "C" .or. ch(ix:ix) > "X") then
      call error_baddig (ch(ix:ix), r)
      return
    endif
    letter = convert (ichar(ch(ix:ix)))
    if (letter == 0) then
      call error_baddig (ch(ix:ix), r)
      return
    endif
    state = new_state
    new_state = table(letter, state)/10
    select case (modulo(table(letter, state), 10))
      case (1)
        r % value = r % value + addend(letter)
        count(letter) = count(letter) + 1
        if (count(letter) > 3) then
          call error_too_many (ch(ix:ix), r)
          return
        endif
        cycle
      case (2)
        r % value = r % value + addend(letter)
        count = 0
        cycle
      case (3)
        call error_misplaced (ch(ix:ix), r)
        return
      case (4)
        r % value = r % value + addend2(letter)
        count = 0
        cycle
    end select
  enddo

  allocate (r % as_seen(1:rlen), stat = space)
!  if (space /= 0) then
!    print *, "(/, 'no more space', /)"
!    stop
!  endif
  do ix = 1, rlen
    r % as_seen(ix) = ch(ix:ix)
  enddo

end subroutine r_ass_c

!pure subroutine c_ass_r (c, r)
impure subroutine c_ass_r (c, r)
type(roman), intent(in)                        :: r
!character(len=size(r % as_seen)), intent(out) :: c
character(len=*), intent(out)                  :: c
integer                                        :: i, j

  do i = 1, min(len(c), size(r % as_seen))
    c(i:i) = r % as_seen(i)
  enddo
  if (i <= len(c)) then
    do j = i, len(c)
      c(j:j) = " "
    enddo
   endif
end subroutine c_ass_r

pure subroutine i_ass_r (i, r)
integer, intent(out)                           :: i
type(roman), intent(in)                        :: r
  i = r % value
end subroutine i_ass_r

! get length of roman number
function rom_len (r) result(rr)
integer                                        :: rr
type(roman), intent(in)                        :: r
  rr = size(r % as_seen)
end function rom_len

! get integer value of roman number
function rom_int(r) result(rr)
integer                                        :: rr
type(roman), intent(in)                        :: r
  rr = r % value
end function rom_int

! get roman number of integer
function roman_number_i (i) result(rr)
type(roman)                                    :: rr
integer, intent(in)                            :: i
  rr = i
end function roman_number_i

! get roman number of character
function roman_number_c (c) result(rr)
type(roman)                                    :: rr
character (len=*), intent(in)                  :: c
  rr = c
end function roman_number_c

! addition
function rom_a_rom (r1, r2) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value + r2 % value
end function rom_a_rom

function int_a_rom (i, r) result(rr)
type(roman)                                    :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i + r % value
end function int_a_rom

function rom_a_int (r, i) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value + i
end function rom_a_int

function ch_a_rom (c, r) result(rr)
type(roman)                                    :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  rr = roman_number(c) + r
end function ch_a_rom

function rom_a_ch (r, c) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  rr = r + roman_number(c)
end function rom_a_ch

!subtraction
function rom_s_rom (r1, r2) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value - r2 % value
end function rom_s_rom

function int_s_rom (i, r) result(rr)
type(roman)                                    :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i - r % value
end function int_s_rom

function rom_s_int (r, i) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value - i
end function rom_s_int

function ch_s_rom (c, r) result(rr)
type(roman)                                    :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  rr = roman_number(c) - r
end function ch_s_rom

function rom_s_ch (r, c) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  rr = r - roman_number(c)
end function rom_s_ch

! multiplication
function rom_m_rom (r1, r2) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value * r2 % value
end function rom_m_rom

function int_m_rom (i, r) result(rr)
type(roman)                                    :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i * r % value
end function int_m_rom

function rom_m_int (r, i) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value * i
end function rom_m_int

function ch_m_rom (c, r) result(rr)
type(roman)                                    :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  rr = roman_number(c) * r
end function ch_m_rom

function rom_m_ch (r, c) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  rr = r * roman_number(c)
end function rom_m_ch

! division
function rom_d_rom (r1, r2) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value / r2 % value
end function rom_d_rom

function int_d_rom (i, r) result(rr)
type(roman)                                    :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i / r % value
end function int_d_rom

function rom_d_int (r, i) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value / i
end function rom_d_int

function ch_d_rom (c, r) result(rr)
type(roman)                                    :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  rr = roman_number(c) / r
end function ch_d_rom

function rom_d_ch (r, c) result(rr)
type(roman)                                    :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  rr = r / roman_number(c)
end function rom_d_ch

! comparison - equal
function rom_eq_rom (r1, r2) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value == r2 % value
end function rom_eq_rom

function int_eq_rom (i, r) result(rr)
logical                                        :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i == r % value
end function int_eq_rom

function rom_eq_int (r, i) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value == i
end function rom_eq_int

function ch_eq_rom (c, r) result(rr)
logical                                        :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
type(roman)                                    :: temp
  temp = c
  rr = temp % value == r % value
end function ch_eq_rom

function rom_eq_ch (r, c) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
type(roman)                                    :: temp
  temp = c
  rr = r % value == temp % value
end function rom_eq_ch

! comparison -  not equal
function rom_ne_rom (r1, r2) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value /= r2 % value
end function rom_ne_rom

function int_ne_rom (i, r) result(rr)
logical                                        :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i /= r % value
end function int_ne_rom

function rom_ne_int (r, i) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value /= i
end function rom_ne_int

function ch_ne_rom (c, r) result(rr)
logical                                        :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  temp = c
  rr = temp % value /= r % value
end function ch_ne_rom

function rom_ne_ch (r, c) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  temp = c
  rr = r % value /= temp % value
end function rom_ne_ch

! comparison - less than
function rom_lt_rom (r1, r2) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value < r2 % value
end function rom_lt_rom

function int_lt_rom (i, r) result(rr)
logical                                        :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i < r % value
end function int_lt_rom

function rom_lt_int (r, i) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value < i
end function rom_lt_int

function ch_lt_rom (c, r) result(rr)
logical                                        :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  temp = c
  rr = temp % value < r % value
end function ch_lt_rom

function rom_lt_ch (r, c) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  temp = c
  rr = r % value < temp % value
end function rom_lt_ch

! comparison - less than or equal
function rom_le_rom (r1, r2) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value <= r2 % value
end function rom_le_rom

function int_le_rom (i, r) result(rr)
logical                                        :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i <= r % value
end function int_le_rom

function rom_le_int (r, i) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value <= i
end function rom_le_int

function ch_le_rom (c, r) result(rr)
logical                                        :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  temp = c
  rr = temp % value <= r % value
end function ch_le_rom

function rom_le_ch (r, c) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  temp = c
  rr = r % value <= temp % value
end function rom_le_ch

! comparison - greater than
function rom_gt_rom (r1, r2) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value > r2 % value
end function rom_gt_rom

function int_gt_rom (i, r) result(rr)
logical                                        :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i > r % value
end function int_gt_rom

function rom_gt_int (r, i) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value > i
end function rom_gt_int

function ch_gt_rom (c, r) result(rr)
logical                                        :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  temp = c
  rr = temp % value > r % value
end function ch_gt_rom

function rom_gt_ch (r, c) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  temp = c
  rr = r % value > temp % value
end function rom_gt_ch

! comparison - greater than or equal
function rom_ge_rom (r1, r2) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r1, r2
  rr = r1 % value >= r2 % value
end function rom_ge_rom

function int_ge_rom (i, r) result(rr)
logical                                        :: rr
integer, intent(in)                            :: i
type(roman), intent(in)                        :: r
  rr = i >= r % value
end function int_ge_rom

function rom_ge_int (r, i) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
integer, intent(in)                            :: i
  rr = r % value >= i
end function rom_ge_int

function ch_ge_rom (c, r) result(rr)
logical                                        :: rr
character (len=*), intent(in)                  :: c
type(roman), intent(in)                        :: r
  temp = c
  rr = temp % value >= r % value
end function ch_ge_rom

function rom_ge_ch (r, c) result(rr)
logical                                        :: rr
type(roman), intent(in)                        :: r
character (len=*), intent(in)                  :: c
  temp = c
  rr = r % value >= temp % value
end function rom_ge_ch

! print roman number
subroutine print_roman (r)
type(roman), intent(in)                        :: r
  if (associated(r % as_seen)) then
    write (unit=*, fmt="(16a1)", advance = "no") r % as_seen
  else
    write (unit=*, fmt="(a)", advance = "no") "0"
  endif
end subroutine print_roman

subroutine error_range (i)
integer, intent(in)                            :: i
  write (unit=stderr, fmt="(a, tr2, i6)") "out of range for roman number", i
end subroutine error_range

impure subroutine error_misplaced (c, r)
character(len=*), intent(in)                   :: c
type(roman), intent(inout)                     :: r
  write (unit=stderr, fmt="(2a)") "badly-formed roman number - misplaced digit ", c
  r % value = 0
  r % as_seen => null()
end subroutine error_misplaced

impure subroutine error_baddig (c, r)
character(len=*), intent(in)                   :: c
type(roman), intent(inout)                     :: r
  write (unit=stderr,fmt="(2a)") "badly-formed roman number - invalid digit ", c
  r % value = 0
  r % as_seen => null()
end subroutine error_baddig

impure subroutine error_too_many (c, r)
character(len=*), intent(in)                   :: c
type(roman), intent(inout)                     :: r
  write (unit=stderr, fmt="(2a)") "badly-formed roman number - too many digits ", c
  r % value = 0
  r % as_seen => null()
end subroutine error_too_many

end module M_roman_numbers
