










!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    M_random(3f) - [M_random::INTRO] Routines for generating random numbers and strings
!!##SYNOPSIS
!!
!!   See the routines:
!!
!!    use M_random, only : init_random_seed_by_system_clock, &
!!    & init_random_seed_by_dat, init_random_seed
!!    use M_random, only : random_string, random_hex, random_int
!!    use M_random, only : random_kiss64
!!    use M_random, only : mtprng_state, mtprng_init_array, mtprng_rand64, &
!!    & mtprng_rand_real1
!!    use M_random, only : mtprng_int, mtprng_int_by_array
!!    use M_random, only : mtprng_rand64, mtprng_rand, mtprng_rand_range
!!    use M_random, only : mtprng_rand_real3, mtprng_rand_real2, &
!!    & mtprng_rand_real1
!!    use M_random, only : scramble
!!
!!##QUOTE
!!
!!   The generation of random numbers is too important to be left to chance
!!   -- Robert R. Coveyou
!!
!!##DESCRIPTION
!!
!!    The M_random(3fm) module contains routines to support random number
!!    generation. This includes supplements for the Fortran intrinsic
!!    random_seed(3f).
!!
!!   SUPPLEMENTING INTRINSIC RANDOM_SEED
!!    o init_random_seed_by_system_clock(3f): initialize random_number(3f) to return a single value with system clock
!!    o init_random_seed_by_dat(3f): initialize random_number(3f) to return a single value using date_and_time(3f)
!!    o init_random_seed(3f): initialize random_number(3f) to return a single value with single integer seed like srand(3c)
!!
!!    o random_string(3f): create random string composed of provided characters of specified length
!!    o random_hex(3f): create random hexadecimal string of specified length
!!    o random_int(3f): return integer uniformly distributed in specified range
!!
!!   MISCELLANEOUS
!!    o random_kiss64(3f): A 64-bit KISS random number generator by George Margaglia.
!!    o scramble(3f): generate an integer array of specified size populated with a random permutation of 1 to size(array)
!!
!!   MERSENNE TWISTER ALGORITHM
!!    o mtprng_int(3f): Initializes the Mersenne Twister random number generator with
!!    o mtprng_int_by_array(3f): Initialize with an array of seeds
!!
!!    o mtprng_rand64(3f): Obtain the next 64-bit integer in the pseudo-random sequence in the range 0 to 2^32-1
!!    o mtprng_rand(3f): Obtain the next 32-bit integer in the pseudo-random sequence in the range 0 to 2^31-1
!!    o mtprng_rand_range(3f): Obtain a pseudo-random integer in the range [lo,hi]
!!
!!    o mtprng_rand_real3(3f): Obtain a pseudo-random real number .gt. 0 and .lt. 1.
!!    o mtprng_rand_real2(3f): Obtain a pseudo-random real number .ge. 0.0 and .lt. 1.0
!!    o mtprng_rand_real1(3f): Obtain a pseudo-random real number .ge. 0 and .le.= 1.
module M_random
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64  !  1 2 4 8
use,intrinsic :: iso_fortran_env, only : real32, real64, real128
implicit none
private                                           ! Everything is private unless explicitly made public private
public init_random_seed_by_system_clock
public init_random_seed_by_dat
public init_random_seed

public random_string
public random_hex
public random_int

public random_kiss64
public scramble

public :: mtprng_state, mtprng_init, mtprng_init_by_array, mtprng_rand64, mtprng_rand
public :: mtprng_rand_range, mtprng_rand_real1, mtprng_rand_real2, mtprng_rand_real3

!==================================================================================================================================!
! Kind types for IEEE 754/IEC 60559 single- and double-precision reals
integer, parameter :: IEEE32 = selected_real_kind(  6,  37 )
integer, parameter :: IEEE64 = selected_real_kind( 15, 307 )
! Constants
integer(INT32), parameter :: N = 624_INT32
integer(INT32), parameter :: M = 397_INT32
! types
type mtprng_state
   integer(INT32)                   :: mti = -1
   integer(INT64), dimension(0:N-1) :: mt
end type
!==================================================================================================================================!
contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    random_string(3f) - [M_random] create random string composed of
!!                        provided characters of specified length
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function random_string(chars,length) result(out)
!!
!!     character(len=*),intent(in)     :: chars
!!     integer,intent(in)              :: length
!!     character(len=:),allocatable    :: out
!!
!!##DESCRIPTION
!!    Given a set of characters and a length, generate a random string of
!!    the specified length composed of the given set of characters.
!!
!!##OPTIONS
!!    chars   list of characters to generate random string with
!!    length  number of characters to place in output string
!!
!!##RESULT
!!    out     string of LENGTH characters randomly filled with characters
!!            from CHARS
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_random_string
!!     use M_random, only : random_string, init_random_seed_by_dat
!!        character(len=64) :: hexstring
!!        ! use date and time to create a seed for calling random_seed(3f)
!!        call init_random_seed_by_dat()
!!        hexstring=random_string('0123456789abcdef',len(hexstring))
!!        ! write random hexadecimal value for use
!!        ! as something like an X11 authorization key
!!        write(*,'(a)')hexstring
!!     end program demo_random_string
!!
!!    Results
!!
!!     2363a3589736e23be0137ec7ebc9d74297a963f27958a176daea3dd850ed8487
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    MIT License
function random_string(chars,length) result(out)

! ident_1="@(#) M_random random_string(3f) create random string composed of provided characters of specified length"

character(len=*),intent(in)     :: chars
integer,intent(in)              :: length
character(len=:),allocatable    :: out
   real                         :: x
   integer                      :: ilen   ! length of list of characters
   integer                      :: which
   integer                      :: i
   ilen=len(chars)
   out=''
   call init_random_seed_by_dat()
   if(ilen.gt.0)then
      do i=1,length
         call random_number(x)
         which=nint(real(ilen-1)*x)+1
         out=out//chars(which:which)
      enddo
   endif
end function random_string
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    random_hex(3f) - [M_random] create a string representing a random
!!                     hexadecimal value of specified length
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function random_hex(chars,length) result(out)
!!
!!     character(len=*),intent(in)     :: chars
!!     integer,intent(in)              :: length
!!     character(len=:),allocatable    :: out
!!
!!##DESCRIPTION
!!    Generate a random string representing a hexadecimal value of a given length
!!
!!##OPTIONS
!!    length  number of characters to place in output string
!!
!!##RESULT
!!    out     string of LENGTH characters randomly filled with characters
!!            representing a hexadecimal value
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_random_hex
!!     use M_random, only : random_hex, init_random_seed_by_dat
!!        character(len=64) :: hexstring
!!        ! use date and time to create a seed for calling random_seed(3f)
!!        call init_random_seed_by_dat()
!!        ! write random hexadecimal value for use
!!        ! as something like an X11 authorization key
!!        hexstring=random_hex(len(hexstring))
!!        write(*,'(a)')hexstring
!!     end program demo_random_hex
!!
!!    Results
!!
!!     2363a3589736e23be0137ec7ebc9d74297a963f27958a176daea3dd850ed8487
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    MIT License
function random_hex(length) result(out)

! ident_2="@(#) M_random random_hex(3f) create random hexadecimal string of specified length"

integer,intent(in)              :: length
character(len=:),allocatable    :: out
   out=random_string('0123456789abcdef',length)
end function random_hex
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    random_int(3f) - [M_random] return an integer between low and high value inclusive
!!    (LICENSE:MIT)
!!##SYNOPSIS
!!
!!   function random_int(first,last) result(rand_int)
!!
!!    integer,intent(in) :: first,last
!!    integer            :: rand_int
!!
!!##DESCRIPTION
!!    Return an integer uniformly distributed from the set {first,,first+1,...,last-1,last}.
!!##OPTIONS
!!    first       lowest value of range of integer values to randomly return
!!    last        highest value of range of integer values to randomly return
!!##RETURNS
!!    rand_int    a random integer value between FIRST LAST inclusive
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_random_int
!!    use M_random, only : random_int, init_random_seed_by_system_clock
!!    implicit none
!!    integer :: i
!!    call init_random_seed_by_system_clock()
!!    write(*,'(*(i0:,1x))')(random_int(1,10),i=1,20)
!!    write(*,'(*(i0:,1x))')(random_int(-5,5),i=1,20)
!!    end program demo_random_int
!!
!!   Sample output
!!
!!    1 3  8 1 2  6  8 7  4  10  7  3  8  3  10 1  5  2 9  8
!!    4 5 -3 5 2 -5 -4 4 -5  -3 -2 -2 -2 -1  -2 4 -2 -2 4 -4
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
function random_int(first,last) result(rand_int)
use,intrinsic :: iso_fortran_env, only : dp=>real64
implicit none
integer,intent(in)   :: first,last ! lowest and highest integer in range of integers to get
integer, allocatable :: seed(:)
integer              :: n
integer              :: rand_int
real(kind=dp)        :: rand_val
logical,save         :: called=.false.
   if(.not.called)then
   ! initialize seed
      call random_seed(size = n)
      if(allocated(seed))deallocate(seed)
      allocate(seed(n))
      call random_seed(get=seed)
      called=.true.
   endif

   ! To have a discrete uniform distribution on the integers {first, first+1,
   ! ..., last-1, last} carve the continuous distribution up into last+1-first
   ! equal sized chunks, mapping each chunk to an integer. One way is:
   !
   ! get real number from 0 up to but not including 1 (ie. [0,1)).
   call random_number(rand_val)
   ! use random value to choose an integer from first to last
   rand_int = first + floor((last-first+1)*rand_val)
   if(allocated(seed))deallocate(seed)
end function random_int
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    scramble(3f) - [M_random] generate an integer array of specified size populated with a random permutation of 1 to size(array)
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    function scramble( number_of_values )
!!    integer,intent(in) :: number_of_values
!!
!!##DESCRIPTION
!!    Return an integer array of the size specified populated with the
!!    numbers 1 to "number_of_values" in random order.
!!
!!    A simple way to randomly scramble a list of any type is to create
!!    a random permutation of all the index values of the array and then
!!    access the original list elements using that list of indices. The
!!    list itself can be re-ordered very succinctly using array syntax.
!!    Given a list size ..
!!
!!    1. create an INTEGER array of the specified size N
!!    2. populate it with the values from 1 to N
!!    3. randomly switch values in the array to randomize it
!!    4. return the newly created array for use as indices
!!
!!    The resulting random permutation of the indices can then be used to
!!    access essentially any type of list in random order.
!!
!!##OPTIONS
!!    number_of_values  size of integer array to create
!!
!!##RETURNS
!!    scramble    Integer array filled with integers 1 to NUMBER_OF_VALUES in random order
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program demo_scramble
!!     use M_random, only : scramble, init_random_seed_by_system_clock
!!     implicit none
!!     character(len=*),parameter :: list(*)=[character(len=5) :: &
!!     & 'one','two','three','four','five',&
!!     & 'six','seven','eight','nine','ten']
!!     integer                    :: i
!!     integer                    :: n=size(list)
!!     character(len=len(list))   :: newlist(size(list))
!!     call init_random_seed_by_system_clock()
!!     do i = 1,8
!!        ! use random values as indices to randomize array
!!        newlist=list(scramble(n))
!!        write(*,'(*(a,1x))') newlist
!!     enddo
!!     end program demo_scramble
!!
!!   Example output
!!
!!    ten   six   eight one   four  nine  two   five  three seven
!!    three five  ten   nine  one   four  six   seven two   eight
!!    four  eight ten   two   seven nine  six   three one   five
!!    three one   nine  seven ten   five  two   six   eight four
!!    two   seven nine  one   four  three eight ten   five  six
!!    three one   nine  six   ten   five  eight two   four  seven
!!    four  five  six   eight one   ten   three nine  seven two
!!    three nine  four  two   one   seven ten   five  six   eight
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
function scramble( number_of_values ) result(array)

! ident_3="@(#) M_random scramble(3f) return integer array of random values 1 to N."

integer,intent(in)    :: number_of_values
integer,allocatable   :: array(:)
integer               :: i, j, k, m, n
integer               :: temp
real                  :: u

   array=[(i,i=1,number_of_values)] ! make the array and populate it with 1 thru number_of_values

! The intrinsic RANDOM_NUMBER(3f) returns a real number (or an array
! of such) from the uniform distribution over the interval [0,1). (ie.
! it includes 0 but not 1.).
!
! To have a discrete uniform distribution on
! the integers {n, n+1, ..., m-1, m} carve the continuous distribution
! up into m+1-n equal sized chunks, mapping each chunk to an integer.
!
! One way is:
!   call random_number(u)
!   j = n + FLOOR((m+1-n)*u)  ! choose one from m-n+1 integers

   n=1
   m=number_of_values
   do k=1,2
      do i=1,m
         call random_number(u)
         j = n + FLOOR((m+1-n)*u)
         ! switch values
         temp=array(j)
         array(j)=array(i)
         array(i)=temp
      enddo
   enddo

end function scramble
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!       random_kiss64(3f) - [M_random] A 64-bit KISS random number generator by George Margaglia.
!!##SYNOPSIS
!!
!!    function random_kiss64()
!!    integer, parameter         :: i8b = selected_int_kind(18)  ! eight-byte integer
!!    integer(i8b)               :: random_kiss64
!!
!!##DESCRIPTION
!!    A simple random number generator that returns a random 64-bit INTEGER. The same
!!    sequence is returned.
!!##EXAMPLE
!!
!!
!!   Sample usage:
!!
!!     program demo_random_kiss64
!!     use M_random, only : random_kiss64
!!     implicit none
!!     integer, parameter    :: i8b = selected_int_kind(18)  ! eight-byte integer
!!     integer(i8b)          :: i, t
!!
!!        write(*,*)'HUGE=',huge(0_i8b)
!!
!!        do i = 1, 100000000
!!           t = random_kiss64()
!!           if(mod(i,1000000_i8b+1_i8b)==1000000_i8b)write(*,*)i,' T=',T
!!        enddo
!!
!!        if (t .eq. 1666297717051644203_i8b) then
!!           print *, "100 million calls to KISS() OK"
!!        else
!!           print *, "Fail"
!!        endif
!!     end program demo_random_kiss64
function random_kiss64()

! ident_4="@(#) M_random random_kiss64(3f) A 64-bit KISS random number generator by George Margaglia."

! From: FortranWiki.org
! Originally posted to comp.lang.fortran in the message 64-bit KISS RNGs.
! No license was specified.
!
! Revised on April 14, 2010 21:40:44 by Jason Blevins (75.178.9.182)
! This version was modified by Jason Blevins to use "implicit none"
! and to declare the 64-bit/eight-byte integer type in a portable manner.
!-----------------------------------------------------------------------------------------------------------------------------------
   integer, parameter         :: i8b = selected_int_kind(18)  ! eight-byte integer
   integer(i8b), save         :: x, y, z, c
   integer(i8b)               :: t, k, m, s, random_kiss64
   data x, y, z, c &
      / 1234567890987654321_i8b, &
      362436362436362436_i8b, &
      1066149217761810_i8b, &
      123456123456123456_i8b /
!-----------------------------------------------------------------------------------------------------------------------------------
   m(x,k) = ieor(x, ishft(x,k))  ! statement function
   s(x) = ishft(x, -63)          ! statement function
!-----------------------------------------------------------------------------------------------------------------------------------
   t = ishft(x, 58) + c
   if (s(x) .eq. s(t)) then
      c = ishft(x, -6) + s(x)
   else
      c = ishft(x, -6) + 1 - s(x + t)
   endif
   x = t + x
   y = m(m(m(y,13_i8b),-17_i8b), 43_i8b)
   z = 6906969069_i8b * z + 1234567
   random_kiss64 = x + y + z
end function random_kiss64
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    init_random_seed_by_system_clock(3f) - [M_random] seed random_number(3f) with system clock value
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine init_random_seed_by_system_clock()
!!
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the system clock to initialize the seed so you can
!!    easily call random_number(3f) with varying pseudo-random real number sequences
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_init_random_seed_by_system_clock
!!     use M_random, only : init_random_seed_by_system_clock
!!     integer :: i
!!     real    :: x
!!        call init_random_seed_by_system_clock()
!!        do i=1,10
!!           ! generate real pseudo-random numbers from 0 to <1.0
!!           call random_number(x)
!!           write(*,*)i,x
!!        enddo
!!     end program demo_init_random_seed_by_system_clock
!!
!!    Results
!!
!!     >   1  0.661672294
!!     >   2  0.274969578
!!     >   3  0.683666587
!!     >   4   7.35652447E-02
!!     >   5  0.457893968
!!     >   6  0.826303899
!!     >   7  0.727411628
!!     >   8  0.542535722
!!     >   9  0.983459771
!!     >  10  0.527638793
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine init_random_seed_by_system_clock()

! ident_5="@(#) M_random init_random_seed_by_system_clock(3f) initialize random_number(3f) to return a single value with system clock"

   integer :: i, n, clock
   integer, dimension(:), allocatable :: seed
   call random_seed(size = n)
   if(allocated(seed))deallocate(seed)
   allocate(seed(n))
   call system_clock(count=clock)
   seed = clock + 37 * (/ (i - 1, i = 1, n) /)
!   write(*,*)seed
!   write(*,*)(/ (i - 1, i = 1, n) /)
   call random_seed(put = seed)
   deallocate(seed)
end subroutine init_random_seed_by_system_clock
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    init_random_seed_by_dat(3f) - [M_random] seed random_number(3f) with values from date_and_time(3f)
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine init_random_seed_by_dat()
!!
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the date_and_time(3f)
!!    intrinsic to initialize the seed so you can easily call
!!    random_number(3f) with varying pseudo-random real number sequences
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_init_random_seed_by_dat
!!     use M_random, only : init_random_seed_by_dat
!!     integer :: i
!!     real    :: x
!!        call init_random_seed_by_dat()
!!        do i=1,10
!!           ! generate real pseudo-random numbers from 0 to <1.0
!!           call random_number(x)
!!           write(*,*)i,x
!!        enddo
!!     end program demo_init_random_seed_by_dat
!!
!!    Results
!!
!!      >     1  0.644704163
!!      >     2  0.244343698
!!      >     3  0.516471267
!!      >     4  0.296542704
!!      >     5  0.681771278
!!      >     6  0.449223280
!!      >     7  0.915870190
!!      >     8  0.466257989
!!      >     9  0.912388682
!!      >    10  0.597788215
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine init_random_seed_by_dat()

! ident_6="@(#) M_random init_random_seed_by_dat(3f) initialize random_number(3f) to return a single value using date_and_time(3f)"

! Initially based on a post on comp.lang.fortran.
  integer :: ival(8)
  integer :: n, v(3), i
  integer, allocatable :: seed(:)
  call date_and_time(values=ival)
  v(1) = ival(8) + 2048*ival(7)
  v(2) = ival(6) + 64*ival(5)                     ! skip value(4) because it is the timezone, which is typically constant
  v(3) = ival(3) + 32*ival(2) + 32*8*ival(1)
  call random_seed(size=n)
  if(allocated(seed))deallocate(seed)
  allocate(seed(n))
  call random_seed()                              ! give the seed an implementation-dependent kick
  call random_seed(get=seed)
  do i=1, n
     seed(i) = seed(i) + v(mod(i-1, 3) + 1)
  enddo
  call random_seed(put=seed)
  deallocate(seed)
end subroutine init_random_seed_by_dat
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    init_random_seed(3f) - [M_random] seed random_number(3f) with single value like srand(3c) usage
!!    (LICENSE:MIT)
!!
!!##SYNOPSIS
!!
!!    subroutine init_random_seed(mine)
!!
!!     integer,intent(in) :: mine
!!
!!##DESCRIPTION
!!    A simple wrapper around random_seed(3f) that uses the single given
!!    integer to initialize the seed so you can easily call random_number(3f)
!!    with varying pseudo-random real number sequences simply, much like
!!    srand(3c) and rand(3c).
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!     program demo_init_random_seed
!!     use M_random, only : init_random_seed
!!     integer :: iseed
!!     integer :: i
!!     real    :: x
!!        iseed=218595421
!!        call init_random_seed(iseed)
!!        do i=1,10
!!           ! generate real pseudo-random numbers from 0 to <1.0
!!           call random_number(x)
!!           write(*,*)i,x
!!        enddo
!!     end program demo_init_random_seed
!!
!!    Results
!!
!!      >     1  0.989341617
!!      >     2  0.296594143
!!      >     3  0.805420995
!!      >     4   4.00894880E-03
!!      >     5   5.73359132E-02
!!      >     6  0.805290103
!!      >     7  0.944527864
!!      >     8  0.789443851
!!      >     9  0.327288270
!!      >    10  0.710926533
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    MIT License
subroutine init_random_seed(mine)

! ident_7="@(#) M_random init_random_seed(3f) initialize random_number(3f) to return a single value with single integer seed like srand(3c)"

! to make this start with a single number like srand(3c) take the seed and
! use the value to fill the seed array, adding 37 to each subsequent value
! till the array is filled.
integer,intent(in) :: mine
   integer         :: i, n
   integer, dimension(:), allocatable :: seed
   call random_seed(size = n)
   if(allocated(seed))deallocate(seed)
   allocate(seed(n))
   seed = mine + 37 * (/ (i - 1, i = 1, n) /)
  !write(*,*)seed
  !write(*,*)(/ (i - 1, i = 1, n) /)
   call random_seed(put = seed)
   deallocate(seed)
end subroutine init_random_seed
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!  An implementation of the Mersenne Twister algorithm for generating pseudo-random sequences.
! This notice applies specifically to the MTPRNG_* routines ....

! From the Algorithmic Conjurings of Scott Robert Ladd comes...
!
!  mtprng.f90 (a Fortran 95 module)
!
!  An implementation of the Mersenne Twister algorithm for generating
!  pseudo-random sequences.
!
!  History
!  -------
!   1.0.0   Initial release
!
!   1.1.0   6 February 2002
!           Updated to support algorithm revisions posted
!           by Matsumoto and Nishimura on 26 January 2002
!
!   1.5.0   12 December 2003
!           Added to hypatia project
!           Minor style changes
!           Tightened code
!           Now state based; no static variables
!           Removed mtprng_rand_real53
!
!   2.0.0   4 January 2004
!           Corrected erroneous unsigned bit manipulations
!           Doubled resolution by using 64-bit math
!           Added mtprng_rand64
!
!    2.0.1  26 April 2018
!           Added IMPLICIT NONE,
!           defined kinds internally to remove need for
!           separate module STDTYPES. Included this
!           module into existing module M_RANDOM.
!
!  ORIGINAL ALGORITHM COPYRIGHT
!  ============================
!  Copyright (C) 1997,2002 Makoto Matsumoto and Takuji Nishimura.
!  Any feedback is very welcome. For any question, comments, see
!  http://www.math.keio.ac.jp/matumoto/emt.html or email
!  matumoto@math.keio.ac.jp
!---------------------------------------------------------------------
!
!  COPYRIGHT NOTICE, DISCLAIMER, and LICENSE:
!
!  This notice applies *only* to this specific expression of this
!  algorithm, and does not imply ownership or invention of the
!  implemented algorithm.
!
!  If you modify this file, you may insert additional notices
!  immediately following this sentence.
!
!  Copyright 2019, John S. Urban
!  corrected subscript causing undersubscripting the seed array and
!  reformatted to conform to more recent Fortran structures and added
!  man pages.
!
!  Copyright 2001, 2002, 2004 Scott Robert Ladd.
!  All rights reserved, except as noted herein.
!
!  This computer program source file is supplied "AS IS". Scott Robert
!  Ladd (hereinafter referred to as "Author") disclaims all warranties,
!  expressed or implied, including, without limitation, the warranties
!  of merchantability and of fitness for any purpose. The Author
!  assumes no liability for direct, indirect, incidental, special,
!  exemplary, or consequential damages, which may result from the use
!  of this software, even if advised of the possibility of such damage.
!
!  The Author hereby grants anyone permission to use, copy, modify, and
!  distribute this source code, or portions hereof, for any purpose,
!  without fee, subject to the following restrictions:
!
!      1. The origin of this source code must not be misrepresented.
!
!      2. Altered versions must be plainly marked as such and must not
!         be misrepresented as being the original source.
!
!      3. This Copyright notice may not be removed or altered from any
!         source or altered source distribution.
!
!  The Author specifically permits (without fee) and encourages the use
!  of this source code for entertainment, education, or decoration. If
!  you use this source code in a product, acknowledgment is not required
!  but would be appreciated.
!
!  Acknowledgement:
!      This license is based on the wonderful simple license that
!      accompanies libpng.
!
!-----------------------------------------------------------------------
!
!  For more information on this software package, please visit
!  Scott's web site, Coyote Gulch Productions, at:
!
!      http://www.coyotegulch.com
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_init(3f) - [M_random:MERSENNE TWISTER] Initialize the Mersenne Twister random number generator with "seed"
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!    subroutine mtprng_init(seed, state)
!!    integer(INT32),     intent(in)  :: seed
!!    type(mtprng_state), intent(out) :: state
!!
!!##DESCRIPTION
!!    Initializes the Mersenne Twister random number generator with "seed"
!!
!!##OPTIONS
!!    seed   A seed value is used to start a specific sequence of pseudo-random numbers
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_init
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!       GET_SEED: block
!!       integer :: count
!!       integer :: count_rate
!!          call system_clock(count, count_rate)
!!          seed=count
!!       endblock GET_SEED
!!      call mtprng_init(seed, state)
!!      ! returns a INT64 integer with a range in 0 .. 2^32-1
!!      write(*,*) mtprng_rand64(state)
!!    end program demo_mtprng_init
!!
!!   Sample Results:
!!
!!      867010878
subroutine mtprng_init(seed, state)

! ident_8="@(#) M_random mtprng_int(3f) Initializes the Mersenne Twister random number generator with "seed""

! arguments
integer(INT32),     intent(in)  :: seed
type(mtprng_state), intent(out) :: state
   ! working storage
   integer :: i
   ! save seed
   state%mt(0) = seed

   ! Set the seed using values suggested by Matsumoto & Nishimura, using
   !   a generator by Knuth. See original source for details.
   do i = 1, N - 1
      state%mt(i) = iand(4294967295_INT64,1812433253_INT64 * ieor(state%mt(i-1),ishft(state%mt(i-1),-30_INT64)) + i)
   enddo

   state%mti = N

end subroutine mtprng_init
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_init_by_array(3f) - [M_random:MERSENNE TWISTER] Initialize the Mersenne Twister random number generator with "seed" array
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!    subroutine mtprng_init_by_array(init_key, state)
!!    integer(INT32), dimension(:), intent(in) :: init_key
!!    type(mtprng_state), intent(out) :: state
!!
!!##DESCRIPTION
!!    Initialize the Mersenne Twister random number generator with "seed" array
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!##RETURNS
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_init_by_array
!!    use M_random, only : mtprng_state, mtprng_init_by_array
!!    use M_random, only : mtprng_rand64, mtprng_rand_real1
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!    integer(INT32)     :: init_key(3)
!!    type(mtprng_state) :: state
!!      GET_SEED: block
!!      integer :: count
!!      integer :: count_rate
!!         call system_clock(count, count_rate)
!!         init_key(1) = 11*count
!!         init_key(2) = 37*count
!!         init_key(3) = 97*count
!!      endblock GET_SEED
!!      call mtprng_init_by_array(init_key, state )
!!      ! returns a INT64 integer with a range in 0 .. 2^32-1
!!      write(*,*) mtprng_rand64(state)
!!      ! returns a IEEE64 real, may be used as double precision
!!      write(*,*) mtprng_rand_real1(state)
!!    end program demo_mtprng_init_by_array
subroutine mtprng_init_by_array(init_key, state)

! ident_9="@(#) M_random mtprng_int_by_array(3f) Initialize with an array of seeds"

! arguments
integer(INT32),intent(in)       :: init_key(:)
type(mtprng_state), intent(out) :: state

   ! working storage
   integer :: key_length
   integer :: i
   integer :: j
   integer :: k

   call mtprng_init(19650218_INT32,state)

   i = 1
   j = 0
   key_length = size(init_key)

   do k = max(N,key_length), 0, -1
      state%mt(i) = ieor(state%mt(i),(ieor(state%mt(i-1),ishft(state%mt(i-1),-30_INT64) * 1664525_INT64))) + init_key(j+1) + j

      i = i + 1
      j = j + 1

      if (i >= N) then
         state%mt(0) = state%mt(N-1)
         i = 1
      endif

      if (j >= key_length) j = 0
   enddo

   do k = N-1, 0, -1
      state%mt(i) = ieor(state%mt(i),(ieor(state%mt(i-1),ishft(state%mt(i-1),-30_INT64) * 1566083941_INT64))) - i

      i = i + 1

      if (i>=N) then
         state%mt(0) = state%mt(N-1)
         i = 1
      endif
   enddo

   state%mt(0) = 1073741824_INT64 ! 0x40000000, assuring non-zero initial array

end subroutine mtprng_init_by_array
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand64(3f) - [M_random:MERSENNE TWISTER] Obtain the next 64-bit integer in the pseudo-random sequence
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!    function mtprng_rand64(state) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    integer(INT64) :: r
!!
!!##DESCRIPTION
!!    Obtain the next 64-bit integer in the pseudo-random sequence in the range 0 to 2^32-1.
!!    Note that the range is considerably below the value of HUGE(0_int64).
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!
!!##RETURNS
!!    r      next pseudo-random value in the range 0 to 2^32-1
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_rand64
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
!!    use, intrinsic :: iso_fortran_env, only : int32, int64
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      GET_SEED: block
!!      integer :: count
!!      integer :: count_rate
!!         call system_clock(count, count_rate)
!!      seed = count
!!      endblock GET_SEED
!!      call mtprng_init(seed, state)
!!      write(*,*) mtprng_rand64(state)
!!    end program demo_mtprng_rand64
function mtprng_rand64(state) result(r)

! ident_10="@(#) M_random mtprng_rand64(3f) Obtain the next 64-bit integer in the pseudo-random sequence"

! arguments
type(mtprng_state), intent(inout) :: state
!return type
integer(INT64) :: r

   ! internal constants
   integer(INT64), dimension(0:1), parameter :: mag01 = (/ 0_INT64, -1727483681_INT64 /)

   ! Period parameters
   integer(INT64), parameter :: UPPER_MASK =  2147483648_INT64
   integer(INT64), parameter :: LOWER_MASK =  2147483647_INT64

   ! Tempering parameters
   integer(INT64), parameter :: TEMPERING_B = -1658038656_INT64
   integer(INT64), parameter :: TEMPERING_C =  -272236544_INT64

   ! Note: variable names match those in original example
   integer(INT32) :: kk

   ! Generate N words at a time
   if (state%mti >= N) then
      ! The value -1 acts as a flag saying that the seed has not been set.
      if (state%mti == -1) call mtprng_init(4357_INT32,state)

      ! Fill the mt array
      do kk = 0, N - M - 1
         r = ior(iand(state%mt(kk),UPPER_MASK),iand(state%mt(kk+1),LOWER_MASK))
         state%mt(kk) = ieor(ieor(state%mt(kk + M),ishft(r,-1_INT64)),mag01(iand(r,1_INT64)))
      enddo

      do kk = N - M, N - 2
         r = ior(iand(state%mt(kk),UPPER_MASK),iand(state%mt(kk+1),LOWER_MASK))
         state%mt(kk) = ieor(ieor(state%mt(kk + (M - N)),ishft(r,-1_INT64)),mag01(iand(r,1_INT64)))
      enddo

      r = ior(iand(state%mt(N-1),UPPER_MASK),iand(state%mt(0),LOWER_MASK))
      state%mt(N-1) = ieor(ieor(state%mt(M-1),ishft(r,-1)),mag01(iand(r,1_INT64)))

      ! Start using the array from first element
      state%mti = 0
   endif

   ! Here is where we actually calculate the number with a series of
   !   transformations
   r = state%mt(state%mti)
   state%mti = state%mti + 1

 !-------------------------
 !*!r = ieor(r,ishft(r,-11))
   r = ieor(r,ishft(iand(4294967295_INT64,r),-11)) ! Added a 32-bit mask to first r shift
 !-------------------------

   r = iand(4294967295_INT64,ieor(r,iand(ishft(r, 7),TEMPERING_B)))
   r = iand(4294967295_INT64,ieor(r,iand(ishft(r,15),TEMPERING_C)))
   r = ieor(r,ishft(r,-18))

end function mtprng_rand64
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand(3f) - [M_random:MERSENNE TWISTER] Obtain the next 32-bit integer in the pseudo-random sequence
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!    function mtprng_rand(state) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    integer(INT32) :: r
!!
!!##DESCRIPTION
!!    Obtain the next 32-bit integer in the pseudo-random sequence
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!
!!##RETURNS
!!    r      The next 32-bit integer in the pseudo-random sequence
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_rand
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand
!!    use, intrinsic :: iso_fortran_env, only : int32
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      GET_SEED: block
!!      integer :: count
!!      integer :: count_rate
!!         call system_clock(count, count_rate)
!!         seed = count
!!      endblock GET_SEED
!!      call mtprng_init(seed, state)
!!      ! returns a INT32 integer with a range in 0 .. 2^31-1
!!      write(*,*) mtprng_rand(state)
!!    end program demo_mtprng_rand
function mtprng_rand(state) result(r)

! ident_11="@(#) M_random mtprng_rand(3f) Obtain the next 32-bit integer in the pseudo-random sequence"

! arguments
type(mtprng_state), intent(inout) :: state
!return type
integer(INT32) :: r

   ! working storage
   integer(INT64) :: x

   ! done
   x = mtprng_rand64(state)

   if (x > 2147483647_INT64) then
      r = x - 4294967296_INT64
   else
      r = x
   endif

end function mtprng_rand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand_range(3f) - [M_random:MERSENNE TWISTER] Obtain a pseudo-random integer in the range [lo,hi]
!!    (LICENSE:CUSTOM OPEN)
!!##SYNOPSIS
!!
!!    function mtprng_rand_range(state, lo, hi) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    integer, intent(in) :: lo
!!    integer, intent(in) :: hi
!!    integer(INT32) :: r
!!
!!##DESCRIPTION
!!    Obtain a pseudo-random integer in the range [lo,hi]
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!    lo     lowest value in desired range of values to return
!!    hi     highest value in desired range of values to return
!!
!!##RETURNS
!!    r      returned pseudo-random value in range from LO to HI
!!
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_rand_range
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand_range
!!    use, intrinsic :: iso_fortran_env, only : int32
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      GET_SEED: block
!!      integer :: count
!!         integer :: count_rate
!!         call system_clock(count, count_rate)
!!         seed = count
!!      endblock GET_SEED
!!      call mtprng_init(seed, state)
!!      write(*,*) mtprng_rand_range(state,20,30)
!!    end program demo_mtprng_rand_range
function mtprng_rand_range(state, lo, hi) result(r)

! ident_12="@(#) M_random mtprng_rand_range(3f) Obtain a pseudo-random integer in the range [lo hi]"

! arguments
type(mtprng_state), intent(inout) :: state
integer, intent(in) :: lo
integer, intent(in) :: hi
! return type
integer(INT32) :: r

   ! Use real value to calculate range
   r = lo + floor((hi - lo + 1.0_IEEE64) * mtprng_rand_real2(state))

end function mtprng_rand_range
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand_real1(3f) - [M_random:MERSENNE TWISTER] Obtain a pseudo-random real number in the range [0.0,1.0]
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!    function mtprng_rand_real1(state) result(r)
!!    type(mtprng_state), intent(inout) :: state
!!    real(IEEE64) :: r
!!##DESCRIPTION
!!    Obtain a pseudo-random real number in the range [0,1], i.e., a number
!!    greater than or equal to 0 and less than or equal to 1.
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!##RETURNS
!!     r      ...
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_real1
!!    use M_random, only : mtprng_init
!!    use M_random, only : mtprng_state
!!    use M_random, only : mtprng_rand_real1
!!    use, intrinsic :: iso_fortran_env, only : int32
!!    implicit none
!!    integer(INT32) :: seed
!!    type(mtprng_state) :: state
!!      GET_SEED: block
!!      integer :: count
!!      integer :: count_rate
!!         call system_clock(count, count_rate)
!!         seed = count
!!      endblock GET_SEED
!!      call mtprng_init(seed, state)
!!      write(*,*) mtprng_rand_real1(state)
!!    end program demo_mtprng_real1
function mtprng_rand_real1(state) result(r)

! ident_13="@(#) M_random mtprng_rand_real1(3f) Obtain a pseudo-random real number .ge. 0 and .le.= 1."

! arguments
type(mtprng_state), intent(inout) :: state
! return type
real(IEEE64) :: r

   ! Local constant; precalculated to avoid division below
   real(IEEE64), parameter :: factor = 1.0_IEEE64 / 4294967295.0_IEEE64

   ! compute
   r = real(mtprng_rand64(state),IEEE64) * factor

end function mtprng_rand_real1
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand_real2(3f) - [M_random:MERSENNE TWISTER] Obtain a pseudo-random real number in the range [0,<1)
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!    Obtain a pseudo-random real number in the range [0,1), i.e., a number
!!    greater than or equal to 0 and less than 1.
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!##RETURNS
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_mtprng_real2
!!    use M_random, only : mtprng_state, mtprng_init, mtprng_rand_real2
!!    use, intrinsic :: iso_fortran_env, only : int32
!!    implicit none
!!    integer(INT32)     :: seed
!!    type(mtprng_state) :: state
!!      GET_SEED: block
!!      integer :: count
!!      integer :: count_rate
!!         call system_clock(count, count_rate)
!!         seed = count
!!      endblock GET_SEED
!!      call mtprng_init(seed, state)
!!      ! returns a IEEE64 real, may be used as double precision
!!      write(*,*) mtprng_rand_real2(state)
!!    end program demo_mtprng_real2
function mtprng_rand_real2(state) result(r)
! ident_14="@(#) M_random mtprng_rand_real2(3f) Obtain a pseudo-random real number .ge. 0.0 and .lt. 1.0"

type(mtprng_state), intent(inout) :: state                                   ! arguments
real(IEEE64)                      :: r                                       ! return type
   real(IEEE64), parameter        :: factor=1.0_IEEE64 / 4294967296.0_IEEE64 ! Local constant; precalculated to avoid division below

   r = real(mtprng_rand64(state),IEEE64) * factor                            ! compute

end function mtprng_rand_real2
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    mtprng_rand_real3(3f) - [M_random:MERSENNE TWISTER] Obtain a pseudo-random real number in the range (0< XXX <1)
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!     function mtprng_rand_real3(state) result(r)
!!     type(mtprng_state), intent(inout) :: state
!!     real(IEEE64) :: r
!!
!!##DESCRIPTION
!!    Obtain a pseudo-random real number in the range (0,1), i.e., a number
!!    greater than 0 and less than 1.
!!
!!##OPTIONS
!!    state  generator state initialized by mtprng_init(3f) or mtprng_init_array(3f)
!!##RETURNS
!!    r      a pseudo-random real number greater than 0 and less than 1.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!     program demo_mtprng_real3
!!     use M_random, only : mtprng_state, mtprng_init, mtprng_rand_real3
!!     use, intrinsic :: iso_fortran_env, only : int32
!!     implicit none
!!     integer(INT32) :: seed
!!     type(mtprng_state) :: state
!!       GET_SEED: block
!!       integer :: count
!!       integer :: count_rate
!!          call system_clock(count, count_rate)
!!          seed = count
!!       endblock GET_SEED
!!       call mtprng_init(seed, state)
!!       write(*,*) mtprng_rand_real3(state)
!!     end program demo_mtprng_real3
function mtprng_rand_real3(state) result(r)

! ident_15="@(#) M_random mtprng_rand_real3(3f) Obtain a pseudo-random real number .gt. 0 and .lt. 1."

! arguments
type(mtprng_state), intent(inout) :: state
! return type
real(IEEE64) :: r

   ! Local constant; precalculated to avoid division below
   real(IEEE64), parameter :: factor = 1.0_IEEE64 / 4294967296.0_IEEE64

   r = (real(mtprng_rand64(state),IEEE64) + 0.5_IEEE64) * factor

end function mtprng_rand_real3
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_random
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
