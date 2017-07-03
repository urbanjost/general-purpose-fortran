!>
!!##NAME
!!      M_factor - [M_factor]module for least common multiple, greatest common divisor, and prime factors
!!##SYNOPSIS
!!
!!
!!    least_common_multiple     least common multiple of two integers (i,j) or
!!                              integer array m(:|:,:|:,:,:)
!!    greatest_common_divisor   greatest common divisor of two integers (i,j) or
!!                              integer array m(:|:,:|:,:,:)
!!    prime_factors             prime factors of a number
!!    i_is_prime                determine if an integer is a prime
!!
!!##DESCRIPTION
!!    This module is a collection of procedures that perform common functions
!!    found in arithmetic and number theory such as Least Common Multiples,
!!    Greatest Common Divisors, and Prime Factors of INTEGER variables.
!!    The INTEGER values are typically limited to values up to the magnitude
!!    (2**31)-1= 2147483647.
!!
!!##PRIMES
!!    Date     10/06/97 at 12:47:29
!!    From     Doctor Rob
!!    Subject  Re: The number 1 and zero
!!
!!    One is neither a prime nor a composite number. A prime number is one
!!    with exactly two positive divisors, itself and one. One has only one
!!    positive divisor. It cannot be written as a product of two factors,
!!    neither of which is itself, so one is also not composite. It falls
!!    in a class of numbers called units. These are the numbers whose
!!    reciprocals are also whole numbers.
!!
!!    Zero is not a prime or a composite number either. Zero has an infinite
!!    number of divisors (any nonzero whole number divides zero). It cannot
!!    be written as a product of two factors, neither of which is itself, so
!!    zero is also not composite. It falls in a class of numbers called
!!    zero-divisors. These are numbers such that, when multiplied by some
!!    nonzero number, the product is zero.
!!
!!    The most important fact of multiplication of integers is called the
!!    Fundamental Theorem of Arithmetic. It says that every whole number
!!    greater than one can be written *uniquely* (except for their order) as
!!    the product of prime numbers. This is so important that we tailor our
!!    idea of what a prime number is to make it true. If 1 were a prime
!!    number, this would be false, since, for example,
!!
!!       7 = 1*7 = 1*1*7 = 1*1*1*7 = ...,
!!
!!    and the uniqueness would fail.
!!
!!##EXAMPLES
!!
!!     The individual man(1) pages for each procedure contain examples and
!!     a full description of the procedure parameters.
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
module M_factor
private
!-----------------------------------------------------------------------------------------------------------------------------------
 public prime_factors                  ! prime factors of a number
 public i_is_prime                     ! determine if an integer is prime or not
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident1='@(#)M_factor::least_common_multiple(3f): function finds LCM for (i,j)|m(:)|m(:,:)|m(:,:,:)'
 public least_common_multiple
    private lcm                    ! least common multiple of two integers
    private lcm_vector             ! least common multiple of integer array m(:)
    private lcm_matrix             ! least common multiple of integer array m(:,:)
    private lcm_cuboid             ! least common multiple of integer cuboid m(:,:,:)

interface least_common_multiple
   module procedure lcm
   module procedure lcm_vector
   module procedure lcm_matrix
   module procedure lcm_cuboid
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
character(len=*),parameter::ident2='@(#)M_factor::greatest_common_divisor(3f): function finds GCD for (i,j)|m(:)|m(:,:)|m(:,:,:)"'

public greatest_common_divisor
   public gcd                      ! greatest common divisor of two integers
   public gcd_2                    ! greatest common divisor of two integers
   private gcd_vector              ! greatest common divisor of integer vector m(:)
   private gcd_matrix              ! greatest common divisor of integer array m(:,:)
   private gcd_cuboid              ! greatest common divisor of integer cuboid m(:,:,:)

interface greatest_common_divisor
   module procedure gcd
   module procedure gcd_vector
   module procedure gcd_matrix
   module procedure gcd_cuboid
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    least_common_multiple(3f) - [M_factor]Least common multiple of two integers
!!                                or vector m(:), matrix m(:,:) or cuboid m(:,:,:)
!!##SYNOPSIS
!!
!!    integer function least_common_multiple(i,j)
!!     integer,intent(in):: i,j
!!      or
!!    integer function least_common_multiple(m)
!!     integer,intent(in):: m(:)
!!      or
!!     integer,intent(in):: m(:,:)
!!      or
!!     integer,intent(in):: m(:,:,:)
!!
!!##DESCRIPTION
!!    From Wikipedia, the free encyclopedia:
!!
!!    In arithmetic and number theory, the least common multiple (also
!!    called the lowest common multiple or smallest common multiple) of
!!    two integers a and b, usually denoted by LCM(a, b), is the smallest
!!    positive integer that is divisible by both a and b. Since division
!!    of integers by zero is undefined, this definition has meaning only
!!    if a and b are both different from zero. However, some authors define
!!    lcm(a,0) as 0 for all a, which is the result of taking the LCM to be
!!    the least upper bound in the lattice of divisibility.
!!
!!    The LCM is familiar from grade-school arithmetic as the "lowest common
!!    denominator" (LCD) that must be determined before fractions can be
!!    added, subtracted or compared. The LCM of more than two integers
!!    is also well-defined: it is the smallest positive integer that is
!!    divisible by each of them.
!!
!!##EXAMPLE
!!
!!   Sample Program:
!!
!!    program demo_lcm
!!    use M_factor, only : lcm=>least_common_multiple
!!    implicit none
!!       write(*,*)'SCALAR:'
!!          call writeit(10,24,120)
!!          call writeit(15,30,30)
!!          call writeit(-15,-30,30)
!!          call writeit(15,-30,30)
!!          call writeit(-15,30,30)
!!
!!       write(*,*)'VECTOR:'
!!          call writeit_v([10,24],120)
!!          call writeit_v([15,30],30)
!!          call writeit_v([-15,-30],30)
!!          call writeit_v([5,-15,-40],120)
!!          call writeit_v([2,3,4,5],60)
!!       write(*,*)'Special cases:'
!!          call writeit_v([15,0],0)
!!          call writeit_v([-15,0],0)
!!          call writeit_v([0],0)
!!          call writeit_v([-10],10)
!!          call writeit_v([22],22)
!!          call writeit_v([0,0],0)
!!          call writeit_v([0,0,0,0,0],0)
!!          call writeit_v([0,0,0,-1,0],0)
!!          call writeit_v([0,0,0,33,0,3,11],0)
!!       contains
!!
!!       subroutine writeit(ii,jj,answer)
!!       integer,intent(in) :: ii,jj
!!       integer,intent(in) :: answer
!!          write(*,'("  For lcm(",I0,",",I0,") the value is ",I0," which is ",L1)')&
!!             & ii,jj,lcm(ii,jj),lcm(ii,jj).eq.answer
!!       end subroutine writeit
!!
!!       subroutine writeit_v(array,answer)
!!       integer,intent(in) :: array(:)
!!       integer,intent(in) :: answer
!!          write(*,'("  For lcm([",*(i0:,1x))',advance='no')array
!!          write(*,'("]) the value is ",i0," which is ",L1)') &
!!             & lcm(array),lcm(array).eq.answer
!!       end subroutine writeit_v
!!
!!    end program demo_lcm
!!
!!   Expected results:
!!
!!     > SCALAR:
!!     >  For lcm(10,24) the value is 120 which is T
!!     >  For lcm(15,30) the value is 30 which is T
!!     >  For lcm(-15,-30) the value is 30 which is T
!!     >  For lcm(15,-30) the value is 30 which is T
!!     >  For lcm(-15,30) the value is 30 which is T
!!     > VECTOR:
!!     >  For lcm([10 24]) the value is 120 which is T
!!     >  For lcm([15 30]) the value is 30 which is T
!!     >  For lcm([-15 -30]) the value is 30 which is T
!!     >  For lcm([5 -15 -40]) the value is 120 which is T
!!     >  For lcm([2 3 4 5]) the value is 60 which is T
!!     > Special cases:
!!     >  For lcm([15 0]) the value is 0 which is T
!!     >  For lcm([-15 0]) the value is 0 which is T
!!     >  For lcm([0]) the value is 0 which is T
!!     >  For lcm([-10]) the value is 10 which is T
!!     >  For lcm([22]) the value is 22 which is T
!!     >  For lcm([0 0]) the value is 0 which is T
!!     >  For lcm([0 0 0 0 0]) the value is 0 which is T
!!     >  For lcm([0 0 0 -1 0]) the value is 0 which is T
!!     >  For lcm([0 0 0 33 0 3 11]) the value is 0 which is T
!!
!!##METHOD
!!    Reduction by the greatest common divisor
!!
!!    The following formula reduces the problem of computing the least
!!    common multiple to the problem of computing the greatest common divisor
!!    (GCD), also known as the greatest common factor:
!!
!!     lcm(a,b) = |a*b| / gcd(a,b)
!!
!!    This formula is also valid when exactly one of a and b is 0, since
!!    gcd(a, 0) = |a|. (However, if both a and b are 0, this formula would
!!    cause division by zero; lcm(0, 0) = 0 is a special case.
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
integer function lcm(i,j)
use M_anyscalar,     only : int128
implicit none

character(len=*),parameter::ident="@(#)M_factor::lcm(3fp): least common multiple of two integers"

integer,intent(in):: i,j
integer(kind=int128) :: lcm_big

   if(i.eq.0.and.j.eq.0)then
      lcm=0
   else
      ! if default integer is 32-bit integer
      ! would be limited to input values < sqrt((2**31)-1) if did not increase from default kind, assuming default <= int64
      lcm_big=abs(int(i,kind=int128)*int(j,kind=int128) )/gcd(i,j)
      if(lcm_big.gt.huge(0))then
         write(*,*)'*lcm* result larger than a standard integer =',lcm_big
         stop 1
      else
         lcm=lcm_big
      endif
   endif

end function lcm
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    lcm_vector(3fp) - least common multiple of integer vector m(:)
!!##SYNOPSIS
!!
!!    integer function lcm_vector(m)
!!
!!     integer,intent(in)  :: m(:)
!!##DESCRIPTION
!!    Find the Least Common Denominator of an INTEGER vector M(:).
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
integer function lcm_vector(m)
implicit none
character(len=*),parameter :: ident="@(#)M_factor::lcm_vector(3fp): least common multiple of integer array m(:)"
   integer,intent(in)  :: m(:)
   integer             :: i
   integer             :: vsize

   vsize=size(m)
   select case(vsize)
   case(:0)                  ! special case for null input list
      lcm_vector=0           ! return 0 ; optionally should fail
   case default
      lcm_vector=m(1)        ! so first call has two defined values duplicate first value
      do i=1,vsize           !
         lcm_vector=lcm(m(i),lcm_vector)
      enddo
   end select

end function lcm_vector
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    lcm_matrix(3fp) - least common multiple of integer array m(:,:)
!!##SYNOPSIS
!!
!!    integer function lcm_matrix(m)
!!
!!     integer,intent(in)  :: m(:,:)
!!##DESCRIPTION
!!    Find the Least Common Denominator of an INTEGER matrix M(:,:).
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
integer function lcm_matrix(m)
implicit none
character(len=*),parameter :: ident="@(#)M_factor::lcm_matrix(3fp):least common multiple of integer matrix m(:,:)"
integer,intent(in) :: m(:,:)
   lcm_matrix=lcm_vector(reshape(m,[size(m)]))
end function lcm_matrix
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    lcm_cuboid(3fp) - least common multiple of integer cuboid m(:,:,:)
!!##SYNOPSIS
!!
!!    integer function lcm_cuboid(m)
!!
!!     integer,intent(in)  :: m(:,:,:)
!!##DESCRIPTION
!!    Find the Least Common Denominator of an INTEGER cuboid M(:,:,:).
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
integer function lcm_cuboid(m)
implicit none
character(len=*),parameter :: ident="@(#)M_factor::lcm_cuboid(3fp):least common multiple of integer cuboid m(:,:,:)"
integer,intent(in) :: m(:,:,:)
   lcm_cuboid=lcm_vector(reshape(m,[size(m)]))
end function lcm_cuboid
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    greatest_common_divisor(3f) - [M_factor]calculate greatest common divisor of
!!                                  two integers or vector m(:),
!!                                  matrix m(:,:) or cuboid m(:,:,:)
!!
!!##SYNOPSIS
!!
!!   The function is generic and may take either two integers or an integer
!!   vector, matrix, or cuboid.
!!
!!    integer function greatest_common_divisor(i,j)
!!     integer,intent(in)::  i,j
!!      or
!!    integer function greatest_common_divisor(m)
!!     integer,intent(in)::  m(:)
!!      or
!!     integer,intent(in)::  m(:,:)
!!      or
!!     integer,intent(in)::  m(:,:,:)
!!
!!##DESCRIPTION
!!
!!  The method used is the Euler algorithm; that for two integers ...
!!
!!    1. Subtract the 2nd number (N) as many times as possible
!!       from the 1st one (M) and save remainder using FORTRAN
!!       function MOD.
!!
!!    2. Test if remainder is equal to zero, if so GCD = N.
!!       If not replace M with N and N with remainder and
!!       proceed with step 1.
!!
!!    3. Repeat both steps until remainder becomes zero.
!!
!!##EXAMPLE
!!
!!
!!  Sample program:
!!
!!    program demo_greatest_common_divisor
!!    use M_factor, only : gcd=>greatest_common_divisor
!!    implicit none
!!    integer, allocatable :: matrix(:,:)
!!       write(*,*)'SCALAR:'
!!          call writeit(26,130,26)
!!          call writeit(91,390,13)
!!          call writeit(-91,390,13)
!!          call writeit(91,-390,13)
!!          call writeit(-41,-43,1)
!!          call writeit(-20,-10,10)
!!          call writeit(20,10,10)
!!       write(*,*)'VECTOR:'
!!          call writeit_v([26,130,91,390],13)
!!          call writeit_v([5,7,11,13,17,19,23,29,31,37,41,43,47],1)
!!          call writeit_v([-20,-10,0],10)
!!          call writeit_v([20,10,0],10)
!!          call writeit_v([26,130],26)
!!          call writeit_v([91,390],13)
!!          call writeit_v([-91,390],13)
!!          call writeit_v([91,-390],13)
!!          call writeit_v([-41,-43],1)
!!          call writeit_v([-20,-10],10)
!!          call writeit_v([20,10],10)
!!       write(*,*)'MATRIX:'
!!          matrix=reshape([ 11,22,33,44,55,66],[2,3])
!!          call write_matrix(matrix,11)
!!          matrix=reshape([5,7,11,13,17,19,23,29,31,37,41,43,47],[13,1])
!!          call write_matrix(matrix,1)
!!          matrix=reshape([40,80,120,160],[2,2])
!!          call write_matrix(matrix,40)
!!
!!       contains
!!
!!       subroutine writeit(ii,jj,answer)
!!       integer,intent(in) :: ii,jj
!!       integer,intent(in) :: answer
!!          write(*,'("gcd([",i0,",",i0,"]) produces ",i0," which is ",l1)') &
!!               & ii,jj,gcd(ii,jj),gcd(ii,jj).eq.answer
!!       end subroutine writeit
!!
!!       subroutine writeit_v(vector,answer)
!!       integer,intent(in) :: vector(:)
!!       integer,intent(in) :: answer
!!          write(*,'("gcd([",*(i0:,","))',advance='no')vector
!!          write(*,'("]) produces ",i0," which is ",l1)') &
!!               & gcd(vector),gcd(vector).eq.answer
!!       end subroutine writeit_v
!!
!!       subroutine write_matrix(matrix,answer)
!!       integer,intent(in) :: matrix(:,:)
!!       integer,intent(in) :: answer
!!          write(*,*)'MATRIX SHAPE:',size(matrix,dim=1),size(matrix,dim=2)
!!          write(*,'("gcd([",*(i0:,","))',advance='no')matrix
!!          write(*,'("]) produces ",i0," which is ",l1)') &
!!               & gcd(matrix),gcd(matrix).eq.answer
!!       end subroutine write_matrix
!!
!!    end program demo_greatest_common_divisor
!!  Expected Output:
!!
!!    >  SCALAR:
!!    > gcd([26,130]) produces 26 which is T
!!    > gcd([91,390]) produces 13 which is T
!!    > gcd([-91,390]) produces 13 which is T
!!    > gcd([91,-390]) produces 13 which is T
!!    > gcd([-41,-43]) produces 1 which is T
!!    > gcd([-20,-10]) produces 10 which is T
!!    > gcd([20,10]) produces 10 which is T
!!    >  VECTOR:
!!    > gcd([26,130,91,390]) produces 13 which is T
!!    > gcd([5,7,11,13,17,19,23,29,31,37,41,43,47]) produces 1 which is T
!!    > gcd([-20,-10,0]) produces 10 which is T
!!    > gcd([20,10,0]) produces 10 which is T
!!    > gcd([26,130]) produces 26 which is T
!!    > gcd([91,390]) produces 13 which is T
!!    > gcd([-91,390]) produces 13 which is T
!!    > gcd([91,-390]) produces 13 which is T
!!    > gcd([-41,-43]) produces 1 which is T
!!    > gcd([-20,-10]) produces 10 which is T
!!    > gcd([20,10]) produces 10 which is T
!!    >  MATRIX:
!!    >  MATRIX SHAPE:           2           3
!!    > gcd([11,22,33,44,55,66]) produces 11 which is T
!!    >  MATRIX SHAPE:          13           1
!!    > gcd([5,7,11,13,17,19,23,29,31,37,41,43,47]) produces 1 which is T
!!    >  MATRIX SHAPE:           2           2
!!    > gcd([40,80,120,160]) produces 40 which is T
!===================================================================================================================================
!>
!! FUNCTION:    gcd(M,N)
!! DATE:        30-Aug-2015
!!##VERSION:     V1.0.0
!! MODIFIED BY: John S. Urban
!! PURPOSE:     Computes the Greatest Common Divisor of two integers.
!!              If M=N=0 then the GCD is defined to be zero.
!!              IE. GCD is a factor of M and N and GCD is the largest such factor.
!! REFERENCES:  Euler algorithm (slow)
!! UUID:        UUID=de642070-4ecd-414b-895c-578ea7547948
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
function gcd(m,n) result(answer)
implicit none
character(len=*),parameter :: ident="@(#)M_factor::gcd(3fp): compute greatest common divisor of two integers"
integer,intent(in)  :: m, n
integer             :: answer
!-----------------------------------------------------------------------------------------------------------------------------------
   integer          :: irest
   intrinsic        :: mod,iabs
   integer          :: ifirst
!-----------------------------------------------------------------------------------------------------------------------------------
   ifirst=iabs(m)
   answer=iabs(n)
   if(answer.eq.0)then
      answer=ifirst
   else
      do
         irest = mod(ifirst,answer)
         if(irest == 0)  exit
         ifirst = answer
         answer = irest
      enddo
      answer= iabs(answer)
   endif
end function gcd
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
integer function gcd_2(m,n)
implicit none
integer,intent(in) :: m,n
integer :: ia,ib,itemp
   ia = m
   ib = n
   do
      if(ib.eq.0)exit
      itemp = ia
      ia = ib
      ib = mod(itemp, ib)
   enddo
   gcd_2 = iabs(ia)
end function gcd_2
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()-
!-----------------------------------------------------------------------------------------------------------------------------------
integer function gcd_vector(m)
implicit none
character(len=*),parameter :: ident="@(#)M_factor::gcd_vector(3fp):greatest common divisor of integer vector m(:)"
integer,intent(in) :: m(:)
integer            :: vsize
integer            :: i
   vsize=size(m)
   if(vsize.gt.0)then
      gcd_vector = m(1)
      TILLONE: do i=1,vsize
         gcd_vector = gcd(gcd_vector,iabs(m(i)))
         if (gcd_vector.eq.1) exit TILLONE
      enddo TILLONE
   else
      gcd_vector=0
   endif
end function gcd_vector
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
integer function gcd_matrix(m)
implicit none
character(len=*),parameter :: ident="@(#)M_factor::gcd_matrix(3fp):greatest common divisor of integer matrix array m(:,:)"
integer,intent(in) :: m(:,:)
integer :: i,j
   if(size(m).gt.0)then
      gcd_matrix = m(1,1)
      OUTER: do j = 1, size(m,dim=2)
         INNER: do i = 1, size(m,dim=1)
            gcd_matrix = gcd(gcd_matrix,iabs(m(i,j)))
            if (gcd_matrix.eq.1) exit OUTER
         enddo INNER
      enddo OUTER
   else
      gcd_matrix=0
   endif
end function gcd_matrix
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
integer function gcd_cuboid(m)
implicit none
character(len=*),parameter :: ident="@(#)M_factor::gcd_cuboid(3fp):greatest common divisor of integer cuboid m(:,:,:)"
integer,intent(in) :: m(:,:,:)
   gcd_cuboid=gcd_vector(reshape(m,[size(m)]))
end function gcd_cuboid
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    prime_factors - [M_factor]decompose a number into its prime factors
!!##SYNOPSIS
!!
!!    call prime_factors(number,nprm,iprm,iexp[,verbose])
!!
!!     integer, intent(in)          :: number
!!     integer, intent(out)         :: nprm
!!     integer, intent(out)         :: iprm(:)
!!     integer, intent(out)         :: iexp(:)
!!     logical, intent(in),optional :: verbose
!!##DESCRIPTION
!!
!!    1. Upon return from PRIME_FACTORS,
!!
!!            NUMBER = IPRM(1)**IEXP(1) * IPRM(2)**IEXP(2) * ...
!!                     *IPRM(NPRM)**IEXP(NPRM)
!!
!!    2. A number represented by a (single-precision) INTEGER
!!       value on the VMS VAX cluster can have at most 9 distinct
!!       prime factors. On machines where the maximum integer is
!!       larger than 2**31 - 1, IPRM and IEXP would, in general,
!!       have to be dimensioned larger since larger numbers may
!!       have more than 9 distinct prime factors.
!!##OPTIONS
!!
!!    NUMBER   INTEGER constant or variable, number to be decomposed into
!!             prime factors. NUMBER .ge. 2.
!!             For 32-bit integers NUMBER <= 2147483647
!!    NPRM     INTEGER variable, will contain the number of distinct prime
!!             factors of the number.
!!    IPRM     INTEGER array of size at least 9, will contain the prime
!!             factors of the number.
!!    IEXP     INTEGER array of size at least 9, will contain the
!!             exponents of the corresponding prime factors.
!!    verbose  optional LOGICAL constant or variable, controls printing
!!             of results.
!!              o .false. - Results are not printed.
!!              o .true. - Results are printed.
!!##EXAMPLE
!!
!!
!!  Sample program:
!!
!!    program find_prime_factors
!!    use M_factor, only : prime_factors
!!    implicit none
!!       integer  :: number
!!       integer  :: iexp(10), iprm(10), nprm
!!       logical  :: verbose=.true.
!!       integer  :: ios
!!       do
!!         write(*,'(a)', advance='no') ' Enter number to be factored: '
!!         read(*,*,iostat=ios,end=999) number
!!         if(ios.eq.0)then
!!            call prime_factors(number, nprm, iprm, iexp, verbose)
!!         endif
!!       enddo
!!    999 continue
!!    end program find_prime_factors
!!
!!
!!##PEDIGREE
!!    o Coded at Madison Academic Computing Center,
!!      University of Wisconsin, Madison
!!    o FORTRAN 77 Version 1988.09
!!    o Code converted using TO_F90 by Alan Miller, 2000-07-14T11:42:45
!!    o Fortran 2003 version 20160918 by John S. Urban
!===================================================================================================================================
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine prime_factors (number, nprm, iprm, iexp, verbose)
implicit none
character(len=*),parameter :: ident="@(#)M_factor::prime_factors(3f):decompose a number into its prime factors"
integer, intent(in)   :: number  ! number to factor
integer, intent(out)  :: nprm    ! number of distinct prime factors
integer, intent(out)  :: iprm(:) ! distinct prime factors found, assumed dimensioned to at least 9
integer, intent(out)  :: iexp(:) ! exponents of the corresponding prime factors in IPRM
logical, intent(in),optional   :: verbose ! .false. no printing; .true. print results
!-----------------------------------------------------------------------------------------------------------------------------------
!--------LOCAL VARIABLES
   integer            :: div, indx, ii, j, n, offset, olddiv, quo, rem
   !---------data to obtain trial divisors 2, 3, 5, 7 and all higher numbers not divisible by 2, 3, 5, 7.
   integer, parameter  :: base(52) = (/   &
       211, 209, 199, 197, 193, 191, 187, 181, 179, 173, 169, 167, 163, &
       157, 151, 149, 143, 139, 137, 131, 127, 121, 113, 109, 107, 103, &
       101,  97,  89,  83,  79,  73,  71,  67,  61,  59,  53,  47,  43, &
        41,  37,  31,  29,  23,  19,  17,  13,  11,   7,   5,   3,   2 /)
   !--------FORMAT TEMPLATE
   logical            :: verbose_local
!-----------------------------------------------------------------------------------------------------------------------------------
   if(present(verbose))then
      verbose_local=verbose
   else
      verbose_local=.false.
   endif
   !--------CHECK NUMBER.  MUST BE .GE. 2
   if (number < 2) then !--------ERROR, ISSUE MESSAGE AND TAKE ERROR EXIT.
      write (*, "(' *** ERROR IN PRIME_FACTORS, NUMBER =', i12, ', NUMBER MUST BE >= 2')") number
      nprm=0
      return
   endif
   !--------INITIALIZATIONS.
   j = 0
   n = number
   olddiv = 0
   offset = 0
   indx = 53
   !--------GET NEXT TRIAL DIVISOR.
   do
     indx = indx - 1
     if (indx <= 0) then
       indx = 48
       offset = offset + 210
     endif
     div = offset + base(indx)
     !--------TEST TRIAL DIVISOR.
        do
           quo = n / div
           rem = n - quo*div
           if (rem /= 0) exit
           !--------FACTOR FOUND, ZERO REMAINDER.
           n = quo
           if (div <= olddiv) then
             !--------MULTIPLE FACTOR.
             iexp(j) = iexp(j) + 1
             cycle
           endif
           !--------NEW FACTOR.
           j = j + 1
           iprm(j) = div
           iexp(j) = 1
           olddiv = div
        enddo
        !--------NOT A FACTOR, POSITIVE REMAINDER.  CHECK DIVISOR SIZE.
     if (div >= quo) exit
   enddo
   !--------FINISHED, WHAT ISN'T FACTORED IS A PRIME (OR 1).
   if (n > 1) then
     j = j + 1
     iexp(j) = 1
     iprm(j) = n
   endif
   nprm = j
   if (verbose_local) then                                           !--------PRINT RESULTS IF REQUESTED.
      if(nprm /= 1 .or. iexp(1) /= 1) then                           !--------NUMBER IS COMPOSITE
         write (*, "( t3, i0, ' factors as ')",advance='no') number

         !! write (*,'(*("(",i0,"**",i0,")":,"*"))') (iprm(ii),iexp(ii),ii=1,nprm)
         do ii=1,nprm
            if(iexp(ii).eq.1)then
               write(*,'(i0)',advance='no') iprm(ii)
            else
               write(*,'("(",i0,"**",i0,")")',advance='no') iprm(ii),iexp(ii)
            endif
            if(ii.eq.nprm)then
               write(*,*)
            else
               write(*,'("*")',advance='no')
            endif
         enddo

      else
         write (*,"( t3, i0, ' IS A PRIME NUMBER' )") number         !--------NUMBER IS PRIME
      endif
   endif
end subroutine prime_factors
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    i_is_prime(3f) - [M_factor]Determine if a number is prime using Sieve of Erasthosthenes
!!##SYNOPSIS
!!
!!    function i_is_prime ( n )
!!
!!     integer,intent(in) :: n
!!
!!##DESCRIPTION
!!    A simple, unoptimized sieve of Erasthosthenes is used to
!!    check whether N can be divided by any integer between 2
!!    and SQRT(N).
!!
!!##VERSION
!!    29 November 1998
!!
!!##AUTHOR
!!    John Burkardt
!!
!!##PARAMETERS
!!    n  Input, integer N, the integer to be tested.
!!##RETURNS
!!    Output  logical I_IS_PRIME(3f) is TRUE if N is prime, and FALSE
!!            otherwise.  Note that negative numbers and 0 are not
!!            considered prime.
!!##EXAMPLE
!!
!!    sample program
!!
!!     program testit
!!     use m_factor, only: i_is_prime
!!     implicit none
!!     integer  :: i
!!     integer  :: icount=0
!!     integer  :: isum=0
!!     integer,parameter :: n= 10000
!!
!!     do i=2, n
!!        if(i_is_prime(i))then
!!           icount=icount+1
!!           isum=isum+i
!!           write(*,*)icount,i
!!        endif
!!     enddo
!!
!!     write(*,*)'number of primes between 2 and ',n,' is ',icount
!!     write(*,*)'sum of primes between 2 and ',n,' is ',isum
!!
!!     end program testit
!! !
!===================================================================================================================================
function i_is_prime ( n )
implicit none
character(len=*),parameter :: ident="@(#)M_factor::i_is_prime(3f): reports whether an integer is prime"
  integer,intent(in) :: n
  integer            :: i
  logical            :: i_is_prime
!
   FINDPRIME: block
     if ( n <= 0 ) then
       i_is_prime = .false.
     else if ( n <= 3 ) then
       i_is_prime = .true.
     else
        do i = 2, int( sqrt ( real ( n ) ) )
          if ( mod ( n, i ) == 0 ) then
            i_is_prime = .false.
            exit FINDPRIME
          end if
        end do
        i_is_prime = .true.
     endif
   endblock FINDPRIME
end function I_IS_PRIME
!-----------------------------------------------------------------------------------------------------------------------------------
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!-----------------------------------------------------------------------------------------------------------------------------------
end module M_factor
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
