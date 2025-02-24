










!>
!!##NAME
!!    M_overload(3fm) - [M_overload::INTRO] overloads of standard operators and intrinsic procedures
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!  overloads on operators
!!
!!    use M_overload, only : operator(==), operator(/=)
!!    ! use == like .eqv.; ie. logical==logical
!!    ! use /= like .neqv.; ie. logical/=logical
!!
!!    use M_overload, only : operator(//)
!!    ! convert intrinsics to strings and concatenate
!!
!!  overloads on INTRINSICS to take strings, logicals, and metamorphic numeric intrinsic values
!!
!!    use M_overload, only : int, real, dble
!!    ! int('string')   int(logical)   int(class(*))
!!    ! real('string')  real(logical)  real(class(*))
!!    ! dble('string')  dble(logical)  dble(class(*))
!!
!!    use M_overload, only : sign
!!    ! When sign(3f) is given a single value, call sign(1,value); ie.  sign(value)
!!    use M_overload, only : merge
!!    ! Allow strings of different length in MERGE
!!
!!  other operators
!!
!!    .fmt.    ! convert an intrinsic value to a CHARACTER variable using format
!!    N.to.M   ! equivalent of [(integer :: i,i=N,M)], generates a range of
!!             ! adjacent whole numbers
!!
!!  Related functions
!!
!!    ! logical functions that return integer values
!!    use M_overload, only : oz, zo, lt, le, eq, ne, gt, ge
!!
!!    ! same operation as .fmt. accept directly using the function
!!    use M_overload, only : fmt
!!
!!
!!
!!##DESCRIPTION
!!
!!   Operator and function overloads have a wide range of applications
!!   from allowing existing Fortran routines to be used with almost no
!!   source-code changes to produce versions using arbitrary precision
!!   or cumulative error bounds on floating-point calculations to adding
!!   intuitive syntax for standard Fortran operations.
!!
!!   Herein are a few more basic examples of overloading and user-specified
!!   operators that demonstrate the concepts of these Fortran features ...
!!
!!##OVERLOADS
!!
!!   //       overloads // to concatenate any two intrinsic types into a string
!!
!!   ==,/=    Allow the syntax "L1 == L2"  and "L1 /= L2" where L1 and L2 are
!!            type LOGICAL as an alternative to the standard expressions
!!            "L1 .EQV. L2" and "L1 .NEQV. L2".
!!
!!            It should be pointed out that
!!
!!               L1 == L2   !! should be L1 .eqv. L2
!!
!!            and
!!
!!               L1 /= L2   !! should be L1 .neqv. L2
!!
!!            should NOT work by default; but often do (probably because
!!            the compiler silently converts LOGICAL to INTEGER when a
!!            LOGICAL appears where a numeric value is required). If your
!!            compiler supports this non-standard (but intuitive-looking)
!!            syntax you can use this module to allow the syntax in a
!!            portable manner with a standard method.
!!
!!    int(), real(), dble()  allow strings to be converted to numeric values
!!                           using the standard intrinsic names
!!
!!    sign(value)            When sign(3f) is given a single value sign(value),
!!                           call sign(1,value).
!!
!!
!!    str=merge('little','big',a.eq.10)  allows for strings of different lengths
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!   program demo_M_overload
!!
!!    use, intrinsic :: iso_fortran_env, only : &
!!            & integer_kinds, int8, int16, int32, int64
!!    use, intrinsic :: iso_fortran_env, only : &
!!            & real32, real64, real128
!!
!!    ! allow strings to be converted to integers
!!    use M_overload, only : int
!!    ! allow strings to be converted to floating point
!!    use M_overload, only : real,dble
!!    ! use == like .eqv.
!!    use M_overload, only : operator(==)
!!    ! use /= like .neqv.
!!    use M_overload, only : operator(/=)
!!    use M_overload, only : operator(//)
!!    ! take a single argument
!!    use M_overload, only : sign
!!    ! allow strings of different length on merge
!!    use M_overload, only : merge
!!    ! convert logical expressions to integer
!!    use M_overload, only : oz, zo, lt, le, eq, ne, gt, ge
!!    implicit none
!!    character(len=:),allocatable :: cmd
!!    character(len=*), parameter :: gen='(*("[",g0,"]":,","))'
!!
!!      ! merge() with different string lengths expanded to longest
!!      write(*,gen)merge('a','bbbbb',1.eq.1)
!!      write(*,gen)merge('a','bbbbb',1.eq.2)
!!      write(*,gen)merge(['a','b'],['bbbbb','ccccc'],1.eq.2)
!!
!!      ! int() can take strings representing a number as input'
!!      if(int('1234') .eq.1234) write(*,*)'int("STRING") works '
!!      ! as can real() and dble()
!!      if(abs(real('1234.56789') - 1234.56789).lt.2*epsilon(0.0)) &
!!       & write(*,*)'real("STRING") works '
!!      if(abs(dble('1234.5678901234567')- 1234.5678901234567d0).lt.epsilon(0.0d0)) &
!!       & write(*,*)'dble("STRING") works '
!!
!!      ! and logical values can be treated numerically
!!      write(*,*) merge('int works for .FALSE.','int fails for .FALSE.',int(.FALSE.).ne.0)
!!      write(*,*) merge('int works for .TRUE.','int fails for .TRUE.',int(.TRUE.).eq.0)
!!      write(*,*) sum(int([.true.,.false.,.true.]))
!!
!!      ! and == and /= work for logical expressions
!!      if (.true. == .true. ) &
!!      & write(*,*)'== works like .eqv. for LOGICAL values'
!!      if (.true. /= .false. ) &
!!      & write(*,*)'/= works like .neqv. for LOGICAL values'
!!
!!      ! // will allow any intrinsic type and convert it to a string
!!      write(*,*)' The value is '//10//' which is less than '//20.2
!!      block
!!      character(len=:),allocatable :: myfmt
!!      integer :: i
!!         i=24
!!         ! build a format with a variable numeric value
!!         myfmt='("[",I'//i//',"]")'
!!         write(*,fmt=myfmt)20
!!      endblock
!!
!!      ! logical values as numeric values
!!      write(*,*) sum([int(.false.),int(.false.)])
!!      write(*,*) int([.false.,.true.,.false.])
!!      write(*,*) sum(int([.false.,.true.,.false.]))
!!
!!
!!      ! and sign() assumes the second argument is 1
!!      write(*,*) merge('sign works','sign fails',&
!!       & sign(10_int8).eq.1 &
!!       & .and. sign(-10_int8).eq.-1 )
!!
!!    contains
!!
!!    end program demo_M_overload
!!
!! Results:
!!
!!  > [a    ]
!!  > [bbbbb]
!!  > [bbbbb],[ccccc]
!!  >  int("STRING") works
!!  >  real("STRING") works
!!  >  dble("STRING") works
!!  >  int works for .FALSE.
!!  >  int works for .TRUE.
!!  >                     1
!!  >  == works like .eqv. for LOGICAL values
!!  >  /= works like .neqv. for LOGICAL values
!!  >   The value is 10 which is less than 20.2000008
!!  > [                      20]
!!  >                     2
!!  >                     1                    0                    1
!!  >                     2
!!  >  sign works
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
module m_overload
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
use,intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT,stdin=>INPUT_UNIT,stdout=>OUTPUT_UNIT
implicit none
! ident_1="@(#) M_overload(3fm) overloads of standard operators and intrinsic procedures"
private
public lt, le, eq, ne, ge, gt, oz, zo
public to
public boolean_equal, boolean_notequal      !
public operator(==)
public operator(/=)
public operator(//)
public operator(.fmt.)
public operator(.to.)

interface operator(.to.)
   module procedure to
end interface

interface operator ( .fmt. )
   module procedure ffmt
end interface operator ( .fmt. )

interface operator ( == )
   module procedure boolean_equal
end interface operator ( == )

interface operator ( /= )
   module procedure boolean_notequal
end interface operator ( /= )

interface operator ( // )
   module procedure g_g
end interface operator ( // )

! already made all intrinsic names public,
! but note extending int, real, dble, adjustl, adjustr, get_command, get_command_argument

! extend intrinsics to accept CHARACTER values
!!interface int;     module procedure int_s2v;              end interface
!!interface real;    module procedure real_s2v;             end interface
!!interface dble;    module procedure dble_s2v;             end interface
interface int;     module procedure ints_s2v;             end interface
interface real;    module procedure reals_s2v;            end interface
interface dble;    module procedure dbles_s2v;            end interface
! extend intrinsics to accept CLASS(*) arguments
interface int;     module procedure anyscalar_to_int64;   end interface
interface real;    module procedure anyscalar_to_real;    end interface
interface dble;    module procedure anyscalar_to_double;  end interface
! allow one argument in sign(3f)
interface sign;    module procedure sign_int8;            end interface
interface sign;    module procedure sign_int16;           end interface
interface sign;    module procedure sign_int32;           end interface
interface sign;    module procedure sign_int64;           end interface
interface sign;    module procedure sign_real32;          end interface
interface sign;    module procedure sign_real64;          end interface
interface sign;    module procedure sign_real128;         end interface
! allow for minimum length option on adjustl and adjustr
interface adjustl; module procedure adjustl_atleast;      end interface
interface adjustr; module procedure adjustr_atleast;      end interface

interface merge
   module procedure strmerge
end interface

public :: fmt

! aliases
interface bool
   module procedure oz
end interface
public bool
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    rename_intrinsics(3f) - [M_overload::INTRINSICS] rename intrinsic functions
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!     use M_overload, only : LOCAL_NAME=>INTRINSIC_NAME
!!
!!##DESCRIPTION
!!
!!    In order to be able to rename intrinsics they are all loaded in
!!    the M_overload module.  That allows for them to be renamed via a
!!    USE statement.
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program rename_intrinsics
!!    ! rename intrinsics
!!
!!    ! shorter names
!!    use M_overload, only : cmd=>execute_command_line
!!    use M_overload, only : getenv=> get_environment_variable
!!
!!    ! longer names
!!    use M_overload, only : cosine=>cos, sine=>sin, tangent=>tan
!!
!!    implicit none
!!    character(len=4096) :: home
!!
!!       call cmd('echo Hello')
!!
!!       call getenv('HOME',home)
!!       write(*,*)trim(home)
!!
!!       write(*,*)cosine(1.0),sine(1.0),tangent(1.0)
!!
!!    end program rename_intrinsics
!!
!!   Results:
!!
!!      > Hello
!!      >  /home/urbanjs/venus/V600
!!      >   0.540302277      0.841470957       1.55740774
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
! this allows you to rename intrinsics, overload them,
intrinsic :: abs,                   achar,                     acos,              acosh,             adjustl
public    :: abs,                   achar,                     acos,              acosh,             adjustl
intrinsic :: adjustr,               aimag,                     aint,              all,               allocated
public    :: adjustr,               aimag,                     aint,              all,               allocated
intrinsic :: anint,                 any,                       asin,              asinh,             associated
public    :: anint,                 any,                       asin,              asinh,             associated
intrinsic :: atan,                  atan2,                     atanh,             atomic_add,        atomic_and
public    :: atan,                  atan2,                     atanh,             atomic_add,        atomic_and
intrinsic :: atomic_cas,            atomic_define,             atomic_fetch_add,  atomic_fetch_and,  atomic_fetch_or
public    :: atomic_cas,            atomic_define,             atomic_fetch_add,  atomic_fetch_and,  atomic_fetch_or
intrinsic :: atomic_fetch_xor,      atomic_or,                 atomic_ref,        atomic_xor,        bessel_j0
public    :: atomic_fetch_xor,      atomic_or,                 atomic_ref,        atomic_xor,        bessel_j0
intrinsic :: bessel_j1,             bessel_jn,                 bessel_y0,         bessel_y1,         bessel_yn
public    :: bessel_j1,             bessel_jn,                 bessel_y0,         bessel_y1,         bessel_yn
intrinsic :: bge,                   bgt,                       bit_size,          ble,               blt
public    :: bge,                   bgt,                       bit_size,          ble,               blt
intrinsic :: btest,                 ceiling,                   char,              cmplx,             command_argument_count
public    :: btest,                 ceiling,                   char,              cmplx,             command_argument_count
intrinsic :: conjg,                 cos,                       cosh,              count,             cpu_time
public    :: conjg,                 cos,                       cosh,              count,             cpu_time
intrinsic :: cshift,                date_and_time,             dble,              digits,            dim
public    :: cshift,                date_and_time,             dble,              digits,            dim
intrinsic :: dot_product,           dprod,                     dshiftl,           dshiftr,           eoshift
public    :: dot_product,           dprod,                     dshiftl,           dshiftr,           eoshift
intrinsic :: epsilon,               erf,                       erfc,              erfc_scaled,       event_query
public    :: epsilon,               erf,                       erfc,              erfc_scaled,       event_query
intrinsic :: execute_command_line,  exp,                       exponent,          extends_type_of,   findloc
public    :: execute_command_line,  exp,                       exponent,          extends_type_of,   findloc
intrinsic :: float,                 floor,                     fraction,          gamma,             get_command
public    :: float,                 floor,                     fraction,          gamma,             get_command
intrinsic :: get_command_argument,  get_environment_variable,  huge,              hypot,             iachar
public    :: get_command_argument,  get_environment_variable,  huge,              hypot,             iachar
intrinsic :: iall,                  iand,                      iany,              ibclr,             ibits
public    :: iall,                  iand,                      iany,              ibclr,             ibits
intrinsic :: ibset,                 ichar,                     ieor,              image_index,       index
public    :: ibset,                 ichar,                     ieor,              image_index,       index
intrinsic :: int,                   ior,                       iparity,           is_contiguous,     ishft
public    :: int,                   ior,                       iparity,           is_contiguous,     ishft
intrinsic :: ishftc,                is_iostat_end,             is_iostat_eor,     kind,              lbound
public    :: ishftc,                is_iostat_end,             is_iostat_eor,     kind,              lbound
intrinsic :: leadz,                 len,                       len_trim,          lge,               lgt
public    :: leadz,                 len,                       len_trim,          lge,               lgt
intrinsic :: lle,                   llt,                       log,               log10,             log_gamma
public    :: lle,                   llt,                       log,               log10,             log_gamma
intrinsic :: logical,               maskl,                     maskr,             matmul,            max
public    :: logical,               maskl,                     maskr,             matmul,            max
intrinsic :: maxexponent,           maxloc,                    maxval,                               merge_bits
public    :: maxexponent,           maxloc,                    maxval,                               merge_bits
!intrinsic ::                                                                      merge ! ifort 2023 bug
public    ::                                                                      merge
intrinsic :: min,                   minexponent,               minloc,            minval,            mod
public    :: min,                   minexponent,               minloc,            minval,            mod
intrinsic :: modulo,                move_alloc,                mvbits,            nearest,           new_line
public    :: modulo,                move_alloc,                mvbits,            nearest,           new_line
intrinsic :: nint,                  norm2,                     not,               null,              num_images
public    :: nint,                  norm2,                     not,               null,              num_images
intrinsic :: pack,                  parity,                    popcnt,            poppar,            precision
public    :: pack,                  parity,                    popcnt,            poppar,            precision
intrinsic :: present,               product,                   radix,             random_number,     random_seed
public    :: present,               product,                   radix,             random_number,     random_seed
intrinsic :: range,                 rank,                      real,              repeat,            reshape
public    :: range,                 rank,                      real,              repeat,            reshape
intrinsic :: rrspacing,             same_type_as,              scale,             scan,              selected_char_kind
public    :: rrspacing,             same_type_as,              scale,             scan,              selected_char_kind
intrinsic :: selected_int_kind,     selected_real_kind,        set_exponent,      shape,             shifta
public    :: selected_int_kind,     selected_real_kind,        set_exponent,      shape,             shifta
intrinsic :: shiftl,                shiftr,                                       sin,               sinh
public    :: shiftl,                shiftr,                    sign,              sin,               sinh
!intrinsic ::                                                   sign ! ifort 2023 bug
intrinsic :: size,                  sngl,                      spacing,           spread,            sqrt
public    :: size,                  sngl,                      spacing,           spread,            sqrt
intrinsic :: storage_size,          sum,                       system_clock,      tan,               tanh
public    :: storage_size,          sum,                       system_clock,      tan,               tanh
intrinsic :: this_image,            tiny,                      trailz,            transfer,          transpose
public    :: this_image,            tiny,                      trailz,            transfer,          transpose
intrinsic :: trim,                  ubound,                    unpack,            verify
public    :: trim,                  ubound,                    unpack,            verify
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
function g_g(value1,value2) result (string)

! ident_2="@(#) M_overload g_g(3f) convert two single intrinsic values to a string"

class(*),intent(in)          :: value1, value2
character(len=:),allocatable :: string1
character(len=:),allocatable :: string2
character(len=:),allocatable :: string
   ! use this instead of str() so character variables are not trimmed and/or spaces are not added
   !ifort_bug!string = fmt(value1) // fmt(value2)
   string1 = fmt(value1)
   string2 = fmt(value2)
   !x! some compilers use the default operator some call recursively
   !x! uses // in module that redefines //. gfortran built it, ifort does not
   !x!function g_g(value1,value2) result (string)
   !x!string=string1//string2
   allocate(character(len=len(string1)+len(string2)) :: string)
   string(1:len(string1))=string1
   string(len(string1)+1:)=string2
end function g_g
!>
!!##NAME
!!    merge(3f) - [M_overload:INTRINSIC] allow MERGE(3f) intrinsic to take
!!    strings of different length
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   result = merge(tsource, fsource, mask)
!!
!!##DESCRIPTION
!!
!!     A convenience for allowing strings of different length in the
!!     MERGE(3f) intrinsic.
!!
!!     All other behavior should be the same as the MERGE(3f) intrinsic.
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program demo_merge
!!    ! allow strings of different length on merge
!!    use M_overload, only : merge
!!    implicit none
!!    character(len=*), parameter :: gen='(*("[",g0,"]":,","))'
!!
!!       write(*,gen)merge('a','bbbbb',1.eq.1)
!!       write(*,gen)merge('a','bbbbb',1.eq.2)
!!       write(*,gen)merge(['a','b'],['bbbbb','ccccc'],1.eq.2)
!!
!!    end program demo_merge
!!
!!   Results:
!!
!!        > [a    ]
!!        > [bbbbb]
!!        > [bbbbb],[ccccc]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
elemental function strmerge(str1,str2,expr) result(strout)
!$@(#) M_overload::strmerge(3f): pads first and second arguments to MERGE(3f) to same length
character(len=*),intent(in)              :: str1
character(len=*),intent(in)              :: str2
logical,intent(in)                       :: expr
character(len=max(len(str1), len(str2))) :: strout
   if(expr)then
      strout=str1
   else
      strout=str2
   endif
end function strmerge
!-----------------------------------------------------------------------------------------------------------------------------------
function adjustl_atleast(line,length) result(strout)

! ident_3="@(#) M_overload adjustl_atleast(3f) return string padded on right to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=max(length,len(trim(line)))) :: strout
   strout=adjustl(line)
end function adjustl_atleast
!-----------------------------------------------------------------------------------------------------------------------------------
function adjustr_atleast(line,length) result(strout)

! ident_4="@(#) M_overload adjustr_atleast(3f) return string padded on left to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=max(length,len(trim(line)))) :: strout
   strout=line
   strout=adjustr(strout)
end function adjustr_atleast
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    sign(3f) - [M_overload::INTRINSIC] When sign(3f) is given a single
!!               value sign(value), call sign(1,value).
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!    pure elemental class(*) function sign([magnitude],copysign)
!!
!!##DESCRIPTION
!!
!!    This is an overload of the SIGN(3f) intrinsic which assumes the
!!    magnitude is 1 if only one argument is supplied.
!!
!!    SIGN(3f) returns a value with the magnitude of MAGNITUDE but with the
!!    sign of COPYSIGN.  All three must be of the same type, which may be
!!    INTEGER or REAL.
!!
!!##OPTIONS
!!    magnitude  the return value will have the same magnitude as this value
!!               Optional. A 1 is assumed if not present.
!!    copysign   the return value will have the same sign as this value
!!
!!##RETURNS
!!
!!    The result is a value with the magnitude of A and the sign of B
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program demo_sign
!!    use, intrinsic :: iso_fortran_env, only : &
!!    & integer_kinds, int8, int16, int32, int64
!!    use, intrinsic :: iso_fortran_env, only : &
!!    & real32, real64, real128
!!    ! overload can take a single argument
!!    use M_overload, only: sign, lt, le, eq, ne, gt, ge
!!    implicit none
!!    ! basics
!!    write(*,*) sign(1234), sign(-1234), sign(huge(0.0)), sign(-huge(0.0))
!!    ! any type of integer or real
!!    write(*,*) merge('sign works','sign fails', sign(10_int8).eq.1         &
!!    & .and. sign(-10_int8).eq.-1        )
!!    write(*,*) merge('sign works','sign fails', sign(10_int16).eq.1        &
!!    & .and. sign(-10_int16).eq.-1       )
!!    write(*,*) merge('sign works','sign fails', sign(10_int32).eq.1        &
!!    & .and. sign(-10_int32).eq.-1       )
!!    write(*,*) merge('sign works','sign fails', sign(10_int64).eq.1        &
!!    & .and. sign(-10_int64).eq.-1       )
!!    write(*,*) merge('sign works','sign fails', sign(10.0_real32).eq.1.0   &
!!    & .and. sign(-10.0_real32).eq.-1.0  )
!!    write(*,*) merge('sign works','sign fails', sign(10.0_real64).eq.1.0d0 &
!!    & .and. sign(-10.0_real64).eq.-1.0d0 )
!!    write(*,*) merge('sign works','sign fails', sign(10.0_real128).eq.1.0  &
!!    & .and. sign(-10.0_real128).eq.-1.0 )
!!    !
!!    !write (*, *) sign(10 < 20)
!!    !
!!    !if (sum(sign([1 > 2, 3 == 4, 10 < 5, 100 > 50])) > 2) then
!!    !   write (*, *) 'two or more are not true'
!!    !endif
!!    !
!!    end program demo_sign
!!
!!   Results:
!!
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >           0
!!     >  two or more are not true
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain

elemental function sign_real128(value)
real(kind=real128),intent(in) :: value
real(kind=real128)            :: sign_real128
intrinsic :: sign ! make it clear just need to call the intrinsic, not the overloaded function
   sign_real128=sign(1.0_real128,value)
end function sign_real128
elemental function sign_real64(value)
real(kind=real64),intent(in) :: value
real(kind=real64)            :: sign_real64
intrinsic :: sign ! make it clear just need to call the intrinsic, not the overloaded function
   sign_real64=sign(1.0_real64,value)
end function sign_real64
elemental function sign_real32(value)
real(kind=real32),intent(in) :: value
real(kind=real32)            :: sign_real32
intrinsic :: sign ! make it clear just need to call the intrinsic, not the overloaded function
   sign_real32=sign(1.0_real32,value)
end function sign_real32

elemental function sign_int64(value)
integer(kind=int64),intent(in) :: value
integer(kind=int64)            :: sign_int64
intrinsic :: sign ! make it clear just need to call the intrinsic, not the overloaded function
   sign_int64=sign(1_int64,value)
end function sign_int64
elemental function sign_int32(value)
integer(kind=int32),intent(in) :: value
integer(kind=int32)            :: sign_int32
intrinsic :: sign ! make it clear just need to call the intrinsic, not the overloaded function
   sign_int32=sign(1_int32,value)
end function sign_int32
elemental function sign_int16(value)
integer(kind=int16),intent(in) :: value
integer(kind=int16)            :: sign_int16
intrinsic :: sign ! make it clear just need to call the intrinsic, not the overloaded function
   sign_int16=sign(1_int16,value)
end function sign_int16
elemental function sign_int8(value)
integer(kind=int8),intent(in) :: value
integer(kind=int8)            :: sign_int8
intrinsic :: sign ! make it clear just need to call the intrinsic, not the overloaded function
   sign_int8=sign(1_int8,value)
end function sign_int8

!-----------------------------------------------------------------------------------------------------------------------------------
logical function boolean_equal(logical_val1,logical_val2)
logical, intent (in) :: logical_val1, logical_val2

   boolean_equal = logical_val1 .eqv. logical_val2

end function boolean_equal
!-----------------------------------------------------------------------------------------------------------------------------------
logical function boolean_notequal(logical_val1,logical_val2)
logical, intent (in) :: logical_val1, logical_val2

   boolean_notequal = logical_val1 .neqv. logical_val2

end function boolean_notequal
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!! dble(3f) - [M_overload::TYPE] allow dble(3f), real(3f), int(3f) to take character arguments
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!! dble(3f), real(3f), and int(3f) extension:
!!
!!       pure elemental doubleprecision function dble(value)
!!       pure elemental real  function real(value)
!!       pure elemental int function int(value)
!!
!!##DESCRIPTION
!!
!!     All the intrinsics dble(3f), real(3f), and int(3f) to
!!     take a character string as an argument, assuming it
!!     represents a numeric value; or a logical value or
!!     expression.
!!
!!        o .true. is 0
!!        o .false. is 1
!!
!!##OPTIONS
!!    value  string or logical expression or value to convert to a numeric
!!           value
!!
!!           If an error occurs zero is returned.
!!##RETURNS
!!
!!    A numeric value
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program demo_dble
!!
!!    use, intrinsic :: iso_fortran_env, only : &
!!            & integer_kinds, int8, int16, int32, int64
!!    use, intrinsic :: iso_fortran_env, only : &
!!            & real32, real64, real128
!!
!!    use M_overload, only : int, real, dble
!!    use M_overload, only : merge
!!    implicit none
!!    character(len=*), parameter :: gen='(*("[",g0,"]":,","))'
!!       ! basics
!!       ! note returns int64
!!       write(*,gen) int('1234')*2
!!       write(*,gen) real('1234.56789')*2
!!       write(*,gen) dble('1234.5678901234567')*2
!!       ! tests
!!       if(int('1234') .eq. 1234 ) &
!!               & write(*,*)'int("STRING") works '
!!       if(abs(real('1234.56789') - 1234.56789) .lt. 2*epsilon(0.0)) &
!!               & write(*,*)'real("STRING") works '
!!       if(abs(dble('1234.5678901234567') - 1234.5678901234567d0) .lt. &
!!       & epsilon(0.0d0)) &
!!               & write(*,*)'dble("STRING") works '
!!       ! logical arguments work as well
!!       ! so let us settle this once and for all
!!       write(*,*)'.true. is',int(.true.)
!!       write(*,*)'.false. is',int(.false.)
!!       write(*,*)'LOGICAL ARRAY   ', dble([.false., .true., .false., .true.])
!!       write(*,*) merge('int works for .FALSE.','int fails for .FALSE.', &
!!               & int(.FALSE.).ne.0)
!!       write(*,*) merge('int works for .TRUE.','int fails for .TRUE.', &
!!               & int(.TRUE.).eq.0)
!!       ! and also note the argument can be metamorphic
!!       ! call a function with a metamorphic argument so values can be
!!       ! any values that represent a numeric value ...
!!       write(*,*)'METAMORPHIC     ', promote(1,1.0,1.0d0)
!!       write(*,*)'METAMORPHIC     ', promote('3',(2.0,0.0),.true.)
!!       write(*,*)'METAMORPHIC     ', promote('3','3','3')
!!       write(*,*)'METAMORPHIC     ', promote(.true.,.false.,.true.)
!!       write(*,*)'METAMORPHIC     ', promote((3.0,4.0),0.0,0)
!!
!!    contains
!!    function promote(value1,value2,value3)
!!       class(*),intent(in) :: value1
!!       class(*),intent(in) :: value2
!!       class(*),intent(in) :: value3
!!       doubleprecision,allocatable :: promote
!!       promote=sum([dble(value1),dble(value2),dble(value3)])
!!    end function promote
!!
!!    end program demo_dble
!!
!!   Results:
!!
!!     > [2468]
!!     > [2469.13574]
!!     > [2469.1357802469133]
!!     >  int("STRING") works
!!     >  real("STRING") works
!!     >  dble("STRING") works
!!     >  .true. is                    0
!!     >  .false. is                    1
!!     >  LOGICAL ARRAY      1.0000 0.0000  1.0000  0.0000
!!     >  int works for .FALSE.
!!     >  int works for .TRUE.
!!     >  METAMORPHIC        3.0000000000000000
!!     >  METAMORPHIC        3.0000000000000000
!!     >  METAMORPHIC        9.0000000000000000
!!     >  METAMORPHIC        1.0000000000000000
!!     >  METAMORPHIC        0.0000000000000000
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
! calls to s2v(3f) for extending intrinsics int(3f), real(3f), dble(3f)
!-----------------------------------------------------------------------------------------------------------------------------------
doubleprecision function dble_s2v(chars)
character(len=*),intent(in) :: chars
   dble_s2v=s2v(chars)
end function dble_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
real function real_s2v(chars)
character(len=*),intent(in) :: chars
intrinsic :: real ! make it clear just need to call the intrinsic, not the overloaded function
   real_s2v=real(s2v(chars))
end function real_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
intrinsic :: int ! make it clear just need to call the intrinsic, not the overloaded function
   int_s2v=int(s2v(chars))
end function int_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
intrinsic :: size ! make it clear just need to call the intrinsic, not an overloaded function
   isize=size(chars)
   allocate(ints_s2v(isize))
   do i=1,isize
      ints_s2v(i)=int(s2v(chars(i)))
   enddo
end function ints_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
function reals_s2v(chars)
real,allocatable            :: reals_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
intrinsic :: size ! make it clear just need to call the intrinsic, not an overloaded function
   isize=size(chars)
   allocate(reals_s2v(isize))
   do i=1,isize
      reals_s2v(i)=real(s2v(chars(i)))
   enddo
end function reals_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
function dbles_s2v(chars)
doubleprecision,allocatable :: dbles_s2v(:)
character(len=*),intent(in) :: chars(:)
integer                     :: i,isize
intrinsic :: size ! make it clear just need to call the intrinsic, not an overloaded function
   isize=size(chars)
   allocate(dbles_s2v(isize))
   do i=1,isize
      dbles_s2v(i)=s2v(chars(i))
   enddo
end function dbles_s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    fmt(3f) - [M_overload] convert any intrinsic to a string using specified format
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    function fmt(value,format) result(string)
!!
!!     class(*),intent(in),optional :: value
!!     character(len=*),intent(in),optional :: format
!!     character(len=:),allocatable :: string
!!##DESCRIPTION
!!    FMT(3f) converts any standard intrinsic value to a string using the specified
!!    format.
!!##OPTIONS
!!    value    value to print the value of. May be of type INTEGER, LOGICAL,
!!             REAL, DOUBLEPRECISION, COMPLEX, or CHARACTER.
!!    format   format to use to print value. It is up to the user to use an
!!             appropriate format. The format does not require being
!!             surrounded by parenthesis. If not present a default is selected
!!             similar to what would be produced with free format, with
!!             trailing zeros removed.
!!##RETURNS
!!    string   A string value
!!##EXAMPLES
!!
!!   Sample program:
!!
!!       program demo_fmt
!!       use :: M_overload, only : f=>fmt
!!       use :: M_overload, only : operator(.fmt.)
!!       use :: M_overload, only : operator( // )
!!       implicit none
!!       character(len=:), allocatable :: output
!!       character(len=:), allocatable :: string
!!       ! some formats
!!       character(len=*), parameter   :: bracket='"[",g0.5,"]"'
!!       character(len=*), parameter   :: five='g0.5'
!!       character(len=*), parameter   :: g0='g0'
!!       ! for statements
!!       character(len=*), parameter   :: gen='(*(g0:,1x))'
!!       character(len=*), parameter   :: asis='(*(g0:))'
!!       character(len=*), parameter   :: comma='(*(g0:,","))'
!!       real :: x
!!       integer :: i
!!       real,allocatable :: arr(:)
!!
!!         ! print will do, just want to control format of a number
!!          print gen,'result is',10,'and',f(10.0/3.0,'f0.3')
!!         ! complex formats can be in nicely named strings, of course
!!          write(*,*)'result is ', f(10.0/3.0,bracket)
!!         ! you can build a string without using an internal write
!!          output=f(.true.,"'The final answer is [',g0,']'")
!!          write(*,*)'the string is now:',output
!!          x=1234.5680088
!!          i=543
!!          ! operator style
!!          string=1234.4567 .fmt.'g0.0'
!!          write(*,*)string
!!          ! function style
!!          string=f(1234.4567, 'g0.0')
!!          write(*,*)string
!!          ! concatenation style
!!          string=10//' is an integer and '//(11.00,22.00)//' is a complex'
!!          write(*,*)string
!!          ! list-directed I/O leaves column 1 blank and often prints more
!!          ! digits than warranted for the precision of a value.
!!          !
!!          ! combined with a g0 format line "asis" you get something very
!!          ! similar to list-directed I/O accept no unexpected line breaks
!!          ! and starts in column 1 and lets you tweek float values and
!!          ! not have to remember a space goes between values unless they
!!          ! are adjacent strings, and you may or may not get while space
!!          ! after numeric values to make them all the same length for a
!!          ! particular type for nice tables or compact for use in composed text
!!          !
!!          ! this is nearly as simple as list-directed but more predictable:
!!          print asis,'The value is ',f(x,'f0.3'),' or there-abouts'
!!
!!          ! and combine multiple formats for use in a single line without
!!          ! using non-advancing I/O
!!          write(*,comma)x,f(x),f(x,g0),f(x,five),f(x,bracket)
!!
!!
!!          ! A common extension in Fortran is a VFE
!!          ! VFE:
!!          ! A Variable FORMAT Expression (VFE) is a format statement which
!!          ! includes angle brackets enclosing a Fortran expression where the
!!          ! value of the variable enclosed in the <> brackets is substituted
!!          ! as a string into the format. So if N=3 this format
!!          ! "FORMAT(I<N>)" would become "FORMAT(I3)". GNU Fortran does not
!!          ! support this legacy extension. The effect of variable format
!!          ! expressions can be reproduced by using the more powerful
!!          ! (and standard) combination of internal output and string formats.
!!          ! but it is easier using the overloaded // operator:
!!          VFE: block
!!             integer,allocatable :: vector(:)
!!             integer :: neg, zero, pos
!!             integer :: left
!!                vector=[-1,-22,-300,0,0,0,0,8,9,10,11,12]
!!                neg=3
!!                zero=4
!!                pos=5
!!                write(*, "(1x,"//&
!!                  &neg//"('N',i0:,1x),"//&
!!                  &zero//"('@',i0:,1x),"//&
!!                  &pos//"('P',i0:,1x))") vector
!!          endblock VFE
!!          !
!!          !not yet! write(*,asis)'['//f(arr,comma)//']'
!!
!!       end program demo_fmt
!!
!!   Results:
!!
!!       > result is 10 and 3.333
!!       >  result is [3.3333]
!!       >  the string is now:The final answer is [T]
!!       >  1234.45667
!!       >  1234.45667
!!       >  10 is an integer and (11,22) is a complex
!!       > The value is 1234.568 or there-abouts
!!       > 1234.56799,1234.56799,1234.56799,1234.6,[1234.6]
!!       >  N-1 N-22 N-300 @0 @0 @0 @0 P8 P9 P10 P11 P12
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
recursive function fmt(generic,format) result (line)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

! ident_5="@(#) M_overload fmt(3f) convert any intrinsic to a string using specified format"

class(*),intent(in)          :: generic
character(len=*),intent(in),optional  :: format
character(len=:),allocatable :: line
character(len=:),allocatable :: fmt_local
character(len=:),allocatable :: re,im
integer                      :: iostat
character(len=255)           :: iomsg
character(len=1),parameter   :: null=char(0)
integer                      :: ilen
logical                      :: trimit
   allocate(character(len=256) :: line) ! cannot currently write into unallocated allocatable variable
   iostat=0
   if(present(format))then
      fmt_local=adjustl(trim(format))
      trimit=.false.
   else
      fmt_local=''
      trimit=.true.
   endif
   if(fmt_local.ne.'')then
      if(fmt_local(1:1) == '(')then
         fmt_local=fmt_local(:len_trim(fmt_local)-1)//',a)'
      else
         fmt_local='('//fmt_local//',a)'
      endif
   endif
   ! add ",a" and print null and use position of null to find length of output
   ! add cannot use SIZE= or POS= or ADVANCE='NO' on WRITE() on INTERNAL READ,
   ! and do not want to trim as trailing spaces can be significant
   select type(generic)
      type is (integer(kind=int8))
         if(fmt_local == '') fmt_local='(i0,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (integer(kind=int16))
         if(fmt_local == '') fmt_local='(i0,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (integer(kind=int32))
         if(fmt_local == '') fmt_local='(i0,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (integer(kind=int64))
         if(fmt_local == '') fmt_local='(i0,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (real(kind=real32))
         if(fmt_local == '') fmt_local='(1pg0,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (real(kind=real64))
         if(fmt_local == '') fmt_local='(1pg0,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (real(kind=real128))
         if(fmt_local == '') fmt_local='(1pg0,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (logical)
         if(fmt_local == '') fmt_local='(l1,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (character(len=*))
         if(fmt_local == '') fmt_local='(a,a)'
         write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
      type is (complex)
         if(trimit)then
            re=fmt(generic%re)
            im=fmt(generic%im)
            call trimzeros_(re)
            call trimzeros_(im)
            fmt_local='("(",g0,",",g0,")",a)'
            write(line,fmt_local,iostat=iostat,iomsg=iomsg) trim(re),trim(im),null
            trimit=.false.
         else
            if(fmt_local == '') fmt_local='("(",1pg0,",",1pg0,")",a)'
            write(line,fmt_local,iostat=iostat,iomsg=iomsg) generic,null
         endif
   end select
   if(iostat /= 0)then
      line='<ERROR>'//trim(iomsg)
   else
      ilen=index(line,null,back=.true.)
      if(ilen == 0)ilen=len(line)
      line=line(:ilen-1)
   endif

   if(index(line,'.') /= 0 .and. trimit) call trimzeros_(line)

end function fmt
!===================================================================================================================================
function ffmt(generic,format) result (line)
! Second argument of operator interface at (1) cannot be optional so call fmt

! ident_6="@(#) M_overload ffmt(3f) convert any intrinsic to a string using required specified format"

class(*),intent(in)          :: generic
character(len=*),intent(in)  :: format
character(len=:),allocatable :: line
   line=fmt(generic,format)
end function ffmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    trimzeros_(3fp) - [M_overload:TYPE] Delete trailing zeros from
!!    numeric decimal string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    subroutine trimzeros_(str)
!!
!!     character(len=*)  :: str
!!
!!##DESCRIPTION
!!    TRIMZEROS_(3f) deletes trailing zeros from a string representing a
!!    number. If the resulting string would end in a decimal point, one
!!    trailing zero is added.
!!
!!##OPTIONS
!!    str   input string will be assumed to be a numeric value and have
!!          trailing zeros removed
!!##EXAMPLES
!!
!!    Sample program:
!!
!!       program demo_trimzeros_
!!       use M_overload, only : trimzeros_
!!       character(len=:),allocatable :: string
!!          string='123.450000000000';  call  printme()
!!          string='12345';             call  printme()
!!          string='12345.';            call  printme()
!!          string='12345.00e3';        call  printme()
!!       contains
!!       subroutine printme()
!!          call trimzeros_(string)
!!          write(*,*)string
!!       end subroutine printme
!!       end program demo_trimzeros_
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
subroutine trimzeros_(string)

! ident_7="@(#) M_overload trimzeros_(3fp) Delete trailing zeros from numeric decimal string"

! if zero needs added at end assumes input string has room
character(len=*)               :: string
character(len=len(string) + 2) :: str
character(len=len(string))     :: exp        ! the exponent string if present
integer                        :: ipos       ! where exponent letter appears if present
integer                        :: i, ii
   str = string                              ! working copy of string
   ipos = scan(str, 'eEdD')                  ! find end of real number if string uses exponent notation
   if (ipos > 0) then                        ! letter was found
      exp = str(ipos:)                       ! keep exponent string so it can be added back as a suffix
      str = str(1:ipos - 1)                  ! just the real part, exponent removed will not have trailing zeros removed
   endif
   if (index(str, '.') == 0) then            ! if no decimal character in original string add one to end of string
      ii = len_trim(str)
      str(ii + 1:ii + 1) = '.'               ! add decimal to end of string
   endif
   do i = len_trim(str), 1, -1               ! scanning from end find a non-zero character
      select case (str(i:i))
      case ('0')                             ! found a trailing zero so keep trimming
         cycle
      case ('.')                             ! found a decimal character at end of remaining string
         if (i <= 1) then
            str = '0'
         else
            str = str(1:i - 1)
         endif
         exit
      case default
         str = str(1:i)                      ! found a non-zero character so trim string and exit
         exit
      end select
   end do
   if (ipos > 0) then                        ! if originally had an exponent place it back on
      string = trim(str)//trim(exp)
   else
      string = str
   endif
end subroutine trimzeros_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sub_s2v(chars,valu,ierr,onerr)

!$@(#) M_overload::sub_s2v(3fp): subroutine returns double value from string

!     1989,2016 John S. Urban.
!
!  o works with any g-format input, including integer, real, and exponential.
!  o if an error occurs in the read, iostat is returned in ierr and value is set to zero. If no error occurs, ierr=0.
!  o onerr -- value to use if an error occurs

character(len=*),intent(in)  :: chars                     ! input string
character(len=:),allocatable :: local_chars
doubleprecision,intent(out)  :: valu                      ! value read from input string
integer,intent(out)          :: ierr                      ! error flag (0 == no error)
class(*),optional,intent(in) :: onerr

character(len=*),parameter   :: fmt="('(bn,g',i5,'.0)')"  ! format used to build frmt
character(len=15)            :: frmt                      ! holds format built to read input string
character(len=256)           :: msg                       ! hold message from I/O errors
character(len=3),save        :: nan_string='NaN'

   ierr=0                                                 ! initialize error flag to zero
   local_chars=chars
   msg=''
   if(len(local_chars).eq.0)local_chars=' '
   write(frmt,fmt)len(local_chars)                        ! build format of form '(BN,Gn.0)'
   read(local_chars,fmt=frmt,iostat=ierr,iomsg=msg)valu   ! try to read value from string
   if(ierr.ne.0)then                                      ! if an error occurred ierr will be non-zero.
      if(present(onerr))then
         select type(onerr)
         type is (integer)
            valu=onerr
         type is (real)
            valu=onerr
         type is (doubleprecision)
            valu=onerr
         end select
      else                                                      ! set return value to NaN
         read(nan_string,'(g3.3)')valu
      endif
      write(*,*)'*s2v* - cannot produce number from string ['//trim(chars)//']'
      if(msg.ne.'')then
         write(*,*)'*s2v* - ['//trim(msg)//']'
      endif
   endif
end subroutine sub_s2v
!===================================================================================================================================
function s2v(string) result (value)
character(len=*),intent(in) :: string
doubleprecision             :: value
integer                     :: ierr
   call sub_s2v(string,value,ierr)
end function s2v
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function atleast(line,length,pattern) result(strout)

!$@(#) M_overload::atleast(3f): return string padded to at least specified length

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=*),intent(in),optional       :: pattern
character(len=max(length,len(trim(line)))) :: strout
if(present(pattern))then
   strout=line//repeat(pattern,len(strout)/len(pattern)+1)
else
   strout=line
endif
end function atleast
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
pure elemental function anyscalar_to_double(valuein) result(d_out)
implicit none
intrinsic dble

! ident_8="@(#) M_overload anyscalar_to_double(3f) convert integer or real parameter of any kind to doubleprecision"

class(*),intent(in)       :: valuein
doubleprecision           :: d_out
!x!doubleprecision,parameter :: big=huge(0.0d0)
   select type(valuein)
   type is (integer(kind=int8));   d_out=dble(valuein)
   type is (integer(kind=int16));  d_out=dble(valuein)
   type is (integer(kind=int32));  d_out=dble(valuein)
   type is (integer(kind=int64));  d_out=dble(valuein)
   type is (real(kind=real32));    d_out=dble(valuein)
   type is (real(kind=real64));    d_out=dble(valuein)
   type is (real(kind=real128))
      !x!if(valuein.gt.big)then
      !x!   write(stderr,*)'*anyscalar_to_double* value too large ',valuein
      !x!endif
      d_out=dble(valuein)
   type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));      read(valuein,*) d_out
   class default
     d_out=0.0d0
     !x!stop '*M_overload::anyscalar_to_double: unknown type'
   end select
end function anyscalar_to_double
!===================================================================================================================================
impure elemental function anyscalar_to_int64(valuein) result(ii38)
implicit none
intrinsic int

! ident_9="@(#) M_overload anyscalar_to_int64(3f) convert integer parameter of any kind to 64-bit integer"

class(*),intent(in)    :: valuein
   integer(kind=int64) :: ii38
   integer             :: ios
   character(len=256)  :: message
   select type(valuein)
   type is (integer(kind=int8));   ii38=int(valuein,kind=int64)
   type is (integer(kind=int16));  ii38=int(valuein,kind=int64)
   type is (integer(kind=int32));  ii38=valuein
   type is (integer(kind=int64));  ii38=valuein
   type is (real(kind=real32));    ii38=int(valuein,kind=int64)
   type is (real(kind=real64));    ii38=int(valuein,kind=int64)
   Type is (real(kind=real128));   ii38=int(valuein,kind=int64)
   type is (logical);              ii38=merge(0_int64,1_int64,valuein)
   type is (character(len=*))   ;
      read(valuein,*,iostat=ios,iomsg=message)ii38
      if(ios.ne.0)then
         write(stderr,*)'*anyscalar_to_int64* ERROR: '//trim(message)
         stop 2
      endif
   class default
      write(stderr,*)'*anyscalar_to_int64* ERROR: unknown integer type'
      stop 3
   end select
end function anyscalar_to_int64
!===================================================================================================================================
pure elemental function anyscalar_to_real(valuein) result(r_out)
implicit none
intrinsic real

! ident_10="@(#) M_overload anyscalar_to_real(3f) convert integer or real parameter of any kind to real"

class(*),intent(in) :: valuein
real                :: r_out
!x!real,parameter      :: big=huge(0.0)
   select type(valuein)
   type is (integer(kind=int8));   r_out=real(valuein)
   type is (integer(kind=int16));  r_out=real(valuein)
   type is (integer(kind=int32));  r_out=real(valuein)
   type is (integer(kind=int64));  r_out=real(valuein)
   type is (real(kind=real32));    r_out=real(valuein)
   type is (real(kind=real64))
      !x!if(valuein.gt.big)then
      !x!   write(stderr,*)'*anyscalar_to_real* value too large ',valuein
      !x!endif
      r_out=real(valuein)
   type is (real(kind=real128))
      !x!if(valuein.gt.big)then
      !x!   write(stderr,*)'*anyscalar_to_real* value too large ',valuein
      !x!endif
      r_out=real(valuein)
   type is (logical);              r_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));     read(valuein,*) r_out
   end select
end function anyscalar_to_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    to(3f) - [M_overload::LOGICAL] return array of adjacent integers over specified range, inclusive
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!    pure elemental integer function to(i,j)
!!
!!     integer,intent(in) :: i
!!     integer,intent(in) :: j
!!     integer,allocatable :: to
!!
!!##DESCRIPTION
!!
!!     A convenience for expressing [(integer :: i=N,M)]
!!
!!##OPTIONS
!!    I  starting value
!!    J  ending value
!!
!!##RETURNS
!!
!!    An array of adjacent whole numbers from I to J
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program demo_to
!!    use M_overload, only: to, operator(.to.)
!!    implicit none
!!    character(len=*),parameter :: gen='(*(g0,1x))'
!!       print gen, [11.to.16]
!!       print gen, 2.5 * [1.to.4]
!!       print gen, 2.5 * to(1,4)+10
!!    end program demo_to
!!
!!   Results:
!!
!!       > 11 12 13 14 15 16
!!       > 2.50000000 5.00000000 7.50000000 10.0000000
!!       > 12.5000000 15.0000000 17.5000000 20.0000000
!!
!!##AUTHOR
!!    John S. Urban, inspired by @beliavsky
!!##LICENSE
!!    Public Domain
pure function to(i, j) result(vec)
integer, intent(in) :: i, j
integer             :: vec(j-i+1)
integer             :: k
if(i.le.j)then
   vec = [(k, k=i,j)]
else
   vec = [(k, k=i,j,-1)]
endif
end function to
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    oz(3f) - [M_overload::LOGICAL] returns One if expression is TRUE, else returns Zero.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!    pure elemental integer function oz(expr)
!!
!!     logical,intent(in) :: expr
!!
!!##DESCRIPTION
!!
!!    oz(3f) returns an integer 1 given a true logical expression.
!!
!!##OPTIONS
!!    expr  A logical expression
!!
!!##RETURNS
!!
!!    The result is a default INTEGER value of 1 if the expression is TRUE,
!!    and a 0 otherwise.
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program demo_oz
!!    use M_overload, only: oz, zo, lt, le, eq, ne, gt, ge
!!    implicit none
!!       write (*, *) 'is 10 < 20 ?', oz(10 < 20)
!!       write (*, *) 'elemental', oz([2 > 1, 3 == 4, 10 < 5, 100 > 50])
!!       if (sum(oz([2 > 1, 3 == 4, 10 < 5, 100 > 50])) >= 2) then
!!          write (*, *) 'two or more are true'
!!       endif
!!    end program demo_oz
!!
!!  Results:
!!
!!     > is 10 < 20 ? 1
!!     > elemental 1 0 0 1
!!     > two or more are true
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure elemental integer function oz(expr)
! ident_11="@(#) M_overload oz(3f) logical to integer TRUE results in 1 FALSE results in 0"
logical, intent(in) :: expr
   oz = merge(1, 0, expr) ! One and Zero
end function oz
!>
!!##NAME
!!    zo(3f) - [M_overload::LOGICAL] returns Zero if expression is TRUE, else returns One.
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!    pure elemental integer function zo(expr)
!!
!!     logical,intent(in) :: expr
!!
!!##DESCRIPTION
!!
!!    zo(3f) returns an integer 0 given a true logical expression.
!!
!!##OPTIONS
!!    expr  A logical expression
!!
!!##RETURNS
!!
!!    The result is a default INTEGER value of 0 if the expression is TRUE,
!!    and a 1 otherwise.
!!
!!##EXAMPLES
!!
!!  Sample usage:
!!
!!    program demo_zo
!!    use M_overload, only: zo, zo, lt, le, eq, ne, gt, ge
!!    implicit none
!!    write (*, *) zo(10 < 20)
!!    if (sum(zo([1 > 2, 3 == 4, 10 < 5, 100 > 50])) > 2) then
!!       write (*, *) 'two or more are not true'
!!    endif
!!    end program demo_zo
!!
!!  Results:
!!
!!    >           0
!!    >  two or more are not true
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure elemental integer function zo(expr)
! ident_12="@(#) M_overload zo(3f) logical to integer TRUE results in 0 FALSE results in 1"
logical, intent(in) :: expr
   zo = merge(0, 1, expr) ! Zero and One
end function zo

pure elemental integer function ge(ia,ib)
! ident_13="@(#) M_overload ge(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   ge = merge(1, 0, ia .ge. ib )
end function ge

pure elemental integer function le(ia,ib)
! ident_14="@(#) M_overload le(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   le = merge(1, 0, ia .le. ib )
end function le

pure elemental integer function eq(ia,ib)
! ident_15="@(#) M_overload eq(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   eq = merge(1, 0, ia .eq. ib )
end function eq

pure elemental integer function lt(ia,ib)
! ident_16="@(#) M_overload lt(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   lt = merge(1, 0, ia .lt. ib )
end function lt

pure elemental integer function gt(ia,ib)
! ident_17="@(#) M_overload gt(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   gt = merge(1, 0, ia .lt. ib )
end function gt

pure elemental integer function ne(ia,ib)
! ident_18="@(#) M_overload ne(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   ne = merge(1, 0, ia .lt. ib )
end function ne
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_overload
