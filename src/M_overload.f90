










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
!!    ! convert intrinsics to strings and contatenate
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
!!    ! convert an intrinsic value to a CHARACTER variable
!!
!!  Related functions
!!
!!    ! logical functions that return integer values
!!    use M_overload, only : oz, zo, lt, le, eq, ne, gt, ge
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
!!    sign(value)            call sign(1,value)
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
!!      if(int('1234')               .eq.1234) &
!!       & write(*,*)'int("STRING") works '
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
!!
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
!!  Results:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
module m_overload
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
implicit none
! ident_1="@(#) M_overload(3fm) overloads of standard operators and intrinsic procedures"
private
public lt, le, eq, ne, ge, gt, oz, zo
public boolean_equal, boolean_notequal      !
public operator(==)
public operator(/=)
public operator(//)
public operator(.fmt.)
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

interface sign;    module procedure sign_int8;            end interface
interface sign;    module procedure sign_int16;           end interface
interface sign;    module procedure sign_int32;           end interface
interface sign;    module procedure sign_int64;           end interface
interface sign;    module procedure sign_real32;          end interface
interface sign;    module procedure sign_real64;          end interface
interface sign;    module procedure sign_real128;         end interface

interface adjustl; module procedure adjustl_atleast;      end interface

interface adjustr; module procedure adjustr_atleast;      end interface


interface merge
   module procedure strmerge
end interface
!-----------------------------------------------------------------------------------------------------------------------------------
! this allwos you to rename intrinsics, overload them,
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
   !ifort_bug!string = ffmt(value1,'(g0)') // ffmt(value2,'(g0)')
   string1 = ffmt(value1,'(g0)')
   string2 = ffmt(value2,'(g0)')
   allocate(character(len=len(string1)+len(string2)) :: string)
   string(1:len(string1))=string1
   string(len(string1)+1:)=string2
end function g_g
!-----------------------------------------------------------------------------------------------------------------------------------
!x! uses // in module that redefines //. gfortran built it, ifort does not
!x!function g_g(value1,value2) result (string)
!x!
!x!$@(#) M_overload::g_g(3f): convert two single intrinsic values to a string
!x!
!x!class(*),intent(in)          :: value1, value2
!x!character(len=:),allocatable :: string
!x!   ! use this instead of str() so character variables are not trimmed and/or spaces are not added
!x!   string = ffmt(value1,'(g0)') // ffmt(value2,'(g0)')
!x!end function g_g
!-----------------------------------------------------------------------------------------------------------------------------------
elemental function strmerge(str1,str2,expr) result(strout)
!$@(#) M_strings::strmerge(3f): pads first and second arguments to MERGE(3f) to same length
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

! ident_3="@(#) M_strings adjustl_atleast(3f) return string padded on right to at least specified length"

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
function ffmt(generic,format) result (line)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

! ident_5="@(#) M_overload ffmt(3f) convert any intrinsic to a string using specified format"

class(*),intent(in)          :: generic
character(len=*),intent(in)  :: format
character(len=:),allocatable :: line
character(len=:),allocatable :: fmt_local
integer                      :: ios
character(len=255)           :: msg
character(len=1),parameter   :: nill=char(0)
integer                      :: ilen
   fmt_local=format
   ! add ",a" and print null and use position of null to find length of output
   ! add cannot use SIZE= or POS= or ADVANCE='NO' on WRITE() on INTERNAL READ,
   ! and do not want to trim as trailing spaces can be significant
   if(fmt_local.eq.'')then
      select type(generic)
         type is (integer(kind=int8));     fmt_local='(i0,a)'
         type is (integer(kind=int16));    fmt_local='(i0,a)'
         type is (integer(kind=int32));    fmt_local='(i0,a)'
         type is (integer(kind=int64));    fmt_local='(i0,a)'
         type is (real(kind=real32));      fmt_local='(1pg0,a)'
         type is (real(kind=real64));      fmt_local='(1pg0,a)'
         type is (real(kind=real128));     fmt_local='(1pg0,a)'
         type is (logical);                fmt_local='(l1,a)'
         type is (character(len=*));       fmt_local='(a,a)'
         type is (complex);                fmt_local='("(",1pg0,",",1pg0,")",a)'
      end select
   else
      if(format(1:1).eq.'(')then
         fmt_local=format(:len_trim(format)-1)//',a)'
      else
         fmt_local='('//fmt_local//',a)'
      endif
   endif
   allocate(character(len=256) :: line) ! cannot currently write into allocatable variable
   ios=0
   select type(generic)
      type is (integer(kind=int8));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (integer(kind=int16));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (integer(kind=int32));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (integer(kind=int64));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (real(kind=real32));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (real(kind=real64));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (real(kind=real128));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (logical);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (character(len=*));       write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
      type is (complex);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,nill
   end select
   if(ios.ne.0)then
      line='<ERROR>'//trim(msg)
   else
      ilen=index(line,nill,back=.true.)
      if(ilen.eq.0)ilen=len(line)
      line=line(:ilen-1)
   endif
end function ffmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sub_s2v(chars,valu,ierr,onerr)

!$@(#) M_strings::sub_s2v(3fp): subroutine returns double value from string

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
integer                     :: ierr, onerr
   call sub_s2v(string,value,ierr)! , ierr, onerr)
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
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
intrinsic dble

! ident_6="@(#) M_overload anyscalar_to_double(3f) convert integer or real parameter of any kind to doubleprecision"

class(*),intent(in)       :: valuein
doubleprecision           :: d_out
doubleprecision,parameter :: big=huge(0.0d0)
   select type(valuein)
   type is (integer(kind=int8));   d_out=dble(valuein)
   type is (integer(kind=int16));  d_out=dble(valuein)
   type is (integer(kind=int32));  d_out=dble(valuein)
   type is (integer(kind=int64));  d_out=dble(valuein)
   type is (real(kind=real32));    d_out=dble(valuein)
   type is (real(kind=real64));    d_out=dble(valuein)
   type is (real(kind=real128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
      !!endif
      d_out=dble(valuein)
   type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));      read(valuein,*) d_out
   class default
     d_out=0.0d0
     !!stop '*M_overload::anyscalar_to_double: unknown type'
   end select
end function anyscalar_to_double
!===================================================================================================================================
impure elemental function anyscalar_to_int64(valuein) result(ii38)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
intrinsic int

! ident_7="@(#) M_overload anyscalar_to_int64(3f) convert integer parameter of any kind to 64-bit integer"

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
         write(error_unit,*)'*anyscalar_to_int64* ERROR: '//trim(message)
         stop 2
      endif
   class default
      write(error_unit,*)'*anyscalar_to_int64* ERROR: unknown integer type'
      stop 3
   end select
end function anyscalar_to_int64
!===================================================================================================================================
pure elemental function anyscalar_to_real(valuein) result(r_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
intrinsic real

! ident_8="@(#) M_overload anyscalar_to_real(3f) convert integer or real parameter of any kind to real"

class(*),intent(in) :: valuein
real                :: r_out
real,parameter      :: big=huge(0.0)
   select type(valuein)
   type is (integer(kind=int8));   r_out=real(valuein)
   type is (integer(kind=int16));  r_out=real(valuein)
   type is (integer(kind=int32));  r_out=real(valuein)
   type is (integer(kind=int64));  r_out=real(valuein)
   type is (real(kind=real32));    r_out=real(valuein)
   type is (real(kind=real64))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_real* value too large ',valuein
      !!endif
      r_out=real(valuein)
   type is (real(kind=real128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_real* value too large ',valuein
      !!endif
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
!!    M_overload(3fm) - [M_overload::LOGICAL] returns One if expression is TRUE, else returns Zero.
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
!!    Returns an integer given a logical expression.
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
! ident_9="@(#) M_strings oz(3f) logical to integer TRUE results in 1 FALSE results in 0"
logical, intent(in) :: expr
   oz = merge(1, 0, expr) ! One and Zero
end function oz

!>
!!##NAME
!!    M_overload(3fm) - [M_overload::LOGICAL] returns Zero if expression is FALSE, else returns One.
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
!!    Returns an integer given a logical expression.
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
! ident_10="@(#) M_strings zo(3f) logical to integer TRUE results in 0 FALSE results in 1"
logical, intent(in) :: expr
   zo = merge(0, 1, expr) ! Zero and One
end function zo

pure elemental integer function ge(ia,ib)
! ident_11="@(#) M_strings ge(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   ge = merge(1, 0, ia .ge. ib )
end function ge

pure elemental integer function le(ia,ib)
! ident_12="@(#) M_strings le(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   le = merge(1, 0, ia .le. ib )
end function le

pure elemental integer function eq(ia,ib)
! ident_13="@(#) M_strings eq(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   eq = merge(1, 0, ia .eq. ib )
end function eq

pure elemental integer function lt(ia,ib)
! ident_14="@(#) M_strings lt(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   lt = merge(1, 0, ia .lt. ib )
end function lt

pure elemental integer function gt(ia,ib)
! ident_15="@(#) M_strings gt(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   gt = merge(1, 0, ia .lt. ib )
end function gt

pure elemental integer function ne(ia,ib)
! ident_16="@(#) M_strings ne(3f) logical to integer TRUE results in 0 FALSE results in 1"
integer,intent(in)  :: ia, ib
   ne = merge(1, 0, ia .lt. ib )
end function ne
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_overload
