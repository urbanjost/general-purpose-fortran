










!>
!!##NAME
!!    M_overload(3fm) - [M_overload] overloads of standard operators and intrinsic procedures
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!  overloads on LOGICAL values
!!
!!    ! use == like .eqv.; ie. logical==logical
!!    use M_overload, only : operator(==)
!!    ! use /= like .neqv.; ie. logical/=logical
!!    use M_overload, only : operator(/=)
!!
!!  overloads on INTRINSICS to take strings, logicals, and metamorphic numeric intrinsic values
!!
!!   use M_overload, only : int, real, dble
!!   ! int('string')   int(logical)   int(class(*))
!!   ! real('string')  real(logical)  real(class(*))
!!   ! dble('string')  dble(logical)  dble(class(*))
!!
!!  overloads on operators
!!
!!   use M_overload, only : operator(==)
!!   ! INTRINSIC // INTRINSIC // INTRINSIC ...
!!
!!   ! When sign(3f) is given a single value, call sign(1,value); ie.  sign(value)
!!   use M_overload, only : sign
!!
!!
!!
!!  Allow strings of different length in MERGE
!!
!!   use M_overload, only : merge
!!   ! str=merge('one','three',i.eq.10)
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
!!    use M_compare_float_numbers, only : operator(.EqualTo.)
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
!!    implicit none
!!    character(len=:),allocatable :: cmd
!!    character(len=*), parameter :: gen='(*("[",g0,"]":,","))'
!!
!!      write(*,gen)merge('a','bbbbb',1.eq.1)
!!      write(*,gen)merge('a','bbbbb',1.eq.2)
!!      write(*,gen)merge(['a','b'],['bbbbb','ccccc'],1.eq.2)
!!
!!      if(int('1234')               .eq.1234) &
!!       & write(*,*)'int("STRING") works '
!!      if(real('1234.56789')        .EqualTo.1234.56789) &
!!       & write(*,*)'real("STRING") works '
!!      if(dble('1234.5678901234567').EqualTo.1234.5678901234567d0) &
!!       & write(*,*)'dble("STRING") works '
!!
!!       if (.true. == .true. ) &
!!       & write(*,*)'== works like .eqv. for LOGICAL values'
!!      if (.true. /= .false. ) &
!!       & write(*,*)'/= works like .neqv. for LOGICAL values'
!!
!!       write(*,*)' The value is '//10//' which is less than '//20.2
!!
!!
!!      write(*,*) merge('sign works','sign fails',&
!!             & sign(10_int8).eq.1 &
!!       & .and. sign(-10_int8).eq.-1 )
!!      write(*,*) merge('sign works','sign fails',&
!!             & sign(10_int16).eq.1 &
!!       & .and. sign(-10_int16).eq.-1 )
!!      write(*,*) merge('sign works','sign fails',&
!!             & sign(10_int32).eq.1 &
!!       & .and. sign(-10_int32).eq.-1 )
!!      write(*,*) merge('sign works','sign fails',&
!!             & sign(10_int64).eq.1 &
!!       & .and. sign(-10_int64).eq.-1 )
!!      write(*,*) merge('sign works','sign fails',&
!!             & sign(10.0_real32).eq.1.0 &
!!       & .and. sign(-10.0_real32).eq.-1.0 )
!!      write(*,*) merge('sign works','sign fails',&
!!             & sign(10.0_real64).eq.1.0 &
!!       & .and. sign(-10.0_real64).eq.-1.0 )
!!      write(*,*) merge('sign works','sign fails',&
!!       & sign(10.0_real128).eq.1.0&
!!       & .and. sign(-10.0_real128).eq.-1.0 )
!!    contains
!!
!!    end program demo_M_overload
!!
!!  Results:
!!     >  [a    ]
!!     >  [bbbbb]
!!     >  [bbbbb],[ccccc]
!!     >  int("STRING") works
!!     >  real("STRING") works
!!     >  dble("STRING") works
!!     >  == works like .eqv. for LOGICAL values
!!     >  /= works like .neqv. for LOGICAL values
!!     >          444         555
!!     >    444.444000       555.554993
!!     >    444.44400000000002        555.55500000000006
!!     >    555.44399999999996        666.66600000000005        777.77700000000004
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  int("STRING") works
!!     >  real("STRING") works
!!     >  dble("STRING") works
!!     >  == works like .eqv. for LOGICAL values
!!     >  /= works like .neqv. for LOGICAL values
!!     >          444         555
!!     >    444.444000       555.554993
!!     >    444.44400000000002        555.55500000000006
!!     >    555.44399999999996        666.66600000000005        777.77700000000004
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     >  sign works
!!     > 57 xx -x -y hello there xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
!!     > 0 0 [xx]
!!     > 1 0 [-x]
!!     > 2 0 [-y]
!!     > 3 0 [hello there]
!!     > 4 0 [xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx]
!!     > 0 0 [xx        ]
!!     > 1 0 [-x        ]
!!     > 2 0 [-y        ]
!!     > 3 -1 [hello ther]
!!     > 4 -1 [xxxxxxxxxx]
!!     > 0 0 [xx                  ]
!!     > 1 0 [-x                  ]
!!     > 2 0 [-y                  ]
!!     > 3 0 [hello there         ]
!!     > 4 -1 [xxxxxxxxxxxxxxxxxxxx]
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
module m_overload
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
use M_strings,                    only : s2v, atleast
use M_anything,                   only : anyscalar_to_real, anyscalar_to_double, anyscalar_to_int64
implicit none
character(len=*),parameter::ident_1="@(#)M_overload(3fm): overloads of standard operators and intrinsic procedures"
private
public boolean_equal, boolean_notequal      !
public operator(==)
public operator(/=)
public operator(//)
public operator(.fmt.)
public int, real, dble                      ! extend intrinsics to accept CHARACTER values
public test_suite_M_overload
public sign
public adjustl, adjustr
public merge
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


interface merge; module procedure strmerge;      end interface
contains
!-----------------------------------------------------------------------------------------------------------------------------------
function g_g(value1,value2) result (string)

character(len=*),parameter::ident_2="@(#)M_overload::g_g(3f): convert two single intrinsic values to a string"

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

character(len=*),parameter::ident_3="&
&@(#)M_strings::adjustl_atleast(3f): return string padded on right to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=max(length,len(trim(line)))) :: strout
   strout=adjustl(line)
end function adjustl_atleast
!-----------------------------------------------------------------------------------------------------------------------------------
function adjustr_atleast(line,length) result(strout)

character(len=*),parameter::ident_4="&
&@(#)M_overload::adjustr_atleast(3f): return string padded on left to at least specified length"

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
   sign_real128=sign(1.0_real128,value)
end function sign_real128

elemental function sign_real64(value)
real(kind=real64),intent(in) :: value
real(kind=real64)            :: sign_real64
   sign_real64=sign(1.0_real64,value)
end function sign_real64

elemental function sign_real32(value)
real(kind=real32),intent(in) :: value
real(kind=real32)            :: sign_real32
   sign_real32=sign(1.0_real32,value)
end function sign_real32

elemental function sign_int64(value)
integer(kind=int64),intent(in) :: value
integer(kind=int64)            :: sign_int64
   sign_int64=sign(1_int64,value)
end function sign_int64

elemental function sign_int32(value)
integer(kind=int32),intent(in) :: value
integer(kind=int32)            :: sign_int32
   sign_int32=sign(1_int32,value)
end function sign_int32

elemental function sign_int16(value)
integer(kind=int16),intent(in) :: value
integer(kind=int16)            :: sign_int16
   sign_int16=sign(1_int16,value)
end function sign_int16

elemental function sign_int8(value)
integer(kind=int8),intent(in) :: value
integer(kind=int8)            :: sign_int8
   sign_int8=sign(1_int8,value)
end function sign_int8
!-----------------------------------------------------------------------------------------------------------------------------------
logical function boolean_equal(logical_val1,logical_val2)
logical, intent (in) :: logical_val1
logical, intent (in) :: logical_val2

   if (logical_val1 .eqv. logical_val2 )then
     boolean_equal=.true.
   else
     boolean_equal=.false.
   endif

end function boolean_equal
!-----------------------------------------------------------------------------------------------------------------------------------
logical function boolean_notequal(logical_val1,logical_val2)
logical, intent (in) :: logical_val1
logical, intent (in) :: logical_val2

   if (logical_val1 .eqv. logical_val2 )then
     boolean_notequal=.false.
   else
     boolean_notequal=.true.
   endif

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
   real_s2v=real(s2v(chars))
end function real_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
integer function int_s2v(chars)
character(len=*),intent(in) :: chars
   int_s2v=int(s2v(chars))
end function int_s2v
!-----------------------------------------------------------------------------------------------------------------------------------
function ints_s2v(chars)
integer,allocatable         :: ints_s2v(:)
character(len=*),intent(in) :: chars(:)
   integer                  :: i,isize
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
   integer                  :: i,isize
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
   integer                  :: i,isize
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
subroutine test_suite_M_overload()
use M_verify,                 only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify,                 only : unit_check_level
use M_verify,                 only : almost
use M_compare_float_numbers, only : operator(.EqualTo.)
implicit none
character(len=:),allocatable :: cmd

!! setup
   unit_check_level=5
   if(command_argument_count().eq.0)then
      call test_boolean_equal()
      call test_boolean_notequal()
      call test_dble_s2v()
      call test_dbles_s2v()
      call test_int_s2v()
      call test_ints_s2v()
      call test_real_s2v()
      call test_reals_s2v()
      call test_sign()
   endif
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sign()
!use M_overload,              only : operator(==)
   call unit_check_start('sign',' &
         & -description "overload SIGN to take a single argument" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('sign',sign(10_int8).eq.1.and.sign(-10_int8).eq.-1,'sign(+-10_int8)',sign(10_int8),sign(-10_int8))
   call unit_check('sign',sign(10_int16).eq.1.and.sign(-10_int16).eq.-1,'sign(+-10_int16)',sign(10_int16),sign(-10_int16))
   call unit_check('sign',sign(10_int32).eq.1.and.sign(-10_int32).eq.-1,'sign(+-10_int32)',sign(10_int32),sign(-10_int32))
   call unit_check('sign',sign(10_int64).eq.1.and.sign(-10_int64).eq.-1,'sign(+-10_int64)',sign(10_int64),sign(-10_int64))
   call unit_check('sign',sign(10.0_real32).eq.1.and.sign(-10.0_real32).eq.-1,&
   & 'sign(+-10_real32)',sign(10.0_real32),sign(-10.0_real32))
   call unit_check('sign',sign(10.0_real64).eq.1.and.sign(-10.0_real64).eq.-1,&
   & 'sign(+-10_real64)',sign(10.0_real64),sign(-10.0_real64))
   call unit_check('sign',sign(10.0_real128).eq.1.and.sign(-10.0_real128).eq.-1,&
   & 'sign(+-10_real128)',sign(10.0_real128),sign(-10.0_real128))
   call unit_check_done('sign',msg='')
end subroutine test_sign
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_boolean_equal()
!use M_overload,              only : operator(==)
   call unit_check_start('boolean_equal',' &
         & -description "overload == to take LOGICAL arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('boolean_equal',.true. == .true. ,'== works like .eqv. for LOGICAL values')
   call unit_check_done('boolean_equal',msg='')
end subroutine test_boolean_equal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_boolean_notequal()
!use M_overload,              only : operator(/=)
   call unit_check_start('boolean_notequal',' &
         & -description "overload /= to take LOGICAL arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('boolean_notequal', (.true. /= .false. ),'/= works like .neqv. for LOGICAL values')
   call unit_check_done('boolean_notequal',msg='')
end subroutine test_boolean_notequal
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dble_s2v()
!use M_overload,              only : int, real, dble
   call unit_check_start('dble_s2v',' &
         & -description "overload dble() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   if(dble('0.3570726221234567').eq. 0.3570726221234567d0)then
      call unit_check_good('dble_s2v')                                             ! string passed to dble
   elseif(dble('0.3570726221234567') .EqualTo. 0.3570726221234567d0 )then
      call unit_check_good('dble_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('dble_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_dble_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dbles_s2v()
!use M_overload,              only : int, real, dble
   call unit_check_start('dbles_s2v',' &
         & -description "overload dble() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   if(all(dble(['10.0d0','20.0d0']).eq. [10.0d0,20.0d0]))then
      call unit_check_good('dbles_s2v')                                             ! string passed to dble
   elseif(all(dble(['10.0d0','20.0d0']) .EqualTo. [10.0d0,20.0d0]))then
      call unit_check_good('dbles_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('dbles_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_dbles_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int_s2v()
!use M_overload,              only : int, real, dble
   call unit_check_start('int_s2v',' &
         & -description "overload INT() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   call unit_check('int_s2v',int('1234').eq.1234,'string passed to int')
   call unit_check_done('int_s2v',msg='')
end subroutine test_int_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ints_s2v()
integer,allocatable :: ibug(:)
   call unit_check_start('ints_s2v',' &
         & -description "overload INT() to take string arguments" &
         & -section 3  &
         & -library libGPF  &
         & -filename `pwd`/M_overload.FF &
         & -documentation y &
         &  -prep         y &
         &  -ccall        n &
         &  -archive      GPF.a &
         & ')
   !!if(all(int(['100','200']).eq. [100,200]))then
   ibug=int(['100','200'])
   if(all(ibug.eq. [100,200]))then
      call unit_check_good('ints_s2v')                                             ! string passed to int
   else
      call unit_check_bad('ints_s2v')                                              ! returned value not equal to expected value
   endif
end subroutine test_ints_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real_s2v()
   call unit_check_start('real_s2v','&
         & -description "overload REAL() to take string arguments" &
         & -section 3                    &
         & -library libGPF               &
         & -filename `pwd`/M_overload.FF &
         & -documentation y              &
         &  -prep         y              &
         &  -ccall        n              &
         &  -archive      GPF.a          &
         & ')
   if(REAL('0.357072622').eq. 0.357072622)then
      call unit_check_good('real_s2v')
   elseif(almost(real('0.357072622'), 0.357072622,7.0) )then
      call unit_check_good('real_s2v')                                          ! string passed to real but not exactly
   else
      call unit_check_bad('real_s2v')                                           ! returned value not equal to expected value
   endif
end subroutine test_real_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reals_s2v()
real,allocatable :: rbug(:)
   call unit_check_start('reals_s2v','&
         & -description "overload REAL() to take string arguments" &
         & -section 3                    &
         & -library libGPF               &
         & -filename `pwd`/M_overload.FF &
         & -documentation y              &
         &  -prep         y              &
         &  -ccall        n              &
         &  -archive      GPF.a          &
         & ')
   rbug=real(['0.357072622','200.0      '])
   if(all(rbug.eq. [0.357072622,200.0]))then
      call unit_check_good('reals_s2v')                                             ! string passed to int
   elseif(all(real(['0.357072622','200.0      ']) .EqualTo. [0.357072622,200.0]))then
      call unit_check_good('reals_s2v')                                             ! string passed to real but not exactly
   else
      call unit_check_bad('reals_s2v')                                              ! returned value not equal to expected value
   endif

end subroutine test_reals_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!===================================================================================================================================
end subroutine test_suite_M_overload
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function ffmt(generic,format) result (line)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

character(len=*),parameter::ident_5="@(#)M_msg::ffmt(3f): convert any intrinsic to a string using specified format"

class(*),intent(in)          :: generic
character(len=*),intent(in)  :: format
character(len=:),allocatable :: line
character(len=:),allocatable :: fmt_local
integer                      :: ios
character(len=255)           :: msg
character(len=1),parameter   :: null=char(0)
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
      type is (integer(kind=int8));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int16));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int32));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (integer(kind=int64));    write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real32));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real64));      write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (real(kind=real128));     write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (logical);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (character(len=*));       write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
      type is (complex);                write(line,fmt_local,iostat=ios,iomsg=msg) generic,null
   end select
   if(ios.ne.0)then
      line='<ERROR>'//trim(msg)
   else
      ilen=index(line,null,back=.true.)
      if(ilen.eq.0)ilen=len(line)
      line=line(:ilen-1)
   endif
end function ffmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_overload
