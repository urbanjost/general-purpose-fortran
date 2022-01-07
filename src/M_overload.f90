










!>
!!##NAME
!!    M_overload(3fm) - [M_overload::INTRO] overloads of standard operators and intrinsic procedures
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
!!  Allow strings of different length in MERGE
!!
!!   use M_overload, only : merge
!!   ! str=merge('one','three',i.eq.10)
!!
!!##OTHER OPERATORS
!!
!!    intrinsic_value .fmt. ''   convert an intrinsic value to a CHARACTER variable
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
!!      if(abs(real('1234.56789') - 1234.56789).lt.2*epsilon(0.0)) &
!!       & write(*,*)'real("STRING") works '
!!      if(abs(dble('1234.5678901234567')- 1234.5678901234567d0).lt.epsilon(0.0d0)) &
!!       & write(*,*)'dble("STRING") works '
!!
!!      write(*,*) merge('int works for .FALSE.','int fails for .FALSE.',int(.FALSE.).ne.0)
!!      write(*,*) merge('int works for .TRUE.','int fails for .TRUE.',int(.TRUE.).eq.0)
!!
!!      if (.true. == .true. ) &
!!      & write(*,*)'== works like .eqv. for LOGICAL values'
!!      if (.true. /= .false. ) &
!!      & write(*,*)'/= works like .neqv. for LOGICAL values'
!!
!!      write(*,*)' The value is '//10//' which is less than '//20.2
!!
!!
!!      write(*,*) merge('sign works','sign fails',&
!!       & sign(10_int8).eq.1 &
!!       & .and. sign(-10_int8).eq.-1 )
!!      write(*,*) merge('sign works','sign fails',&
!!       & sign(10_int16).eq.1 &
!!       & .and. sign(-10_int16).eq.-1 )
!!      write(*,*) merge('sign works','sign fails',&
!!       & sign(10_int32).eq.1 &
!!       & .and. sign(-10_int32).eq.-1 )
!!      write(*,*) merge('sign works','sign fails',&
!!       & sign(10_int64).eq.1 &
!!       & .and. sign(-10_int64).eq.-1 )
!!      write(*,*) merge('sign works','sign fails',&
!!       & sign(10.0_real32).eq.1.0 &
!!       & .and. sign(-10.0_real32).eq.-1.0 )
!!      write(*,*) merge('sign works','sign fails',&
!!       & sign(10.0_real64).eq.1.0 &
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
implicit none
! ident_1="@(#)M_overload(3fm): overloads of standard operators and intrinsic procedures"
private
public boolean_equal, boolean_notequal      !
public operator(==)
public operator(/=)
public operator(//)
public operator(.fmt.)
public int, real, dble                      ! extend intrinsics to accept CHARACTER values
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

! ident_2="@(#)M_overload::g_g(3f): convert two single intrinsic values to a string"

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

! ident_3="@(#)M_strings::adjustl_atleast(3f): return string padded on right to at least specified length"

character(len=*),intent(in)                :: line
integer,intent(in)                         :: length
character(len=max(length,len(trim(line)))) :: strout
   strout=adjustl(line)
end function adjustl_atleast
!-----------------------------------------------------------------------------------------------------------------------------------
function adjustr_atleast(line,length) result(strout)

! ident_4="@(#)M_overload::adjustr_atleast(3f): return string padded on left to at least specified length"

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
function ffmt(generic,format) result (line)
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128

! ident_5="@(#)M_overload::ffmt(3f): convert any intrinsic to a string using specified format"

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

! ident_6="@(#)M_anything::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision"

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
   Type is (real(kind=real128))
      !!if(valuein.gt.big)then
      !!   write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
      !!endif
      d_out=dble(valuein)
   type is (logical);              d_out=merge(0.0d0,1.0d0,valuein)
   type is (character(len=*));      read(valuein,*) d_out
   !type is (real(kind=real128))
   !   if(valuein.gt.big)then
   !      write(error_unit,*)'*anyscalar_to_double* value too large ',valuein
   !   endif
   !   d_out=dble(valuein)
   class default
     d_out=0.0d0
     !!stop '*M_anything::anyscalar_to_double: unknown type'
   end select
end function anyscalar_to_double
!===================================================================================================================================
impure elemental function anyscalar_to_int64(valuein) result(ii38)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
intrinsic int

! ident_7="@(#)M_anything::anyscalar_to_int64(3f): convert integer parameter of any kind to 64-bit integer"

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

! ident_8="@(#)M_anything::anyscalar_to_real(3f): convert integer or real parameter of any kind to real"

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
   !type is (real(kind=real128));  r_out=real(valuein)
   end select
end function anyscalar_to_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_overload
