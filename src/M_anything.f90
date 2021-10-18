










!===================================================================================================================================
! This module and the example function squarei() that uses it shows how you
! can use polymorphism to allow arguments of different types generically by casting
!===================================================================================================================================
!>
!!##NAME
!!    M_anything(3fm) - [M_anything] procedures that use polymorphism to allow arguments of different types generically
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    public anyinteger_to_string
!!    public anyscalar_to_int64
!!    public anyscalar_to_real
!!    public anyscalar_to_real128
!!    public anyscalar_to_double
!!    public anything_to_bytes
!!    public bytes_to_anything
!!    use M_anything, only : empty, assignment(=)
!!##DESCRIPTION
!!    anyinteger_to_string    convert integer parameter of any kind to string
!!    anyscalar_to_int64      convert integer parameter of any kind to 64-bit integer
!!    anyscalar_to_real       convert integer or real parameter of any kind to real
!!    anyscalar_to_real128    convert integer or real parameter of any kind to real128
!!    anyscalar_to_double     convert integer or real parameter of any kind to doubleprecision
!!    anything_to_bytes       convert anything to bytes
!!    empty                   create an empty array
!!##EXAMPLE
!!
!!   Sample program:
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
module M_anything
use, intrinsic :: ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use, intrinsic :: ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
implicit none
private
integer,parameter        :: dp=kind(0.0d0)
public anyinteger_to_string  ! convert integer parameter of any kind to string
public anyscalar_to_int64    ! convert integer parameter of any kind to 64-bit integer
public anyscalar_to_real     ! convert integer or real parameter of any kind to real
public anyscalar_to_real128  ! convert integer or real parameter of any kind to real128
public anyscalar_to_double   ! convert integer or real parameter of any kind to doubleprecision
public anything_to_bytes
public bytes_to_anything
!!public setany
public test_suite_M_anything

interface anything_to_bytes
   module procedure anything_to_bytes_arr
   module procedure anything_to_bytes_scalar
end interface anything_to_bytes
!===================================================================================================================================
!   Because there is no builtin "empty array" object, I've tried to mimic
!   it with some user-defined type (just for fun).  -- spectrum
!
! So, if there is a language support, it might be not too difficult
! to think of a common "empty array" thing (though not sure if it is
! sufficiently useful).
!
public empty, assignment(=)

   type Empty_t
   endtype
   type(Empty_t) empty   !! singleton

   interface assignment(=)
       module procedure  &
       & ints_empty_,    &
       & reals_empty_,   &
       & doubles_empty_, &
       & strings_empty_
   endinterface

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    empty(3f) - [M_anything] set an allocatable array to zero
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    use M_anything, only : empty, assignment(=)
!!##DESCRIPTION
!!    A convenience routine that sets an array to an empty set.
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_empty_
!!    use M_anything, only : empty, assignment(=)
!!    integer, allocatable      :: ints(:)
!!    character(:), allocatable :: strs(:)
!!    real, allocatable      :: reals(:)
!!       ints=empty
!!       write(*,*)size(ints)
!!
!!       write(*,*)'give them some size ...'
!!       reals = [1.0,2.0,3.0]
!!       ints = [1,2,3]
!!       strs = [character(len=10) :: "one","two","three","four"]
!!       write(*,*)size(ints)
!!       write(*,*)size(reals)
!!       write(*,*)size(strs)
!!
!!       ints=empty
!!       reals=empty
!!       strs=empty
!!       write(*,*)'back to empty ...'
!!       write(*,*)size(ints)
!!       write(*,*)size(reals)
!!       write(*,*)size(strs)
!!
!!    end program demo_empty_
!!
!!   Expected output:
!!
!!               0
!!     give them some size ...
!!               3
!!               3
!!               4
!!     back to empty ...
!!               0
!!               0
!!               0
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
   subroutine ints_empty_( x, emp )
       integer, allocatable, intent(inout) :: x(:)
       type(Empty_t), intent(in) :: emp
       if ( allocated( x ) ) deallocate( x )
       allocate( x( 0 ) )
   end subroutine ints_empty_

   subroutine doubles_empty_( x, emp )
       doubleprecision, allocatable, intent(inout) :: x(:)
       type(Empty_t), intent(in) :: emp
       if ( allocated( x ) ) deallocate( x )
       allocate( x( 0 ) )
   end subroutine doubles_empty_

   subroutine reals_empty_( x, emp )
       real, allocatable, intent(inout) :: x(:)
       type(Empty_t), intent(in) :: emp
       if ( allocated( x ) ) deallocate( x )
       allocate( x( 0 ) )
   end subroutine reals_empty_

   subroutine strings_empty_( x, emp )
       character(:), allocatable, intent(inout) :: x(:)
       type(Empty_t), intent(in) :: emp
       if ( allocated( x ) ) deallocate( x )
       allocate( character(0) :: x( 0 ) )
   end subroutine strings_empty_
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    bytes_to_anything(3f) - [M_anything] convert bytes(character)len=1):: array(:)) to standard types
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine bytes_to_anything(chars,anything)
!!
!!    character(len=1),allocatable :: chars(:)
!!    class(*) :: anything
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow input arguments of different
!!    types. It is used to create other procedures that can take many
!!    argument types as input options and convert them to a single type
!!    to simplify storing arbitrary data, to simplify generating data
!!    hashes, ...
!!
!!##OPTIONS
!!    CHARS     The input value is an array of bytes (character(len=1)).
!!
!!##RETURN
!!    ANYTHING  May be of KIND INTEGER(kind=int8), INTEGER(kind=int16),
!!              INTEGER(kind=int32), INTEGER(kind=int64),
!!              REAL(kind=real32, REAL(kind=real64),
!!              REAL(kind=real128), complex, or CHARACTER(len=*)
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!   Expected output
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine bytes_to_anything(chars,anything)
   character(len=1),allocatable :: chars(:)
   class(*) :: anything
   select type(anything)
    type is (character(len=*));     anything=transfer(chars,anything)
    type is (complex);              anything=transfer(chars,anything)
    type is (complex(kind=dp));     anything=transfer(chars,anything)
    type is (integer(kind=int8));   anything=transfer(chars,anything)
    type is (integer(kind=int16));  anything=transfer(chars,anything)
    type is (integer(kind=int32));  anything=transfer(chars,anything)
    type is (integer(kind=int64));  anything=transfer(chars,anything)
    type is (real(kind=real32));    anything=transfer(chars,anything)
    type is (real(kind=real64));    anything=transfer(chars,anything)
    type is (real(kind=real128));   anything=transfer(chars,anything)
    type is (logical);              anything=transfer(chars,anything)
    class default
      stop 'crud. bytes_to_anything(1) does not know about this type'
   end select
end subroutine bytes_to_anything
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    anything_to_bytes(3f) - [M_anything] convert standard types to bytes (character(len=1):: array(:))
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    function anything_to_bytes_arr(anything) result(chars)
!!
!!     class(*),intent(in)  :: anything
!!             or
!!     class(*),intent(in)  :: anything(:)
!!
!!     character(len=1),allocatable :: chars(:)
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow input arguments of different
!!    types. It is used to create other procedures that can take many
!!    argument types as input options and convert them to a single type
!!    to simplify storing arbitrary data, to simplify generating data
!!    hashes, ...
!!
!!##OPTIONS
!!
!!    VALUEIN  input array or scalar to convert to type CHARACTER(LEN=1).
!!             May be of KIND INTEGER(kind=int8), INTEGER(kind=int16),
!!             INTEGER(kind=int32), INTEGER(kind=int64),
!!             REAL(kind=real32, REAL(kind=real64),
!!             REAL(kind=real128), complex, or CHARACTER(len=*)
!!##RETURN
!!
!!    CHARS    The returned value is an array of bytes (character(len=1)).
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!    program demo_anything_to_bytes
!!    use M_anything,      only : anything_to_bytes
!!    !!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!    !!use, intrinsic :: iso_fortran_env, only : real32, real64, real128
!!    implicit none
!!    integer :: i
!!       write(*,'(/,4(1x,z2.2))')anything_to_bytes([(i*i,i=1,10)])
!!       write(*,'(/,4(1x,z2.2))')anything_to_bytes([11.11,22.22,33.33])
!!       write(*,'(/,4(1x,z2.2))')anything_to_bytes('This is a string')
!!    end program demo_anything_to_bytes
!!
!!   Expected output
!!
!!     01 00 00 00
!!     04 00 00 00
!!     09 00 00 00
!!     10 00 00 00
!!     19 00 00 00
!!     24 00 00 00
!!     31 00 00 00
!!     40 00 00 00
!!     51 00 00 00
!!     64 00 00 00
!!
!!     8F C2 31 41
!!     8F C2 B1 41
!!     EC 51 05 42
!!
!!     54 68 69 73
!!     20 69 73 20
!!     61 20 73 74
!!     72 69 6E 67
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
function anything_to_bytes_arr(anything) result(chars)
implicit none

! ident_1="@(#)M_anything::anything_to_bytes_arr(3fp): any vector of intrinsics to bytes (an array of CHARACTER(LEN=1) variables)"

class(*),intent(in)          :: anything(:)
character(len=1),allocatable :: chars(:)
   select type(anything)

    type is (character(len=*));     chars=transfer(anything,chars)
    type is (complex);              chars=transfer(anything,chars)
    type is (complex(kind=dp));     chars=transfer(anything,chars)
    type is (integer(kind=int8));   chars=transfer(anything,chars)
    type is (integer(kind=int16));  chars=transfer(anything,chars)
    type is (integer(kind=int32));  chars=transfer(anything,chars)
    type is (integer(kind=int64));  chars=transfer(anything,chars)
    type is (real(kind=real32));    chars=transfer(anything,chars)
    type is (real(kind=real64));    chars=transfer(anything,chars)
    type is (real(kind=real128));   chars=transfer(anything,chars)
    type is (logical);              chars=transfer(anything,chars)
    class default
      stop 'crud. anything_to_bytes_arr(1) does not know about this type'

   end select
end function anything_to_bytes_arr
!-----------------------------------------------------------------------------------------------------------------------------------
function  anything_to_bytes_scalar(anything) result(chars)
implicit none

! ident_2="@(#)M_anything::anything_to_bytes_scalar(3fp): anything to bytes (an array of CHARACTER(LEN=1) variables)"

class(*),intent(in)          :: anything
character(len=1),allocatable :: chars(:)
   select type(anything)

    type is (character(len=*));     chars=transfer(anything,chars)
    type is (complex);              chars=transfer(anything,chars)
    type is (complex(kind=dp));     chars=transfer(anything,chars)
    type is (integer(kind=int8));   chars=transfer(anything,chars)
    type is (integer(kind=int16));  chars=transfer(anything,chars)
    type is (integer(kind=int32));  chars=transfer(anything,chars)
    type is (integer(kind=int64));  chars=transfer(anything,chars)
    type is (real(kind=real32));    chars=transfer(anything,chars)
    type is (real(kind=real64));    chars=transfer(anything,chars)
    type is (real(kind=real128));   chars=transfer(anything,chars)
    type is (logical);              chars=transfer(anything,chars)
    class default
      stop 'crud. anything_to_bytes_scalar(1) does not know about this type'

   end select
end function  anything_to_bytes_scalar
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!!subroutine setany(anything,default,answer)
!!implicit none
!!
!!$@(#) M_anything::setany(3fp): set absent parameter to default value
!!
!!class(*),intent(in),optional     :: anything
!!class(*),intent(in)              :: default
!!class(*),intent(out),allocatable :: answer
!!if(present(anything))then
!!   answer=anything
!!else
!!   answer=default
!!endif
!!end subroutine setany
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    anyscalar_to_real128(3f) - [M_anything] convert integer or real parameter of any kind to real128
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure elemental function anyscalar_to_real128(valuein) result(d_out)
!!
!!     class(*),intent(in) :: valuein
!!     real(kind=128)      :: d_out
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow input arguments of different
!!    types. It is used to create other procedures that can take many
!!    scalar arguments as input options.
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type REAL128.
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=real32, kind=real64, or kind=real128
!!##RESULTS
!!
!!    D_OUT    The value of VALUIN converted to REAL128 (assuming
!!             it is actually in the range of type REAL128).
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!     program demo_anyscalar_to_real128
!!     use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!     use, intrinsic :: iso_fortran_env, only : real32, real64, real128
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(2_int8)
!!        write(*,*)squarei(2_int16)
!!        write(*,*)squarei(2_int32)
!!        write(*,*)squarei(2_int64)
!!        write(*,*)squarei(2.0_real32)
!!        write(*,*)squarei(2.0_real64)
!!        write(*,*)squarei(2.0_real128)
!!     contains
!!
!!     function squarei(invalue) result (dvalue)
!!     use M_anything, only : anyscalar_to_real128
!!     class(*),intent(in)  :: invalue
!!     real(kind=real128)   :: invalue_local
!!     real(kind=real128)   :: dvalue
!!        invalue_local=anyscalar_to_real128(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program demo_anyscalar_to_real128
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure elemental function anyscalar_to_real128(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_3="@(#)M_anything::anyscalar_to_real128(3f): convert integer or real parameter of any kind to real128"

class(*),intent(in)          :: valuein
real(kind=real128)           :: d_out
character(len=3)             :: readable
   select type(valuein)
   type is (integer(kind=int8));   d_out=real(valuein,kind=real128)
   type is (integer(kind=int16));  d_out=real(valuein,kind=real128)
   type is (integer(kind=int32));  d_out=real(valuein,kind=real128)
   type is (integer(kind=int64));  d_out=real(valuein,kind=real128)
   type is (real(kind=real32));    d_out=real(valuein,kind=real128)
   type is (real(kind=real64));    d_out=real(valuein,kind=real128)
   Type is (real(kind=real128));   d_out=valuein
   type is (logical);              d_out=merge(0.0_real128,1.0_real128,valuein)
   type is (character(len=*));     read(valuein,*) d_out
   class default
    !!d_out=huge(0.0_real128)
    readable='NaN'
    read(readable,*)d_out
    !!stop '*M_anything::anyscalar_to_real128: unknown type'
   end select
end function anyscalar_to_real128
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    anyscalar_to_double(3f) - [M_anything] convert integer or real parameter of any kind to doubleprecision
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure elemental function anyscalar_to_double(valuein) result(d_out)
!!
!!     class(*),intent(in)  :: valuein
!!     doubleprecision      :: d_out
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow input arguments of different
!!    types. It is used to create other procedures that can take many
!!    scalar arguments as input options.
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type DOUBLEPRECISION.
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=real32, kind=real64, or kind=real128
!!##RESULTS
!!
!!    D_OUT    The value of VALUIN converted to doubleprecision (assuming
!!             it is actually in the range of type DOUBLEPRECISION).
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!     program demo_anyscalar_to_double
!!     use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!     use, intrinsic :: iso_fortran_env, only : real32, real64, real128
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(2_int8)
!!        write(*,*)squarei(2_int16)
!!        write(*,*)squarei(2_int32)
!!        write(*,*)squarei(2_int64)
!!        write(*,*)squarei(2.0_real32)
!!        write(*,*)squarei(2.0_real64)
!!        write(*,*)squarei(2.0_real128)
!!     contains
!!
!!     function squarei(invalue) result (dvalue)
!!     use M_anything, only : anyscalar_to_double
!!     class(*),intent(in)  :: invalue
!!     doubleprecision      :: invalue_local
!!     doubleprecision      :: dvalue
!!        invalue_local=anyscalar_to_double(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program demo_anyscalar_to_double
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure elemental function anyscalar_to_double(valuein) result(d_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_4="@(#)M_anything::anyscalar_to_double(3f): convert integer or real parameter of any kind to doubleprecision"

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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    anyscalar_to_real(3f) - [M_anything] convert integer or real parameter of any kind to real
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    pure elemental function anyscalar_to_real(valuein) result(r_out)
!!
!!     class(*),intent(in)  :: valuein
!!     real                 :: r_out
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow input arguments of different types.
!!    It is used to create other procedures that can take
!!    many scalar arguments as input options.
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type REAL.
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=real32, kind=real64, or kind=real128.
!!##RESULTS
!!
!!    R_OUT    The value of VALUIN converted to real (assuming it is actually
!!             in the range of type REAL).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program demo_anyscalar_to_real
!!     use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!     use, intrinsic :: iso_fortran_env, only : real32, real64, real128
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(2_int8)
!!        write(*,*)squarei(2_int16)
!!        write(*,*)squarei(2_int32)
!!        write(*,*)squarei(2_int64)
!!        write(*,*)squarei(2.0_real32)
!!        write(*,*)squarei(2.0_real64)
!!        write(*,*)squarei(2.0_real128)
!!     contains
!!
!!     function squarei(invalue) result (dvalue)
!!     use M_anything, only : anyscalar_to_real
!!     class(*),intent(in)  :: invalue
!!     real                 :: invalue_local
!!     real                 :: dvalue
!!        invalue_local=anyscalar_to_real(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program demo_anyscalar_to_real
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
pure elemental function anyscalar_to_real(valuein) result(r_out)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_5="@(#)M_anything::anyscalar_to_real(3f): convert integer or real parameter of any kind to real"

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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    anyscalar_to_int64(3f) - [M_anything] convert integer any kind to integer(kind=int64)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!
!!    impure elemental function anyscalar_to_int64(intin) result(value)
!!
!!     class(*),intent(in) :: intin
!!     integer(kind=int64) :: value
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow arguments of different INTEGER types
!!    as input. It is typically used to create other procedures that can take
!!    many scalar arguments as input options, equivalent to passing the
!!    parameter VALUE as int(VALUE,0_int64).
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type INTEGER(KIND=int64).
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64.
!!##RESULTS
!!             The value of VALUIN converted to INTEGER(KIND=INT64).
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program demo_anyscalar_to_int64
!!     use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(huge(0_int8)),huge(0_int8) , &
!!        & '16129'
!!        write(*,*)squarei(huge(0_int16)),huge(0_int16) , &
!!        & '1073676289'
!!        write(*,*)squarei(huge(0_int32)),huge(0_int32) , &
!!        & '4611686014132420609'
!!        write(*,*)squarei(huge(0_int64)),huge(0_int64) , &
!!        & '85070591730234615847396907784232501249'
!!     contains
!!     !
!!     function squarei(invalue)
!!     use M_anything, only : anyscalar_to_int64
!!     class(*),intent(in)  :: invalue
!!     doubleprecision      :: invalue_local
!!     doubleprecision      :: squarei
!!        invalue_local=anyscalar_to_int64(invalue)
!!        squarei=invalue_local*invalue_local
!!     end function squarei
!!     !
!!     end program demo_anyscalar_to_int64
!!
!!   Results
!!
!!    16129.000000000000       127 !!    16129
!!    1073676289.0000000       32767 !!    1073676289
!!    4.6116860141324206E+018  2147483647 !!    4611686014132420609
!!    8.5070591730234616E+037  9223372036854775807 !!    85070591730234615847396907784232501249
!!    2.8948022309329049E+076 170141183460469231731687303715884105727 !!    28948022309329048855892746252171976962977213799489202546401021394546514198529
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
impure elemental function anyscalar_to_int64(valuein) result(ii38)
use, intrinsic :: iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none

! ident_6="@(#)M_anything::anyscalar_to_int64(3f): convert integer parameter of any kind to 64-bit integer"

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
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!
!!    anyinteger_to_string(3f) - [M_anything] convert integer of any kind to a string
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    impure function anyinteger_to_string(intin) result(str)
!!
!!     character(len=:),allocatable :: anyinteger_to_string
!!     class(*),intent(in)          :: intin
!!
!!##DESCRIPTION
!!
!!    Converts an integer value to a string representing the value.
!!    This function allows arguments of different INTEGER types as input.
!!
!!##OPTIONS
!!
!!    VALUEIN  INTEGER input argument to be converted to a string.
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64.
!!
!!##RESULTS
!!             The value of VALUIN converted to a CHARACTER string.
!!
!!##EXAMPLE
!!
!!
!!   Sample program
!!
!!    program demo_anyinteger_to_string
!!    use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
!!    use M_anything, only : itoc=>anyinteger_to_string
!!    implicit none
!!       write(*,*)itoc(huge(0_int8)),       '=> 127'
!!       write(*,*)itoc(huge(0_int16)),      '=> 32767'
!!       write(*,*)itoc(huge(0_int32)),      '=> 2147483647'
!!       write(*,*)itoc(huge(0_int64)),      '=> 9223372036854775807',huge(0_int64)
!!       write(*,*)itoc(-(huge(0_int64)-1)), '=> -9223372036854775806'
!!    end program demo_anyinteger_to_string
!!
!!   Results:
!!
!!    127=> 127
!!    32767=> 32767
!!    2147483647=> 2147483647
!!    9223372036854775807=> 9223372036854775807
!!    -9223372036854775806=> -9223372036854775806
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
impure function anyinteger_to_string(int) result(out)
use,intrinsic :: iso_fortran_env, only : int64

! ident_7="@(#)M_anything::anyinteger_to_string(3f): function that converts an integer value to a character string"

class(*),intent(in)          :: int
character(len=:),allocatable :: out
integer,parameter            :: maxlen=32         ! assumed more than enough characters for largest input value
integer                      :: i, k
integer(kind=int64)          :: intval
integer(kind=int64)          :: int_local
integer                      :: str(maxlen)
integer,parameter            :: dig0=  ichar('0')
integer,parameter            :: minus= ichar('-')

   int_local = anyscalar_to_int64(int)           ! convert input to largest integer type
   intval = abs(int_local)
   do i=1,maxlen                                  ! generate digits from smallest significant digit to largest
      str(i) = dig0 + mod(intval,10_int64)
      intval = intval / 10
      if(intval == 0 )exit
   enddo
   if (int_local < 0 ) then                       ! now make sure the sign is correct
      i=i+1
      str(i) = minus
   endif
   allocate(character(len=i) :: out)
   do k=i,1,-1                                    ! have all the digits in reverse order, now flip them and convert to a string
      out(i-k+1:i-k+1)=char(str(k))
   enddo
end function anyinteger_to_string
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine test_suite_M_anything()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
!! setup
   call test_anyscalar_to_int64()
   call test_anyinteger_to_string()
   call test_anyscalar_to_real()
   call test_anyscalar_to_double()
   call test_anything_to_bytes()

   call test_empty()
!!teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyscalar_to_int64()

call unit_check_start('anyscalar_to_int64',msg='')
call unit_check('anyscalar_to_int64',anyscalar_to_int64(huge(0_int8)) .eq.127_int64, huge(0_int8))
call unit_check('anyscalar_to_int64',anyscalar_to_int64(huge(0_int16)).eq.32767_int64, huge(0_int16))
call unit_check('anyscalar_to_int64',anyscalar_to_int64(huge(0_int32)).eq.2147483647_int64, huge(0_int32))
call unit_check('anyscalar_to_int64',anyscalar_to_int64(huge(0_int64)).eq.9223372036854775807_int64, huge(0_int64))
call unit_check_done('anyscalar_to_int64',msg='')
end subroutine test_anyscalar_to_int64
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyinteger_to_string()

call unit_check_start('anyinteger_to_string',msg='')
call unit_check('anyinteger_to_string',anyinteger_to_string(huge(0_int8)) .eq.'127', huge(0_int8))
call unit_check('anyinteger_to_string',anyinteger_to_string(huge(0_int16)).eq.'32767', huge(0_int16))
call unit_check('anyinteger_to_string',anyinteger_to_string(huge(0_int32)).eq.'2147483647', huge(0_int32))
call unit_check('anyinteger_to_string',anyinteger_to_string(huge(0_int64)).eq.'9223372036854775807', huge(0_int64))
call unit_check_done('anyinteger_to_string',msg='')
end subroutine test_anyinteger_to_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyscalar_to_double()

real :: infinity
!!character(len=*),parameter :: line='infinity'
character(len=8)           :: line='infinity'
read(line,*)infinity
call unit_check_start('anyscalar_to_double',msg='')
call unit_check('anyscalar_to_double',anyscalar_to_double(huge(0_int8))     .eq. huge(0_int8),     huge(0_int8))
call unit_check('anyscalar_to_double',anyscalar_to_double(huge(0_int16))    .eq. huge(0_int16),    huge(0_int16))
call unit_check('anyscalar_to_double',anyscalar_to_double(huge(0_int32))    .eq. huge(0_int32),    huge(0_int32))
call unit_check('anyscalar_to_double',anyscalar_to_double(huge(0_int64))    .eq. huge(0_int64),    huge(0_int64))
call unit_check('anyscalar_to_double',anyscalar_to_double(huge(0.0_real32)) .eq. huge(0.0_real32), huge(0.0_real32))
call unit_check('anyscalar_to_double',anyscalar_to_double(huge(0.0_real64)) .eq. huge(0.0_real64), huge(0.0_real64))

call unit_check('anyscalar_to_double',anyscalar_to_double(huge(0.0_real128))  .eq.  infinity,      huge(0.0_real128))
call unit_check('anyscalar_to_double',anyscalar_to_double(1234.0_real128)  .eq.  1234.0_real128,   1234.0_real128)
call unit_check_done('anyscalar_to_double',msg='')
end subroutine test_anyscalar_to_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anyscalar_to_real()

real :: infinity
!!character(len=*),parameter :: line='infinity'
character(len=8)           :: line='infinity'
read(line,*)infinity
call unit_check_start('anyscalar_to_real',msg='')
call unit_check('anyscalar_to_real',anyscalar_to_real(huge(0_int8))     .eq. real(huge(0_int8)),     huge(0_int8))
call unit_check('anyscalar_to_real',anyscalar_to_real(huge(0_int16))    .eq. real(huge(0_int16)),    huge(0_int16))
call unit_check('anyscalar_to_real',anyscalar_to_real(huge(0_int32))    .eq. real(huge(0_int32)),    huge(0_int32))
call unit_check('anyscalar_to_real',anyscalar_to_real(huge(0_int64))    .eq. real(huge(0_int64)),    huge(0_int64))
call unit_check('anyscalar_to_real',anyscalar_to_real(huge(0.0_real32)) .eq. real(huge(0.0_real32)), huge(0.0_real32))

call unit_check('anyscalar_to_real',anyscalar_to_real(huge(0.0_real64)) .eq. infinity,               huge(0.0_real64))
call unit_check('anyscalar_to_real',anyscalar_to_real(huge(0.0_real128)).eq. infinity,               huge(0.0_real128))
call unit_check('anyscalar_to_real',anyscalar_to_real(1234.0_real64)   .eq. 1234.0_real64,   1234.0_real64)
call unit_check('anyscalar_to_real',anyscalar_to_real(1234.0_real128)  .eq. 1234.0_real128,  1234.0_real128)
call unit_check_done('anyscalar_to_real',msg='')
end subroutine test_anyscalar_to_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_anything_to_bytes()
call unit_check_start('anything_to_bytes',msg='')
call unit_check('anything_to_bytes',any(anything_to_bytes(huge(0_int8))      .eq. transfer(huge(0_int8),'A')) )
call unit_check('anything_to_bytes',any(anything_to_bytes(huge(0_int16))     .eq. transfer(huge(0_int16),'A')) )
call unit_check('anything_to_bytes',any(anything_to_bytes(huge(0_int32))     .eq. transfer(huge(0_int32),'A')) )
call unit_check('anything_to_bytes',any(anything_to_bytes(huge(0_int64))     .eq. transfer(huge(0_int64),'A')) )
call unit_check('anything_to_bytes',any(anything_to_bytes(huge(0.0_real32))  .eq. transfer(huge(0.0_real32),'A')) )
call unit_check('anything_to_bytes',any(anything_to_bytes(huge(0.0_real64))  .eq. transfer(huge(0.0_real64),'A')) )
call unit_check('anything_to_bytes',any(anything_to_bytes(huge(0.0_real128)) .eq. transfer(huge(0.0_real128),'A')) )
call unit_check_done('anything_to_bytes',msg='')
end subroutine test_anything_to_bytes
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_empty
!!use M_anything, only : empty, assignment(=)
implicit none
doubleprecision,allocatable  :: d(:)
integer,allocatable          :: i(:)
real,allocatable             :: r(:)
character(len=:),allocatable :: c(:)
integer, allocatable         :: ints(:)
character(:), allocatable    :: strs(:)
integer                      :: answer

   call unit_check_start('empty') !  register an entry for specified name in database with status of zero (0)

   d=empty
   r=empty
   i=empty
   c=empty

   call unit_check('empty', size(d).eq.0, 'checking double')
   call unit_check('empty', size(r).eq.0, 'checking real')
   call unit_check('empty', size(i).eq.0, 'checking integer')
   call unit_check('empty', size(c).eq.0, 'checking character')

   ints = empty
   answer=0
   call check_ints(answer,ints)

   ints = [1,2,3]
   answer=3
   call check_ints(answer,ints)
   call unit_check('empty',all(ints.eq.[1,2,3]),msg='normal allocation')

   ints = empty
   answer=0
   call check_ints(answer,ints)

   strs = empty
   answer=0
   call check_strs(answer,strs)

   strs = [ "apple", "orang", "banan" ]
   answer=3
   call check_strs(answer,strs)
   call unit_check('empty',all(strs.eq.["apple","orang","banan"]),msg='normal allocation')

   strs = empty
   answer=0
   call check_strs(answer,strs)

   call unit_check_done('empty')

end subroutine test_empty
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine check_ints(answer,ints)
integer,intent(in),allocatable :: ints(:)
integer,intent(in) :: answer
   !  if mask test fails, change database status for specified entry to -1 and stop program, else continue
   if(allocated(ints))then
      call unit_check('empty',size(ints).eq.answer,'size is',answer)
   endif
end subroutine check_ints
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine check_strs(answer,strs)
integer,intent(in)           :: answer
character(len=:),allocatable,intent(in)  :: strs(:)
integer k
   if ( allocated(strs) ) then
       if(unit_check_level.gt.0)then
          print *, "strs: val = ", ( strs( k ) // " ", k=1,size(strs) )
          print *, "      len_elem = ", len(strs( 1 ))
       endif
      call unit_check('empty',size(strs).eq.answer,'size is',answer)
   endif
end subroutine check_strs
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_suite_M_anything
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_anything
!===================================================================================================================================
