!===================================================================================================================================
! This module and the example function squarei() that uses it shows how you
! can use polymorphism to allow arguments of different types generically
!===================================================================================================================================
module M_anyscalar
use ISO_FORTRAN_ENV, only : INT8, INT16, INT32, INT64       !  1           2           4           8
use ISO_FORTRAN_ENV, only : REAL32, REAL64, REAL128         !  4           8          10
private

! on this platform, (select_int_kind(i),i=1,100) returns
! 1:2=1 ,3:4=2 ,5:9=4 ,10:18= 8 ,19:38=16 ,39:=-1

! on this platform, (select_real_kind(i),i=1,100) returns
! 1:6=   4, 7:15 = 8, 16:18= 10, 19:33= 16, 34:  = -1

integer,parameter        :: k(38)=[(selected_int_kind(i),i=1,38)]
integer,public,parameter :: int128=k(38)

integer,parameter        :: r(34)=[(selected_int_kind(i),i=1,34)]
integer,public,parameter :: real256=r(34)

public anyinteger_to_128bit
public anyscalar_to_real

contains
!===================================================================================================================================
!>
!!##NAME
!!    anyscalar_to_real(3f) - [M_anyscalar]convert integer or real parameter of any kind to real
!!
!!##SYNOPSIS
!!
!!    pure function anyscalar_to_real(valuein) result(r_out)
!!
!!     class(*),intent(in)  :: valuein
!!     real                 :: r_out
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow arguments of different types
!!    generically. It is used to create other procedures that can take
!!    many scalar arguments as input options.
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type REAL.
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=int128, kind=real32, kind=real64, kind=real128,
!!             or kind=real256
!!##RESULTS
!!             The returned value is of kind REAL and is the value of VALUIN
!!             converted to real (assuming it is actually in the range of
!!             type REAL).
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!     program scalars
!!     use M_anyscalar,     only : int128, real256
!!     use iso_fortran_env, only : int8, int16, int32, int64
!!     use iso_fortran_env, only : real32, real64, real128
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(2_int8)
!!        write(*,*)squarei(2_int16)
!!        write(*,*)squarei(2_int32)
!!        write(*,*)squarei(2_int64)
!!        write(*,*)squarei(2_int128)
!!        write(*,*)squarei(2_real32)
!!        write(*,*)squarei(2_real64)
!!        !!write(*,*)squarei(2_real128)
!!        write(*,*)squarei(2_real256)
!!     contains
!!
!!     function squarei(invalue) result (dvalue)
!!     use M_anyscalar, only : anyscalar_to_real
!!     implicit none
!!     class(*),intent(in)  :: invalue
!!     real                 :: invalue_local
!!     real                 :: dvalue
!!        invalue_local=anyscalar_to_real(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program scalars
!! !===================================================================================================================================
!===================================================================================================================================
pure function anyscalar_to_real(valuein) result(r_out)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
character(len=*),parameter :: ident= '@(#)M_anyscalar::anyscalar_to_real(3f): convert integer or real parameter of any kind to real'
class(*),intent(in)     :: valuein
   real              :: r_out
   select type(valuein)
   type is (integer(kind=int8));   r_out=real(valuein)
   type is (integer(kind=int16));  r_out=real(valuein)
   type is (integer(kind=int32));  r_out=real(valuein)
   type is (integer(kind=int64));  r_out=real(valuein)
   type is (integer(kind=int128)); r_out=real(valuein)
   type is (real(kind=real32));    r_out=real(valuein)
   type is (real(kind=real64));    r_out=real(valuein)
   type is (real(kind=real128));   r_out=real(valuein)
   type is (real(kind=real256));   r_out=real(valuein)
   end select
end function anyscalar_to_real
!===================================================================================================================================
!>
!!##NAME
!!
!!    anyinteger_to_128bit(3f) - [M_anyscalar]convert integer any kind to integer(kind=128)
!!
!!##SYNOPSIS
!!
!!
!!    function anyinteger_to_128bit(intin) result(ii38)
!!
!!     integer(kind=int128) function anyinteger_to_128bit(value)
!!     class(*),intent(in)     :: intin
!!     integer(kind=int8|int16|int32|int64|int128) :: value
!!
!!##DESCRIPTION
!!
!!    This function uses polymorphism to allow arguments of different types
!!    generically. It is used to create other procedures that can take
!!    many scalar arguments as input options, equivalent to  passing the
!!    parameter VALUE as int(VALUE,0_int128).
!!
!!##OPTIONS
!!
!!    VALUEIN  input argument of a procedure to convert to type REAL(KIND=128).
!!             May be of KIND kind=int8, kind=int16, kind=int32, kind=int64,
!!             kind=int128, kind=real32, kind=real64, kind=real128,
!!             or kind=real256
!!##RESULTS
!!             The returned value is of kind REAL(KIND=REAL128)  and is
!!             the value of VALUIN converted to real(KIND=REAL128).
!!##EXAMPLE
!!
!!    Sample program
!!
!!     program scalars
!!     use M_anyscalar,     only : int128
!!     use iso_fortran_env, only : int8, int16, int32, int64
!!     implicit none
!!        ! call same function with many scalar input types
!!        write(*,*)squarei(2_int8)
!!        write(*,*)squarei(2_int16)
!!        write(*,*)squarei(2_int32)
!!        write(*,*)squarei(2_int64)
!!        write(*,*)squarei(2_int128)
!!     contains
!!
!!     function squarei(invalue)
!!     use M_anyscalar, only : anyinteger_to_128bit, int128
!!     implicit none
!!     class(*),intent(in)  :: invalue
!!     real                 :: invalue_local
!!     integer(kind=int128) :: squarei
!!        invalue_local=anyinteger_to_128bit(invalue)
!!        dvalue=invalue_local*invalue_local
!!     end function squarei
!!
!!     end program scalars
!! !===================================================================================================================================
!===================================================================================================================================
function anyinteger_to_128bit(intin) result(ii38)
use iso_fortran_env, only : error_unit !! ,input_unit,output_unit
implicit none
character(len=*),parameter :: ident= '@(#)M_anyscalar::anyinteger_to_128: convert integer parameter of any kind to 128-bit integer'
class(*),intent(in)     :: intin
   integer(kind=int128) :: ii38
   select type(intin)
   type is (integer(kind=int8));   ii38=int(intin,kind=int128)
   type is (integer(kind=int16));  ii38=int(intin,kind=int128)
   type is (integer(kind=int32));  ii38=intin
   type is (integer(kind=int64));  ii38=intin
   type is (integer(kind=int128)); ii38=intin
   class default
      write(error_unit,*)'ERROR: unknown integer type'
      stop
   end select
end function anyinteger_to_128bit
!===================================================================================================================================
end module M_anyscalar
!===================================================================================================================================
