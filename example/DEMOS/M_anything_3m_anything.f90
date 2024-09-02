      program demo_M_anything
      use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
         ! call same function with many scalar input types
         write(*,*)squareall(2_int8)
         write(*,*)squareall(2_int16)
         write(*,*)squareall(2_int32)
         write(*,*)squareall(2_int64)
         write(*,*)squareall(2.0_real32)
         write(*,*)squareall(2.0_real64)
         write(*,*)squareall(2.0_real128)
      contains

      function squareall(invalue) result (dvalue)
      use M_anything, only : anyscalar_to_double
      class(*),intent(in)  :: invalue
      doubleprecision      :: invalue_local
      doubleprecision      :: dvalue
         invalue_local=anyscalar_to_double(invalue)
         dvalue=invalue_local*invalue_local
      end function squareall

      end program demo_M_anything
