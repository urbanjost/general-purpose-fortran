      program demo_anyscalar_to_double
      use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
         ! call same function with many scalar input types
         write(*,*)sqrtany(2_int8)
         write(*,*)sqrtany(2_int16)
         write(*,*)sqrtany(2_int32)
         write(*,*)sqrtany(2_int64)
         write(*,*)sqrtany(2.0_real32)
         write(*,*)sqrtany(2.0_real64)
         write(*,*)sqrtany(2.0_real128)
      contains

      function sqrtany(invalue) result (value)
      use M_anything, only : anyscalar_to_double
      class(*),intent(in)  :: invalue
      doubleprecision      :: value
         value=sqrt(anyscalar_to_double(invalue))
      end function sqrtany

      end program demo_anyscalar_to_double
