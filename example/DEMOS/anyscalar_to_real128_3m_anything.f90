           program demo_anyscalar_to_real128
           use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
           use, intrinsic :: iso_fortran_env, only : real32, real64, real128
           implicit none
              ! call same function with many scalar input types
              write(*,*)squarei(2_int8)
              write(*,*)squarei(2_int16)
              write(*,*)squarei(2_int32)
              write(*,*)squarei(2_int64)
              write(*,*)squarei(2.0_real32)
              write(*,*)squarei(2.0_real64)
              write(*,*)squarei(2.0_real128)
           contains

           function squarei(invalue) result (dvalue)
           use M_anything, only : anyscalar_to_real128
           class(*),intent(in)  :: invalue
           real(kind=real128)   :: invalue_local
           real(kind=real128)   :: dvalue
              invalue_local=anyscalar_to_real128(invalue)
              dvalue=invalue_local*invalue_local
           end function squarei

           end program demo_anyscalar_to_real128
