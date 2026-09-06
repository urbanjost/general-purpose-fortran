      program demo_anyscalar_to_real128
      use, intrinsic :: iso_fortran_env, only : &
         & i8=>int8, i16=>int16, i32=>int32, i64=>int64
      use, intrinsic :: iso_fortran_env, only : &
         & sp=>real32, dp=>real64, qp=>real128
      implicit none
         ! call same function with many scalar input types
         write(*,*)minall(&
         & 2_i8, 7_i16, 8_i32, 9_i64, 2.0123123_sp, 3.0123_dp, 5.0_qp)
         write(*,*)minall(&
         & 5.0_qp, 3.0123_dp, 2.0123123_sp, 9_i64, 8_i32, 7_i16, 2_i8)
      contains

      function minall(a,b,c,d,e,f,g) result (value)
      use M_anything, only : x=>anyscalar_to_real128
      class(*),intent(in) :: a,b,c,d,e,f,g
      real(kind=qp)       :: value
         value=min( x(a),x(b),x(c),x(d),x(e),x(f),x(g) )
      end function minall

      end program demo_anyscalar_to_real128
