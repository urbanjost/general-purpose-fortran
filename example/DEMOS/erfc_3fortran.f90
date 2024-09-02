      program demo_erfc
      use, intrinsic :: iso_fortran_env, only : &
       & real_kinds, real32, real64, real128
      implicit none
      real(kind=real64) :: x = 0.17_real64
         write(*,'(*(g0))')'X=',x, ' ERFC(X)=',erfc(x)
         write(*,'(*(g0))')'equivalently 1-ERF(X)=',1-erf(x)
      end program demo_erfc
