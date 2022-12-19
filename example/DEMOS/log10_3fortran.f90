      program demo_log10
      use, intrinsic :: iso_fortran_env, only : real_kinds, &
       & real32, real64, real128
      implicit none
      real(kind=real64) :: x = 10.0_real64

         x = log10(x)
         write(*,'(*(g0))')'log10(',x,') is ',log10(x)

         ! elemental
         write(*, *)log10([1.0, 10.0, 100.0, 1000.0, 10000.0, &
                           & 100000.0, 1000000.0, 10000000.0])

      end program demo_log10
