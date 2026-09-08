      program demo_tanpi
      use, intrinsic :: iso_fortran_env, only : real64
      implicit none
      integer :: i
      real(kind=real64) :: x
         do i=0,8
            x=0.250000000d0*i
            write(*,101)x, tanpi(x), tanpi(x)*180.0d0
         enddo
      101 format(g0,t23,g0,t50,g0)
      end program demo_tanpi
