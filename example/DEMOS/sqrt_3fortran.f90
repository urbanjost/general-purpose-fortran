      program demo_sqrt
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      real(kind=real64) :: x, x2
      complex :: z, z2

        ! basics
         x = 2.0_real64
         ! complex
         z = (1.0, 2.0)
         write(*,*)'input values ',x,z

         x2 = sqrt(x)
         z2 = sqrt(z)
         write(*,*)'output values ',x2,z2

        ! elemental
        write(*,*)'elemental',sqrt([64.0,121.0,30.0])

        ! alternatives
         x2 = x**0.5
         z2 = z**0.5
         write(*,*)'alternatively',x2,z2

      end program demo_sqrt
