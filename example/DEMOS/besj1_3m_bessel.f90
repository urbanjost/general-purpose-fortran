     program demo_besj1
     use, intrinsic :: iso_fortran_env, only : real32, real64, real128
     use M_bessel, only: besj1
     implicit none
     real(kind=real64) :: x = 1.0_real64
        write(*,*)'value:   ',x
        write(*,*)'bessel_1:',bessel_j1(x)
        write(*,*)'besj1:   ',besj1(x)
     end program demo_besj1
