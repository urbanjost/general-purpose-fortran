     program demo_besj0
     use, intrinsic :: iso_fortran_env, only : real32, real64, real128
     use, intrinsic :: iso_fortran_env, only : wp=>real64
     use M_bessel, only: besj0
     implicit none
     real(kind=wp)           :: y, z
     real(kind=wp),parameter :: x(*)=[-huge(0.0_wp),0.0_wp,-100.0_wp, &
     & 30000.0_wp,huge(0.0_wp)]
     integer                 :: i
        do i=1,size(x)
           y = bessel_j0(x(i))
           z = besj0(x(i))
           write(*,*)x(i),y,z
        enddo
     end program demo_besj0
