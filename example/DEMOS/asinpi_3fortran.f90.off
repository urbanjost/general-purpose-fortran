      program demo_asinpi
      use, intrinsic :: iso_fortran_env, only : dp=>real64
      implicit none
      ! value to convert degrees to half-revolutions
      real(kind=dp),parameter :: D2HR=1/180.0_dp
      real(kind=dp)           :: angle, rise, run
      character(len=*),parameter :: all='(*(g0,1x))'
        ! basics
        ! elemental
        print all, asinpi( [0.0d0, 0.5d0, -0.5d0, 1.0d0, -1.0d0 ])
        !
        ! sample application
        ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
        ! then taking the arcsine of both sides of the equality yields
        ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
        rise=1.250_dp
        run=50.00_dp
        angle = asinpi(rise/run)
        print all, 'angle of incline(half-revolutions) = ', angle
        angle = angle/D2HR
        print all, 'angle of incline(degrees) = ', angle
        print all, 'percent grade=',rise/run*100.0_dp
      contains
      elemental function asinpi(x)
      real(kind=dp),parameter  :: PI=acos(-1.0_dp)
      real(kind=dp),intent(in) :: x
      real(kind=dp)            :: asinpi
         asinpi=asin(x)/PI
      end function asinpi
      end program demo_asinpi
