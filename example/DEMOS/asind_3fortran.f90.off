      program demo_asind
      use, intrinsic :: iso_fortran_env, only : dp=>real64
      implicit none
      ! value to convert degrees to radians
      real(kind=dp),parameter :: R2D=180.0_dp/acos(-1.0_dp)
      real(kind=dp)           :: angle, rise, run
      character(len=*),parameter :: all='(*(g0,1x))'
        ! given sine(theta) = 1.25 miles/50 miles (opposite/hypotenuse)
        ! then taking the arcsine of both sides of the equality yields
        ! theta = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
        rise=1.250_dp
        run=50.00_dp
        angle = asind(rise/run)
        print all, 'angle of incline(degrees) = ', angle
        angle = angle/R2D
        print all, 'angle of incline(radians) = ', angle

        print all, 'percent grade=',rise/run*100.0_dp
      contains
      subroutine sub1()
      ! notice the (incidentally empty) type is defined below
      ! the implicit statement
      implicit type(nil) (a)
      type nil
      end type nil
      type(nil) :: anull
      end subroutine sub1
      end program demo_asind
