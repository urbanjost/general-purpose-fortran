      program demo_atan
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      character(len=*),parameter  :: g='(*(g0,1x))'
      real(kind=real64),parameter :: &
       Deg_Per_Rad = 57.2957795130823208767981548_real64
      real(kind=real64)           :: x
      real(kind=real64),parameter              :: &

       xvals(*)=[2.0d0, 2.0d0, 2.0d0,  2.0d0,  -2.0d0, -2.0d0, -2.0d0, -2.0d0 ]
      real(kind=real64),parameter              :: &
       yvals(*)=[2.0d0, 2.0d0, -2.0d0, -2.0d0, 2.0d0,  2.0d0,  -2.0d0, -2.0d0 ]
         !
         ! basics
         !
         ! with just a real X returns angles in radians
         ! in the interval [-PI/2, PI/2].
          x=2.866_real64
          print g, atan(x)
         !
         ! all the quadrants using two arguments
         !
          print g, atan( 2.0d0, 2.0d0),atan( 2.0d0, 2.0d0)*Deg_Per_Rad
          print g, atan( 2.0d0,-2.0d0),atan( 2.0d0,-2.0d0)*Deg_Per_Rad
          print g, atan(-2.0d0, 2.0d0),atan(-2.0d0, 2.0d0)*Deg_Per_Rad
          print g, atan(-2.0d0,-2.0d0),atan(-2.0d0,-2.0d0)*Deg_Per_Rad
         !
         ! elemental
         !
          print g, 'elemental:'
          print g, atan(xvals,yvals)*Deg_Per_Rad
          print g, 'elemental:'
         !
         ! when x and y are present, atan(3) is an alias for atan2(2)
         !
          print g, 'For comparison to atan2(3):'
          print g, atan2(xvals,yvals)*Deg_Per_Rad
          print g, 'test1 ',merge('PASSED','FAILED',     &
          & all(atan(xvals,yvals)==atan2(xvals,yvals))), &
          & atan(xvals,yvals)==atan2(xvals,yvals)

      end program demo_atan
