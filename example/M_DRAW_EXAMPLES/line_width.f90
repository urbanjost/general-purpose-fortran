program demo_linewidth
!(LICENSE:PD)
use M_DRAW, only: prefsize, vinit, ortho2, clear, getkey
use M_DRAW, only: move2, draw2, vexit, color, linewidth
implicit none
integer :: i
integer :: ipaws
real    :: x, y, r, a, b, theta
! The Archimedean spiral is the locus of points corresponding
! to the locations over time of a point moving away from a
! fixed point with a constant speed along a line which rotates
! with constant angular velocity.
!    r=a+b*theta
! Changing the parameter A will turn the spiral,
! while b controls the distance between successive turnings.
   call prefsize(401, 401)
   call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
   call ortho2(-150.0, 150.0, -150.0, 150.0)
   call clear()
   call move2(0.0, 0.0)
   call color(2)
   a = 0.0
   b = 2.0
   do i = 0, 360*10, 5
      theta = d2r(i)
      r = a + b*theta
      call polar_to_cartesian(r, theta, x, y)
      call linewidth(i/5/3)
      call draw2(x, y)
   end do
   ipaws = getkey()
   call vexit()
contains
   subroutine polar_to_cartesian(radius, inclination, x, y)
   implicit none
   character(len=*), parameter::ident_21 = "@(#)M_units::polar_to_cartesian(3f): convert polar coordinates to cartesian coordinates"
   real, intent(in) :: radius, inclination
   real, intent(out)  :: x, y
      if (radius == 0) then
         x = 0.0
         y = 0.0
      else
         x = radius*cos(inclination)
         y = radius*sin(inclination)
      end if
   end subroutine polar_to_cartesian
   elemental doubleprecision function d2r(idegrees)

      character(len=*), parameter::ident_8 = "@(#)M_units::d2r(3f): Convert degrees to radians"

      doubleprecision, parameter :: RADIAN = 57.2957795131d0 ! degrees
      integer, intent(in) :: idegrees                      ! input degrees to convert to radians
      d2r = dble(idegrees)/RADIAN                      ! do the unit conversion
   end function d2r
end program demo_linewidth
