program demo_linewidth
!(LICENSE:PD)
   use M_DRAW,    only : prefsize, vinit, ortho2, clear, getkey
   use M_DRAW,    only : move2, draw2, vexit, color, linewidth
   use M_units,    only : d2r, polar_to_cartesian
   implicit none
   integer :: i
   integer :: ipaws
   real    :: x,y,r,a,b,theta
   ! The Archimedean spiral is the locus of points corresponding
   ! to the locations over time of a point moving away from a
   ! fixed point with a constant speed along a line which rotates
   ! with constant angular velocity.
   !    r=a+b*theta
   ! Changing the parameter A will turn the spiral,
   ! while b controls the distance between successive turnings.
   call prefsize(401,401)
   call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
   call ortho2(-150.0,150.0,-150.0,150.0)
   call clear()
   call move2(0.0,0.0)
   call color(2)
   a=0.0
   b=2.0
   do i=0,360*10,5
      theta=d2r(i)
      r=a+b*theta
      call polar_to_cartesian(r,theta,x,y)
      call linewidth(i/5/3)
      call draw2(x,y)
   enddo
   ipaws=getkey()
   call vexit()
end program demo_linewidth
