program demo_circle
!(LICENSE:PD)
use M_DRAW
integer :: ipaws
   !! set up drawing surface
   call prefsize(400,400)
   call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
   call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)
   call color(3)
   call clear()
   call color(4)
   call linewidth(200)
   !! draw some circles
   call circle(0.0, 0.0, 90.0)
   call color(1)
   call circle(0.0, 0.0, 40.0)
   call color(2)
   call circle(-25.0, 25.0, 50.0)
   call circle(-25.0,-25.0, 50.0)
   call circle( 25.0, 25.0, 50.0)
   call circle( 25.0,-25.0, 50.0)

   ipaws=getkey()
   !! exit graphics mode
   call vexit()
end program demo_circle
