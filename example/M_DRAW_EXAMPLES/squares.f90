program demo_rect
!(LICENSE:PD)
   use M_draw
   implicit none
   integer :: i, ipaws
   real    :: r

   !! set up graphics area
   call prefsize(400,400)
   call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
   call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)

   !! draw some filled rectangles
   call polyfill(.true.)
   do i=95,5,-10
      call color(i/10)
      r=real(i)
      call rect( -1.0*r, -1.0*r, 1.0*r, 1.0*r )
   enddo

   !! draw some rectangles
   call polyfill(.false.)
   call linewidth(50)
   call color(7)
   do i=5,95,5
      r=real(i)
      call rect( -1.0*r, -1.0*r, 1.0*r, 1.0*r )
   enddo

   !! pause
   call vflush()
   ipaws=getkey()

   !! wrap up graphics
   call vexit()

end program demo_rect
