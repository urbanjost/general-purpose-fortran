program fshapes
!(LICENSE:PD)
!
!@(#)  This program shows some of the simple primitives.
!
use M_draw
implicit none
integer :: ix
integer :: iy
integer :: idum
character(len=50) :: device

   print*,'Enter output device:'
   read(*,'(a)')device
   if(device.eq.'x11')then
      write(*,*)'enter position'
      read(*,*)ix,iy
      call prefposition(ix,iy)
      call prefsize(500,500)
   endif
   call vinit(device)
!
! the two lines below clear the screen to white if we have
! colours. Color is ignored on a monochrome device so in that case
! screen will be cleared to its background color, normally black.
!
   call color(D_BLACK)
   call clear()
!
! set the screen to be 2.0 units wide and 2.0 units wide, with
! the drawable coordinates going from -1.0 to 1.0.
!
   call ortho2(-1.0, 1.0, -1.0, 1.0)
!
! set the text size. After this call no characters in the
! current font will be wider than 0.1 or higher than 0.1 in
! world units. As the window is currently 2.0 units wide and
! 2.0 units high text will be about one twentieth of the screen
! size.
!
   call textsize(0.1, 0.1)

   call color(D_MAGENTA)
!
! okay, so we want to draw in the range -1 to 1, but we
! only want to draw in the top lefthand corner of the
! screen. The call to viewport allows us to do this. As
! viewport always takes screen coordinates, which are
! always in the range of -1 to 1 we can change
! the viewport to somewhere else on the screen
! by giving a range in -1 to 1.
!
   call viewport(-1.0, 0.0, 0.0, 1.0)
!
! write out a heading
!
   call move2(-0.9, -0.5)
   call drawstr('rect')
!
! draw a rectangle around the points (-0.2, -0.2), (-0.2, 0.2),
! (0.3, 0.2), and (0.3, -0.2).
!
   call rect(-0.2, -0.2, 0.3, 0.2)

   call color(D_BLUE)
!
! now we want to draw in the top right corner of the screen
!
   call viewport(0.0, 1.0, 0.0, 1.0)

   call move2(-0.9, -0.5)
   call drawstr('circle')
!
! draw a circle of radius 0.4 around the point (0.0, 0.0)
!
   call circle(0.0, 0.0, 0.4)

   call color(D_GREEN)
!
! draw in bottom left corner
!
   call viewport(-1.0, 0.0, -1.0, 0.0)

   call move2(-0.9, -0.5)
   call drawstr('ellipse')
!
! To draw an ellipse we change the aspect ratio so it is no longer
! 1 and call circle. In this case we use ortho2 to make the square
! viewport appear to be higher than it is wide. Alternatively you
! could use arc to construct one.
!
! The call to pushmatrix saves the current viewing transformation.
! After the ortho2 has been done, we restore the current viewing
! transformation with a call to popmatrix. (Otherwise everything
! after the call to ortho would come out looking squashed as the
! world aspect ratio is no longer 1).
!
   call pushmatrix()
   call ortho2(-1.0, 1.0, -1.0, 2.0)
   call circle(0.0, 0.5, 0.4)
   call popmatrix()

   call color(D_RED)
!
! now draw in bottom right corner
!
   call viewport(0.0, 1.0, -1.0, 0.0)

   call move2(-0.9, -0.5)
   call drawstr('arc')
!
! draw an arc centered at (0.0, 0.0), radius of 0.4. 0.0 is the start
! angle and 90.0 is the end angle of the arc being drawn. So this
! draws a quarter circle.
!
   call arc(0.0, 0.0, 0.4, 0.0, 90.0)

   idum=getkey()

   call vexit()

end program fshapes
