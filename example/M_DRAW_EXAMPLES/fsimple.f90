!@(#)  show basic line drawing, text and (if applicable) color.
!
!  As none of the projection routines have been called we
!  move and draw in the initial coordinate system -1.0 to 1.0.
!
program fsimple
!(LICENSE:PD)
use M_draw
integer,parameter :: BLACK = 0, GREEN = 2
character(len=50) :: device
character(len=80) :: fname
character(len=11) :: p
data p/'Hello world'/
   print*,'Enter output device:'
   read(*,'(a)') device
   print*,'Enter a font name:'
   read(*,'(a)') fname
!  set up device 
   call vinit(device)
!  change font to the argument 
   call font(fname)
!  set current color
   call color(BLACK)
!  clear screen to current color 
   call clear
   call color(GREEN)
!  2 d move to start where we want drawstr to start 
   call move2(-0.9, 0.9)
!  draw string in current color 
   call drawstr('A Simple Example')
!  the next four lines draw the x 
   call move2(0.0, 0.0)
   call draw2(0.76, 0.76)
   call move2(0.0, 0.76)
   call draw2(0.76, 0.0)

   call move2(0.0, 0.5)
   call drawstr('x done')
   call drawstr('next sentence')

   call move2(0.0, 0.1)
   do i = 1, 11
      call drawchar(p(i:i))
   enddo
!  the next five lines draw the square
   call move2(0.0, 0.0)
   call draw2(0.76, 0.0)
   call draw2(0.76, 0.76)
   call draw2(0.0, 0.76)
   call draw2(0.0, 0.0)
!  wait for some input 
   idum=getkey()
!  set the screen back to its original state 
   call vexit
end program fsimple
