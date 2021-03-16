!@(#) basic test program for a driver.
!  if we can draw a line and do hardware text we are almost there!
!
program ftrivial
!(LICENSE:PD)
use M_draw
use ISO_C_BINDING
implicit none
character(len=50) :: device
integer           :: ios
integer           :: idum
   print*,'Enter output device:' ! read in device name
   read(*,'(a)',iostat=ios)device
   if(ios.ne.0)device=''
   call vinit(device)
   call font('large')       ! set font to hardware text large
   call color(D_BLACK)      ! set current color to black
   call clear               ! clear to current color
   call color(D_GREEN)      ! we want to draw in green
   call move2(-1.0, 0.0)    ! draw a horizontal line at y = 0
   call draw2(1.0, 0.0)
   idum=getkey()            ! pause for some input
   call move2(0.0, 0.0)     ! draw a line along x = 0
   call draw2(0.0, 1.0)
   call move2(0.0, 0.0)     ! move to the middle of the screen
   call drawstr('Hello')    ! draw 'Hello' starting at the origin
   idum=getkey()            ! pause again
   call vexit               ! set screen back to original state
end program ftrivial
