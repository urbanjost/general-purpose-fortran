! @(#) test some object-related procedures
program sobj
!(LICENSE:PD)
use M_draw
character(len=20) :: device
   write(*,*)"Fortran: Enter output device: "
   read(*,*)device
   call prefsize(300, 300)
   call prefposition(100, 100)
   call vinit(device)   ! set up device 
   call color(7)   ! set current color 
   call clear()   ! clear screen to current color 
   call makeobj(3)
      call polyfill(.true.)
      call color(2)
      call circle(0.0,0.0,4.0)
   call closeobj()
!   juaspct(-5.0,-5.0,5.0,5.0)
   call ortho2(-5.1,5.3,-5.2,5.4)
   call callobj(3)
   call move2(-5.0,-5.0)
   call draw2(5.0,5.0)
   call move2(-5.0,5.0)
   call draw2(5.0,-5.0)
   call saveobj(3,"fthree.obj")
   if(isobj(3))then
      write(*,*)' 3 is an object (CORRECT)'
   else
      write(*,*)' 3 is not an object (ERROR)'
   endif
   if(isobj(4))then
      write(*,*)' 4 is an object (ERROR)'
   else
      write(*,*)' 4 is not an object (CORRECT)'
   endif
   idum=getkey()! wait for some input 
   call vexit()!  set the screen back to its original state
end program sobj
