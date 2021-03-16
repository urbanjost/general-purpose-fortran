program fgetstr
!(LICENSE:PD)

use M_draw

!@(#)   reading a string from graphic input

   integer,parameter  :: BLACK = 0, GREEN = 2, YELLOW = 3
   character(len=128) ::  buf(10)
   character(len=20)  ::  dev

   print*, 'Enter device:'
   read (*, '(a)') dev

   print*, 'Enter a font name:'
   read (*, '(a)') buf(1)

   call vinit(dev)

   call font(buf(1))

   call clipping(.false.)

   call window(-1.0, 1.0, -1.0, 1.0, 1.0, -1.0)
   call lookat(0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0)

   call textsize(0.1, 0.25)


   call rotate(30.0, 'x')
   call rotate(30.0, 'z')
   call rotate(60.0, 'y')

   call color(BLACK)
   call clear()
   call color(YELLOW)

   call rect(-0.5, -0.5, 0.5, 0.5)
   call move2(-0.5, 0.0)

   call color(GREEN)

   n = 0
1  continue
   n = n + 1
   i = getstring(BLACK, buf(n))
   if (i .gt. 0 .and. n .le. 10) goto 1

   call vexit

   do 2 i = 1, n - 1
      write(*, '(1x, ''Line'',i3,'' was: '', a)') i, buf(i)
2  continue

end program fgetstr
