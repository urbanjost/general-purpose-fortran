          program demo_invokeobj
          use M_draw
          implicit none
          real :: a, angle, step
          integer :: i, idum
          ! set window size
             call prefsize(700,700)
             call prefposition( 0, 0)
             call vinit ('X11')
             a=1.0
          ! make an object to draw ( a disk with an arrow on it)
             call makeobj(12345)
             call polyfill(.TRUE.)
             call color( 5)
             call circle( 0.0, 0.0, a)
             call color( 3)
             call makepoly()
             call move2( 0.00*a, 0.80*a)
             call draw2( 0.50*a, 0.30*a)
             call draw2( 0.20*a, 0.30*a)
             call draw2( 0.20*a,-0.80*a)
             call draw2(-0.20*a,-0.80*a)
             call draw2(-0.20*a, 0.30*a)
             call draw2(-0.50*a, 0.30*a)
             call draw2( 0.00*a, 0.80*a)
             call closepoly()
             call polyfill(.FALSE.)
             call color(7)
             call linewidth(20)
             call circleprecision(200)
             call circle( 0.0, 0.0, a)
             call vflush()
             call closeobj()
          ! draw the disk invoking different rotation
             ANGLE=0.0
             STEP=0.4
             idum=backbuffer()
             idum=-1
             if(idum.ne.-1)then
                do i=1,int(360/STEP*10)
                   idum=backbuffer()
                   call clear()
                   call invokeobj( 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, ANGLE, ANGLE, ANGLE,12345)
                   ANGLE=ANGLE+STEP
                   call swapbuffers()
                enddo
             else
                ANGLE=45.0
                call invokeobj( 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, ANGLE, ANGLE, ANGLE,12345)
                idum=getkey()
             endif
             call vexit()
          end program demo_invokeobj
