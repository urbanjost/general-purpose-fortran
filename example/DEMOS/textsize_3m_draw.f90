          program demo_textsize
          use M_draw
          implicit none
          integer :: i,ii
          integer :: ipaws
             !! set up long bar as plotting area
             call prefsize(900,150)
             call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
             call ortho2(-30.0, 30.0, -5.0, 5.0)
             call font('times.r')

             call move2(-23.0,-4.5)
             call color(D_WHITE)
             call textsize(2.0,2.0)
             call move2(-27.5,-3.0)
             call draw2( 27.5,-3.0)
             call move2(-27.5,-3.0)

             do i=1,7
                ii=nint((i*20)*0.30)
                call linewidth(nint(ii*2.35))
                call textsize(real(i),real(i))
                call color(D_MAGENTA)
                call drawstr('aA')
             enddo

             ipaws=getkey()

             call vexit()

          end program demo_textsize
