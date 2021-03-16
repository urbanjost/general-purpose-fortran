          program demo_textsize
          use M_pixel
          use M_writegif, only : writegif
          implicit none
          integer :: i,ii
             !! set up long bar as plotting area
             call prefsize(900,150)
             call vinit()
             call ortho2(-30.0, 30.0, -5.0, 5.0)
             call font('DUPLEX')
             call move2(-23.0,-4.5)
             call color(7)
             call textsize(2.0,2.0)
             call move2(-27.5,-3.0)
             call draw2( 27.5,-3.0)
             call move2(-27.5,-3.0)
             do i=1,7
                ii=nint((i*20)*0.30)
                call linewidth(nint(ii*2.35))
                call textsize(real(i),real(i))
                call color(5)
                call drawstr('aA')
             enddo
             ! write plot as GIF file
             call writegif('textsize.3m_pixel.gif',P_pixel,P_colormap)
             call vexit()
             ! use system to display GIF file
             call execute_command_line('display textsize.3m_pixel.gif')
          end program demo_textsize
