          program demo_textang
          use :: M_pixel
          use :: M_pixel, only : cosd, sind
          use :: M_writegif, only : writegif
          implicit none
          integer :: i
          !! set up drawing environment
          call prefsize(600,600)
          call vinit()
          call ortho2(-100.0,100.0,-100.0,100.0)
          call textsize(7.0,7.0)
          call linewidth(20)
          do i=1,30
             !! draw radial lines
             call color(1)
             call move2(0.0,0.0)
             call draw2(100.0*cosd(i*12),100.0*sind(i*12))
             !! draw rotated text
             call color(7)
             call move2(30.0*cosd(i*12),30.0*sind(i*12))
             call textang(i*12.0)
             call drawstr('angled text')
          enddo

          call writegif('textang.3m_pixel.gif',P_pixel,P_colormap)
          call execute_command_line('display textang.3m_pixel.gif')

          call vexit()

          end program demo_textang
