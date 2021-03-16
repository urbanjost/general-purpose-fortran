          program demo_rect
          use M_pixel
          use M_writegif, only : writegif
          implicit none
             integer :: i

             !! set up graphics area
             call prefsize(400,400)
             call vinit()
             call ortho2(left=-100.0, right=100.0, bottom=-100.0, top=100.0)

             !! draw some filled rectangles
             do i=95,5,-10
                call makepoly()
                call color(i/10)
                call rect( -1.0*i, -1.0*i, 1.0*i, 1.0*i )
                call closepoly()
             enddo

             !! draw some rectangles
             call linewidth(50)
             call color(7)
             do i=5,95,5
                call rect( -1.0*i, -1.0*i, 1.0*i, 1.0*i )
             enddo

             !! render pixel array to a file
             call writegif('rect.3m_pixel.gif',P_pixel,P_colormap)

             !! display graphic assuming display(1) is available
             call execute_command_line('display rect.3m_pixel.gif')

             !! wrap up graphics
             call vexit()

          end program demo_rect
