            program demo_rmove2
            use M_pixel, only: prefsize, vinit, ortho2, clear
            use M_pixel, only: move2, rmove2, rdraw2, vexit
            use M_pixel, only: linewidth
            use M_pixel, only: P_pixel, P_colormap
            use M_writegif, only : writegif
            implicit none
            integer :: i
               call prefsize(500,500)
               call vinit()
               call ortho2(-110.0,110.0,-110.0,110.0)
               call move2(-100.0,-100.0)
               call linewidth(70)
               do i=1,20
                  call rmove2(10.0, 0.0)
                  call rdraw2( 0.0,10.0)
               enddo
               call writegif('rmove2.3m_pixel.gif',P_pixel,P_colormap)
               call  execute_command_line('display rmove2.3m_pixel.gif')
               call vexit()
            end program demo_rmove2
