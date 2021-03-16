            program demo_rdraw2
            use M_pixel, only: vinit, prefsize, ortho2,linewidth
            use M_pixel, only: clear, move2, rdraw2, vexit,color
            use M_pixel, only: P_pixel, P_colormap
            use M_writegif, only : writegif
            implicit none

               call prefsize(200,200)
               call vinit()
               call ortho2(-55.0, 55.0, -55.0,  55.0)
               call linewidth(400)
               call color(7)
               call clear()

               call color(1)
               call move2(-50.0,0.0)
               call square(50.0)

               call linewidth(200)
               call color(2)
               call move2(  0.0,-50.0)
               call square(50.0)

               call writegif('rdraw2.3m_pixel.gif',P_pixel,P_colormap)
               call execute_command_line('display rdraw2.3m_pixel.gif')
               call vexit()

               contains

               subroutine square(side)
               real,intent(in) :: side
               call rdraw2( side,   0.0)
               call rdraw2(  0.0,  side)
               call rdraw2(-side,   0.0)
               call rdraw2(  0.0, -side)
               end subroutine square

              end program demo_rdraw2
