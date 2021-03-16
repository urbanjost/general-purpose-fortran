          program demo_point2
          use :: M_pixel
          use :: M_writegif, only : writegif
          implicit none
          integer :: i
          call vinit()
          call color(5)
          do i=1,20
             call linewidth(50*i)
             call point2(real(i*25),real(i*25))
          enddo
          call writegif('point2.3m_pixel.gif',P_pixel,P_colormap)
          call vexit()
          end program demo_point2
