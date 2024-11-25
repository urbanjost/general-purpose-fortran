       program demo_prefsize
       use M_pixel, only: prefsize, vinit, ortho2, clear
       use M_pixel, only: move2, draw2, vexit, color
       use M_pixel, only : P_pixel,P_colormap
       use M_pixel__writegif, only : writegif
       implicit none
          ! make first file with one size
          call prefsize(60*2,40*2)
          call vinit()
          call picture()
          call writegif('prefsize.3m_pixel.gif',P_pixel,P_colormap)
          call vexit()

          ! make second file with another size
          call prefsize(60*3,40*3)
          call vinit()
          call picture()
          call writegif('prefsize_B.3m_pixel.gif',P_pixel,P_colormap)
          call vexit()
       contains
       subroutine picture
          call ortho2(-300.0,300.0,-200.0,200.0)
          call clear(0)
          call color(1)
          call move2(-300.0,-200.0)
          call draw2(300.0,200.0)
          call move2(300.0,-200.0)
          call draw2(-300.0,200.0)
       end subroutine picture
       end program demo_prefsize
