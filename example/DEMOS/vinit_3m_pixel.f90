       program demo_vinit
       use M_pixel, only    : prefsize, vinit, ortho2, clear
       use M_pixel, only    : move2, draw2, vexit, color
       use M_pixel, only    : P_pixel, P_colormap
       use M_pixel__writegif, only : writegif
       implicit none
          call prefsize(60,40)
          call vinit()
          call ortho2(-300.0,300.0,-200.0,200.0)
          call clear(0)
          call color(1)
          call move2(-300.0,-200.0)
          call draw2(300.0,200.0)
          call move2(300.0,-200.0)
          call draw2(-300.0,200.0)
          call writegif('vinit.3m_pixel.gif',P_pixel,P_colormap)
          call vexit()
       end program demo_vinit
