          program demo_arc
          use M_pixel
          use M_writegif, only : writegif
          implicit none
          integer  :: transparent=0
             call prefsize(600,240)
             call vinit()
             call ortho2(0.0,60.0,0.0,24.0)
             call linewidth(400)
             call color(1)
             call arc(16.0,12.0,12.0,90.0,270.0)
             call color(2)
             call arc(44.0,12.0,12.0,-90.0,90.0)
             ! write gif with a transparent background
             call writegif('arc.3m_pixel.gif',P_pixel,P_ColorMap,transparent)
             call vexit()
          end program demo_arc
