          program demo_polyline2
          use M_pixel
          use M_writegif, only : writegif
          implicit none
          integer :: transparent=0
          integer :: ipaws
             call prefsize(300,300)
             call vinit(' ')
             call ortho2(-2.0,2.0,-2.0,2.0)
             call color(2)
             call linewidth(100)
             call polyline2([-0.5,-0.5, -0.5,+0.5, +0.5,+0.5, +0.5,-0.5])
             call color(4)
             call polyline2( [-1,-1,+1,+1,-1] , &  ! X values
             & [-1,+1,+1,-1,-1] )    ! Y values
              ! write gif with a transparent background
             call writegif('polyline2.3m_pixel.gif',P_pixel,P_ColorMap,transparent)
             call vexit()
          end program demo_polyline2
