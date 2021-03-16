          program demo_draw2
          use M_pixel,    only : prefsize, vinit, ortho2, clear
          use M_pixel,    only : move2, draw2, vexit, color,linewidth
          use M_pixel,    only : P_pixel, P_colormap
          use M_writegif, only : writegif
          use M_pixel,    only : d2r, polar_to_cartesian
          !
          ! The Archimedean spiral is the locus of points corresponding
          ! to the locations over time of a point moving away from a
          ! fixed point with a constant speed along a line which rotates
          ! with constant angular velocity.
          !    r=A+B*theta
          ! Changing the parameter A will turn the spiral,
          ! while B controls the distance between successive turnings.
          !
          implicit none
          integer        :: i
          real           :: x,y,radius,theta
          real,parameter :: rotate=0.0, gap=2.0
             call prefsize(400,400)
             call vinit('')
             call ortho2(-150.0,150.0,-150.0,150.0)
             call color(5)
             call clear()
             call move2(0.0,0.0)
             call color(0)
             call linewidth(40)
             do i=0,360*10,5
                theta=d2r(i)
                ! equation in polar coordinates
                radius=rotate+gap*theta
                ! convert polar coordinates to cartesian
                call polar_to_cartesian(radius,theta,x,y)
                ! draw from current position to end of next segment
                call draw2(x,y)
             enddo
             ! write the pixel map array as a GIF image file
             call writegif('draw2.3m_pixel.gif',P_pixel,P_colormap)
             ! exit graphics mode
             call vexit()
          end program demo_draw2
