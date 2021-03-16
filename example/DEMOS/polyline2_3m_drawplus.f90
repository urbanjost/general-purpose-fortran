            program demo_polyline2
            use M_draw
            use M_drawplus, only : polyline2
            implicit none
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
               ipaws=getkey()
               call vexit()
            end program demo_polyline2
