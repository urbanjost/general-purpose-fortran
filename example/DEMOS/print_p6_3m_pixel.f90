       program demo_print_p6
       use M_pixel, only : prefsize,vinit,ortho2,vexit
       use M_pixel, only : linewidth,circle,color
       use M_pixel, only : print_p6
       implicit none
          call prefsize(40,40)
          call vinit()
          call ortho2(-100.0,100.0,-100.0,100.0)
          call linewidth(400)
          call circle(0.0,0.0,45.0)
          call color(3)
          call circle(0.0,0.0,25.0)
          call print_p6('demo_print.p6')
          call vexit()
       end program demo_print_p6
