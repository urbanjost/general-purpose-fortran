     program demo_print_ansi
     use M_pixel
     implicit none
     call prefsize(80,24)
        call vinit()
        call ortho2(0.0,80.0,0.0,24.0)
        call linewidth(400)
        call color(1)
        call circle(12.0,12.0,6.0)
        call color(2)
        call circle(72.0,12.0,6.0)
        call print_ansi()
        call vexit()
     end program demo_print_ansi
