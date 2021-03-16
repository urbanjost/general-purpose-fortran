            program demo_clear
            use M_draw, only  : prefsize, vinit, ortho2, clear, getkey
            use M_draw, only  : vexit, color, circle, polyfill
            use M_draw, only  : D_BLACK,   D_WHITE
            use M_draw, only  : D_RED,     D_GREEN,    D_BLUE
            use M_draw, only  : D_YELLOW,  D_MAGENTA,  D_CYAN
            implicit none
            integer :: ipaws

            call prefsize(300,300)
            call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
            ipaws=getkey()
            call ortho2(-100.0,100.0,-100.0,100.0)

            call color(D_BLACK)               ! set current  color
            call clear()                ! clear background to current color
            call color(D_RED)               ! set color to draw with
            call circle(0.0,0.0,50.0)
            ipaws=getkey()              ! pause for a keypress on interactive devices

            call color(D_GREEN)               ! make a second page
            call clear()
            call polyfill(.true.)
            call color(D_YELLOW)
            call circle(0.0,0.0,50.0)
            ipaws=getkey()

            call vexit()

            end program demo_clear
