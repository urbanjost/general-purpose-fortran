            program demo_rdraw2
            use M_draw, only: vinit, prefsize, ortho2,linewidth,getkey
            use M_draw, only: clear, move2, rdraw2, vexit,color
            use M_draw,    only  : D_BLACK,   D_WHITE
            use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
            use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
            integer :: ipaws

            call prefsize(200,200)
            call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
            call ortho2(-55.0, 55.0, -55.0, 55.0)
            call linewidth(400)
            call color(D_WHITE)
            call clear()

            call color(D_RED)
            call move2(-50.0,0.0)
            call square(50.0)

            call linewidth(200)
            call color(D_GREEN)
            call move2(  0.0,-50.0)
            call square(50.0)

            ipaws=getkey()
            call vexit()

            contains

            subroutine square(side)
            call rdraw2( side,   0.0)
            call rdraw2(  0.0,  side)
            call rdraw2(-side,   0.0)
            call rdraw2(  0.0, -side)
            end subroutine square

            end program demo_rdraw2
