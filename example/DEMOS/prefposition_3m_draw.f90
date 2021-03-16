            program demo_prefposition
            use M_draw, only    : prefsize, vinit, ortho2, clear, getkey, prefposition
            use M_draw, only    : move2, draw2, vexit, color
            use M_draw,    only  : D_BLACK,   D_WHITE
            use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
            use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
            implicit none
            integer :: ipaws

            call prefsize(60,40)
            call prefposition(100,100)

            call vinit(' ')         ! start graphics using device $M_DRAW_DEVICE
            call ortho2(-300.0,300.0,-200.0,200.0)
            call color(D_BLACK)
            call clear()
            call color(D_RED)
            call move2(-300.0,-200.0)
            call draw2(300.0,200.0)
            call move2(300.0,-200.0)
            call draw2(-300.0,200.0)
            ipaws=getkey()
            call vexit()

            end program demo_prefposition
