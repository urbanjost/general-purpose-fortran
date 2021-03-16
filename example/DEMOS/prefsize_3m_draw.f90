            program demo_prefsize
            use M_draw, only: prefsize, vinit, ortho2, clear, getkey
            use M_draw, only: move2, draw2, vexit, color
            use M_draw,    only  : D_BLACK,   D_WHITE
            use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
            use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
            implicit none
            integer :: ipaws
               ! make first file with one size
               call prefsize(60*2,40*2)
               call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
               call picture()
               ipaws=getkey()
               call vexit()

               ! make second file with another size
               call prefsize(60*3,40*3)
               call vinit(' ')
               call picture()
               ipaws=getkey()
               call vexit()
            contains
            subroutine picture
               call ortho2(-300.0,300.0,-200.0,200.0)
               call color(D_BLACK)
               call clear()
               call color(D_RED)
               call move2(-300.0,-200.0)
               call draw2(300.0,200.0)
               call move2(300.0,-200.0)
               call draw2(-300.0,200.0)
            end subroutine picture
            end program demo_prefsize
