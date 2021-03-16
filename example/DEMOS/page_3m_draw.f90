          program demo_page
          use M_draw
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          implicit none
          integer :: ipaws
          real,parameter :: radius=25.0
             call prefsize(600,600)
             call vinit(' ') ! start graphics using device $M_draw_DEVICE
             call page(-radius, radius, -radius, radius)
             call linewidth(200)
             call clear()
             call color(D_BLUE)
             call move2(-radius, -radius)
             call draw2(-radius, radius)
             call draw2(radius, radius)
             call draw2(radius, -radius)
             call draw2(-radius, -radius)
             call color(D_CYAN)
             call circle(0.0,0.0,radius)
             call vflush()
             ipaws=getkey()
             call vexit()
          end program demo_page
