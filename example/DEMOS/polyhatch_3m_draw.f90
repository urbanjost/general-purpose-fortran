          program demo_polyhatch
          use M_draw
          use M_drawplus, only : spirograph
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          real :: N=11
          call prefsize(600*10/6,200*10/6)
          call vinit(' ')
          call page( -15.0, 15.0, -5.0, 5.0)
          call linewidth(100)
          call color(D_BLACK)
          call clear()
          call color(D_RED)
          call spirograph(-10.0, 0.0, N, 1.0, N, 5.0, 1000, 0.0, 0.0, 0)
          call polyhatch(.true.) ! turn on polygon hatching
          call hatchang(45.0)
          call hatchpitch(0.3)
          call color(D_GREEN)
          call spirograph(10.0, 0.0, N, 1.0, N, 5.0, 1000, 0.0, 0.0, 2)
          call vflush()
          key=getkey()
          call vexit()
          end program demo_polyhatch
