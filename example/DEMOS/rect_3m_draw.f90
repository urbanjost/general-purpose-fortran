          program demo_rect
          use M_draw
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          implicit none
          integer :: ipaws
          real    :: b=0.2

          !! set up graphics area
          call prefsize(1000,200)
          call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
          call page(-25.0-b, 25.0+b, -5.0-b, 5.0+b)

          call linewidth(150)
          call color(D_RED)
          call rect(-24.0, -4.0, -12.0, 4.0)
          call polyfill(.true.)
          call color(D_GREEN)
          call rect(-10.0, -4.0, -2.0, 4.0)
          call polyhatch(.true.)
          call hatchpitch(0.4)
          call hatchang(30.0)
          call linewidth(20)
          call color(D_BLUE)
          call rect(0.0, -4.0, 20.0, 3.0)
          call linewidth(200)
          call color(D_BLUE)
          call move2(-25.0, -5.0)
          call draw2(-25.0, 5.0)
          call draw2(25.0, 5.0)
          call draw2(25.0, -5.0)
          call draw2(-25.0, -5.0)

          !! pause
          call vflush()
          ipaws=getkey()

          !! wrap up graphics
          call vexit()

          end program demo_rect
