          program demo_expandviewport
          use M_draw
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          implicit none
          integer :: ipaws

          !! set up graphics area
          call prefsize(1000,200)
          call vinit(' ') ! start graphics using device $M_DRAW_DEVICE

          !! draw box that fills default world coordinate window
          call color(D_RED)
          call polyfill(.true.)
          call rect(-1.0, -1.0, 1.0, 1.0)
          call color(D_GREEN)
          call circle(0.0,0.0,1.0)

          call expandviewport()
          !! the meaning of viewport numbers has now changed, but the
          !! viewport itself has not. Now <-1,-1> <1,1> defines the
          !! entire display area, where before it defined the largest square
          !! that would fit on the display
          call viewport(-1.0,1.0,-1.0,1.0)
          !! draw box that fills default world coordinate window again

          !! instead of a square and circle, the mapping now
          !! produces an ellipse and rectangle unless this
          !! device has a square display
          call polyhatch(.true.)
          call hatchpitch(0.1)
          call hatchang(30.0)
          call linewidth(20)
          call color(D_CYAN)
          call rect(-1.0, -1.0, 1.0, 1.0)
          call color(D_YELLOW)
          call hatchang(120.0)
          call circle(0.0,0.0,1.0)

          !! border
          call linewidth(100)
          call color(D_BLUE)
          call move2(-1.0, -1.0)
          call draw2(-1.0, 1.0)
          call draw2(1.0, 1.0)
          call draw2(1.0, -1.0)
          call draw2(-1.0, -1.0)

          call vflush(); ipaws=getkey() !! pause
          call vexit()                  !! wrap up graphics

          end program demo_expandviewport
