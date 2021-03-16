          program demo_unexpandviewport
          use M_draw
          use M_draw,    only  : D_BLACK,   D_WHITE
          use M_draw,    only  : D_RED,     D_GREEN,    D_BLUE
          use M_draw,    only  : D_YELLOW,  D_MAGENTA,  D_CYAN
          implicit none
          integer :: ipaws

          !! set up graphics area
          call prefsize(1000,200)
          call vinit(' ') ! start graphics using device $M_DRAW_DEVICE

          !! draw circle that fills default world coordinate window
          call polyfill(.true.)
          call color(D_GREEN)
          call circle(0.0,0.0,1.0)
          ipaws=getkey() !! pause

          !! set new scales for viewport so <-1,-1> and <1,1> are at
          !! corners of display instead of corners of largest square
          !! that can fit on display
          call expandviewport()
          call viewport(-1.0,1.0,-1.0,1.0)

          !! draw circle that fills default world coordinate window again
          !! instead of a circle, the mapping now produces an ellipse unless
          !! this device has a square display
          call polyhatch(.true.)
          call hatchpitch(0.1)
          call hatchang(30.0)
          call linewidth(40)
          call color(D_CYAN)
          call circle(0.0,0.0,1.0)
          ipaws=getkey() !! pause

          !! set new scales for viewport so <-1,-1> and <1,1> are at
          !! corners of largest square that fits on display
          call unexpandviewport()
          !! actually change to the new viewport
          call viewport(-1.0,1.0,-1.0,1.0)

          !! now the same circle should draw where the original one did
          call color(D_BLACK)
          call hatchang(120.0)
          call linewidth(40)
          call circle(0.0,0.0,1.0)
          ipaws=getkey() !! pause

          call vexit()                  !! wrap up graphics

          end program demo_unexpandviewport
