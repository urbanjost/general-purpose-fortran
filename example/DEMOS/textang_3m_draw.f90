          program demo_textang
          use :: M_draw
          use :: M_units, only : cosd, sind

          !! set up drawing environment
          call prefsize(600,600)
          call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
          call ortho2(-100.0,100.0,-100.0,100.0)
          call textsize(7.0,7.0)
          call linewidth(20)
          call color(D_BLACK)
          call clear()

          do i=1,30
             !! draw radial lines
             call color(D_RED)
             call move2(0.0,0.0)
             call draw2(100.0*cosd(i*12),100.0*sind(i*12))
             !! draw rotated text
             call color(D_WHITE)
             call move2(30.0*cosd(i*12),30.0*sind(i*12))
             call textang(i*12.0)
             call drawstr('angled text')
          enddo

          ipaws=getkey()

          call vexit()

          end program demo_textang
