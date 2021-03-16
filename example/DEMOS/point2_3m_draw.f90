          program demo_point2
          use :: M_draw
          implicit none
          integer :: i
          integer :: ipaws
          call prefsize(300,300)
          call vinit(' ') ! start graphics using device $M_DRAW_DEVICE
          call ortho2(0.0, 20.0, 0.0, 20.0)
          call color(D_MAGENTA)
          do i=1,20
             call linewidth(20*i)
             call point2(real(i),real(i))
          enddo
          ipaws=getkey()
          call vexit()
          end program demo_point2
