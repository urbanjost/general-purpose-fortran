          program demo_print
          use M_draw
          implicit none
          real :: angle
          integer :: idum
          character(len=*),parameter :: space='       '
             ! set window size
             call prefsize(700,700)
             call prefposition( 0, 0)
             call vinit('X11')
             call page(-5.0,5.0,-5.0,5.0)
             call textsize(0.3,0.3)
             call color(D_BLUE)
             angle=0.0
             call turn()
             call print(space,'a logical such as ',.true.)
             call turn()
             call print(space,'a real value',3.1416)
             call turn()
             call print(space,'double precision',7890.123456d0)
             call turn()
             call print(space,'integer ',1234)
             call turn()
             call print(space,'lots of stuff',1234,.false.,cmplx(20.0,30.0))
             idum=getkey()
             call vexit()
          contains
             subroutine turn()
                call move2(-4.0,-3.5)
                call textang(angle)
                angle=angle+15.0
             end subroutine turn
          end program demo_print
