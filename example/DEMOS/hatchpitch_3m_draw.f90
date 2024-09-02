        program demo_hatchpitch
        use M_draw
        implicit none
        real :: b
        integer :: idum

           call prefsize(1000, 200)
           call vinit(' ')
           b = 0.1
           call page(-25.0 - b, 25.0 + b, -5.0 - b, 5.0 + b)
           call color(0)
           call clear()
           call textsize(0.5, 0.6)
           call font('futura.l')
           call leftjustify()
           call circleprecision(3)
           ! draw circles with hatching
           call linewidth(150)
           call polyhatch(.true.)
           call hatchang(30.0)

           call try(pitch=1.0/3.0, col=7, X=-20.0, label='1/3')
           call try(pitch=1.0/2.0, col=2, X=-10.0, label='1/2')
           call try(pitch=1.0,     col=6, X= -0.0, label='1')
           call try(pitch=2.0,     col=5, X= 10.0, label='2')
           call try(pitch=3.0,     col=4, X= 20.0, label='3')
           idum = getkey()
           call vexit()
        contains
           subroutine try(pitch, col, x, label)
              real, intent(in) :: pitch
              integer, intent(in) :: col
              real, intent(in) :: x
              character(len=*), intent(in) :: label
              call hatchpitch(pitch)
              call color(col)
              call circle(X, 0.0, 5.0)
              call move2(X - 4.9,-4.9)
              call color(7)
              call drawstr(label)
           end subroutine try

        end program demo_hatchpitch
