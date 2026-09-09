        program demo_hatchang
        use M_draw
        implicit none
        real :: b
        integer :: idum
           call prefsize(1000,200)
           call vinit(' ')
           b = 0.4
           call page(-25.0 - b, 25.0 + b, -5.0 - b, 5.0 + b)
           call color(0)
           call clear()
           call textsize(0.6, 0.7)
           call font('futura.l')
           call centertext(.true.)
           call leftjustify()
           call linewidth(50)
           call polyhatch(.true.)
           call hatchpitch(1.0/2.0)
           ! draw circles with hatching
           call drawc(ang=  90.1, col=7, x=-20.0, label='90 degrees')
           call drawc(ang=  45.0, col=2, x=-10.0, label='45 degrees')
           call drawc(ang=   0.0, col=6, x=  0.0, label='0 degrees')
           call drawc(ang= -45.0, col=5, x= 10.0, label='-45 degrees')
           call drawc(ang= -90.0, col=4, x= 20.0, label='-90 degrees')

           call linewidth(130)
           call move2(0.0, 0.0)
           call draw2(-5.0, 0.0)
           call move2(-5.0, 0.0)
           call draw2(-4.4, 0.3)
           call move2(-5.0, 0.0)
           call draw2(-4.4, - 0.3)
           call rightjustify()
           call linewidth(60)
           call move2(-5.0,0.0)
           call drawstr('0 degrees')
           idum = getkey()
           call vexit()
        contains
           subroutine drawc(ang,col,x,label)
           real,intent(in) :: ang, x
           integer,intent(in) :: col
           character(len=*),intent(in) :: label
           real :: y
           y=0.0
           call linewidth(90)
           call hatchang(ang)
           call color(col)
           call circle(X, Y, 5.0)
           y = -4.9
           call move2(X - 4.9, Y)
           call color(7)
           call linewidth(60)
           call drawstr(label)
           end subroutine drawc

        end program demo_hatchang
