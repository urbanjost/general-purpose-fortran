        program demo_rotate
        use M_draw
        implicit none
        real :: x, y, size
        integer :: idum

        ! set up display
           call prefsize(300, 300)
           call prefposition(200, 10)
           call vinit('X11')

           SIZE = 1.2
           X = -0.75
           Y = 0.75
           call ortho2(-SIZE, SIZE, -SIZE, SIZE)
           call color(3)
           call clear()
        ! create an object to repeatedly draw
           call makeobj(1)
             call polyfill(.true.)
             call color(1)
             call rect(0.0, 0.0, X, Y)
             call polyfill(.false.)
             call linewidth(200)
             call color(2)
             call rect(0.0, 0.0, X, Y)
           call closeobj()
        ! draw object, rotating coordinate system between instantiations
           call callobj(1)
           call rotate(45.0, 'z')
           call callobj(1)
           call rotate(45.0, 'z')
           call callobj(1)
           call circle(0.0, 0.0, X/3)
           idum = getkey()
           call vexit()

        end program demo_rotate
