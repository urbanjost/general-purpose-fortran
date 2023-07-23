        program demo_scale
        use M_draw
        implicit none
        real :: size, x, y
        integer :: idum
        ! set up display
           call prefsize(300, 300)
           call prefposition(200, 10)
           call vinit('X11')
           SIZE = 1.2
           X = -0.75
           Y = 0.75
           call color(3)
           call clear()
           call color(2)
           call ortho2(-SIZE, SIZE, -SIZE, SIZE)
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
        ! draw object, scaling coordinate system between instantiations
           call pushmatrix()
             call scale(1.1, 2.0, 0.0)
             call callobj(1)
             ! scaling accumulates
             call scale(0.5, 0.5, 0.0)
             call callobj(1)
             ! circles appear as ellipses in this coordinate system
             call circle(0.0, 0.0, X/3.0)
           ! return back to saved coordinate system
           call popmatrix()
           ! now a circle is a circle again
           call color(5)
           call circle(0.0, 0.0, X/3.0)
           idum = getkey()
           call vexit()
        end program demo_scale
