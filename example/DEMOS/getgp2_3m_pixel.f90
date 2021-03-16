            program demo_getgp2
            use M_pixel
            implicit none
            real :: X,Y
            call prefsize(20,20)
            call vinit()
            call ortho2(-100.0,100.0,-100.0,100.0)
            call move2(0.0,0.0)
            call draw2(96.5,98.333)

            call getgp2(X,Y)
            write(*,*)'CURRENT POSITION (X,Y)=',X,Y

            call vexit()
            end program demo_getgp2
