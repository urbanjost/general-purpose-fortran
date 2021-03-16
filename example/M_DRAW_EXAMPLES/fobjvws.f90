!@(#) Demonstrate just how much you can put in an object
!(LICENSE:PD)
!
        program fobjviews

      use M_draw

        integer BLACK, RED, GREEN, YELLOW, BLUE, CYAN, MAGENTA
        parameter(BLACK = 0, RED = 1, GREEN = 2, YELLOW = 3, BLUE = 4, CYAN = 5, MAGENTA = 6)
        
        integer CUBE, TOPLEFT, TOPRIGHT, BOTTOMLEFT, BOTTOMRIGHT
        parameter(CUBE = 1, TOPLEFT = 2, TOPRIGHT = 3, BOTTOMLEFT = 4, BOTTOMRIGHT = 5)

        character(len=20) :: device

        print*,'Enter device name:'
        read(*,'(A)')device
        call vinit(device)
        call pushviewport

        call textsize(0.5, 0.9)
        call font('futura.m')

        call color(BLACK)
        call clear
!
! make an object that represents the cube
!
        call makecube
!
! set up an object which draws in the top left of the screen.
!
        call makeobj(TOPLEFT)
                call viewport(-1.0, 0.0, 0.0, 1.0)
                call ortho2(-5.0, 5.0, -5.0, 5.0)

                call color(RED)

                call rect(-5.0, -5.0, 5.0, 5.0)

                call perspective(40.0, 1.0, 0.1, 1000.0)
                call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
!
!          Call an object within another object
!
                call callobj(CUBE)

                call color(GREEN)

                call move2(-4.5, -4.5)
                call drawstr('perspective/lookat')
        call closeobj

!
! now set up one which draws in the top right of the screen
!
        call makeobj(TOPRIGHT)
                call viewport(0.0, 1.0, 0.0, 1.0)
                call ortho2(-5.0, 5.0, -5.0, 5.0)

                call color(GREEN)

                call rect(-5.0, -5.0, 5.0, 5.0)

                call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
                call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)

                call callobj(CUBE)

                call color(RED)

                call move2(-4.5, -4.5)
                call drawstr('window/lookat')
        call closeobj

!
! try the bottom left
!
        call makeobj(BOTTOMLEFT)
                call viewport(-1.0, 0.0, -1.0, 0.0)
                call ortho2(-5.0, 5.0, -5.0, 5.0)

                call color(MAGENTA)

                call rect(-5.0, -5.0, 5.0, 5.0)

                call perspective(40.0, 1.0, 0.1, 1000.0)
                call polarview(15.0, 30.0, 30.0, 30.0)

                call callobj(CUBE)

                call color(YELLOW)

                call move2(-4.5, -4.5)
                call drawstr('perspective/polarview')
        call closeobj

!
! and the bottom right
!
        call makeobj(BOTTOMRIGHT)
                call viewport(0.0, 1.0, -1.0, 0.0)
                call ortho2(-5.0, 5.0, -5.0, 5.0)

                call color(CYAN)

                call rect(-5.0, -5.0, 5.0, 5.0)

                call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
                call polarview(8.0, -18.0, -3.0, 18.0)

                call callobj(CUBE)

                call color(BLUE)

                call move2(-4.5, -4.5)
                call drawstr('window/polarview')
        call closeobj

!
! now draw them
!
        call callobj(TOPLEFT)
        call callobj(TOPRIGHT)
        call callobj(BOTTOMLEFT)
        call callobj(BOTTOMRIGHT)

        idum=getkey()

        call vexit

        end

!
! makecube
!
! set up a cube
!
        subroutine makecube

      use M_draw

        integer CUBE
        parameter (CUBE = 1)

        call makeobj(CUBE)

!
! The border around the cube
!
            call rect(-5.0, -5.0, 10.0, 10.0)

!
! Make the cube from 4 squares
!
            call pushmatrix
                call side
                call rotate(90.0, 'x')
                call side
                call rotate(90.0, 'x')
                call side
                call rotate(90.0, 'x')
                call side
            call popmatrix

        call closeobj

        end

!
! side
!
! define a face for the cube
!
        subroutine side

      use M_draw

        call pushmatrix
            call translate(0.0, 0.0, 1.0)
            call rect(-1.0, -1.0, 1.0, 1.0)
        call popmatrix

        end
