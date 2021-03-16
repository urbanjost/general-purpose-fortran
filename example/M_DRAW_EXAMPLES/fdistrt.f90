!
!@(#) using non-square viewports, the associated distortion -- and how to fix it
!(LICENSE:PD)
!
        program fdistrt

      use M_draw

        integer BLACK, RED, GREEN, YELLOW, BLUE, MAGENTA, CYAN, WHITE
        parameter(BLACK = 0)
        parameter(RED = 1)
        parameter(GREEN = 2)
        parameter(YELLOW = 3)
        parameter(BLUE = 4)
        parameter(MAGENTA = 5)
        parameter(CYAN = 6)
        parameter(WHITE = 7)
        character(len=50)  :: device
        character(len=120) :: buf
        real    xfact, yfact

        print*,'Enter output device:'
        read(*,'(a)') device

        call vinit(device)

        call color(BLACK)
        call clear

! 
! Make the viewport the same size as the screen/window.
!
        call getfactors(xfact, yfact)
        call viewport(-1.0, xfact, -1.0, yfact)

!
! Draw a square. (Looks like a rectangle, if the viewport
! wasn't "accidentally" square)
!
        call color(1)
        call rect(-0.5, -0.5, 0.5, 0.5)

! 
! Tell them what it is.
!
        call move2(-1.0, 0.9)
        write(buf,'(''Distorted square (viewport(-1, '', F7.3, '', -1, '', F7.3, ''))'')') xfact, yfact
        call drawstr(buf)

        idum=getkey()

!
! Fix up the distortion (The actual formula to fix
! the distortion is (viewport.xmax * (1 + xfact) / 2.0),
! and similar for the y axis.
!
        call ortho2(-1.0, xfact, -1.0, yfact)

!
! Draw another square (Really is square this time)
!
        call color(3)
        call rect(-0.5, -0.5, 0.5, 0.5)

! 
! Tell them what it is.
!
        call move2(-1.0, -0.9)
        write(buf,'(''Fixed up square with ortho2(-1, '', F7.3, '', -1, '', F7.3, '')'')') xfact, yfact
        call drawstr(buf)

        idum=getkey()

!
! Do it with world coords going from 0 - 5, 0 - 5.
! Reset square viewport.
!

        call color(0)
        call clear

        call viewport(-1.0, 1.0, -1.0, 1.0)
        call ortho2(0.0, 5.0, 0.0, 5.0)
        call textsize(0.1, 0.1)

!
! Square from 1 to 3. (Really is square)
!

        call color(2)
        call rect(1.0, 1.0, 3.0, 3.0)

        call move2(0.0, 4.5)
        call drawstr('Square from 0 - 3, 0 - 3')

        idum=getkey()

!
! Distort it with a non-square viewport.
!
        call viewport(-1.0, xfact, -1.0, yfact)

        call color(4)
        call rect(1.0, 1.0, 3.0, 3.0)

        call move2(0.0, 0.5)
        call drawstr('Distorted square from 0 - 3, 0 - 3')

        idum=getkey()

! 
! Fix the distortion.
! 
        call ortho2(0.0, 5.0 * (1.0 + xfact) / 2.0, 0.0, 5.0 * (1.0 + yfact) / 2.0)
        
        call color(5)
        call rect(1.0, 1.0, 3.0, 3.0)

        call move2(0.0, 2.5)
        call drawstr('Fixed up  square from 0 - 3, 0 - 3')

        idum=getkey()

        call vexit

        end
