          program demo_perspective
          !
          !     Shows various combinations of viewing and projection transformations
          use M_draw
          !
          character(len=50) :: device
          integer :: ios
          !
             print*,'Enter output device:'
             read(*,'(a)',iostat=ios)device
             if(ios.ne.0)device=' '
          !
             call vinit(device)
          !
             call color(D_BLACK)
             call clear()
          !
          ! we want to draw just within the boundaries of the screen
          !
             call viewport(-0.9, 0.9, -0.9, 0.9)
          !
          ! set the world size
          !
             call ortho2(-5.0, 5.0, -5.0, 5.0)
          !
          ! draw a boundary frame
          !
             call color(D_RED)
             call rect(-5.0, -5.0, 5.0, 5.0)
          !
          ! set up a perspective projection with a field of view of
          ! 40.0 degrees, aspect ratio of 1.0, near clipping plane 0.1,
          ! and the far clipping plane at 1000.0.
          !
             call perspective(40.0, 1.0, 0.1, 1000.0)
          !
          ! we want the drawing to be done with our eye point at (5.0, 8.0, 5.0)
          ! looking towards (0.0, 0.0, 0.0). The last parameter gives a twist
          ! in degrees around the line of sight, in this case zero.
          !
             call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
          !
             call drawtetra()
          !
          ! set the text size
          !
             call textsize(0.6, 0.9)
          !
             call move2(-4.5, -4.5)
             call drawstr('perspective/lookat')
          !
             idum=getkey()
          !
          ! window can also be used to give a perspective projection. Its
          ! arguments are 6 clipping planes, left, right, bottom, top, near,
          ! and far.
          !
             call window(-5.0, 5.0, -5.0, 5.0, -5.0, 5.0)
          !
          ! as window replaces the current transformation matrix we must
          ! specify our viewpoint again.
          !
             call lookat(5.0, 8.0, 5.0, 0.0, 0.0, 0.0, 0.0)
          !
             call color(D_BLACK)
             call clear()
          !
             call color(D_GREEN)
             call rect(-5.0, -5.0, 5.0, 5.0)
          !
             call drawtetra()
          !
             call textsize(0.6, 0.9)
             call move2(-4.5,-4.5)
             call drawstr('window/lookat')
          !
             idum=getkey()
          !
          ! set up our original perspective projection again.
          !
             call perspective(40.0, 1.0, 0.1, 1000.0)
          !
          ! polarview also specifies our viewpoint, but, unlike lookat, in polar
          ! coordinates. Its arguments are the distance from the world origin, an
          ! azimuthal angle in the x-y plane measured from the y axis, an
          ! incidence angle in the y-z plane measured from the z axis, and a
          ! twist around the line of sight.
          !
             call polarview(15.0, 30.0, 30.0, 30.0)
          !
             call color(D_BLACK)
             call clear()
          !
             call color(D_MAGENTA)
             call rect(-5.0, -5.0, 5.0, 5.0)
          !
             call drawtetra()
          !
             call move2(-4.5,-4.5)
             call textsize(0.6, 0.9)
             call drawstr('perspective/polarview')
          !
             idum=getkey()
          !
          ! once more with window for comparison
          !
             call window(-4.0, 4.0, -4.0, 4.0, -4.0, 4.0)
             call polarview(6.0, 20.0, -30.0, 70.0)
          !
             call color(D_BLACK)
             call clear()
          !
             call color(D_YELLOW)
             call rect(-5.0, -5.0, 5.0, 5.0)
          !
             call drawtetra()
          !
             call move2(-4.5,-4.5)
             call textsize(0.6, 0.9)
             call drawstr('window/polarview')
          !
             idum=getkey()
          !
             call vexit()
          !
          contains
          !
          subroutine drawtetra()
          !
          ! generate a tetrahedron as a series of move draws
          !
             call move(-0.5,  0.866, -0.5)
             call draw(-0.5, -0.866, -0.5)
             call draw( 1.0,  0.0,   -0.5)
             call draw(-0.5,  0.866, -0.5)
             call draw( 0.0,  0.0,    1.5)
             call draw(-0.5, -0.866, -0.5)
             call move( 1.0,  0.0,   -0.5)
             call draw( 0.0,  0.0,    1.5)
          !
          !    Label the vertices.
          !
             call color(D_WHITE)
             call textsize(0.3, 0.5)
             call move(-0.5,  0.866, -0.5)
             call drawchar('a')
             call move(-0.5, -0.866, -0.5)
             call drawchar('b')
             call move( 1.0,  0.0,   -0.5)
             call drawchar('c')
             call move( 0.0,  0.0,    1.5)
             call drawchar('d')
          !
          end subroutine drawtetra
          !
          end program demo_perspective
