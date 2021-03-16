          program demo_isobj
          use M_draw
             call prefsize(300, 300)
             call prefposition(100, 100)
             call vinit(' ')   ! set up device
             call ortho2(-5.0,5.0,-5.0,5.0)
             call color(D_WHITE)  ! set current color
             call clear()   ! clear screen to current color

             call makeobj(3)  ! create a simple object
                call polyfill(.true.)
                call color(D_GREEN)
                call circle(0.0,0.0,4.0)
             call closeobj()

             if(isobj(3))then
                write(*,*)' 3 is an object (CORRECT)'
                call callobj(3)
             else
                write(*,*)' 3 is not an object (ERROR)'
             endif

             if(isobj(4))then
                write(*,*)' 4 is an object (ERROR)'
             else
                write(*,*)' 4 is not an object (CORRECT)'
             endif

             call callobj(4) ! note: calling a non-existent object is a no-op

             idum=getkey()! wait for some input
             call vexit()!  set the screen back to its original state

          end program demo_isobj
