          program demo_quadratic
          use M_math, only : quadratic
          implicit none
          ! Calculate and print the roots of a quadratic formula
          ! even if they are complex
          real    :: a, b, c ! coefficients
          complex :: z1, z2  ! roots
          real    :: discriminant
             a = 4.0
             b = 8.0
             c = 21.0
             call quadratic(a,b,c,z1,z2,discriminant) !  Calculate the roots
             if (abs(discriminant) < 0) then
                write(*,*) "the roots are real and equal:"
             else if (discriminant > 0) then
                write(*,*) "the roots are real:"
             else
                write(*,*) "the roots are complex:"
             end if
          !  Print the roots
             print *, "The roots(ie. x-intercepts)  are:"
             print *, "z1 =", z1
             print *, "z2 =", z2
          end program demo_quadratic
