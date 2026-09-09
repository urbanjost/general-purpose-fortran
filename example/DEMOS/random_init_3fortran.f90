          program demo_random_init
          implicit none
          real x(3), y(3)
             call random_init(.true., .true.)
             call random_number(x)
             call random_init(.true., .true.)
             call random_number(y)
             ! x and y should be the same sequence
             if ( any(x /= y) ) stop "x(:) and y(:) are not all equal"
             write(*,*)x
             write(*,*)y
          end program demo_random_init
