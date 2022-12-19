      program demo_ceiling
      implicit none
      ! just a convenient format for a list of integers
      character(len=*),parameter :: ints='(*("   > ",5(i0:,",",1x),/))'
      real :: x
      real :: y
        ! basic usage
         x = 63.29
         y = -63.59
         print ints, ceiling(x)
         print ints, ceiling(y)
         ! note the result was the next integer larger to the right

        ! real values equal to whole numbers
         x = 63.0
         y = -63.0
         print ints, ceiling(x)
         print ints, ceiling(y)

        ! elemental (so an array argument is allowed)
         print ints , &
         & ceiling([ &
         &  -2.7,  -2.5, -2.2, -2.0, -1.5, &
         &  -1.0,  -0.5,  0.0, +0.5, +1.0, &
         &  +1.5,  +2.0, +2.2, +2.5, +2.7  ])

      end program demo_ceiling
