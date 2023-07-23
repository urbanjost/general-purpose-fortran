      program demo_floor
      implicit none
      real :: x = 63.29
      real :: y = -63.59
          print *, x, floor(x)
          print *, y, floor(y)
         ! elemental
         print *,floor([ &
         &  -2.7,  -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, &
         &  0.0,   &
         &  +0.5,  +1.0, +1.5, +2.0, +2.2, +2.5, +2.7  ])

         ! note even a small deviation from the whole number changes the result
         print *,      [2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)]
         print *,floor([2.0,2.0-epsilon(0.0),2.0-2*epsilon(0.0)])

         ! A=Nan, Infinity or  <huge(0_KIND)-1 < A > huge(0_KIND) is undefined
      end program demo_floor
