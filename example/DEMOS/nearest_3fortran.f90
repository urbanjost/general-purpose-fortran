      program demo_nearest
      implicit none

         real :: x, y
         x = nearest(42.0, 1.0)
         y = nearest(42.0, -1.0)
         write (*,"(3(g20.15))") x, y, x - y

      !  write (*,"(3(g20.15))") &
      !   nearest(tiny(0.0),1.0), &
      !   nearest(tiny(0.0),-1.0), &
      !   nearest(tiny(0.0),1.0) -nearest(tiny(0.0),-1.0)

      !  write (*,"(3(g20.15))") &
      !   nearest(huge(0.0),1.0), &
      !   nearest(huge(0.0),-1.0), &
      !   nearest(huge(0.0),1.0)- nearest(huge(0.0),-1.0)

      end program demo_nearest
