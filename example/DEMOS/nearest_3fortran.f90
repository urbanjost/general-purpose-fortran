          program demo_nearest
          implicit none
            real :: x, y
            x = nearest(42.0, 1.0)
            y = nearest(42.0, -1.0)
            write (*,"(3(g20.15))") x, y, x - y
          end program demo_nearest
