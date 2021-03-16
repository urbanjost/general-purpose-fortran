           program demo_dot_prod
           implicit none
               integer, dimension(3) :: a, b
               a = [ 1, 2, 3 ]
               b = [ 4, 5, 6 ]
               print '(3i3)', a
               print *
               print '(3i3)', b
               print *
               print *, dot_product(a,b)
           end program demo_dot_prod
