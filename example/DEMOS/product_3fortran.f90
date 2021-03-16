           program demo_product
           implicit none
             integer :: x(5) = [ 1, 2, 3, 4 ,5 ]
             print *, product(x)                    ! all elements, product = 120
             print *, product(x, mask=mod(x, 2)==1) ! odd elements, product = 15
           end program demo_product
