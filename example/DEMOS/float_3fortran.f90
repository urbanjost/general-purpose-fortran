           program demo_float
           implicit none
               integer :: i = 1
               if (float(i) /= 1.) stop ' FLOAT FAILED'
           end program demo_float
