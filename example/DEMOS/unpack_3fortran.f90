           program demo_unpack
           implicit none
             integer :: vector(2)  = [1,1]
             logical :: mask(4)  = [ .true., .false., .false., .true. ]
             integer :: field(2,2) = 0, unity(2,2)

             ! result: unity matrix
             unity = unpack(vector, reshape(mask, [2,2]), field)
           end program demo_unpack
