           program demo_sign
           implicit none
             print *, sign( -12,  1 )
             print *, sign( -12,  0 )
             print *, sign( -12, -1 )

             print *, sign( -12.0,  1.0 )
             print *, sign( -12.0,  0.0 )
             print *, sign( -12.0, -1.0 )
           end program demo_sign
