           program demo_kind
           implicit none
             integer,parameter :: kc = kind(' ')
             integer,parameter :: kl = kind(.true.)

           print *, "The default character kind is ", kc
             print *, "The default logical kind is ", kl
           end program demo_kind
