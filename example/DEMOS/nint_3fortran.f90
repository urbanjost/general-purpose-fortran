           program demo_nint
           implicit none
           integer,parameter :: dp=kind(0.0d0)
             real ::  x4
             real(kind=dp) :: x8
             x4 = 1.234E0
             x8 = 4.321_dp
             print *, nint(x4), idnint(x8)
           end program demo_nint
