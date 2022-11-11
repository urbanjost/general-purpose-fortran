        program demo_parity
        implicit none
        logical :: x(2) = [ .true., .false. ]
           print *, parity(x)
        end program demo_parity
