          program demo_meters_to_feet
          use M_units, only : meters_to_feet
          implicit none
             write(*,*)'With REAL array input    ', meters_to_feet([ -1.0, 0.0, 1.0 ])
             write(*,*)'With INTEGER array input ', meters_to_feet([ -1,   0,   1   ])
             write(*,*)'With DOUBLEPRECISION     ', meters_to_feet(-1.0d0),meters_to_feet(0.0d0),meters_to_feet(1.0d0)
          end program demo_meters_to_feet
