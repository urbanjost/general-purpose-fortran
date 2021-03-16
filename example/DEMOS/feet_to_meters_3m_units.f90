          program demo_feet_to_meters
          use M_units, only : feet_to_meters
          implicit none
             write(*,*)'With REAL array input    ', &
                     & feet_to_meters([ -1.0, 0.0, 1.0 ,1.0/12.0])
             write(*,*)'With INTEGER array input ', &
                     & feet_to_meters([ -1,   0,   1   ])
             write(*,*)'With DOUBLEPRECISION     ', &
                     & feet_to_meters(-1.0d0), &
                     & feet_to_meters(0.0d0), &
                     & feet_to_meters(1.0d0)
          end program demo_feet_to_meters
