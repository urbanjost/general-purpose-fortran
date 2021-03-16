          program demo_d2r
          use M_units, only :  d2r
          implicit none
             write(*,*)'With REAL array input    ', d2r([0.0,45.0,90.0,135.0,180.0])
             write(*,*)'With INTEGER array input ', d2r([0,  45,  90,  135,  180  ])
             write(*,*)'With DOUBLEPRECISION     ', &
             & d2r(0.0d0),d2r(45.0d0),d2r(90.0d0),d2r(135.0d0),d2r(180.0d0)
          end program demo_d2r
