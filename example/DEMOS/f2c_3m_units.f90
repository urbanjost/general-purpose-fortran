          program demo_f2c
          use M_units, only :  f2c
          implicit none
             write(*,*)'With REAL array input    ', f2c([ -40.0,32.0, 212.0 ])
             write(*,*)'With INTEGER array input ', f2c([ -40,  32,   212   ])
             write(*,*)'With DOUBLEPRECISION     ', f2c(-40.0d0),f2c(32.0d0),f2c(212.0d0)
          end program demo_f2c
