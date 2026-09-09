     program demo_c2f
     use M_units, only : c2f
     implicit none
        write(*,*)'With REAL array input    ', c2f([ -40.0, 0.0, 100.0 ])
        write(*,*)'With INTEGER array input ', c2f([ -40,   0,   100   ])
        write(*,*)'With DOUBLEPRECISION     ', c2f(-40.0d0),c2f(0.0d0),c2f(100.0d0)
     end program demo_c2f
