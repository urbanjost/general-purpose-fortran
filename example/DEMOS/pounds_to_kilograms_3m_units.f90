     program demo_pounds_to_kilograms
     use M_units, only : pounds_to_kilograms
     implicit none
        write(*,*)'REAL            ', pounds_to_kilograms(1.0)
        write(*,*)'INTEGER array   ', pounds_to_kilograms([ 0, 1, 100, 200 ])
        write(*,*)'DOUBLEPRECISION ', pounds_to_kilograms(1.0d0)
     end program demo_pounds_to_kilograms
