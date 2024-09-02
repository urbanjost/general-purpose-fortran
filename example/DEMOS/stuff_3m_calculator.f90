     program demo_stuff
     use M_calculator, only : stuff, dnum0
     implicit none
     doubleprecision :: value
        call stuff('A',10.0)
        call stuff('PI',3.1415926535897932384626433832795)
        value=dnum0('A*PI')
        write(*,*)value
     end program demo_stuff
