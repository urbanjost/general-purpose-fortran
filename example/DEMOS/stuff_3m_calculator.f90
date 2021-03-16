          program demo_stuff
          use M_calculator, only : stuff, dnum0
          implicit none
          doubleprecision :: value
          call stuff('A',10.0)
          call stuff('PI',3.141592653589793238462643383279502884197169399375105820974944592307d0)
          value=dnum0('A*PI')
          write(*,*)value
          end program demo_stuff
