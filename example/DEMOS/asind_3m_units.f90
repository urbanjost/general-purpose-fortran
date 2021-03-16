          program demo_asind
          use M_units, only :  asind
          implicit none
             write(*, *)asind([ 0.0, 0.258819044, 0.5, 0.707106829, 0.866025448,  &
                             & 0.965925813, 1.0, -8.74227766E-08, -1.0 ])
          end program demo_asind
