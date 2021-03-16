          program demo_c_sizeof
          use iso_c_binding
          implicit none
          real(c_float) :: r, s(5)
             print *, (c_sizeof(s)/c_sizeof(r) == 5)
          end program demo_c_sizeof
