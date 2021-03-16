          program demo_atomic_and
          use iso_fortran_env
          implicit none
          integer(atomic_int_kind) :: atom[*]
             call atomic_and(atom[1], int(b'10100011101'))
          end program demo_atomic_and
