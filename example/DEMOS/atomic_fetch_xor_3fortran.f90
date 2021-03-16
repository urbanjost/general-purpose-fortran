          program demo_atomic_fetch_xor
          use iso_fortran_env
          implicit none
          integer(atomic_int_kind) :: atom[*], old
             call atomic_fetch_xor (atom[1], int(b'10100011101'), old)
          end program demo_atomic_fetch_xor
