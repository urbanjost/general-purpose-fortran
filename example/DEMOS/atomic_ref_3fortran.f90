          program demo_atomic_ref
          use iso_fortran_env
          implicit none
          logical(atomic_logical_kind) :: atom[*]
          logical :: val
             call atomic_ref( val, atom[1] )
             ! ...
             call atomic_ref( val, atom[1] )
             if (val) then
                print *, "Obtained"
             endif
          end program demo_atomic_ref
