      program demo_atomic_fetch_or
      use iso_fortran_env
      implicit none
      integer(atomic_int_kind) :: atom[*], old
         call atomic_fetch_or(atom[1], int(b'10100011101'), old)
      end program demo_atomic_fetch_or
