      program demo_atomic_fetch_add
      use iso_fortran_env
      implicit none
      integer(atomic_int_kind) :: atom[*], old
         call atomic_add(atom[1], this_image(), old)
      end program demo_atomic_fetch_add
