      program demo_atomic_add
      use iso_fortran_env
      implicit none
      integer(atomic_int_kind) :: atom[*]
         call atomic_add (atom[1], this_image())
      end program demo_atomic_add
