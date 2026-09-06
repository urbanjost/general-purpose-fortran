      program demo_atomic_cas_example
      use iso_fortran_env
      implicit none
      integer(atomic_int_kind) :: lock[*]
      integer(atomic_int_kind) :: old
      integer :: stat, me

        if (this_image() == 1) lock = 0
        sync all

        me = this_image()
        call atomic_cas(lock[1], old, 0, me, stat)

        if (stat /= 0) then
          print *, "Image", me, ": Failed with STAT =", stat
        else
          print *, "Image", me, ": Old =", old, ", New =", lock[1]
        end if
        sync all

        if (this_image() == 1) print *, "Final lock:", lock
      end program demo_atomic_cas_example
