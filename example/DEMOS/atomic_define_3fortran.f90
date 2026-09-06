      program demo_atomic_define
        use iso_fortran_env
        implicit none
        integer(atomic_int_kind) :: counter[*]
        integer :: stat, me

        if (this_image() == 1) counter = 0
        sync all

        me = this_image()
        if (me == 2) call atomic_define(counter[1], 42, stat)

        if (stat /= 0) print *, "Image", me, ": Failed with STAT =", stat
        sync all

        if (this_image() == 1) print *, "Final counter:", counter
      end program demo_atomic_define
