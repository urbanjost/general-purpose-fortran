      program demo_atomic_or
        use iso_fortran_env
        implicit none
        integer(atomic_int_kind) :: flags[*]
        integer :: stat, me

        if (this_image() == 1) flags = int(b'1000', atomic_int_kind)
        sync all

        me = this_image()
        call atomic_or(flags[1], int(b'0011', atomic_int_kind), stat)

        if (stat /= 0) print *, "Image", me, ": Failed with STAT =", stat
        sync all

        if (this_image() == 1) print *, "Final flags:", flags
      end program demo_atomic_or
