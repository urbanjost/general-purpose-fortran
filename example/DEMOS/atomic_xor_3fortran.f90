      program demo_atomic_xor
        use iso_fortran_env
        implicit none
        integer(atomic_int_kind) :: flags[*]
        integer :: stat, me

        if (this_image() == 1) flags = int(b'1100', atomic_int_kind)
        sync all

        me = this_image()
        call atomic_xor(flags[1], int(b'1010', atomic_int_kind), stat)

        if (stat /= 0) print *, "Image", me, ": Failed with STAT =", stat
        sync all

        if (this_image() == 1) print *, "Final flags:", flags
      end program demo_atomic_xor
