      program demo_atomic_fetch_xor

        use iso_fortran_env
        implicit none
        integer(atomic_int_kind) :: flags[*], old
        integer :: stat, me

        if (this_image() == 1) flags = int(b'1100', atomic_int_kind)
        sync all

        me = this_image()
        call atomic_fetch_xor(flags[1], int(b'1010', atomic_int_kind), old, stat)

        if (stat /= 0) print *, "Image", me, ": Failed with STAT =", stat
        print *, "Image", me, ": Old =", old
        sync all

        if (this_image() == 1) print *, "Final flags:", flags
      end program demo_atomic_fetch_xor
