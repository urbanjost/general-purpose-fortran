      program demo_atomic_ref
        use iso_fortran_env
        implicit none
        integer(atomic_int_kind) :: counter[*], value
        integer :: stat, me

        if (this_image() == 1) counter = 42
        sync all

        me = this_image()
        call atomic_ref(value, counter[1], stat)

        if (stat /= 0) then
          print *, "Image", me, ": Failed with STAT =", stat
        else
          print *, "Image", me, ": Retrieved value =", value
        end if
      end program demo_atomic_ref
