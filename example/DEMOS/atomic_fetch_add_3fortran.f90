      program demo_atomic_fetch_add
        use iso_fortran_env
        implicit none
        integer(atomic_int_kind) :: counter[*]  ! Coarray for shared counter
        integer(atomic_int_kind) :: old_value   ! Stores value before addition
        integer :: stat, me, i

        ! Initialize counter on image 1
        if (this_image() == 1) counter = 0
        sync all  ! Ensure all images see initialized counter

        me = this_image()  ! Get current image number

        ! Each image atomically adds its image number to the counter
        call atomic_fetch_add(counter[1], me, old_value, stat)

        ! Check for errors
        if (stat /= 0) then
          print *, "Image", me, ": Operation failed with STAT =", stat
        else
          print *, "Image", me, ": Old value =", old_value, ", Added", me
        end if

        ! Synchronize all images before printing final result
        sync all

        ! Image 1 prints the final counter value
        if (this_image() == 1) then
          print *, "Final counter value:", counter
        end if
      end program demo_atomic_fetch_add
