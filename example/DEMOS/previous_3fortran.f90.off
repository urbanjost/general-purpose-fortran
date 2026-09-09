      program demo_previous
      implicit none

      ! Fortran 2023 strongly-typed enumeration
      enum, bind(c) :: color
         enumerator :: red, green, blue
      end enum

      type(color) :: current_color

        ! Initialize to the first item
        current_color = red
        print *, "Initial position: ", int(current_color)

        ! Advance using the new NEXT intrinsic
        current_color = next(current_color)
        print *, "Next position (green): ", int(current_color)

        ! Advance again
        current_color = next(current_color)
        print *, "Next position (blue): ", int(current_color)

        ! Move backward using the new PREVIOUS intrinsic
        current_color = previous(current_color)
        print *, "Previous position (green): ", int(current_color)

      end program demo_previous
