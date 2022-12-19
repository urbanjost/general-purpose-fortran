      program demo_epsilon
      use,intrinsic :: iso_fortran_env, only : dp=>real64,sp=>real32
      implicit none
      real(kind=sp) :: x = 3.143
      real(kind=dp) :: y = 2.33d0

         ! so if x is of type real32, epsilon(x) has the value 2**-23
         print *, epsilon(x)
         ! note just the type and kind of x matter, not the value
         print *, epsilon(huge(x))
         print *, epsilon(tiny(x))

         ! the value changes with the kind of the real value though
         print *, epsilon(y)

         ! adding and subtracting epsilon(x) changes x
         write(*,*)x == x + epsilon(x)
         write(*,*)x == x - epsilon(x)

         ! these next two comparisons will be .true. !
         write(*,*)x == x + epsilon(x) * 0.999999
         write(*,*)x == x - epsilon(x) * 0.999999

         ! you can calculate epsilon(1.0d0)
         write(*,*)my_dp_eps()

      contains

         function my_dp_eps()
         ! calculate the epsilon value of a machine the hard way
         real(kind=dp) :: t
         real(kind=dp) :: my_dp_eps

            ! starting with a value of 1, keep dividing the value
            ! by 2 until no change is detected. Note that with
            ! infinite precision this would be an infinite loop,
            ! but floating point values in Fortran have a defined
            ! and limited precision.
            my_dp_eps = 1.0d0
            SET_ST: do
               my_dp_eps = my_dp_eps/2.0d0
               t = 1.0d0 + my_dp_eps
               if (t <= 1.0d0) exit
            enddo SET_ST
            my_dp_eps = 2.0d0*my_dp_eps

         end function my_dp_eps
      end program demo_epsilon
