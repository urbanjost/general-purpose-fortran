      program demo_anint
      use, intrinsic :: iso_fortran_env, only : real32, real64, real128
      implicit none
      real,allocatable :: arr(:)

        ! basics
         print *, 'ANINT (2.783) has the value 3.0 =>', anint(2.783)
         print *, 'ANINT (-2.783) has the value -3.0 =>', anint(-2.783)

         print *, 'by default the kind of the output is the kind of the input'
         print *, anint(1234567890.1234567890e0)
         print *, anint(1234567890.1234567890d0)

         print *, 'sometimes specifying the result kind is useful when passing'
         print *, 'results as an argument, for example.'
         print *, 'do you know why the results are different?'
         print *, anint(1234567890.1234567890,kind=real64)
         print *, anint(1234567890.1234567890d0,kind=real64)

        ! elemental
         print *, 'numbers on a cusp are always the most troublesome'
         print *, anint([ -2.7, -2.5, -2.2, -2.0, -1.5, -1.0, -0.5, 0.0 ])

         print *, 'negative zero is processor dependent'
         arr=[ 0.0, 0.1, 0.5, 1.0, 1.5, 2.0, 2.2, 2.5, 2.7 ]
         print *, anint(arr)
         arr=[ -0.0, -0.1, -0.5, -1.0, -1.5, -2.0, -2.2, -2.5, -2.7 ]
         print *, anint(arr)

      end program demo_anint
