      program demo_cpu_time
      use, intrinsic :: iso_fortran_env, only : real_kinds,real32,real64,real128
      implicit none
      real :: start, finish
      real(kind=real64) :: startd, finishd
         !
         call cpu_time(start)
         call cpu_time(startd)
         ! put code to time here
         call cpu_time(finish)
         call cpu_time(finishd)
         !
        ! writes processor time taken by the piece of code.

        ! the accuracy of the clock and whether it includes system time
        ! as well as user time is processor dependent. Accuracy up to
        ! milliseconds is common but not guaranteed, and may be much
        ! higher or lower
         print '("Processor Time = ",f6.3," seconds.")',finish-start

         ! see your specific compiler documentation for how to measure
         ! parallel jobs and for the precision of the time returned
         print '("Processor Time = ",g0," seconds.")',finish-start
         print '("Processor Time = ",g0," seconds.")',finishd-startd
      end program demo_cpu_time
