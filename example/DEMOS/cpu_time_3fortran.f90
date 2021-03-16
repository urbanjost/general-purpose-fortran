           program demo_cpu_time
           implicit none
           real :: start, finish
              call cpu_time(start)
              ! put code to test here
              call cpu_time(finish)
              ! writes processor time taken by the piece of code.
              print '("Processor Time = ",f6.3," seconds.")',finish-start
           end program demo_cpu_time
