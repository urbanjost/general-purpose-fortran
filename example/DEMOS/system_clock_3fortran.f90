             program demo_system_clock
             implicit none
               integer :: count, count_rate, count_max
               call system_clock(count, count_rate, count_max)
               write(*,*) count, count_rate, count_max
             end program demo_system_clock
