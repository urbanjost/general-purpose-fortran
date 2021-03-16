          program demo_system_cpu_time

          use M_system, only : system_cpu_time
          use ISO_C_BINDING, only : c_float
          implicit none
          real    :: user_start, system_start, total_start
          real    :: user_finish, system_finish, total_finish
          integer :: i
          integer :: itimes=1000000
          real    :: value

             call system_cpu_time(total_start,user_start,system_start)

             value=0.0
             do i=1,itimes
                value=sqrt(real(i)+value)
             enddo
             write(10,*)value
             flush(10)
             write(*,*)'average sqrt value=',value/itimes
             call system_cpu_time(total_finish,user_finish,system_finish)
             write(*,*)'USER ......',user_finish-user_start
             write(*,*)'SYSTEM ....',system_finish-system_start
             write(*,*)'TOTAL .....',total_finish-total_start

              end program demo_system_cpu_time
