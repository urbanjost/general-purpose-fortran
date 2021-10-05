          program demo_set_environment_variable
          use M_system, only : set_environment_variable
          use iso_c_binding
          implicit none
          integer :: ierr
             !x!
             write(*,'(a)')'no environment variables containing "GRU":'
             call execute_command_line('env|grep GRU')
             !x!
             call set_environment_variable('GRU','this is the value',ierr)
             write(*,'(a,i0)')'now "GRU" should be defined, status=',ierr
             call execute_command_line('env|grep GRU')
             !x!
             call set_environment_variable('GRU2','this is the second value',ierr)
             write(*,'(a,i0)')'now "GRU" and "GRU2" should be defined, status =',ierr
             !x!
             call execute_command_line('env|grep GRU')
          end program demo_set_environment_variable
