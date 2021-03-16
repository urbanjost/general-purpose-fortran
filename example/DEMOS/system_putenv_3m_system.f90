           program demo_system_putenv
           use M_system, only : system_putenv
           use iso_c_binding
           implicit none
           integer :: ierr
              !
              write(*,'(a)')'no environment variables containing "GRU":'
              call execute_command_line('env|grep GRU')
              !
              call system_putenv('GRU=this is the value',ierr)
              write(*,'(a,i0)')'now "GRU" should be defined: ',ierr
              call execute_command_line('env|grep GRU')
              !
              call system_putenv('GRU2=this is the second value',ierr)
              write(*,'(a,i0)')'now "GRU" and "GRU2" should be defined: ',ierr
              call execute_command_line('env|grep GRU')
              !
              call system_putenv('GRU2',ierr)
              call system_putenv('GRU',ierr)
              write(*,'(a,i0)')'should be gone, varies with different putenv(3c): ',ierr
              call execute_command_line('env|grep GRU')
              write(*,'(a)')'system_unsetenv(3f) is a better way to remove variables'
              !
           end program demo_system_putenv
