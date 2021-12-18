       program demo_system_unsetenv
       use M_system, only : system_unsetenv, system_putenv
       implicit none
       call system_putenv('GRU=this is the value')
       write(*,'(a)')'The variable GRU should be set'
       call execute_command_line('env|grep GRU')
       call system_unsetenv('GRU')
       write(*,'(a)')'The variable GRU should not be set'
       call execute_command_line('env|grep GRU')
       end program demo_system_unsetenv
