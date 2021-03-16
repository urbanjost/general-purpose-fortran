            program demo_system_clearenv
            use M_system, only : system_clearenv
            implicit none
            ! environment before clearing
            call execute_command_line('env|wc')
            ! environment after clearing (not necessarily blank!)
            call system_clearenv()
            call execute_command_line('env')
            end program demo_system_clearenv
