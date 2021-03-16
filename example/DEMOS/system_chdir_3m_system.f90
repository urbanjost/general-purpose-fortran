            program demo_system_chdir
            use M_system, only : system_chdir
            implicit none
            integer :: ierr

            call execute_command_line('pwd')
            call system_chdir('/tmp',ierr)
            call execute_command_line('pwd')
            write(*,*)'*CHDIR TEST* IERR=',ierr

            end program demo_system_chdir
