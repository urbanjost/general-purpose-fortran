     program demo_system_getppid
     use M_system, only : system_getppid
     implicit none
        write(*,*)'PPID=',system_getppid()
     end program demo_system_getppid
