     program demo_system_getpid
     use M_system, only : system_getpid
     implicit none
        write(*,*)'PID=',system_getpid()
     end program demo_system_getpid
