     program demo_system_getgid
     use M_system, only : system_getgid
     implicit none
        write(*,*)'GID=',system_getgid()
     end program demo_system_getgid
