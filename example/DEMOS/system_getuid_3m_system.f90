     program demo_system_getuid
     use M_system, only : system_getuid
     implicit none
        write(*,*)'UID=',system_getuid()
     end program demo_system_getuid
