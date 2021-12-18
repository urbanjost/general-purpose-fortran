     program demo_system_getegid
     use M_system, only : system_getegid
     implicit none
        write(*,*)'GID=',system_getegid()
     end program demo_system_getegid
