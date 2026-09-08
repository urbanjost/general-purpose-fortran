     program demo_system_getegid
     use M_system, only : system_getegid
     implicit none
        write(*,*)'EGID=',system_getegid()
     end program demo_system_getegid
