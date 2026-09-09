     program demo_system_geteuid
     use M_system, only : system_geteuid
     implicit none
        write(*,*)'EFFECTIVE UID=',system_geteuid()
     end program demo_system_geteuid
