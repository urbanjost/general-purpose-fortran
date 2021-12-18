     program demo_system_getsid
     use M_system,      only : system_getsid
     use ISO_C_BINDING, only : c_int
     implicit none
        write(*,*)'SID=',system_getsid(0_c_int)
     end program demo_system_getsid
