          program demo_system_setsid
          use M_system,      only : system_setsid
          implicit none
             write(*,*)'SID=',system_setsid()
          end program demo_system_setsid
