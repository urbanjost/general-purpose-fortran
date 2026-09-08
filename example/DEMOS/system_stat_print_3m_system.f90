     program demo_system_stat_print
     use M_system, only : system_stat_print
     implicit none
        call system_stat_print('/tmp')
        call system_stat_print('/etc/hosts')
     end program demo_system_stat_print
