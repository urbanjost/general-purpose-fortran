     program demo_system_getgrgid
     use M_system, only : system_getgrgid
     use M_system, only : system_getgid
     implicit none
     character(len=:),allocatable :: name
     name=system_getgrgid( system_getgid() )
     write(*,'("group[",a,"] for ",i0)')name,system_getgid()
     end program demo_system_getgrgid
