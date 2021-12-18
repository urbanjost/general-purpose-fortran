     program demo_system_perror
     use M_system, only : system_perror,system_rmdir
     implicit none
     character(len=:),allocatable :: DIRNAME
     DIRNAME='/NOT/THERE/OR/ANYWHERE'
     ! generate an error with a routine that supports errno and perror(3c)
     if(system_rmdir(DIRNAME).ne.0)then
        call system_perror('*demo_system_perror*:'//DIRNAME)
     endif
     write(*,'(a)')"That's all Folks!"
     end program demo_system_perror
