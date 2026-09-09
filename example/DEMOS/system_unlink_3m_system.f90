     program demo_system_unlink
     use M_system, only : system_unlink, system_perror
     integer :: ierr
     ierr = system_unlink('myfile1')
     if(ierr.ne.0)then
        call system_perror('*demo_system_unlink*')
     endif
     end program demo_system_unlink
