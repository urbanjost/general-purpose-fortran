     program demo_system_link
     use M_system, only : system_link, system_perror
     integer :: ierr
     ierr = system_link('myfile1','myfile2')
     if(ierr.ne.0)then
        call system_perror('*demo_system_link*')
     endif
     end program demo_system_link
