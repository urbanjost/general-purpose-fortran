     program demo_system_errno
     use M_system, only : system_errno, system_unlink, system_perror
     implicit none
     integer :: stat
     stat=system_unlink('not there/OR/anywhere')
     if(stat.ne.0)then
             write(*,*)'err=',system_errno()
             call system_perror('*demo_system_errno*')
     endif
     end program demo_system_errno
