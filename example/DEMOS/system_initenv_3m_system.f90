     program demo_system_initenv
     use M_system, only : system_initenv, system_readenv
     character(len=:),allocatable :: string
        call system_initenv()
        do
           string=system_readenv()
           if(string.eq.'')then
              exit
           else
              write(*,'(a)')string
           endif
        enddo
     end program demo_system_initenv
