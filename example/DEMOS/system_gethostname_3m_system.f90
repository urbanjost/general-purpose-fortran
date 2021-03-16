          program demo_system_gethostname
          use M_system, only : system_gethostname
          implicit none
          character(len=:),allocatable :: name
          integer                      :: ierr
             call system_gethostname(name,ierr)
             if(ierr.eq.0)then
                write(*,'("hostname[",a,"]")')name
             else
                write(*,'(a)')'ERROR: could not get hostname'
             endif
          end program demo_system_gethostname
