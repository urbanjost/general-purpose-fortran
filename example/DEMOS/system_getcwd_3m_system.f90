            program demo_system_getcwd
            use M_system, only : system_getcwd
            implicit none
            character(len=:),allocatable :: dirname
            integer                      :: ierr
            call system_getcwd(dirname,ierr)
            if(ierr.eq.0)then
               write(*,*)'CURRENT DIRECTORY ',trim(dirname)
            else
               write(*,*)'ERROR OBTAINING CURRENT DIRECTORY NAME'
            endif
            end program demo_system_getcwd
