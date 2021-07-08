           program demo_process_readall
            use M_process, only: process_readall
            implicit none
            integer :: ierr
            character(len=:),allocatable :: string
                string=process_readall('ls',ierr=ierr)
                write(*,*)ierr,string
            end program demo_process_readall
