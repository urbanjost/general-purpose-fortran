             program demo_get_env
             use M_io, only : get_env
             character(len=:),allocatable :: HOME
                HOME=get_env('HOME','UNKNOWN')
                write(*,'(a)')HOME,get_env('PATH')
                write(*,'(a)')get_env('HOME'),get_env('PATH')
             end program demo_get_env
