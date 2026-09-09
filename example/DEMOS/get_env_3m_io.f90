     program demo_get_env
     use M_io, only : get_env, getname
     character(len=*),parameter :: g='(*(g0))'
     integer :: ierr
     character(len=:),allocatable :: HOME
       !Basics
        HOME=get_env('HOME','UNKNOWN')
        write(*,'(a)')HOME
        write(*,'(a)')Get_env('PATH')

       !call this program setting STOP=RUN unless STOP=RUN
       !otherwise print various environment variable values
       !converted to various types
        if(get_env('STOP').eq.'RUN')then
           write(*,g)repeat('-',80)
           write(*,g)get_env('CHARACTER','string')
           write(*,g)get_env('INTEGER',100)
           write(*,g)get_env('REAL',200.0)
           write(*,g)get_env('DOUBLE',300.0d0)
           write(*,g)get_env('LOGICAL',.true.)

           write(*,g)repeat('-',80)
           write(*,g)get_env('CHARACTER','string',ierr=ierr)
           write(*,*)'ierr=',ierr
           write(*,g)get_env('INTEGER',100,ierr=ierr)
           write(*,*)'ierr=',ierr
           write(*,g)get_env('REAL',200.0,ierr=ierr)
           write(*,*)'ierr=',ierr
           write(*,g)get_env('DOUBLE',300.0d0,ierr=ierr)
           write(*,*)'ierr=',ierr
           write(*,g)get_env('LOGICAL',.true.)
           write(*,*)'ierr=',ierr

           write(*,g)repeat('-',80)
           write(*,g)get_env('CHARACTER')
           write(*,g)get_env('HOME')
         else
           write(*,g)repeat('-',80)
           call execute_command_line('env STOP=RUN '//getname())
           call execute_command_line('env STOP=RUN CHARACTER=aaaa &
           & INTEGER=1 REAL=2.3 DOUBLE=444444444444 '//getname())
           call execute_command_line('env STOP=RUN CHARACTER=bbbb &
           & INTEGER=1 REAL=2.3 DOUBLE=44.555 '//getname())
           call execute_command_line('env STOP=RUN CHARACTER=cccc &
           & INTEGER=asdf REAL=asdf DOUBLE=adsf '//getname())
           write(*,g)repeat('-',80)
           stop
        endif

     end program demo_get_env
