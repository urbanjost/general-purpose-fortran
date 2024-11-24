     program demo_readenv
     use M_io, only : readenv, getname
     character(len=*),parameter :: g='(*(g0))'
     integer :: ierr

        if(readenv('STOP').eq.'RUN')then
           write(*,g)repeat('-',80)
           write(*,g)readenv('CHARACTER','string')
           write(*,g)readenv('INTEGER',100)
           write(*,g)readenv('REAL',200.0)
           write(*,g)readenv('DOUBLE',300.0d0)
           write(*,g)readenv('LOGICAL',.true.)

           write(*,g)repeat('-',80)
           write(*,g)readenv('CHARACTER','string',ierr=ierr)
           write(*,*)'ierr=',ierr
           write(*,g)readenv('INTEGER',100,ierr=ierr)
           write(*,*)'ierr=',ierr
           write(*,g)readenv('REAL',200.0,ierr=ierr)
           write(*,*)'ierr=',ierr
           write(*,g)readenv('DOUBLE',300.0d0,ierr=ierr)
           write(*,*)'ierr=',ierr
           write(*,g)readenv('LOGICAL',.true.)
           write(*,*)'ierr=',ierr

           write(*,g)repeat('-',80)
           write(*,g)readenv('CHARACTER')
           write(*,g)readenv('HOME')
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

     end program demo_readenv
