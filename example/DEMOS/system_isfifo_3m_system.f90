     program demo_system_isfifo
     use M_system, only : system_isfifo
     implicit none
     integer                     :: i
     character(len=80),parameter :: names(*)=[ &
     '/tmp            ', &
     '/tmp/NOTTHERE   ', &
     '/usr/local      ', &
     '.               ', &
     'fifo.test       ', &
     'PROBABLY_NOT    ']
     do i=1,size(names)
        write(*,*)' is ',trim(names(i)),' a fifo(named pipe)? ', &
                & system_isfifo(names(i))
     enddo
     end program demo_system_isfifo
