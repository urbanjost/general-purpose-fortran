     program demo_system_issock
     use M_system, only : system_issock
     implicit none
     integer                     :: i
     character(len=80),parameter :: names(*)=[ &
     '/tmp            ', &
     '/tmp/NOTTHERE   ', &
     '/usr/local      ', &
     '.               ', &
     'sock.test       ', &
     'PROBABLY_NOT    ']
     do i=1,size(names)
        write(*,*)' is ',trim(names(i)),' a socket? ', &
                & system_issock(names(i))
     enddo
     end program demo_system_issock
