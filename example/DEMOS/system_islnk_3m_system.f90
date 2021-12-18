     program demo_system_islnk
     use M_system, only : system_islnk
     implicit none
     integer                     :: i
     character(len=80),parameter :: names(*)=[ &
     '/tmp            ', &
     '/tmp/NOTTHERE   ', &
     '/usr/local      ', &
     '.               ', &
     'link.test       ', &
     'PROBABLY_NOT    ']
     do i=1,size(names)
        write(*,*)' is ',trim(names(i)),' a link? ', system_islnk(names(i))
     enddo
     end program demo_system_islnk
