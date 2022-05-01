     program demo_system_ischr
     use M_system, only : system_ischr
     implicit none
     integer                     :: i
     character(len=80),parameter :: names(*)=[ &
     '/tmp            ', &
     '/tmp/NOTTHERE   ', &
     '/usr/local      ', &
     '.               ', &
     'char_dev.test   ', &
     'PROBABLY_NOT    ']
     do i=1,size(names)
        write(*,*)' is ',                   &
                 & trim(names(i)),          &
                 & ' a character device? ', &
                 & system_ischr(names(i))
     enddo
     end program demo_system_ischr
