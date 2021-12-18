     program demo_system_isblk
     use M_system, only : system_isblk
     implicit none
     integer                     :: i
     character(len=80),parameter :: names(*)=[ &
     '/tmp            ', &
     '/tmp/NOTTHERE   ', &
     '/usr/local      ', &
     '.               ', &
     'block_device.tst', &
     'PROBABLY_NOT    ']
     do i=1,size(names)
         write(*,*)' is ',trim(names(i)),' a block device? ', system_isblk(names(i))
     enddo
     end program demo_system_isblk
