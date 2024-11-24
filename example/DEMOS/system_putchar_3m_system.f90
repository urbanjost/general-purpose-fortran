       program demo_system_putchar
       use M_system,      only : system_putchar
       implicit none
       integer :: i
       integer :: iostat
       do i=32,126 ! printable ASCII characters
          iostat=system_putchar(achar(i))
          if(iostat.lt.0)stop '<ERROR> *main* character '//achar(i)
       enddo
       iostat=system_putchar(new_line('a'))
       end program demo_system_putchar
